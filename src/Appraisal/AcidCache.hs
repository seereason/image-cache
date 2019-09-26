-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the 'build'
-- method of the MonadCache instance, and stored using acid-state.

{-# LANGUAGE CPP, DataKinds #-}
{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wall -Wredundant-constraints -fno-warn-orphans #-}

module Appraisal.AcidCache
    ( initCacheMap
    , openCache
    , withCache
    -- * Cached map events
    , PutValue(..)
    , PutValues(..)
    , LookValue(..)
    , LookValues(..)
    , LookMap(..)
    , DeleteValue(..)
    , DeleteValues(..)
    -- * Monad class for cached map
    , HasCache(askCacheAcid, buildCacheValue)
    , cacheMap
    , cacheLook
    , cacheInsert
    , cacheDelete
    -- * Instance
    -- , runMonadCacheT
    ) where

import Data.Map.Strict as Map (Map)
import Data.SafeCopy -- (deriveSafeCopy, extension, Migrate(..), SafeCopy)
import Control.Lens ((%=), at, view)
import Control.Monad.Catch (bracket, {-MonadCatch,-} MonadMask)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.State (liftIO)
import Data.Acid (AcidState, makeAcidic, openLocalStateFrom, Query, query, Update, update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Generics (Proxy, Typeable)
import Data.Generics.Product (field)
import Data.Map.Strict as Map (delete, difference, fromSet, insert, intersection, union)
import Data.Set as Set (Set)
import Extra.Except (liftIOError, MonadIO, MonadIOError)
import FileCache.Types

-- | Install a key/value pair into the cache.
putValue :: Ord key => key -> CacheValue err val -> Update (CacheMap key val err) ()
putValue key img = field @"_unCacheMap" %= Map.insert key img

-- | Install several key/value pairs into the cache.
putValues :: Ord key => Map key (CacheValue err val) -> Update (CacheMap key val err) ()
putValues pairs = field @"_unCacheMap" %= Map.union pairs

-- | Look up a key.
lookValue :: Ord key => key -> Query (CacheMap key val err) (Maybe (CacheValue err val))
lookValue key = view (field @"_unCacheMap" . at key)

-- | Look up several keys.
lookValues :: Ord key => Set key -> Query (CacheMap key val err) (Map key (CacheValue err val))
lookValues keys = Map.intersection <$> view (field @"_unCacheMap") <*> pure (Map.fromSet (const ()) keys)

-- | Return the entire cache.  (Despite what ghc says, this constraint
-- isn't redundant, without it the makeAcidic call has a missing Ord
-- key instance.)
lookMap :: {-Ord key =>-} Query (CacheMap key val err) (CacheMap key val err)
lookMap = ask

-- | Remove values from the database.
deleteValue :: (Ord key{-, Serialize key, Serialize val, Serialize e-}) => key -> Update (CacheMap key val err) ()
deleteValue key = field @"_unCacheMap" %= Map.delete key

deleteValues :: Ord key => Set key -> Update (CacheMap key val err) ()
deleteValues keys = field @"_unCacheMap" %= (`Map.difference` (Map.fromSet (const ()) keys))

initCacheMap :: Ord key => CacheMap key val err
initCacheMap = CacheMap mempty

openCache :: (SafeCopy key, Typeable key, Ord key,
              SafeCopy err, Typeable err,
              SafeCopy val, Typeable val) => FilePath -> IO (AcidState (CacheMap key val err))
openCache path = openLocalStateFrom path initCacheMap

-- | In theory the MonadError type e1 might differ from the error type
-- stored in the map e2.  But I'm not sure if it would work in practice.
withCache :: (MonadIOError e m, MonadMask m,
              SafeCopy val, Typeable val, SafeCopy err, Typeable err,
              Ord key, Typeable key, SafeCopy key) => FilePath -> (AcidState (CacheMap key val err) -> m b) -> m b
withCache path f = bracket (liftIOError (openCache path)) (liftIOError . createCheckpointAndClose) $ f

-- | Note that class 'HasCache' and the 'cacheInsert' function return
-- values containing a 'FileError', but the monad m only has the
-- constraint HasFileError.
class (Ord key, SafeCopy key, Typeable key, Show key,
       SafeCopy val, Typeable val,
       SafeCopy err, Typeable err, MonadIO m) => HasCache key val err m where
    askCacheAcid :: m (AcidState (CacheMap key val err))
    buildCacheValue :: key -> m (CacheValue err val)

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues])

-- | Call the build function on cache miss to build the value.
cacheInsert :: forall key val err m. (HasCache key val err m) => key -> m (CacheValue err val)
cacheInsert key = do
  st <- askCacheAcid
  liftIO (query st (LookValue key)) >>= maybe (cacheMiss key) return

cacheMiss :: forall key val err m. (HasCache key val err m) => key -> m (CacheValue err val)
cacheMiss key = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val err))
  val <- buildCacheValue key
  () <- liftIO $ update st (PutValue key val)
  return val

-- | Query the cache, but do nothing on cache miss.
cacheLook :: HasCache key val err m => key -> m (Maybe (CacheValue err val))
cacheLook key = do
  st <- askCacheAcid
  liftIO $ query st (LookValue key)

cacheMap :: HasCache key val err m => m (CacheMap key val err)
cacheMap = do
  st <- askCacheAcid
  liftIO $ query st LookMap

cacheDelete :: forall key val err m. (HasCache key val err m) => Proxy (val, err) -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val err)) <- askCacheAcid
  liftIO $ update st (DeleteValues keys)
