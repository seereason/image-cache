-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the 'build'
-- method of the MonadCache instance, and stored using acid-state.

{-# LANGUAGE CPP, ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -Wredundant-constraints -fno-warn-orphans #-}

module Appraisal.AcidCache
    ( -- * Open cache
      CacheMap(..)
    , unCacheMap
    , initCacheMap
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
    , HasCacheAcid(askCacheAcid)
    , HasCacheFunction(buildCacheValue)
    , cacheMap
    , cacheLook
    , cacheInsert
    , cacheDelete
    -- * Instance
    -- , runMonadCacheT
    ) where

import Appraisal.FileError (HasFileError)
import Control.Lens ((%=), at, makeLenses, view)
import Control.Monad.Catch (bracket, {-MonadCatch,-} MonadMask)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.State (liftIO)
import Data.Acid (AcidState, makeAcidic, openLocalStateFrom, Query, query, Update, update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Generics (Data, Proxy, Typeable)
import Data.Map.Strict as Map (delete, difference, fromSet, insert, intersection, Map, union)
import Data.SafeCopy -- (deriveSafeCopy, extension, Migrate(..), SafeCopy)
import Data.Serialize (label, Serialize)
import Data.Set as Set (Set)
import Extra.Except (catchError, HasIOException, liftIOError, MonadIO, MonadError, MonadIOError)
import GHC.Generics (Generic)

data CacheMap key val e = CacheMap {_unCacheMap :: Map key (Either e val)} deriving (Data, Generic, Serialize)
$(makeLenses ''CacheMap)

#if 0
$(deriveSafeCopy 2 'extension ''CacheMap)
-- $(safeCopyInstance 2 'extension [t|CacheMap|])
#else
instance (Ord key, SafeCopy key, SafeCopy val, SafeCopy e) => SafeCopy (CacheMap key val e) where
      putCopy (CacheMap a)
        = contain
            (do safeput <- getSafePut
                safeput a
                return ())
      getCopy
        = contain
            ((label "Appraisal.AcidCache.CacheMap:")
               (do safeget <- getSafeGet
                   (return CacheMap <*> safeget)))
      version = 2
      kind = extension
      errorTypeName _ = "Appraisal.AcidCache.CacheMap"
#endif

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap key val e) where
    type MigrateFrom (CacheMap key val e) = Map key val
    migrate mp = CacheMap (fmap Right mp)

-- | Install a key/value pair into the cache.
putValue :: Ord key => key -> Either e val -> Update (CacheMap key val e) ()
putValue key img = unCacheMap %= Map.insert key img

-- | Install several key/value pairs into the cache.
putValues :: Ord key => Map key (Either e val) -> Update (CacheMap key val e) ()
putValues pairs = unCacheMap %= Map.union pairs

-- | Look up a key.
lookValue :: Ord key => key -> Query (CacheMap key val e) (Maybe (Either e val))
lookValue key = view (unCacheMap . at key)

-- | Look up several keys.
lookValues :: Ord key => Set key -> Query (CacheMap key val e) (Map key (Either e val))
lookValues keys = Map.intersection <$> view unCacheMap <*> pure (Map.fromSet (const ()) keys)

-- | Return the entire cache.  (Despite what ghc says, this constraint
-- isn't redundant, without it the makeAcidic call has a missing Ord
-- key instance.)
lookMap :: Ord key => Query (CacheMap key val e) (CacheMap key val e)
lookMap = ask

-- | Remove values from the database.
deleteValue :: (Ord key{-, Serialize key, Serialize val, Serialize e-}) => key -> Update (CacheMap key val e) ()
deleteValue key = unCacheMap %= Map.delete key

deleteValues :: Ord key => Set key -> Update (CacheMap key val e) ()
deleteValues keys = unCacheMap %= (`Map.difference` (Map.fromSet (const ()) keys))

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues])

initCacheMap :: Ord key => CacheMap key val e
initCacheMap = CacheMap mempty

openCache :: (SafeCopy e, Typeable e,
              SafeCopy key, Typeable key,
              SafeCopy val, Typeable val, Ord key) => FilePath -> IO (AcidState (CacheMap key val e))
openCache path = openLocalStateFrom path initCacheMap

-- | In theory the MonadError type e1 might differ from the error type
-- stored in the map e2.  But I'm not sure if it would work in practice.
withCache :: (HasIOException e1, MonadError e1 m, MonadIO m, MonadMask m,
              Typeable e2, SafeCopy e2,
              Typeable val, SafeCopy val,
              Ord key, Typeable key, SafeCopy key) => FilePath -> (AcidState (CacheMap key val e2) -> m b) -> m b
withCache path f = bracket (liftIOError (openCache path)) (liftIOError . createCheckpointAndClose) $ f

class (Ord key, SafeCopy key, Typeable key, Show key, Serialize key,
       SafeCopy val, Typeable val, Serialize val,
       HasFileError e, MonadIOError e m, Serialize e, SafeCopy e, Typeable e) => HasCacheAcid key val e m where
    askCacheAcid :: m (AcidState (CacheMap key val e))

class (Ord key, HasFileError e, MonadIOError e m) => HasCacheFunction key val e m where
    buildCacheValue :: key -> m (Either e val)

-- | Call the build function on cache miss to build the value.
cacheInsert :: forall key val e m. (HasCacheAcid key val e m, HasCacheFunction key val e m) => key -> m (Either e val)
cacheInsert key = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val e))
  liftIO (query st (LookValue key)) >>= maybe (cacheMiss `catchError` cacheError) return
    where
      cacheMiss :: m (Either e val)
      cacheMiss = do
        val <- buildCacheValue key
        st <- askCacheAcid :: m (AcidState (CacheMap key val e))
        () <- liftIO $ update st (PutValue key val)
        return val
      cacheError :: e -> m (Either e val)
      cacheError e = return (Left e)

-- | Query the cache, but do nothing on cache miss.
cacheLook :: HasCacheAcid key val e m => key -> m (Maybe (Either e val))
cacheLook key = do
  st <- askCacheAcid
  liftIO $ query st (LookValue key)

cacheMap :: HasCacheAcid key val e m => m (CacheMap key val e)
cacheMap = do
  st <- askCacheAcid
  liftIO $ query st LookMap

cacheDelete :: forall key val e m. (HasCacheAcid key val e m) => Proxy val -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val e)) <- askCacheAcid
  liftIO $ update st (DeleteValues keys)
