-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the 'build'
-- method of the MonadCache instance, and stored using acid-state.

{-# LANGUAGE CPP, ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}

module Appraisal.AcidCache
    ( -- * Open cache
      openValueCache
    , withValueCache
    -- * Cached map events
    , AcidVal
    , AcidKey
    , PutValue(..)
    , PutValues(..)
    , LookValue(..)
    , LookValues(..)
    , LookMap(..)
    , DeleteValue(..)
    , DeleteValues(..)
    -- * Monad class for cached map
    , MonadCache(askAcidState, build)
    , cacheMap
    , cacheLook
    , cacheInsert
    , cacheDelete
    -- * Instance
    , runMonadCacheT
    ) where

import Appraisal.FileError (FileError, IsFileError)
import Control.Monad.Catch (bracket, {-MonadCatch,-} MonadMask)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.RWS
import Control.Monad.State (liftIO, MonadIO, modify)
import Data.Acid (AcidState, makeAcidic, openLocalStateFrom, Query, query, Update, update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Generics (Proxy, Typeable)
import Data.Map.Strict as Map (delete, difference, fromSet, insert, intersection, lookup, Map, union)
import Data.SafeCopy (SafeCopy)
import Data.Set as Set (Set)

type AcidVal val = (Show val, SafeCopy val, Typeable val)
type AcidKey key = (AcidVal key, Eq key, Ord key)

-- | Install a key/value pair into the cache.
putValue :: (AcidKey key{-, AcidVal val-}) => key -> val -> Update (Map key val) ()
putValue key img = modify (Map.insert key img)

-- | Install several key/value pairs into the cache.
putValues :: (AcidKey key{-, AcidVal val-}) => Map key val -> Update (Map key val) ()
putValues pairs = modify (Map.union pairs)

-- | Look up a key.
lookValue :: (AcidKey key{-, AcidVal val-}) => key -> Query (Map key val) (Maybe val)
lookValue key = Map.lookup key <$> ask

-- | Look up several keys.
lookValues :: (AcidKey key{-, AcidVal val-}) => Set key -> Query (Map key val) (Map key val)
lookValues keys = Map.intersection <$> ask <*> pure (Map.fromSet (const ()) keys)

-- | Return the entire cache
lookMap :: (AcidKey key{-, AcidVal val-}) => Query (Map key val) (Map key val)
lookMap = ask

-- | Remove values from the database.
deleteValue :: (AcidKey key{-, AcidVal val-}) => key -> Update (Map key val) ()
deleteValue key = modify (Map.delete key)

deleteValues :: (AcidKey key{-, AcidVal val-}) => Set key -> Update (Map key val) ()
deleteValues keys = modify (`Map.difference` (Map.fromSet (const ()) keys))

$(makeAcidic ''Map ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues])

_initCacheMap :: Ord key => Map key val
_initCacheMap = mempty

openValueCache :: (AcidKey key, AcidVal val) =>
                  FilePath -> IO (AcidState (Map key val))
openValueCache path = openLocalStateFrom path _initCacheMap

withValueCache :: (AcidKey key, AcidVal val, MonadIO m, MonadMask m) =>
                  FilePath -> (AcidState (Map key val) -> m a) -> m a
withValueCache path f = bracket (liftIO (openValueCache path)) (liftIO . createCheckpointAndClose) $ f
#if 0
withAcidState ::
    (IsAcidic a, Typeable a)
    => FilePath
    -> a
    -> (AcidState a -> IO r)
    -> IO r
withAcidState path initial f = bracket (openLocalStateFrom path initial) createCheckpointAndClose $ f
#endif

-- | Class of monads for managing a key/value cache in acid state.
-- The monad must be in MonadIO because it needs to query the acid
-- state.
class (AcidKey key, AcidVal val, MonadIO m{-, IsFileError e, MonadError e m-}) => MonadCache key val m where
    askAcidState :: m (AcidState (Map key val))
    build :: key -> ExceptT FileError m val
    -- ^ A monadic, possibly expensive function to create a new map entry.
    -- Our application in ImageCache.hs is to scale/rotate/crop an image.

-- | Call the build function on cache miss to build the value.
cacheInsert :: forall key val m. (MonadCache key val m) => key -> ExceptT FileError m val
cacheInsert key = do
  st <- lift askAcidState
  mval <- liftIO $ query st (LookValue key)
  maybe (do val <- build key
            () <- liftIO $ update st (PutValue key val)
            return val) return mval

-- | Query the cache, but do nothing on cache miss.
cacheLook :: forall key val e m. (MonadCache key val m, IsFileError e, MonadError e m) => key -> ExceptT e m (Maybe val)
cacheLook key = do
  st <- lift askAcidState
  liftIO $ query st (LookValue key)

cacheMap :: forall key val e m. (MonadCache key val m, IsFileError e, MonadError e m) => ExceptT e m (Map key val)
cacheMap = do
  st <- lift askAcidState
  liftIO $ query st LookMap

cacheDelete :: forall key val e m. (MonadCache key val m, IsFileError e, MonadError e m) => Proxy val -> Set key -> ExceptT e m ()
cacheDelete _ keys = do
  (st :: AcidState (Map key val)) <- lift askAcidState
  liftIO $ update st (DeleteValues keys)

-- | Given the AcidState object for the cache, Run an action in the CacheIO monad.
runMonadCacheT :: Monad m => RWST (AcidState (Map key val)) () () m a -> AcidState (Map key val) -> m a
runMonadCacheT action st = fst <$> evalRWST action st ()
