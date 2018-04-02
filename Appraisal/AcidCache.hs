-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the 'build'
-- method of the MonadCache instance, and stored using acid-state.

{-# LANGUAGE ConstraintKinds #-}
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
    ( -- * Cache declarations
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

import Control.Exception (bracket)
import Control.Monad.Reader (MonadReader(ask), ReaderT(runReaderT))
import Control.Monad.State (MonadIO(..), MonadState(get, put), modify)
import Data.Acid (AcidState, makeAcidic, openLocalStateFrom, Query, query, Update, update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Generics (Typeable)
import Data.Map as Map (delete, fromList, insert, lookup, Map)
import Data.Proxy (Proxy)
import Data.SafeCopy (SafeCopy)

type AcidVal val = (Show val, SafeCopy val, Typeable val)
type AcidKey key = (AcidVal key, Eq key, Ord key)

-- | Install a key/value pair into the cache.
putValue :: (AcidKey key, AcidVal val) => key -> val -> Update (Map key val) ()
putValue key img = modify (Map.insert key img)

-- | Install several key/value pairs into the cache.
putValues :: (AcidKey key, AcidVal val) => [(key, val)] -> Update (Map key val) ()
putValues pairs =
    do mp <- get
       put $ foldl (\ mp' (k, file) -> Map.insert k file mp') mp pairs

-- | Look up a key.
lookValue :: (AcidKey key, AcidVal val) => key -> Query (Map key val) (Maybe val)
lookValue key = Map.lookup key <$> ask

-- | Look up several keys.
lookValues :: (AcidKey key, AcidVal val) => [key] -> Query (Map key val) (Map key (Maybe val))
lookValues keys =
    do mp <- ask
       let imgs = map (`Map.lookup` mp) keys
       return $ fromList (zip keys imgs)

-- | Return the entire cache
lookMap :: (AcidKey key, AcidVal val) => Query (Map key val) (Map key val)
lookMap = ask

-- | Remove values from the database.
deleteValues :: (AcidKey key, AcidVal val) => [key] -> Update (Map key val) ()
deleteValues keys =
  do mp <- get
     put $ foldr Map.delete mp keys

$(makeAcidic ''Map ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValues])

initCacheMap :: Ord key => Map key val
initCacheMap = mempty

openValueCache :: (AcidKey key, AcidVal val) =>
                  FilePath -> IO (AcidState (Map key val))
openValueCache path = openLocalStateFrom path initCacheMap

withValueCache :: (AcidKey key, AcidVal val) =>
                  FilePath -> (AcidState (Map key val) -> IO a) -> IO a
withValueCache path f = bracket (openValueCache path) createCheckpointAndClose $ f

-- | Class of monads for managing a key/value cache in acid state.
-- The monad must be in MonadIO because it needs to query the acid
-- state.
class (AcidKey key, AcidVal val, MonadIO m)
    => MonadCache key val m where
    askAcidState :: m (AcidState (Map key val))
    build :: key -> m val
    -- ^ A monadic, possibly expensive function to create a new map entry.
    -- Our application is to scale/rotate/crop an image.

-- | Call the build function on cache miss to build the value.
cacheInsert :: MonadCache key val m => key -> m val
cacheInsert key = do
  st <- askAcidState
  mval <- liftIO $ query st (LookValue key)
  maybe (do val <- build key
            () <- liftIO $ update st (PutValue key val)
            return val) return mval

-- | Query the cache, but do nothing on cache miss.
cacheLook :: MonadCache key val m => key -> m (Maybe val)
cacheLook key = do
  st <- askAcidState
  liftIO $ query st (LookValue key)

cacheMap :: MonadCache key val m => m (Map key val)
cacheMap = do
  st <- askAcidState
  liftIO $ query st LookMap

cacheDelete :: forall key val m. Show val => MonadCache key val m => Proxy val -> [key] -> m ()
cacheDelete _ keys = do
  (st :: AcidState (Map key val)) <- askAcidState
  liftIO $ update st (DeleteValues keys)

-- | Given the AcidState object for the cache, Run an action in the CacheIO monad.
runMonadCacheT :: ReaderT (AcidState (Map key val)) m a -> AcidState (Map key val) -> m a
runMonadCacheT action st = runReaderT action st
