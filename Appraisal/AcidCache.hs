-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the 'build'
-- method of the MonadCache instance, and stored using acid-state.

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Appraisal.AcidCache
    ( -- * Cache declarations
      openValueCache
    , withValueCache
    , runMonadCacheT
    -- * Cached map events
    , PutValue(..)
    , PutValues(..)
    , LookValue(..)
    , LookValues(..)
    , LookMap(..)
    -- * Monad class for cached map
    , MonadCache(askAcidState, build)
    , cacheMap
    , cacheLook
    , cacheInsert
    ) where

import Control.Exception (bracket)
import Control.Monad.Reader (MonadReader(ask), ReaderT(runReaderT))
import Control.Monad.State (MonadIO(..), MonadState(get, put), modify)
import Data.Acid (AcidState, makeAcidic, openLocalStateFrom, Query, query, Update, update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Generics (Typeable)
import Data.Map as Map (fromList, insert, lookup, Map)
import Data.SafeCopy (SafeCopy)

-- | Install a key/value pair into the cache.
putValue :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => key -> val -> Update (Map key val) ()
putValue key img = modify (Map.insert key img)

-- | Install several key/value pairs into the cache.
putValues :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => [(key, val)] -> Update (Map key val) ()
putValues pairs =
    do mp <- get
       put $ foldl (\ mp' (k, file) -> Map.insert k file mp') mp pairs

-- | Look up a key.
lookValue :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => key -> Query (Map key val) (Maybe val)
lookValue key = Map.lookup key <$> ask

-- | Look up several keys.
lookValues :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => [key] -> Query (Map key val) (Map key (Maybe val))
lookValues keys =
    do mp <- ask
       let imgs = map (`Map.lookup` mp) keys
       return $ fromList (zip keys imgs)

-- | Return the entire cache
lookMap :: (Show key, SafeCopy key, Ord key, Show val, SafeCopy val, Typeable key, Typeable val) => Query (Map key val) (Map key val)
lookMap = ask

$(makeAcidic ''Map ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap])

initCacheMap :: Ord key => Map key val
initCacheMap = mempty

openValueCache :: (Show key, Ord key, Typeable key, SafeCopy key, Typeable val, Show val, SafeCopy val) =>
                  FilePath -> IO (AcidState (Map key val))
openValueCache path = openLocalStateFrom path initCacheMap

withValueCache :: (Show key, Ord key, Typeable key, SafeCopy key, Typeable val, Show val, SafeCopy val) =>
                  FilePath -> (AcidState (Map key val) -> IO a) -> IO a
withValueCache path f = bracket (openValueCache path) createCheckpointAndClose $ f

-- | Given the AcidState object for the cache, Run an action in the CacheIO monad.
runMonadCacheT :: ReaderT (AcidState (Map key val)) m a -> AcidState (Map key val) -> m a
runMonadCacheT action st = runReaderT action st

-- | Class of monads for managing a key/value cache in acid state.
-- The monad must be in MonadIO because it needs to query the acid
-- state.
class (Show key, SafeCopy key, Eq key, Ord key, Typeable key,
       Typeable val, Show val, SafeCopy val, MonadIO m)
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
