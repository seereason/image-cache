{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Appraisal.Cache
    ( MonadCache(askAcidState, build)
    , CacheMap
    , CacheState
    -- , initCacheMap
    , openValueCache
    , withValueCache
    , runMonadCacheT
    , cacheLook
    , cacheInsert

    , PutValue(..)
    , PutValues(..)
    , LookValue(..)
    , LookValues(..)
    , LookMap(..)
    ) where

import Control.Exception (bracket)
import Control.Monad.Reader (MonadReader(ask), ReaderT(runReaderT))
import Control.Monad.State (MonadIO(..), MonadState(get, put))
import Data.Acid (AcidState, makeAcidic, openLocalStateFrom, query, Query, update, Update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Generics (Data, Typeable)
import Data.Map as Map (fromList, insert, lookup, Map)
import Data.Monoid (Monoid(mempty))
import Data.SafeCopy (SafeCopy)

type CacheMap key val = Map key val

-- | Install an image into the cache.
putValue :: Ord key => key -> val -> Update (CacheMap key val) ()
putValue key img =
    do mp <- get
       put $ Map.insert key img mp

-- | Install several images into the cache.
putValues :: Ord key => [(key, val)] -> Update (CacheMap key val) ()
putValues pairs =
    do mp <- get
       put $ foldl (\ mp' (k, file) -> Map.insert k file mp') mp pairs

-- | Retrieve one image
lookValue :: Ord key => key -> Query (CacheMap key val) (Maybe val)
lookValue key =
    do mp <- ask
       return $ Map.lookup key mp

-- | Retrieve several images
lookValues :: Ord key => [key] -> Query (CacheMap key val) (Map key (Maybe val))
lookValues keys =
    do mp <- ask
       let imgs = map (`Map.lookup` mp) keys
       return $ fromList (zip keys imgs)

-- | Return the entire cache
lookMap :: Ord key => Query (CacheMap key val) (Map key val)
lookMap = ask

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap])

type CacheState key val = AcidState (CacheMap key val)

-- | Class of monads for managing a cache in acid state.
class (SafeCopy key, Eq key, Ord key, Typeable key, Data key,
       Typeable val, SafeCopy val, MonadIO m) => MonadCache key val m | val -> key where
    askAcidState :: m (CacheState key val)
    build :: key -> m val -- ^ A possibly expensive function to create a new map entry.

-- | Call the build function on cache miss
cacheInsert :: MonadCache key val m => key -> m val
cacheInsert key = do
  st <- askAcidState
  mval <- liftIO $ query st (LookValue key)
  maybe (do val <- build key
            () <- liftIO $ update st (PutValue key val)
            return val) return mval

cacheLook :: MonadCache key val m => key -> m (Maybe val)
cacheLook key = do
  st <- askAcidState
  liftIO $ query st (LookValue key)

initCacheMap :: Ord key => CacheMap key val
initCacheMap = mempty

openValueCache :: (Ord key, Typeable key, SafeCopy key, Typeable val, SafeCopy val) =>
                  FilePath -> IO (CacheState key val)
openValueCache path = openLocalStateFrom path initCacheMap

withValueCache :: (Ord key, Typeable key, SafeCopy key, Typeable val, SafeCopy val) =>
                  FilePath -> (CacheState key val -> IO a) -> IO a
withValueCache path f = bracket (openValueCache path) createCheckpointAndClose $ f

-- | Given the Paths and AcidState objects, Run an action in the CacheIO monad.
runMonadCacheT :: ReaderT (CacheState key val) m a -> (CacheState key val) -> m a
runMonadCacheT action st = runReaderT action st
