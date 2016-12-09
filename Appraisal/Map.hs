-- | Acid-state operations on a key/value map

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Appraisal.Map
    ( CacheMap
    , CacheState
    -- , initCacheMap
    , openValueCache
    , withValueCache
    , runMonadCacheT

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

-- | Install a key/value pair into the cache.
putValue :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => key -> val -> Update (CacheMap key val) ()
putValue key img =
    do mp <- get
       put $ Map.insert key img mp

-- | Install several key/value pairs into the cache.
putValues :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => [(key, val)] -> Update (CacheMap key val) ()
putValues pairs =
    do mp <- get
       put $ foldl (\ mp' (k, file) -> Map.insert k file mp') mp pairs

-- | Look up a key.
lookValue :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => key -> Query (CacheMap key val) (Maybe val)
lookValue key =
    do mp <- ask
       return $ Map.lookup key mp

-- | Look up several keys.
lookValues :: (Show key, SafeCopy key, Ord key, Typeable key, Show val, SafeCopy val, Typeable val) => [key] -> Query (CacheMap key val) (Map key (Maybe val))
lookValues keys =
    do mp <- ask
       let imgs = map (`Map.lookup` mp) keys
       return $ fromList (zip keys imgs)

-- | Return the entire cache
lookMap :: (Show key, SafeCopy key, Ord key, Show val, SafeCopy val, Typeable key, Typeable val) => Query (CacheMap key val) (Map key val)
lookMap = ask

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap])

type CacheState key val = AcidState (CacheMap key val)

initCacheMap :: Ord key => CacheMap key val
initCacheMap = mempty

openValueCache :: (Show key, Ord key, Typeable key, SafeCopy key, Typeable val, Show val, SafeCopy val) =>
                  FilePath -> IO (CacheState key val)
openValueCache path = openLocalStateFrom path initCacheMap

withValueCache :: (Show key, Ord key, Typeable key, SafeCopy key, Typeable val, Show val, SafeCopy val) =>
                  FilePath -> (CacheState key val -> IO a) -> IO a
withValueCache path f = bracket (openValueCache path) createCheckpointAndClose $ f

-- | Given the AcidState object for the cache, Run an action in the CacheIO monad.
runMonadCacheT :: ReaderT (CacheState key val) m a -> CacheState key val -> m a
runMonadCacheT action st = runReaderT action st
