-- | 'MonadCache' lets us maintain 

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Appraisal.Cache
    ( MonadCache(askAcidState, build)
    , cacheMap
    , cacheLook
    , cacheInsert
    ) where

import Appraisal.Map
import Control.Monad.State (MonadIO(..))
import Data.Acid (query, update)
import Data.Generics (Data, Typeable)
import Data.SafeCopy (SafeCopy)

-- | Class of monads for managing a key/value cache in acid state.
-- The monad must be in MonadIO because it needs to query the acid
-- state.
class (Show key, SafeCopy key, Eq key, Ord key, Typeable key, Data key,
       Typeable val, Show val, SafeCopy val, MonadIO m)
    => MonadCache key val m where
    askAcidState :: m (CacheState key val)
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

cacheLook :: MonadCache key val m => key -> m (Maybe val)
cacheLook key = do
  st <- askAcidState
  liftIO $ query st (LookValue key)

cacheMap :: MonadCache key val m => m (CacheMap key val)
cacheMap = do
  st <- askAcidState
  liftIO $ query st LookMap
