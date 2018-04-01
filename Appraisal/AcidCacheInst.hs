{-# OPTIONS -Wall #-}

module Appraisal.AcidCacheInst
    ( runMonadCacheT
    ) where

import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Acid (AcidState)
import Data.Map as Map (Map)

-- | Given the AcidState object for the cache, Run an action in the CacheIO monad.
runMonadCacheT :: ReaderT (AcidState (Map key val)) m a -> AcidState (Map key val) -> m a
runMonadCacheT action st = runReaderT action st
