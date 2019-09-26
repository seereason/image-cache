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

module FileCache.Types
    ( -- * Open cache
      CacheMap(..)
    , CacheValue(..){-, _InProgress, _Cached, _Failed-}
    ) where

import Data.Generics (Data)
import Data.Map.Strict as Map (Map)
import Data.Serialize (label)
import Data.SafeCopy -- (deriveSafeCopy, extension, Migrate(..), SafeCopy)
import GHC.Generics (Generic)

data CacheValue err val
    = InProgress
    | Cached val
    | Failed err
    deriving (Generic, Eq, Ord, Functor)

-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
data CacheMap key val err =
    CacheMap {_unCacheMap :: Map key (CacheValue err val)}
    deriving (Generic, Eq, Ord)

#if 1
$(deriveSafeCopy 1 'base ''CacheValue)
instance (Ord key, SafeCopy key, SafeCopy val, SafeCopy err) => SafeCopy (CacheMap key val err) where
      putCopy (CacheMap a)
        = contain
            (do safeput <- getSafePut
                safeput a
                return ())
      getCopy
        = contain
            ((label "Appraisal.AcidCache.CacheMap:")
               (do safeget <- getSafeGet @(Map key (CacheValue err val))
                   (return CacheMap <*> safeget)))
      version = 2
      kind = extension
      errorTypeName _ = "Appraisal.AcidCache.CacheMap"
#else
instance (SafeCopy err, SafeCopy val) => SafeCopy (CacheValue err val) where version = 1
instance (Ord key, SafeCopy key, SafeCopy val, SafeCopy err) => SafeCopy (CacheMap key val err) where
  version = 2
  kind = extension
#endif

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap key val err) where
    type MigrateFrom (CacheMap key val err) = Map key val
    migrate mp = CacheMap (fmap Cached mp)

{-
$(concat <$>
  sequence
  [ makePrisms ''CacheValue
  , makeLenses ''CacheMap
  ])
-}

deriving instance (Data err, Data val) => Data (CacheValue err val)
deriving instance (Ord key, Data key, Data val, Data err) => Data (CacheMap key val err)
deriving instance (Show err, Show val) => Show (CacheValue err val)
deriving instance (Show key, Show val, Show err) => Show (CacheMap key val err)
