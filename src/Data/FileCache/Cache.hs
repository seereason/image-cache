-- | The cache of files.

{-# LANGUAGE DeriveFunctor, DeriveGeneric, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}

module Data.FileCache.Cache
  ( CacheMap(..)
  , CacheValue(..)
  ) where

import Data.Map
import Data.SafeCopy
import GHC.Generics

-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
data CacheMap key val err =
    CacheMap {_unCacheMap :: Map key (CacheValue err val)}
    deriving (Generic, Eq, Ord)

#if 0
instance (Ord key, SafeCopy key, SafeCopy val, SafeCopy err) => SafeCopy (CacheMap key val err) where
      putCopy (CacheMap a)
        = contain
            (do safeput <- getSafePut
                safeput a
                return ())
      getCopy
        = contain
            ((label "Data.FileCache.Acid.CacheMap:")
               (do safeget <- getSafeGet @(Map key (CacheValue err val))
                   (return CacheMap <*> safeget)))
      version = 2
      kind = extension
      errorTypeName _ = "Data.FileCache.Types.CacheMap"
#else
instance (Ord key, SafeCopy' key, SafeCopy' val, SafeCopy' err) => SafeCopy (CacheMap key val err) where
  version = 2
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap"
#endif

deriving instance (Show key, Show val, Show err) => Show (CacheMap key val err)

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap key val err) where
    type MigrateFrom (CacheMap key val err) = Map key val
    migrate mp = CacheMap (fmap Value mp)

data CacheValue err val
    = InProgress
    | Value val
    | Failed err
    deriving (Generic, Eq, Ord, Functor)

deriving instance (Show err, Show val) => Show (CacheValue err val)

#if 1
$(deriveSafeCopy 1 'base ''CacheValue)
#else
instance (SafeCopy' err, SafeCopy' val) => SafeCopy (CacheValue err val) where
  version = 1
  errorTypeName _ = "Data.FileCacheCache.CacheValue"
#endif
