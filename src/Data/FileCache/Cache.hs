-- | The cache of files.

{-# LANGUAGE DeriveFunctor, DeriveGeneric, LambdaCase, StandaloneDeriving, TemplateHaskell, TypeFamilies #-}

module Data.FileCache.Cache
  ( CacheMap(..)
  , CacheValue
  , FileCacheTop(..)
  , HasFileCacheTop(fileCacheTop)
  ) where

import Control.Lens (_2, view)
import Control.Monad.Except (ExceptT, lift)
import Control.Monad.Reader (ReaderT)
import Control.Monad.RWS (RWST)
import Data.FileCache.FileError (FileError)
import Data.Map
import Data.SafeCopy
import GHC.Generics

-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
data CacheMap key val =
    CacheMap {_unCacheMap :: Map key (CacheValue val)}
    deriving (Generic, Eq, Ord)

type CacheValue val = Either FileError val

instance (Ord key, SafeCopy' key, SafeCopy' val) => SafeCopy (CacheMap key val) where
  version = 3
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

instance (Ord key, SafeCopy' key, SafeCopy' val) => Migrate (CacheMap key val) where
  type MigrateFrom (CacheMap key val) = CacheMap_2 key val
  migrate (CacheMap_2 mp) =
    CacheMap (fmap (\case Value_1 a -> Right a; Failed_1 e -> Left e; _ -> error "Migrate CacheMap") mp)

data CacheMap_2 key val =
    CacheMap_2 {_unCacheMap_2 :: Map key (CacheValue_1 val)}
    deriving (Generic, Eq, Ord)

instance (Ord key, SafeCopy' key, SafeCopy' val) => SafeCopy (CacheMap_2 key val) where
  version = 2
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap_2"

deriving instance (Show key, Show val) => Show (CacheMap key val)

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap_2 key val) where
    type MigrateFrom (CacheMap_2 key val) = Map key val
    migrate mp = CacheMap_2 (fmap Value_1 mp)

data CacheValue_1 val
    = InProgress_1
    | Value_1 val
    | Failed_1 FileError
    deriving (Generic, Eq, Ord, Functor)

deriving instance Show val => Show (CacheValue_1 val)

$(deriveSafeCopy 1 'base ''CacheValue_1)

newtype FileCacheTop = FileCacheTop {_unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- directory of a file cache.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FileCacheTop

instance (Monad m, Monoid w) => HasFileCacheTop (RWST (acid, FileCacheTop) w s m) where
    fileCacheTop = view _2

instance Monad m => HasFileCacheTop (ReaderT (acid, FileCacheTop) m) where
    fileCacheTop = view _2

instance HasFileCacheTop m => HasFileCacheTop (ExceptT e m) where
    fileCacheTop = lift fileCacheTop
