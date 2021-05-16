{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.CacheMap
  ( ImageCached(..)
  , CacheMap(..)
  ) where

import Control.Lens ( Identity )
import Data.FileCache.FileError ( FileError )
import Data.FileCache.ImageShape ( HasImageShapeM(..) )
import Data.FileCache.ImageType ( HasImageType(imageType) )
import Data.FileCache.ImageKey
    ( HasImagePath(..),
      ImagePath(ImagePath),
      HasImageKey(..),
      ImageKey(..) )
import Data.FileCache.ImageFile
    ( ImageReady(_imageFile),
      ImageFile(ImageFileReady),
      ImageKey_2(..) )
import Data.FileCache.File ( File(_fileChksum) )
import Data.Map ( fromList, Map, toList )
import Data.SafeCopy ( extension, Migrate(..), SafeCopy(..), safeGet, safePut )
import Data.Serialize ( Serialize(..) )
import GHC.Generics ( Generic )

-- * ImageCached

-- | This is the information in one entry of CacheMap
data ImageCached =
  ImageCached { _imageCachedKey :: ImageKey -- the key that produced the ImageFile
              , _imageCachedFile :: ImageFile
              } deriving (Generic, Eq, Ord, Show)

instance Serialize ImageCached where get = safeGet; put = safePut
instance SafeCopy ImageCached

instance HasImageShapeM Identity ImageCached where
  imageShapeM = imageShapeM . _imageCachedFile

instance HasImageKey ImageCached where
  imageKey (ImageCached x _) = imageKey x

instance HasImagePath ImageCached where
  imagePath (ImageCached key img) = ImagePath key {-(imageType img)-}

-- * CacheMap


-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
newtype CacheMap =
    CacheMap {_unCacheMap :: Map ImageKey (Either FileError ImageFile)}
    deriving (Generic, Eq, Ord, Serialize)

instance SafeCopy CacheMap where
  version = 3
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

deriving instance Show CacheMap

-- MIGRATIONS

instance Migrate CacheMap where
  type MigrateFrom CacheMap = CacheMap_2 ImageKey_2 ImageFile
  -- This is delicate - know before you edit!
  migrate (CacheMap_2 mp) =
    CacheMap (fromList $ fmap migratePair $ toList mp)
    where
      migratePair :: (ImageKey_2, CacheValue_1 ImageFile) -> (ImageKey, Either FileError ImageFile)
      migratePair (key, Value_1 img) = (migrateKey img key, Right img)
      migratePair (_, Failed_1 _) = error "unexpected"
      migratePair (_, InProgress_1) = error "unexpected"
      migrateKey :: ImageFile -> ImageKey_2 -> ImageKey
      migrateKey (ImageFileReady img) (ImageOriginal_2 _) =
        ImageOriginal (_fileChksum (_imageFile img)) (imageType img)
      migrateKey (ImageFileReady img) (ImageCropped_2 crop key) = ImageCropped crop (migrateKey (ImageFileReady img) key)
      migrateKey (ImageFileReady img) (ImageScaled_2 sz dpi key) = ImageScaled sz dpi (migrateKey (ImageFileReady img) key)
      migrateKey (ImageFileReady img) (ImageUpright_2 key) = ImageUpright (migrateKey (ImageFileReady img) key)
      migrateKey _ _ = error "Unexpected value during migration"

data CacheMap_2 key val =
    CacheMap_2 {_unCacheMap_2 :: Map key (CacheValue_1 val)}
    deriving (Generic, Eq, Ord)

instance (Ord key, SafeCopy key, SafeCopy val) => SafeCopy (CacheMap_2 key val) where
  version = 2
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap_2"

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap_2 key val) where
    type MigrateFrom (CacheMap_2 key val) = Map key val
    migrate mp = CacheMap_2 (fmap Value_1 mp)

data CacheValue_1 val
    = InProgress_1
    | Value_1 val
    | Failed_1 FileError
    deriving (Generic, Eq, Ord, Functor)

deriving instance Show val => Show (CacheValue_1 val)

instance SafeCopy val => SafeCopy (CacheValue_1 val) where version = 1
