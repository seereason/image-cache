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
import Data.FileCache.ImageKey
    ( HasImagePath(..),
      ImagePath(ImagePath),
      HasImageKey(..),
      ImageKey(..) )
import Data.FileCache.ImageFile (ImageFile)
import Data.Map ( Map )
import Data.SafeCopy ( base, SafeCopy(..), safeGet, safePut )
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
  kind = base
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

deriving instance Show CacheMap
