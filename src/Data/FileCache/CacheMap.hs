{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.CacheMap
  ( ImageCached(..)
  , CacheMap(..)
  ) where

import Control.Monad.Except (throwError)
import Control.Lens ( Identity )
import Control.Lens.Path ( HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(..) )
import Data.FileCache.FileError ( FileError )
import Data.FileCache.ImageKey
    ( HasImageShapeM(..), ImageShape,
      HasImagePath(..),
      ImagePath(ImagePath),
      HasImageKey(..),
      ImageKey(..) )
import Data.FileCache.ImageFile (ImageFile)
import Data.Map ( Map )
import Data.SafeCopy ( base, extension, Migrate(..), SafeCopy(..), safeGet, safePut )
import Data.Serialize ( Serialize(..) )
import Data.Typeable (typeRep)
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
  imagePath (ImageCached key _img) = ImagePath key {-(imageType img)-}

-- * CacheMap


-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
newtype CacheMap_3 =
    CacheMap_3 {_unCacheMap_3 :: Map ImageKey (Either FileError ImageFile)}
    deriving (Generic, Eq, Ord, Serialize)

instance SafeCopy CacheMap_3 where
  version = 3
  kind = base
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

data CacheMap =
  CacheMap
  { _unCacheMap :: Map ImageKey (Either FileError ImageFile)
  , _requested :: Map ImageKey ImageShape
  -- ^ Images that have been requested but not yet generated by the
  -- background image builder.
  } deriving (Generic, Eq, Ord, Serialize, Show)

instance Migrate CacheMap where
  type MigrateFrom CacheMap = CacheMap_3
  migrate (CacheMap_3 mp) = CacheMap mp mempty

instance SafeCopy CacheMap where
  version = 4
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

$(concat <$>
  sequence
  [ pathInstances [FIELDS] =<< [t|CacheMap|]
  ])

instance Value CacheMap where hops _ = [RecType, CtorType]
