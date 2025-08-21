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

import Control.Lens (Identity)
import Control.Monad.Except (throwError)
import Control.Lens.Path ( HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(..) )
import Data.FileCache.FileError ( FileError )
import Data.FileCache.ImageKey
  (HasImageShapeM(..), ImageShape, HasImagePath(..),
   ImagePath(ImagePath), HasImageKey(..), ImageKey(..))
import Data.FileCache.ImageFile (ImageFile)
import Data.Map as Map (Map)
import Data.SafeCopy ( base, extension, Migrate(..), SafeCopy(..), safeGet, safePut )
import Data.Serialize ( Serialize(..) )
import Data.Typeable (typeRep)
import GHC.Generics ( Generic )
import GHC.Stack (callStack)

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

-- | An 'ImageKey' can be recovered from an entry in the image cache.
instance HasImageKey ImageCached where
  imageKey (ImageCached x _) = imageKey x

instance HasImagePath ImageCached where
  imagePath (ImageCached key _img) = ImagePath key {-(imageType img)-}

-- * CacheMap

data CacheMap =
  CacheMap
  { _unCacheMap :: Map ImageKey (Either FileError ImageFile)
  , _requested :: Map ImageKey ImageShape
  } deriving (Generic, Eq, Ord, Serialize, Show)

instance SafeCopy CacheMap where
  version = 4
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

instance Migrate CacheMap where
  type MigrateFrom CacheMap = CacheMap_3
  migrate (CacheMap_3 mp) = CacheMap mp mempty

newtype CacheMap_3 =
    CacheMap_3 {_unCacheMap_3 :: Map ImageKey (Either FileError ImageFile)}
    deriving (Generic, Eq, Ord, Serialize)

instance SafeCopy CacheMap_3 where
  version = 3
  kind = base
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

$(concat <$>
  sequence
  [ pathInstances [FIELDS] =<< [t|CacheMap|]
  ])

instance Value CacheMap where hops _ = [RecType, CtorType]
