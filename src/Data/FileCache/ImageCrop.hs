{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageCrop
  ( -- * ImageCrop
    ImageCrop(..)
  , Rotation(..)
  ) where

import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( extension, safeGet, safePut, Migrate(..), SafeCopy(kind, version) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Prelude ( (++), ($), Eq, Ord, Read, Show(show), Int )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- * ImageCrop

-- |This describes the cropping and rotation of an image.
data ImageCrop
    = ImageCrop
      { topCrop :: Int
      , bottomCrop :: Int
      , leftCrop :: Int
      , rightCrop :: Int
      , rotation :: Rotation
      } deriving (Generic, Eq, Ord)

data Rotation = ZeroHr | ThreeHr | SixHr | NineHr deriving (Generic, Eq, Ord, Show, Read, Data, Typeable)

instance Default ImageCrop where def = ImageCrop 0 0 0 0 ZeroHr
instance Serialize ImageCrop where get = safeGet; put = safePut
instance SafeCopy ImageCrop where kind = extension; version = 1
deriving instance Data ImageCrop
deriving instance Read ImageCrop
deriving instance Show ImageCrop
deriving instance Typeable ImageCrop
instance Default Rotation where def = ZeroHr
instance SafeCopy Rotation where version = 0
instance Serialize Rotation where get = safeGet; put = safePut

instance Pretty ImageCrop where
    pPrint (ImageCrop 0 0 0 0 ZeroHr) = text "(no crop)"
    pPrint (ImageCrop t b l r ZeroHr) = text $ "(crop " <> show (b, l) <> " -> " <> show (t, r) <> ")"
    pPrint (ImageCrop t b l r rot) = text $ "(crop " <> show (b, l) <> " -> " <> show (t, r) <> ", rot " ++ show rot ++ ")"

-- MIGRATIONS

data ImageCrop_0
    = ImageCrop_0
      { topCrop_0 :: Int
      , bottomCrop_0 :: Int
      , leftCrop_0 :: Int
      , rightCrop_0 :: Int
      , rotation_0 :: Int         -- 0, 90, 180, 270
      } deriving (Generic, Eq, Ord)

instance Migrate ImageCrop where
  type MigrateFrom ImageCrop = ImageCrop_0
  migrate (ImageCrop_0 t b l r rot) =
    ImageCrop t b l r (case rot of
                         90 -> ThreeHr
                         180 -> SixHr
                         270 -> NineHr
                         _ -> ZeroHr)
instance SafeCopy ImageCrop_0 where version = 0
