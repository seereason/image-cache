{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.Common
  ( module Data.FileCache.Happstack
  , module Data.FileCache.Rational
  , module Data.FileCache.ImageSize
  , module Data.FileCache.ImageType
  , module Data.FileCache.ImageCrop
  , module Data.FileCache.File
  , module Data.FileCache.ImageKey
  , module Data.FileCache.ImageShape
  , module Data.FileCache.ImageFile
  , module Data.FileCache.FileError
  , module Data.FileCache.CacheMap
  ) where

import Control.Lens.Path ( HOP(FIELDS), HopType(CtorType, RecType, ViewType), Value(..) )
import Control.Lens.Path.TH ( makePathInstances' )
import Data.FileCache.CommandError ( CommandError )
import Data.FileCache.Rational
import Data.FileCache.FileError
import Data.FileCache.ImageShape
import Data.FileCache.ImageType
import Data.FileCache.ImageCrop
import Data.FileCache.ImageKey
import Data.FileCache.ImageFile
import Data.FileCache.File
import Data.FileCache.CacheMap
import Data.FileCache.ImageSize
import Data.FileCache.Happstack
import GHC.Generics
-- import Language.Haskell.TH.Instances ()
import Prelude ( Traversable(sequence), concat, (<$>) )

instance Value File where hops _ = [RecType, CtorType]
instance Value FileError where hops _ = []
instance Value FileSource where hops _ = [RecType, CtorType]
instance Value ImageFile where hops _ = [RecType, CtorType]
instance Value ImageReady where hops _ = [RecType, CtorType]
instance Value ImageShape where hops _ = [RecType, CtorType]
instance Value ImageType where hops _ = []
instance Value ImageSize where hops _ = [RecType, CtorType]
instance Value Dimension where hops _ = []
instance Value ImageCrop where hops _ = [RecType, CtorType]
instance Value ImageKey where hops _ = [RecType, CtorType]
instance Value Units where hops _ = []
instance Value Rotation where hops _ = []
instance Value CacheMap where hops _ = [RecType, CtorType]
instance Value ContentType where hops _ = [RecType, CtorType]
instance Value (SaneSize ImageSize) where hops _ = [ViewType]

$(concat <$>
  sequence
  [ makePathInstances' [FIELDS] ''File
  , makePathInstances' [FIELDS] ''FileSource
  , makePathInstances' [FIELDS] ''ImageFile
  , makePathInstances' [FIELDS] ''ImageReady
  , makePathInstances' [FIELDS] ''ImageShape
  , makePathInstances' [FIELDS] ''ImageSize
  , makePathInstances' [FIELDS] ''ImageCrop
  , makePathInstances' [FIELDS] ''ImageKey
  , makePathInstances' [FIELDS] ''CacheMap
  , makePathInstances' [FIELDS] ''ContentType
  ])
