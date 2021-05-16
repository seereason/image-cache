{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageSize
  (
    -- * ImageSize
    HasImageSize(imageSize)
  , ImageSize(..) -- , dim, size, units
  , Dimension(..)
  , Units(..)
  , saneSize
  , SaneSize(..) -- , unSaneSize
  , defaultSize
  , inches
  ) where

import Control.Lens ( iso, _Show )
import Control.Lens.Path ( newtypeIso, View(..) )
import Control.Lens.Path.View ( viewIso )
import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.FileCache.Rational ( showRational )
import Data.Monoid ( (<>) )
import Data.Ratio ( (%) )
import Data.SafeCopy ( safeGet, safePut, SafeCopy(version) )
import Data.Serialize ( Serialize(..) )
import Data.Text ( Text, pack, unpack )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
-- import Language.Haskell.TH.Instances ()
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- * ImageSize, Dimension, Units, SaneSize

data ImageSize
    = ImageSize
      { _dim :: Dimension
      , _size :: Rational
      , _units :: Units
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Default ImageSize where
    def = ImageSize TheArea 15.0 Inches
instance SafeCopy ImageSize where version = 2
instance Serialize ImageSize where get = safeGet; put = safePut

-- > pPrint (ImageSize TheWidth 9 Inches)
-- 9.0in wide
instance Pretty ImageSize where
  pPrint (ImageSize dim size units) =
    text (showRational size) <> pPrint units <> text " " <> pPrint dim

data Dimension
    = TheHeight
    | TheWidth
    | TheArea
    deriving (Generic, Eq, Ord, Enum, Bounded, Data, Typeable, Read, Show)

instance View Dimension where type ViewType Dimension = Text; _View = viewIso _Show TheHeight . iso pack unpack
instance SafeCopy Dimension where version = 1
instance Serialize Dimension where get = safeGet; put = safePut

instance Pretty Dimension where
    pPrint TheHeight = text "h"
    pPrint TheWidth = text "w"
    pPrint TheArea = text "sq"

data Units
    = Inches
    | Cm
    | Points
    deriving (Generic, Eq, Ord, Enum, Bounded, Data, Typeable, Read, Show)

instance View Units where type ViewType Units = Text; _View = viewIso _Show Inches . iso pack unpack
instance SafeCopy Units where version = 0
instance Serialize Units where get = safeGet; put = safePut

instance Pretty Units where
    pPrint Inches = text "in"
    pPrint Cm = text "cm"
    pPrint Points = text "pt"

class HasImageSize a where imageSize :: a -> ImageSize
instance HasImageSize ImageSize where imageSize = id

-- | A wrapper type to suggest that lens_saneSize has been applied to
-- the ImageSize within.
newtype SaneSize a = SaneSize {_unSaneSize :: a} deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance (SafeCopy a, Typeable a) => SafeCopy (SaneSize a) where version = 1
instance (SafeCopy a, Typeable a) => Serialize (SaneSize a) where get = safeGet; put = safePut

instance View (SaneSize ImageSize) where
    type ViewType (SaneSize ImageSize) = ImageSize
    _View = newtypeIso

saneSize :: ImageSize -> SaneSize ImageSize
saneSize sz = SaneSize $
    case (_dim sz, inches sz) of
      (TheArea, n) | n < minArea -> sz {_units = Inches, _size = minArea}
      (TheArea, n) | n > maxArea -> sz {_units = Inches, _size = maxArea}
      (_, n) | n < minDist -> sz {_units = Inches, _size = toRational minDist}
      (_, n) | n > maxDist -> sz {_units = Inches, _size = maxDist}
      _ -> sz
    where
      -- inches and square inches
      minDist = 25 % 100
      maxDist = 25
      minArea = 625 % 10000
      maxArea = 625

-- Surely, SaneSize should be a class so we could apply it to things
-- other than ImageSize.  But for the moment it is what it is.
instance Default (SaneSize ImageSize) where
    def = saneSize def

defaultSize :: ImageSize
defaultSize = ImageSize {_dim = TheArea, _units = Inches, _size = 6.0}

-- | Return the value of size in inches
inches :: ImageSize -> Rational
inches sz =
    _size sz / case (_dim sz, _units sz) of
                (_, Inches) -> 1
                (TheArea, Cm) -> (254 % 100) * (254 % 100)
                (TheArea, Points) -> (7227 % 100) * (7227 % 100)
                (_, Cm) -> 254 % 100
                (_, Points) -> 7227 % 100
