{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageSize
  (
    -- * ImageSize
    ImageSize(..) -- , dim, size, units
  , HasImageSize(imageSize)
  , Dimension(..)
  , Units(..)
  , saneSize
  , SaneSize(..) -- , unSaneSize
  , defaultSize
  , inches
  ) where

import Control.Lens.Path ( HOP(FIELDS), HopType(CtorType, RecType, ViewType), pathInstances, Value(..) )
import Control.Monad.Except (throwError)
import Control.Lens ( iso, _Show )
import Control.Lens.Path ( Value(hops), View(..), viewIso )
import Data.Data ( Data )
import Data.FileCache.Rational ((%), showRational)
import Data.Monoid ( (<>) )
import Data.SafeCopy ( safeGet, safePut, SafeCopy(version) )
import Data.Serialize ( Serialize(..) )
import Data.Text ( Text, pack, unpack )
import Data.Typeable ( Typeable, typeRep )
import GHC.Generics ( Generic )
-- import Language.Haskell.TH.Instances ()
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Web.Routes.TH ( derivePathInfo )

-- * ImageSize, Dimension, Units, SaneSize

data ImageSize
    = ImageSize
      { _dim :: Dimension
      , _size :: Rational
      , _units :: Units
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance SafeCopy ImageSize where version = 2
instance Serialize ImageSize where get = safeGet; put = safePut
instance Value ImageSize where hops _ = [RecType, CtorType]

-- > pPrint (ImageSize TheWidth 9 Inches)
-- 9.0in wide
instance Pretty ImageSize where
  pPrint (ImageSize dim size units) =
    text (showRational size) <> pPrint units <> text " " <> pPrint dim

class HasImageSize a where imageSize :: a -> ImageSize
instance HasImageSize ImageSize where imageSize = id

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

-- * Dimension

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

instance Value Dimension where hops _ = []
-- * Units

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

instance Value Units where hops _ = []

-- * SaneSize

-- | A wrapper type to suggest that lens_saneSize has been applied to
-- the ImageSize within.
newtype SaneSize a = SaneSize {_unSaneSize :: a} deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance (SafeCopy a, Typeable a) => SafeCopy (SaneSize a) where version = 1
instance (SafeCopy a, Typeable a) => Serialize (SaneSize a) where get = safeGet; put = safePut

instance View (SaneSize ImageSize) where
    type ViewType (SaneSize ImageSize) = ImageSize
    _View = iso _unSaneSize saneSize

saneSize :: ImageSize -> SaneSize ImageSize
saneSize sz = SaneSize $
    case (_dim sz, inches sz) of
      (TheArea, n) | n < minArea -> sz {_units = Inches, _size = minArea}
      (TheArea, n) | n > maxArea -> sz {_units = Inches, _size = maxArea}
      (_, n) | n < minDist -> sz {_units = Inches, _size = minDist}
      (_, n) | n > maxDist -> sz {_units = Inches, _size = maxDist}
      _ -> sz
    where
      -- inches and square inches
      minDist = 25 % 100
      maxDist = 25
      minArea = 625 % 10000
      maxArea = 625

instance Value (SaneSize ImageSize) where hops _ = [ViewType]

#if MIN_VERSION_template_haskell(2,17,0)
instance PathInfo ImageSize where
      toPathSegments inp_aAxV
        = case inp_aAxV of {
            ImageSize arg_aAxW arg_aAxX arg_aAxY
              -> ((++) [pack "image-size"])
                   (((++) (toPathSegments arg_aAxW))
                      (((++) (toPathSegments arg_aAxX)) (toPathSegments arg_aAxY))) }
      fromPathSegments
        = (ap
             ((ap
                 ((ap
                     (segment (pack "image-size")
                        >> return ImageSize))
                    fromPathSegments))
                fromPathSegments))
            fromPathSegments
instance PathInfo Dimension where
      toPathSegments inp_aAy2
        = case inp_aAy2 of
            TheHeight -> [pack "the-height"]
            TheWidth -> [pack "the-width"]
            TheArea -> [pack "the-area"]
      fromPathSegments
        = ((<|>)
             (((<|>)
                 (segment (pack "the-height")
                    >> return TheHeight))
                (segment (pack "the-width")
                   >> return TheWidth)))
            (segment (pack "the-area")
               >> return TheArea)
instance PathInfo Units where
      toPathSegments inp_aAy6
        = case inp_aAy6 of
            Inches -> [pack "inches"]
            Cm -> [pack "cm"]
            Points -> [pack "points"]
      fromPathSegments
        = ((<|>)
             (((<|>)
                 (segment (pack "inches")
                    >> return Inches))
                (segment (pack "cm") >> return Cm)))
            (segment (pack "points") >> return Points)
#else
$(concat <$>
  sequence
  [ derivePathInfo ''ImageSize
  , derivePathInfo ''Dimension
  , derivePathInfo ''Units
  , pathInstances [FIELDS] =<< [t|ImageSize|]
  ])
#endif
