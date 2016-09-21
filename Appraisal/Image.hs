{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, TypeFamilies #-}
module Appraisal.Image 
    ( ImageSize(..)
    , ImageCrop(..)
    , Dimension(..)
    , Units(..)
    , PixmapShape(..)
    , scaleFromDPI
    , widthInInches
    , widthInInches'
    , heightInInches
    , lens_saneSize
    , defaultSize
    , latexSize
    , latexWidth
    ) where

import Control.Lens (Iso', iso)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (defaultOptions)
import Data.Generics(Data, Typeable)
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base)
import qualified Text.LaTeX.Base.Syntax as LaTeX (Measure(In, Cm, Pt))
import Text.LaTeX.Packages.Graphicx (IGOption(IGWidth))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

-- |This can describe an image size in various ways.
data ImageSize
    = ImageSize
      { dim :: Dimension
      , size :: Double
      , units :: Units
      } deriving (Show, Read, Eq, Ord, Typeable, Data)

data Dimension
    = TheHeight
    | TheWidth
    | TheArea
    | Invalid String
    deriving (Show, Read, Eq, Ord, Typeable, Data)

data Units
    = Inches
    | Cm
    | Points
    deriving (Show, Read, Eq, Ord, Typeable, Data)

-- |This describes the cropping and rotation of an image.
data ImageCrop
    = ImageCrop
      { topCrop :: Int
      , bottomCrop :: Int
      , leftCrop :: Int
      , rightCrop :: Int
      , rotation :: Int         -- 0, 90, 180, 270
      } deriving (Show, Read, Eq, Ord, Typeable, Data)

-- | Access to the original dimensions of the image, so
-- we can compute the aspect ratio.
class PixmapShape a where
    pixmapHeight :: a -> Int
    pixmapWidth :: a -> Int
    pixmapMaxVal :: a -> Int

-- |Given the desired DPI and image dimensions, return the factor by
-- which an image should be scaled.
scaleFromDPI :: PixmapShape a => Double -> ImageSize -> a -> Maybe Double
scaleFromDPI dpi sz file =
    case dim sz of
      _ | size sz < 0.000001 || size sz > 1000000.0 -> Nothing
      TheHeight -> Just $ inches sz * dpi / h
      TheWidth -> Just $ inches sz * dpi / w
      -- If we want an area of 9 square inches, and the dpi is 100, and the image
      -- size is 640x480 pixels, the scale is (9 * 100 * 100) / (640 * 480)
      TheArea -> Just (sqrt (inches sz * dpi * dpi / (w * h)))
      _ -> fail "Invalid dimension"
    where
      w = fromInteger (toInteger (pixmapWidth file)) :: Double
      h = fromInteger (toInteger (pixmapHeight file)) :: Double

widthInInches :: PixmapShape a => a -> ImageSize -> Double
widthInInches p s =
    case dim s of
      TheWidth -> toInches (units s) (size s)
      TheHeight -> widthInInches p (s {dim = TheWidth, size = size s / r})
      TheArea -> widthInInches p (s {dim = TheWidth, size = sqrt (size s / r)})
      _ -> error "Invalid dimension"
    where
      r = h / w
      w = (fromInteger . toInteger . pixmapWidth $ p) :: Double
      h = (fromInteger . toInteger . pixmapHeight  $ p) :: Double
      toInches Inches x = x
      toInches Cm x = x / 2.54
      toInches Points x = x / 72.27

heightInInches :: PixmapShape a => a -> ImageSize -> Double
heightInInches p s =
    case dim s of
      TheHeight -> toInches (units s) (size s)
      TheWidth -> heightInInches p (s {dim = TheHeight, size = size s / r})
      TheArea -> heightInInches p (s {dim = TheHeight, size = sqrt (size s / r)})
      _ -> error "Invalid dimension"
    where
      r = w / h
      w = (fromInteger . toInteger . pixmapWidth $ p) :: Double
      h = (fromInteger . toInteger . pixmapHeight  $ p) :: Double
      toInches Inches x = x
      toInches Cm x = x / 2.54
      toInches Points x = x / 72.27

-- |Modify an ImageSize so that the dimension is width and the units
-- are inches.  This way we can figure out how many images fit across
-- the page.
widthInInches' :: PixmapShape a => a -> ImageSize -> ImageSize
widthInInches' p s = s {units = Inches, size = widthInInches p s, dim = TheWidth}

saneSize :: ImageSize -> ImageSize
saneSize sz =
    case (dim sz, inches sz) of
      (TheArea, n) | n < 0.0625 -> sz {units = Inches, size = 0.625}
      (TheArea, n) | n > 625.0 -> sz {units = Inches, size = 625.0}
      (_, n) | n < 0.25 -> sz {units = Inches, size = 0.25}
      (_, n) | n > 25.0 -> sz {units = Inches, size = 25.0}
      _ -> sz

lens_saneSize :: Iso' ImageSize ImageSize
lens_saneSize = iso id saneSize

defaultSize :: ImageSize
defaultSize = ImageSize {dim = TheArea, units = Inches, size = 6.0}

-- | Return the value of size in inches
inches :: ImageSize -> Double
inches sz =
    size sz / case (dim sz, units sz) of
                (_, Inches) -> 1.0
                (TheArea, Cm) -> (2.54 * 2.54)
                (TheArea, Points) -> (72.27 * 72.27)
                (_, Cm) -> 2.54
                (_, Points) -> 72.27

-- | Return a LaTeX formatted size string for an image, e.g. width=3.0in
latexSize :: PixmapShape a => a -> ImageSize -> IGOption
latexSize p sz = IGWidth (latexWidth p sz)

-- | Return a LaTeX formatted size string for an image, e.g. width=3.0in
latexWidth :: PixmapShape a => a -> ImageSize -> LaTeX.Measure
latexWidth p sz =
    case dim sz of
      TheWidth -> unitsToMeasureCon (units sz) (size sz)
      _ -> latexWidth p (sz {dim = TheWidth, size = widthInInches p sz, units = Inches})
    where
      unitsToMeasureCon Inches = LaTeX.In
      unitsToMeasureCon Cm = LaTeX.Cm
      unitsToMeasureCon Points = LaTeX.Pt

instance Pretty Dimension where
    pPrint TheHeight = text "h"
    pPrint TheWidth = text "w"
    pPrint TheArea = text "a"
    pPrint x = text (show x)

instance Pretty Units where
    pPrint Inches = text "in"
    pPrint Cm = text "cm"
    pPrint Points = text "pt"

instance Pretty ImageSize where
    pPrint (ImageSize d sz u) = pPrint d <> text ("=" <> show sz <> " ") <> pPrint u

instance Pretty ImageCrop where
    pPrint (ImageCrop t b l r _) = text $ "crop (" <> show (b, l) <> " -> " <> show (t, r) <> ")"

$(deriveJSON defaultOptions ''Units)
$(deriveJSON defaultOptions ''Dimension)
$(deriveJSON defaultOptions ''ImageCrop)
$(deriveJSON defaultOptions ''ImageSize)

$(deriveSafeCopy 1 'base ''ImageSize)
$(deriveSafeCopy 0 'base ''Dimension)
$(deriveSafeCopy 0 'base ''Units)
$(deriveSafeCopy 0 'base ''ImageCrop)
