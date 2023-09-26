{-# LANGUAGE RecordWildCards #-}

module Data.FileCache.ImageRect
  ( ImageRect
  , makeImageRect
  , HasImageRect(imageRect)
  , widthInInches
  , widthInInches'
  , heightInInches
  , scaleImageRect
  , scaleFromDPI
  , cropImageRect
  , rotateImageRect
  , uprightImageRect
  ) where

import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.FileCache.ImageCrop ( Rotation(..), ImageCrop(..) )
-- import Data.FileCache.ImageKey ( OriginalKey(..), ImageKey(..) )
import Data.FileCache.ImageSize ( Units(..), Dimension(TheArea, TheWidth, TheHeight), ImageSize(_units, _size, _dim), inches )
import Data.FileCache.Rational ((%), approx, rsqrt)
import Data.Generics.Labels ()
import Data.Monoid ( (<>) )
import Data.SafeCopy (base, safeGet, safePut, SafeCopy(version, kind) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

class HasImageRect a where imageRect :: a -> Maybe ImageRect

data ImageRect
  = ImageRect
    { _imageShapeWidth :: Int
    , _imageShapeHeight :: Int
    , _imageFileOrientation :: Rotation
    } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Serialize ImageRect where get = safeGet; put = safePut
instance SafeCopy ImageRect where version = 1; kind = base

instance Pretty ImageRect where
  pPrint (ImageRect w h rot) =
    text "ImageRect (" <>
    text (show w) <> text "x" <> text (show h) <> text " " <>
    (case rot of
       ZeroHr -> text " UL"
       ThreeHr -> text " UR"
       SixHr -> text " LR"
       NineHr -> text " LL") <>
    text ")"

makeImageRect :: Int -> Int -> Rotation -> ImageRect
makeImageRect w h rot =
  ImageRect {_imageShapeWidth = w, _imageShapeHeight = h, _imageFileOrientation = rot}

widthInInches :: ImageRect -> ImageSize -> Rational
widthInInches rect@(ImageRect {_imageShapeHeight = h, _imageShapeWidth = w}) s =
    case _dim s of
      TheWidth -> toInches (_units s) (_size s)
      TheHeight -> widthInInches rect (s {_dim = TheWidth, _size = approx (_size s / aspect)})
      TheArea -> widthInInches rect (s {_dim = TheWidth, _size = approx (rsqrt (_size s / aspect))})
    where
      aspect :: Rational
      aspect = fromIntegral h % fromIntegral w
      toInches :: Units -> Rational -> Rational
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

heightInInches :: ImageRect -> ImageSize -> Rational
heightInInches rect@(ImageRect {_imageShapeHeight = h, _imageShapeWidth = w}) s =
    case _dim s of
      TheHeight -> toInches (_units s) (_size s)
      TheWidth -> heightInInches rect (s {_dim = TheHeight, _size = approx (_size s / aspect)})
      TheArea -> heightInInches rect (s {_dim = TheHeight, _size = approx (rsqrt (_size s / aspect))})
    where
      aspect :: Rational
      aspect = fromIntegral h % fromIntegral w
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

-- |Modify an ImageSize so that the dimension is width and the units
-- are inches.  This way we can figure out how many images fit across
-- the page.
widthInInches' :: ImageRect -> ImageSize -> ImageSize
widthInInches' p s =
    s {_units = Inches, _size = approx (widthInInches p s), _dim = TheWidth}

rotateImageRect :: Rotation -> ImageRect -> ImageRect
rotateImageRect ZeroHr rect = rect
rotateImageRect SixHr rect = rect
rotateImageRect ThreeHr rect =
  rect {_imageShapeWidth = _imageShapeHeight rect,
        _imageShapeHeight = _imageShapeWidth rect}
rotateImageRect NineHr rect = rotateImageRect ThreeHr rect

scaleImageRect :: ImageSize -> Rational -> ImageRect -> ImageRect
scaleImageRect sz dpi rect =
  if scale == 1
  then rect
  else rect { _imageShapeWidth = round (fromIntegral (_imageShapeWidth rect) * scale)
            , _imageShapeHeight = round (fromIntegral (_imageShapeHeight rect) * scale) }
  where
    scale :: Rational
    scale = maybe 1 approx $ scaleFromDPI sz dpi rect

-- |Given the desired DPI and image dimensions, return the factor by
-- which an image should be scaled.  Result of Nothing means the scale
-- is pathological.
scaleFromDPI :: ImageSize -> Rational -> ImageRect -> Maybe Rational
scaleFromDPI sz dpi (ImageRect {_imageShapeHeight = h, _imageShapeWidth = w}) =
  case _dim sz of
    _ | _size sz < 0.000001 || _size sz > 1000000.0 -> Nothing
    TheHeight -> Just $ inches sz * dpi / fromIntegral h
    TheWidth -> Just $ inches sz * dpi / fromIntegral w
    -- If we want an area of 9 square inches, and the dpi is 100, and the image
    -- size is 640x480 pixels, the scale is (9 * 100 * 100) / (640 * 480)
    TheArea -> Just (rsqrt (inches sz * dpi * dpi / (fromIntegral w * fromIntegral h)))

cropImageRect :: ImageCrop -> ImageRect -> ImageRect
cropImageRect crop rect | crop == def = rect
cropImageRect (ImageCrop{..}) rect =
  rect {_imageShapeWidth = _imageShapeWidth rect - (leftCrop + rightCrop),
        _imageShapeHeight = _imageShapeHeight rect - (topCrop + bottomCrop) }

-- | This seems to have evolved into a no-op.
uprightImageRect :: ImageRect -> ImageRect
uprightImageRect rect =
  case _imageFileOrientation rect of
    ZeroHr -> rect
    SixHr -> rect
    ThreeHr -> rect
    NineHr -> rect
