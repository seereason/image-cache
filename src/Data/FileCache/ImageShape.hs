-- | Size, orientation, and format of an image.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageShape
  (
    -- * ImageShape, HasImageShape
    ImageShape(..)
  , HasImageShapeM(imageShapeM)
  , HasImageShape, imageShape
  , HasOriginalShape(originalShape)
  , scaleFromDPI
  , widthInInches
  , widthInInches'
  , heightInInches
  , scaleImageShape
  , cropImageShape
  , rotateImageShape
  , uprightImageShape
  , shapeFromKey
  , ImageStats(..)
  ) where

import Control.Lens ( Identity(runIdentity) )
import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.FileCache.Rational ( approx, rsqrt )
import Data.FileCache.File ( File )
import Data.FileCache.ImageCrop ( Rotation(..), ImageCrop(..) )
import Data.FileCache.ImageKey ( OriginalKey(..), ImageKey(..) )
import Data.FileCache.ImageSize ( Units(..), Dimension(TheArea, TheWidth, TheHeight), ImageSize(_units, _size, _dim), inches )
import Data.FileCache.ImageType ( HasImageType(..), ImageType(JPEG) )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Ratio ( (%) )
import Data.SafeCopy ( base, safeGet, safePut, Migrate(..), SafeCopy(version, kind) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Numeric ( fromRat )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- * ImageShape

class HasOriginalShape a where
  originalShape :: a -> ImageShape

instance HasImageShapeM Identity ImageShape where
  imageShapeM s = pure s

data ImageShape
  = ImageShape
      { _imageShapeType :: ImageType
      , _imageShapeWidth :: Int
      , _imageShapeHeight :: Int
      , _imageFileOrientation :: Rotation
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Serialize ImageShape where get = safeGet; put = safePut
instance SafeCopy ImageShape where version = 1; kind = base

instance Pretty ImageShape where
  pPrint (ImageShape typ w h rot) =
    (text "ImageShape (" <> text (show w) <> text "x" <> text (show h) <> text " " <> pPrint typ <>
     (case rot of
        ZeroHr -> text " UL"
        ThreeHr -> text " UR"
        SixHr -> text " LR"
        NineHr -> text " LL") <> text ")")

instance HasImageType ImageShape where imageType = _imageShapeType

-- | We can infer the file type from the key using insider info on how
-- the various IO operations work.  E.g. all the cropping operations
-- other than the identity crop result in a JPEG.
instance HasImageShape a => HasImageType (ImageKey, a) where
  imageType (ImageOriginal _ typ, _) = typ
  imageType (ImageUpright key, shape) = imageType (key, shape)
  imageType (ImageCropped crop key, shape) =
    if crop == def then imageType (key, shape) else JPEG
  imageType (ImageScaled sz dpi key, shape) =
    let sc' = scaleFromDPI sz dpi shape
        sc :: Double
        sc = fromRat (fromMaybe 1 sc') in
      if approx (toRational sc) == 1 then imageType (key, shape) else JPEG

scaleImageShape :: ImageSize -> Rational -> ImageShape -> ImageShape
scaleImageShape sz dpi shape =
  if scale == 1
  then shape
  else shape { _imageShapeType = JPEG -- the scaling pipeline results in a jpeg file
             , _imageShapeWidth = round (fromIntegral (_imageShapeWidth shape) * scale)
             , _imageShapeHeight = round (fromIntegral (_imageShapeHeight shape) * scale) }
  where
    scale :: Rational
    scale = maybe 1 approx $ scaleFromDPI sz dpi shape

instance OriginalKey (File, ImageShape) where
  originalKey (f, shape) = originalKey (f, _imageShapeType shape)

-- * HasImageShape

-- | A class whose primary (only?) instance is ImageFile.  Access to
-- the original dimensions of the image, so we can compute the aspect
-- ratio.
class HasImageShapeM m a where
  imageShapeM :: a -> m ImageShape

type HasImageShape a = HasImageShapeM Identity a
imageShape :: HasImageShape a => a -> ImageShape
imageShape = runIdentity . imageShapeM

-- |Given the desired DPI and image dimensions, return the factor by
-- which an image should be scaled.  Result of Nothing means the scale
-- is pathological.
scaleFromDPI :: HasImageShape a => ImageSize -> Rational -> a -> Maybe Rational
scaleFromDPI sz dpi file =
    case _dim sz of
      _ | _size sz < 0.000001 || _size sz > 1000000.0 -> Nothing
      TheHeight -> Just $ inches sz * dpi / fromIntegral h
      TheWidth -> Just $ inches sz * dpi / fromIntegral w
      -- If we want an area of 9 square inches, and the dpi is 100, and the image
      -- size is 640x480 pixels, the scale is (9 * 100 * 100) / (640 * 480)
      TheArea -> Just (rsqrt (inches sz * dpi * dpi / (fromIntegral w * fromIntegral h)))
    where
      ImageShape {_imageShapeHeight = h, _imageShapeWidth = w} = imageShape file

widthInInches :: HasImageShape a => a -> ImageSize -> Rational
widthInInches p s =
    case _dim s of
      TheWidth -> toInches (_units s) (_size s)
      TheHeight -> widthInInches p (s {_dim = TheWidth, _size = approx (_size s / aspect)})
      TheArea -> widthInInches p (s {_dim = TheWidth, _size = approx (rsqrt (_size s / aspect))})
    where
      ImageShape {_imageShapeWidth = w, _imageShapeHeight = h} = imageShape p
      aspect :: Rational
      aspect = fromIntegral h % fromIntegral w
      toInches :: Units -> Rational -> Rational
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

heightInInches :: HasImageShape a => a -> ImageSize -> Rational
heightInInches p s =
    case _dim s of
      TheHeight -> toInches (_units s) (_size s)
      TheWidth -> heightInInches p (s {_dim = TheHeight, _size = approx (_size s / aspect)})
      TheArea -> heightInInches p (s {_dim = TheHeight, _size = approx (rsqrt (_size s / aspect))})
    where
      ImageShape {_imageShapeWidth = w, _imageShapeHeight = h} = imageShape p
      aspect :: Rational
      aspect = fromIntegral h % fromIntegral w
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

-- |Modify an ImageSize so that the dimension is width and the units
-- are inches.  This way we can figure out how many images fit across
-- the page.
widthInInches' :: HasImageShape a => a -> ImageSize -> ImageSize
widthInInches' p s = s {_units = Inches, _size = approx (widthInInches p s), _dim = TheWidth}

rotateImageShape :: Rotation -> ImageShape -> ImageShape
rotateImageShape ZeroHr shape = shape
rotateImageShape SixHr shape =
  -- Note that the image type is changed to JPEG because the tool we
  -- currently use to rotate images returns a JPEG image.
  shape {_imageShapeType = JPEG}
rotateImageShape ThreeHr shape =
  shape { _imageShapeType = JPEG
        , _imageShapeWidth = _imageShapeHeight shape
        , _imageShapeHeight = _imageShapeWidth shape }
rotateImageShape NineHr shape = rotateImageShape ThreeHr shape

cropImageShape :: ImageCrop -> ImageShape -> ImageShape
cropImageShape crop shape | crop == def = shape
cropImageShape (ImageCrop{..}) shape =
  shape { _imageShapeWidth = _imageShapeWidth shape - (leftCrop + rightCrop)
        , _imageShapeHeight = _imageShapeHeight shape - (topCrop + bottomCrop) }

uprightImageShape :: ImageShape -> ImageShape
uprightImageShape shape@(ImageShape {_imageFileOrientation = rot}) =
  case rot of
    ZeroHr -> shape
    SixHr -> shape
    ThreeHr -> shape
    NineHr -> shape

-- | Compute a derived image shape from the original image shape and the key.
shapeFromKey :: ImageShape -> ImageKey -> ImageShape
shapeFromKey original key@(ImageOriginal csum typ) =
  original
shapeFromKey original (ImageUpright key) =
  uprightImageShape $ shapeFromKey original key
shapeFromKey original (ImageCropped crop key) =
  cropImageShape crop $ shapeFromKey original key
shapeFromKey original (ImageScaled sz dpi key) =
  scaleImageShape sz dpi $ shapeFromKey original key

-- Statistics about the server status of the images in this reports.
data ImageStats
  = ImageStats
    { _keys :: Int
    , _ready :: Int
    , _shapes :: Int
    , _errors :: Int
    } deriving (Generic, Eq, Ord, Show, Serialize)

instance SafeCopy ImageStats
