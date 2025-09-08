{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE NoOverloadedLists #-}

module Data.FileCache.ImageRect
  ( ImageRect(_imageRectWidth, _imageRectHeight, _imageFileOrientation)
  , makeImageRect
  , imageAspect
  , HasImageRect(imageRect)
  , widthInInches
  , widthInInches'
  , heightInInches
  , scaleImageRect
  , scaleFromDPI
  , cropImageRect
  , uprightImageRect
  ) where

import Control.Lens.Path (HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(hops))
import Control.Monad.Except (throwError)
import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.FileCache.ImageCrop ( Rotation(..), ImageCrop(..) )
-- import Data.FileCache.ImageKey ( OriginalKey(..), ImageKey(..) )
import Data.FileCache.ImageSize (Units(..), Dimension(TheArea, TheWidth, TheHeight), ImageSize(_units, _size, _dim), inches )
import Data.FileCache.Rational ((%), approx, rsqrt)
import Data.Generics.Labels ()
import Data.Monoid ( (<>) )
import Data.SafeCopy (base, safeGet, safePut, SafeCopy(version, kind) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable ( Typeable, typeRep )
import GHC.Generics ( Generic )
import GHC.Stack (callStack, HasCallStack)
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- * ImageRect

data ImageRect
  = ImageRect
    { _imageRectWidth :: Int
    , _imageRectHeight :: Int
    , _imageFileOrientation :: Rotation
    } deriving (Generic, Eq, Ord, Data, Typeable, Read)

instance Show ImageRect where
  show ImageRect{..} = "(makeImageRect " <> show _imageRectWidth <> " " <> show _imageRectHeight <> " " <> show _imageFileOrientation <> ")"

instance Serialize ImageRect where get = safeGet; put = safePut
instance SafeCopy ImageRect where version = 1; kind = base
instance Value ImageRect where hops _ = [RecType, CtorType]

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

makeImageRect :: HasCallStack => Int -> Int -> Rotation -> ImageRect
makeImageRect w h rot =
  ImageRect {_imageRectWidth = w, _imageRectHeight = h, _imageFileOrientation = rot}
  where _ = callStack

imageAspect :: HasCallStack => ImageRect -> Rational
imageAspect rect@ImageRect{..} =
  case _imageFileOrientation of
    ThreeHr -> imageAspect (uprightImageRect rect)
    NineHr -> imageAspect (uprightImageRect rect)
    _ -> fromIntegral _imageRectHeight % fromIntegral _imageRectWidth

widthInInches :: HasCallStack => ImageRect -> ImageSize -> Rational
widthInInches rect@ImageRect{..} s =
  case _dim s of
    TheWidth -> toInches (_units s) (_size s)
    TheHeight -> widthInInches upright (s {_dim = TheWidth, _size = approx (_size s / aspect)})
    TheArea -> widthInInches upright (s {_dim = TheWidth, _size = approx (rsqrt (_size s / aspect))})
    where
      upright = uprightImageRect rect
      aspect :: Rational
      aspect = imageAspect rect
      toInches :: Units -> Rational -> Rational
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

heightInInches :: HasCallStack => ImageRect -> ImageSize -> Rational
heightInInches rect s =
  case _dim s of
    TheHeight -> toInches (_units s) (_size s)
    TheWidth -> heightInInches upright (s {_dim = TheHeight, _size = approx (_size s * aspect)})
    TheArea -> heightInInches upright (s {_dim = TheHeight, _size = approx (rsqrt (_size s * aspect))})
    where
      upright = uprightImageRect rect
      aspect :: Rational
      aspect = imageAspect rect
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

-- |Modify an ImageSize so that the dimension is width and the units
-- are inches.  This way we can figure out how many images fit across
-- the page.
widthInInches' :: HasCallStack => ImageRect -> ImageSize -> ImageSize
widthInInches' p s =
    s {_units = Inches, _size = approx (widthInInches p s), _dim = TheWidth}

-- | This seems to have evolved into a no-op.
uprightImageRect :: HasCallStack => ImageRect -> ImageRect
uprightImageRect rect =
  case _imageFileOrientation rect of
    ZeroHr -> rect
    SixHr -> rect
    ThreeHr ->
      ImageRect {_imageFileOrientation = ZeroHr,
                 _imageRectWidth = _imageRectHeight rect,
                 _imageRectHeight = _imageRectWidth rect}
    NineHr ->
      ImageRect {_imageFileOrientation = ZeroHr,
                 _imageRectWidth = _imageRectHeight rect,
                 _imageRectHeight = _imageRectWidth rect}
  where _ = callStack

scaleImageRect :: HasCallStack => ImageSize -> Rational -> ImageRect -> ImageRect
scaleImageRect sz dpi rect =
  if scale == 1
  then rect
  else rect { _imageRectWidth = round (fromIntegral (_imageRectWidth rect) * scale)
            , _imageRectHeight = round (fromIntegral (_imageRectHeight rect) * scale) }
  where
    scale :: Rational
    scale = maybe 1 approx $ scaleFromDPI sz dpi rect

-- |Given the desired DPI and image dimensions, return the factor by
-- which an image should be scaled.  Result of Nothing means the scale
-- is pathological.
scaleFromDPI :: HasCallStack => ImageSize -> Rational -> ImageRect -> Maybe Rational
scaleFromDPI sz dpi (ImageRect {_imageRectHeight = h, _imageRectWidth = w}) =
  case _dim sz of
    _ | _size sz < 0.000001 || _size sz > 1000000.0 -> Nothing
    TheHeight | h == 0 -> Nothing
    TheHeight -> Just $ inches sz * dpi / fromIntegral h
    TheWidth | w == 0 -> Nothing
    TheWidth -> Just $ inches sz * dpi / fromIntegral w
    -- If we want an area of 9 square inches, and the dpi is 100, and the image
    -- size is 640x480 pixels, the scale is (9 * 100 * 100) / (640 * 480)
    TheArea | h == 0 || w == 0 -> Nothing
    TheArea -> Just (rsqrt (inches sz * dpi * dpi / (fromIntegral w * fromIntegral h)))
  where _ = callStack

cropImageRect :: HasCallStack => ImageCrop -> ImageRect -> ImageRect
cropImageRect crop rect | crop == def = rect
cropImageRect (ImageCrop{..}) rect =
  rect {_imageRectWidth = _imageRectWidth rect - (leftCrop + rightCrop),
        _imageRectHeight = _imageRectHeight rect - (topCrop + bottomCrop) }
  where _ = callStack

class HasImageRect a where imageRect :: a -> Either String ImageRect

$(concat <$>
  sequence
  [pathInstances [FIELDS] =<< [t|ImageRect|]
  ])
