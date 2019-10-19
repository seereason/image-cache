-- | Pure functions to deal with image data.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.Image
    ( ImageSize(..) -- , dim, size, units
    , approx
    , rationalIso
    , rationalLens
    , ImageCrop(..)
    , Dimension(..)
    , Units(..)
    , ImageFile(..) -- , imageFile, imageFileType, imageFileWidth, imageFileHeight, imageFileMaxVal
    , imageFileArea
    , PixmapShape(..)
    , ImageType(..)
    , fileExtension
    , ImageKey(..)
    , CacheImage
    , ImageCacheMap
    , scaleFromDPI
    , widthInInches
    , widthInInches'
    , heightInInches
    , saneSize
    , SaneSize(..) -- , unSaneSize
    , defaultSize
    , fixKey
    , readRationalMaybe
    , showRational
    ) where

import Control.Lens (Iso', iso, Lens', lens, _Show)
import Control.Lens.Path
import Control.Lens.Path.PathValueMap (newtypeIso)
import Control.Lens.Path.View (viewIso)
--import Control.Monad.Except (catchError)
import Data.Default (Default(def))
import Data.FileCache.Cache (CacheValue)
import Data.FileCache.File (Checksum, Extension, File(..))
import Data.Generics (Data, Typeable)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.Ratio ((%), approxRational)
import Data.SafeCopy (SafeCopy(..), safeGet, safePut)
import Data.Serialize (Serialize(..))
import Data.Text (pack, Text, unpack)
import GHC.Generics (Generic)
import Language.Haskell.TH (Ppr(ppr))
import Language.Haskell.TH.PprLib (ptext)
import Numeric (fromRat, readSigned, readFloat, showSigned, showFFloat)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

import Text.Read (readMaybe)

-- | Simplify the ratio to avoid a long representation:
--
-- > λ> toRational 0.123456
-- > 8895942329546431 % 72057594037927936
-- > λ> approxRational (toRational 0.123456) (1 % 10000)
-- > 10 % 81
-- > λ> 10 / 81
-- > 0.12345679012345678   (wow, that's wierd)
--
-- This is important for values that might become part of a path,
-- we don't want them to be too long or subject to rounding errors.
approx :: Rational -> Rational
approx x = approxRational x (1 % 10000)

-- | readShowLens is not a good choice for rational numbers,  because
-- it only understands strings like "15 % 4", not "15" or "3.5".
-- If an invalid string is input this returns 0.
rationalLens :: Lens' Rational String
rationalLens = lens showRational (\r s -> either (const r) id (readRationalMaybe s))

rationalIso :: Iso' Rational String
rationalIso = iso showRational (readRational 0)
    where
      readRational :: Rational -> String -> Rational
      readRational d = either (const d) id . readRationalMaybe

showRational :: Rational -> String
showRational x = showSigned (showFFloat Nothing) 0 (fromRat x :: Double) ""

readRationalMaybe :: Monad m => String -> m Rational
readRationalMaybe s =
    case (map fst $ filter (null . snd) $ readSigned readFloat s) of
      [r] -> return r
      [] -> fail $ "readRationalMaybe " ++ s
      _rs -> fail $ "readRationalMaybe " ++ s

instance View Rational where type ViewType Rational = Text; _View = rationalIso . iso pack unpack

-- mapRatio :: (Integral a, Integral b) => (a -> b) -> Ratio a -> Ratio b
-- mapRatio f r = f (numerator r) % f (denominator r)

data ImageSize
    = ImageSize
      { _dim :: Dimension
      , _size :: Rational
      , _units :: Units
      } deriving (Generic, Eq, Ord)

instance Default ImageSize where
    def = ImageSize TheArea 15.0 Inches

data Dimension
    = TheHeight
    | TheWidth
    | TheArea
    deriving (Generic, Eq, Ord, Enum, Bounded)

instance View Dimension where type ViewType Dimension = Text; _View = viewIso _Show TheHeight . iso pack unpack

data Units
    = Inches
    | Cm
    | Points
    deriving (Generic, Eq, Ord, Enum, Bounded)

instance View Units where type ViewType Units = Text; _View = viewIso _Show Inches . iso pack unpack

-- |This describes the cropping and rotation of an image.
data ImageCrop
    = ImageCrop
      { topCrop :: Int
      , bottomCrop :: Int
      , leftCrop :: Int
      , rightCrop :: Int
      , rotation :: Int         -- 0, 90, 180, 270
      } deriving (Generic, Eq, Ord)

instance Default ImageCrop where
    def = ImageCrop 0 0 0 0 0

-- | A class whose primary (only?) instance is ImageFile.  Access to
-- the original dimensions of the image, so we can compute the aspect
-- ratio.
class PixmapShape a where
    pixmapHeight :: a -> Int
    pixmapWidth :: a -> Int
    pixmapMaxVal :: a -> Int

-- |Given the desired DPI and image dimensions, return the factor by
-- which an image should be scaled.  Result of Nothing means the scale
-- is pathological.
scaleFromDPI :: PixmapShape a => Rational -> ImageSize -> a -> Maybe Rational
scaleFromDPI dpi sz file =
    case _dim sz of
      _ | _size sz < 0.000001 || _size sz > 1000000.0 -> Nothing
      TheHeight -> Just $ inches sz * dpi / h
      TheWidth -> Just $ inches sz * dpi / w
      -- If we want an area of 9 square inches, and the dpi is 100, and the image
      -- size is 640x480 pixels, the scale is (9 * 100 * 100) / (640 * 480)
      TheArea -> Just (rsqrt (inches sz * dpi * dpi / (w * h)))
    where
      w = fromIntegral (pixmapWidth file)
      h = fromIntegral (pixmapHeight file)

widthInInches :: PixmapShape a => a -> ImageSize -> Rational
widthInInches p s =
    case _dim s of
      TheWidth -> toInches (_units s) (_size s)
      TheHeight -> widthInInches p (s {_dim = TheWidth, _size = approx (_size s / r)})
      TheArea -> widthInInches p (s {_dim = TheWidth, _size = approx (rsqrt (_size s / r))})
    where
      r :: Rational
      r = fromIntegral (pixmapHeight p) % fromIntegral (pixmapWidth p)
      toInches :: Units -> Rational -> Rational
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

rsqrt :: Rational -> Rational
rsqrt = toRational . (sqrt :: Double -> Double) . fromRat

heightInInches :: PixmapShape a => a -> ImageSize -> Rational
heightInInches p s =
    case _dim s of
      TheHeight -> toInches (_units s) (_size s)
      TheWidth -> heightInInches p (s {_dim = TheHeight, _size = approx (_size s / r)})
      TheArea -> heightInInches p (s {_dim = TheHeight, _size = approx (rsqrt (_size s / r))})
    where
      r :: Rational
      r = fromIntegral (pixmapHeight p) % fromIntegral (pixmapWidth p)
      toInches Inches x = x
      toInches Cm x = x / (254 % 100)
      toInches Points x = x / (7227 % 100)

-- |Modify an ImageSize so that the dimension is width and the units
-- are inches.  This way we can figure out how many images fit across
-- the page.
widthInInches' :: PixmapShape a => a -> ImageSize -> ImageSize
widthInInches' p s = s {_units = Inches, _size = approx (widthInInches p s), _dim = TheWidth}

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

-- | A wrapper type to suggest that lens_saneSize has been applied to
-- the ImageSize within.
newtype SaneSize a = SaneSize {_unSaneSize :: a} deriving (Generic, Eq, Ord)

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

instance Pretty Dimension where
    pPrint TheHeight = text "height"
    pPrint TheWidth = text "width"
    pPrint TheArea = text "area"

instance Pretty Units where
    pPrint Inches = text "in"
    pPrint Cm = text "cm"
    pPrint Points = text "pt"

instance Pretty ImageSize where
    pPrint (ImageSize d sz u) = pPrint d <> text ("=" <> showRational sz <> " ") <> pPrint u

instance Pretty ImageCrop where
    pPrint (ImageCrop t b l r rot) = text $ "(crop " <> show (b, l) <> " -> " <> show (t, r) <> ", rot " ++ show rot ++ ")"

-- | A file containing an image plus meta info.
data ImageFile
    = ImageFile
      { _imageFile :: File
      , _imageFileType :: ImageType
      , _imageFileWidth :: Int
      , _imageFileHeight :: Int
      , _imageFileMaxVal :: Int
      } deriving (Generic, Eq, Ord)

data ImageType = PPM | JPEG | GIF | PNG deriving (Generic, Eq, Ord)

instance PixmapShape ImageFile where
    pixmapHeight = _imageFileHeight
    pixmapWidth = _imageFileWidth
    pixmapMaxVal = _imageFileMaxVal

instance Pretty ImageFile where
    pPrint (ImageFile f typ w h _mx) = text "ImageFile(" <> pPrint f <> text (" " <> show w <> "x" <> show h <> " " <> show typ <> ")")

-- |Return the area of an image in square pixels.
imageFileArea :: ImageFile -> Int
imageFileArea image = _imageFileWidth image * _imageFileHeight image

fileExtension :: ImageType -> Extension
fileExtension JPEG = ".jpg"
fileExtension PPM = ".ppm"
fileExtension GIF = ".gif"
fileExtension PNG = ".png"

-- | Describes an ImageFile and, if it was derived from other image
-- files, how.
data ImageKey
    = ImageOriginal ImageFile
    -- ^ An unmodified upload
    | ImageCropped ImageCrop ImageKey
    -- ^ A cropped version of another image
    | ImageScaled ImageSize Rational ImageKey
    -- ^ A resized version of another image
    | ImageUpright ImageKey
    -- ^ Image uprighted using the EXIF orientation code, see  "Appraisal.Exif"
    deriving (Generic, Eq, Ord)

instance Pretty ImageKey where
    pPrint (ImageOriginal _) = text "ImageOriginal"
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> text ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled sz dpi x) = text "Scale (" <> pPrint sz <> text " @" <> text (showRational dpi) <> text " dpi) (" <> pPrint x <> text ")"

instance Ppr ImageKey where ppr = ptext . show

type CacheImage = CacheValue ImageFile
type ImageCacheMap = Map ImageKey ImageFile

-- | Remove null crops
fixKey :: ImageKey -> ImageKey
fixKey key@(ImageOriginal _) = key
fixKey (ImageCropped crop key) | crop == def = fixKey key
fixKey (ImageCropped crop key) = ImageCropped crop (fixKey key)
fixKey (ImageScaled sz dpi key) = ImageScaled sz dpi (fixKey key)
fixKey (ImageUpright key) = ImageUpright (fixKey key)

instance SafeCopy ImageSize where version = 2
instance (SafeCopy a, Typeable a) => SafeCopy (SaneSize a) where version = 1
instance SafeCopy Dimension where version = 1
instance SafeCopy Units where version = 0
instance SafeCopy ImageCrop where version = 0
instance SafeCopy ImageKey where version = 2
instance SafeCopy ImageType where version = 0
instance SafeCopy ImageFile where version = 1

instance Serialize ImageSize where get = safeGet; put = safePut
instance Serialize Dimension where get = safeGet; put = safePut
instance Serialize Units where get = safeGet; put = safePut
instance Serialize ImageCrop where get = safeGet; put = safePut
instance (SafeCopy a, Typeable a) => Serialize (SaneSize a) where get = safeGet; put = safePut
instance Serialize ImageFile where get = safeGet; put = safePut
instance Serialize ImageType where get = safeGet; put = safePut
instance Serialize ImageKey where get = safeGet; put = safePut

deriving instance Data ImageSize
deriving instance Data Dimension
deriving instance Data Units
deriving instance Data ImageCrop
deriving instance Data a => Data (SaneSize a)
deriving instance Data ImageFile
deriving instance Data ImageType
deriving instance Data ImageKey

deriving instance Read ImageSize
deriving instance Read Dimension
deriving instance Read Units
deriving instance Read ImageCrop
deriving instance Read a => Read (SaneSize a)
deriving instance Read ImageFile
deriving instance Read ImageType
deriving instance Read ImageKey

deriving instance Show ImageSize
deriving instance Show Dimension
deriving instance Show Units
deriving instance Show ImageCrop
deriving instance Show a => Show (SaneSize a)
deriving instance Show ImageFile
deriving instance Show ImageType
deriving instance Show ImageKey

deriving instance Typeable ImageKey
deriving instance Typeable ImageType
deriving instance Typeable ImageSize
deriving instance Typeable Dimension
deriving instance Typeable Units
deriving instance Typeable ImageCrop
deriving instance Typeable (SaneSize a)
deriving instance Typeable ImageFile

instance View (SaneSize ImageSize) where
    type ViewType (SaneSize ImageSize) = ImageSize
    _View = newtypeIso

instance View (Maybe ImageFile) where type ViewType (Maybe ImageFile) = String; _View = iso (maybe "" show) readMaybe

$(concat <$>
  sequence
  [ makeValueInstance [] [t|Rational|]
  , makePathInstances [FIELDS] ''ImageFile
  , makePathInstances [] ''ImageType
  , makePathInstances [FIELDS] ''ImageSize
  , makePathInstances [] ''Dimension
  , makePathInstances [FIELDS] ''ImageCrop
  , makePathInstances [FIELDS] ''ImageKey
  , makePathInstances [] ''Units
  , makeValueInstance [NEWTYPE, VIEW] [t|SaneSize ImageSize|]
  ])
