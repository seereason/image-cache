{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.Common
  ( -- * Rational
    approx
  -- , rationalIso
  , rationalLens
  , readRationalMaybe
  , showRational

    -- * ImageSize
  , HasImageSize(imageSize)
  , ImageSize(..) -- , dim, size, units
  , Dimension(..)
  , Units(..)
  , saneSize
  , SaneSize(..) -- , unSaneSize
  , defaultSize
    -- * ImageShape, HasImageShape
  , ImageShape(..)
  , HasImageShapeM(imageShapeM)
  , HasImageShape, imageShape
  , scaleFromDPI
  , widthInInches
  , widthInInches'
  , heightInInches
  , scaleImageShape

    -- * ImageCrop
  , ImageCrop(..)
  , cropImageShape
  , Rotation(..)
  , rotateImageShape

    -- * ImageType
  , ImageType(..)
  , Extension
  , HasFileExtension(fileExtension)
  , HasImageType(imageType)

    -- * File
  , File(..)
  , FileSource(..)
  , Checksum
  , HasFileChecksum(fileChecksum)
  -- , fileURI
  -- , addMessage
  , HasURIPath(toURIPath)

    -- * ImageFile, ImageReady
  , ImageFile(..)
  , ImageReady(..)

    -- * ImageKey
  , ImageKey(..)
  , ImageKey_2(..)
  , HasImageKey(imageKey)
  , OriginalKey(originalKey)
  , UprightKey(uprightKey)
  , EditedKey(editedKey)
  , ScaledKey(scaledKey)
  , ImagePath(..)
  , HasImagePath(..)

    -- * ImageCached
  , ImageCached(..)

--    -- * FileError
  , FileError(..)
  , CommandError
  , HasFileError(fromFileError),
--  , logErrorCall
  ) where

import Control.Exception as E (Exception, ErrorCall, fromException, IOException, toException)
import Control.Lens ( Identity(runIdentity), Iso', iso, Lens', lens, _Show )
import Control.Lens.Path ( HOP(FIELDS), makePathInstances, makeValueInstance, HOP(VIEW, NEWTYPE), View(..), newtypeIso )
import Control.Lens.Path.View ( viewIso )
import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.FileCache.Types (CommandError)
import Data.Maybe (fromMaybe)
import Data.Monoid ( (<>) )
import Data.Ratio ( (%), approxRational, denominator, numerator )
import Data.SafeCopy ( extension, Migrate(..), SafeCopy(..), safeGet, safePut )
import Data.Serialize ( Serialize(..) )
import Data.String (IsString(fromString))
import Data.Text ( pack, Text, unpack )
import Data.Typeable ( Typeable )
import Extra.Except (FromSomeNonPseudoException(..), HasIOException(..), HasErrorCall(..))
import Extra.Text (Texty(..))
import GHC.Generics ( Generic, M1(M1) )
--import Language.Haskell.TH ( Loc(..) )
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ( Lift )
--import Language.Haskell.TH.Syntax ( Loc(loc_module) )
--import Network.URI ( URI(..), parseRelativeReference, parseURI )
import Numeric ( fromRat, readSigned, readFloat, showSigned, showFFloat )
import System.FilePath ( makeRelative )
--import System.Log.Logger ( Priority(ERROR), logM )
import Text.Parsec {-as Parsec ((<|>), anyChar, char, choice, digit, many, many1, sepBy,
                              spaces, try, parse, string, noneOf)-}
--import Text.Parsec.String as String (Parser)
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Text.PrettyPrint.HughesPJClass ()
import Text.Read ( readMaybe )
import Web.Routes ( PathInfo(..), segment, toPathInfo )
import Web.Routes.TH ( derivePathInfo )

-- * Rational

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

rsqrt :: Rational -> Rational
rsqrt = toRational . (sqrt :: Double -> Double) . fromRat

instance View Rational where
  type ViewType Rational = Text
  _View = rationalIso . iso pack unpack

instance PathInfo Rational where
  toPathSegments r =
    toPathSegments (numerator r') <> toPathSegments (denominator r')
    -- This means that toPathSegments might not be the exact inverse
    -- of fromPathSegments - is there any danger from this?
    where r' = approx r
  fromPathSegments = (%) <$> fromPathSegments <*> fromPathSegments

-- mapRatio :: (Integral a, Integral b) => (a -> b) -> Ratio a -> Ratio b
-- mapRatio f r = f (numerator r) % f (denominator r)


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

-- * ImageShape

instance HasImageShapeM Identity ImageShape where
  imageShapeM s = pure s

data ImageShape
  = ImageShape
      { _imageShapeType :: ImageType
      , _imageShapeWidth :: Int
      , _imageShapeHeight :: Int
      , _imageFileOrientation :: Rotation
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Migrate ImageShape where
  type MigrateFrom ImageShape = ImageShape_0
  -- We need to go through and repair the _imageFileOrientation field
  -- after this migration occurs, probably in SetImageFileTypes.
  migrate (ImageShape_0 typ w h) = ImageShape typ w h ZeroHr

data ImageShape_0 = ImageShape_0 ImageType Int Int deriving Generic

instance Serialize ImageShape where get = safeGet; put = safePut
instance SafeCopy ImageShape where version = 1; kind = extension
instance SafeCopy ImageShape_0

instance Pretty ImageShape where
  pPrint (ImageShape typ w h rot) =
    (text "ImageShape (" <> text (show w) <> text "x" <> text (show h) <> text " " <> pPrint typ <>
     (case rot of
        ZeroHr -> text " UL"
        ThreeHr -> text " UR"
        SixHr -> text " LR"
        NineHr -> text " LL") <> text ")")

instance HasImageType ImageShape where imageType = _imageShapeType

scaleImageShape :: ImageSize -> Rational -> ImageShape -> ImageShape
scaleImageShape sz dpi shape =
  if approx (toRational scale) == 1
  then shape
  else shape { _imageShapeType = JPEG -- the scaling pipeline results in a jpeg file
             , _imageShapeWidth = round (fromIntegral (_imageShapeWidth shape) * scale)
             , _imageShapeHeight = round (fromIntegral (_imageShapeHeight shape) * scale) }
  where
    scale' = scaleFromDPI sz dpi shape
    scale :: Double
    scale = fromRat (fromMaybe 1 scale')

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

instance Default ImageCrop where def = ImageCrop 0 0 0 0 ZeroHr
instance Serialize ImageCrop where get = safeGet; put = safePut
instance SafeCopy ImageCrop_0 where version = 0
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

-- * ImageType and Checksum

data ImageType = PPM | JPEG | GIF | PNG | Unknown deriving (Generic, Eq, Ord)

deriving instance Data ImageType
deriving instance Read ImageType
deriving instance Show ImageType
deriving instance Typeable ImageType
instance Serialize ImageType where get = safeGet; put = safePut
instance SafeCopy ImageType where version = 0
instance Pretty ImageType where pPrint = text . show

type Extension = Text

class HasImageType a where imageType :: a -> ImageType

class HasFileExtension a where fileExtension :: a -> Extension
instance HasFileExtension Extension where fileExtension = id

instance HasFileExtension ImageType where
  fileExtension JPEG = ".jpg"
  fileExtension PPM = ".ppm"
  fileExtension GIF = ".gif"
  fileExtension PNG = ".png"
  fileExtension Unknown = ".jpg"

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = Text

class HasFileChecksum a where fileChecksum :: a -> Checksum

-- * File

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           , _fileExt :: Extension               -- ^ Name is formed by appending this to checksum
           } deriving (Generic, Eq, Ord)

instance Migrate File where
  type MigrateFrom File = File_2
  migrate (File_2 src cksum msgs ext) =
    File {_fileSource = src, _fileChksum = pack cksum, _fileMessages = msgs, _fileExt = pack ext}

-- |A local cache of a file obtained from a 'FileSource'.
data File_2
    = File_2 { _fileSource_2 :: Maybe FileSource
             , _fileChksum_2 :: String
             , _fileMessages_2 :: [String]
             , _fileExt_2 :: String
             } deriving (Generic, Eq, Ord)

instance HasFileChecksum File where fileChecksum = _fileChksum
instance HasFileExtension File where fileExtension = _fileExt

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File " <> take 7 (unpack cksum) <> unpack ext)
instance SafeCopy File_2 where version = 2
instance SafeCopy File where version = 3; kind = extension
instance Serialize File where get = safeGet; put = safePut
deriving instance Show File
deriving instance Read File
deriving instance Data File
deriving instance Typeable File
deriving instance Lift File

-- |The original source if the file is saved, in case
-- the cache needs to be reconstructed.  However, we don't
-- store the original ByteString if that is all we began
-- with, that would be redundant and wasteful.
data FileSource
    = TheURI String
    | ThePath FilePath
    deriving (Generic, Eq, Ord)

instance SafeCopy FileSource where version = 1
instance Serialize FileSource where get = safeGet; put = safePut
deriving instance Show FileSource
deriving instance Read FileSource
deriving instance Data FileSource
deriving instance Typeable FileSource
deriving instance Lift FileSource

$(concat <$>
  sequence
  [ makePathInstances [FIELDS] ''File
  , makePathInstances [FIELDS] ''FileSource ])

#if ARBITRARY
instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure [] <*> arbitrary

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]
#endif

-- * ImageFile

data ImageFile
  = ImageFileShape ImageShape
  | ImageFileReady ImageReady
  deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

data ImageReady
  = ImageReady {_imageFile :: File, _imageShape :: ImageShape}
  deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance SafeCopy ImageFile where version = 3; kind = extension
instance SafeCopy ImageReady where version = 1

instance HasFileExtension ImageFile where
  fileExtension (ImageFileShape x) = fileExtension x
  fileExtension (ImageFileReady x) = fileExtension x

instance HasFileExtension ImageReady where
  fileExtension = fileExtension . _imageFile

instance HasFileExtension ImageShape where
  fileExtension = fileExtension . _imageShapeType

instance Migrate ImageFile where
  type MigrateFrom ImageFile = ImageFile_2
  migrate (ImageFile_2 f s) = ImageFileReady (ImageReady f s)

instance Serialize ImageFile where get = safeGet; put = safePut
instance Serialize ImageReady where get = safeGet; put = safePut

instance Pretty ImageFile where
  pPrint (ImageFileShape s) = text "ImageFileShape (" <> pPrint s <> text ")"
  pPrint (ImageFileReady f) = text "ImageFileReady (" <> pPrint f <> text ")"
instance Pretty ImageReady where
  pPrint (ImageReady f s) = text "ImageReady (" <> pPrint f <> text ") (" <> pPrint s <> text ")"
instance HasImageType ImageFile where
  imageType (ImageFileReady f) = imageType f
  imageType (ImageFileShape s) = imageType s
instance HasImageType ImageReady where
  imageType = imageType . _imageShape

-- * ImageFile

-- | A file containing an image plus meta info.
data ImageFile_2
    = ImageFile_2
      { _imageFile_2 :: File
      , _imageFileShape_2 :: ImageShape
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance SafeCopy ImageFile_2 where kind = extension; version = 2

-- 1 Nov 2019
instance Migrate ImageFile_2 where
  type MigrateFrom ImageFile_2 = ImageFile_1
  migrate (ImageFile_1 f t w h _) = ImageFile_2 f (migrate (ImageShape_0 t w h))

data ImageFile_1
    = ImageFile_1
      { _imageFile_1 :: File
      , _imageFileType_1 :: ImageType
      , _imageFileWidth_1 :: Int
      , _imageFileHeight_1 :: Int
      , _imageFileMaxVal_1 :: Int
      } deriving (Generic, Eq, Ord)

instance SafeCopy ImageFile_1 where version = 1

instance View (Maybe ImageFile) where
  type ViewType (Maybe ImageFile) = String
  _View = iso (maybe "" show) readMaybe

instance HasImageShapeM Identity ImageFile where
  imageShapeM (ImageFileReady f) = imageShapeM f
  imageShapeM (ImageFileShape f) = imageShapeM f

instance HasImageShapeM Identity ImageReady where
  imageShapeM = imageShapeM . _imageShape

-- * ImageKey

-- | Describes an ImageFile and, if it was derived from other image
-- files, how.
data ImageKey
    = ImageOriginal Checksum ImageType
    -- ^ An unmodified upload, the info lets us construct an URL
    | ImageCropped ImageCrop ImageKey
    -- ^ A cropped version of another image
    | ImageScaled ImageSize Rational ImageKey
    -- ^ A resized version of another image
    | ImageUpright ImageKey
    -- ^ Image uprighted using the EXIF orientation code, see  "Appraisal.Exif"
    deriving (Generic, Eq, Ord)

{-
instance Migrate ImageKey where
  type MigrateFrom ImageKey = ImageKey_3
  migrate (ImageOriginal_3 csum) =
    -- We need to update the image original types with a migration of CacheMap
    ImageOriginal csum Unknown
  migrate (ImageCropped_3 crop key) = ImageCropped crop key
  migrate (ImageScaled_3 size dpi key) = ImageScaled size dpi key
  migrate (ImageUpright_3 key) = ImageUpright key

-- | Describes an ImageFile and, if it was derived from other image
-- files, how.
data ImageKey_3
    = ImageOriginal_3 Checksum
    -- ^ An unmodified upload, the info lets us construct an URL
    | ImageCropped_3 ImageCrop ImageKey
    -- ^ A cropped version of another image
    | ImageScaled_3 ImageSize Rational ImageKey
    -- ^ A resized version of another image
    | ImageUpright_3 ImageKey
    -- ^ Image uprighted using the EXIF orientation code, see  "Appraisal.Exif"
    deriving (Generic, Eq, Ord)

instance Migrate ImageKey_3 where
  type MigrateFrom ImageKey_3 = ImageKey_2
  migrate (ImageOriginal_2 i) =
    -- We need to update the image original types with a migration of CacheMap
    ImageOriginal_3 (_fileChksum f)
    where f = case i of
                ImageFileShape _ -> error "unexpected value during migration"
                ImageFileReady file -> _imageFile file
  migrate (ImageCropped_2 crop key) = ImageCropped_3 crop key
  migrate (ImageScaled_2 size dpi key) = ImageScaled_3 size dpi key
  migrate (ImageUpright_2 key) = ImageUpright_3 key
-}

-- When this is removed the 'setImageFileTypes' function should also
-- be removed.
data ImageKey_2
    = ImageOriginal_2 ImageFile
    | ImageCropped_2 ImageCrop ImageKey_2
    | ImageScaled_2 ImageSize Rational ImageKey_2
    | ImageUpright_2 ImageKey_2
    deriving (Generic, Eq, Ord)

deriving instance Data ImageKey
deriving instance Read ImageKey
deriving instance Show ImageKey
deriving instance Typeable ImageKey
instance Serialize ImageKey where get = safeGet; put = safePut
instance SafeCopy ImageKey_2 where version = 2
-- instance SafeCopy ImageKey_3 where kind = extension; version = 3
-- This is not an extension of ImageKey_2, it is a new type
-- created by the migration of ImageCache.
instance SafeCopy ImageKey where version = 4

instance Pretty ImageKey where
    pPrint (ImageOriginal csum typ) = text (take 7 (unpack csum)) <> text (unpack (fileExtension typ))
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled sz dpi x) = text "Scale (" <> pPrint sz <> text " @" <> text (showRational dpi) <> text " dpi) (" <> pPrint x <> text ")"

class HasImageKey a where
  imageKey :: a -> ImageKey
instance HasImageKey ImageKey where
  imageKey = id

instance OriginalKey ImageKey where
  originalKey key@(ImageOriginal _ _) = key
  originalKey (ImageUpright key) = originalKey key
  originalKey (ImageScaled _ _ key) = originalKey key
  originalKey (ImageCropped _ key) = originalKey key

-- | Various ways to build an OriginalKey.
class OriginalKey a where
  originalKey :: a -> ImageKey
instance OriginalKey (Checksum, ImageType) where -- danger - Checksum is just String
  originalKey = uncurry ImageOriginal
--instance OriginalKey ImageFile where
--  originalKey (ImageFileReady i) = originalKey i
--  originalKey (ImageFileShape i) = originalKey i
instance OriginalKey ImageReady where
  originalKey i = originalKey (_imageFile i, _imageShape i)
instance OriginalKey (File, ImageShape) where
  originalKey (f, shape) = originalKey (f, _imageShapeType shape)
instance OriginalKey (File, ImageType) where
  originalKey (f, typ) = ImageOriginal (_fileChksum f) typ

class UprightKey a where
  uprightKey :: a -> ImageKey
instance UprightKey ImageReady where
  uprightKey img = ImageUpright (originalKey img)
instance UprightKey ImageKey where
  uprightKey (ImageScaled _ _ key) = uprightKey key
  uprightKey (ImageCropped _ key) = uprightKey key
  uprightKey (ImageUpright key) = uprightKey key
  uprightKey key@(ImageOriginal _ _) = ImageUpright key

class EditedKey a where
  editedKey :: a -> ImageKey
instance EditedKey ImageReady where
  editedKey img = uprightKey img
instance EditedKey ImageKey where
  editedKey (ImageScaled _ _ key) = editedKey key
  editedKey key@(ImageCropped _ _) = key
  editedKey key@(ImageUpright _) = key
  editedKey key@(ImageOriginal _ _) = ImageUpright key

class HasImageSize size => ScaledKey size a where
  scaledKey :: size -> Rational -> a -> ImageKey
instance ScaledKey ImageSize ImageReady where
  scaledKey size dpi x = ImageScaled (imageSize size) dpi (editedKey x)

-- * FileError, CommandInfo

data FileError
    = IOException IOError -- ^ Caught an IOException
    | ErrorCall E.ErrorCall -- ^ Caught a call to error
    | FromString String -- ^ FileError created via IsString(fromstring)
    | UnexpectedException String -- ^ Something unanticipated
    | CommandFailure CommandError -- ^ A shell command failed
    | CacheDamage Text -- ^ The contents of the cache is wrong
    | NoShape
      -- ^ Could not determine the dimensions of an image.  This comes
      -- from failed attempt to parse the output of the unix file(1)
      -- command.
    deriving (Eq, Ord, Generic)

-- Dubious instance, but omitting makes other things more dubious.
instance IsString FileError where fromString = FromString

instance Migrate FileError where
  type MigrateFrom FileError = FileError_1
  migrate (IOException_1 _) = error "unexpected FileError migration"
  migrate (ErrorCall_1 _) = error "unexpected FileError migration"
  migrate (CommandFailure_1 info) = CommandFailure info
  migrate CacheDamage_1 = CacheDamage ""

data FileError_1
    = IOException_1 Text
    | ErrorCall_1 Text
    | CommandFailure_1 CommandError
    | CacheDamage_1
    deriving (Eq, Ord, Generic)

instance Exception FileError
instance SafeCopy FileError_1 where version = 1
instance SafeCopy FileError where version = 2; kind = extension

instance Serialize FileError where get = safeGet; put = safePut

--deriving instance Data FileError
--deriving instance Data CommandInfo
deriving instance Show FileError

-- | This ensures that runExceptT catches IOException
instance HasIOException FileError where fromIOException = IOException
instance HasErrorCall FileError where fromErrorCall = ErrorCall

class HasFileError e where fromFileError :: FileError -> e
instance HasFileError FileError where fromFileError = id

instance FromSomeNonPseudoException FileError where
  fromSomeNonPseudoException e =
    maybe
      (maybe
       (fromString ("fromSomeNonPseudoException - unexpected argument: " <> show e))
       fromErrorCall
       (fromException (toException e) :: Maybe ErrorCall))
      fromIOException
      (fromException (toException e) :: Maybe IOException)

#if 0
fromFileError :: HasFileError e => FileError -> e
fromFileError = review fileErrorPrism

withFileError :: HasFileError e => (Maybe FileError -> r) -> e -> r
withFileError f = f . preview fileErrorPrism
#endif

#if 0
class Loggable a where
  logit :: Priority -> Loc -> a -> IO ()

instance Loggable FileError where
  logit priority loc (IOException e) = (logM (loc_module loc) priority (" - IO exception: " <> unpack e))
  logit priority loc (ErrorCall e) = (logM (loc_module loc) priority (" - error call: " <> show e))
  logit priority loc (CommandFailure info) = (logM (loc_module loc) priority " - shell command failed:" >> logCommandInfo priority loc info)
  logit priority loc (CacheDamage t) = logM (loc_module loc) priority (" - file cache is damaged: " <> unpack t)
  logit priority loc NoShape = (logM (loc_module loc) priority (" - NoShape: file(1) output notparsed"))

logCommandInfo :: Priority -> Loc -> CommandInfo -> IO ()
logCommandInfo priority loc (Description s e) = logM (loc_module loc) priority (" - error description: " <> s) >> logCommandInfo priority loc e
logCommandInfo priority loc (FunctionName n e) = logM (loc_module loc) priority (" - error function " <> n) >> logCommandInfo priority loc e
logCommandInfo priority loc (Command cmd code) = logM (loc_module loc) priority (" - command: " <> show cmd <> ", exit code: " <> show code)
logCommandInfo priority loc (CommandInput bs e) = logM (loc_module loc) priority (" - command input: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e
logCommandInfo priority loc (CommandOut bs e) = logM (loc_module loc) priority (" - command stdout: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e
logCommandInfo priority loc (CommandErr bs e) = logM (loc_module loc) priority (" - command stderr: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e

logErrorCall :: Unexceptional m => m (Either SomeException a) -> m (Either SomeException a)
logErrorCall x =
    x >>= either (\e -> case fromException e :: Maybe E.ErrorCall of
                          Just (ErrorCallWithLocation msg loc) ->
                              liftIO (logM "Appraisal.FileError" ERROR (show loc ++ ": " ++ msg)) >> return (Left e)
                          _ -> return (Left e)) (return . Right)
#endif

-- * ImagePath

data ImagePath =
  ImagePath { _imagePathKey :: ImageKey
            , _imagePathType :: ImageType
            } deriving (Generic, Eq, Ord)

class HasImagePath a where imagePath :: a -> ImagePath
instance HasImagePath ImagePath where imagePath = id
instance HasImageKey ImagePath where imageKey (ImagePath x _) = imageKey x

class HasURIPath a where
  toURIPath :: Texty text => a -> text

instance HasURIPath ImageKey where
  toURIPath (ImageOriginal csum typ) = textyText (csum <> fileExtension typ)
  toURIPath key = textyString (makeRelative "/" (unpack (toPathInfo key)))

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

instance HasImageKey ImageCached where
  imageKey (ImageCached x _) = imageKey x

instance HasImagePath ImageCached where
  imagePath (ImageCached key img) = ImagePath key (imageType img)

#if 0
let file = ImageCached {_imageCachedKey = ImageScaled (ImageSize {_dim = TheArea, _size = 15 % 1, _units = Inches}) (100 % 1) (ImageUpright (ImageOriginal "c3bd1388b41fa5d956e4308ce518a8bd" PNG)), _imageCachedFile = ImageFile {_imageFile = File {_fileSource = Nothing, _fileChksum = "be04a29700b06072326364fa1ce45f39", _fileMessages = [], _fileExt = ".jpg"}, _imageShape = ImageShape {_imageShapeType = JPEG, _imageShapeWidth = 885, _imageShapeHeight = 170}}
toURIPath file
"/image-path/image-scaled/image-size/the-area/15/1/inches/100/1/image-upright/image-original/c3bd1388b41fa5d956e4308ce518a8bd/i.png/i.jpg"
#endif

$(concat <$>
  sequence
  [ makePathInstances [FIELDS] ''ImageFile
  , makePathInstances [FIELDS] ''ImageReady
  , makePathInstances [FIELDS] ''ImageShape
  , makePathInstances [] ''ImageType
  , makePathInstances [FIELDS] ''ImageSize
  , makePathInstances [] ''Dimension
  , makePathInstances [FIELDS] ''ImageCrop
  , makePathInstances [FIELDS] ''ImageKey
  , makePathInstances [] ''Units
  , makePathInstances [] ''Rotation
  , makeValueInstance [NEWTYPE, VIEW] [t|SaneSize ImageSize|]
  , derivePathInfo ''ImagePath
  , derivePathInfo ''ImageKey
  , derivePathInfo ''ImageCrop
  , derivePathInfo ''ImageSize
  -- , derivePathInfo ''ImageType
  , derivePathInfo ''Dimension
  , derivePathInfo ''Units
  , derivePathInfo ''Rotation
  ])

instance PathInfo ImageType where
  toPathSegments inp =
    case inp of
      PPM -> [pack "i.ppm"]
      JPEG -> [pack "i.jpg"]
      GIF -> [pack "i.gif"]
      PNG -> [pack "i.png"]
      Unknown -> [pack "i.???"]
  fromPathSegments =
    (segment (pack "i.ppm") >> return PPM) <|>
    (segment (pack "i.jpg") >> return JPEG) <|>
    (segment (pack "i.gif") >> return GIF) <|>
    (segment (pack "i.png") >> return PNG) <|>
    (segment (pack "i.???") >> return Unknown)

{-
λ> toPathInfo (ImageOriginal "1c478f102062f2e0fd4b8147fb3bbfd0" JPEG)
"/image-original/1c478f102062f2e0fd4b8147fb3bbfd0"
λ> toPathInfo (ImageUpright (ImageOriginal "1c478f102062f2e0fd4b8147fb3bbfd0" JPEG))
"/image-upright/image-original/1c478f102062f2e0fd4b8147fb3bbfd0"
λ> toPathInfo (ImageScaled (ImageSize TheWidth 3 Inches) (1 % 3) (ImageOriginal "1c478f102062f2e0fd4b8147fb3bbfd0" JPEG))
"/image-scaled/image-size/the-width/3/1/inches/1/3/image-original/1c478f102062f2e0fd4b8147fb3bbfd0"
-}

#if 0
-- Ultimately we will need a custom PathInfo instance

pChecksum :: String.Parser Checksum
pChecksum = pack <$> many (noneOf ['.'])

pExtension :: String.Parser ImageType
pExtension = testExtension <$> many anyChar
  where
    testExtension :: String -> ImageType
    testExtension ".jpg" = JPEG
    testExtension ".png" = PNG
    testExtension ".pbm" = PPM
    testExtension ".pgm" = PPM
    testExtension ".ppm" = PPM
    testExtension ".pnm" = PPM
    testExtension ".gif" = GIF
    testExtension s = error ("testExtension " <> show s)

pImageKey :: String.Parser ImageKey
pImageKey = ImageOriginal <$> pChecksum <*> pExtension

instance HasImageType ImageKey => PathInfo ImageKey where
  toPathSegments (ImageOriginal csum typ) = [csum <> fileExtension typ]
  fromPathSegments = p2u pImageKey
#endif
