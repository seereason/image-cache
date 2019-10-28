{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, TemplateHaskell, UndecidableInstances #-}

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
    -- * PixmapShape
  , PixmapShape(..)
  , scaleFromDPI
  , widthInInches
  , widthInInches'
  , heightInInches

    -- * ImageCrop
  , ImageCrop(..)

    -- * ImageType
  , ImageType(..)
  , Extension
  , HasFileExtension(fileExtension)

    -- * File
  , File(..)
  , FileSource(..)
  , Checksum
  , HasFileChecksum(fileChecksum)
  , fileURI
  , filePath
  , fileDir
--  , addMessage
  , md5'

    -- * ImageFile
  , ImageFile(..)
  , imageFileArea

    -- * ImageKey
  , ImageKey(..)
  , OriginalKey(originalKey)
  , UprightKey(uprightKey)
  , EditedKey(editedKey)
  , ScaledKey(scaledKey)

--    -- * FileError
  , FileError(..)
  , CommandInfo(..)
  , HasFileError(fileErrorPrism), fromFileError, withFileError
--  , logErrorCall

    -- * FileCacheTop
  , FileCacheTop(..)
  , HasFileCacheTop(fileCacheTop)
  , CacheMap(..)
  ) where

import Control.Exception as E ( ErrorCall(ErrorCallWithLocation), Exception, fromException, SomeException )
import Control.Lens ( Iso', iso, Lens', lens, _Show, _2, view, over, preview, Prism', review )
import Control.Lens.Path ( HOP(FIELDS), makePathInstances, makeValueInstance, HOP(VIEW, NEWTYPE), View(..), newtypeIso )
import Control.Lens.Path.View ( viewIso )
import Control.Monad.Except ( ExceptT, lift )
import Control.Monad.RWS ( RWST )
import Control.Monad.Reader ( ReaderT )
import Control.Monad.Trans ( MonadIO(liftIO) )
import qualified Data.ByteString as P ( ByteString, take )
import qualified Data.ByteString.Lazy.Char8 as Lazy ( fromChunks )
import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.Digest.Pure.MD5 ( md5 )
import Data.Generics.Product ( field )
import Data.Map ( Map )
import Data.Monoid ( (<>) )
import Data.Ratio ( (%), approxRational, denominator, numerator )
import Data.SafeCopy ( deriveSafeCopy, base, SafeCopy', extension, Migrate(..), SafeCopy(..), safeGet, safePut )
import Data.Serialize ( Serialize(..) )
import Data.String (IsString(fromString))
import Data.Text ( pack, Text, unpack )
import Data.Typeable ( Typeable )
import Extra.Except ( HasIOException(..) )
import GHC.Generics ( Generic, M1(M1) )
import Language.Haskell.TH ( Loc(..) )
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ( Lift )
import Language.Haskell.TH.Syntax ( Loc(loc_module) )
import Network.URI ( URI(..), parseRelativeReference, parseURI )
import Numeric ( fromRat, readSigned, readFloat, showSigned, showFFloat )
import System.FilePath ( makeRelative, (</>) )
import System.Log.Logger ( Priority(ERROR), logM )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Text.PrettyPrint.HughesPJClass ()
import Text.Read ( readMaybe )
import Web.Routes ( PathInfo(..) )
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

instance View Rational where type ViewType Rational = Text; _View = rationalIso . iso pack unpack

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
      } deriving (Generic, Eq, Ord)

instance Default ImageSize where
    def = ImageSize TheArea 15.0 Inches
instance SafeCopy ImageSize where version = 2
instance Serialize ImageSize where get = safeGet; put = safePut
deriving instance Data ImageSize
deriving instance Read ImageSize
deriving instance Show ImageSize
deriving instance Typeable ImageSize

data Dimension
    = TheHeight
    | TheWidth
    | TheArea
    deriving (Generic, Eq, Ord, Enum, Bounded)

instance View Dimension where type ViewType Dimension = Text; _View = viewIso _Show TheHeight . iso pack unpack
instance SafeCopy Dimension where version = 1
instance Serialize Dimension where get = safeGet; put = safePut
deriving instance Data Dimension
deriving instance Read Dimension
deriving instance Show Dimension
deriving instance Typeable Dimension

data Units
    = Inches
    | Cm
    | Points
    deriving (Generic, Eq, Ord, Enum, Bounded)

instance View Units where type ViewType Units = Text; _View = viewIso _Show Inches . iso pack unpack
instance SafeCopy Units where version = 0
instance Serialize Units where get = safeGet; put = safePut
deriving instance Data Units
deriving instance Read Units
deriving instance Show Units
deriving instance Typeable Units

class HasImageSize a where imageSize :: a -> ImageSize
instance HasImageSize ImageSize where imageSize = id

-- | A wrapper type to suggest that lens_saneSize has been applied to
-- the ImageSize within.
newtype SaneSize a = SaneSize {_unSaneSize :: a} deriving (Generic, Eq, Ord)

instance (SafeCopy a, Typeable a) => SafeCopy (SaneSize a) where version = 1
instance (SafeCopy a, Typeable a) => Serialize (SaneSize a) where get = safeGet; put = safePut
deriving instance Data a => Data (SaneSize a)
deriving instance Read a => Read (SaneSize a)
deriving instance Show a => Show (SaneSize a)
deriving instance Typeable (SaneSize a)

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

-- * PixmapShape

-- | A class whose primary (only?) instance is ImageFile.  Access to
-- the original dimensions of the image, so we can compute the aspect
-- ratio.
class PixmapShape a where
    pixmapHeight :: a -> Int
    pixmapWidth :: a -> Int
    pixmapMaxVal :: a -> Int

instance PixmapShape (Int, Int) where
  pixmapWidth (w, _) = w
  pixmapHeight (_, h) = h
  pixmapMaxVal _ = 255 -- whatever

-- |Given the desired DPI and image dimensions, return the factor by
-- which an image should be scaled.  Result of Nothing means the scale
-- is pathological.
scaleFromDPI :: PixmapShape a => ImageSize -> Rational -> a -> Maybe Rational
scaleFromDPI sz dpi file =
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

-- * ImageCrop

-- |This describes the cropping and rotation of an image.
data ImageCrop
    = ImageCrop
      { topCrop :: Int
      , bottomCrop :: Int
      , leftCrop :: Int
      , rightCrop :: Int
      , rotation :: Int         -- 0, 90, 180, 270
      } deriving (Generic, Eq, Ord)

instance Default ImageCrop where def = ImageCrop 0 0 0 0 0
instance SafeCopy ImageCrop where version = 0
instance Serialize ImageCrop where get = safeGet; put = safePut
deriving instance Data ImageCrop
deriving instance Read ImageCrop
deriving instance Show ImageCrop
deriving instance Typeable ImageCrop

instance Pretty ImageCrop where
    pPrint (ImageCrop t b l r rot) = text $ "(crop " <> show (b, l) <> " -> " <> show (t, r) <> ", rot " ++ show rot ++ ")"

-- * ImageType and Checksum

data ImageType = PPM | JPEG | GIF | PNG deriving (Generic, Eq, Ord)

deriving instance Data ImageType
deriving instance Read ImageType
deriving instance Show ImageType
deriving instance Typeable ImageType
instance Serialize ImageType where get = safeGet; put = safePut
instance SafeCopy ImageType where version = 0
instance Pretty ImageType where pPrint = text . show

type Extension = Text

class HasFileExtension a where fileExtension :: a -> Extension
instance HasFileExtension Extension where fileExtension = id

instance HasFileExtension ImageType where
  fileExtension JPEG = ".jpg"
  fileExtension PPM = ".ppm"
  fileExtension GIF = ".gif"
  fileExtension PNG = ".png"

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
    pPrint (File _ cksum _ ext) = text ("File(" <> show (cksum <> ext) <> ")")
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

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: File -> Maybe URI
fileURI file = case _fileSource file of
                 Just (TheURI uri) -> maybe (parseRelativeReference uri) Just (parseURI uri)
                 _ -> Nothing

-- |Add a message to the file message list.
addMessage :: String -> File -> File
addMessage message file = over (field @"_fileMessages") (++ [message]) file

filePath :: (HasFileExtension a, HasFileChecksum a) => a -> FilePath
filePath file = fileDir file <++> unpack (fileChecksum file) <> unpack (fileExtension file)

fileDir :: HasFileChecksum a => a -> FilePath
fileDir = take 2 . unpack . fileChecksum

-- feels like </> but I think its a little different
(<++>) :: FilePath -> FilePath -> FilePath
a <++> b = a </> (makeRelative "" b)

md5' :: P.ByteString -> String
#ifdef LAZYIMAGES
md5' = show . md5
#else
md5' = show . md5 . Lazy.fromChunks . (: [])
#endif

#if ARBITRARY
instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure [] <*> arbitrary

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]
#endif

-- * ImageFile

-- | A file containing an image plus meta info.
data ImageFile
    = ImageFile
      { _imageFile :: File
      , _imageFileType :: ImageType
      , _imageFileWidth :: Int
      , _imageFileHeight :: Int
      , _imageFileMaxVal :: Int
      } deriving (Generic, Eq, Ord)

deriving instance Data ImageFile
deriving instance Read ImageFile
deriving instance Show ImageFile
deriving instance Typeable ImageFile
instance Serialize ImageFile where get = safeGet; put = safePut
instance SafeCopy ImageFile where version = 1
instance View (Maybe ImageFile) where
  type ViewType (Maybe ImageFile) = String
  _View = iso (maybe "" show) readMaybe

instance PixmapShape ImageFile where
    pixmapHeight = _imageFileHeight
    pixmapWidth = _imageFileWidth
    pixmapMaxVal = _imageFileMaxVal

instance Pretty ImageFile where
    pPrint (ImageFile f typ w h _mx) = text "ImageFile(" <> pPrint f <> text (" " <> show w <> "x" <> show h <> " " <> show typ <> ")")

-- |Return the area of an image in square pixels.
imageFileArea :: ImageFile -> Int
imageFileArea image = _imageFileWidth image * _imageFileHeight image

-- * ImageKey

-- | Describes an ImageFile and, if it was derived from other image
-- files, how.
data ImageKey
    = ImageOriginal Checksum
    -- ^ An unmodified upload, the info lets us construct an URL
    | ImageCropped ImageCrop ImageKey
    -- ^ A cropped version of another image
    | ImageScaled ImageSize Rational ImageKey
    -- ^ A resized version of another image
    | ImageUpright ImageKey
    -- ^ Image uprighted using the EXIF orientation code, see  "Appraisal.Exif"
    deriving (Generic, Eq, Ord)

instance Migrate ImageKey where
  type MigrateFrom ImageKey = ImageKey_2
  migrate (ImageOriginal_2 i) = ImageOriginal (_fileChksum f) where f = _imageFile i
  migrate (ImageCropped_2 crop key) = ImageCropped crop key
  migrate (ImageScaled_2 size dpi key) = ImageScaled size dpi key
  migrate (ImageUpright_2 key) = ImageUpright key

data ImageKey_2
    = ImageOriginal_2 ImageFile
    | ImageCropped_2 ImageCrop ImageKey
    | ImageScaled_2 ImageSize Rational ImageKey
    | ImageUpright_2 ImageKey
    deriving (Generic, Eq, Ord)

deriving instance Data ImageKey
deriving instance Read ImageKey
deriving instance Show ImageKey
deriving instance Typeable ImageKey
instance Serialize ImageKey where get = safeGet; put = safePut
instance SafeCopy ImageKey_2 where version = 2
instance SafeCopy ImageKey where version = 3; kind = extension

instance Pretty ImageKey where
    pPrint (ImageOriginal _) = text "ImageOriginal"
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> text ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled sz dpi x) = text "Scale (" <> pPrint sz <> text " @" <> text (showRational dpi) <> text " dpi) (" <> pPrint x <> text ")"

-- | This describes how the keys we use are constructed
class OriginalKey a where
  originalKey :: a -> ImageKey
instance OriginalKey Checksum where -- danger - Checksum is just String
  originalKey = ImageOriginal
instance OriginalKey ImageFile where
  originalKey = originalKey . _imageFile
instance OriginalKey File where
  originalKey f = ImageOriginal (_fileChksum f)

class UprightKey a where
  uprightKey :: a -> ImageKey
instance UprightKey ImageFile where
  uprightKey img = ImageUpright (originalKey img)

class EditedKey a where
  editedKey :: a -> ImageKey
instance EditedKey ImageFile where
  editedKey img = uprightKey img

class HasImageSize size => ScaledKey size a where
  scaledKey :: size -> Rational -> a -> ImageKey
instance ScaledKey ImageSize ImageFile where
  scaledKey size dpi x = ImageScaled (imageSize size) dpi (editedKey x)

-- * FileError, CommandInfo

-- | It would be nice to store the actual IOException and E.ErrorCall,
-- but then the FileError type wouldn't be serializable.
data FileError
    = IOException {-IOException-} Text -- ^ Caught an IOException
    | ErrorCall {-E.ErrorCall-} Text -- ^ Caught a call to error
    | CommandFailure CommandInfo -- ^ A shell command failed
    | CacheDamage Text -- ^ The contents of the cache is wrong
    | NoShape
      -- ^ Could not determine the dimensions of an image.  This comes
      -- from failed attempt to parse the output of the unix file(1)
      -- command.
    deriving (Eq, Ord, Generic)

-- Dubious instance, but omitting makes other things more dubious.
instance IsString FileError where fromString = ErrorCall . pack

instance Migrate FileError where
  type MigrateFrom FileError = FileError_1
  migrate (IOException_1 t) = IOException t
  migrate (ErrorCall_1 t) = ErrorCall t
  migrate (CommandFailure_1 info) = CommandFailure info
  migrate CacheDamage_1 = CacheDamage ""

data FileError_1
    = IOException_1 Text
    | ErrorCall_1 Text
    | CommandFailure_1 CommandInfo
    | CacheDamage_1
    deriving (Eq, Ord, Generic)

instance Exception FileError
instance SafeCopy CommandInfo where version = 1
instance SafeCopy FileError_1 where version = 1
instance SafeCopy FileError where version = 2; kind = extension

instance Serialize CommandInfo where get = safeGet; put = safePut
instance Serialize FileError where get = safeGet; put = safePut

deriving instance Data FileError
deriving instance Data CommandInfo
deriving instance Show FileError
deriving instance Show CommandInfo

-- | This ensures that runExceptT catches IOException
instance HasIOException FileError where fromIOException = IOException . pack . show

class HasIOException e => HasFileError e where fileErrorPrism :: Prism' e FileError
instance HasFileError FileError where fileErrorPrism = id

fromFileError :: HasFileError e => FileError -> e
fromFileError = review fileErrorPrism

withFileError :: HasFileError e => (Maybe FileError -> r) -> e -> r
withFileError f = f . preview fileErrorPrism

-- | Information about a shell command that failed.  This is
-- recursive so we can include as much or as little as desired.
data CommandInfo
    = Command Text Text -- ^ CreateProcess and ExitCode
    | CommandInput P.ByteString CommandInfo -- ^ command input
    | CommandOut P.ByteString CommandInfo -- ^ stdout
    | CommandErr P.ByteString CommandInfo -- ^ stderr
    | FunctionName String CommandInfo -- ^ The function that ran the command
    | Description String CommandInfo -- ^ free form description of what happened
    deriving (Eq, Ord, Generic)

class Loggable a where
  logit :: Priority -> Loc -> a -> IO ()

instance Loggable FileError where
  logit priority loc (IOException e) = (logM (loc_module loc) priority (" - IO exception: " <> unpack e))
  logit priority loc (ErrorCall e) = (logM (loc_module loc) priority (" - error call: " <> show e))
  logit priority loc (CommandFailure info) = (logM (loc_module loc) priority " - shell command failed:" >> logCommandInfo priority loc info)
  logit priority loc (CacheDamage t) = logM (loc_module loc) priority (" - file cache is damaged: " <> unpack t)

logCommandInfo :: Priority -> Loc -> CommandInfo -> IO ()
logCommandInfo priority loc (Description s e) = logM (loc_module loc) priority (" - error description: " <> s) >> logCommandInfo priority loc e
logCommandInfo priority loc (FunctionName n e) = logM (loc_module loc) priority (" - error function " <> n) >> logCommandInfo priority loc e
logCommandInfo priority loc (Command cmd code) = logM (loc_module loc) priority (" - command: " <> show cmd <> ", exit code: " <> show code)
logCommandInfo priority loc (CommandInput bs e) = logM (loc_module loc) priority (" - command input: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e
logCommandInfo priority loc (CommandOut bs e) = logM (loc_module loc) priority (" - command stdout: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e
logCommandInfo priority loc (CommandErr bs e) = logM (loc_module loc) priority (" - command stderr: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e

logErrorCall :: MonadIO m => m (Either SomeException a) -> m (Either SomeException a)
logErrorCall x =
    x >>= either (\e -> case fromException e :: Maybe E.ErrorCall of
                          Just (ErrorCallWithLocation msg loc) ->
                              liftIO (logM "Appraisal.FileError" ERROR (show loc ++ ": " ++ msg)) >> return (Left e)
                          _ -> return (Left e)) (return . Right)

-- * FileCacheTop

newtype FileCacheTop = FileCacheTop {_unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- directory of a file cache.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FileCacheTop

instance (Monad m, Monoid w) => HasFileCacheTop (RWST (acid, FileCacheTop) w s m) where
    fileCacheTop = view _2

instance Monad m => HasFileCacheTop (ReaderT (acid, FileCacheTop) m) where
    fileCacheTop = view _2

instance HasFileCacheTop m => HasFileCacheTop (ExceptT e m) where
    fileCacheTop = lift fileCacheTop

-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
data CacheMap key val =
    CacheMap {_unCacheMap :: Map key (Either FileError val)}
    deriving (Generic, Eq, Ord)

instance (Ord key, SafeCopy' key, SafeCopy' val) => SafeCopy (CacheMap key val) where
  version = 3
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

instance (Ord key, SafeCopy' key, SafeCopy' val) => Migrate (CacheMap key val) where
  type MigrateFrom (CacheMap key val) = CacheMap_2 key val
  migrate (CacheMap_2 mp) =
    CacheMap (fmap (\case Value_1 a -> Right a; Failed_1 e -> Left e; _ -> error "Migrate CacheMap") mp)

data CacheMap_2 key val =
    CacheMap_2 {_unCacheMap_2 :: Map key (CacheValue_1 val)}
    deriving (Generic, Eq, Ord)

instance (Ord key, SafeCopy' key, SafeCopy' val) => SafeCopy (CacheMap_2 key val) where
  version = 2
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap_2"

deriving instance (Show key, Show val) => Show (CacheMap key val)

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap_2 key val) where
    type MigrateFrom (CacheMap_2 key val) = Map key val
    migrate mp = CacheMap_2 (fmap Value_1 mp)

data CacheValue_1 val
    = InProgress_1
    | Value_1 val
    | Failed_1 FileError
    deriving (Generic, Eq, Ord, Functor)

deriving instance Show val => Show (CacheValue_1 val)

$(deriveSafeCopy 1 'base ''CacheValue_1)
#if 0

__LOC__ :: Q Exp
__LOC__ = TH.lift =<< location

logAndThrow :: (MonadError e m, MonadIO m, Show e) => String -> Priority -> e -> m b
logAndThrow m p e = liftIO (logM m p ("logAndThrow - " ++ show e)) >> throwError e

-- | Create an expression of type (MonadIO m => Priority -> m a -> m a) that we can
-- apply to an expression so that it catches, logs, and rethrows any
-- exception.
logException :: ExpQ
logException =
    [| \priority action ->
         action `catchError` (\e -> do
                                liftIO (logM (loc_module $__LOC__)
                                             priority
                                             ("Logging exception: " <> (pprint $__LOC__) <> " -> " ++ show e))
                                throwError e) |]

logExceptionV :: ExpQ
logExceptionV =
    [| \priority action ->
         action `catchError` (\e -> do
                                liftIO (logM (loc_module $__LOC__)
                                             priority
                                             ("Logging exception: " <> (pprint $__LOC__) <> " -> " ++ show (V e)))
                                throwError e) |]

type CacheValue val = Either FileError val

type CacheImage = CacheValue ImageFile
type ImageCacheMap = Map ImageKey ImageFile

-- | Remove null crops
fixKey :: ImageKey -> ImageKey
fixKey key@(ImageOriginal _) = key
fixKey (ImageCropped crop key) | crop == def = fixKey key
fixKey (ImageCropped crop key) = ImageCropped crop (fixKey key)
fixKey (ImageScaled sz dpi key) = ImageScaled sz dpi (fixKey key)
fixKey (ImageUpright key) = ImageUpright (fixKey key)
#endif

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
  , derivePathInfo ''ImageKey
  , derivePathInfo ''ImageCrop
  , derivePathInfo ''ImageSize
  , derivePathInfo ''ImageType
  , derivePathInfo ''Dimension
  , derivePathInfo ''Units
  ])

{-
λ> toPathInfo (ImageOriginal "1c478f102062f2e0fd4b8147fb3bbfd0")
"/image-original/1c478f102062f2e0fd4b8147fb3bbfd0"
λ> toPathInfo (ImageUpright (ImageOriginal "1c478f102062f2e0fd4b8147fb3bbfd0"))
"/image-upright/image-original/1c478f102062f2e0fd4b8147fb3bbfd0"
λ> toPathInfo (ImageScaled (ImageSize TheWidth 3 Inches) (1 % 3) (ImageOriginal "1c478f102062f2e0fd4b8147fb3bbfd0"))
"/image-scaled/image-size/the-width/3/1/inches/1/3/image-original/1c478f102062f2e0fd4b8147fb3bbfd0"
-}
