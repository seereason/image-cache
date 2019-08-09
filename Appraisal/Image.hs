-- | Pure functions to deal with image data.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Appraisal.Image
    ( ImageSize(..), dim, size, units
    , approx
    , rationalIso
    , rationalLens
    , ImageCrop(..)
    , Dimension(..)
    , Units(..)
    , ImageFile(..), imageFile, imageFileType, imageFileWidth, imageFileHeight, imageFileMaxVal
    , imageFileArea
    , PixmapShape(..)
    , ImageType(..)
    , fileExtension
    , ImageKey(..)
    , ImageCacheMap
    , scaleFromDPI
    , widthInInches
    , widthInInches'
    , heightInInches
    , saneSize
    , SaneSize(..), unSaneSize
    , defaultSize
    , fixKey
    , readRationalMaybe
    , showRational
#if !__GHCJS__
    , getFileType
    -- * validation tools
    , validateJPG
    , tests
#endif
    ) where

import Appraisal.FileCache (File(..))
import Control.Lens (Iso', iso, Lens', lens, makeLenses, _Show)
import Control.Lens.Path
import Control.Lens.Path.View (viewIso)
--import Control.Monad.Except (catchError)
#if !__GHCJS__
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString.UTF8 as P
#endif
#endif
import Data.Default (Default(def))
import Data.Generics (Data, Typeable)
import Data.Map (Map)
--import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%), approxRational)
import Data.SafeCopy (SafeCopy(..), safeGet, safePut)
import Data.Serialize (Serialize(..))
import Data.Text (pack, Text, unpack)
import GHC.Generics (Generic)
import Language.Haskell.TH (Ppr(ppr))
import Language.Haskell.TH.PprLib (ptext)
import Numeric (fromRat, readSigned, readFloat, showSigned, showFFloat)
#if !__GHCJS__
import Control.Lens (_2, view)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.TH.Lift (Lift)
import System.Exit (ExitCode)
import System.Process (proc{-, showCommandForUser-})
import System.Process.ListLike (readCreateProcess, readProcessWithExitCode)
import System.Process.ByteString ()
import Test.HUnit (assertEqual, Test(..))
import Test.QuickCheck (Arbitrary(..), choose, elements, Gen, oneof)
import "regex-compat-tdfa" Text.Regex (Regex, mkRegex, matchRegex)
#endif
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

import Text.Parsec
import Text.Read (readMaybe)
import Data.Char (isSpace)

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

#if 0
$(makePathInstances [] ''Rational)
#else
instance Value Rational where
  ixValue_ _ _ _ = error "_ixHop Rational"
  atValue_ _ _ _ = error "_atHop Rational"
  nonValue_ _ _ _ = error "_nonHop Rational"
  recValue_ _ _ _ = error "_fPos Rational"
  ctorValue_ _ _ _ = error "_fPos Rational"
  newtypeValue_ _ _ = error "_newtypeHop Rational"
  viewValue_ _ _ = error "_viewHop Rational"
  ixedValue_ _ _ = error "ixedValue_ Rational"
  orderValue_ _ _ = error "_orderHop Rational"
#endif

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
-- which an image should be scaled.
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

#if !__GHCJS__
tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]
#endif

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

#if !__GHCJS__
-- | Helper function to learn the 'ImageType' of a file by runing
-- @file -b@.
getFileType :: P.ByteString -> IO ImageType
getFileType bytes =
    readProcessWithExitCode cmd args bytes >>= test . view _2
    where
      cmd = "file"
      args = ["-b", "-"]
      test :: Monad m => P.ByteString -> m ImageType
      test s = maybe (fail $ "ImageFile.getFileType - Not an image: (Ident string: " ++ show s ++ ")") return (foldr (testre (P.toString s)) Nothing reTests)
      testre :: String -> (Regex, ImageType) -> Maybe ImageType -> Maybe ImageType
      testre _ _ (Just result) = Just result
      testre s (re, typ) Nothing = maybe Nothing (const (Just typ)) (matchRegex re s)
      -- Any more?
      reTests =
              [(mkRegex "Netpbm P[BGPP]M \"rawbits\" image data$", PPM)
              ,(mkRegex "JPEG image data", JPEG)
              ,(mkRegex "PNG image data", PNG)
              ,(mkRegex "GIF image data", GIF)]
#endif

instance PixmapShape ImageFile where
    pixmapHeight = _imageFileHeight
    pixmapWidth = _imageFileWidth
    pixmapMaxVal = _imageFileMaxVal

instance Pretty ImageFile where
    pPrint (ImageFile f typ w h _mx) = text "ImageFile(" <> pPrint f <> text (" " <> show w <> "x" <> show h <> " " <> show typ <> ")")

-- |Return the area of an image in square pixels.
imageFileArea :: ImageFile -> Int
imageFileArea image = _imageFileWidth image * _imageFileHeight image

fileExtension :: ImageType -> String
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

type ImageCacheMap = Map ImageKey ImageFile

#if !__GHCJS__
instance Arbitrary Units where
    arbitrary = elements [Inches, Cm, Points]

instance Arbitrary ImageType where
    arbitrary = elements [PPM, JPEG, GIF, PNG]

instance Arbitrary Dimension where
    arbitrary = oneof [pure TheHeight, pure TheWidth, pure TheArea]

instance Arbitrary ImageSize where
    arbitrary = ImageSize <$> arbitrary <*> ((% 100) <$> (choose (1,10000) :: Gen Integer)) <*> arbitrary

instance Arbitrary a => Arbitrary (SaneSize a) where
    arbitrary = SaneSize <$> arbitrary

instance Arbitrary ImageFile where
    arbitrary = ImageFile <$> arbitrary
                          <*> arbitrary
                          <*> choose (1,5000)
                          <*> choose (1,5000)
                          <*> choose (1,255)

instance Arbitrary ImageCrop where
    arbitrary = ImageCrop <$> choose (0,100)
                          <*> choose (0,100)
                          <*> choose (0,100)
                          <*> choose (0,100)
                          <*> elements [0, 90, 180, 270]

instance Arbitrary ImageKey where
    arbitrary = oneof [ ImageOriginal <$> arbitrary
                      , ImageCropped <$> arbitrary <*> arbitrary
                      , ImageScaled <$> arbitrary <*> arbitrary <*> arbitrary
                      , ImageUpright <$> arbitrary ]
#endif

-- | Remove null crops
fixKey :: ImageKey -> ImageKey
fixKey key@(ImageOriginal _) = key
fixKey (ImageCropped crop key) | crop == def = fixKey key
fixKey (ImageCropped crop key) = ImageCropped crop (fixKey key)
fixKey (ImageScaled sz dpi key) = ImageScaled sz dpi (fixKey key)
fixKey (ImageUpright key) = ImageUpright (fixKey key)

data Format = Binary | Gray | Color
data RawOrPlain = Raw | Plain
data Pnmfile = Pnmfile Format RawOrPlain (Integer, Integer, Maybe Integer)

#if !__GHCJS__
-- | Check whether the outputs of extractbb is valid by comparing it
-- to the output of pnmfile.
validateJPG :: FilePath -> IO (Either String (Integer, Integer))
validateJPG path = do
  (_code, bs, _) <- readCreateProcess (proc "jpegtopnm" [path]) mempty :: IO (ExitCode, P.ByteString, P.ByteString)
  (_code, s1', _) <- readCreateProcess (proc "pnmfile" []) bs :: IO (ExitCode, P.ByteString, P.ByteString)
  let s1 = decodeUtf8 s1'
  case parse parsePnmfileOutput path s1 of
    Left e -> return (Left ("Error parsing " ++ show s1 ++ ": " ++ show e))
    Right (Pnmfile _ _ (w, h, _)) -> do
      (_code, s2, _) <- readCreateProcess (proc "extractbb" ["-O", path]) ("" :: Text) :: IO (ExitCode, Text, Text)
      case parse parseExtractBBOutput path s2 of
        Left e -> return (Left ("Error parsing " ++ show s2 ++ ": " ++ show e))
        Right (ExtractBB (l, t, r, b) _) ->
          if l /= 0 || t /= 0 || r < 1 || b < 1 || r > 1000000 || b > 1000000
          then return (Left (path ++ ": image data error\n\npnmfile ->\n" ++ show s1 ++ "\nextractbb ->\n" ++ show s2))
          else return (Right (w, h))
#endif

-- | Parse the output of the pnmfile command (based on examination of
-- its C source code.)
parsePnmfileOutput :: Parsec Text () Pnmfile
parsePnmfileOutput = do
      _ <- char 'P'
      format <- (\c -> case c of
                         'B' -> Binary
                         'G' -> Gray
                         'P' -> Color
                         _ -> error "parser failure") <$> oneOf "BGP"
      _ <- string "M "
      rawOrPlain <- (\s -> case s of
                             "plain" -> Plain
                             "raw" -> Raw
                             _ -> error "parser failure") <$> (string "plain" <|> string "raw")
      _ <- string ", "
      w <- many1 (oneOf "-0123456789")
      _ <- string " by "
      h <- many1 (oneOf "-0123456789")
      _ <- spaces
      mv <- optionMaybe (string "maxval " >> many1 digit)
      _ <- newline
      return $ Pnmfile format rawOrPlain (read w, read h, fmap read mv)

data ExtractBB =
    ExtractBB (Integer, Integer, Integer, Integer)
              (Hires, Hires, Hires, Hires)

data Hires = Inf | Rational Rational

-- | Parse the output of extractbb (based on trial and error.)
parseExtractBBOutput :: Parsec Text () ExtractBB
parseExtractBBOutput = do
  _ <- title
  _ <- creator
  bb <- boundingBox
  hbb <- hiResBoundingBox
  creationDate
  return $ ExtractBB bb hbb
    where
      title :: Parsec Text () String
      title = string "%%Title:" >> spaces >> many (noneOf "\n") >>= \r -> newline >> return r

      creator = string "%%Creator:" >> spaces >> many (noneOf "\n") >> newline

      boundingBox :: Parsec Text () (Integer, Integer, Integer, Integer)
      boundingBox = do
        _ <- string "%%BoundingBox:"
        spaces
        l <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        t <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        r <- many1 (satisfy (not . isSpace))
        _ <- many1 space
        b <- many1 (satisfy (not . isSpace))
        _ <- many newline
        return (read l, read t, read r, read b)

      hiResBoundingBox :: Parsec Text () (Hires, Hires, Hires, Hires)
      hiResBoundingBox = do
        _ <- string "%%HiResBoundingBox:"
        spaces
        (l :: Hires) <- (const Inf <$> string "inf") <|> (many1 (satisfy (not . isSpace)) >>= readRationalMaybe >>= return . Rational)
        _ <- many1 space
        t <- (const Inf <$> string "inf") <|> (many1 (satisfy (not . isSpace)) >>= readRationalMaybe >>= return . Rational)
        _ <- many1 space
        r <- (const Inf <$> string "inf") <|> (many1 (satisfy (not . isSpace)) >>= readRationalMaybe >>= return . Rational)
        _ <- many1 space
        b <- (const Inf <$> string "inf") <|> (many1 (satisfy (not . isSpace)) >>= readRationalMaybe >>= return . Rational)
        _ <- newline
        maybe (fail "") return (pure (l, t, r, b))

      creationDate :: Parsec Text () ()
      creationDate = string "%%CreationDate:" >> many (noneOf "\n") >> newline >> return ()

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
deriving instance Show Format
deriving instance Show RawOrPlain
deriving instance Show Pnmfile
deriving instance Show ExtractBB
deriving instance Show Hires

deriving instance Typeable ImageKey
deriving instance Typeable ImageType
deriving instance Typeable ImageSize
deriving instance Typeable Dimension
deriving instance Typeable Units
deriving instance Typeable ImageCrop
deriving instance Typeable (SaneSize a)
deriving instance Typeable ImageFile

#if !__GHCJS__
deriving instance Lift ImageFile
deriving instance Lift ImageType
deriving instance Lift ImageKey
deriving instance Lift ImageSize
deriving instance Lift Units
deriving instance Lift ImageCrop
deriving instance Lift Dimension
deriving instance Lift a => Lift (SaneSize a)
#endif

$(concat <$>
  sequence
  [ makeLenses ''ImageFile
  , makeLenses ''ImageSize
  , makeLenses ''SaneSize
  ])

instance View (SaneSize ImageSize) where
    type ViewType (SaneSize ImageSize) = ImageSize
    _View = unSaneSize

instance View (Maybe ImageFile) where type ViewType (Maybe ImageFile) = String; _View = iso (maybe "" show) readMaybe

$(concat <$>
  sequence
  [ makePathInstances [FIELDS] ''ImageFile
  , makePathInstances [FIELDS] ''ImageType
  , makePathInstances [FIELDS] ''ImageSize
  , makePathInstances [FIELDS] ''Dimension
  , makePathInstances [FIELDS] ''ImageCrop
  , makePathInstances [FIELDS] ''ImageKey
  , makePathInstances [FIELDS] ''Units
  ])

#if 0
$(makePathInstances [NEWTYPE, VIEW] ''SaneSize)
#else
instance Value ImageSize => Value (SaneSize ImageSize) where
  ixValue_ _ _ _ = error "ixValue_"
  atValue_ _ _ _ = error "atValue_"
  nonValue_ _ _ _ = error "nonValue_"
  recValue_ _ _ _ = error "recValue_"
  ctorValue_ _ _ _ = error "recValue_"
  ixedValue_ _ _ = error "orderValue_"
  orderValue_ _ _ = error "orderValue_"
#endif
