-- | Pure functions to deal with image data.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Appraisal.Image
    ( ImageSize(..), ImageSize_1(..)
    , approx
    , rationalIso
    , rationalLens
    , ImageCrop(..)
    , Dimension_0(..)
    , Dimension(..)
    , Units(..)
    , ImageFile(..), imageFile, imageFileType, imageFileWidth, imageFileHeight, imageFileMaxVal
    , ImageFile_0(..)
    , imageFileArea
    , PixmapShape(..)
    , ImageType(..)
    , getFileType
    , fileExtension
    , ImageKey(..)
    , ImageKey_1(..)
    , ImageCacheMap
    , scaleFromDPI
    , widthInInches
    , widthInInches'
    , heightInInches
    , saneSize
    , SaneSize(unSaneSize)
    , defaultSize
    , fixKey
    , tests
    , readRationalMaybe
    , showRational
    -- * validation tools
    , validateJPG
    ) where

import Appraisal.FileCache (File(..))
--import Appraisal.Utils.ErrorWithIO (logException)
import Control.Lens (_2, Iso', iso, Lens', lens, makeLenses, view)
--import Control.Monad.Except (catchError)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString.UTF8 as P
#endif
import Data.Default (Default(def))
import Data.Generics (Data, Typeable)
import Data.Map (Map)
--import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.Ratio ((%), approxRational)
import Data.SafeCopy (base, deriveSafeCopy, extension, Migrate(..))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.THUnify.Serialize (deriveSerialize)
import Language.Haskell.TH (Ppr(ppr))
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.PprLib (ptext)
import Numeric (fromRat, readSigned, readFloat, showSigned, showFFloat)
import System.Exit (ExitCode)
import System.Process (proc{-, showCommandForUser-})
import System.Process.ListLike (readCreateProcess, readProcessWithExitCode)
import System.Process.ByteString ()
import Test.HUnit (assertEqual, Test(..))
import Test.QuickCheck (Arbitrary(..), choose, elements, Gen, oneof)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Text.Regex (Regex, mkRegex, matchRegex)

import Data.Maybe (catMaybes)
import Text.Parsec
import Data.Char (isSpace)

-- |This can describe an image size in various ways.
data ImageSize_1
    = ImageSize_1
      { _dim_1 :: Dimension
      , _size_1 :: Double
      , _units_1 :: Units
      } deriving (Show, Read, Eq, Ord, Typeable, Data)

instance Migrate ImageSize where
    type MigrateFrom ImageSize = ImageSize_1
    migrate (ImageSize_1 d s u) =
        -- We want a ratio which approximates the double
        -- to about four significant digits.
        ImageSize d s' u
        where
          s' :: Rational
          s' = approx (toRational s)

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

-- mapRatio :: (Integral a, Integral b) => (a -> b) -> Ratio a -> Ratio b
-- mapRatio f r = f (numerator r) % f (denominator r)

data ImageSize
    = ImageSize
      { dim :: Dimension
      , size :: Rational
      , units :: Units
      } deriving (Show, Read, Eq, Ord, Typeable, Data)

instance Default ImageSize where
    def = ImageSize TheArea 15.0 Inches

data Dimension_0
    = TheHeight_0
    | TheWidth_0
    | TheArea_0
    | Invalid String
    deriving (Show, Read, Eq, Ord, Typeable, Data)

data Dimension
    = TheHeight
    | TheWidth
    | TheArea
    deriving (Show, Read, Eq, Ord, Typeable, Data)

instance Migrate Dimension where
    type MigrateFrom Dimension = Dimension_0
    migrate TheHeight_0 = TheHeight
    migrate TheWidth_0 = TheWidth
    migrate TheArea_0 = TheArea
    migrate (Invalid _) = error "Unexpected Dimension_0"

data Units
    = Inches
    | Cm
    | Points
    deriving (Show, Read, Eq, Ord, Typeable, Data, Enum, Bounded)

-- |This describes the cropping and rotation of an image.
data ImageCrop
    = ImageCrop
      { topCrop :: Int
      , bottomCrop :: Int
      , leftCrop :: Int
      , rightCrop :: Int
      , rotation :: Int         -- 0, 90, 180, 270
      } deriving (Show, Read, Eq, Ord, Typeable, Data)

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
    case dim sz of
      _ | size sz < 0.000001 || size sz > 1000000.0 -> Nothing
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
    case dim s of
      TheWidth -> toInches (units s) (size s)
      TheHeight -> widthInInches p (s {dim = TheWidth, size = approx (size s / r)})
      TheArea -> widthInInches p (s {dim = TheWidth, size = approx (rsqrt (size s / r))})
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
    case dim s of
      TheHeight -> toInches (units s) (size s)
      TheWidth -> heightInInches p (s {dim = TheHeight, size = approx (size s / r)})
      TheArea -> heightInInches p (s {dim = TheHeight, size = approx (rsqrt (size s / r))})
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
widthInInches' p s = s {units = Inches, size = approx (widthInInches p s), dim = TheWidth}

saneSize :: ImageSize -> SaneSize ImageSize
saneSize sz = SaneSize $
    case (dim sz, inches sz) of
      (TheArea, n) | n < minArea -> sz {units = Inches, size = minArea}
      (TheArea, n) | n > maxArea -> sz {units = Inches, size = maxArea}
      (_, n) | n < minDist -> sz {units = Inches, size = toRational minDist}
      (_, n) | n > maxDist -> sz {units = Inches, size = maxDist}
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
newtype SaneSize a = SaneSize {unSaneSize :: a} deriving (Read, Show, Eq, Ord, Typeable, Data)

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {dim = TheHeight, size = 0.25, units = Inches}))
                               (saneSize (ImageSize {dim = TheHeight, size = 0.0, units = Inches})))
                 ]

defaultSize :: ImageSize
defaultSize = ImageSize {dim = TheArea, units = Inches, size = 6.0}

-- | Return the value of size in inches
inches :: ImageSize -> Rational
inches sz =
    size sz / case (dim sz, units sz) of
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

-- | A file containing an image plus meta info. This type is the same as
-- ImageFile_0, we just need to migrate the File
data ImageFile
    = ImageFile
      { _imageFile :: File
      , _imageFileType :: ImageType
      , _imageFileWidth :: Int
      , _imageFileHeight :: Int
      , _imageFileMaxVal :: Int
      } deriving (Show, Read, Eq, Ord, Data, Typeable)

data ImageFile_0
    = ImageFile_0 File ImageType Int Int Int
      deriving (Show, Read, Eq, Ord, Data, Typeable)

-- This migration just corrects the value of _fileExt, which is
-- a function of the image file type.
instance Migrate ImageFile where
  type MigrateFrom ImageFile = ImageFile_0
  migrate (ImageFile_0 f t w h m) =
      ImageFile (f {_fileExt = case t of
                                 JPEG -> ".jpg"
                                 GIF -> ".gif"
                                 PPM -> ".ppm"
                                 PNG -> ".png"}) t w h m

data ImageType = PPM | JPEG | GIF | PNG deriving (Show, Read, Eq, Ord, Typeable, Data)

-- | Helper function to learn the 'ImageType' of a file by runing
-- @file -b@.
getFileType :: P.ByteString -> IO ImageType
getFileType bytes =
    readProcessWithExitCode cmd args bytes >>= test . view _2
    where
      cmd = "file"
      args = ["-b", "-"]
      test :: Monad m => P.ByteString -> m ImageType
      test s = maybe (fail $ "ImageFile.getFileType - Not an image: (Ident string: " ++ show s ++ ")") return (foldr (testre (P.toString s)) Nothing tests)
      testre :: String -> (Regex, ImageType) -> Maybe ImageType -> Maybe ImageType
      testre _ _ (Just result) = Just result
      testre s (re, typ) Nothing = maybe Nothing (const (Just typ)) (matchRegex re s)
      -- Any more?
      tests = [(mkRegex "Netpbm P[BGPP]M \"rawbits\" image data$", PPM)
              ,(mkRegex "JPEG image data", JPEG)
              ,(mkRegex "PNG image data", PNG)
              ,(mkRegex "GIF image data", GIF)]

instance PixmapShape ImageFile where
    pixmapHeight = _imageFileHeight
    pixmapWidth = _imageFileWidth
    pixmapMaxVal = _imageFileMaxVal

instance Pretty ImageFile where
    pPrint (ImageFile f typ w h _mx) = text "ImageFile(" <> pPrint f <> text (" " <> show w <> "x" <> show h <> " " <> show typ <> ")")

$(makeLenses ''ImageFile)

-- |Return the area of an image in square pixels.
imageFileArea :: ImageFile -> Int
imageFileArea image = view imageFileWidth image * view imageFileHeight image

fileExtension :: ImageType -> String
fileExtension JPEG = ".jpg"
fileExtension PPM = ".ppm"
fileExtension GIF = ".gif"
fileExtension PNG = ".png"

data ImageKey_1
    = ImageOriginal_1 ImageFile
    -- ^ An unmodified upload
    | ImageCropped_1 ImageCrop ImageKey
    -- ^ A cropped version of another image
    | ImageScaled_1 ImageSize Double ImageKey
    -- ^ A resized version of another image
    | ImageUpright_1 ImageKey
    -- ^ Image uprighted using the EXIF orientation code, see  "Appraisal.Exif"
    deriving (Eq, Ord, Read, Show, Typeable, Data)

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
    deriving (Eq, Ord, Read, Show, Typeable, Data)

instance Migrate ImageKey where
    type MigrateFrom ImageKey = ImageKey_1
    migrate (ImageOriginal_1 f) = ImageOriginal f
    migrate (ImageCropped_1 c k) = ImageCropped c k
    -- Change scale factor to a rational with about four significant digits.
    migrate (ImageScaled_1 s f k) = ImageScaled s (approx (toRational f)) k
    migrate (ImageUpright_1 k) = ImageUpright k

instance Pretty ImageKey where
    pPrint (ImageOriginal _) = text "ImageOriginal"
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> text ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled size dpi x) = text "Scale (" <> pPrint size <> text " @" <> text (showRational dpi) <> text " dpi) (" <> pPrint x <> text ")"

instance Ppr ImageKey where ppr = ptext . show

type ImageCacheMap = Map ImageKey ImageFile

instance Arbitrary Units where
    arbitrary = elements [Inches, Cm, Points]

instance Arbitrary ImageType where
    arbitrary = elements [PPM, JPEG, GIF, PNG]

instance Arbitrary Dimension_0 where
    arbitrary = oneof [pure TheHeight_0, pure TheWidth_0, pure TheArea_0, Invalid <$> arbitrary]

instance Arbitrary Dimension where
    arbitrary = oneof [pure TheHeight, pure TheWidth, pure TheArea]

instance Arbitrary ImageSize where
    arbitrary = ImageSize <$> arbitrary <*> ((% 100) <$> (choose (1,10000) :: Gen Integer)) <*> arbitrary

instance Arbitrary a => Arbitrary (SaneSize a) where
    arbitrary = SaneSize <$> arbitrary

instance Arbitrary ImageSize_1 where
    arbitrary = ImageSize_1 <$> arbitrary <*> (fromInteger <$> (choose (1,10000) :: Gen Integer)) <*> arbitrary

instance Arbitrary ImageFile where
    arbitrary = ImageFile <$> arbitrary
                          <*> arbitrary
                          <*> choose (1,5000)
                          <*> choose (1,5000)
                          <*> choose (1,255)

instance Arbitrary ImageFile_0 where
    arbitrary = ImageFile_0
                          <$> arbitrary
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

instance Arbitrary ImageKey_1 where
    arbitrary = oneof [ ImageOriginal_1 <$> arbitrary
                      , ImageCropped_1 <$> arbitrary <*> arbitrary
                      , ImageScaled_1 <$> arbitrary <*> arbitrary <*> arbitrary
                      , ImageUpright_1 <$> arbitrary ]

-- | Remove null crops
fixKey :: ImageKey -> ImageKey
fixKey key@(ImageOriginal _) = key
fixKey (ImageCropped crop key) | crop == def = fixKey key
fixKey (ImageCropped crop key) = ImageCropped crop (fixKey key)
fixKey (ImageScaled sz dpi key) = ImageScaled sz dpi (fixKey key)
fixKey (ImageUpright key) = ImageUpright (fixKey key)

data Format = Binary | Gray | Color deriving Show
data RawOrPlain = Raw | Plain deriving Show
data Pnmfile = Pnmfile Format RawOrPlain (Integer, Integer, Maybe Integer) deriving Show

-- | Check whether the outputs of extractbb is valid by comparing it
-- to the output of pnmfile.
validateJPG :: FilePath -> IO (Either String (Integer, Integer))
validateJPG path = do
  (_code, bs, _) <- readCreateProcess (proc "jpegtopnm" [path]) mempty :: IO (ExitCode, P.ByteString, P.ByteString)
  (_code, s1', _) <- readCreateProcess (proc "pnmfile" []) bs :: IO (ExitCode, P.ByteString, P.ByteString)
  let s1 = unpack (decodeUtf8 s1')
  case parse parsePnmfileOutput path s1 of
    Left e -> return (Left ("Error parsing " ++ s1 ++ ": " ++ show e))
    Right (Pnmfile _ _ (w, h, _)) -> do
      (_code, s2, _) <- readCreateProcess (proc "extractbb" ["-O", path]) ("" :: String) :: IO (ExitCode, String, String)
      case parse parseExtractBBOutput path s2 of
        Left e -> return (Left ("Error parsing " ++ show s2 ++ ": " ++ show e))
        Right (ExtractBB (l, t, r, b) _) ->
          if l /= 0 || t /= 0 || r < 1 || b < 1 || r > 1000000 || b > 1000000
          then return (Left (path ++ ": image data error\n\npnmfile ->\n" ++ s1 ++ "\nextractbb ->\n" ++ s2))
          else return (Right (w, h))

-- | Parse the output of the pnmfile command (based on examination of
-- its C source code.)
parsePnmfileOutput :: Parsec String () Pnmfile
parsePnmfileOutput = do
  r <- go'
  case r of
    [x] -> return x
    [] -> fail "parsePnmfileOutput"
    _xs -> fail "multi"
  where
    go' = catMaybes <$> many ((Just <$> go) <|> (const Nothing <$> anyChar))
    go = do
      _ <- char 'P'
      format <- (\c -> case c of
                         'B' -> Binary
                         'G' -> Gray
                         'P' -> Color) <$> oneOf "BGP"
      _ <- string "M "
      rawOrPlain <- (\s -> case s of
                             "plain" -> Plain
                             "raw" -> Raw) <$> (string "plain" <|> string "raw")
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
              (Hires, Hires, Hires, Hires) deriving Show

data Hires = Inf | Rational Rational deriving Show

-- | Parse the output of extractbb (based on trial and error.)
parseExtractBBOutput :: Parsec String () ExtractBB
parseExtractBBOutput = do
  _ <- title
  _ <- creator
  bb <- boundingBox
  hbb <- hiResBoundingBox
  creationDate
  return $ ExtractBB bb hbb
    where
      title :: Parsec String () String
      title = string "%%Title:" >> spaces >> many (noneOf "\n") >>= \r -> newline >> return r

      creator = string "%%Creator:" >> spaces >> many (noneOf "\n") >> newline

      boundingBox :: Parsec String () (Integer, Integer, Integer, Integer)
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

      hiResBoundingBox :: Parsec String () (Hires, Hires, Hires, Hires)
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

      creationDate :: Parsec String () ()
      creationDate = string "%%CreationDate:" >> many (noneOf "\n") >> newline >> return ()

$(deriveSerialize [t|ImageSize_1|])
$(deriveSerialize [t|ImageSize|])
$(deriveSerialize [t|SaneSize ImageSize|])
$(deriveSerialize [t|Dimension_0|])
$(deriveSerialize [t|Dimension|])
$(deriveSerialize [t|Units|])
$(deriveSerialize [t|ImageCrop|])
$(deriveSerialize [t|ImageKey_1|])
$(deriveSerialize [t|ImageKey|])
$(deriveSerialize [t|ImageType|])
$(deriveSerialize [t|ImageFile|])
$(deriveSerialize [t|ImageFile_0|])

$(deriveSafeCopy 1 'base ''ImageSize_1)
$(deriveSafeCopy 2 'extension ''ImageSize)
$(deriveSafeCopy 1 'base ''SaneSize)
$(deriveSafeCopy 0 'base ''Dimension_0)
$(deriveSafeCopy 1 'extension ''Dimension)
$(deriveSafeCopy 0 'base ''Units)
$(deriveSafeCopy 0 'base ''ImageCrop)
$(deriveSafeCopy 1 'base ''ImageKey_1)
$(deriveSafeCopy 2 'extension ''ImageKey)
$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''ImageFile_0)
$(deriveSafeCopy 1 'extension ''ImageFile)

$(deriveLiftMany [
   ''ImageFile,
   ''ImageType,
   ''ImageKey,
   ''ImageSize,
   ''Units,
   ''ImageCrop,
   ''Dimension,
   ''SaneSize
  ])
