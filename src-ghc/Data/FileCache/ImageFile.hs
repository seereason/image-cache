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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.ImageFile
    ( getFileType
    -- * validation tools
    , validateJPG
    , tests
    ) where

import Control.Lens (_2, view)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString.UTF8 as P
#endif
import Data.Char (isSpace)
import Data.FileCache.Image
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Language.Haskell.TH.Lift (Lift)
import System.Exit (ExitCode)
import System.Process (proc{-, showCommandForUser-})
import System.Process.ListLike (readCreateProcess, readProcessWithExitCode)
import System.Process.ByteString ()
import Test.HUnit (assertEqual, Test(..))
#if ARBITRARY
import Data.Ratio ((%))
import Test.QuickCheck (Arbitrary(..), choose, elements, Gen, oneof)
#endif
import "regex-compat-tdfa" Text.Regex (Regex, mkRegex, matchRegex)
import Text.Parsec
--import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
--import Text.Read (readMaybe)

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]

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

#if ARBITRARY
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

data Format = Binary | Gray | Color
data RawOrPlain = Raw | Plain
data Pnmfile = Pnmfile Format RawOrPlain (Integer, Integer, Maybe Integer)

deriving instance Show Format
deriving instance Show RawOrPlain
deriving instance Show Pnmfile

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

deriving instance Show ExtractBB
deriving instance Show Hires

deriving instance Lift ImageFile
deriving instance Lift ImageType
deriving instance Lift ImageKey
deriving instance Lift ImageSize
deriving instance Lift Units
deriving instance Lift ImageCrop
deriving instance Lift Dimension
deriving instance Lift a => Lift (SaneSize a)
