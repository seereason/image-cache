{-# LANGUAGE OverloadedStrings, TupleSections, UndecidableInstances #-}

-- | Beginning of a parser for the output of file(1).

module Data.FileCache.FileInfo
  ( fileInfoFromBytes
  , fileInfoFromPath
  ) where

import Control.Exception ( IOException )
import Control.Lens ( view, Field2(_2) )
import Data.ByteString as BS ( ByteString )
import Data.ByteString.UTF8 ( toString )
import Data.FileCache.Common
  ( ImageType(..), Rotation(..), HasImageShapeM(..), ImageShape(..),
    FileCacheErrors, FileError(NoShape) )
import Data.ListLike ( show )
import Data.Maybe ( catMaybes, listToMaybe )
import Data.Text ( pack, Text )
import SeeReason.Errors ( liftUIO, throwMember, Member, NonIOException, OneOf )
import Extra.Except ( MonadError )
import Prelude hiding (show)
import SeeReason.LogServer (alog, Priority(DEBUG, ERROR))
import qualified System.Process.ListLike as LL ( readProcessWithExitCode )
import Text.Parsec as Parsec
    ( (<|>), char, choice, digit, many, many1, sepBy, spaces, try, parse, string, noneOf )
import Text.Parsec.Text ( Parser )
import UnexceptionalIO.Trans ( Unexceptional, unsafeFromIO )

instance (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m) => HasImageShapeM m BS.ByteString where
  imageShapeM bytes = fileInfoFromPath ("-", bytes)
instance (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m) => HasImageShapeM m (FilePath, BS.ByteString) where
  imageShapeM (path, input) = fileInfoFromPath (path, input)

-- | Helper function to learn the 'ImageType' of a file by running
-- @file -b@.
fileInfoFromBytes ::
  forall e m. (FileCacheErrors e m)
  => BS.ByteString
  -> m ImageShape
fileInfoFromBytes bytes = fileInfoFromPath ("-", bytes)

fileInfoFromPath ::
  forall e m. (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m)
  => (FilePath, BS.ByteString)
  -> m ImageShape
fileInfoFromPath (path, input) =
  liftUIO (LL.readProcessWithExitCode cmd args input) >>= (fileInfoFromOutput path . view _2)
  where
    cmd = "file"
    args = ["-b", path]

-- Note - no IO here
fileInfoFromOutput ::
  forall e m. (Member FileError e, MonadError (OneOf e) m, Unexceptional m)
  => FilePath
  -> BS.ByteString
  -> m ImageShape
fileInfoFromOutput path output = do
  unsafeFromIO $ alog DEBUG ("fileInfoFromOutput " <> show path <> " " <> show output)
  case parse pFileOutput path output' of
    Left _e -> do
      unsafeFromIO $ alog ERROR ("pFileOutput -> " <> show _e)
      return $ ImageShape {_imageShapeType = Unknown, _imageShapeWidth = 0, _imageShapeHeight = 0, _imageFileOrientation = ZeroHr}
      -- throwError $ fileError $ fromString $ "Failure parsing file(1) output: e=" ++ show e ++ " output=" ++ show output
    Right (PDF, []) -> return $ ImageShape PDF 0 0 ZeroHr
    Right (typ, attrs) ->
      case (listToMaybe (catMaybes (fmap findShape attrs)),
            listToMaybe (catMaybes (fmap findRotation attrs))) of
        (Just (w, h), Just rot) ->
          return $ ImageShape {_imageShapeType = typ, _imageShapeWidth = w, _imageShapeHeight = h, _imageFileOrientation = rot}
        (Just (w, h), Nothing) ->
          return $ ImageShape {_imageShapeType = typ, _imageShapeWidth = w, _imageShapeHeight = h, _imageFileOrientation = ZeroHr}
        _ -> throwMember (NoShape ("fileInfoFromOutput path=" <> show path <> " output=" <> show output))
  where
    output' :: Text
    output' = pack (toString output)
    findShape :: ImageAttribute -> Maybe (Int, Int)
    findShape (Shape shape) = Just shape
    findShape _ = Nothing
    findRotation :: ImageAttribute -> Maybe Rotation
    findRotation (Orientation rotation) = Just rotation
    findRotation _ = Nothing

data ImageAttribute = Shape (Int, Int) | Orientation Rotation deriving Show

pFileOutput :: Parser (ImageType, [ImageAttribute])
pFileOutput =
  (,) <$> choice [pPPM, pJPEG, pPNG, pGIF, pPDF]
      <*> (catMaybes <$> (sepBy (Parsec.try pShape <|> pOrientation <|> pNotAShape) pSep))

pSep :: Parser ()
pSep = spaces >> char ',' >> spaces

pShape :: Parser (Maybe ImageAttribute)
pShape = do
  w <- read <$> many1 digit
  pBy
  h <- read <$> many1 digit
  return $ Just $ Shape (w, h)

pOrientation :: Parser (Maybe ImageAttribute)
pOrientation = do
  (Just . testOrientation) <$> (string "orientation=" *> many1 (noneOf [',', ' ']))
  where
    testOrientation :: String -> ImageAttribute
    testOrientation "upper-left" = Orientation ZeroHr
    testOrientation "upper-right" = Orientation ThreeHr
    testOrientation "lower-left" = Orientation NineHr
    testOrientation "lower-right" = Orientation SixHr
    testOrientation _ = Orientation ZeroHr

{-
Data.ByteString.readFile "/srv/appraisalscribe3-development/images/00/00314183eddf66b90c7e60cf7d88d993.jpg" >>= runExceptT @FileError . getFileInfo
Data.ByteString.readFile "/srv/appraisalscribe3-development/images/00/00af3c5fc686cba7bacd6e9415308b66.jpg" >>= runExceptT @FileError . getFileInfo
-}

pNotAShape :: Parser (Maybe ImageAttribute)
pNotAShape = many (noneOf [',']) >> pure Nothing

pBy :: Parser ()
pBy = spaces >> char 'x' >> spaces >> pure ()

pJPEG :: Parser ImageType
pJPEG = Parsec.try (string "JPEG image data" >> pSep >> return JPEG)
pPNG :: Parser ImageType
pPNG = Parsec.try (string "PNG image data" >> pSep >> return PNG)
pGIF :: Parser ImageType
pGIF = Parsec.try (string "GIF image data" >> pSep >> return GIF)
pPPM :: Parser ImageType
pPPM = Parsec.try (string "Netpbm P[BGPP]M \"rawbits\" image data$" >> pSep >> return PPM)
pPDF :: Parser ImageType
pPDF = Parsec.try (string "PDF document" >> pSep >> return PDF)
#if 0
pICON = string "MS Windows icon resource" >> many anyChar >> return ???
#endif

#if 0
tests :: [Either ParseError (ImageType, [(Int, Int)])]
tests = fmap (parse pFileOutput "<text>")
  [ "JPEG image data, JFIF standard 1.02, aspect ratio, density 100x100, segment length 16, baseline, precision 8, 2861x2055, frames 3",
    "JPEG image data, Exif standard: [TIFF image data, big-endian, direntries=6, manufacturer=Apple, model=iPhone, orientation=upper-right, resolutionunit=2, datetime=2007:11:22 15:29:35], baseline, precision 8, 1600x1200, frames 3",
    "JPEG image data, JFIF standard 1.01, aspect ratio, density 1x1, segment length 16, progressive, precision 8, 612x612, frames 3",
    "PNG image data, 500 x 640, 8-bit/color RGB, non-interlaced",
    "PDF document, version 1.5",
    "PDF document, version 1.4",
    "HTML document, UTF-8 Unicode text",
    "Rich Text Format data, version 1, ANSI",
    "ASCII text, with very long lines",
    "MS Windows icon resource - 1 icon, 256x256 withPNG image data, 392 x 383, 8-bit/color RGBA, non-interlaced, 24 bits/pixel",
    "LaTeX 2e document, UTF-8 Unicode text, with very long lines",
    "unified diff output, ASCII text, with CRLF, LF line terminators",
    "gzip compressed data, last modified: Sun Dec  3 20:16:07 2017, from Unix" ]
#endif
