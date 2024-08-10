{-# LANGUAGE OverloadedStrings, TupleSections, UndecidableInstances #-}

-- | Beginning of a parser for the output of file(1).

module Data.FileCache.FileInfo
  ( fileInfoFromBytes
  , fileInfoFromPath
  ) where

import Control.Lens ( view, Field2(_2) )
import Data.ByteString.Lazy as BS ( ByteString, toStrict )
import Data.FileCache.FileError (MonadFileIO, FileError(NoShapeFromPath))
import Data.FileCache.ImageCrop (Rotation(..))
import Data.FileCache.ImageKey (HasImageShapeM(..), ImageShape(..), FileType(..))
import Data.FileCache.ImageRect (makeImageRect)
import Data.ListLike ( show )
import Data.Maybe ( catMaybes, fromMaybe, listToMaybe )
import Data.Text ( Text )
import Data.Text.Encoding (decodeUtf8)
import GHC.Stack (HasCallStack)
import SeeReason.Errors (throwMember)
import SeeReason.UIO (liftUIO, unsafeFromIO)
import Prelude hiding (show)
import SeeReason.LogServer (alog, Priority(DEBUG, ERROR))
import qualified System.Process.ListLike as LL ( readProcessWithExitCode )
import Text.Parsec as Parsec
    ( (<|>), char, choice, digit, many, many1, sepBy, spaces, try, parse, string, noneOf )
import Text.Parsec.Text ( Parser )

instance MonadFileIO e m => HasImageShapeM m BS.ByteString where
  imageShapeM bytes = fileInfoFromPath Nothing ("-", bytes)
instance MonadFileIO e m => HasImageShapeM m (FilePath, BS.ByteString) where
  imageShapeM (path, input) = fileInfoFromPath Nothing (path, input)

-- | Helper function to learn the 'FileType' of a file by running
-- @file -b@.
fileInfoFromBytes :: forall e m. (MonadFileIO e m, HasCallStack) => BS.ByteString -> m ImageShape
fileInfoFromBytes bytes = fileInfoFromPath Nothing ("-", bytes)

fileInfoFromPath :: forall e m. (MonadFileIO e m, HasCallStack) => Maybe FileType -> (FilePath, BS.ByteString) -> m ImageShape
fileInfoFromPath mtyp (path, input) =
  liftUIO (LL.readProcessWithExitCode cmd args input) >>= (fileInfoFromOutput mtyp path . decodeUtf8 . toStrict . view _2)
  where
    cmd = "file"
    args = ["-b", path]

-- Parse the output of file -b.   Note - no IO here
fileInfoFromOutput ::
  forall e m. (MonadFileIO e m, HasCallStack) => Maybe FileType -> FilePath -> Text -> m ImageShape
fileInfoFromOutput mtyp path output = do
  unsafeFromIO $ alog DEBUG ("fileInfoFromOutput " <> show mtyp <> " " <> show path <> " " <> show output)
  case parse pFileOutput path output of
    Left _e -> do
      unsafeFromIO $ alog ERROR ("pFileOutput -> " <> show _e)
      return $ ImageShape {_imageShapeType = fromMaybe Unknown mtyp, _imageShapeRect = Nothing}
      -- throwError $ fileError $ fromString $ "Failure parsing file(1) output: e=" ++ show e ++ " output=" ++ show output
    Right (PDF, []) -> return $ ImageShape (fromMaybe PDF mtyp) Nothing
    Right (CSV, []) -> return $ ImageShape (fromMaybe CSV mtyp) Nothing
    Right (typ, attrs) ->
      case (listToMaybe (catMaybes (fmap findShape attrs)),
            listToMaybe (catMaybes (fmap findRotation attrs))) of
        (Just (w, h), Just rot) ->
          return $ ImageShape {_imageShapeType = fromMaybe typ mtyp, _imageShapeRect = Just $ makeImageRect w h rot}
        (Just (w, h), Nothing) ->
          return $ ImageShape {_imageShapeType = fromMaybe typ mtyp, _imageShapeRect = Just $ makeImageRect w h ZeroHr}
        _ -> throwMember (NoShapeFromPath path output)
  where
    findShape :: ImageAttribute -> Maybe (Int, Int)
    findShape (Shape shape) = Just shape
    findShape _ = Nothing
    findRotation :: ImageAttribute -> Maybe Rotation
    findRotation (Orientation rotation) = Just rotation
    findRotation _ = Nothing

data ImageAttribute = Shape (Int, Int) | Orientation Rotation deriving Show

pFileOutput :: Parser (FileType, [ImageAttribute])
pFileOutput =
  (,) <$> choice [pPPM, pJPEG, pPNG, pGIF, pPDF, pCSV]
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

pJPEG :: Parser FileType
pJPEG = Parsec.try (string "JPEG image data" >> pSep >> return JPEG)
pPNG :: Parser FileType
pPNG = Parsec.try (string "PNG image data" >> pSep >> return PNG)
pGIF :: Parser FileType
pGIF = Parsec.try (string "GIF image data" >> pSep >> return GIF)
pPPM :: Parser FileType
pPPM = Parsec.try (string "Netpbm P[BGPP]M \"rawbits\" image data$" >> pSep >> return PPM)
pPDF :: Parser FileType
pPDF = Parsec.try (string "PDF document" >> pSep >> return PDF)
pCSV :: Parser FileType
pCSV = Parsec.try (string "ASCII text" >> pSep >> return CSV)
#if 0
pICON = string "MS Windows icon resource" >> many anyChar >> return ???
#endif

#if 0
tests :: [Either ParseError (FileType, [(Int, Int)])]
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
