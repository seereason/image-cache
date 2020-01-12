-- | Beginning of a parser for the output of file(1).

{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.FileCache.ImageType
  ( fileInfoFromBytes
  , fileInfoFromPath
  ) where

import Control.Exception (IOException)
import Control.Monad.Catch as Catch (Exception, MonadCatch, try)
import Control.Lens (_2, preview, review, view)
import Data.ByteString as BS (ByteString)
import Data.ByteString.UTF8 (toString)
import Data.FileCache.Common
  (FileError(ErrorCall, NoShape), fromFileError, HasFileError, HasImageShapeM(..),
   ImageShape(..), ImageType(..), Rotation(..))
import Data.Maybe(catMaybes, listToMaybe)
import Data.String (fromString)
import Data.Text (pack)
import Extra.ErrorControl (controlError, ErrorControl)
import Extra.Except (Except, ExceptT, HasIOException(ioException), HasNonIOException(nonIOException), liftEither, throwError, withExceptT)
import System.Exit (ExitCode)
import qualified System.Process.ListLike as LL ( readProcessWithExitCode)
import Text.Parsec as Parsec ((<|>), char, choice, digit, many, many1, sepBy,
                              spaces, try, parse, string, noneOf)
import Text.Parsec.Text (Parser)
import UnexceptionalIO.Trans (fromIO, fromIO', SomeNonPseudoException, Unexceptional)

instance Unexceptional m => HasImageShapeM (ExceptT FileError m) BS.ByteString where
  imageShapeM bytes = fileInfoFromPath ("-", bytes)
instance Unexceptional m => HasImageShapeM (ExceptT FileError m) (FilePath, BS.ByteString) where
  imageShapeM (path, input) = fileInfoFromPath (path, input)

-- | Helper function to learn the 'ImageType' of a file by running
-- @file -b@.
fileInfoFromBytes ::
  forall e m. (Unexceptional m, HasFileError e, Exception e, HasIOException e, HasNonIOException e)
  => BS.ByteString
  -> ExceptT e m ImageShape
fileInfoFromBytes bytes = fileInfoFromPath ("-", bytes)

fileInfoFromPath ::
  forall e m. (Unexceptional m, HasFileError e, Exception e, HasIOException e, HasNonIOException e)
  => (FilePath, BS.ByteString)
  -> ExceptT e m ImageShape
fileInfoFromPath (path, input) =
#if 0
  withExceptT (review ioException :: IOException -> e) io >>= fileInfoFromOutput path . view _2
    where
      io =
        controlError
          (fromIO (LL.readProcessWithExitCode cmd args input) :: ExceptT SomeNonPseudoException m (ExitCode, ByteString, ByteString))
          (\e -> maybe undefined throwError (preview ioException e) :: ExceptT IOException m a)
#else
  controlError
    (fromIO (LL.readProcessWithExitCode cmd args input) :: ExceptT SomeNonPseudoException m (ExitCode, ByteString, ByteString))
    (\e -> maybe undefined (throwError . review ioException) (preview ioException e) :: ExceptT e m a) >>= fileInfoFromOutput path . view _2
    where
#endif
      cmd = "file"
      args = ["-b", path]

-- Note - no IO here
fileInfoFromOutput ::
  forall e m. (Monad m, HasFileError e)
  => FilePath
  -> BS.ByteString
  -> ExceptT e m ImageShape
fileInfoFromOutput path output =
  case parse pFileOutput path (pack (toString output)) of
    Left e -> throwError $ fromFileError $ fromString $ "Failure parsing file(1) output: e=" ++ show e ++ " output=" ++ show output
    Right (typ, attrs) ->
      case (listToMaybe (catMaybes (fmap findShape attrs)),
            listToMaybe (catMaybes (fmap findRotation attrs))) of
        (Just (w, h), Just rot) ->
          return $ ImageShape {_imageShapeType = typ, _imageShapeWidth = w, _imageShapeHeight = h, _imageFileOrientation = rot}
        (Just (w, h), Nothing) ->
          return $ ImageShape {_imageShapeType = typ, _imageShapeWidth = w, _imageShapeHeight = h, _imageFileOrientation = ZeroHr}
        _ -> throwError (fromFileError NoShape)
  where
    findShape :: ImageAttribute -> Maybe (Int, Int)
    findShape (Shape shape) = Just shape
    findShape _ = Nothing
    findRotation :: ImageAttribute -> Maybe Rotation
    findRotation (Orientation rotation) = Just rotation
    findRotation _ = Nothing
#if 0
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

data ImageAttribute = Shape (Int, Int) | Orientation Rotation deriving Show

pFileOutput :: Parser (ImageType, [ImageAttribute])
pFileOutput =
  (,) <$> choice [pPPM, pJPEG, pPNG, pGIF]
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
pJPEG = string "JPEG image data" >> pSep >> return JPEG
pPNG :: Parser ImageType
pPNG = string "PNG image data" >> pSep >> return PNG
pGIF :: Parser ImageType
pGIF = string "GIF image data" >> pSep >> return GIF
pPPM :: Parser ImageType
pPPM = string "Netpbm P[BGPP]M \"rawbits\" image data$" >> pSep >> return PPM
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
