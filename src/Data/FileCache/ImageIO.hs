{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.ImageIO
  ( -- * Image IO
    MakeByteString(makeByteString)
  , validateJPG
  , uprightImage'
  , scaleImage'
  , editImage'
  ) where

import Codec.Picture.Jpg (decodeJpegWithMetadata)
import Codec.Picture.Metadata (Keys(Exif), lookup)
import Codec.Picture.Metadata.Exif (ExifData(..), ExifTag(TagOrientation))
import Control.Exception ( IOException )
import Control.Lens (preview, _Right, _2, to, _Just)
import Control.Monad.Trans (liftIO)
import Data.Generics.Sum (_Ctor)
import Control.Monad.Trans.Except ( runExceptT )
import qualified Data.ByteString.Lazy as BS ( ByteString, empty, hPutStr, readFile, toStrict )
--import Data.ByteString.Lazy ( fromStrict, toStrict )
--import qualified Data.ByteString.Lazy as LBS ( ByteString, unpack, pack, take, drop, concat )
import Data.Char ( isSpace )
import Data.Default ( def )
import Data.FileCache.CommandError ( CommandInfo(..) )
import Data.FileCache.FileError (FileError(..))
import Data.FileCache.ImageCrop ( ImageCrop(..), Rotation(..) )
import Data.FileCache.ImageRect (ImageRect (_imageRectWidth, _imageRectHeight))
import Data.FileCache.ImageShape ( ImageShape(..), FileType(..) )
import Data.FileCache.LogException ( logException )
import Data.FileCache.Pipify ( heifConvert )
import Data.FileCache.Process ( readCreateProcessWithExitCode', pipeline )
import Data.FileCache.Rational (approx, readRationalMaybe)
import Data.List ( intercalate )
import Data.ListLike ( StringLike(show) )
import Data.Monoid ( (<>) )
import Data.String ( fromString )
import Data.Text as T ( Text )
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding ( decodeUtf8 )
import qualified SeeReason.Errors as Errors ()
import Extra.Except ( ExceptT, MonadError(throwError), tryError )
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Network.URI ( URI(..), uriToString )
import Numeric ( showFFloat )
import Prelude hiding (show)
import System.Exit ( ExitCode(..) )
import System.IO (Handle, hFlush, hClose)
import System.IO.Temp (withSystemTempFile)
import System.Log.Logger ( Priority(ERROR) )
import System.Process ( proc, shell, showCommandForUser, CreateProcess )
import System.Process.ByteString.Lazy as BS ( readCreateProcessWithExitCode )
import System.Process.ListLike as LL ( readCreateProcess )
import Text.Parsec
    ( Parsec,
      (<|>),
      many,
      parse,
      char,
      digit,
      newline,
      noneOf,
      oneOf,
      satisfy,
      space,
      spaces,
      string,
      many1,
      optionMaybe )
import SeeReason.Errors as Err ( throwMember, Member, OneOf)
import SeeReason.UIO (liftUIO, NonIOException, Unexceptional, unsafeFromIO)

class MakeByteString a where
  makeByteString :: (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m) => a -> m BS.ByteString

instance MakeByteString BS.ByteString where
  makeByteString = return

instance MakeByteString FilePath where
  makeByteString path = liftUIO (BS.readFile path)

instance MakeByteString CreateProcess where
  makeByteString cmd = makeByteString (cmd, BS.empty)

instance MakeByteString (CreateProcess, BS.ByteString) where
  makeByteString (cmd, input) = do
    (code, bytes, _err) <- liftUIO (readCreateProcessWithExitCode' cmd input)
    case code of
      ExitSuccess -> return bytes
      ExitFailure _ ->
        throwMember $ CommandFailure [StartedFrom "MakeByteString CreateProcess",
                                      CommandCreateProcess cmd,
                                      CommandExitCode code]

instance MakeByteString URI where
  makeByteString uri = do
    let cmd = proc "curl" ["-s", uriToString id uri ""]
    (code, bytes, _err) <-
      liftUIO $ readCreateProcessWithExitCode' cmd BS.empty
    case code of
      ExitSuccess -> return bytes
      _ -> throwMember $
                        CommandFailure [StartedFrom "instance MakeByteString URI",
                                        CommandCreateProcess cmd,
                                        CommandExitCode code]

-- * Image IO

-- | Try to create a version of an image with its orientation
-- corrected based on the EXIF orientation flag.  If the image is
-- already upright this will return Nothing.
--
-- Note that the compiler reports "Redundant constraint: MonadError
-- (OneOf e) m", but in fact its not redundant.
uprightImage' ::
  forall e m. (Unexceptional m, MonadError (OneOf e) m, Member IOException e, Member NonIOException e)
  => BS.ByteString
  -> m (Maybe BS.ByteString)
uprightImage' bs =
  -- Use liftUIO to turn the IOException into ExceptT FileError m,
  -- then flatten the two layers of ExceptT FileError into one, then
  -- turn the remaining one into a Maybe.
  liftUIO $ either (\(_ :: FileError) -> Nothing) Just <$> runExceptT (normalizeOrientationCode bs)
    -- runExceptT (runExceptT (liftUIO (normalizeOrientationCode (fromStrict bs))) >>= liftEither)

deriving instance Generic ExifData

-- | Given a bytestring containing a JPEG file, examine the EXIF
-- orientation flag and if it is something other than 1 transform the
-- image into the "normal" orientation and change the orientation flag
-- to 1.  The result is the new bytestring.  If the old bytestring was
-- already normalized, or absolutely anything else goes wrong, the
-- result is a Left.  This means the original bytestring should be used.
--
-- This is an IO operation because it runs jpegtran(1) to perform the
-- transformation on the jpeg image.
--
-- % let lns = (_Right . _2 . to (Codec.Picture.Metadata.lookup (Exif TagOrientation)) . _Just . _Ctor @"ExifShort")
-- % bs <- Data.ByteString.readFile "../happstack-ghcjs/IMAGES/IMG_4705.JPG"
-- % preview lns (decodeJpegWithMetadata bs)
-- Just 6
-- % Right bs' <- runExceptT (normalizeOrientationCode bs)
-- % preview lns (decodeJpegWithMetadata bs')
-- Just 1
normalizeOrientationCode :: BS.ByteString -> ExceptT FileError IO BS.ByteString
normalizeOrientationCode bs = do
  case preview (_Right . _2 . to (Codec.Picture.Metadata.lookup (Exif TagOrientation)) . _Just . _Ctor @"ExifShort") (decodeJpegWithMetadata (BS.toStrict bs)) of
    Just 2 -> transform ["-F"] -- ["-flip", "horizontal"]
    Just 3 -> transform ["-1"] -- ["-rotate", "180"]
    Just 4 -> transform ["-f"] -- ["-flip", "vertical"]
    Just 5 -> transform ["-t"] -- ["-transpose"]
    Just 6 -> transform ["-9"] -- ["-rotate", "90"]
    Just 7 -> transform ["-T"] -- ["-transverse"]
    Just 8 -> transform ["-2"] -- ["-rotate", "270"]
    Just x -> throwError $ fromString @FileError ("Unexpected exif orientation code: " <> show x)
    Nothing -> throwError $ fromString @FileError "Failure parsing exif orientation code"
    where
      transform :: [String] -> ExceptT FileError IO BS.ByteString
      transform args = withSystemTempFile "exiftran" (\path handle -> run args path handle)
      run :: [String] -> FilePath -> Handle -> ExceptT FileError IO BS.ByteString
      run args path handle = do
        liftIO $ BS.hPutStr handle bs
        liftIO $ hFlush handle
        liftIO $ hClose handle
        let cp = proc "exiftran" (["-i"] <> args <> [path])
        (result, _out, err) <- liftIO $ BS.readCreateProcessWithExitCode cp bs
        case result of
          ExitSuccess -> liftIO $ BS.readFile path
          ExitFailure _ -> throwError $ fromString @FileError (show (CommandFailure [CommandErr err, CommandCreateProcess cp, CommandExitCode result]))

data Format = Binary | Gray | Color
data RawOrPlain = Raw | Plain
data Pnmfile = Pnmfile Format RawOrPlain (Integer, Integer, Maybe Integer)

deriving instance Show Format
deriving instance Show RawOrPlain
deriving instance Show Pnmfile

-- | Check whether the outputs of extractbb is valid by comparing it
-- to the output of pnmfile.
validateJPG :: FilePath -> IO (Either FileError (Integer, Integer))
validateJPG path = do
  (_code, bs, _) <- LL.readCreateProcess (proc "jpegtopnm" [path]) mempty :: IO (ExitCode, BS.ByteString, BS.ByteString)
  (_code, s1', _) <- LL.readCreateProcess (proc "pnmfile" []) bs :: IO (ExitCode, BS.ByteString, BS.ByteString)
  let s1 = toStrict (decodeUtf8 s1')
  case parse parsePnmfileOutput path s1 of
    Left _e -> return (Left (UnexpectedPnmfileOutput s1))
    Right (Pnmfile _ _ (w, h, _)) -> do
      (_code, s2, _) <- LL.readCreateProcess (proc "extractbb" ["-O", path]) ("" :: Text) :: IO (ExitCode, Text, Text)
      case parse parseExtractBBOutput path s2 of
        Left _e -> return (Left (ExtractBBFailed path s2))
        Right (ExtractBB (l, t, r, b) _) ->
          if l /= 0 || t /= 0 || r < 1 || b < 1 || r > 1000000 || b > 1000000
          then return (Left (InvalidBoundingBox path s1 s2))
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

-- | Build an image resized by decoding, applying pnmscale, and then
-- re-encoding.  The new image inherits attributes of the old (other
-- than size.)
scaleImage' ::
  (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m, HasCallStack)
  => Double
  -> BS.ByteString
  -> FileType
  -> m (Maybe BS.ByteString)
scaleImage' sc _ _ | approx (toRational sc) == 1 = return Nothing
scaleImage' _ _ PDF = throwMember $ CannotScale PDF
scaleImage' _ _ CSV = throwMember $ CannotScale CSV
scaleImage' _ _ Unknown = throwMember $ CannotScale Unknown
scaleImage' sc bytes typ = do
    let decoder = case typ of
                    GIF -> showCommandForUser "giftopnm" ["-"]
                    HEIC -> heifConvert
                    JPEG -> showCommandForUser "jpegtopnm" ["-"]
                    PDF -> error "scaleImage' - Unexpected file type"
                    CSV -> error "scaleImage' - Unexpected file type"
                    PNG -> showCommandForUser "pngtopnm" ["-"]
                    PPM -> showCommandForUser "cat" ["-"]
                    TIFF -> showCommandForUser "tifftopnm" ["-"]
                    Unknown -> error "scaleImage' - Unexpected file type"
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) sc ""]
        -- To save space, build a jpeg here rather than the original file type.
        encoder = case typ of
                    GIF -> showCommandForUser {-"ppmtogif"-} "cjpeg" []
                    HEIC -> showCommandForUser "cjpeg" []
                    JPEG -> showCommandForUser "cjpeg" []
                    PDF -> error "scaleImage' - Unexpected file type"
                    CSV -> error "scaleImage' - Unexpected file type"
                    PNG -> showCommandForUser {-"pnmtopng"-} "cjpeg" []
                    PPM -> showCommandForUser {-"cat"-} "cjpeg" []
                    TIFF -> showCommandForUser "cjpeg" []
                    Unknown -> error "scaleImage' - Unexpected file type"
        cmd = intercalate " | " [decoder, scaler, encoder]
    Just <$> makeByteString (shell cmd, bytes)

logIOError' :: (Unexceptional m, MonadError e m) => m a -> m a
logIOError' io =
  tryError io >>= either (\e -> unsafeFromIO ($logException ERROR (pure e)) >> throwError e) return
-- logIOError' = handleError (\e -> liftIO ($logException ERROR (pure e)) >> throwError e)

editImage' ::
    forall e m. (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m)
    => ImageCrop -> BS.ByteString -> FileType -> ImageShape -> m (Maybe BS.ByteString)
editImage' crop _ _ _ | crop == def = return Nothing
editImage' crop bs typ ImageShape{_imageShapeRect = Just rect} =
  logIOError' $
    case commands of
      [] -> return Nothing
      _ -> Just <$> pipeline commands bs
    where
      commands = buildPipeline typ [cut, rotate] (latexImageFileType typ)
      -- We can only embed JPEG and PNG images in a LaTeX
      -- includegraphics command, so here we choose which one to use.
      latexImageFileType GIF = JPEG
      latexImageFileType HEIC = JPEG
      latexImageFileType PPM = JPEG
      latexImageFileType JPEG = JPEG
      latexImageFileType PNG = JPEG
      latexImageFileType PDF = error "editImage' - Unexpected file type"
      latexImageFileType CSV = error "editImage' - Unexpected file type"
      latexImageFileType TIFF = JPEG
      latexImageFileType Unknown = error "editImage' - Unexpected file type"
      cut = case (leftCrop crop, rightCrop crop, topCrop crop, bottomCrop crop) of
              (0, 0, 0, 0) -> Nothing
              (l, r, t, b) -> Just (PPM, proc "pnmcut" ["-left", show l,
                                                        "-right", show (_imageRectWidth rect - r - 1),
                                                        "-top", show t,
                                                        "-bottom", show (_imageRectHeight rect - b - 1)], PPM)
      rotate = case rotation crop of
                 ThreeHr -> Just (JPEG, proc "jpegtran" ["-rotate", "90"], JPEG)
                 SixHr -> Just (JPEG, proc "jpegtran" ["-rotate", "180"], JPEG)
                 NineHr -> Just (JPEG, proc "jpegtran" ["-rotate", "270"], JPEG)
                 ZeroHr -> Nothing
      -- ImageShape {_imageShapeWidth = w, _imageShapeHeight = h} = imageShape shape
      buildPipeline :: FileType -> [Maybe (FileType, CreateProcess, FileType)] -> FileType -> [CreateProcess]
      buildPipeline start [] end = convert start end
      buildPipeline start (Nothing : ops) end = buildPipeline start ops end
      buildPipeline start (Just (a, cmd, b) : ops) end | start == a = cmd : buildPipeline b ops end
      buildPipeline start (Just (a, cmd, b) : ops) end = convert start a ++ buildPipeline a (Just (a, cmd, b) : ops) end
      convert JPEG PPM = [proc "jpegtopnm" []]
      convert GIF PPM = [proc "giftpnm" []]
      convert PNG PPM = [proc "pngtopnm" []]
      convert PPM JPEG = [proc "cjpeg" []]
      convert PPM GIF = [proc "ppmtogif" []]
      convert PPM PNG = [proc "pnmtopng" []]
      convert PNG x = proc "pngtopnm" [] : convert PPM x
      convert GIF x = proc "giftopnm" [] : convert PPM x
      convert a b | a == b = []
      convert a b = error $ "Unknown conversion: " ++ show a ++ " -> " ++ show b
editImage' _ _ typ _ = throwMember $ CannotCrop typ
