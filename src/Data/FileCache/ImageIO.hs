{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.ImageIO
  ( -- * Image IO
    MakeByteString(makeByteString)
  , validateJPG
  , uprightImage'
  , scaleImage'
  , editImage'
  ) where

import Control.Exception ( IOException )
import Control.Monad ( when )
import Control.Monad.Trans.Except ( runExceptT )
import Data.Binary.Get
    ( getLazyByteString,
      Get,
      skip,
      bytesRead,
      getWord16be,
      getWord32be,
      getWord16le,
      getWord32le,
      runGetOrFail )
import qualified Data.ByteString as BS ( ByteString, empty, readFile )
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Lazy as LBS ( ByteString, unpack, pack, take, drop, concat )
import Data.Char ( isSpace )
import Data.Default ( def )
import Data.FileCache.Process ( readCreateProcessWithExitCode', pipeline )
import Data.FileCache.Common
import Data.FileCache.LogException ( logException )
import Data.FileCache.CommandError ( CommandInfo(..) )
import Data.List ( intercalate )
import Data.ListLike ( StringLike(show) )
import Data.Monoid ( (<>) )
import Data.String ( fromString )
import Data.Text as T ( Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Word ( Word16, Word32 )
import SeeReason.Errors ( liftUIO, throwMember, Member, NonIOException, OneOf )
import qualified SeeReason.Errors as Errors ()
import Extra.Except ( MonadError(throwError), tryError )
import GHC.Int ( Int64 )
import Language.Haskell.TH.Instances ()
import Network.URI ( URI(..), uriToString )
import Numeric ( showFFloat )
import Prelude hiding (show)
import System.Exit ( ExitCode(..) )
import System.Log.Logger ( Priority(ERROR) )
import System.Process ( proc, shell, showCommandForUser, CreateProcess )
import System.Process.ByteString.Lazy as LBS ( readCreateProcessWithExitCode )
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
import UnexceptionalIO.Trans ( Unexceptional )
import UnexceptionalIO.Trans as UIO ( unsafeFromIO )

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
  forall e m. (Unexceptional m, MonadError (OneOf e) m, Member IOException e, Member NonIOException e, Member FileError e)
  => BS.ByteString
  -> m (Maybe BS.ByteString)
uprightImage' bs =
  -- Use liftUIO to turn the IOException into ExceptT FileError m,
  -- then flatten the two layers of ExceptT FileError into one, then
  -- turn the remaining one into a Maybe.
  either (\(_ :: OneOf e) -> Nothing) (Just . toStrict) <$> runExceptT (normalizeOrientationCode @e (fromStrict bs))
    -- runExceptT (runExceptT (liftUIO (normalizeOrientationCode (fromStrict bs))) >>= liftEither)

-- | Given a bytestring containing a JPEG file, examine the EXIF
-- orientation flag and if it is something other than 1 transform the
-- image into the "normal" orientation and change the orientation flag
-- to 1.  The result is the new bytestring.  If the old bytestring was
-- already normalized, or absolutely anything else goes wrong, the
-- result is a Left.  This means the original bytestring should be used.
--
-- This is an IO operation because it runs jpegtran(1) to perform the
-- transformation on the jpeg image.
normalizeOrientationCode ::
  forall e m. (Unexceptional m, Member FileError e, Member IOException e, Member NonIOException e, MonadError (OneOf e) m)
  => LBS.ByteString
  -> m LBS.ByteString
normalizeOrientationCode bs = do
  let result = runGetOrFail getEXIFOrientationCode bs :: (Either (LBS.ByteString, Int64, String) (LBS.ByteString, Int64, (Int, Int64, Bool)))
  case result of
    Right (_, _, (2, pos, flag)) -> transform ["-flip", "horizontal"] pos flag
    Right (_, _, (3, pos, flag)) -> transform ["-rotate", "180"] pos flag
    Right (_, _, (4, pos, flag)) -> transform ["-flip", "vertical"] pos flag
    Right (_, _, (5, pos, flag)) -> transform ["-transpose"] pos flag
    Right (_, _, (6, pos, flag)) -> transform ["-rotate", "90"] pos flag
    Right (_, _, (7, pos, flag)) -> transform ["-transverse"] pos flag
    Right (_, _, (8, pos, flag)) -> transform ["-rotate", "270"] pos flag
    Right x -> throwMember $ fromString @FileError $ "Unexpected exif orientation code: " <> show x
    Left x -> throwMember $ fromString @FileError $ "Failure parsing exif orientation code: " <> show x
    where
      transform :: [String] -> Int64 -> Bool -> m LBS.ByteString
      transform args pos isMotorola = do
        let cp = proc cmd args'
            cmd = "jpegtran"
            args' = (["-copy", "all"] ++ args)
            hd = LBS.take pos bs            -- everything before the orientation code
            flag = LBS.pack (if isMotorola then [0x0, 0x1] else [0x1, 0x0]) -- orientation code 1
            tl = LBS.drop (pos + 2) bs      -- everything after the orientation code
            bs' = LBS.concat [hd, flag, tl]
        (result, out, err) <- liftUIO (LBS.readCreateProcessWithExitCode (proc cmd args') bs')
        case result of
          ExitSuccess -> return out
          ExitFailure _ -> throwMember $ CommandFailure [CommandErr (toStrict err), CommandCreateProcess cp, CommandExitCode result]

-- | Read the orientation code of a JPEG file, returning its value,
-- the offset of the two byte code in the file, and the "isMotorola"
-- flag which determines something about the endianness.
getEXIFOrientationCode :: Get (Int, Int64, Bool)
getEXIFOrientationCode = do
  getLazyByteString 4 >>= doJPEGHeader
  headerLength <- getWord16be >>= markerParameterLength
  getLazyByteString 6 >>= testEXIFHead
  isMotorola <- getLazyByteString 2 >>= discoverByteOrder
  getLazyByteString 2 >>= checkTagMark isMotorola
  offset <- getWord32Motorola isMotorola >>= testIFDOffset headerLength
  skip (fromIntegral offset - 8)
  numberOfTags <- getWord16Motorola isMotorola >>= testNumberOfTags
  findOrientationTag isMotorola (headerLength - 8) numberOfTags (offset + 2)
    where
      doJPEGHeader :: Monad m => LBS.ByteString -> m ()
      doJPEGHeader x = when (LBS.unpack x /= [0xff, 0xd8, 0xff, 0xe1] && LBS.unpack x /= [0xff, 0xd8, 0xff, 0xe0]) (fail $ "Invalid JPEG header: " ++ show (LBS.unpack x))

      markerParameterLength :: Word16 -> Get Int64
      markerParameterLength w
          | w < 8 = fail $ "Length field much too short: " ++ show w
          | w < 20 = fail $ "Length field too short: " ++ show w
          | otherwise = return $ fromIntegral $ w - 8

      testEXIFHead :: LBS.ByteString -> Get ()
      testEXIFHead x = when (LBS.unpack x /= [0x45, 0x78, 0x69, 0x66, 0x0, 0x0]) (fail $ "Invalid EXIF header: " ++ show (LBS.unpack x))

      discoverByteOrder :: LBS.ByteString -> Get Bool
      discoverByteOrder x =
          case LBS.unpack x of
            [0x49, 0x49] -> return True
            [0x4d, 0x4d] -> return False
            s -> fail $ "Invalid byte order: " ++ show s

      checkTagMark :: Bool -> LBS.ByteString -> Get ()
      checkTagMark True x = when (LBS.unpack x /= [0x2a, 0x0]) (fail $ "Invalid tag mark True: " ++ show (LBS.unpack x))
      checkTagMark False x = when (LBS.unpack x /= [0x0, 0x2a]) (fail $ "Invalid tag mark False: " ++ show (LBS.unpack x))

      testIFDOffset :: Int64 -> Word32 -> Get Word32
      testIFDOffset len x = if x > 0xffff || fromIntegral x > len - 2 then fail ("Invalid IFD offset: " ++ show x) else return x

      testNumberOfTags :: Word16 -> Get Word16
      testNumberOfTags n = if n > 0 then return n else fail "No tags"

findOrientationTag :: Bool -> Int64 -> Word16 -> Word32 -> Get (Int, Int64, Bool)
findOrientationTag isMotorola headerLength numberOfTags offset = do
    when  (fromIntegral offset > headerLength - 12 || numberOfTags < 1) (fail "No orientation tag")
    tagnum <- getWord16Motorola isMotorola
    case tagnum of
      0x0112 -> do
        skip 6
        pos <- bytesRead
        flag <- getWord16Motorola isMotorola
        if flag < 1 || flag > 8 then fail "Invalid orientation flag" else return (fromIntegral flag, pos, isMotorola)
      _ -> do
        skip 10
        findOrientationTag isMotorola headerLength (numberOfTags - 1) (offset + 12)

getWord16Motorola :: Bool -> Get Word16
getWord16Motorola True = getWord16le
getWord16Motorola False = getWord16be

getWord32Motorola :: Bool -> Get Word32
getWord32Motorola True = getWord32le
getWord32Motorola False = getWord32be

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
  let s1 = decodeUtf8 s1'
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
  (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m)
  => Double
  -> BS.ByteString
  -> ImageType
  -> m (Maybe BS.ByteString)
scaleImage' sc _ _ | approx (toRational sc) == 1 = return Nothing
scaleImage' _ _ PDF = throwMember $ (NoShape "scaleImage'")
scaleImage' _ _ Unknown = throwMember $ NoShape "scaleImage'"
scaleImage' sc bytes typ = do
    let decoder = case typ of
                    JPEG -> showCommandForUser "jpegtopnm" ["-"]
                    PPM -> showCommandForUser "cat" ["-"]
                    GIF -> showCommandForUser "giftopnm" ["-"]
                    PNG -> showCommandForUser "pngtopnm" ["-"]
                    PDF -> error "scaleImge' - Unexpected file type"
                    Unknown -> error "scaleImge' - Unexpected file type"
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) sc ""]
        -- To save space, build a jpeg here rather than the original file type.
        encoder = case typ of
                    JPEG -> showCommandForUser "cjpeg" []
                    PPM -> showCommandForUser {-"cat"-} "cjpeg" []
                    GIF -> showCommandForUser {-"ppmtogif"-} "cjpeg" []
                    PNG -> showCommandForUser {-"pnmtopng"-} "cjpeg" []
                    PDF -> error "scaleImge' - Unexpected file type"
                    Unknown -> error "scaleImge' - Unexpected file type"
        cmd = intercalate " | " [decoder, scaler, encoder]
    Just <$> makeByteString (shell cmd, bytes)

logIOError' :: (Unexceptional m, MonadError e m) => m a -> m a
logIOError' io =
  tryError io >>= either (\e -> unsafeFromIO ($logException ERROR (pure e)) >> throwError e) return
-- logIOError' = handleError (\e -> liftIO ($logException ERROR (pure e)) >> throwError e)

editImage' ::
    forall e m shape. (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m, HasImageShape shape)
    => ImageCrop -> BS.ByteString -> ImageType -> shape -> m (Maybe BS.ByteString)
editImage' crop _ _ _ | crop == def = return Nothing
editImage' _ _ PDF _ = throwMember $ NoShape "editImage'"
editImage' _ _ Unknown _ = throwMember $ NoShape "editImage'"
editImage' crop bs typ shape =
  logIOError' $
    case commands of
      [] -> return Nothing
      _ -> Just <$> pipeline commands bs
    where
      commands = buildPipeline typ [cut, rotate] (latexImageFileType typ)
      -- We can only embed JPEG and PNG images in a LaTeX
      -- includegraphics command, so here we choose which one to use.
      latexImageFileType GIF = JPEG
      latexImageFileType PPM = JPEG
      latexImageFileType JPEG = JPEG
      latexImageFileType PNG = JPEG
      latexImageFileType PDF = error "editImage' - Unexpected file type"
      latexImageFileType Unknown = error "editImage' - Unexpected file type"
      cut = case (leftCrop crop, rightCrop crop, topCrop crop, bottomCrop crop) of
              (0, 0, 0, 0) -> Nothing
              (l, r, t, b) -> Just (PPM, proc "pnmcut" ["-left", show l,
                                                        "-right", show (w - r - 1),
                                                        "-top", show t,
                                                        "-bottom", show (h - b - 1)], PPM)
      rotate = case rotation crop of
                 ThreeHr -> Just (JPEG, proc "jpegtran" ["-rotate", "90"], JPEG)
                 SixHr -> Just (JPEG, proc "jpegtran" ["-rotate", "180"], JPEG)
                 NineHr -> Just (JPEG, proc "jpegtran" ["-rotate", "270"], JPEG)
                 ZeroHr -> Nothing
      ImageShape {_imageShapeWidth = w, _imageShapeHeight = h} = imageShape shape
      buildPipeline :: ImageType -> [Maybe (ImageType, CreateProcess, ImageType)] -> ImageType -> [CreateProcess]
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
