{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators, UndecidableInstances #-}
{-# OPTIONS -Wno-orphans #-}

module Data.FileCache.Server
  ( -- * FileCacheTop
    FileCacheTop(..)
  , HasFileCacheTop(fileCacheTop)
  , HasCacheAcid(cacheAcid)
  , CacheAcid
  , CacheMap(..)
    -- * Cache Events
  , initCacheMap
  , openCache
  , PutValue(..)
  , PutValues(..)
  , LookValue(..)
  , LookValues(..)
  , LookMap(..)
  , DeleteValue(..)
  , DeleteValues(..)
  , Replace(..)
    -- * Image IO
  , validateJPG
    -- * File Cache
  , HasImageFilePath(toFilePath)
  , fileCachePath
  , fileCachePathIO
  , FileCacheT
  , runFileCacheT, evalFileCacheT, execFileCacheT
  , cacheLook, cacheDelete, cacheMap
    -- * Image Cache
  , ImageCacheT
  , HasImageAcid
  , ImageAcid
  , imageAcid
  , evalImageCacheUIO
  , evalImageCacheIO
    -- * Create original and derived images
  , cacheOriginalImage
  , cacheOriginalImages
  , getImageFiles
  , getImageFile
  , cacheDerivedImagesForeground
  , cacheImageFile
  -- , cacheDerivedImage
  , buildImageShape

  , ImageChan
  , HasImageBuilder(imageBuilder)
#if 0
  , cacheDerivedImagesBackground
  , startImageBuilder
  -- , queueImageBuild
#endif

  , fixImageShapes
  , validateImageKey
  , validateImageFile
  , test1
  , tests
  ) where

--import Debug.Trace
--import Control.Concurrent (ThreadId{-, threadDelay-})
import Control.Concurrent.Chan (Chan)
import Control.Exception (Exception)
import Control.Lens ( (%=), _1, _2, _3, at, ix, over, preview, _Right, view )
import Control.Monad (unless, when)
import Control.Monad.RWS ( get, modify, MonadIO(liftIO), MonadState, put, RWST(runRWST) )
import Control.Monad.Reader ( MonadReader(ask), ReaderT, runReaderT )
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import Data.Acid ( AcidState, makeAcidic, openLocalStateFrom, Query, Update, query, update )
import Data.Binary.Get ( getLazyByteString, Get, skip, bytesRead, getWord16be, getWord32be, getWord16le, getWord32le, runGetOrFail )
import qualified Data.ByteString as BS ( ByteString, empty, readFile )
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Lazy as LBS ( ByteString, unpack, pack, take, drop, concat )
--import qualified Data.ByteString.UTF8 as P ( toString )
import Data.Char ( isSpace )
import Data.Default (def)
import Data.Digest.Pure.MD5 (md5)
import Data.FileCache.Common
import Data.FileCache.ImageType (fileInfoFromPath)
import Data.FileCache.LogException (logException)
import Data.FileCache.Types
import Data.Generics.Product ( field )
import Data.List ( intercalate )
import Data.ListLike ( length, show, toString )
import Data.Map.Strict as Map ( delete, difference, fromList, Map, fromSet, insert, intersection, lookup, toList, union )
import Data.Maybe (fromMaybe)
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.Ratio ((%))
import Data.Set as Set (member, Set)
import Data.String (fromString)
import Data.Text as T ( cons, pack, Text, unpack )
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Typeable (typeOf)
import Data.Word ( Word16, Word32 )
import Extra.Except (lyftIO', lyftIO, HasIOException(..), HasNonIOException(..), MonadError, throwError, tryError)
import Extra.Log ( alog )
import Extra.Text hiding (tests)
import GHC.Exts (fromString)
import GHC.Int ( Int64 )
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Network.URI ( URI(..), uriToString )
import Numeric ( fromRat, showFFloat )
import Prelude hiding (length, show)
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), makeRelative, takeDirectory )
import System.FilePath.Extra ( writeFileReadable )
import System.IO (hFlush, stdout)
import System.Log.Logger ( Priority(..) )
import System.Posix.Files (createLink)
import qualified System.Process.ListLike as LL ( showCreateProcessForUser )
import System.Process ( CreateProcess(..), CmdSpec(..), proc, showCommandForUser, shell )
import System.Process.ByteString.Lazy as LBS ( readCreateProcessWithExitCode )
import System.Process.ListLike as LL ( ListLikeProcessIO, readCreateProcessWithExitCode, readCreateProcess )
import Test.HUnit ( assertEqual, Test(..) )
import Test.QuickCheck
import Text.Parsec ( Parsec, (<|>), many, parse, char, digit, newline, noneOf, oneOf, satisfy, space, spaces, string, many1, optionMaybe )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), prettyShow, text )
import UnexceptionalIO.Trans (Unexceptional)
import UnexceptionalIO.Trans as UIO hiding (lift, ErrorCall)
import Web.Routes (fromPathInfo, PathInfo, toPathInfo)
import Web.Routes.QuickCheck (pathInfoInverse_prop)

-- * Orphan Instances

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)

-- | We can infer the file type from the key using insider info on how
-- the various IO operations work.
instance HasImageShape a => HasImageType (ImageKey, a) where
  imageType (ImageOriginal _ typ, _) = typ
  imageType (ImageUpright key, shape) = imageType (key, shape)
  imageType (ImageCropped crop key, shape) =
    if crop == def then imageType (key, shape) else JPEG
  imageType (ImageScaled sz dpi key, shape) =
    let scale' = scaleFromDPI sz dpi shape
        scale :: Double
        scale = fromRat (fromMaybe 1 scale') in
      if approx (toRational scale) == 1 then imageType (key, shape) else JPEG

-- * Processes and IO

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    $logException ERROR (LL.readCreateProcessWithExitCode p s)

pipeline ::
  forall m. (Unexceptional m, HasCallStack)
  => [CreateProcess]
  -> BS.ByteString
  -> ExceptT FileError m BS.ByteString
pipeline [] bytes = return bytes
pipeline (p : ps) bytes =
  lyftIO' (LL.readCreateProcessWithExitCode p bytes) >>= doResult
  where
    doResult :: (ExitCode, BS.ByteString, BS.ByteString) -> ExceptT FileError m BS.ByteString
    -- doResult (Left e) = unsafeFromIO (alog "Appraisal.ImageFile" ERROR (LL.showCreateProcessForUser p ++ " -> " ++ show e)) >> throwError e
    doResult (ExitSuccess, out, _) = pipeline ps out
    doResult (code, _, err) =
      let message = (LL.showCreateProcessForUser p ++ " -> " ++ show code ++ " (" ++ show err ++ ")") in
        unsafeFromIO (alog "Appraisal.ImageFile" ERROR message) >>
        -- Not actually an IOExeption, this is a process error exit
        (throwError (fromString message))

-- * FileCacheTop

newtype FileCacheTop = FileCacheTop {_unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- directory of a file cache.
class HasFileCacheTop a where fileCacheTop :: a -> FileCacheTop
instance HasFileCacheTop FileCacheTop where fileCacheTop = id
-- instance HasFileCacheTop (ImageAcid, FileCacheTop) where fileCacheTop = snd
-- instance HasFileCacheTop (ImageAcid, FileCacheTop, c) where fileCacheTop = view _2

type CacheAcid = AcidState CacheMap
class HasCacheAcid a where cacheAcid :: a -> AcidState CacheMap
instance  HasCacheAcid CacheAcid where cacheAcid = id
instance  HasCacheAcid (CacheAcid, top) where cacheAcid = fst
instance  HasCacheAcid (CacheAcid, a, b) where cacheAcid = view _1

-- * Events

-- | Install a key/value pair into the cache.
putValue ::
     ImageKey
  -> Either FileError ImageFile
  -> Update CacheMap ()
putValue key img = do
  field @"_unCacheMap" %= Map.insert key img

-- | Install several key/value pairs into the cache.
putValues :: Map ImageKey (Either FileError ImageFile) -> Update CacheMap ()
putValues pairs = do
  field @"_unCacheMap" %= Map.union pairs

-- | Look up a key.
lookValue :: ImageKey -> Query CacheMap (Maybe (Either FileError ImageFile))
lookValue key = view (field @"_unCacheMap" . at key)

-- | Look up several keys.
lookValues :: Set ImageKey -> Query CacheMap (Map ImageKey (Either FileError ImageFile))
lookValues keys = Map.intersection <$> view (field @"_unCacheMap") <*> pure (Map.fromSet (const ()) keys)

-- | Return the entire cache.  (Despite what ghc says, this constraint
-- isn't redundant, without it the makeAcidic call has a missing Ord
-- key instance.)
lookMap :: Query CacheMap CacheMap
lookMap = ask

-- | Remove values from the database.
deleteValue :: ImageKey -> Update CacheMap ()
deleteValue key = field @"_unCacheMap" %= Map.delete key

deleteValues :: Set ImageKey -> Update CacheMap ()
deleteValues keys = field @"_unCacheMap" %= (`Map.difference` (Map.fromSet (const ()) keys))

replace :: CacheMap  -> Update CacheMap ()
replace = put

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues, 'replace])

openCache :: FilePath -> IO (AcidState CacheMap)
openCache path = openLocalStateFrom path initCacheMap

initCacheMap :: CacheMap
initCacheMap = CacheMap mempty

-- * ByteString

class MakeByteString a where
  makeByteString :: (Unexceptional m) => a -> ExceptT FileError m BS.ByteString

instance MakeByteString BS.ByteString where
  makeByteString = return

instance MakeByteString FilePath where
  makeByteString path = lyftIO' (BS.readFile path)

instance MakeByteString CreateProcess where
  makeByteString cmd = makeByteString (cmd, BS.empty)

instance MakeByteString (CreateProcess, BS.ByteString) where
  makeByteString (cmd, input) = do
    (code, bytes, _err) <- lyftIO' (readCreateProcessWithExitCode' cmd input)
    case code of
      ExitSuccess -> return bytes
      ExitFailure _ ->
        throwError $ CommandFailure [StartedFrom "MakeByteString CreateProcess",
                                     CommandCreateProcess cmd,
                                     CommandExitCode code]

instance MakeByteString URI where
  makeByteString uri = do
    let cmd = proc "curl" ["-s", uriToString id uri ""]
    (code, bytes, _err) <-
      lyftIO' $ readCreateProcessWithExitCode' cmd BS.empty
    case code of
      ExitSuccess -> return bytes
      _ -> throwError $ CommandFailure [StartedFrom "instance MakeByteString URI",
                                        CommandCreateProcess cmd,
                                        CommandExitCode code]

-- * Image IO

-- | Try to create a version of an image with its orientation
-- corrected based on the EXIF orientation flag.  If the image is
-- already upright this will return Nothing.
uprightImage' :: Unexceptional m => BS.ByteString -> m (Maybe BS.ByteString)
uprightImage' bs =
  -- Use lyftIO' to turn the IOException into ExceptT FileError m,
  -- then flatten the two layers of ExceptT FileError into one, then
  -- turn the remaining one into a Maybe.
  either (const Nothing) (Just . toStrict) <$> runExceptT (normalizeOrientationCode (fromStrict bs))
    -- runExceptT (runExceptT (lyftIO' (normalizeOrientationCode (fromStrict bs))) >>= liftEither)

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
  forall m. Unexceptional m
  => LBS.ByteString
  -> ExceptT FileError m LBS.ByteString
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
    Right x -> throwError $ fromString $ "Unexpected exif orientation code: " <> show x
    Left x -> throwError $ fromString $ "Failure parsing exif orientation code: " <> show x
    where
      transform :: [String] -> Int64 -> Bool -> ExceptT FileError m LBS.ByteString
      transform args pos isMotorola = do
        let cp = proc cmd args'
            cmd = "jpegtran"
            args' = (["-copy", "all"] ++ args)
            hd = LBS.take pos bs            -- everything before the orientation code
            flag = LBS.pack (if isMotorola then [0x0, 0x1] else [0x1, 0x0]) -- orientation code 1
            tl = LBS.drop (pos + 2) bs      -- everything after the orientation code
            bs' = LBS.concat [hd, flag, tl]
        (result, out, err) <- lyftIO' (LBS.readCreateProcessWithExitCode (proc cmd args') bs')
        case result of
          ExitSuccess -> return out
          ExitFailure _ -> throwError $ CommandFailure [CommandErr (toStrict err), CommandCreateProcess cp, CommandExitCode result]

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
validateJPG :: FilePath -> IO (Either String (Integer, Integer))
validateJPG path = do
  (_code, bs, _) <- LL.readCreateProcess (proc "jpegtopnm" [path]) mempty :: IO (ExitCode, BS.ByteString, BS.ByteString)
  (_code, s1', _) <- LL.readCreateProcess (proc "pnmfile" []) bs :: IO (ExitCode, BS.ByteString, BS.ByteString)
  let s1 = decodeUtf8 s1'
  case parse parsePnmfileOutput path s1 of
    Left e -> return (Left ("Error parsing " ++ show s1 ++ ": " ++ show e))
    Right (Pnmfile _ _ (w, h, _)) -> do
      (_code, s2, _) <- LL.readCreateProcess (proc "extractbb" ["-O", path]) ("" :: Text) :: IO (ExitCode, Text, Text)
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

-- | Build an image resized by decoding, applying pnmscale, and then
-- re-encoding.  The new image inherits attributes of the old (other
-- than size.)
scaleImage' ::
  Unexceptional m
  => Double
  -> BS.ByteString
  -> ImageType
  -> ExceptT FileError m (Maybe BS.ByteString)
scaleImage' scale _ _ | approx (toRational scale) == 1 = return Nothing
scaleImage' _ _ PDF = throwError (NoShape "scaleImage'")
scaleImage' _ _ Unknown = throwError (NoShape "scaleImage'")
scaleImage' scale bytes typ = do
    let decoder = case typ of
                    JPEG -> showCommandForUser "jpegtopnm" ["-"]
                    PPM -> showCommandForUser "cat" ["-"]
                    GIF -> showCommandForUser "giftopnm" ["-"]
                    PNG -> showCommandForUser "pngtopnm" ["-"]
                    PDF -> error "scaleImge' - Unexpected file type"
                    Unknown -> error "scaleImge' - Unexpected file type"
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
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
    forall m shape. (Unexceptional m, HasImageShape shape)
    => ImageCrop -> BS.ByteString -> ImageType -> shape -> ExceptT FileError m (Maybe BS.ByteString)
editImage' crop _ _ _ | crop == def = return Nothing
editImage' _ _ PDF _ = throwError (NoShape "editImage'")
editImage' _ _ Unknown _ = throwError (NoShape "editImage'")
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

-- * FileCacheTop

-- | The common suffix of the path to the image URI and its server
-- FilePath.
class HasImageFilePath a where
  toFilePath :: a -> FilePath
  toURIDir :: a -> FilePath

instance HasImageFilePath (Checksum, ImageType) where
  toURIDir (csum, _typ) = take 2 $ unpack csum
  toFilePath p@(csum, typ) =
     toURIDir p </> unpack (csum <> fileExtension typ)

instance HasImageFilePath ImagePath where
  toURIDir (ImagePath (ImageOriginal csum typ) _) = toURIDir (csum, typ)
  toURIDir (ImagePath (ImageUpright key) typ) = toURIDir (ImagePath key typ)
  toURIDir (ImagePath (ImageScaled _ _ key) typ) = toURIDir (ImagePath key typ)
  toURIDir (ImagePath (ImageCropped _ key) typ) = toURIDir (ImagePath key typ)
  -- for backwards compatibility, special case ImageOriginal
  toFilePath (ImagePath (ImageOriginal csum typ) _) = toFilePath (csum, typ)
  toFilePath p = toURIDir p </> makeRelative "/" (unpack (toPathInfo p))

instance HasImageFilePath ImageCached where
  toURIDir c = toURIDir (imagePath c)
  toFilePath c@(ImageCached _ _) = toFilePath (imagePath c)

instance HasImageFilePath (ImageKey, ImageFile) where
  toURIDir (key, ImageFileShape shape) = toURIDir (key, shape)
  toURIDir (key, ImageFileReady ready) = toURIDir (key, ready)
  toFilePath (key, ImageFileShape shape) = toFilePath (key, shape)
  toFilePath (key, ImageFileReady ready) = toFilePath (key, ready)

instance HasImageFilePath (ImageKey, ImageReady) where
  toURIDir (key, ImageReady _ shape) = toURIDir (key, shape)
  toFilePath (key, ImageReady _ shape) = toFilePath (key, shape)

instance HasImageFilePath (ImageKey, ImageShape) where
  toURIDir (key, shape) = toURIDir (ImagePath key (_imageShapeType shape))
  toFilePath (key, shape) = toFilePath (ImagePath key (_imageShapeType shape))

-- | The full path name for the local cache of the file.
fileCachePath :: (HasImageFilePath a, MonadReader r m, HasFileCacheTop r) => a -> m FilePath
fileCachePath file = do
  (FileCacheTop top) <- fileCacheTop <$> ask
  let path = toFilePath file
  return $ top </> makeRelative "/" path

-- | Create any missing directories and evaluate 'fileCachePath'
fileCachePathIO :: (HasImageFilePath a, MonadReader r m, HasFileCacheTop r, Unexceptional m, MonadError FileError m) => a -> m FilePath
fileCachePathIO file = do
  path <- fileCachePath file
  let dir = takeDirectory path
  lyftIO (createDirectoryIfMissing True dir)
  return path

-- * FileCacheT

type FileCacheT r s m = RWST r () s m

runFileCacheT :: r -> s -> FileCacheT r s m a -> m (a, s, ())
runFileCacheT r s0 action = runRWST action r s0

evalFileCacheT :: Functor m => r -> s -> FileCacheT r s m a -> m a
evalFileCacheT r s0 action = view _1 <$> runFileCacheT r s0 action
execFileCacheT :: Functor m => r -> s -> FileCacheT r s m a-> m s
execFileCacheT r s0 action = view _2 <$> runFileCacheT r s0 action

askCacheAcid :: (MonadReader r m, HasCacheAcid r) => m CacheAcid
askCacheAcid = cacheAcid <$> ask

-- FIXME - the result should be (Either FileError val), the FileError
-- is part of the cached value now, its not an exception.
cachePut ::
  forall r e m. (Unexceptional m, Exception e, MonadError e m,
                 HasIOException e, HasNonIOException e, MonadReader r m, HasCacheAcid r)
  => ImageKey -> Either FileError ImageFile -> m (Either FileError ImageFile)
cachePut key val = do
  st <- askCacheAcid
  lyftIO (update st (PutValue key val))
  return val

-- | Same as 'cachePut' but returns ().  Same FIXME applies.
cachePut' ::
  forall e r m. (Unexceptional m, Exception e, MonadError e m,
                 HasIOException e, HasNonIOException e, MonadReader r m, HasCacheAcid r)
  => ImageKey -> Either FileError ImageFile -> m ()
cachePut' key val = do
  st <- askCacheAcid
  lyftIO (update st (PutValue key val))

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  forall r m. (Unexceptional m, MonadReader r m, HasCacheAcid r)
  => ImageKey -> m (Maybe (Either FileError ImageFile))
cacheLook key = do
  st <- askCacheAcid
  handleQueryException <$> runExceptT (lyftIO $ query st (LookValue key))
  where
    -- Should we distinguish between errors that occurred building the
    -- image file and error that occurred during the query?  The
    -- correct answer is usually yes, and soon I will get to that.
    -- But at the moment this code merges those errors into the result.
    handleQueryException :: Either FileError (Maybe (Either FileError ImageFile)) ->  Maybe (Either FileError ImageFile)
    handleQueryException (Left e) = Just (Left e)
    handleQueryException (Right (Just r)) = Just r
    handleQueryException (Right Nothing) = Nothing

cacheMap ::
  (Unexceptional m, Exception e, MonadError e m,
   HasIOException e, HasNonIOException e, MonadReader r m, HasCacheAcid r)
  => m CacheMap
cacheMap = do
  st <- askCacheAcid
  lyftIO (query st LookMap)

cacheDelete ::
  forall e r m. (Unexceptional m, Exception e, MonadError e m,
                 HasIOException e, HasNonIOException e, MonadReader r m, HasCacheAcid r)
  => Proxy ImageFile -> Set ImageKey -> m ()
cacheDelete _ keys = do
  (st :: AcidState CacheMap) <- cacheAcid <$> ask
  lyftIO (update st (DeleteValues keys))

-- * ImageCache

type ImageCacheT r s m = FileCacheT r s m
type HasImageAcid r = HasCacheAcid r
type ImageAcid = CacheAcid
imageAcid :: HasImageAcid a => a -> ImageAcid
imageAcid = cacheAcid

-- | Run some 'Unexceptional' image cache IO in a a MonadIO instance.
evalImageCacheUIO ::
  forall r s m a. (MonadIO m, MonadState s m, MonadReader r m)
  => FileCacheT r s (ExceptT SomeNonPseudoException UIO) a
  -> ExceptT SomeNonPseudoException m a
evalImageCacheUIO io = do
  (r :: r) <- ask
  s0 <- get
  result <- liftIO (run (runExceptT (runFileCacheT r s0 io)))
  either throwError (\(a, s', ()) -> put s' >> return a) result

-- | Lift an ImageCacheT operation (which has the Unexceptional
-- constraint) into any MonadIO instance.
evalImageCacheIO ::
  forall r s e m a. MonadIO m
  => r
  -> s
  -> FileCacheT r s (ReaderT r (ExceptT e UIO)) a
  -> m (Either e a)
evalImageCacheIO r s0 action =
  liftIO (run (runExceptT (runReaderT (evalFileCacheT r s0 action :: ReaderT r (ExceptT e UIO) a) r
                          ) :: UIO (Either e a)
              ) :: IO (Either e a)
         ) :: m (Either e a)

-- * Create original images

-- | Add some image files to an image repository - updates the acid
-- state image map and copies the file to a location determined by the
-- FileCacheTop and its checksum.
cacheOriginalImages ::
  forall x e r m. (MakeByteString x, Ord x,
                   Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
                   MonadReader r m, HasImageAcid r, HasFileCacheTop r,
                   MonadState (Map x (Either FileError (ImageKey, ImageFile))) m)
  => [(FileSource, x)] -> m ()
cacheOriginalImages =
  mapM_ (\(source, x) -> runExceptT (cacheOriginalImage (Just source) x) >>= modify . Map.insert x)

-- | Build an original (not derived) ImageFile from a URI or a
-- ByteString, insert it into the cache, and return it.
cacheOriginalImage ::
  forall x e r m. (MakeByteString x, Unexceptional m, Exception e, MonadError e m, HasIOException e,
                   HasNonIOException e, MonadReader r m, HasFileCacheTop r, HasImageAcid r, HasCallStack)
  => Maybe FileSource
  -> x
  -> ExceptT FileError m (ImageKey, ImageFile)
cacheOriginalImage source x = do
  img <- buildOriginalImage source x
  let key = originalKey img
      val = ImageFileReady img
  unsafeFromIO (alog "Data.FileCache.Server" DEBUG ("cachePut " ++ show key))
  lift (cachePut' key (Right val))
  return (key, val)

buildOriginalImage ::
  forall x r m. (MakeByteString x, Unexceptional m,
                 MonadReader r m, HasFileCacheTop r)
  => Maybe FileSource
  -> x
  -> ExceptT FileError m ImageReady
buildOriginalImage source x = do
  bs <- makeByteString x
  let csum = T.pack $ show $ md5 $ fromStrict bs
  shape@ImageShape {..} <- imageShapeM bs
  -- FIXME: The image-replace command in appraisalscope will pass
  -- Nothing to the source parameter.  Could the correct source
  -- possibly be found in by looking in the image database?
  let file = File { _fileSource = fromMaybe Missing source
                  , _fileChksum = csum
                  , _fileMessages = []
                  , _fileExt = fileExtension (imageType shape) }
  let img = ImageReady { _imageFile = file, _imageShape = shape }
  path <- fileCachePathIO (ImageCached (ImageOriginal csum _imageShapeType) (ImageFileReady img))
  exists <- lyftIO $ doesFileExist path
  unless exists $ lyftIO $ writeFileReadable path bs
  return img

type ImageChan = Chan [(ImageKey, ImageShape)]
class HasImageBuilder a where imageBuilder :: a -> Maybe ImageChan
instance HasImageBuilder ImageChan where imageBuilder = Just
instance HasImageBuilder (a, b, ImageChan) where imageBuilder = Just . view _3

-- * Create derived images

-- | This is just a wrapper around cacheDerivedImagesForeground.
getImageFile ::
  forall r e m. (Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
                 MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => ImageKey
  -> m (Either FileError ImageFile)
getImageFile key = do
  preview (ix key) <$> (cacheDerivedImagesForeground mempty [key] :: m (Map ImageKey (Either FileError ImageFile))) >>=
    maybe missingKey (either (return . Left) (return . Right))
  where
    missingKey = do
      unsafeFromIO $ alog "Appraisal.LaTeX" WARNING ("getImageFile missingKey: " ++ show key)
      return (Left (CacheDamage $ "cacheDerivedImagesForeground failed for " <> textshow key))

getImageFiles ::
  forall r e m. (Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
                 MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => [ImageKey] -> m (Map ImageKey (Either FileError ImageFile))
getImageFiles = cacheDerivedImagesForeground mempty

-- getImageFile' :: forall r s e m. AppLaTeXStrict r s e m => ImageKey -> m ImageCached
-- getImageFile' key = getImageFile key >>= either (throwError . fromFileError) return

data CacheFlag
  = RetryErrors
  | RetryShapes
  deriving (Eq, Ord, Show)

-- Is this guaranteed to have a map entry for every key passed in?
cacheDerivedImagesForeground ::
  forall r e m. (Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
                 MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
cacheDerivedImagesForeground flags keys =
  cacheLookImages keys >>=
  mapM (cacheImageShape flags) >>=
  runExceptT . foregroundBuilds >>=
  either throwError (return . Map.fromList)

#if 0
cacheDerivedImagesBackground ::
  forall r e m. (Unexceptional m, Exception e, MonadError e m, HasFileError e,
                 HasIOException e, HasNonIOException e, MonadReader r m, HasCacheAcid r,
                 HasImageBuilder r, HasFileCacheTop r)
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
cacheDerivedImagesBackground flags keys =
  cacheLookImages keys >>=
  mapM (cacheImageShape flags) >>=
  runExceptT . backgroundBuilds >>=
  either throwError (return . Map.fromList)
#endif

-- | See if images are already in the cache
cacheLookImages ::
  (Unexceptional m, MonadReader r m, HasCacheAcid r)
  => [ImageKey] -> m [(ImageKey, Maybe (Either FileError ImageFile))]
cacheLookImages keys = mapM (\key -> (key,) <$> cacheLook key) keys

-- | Compute the shapes of requested images
cacheImageShape ::
  (Unexceptional m, Exception e, MonadReader r m, HasCallStack,
   HasIOException e, HasNonIOException e, MonadError e m, HasCacheAcid r)
  => Set CacheFlag
  -> (ImageKey, Maybe (Either FileError ImageFile))
  -> m (ImageKey, Either FileError ImageFile)
cacheImageShape _ (key, Nothing) = do
  unsafeFromIO $ alog "Appraisal.LaTeX" DEBUG ("cacheImageShape key=" ++ prettyShow key ++ " (miss)")
  cachePut' key (noShape ("cacheImageShape " <> show key <> " :: " <> show (typeOf key)))
  (key,) <$> (runExceptT (buildImageShape key) >>= cachePut key . over _Right ImageFileShape)
cacheImageShape flags (key, Just (Left _))
  | Set.member RetryErrors flags = do
      unsafeFromIO $ alog "Appraisal.LaTeX" INFO ("cacheImageShape key=" ++ prettyShow key ++ " (retry)")
      (key,) <$> (runExceptT (buildImageShape key) >>= cachePut key . over _Right ImageFileShape)
cacheImageShape _ (key, Just (Left e)) = do
  unsafeFromIO $ alog "Appraisal.LaTeX" INFO ("cacheImageShape key=" ++ prettyShow key ++ " (error)")
  return (key, Left e)
cacheImageShape _ (key, Just (Right (ImageFileShape shape))) = do
  unsafeFromIO $ alog "Appraisal.LaTeX" INFO ("cacheImageShape key=" ++ prettyShow key ++ " (shape)")
  -- This value shouldn't be here in normal operation
  return (key, Right (ImageFileShape shape))
cacheImageShape _ (key, Just (Right (ImageFileReady img))) = do
  unsafeFromIO $ alog "Appraisal.LaTeX" INFO ("cacheImageShape key=" ++ prettyShow key ++ " (hit)")
  return (key, Right (ImageFileReady img))

#if 0
backgroundBuilds ::
  (Unexceptional m, Exception e, HasFileError e, HasIOException e, HasNonIOException e,
   MonadReader r m, HasImageBuilder r, HasFileCacheTop r)
  => [(ImageKey, Either FileError ImageFile)]
  -> ExceptT e m [(ImageKey, Either FileError ImageFile)]
backgroundBuilds pairs =
  queueImageBuild (mapMaybe isShape pairs) >> return pairs
  where isShape (key, Right (ImageFileShape shape)) = Just (key, shape)
        isShape _ = Nothing
#endif

foregroundBuilds ::
  (Unexceptional m, MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => [(ImageKey, Either FileError ImageFile)]
  -> m [(ImageKey, Either FileError ImageFile)]
foregroundBuilds pairs =
  mapM (uncurry doImage) pairs
  where
    doImage key (Right (ImageFileShape shape)) =
      (key,) <$> (cacheImageFile key shape)
    doImage key x = return (key, x)

noShape :: Text -> Either FileError ImageFile
noShape = Left . NoShape

#if 0
-- | Insert an image build request into the channel that is being polled
-- by the thread launched in startCacheImageFileQueue.
queueImageBuild ::
  (Unexceptional m, Exception e, HasFileError e, HasIOException e, HasNonIOException e,
   MonadReader r m, HasImageBuilder r, HasFileCacheTop r, HasCallStack)
  => [(ImageKey, ImageShape)]
  -> ExceptT e m ()
queueImageBuild pairs = do
  -- Write empty files into cache
  -- mapM (runExceptT . fileCachePathIO) pairs >>= mapM_ (either (throwError . fromFileError) (lyftIO . flip writeFile mempty))
  unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("queueImageBuild - requesting " ++ show (length pairs) ++ " images")
  chan <- lift (imageBuilder <$> ask)
  lyftIO (writeChan chan pairs)
  unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("queueImageBuild - requested " ++ show (length pairs) ++ " images")

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startImageBuilder ::
  forall r e m. (Unexceptional m, Exception e, HasIOException e, HasNonIOException e,
                 MonadError e m, MonadReader r m, HasImageAcid r, HasFileCacheTop r, HasCallStack)
  => m (ImageChan, ThreadId)
startImageBuilder = do
  r <- ask
  (chan :: ImageChan) <- lyftIO newChan
  (,) <$> pure chan <*> fork (forever (runExceptT (fromIO' (review someNonPseudoException) (readChan chan)) >>= either doError (doImages r)))
  where
    -- This is the background task
    doError (e :: SomeNonPseudoException) =
      unsafeFromIO $ alog "Data.FileCache.Server" ERROR ("Failure reading image cache request channel: " ++ show e)
    doImages :: r -> [(ImageKey, ImageShape)] -> UIO ()
    doImages r pairs = do
      unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("doImages - building " ++ show (length pairs) ++ " images")
      -- the threadDelay is to test the behavior of the server for lengthy image builds
      r <- mapM (\(key, shape) -> runExceptT @e (runReaderT (cacheImageFile' key shape {- >> unsafeFromIO (threadDelay 5000000)-}) r)) pairs
      mapM_ (\case ((key, shape), Left e) -> unsafeFromIO (alog "Data.FileCache.Server" ERROR ("doImages - error building " <> show key <> ": " ++ show e))
                   ((key, shape), Right (Left e)) -> unsafeFromIO (alog "Data.FileCache.Server" ERROR ("doImages - error in cache for " <> show key <> ": " ++ show e))
                   ((key, shape), Right (Right _e)) -> unsafeFromIO (alog "Data.FileCache.Server" ERROR ("doImages - completed " <> show key)))
        (zip pairs r)
#endif

cacheImageFile ::
  forall r m. (Unexceptional m, MonadReader r m, HasImageAcid r, HasFileCacheTop r)
  => ImageKey
  -> ImageShape
  -> m (Either FileError ImageFile)
cacheImageFile key shape = do
  r <- ask
  -- Errors that occur in buildImageFile are stored in the cache, errors
  -- that occur in cachePut are returned.  Actually that's not right.
  either Left id <$> runExceptT (evalFileCacheT r () (runExceptT (buildImageFile key shape) >>= cachePut key))

cacheImageFile' ::
  (HasIOException e, HasNonIOException e, Unexceptional m, Exception e,
   MonadError e m, HasFileCacheTop r, HasCacheAcid r, MonadReader r m)
  => ImageKey -> ImageShape -> m (Either FileError ImageFile)
cacheImageFile' key shape = do
  r <- ask
  -- Errors that occur in buildImageFile are stored in the cache, errors
  -- that occur in cachePut are returned.  Actually that's not right.
  evalFileCacheT r () (runExceptT (buildImageFile key shape) >>= cachePut key)

-- | These are meant to be inexpensive operations that determine the
-- shape of the desired image, with the actual work of building them
-- deferred and forked into the background.  Are they actually
-- inexpensive?  We read the original bytestring and call file(1) to
-- get the shape and orientation of that image, and the other
-- operations are pure.
buildImageShape ::
  (Unexceptional m, MonadReader r m, HasImageAcid r)
  => ImageKey -> ExceptT FileError m ImageShape
buildImageShape key@(ImageOriginal _csum _typ) =
  lift (cacheLook key) >>=
  maybe (throwError (CacheDamage $ ("Original image not in cache: key=" <> pack (show key))))
        (either throwError
                (\case ImageFileReady img -> return (_imageShape img)
                       ImageFileShape s -> return s))
  -- buildOriginalImageBytes csum typ >>= getFileInfo
buildImageShape (ImageUpright key) =
  uprightImageShape <$> buildImageShape key
buildImageShape (ImageCropped crop key) =
  cropImageShape crop <$> buildImageShape key
buildImageShape (ImageScaled sz dpi key) =
  scaleImageShape sz dpi <$> buildImageShape key

uprightImageShape :: ImageShape -> ImageShape
uprightImageShape shape@(ImageShape {_imageFileOrientation = rot}) =
  case rot of
    ZeroHr -> shape
    SixHr -> shape
    ThreeHr -> shape
    NineHr -> shape

buildImageFile ::
  forall e r m. (Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
                 MonadReader r m, HasImageAcid r, HasFileCacheTop r, HasCallStack)
  => ImageKey -> ImageShape -> ExceptT FileError m ImageFile
buildImageFile key shape = do
  (key', bs) <- buildImageBytes Nothing key -- key' may differ from key due to removal of no-ops
  let file = File { _fileSource = Derived
                  , _fileChksum = T.pack $ show $ md5 $ fromStrict bs
                  , _fileMessages = []
                  , _fileExt = fileExtension (_imageShapeType shape) }
  let img = ImageFileReady (ImageReady { _imageFile = file, _imageShape = shape })
  path <- fileCachePathIO (ImageCached key img) -- the rendered key
  exists <- lyftIO' $ doesFileExist path
  path' <- fileCachePathIO (ImageCached key' img) -- the equivalent file
  -- path' should exist, hard link path to path'
  case exists of
    False -> do
      case key == key' of
        True -> do
          unsafeFromIO $ alog "Data.FileCache.Server" INFO ("Writing new cache file: " <> show path)
          lyftIO' $ writeFileReadable path bs
        False -> do
          unsafeFromIO $ alog "Data.FileCache.Server" INFO ("Hard linking " <> show path' <> " -> " <> show path)
          lyftIO' $ createLink path' path
    True -> do
      -- Don't mess with it if it exists, there is probably
      -- a process running that is writing it out.
      bs' <- lyftIO' $ BS.readFile path
      case bs == bs' of
        False -> do
          unsafeFromIO $ alog "Data.FileCache.Server" WARNING ("Replacing damaged cache file: " <> show path <> " length " <> show (length bs') <> " -> " <> show (length bs))
          lyftIO' $ writeFileReadable path bs
        True -> unsafeFromIO $ alog "Data.FileCache.Server" WARNING ("Cache file for new key already exists: " <> show path)
  unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("added to cache: " <> prettyShow img)
  return img

-- | Retrieve the 'ByteString' associated with an 'ImageKey'.
buildImageBytes ::
  forall r e m. (Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
                 MonadReader r m, HasFileCacheTop r, HasImageAcid r)
  => Maybe FileSource -> ImageKey -> ExceptT FileError m (ImageKey, BS.ByteString)
buildImageBytes source key@(ImageOriginal csum typ) =
  lift (cacheLook key) >>=
  maybe ((key,) <$> buildImageBytesFromFile source key csum typ)
        (\img -> (key,) <$> either (rebuildImageBytes source False key typ)
                                   (lookImageBytes . ImageCached key) img)
buildImageBytes source key@(ImageUpright key') = do
  (key'', bs) <- buildImageBytes source key'
  uprightImage' bs >>= return . maybe (key'', bs) (key,)
buildImageBytes source key@(ImageScaled sz dpi key') = do
  (key'', bs) <- buildImageBytes source key'
  -- the buildImageBytes that just ran might have this info
  shape <- imageShapeM bs
  let scale' = scaleFromDPI sz dpi shape
  case scale' of
    Nothing -> return (key'', bs)
    Just scale ->
      maybe (key'', bs) (key,) <$> scaleImage' (fromRat scale) bs (imageType shape)
buildImageBytes source key@(ImageCropped crop key') = do
  (key'', bs) <- buildImageBytes source key'
  shape <- imageShapeM bs
  maybe (key'', bs) (key,) <$> editImage' crop bs (imageType shape) (imageShape shape)

-- There's a chance the file is just sitting on disk even though
-- it is not in the database - see if we can read it and verify
-- its checksum.
buildImageBytesFromFile ::
  (Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
   MonadReader r m, HasImageAcid r, HasFileCacheTop r, HasCallStack)
  => Maybe FileSource -> ImageKey -> Text -> ImageType -> ExceptT FileError m BS.ByteString
buildImageBytesFromFile source key csum typ = do
  -- If we get a cache miss for an ImageOriginal key something
  -- has gone wrong.  Try to rebuild from the file if it exists.
  path <- fileCachePath (ImagePath key typ)
  exists <- lyftIO' (doesFileExist path)
  case exists of
    False -> do
      let e = CacheDamage ("buildImageBytes - missing original: " <> T.pack (show key <> " -> " <> path))
      lift (cachePut' key (Left e :: Either FileError ImageFile))
      throwError e
    True -> do
      bs <- makeByteString path
      let csum' = T.pack $ show $ md5 $ fromStrict bs
      case csum' == csum of
        False -> do
          let e = CacheDamage ("buildImageBytes - original damaged: " <> T.pack (show key <> " -> " <> path))
          unsafeFromIO (alog "Data.FileCache.Server" ERROR ("Checksum mismatch - " <> show e))
          lift (cachePut' key (Left e :: Either FileError ImageFile))
          throwError e
        True -> do
          unsafeFromIO (alog "Data.FileCache.Server" ALERT ("recaching " ++ show key))
          _cached <- cacheOriginalImage source bs
          return bs

-- | Look up the image FilePath and read the ByteString it contains.
lookImageBytes ::
  (MonadReader r m, HasFileCacheTop r, HasIOException e, HasNonIOException e,
   Unexceptional m, Exception e, HasImageFilePath a)
  => a -> ExceptT e m BS.ByteString
lookImageBytes a = fileCachePath a >>= lyftIO' . BS.readFile

-- | There is an error stored in the cache, maybe it can be repaired
-- now?  Be careful not to get into a loop doing this.
rebuildImageBytes ::
  forall r e m. (Unexceptional m, Exception e, MonadError e m, HasIOException e, HasNonIOException e,
                 MonadReader r m, HasImageAcid r, HasFileCacheTop r, HasCallStack)
  => Maybe FileSource -> Bool -> ImageKey -> ImageType -> FileError -> ExceptT FileError m BS.ByteString
rebuildImageBytes _ False _key _typ e = throwError e
rebuildImageBytes source True key typ e@(CacheDamage _) = do
  unsafeFromIO (alog "Data.FileCache.Server" ALERT ("Retrying build of " ++ show key ++ " (e=" ++ show e ++ ")"))
  path <- fileCachePath (ImagePath key typ)
  -- This and other operations like it may throw an
  -- IOException - I need LyftIO to make sure this is caught.
  bs <- lyftIO' (BS.readFile path)
  _cached <- cacheOriginalImage source bs
  return bs
rebuildImageBytes _ True key _typ e = do
  unsafeFromIO (alog "Data.FileCache.Server" INFO ("Not retrying build of " ++ show key ++ " (e=" ++ show e ++ ")"))
  throwError e

-- | Integrity testing
validateImageKey ::
  forall r m. (Unexceptional m, MonadError FileError m,
               MonadReader r m, HasImageAcid r, HasFileCacheTop r)
  => ImageKey -> m ()
validateImageKey key = do
  cacheLook key >>=
    maybe (throwError (CacheDamage ("validateImageKey - missing: " <> pack (show key))))
          (either (\e -> (throwError (CacheDamage ("validateImageKey - image " <> pack (show key) <> " got error from cacheLook: " <> pack (show e)))))
                  (validateImageFile key))

validateImageFile ::
  forall r m. (Unexceptional m, MonadError FileError m, MonadReader r m, HasFileCacheTop r)
  => ImageKey -> ImageFile -> m ()
validateImageFile _key (ImageFileShape _) = return ()
validateImageFile key (ImageFileReady i@(ImageReady {..})) = do
  path <- fileCachePath (ImagePath key (imageType i))
  when (imageType i == JPEG)
    (lyftIO (validateJPG path) >>=
     either (\e -> throwError (CacheDamage ("image " <> pack (show (fileChecksum _imageFile)) <> " not a valid jpeg: " <> pack (show e))))
            (\_ -> return ()))
  runExceptT (lyftIO (BS.readFile path)) >>= checkFile _imageFile path
  where
    checkFile :: File -> FilePath -> Either FileError BS.ByteString -> m ()
    checkFile _file _path (Left e) =
      unsafeFromIO (putStrLn ("error loading " ++ show _imageFile ++ ": " ++ show e))
    checkFile file _path (Right bs)
      | T.pack (show (md5 (fromStrict bs))) /= (_fileChksum file) =
          unsafeFromIO (putStrLn ("checksum mismatch in file " ++ show file))
    checkFile _file _path _bs = return ()

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]

-- splits (splitAt 3) [1,2,3,4,5,6,7] -> [[1,2,3],[4,5,6],[7]]
splits :: ([a] -> ([a], [a])) -> [a] -> [[a]]
splits _ [] = []
splits f xs = let (lhs, rhs) = f xs in (lhs : splits f rhs)

-- | The migration of 'ImageKey' sets the 'ImageType' field to
-- 'Unknown' everywhere, this looks at the pairs in the cache map and
-- copies the 'ImageType' of the 'ImageFile' into the keys.  We can't
-- do this in the CacheMap migration above because the types are too
-- specific.  But this should be removed when 'ImageKey' version 2 is
-- removed.
--
-- Also recomputes the orientation field of ImageShape.
fixImageShapes ::
  forall r m. (Unexceptional m, MonadReader r m, HasFileCacheTop r)
  => Map ImageKey (Either FileError ImageFile)
  -> m (Map ImageKey (Either FileError ImageFile))
fixImageShapes mp =
  let mp' = fromList (fixOriginalKeys (changes mp) (toList mp)) in
    (fromList . concat) <$> mapM fixPairs (splits (splitAt 100) (toList mp'))
  where
    fixPairs :: [(ImageKey, Either FileError ImageFile)] -> m [(ImageKey, Either FileError ImageFile)]
    fixPairs pairs = unsafeFromIO (putStr "." >> hFlush stdout) >> mapM fixPair pairs
    fixPair :: (ImageKey, Either FileError ImageFile) -> m (ImageKey, Either FileError ImageFile)
    -- If already damaged use the result
    fixPair (key, Left e) = either ((key,) . Left) id <$> runExceptT (fixImageCached (key, Left e))
    -- If not damaged only use the result if it succeeds
    fixPair (key, Right val) = either (\_ -> (key, Right val)) id <$> runExceptT (fixImageCached (key, Right val))
    fixImageCached :: (ImageKey, Either FileError ImageFile) -> ExceptT FileError m (ImageKey, Either FileError ImageFile)
    fixImageCached (key@(ImageOriginal csum typ), Right (ImageFileShape _s)) =
      fixImageShape (csum, typ) >>= \s' -> return (key, Right (ImageFileShape s'))
    fixImageCached (key@(ImageOriginal csum typ), Right (ImageFileReady (ImageReady f _s))) =
      fixImageShape (csum, typ) >>= \s' -> return (key, Right (ImageFileReady (ImageReady f s')))
    fixImageCached x = return x
    -- Actually we just compute it from scratch
    fixImageShape :: HasImageFilePath a => a -> ExceptT FileError m ImageShape
    fixImageShape a = fileCachePath a >>= imageShapeM . (, BS.empty)

instance (Unexceptional m, MonadReader r m, HasFileCacheTop r) => HasImageShapeM (ExceptT FileError m) (Checksum, ImageType) where
  imageShapeM (csum, typ) = fileCachePath (csum, typ) >>= fileInfoFromPath . (, BS.empty)

-- | The repairs we need to perform on the original keys during 'setImageFileTypes'.
changes ::
     Map ImageKey (Either FileError ImageFile)
  -> (ImageKey, Either FileError ImageFile)
  -> (ImageKey, Either FileError ImageFile)
changes mp = \(key, file) -> (maybe key id (Map.lookup key changeMap), file)
  where
    changeMap :: Map ImageKey ImageKey
    changeMap = fromList (fmap (\(key, img) -> (key, fixOriginalKey key img)) (toList mp))
    fixOriginalKey :: ImageKey -> (Either FileError ImageFile) -> ImageKey
    fixOriginalKey (ImageOriginal csum Unknown) (Right img) = ImageOriginal csum (imageType img)
    fixOriginalKey key _img = key

fixOriginalKeys ::
     ((ImageKey, Either FileError ImageFile) -> (ImageKey, Either FileError ImageFile))
  -> [(ImageKey, Either FileError ImageFile)]
  -> [(ImageKey, Either FileError ImageFile)]
fixOriginalKeys f pairs = fmap f pairs

instance Arbitrary ImageKey where
  arbitrary = scaled $ crop $ upright original
    where
      original :: Gen ImageKey
      original = ImageOriginal <$> (pack <$> listOf1 (elements "0123456789abcdef")) <*> arbitrary
      upright :: Gen ImageKey -> Gen ImageKey
      upright i = oneof [i, ImageUpright <$> i]
      crop :: Gen ImageKey -> Gen ImageKey
      crop i = oneof [i, ImageCropped <$> arbitrary <*> i]
      scaled :: Gen ImageKey -> Gen ImageKey
      scaled i = oneof [i, ImageScaled
                             <$> ((_unSaneSize . saneSize) <$> arbitrary)
                             <*> ((approx . abs) <$> arbitrary)
                             <*> i]

instance Arbitrary ImageSize where
  arbitrary = (ImageSize <$> arbitrary <*> ((approx . abs) <$> arbitrary) <*> arbitrary)

instance Arbitrary ImageCrop where
  arbitrary = ImageCrop <$> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> arbitrary

instance Arbitrary ImageType where
  arbitrary = oneof [pure PPM, pure JPEG, pure GIF, pure PNG, pure PDF, pure Unknown]

instance Arbitrary Dimension where arbitrary = elements [minBound..maxBound]
instance Arbitrary Units where arbitrary = elements [minBound..maxBound]
instance Arbitrary Rotation where arbitrary = elements [ZeroHr, ThreeHr, SixHr, NineHr]

#if 1
test1 :: Test
test1 =
  TestCase (assertEqual "test1"
              -- This is not the path to the file on disk, its what is used in the URI.
              ("/image-path/image-scaled/image-size/the-area/15/1/inches/100/1/image-upright/c3bd1388b41fa5d956e4308ce518a8bd.png" :: Text
            -- "/image-path/image-scaled/image-size/the-area/15/1/inches/100/1/image-upright/image-original/c3bd1388b41fa5d956e4308ce518a8bd/i.png" :: Text
              )
              (toPathInfo (_imageCachedKey file)))
  where
    file = ImageCached
             {_imageCachedKey = ImageScaled
                                  (ImageSize {_dim = TheArea, _size = 15 % 1, _units = Inches})
                                  (100 % 1)
                                  (ImageUpright (ImageOriginal "c3bd1388b41fa5d956e4308ce518a8bd" PNG)),
              _imageCachedFile = ImageFileReady (ImageReady {_imageFile = File {_fileSource = Legacy, _fileChksum = "be04a29700b06072326364fa1ce45f39", _fileMessages = [], _fileExt = ".jpg"},
                                            _imageShape = ImageShape {_imageShapeType = JPEG, _imageShapeWidth = 885, _imageShapeHeight = 170, _imageFileOrientation = ZeroHr}})}

#endif

testKey :: ImageKey -> IO ()
testKey key = do
  putStrLn (show key)
  let path = toPathInfo key :: Text
  putStrLn (unpack path)
  case fromPathInfo (encodeUtf8 path) of
    Left s -> putStrLn s
    Right (key' :: ImageKey) -> do
      putStrLn (show key')
      putStrLn (unpack (toPathInfo key'))

-- This fails because approx actually isn't idempotent.  I'm not 100%
-- sure whether it should be or not.
--
-- > approx (1409154553389 % 1397579329319)
-- 121 % 120
-- > approx (121 % 120)
-- 120 % 119
-- > approx (120 % 119)
-- 119 % 118
approx_prop :: Rational -> Bool
approx_prop r = approx r == approx (approx r)

quickTests = do
  quickCheck (withMaxSuccess 1000 (pathInfoInverse_prop :: ImageKey -> Bool))
  -- quickCheck (approx_prop :: Rational -> Bool)

{-
> toPathInfo (ImageScaled (ImageSize {_dim = TheWidth, _size = 1 % 4, _units = Inches}) (142 % 163) (ImageCropped (ImageCrop {topCrop = 0, bottomCrop = 0, leftCrop = 1, rightCrop = 1, rotation = SixHr}) (ImageUpright (ImageOriginal "" PNG))))
"/image-scaled/image-size/the-width/1/4/inches/115/132/image-cropped/image-crop/0/0/1/1/six-hr/image-upright/image-original//i.png"
toPathSegments (ImageScaled (ImageSize {_dim = TheWidth, _size = 1 % 4, _units = Inches}) (142 % 163) (ImageCropped (ImageCrop {topCrop = 0, bottomCrop = 0, leftCrop = 1, rightCrop = 1, rotation = SixHr}) (ImageUpright (ImageOriginal "" PNG))))
["image-scaled","image-size","the-width","1","4","inches","115","132","image-cropped","image-crop","0","0","1","1","six-hr","image-upright","image-original","","i.png"]
% fromPathInfo "/image-scaled/image-size/the-width/1/4/inches/115/132/image-cropped/image-crop/0/0/1/1/six-hr/image-upright/image-original//i.png" :: Either String ImageKey
Right (ImageScaled (ImageSize {_dim = TheWidth, _size = 1 % 4, _units = Inches}) (115 % 132) (ImageCropped (ImageCrop {topCrop = 0, bottomCrop = 0, leftCrop = 1, rightCrop = 1, rotation = SixHr}) (ImageUpright (ImageOriginal "" PNG))))
-}
