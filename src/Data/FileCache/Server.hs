{-# LANGUAGE DeriveLift, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, UndecidableInstances #-}

module Data.FileCache.Server
  ( -- * Cache Events
    initCacheMap
  , openCache
  , PutValue(..)
  , PutValues(..)
  , LookValue(..)
  , LookValues(..)
  , LookMap(..)
  , DeleteValue(..)
  , DeleteValues(..)
  -- * Image IO
  , validateJPG
  -- * File Cache
  , HasFileCachePath(fileCacheDir, fileCachePath, fileCachePathIO)
  , FileCacheT
  , runFileCacheT, evalFileCacheT, execFileCacheT -- , writeFileCacheT
  , MonadFileCache(askCacheAcid, buildCacheValue)
  , cacheInsert, cacheLook, cacheMap, cacheDelete, cacheMiss, cachePut
  -- * Image Cache
  , ImageCacheT
  , MonadImageCache
  , cacheImageOriginal
  , cacheImageOriginals
  , cacheImagesByKey
  , cacheImageByKey

  , validateImageKey
  , validateImageFile
  , tests
  ) where

-- import "regex-compat-tdfa" Text.Regex ( Regex, mkRegex, matchRegex )
import Control.Exception ( throw )
import Control.Lens ( (%=), _1, _2, at, view )
import Control.Monad ( unless, when )
import Control.Monad.Catch ( MonadCatch, try )
import Control.Monad.RWS ( modify, MonadState, RWST(runRWST) )
import Control.Monad.Reader ( MonadReader(ask) )
import Control.Monad.Trans ( MonadTrans(lift), liftIO )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import Data.Acid ( AcidState, makeAcidic, openLocalStateFrom, Query, Update, query, update )
import Data.Binary.Get ( getLazyByteString, Get, skip, bytesRead, getWord16be, getWord32be, getWord16le, getWord32le, runGetOrFail )
import qualified Data.ByteString as BS ( ByteString, empty, readFile )
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Lazy as LBS ( ByteString, unpack, pack, take, drop, concat )
--import qualified Data.ByteString.UTF8 as P ( toString )
import Data.Char ( isSpace )
import Data.Digest.Pure.MD5 (md5)
import Data.FileCache.Common
import Data.FileCache.ImageType (getFileInfo)
import Data.FileCache.LogException (logException)
import Data.Generics ( Typeable )
import Data.Generics.Product ( field )
import Data.List ( intercalate )
import Data.Map.Strict as Map ( delete, difference, Map, fromSet, insert, intersection, union )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.SafeCopy ( SafeCopy )
import Data.Set as Set ( Set )
import Data.Text as T ( pack, Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Word ( Word16, Word32 )
import Extra.Except ( catchError, liftEither, logIOError, MonadError, MonadIO, throwError, tryError )
import Extra.Log ( alog )
import GHC.IO.Exception ( IOException )
import GHC.Int ( Int64 )
import Language.Haskell.TH.Instances ()
import Network.URI ( URI(..), uriToString )
import Numeric ( fromRat, showFFloat )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), makeRelative, takeDirectory )
import System.FilePath.Extra ( writeFileReadable )
import System.Log.Logger ( Priority(..) )
import qualified System.Process.ListLike as LL ( showCreateProcessForUser )
import System.Process ( CreateProcess(..), CmdSpec(..), proc, showCommandForUser, shell )
import System.Process.ByteString.Lazy as LBS ( readCreateProcessWithExitCode )
import System.Process.ListLike as LL ( ListLikeProcessIO, readCreateProcessWithExitCode, readCreateProcess )
import Test.HUnit ( assertEqual, Test(..) )
import Text.Parsec ( Parsec, (<|>), many, parse, char, digit, newline, noneOf, oneOf, satisfy, space, spaces, string, many1, optionMaybe )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- * Orphan Instances

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)

-- * Processes

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    $logException ERROR (LL.readCreateProcessWithExitCode p s)

pipeline :: [CreateProcess] -> BS.ByteString -> IO BS.ByteString
pipeline [] bytes = return bytes
pipeline (p : ps) bytes =
    (LL.readCreateProcessWithExitCode p bytes >>= doResult)
      `catchError` (\ (e :: IOException) -> doException (LL.showCreateProcessForUser p ++ " -> " ++ show e) e)
    where
      doResult (ExitSuccess, out, _) = pipeline ps out
      doResult (code, _, err) = let message = (LL.showCreateProcessForUser p ++ " -> " ++ show code ++ " (" ++ show err ++ ")") in doException message (userError message)
      -- Is there any exception we should ignore here?
      doException message e = alog "Appraisal.ImageFile" ERROR message >> throw e

pipe' :: [String] -> String
pipe' = intercalate " | "

-- * Events

-- | Install a key/value pair into the cache.
putValue :: Ord key => key -> Either FileError val -> Update (CacheMap key val) (Either FileError val)
putValue key img = do
  field @"_unCacheMap" %= Map.insert key img
  return img

-- | Install several key/value pairs into the cache.
putValues :: Ord key => Map key (Either FileError val) -> Update (CacheMap key val) (Map key (Either FileError val))
putValues pairs = do
  field @"_unCacheMap" %= Map.union pairs
  return pairs

-- | Look up a key.
lookValue :: Ord key => key -> Query (CacheMap key val) (Maybe (Either FileError val))
lookValue key = view (field @"_unCacheMap" . at key)

-- | Look up several keys.
lookValues :: Ord key => Set key -> Query (CacheMap key val) (Map key (Either FileError val))
lookValues keys = Map.intersection <$> view (field @"_unCacheMap") <*> pure (Map.fromSet (const ()) keys)

-- | Return the entire cache.  (Despite what ghc says, this constraint
-- isn't redundant, without it the makeAcidic call has a missing Ord
-- key instance.)
lookMap :: Query (CacheMap key val) (CacheMap key val)
lookMap = ask

-- | Remove values from the database.
deleteValue :: (Ord key{-, Serialize key, Serialize val, Serialize e-}) => key -> Update (CacheMap key val) ()
deleteValue key = field @"_unCacheMap" %= Map.delete key

deleteValues :: Ord key => Set key -> Update (CacheMap key val) ()
deleteValues keys = field @"_unCacheMap" %= (`Map.difference` (Map.fromSet (const ()) keys))

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues])

openCache :: (SafeCopy key, Typeable key, Ord key,
              SafeCopy val, Typeable val) => FilePath -> IO (AcidState (CacheMap key val))
openCache path = openLocalStateFrom path initCacheMap

initCacheMap :: Ord key => CacheMap key val
initCacheMap = CacheMap mempty

-- * ByteString

class MakeByteString a where
  makeByteString :: (MonadIO m) => a -> ExceptT FileError m BS.ByteString

instance MakeByteString FilePath where
  makeByteString path = liftIO $ BS.readFile path

instance MakeByteString CreateProcess where
  makeByteString cmd = makeByteString (cmd, BS.empty)

instance MakeByteString (CreateProcess, BS.ByteString) where
  makeByteString (cmd, input) = do
    (code, bytes, _err) <- liftIO (readCreateProcessWithExitCode' cmd input)
    case code of
      ExitSuccess -> return bytes
      ExitFailure _ ->
        throwError $ CommandFailure (FunctionName "MakeByteString CreateProcess" (Command (T.pack (show cmd)) (T.pack (show code))))

instance MakeByteString URI where
  makeByteString uri = do
    let cmd = proc "curl" ["-s", uriToString id uri ""]
    (code, bytes, _err) <-
      liftIO $ readCreateProcessWithExitCode' cmd BS.empty
    case code of
      ExitSuccess -> return bytes
      _ -> throwError $ CommandFailure (FunctionName "MakeByteString URI" (Command (T.pack (show cmd)) (T.pack (show code))))

-- * Image IO

-- | Try to create a version of an image with its orientation
-- corrected based on the EXIF orientation flag.  If the image is
-- already upright this will return Left.
uprightImage' :: BS.ByteString -> IO (Maybe BS.ByteString)
uprightImage' bs = $logException ERROR $ do
  normalizeOrientationCode (fromStrict bs) >>=
    either (const (return Nothing)) (return . Just . toStrict)

-- | Given a bytestring containing a JPEG file, examine the EXIF
-- orientation flag and if it is something other than 1 transform the
-- image into the "normal" orientation and change the orientation flag
-- to 1.  The result is the new bytestring.  If the old bytestring was
-- already normalized, or absolutely anything else goes wrong, the
-- result is a Left.  This means the original bytestring should be used.
--
-- This is an IO operation because it runs jpegtran(1) to perform the
-- transformation on the jpeg image.
normalizeOrientationCode :: LBS.ByteString -> IO (Either String LBS.ByteString)
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
    Right x -> return $ Left $ "Unexpected exif orientation code: " ++ show x
    Left x -> return $ Left $ "Failure parsing exif orientation code: " ++ show x
    where
      transform :: [String] -> Int64 -> Bool -> IO (Either String LBS.ByteString)
      transform args pos isMotorola = do
        let cmd = "jpegtran"
            args' = (["-copy", "all"] ++ args)
            hd = LBS.take pos bs            -- everything before the orientation code
            flag = LBS.pack (if isMotorola then [0x0, 0x1] else [0x1, 0x0]) -- orientation code 1
            tl = LBS.drop (pos + 2) bs      -- everything after the orientation code
            bs' = LBS.concat [hd, flag, tl]
        (result, out, err) <- LBS.readCreateProcessWithExitCode (proc cmd args') bs'
        case result of
          ExitSuccess -> return $ Right out
          ExitFailure n -> return $ Left $ showCommandForUser cmd args' ++ " -> " ++ show n ++ "\n error output: " ++ show err

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
  MonadIO m
  => Double
  -> BS.ByteString
  -> ImageType
  -> ExceptT FileError m (Maybe BS.ByteString)
scaleImage' scale _ _ | approx (toRational scale) == 1 = return Nothing
scaleImage' scale bytes typ = do
    let decoder = case typ of
                    JPEG -> showCommandForUser "jpegtopnm" ["-"]
                    PPM -> showCommandForUser "cat" ["-"]
                    GIF -> showCommandForUser "giftopnm" ["-"]
                    PNG -> showCommandForUser "pngtopnm" ["-"]
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
        -- To save space, build a jpeg here rather than the original file type.
        encoder = case typ of
                    JPEG -> showCommandForUser "cjpeg" []
                    PPM -> showCommandForUser {-"cat"-} "cjpeg" []
                    GIF -> showCommandForUser {-"ppmtogif"-} "cjpeg" []
                    PNG -> showCommandForUser {-"pnmtopng"-} "cjpeg" []
        cmd = pipe' [decoder, scaler, encoder]
    Just <$> makeByteString (shell cmd, bytes)

editImage' ::
    forall m shape. (MonadIO m, PixmapShape shape)
    => ImageCrop -> BS.ByteString -> ImageType -> shape -> ExceptT FileError m (Maybe BS.ByteString)
editImage' crop bs typ shape =
  logIOError $
    case commands of
      [] -> return Nothing
      _ -> Just <$> liftIO (pipeline commands bs)
    where
      commands = buildPipeline typ [cut, rotate] (latexImageFileType typ)
      -- We can only embed JPEG and PNG images in a LaTeX
      -- includegraphics command, so here we choose which one to use.
      latexImageFileType GIF = JPEG
      latexImageFileType PPM = JPEG
      latexImageFileType JPEG = JPEG
      latexImageFileType PNG = JPEG
      cut = case (leftCrop crop, rightCrop crop, topCrop crop, bottomCrop crop) of
              (0, 0, 0, 0) -> Nothing
              (l, r, t, b) -> Just (PPM, proc "pnmcut" ["-left", show l,
                                                        "-right", show (w - r - 1),
                                                        "-top", show t,
                                                        "-bottom", show (h - b - 1)], PPM)
      rotate = case rotation crop of
                 90 -> Just (JPEG, proc "jpegtran" ["-rotate", "90"], JPEG)
                 180 -> Just (JPEG, proc "jpegtran" ["-rotate", "180"], JPEG)
                 270 -> Just (JPEG, proc "jpegtran" ["-rotate", "270"], JPEG)
                 _ -> Nothing
      w = pixmapWidth shape
      h = pixmapHeight shape
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

class HasFileCachePath a where
  fileCacheDir :: HasFileCacheTop m => a -> m FilePath
  -- ^ The subdirectory of images where the file will be placed
  fileCachePath :: HasFileCacheTop m => a -> m FilePath
  -- ^ The full path name for the local cache of the file.
  fileCachePathIO :: (HasFileCacheTop m, MonadIO m) => a -> m FilePath
  fileCachePathIO file = do
    path <- fileCachePath file
    liftIO $ createDirectoryIfMissing True $ takeDirectory path
    return path
  -- ^ Create any missing directories and evaluate 'fileCachePath'

instance HasFileCachePath File where
  fileCacheDir file =
    fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> fileDir file
  fileCachePath file =
    fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> filePath file

(<++>) :: FilePath -> FilePath -> FilePath
a <++> b = a </> (makeRelative "" b)

-- * FileCacheT

type FileCacheT key val s m =
  RWST (AcidState (CacheMap key val), FileCacheTop) () s (ExceptT FileError m)

runFileCacheT ::
     acid
  -> FileCacheTop
  -> s
  -> RWST (acid, FileCacheTop) () s m a
  -> m (a, s, ())
runFileCacheT r0 top s0 action = runRWST action (r0, top) s0

evalFileCacheT ::
  Functor m
  => acid
  -> FileCacheTop
  -> s
  -> RWST (acid, FileCacheTop) () s m a
  -> m a
evalFileCacheT r0 top s0 action = view _1 <$> runFileCacheT r0 top s0 action
execFileCacheT :: Functor f => acid -> FileCacheTop -> s -> RWST (acid, FileCacheTop) () s f a -> f s
execFileCacheT r0 top s0 action = view _2 <$> runFileCacheT r0 top s0 action

-- No MonadIO constraint here - not all MonadFileCache operations require
-- MonadIO, and we might want to use MonadIOError instead.
class (HasFileCacheTop m,
       Ord key, SafeCopy key, Typeable key, Show key,
       SafeCopy val, Typeable val)
  => MonadFileCache key val m where
  askCacheAcid :: m (AcidState (CacheMap key val))
  buildCacheValue :: (MonadIO m, MonadCatch m) => key -> ExceptT FileError m val

-- | Call the build function on cache miss to build the value.
cacheInsert ::
  forall key val m. (MonadFileCache key val m, MonadIO m, MonadCatch m)
  => key -> ExceptT FileError m val
cacheInsert key = cacheLook key >>= maybe (cacheMiss key) return

cacheMiss ::
  forall key val m. (MonadFileCache key val m, MonadIO m, MonadCatch m)
  => key -> ExceptT FileError m val
cacheMiss key = tryError (buildCacheValue key) >>= cachePut key

cachePut ::
  forall key val m. (MonadFileCache key val m, MonadIO m)
  => key -> Either FileError val -> ExceptT FileError m val
cachePut key val = do
  st <- lift askCacheAcid
  liftIO (update st (PutValue key val)) >>= liftEither

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  (MonadFileCache key val m, MonadIO m)
  => key -> ExceptT FileError m (Maybe val)
cacheLook key = do
  st <- lift askCacheAcid
  liftIO (query st (LookValue key)) >>=
    maybe (return Nothing) (either throwError (return . Just))

cacheMap ::
  (MonadFileCache key val m, MonadIO m)
  => m (CacheMap key val)
cacheMap = do
  st <- askCacheAcid
  liftIO $ query st LookMap

cacheDelete ::
  forall key val m. (MonadFileCache key val m, MonadIO m)
  => Proxy (val, FileError) -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val)) <- askCacheAcid
  liftIO $ update st (DeleteValues keys)

-- * ImageCache

type ImageCacheT s m = FileCacheT ImageKey ImageFile s m
type MonadImageCache m = MonadFileCache ImageKey ImageFile m

instance HasFileCachePath ImageFile where
  fileCacheDir = fileCacheDir . view (field @"_imageFile")
  fileCachePath = fileCachePath . view (field @"_imageFile")

-- | 'MonadFileCache' instance for images on top of the 'RWST' monad run by
-- 'runFileCacheT'
instance (MonadError e m, acid ~ AcidState (CacheMap ImageKey ImageFile), top ~ FileCacheTop)
  => MonadFileCache ImageKey ImageFile (RWST (acid, top) () s m) where
    askCacheAcid = view _1
    buildCacheValue key = makeImageFile key

buildImage ::
  forall m. (MonadImageCache m, MonadIO m, MonadCatch m)
  => ImageKey
  -> ExceptT FileError m BS.ByteString
buildImage key@(ImageOriginal _) = do
  -- This should already be in the cache
  cacheLook key >>= maybe miss hit
    where miss = throwError (CacheDamage ("Missing original: " <> T.pack (show key)))
          hit (img :: ImageFile) = fileCachePath img >>= liftIO . BS.readFile
buildImage (ImageUpright key) =
  buildImage key >>= \bs -> liftIO (uprightImage' bs) >>= maybe (return bs) return
buildImage (ImageScaled sz dpi key) = do
  bs <- buildImage key
  -- the buildImage that just ran might have this info
  (typ, shape) <- getFileInfo bs
  let scale' = scaleFromDPI sz dpi shape
      scale = fromRat (fromMaybe 1 scale')
  scaleImage' scale bs typ >>= maybe (return bs) return
buildImage (ImageCropped crop key) = do
  bs <- buildImage key
  (typ, shape) <- getFileInfo bs
  editImage' crop bs typ shape >>= maybe (return bs) return

-- | Add some image files to an image repository - updates the acid
-- state image map and copies the file to a location determined by the
-- FileCacheTop and its checksum.
cacheImageOriginals ::
  forall x m. (MakeImageFile x, Ord x, MonadImageCache m, MonadIO m, MonadCatch m,
               MonadState (Map x (Either FileError (ImageKey, ImageFile))) m)
  => [x] -> m ()
cacheImageOriginals =
  mapM_ (\x -> runExceptT (cacheImageOriginal x) >>= modify  . Map.insert x)

-- | Build an original (not derived) ImageFile from a URI or a
-- ByteString, insert it into the cache, and return it.
cacheImageOriginal ::
    forall f m. (MakeImageFile f, MonadIO m, MonadCatch m, MonadImageCache m)
    => f
    -> ExceptT FileError m (ImageKey, ImageFile)
cacheImageOriginal src = do
  (img :: ImageFile) <- makeImageFile src
  let key = originalKey img
  (key,) <$> cachePut key (Right img)

class MakeImageFile a where
  makeImageFile ::
    (MonadIO m, MonadCatch m, MonadImageCache m)
    => a -> ExceptT FileError m ImageFile

instance MakeImageFile BS.ByteString where
  makeImageFile bs = do
    (typ, (width, height)) <- getFileInfo bs
    let file = File { _fileSource = Nothing
                    , _fileChksum = T.pack (md5' bs)
                    , _fileMessages = []
                    , _fileExt = fileExtension typ }
    path <- fileCachePathIO file
    exists <- liftIO $ doesFileExist path
    unless exists $ liftIO $ writeFileReadable path bs
    let img = ImageFile { _imageFile = file
                        , _imageFileType = typ
                        , _imageFileWidth = width
                        , _imageFileHeight = height
                        , _imageFileMaxVal = 1 }
    return img
instance MakeImageFile ImageKey where
  makeImageFile key = do
    buildImage key >>= makeImageFile
instance MakeImageFile URI where
  makeImageFile uri = makeByteString uri >>= makeImageFile
    -- fileFromURI (liftIO . getFileType) uri >>= makeImageFile
instance MakeImageFile FilePath where
  makeImageFile path =
    makeByteString path >>= makeImageFile

-- | Build the image described by the 'ImageKey' if necessary, and
-- return its meta information as an 'ImageFile'.
cacheImageByKey :: forall m. (MonadImageCache m, MonadIO m, MonadCatch m)
  => ImageKey
  -> ExceptT FileError m ImageFile
cacheImageByKey key = cacheInsert key

-- | Build an image map in the state monad
cacheImagesByKey ::
  forall m. (MonadImageCache m, MonadIO m, MonadCatch m,
             MonadState (Map ImageKey (Either FileError ImageFile)) m)
  => [ImageKey]
  -> m ()
cacheImagesByKey =
  mapM_ (\key -> runExceptT (cacheImageByKey key) >>= modify . Map.insert key)

-- | Integrity testing
validateImageKey ::
  forall m. (MonadImageCache m, MonadIO m, MonadCatch m, MonadError FileError m)
  => ImageKey -> m ()
validateImageKey key = do
  runExceptT (cacheLook key :: ExceptT FileError m (Maybe ImageFile)) >>=
    either (\e -> (throwError (CacheDamage ("validateImageKey - image " <> pack (show key) <> " got error from cacheLook: " <> pack (show e)))))
           (maybe (throwError (CacheDamage ("validateImageKey - missing: " <> pack (show key))))
                  validateImageFile)

validateImageFile ::
  forall m. (MonadImageCache m, MonadIO m, MonadCatch m, MonadError FileError m)
  => ImageFile -> m ()
validateImageFile (ImageFile {..}) = do
  path <- fileCachePath _imageFile
  when (_imageFileType == JPEG)
    (liftIO (validateJPG path) >>=
     either (\e -> throwError (CacheDamage ("image " <> pack (show (fileChecksum _imageFile)) <> " not a valid jpeg: " <> pack (show e))))
            (\_ -> return ()))
  bs <- try (liftIO (BS.readFile path) >>= checkFile _imageFile path)
  case bs of
    Left (e :: IOException) -> liftIO (putStrLn ("error loading " ++ show _imageFile ++ ": " ++ show e))
    Right _ -> return ()
  where
    checkFile :: File -> FilePath -> BS.ByteString -> m ()
    checkFile file _path bs
      | T.pack (show (md5 (fromStrict bs))) /= (_fileChksum file) =
          liftIO (putStrLn ("checksum mismatch in file " ++ show file))
    checkFile _file _path _bs = return ()

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]
