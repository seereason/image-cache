{-# LANGUAGE DeriveLift, OverloadedStrings, PackageImports, TemplateHaskell, TupleSections, UndecidableInstances #-}

module Data.FileCache.Server
  ( initCacheMap
  , openCache
  , PutValue(..)
  , PutValues(..)
  , LookValue(..)
  , LookValues(..)
  , LookMap(..)
  , DeleteValue(..)
  , DeleteValues(..)
  , loadBytesUnsafe
  , fileCachePath
  , getFileType
  , validateJPG
  , FileCacheT, W(W)
  , ensureFileCacheTop
  , runFileCacheT, evalFileCacheT, execFileCacheT, writeFileCacheT
  , MonadFileCache(askCacheAcid, buildCacheValue)
  , cacheInsert, cacheLook, cacheMap, cacheDelete, cacheMiss, cachePut
  , ImageCacheT
  , MonadImageCache
  , fromImageCacheT
  , imageFilePath
  , cacheImageOriginal
  , cacheImagesByKey
  , storeByteStrings
  , storeByteString
  , storeImageFiles
  , storeImageFile

  , tests
  ) where

import "regex-compat-tdfa" Text.Regex ( Regex, mkRegex, matchRegex )
import Control.Exception ( throw )
import Control.Lens ( (%=), _1, _2, _3, _Left, over, makeLensesFor, at, _Right, view, set )
import Control.Monad ( unless, when )
import Control.Monad.Catch ( try )
import Control.Monad.RWS ( RWST(runRWST) )
import Control.Monad.Reader ( MonadReader(ask), ReaderT )
import Control.Monad.Trans ( MonadTrans(lift), liftIO )
import Control.Monad.Trans.Except ( ExceptT, runExceptT )
import Data.Acid ( AcidState, makeAcidic, openLocalStateFrom, Query, Update, query, update )
import Data.Binary.Get ( getLazyByteString, Get, skip, bytesRead, getWord16be, getWord32be, getWord16le, getWord32le, runGetOrFail )
import qualified Data.ByteString as BS ( ByteString, empty, readFile )
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Lazy as LBS ( ByteString, unpack, pack, take, drop, concat )
import qualified Data.ByteString.UTF8 as P ( toString )
import Data.Char ( isSpace )
import Data.FileCache.Common
import Data.FileCache.LogException (logException)
import Data.Generics ( Typeable )
import Data.Generics.Product ( field )
import Data.List ( intercalate )
import Data.Map.Strict as Map ( delete, difference, fromList, Map, fromSet, insert, intersection, union )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.SafeCopy ( SafeCopy )
import Data.Set as Set ( Set )
import Data.String (fromString)
import Data.Text as T ( pack, take, Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Word ( Word16, Word32 )
import Extra.Except ( catchError, liftEither, logIOError, MonadError, MonadIO, throwError, tryError, withExceptT )
import Extra.Log ( alog )
import GHC.IO.Exception ( IOException )
import GHC.Int ( Int64 )
import Language.Haskell.TH.Instances ()
import Network.URI ( URI(..), uriToString )
import Numeric ( fromRat, showFFloat )
import System.Directory ( createDirectoryIfMissing, doesFileExist, getDirectoryContents, renameFile )
import System.Exit ( ExitCode(..) )
import System.FilePath ( makeRelative, (</>) )
import System.FilePath.Extra ( writeFileReadable, makeReadableAndClose )
import System.IO ( openBinaryTempFile )
import System.Log.Logger ( Priority(ERROR), logM, Priority(DEBUG, CRITICAL) )
import qualified System.Process.ListLike as LL ( readProcessWithExitCode, showCreateProcessForUser )
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

showCmdSpec :: CmdSpec -> String
showCmdSpec (ShellCommand s) = s
showCmdSpec (RawCommand p ss) = showCommandForUser p ss

pipe :: [CreateProcess] -> CreateProcess
pipe xs = foldl1 (<>) xs

instance Semigroup CreateProcess where
  a <> b =
    if cwd a == cwd b &&
       env a == env b &&
       close_fds a == close_fds b &&
       create_group a == create_group b
    then a {cmdspec = ShellCommand (showCmdSpec (cmdspec a) ++ " | " ++ showCmdSpec (cmdspec b))}
    else error $ "Pipeline of incompatible commands: " ++ LL.showCreateProcessForUser a ++ " | " ++ LL.showCreateProcessForUser b

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

{-
pipelineWithExitCode :: [(String, [String])] -> B.ByteString -> IO (ExitCode, B.ByteString, [B.ByteString])
pipelineWithExitCode cmds inp =
    pipeline' cmds inp (ExitSuccess, [])
    where
      pipeline' _ bytes (code@(ExitFailure _), errs) = return (code, bytes, errs)
      pipeline' [] bytes (code, errs) = return (code, bytes, reverse errs)
      pipeline' ((cmd, args) : rest) bytes (code, errs) =
          do (code, out, err) <- readProcessWithExitCode cmd args bytes
             pipeline' rest out (code, err : errs)

showPipelineForUser :: [(String, [String])] -> String
showPipelineForUser ((cmd, args) : rest) =
    showCommandForUser cmd args ++
    case rest of 
      [] -> ""
      _ -> " | " ++ showPipelineForUser rest
-}
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

-- * FileCacheTop

-- | Read and return the contents of the file from the cache as a
-- ByteString.  Verify that the checksum matches the checksum field of
-- the 'File'.
loadBytesSafe ::
    forall e m. (HasFileError e, MonadIO m, MonadError e m, HasFileCacheTop m)
    => File -> m BS.ByteString
loadBytesSafe file =
    do path <- fileCachePath file
       bytes <- readFileBytes path
       case T.pack (md5' bytes) == _fileChksum file of
         True -> return bytes
         -- If the checksum of the file we read from the cache does
         -- not match its checksum field, we've got serious trouble.
         -- We should probably try to read back files when we create
         -- them
         False -> do
           let msg = "Checksum mismatch: expected " ++ show (_fileChksum file) ++ ", file contains " ++ show (md5' bytes)
           liftIO $ logM "Appraisal.FileCache" CRITICAL msg
           throwError $ fromFileError (CacheDamage ("Checksum problem in " <> T.pack (show file)))

-- | Load an image file without verifying its checksum
loadBytesUnsafe :: ({-HasFileError e,-} MonadIO m, HasFileCacheTop m) => File -> m BS.ByteString
loadBytesUnsafe file = fileCachePath file >>= readFileBytes

readFileBytes :: MonadIO m => FilePath -> m BS.ByteString
readFileBytes path = liftIO $ BS.readFile path

fileCachePathIO :: (MonadIO m, HasFileCacheTop m) => File -> m FilePath
fileCachePathIO file = do
  dir <- fileCacheDir file
  liftIO $ createDirectoryIfMissing True dir
  fileCachePath file

-- | The full path name for the local cache of the file.
fileCachePath :: HasFileCacheTop m => File -> m FilePath
fileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> filePath file

fileCacheDir :: HasFileCacheTop m => File -> m FilePath
fileCacheDir file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> fileDir file

(<++>) :: FilePath -> FilePath -> FilePath
a <++> b = a </> (makeRelative "" b)

-- * File

-- | Turn the bytes in a ByteString into a File.  This is an IO
-- operation because it saves the data into the local cache.  We
-- use writeFileReadable because the files we create need to be
-- read remotely by our backup program.
fileFromBytes ::
    forall m. (MonadIO m, HasFileCacheTop m)
    => Extension
    -> BS.ByteString
    -> m File
fileFromBytes fileExt bytes =
      do let file = File { _fileSource = Nothing
                         , _fileChksum = T.pack (md5' bytes)
                         , _fileMessages = []
                         , _fileExt = fileExt }
         path <- fileCachePathIO file
         exists <- liftIO $ doesFileExist path
         unless exists (liftIO (writeFileReadable path bytes))
         return file

-- |Read the contents of a local path into a File.
fileFromPath ::
    forall m a. (MonadIO m, HasFileCacheTop m, HasFileExtension a)
    => (BS.ByteString -> m a)
    -> FilePath
    -> m (File, a)
fileFromPath byteStringInfo path = do
  bytes <- liftIO $ BS.readFile path
  a <- byteStringInfo bytes
  file <- fileFromBytes (fileExtension a) bytes
  return (set (field @"_fileSource") (Just (ThePath path)) file, a)

-- | A shell command whose output becomes the contents of the file.
fileFromCmd ::
    forall e m a. (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m, HasFileExtension a)
    => (BS.ByteString -> m a)
    -> CreateProcess
    -> m (File, a)
fileFromCmd byteStringInfo cmd = do
  (code, bytes, _err) <- liftIO (readCreateProcessWithExitCode' cmd BS.empty)
  case code of
    ExitSuccess ->
        do a <- byteStringInfo bytes
           file <- fileFromBytes (fileExtension a) bytes
           return $ (set (field @"_fileSource") (Just (ThePath (show cmd))) file, a)
    ExitFailure _ ->
        throwError $ (fromFileError :: FileError -> e) $ CommandFailure (FunctionName "fileFromCmd" (Command (T.pack (show cmd)) (T.pack (show code))))

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI ::
    forall e m a. (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m, HasFileExtension a)
    => (BS.ByteString -> m a)
    -> URI
    -> m (File, a)
fileFromURI byteStringInfo uri =
    do let args = ["-s", uriToString id uri ""]
           cmd = (proc "curl" args)
       (code, bytes, _err) <- liftIO $ readCreateProcessWithExitCode' cmd BS.empty
       case code of
         ExitSuccess ->
             do a <- byteStringInfo bytes
                file <- fileFromBytes (fileExtension a) bytes
                return (set (field @"_fileSource") (Just (TheURI (uriToString id uri ""))) file, a)
         _ -> throwError $ fromFileError $ CommandFailure (FunctionName "fileFromURI" (Command (T.pack (show cmd)) (T.pack (show code))))

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]

-- * Image IO

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

-- | Helper function to learn the 'ImageType' of a file by runing
-- @file -b@.
getFileType :: BS.ByteString -> IO ImageType
getFileType bytes =
    LL.readProcessWithExitCode cmd args bytes >>= test . view _2
    where
      cmd = "file"
      args = ["-b", "-"]
      test :: Monad m => BS.ByteString -> m ImageType
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

-- | Helper function to build an image once its type is known - JPEG,
-- GIF, etc.
imageFileFromType :: FilePath -> File -> ImageType -> IO ImageFile
imageFileFromType path file typ = do
  -- logM "Appraisal.ImageFile.imageFileFromType" DEBUG ("Appraisal.ImageFile.imageFileFromType - typ=" ++ show typ) >>
  let cmd = case typ of
              JPEG -> pipe [proc "jpegtopnm" [path], proc "pnmfile" []]
              PPM ->  (proc "pnmfile" [])
              GIF -> pipe [proc "giftopnm" [path], proc "pnmfile" []]
              PNG -> pipe [proc "pngtopnm" [path], proc "pnmfile" []]
  -- err may contain "Output file write error --- out of disk space?"
  -- because pnmfile closes the output descriptor of the decoder
  -- process early.  This can be ignored.
  (code, out, _err) <- LL.readCreateProcessWithExitCode cmd BS.empty
  case code of
    ExitSuccess -> imageFileFromPnmfileOutput file typ out
    ExitFailure _ -> error $ "Failure building image file:\n " ++ showCmdSpec (cmdspec cmd) ++ " -> " ++ show code

-- | Helper function to load a PNM file.
imageFileFromPnmfileOutput :: File -> ImageType -> BS.ByteString -> IO ImageFile
imageFileFromPnmfileOutput file typ out =
        case matchRegex pnmFileRegex (P.toString out) of
          Just [width, height, _, maxval] ->
            return $ ImageFile { _imageFile = file
                               , _imageFileType = typ
                               , _imageFileWidth = read width
                               , _imageFileHeight = read height
                               , _imageFileMaxVal = if maxval == "" then 1 else read maxval }
          _ -> error $ "Unexpected output from pnmfile: " ++ show out
  where
      pnmFileRegex = mkRegex "^stdin:\tP[PGB]M raw, ([0-9]+) by ([0-9]+)([ ]+maxval ([0-9]+))?$"

-- | The image file names are just checksums.  This makes sure a link
-- with a suitable extension (.jpg, .gif) also exists.
-- ensureExtensionLink :: MonadFileCacheIO st IOException m => File -> String -> m ()
-- ensureExtensionLink file ext = fileCachePath file >>= \ path -> liftIO $ ensureLink (view fileChksum file) (path ++ ext)

-- * FileCacheT

type FileCacheT key val s m = RWST (AcidState (CacheMap key val), FileCacheTop) W s (ExceptT FileError m)

data W = W
instance Semigroup W where W <> W = W
instance Monoid W where mempty = W; mappend = (<>)

runFileCacheT ::
     acid
  -> FileCacheTop
  -> s
  -> RWST (acid, FileCacheTop) W s m a
  -> m (a, s, W)
runFileCacheT r0 top s0 action = runRWST action (r0, top) s0

evalFileCacheT ::
  Functor m
  => acid
  -> FileCacheTop
  -> s
  -> RWST (acid, FileCacheTop) W s m a
  -> m a
evalFileCacheT r0 top s0 action = view _1 <$> runFileCacheT r0 top s0 action
execFileCacheT r0 top s0 action = view _2 <$> runFileCacheT r0 top s0 action
writeFileCacheT r0 top s0 action = view _3 <$> runFileCacheT r0 top s0 action

ensureFileCacheTop :: MonadIO m => FileCacheT key val s m ()
ensureFileCacheTop = do
  fileCacheTop >>= lift . lift . liftIO . createDirectoryIfMissing True . _unFileCacheTop

-- No MonadIO constraint here - not all MonadFileCache operations require
-- MonadIO, and we might want to use MonadIOError instead.
class (HasFileCacheTop m,
       Ord key, SafeCopy key, Typeable key, Show key,
       SafeCopy val, Typeable val) => MonadFileCache key val m where
    askCacheAcid :: m (AcidState (CacheMap key val))
    buildCacheValue :: (MonadIO m, MonadError e m, HasFileError e) => key -> m (Either FileError val)

-- | Call the build function on cache miss to build the value.
cacheInsert ::
  forall key val e m. (MonadFileCache key val m, MonadIO m, MonadError e m, HasFileError e)
  => key -> m (Either FileError val)
cacheInsert key = do
  st <- askCacheAcid
  liftIO (query st (LookValue key)) >>= maybe (cacheMiss key) return

cacheMiss ::
  forall key val e m. (MonadFileCache key val m, MonadIO m, MonadError e m, HasFileError e)
  => key -> m (Either FileError val)
cacheMiss key = buildCacheValue key >>= cachePut key

cachePut ::
  forall key val m. (MonadFileCache key val m, MonadIO m)
  => key -> (Either FileError val) -> m (Either FileError val)
cachePut key val = do
  st <- askCacheAcid
  liftIO $ update st (PutValue key val)

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  (MonadFileCache key val m, MonadIO m)
  => key -> m (Maybe (Either FileError val))
cacheLook key = do
  st <- askCacheAcid
  liftIO $ query st (LookValue key)

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

-- * ImageFile

type ImageCacheT s m = FileCacheT ImageKey ImageFile s m
type MonadImageCache m = MonadFileCache ImageKey ImageFile m

-- | Build and return the 'ImageFile' described by the 'ImageKey'.
buildImageFile ::
  forall e m. (MonadImageCache m, MonadIO m, MonadError e m, HasFileError e)
  => ImageKey
  -> m (Either FileError ImageFile)
buildImageFile key@(ImageOriginal _) = do
  -- This should already be in the cache
  (r :: Maybe (Either FileError ImageFile)) <- cacheLook key
  case r of
    -- Should we write this into the cache?  Probably not, if we leave
    -- it as it is the software could later corrected.
    Nothing -> return (Left (CacheDamage ("Missing original: " <> T.pack (show key))))
    Just c -> return c
  -- maybe (throwError (fromFileError (CacheDamage ("Missing original: " <> pack (show key))) :: e)) (return . _unCached) r
buildImageFile (ImageUpright key) = do
  -- mapError (\m -> either (Left . fromFileError) Right <$> m) $
  buildImageFile key >>= overCached uprightImage
buildImageFile (ImageScaled sz dpi key) = do
  buildImageFile key >>= overCached (\img ->
                                        let scale = scaleFromDPI sz dpi img in
                                        logIOError $ scaleImage (fromRat (fromMaybe 1 scale)) img)
buildImageFile (ImageCropped crop key) = do
  buildImageFile key >>= overCached (editImage crop)

overCached ::
  Monad m
  => (a -> m (Either FileError a))
  -> Either FileError a
  -> m (Either FileError a)
overCached f (Right a) = f a
overCached _ v = pure v

-- | 'MonadFileCache' instance for images on top of the 'RWST' monad run by
-- 'runFileCacheT'
instance (MonadError e m, acid ~ AcidState (CacheMap ImageKey ImageFile), top ~ FileCacheTop)
  => MonadFileCache ImageKey ImageFile (RWST (acid, top) W s m) where
    askCacheAcid = view _1 :: RWST (acid, top) W s m (AcidState (CacheMap ImageKey ImageFile))
    buildCacheValue = buildImageFile

-- mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
-- evalFileCacheT :: Functor m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m a
-- runExceptT :: ExceptT e m a -> m (Either e a)

-- Crazy function to turn FileError into e.
fromImageCacheT ::
  forall e m a n. (HasFileError e, MonadIO m, MonadError e m, MonadImageCache m, n ~ ImageCacheT () IO)
  => n a
  -> m a
fromImageCacheT action = do
  acid <- askCacheAcid @ImageKey @ImageFile
  top <- fileCacheTop
  flattenExcept (evalFileCacheT acid top () action)
  where
    flattenExcept :: ExceptT FileError IO a -> m a
    flattenExcept = liftFileError . runExceptT

    liftFileError :: IO (Either FileError a) -> m a
    liftFileError action' = liftIO action' >>= either (throwError . fromFileError) return

-- This creates the file in the image cache but doesn't add it to the
-- database.  Why?  I'm not entirely sure.
class MakeImageFile a where
  makeImageFile :: (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m) => a -> m (Either FileError ImageFile)

instance MakeImageFile BS.ByteString where
  makeImageFile bs = do
    a <- liftIO $ getFileType bs
    file <- fileFromBytes (fileExtension a) bs
    makeImageFile (file, a)
instance MakeImageFile URI where
  makeImageFile uri = do
    fileFromURI (liftIO . getFileType) uri >>= makeImageFile
instance MakeImageFile FilePath where
  makeImageFile path =
    fileFromPath (liftIO . getFileType) path >>= makeImageFile
-- | Create an image file from a 'File'.  The existance of a 'File'
-- value implies that the image has been found in or added to the
-- acid-state cache.  Note that 'InProgress' is not a possible result
-- here, it will only occur for derived (scaled, cropped, etc.)
-- images.
instance MakeImageFile (File, ImageType) where
  makeImageFile (file, ityp) = do
    path <- fileCachePath file
    liftIO $ liftIO $
      (try ($logException ERROR (liftIO $ imageFileFromType path file ityp)))

-- | Return the local pathname of an image file.  The path will have a
-- suitable extension (e.g. .jpg) for the benefit of software that
-- depends on this, so the result might point to a symbolic link.
imageFilePath :: HasFileCacheTop m => ImageFile -> m FilePath
imageFilePath img = fileCachePath (view (field @"_imageFile") img)

-- | Find or create a version of some image with its orientation
-- corrected based on the EXIF orientation flag.  If the image is
-- already upright this will return the original ImageFile.
uprightImage ::
    (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m)
    => ImageFile
    -> m (Either FileError ImageFile)
uprightImage orig = do
  bs <- loadBytesSafe (view (field @"_imageFile") orig)
  bs' <- liftIO $ $logException ERROR $ normalizeOrientationCode (fromStrict bs)
  either
    (\_ -> return (Right orig))
    (\bs'' -> do
        let bs''' = toStrict bs''
        a <- liftIO ($logException ERROR (getFileType bs'''))
        file <- fileFromBytes (fileExtension a) (toStrict bs'')
        makeImageFile (file, a))
    bs'

-- | Find or create a version of some image with its orientation
-- corrected based on the EXIF orientation flag.  If the image is
-- already upright this will return Nothing.
uprightImage' :: MonadIO m => BS.ByteString -> ExceptT FileError m (Maybe BS.ByteString)
uprightImage' bs = do
  liftIO io >>= return . either (const Nothing) (Just . toStrict)
  where io = $logException ERROR $ normalizeOrientationCode (fromStrict bs)

-- | Find or create a cached image resized by decoding, applying
-- pnmscale, and then re-encoding.  The new image inherits attributes
-- of the old other than size.
scaleImage ::
  forall e m. (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m)
  => Double -> ImageFile -> m (Either FileError ImageFile)
scaleImage scale orig | approx (toRational scale) == 1 = return (Right orig)
scaleImage scale orig = {- liftIO $ $logException ERROR $ -} do
    path <- fileCachePath (view (field @"_imageFile") orig)
    let decoder = case view (field @"_imageFileType") orig of
                    JPEG -> showCommandForUser "jpegtopnm" [path]
                    PPM -> showCommandForUser "cat" [path]
                    GIF -> showCommandForUser "giftopnm" [path]
                    PNG -> showCommandForUser "pngtopnm" [path]
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
        -- To save space, build a jpeg here rather than the original file type.
        encoder = case view (field @"_imageFileType") orig of
                    JPEG -> showCommandForUser "cjpeg" []
                    PPM -> showCommandForUser {-"cat"-} "cjpeg" []
                    GIF -> showCommandForUser {-"ppmtogif"-} "cjpeg" []
                    PNG -> showCommandForUser {-"pnmtopng"-} "cjpeg" []
        cmd = pipe' [decoder, scaler, encoder]
    fileFromCmd (liftIO . getFileType) (shell cmd) >>= makeImageFile

-- | Find or create a cached image which is a cropped version of
-- another.
editImage ::
    forall e m. (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m)
    => ImageCrop -> ImageFile -> m (Either FileError ImageFile)
editImage crop ifile =
  logIOError $
    case commands of
      [] ->
          return (Right ifile)
      _ ->
          (do bs <- loadBytesSafe (view (field @"_imageFile") ifile)
              bs' <- liftIO $ pipeline commands bs
              a <- liftIO $ getFileType bs'
              file <- fileFromBytes (fileExtension a) bs'
              makeImageFile (file, a)) `catchError` err
    where
      commands = buildPipeline (view (field @"_imageFileType") ifile) [cut, rotate] (latexImageFileType (view (field @"_imageFileType") ifile))
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
      w = pixmapWidth ifile
      h = pixmapHeight ifile
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
      err :: e -> m (Either FileError ImageFile)
      err e = withFileError err' e
        where err' :: Maybe FileError -> m (Either FileError ImageFile)
              err' (Just e') = return $ Left $ e'
              -- err' (Just e') = return $ Failed $ ErrorCall $ "editImage Failure: file=" <> pack (show file) <> ", error=" <> pack (show e)
              err' Nothing = throwError e

-- | Build an original (not derived) ImageFile from a URI or a
-- ByteString, insert it into the cache, and return it.
cacheImageOriginal ::
    forall f e m. (MakeImageFile f, MonadIO m, MonadError e m, HasFileError e, MonadImageCache m)
    => f
    -> m (ImageKey, Either FileError ImageFile)
cacheImageOriginal src = do
  (img' :: (Either FileError ImageFile)) <- makeImageFile src
  case img' of
    Left e -> throwError (fromFileError e)
    Right img -> do
      let key = originalKey img
      (key,) <$> cachePut key (Right img)

-- | Scan for ReportImage objects and ensure that one version of that
-- image has been added to the cache, adding it if necessary.
cacheImagesByKey ::
  forall a e m. (Ord a, MonadImageCache m, MonadIO m, MonadError e m, HasFileError e)
  => (a -> ImageKey)
  -> [a]
  -> m (Map a (Either FileError ImageFile))
cacheImagesByKey keyfn keys =
  fromList <$> mapM (\img -> let key = keyfn img in cacheInsert key >>= \val -> return (img, val)) keys

-- | Add some image files to an image repository - updates the acid
-- state image map and copies the file to a location determined by the
-- FileCacheTop and its checksum.
storeImageFiles ::
    HasFileError e
    => FileCacheTop
    -> AcidState (CacheMap ImageKey ImageFile)
    -> [FilePath]
    -> ExceptT e IO [(ImageKey, Either FileError ImageFile)]
storeImageFiles images acid files =
  evalFileCacheT acid images () (mapM storeImageFile files)

-- storeFile :: (MonadIO m, MonadImageCache m, MonadCatch m, HasFileCacheTop m) => FilePath -> m ReportImage
-- FileCacheT is an instance of MonadFileCache

instance HasFileExtension a => HasFileExtension (b, a) where
  fileExtension (_, a) = fileExtension a

storeImageFile ::
    forall e m. (MonadImageCache m, HasFileError e, MonadIO m, MonadError e m)
    => FilePath -> m (ImageKey, Either FileError ImageFile)
storeImageFile fp = do
  (_file, (bytes, _typ)) :: (File, (BS.ByteString, ImageType)) <-
    fileFromPath (\bytes -> liftIO (getFileType bytes) >>= \typ -> return (bytes, typ)) fp
  cacheImageOriginal bytes

-- This should be in FileCacheT, not IO
storeByteStrings ::
    HasFileError e
    => FileCacheTop
    -> AcidState (CacheMap ImageKey ImageFile)
    -> [(ImageFile, BS.ByteString)]
    -> ExceptT e IO [Either FileError ImageFile]
storeByteStrings top acid pairs = do
  evalFileCacheT acid top () (mapM storeByteString (fmap snd pairs))

storeByteString ::
    (HasFileError e, MonadIO m, MonadError e m, MonadImageCache m)
    => BS.ByteString -> m (Either FileError ImageFile)
storeByteString bytes = do
  typ <- liftIO (getFileType bytes)
  _file <- fileFromBytes (fileExtension typ) bytes
  (_key, img) <- cacheImageOriginal bytes
  return img
