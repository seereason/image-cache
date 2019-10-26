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
  ) where

import "regex-compat-tdfa" Text.Regex ( Regex, mkRegex, matchRegex )
import Control.Exception ( throw )
import Control.Lens ( (%=), at, view, Field1(_1), Field3(_3), Field2(_2), _Left, over, makeLensesFor, set )
import Control.Monad ( unless, when )
import Control.Monad.Catch ( bracket, MonadMask, catchJust, try )
import Control.Monad.Except ( MonadError, catchError, throwError )
import Control.Monad.RWS ( RWST(runRWST) )
import Control.Monad.Reader ( MonadReader(ask), ReaderT )
import Control.Monad.Trans ( MonadTrans(lift), liftIO )
import Control.Monad.Trans.Except ( ExceptT(ExceptT), runExceptT )
import Data.Acid ( AcidState, makeAcidic, openLocalStateFrom, Query, Update, query, update )
import Data.Acid.Local ( createCheckpointAndClose )
import Data.Binary.Get ( getLazyByteString, Get, skip, bytesRead, getWord16be, getWord32be, getWord16le, getWord32le, runGetOrFail )
import qualified Data.ByteString as BS ( ByteString, empty, readFile )
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Lazy as LBS ( ByteString, unpack, pack, take, drop, concat )
import qualified Data.ByteString.UTF8 as P ( toString )
import Data.Char ( isSpace )
import Data.FileCache.Common ( CacheMap(..), {-CacheValue,-} File(..), FileCacheTop(..), HasFileCacheTop(..), Extension, CommandInfo(..), FileError(..), HasFileError, fromFileError, md5', FileSource(ThePath, TheURI), filePath, fileDir, FileError(CacheDamage), OriginalKey(originalKey), {-CacheImage,-} ImageKey(..), PixmapShape(pixmapHeight, pixmapWidth), ImageCrop(rotation, bottomCrop, topCrop, rightCrop, leftCrop), scaleFromDPI, HasFileCacheTop(fileCacheTop), SaneSize(SaneSize), Units(..), Dimension(..), ImageSize(..), readRationalMaybe, saneSize, withFileError, ImageType(..), ImageFile(..), ImageCrop(ImageCrop), approx, fileExtension )
import Data.FileCache.LogException (logException)
import Data.Generics ( Typeable )
import Data.Generics.Product ( field )
import Data.List ( intercalate )
import Data.Map.Strict as Map ( delete, difference, fromList, Map, fromSet, insert, intersection, union )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.SafeCopy ( SafeCopy, SafeCopy(version), SafeCopy' )
import Data.Set as Set ( Set )
import Data.Text as T ( pack, take, Text, unpack )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Word ( Word16, Word32 )
import Extra.Except ( MonadIO, logIOError )
import Extra.Log ( alog )
import GHC.Generics ( Generic )
import GHC.IO.Exception ( IOException(ioe_description) )
import GHC.Int ( Int64 )
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift ( Lift )
import Network.URI ( URI(..), uriToString )
import Numeric ( fromRat, showFFloat )
import System.Directory ( copyFile, createDirectoryIfMissing, doesFileExist, getDirectoryContents, renameFile )
import System.Exit ( ExitCode(..) )
import System.FilePath ( makeRelative, (</>) )
import System.FilePath.Extra ( writeFileReadable, makeReadableAndClose )
import System.IO ( openBinaryTempFile )
import System.IO.Error ( isDoesNotExistError )
import System.Log.Logger ( Priority(ERROR), logM, Priority(DEBUG, CRITICAL) )
import qualified System.Posix.Files as F ( createSymbolicLink, getSymbolicLinkStatus )
import qualified System.Process.ListLike as LL ( readProcessWithExitCode, showCreateProcessForUser )
import System.Process ( CreateProcess(..), CmdSpec(..), proc, showCommandForUser, shell )
import System.Process.ByteString.Lazy as LBS ( readCreateProcessWithExitCode )
import System.Process.ListLike as LL ( ListLikeProcessIO, ProcessResult, readCreateProcessWithExitCode, readCreateProcess )
import Test.HUnit ( assertEqual, Test(..) )
import Text.Parsec ( Parsec, (<|>), many, parse, char, digit, newline, noneOf, oneOf, satisfy, space, spaces, string, many1, optionMaybe )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

type ErrorWithIO m = ExceptT IOError m

modify :: Monad m => (IOError -> IOError) -> ErrorWithIO m a -> ErrorWithIO m a
modify f action = ExceptT (runExceptT action >>= return . either (Left . f) Right)

-- | Add a prefix to an IOError's description.
prefix :: Monad m => String -> ErrorWithIO m a -> ErrorWithIO m a
prefix s action = modify (mapIOErrorDescription (s ++)) action

mapIOErrorDescription :: (String -> String) -> IOError -> IOError
mapIOErrorDescription f e = e {ioe_description = f (ioe_description e)}

ensureLink :: String -> FilePath -> IO ()
ensureLink file path = (F.getSymbolicLinkStatus path >> return ()) `catchDoesNotExist` (\ () -> F.createSymbolicLink file path)

catchDoesNotExist :: IO a -> (() -> IO a) -> IO a
catchDoesNotExist = catchJust (\ e -> if isDoesNotExistError e then Just () else Nothing)

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    -- logM "Appraisal.Utils.ErrorWithIO" DEBUG ("readCreateProcessWithExitCode': " <> show (pPrint p)) >>
    $logException ERROR (LL.readCreateProcessWithExitCode p s)

readCreateProcess' :: (ListLikeProcessIO a c, ProcessResult a b) => CreateProcess -> a -> IO b
readCreateProcess' p s =
    -- logM "Appraisal.Utils.ErrorWithIO" DEBUG ("readCreateProcess': " <> show (pPrint p)) >>
    $logException ERROR (LL.readCreateProcess p s)

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)

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

initCacheMap :: Ord key => CacheMap key val
initCacheMap = CacheMap mempty

openCache :: (SafeCopy key, Typeable key, Ord key,
              SafeCopy val, Typeable val) => FilePath -> IO (AcidState (CacheMap key val))
openCache path = openLocalStateFrom path initCacheMap

-- | In theory the MonadError type e1 might differ from the error type
-- stored in the map e2.  But I'm not sure if it would work in practice.
withCache :: (MonadIO m, MonadMask m,
              SafeCopy val, Typeable val,
              Ord key, Typeable key, SafeCopy key) => FilePath -> (AcidState (CacheMap key val) -> m b) -> m b
withCache path f = bracket (liftIO (openCache path)) (liftIO . createCheckpointAndClose) $ f

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues])

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

{-
-- Test

dir = "/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images"

main = do
  names <- getDirectoryContents dir >>= return . filter (not . (`elem` [".", ".."])) . filter (not . isSuffixOf ".jpg")
  mapM_ (uncurry doName) (zip [1..] names)

doName :: Int -> FilePath -> IO ()
doName count path = Data.ByteString.Lazy.readFile (dir </> path) >>= handle (\ (e :: SomeException) -> return ()) . doCode count (dir </> path)

doCode :: Int -> FilePath -> ByteString -> IO ()
doCode count path bs =
    case getEXIFOrientationCode bs of
      (1, _, _) -> return ()
      result -> putStrLn (show count ++ ". " ++ show path ++ ": " ++ show result)

Images with valid orientation codes other than 1:

"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/6aa7ea4bd79f39c3092b6e4251b3b073": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/9624fd5634a5b6257725b42fbad1cbde": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b7e8f8e5cf18926e839b3cf6083d4932": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/62368114d7b71b5c6b6899f92651b12e": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/198ee040f479b85490b92fb2a2804ecf": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/72cc1a950694cc2409c3a8f4bc71301f": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/2a504ec035e8868c2b95e9a7d9c3c69e": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/304837ab54c63738fecf556f0ed0ba2a": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b41611aa15441535aead51769f801620": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/1a4b46c6272d45223cea4b2fd85882ff": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/12b4e042540ca1e1c8499bce228652e3": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0197efa1312688937c7dfe80c1cd5a08": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/34711b918bea81cf6d7f4481b72c017b": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/32004c379d003ca780a6345fa79050c8": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/d0276f1a87d4617bd21664a894daab5a": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b46817cecb5348ef82643b7654e01326": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/f8ed3f915cb9424f20b371f1667b2ecb": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3a391f9f15e49c2d6f506556e08b45dd": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/e855b29fee68fcea34152c8ab6f05df7": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/fe08b904b41a159a469c519e9666325c": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0d27c6db4f72952e134212b5eb56947d": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b31797cf58d461ddf57af90a54bc9bda": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0d423f15b9d129a923c63aac41b652b8": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/7c324add279ed38272ac1a03c490fa9b": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3077253364f0a682d9c654bf0e087246": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/ae344bc17e1c6b92bf783ea7d3adeda3": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/6da21a666fbfd29e16aca6602fdc8478": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b66b7a09b5ea7adcd0b9d4a4fbcb90ae": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3cd5da3d4568095e6c15bd992a676d1c": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/22d628676e6b561f2aaba590dacf63e7": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/9172d8098bdf0c4ff9360801339d632d": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/144177d35e36e0a93e269af46ad01346": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/96a961a0f288e7e05b243889a1d06c14": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/4aa04ca61174a6d5bdfc30278ad04e9a": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/918a82dff691604601f21c4b4932c5bc": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3337b1f7388639d2fc53f74a7bf834fa": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/e39a014def56a031b78c2e5e96d1f7c5": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0bf267e6a880422d4479ef256d0836e9": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/135bf2de2dd4c2fa94ae05f117867b5e": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/5c100432de39d6f4fe911b69523cb22c": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0aeecd29b06ed4738a72f554288ffc47": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/20a29b7d21ef647cd2e4841cf67656fa": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/c0419800d504d32c51f5c5ed6bb362cb": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b619922e4856ba6e0f443ffb49d488f6": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/01e6856e1091b02642840a59386236e3": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/1a7f39310a5cfc3c38ab4f58c2d1a688": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/e04b93bf3d3183f8e6073857626fdda6": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/45842965ab1e4032625ca14309ee35b4": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/254f665f816cb484e4feb0d8197fe642": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/ae51018932436f9494ce087e2ec628f5": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/7231ae84a7c45da7f022da2edb22470c": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/4485d40fc73aeb5b3b4a59e56a03cad6": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/d8ab2be3f85b2033ab5ce6e798ee2a6a": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/a41ab6f5b45888755eeae2edcdecb626": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/dd0d1578040c42da93633b4ed7efd1ff": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/1fba9c9a78ba2e8284c2b7cc804feaa8": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3aeb2f1b415291ab5a90349dc21e2620": (8,54,False)
-}

(<++>) :: FilePath -> FilePath -> FilePath
a <++> b = a </> (makeRelative "" b)

-- | Build a URI for the locally cached version of the file given the
-- uri of the cache home directory.
fileCacheURI :: URI -> File -> URI
fileCacheURI cacheDirectoryURI file =
    cacheDirectoryURI {uriPath = uriPath cacheDirectoryURI <++> T.unpack (_fileChksum file)}

-- | Turn the bytes in a ByteString into a File.  This is an IO
-- operation because it saves the data into the local cache.  We
-- use writeFileReadable because the files we create need to be
-- read remotely by our backup program.
fileFromBytes ::
    forall m a. (MonadIO m, HasFileCacheTop m)
    => (BS.ByteString -> m a)
    -> (a -> Extension)
    -> BS.ByteString
    -> m (File, a)
fileFromBytes byteStringInfo toFileExt bytes =
      do a <- byteStringInfo bytes
         let file = File { _fileSource = Nothing
                         , _fileChksum = T.pack (md5' bytes)
                         , _fileMessages = []
                         , _fileExt = toFileExt a }
         path <- fileCachePathIO file
         exists <- liftIO $ doesFileExist path
         unless exists (liftIO (writeFileReadable path bytes))
         return (file, a)

-- |Read the contents of a local path into a File.
fileFromPath ::
    forall m a. (MonadIO m, HasFileCacheTop m)
    => (BS.ByteString -> m a)
    -> (a -> Extension)
    -> FilePath
    -> m (File, a)
fileFromPath byteStringInfo toFileExt path = do
  bytes <- liftIO $ BS.readFile path
  (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
  return (set (field @"_fileSource") (Just (ThePath path)) file, a)

-- | A shell command whose output becomes the contents of the file.
fileFromCmd ::
    forall e m a. (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m)
    => (BS.ByteString -> m a)
    -> (a -> Extension)
    -> String
    -> m (File, a)
fileFromCmd byteStringInfo toFileExt cmd = do
  (code, bytes, _err) <- liftIO (readCreateProcessWithExitCode' (shell cmd) BS.empty)
  case code of
    ExitSuccess ->
        do (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
           return $ (set (field @"_fileSource") (Just (ThePath cmd)) file, a)
    ExitFailure _ ->
        throwError $ (fromFileError :: FileError -> e) $ CommandFailure (FunctionName "fileFromCmd" (Command (T.pack (show (shell cmd))) (T.pack (show code))))

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI ::
    forall e m a. (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m)
    => (BS.ByteString -> m a)
    -> (a -> Extension)
    -> String
    -> m (File, a)
fileFromURI byteStringInfo toFileExt uri =
    do let args = ["-s", uri]
           cmd = (proc "curl" args)
       (code, bytes, _err) <- liftIO $ readCreateProcessWithExitCode' cmd BS.empty
       case code of
         ExitSuccess ->
             do (file, bytes') <- fileFromBytes byteStringInfo toFileExt bytes
                return (set (field @"_fileSource") (Just (TheURI uri)) file, bytes')
         _ -> throwError $ fromFileError $ CommandFailure (FunctionName "fileFromURI" (Command (T.pack (show cmd)) (T.pack (show code))))

-- | Build a file from the output of a command.  This uses a temporary
-- file to store the contents of the command while we checksum it.  This
-- is to avoid reading the file contents into a Haskell ByteString, which
-- may be slower than using a unix pipeline.  Though it shouldn't be.
fileFromCmdViaTemp ::
    forall e m. (MonadIO m, MonadError e m, HasFileCacheTop m, HasFileError e)
    => Text
    -> String
    -> m File
fileFromCmdViaTemp ext exe = do
  FileCacheTop dir <- fileCacheTop
  (tmp, h) <- liftIO $ openBinaryTempFile dir "scaled"
  let cmd = shell (exe ++ " > " ++ tmp)
  liftIO $ makeReadableAndClose h
  -- io (hClose h)
  (code, _out, _err) <- liftIO (readCreateProcessWithExitCode' cmd BS.empty)
  case code of
    ExitSuccess -> installFile tmp
    ExitFailure _ -> throwError $ fromFileError $ CommandFailure $ FunctionName "fileFromCmdViaTemp" $ Command (T.pack (show cmd)) (T.pack (show code))
    where
      installFile :: FilePath -> m File
      installFile tmp = fileFromPathViaRename (fromFileError . CommandFailure . FunctionName "fileFromCmdViaTemp" . Description "install failed") ext tmp

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaRename ::
    forall e m. (HasFileError e, MonadIO m, MonadError e m, HasFileCacheTop m)
    => (CommandInfo -> FileError) -- ^ Use this to customize exception thrown here
    -> Extension
    -> FilePath
    -> m File
fileFromPathViaRename err ext path = do
  let cmd = shell ("md5sum < " ++ showCommandForUser path [])
  result <- liftIO (LL.readCreateProcessWithExitCode cmd "")
  case result of
    (ExitSuccess, out, _err) -> do
      let file = File { _fileSource = Just (ThePath path)
                      , _fileChksum = T.take 32 out
                      , _fileMessages = []
                      , _fileExt = ext }
      dest <- fileCachePathIO file
      liftIO $ do
        logM "Appraisal.FileCache" DEBUG ("fileFromPathViaRename - renameFile " <> path <> " " <> dest)
        renameFile path dest
      return file
    (code, _, _) -> throwError $ fromFileError $ err (Command (T.pack (show cmd)) (T.pack (show code)))

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaCopy ::
    forall m. (MonadIO m, HasFileCacheTop m)
    => Extension
    -> FilePath
    -> m File
fileFromPathViaCopy ext path = do
  cksum <- (\(_, out, _) -> T.take 32 out) <$> liftIO (LL.readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
  let file = File { _fileSource = Just (ThePath path)
                  , _fileChksum = cksum
                  , _fileMessages = []
                  , _fileExt = ext }
  dest <- fileCachePathIO file
  liftIO $ logM "Appraisal.FileCache" DEBUG ("fileFromPathViaCopy - copyFile " <> path <> " " <> dest)
  liftIO $ copyFile path dest
  return file

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

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."

-- | Scan all the file cache directories for files without using
-- the database.
allFiles :: MonadIO m => ReaderT (st, FileCacheTop) m [FilePath]
allFiles = do
  FileCacheTop top <- fileCacheTop
  dirs <- liftIO $ listDirectory top
  Prelude.concat <$> mapM (\dir -> let dir' = top </> dir in
                           fmap (dir' </>) <$> liftIO (listDirectory dir')) dirs

-- | The full path name for the local cache of the file.
fileCachePath :: HasFileCacheTop m => File -> m FilePath
fileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> filePath file

oldFileCachePath :: HasFileCacheTop m => File -> m FilePath
oldFileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> T.unpack (_fileChksum file)

fileCacheDir :: HasFileCacheTop m => File -> m FilePath
fileCacheDir file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> fileDir file

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]

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

deriving instance Lift ImageFile
deriving instance Lift ImageType
deriving instance Lift ImageKey
deriving instance Lift ImageSize
deriving instance Lift Units
deriving instance Lift ImageCrop
deriving instance Lift Dimension
deriving instance Lift a => Lift (SaneSize a)

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

showCmdSpec :: CmdSpec -> String
showCmdSpec (ShellCommand s) = s
showCmdSpec (RawCommand p ss) = showCommandForUser p ss

pipe :: [CreateProcess] -> CreateProcess
pipe xs = foldl1 pipe2 xs

pipe2 :: CreateProcess -> CreateProcess -> CreateProcess
pipe2 a b =
    if cwd a == cwd b &&
       env a == env b &&
       close_fds a == close_fds b &&
       create_group a == create_group b
    then a {cmdspec = ShellCommand (showCmdSpec (cmdspec a) ++ " | " ++ showCmdSpec (cmdspec b))}
    else error $ "Pipeline of incompatible commands: " ++ LL.showCreateProcessForUser a ++ " | " ++ LL.showCreateProcessForUser b

$(makeLensesFor [("imageFile", "imageFileL")] ''ImageFile)

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
cacheMiss key = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val))
  val <- buildCacheValue key
  liftIO $ update st (PutValue key val)

cachePut ::
  forall key val m. (MonadFileCache key val m, MonadIO m{-, MonadError e m, HasFileError e, Show val-})
  => key -> val -> m (Either FileError val)
cachePut key val = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val))
  liftIO $ update st (PutValue key (Right val))

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

type ImageCacheT s m = FileCacheT ImageKey ImageFile s m
type MonadImageCache m = MonadFileCache ImageKey ImageFile m

#if 0
runImageCacheT ::
  (HasFileError e, MonadError e m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W s m a
  -> m (a, s, W)
runImageCacheT = runFileCacheT

evalImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m a
evalImageCacheT = evalFileCacheT
execImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m s
execImageCacheT = execFileCacheT
writeImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m W
writeImageCacheT = writeFileCacheT

runImageCacheIOT ::
  (HasFileError e, MonadIO m, MonadError e m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W S m a
  -> m (a, S, W)
runImageCacheIOT = runFileCacheIOT

evalImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m a
evalImageCacheT = evalFileCacheT
execImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m S
execImageCacheT = execFileCacheT
writeImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m W
writeImageCacheT = writeFileCacheT
#endif

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

overCached :: Monad m => (a -> m (Either FileError a)) -> Either FileError a -> m (Either FileError a)
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
fromImageCacheT' ::
  forall e m a n. (HasFileError e, MonadIO m, MonadImageCache m, n ~ ImageCacheT () IO)
  => n (Either FileError a)
  -> m (Either e a)
fromImageCacheT' action1 = do
  acid <- askCacheAcid @ImageKey @ImageFile
  top <- fileCacheTop
  flattenExcept1 (evalFileCacheT acid top () action1)
  where
    flattenExcept1 :: ExceptT FileError IO (Either FileError a) -> m (Either e a)
    flattenExcept1 action = liftFileError (flattenEither <$> (runExceptT action))

    liftFileError :: IO (Either FileError a) -> m (Either e a)
    liftFileError action = over _Left fromFileError <$> liftIO action

    flattenEither = either Left id

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
  makeImageFile bs =
    fileFromBytes (liftIO . getFileType) fileExtension bs >>= makeImageFile
instance MakeImageFile URI where
  makeImageFile uri =
    fileFromURI (liftIO . getFileType) fileExtension (uriToString id uri "") >>= makeImageFile
instance MakeImageFile FilePath where
  makeImageFile path =
    fileFromPath (liftIO . getFileType) fileExtension path >>= makeImageFile
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
    (\bs'' -> fileFromBytes (liftIO . $logException ERROR . getFileType) fileExtension (toStrict bs'') >>= makeImageFile)
    bs'

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
    fileFromCmd (liftIO . getFileType) fileExtension cmd >>= makeImageFile

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

-- | Find or create a cached image which is a cropped version of
-- another.
editImage ::
    forall e m. (MonadIO m, MonadError e m, HasFileError e, HasFileCacheTop m)
    => ImageCrop -> ImageFile -> m (Either FileError ImageFile)
editImage crop file =
  logIOError $
    case commands of
      [] ->
          return (Right file)
      _ ->
          (loadBytesSafe (view (field @"_imageFile") file) >>=
           liftIO . pipeline commands >>=
           fileFromBytes (liftIO . getFileType) fileExtension >>=
           makeImageFile) `catchError` err
    where
      commands = buildPipeline (view (field @"_imageFileType") file) [cut, rotate] (latexImageFileType (view (field @"_imageFileType") file))
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
      w = pixmapWidth file
      h = pixmapHeight file
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
      (key,) <$> cachePut key img

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

storeImageFile ::
    forall e m. (MonadImageCache m, HasFileError e, MonadIO m, MonadError e m)
    => FilePath -> m (ImageKey, Either FileError ImageFile)
storeImageFile fp = do
  (_file, (bytes, _typ)) :: (File, (BS.ByteString, ImageType)) <-
    fileFromPath (\bytes -> liftIO (getFileType bytes) >>= \typ -> return (bytes, typ))
                 (fileExtension . snd) fp
  cacheImageOriginal bytes

-- This should be in FileCacheT, not IO
storeByteStrings ::
    HasFileError e
    => FileCacheTop
    -> AcidState (CacheMap ImageKey ImageFile)
    -> [(ImageFile, BS.ByteString)]
    -> ExceptT e IO [Either FileError ImageFile]
storeByteStrings top acid pairs = do
  evalFileCacheT acid top () (mapM (uncurry storeByteString) pairs)
  -- ~(Right a) <- runExceptT (evalFileCacheT acid top (mapM (uncurry storeByteString) pairs))
  -- return $ fmap (either Failed Value) a

storeByteString ::
    (HasFileError e, MonadIO m, MonadError e m, MonadImageCache m)
    => p -> BS.ByteString -> m (Either FileError ImageFile)
storeByteString _ifile bytes = do
  (_file, (_bytes', _typ)) :: (File, (BS.ByteString, ImageType)) <-
     fileFromBytes (\bytes'' -> liftIO (getFileType bytes'') >>= \typ -> return (bytes'', typ))
                   (fileExtension . snd) bytes
  (_key, img) <- cacheImageOriginal bytes
  return img
