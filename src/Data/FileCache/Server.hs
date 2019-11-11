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
    -- * Image IO
  , validateJPG
    -- * File Cache
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
  , cacheDerivedImages
  -- , cacheDerivedImage

  , ImageChan
  , HasImageBuilder(imageBuilder)
  , startImageBuilder
  -- , queueImageBuild

  , fixImageShapes
  , validateImageKey
  , validateImageFile
  , tests
  ) where

import Control.Concurrent (ThreadId{-, threadDelay-})
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (Exception)
import Control.Lens ( (%=), _1, _2, _3, at, view )
import Control.Monad (forever, unless, when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.RWS ( get, modify, MonadIO(liftIO), MonadState, put, RWST(runRWST) )
import Control.Monad.Reader ( MonadReader(ask), ReaderT, runReaderT )
import Control.Monad.State (execStateT)
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
import Data.Typeable -- ( (:~:), eqT, Typeable )
import Data.Generics (mkT, everywhere)
import Data.Generics.Product ( field )
import Data.List ( intercalate )
import Data.Map.Strict as Map ( delete, difference, fromList, Map, fromSet, insert, intersection, lookup, toList, union )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.SafeCopy ( base, deriveSafeCopy, extension, Migrate(..), SafeCopy(..), SafeCopy' )
import Data.Set as Set (insert, Set)
import Data.Text as T ( pack, Text )
import Data.Text.Encoding ( decodeUtf8 )
import Data.Word ( Word16, Word32 )
import Extra.Except ( HasSomeNonPseudoException(..), lyftIO, MonadError, throwError, tryError)
import Extra.Log ( alog )
import GHC.Generics (Generic)
import GHC.Int ( Int64 )
import Language.Haskell.TH.Instances ()
import Network.URI ( URI(..), uriToString )
import Numeric ( fromRat, showFFloat )
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), makeRelative, takeDirectory )
import System.FilePath.Extra ( writeFileReadable )
import System.IO (hFlush, stdout)
import System.Log.Logger ( Priority(..) )
import qualified System.Process.ListLike as LL ( showCreateProcessForUser )
import System.Process ( CreateProcess(..), CmdSpec(..), proc, showCommandForUser, shell )
import System.Process.ByteString.Lazy as LBS ( readCreateProcessWithExitCode )
import System.Process.ListLike as LL ( ListLikeProcessIO, readCreateProcessWithExitCode, readCreateProcess )
import Test.HUnit ( assertEqual, Test(..) )
import Text.Parsec ( Parsec, (<|>), many, parse, char, digit, newline, noneOf, oneOf, satisfy, space, spaces, string, many1, optionMaybe )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import UnexceptionalIO.Trans (Unexceptional)
import UnexceptionalIO.Trans as UIO hiding (lift, ErrorCall)

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
  forall m. Unexceptional m
  => [CreateProcess]
  -> BS.ByteString
  -> ExceptT FileError m BS.ByteString
pipeline [] bytes = return bytes
pipeline (p : ps) bytes =
  tryError (lyftIO (LL.readCreateProcessWithExitCode p bytes)) >>= doResult
  where
    doResult :: Either FileError (ExitCode, BS.ByteString, BS.ByteString) -> ExceptT FileError m BS.ByteString
    doResult (Left e) = unsafeFromIO (alog "Appraisal.ImageFile" ERROR (LL.showCreateProcessForUser p ++ " -> " ++ show e)) >> throwError (IOException (pack (show e)))
    doResult (Right (ExitSuccess, out, _)) = pipeline ps out
    doResult (Right (code, _, err)) =
      let message = (LL.showCreateProcessForUser p ++ " -> " ++ show code ++ " (" ++ show err ++ ")") in
        unsafeFromIO (alog "Appraisal.ImageFile" ERROR message) >>
        -- Not actually an IOExeption, this is a process error exit
        (throwError (IOException (pack (show (userError message)))))

-- * FileCacheTop

newtype FileCacheTop = FileCacheTop {_unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- directory of a file cache.
class HasFileCacheTop a where fileCacheTop :: a -> FileCacheTop
instance HasFileCacheTop FileCacheTop where fileCacheTop = id
-- instance HasFileCacheTop (ImageAcid, FileCacheTop) where fileCacheTop = snd
-- instance HasFileCacheTop (ImageAcid, FileCacheTop, c) where fileCacheTop = view _2

type CacheAcid key val = AcidState (CacheMap key val)
class HasCacheAcid key val a where cacheAcid :: a -> AcidState (CacheMap key val)
instance  HasCacheAcid key val (CacheAcid key val) where cacheAcid = id
instance  HasCacheAcid key val (CacheAcid key val, top) where cacheAcid = fst
instance  HasCacheAcid key val (CacheAcid key val, a, b) where cacheAcid = view _1

-- * CacheMap

-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
data CacheMap key val =
    CacheMap {_unCacheMap :: Map key (Either FileError val)}
    deriving (Generic, Eq, Ord)

instance (Ord key, SafeCopy' key, SafeCopy' val) => SafeCopy (CacheMap key val) where
  version = 3
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap"

instance (Ord key, SafeCopy' key, SafeCopy' val) => Migrate (CacheMap key val) where
  type MigrateFrom (CacheMap key val) = CacheMap_2 key val
  migrate (CacheMap_2 mp) =
    CacheMap (fmap (\case Value_1 a -> Right a; Failed_1 e -> Left e; _ -> error "Migrate CacheMap") mp)

data CacheMap_2 key val =
    CacheMap_2 {_unCacheMap_2 :: Map key (CacheValue_1 val)}
    deriving (Generic, Eq, Ord)

instance (Ord key, SafeCopy' key, SafeCopy' val) => SafeCopy (CacheMap_2 key val) where
  version = 2
  kind = extension
  errorTypeName _ = "Data.FileCache.Types.CacheMap_2"

deriving instance (Show key, Show val) => Show (CacheMap key val)

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap_2 key val) where
    type MigrateFrom (CacheMap_2 key val) = Map key val
    migrate mp = CacheMap_2 (fmap Value_1 mp)

data CacheValue_1 val
    = InProgress_1
    | Value_1 val
    | Failed_1 FileError
    deriving (Generic, Eq, Ord, Functor)

deriving instance Show val => Show (CacheValue_1 val)

$(deriveSafeCopy 1 'base ''CacheValue_1)

-- * Events

-- | Install a key/value pair into the cache.
putValue ::
  Ord key
  => key
  -> Either FileError val
  -> Update (CacheMap key val) ()
putValue key img = do
  field @"_unCacheMap" %= Map.insert key img

-- | Install several key/value pairs into the cache.
putValues :: Ord key => Map key (Either FileError val) -> Update (CacheMap key val) ()
putValues pairs = do
  field @"_unCacheMap" %= Map.union pairs

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

openCache :: (key ~ ImageKey, SafeCopy key, Typeable key,
              val ~ ImageFile, SafeCopy val, Typeable val) => FilePath -> IO (AcidState (CacheMap key val))
openCache path = openLocalStateFrom path initCacheMap

initCacheMap :: Ord key => CacheMap key val
initCacheMap = CacheMap mempty

-- * ByteString

class MakeByteString a where
  makeByteString :: (Unexceptional m, MonadCatch m) => a -> ExceptT FileError m BS.ByteString

instance MakeByteString BS.ByteString where
  makeByteString = return

instance MakeByteString FilePath where
  makeByteString path = lyftIO (BS.readFile path)

instance MakeByteString CreateProcess where
  makeByteString cmd = makeByteString (cmd, BS.empty)

instance MakeByteString (CreateProcess, BS.ByteString) where
  makeByteString (cmd, input) = do
    (code, bytes, _err) <- lyftIO (readCreateProcessWithExitCode' cmd input)
    case code of
      ExitSuccess -> return bytes
      ExitFailure _ ->
        throwError $ CommandFailure (FunctionName "MakeByteString CreateProcess" (Command (T.pack (show cmd)) (T.pack (show code))))

instance MakeByteString URI where
  makeByteString uri = do
    let cmd = proc "curl" ["-s", uriToString id uri ""]
    (code, bytes, _err) <-
      lyftIO $ readCreateProcessWithExitCode' cmd BS.empty
    case code of
      ExitSuccess -> return bytes
      _ -> throwError $ CommandFailure (FunctionName "MakeByteString URI" (Command (T.pack (show cmd)) (T.pack (show code))))

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
    -- runExceptT (runExceptT (lyftIO (normalizeOrientationCode (fromStrict bs))) >>= liftEither)

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
    Right x -> throwError $ ErrorCall ("Unexpected exif orientation code: " <> pack (show x))
    Left x -> throwError $  ErrorCall ("Failure parsing exif orientation code: " <> pack (show x))
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
        (result, out, err) <- lyftIO (LBS.readCreateProcessWithExitCode (proc cmd args') bs')
        case result of
          ExitSuccess -> return out
          ExitFailure _ -> throwError $ CommandFailure $ CommandErr (toStrict err) $ Command (pack (show cp)) (pack (show result))

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
  (Unexceptional m, MonadCatch m)
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
                    Unknown -> error "scaleImage' unknown file type"
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
        -- To save space, build a jpeg here rather than the original file type.
        encoder = case typ of
                    JPEG -> showCommandForUser "cjpeg" []
                    PPM -> showCommandForUser {-"cat"-} "cjpeg" []
                    GIF -> showCommandForUser {-"ppmtogif"-} "cjpeg" []
                    PNG -> showCommandForUser {-"pnmtopng"-} "cjpeg" []
                    Unknown -> error "scaleImage' unknown file type"
        cmd = intercalate " | " [decoder, scaler, encoder]
    Just <$> makeByteString (shell cmd, bytes)

logIOError' :: (Unexceptional m, MonadError e m) => m a -> m a
logIOError' io =
  tryError io >>= either (\e -> unsafeFromIO ($logException ERROR (pure e)) >> throwError e) return
-- logIOError' = handleError (\e -> liftIO ($logException ERROR (pure e)) >> throwError e)

editImage' ::
    forall m shape. (Unexceptional m, HasImageShape shape)
    => ImageCrop -> BS.ByteString -> ImageType -> shape -> ExceptT FileError m (Maybe BS.ByteString)
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
      latexImageFileType Unknown = JPEG -- whatever
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

-- | The full path name for the local cache of the file.
fileCachePath :: (HasURIPath a, MonadReader r m, HasFileCacheTop r) => a -> m FilePath
fileCachePath file = do
  (FileCacheTop top) <- fileCacheTop <$> ask
  let path = toURIPath file
  return $ top </> makeRelative "/" path

-- | Create any missing directories and evaluate 'fileCachePath'
fileCachePathIO :: (HasURIPath a, MonadReader r m, HasFileCacheTop r, Unexceptional m, MonadError FileError m) => a -> m FilePath
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

askCacheAcid :: (MonadReader r m, HasCacheAcid key val r) => m (CacheAcid key val)
askCacheAcid = cacheAcid <$> ask

-- FIXME - the result should be (Either FileError val), the FileError
-- is part of the cached value now, its not an exception.
cachePut ::
  forall key val r e m. (Unexceptional m,
                         MonadError e m, HasSomeNonPseudoException e,
                         MonadReader r m, HasCacheAcid key val r,
                         Ord key, Typeable key, Typeable val)
  => key -> Either FileError val -> m (Either FileError val)
cachePut key val = do
  st <- askCacheAcid
  lyftIO (update st (PutValue key val))
  return val

-- | Same as 'cachePut' but returns ().  Same FIXME applies.
cachePut' ::
  forall e key val r m. (Unexceptional m, MonadError e m, HasSomeNonPseudoException e,
                         MonadReader r m, HasCacheAcid key val r, Ord key, Typeable key, Typeable val)
  => key -> Either FileError val -> m ()
cachePut' key val = do
  st <- askCacheAcid
  lyftIO (update st (PutValue key val))

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  forall key val r m. (Unexceptional m, MonadReader r m, HasCacheAcid key val r, Ord key, Typeable key, Typeable val)
  => key -> m (Maybe (Either FileError val))
cacheLook key = do
  st <- askCacheAcid
  handleQueryException <$> runExceptT (lyftIO $ query st (LookValue key))
  where
    -- Should we distinguish between errors that occurred building the
    -- image file and error that occurred during the query?  The
    -- correct answer is usually yes, and soon I will get to that.
    -- But at the moment this code merges those errors into the result.
    handleQueryException :: Either FileError (Maybe (Either FileError val)) ->  Maybe (Either FileError val)
    handleQueryException (Left e) = Just (Left e)
    handleQueryException (Right (Just r)) = Just r
    handleQueryException (Right Nothing) = Nothing

cacheMap ::
  (Unexceptional m, MonadError e m, HasSomeNonPseudoException e, MonadReader r m, HasCacheAcid key val r, Typeable key, Typeable val)
  => m (CacheMap key val)
cacheMap = do
  st <- askCacheAcid
  lyftIO (query st LookMap)

cacheDelete ::
  forall key val e r m. (Unexceptional m, MonadError e m, HasSomeNonPseudoException e,
                         MonadReader r m, HasCacheAcid key val r, Ord key, Typeable key, Typeable val)
  => Proxy val -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val)) <- cacheAcid <$> ask
  lyftIO (update st (DeleteValues keys))

-- * ImageCache

type ImageCacheT r s m = FileCacheT r s m
type HasImageAcid r = HasCacheAcid ImageKey ImageFile r
type ImageAcid = CacheAcid ImageKey ImageFile
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

-- * Create original and derived images

-- | Add some image files to an image repository - updates the acid
-- state image map and copies the file to a location determined by the
-- FileCacheTop and its checksum.
cacheOriginalImages ::
  forall x e r m. (MakeByteString x, Ord x, Unexceptional m, MonadCatch m,
                   MonadError e m, HasSomeNonPseudoException e,
                   MonadReader r m, HasImageAcid r, HasFileCacheTop r,
                   MonadState (Map x (Either FileError (ImageKey, ImageFile))) m)
  => [x] -> m ()
cacheOriginalImages =
  mapM_ (\x -> runExceptT (cacheOriginalImage x) >>= modify  . Map.insert x)

-- | Build an original (not derived) ImageFile from a URI or a
-- ByteString, insert it into the cache, and return it.
cacheOriginalImage ::
  forall x e r m. (MakeByteString x, Unexceptional m, MonadCatch m,
                   MonadError e m, HasSomeNonPseudoException e,
                   MonadReader r m, HasFileCacheTop r, HasImageAcid r)
  => x
  -> ExceptT FileError m (ImageKey, ImageFile)
cacheOriginalImage x = do
  img <- buildOriginalImage x
  let key = originalKey img
      val = ImageFileReady img
  unsafeFromIO (alog "Data.FileCache.Server" DEBUG ("cachePut " ++ show key))
  lift (cachePut key (Right val))
  return (key, val)

buildOriginalImage ::
  forall x r m. (MakeByteString x, Unexceptional m, MonadCatch m,
                 MonadReader r m, HasFileCacheTop r)
  => x
  -> ExceptT FileError m ImageReady
buildOriginalImage x = do
  bs <- makeByteString x
  let csum = T.pack $ show $ md5 $ fromStrict bs
  shape@ImageShape {..} <- imageShapeM bs
  let file = File { _fileSource = Nothing
                  , _fileChksum = csum
                  , _fileMessages = []
                  , _fileExt = fileExtension (imageType shape) }
  let img = ImageReady { _imageFile = file, _imageShape = shape }
  path <- fileCachePathIO (ImageCached (ImageOriginal csum _imageShapeType) (ImageFileReady img))
  exists <- lyftIO $ doesFileExist path
  unless exists $ lyftIO $ writeFileReadable path bs
  return img

type ImageChan = Chan [(ImageKey, ImageShape)]
class HasImageBuilder a where imageBuilder :: a -> ImageChan
instance HasImageBuilder ImageChan where imageBuilder = id
instance HasImageBuilder (a, b, ImageChan) where imageBuilder = view _3

data Results
  = Results
    { _badKeys :: Set ImageKey
    , _shapeErrors :: Map ImageKey FileError
    , _cacheMisses :: Map ImageKey ImageShape
    , _cachedErrors :: Map ImageKey FileError
    , _cachedReady :: Map ImageKey ImageReady
    , _cachedShapes :: Map ImageKey ImageShape
    } deriving (Generic)

instance Semigroup Results where
  Results a1 a2 a3 a4 a5 a6 <> Results b1 b2 b3 b4 b5 b6 =
    Results (a1 <> b1) (a2 <> b2) (a3 <> b3) (a4 <> b4) (a5 <> b5) (a6 <> b6)

instance Monoid Results where
  mappend = (<>)
  mempty = Results mempty mempty mempty mempty mempty mempty

-- | Build an image map in the state monad
cacheDerivedImages ::
  forall r e m. (Unexceptional m, MonadError e m, HasSomeNonPseudoException e, Exception e,
                 MonadReader r m, HasCacheAcid ImageKey ImageFile r, HasImageBuilder r)
  => Bool
  -- ^ Should we send _cachedErrors to the builder again?
  -- This sounds dangerous for normal operation, but useful
  -- for appraisalscope.
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
cacheDerivedImages retry keys = do
  Results{..} <- execStateT (mapM_ cacheDerivedImage keys) mempty
  -- Send all cacheMisses to the builder.  It will update the
  -- cache as things complete.
  runExceptT (queueImageBuild (Map.toList _cacheMisses)) >>= either buildQueueFailure return
  return (fmap Left _shapeErrors <>
          fmap (Right . ImageFileShape) _cacheMisses <>
          fmap Left _cachedErrors <>
          fmap (Right . ImageFileReady) _cachedReady <>
          fmap (Right . ImageFileShape) _cachedShapes)
  where
      buildQueueFailure (e :: e) =
        unsafeFromIO (alog "Data.FileCache.Server" DEBUG
                       ("queueCacheImageFile failed: " ++ show e))

#if 0
      -- alog "Data.FileCache.Server" DEBUG ("cacheMiss - buildImageShape key=" <> show key)
      case shape of
        Left e -> modify (insert key (Left e))
        Right shape' -> do
          modify (insert key (Right (ImageFileShape shape')))
          -- We could collect these and send them all at once
          runExceptT (queueImageBuild [(key, shape')]) >>= handleQueueFailure
          -- modify (insert key (Right (ImageFileBuilding shape')))

#endif

-- | Build the image described by the 'ImageKey' if necessary, and
-- return its meta information as an 'ImageFile'.
cacheDerivedImage ::
  forall r e m. (Unexceptional m, MonadState Results m,
                 MonadError e m, HasSomeNonPseudoException e,
                 MonadReader r m, HasImageAcid r)
  => ImageKey
  -> m ()
cacheDerivedImage key@(ImageOriginal _ _) = do
  -- Not enough information to create a ByteString
  unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("cacheDerivedImage - invalid argument: " ++ show key)
  field @"_badKeys" %= Set.insert key
cacheDerivedImage key =
  cacheLook key >>= maybe cacheMiss cacheHit
  where
    cacheHit :: Either FileError ImageFile -> m () -- The cache has a recorded error
    cacheHit (Left e) = do
      field @"_cachedErrors" %= Map.insert key e
    cacheHit (Right (ImageFileReady img)) =
      field @"_cachedReady" %= Map.insert key img
    -- We need to distinguish which images returned by this function
    -- should be added to the build queue.  Th
    -- ImageFileShape means an image was requested and it was not in
    -- the cache.  Therefore ImageFileShape is never inserted into the
    -- acid cache.  Instead we insert ImageFileBuilding.
    cacheHit (Right (ImageFileShape shape)) =
      field @"_cachedShapes" %= Map.insert key shape
    -- ImageFileBuilding should mean the image is under construction,
    -- and will eventually appear as ImageFileReady or a FileError.
    -- FIXME: It might be that the server dies and leaves it in this
    -- state.  For this reason we should have a record of what images
    -- are currently in the ImageFileBuilding state and re-queue them
    -- when the server starts.
    cacheMiss :: m ()
    cacheMiss = do
      -- Its not in the cache, lets fix that immediately.
      cachePut' key noShape
      field @"_shapeErrors" %= Map.insert key NoShape
      -- Compute and save the shape of the requested image from its
      -- key.  This is not an expensive operation, but it does involve
      -- running file(1).
      runExceptT (buildImageShape key) >>= either shapeError shapeReady
    shapeError :: FileError -> m ()
    shapeError e = do
      field @"_shapeErrors" %= Map.insert key e
      cachePut' key (Left e :: Either FileError ImageFile)
    shapeReady :: ImageShape -> m ()
    shapeReady shape = do
      field @"_shapeErrors" %= Map.delete key
      field @"_cacheMisses" %= Map.insert key shape
      cachePut' key (Right (ImageFileShape shape))

    noShape :: Either FileError ImageFile
    noShape = Left NoShape

-- | Insert an image build request into the channel that is being polled
-- by the thread launched in startCacheImageFileQueue.
queueImageBuild ::
  (Unexceptional m, HasSomeNonPseudoException e, MonadReader r m, HasImageBuilder r)
  => [(ImageKey, ImageShape)]
  -> ExceptT e m ()
queueImageBuild pairs = do
  unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("queueImageBuild - requesting " ++ show (length pairs) ++ " images")
  chan <- lift (imageBuilder <$> ask)
  lyftIO (writeChan chan pairs)
  unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("queueImageBuild - requested " ++ show (length pairs) ++ " images")

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startImageBuilder ::
  forall r e m. (Unexceptional m, MonadError e m, HasSomeNonPseudoException e,
                 MonadReader r m, HasImageAcid r, HasFileCacheTop r)
  => m (ImageChan, ThreadId)
startImageBuilder = do
  r <- ask
  (chan :: ImageChan) <- lyftIO newChan
  (,) <$> pure chan <*> fork (forever (runExceptT (fromIO' (readChan chan)) >>= either doError (doImages r)))
  where
    doError (e :: SomeNonPseudoException) =
      unsafeFromIO $ alog "Data.FileCache.Server" ERROR ("Failure reading image cache request channel: " ++ show e)
    doImages :: r -> [(ImageKey, ImageShape)] -> UIO ()
    doImages r pairs = do
      unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("doImages - building " ++ show (length pairs) ++ " images")
      -- the threadDelay is to test the behavior of the server for lengthy image builds
      mapM_ (\(key, shape) -> cacheImageFile r key shape {- >> unsafeFromIO (threadDelay 5000000)-}) pairs
      unsafeFromIO $ alog "Data.FileCache.Server" DEBUG ("doImages - completed " ++ show (length pairs) ++ " images")

cacheImageFile ::
  forall r. (HasImageAcid r, HasFileCacheTop r)
  => r
  -> ImageKey
  -> ImageShape
  -> UIO (Either FileError (Either FileError ImageFile))
cacheImageFile r key shape =
  -- FIXME - get the reader monad from FileCacheT
  runReaderT (runExceptT (evalFileCacheT r () (runExceptT (buildImageFile key shape) >>= cachePut key))) r

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
  lift (cacheLook @ImageKey @ImageFile key) >>=
  maybe (throwError (CacheDamage "Original image not in cache"))
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
  forall e r m. (MonadCatch m, Unexceptional m,
                 MonadError e m, HasSomeNonPseudoException e,
                 MonadReader r m, HasImageAcid r, HasFileCacheTop r)
  => ImageKey -> ImageShape -> ExceptT FileError m ImageFile
buildImageFile key shape = do
  bs <- buildImageBytes key
  let file = File { _fileSource = Nothing
                  , _fileChksum = T.pack $ show $ md5 $ fromStrict bs
                  , _fileMessages = []
                  , _fileExt = fileExtension (_imageShapeType shape) }
  let img = ImageFileReady (ImageReady { _imageFile = file, _imageShape = shape })
  path <- fileCachePathIO (ImageCached key img)
  exists <- lyftIO $ doesFileExist path
  unless exists $ lyftIO $ writeFileReadable path bs
  return img

-- | Retrieve the 'ByteString' associated with an 'ImageKey'.
buildImageBytes ::
  forall r e m. (Unexceptional m, MonadCatch m, MonadError e m, HasSomeNonPseudoException e,
                 MonadReader r m, HasFileCacheTop r, HasImageAcid r)
  => ImageKey -> ExceptT FileError m BS.ByteString
buildImageBytes (ImageOriginal csum typ) = buildOriginalImageBytes csum typ
buildImageBytes (ImageUpright key) = buildUprightImageBytes key
buildImageBytes (ImageScaled sz dpi key) = buildScaledImageBytes sz dpi key
buildImageBytes (ImageCropped crop key) = buildCroppedImageBytes crop key

buildOriginalImageBytes ::
  forall r e m. (Unexceptional m, MonadCatch m,
                 MonadReader r m, HasImageAcid r, HasFileCacheTop r,
                 MonadError e m, HasSomeNonPseudoException e)
  => Checksum -> ImageType -> ExceptT FileError m BS.ByteString
buildOriginalImageBytes csum typ =
  lift (cacheLook key) >>=
  maybe (buildImageBytesFromFile key csum typ)
        (either (rebuildImageBytes False key typ) (lookImageBytes . ImageCached key))
  where
    key = ImageOriginal csum typ

-- There's a chance the file is just sitting on disk even though
-- it is not in the database - see if we can read it and verify
-- its checksum.
buildImageBytesFromFile ::
  (Unexceptional m, MonadCatch m,
   MonadError e m, HasSomeNonPseudoException e,
   MonadReader r m, HasImageAcid r, HasFileCacheTop r)
  => ImageKey -> Text -> ImageType -> ExceptT FileError m BS.ByteString
buildImageBytesFromFile key csum typ = do
  -- If we get a cache miss for an ImageOriginal key something
  -- has gone wrong.  Try to rebuild from the file if it exists.
  path <- fileCachePath (ImagePath key typ)
  exists <- lyftIO (doesFileExist path)
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
          _cached <- cacheOriginalImage bs
          return bs

buildUprightImageBytes ::
  (Unexceptional m, MonadCatch m,
   MonadError e m, HasSomeNonPseudoException e,
   MonadReader r m, HasFileCacheTop r, HasImageAcid r)
  => ImageKey -> ExceptT FileError m BS.ByteString
buildUprightImageBytes key =
  buildImageBytes key >>= \bs -> uprightImage' bs >>= maybe (return bs) return

buildScaledImageBytes ::
  (Unexceptional m, MonadCatch m,
   MonadError e m, HasSomeNonPseudoException e,
   MonadReader r m, HasFileCacheTop r, HasImageAcid r)
  => ImageSize -> Rational -> ImageKey -> ExceptT FileError m BS.ByteString
buildScaledImageBytes sz dpi key = do
  bs <- buildImageBytes key
  -- the buildImageBytes that just ran might have this info
  shape <- imageShapeM bs
  let scale' = scaleFromDPI sz dpi shape
      scale = fromRat (fromMaybe 1 scale')
  scaleImage' scale bs (imageType shape) >>= maybe (return bs) return

buildCroppedImageBytes ::
  (Unexceptional m, MonadCatch m,
   MonadError e m, HasSomeNonPseudoException e,
   MonadReader r m, HasFileCacheTop r, HasImageAcid r)
  => ImageCrop -> ImageKey -> ExceptT FileError m BS.ByteString
buildCroppedImageBytes crop key = do
  bs <- buildImageBytes key
  shape <- imageShapeM bs
  editImage' crop bs (imageType shape) (imageShape shape) >>= maybe (return bs) return

-- | Look up the image FilePath and read the ByteString it contains.
lookImageBytes ::
  (MonadReader r m, HasFileCacheTop r, Unexceptional m, HasSomeNonPseudoException e, HasURIPath a)
  => a -> ExceptT e m BS.ByteString
lookImageBytes a = fileCachePath a >>= lyftIO . BS.readFile

-- | There is an error stored in the cache, maybe it can be repaired
-- now?  Be careful not to get into a loop doing this.
rebuildImageBytes ::
  forall r e m. (Unexceptional m, MonadCatch m,
                 MonadError e m, HasSomeNonPseudoException e,
                 MonadReader r m, HasImageAcid r, HasFileCacheTop r)
  => Bool -> ImageKey -> ImageType -> FileError -> ExceptT FileError m BS.ByteString
rebuildImageBytes False _key _typ e = throwError e
rebuildImageBytes True key typ e@(CacheDamage _) = do
  unsafeFromIO (alog "Data.FileCache.Server" ALERT ("Retrying build of " ++ show key ++ " (e=" ++ show e ++ ")"))
  path <- fileCachePath (ImagePath key typ)
  -- This and other operations like it may throw an
  -- IOException - I need LyftIO to make sure this is caught.
  bs <- lyftIO (BS.readFile path)
  _cached <- cacheOriginalImage bs
  return bs
rebuildImageBytes True key _typ e = do
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
      lyftIO (putStrLn ("error loading " ++ show _imageFile ++ ": " ++ show e))
    checkFile file _path (Right bs)
      | T.pack (show (md5 (fromStrict bs))) /= (_fileChksum file) =
          lyftIO (putStrLn ("checksum mismatch in file " ++ show file))
    checkFile _file _path _bs = return ()

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]

-- splits (splitAt 3) [1,2,3,4,5,6,7] -> [[1,2,3],[4,5,6],[7]]
splits :: ([a] -> ([a], [a])) -> [a] -> [[a]]
splits f [] = []
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
    fixImageShape :: HasURIPath a => a -> ExceptT FileError m ImageShape
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
fixOriginalKeys f pairs = everywhere (mkT f) pairs
