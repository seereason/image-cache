-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Background
  ( ImageChan
  , HasImageBuilder(imageBuilder)
  , cacheDerivedImagesBackground
  , startImageBuilder
  -- , queueImageBuild
  ) where

--import Debug.Trace
import Control.Concurrent (ThreadId{-, threadDelay-}, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Exception (IOException)
import Control.Lens
import Control.Monad (forever, unless, when)
import Control.Monad.RWS (modify, MonadState, put, RWST(runRWST))
import Control.Monad.Reader (MonadReader(ask), runReaderT)
import Control.Monad.Trans.Except (runExceptT)
import Data.Acid ( AcidState, makeAcidic, openLocalStateFrom, Query, Update, query, update )
import Data.Binary.Get ( getLazyByteString, Get, skip, bytesRead, getWord16be, getWord32be, getWord16le, getWord32le, runGetOrFail )
import qualified Data.ByteString as BS ( ByteString, empty, readFile )
import Data.ByteString.Lazy ( fromStrict, toStrict )
import qualified Data.ByteString.Lazy as LBS ( ByteString, unpack, pack, take, drop, concat )
--import qualified Data.ByteString.UTF8 as P ( toString )
import Data.Char ( isSpace )
import Data.Default (def)
import Data.Digest.Pure.MD5 (md5)
import Data.Either (isLeft)
import Data.FileCache.Process
import Data.FileCache.FileCacheTop
import Data.FileCache.Acid
import Data.FileCache.Derive (cacheImageFile, cacheLookImages)
import Data.FileCache.CacheMap
import Data.FileCache.Derive
import Data.FileCache.ImageIO
import Data.FileCache.FileCache
import Data.FileCache.Common
import Data.FileCache.FileInfo (fileInfoFromPath)
import Data.FileCache.LogException (logException)
import Data.FileCache.CommandError (CommandInfo(..))
import Data.FileCache.Upload
import Data.Generics.Product ( field )
import Data.List ( intercalate )
import Data.ListLike ( length, show )
import Data.Map.Strict as Map ( delete, difference, fromList, Map, fromSet, insert, intersection, lookup, toList, union )
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.Ratio ((%))
import Data.Set as Set (member, Set)
import Data.String (fromString)
import Data.Text as T ( pack, Text, unpack )
import Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import Data.Typeable (Typeable, typeOf)
import Data.Word ( Word16, Word32 )
import SeeReason.Errors (HasNonIOException, liftUIO, Member, NonIOException(..), OneOf, runOneOf, throwMember)
import qualified SeeReason.Errors as Errors (oneOf)
import Extra.Except (ExceptT, HasIOException, lift,
                     MonadError, throwError, tryError)
import GHC.Int ( Int64 )
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Network.URI ( URI(..), uriToString )
import Numeric ( fromRat, showFFloat )
import Prelude hiding (length, show)
import SeeReason.LogServer (alog)
import System.Directory ( createDirectoryIfMissing, doesFileExist )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>), makeRelative, takeDirectory )
import System.FilePath.Extra ( writeFileReadable )
import System.IO (hFlush, stdout)
import System.Log.Logger (Priority(..))
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
import Web.Routes (fromPathInfo, toPathInfo)
import Web.Routes.QuickCheck (pathInfoInverse_prop)

{-
class HasSomeNonPseudoException e where
  someNonPseudoException :: Prism e SomeNonPseudoException

instance HasSomeNonPseudoException ReportError where
  someNonPseudoException = _Ctor @"FileError" . someNonPseudoException
-}

type ImageChan = Chan [(ImageKey, ImageShape)]
class HasImageBuilder a where imageBuilder :: a -> Maybe ImageChan
instance HasImageBuilder ImageChan where imageBuilder = Just
instance HasImageBuilder (a, b, ImageChan) where imageBuilder = Just . view _3

cacheDerivedImagesBackground ::
  forall r e m. (FileCacheErrors e m, MonadReader r m, HasCacheAcid r,
                 HasImageBuilder r, HasFileCacheTop r)
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
cacheDerivedImagesBackground flags keys =
  cacheLookImages keys >>=
  mapM (cacheImageShape flags) >>=
  runExceptT . backgroundBuilds >>=
  either throwError (return . Map.fromList)

backgroundBuilds ::
  (Unexceptional m, HasFileError e, HasIOException e, HasNonIOException e,
   MonadReader r m, HasImageBuilder r, HasFileCacheTop r)
  => [(ImageKey, Either FileError ImageFile)]
  -> ExceptT e m [(ImageKey, Either FileError ImageFile)]
backgroundBuilds pairs =
  queueImageBuild (mapMaybe isShape pairs) >> return pairs
  where isShape (key, Right (ImageFileShape shape)) = Just (key, shape)
        isShape _ = Nothing

-- | Insert an image build request into the channel that is being polled
-- by the thread launched in startCacheImageFileQueue.
queueImageBuild ::
  (FileCacheErrors e m, MonadReader r m, HasImageBuilder r, HasFileCacheTop r, HasCallStack)
  => [(ImageKey, ImageShape)]
  -> ExceptT (OneOf e) m ()
queueImageBuild pairs = do
  -- Write empty files into cache
  -- mapM (runExceptT . fileCachePathIO) pairs >>= mapM_ (either (throwError . review fileError) (liftUIO . flip writeFile mempty))
  unsafeFromIO $ alog DEBUG ("queueImageBuild - requesting " ++ show (length pairs) ++ " images")
  chan <- lift (imageBuilder <$> ask)
  liftUIO (writeChan chan pairs)
  unsafeFromIO $ alog DEBUG ("queueImageBuild - requested " ++ show (length pairs) ++ " images")

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startImageBuilder ::
  forall r e m. (FileCacheErrors e m, MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => m (ImageChan, ThreadId)
startImageBuilder = do
  r <- ask
  (chan :: ImageChan) <- liftUIO newChan
  (,) <$> pure chan <*> fork (forever (runExceptT (fromIO' (review someNonPseudoException) (readChan chan)) >>= either doError (doImages r)))
  where
    -- This is the background task
    doError (e :: SomeNonPseudoException) =
      unsafeFromIO $ alog ERROR ("Failure reading image cache request channel: " ++ show e)
    doImages :: r -> [(ImageKey, ImageShape)] -> UIO ()
    doImages r pairs = do
      unsafeFromIO $ alog DEBUG ("doImages - building " ++ show (length pairs) ++ " images")
      -- the threadDelay is to test the behavior of the server for lengthy image builds
      r <- mapM (\(key, shape) -> runExceptT @e (runReaderT (cacheImageFile key shape {- >> unsafeFromIO (threadDelay 5000000)-}) r)) pairs
      mapM_ (\case ((key, shape), Left e) -> unsafeFromIO (alog ERROR ("doImages - error building " <> show key <> ": " ++ show e))
                   ((key, shape), Right (Left e)) -> unsafeFromIO (alog ERROR ("doImages - error in cache for " <> show key <> ": " ++ show e))
                   ((key, shape), Right (Right _e)) -> unsafeFromIO (alog ERROR ("doImages - completed " <> show key)))
        (zip pairs r)
