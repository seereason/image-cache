-- | Maintain a cache of files.
--
-- A data structure representing a local cache of a data file.  The
-- cached file persists across runs of our application, and can be
-- accessed by name and passed to software which requires a file, for
-- example a document formatter such as LaTeX.  The original data can
-- be supplied as either a URI, a local file path, or as a ByteString.
-- The file is then downloaded and stored on the local machine at a
-- location based on the file's checksum.

{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Appraisal.FileCache
    ( -- * Monad and Class
      MonadFileCache(fileCacheTop)
    , FileCacheTop(..)
    , FileCacheT(unFileCacheT)
    , runFileCacheT
    , MonadFileCacheIO
    , runFileCacheIO
    , runFileCache
    -- Types
    , Checksum
    , FileSource(..), fileSource, fileChksum, fileMessages
    , File(..)
    , fileURI
    , fileCacheURI
    , addMessage
    , md5'
    -- * Create Files
    , fileFromBytes
    , fileFromURI               -- was importFile
    , fileFromPath
    , fileFromPathViaRename
    , fileFromCmd
    , fileFromCmdViaTemp
    , cacheFile
    -- * Query Files
    , loadBytes
    , fileCachePath
    , fileCachePathIO
    ) where

import Appraisal.AcidCache (runMonadCacheT)
import Appraisal.Utils.ErrorWithIO (logException, readCreateProcessWithExitCode')
import Control.Exception (Exception, IOException)
import Control.Lens (makeLenses, over, set, view)
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.Except (ExceptT, MonadError, catchError, throwError)
import Control.Monad.Reader (mapReaderT, MonadReader(ask, local), ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans (MonadIO, MonadTrans(lift), liftIO)
import Data.Acid (AcidState)
import qualified Data.ByteString.Lazy.Char8 as Lazy
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Digest.Pure.MD5 (md5)
import Data.Generics (Data(..), Typeable)
import Data.Map (Map)
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy)
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.TypeGraph.Serialize (deriveSerialize)
import Network.URI (URI(..), URIAuth(..), parseRelativeReference, parseURI)
import System.Directory (createDirectoryIfMissing, doesFileExist, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath.Extra (writeFileReadable, makeReadableAndClose)
import System.IO (openBinaryTempFile)
import System.Log.Logger (logM, Priority(DEBUG))
import System.Process (proc, shell{-, showCommandForUser-})
--import System.Process.ListLike (readCreateProcessWithExitCode)
import System.Unix.FilePath ((<++>))
import Test.QuickCheck (Arbitrary(..), oneof)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

-- | Almost all file cache operations require IO, but constructing
-- paths do not.  So MonadIO is not a superclass here.
class Monad m => MonadFileCache m where
    fileCacheTop :: m FilePath

-- This instance is omitted because it prevents us from using the
-- reader monad for anything but MonadFileCache.
-- instance MonadReader FileCacheTop m => MonadFileCache m where
--     fileCacheTop = unFileCacheTop <$> ask

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

-- | In order to avoid type ambiguities between different reader monads, we need
-- a newtype wrapper around this ReaderT FileCacheTop.
newtype FileCacheT m a = FileCacheT {unFileCacheT :: ReaderT FileCacheTop m a} deriving (Monad, Applicative, Functor)

-- | Get 'fileCacheTop' from the wrapped reader monad.
instance (Monad m, Monad (FileCacheT m)) => MonadFileCache (FileCacheT m) where
    -- fileCacheTop :: FileCacheT m FilePath
    fileCacheTop = FileCacheT (ReaderT (return . unFileCacheTop))

mapFileCacheT :: (m a -> m a) -> FileCacheT m a -> FileCacheT m a
mapFileCacheT f = FileCacheT . mapReaderT f . unFileCacheT

instance MonadTrans FileCacheT where
    lift = FileCacheT . lift
instance MonadReader r m => MonadReader r (FileCacheT m) where
    ask = lift ask
    local = mapFileCacheT . local
instance MonadIO m => MonadIO (FileCacheT m) where
    liftIO = FileCacheT . liftIO
instance MonadThrow m => MonadThrow (FileCacheT m) where
    throwM e = lift $ throwM e
instance MonadCatch m => MonadCatch (FileCacheT m) where
    catch :: forall e a. Exception e => FileCacheT m a -> (e -> FileCacheT m a) -> FileCacheT m a
    catch (FileCacheT m) c = FileCacheT $ m `catch` (unFileCacheT . c)
-- Hmm, familiar...
instance MonadError e m => MonadError e (FileCacheT m) where
    throwError :: e -> FileCacheT m a
    throwError e = lift $ throwError e
    catchError :: FileCacheT m a -> (e -> FileCacheT m a) -> FileCacheT m a
    catchError (FileCacheT m) c = FileCacheT $ m `catchError` (unFileCacheT . c)
instance MonadFileCache m => MonadFileCache (ExceptT IOException m) where
    fileCacheTop = lift fileCacheTop

runFileCacheIO :: forall key val m a.
                  (MonadIO m, MonadCatch m, MonadError IOException m) =>
                  AcidState (Map key val)
               -> FileCacheTop
               -> FileCacheT (ReaderT (AcidState (Map key val)) m) a
               -> m a
runFileCacheIO fileAcidState fileCacheDir action =
    runMonadCacheT (runFileCacheT fileCacheDir action) fileAcidState

-- | Like runFileCacheIO, but without the MonadIO superclass.  No acid
-- state value is passed because you need IO to use acid state.
-- Typical use is to construct paths to the file cache.
runFileCache :: FileCacheTop -> FileCacheT m a -> m a
runFileCache fileCacheDir action = runReaderT (unFileCacheT action) fileCacheDir

runFileCacheT :: forall m a.
                 (MonadIO m, MonadCatch m, MonadError IOException m) =>
                 FileCacheTop
              -> FileCacheT m a
              -> m a
runFileCacheT fileCacheDir action =
    runReaderT (unFileCacheT (ensureFileCacheTop >> action)) fileCacheDir

-- | This is the class for operations that do require IO.  Almost all
-- operations require IO, but you can build paths into the cache
-- without it.
class (MonadFileCache m, MonadCatch m, MonadError IOException m, MonadIO m) => MonadFileCacheIO m where
    ensureFileCacheTop :: m () -- Create the fileCacheTop directory if necessary

-- | Probably the only meaningful instance of MonadFileCacheIO.
instance (MonadFileCache m, MonadCatch m, MonadError IOException m, MonadIO m) => MonadFileCacheIO m where
    ensureFileCacheTop = fileCacheTop >>= liftIO . createDirectoryIfMissing True

-- |The original source if the file is saved, in case
-- the cache needs to be reconstructed.  However, we don't
-- store the original ByteString if that is all we began
-- with, that would be redundant and wasteful.
data FileSource
    = TheURI String
    | ThePath FilePath
    deriving (Show, Read, Eq, Ord, Data, Typeable)

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = String

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

$(makeLenses ''File)

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: File -> Maybe URI
fileURI file = case view fileSource file of
                 Just (TheURI uri) -> maybe (parseRelativeReference uri) Just (parseURI uri)
                 _ -> Nothing

-- | Build a URI for the locally cached version of the file given the
-- uri of the cache home directory.
fileCacheURI :: URI -> File -> URI
fileCacheURI cacheDirectoryURI file =
    cacheDirectoryURI {uriPath = uriPath cacheDirectoryURI <++> view fileChksum file}

-- |Add a message to the file message list.
addMessage :: String -> File -> File
addMessage message file = over fileMessages (++ [message]) file

md5' :: P.ByteString -> String
#ifdef LAZYIMAGES
md5' = show . md5
#else
md5' = show . md5 . Lazy.fromChunks . (: [])
#endif

-- | Turn the bytes in a ByteString into a File.  This is an IO
-- operation because it saves the data into the local cache.  We
-- use writeFileReadable because the files we create need to be
-- read remotely by our backup program.
fileFromBytes ::
    MonadFileCacheIO m
    => P.ByteString
    -> m (File, P.ByteString)
fileFromBytes bytes =
      do let file = File { _fileSource = Nothing
                         , _fileChksum = md5' bytes
                         , _fileMessages = [] }
         path <- fileCachePath file
         exists <- liftIO $ doesFileExist path
         case exists of
           True -> return (file, bytes)
           False -> liftIO (writeFileReadable path bytes) >> return (file, bytes)

-- |Read the contents of a local path into a File.
fileFromPath ::
    MonadFileCacheIO m
    => FilePath
    -> m (File, P.ByteString)
fileFromPath path = do
  bytes <- liftIO $ P.readFile path
  (file, bytes') <- fileFromBytes bytes
  return (set fileSource (Just (ThePath path)) file, bytes')

-- | A shell command whose output becomes the contents of the file.
fileFromCmd ::
    MonadFileCacheIO m
    => String
    -> m (File, P.ByteString)
fileFromCmd cmd = do
  (code, out, _err) <- liftIO (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do (file, bytes) <- fileFromBytes out
           return $ (set fileSource (Just (ThePath cmd)) file, bytes)
    ExitFailure _ -> error $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI ::
    MonadFileCacheIO m
    => String
    -> m (File, P.ByteString)
fileFromURI uri =
    do let cmd = "curl"
           args = ["-s", uri]
       (code, bytes, _err) <- liftIO $ readCreateProcessWithExitCode' (proc cmd args) P.empty
       case code of
         ExitSuccess ->
             do (file, bytes') <- fileFromBytes bytes
                return (set fileSource (Just (TheURI uri)) file, bytes')
         _ -> $logException $ fail $ "fileFromURI Failure: " ++ cmd ++ " -> " ++ show code

-- | Build a file from the output of a command.  This uses a temporary
-- file to store the contents of the command while we checksum it.  This
-- is to avoid reading the file contents into a Haskell ByteString, which
-- may be slower than using a unix pipeline.  Though it shouldn't be.
fileFromCmdViaTemp ::
    forall m. MonadFileCacheIO m
    => String
    -> m (File, P.ByteString)
fileFromCmdViaTemp cmd = do
  dir <- fileCacheTop
  (tmp, h) <- liftIO (openBinaryTempFile dir "scaled")
  let cmd' = cmd ++ " > " ++ tmp
  liftIO (makeReadableAndClose h)
  -- io (hClose h)
  (code, _out, _err) <- liftIO (readCreateProcessWithExitCode' (shell cmd') P.empty)
  case code of
    ExitSuccess -> installFile tmp
    ExitFailure _ -> error $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code
    where
      installFile :: FilePath -> m (File, P.ByteString)
      installFile tmp = fileFromPathViaRename tmp `catchError` (\ e -> throwError (userError $ "fileFromCmdViaTemp - install failed: " ++ show e))

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaRename :: MonadFileCacheIO m => FilePath -> m (File, P.ByteString)
fileFromPathViaRename path = do
  bytes <- liftIO $ P.readFile path
  let file = File { _fileSource = Just (ThePath path)
                  , _fileChksum = md5' bytes
                  , _fileMessages = [] }
  -- cksum <- (\(_, out, _) -> take 32 out) <$> liftIO (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
  dest <- fileCachePathIO file
  liftIO (logM "fileFromPathViaRename" DEBUG ("renameFile " <> path <> " " <> dest) >>
          renameFile path dest)
  return (file, bytes)

-- | Given a file and a ByteString containing the expected contents,
-- verify the contents.  If it isn't installed or isn't correct,
-- (re)install it.
cacheFile :: MonadFileCacheIO m => File -> P.ByteString -> m File
cacheFile file bytes = do
  path <- fileCachePath file
  (loadBytes file >>= checkBytes) `catchError` (\ _e -> liftIO (writeFileReadable path bytes) >> return file)
    where
      checkBytes loaded = if loaded == bytes
                          then $logException $ fail "cacheFile - Checksum error"
                          else return file

-- | Read and return the contents of the file from the cache as a ByteString.
loadBytes :: MonadFileCacheIO m => File -> m P.ByteString
loadBytes file =
    do path <- fileCachePath file
       bytes <- liftIO (P.readFile path)
       case md5' bytes == view fileChksum file of
         True -> return bytes
         False -> $logException $ fail $ "Checksum mismatch: expected " ++ show (view fileChksum file) ++ ", file contains " ++ show (md5' bytes)

instance Pretty File where
    pPrint (File _ cksum _) = text ("File(" <> show cksum <> ")")

-- | The full path name for the local cache of the file.
fileCachePath :: MonadFileCache m => File -> m FilePath
fileCachePath file = fileCacheTop >>= \ver -> return $ ver <++> view fileChksum file

fileCacheDir :: MonadFileCache m => File -> m FilePath
fileCacheDir _file = fileCacheTop

fileCachePathIO :: MonadFileCacheIO m => File -> m FilePath
fileCachePathIO file = do
  dir <- fileCacheDir file
  liftIO $ createDirectoryIfMissing True dir
  fileCachePath file

instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure []

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]

$(deriveSafeCopy 1 'base ''FileSource)
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
$(deriveSafeCopy 1 'base ''File)
$(deriveLiftMany [
   ''FileSource,
   ''URI,
   ''URIAuth,
   ''File])
$(deriveSerialize [t|FileSource|])
$(deriveSerialize [t|File|])
