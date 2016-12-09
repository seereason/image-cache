-- | A data structure representing a local cache of a data file.  The
-- cached file persists across runs of our application, and can be
-- accessed by name and passed to software which requires a file, for
-- example a document formatter such as LaTeX.  The original data can
-- be supplied as either a URI, a local file path, or as a ByteString.
-- The file is then downloaded and stored on the local machine at a
-- location based on the file's checksum.

{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, PartialTypeSignatures,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Appraisal.FileCache
    ( module Network.URI
    , MonadFileCache(fileCacheTop)
    , FileCacheTop(..)
    , CacheFile(..)
    , Checksum
    , FileSource(..)
    , fileFromURI               -- was importFile
    , fileFromPath
    , fileFromCmd
    , fileFromCmdViaTemp
    , fileURI
    , fileCacheURI
    , loadBytes
    , cacheFile
    , addMessage
    , md5'
    ) where

import Appraisal.Utils.ErrorWithIO (logException, readCreateProcessWithExitCode')
import Control.Exception (IOException)
import Control.Lens (Lens', over, set, view)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (defaultOptions)
import qualified Data.ByteString.Lazy.Char8 as Lazy
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Digest.Pure.MD5 (md5)
import Data.Generics (Data(..), Typeable)
import Data.SafeCopy (deriveSafeCopy, base)
import Language.Haskell.TH.Lift (deriveLiftMany)
import Network.URI (URI(..), URIAuth(..), parseRelativeReference, parseURI)
import System.Exit (ExitCode(..))
import System.FilePath.Extra (writeFileReadable, makeReadableAndClose)
import System.IO (openBinaryTempFile)
import System.Process (proc, shell)
import System.Unix.FilePath ((<++>))

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

class (MonadCatch m, MonadError IOException m, MonadIO m) => MonadFileCache m where
    fileCacheTop :: m FilePath

instance (MonadReader FileCacheTop m, MonadCatch m, MonadError IOException m, MonadIO m)
    => MonadFileCache m where
    fileCacheTop = unFileCacheTop <$> ask

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

class CacheFile file where
    fileSource :: Lens' file (Maybe FileSource)
    fileChksum :: Lens' file Checksum
    fileMessages :: Lens' file [String]
    -- ^ Any messages that were produced when creating the file
    fileCachePath :: MonadFileCache m => file -> m FilePath
    -- ^ The full path name for the local cache of the file.
    fileFromFile :: MonadFileCache m => FilePath -> m file
    fileFromBytes :: MonadFileCache m => P.ByteString -> m file
    -- ^ Turn the bytes in a ByteString into a File.  This is an IO
    -- operation because it saves the data into the local cache.  We
    -- use writeFileReadable because the files we create need to be
    -- read remotely by our backup program.

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI :: (CacheFile file, MonadFileCache m) => String -> m (file, P.ByteString)
fileFromURI uri =
    do let cmd = "curl"
           args = ["-s", uri]
       (code, bytes, _err) <- liftIO $ readCreateProcessWithExitCode' (proc cmd args) P.empty
       case code of
         ExitSuccess ->
             do file <- fileFromBytes bytes
                return (set fileSource (Just (TheURI uri)) file, bytes)
         _ -> $logException $ fail $ "fileFromURI Failure: " ++ cmd ++ " -> " ++ show code

-- |Read the contents of a local path into a File.
fileFromPath :: (CacheFile file, MonadFileCache m) => FilePath -> m (file, P.ByteString)
fileFromPath path =
    do bytes <- liftIO $ P.readFile path
       file <- fileFromBytes bytes
       return (set fileSource (Just (ThePath path)) file, bytes)

-- | Move a file into the file cache and incorporate it into a File.
#if 0
fileFromFile :: (CacheFile file, MonadFileCache m) =>
                FilePath        -- ^ The local pathname to copy into the cache
             -> m file
fileFromFile path = do
    cksum <- (\ (_, out, _) -> take 32 out) <$> liftIO (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
    let file = File { _fileSource = Just (ThePath path)
                    , _fileChksum = cksum
                    , _fileMessages = [] }
    dest <- fileCachePath file
    liftIO (logM "fileFromFile" DEBUG ("renameFile " <> path <> " " <> dest) >>
            renameFile path dest)
    return file
#endif

fileFromCmd :: (CacheFile file, MonadFileCache m) =>
               String           -- ^ A shell command whose output becomes the contents of the file.
            -> m file
fileFromCmd cmd = do
  (code, out, _err) <- liftIO (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do file <- fileFromBytes out
           return $ set fileSource (Just (ThePath cmd)) file
    ExitFailure _ -> error $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code

-- | Build a file from the output of a command.  This uses a temporary
-- file to store the contents of the command while we checksum it.  This
-- is to avoid reading the file contents into a Haskell ByteString, which
-- seems to be slower than using a unix pipeline.
fileFromCmdViaTemp :: forall m file. (MonadFileCache m, CacheFile file) => String -> m file
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
      installFile :: FilePath -> m file
      installFile tmp = fileFromFile tmp `catchError` (\ e -> throwError (userError $ "fileFromCmdViaTemp - install failed: " ++ show e))

-- | Given a file and a ByteString containing the expected contents,
-- verify the contents.  If it isn't installed or isn't correct,
-- (re)install it.
cacheFile :: (MonadFileCache m, CacheFile file) => file -> P.ByteString -> m file
cacheFile file bytes = do
  path <- fileCachePath file
  (loadBytes file >>= checkBytes) `catchError` (\ _e -> liftIO (writeFileReadable path bytes) >> return file)
    where
      checkBytes loaded = if loaded == bytes
                          then $logException $ fail "cacheFile - Checksum error"
                          else return file

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: CacheFile file => file -> Maybe URI
fileURI file = case view fileSource file of
                 Just (TheURI uri) -> maybe (parseRelativeReference uri) Just (parseURI uri)
                 _ -> Nothing

-- | Build a URI for the locally cached version of the file given the
-- uri of the cache home directory.
fileCacheURI :: CacheFile file => URI -> file -> URI
fileCacheURI cacheDirectoryURI file =
    cacheDirectoryURI {uriPath = uriPath cacheDirectoryURI <++> view fileChksum file}

-- | Read and return the contents of the file from the cache as a ByteString.
loadBytes :: (MonadFileCache m, CacheFile file) => file -> m P.ByteString
loadBytes file =
    do path <- fileCachePath file
       bytes <- liftIO (P.readFile path)
       case md5' bytes == view fileChksum file of
         True -> return bytes
         False -> $logException $ fail $ "Checksum mismatch: expected " ++ show (view fileChksum file) ++ ", file contains " ++ show (md5' bytes)

-- |Add a message to the file message list.
addMessage :: CacheFile file => String -> file -> file
addMessage message file = over fileMessages (++ [message]) file

md5' :: P.ByteString -> String
#ifdef LAZYIMAGES
md5' = show . md5
#else
md5' = show . md5 . Lazy.fromChunks . (: [])
#endif

$(deriveSafeCopy 1 'base ''FileSource)
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
$(deriveLiftMany [
   ''FileSource,
   ''URI,
   ''URIAuth
  ])
$(deriveJSON defaultOptions ''FileSource)
