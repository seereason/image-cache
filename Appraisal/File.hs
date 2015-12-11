{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}
-- |A data structure representing a local cache of a data file.  The
-- cached file persists across runs of our application, and can be
-- accessed by name and passed to software which requires a file, for
-- example a document formatter such as LaTeX.  The original data can
-- be supplied as either a URI, a local file path, or as a ByteString.
-- The file is then downloaded and stored on the local machine at a
-- location based on the file's checksum.
module Appraisal.File
    ( module Network.URI
    , MonadFileCacheTop(fileCacheTop)
    , FileCacheTop(..)
    , Checksum
    , File(..)
    , FileSource(..)
    , fileFromURI               -- was importFile
    , fileFromPath
    , fileFromBytes
    , fileFromFile
    , fileFromCmd
    , fileFromCmdViaTemp
    , fileURI
    , fileCachePath
    , fileCacheURI
    , loadBytes
    , cacheFile
    , addMessage
    ) where

import Appraisal.Utils.ErrorWithIO (logException, readCreateProcessWithExitCode')
import Control.Applicative ((<$>))
import Control.Exception (IOException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError, catchError, throwError)
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.ByteString.Lazy.Char8 as Lazy
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Digest.Pure.MD5 (md5)
import Data.Generics (Data(..), Typeable)
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base)
import Network.URI (URI(..), URIAuth(..), parseRelativeReference, parseURI)
import System.Directory (doesFileExist, renameFile)
import System.Exit (ExitCode(..))
import System.FilePath.Extra (writeFileReadable, makeReadableAndClose)
import System.IO (openBinaryTempFile)
import System.Log.Logger (logM, Priority(DEBUG))
import System.Process (proc, shell, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode)
import System.Unix.FilePath ((<++>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

class Monad m => MonadFileCacheTop m where
    fileCacheTop :: m FilePath

instance Monad m => MonadFileCacheTop (ReaderT FileCacheTop m) where
    fileCacheTop  = ask >>= \ (FileCacheTop x) -> return x

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
    = File { fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , fileMessages :: [String]           -- ^ Messages received while manipulating the file
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Pretty File where
    pPrint (File _ cksum _) = text ("File(" <> show cksum <> ")")

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) =>
               String           -- ^ The URI to retrieve
            -> m (File, P.ByteString)
fileFromURI uri =
    do let cmd = "curl"
           args = ["-s", uri]
       (code, bytes, _err) <- liftIO $ readCreateProcessWithExitCode' (proc cmd args) P.empty
       case code of
         ExitSuccess ->
             do file <- fileFromBytes bytes
                return (file {fileSource = Just (TheURI uri)}, bytes)
         _ -> $logException $ fail $ "fileFromURI Failure: " ++ cmd ++ " -> " ++ show code

-- |Read the contents of a local path into a File.
fileFromPath :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m) =>
                FilePath        -- ^ The local pathname to copy into the cache
             -> m (File, P.ByteString)
fileFromPath path =
    do bytes <- liftIO $ P.readFile path
       file <- fileFromBytes bytes
       return (file {fileSource = Just (ThePath path)}, bytes)

-- | Move a file into the file cache and incorporate it into a File.
fileFromFile :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m, Functor m) =>
                FilePath        -- ^ The local pathname to copy into the cache
             -> m File
fileFromFile path = do
    cksum <- (\ (_, out, _) -> take 32 out) <$> liftIO (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
    let file = File { fileSource = Just (ThePath path)
                    , fileChksum = cksum
                    , fileMessages = [] }
    dest <- fileCachePath file
    liftIO (logM "fileFromFile" DEBUG ("renameFile " <> path <> " " <> dest) >>
            renameFile path dest)
    return file

fileFromCmd :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m) =>
               String           -- ^ A shell command whose output becomes the contents of the file.
            -> m File
fileFromCmd cmd = do
  (code, out, _err) <- liftIO (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do file <- fileFromBytes out
           return $ file {fileSource = Just (ThePath cmd)}
    ExitFailure _ -> error $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code

-- | Build a file from the output of a command.  We use a temporary
-- file to store the contents of the command while we checksum it to
-- avoid reading the command's output into RAM.
fileFromCmdViaTemp :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m, Functor m) =>
                      String           -- ^ A shell command whose output becomes the contents of the file.
                   -> m File
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
      installFile tmp = fileFromFile tmp `catchError` (\ e -> throwError (userError $ "fileFromCmdViaTemp - install failed: " ++ show e))

-- |Turn the bytes in a ByteString into a File.  This is an IO operation
-- because it saves the data into the local cache.  We use writeFileReadable
-- because the files we create need to be read remotely by our backup program.
fileFromBytes :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m) =>
                 P.ByteString   -- ^ The bytes to store as the file's contents
              -> m File
fileFromBytes bytes =
    do let file = File { fileSource = Nothing
                       , fileChksum = md5' bytes
                       , fileMessages = [] }
       path <- fileCachePath file
       exists <- liftIO $ doesFileExist path
       case exists of
         True -> return file
         False -> liftIO (writeFileReadable path bytes) >> return file

-- | Make sure a file is correctly installed in the cache, and if it
-- isn't install it.
cacheFile :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) =>
             File                     -- ^ The file to verify
          -> P.ByteString             -- ^ Expected contents
          -> m File
cacheFile file bytes = do
  path <- fileCachePath file
  (loadBytes file >>= checkBytes) `catchError` (\ _e -> liftIO (writeFileReadable path bytes) >> return file)
    where
      checkBytes loaded = if loaded == bytes
                          then $logException $ fail "cacheFile - Checksum error"
                          else return file

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: File -> Maybe URI
fileURI (File {fileSource = Just (TheURI uri)}) = maybe (parseRelativeReference uri) Just (parseURI uri)
fileURI _ = Nothing

-- |A URI for the locally cached version of the file.
fileCacheURI :: URI             -- ^ The URI of the cache home directory
             -> File            -- ^ The file whose URI should be returned
             -> URI
fileCacheURI cacheDirectoryURI file =
    cacheDirectoryURI {uriPath = uriPath cacheDirectoryURI <++> fileChksum file}

-- |The full path name for the local cache of the file.
fileCachePath :: MonadFileCacheTop m =>
                 File           -- ^ The file whose path should be returned
              -> m FilePath
fileCachePath file = fileCacheTop >>= \ ver -> return $ ver <++> fileChksum file

-- |Read and return the contents of the file from the cache.
loadBytes :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) =>
             File               -- ^ The file whose bytes should be loaded
          -> m P.ByteString
loadBytes file =
    do path <- fileCachePath file
       bytes <- liftIO (P.readFile path)
       case md5' bytes == fileChksum file of
         True -> return bytes
         False -> $logException $ fail $ "Checksum mismatch: expected " ++ show (fileChksum file) ++ ", file contains " ++ show (md5' bytes)

-- |Add a message to the file message list.
addMessage :: String -> File -> File
addMessage message file = file {fileMessages = fileMessages file ++ [message]}

md5' :: P.ByteString -> String
#ifdef LAZYIMAGES
md5' = show . md5
#else
md5' = show . md5 . Lazy.fromChunks . (: [])
#endif

$(deriveSafeCopy 1 'base ''File)
$(deriveSafeCopy 1 'base ''FileSource)
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
