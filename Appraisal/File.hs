{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
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
    , ImageCacheTop(..)
    , Checksum
    , File(..)
    , FileSource(..)
    , fileFromURI		-- was importFile
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

import Appraisal.Utils.ErrorWithIO (ErrorWithIO, io, catch, logExceptionM,
                                    readCreateProcessWithExitCode')
import Appraisal.Utils.Files (writeFileReadable, makeReadableAndClose)
import Appraisal.Utils.Prelude
import Control.Applicative ((<$>))
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
import System.IO (openBinaryTempFile)
import System.Log.Logger (logM, Priority(DEBUG))
import System.Process (proc, shell, showCommandForUser)
import System.Process.String (readCreateProcess)
import System.Process.ListLike (unStdoutWrapper)
import System.Process.ListLike.StrictString ()
import System.Unix.FilePath ((<++>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

newtype ImageCacheTop = ImageCacheTop {images :: FilePath}

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
    = File { fileSource :: Maybe FileSource	-- ^ Where the file's contents came from
           , fileChksum :: Checksum		-- ^ The checksum of the file's contents
           , fileMessages :: [String]		-- ^ Messages received while manipulating the file
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Pretty File where
    pPrint (File _ cksum _) = text ("File(" <> show cksum <> ")")

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI :: ImageCacheTop		-- ^ The home directory of the cache
            -> String		-- ^ The URI to retrieve
            -> ErrorWithIO (File, P.ByteString)
fileFromURI ver uri =
    do let cmd = "curl"
           args = ["-s", uri]
       (code, bytes, _err) <- io $ readCreateProcessWithExitCode' (proc cmd args) P.empty
       case code of
         ExitSuccess ->
             do file <- fileFromBytes ver bytes
                return (file {fileSource = Just (TheURI uri)}, bytes)
         _ -> logExceptionM "Appraisal.File.fileFromURI" $ fail $ "fileFromURI Failure: " ++ cmd ++ " -> " ++ show code

-- |Read the contents of a local path into a File.
fileFromPath :: ImageCacheTop       	-- ^ The home directory of the cache
             -> FilePath	-- ^ The local pathname to copy into the cache
             -> ErrorWithIO (File, P.ByteString)
fileFromPath ver path =
    do bytes <- io $ P.readFile path
       file <- fileFromBytes ver bytes
       return (file {fileSource = Just (ThePath path)}, bytes)

-- | Move a file into the file cache and incorporate it into a File.
fileFromFile :: ImageCacheTop       	-- ^ The home directory of the cache
             -> FilePath	-- ^ The local pathname to copy into the cache
             -> ErrorWithIO File
fileFromFile ver path = do
    cksum <- (take 32 . unStdoutWrapper) <$> io (readCreateProcess (shell ("md5sum < " ++ showCommandForUser path [])) "")
    let file = File { fileSource = Just (ThePath path)
                    , fileChksum = cksum
                    , fileMessages = [] }
    io (logM "fileFromFile" DEBUG ("renameFile " <> path <> " " <> fileCachePath ver file) >>
        renameFile path (fileCachePath ver file))
    return file

fileFromCmd :: ImageCacheTop
            -> String           -- ^ A shell command whose output becomes the contents of the file.
            -> ErrorWithIO File
fileFromCmd ver cmd = do
  (code, out, _err) <- io (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do file <- fileFromBytes ver out
           return $ file {fileSource = Just (ThePath cmd)}
    ExitFailure _ -> myerror $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code

-- | Build a file from the output of a command.  We use a temporary
-- file to store the contents of the command while we checksum it to
-- avoid reading the command's output into RAM.
fileFromCmdViaTemp :: ImageCacheTop
                   -> String           -- ^ A shell command whose output becomes the contents of the file.
                   -> ErrorWithIO File
fileFromCmdViaTemp ver cmd = do
  -- "images" is a misnomer, it should be "files".
  (tmp, h) <- io (openBinaryTempFile (images ver) "scaled")
  let cmd' = cmd ++ " > " ++ tmp
  io (makeReadableAndClose h)
  -- io (hClose h)
  (code, _out, _err) <- io (readCreateProcessWithExitCode' (shell cmd') P.empty)
  case code of
    ExitSuccess -> installFile tmp
    ExitFailure _ -> myerror $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code
    where
      installFile :: FilePath -> ErrorWithIO File
      installFile tmp = fileFromFile ver tmp `catch` (\ e -> fail $ "fileFromCmdViaTemp - install failed: " ++ show e)

-- |Turn the bytes in a ByteString into a File.  This is an IO operation
-- because it saves the data into the local cache.  We use writeFileReadable
-- because the files we create need to be read remotely by our backup program.
fileFromBytes :: ImageCacheTop        	-- ^ The home directory of the cache
              -> P.ByteString	-- ^ The bytes to store as the file's contents
              -> ErrorWithIO File
fileFromBytes ver bytes =
    do exists <- io $ doesFileExist path
       case exists of
         True -> return file
         False -> io (writeFileReadable path bytes) >> return file
    where
      path = fileCachePath ver file
      file = File { fileSource = Nothing
                  , fileChksum = md5' bytes
                  , fileMessages = [] }

-- | Make sure a file is correctly installed in the cache, and if it
-- isn't install it.
cacheFile :: ImageCacheTop                   -- ^ The home directory of the cache
          -> File                     -- ^ The file to verify
          -> P.ByteString             -- ^ Expected contents
          -> ErrorWithIO File
cacheFile home file bytes =
    (loadBytes home file >>= checkBytes) `catch` reCache
    where
      path = fileCachePath home file
      checkBytes loaded = if loaded == bytes
                          then logExceptionM "Appraisal.File.cacheFile" $ fail "cacheFile - Checksum error"
                          else return file
      reCache :: IOError -> ErrorWithIO File
      reCache _e = io (writeFileReadable path bytes) >> return file

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: File -> Maybe URI
fileURI (File {fileSource = Just (TheURI uri)}) = maybe (parseRelativeReference uri) Just (parseURI uri)
fileURI _ = Nothing

-- |A URI for the locally cached version of the file.
fileCacheURI :: URI		-- ^ The URI of the cache home directory
             -> File		-- ^ The file whose URI should be returned
             -> URI
fileCacheURI cacheDirectoryURI file =
    cacheDirectoryURI {uriPath = uriPath cacheDirectoryURI <++> fileChksum file}

-- |The full path name for the local cache of the file.
fileCachePath :: ImageCacheTop         -- ^ The home directory of the cache
              -> File		-- ^ The file whose path should be returned
              -> FilePath
fileCachePath ver file = images ver <++> fileChksum file

-- |Read and return the contents of the file from the cache.
loadBytes :: ImageCacheTop  		-- ^ The home directory of the cache
          -> File		-- ^ The file whose bytes should be loaded
          -> ErrorWithIO P.ByteString
loadBytes home file =
    do bytes <- io (P.readFile (fileCachePath home file))
       case md5' bytes == fileChksum file of
         True -> return bytes
         False -> logExceptionM "Appraisal.File.loadBytes" $ fail $ "Checksum mismatch: expected " ++ show (fileChksum file) ++ ", file contains " ++ show (md5' bytes)

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
