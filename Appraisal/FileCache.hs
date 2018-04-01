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
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Appraisal.FileCache
    ( -- * Monad and Class
      HasFileCacheTop(fileCacheTop)
    , MonadFileCacheIO(ensureFileCacheTop)
    -- Types
    , Checksum
    , FileSource(..), fileSource, fileChksum, fileMessages, fileExt
    , File(..)
    , File_1(..)
    , fileURI
    , fileCacheURI
    , addMessage
    , md5'
    -- * Create Files
    , fileFromBytes
    , fileFromURI               -- was importFile
    , fileFromPath
    , fileFromPathViaRename
    , fileFromPathViaCopy
    , fileFromCmd
    , fileFromCmdViaTemp
    , cacheFile
    -- * Query Files
    , loadBytes
    , fileDir
    , filePath
    , fileCachePath
    , oldFileCachePath
    , fileCacheDir
    , fileCachePathIO
    -- * Utility
    , allFiles
    ) where

import Appraisal.Utils.ErrorWithIO ( logAndFail, logException, readCreateProcessWithExitCode' )
import Control.Exception ( IOException )
import Control.Lens ( makeLenses, over, set, view )
import Control.Monad ( unless )
import Control.Monad.Catch ( MonadCatch )
import Control.Monad.Except ( MonadError(..) )
import Control.Monad.Trans ( MonadIO(..) )
import qualified Data.ByteString.Lazy.Char8 as Lazy ( fromChunks )
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Digest.Pure.MD5 ( md5 )
import Data.Generics ( Data(..), Typeable )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, deriveSafeCopy, extension, Migrate(..) )
import Data.THUnify.Serialize ( deriveSerialize )
import Language.Haskell.TH.Lift ( deriveLiftMany )
import Network.URI ( URI(..), URIAuth(..), parseRelativeReference, parseURI )
import System.Directory ( copyFile, createDirectoryIfMissing, doesFileExist, getDirectoryContents, renameFile )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>) )
import System.FilePath.Extra ( writeFileReadable, makeReadableAndClose )
import System.IO ( openBinaryTempFile )
import System.Log.Logger ( logM, Priority(DEBUG) )
import System.Process ( proc, shell, showCommandForUser )
import System.Process.ListLike ( readCreateProcessWithExitCode )
import System.Unix.FilePath ( (<++>) )
import Test.QuickCheck ( Arbitrary(..), oneof )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- | Class of monads with a 'FilePath' value containing the top
-- of a 'FileCache'.
-- paths do not.  So MonadIO is not a superclass here.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FilePath

-- | This is the class for operations that do require IO.  Almost all
-- operations require IO, but you can build paths into the cache
-- without it.
class (HasFileCacheTop m, MonadCatch m, MonadError e m, MonadIO m) => MonadFileCacheIO e m where
    ensureFileCacheTop :: m () -- Create the fileCacheTop directory if necessary

-- | Probably the only meaningful instance of MonadFileCacheIO.
instance (HasFileCacheTop m, MonadCatch m, MonadError IOException m, MonadIO m) => MonadFileCacheIO IOException m where
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
           , _fileExt :: String                  -- ^ Name is formed by appending this to checksum
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Migrate File where
    type MigrateFrom File = File_1
    migrate (File_1 s c m) = File s c m ""

data File_1 = File_1 (Maybe FileSource) Checksum [String] deriving (Show, Read, Eq, Ord, Data, Typeable)

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
    MonadFileCacheIO IOException m
    => (P.ByteString -> m a)
    -> (a -> String)
    -> P.ByteString
    -> m (File, a)
fileFromBytes byteStringInfo toFileExt bytes =
      do a <- byteStringInfo bytes
         let file = File { _fileSource = Nothing
                         , _fileChksum = md5' bytes
                         , _fileMessages = []
                         , _fileExt = toFileExt a }
         path <- fileCachePathIO file
         exists <- liftIO $ doesFileExist path
         unless exists (liftIO (writeFileReadable path bytes))
         return (file, a)

-- |Read the contents of a local path into a File.
fileFromPath ::
    MonadFileCacheIO IOException m
    => (P.ByteString -> m a)
    -> (a -> String)
    -> FilePath
    -> m (File, a)
fileFromPath byteStringInfo toFileExt path = do
  bytes <- liftIO $ P.readFile path
  (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
  return (set fileSource (Just (ThePath path)) file, a)

-- | A shell command whose output becomes the contents of the file.
fileFromCmd ::
    MonadFileCacheIO IOException m
    => (P.ByteString -> m a)
    -> (a -> String)
    -> String
    -> m (File, a)
fileFromCmd byteStringInfo toFileExt cmd = do
  (code, bytes, _err) <- liftIO (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
           return $ (set fileSource (Just (ThePath cmd)) file, a)
    ExitFailure _ -> fail $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI ::
    MonadFileCacheIO IOException m
    => (P.ByteString -> m a)
    -> (a -> String)
    -> String
    -> m (File, a)
fileFromURI byteStringInfo toFileExt uri =
    do let cmd = "curl"
           args = ["-s", uri]
       (code, bytes, _err) <- liftIO $ readCreateProcessWithExitCode' (proc cmd args) P.empty
       case code of
         ExitSuccess ->
             do (file, bytes') <- fileFromBytes byteStringInfo toFileExt bytes
                return (set fileSource (Just (TheURI uri)) file, bytes')
         _ -> $logException $ fail $ "fileFromURI Failure: " ++ cmd ++ " -> " ++ show code

-- | Build a file from the output of a command.  This uses a temporary
-- file to store the contents of the command while we checksum it.  This
-- is to avoid reading the file contents into a Haskell ByteString, which
-- may be slower than using a unix pipeline.  Though it shouldn't be.
fileFromCmdViaTemp ::
    forall m. MonadFileCacheIO IOException m
    => String
    -> String
    -> m File
fileFromCmdViaTemp ext cmd = do
  dir <- fileCacheTop
  (tmp, h) <- liftIO (openBinaryTempFile dir "scaled")
  let cmd' = cmd ++ " > " ++ tmp
  liftIO (makeReadableAndClose h)
  -- io (hClose h)
  (code, _out, _err) <- liftIO (readCreateProcessWithExitCode' (shell cmd') P.empty)
  case code of
    ExitSuccess -> installFile tmp
    ExitFailure _ -> fail $ "Failure building file:\n " ++ show cmd ++ " -> " ++ show code
    where
      installFile :: FilePath -> m File
      installFile tmp = fileFromPathViaRename ext tmp `catchError` (\ e -> throwError (userError $ "fileFromCmdViaTemp - install failed: " ++ show e))

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaRename :: MonadFileCacheIO IOException m => String -> FilePath -> m File
fileFromPathViaRename ext path = do
  cksum <- (\(_, out, _) -> take 32 out) <$> liftIO (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
  let file = File { _fileSource = Just (ThePath path)
                  , _fileChksum = cksum
                  , _fileMessages = []
                  , _fileExt = ext }
  dest <- fileCachePathIO file
  liftIO (logM "fileFromPathViaRename" DEBUG ("renameFile " <> path <> " " <> dest) >>
          renameFile path dest)
  return file

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaCopy :: MonadFileCacheIO IOException m => String -> FilePath -> m File
fileFromPathViaCopy ext path = do
  cksum <- (\(_, out, _) -> take 32 out) <$> liftIO (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
  let file = File { _fileSource = Just (ThePath path)
                  , _fileChksum = cksum
                  , _fileMessages = []
                  , _fileExt = ext }
  dest <- fileCachePathIO file
  liftIO (-- logM "fileFromPathViaCopy" DEBUG ("copyFile " <> path <> " " <> dest) >>
          copyFile path dest)
  return file

-- | Given a file and a ByteString containing the expected contents,
-- verify the contents.  If it isn't installed or isn't correct,
-- (re)install it.
cacheFile :: MonadFileCacheIO IOException m => File -> P.ByteString -> m File
cacheFile file bytes = do
  path <- fileCachePath file
  (loadBytes file >>= checkBytes) `catchError` (\ _e -> liftIO (writeFileReadable path bytes) >> return file)
    where
      checkBytes loaded = if loaded == bytes
                          then $logAndFail "cacheFile - Checksum error"
                          else return file

-- | Read and return the contents of the file from the cache as a ByteString.
loadBytes :: MonadFileCacheIO IOException m => File -> m P.ByteString
loadBytes file =
    do path <- fileCachePath file
       bytes <- liftIO (P.readFile path)
       case md5' bytes == view fileChksum file of
         True -> return bytes
         False -> $logAndFail $ "Checksum mismatch: expected " ++ show (view fileChksum file) ++ ", file contains " ++ show (md5' bytes)

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File(" <> show (cksum <> ext) <> ")")

-- | The full path name for the local cache of the file.
fileCachePath :: HasFileCacheTop m => File -> m FilePath
fileCachePath file = fileCacheTop >>= \ver -> return $ ver <++> filePath file

oldFileCachePath :: HasFileCacheTop m => File -> m FilePath
oldFileCachePath file = fileCacheTop >>= \ver -> return $ ver <++> view fileChksum file

fileCacheDir :: HasFileCacheTop m => File -> m FilePath
fileCacheDir file = fileCacheTop >>= \ver -> return $ ver <++> fileDir file

fileCachePathIO :: MonadFileCacheIO IOException m => File -> m FilePath
fileCachePathIO file = do
  dir <- fileCacheDir file
  liftIO $ createDirectoryIfMissing True dir
  fileCachePath file

filePath :: File -> FilePath
filePath file = fileDir file <++> view fileChksum file <> view fileExt file

fileDir :: File -> FilePath
fileDir file = take 2 (view fileChksum file)

instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure [] <*> arbitrary

instance Arbitrary File_1 where
    arbitrary = File_1 <$> arbitrary <*> arbitrary <*> pure []

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]

-- | Scan all the file cache directories for files without using
-- the database.
allFiles :: MonadFileCacheIO e m => m [FilePath]
allFiles = do
  top <- fileCacheTop
  dirs <- liftIO $ liftIO (listDirectory top)
  concat <$> mapM (\dir -> let dir' = top </> dir in
                           fmap (dir' </>) <$> liftIO (listDirectory dir')) dirs

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."

$(deriveSafeCopy 1 'base ''FileSource)
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
$(deriveSafeCopy 2 'extension ''File)
$(deriveSafeCopy 1 'base ''File_1)
$(deriveLiftMany [
   ''FileSource,
   ''URI,
   ''URIAuth,
   ''File])
$(deriveSerialize [t|FileSource|])
$(deriveSerialize [t|File|])
$(deriveSerialize [t|File_1|])
