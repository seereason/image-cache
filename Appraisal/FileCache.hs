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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Appraisal.FileCache
    ( FileError(..)
    , logFileError
      -- * Monad and Class
    , FileCacheTop(..)
    , HasFileCacheTop(fileCacheTop)
    -- , MonadFileCacheIO
    , ensureFileCacheTop
    , FileCacheT
    , runFileCacheT
    , runFileCacheTop
    , runFileCache
    , mapFileCacheT
    , liftIOToF
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
    , logException
    , logAndThrow
    ) where

import Appraisal.FileCacheT
import Appraisal.Serialize (deriveSerialize)
import Appraisal.Utils.ErrorWithIO (readCreateProcessWithExitCode')
import Control.Exception (try)
import Control.Lens (makeLenses, over, set, view)
import Control.Monad ( unless )
import "mtl" Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadIO(..))
import qualified Data.ByteString.Lazy.Char8 as Lazy ( fromChunks )
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Digest.Pure.MD5 ( md5 )
import Data.Generics ( Data(..), Typeable )
--import Data.Map ( Map )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, deriveSafeCopy, extension, Migrate(..) )
import Debug.Show (V(V))
import Language.Haskell.TH (ExpQ, Exp, location, pprint, Q)
import qualified Language.Haskell.TH.Lift as TH (deriveLiftMany, lift)
import Network.URI ( URI(..), URIAuth(..), parseRelativeReference, parseURI )
import System.Directory ( copyFile, createDirectoryIfMissing, doesFileExist, getDirectoryContents, renameFile )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>) )
import System.FilePath.Extra ( writeFileReadable, makeReadableAndClose )
import System.IO ( openBinaryTempFile )
import System.Log.Logger ( logM, Priority(DEBUG, ERROR) )
import System.Process (proc, shell, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode)
#if !MIN_VERSION_process(1,4,3)
import System.Process.ListLike (showCreateProcessForUser)
#endif
import System.Unix.FilePath ( (<++>) )
import Test.QuickCheck ( Arbitrary(..), oneof )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

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
    forall st m a. (MonadIO m)
    => (P.ByteString -> FileCacheT st FileError m a)
    -> (a -> String)
    -> P.ByteString
    -> FileCacheT st FileError m (File, a)
fileFromBytes byteStringInfo toFileExt bytes =
      do a <- byteStringInfo bytes
         let file = File { _fileSource = Nothing
                         , _fileChksum = md5' bytes
                         , _fileMessages = []
                         , _fileExt = toFileExt a }
         path <- fileCachePathIO file
         exists <- liftIOToF $ doesFileExist path
         unless exists (liftIOToF (writeFileReadable path bytes))
         return (file, a)

-- |Read the contents of a local path into a File.
fileFromPath ::
    forall st m a. (MonadIO m)
    => (P.ByteString -> FileCacheT st FileError m a)
    -> (a -> String)
    -> FilePath
    -> FileCacheT st FileError m (File, a)
fileFromPath byteStringInfo toFileExt path = do
  bytes <- liftIOToF $ P.readFile path
  (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
  return (set fileSource (Just (ThePath path)) file, a)

-- | A shell command whose output becomes the contents of the file.
fileFromCmd ::
    forall st m a. MonadIO m
    => (P.ByteString -> FileCacheT st FileError m a)
    -> (a -> String)
    -> String
    -> FileCacheT st FileError m (File, a)
fileFromCmd byteStringInfo toFileExt cmd = do
  (code, bytes, _err) <- liftIOToF (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
           return $ (set fileSource (Just (ThePath cmd)) file, a)
    ExitFailure _ ->
        throwError (FunctionName "fileFromCmd" (Command (shell cmd) code))

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI ::
    forall st m a. (MonadIO m{-, MonadCatch m-})
    => (P.ByteString -> FileCacheT st FileError m a)
    -> (a -> String)
    -> String
    -> FileCacheT st FileError m (File, a)
fileFromURI byteStringInfo toFileExt uri =
    do let args = ["-s", uri]
           cmd = (proc "curl" args)
       (code, bytes, _err) <- liftIOToF $ readCreateProcessWithExitCode' cmd P.empty
       case code of
         ExitSuccess ->
             do (file, bytes') <- fileFromBytes byteStringInfo toFileExt bytes
                return (set fileSource (Just (TheURI uri)) file, bytes')
         _ -> throwError (FunctionName "fileFromURI" (Command cmd code))

-- | Build a file from the output of a command.  This uses a temporary
-- file to store the contents of the command while we checksum it.  This
-- is to avoid reading the file contents into a Haskell ByteString, which
-- may be slower than using a unix pipeline.  Though it shouldn't be.
fileFromCmdViaTemp ::
    forall st m. MonadIO m
    => String
    -> String
    -> FileCacheT st FileError m File
fileFromCmdViaTemp ext exe = do
  FileCacheTop dir <- fileCacheTop
  (tmp, h) <- liftIOToF $ openBinaryTempFile dir "scaled"
  let cmd = shell (exe ++ " > " ++ tmp)
  liftIOToF $ makeReadableAndClose h
  -- io (hClose h)
  (code, _out, _err) <- liftIOToF (readCreateProcessWithExitCode' cmd P.empty)
  case code of
    ExitSuccess -> installFile tmp
    ExitFailure _ -> throwError (FunctionName "fileFromCmdViaTemp" (Command cmd code))
    where
      installFile :: FilePath -> FileCacheT st FileError m File
      installFile tmp = fileFromPathViaRename ext tmp `catchError` (throwError . FunctionName "fileFromCmdViaTemp" . Description "install failed")

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaRename ::
    forall st m. (MonadIO m)
    => String
    -> FilePath
    -> FileCacheT st FileError m File
fileFromPathViaRename ext path = do
  let cmd = shell ("md5sum < " ++ showCommandForUser path [])
  result <- liftIOToF (try (readCreateProcessWithExitCode cmd ""))
  case result of
    Right (ExitSuccess, out, _err) -> do
      let file = File { _fileSource = Just (ThePath path)
                      , _fileChksum = take 32 out
                      , _fileMessages = []
                      , _fileExt = ext }
      dest <- fileCachePathIO file
      liftIOToF $ do
        logM "fileFromPathViaRename" DEBUG ("renameFile " <> path <> " " <> dest)
        renameFile path dest
      return file
    Right (code, _, _) -> throwError (Command cmd code :: FileError)
    Left e -> throwError (IOException e)

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaCopy ::
    forall st m. MonadIO m
    => String
    -> FilePath
    -> FileCacheT st FileError m File
fileFromPathViaCopy ext path = do
  cksum <- (\(_, out, _) -> take 32 out) <$> liftIOToF (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
  let file = File { _fileSource = Just (ThePath path)
                  , _fileChksum = cksum
                  , _fileMessages = []
                  , _fileExt = ext }
  dest <- fileCachePathIO file
  liftIOToF $ do
    logM "fileFromPathViaCopy" DEBUG ("copyFile " <> path <> " " <> dest)
    copyFile path dest
  return file

-- | Given a file and a ByteString containing the expected contents,
-- verify the contents.  If it isn't installed or isn't correct,
-- (re)install it.
cacheFile :: MonadIO m => File -> P.ByteString -> FileCacheT st FileError m File
cacheFile file bytes = do
  path <- fileCachePath file
  (loadBytes file >>= checkBytes) `catchError`
    (\ (_e :: FileError) -> liftIOToF (writeFileReadable path bytes) >> return file)
    where
      checkBytes loaded = if loaded == bytes
                          then throwError (FunctionName "cacheFile" (Failure "Checksum error"))
                          else return file

-- | Read and return the contents of the file from the cache as a ByteString.
loadBytes :: MonadIO m => File -> FileCacheT st FileError m P.ByteString
loadBytes file =
    do path <- fileCachePath file
       bytes <- liftIOToF (P.readFile path)
       case md5' bytes == view fileChksum file of
         True -> return bytes
         False -> do
           let msg = "Checksum mismatch: expected " ++ show (view fileChksum file) ++ ", file contains " ++ show (md5' bytes)
           liftIOToF (logM "FileCache.hs" ERROR msg)
           throwError (FunctionName "loadBytes" (Failure msg))

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File(" <> show (cksum <> ext) <> ")")

-- | The full path name for the local cache of the file.
fileCachePath :: HasFileCacheTop m => File -> m FilePath
fileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> filePath file

oldFileCachePath :: HasFileCacheTop m => File -> m FilePath
oldFileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> view fileChksum file

fileCacheDir :: HasFileCacheTop m => File -> m FilePath
fileCacheDir file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> fileDir file

fileCachePathIO :: MonadIO m => File -> FileCacheT st FileError m FilePath
fileCachePathIO file = do
  dir <- fileCacheDir file
  liftIOToF $ createDirectoryIfMissing True dir
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

__LOC__ :: Q Exp
__LOC__ = TH.lift =<< location

logAndThrow :: MonadIO m => FileError -> FileCacheT st FileError m b
logAndThrow e = liftIOToF (logM "logAndThrow" ERROR (show e)) >> throwError e

logException :: ExpQ
logException =
    [| \ action -> let f e = liftIOToF (logM "logException" ERROR ("Logging exception: " ++ (pprint $__LOC__) ++ " -> " ++ show (V e))) >> throwError e in
                   action `catchError` f |]

-- | Scan all the file cache directories for files without using
-- the database.
allFiles :: (MonadIO m, MonadReader (st, FilePath) (FileCacheT st FileError m)) => FileCacheT st FileError m [FilePath]
allFiles = do
  FileCacheTop top <- fileCacheTop
  dirs <- liftIOToF $ listDirectory top
  concat <$> mapM (\dir -> let dir' = top </> dir in
                           fmap (dir' </>) <$> liftIOToF (listDirectory dir')) dirs

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."

$(deriveSafeCopy 1 'base ''FileSource)
$(deriveSafeCopy 0 'base ''URI)
$(deriveSafeCopy 0 'base ''URIAuth)
$(deriveSafeCopy 2 'extension ''File)
$(deriveSafeCopy 1 'base ''File_1)
$(TH.deriveLiftMany [
   ''FileSource,
   ''URI,
   ''URIAuth,
   ''File])
$(deriveSerialize [t|FileSource|])
$(deriveSerialize [t|File|])
$(deriveSerialize [t|File_1|])
