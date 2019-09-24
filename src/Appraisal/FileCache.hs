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
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass, DeriveLift #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints -fno-warn-orphans #-}

module Appraisal.FileCache
    (
    -- * Types
      Checksum
    , FileSource(..), fileSource, fileChksum, fileMessages, fileExt
    , File(..)
    , fileURI
    , fileDir
    , addMessage
    , md5'
    -- * Create Files
#if !__GHCJS__
    , fileCacheURI
    , fileFromBytes
    , fileFromURI               -- was importFile
    , fileFromPath
    , fileFromPathViaRename
    , fileFromPathViaCopy
    , fileFromCmd
    , fileFromCmdViaTemp
    , cacheFile
    -- * Query Files
    , loadBytesSafe
    , loadBytesUnsafe
    , fileCachePathIO
    , allFiles
    , filePath
    , fileCachePath
    , oldFileCachePath
    , fileCacheDir
#endif
    ) where

import Control.Lens (makeLenses, over, view)
import Control.Lens.Path (makePathInstances, HOP(FIELDS))
--import Control.Monad ( unless )
--import "mtl" Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
--import Control.Monad.Reader (MonadReader)
--import Control.Monad.Trans (MonadIO(..))
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
import Data.SafeCopy ({-base, deriveSafeCopy,-} SafeCopy(version), safeGet, safePut)
import Data.Serialize (Serialize(get, put))
--import Data.Text (pack, unpack)
--import Extra.Except
import GHC.Generics (Generic)
--import Language.Haskell.TH.Lift as TH (Lift)
import Network.URI ( URI(..), parseRelativeReference, parseURI )
--import System.FilePath ( (</>) )
--import System.Log.Logger ( logM, Priority(DEBUG, ERROR) )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

#if !__GHCJS__
import Appraisal.FileCacheT (FileCacheT, FileCacheTop(FileCacheTop), HasFileCacheTop(fileCacheTop))
import Control.Lens (makeLenses, set)
import Control.Monad ( unless )
import "mtl" Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Data.Text (pack, unpack)
import Extra.Except
import Language.Haskell.TH.Lift as TH (Lift)
import System.FilePath ( (</>) )
import System.Log.Logger ( logM, Priority(DEBUG, ERROR) )
import Appraisal.FileError (CommandInfo(..), FileError(..), HasFileError(fromFileError))
import Appraisal.Utils.ErrorWithIO (readCreateProcessWithExitCode')
import System.Directory ( copyFile, createDirectoryIfMissing, doesFileExist, getDirectoryContents, renameFile )
import System.Exit ( ExitCode(..) )
import System.FilePath.Extra ( writeFileReadable, makeReadableAndClose )
import System.IO ( openBinaryTempFile )
import System.Process (proc, shell, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode)
import System.Unix.FilePath ( (<++>) )
import Test.QuickCheck ( Arbitrary(..), oneof )
#endif

-- |The original source if the file is saved, in case
-- the cache needs to be reconstructed.  However, we don't
-- store the original ByteString if that is all we began
-- with, that would be redundant and wasteful.
data FileSource
    = TheURI String
    | ThePath FilePath
    deriving (Generic, Eq, Ord)

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = String

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           , _fileExt :: String                  -- ^ Name is formed by appending this to checksum
           } deriving (Generic, Eq, Ord)

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File(" <> show (cksum <> ext) <> ")")

$(makeLenses ''File)

instance SafeCopy FileSource where version = 1
instance SafeCopy File where version = 2
instance Serialize FileSource where get = safeGet; put = safePut
instance Serialize File where get = safeGet; put = safePut

#if !__GHCJS__
deriving instance Lift FileSource
deriving instance Lift File

-- | Build a URI for the locally cached version of the file given the
-- uri of the cache home directory.
fileCacheURI :: URI -> File -> URI
fileCacheURI cacheDirectoryURI file =
    cacheDirectoryURI {uriPath = uriPath cacheDirectoryURI <++> view fileChksum file}
#endif

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: File -> Maybe URI
fileURI file = case view fileSource file of
                 Just (TheURI uri) -> maybe (parseRelativeReference uri) Just (parseURI uri)
                 _ -> Nothing

-- |Add a message to the file message list.
addMessage :: String -> File -> File
addMessage message file = over fileMessages (++ [message]) file

md5' :: P.ByteString -> String
#ifdef LAZYIMAGES
md5' = show . md5
#else
md5' = show . md5 . Lazy.fromChunks . (: [])
#endif

#if !__GHCJS__
-- | Turn the bytes in a ByteString into a File.  This is an IO
-- operation because it saves the data into the local cache.  We
-- use writeFileReadable because the files we create need to be
-- read remotely by our backup program.
fileFromBytes ::
    forall e m a. (MonadIOError e m, HasFileCacheTop m)
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
         exists <- liftIOError $ doesFileExist path
         unless exists (liftIOError (writeFileReadable path bytes))
         return (file, a)

-- |Read the contents of a local path into a File.
fileFromPath ::
    forall e m a. (MonadIOError e m, HasFileCacheTop m)
    => (P.ByteString -> m a)
    -> (a -> String)
    -> FilePath
    -> m (File, a)
fileFromPath byteStringInfo toFileExt path = do
  bytes <- liftIOError $ P.readFile path
  (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
  return (set fileSource (Just (ThePath path)) file, a)

-- | A shell command whose output becomes the contents of the file.
fileFromCmd ::
    forall e m a. (MonadIOError e m, HasFileCacheTop m, HasFileError e)
    => (P.ByteString -> m a)
    -> (a -> String)
    -> String
    -> m (File, a)
fileFromCmd byteStringInfo toFileExt cmd = do
  (code, bytes, _err) <- liftIOError (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
           return $ (set fileSource (Just (ThePath cmd)) file, a)
    ExitFailure _ ->
        throwError $ fromFileError $ CommandFailure (FunctionName "fileFromCmd" (Command (pack (show (shell cmd))) (pack (show code))))

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI ::
    forall e m a. (MonadIOError e m, HasFileCacheTop m, HasFileError e)
    => (P.ByteString -> m a)
    -> (a -> String)
    -> String
    -> m (File, a)
fileFromURI byteStringInfo toFileExt uri =
    do let args = ["-s", uri]
           cmd = (proc "curl" args)
       (code, bytes, _err) <- liftIOError $ readCreateProcessWithExitCode' cmd P.empty
       case code of
         ExitSuccess ->
             do (file, bytes') <- fileFromBytes byteStringInfo toFileExt bytes
                return (set fileSource (Just (TheURI uri)) file, bytes')
         _ -> throwError $ fromFileError $ CommandFailure (FunctionName "fileFromURI" (Command (pack (show cmd)) (pack (show code))))

-- | Build a file from the output of a command.  This uses a temporary
-- file to store the contents of the command while we checksum it.  This
-- is to avoid reading the file contents into a Haskell ByteString, which
-- may be slower than using a unix pipeline.  Though it shouldn't be.
fileFromCmdViaTemp ::
    forall e m. (MonadIOError e m, HasFileCacheTop m, HasFileError e)
    => String
    -> String
    -> m File
fileFromCmdViaTemp ext exe = do
  FileCacheTop dir <- fileCacheTop
  (tmp, h) <- liftIOError $ openBinaryTempFile dir "scaled"
  let cmd = shell (exe ++ " > " ++ tmp)
  liftIOError $ makeReadableAndClose h
  -- io (hClose h)
  (code, _out, _err) <- liftIOError (readCreateProcessWithExitCode' cmd P.empty)
  case code of
    ExitSuccess -> installFile tmp
    ExitFailure _ -> throwError $ fromFileError $ CommandFailure $ FunctionName "fileFromCmdViaTemp" $ Command (pack (show cmd)) (pack (show code))
    where
      installFile :: FilePath -> m File
      installFile tmp = fileFromPathViaRename (fromFileError . CommandFailure . FunctionName "fileFromCmdViaTemp" . Description "install failed") ext tmp

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaRename ::
    forall e m. (HasFileError e, MonadIOError e m, HasFileCacheTop m)
    => (CommandInfo -> FileError) -- ^ Use this to customize exception thrown here
    -> String
    -> FilePath
    -> m File
fileFromPathViaRename err ext path = do
  let cmd = shell ("md5sum < " ++ showCommandForUser path [])
  result <- liftIOError (readCreateProcessWithExitCode cmd "")
  case result of
    (ExitSuccess, out, _err) -> do
      let file = File { _fileSource = Just (ThePath path)
                      , _fileChksum = take 32 (unpack out)
                      , _fileMessages = []
                      , _fileExt = ext }
      dest <- fileCachePathIO file
      liftIOError $ do
        logM "Appraisal.FileCache" DEBUG ("fileFromPathViaRename - renameFile " <> path <> " " <> dest)
        renameFile path dest
      return file
    (code, _, _) -> throwError $ fromFileError $ err (Command (pack (show cmd)) (pack (show code)))

-- | Move a file into the file cache and incorporate it into a File.
fileFromPathViaCopy ::
    forall e m. (MonadIOError e m, HasFileCacheTop m)
    => String
    -> FilePath
    -> m File
fileFromPathViaCopy ext path = do
  cksum <- (\(_, out, _) -> take 32 out) <$> liftIOError (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
  let file = File { _fileSource = Just (ThePath path)
                  , _fileChksum = cksum
                  , _fileMessages = []
                  , _fileExt = ext }
  dest <- fileCachePathIO file
  liftIOError $ logM "Appraisal.FileCache" DEBUG ("fileFromPathViaCopy - copyFile " <> path <> " " <> dest)
  liftIOError $ copyFile path dest
  return file

-- | Given a file and a ByteString containing the expected contents,
-- verify the contents.  If it isn't installed or isn't correct,
-- (re)install it.
cacheFile ::
    (MonadIOError e m, HasFileError e, HasFileCacheTop m)
    => File -> P.ByteString -> m File
cacheFile file bytes = do
  path <- fileCachePath file
  (loadBytesUnsafe file >>= checkBytes) `catchError`
    (\_e -> liftIOError (writeFileReadable path bytes) >> return file)
    where
      checkBytes loaded = if loaded == bytes
                          -- then throwError (fromFileError (FunctionName "cacheFile" (SomeFileError "Checksum error")))
                          then throwError $ fromFileError CacheDamage
                          else return file

-- | Read and return the contents of the file from the cache as a ByteString.
loadBytesSafe ::
    (HasFileError e, MonadIOError e m, HasFileCacheTop m)
    => File -> m P.ByteString
loadBytesSafe file =
    do path <- fileCachePath file
       bytes <- readFileBytes path
       case md5' bytes == view fileChksum file of
         True -> return bytes
         False -> do
           let msg = "Checksum mismatch: expected " ++ show (view fileChksum file) ++ ", file contains " ++ show (md5' bytes)
           liftIOError (logM "Appraisal.FileCache" ERROR msg)
           throwError $ fromFileError CacheDamage

-- | Load an image file without verifying its checksum
loadBytesUnsafe :: (HasFileError e, MonadIOError e m, HasFileCacheTop m) => File -> m P.ByteString
loadBytesUnsafe file = fileCachePath file >>= readFileBytes

readFileBytes :: (MonadIOError e m) => FilePath -> m P.ByteString
readFileBytes path = liftIOError (P.readFile path)

fileCachePathIO :: (MonadIOError e m, HasFileCacheTop m) => File -> m FilePath
fileCachePathIO file = do
  dir <- fileCacheDir file
  liftIOError $ createDirectoryIfMissing True dir
  fileCachePath file

listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
  (filter f) <$> (getDirectoryContents path)
  where f filename = filename /= "." && filename /= ".."

-- | Scan all the file cache directories for files without using
-- the database.
allFiles :: (MonadIOError e m, Monoid w) => FileCacheT st w s m [FilePath]
allFiles = do
  FileCacheTop top <- fileCacheTop
  dirs <- liftIOError $ listDirectory top
  concat <$> mapM (\dir -> let dir' = top </> dir in
                           fmap (dir' </>) <$> liftIOError (listDirectory dir')) dirs

-- | The full path name for the local cache of the file.
fileCachePath :: HasFileCacheTop m => File -> m FilePath
fileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> filePath file

oldFileCachePath :: HasFileCacheTop m => File -> m FilePath
oldFileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> view fileChksum file

fileCacheDir :: HasFileCacheTop m => File -> m FilePath
fileCacheDir file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> fileDir file

filePath :: File -> FilePath
filePath file = fileDir file <++> view fileChksum file <> view fileExt file
#endif

fileDir :: File -> FilePath
fileDir file = take 2 (view fileChksum file)

#if !__GHCJS__
instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure [] <*> arbitrary

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]
#endif

deriving instance Show FileSource
deriving instance Read FileSource
deriving instance Data FileSource
deriving instance Typeable FileSource

deriving instance Show File
deriving instance Read File
deriving instance Data File
deriving instance Typeable File

$(makePathInstances [FIELDS] ''File)
$(makePathInstances [FIELDS] ''FileSource)
