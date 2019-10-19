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
{-# LANGUAGE DeriveFunctor, DeriveGeneric, DeriveAnyClass #-}
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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints -fno-warn-orphans #-}

module Data.FileCache.FileIO
    ( fileCacheURI
    , fileFromBytes
    , fileFromURI               -- was importFile
    , fileFromPath
    , fileFromPathViaRename
    , fileFromPathViaCopy
    , fileFromCmd
    , fileFromCmdViaTemp
    -- * Query Files
    , loadBytesSafe
    , loadBytesUnsafe
    , fileCachePathIO
    , allFiles
    , fileCachePath
    , oldFileCachePath
    , fileCacheDir
    ) where

import Control.Lens (set)
import Control.Monad ( unless )
import Control.Monad.Except (catchError, throwError)
import Control.Monad.Reader (ReaderT)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.FileCache.File
import Data.FileCache.MonadFileCache
import Data.FileCache.Cache
import Data.FileCache.ErrorWithIO (readCreateProcessWithExitCode')
import Data.FileCache.FileError (CommandInfo(..), FileError(..), HasFileError, fromFileError)
import Data.Generics.Product (field)
import Data.Monoid ( (<>) )
import Data.Text as T (pack, take, Text, unpack)
import Extra.Except (liftIOError, MonadIOError)
import Network.URI (URI(..))
import System.Directory ( copyFile, createDirectoryIfMissing, doesFileExist, getDirectoryContents, renameFile )
import System.Exit ( ExitCode(..) )
import System.FilePath (makeRelative, (</>))
import System.FilePath.Extra ( writeFileReadable, makeReadableAndClose )
import System.IO ( openBinaryTempFile )
import System.Log.Logger ( logM, Priority(DEBUG, ERROR, CRITICAL) )
import System.Process (proc, shell, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode)

(<++>) :: FilePath -> FilePath -> FilePath
a <++> b = a </> (makeRelative "" b)

-- | Build a URI for the locally cached version of the file given the
-- uri of the cache home directory.
fileCacheURI :: URI -> File -> URI
fileCacheURI cacheDirectoryURI file =
    cacheDirectoryURI {uriPath = uriPath cacheDirectoryURI <++> unpack (_fileChksum file)}

-- | Turn the bytes in a ByteString into a File.  This is an IO
-- operation because it saves the data into the local cache.  We
-- use writeFileReadable because the files we create need to be
-- read remotely by our backup program.
fileFromBytes ::
    forall e m a. (MonadIOError e m, HasFileCacheTop m)
    => (P.ByteString -> m a)
    -> (a -> Extension)
    -> P.ByteString
    -> m (File, a)
fileFromBytes byteStringInfo toFileExt bytes =
      do a <- byteStringInfo bytes
         let file = File { _fileSource = Nothing
                         , _fileChksum = pack (md5' bytes)
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
    -> (a -> Extension)
    -> FilePath
    -> m (File, a)
fileFromPath byteStringInfo toFileExt path = do
  bytes <- liftIOError $ P.readFile path
  (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
  return (set (field @"_fileSource") (Just (ThePath path)) file, a)

-- | A shell command whose output becomes the contents of the file.
fileFromCmd ::
    forall e m a. (MonadIOError e m, HasFileError e, HasFileCacheTop m)
    => (P.ByteString -> m a)
    -> (a -> Extension)
    -> String
    -> m (File, a)
fileFromCmd byteStringInfo toFileExt cmd = do
  (code, bytes, _err) <- liftIOError (readCreateProcessWithExitCode' (shell cmd) P.empty)
  case code of
    ExitSuccess ->
        do (file, a) <- fileFromBytes byteStringInfo toFileExt bytes
           return $ (set (field @"_fileSource") (Just (ThePath cmd)) file, a)
    ExitFailure _ ->
        throwError $ (fromFileError :: FileError -> e) $ CommandFailure (FunctionName "fileFromCmd" (Command (pack (show (shell cmd))) (pack (show code))))

-- |Retrieve a URI using curl and turn the resulting data into a File.
fileFromURI ::
    forall e m a. (MonadIOError e m, HasFileError e, HasFileCacheTop m)
    => (P.ByteString -> m a)
    -> (a -> Extension)
    -> String
    -> m (File, a)
fileFromURI byteStringInfo toFileExt uri =
    do let args = ["-s", uri]
           cmd = (proc "curl" args)
       (code, bytes, _err) <- liftIOError $ readCreateProcessWithExitCode' cmd P.empty
       case code of
         ExitSuccess ->
             do (file, bytes') <- fileFromBytes byteStringInfo toFileExt bytes
                return (set (field @"_fileSource") (Just (TheURI uri)) file, bytes')
         _ -> throwError $ fromFileError $ CommandFailure (FunctionName "fileFromURI" (Command (pack (show cmd)) (pack (show code))))

-- | Build a file from the output of a command.  This uses a temporary
-- file to store the contents of the command while we checksum it.  This
-- is to avoid reading the file contents into a Haskell ByteString, which
-- may be slower than using a unix pipeline.  Though it shouldn't be.
fileFromCmdViaTemp ::
    forall e m. (MonadIOError e m, HasFileCacheTop m, HasFileError e)
    => Text
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
    -> Extension
    -> FilePath
    -> m File
fileFromPathViaRename err ext path = do
  let cmd = shell ("md5sum < " ++ showCommandForUser path [])
  result <- liftIOError (readCreateProcessWithExitCode cmd "")
  case result of
    (ExitSuccess, out, _err) -> do
      let file = File { _fileSource = Just (ThePath path)
                      , _fileChksum = T.take 32 out
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
    => Extension
    -> FilePath
    -> m File
fileFromPathViaCopy ext path = do
  cksum <- (\(_, out, _) -> T.take 32 out) <$> liftIOError (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
  let file = File { _fileSource = Just (ThePath path)
                  , _fileChksum = cksum
                  , _fileMessages = []
                  , _fileExt = ext }
  dest <- fileCachePathIO file
  liftIOError $ logM "Appraisal.FileCache" DEBUG ("fileFromPathViaCopy - copyFile " <> path <> " " <> dest)
  liftIOError $ copyFile path dest
  return file

-- | Read and return the contents of the file from the cache as a
-- ByteString.  Verify that the checksum matches the checksum field of
-- the 'File'.
loadBytesSafe ::
    forall e m. (HasFileError e, MonadIOError e m, HasFileCacheTop m)
    => File -> m P.ByteString
loadBytesSafe file =
    do path <- fileCachePath file
       bytes <- readFileBytes @e path
       case pack (md5' bytes) == _fileChksum file of
         True -> return bytes
         -- If the checksum of the file we read from the cache does
         -- not match its checksum field, we've got serious trouble.
         -- We should probably try to read back files when we create
         -- them
         False -> do
           let msg = "Checksum mismatch: expected " ++ show (_fileChksum file) ++ ", file contains " ++ show (md5' bytes)
           liftIOError $ logM "Appraisal.FileCache" CRITICAL msg
           throwError $ fromFileError (CacheDamage ("Checksum problem in " <> pack (show file)))

-- | Load an image file without verifying its checksum
loadBytesUnsafe :: ({-HasFileError e,-} MonadIOError e m, HasFileCacheTop m) => File -> m P.ByteString
loadBytesUnsafe file = fileCachePath file >>= readFileBytes

readFileBytes :: (MonadIOError e m) => FilePath -> m P.ByteString
readFileBytes path = liftIOError $ P.readFile path

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
allFiles :: (MonadIOError e m) => ReaderT (st, FileCacheTop) m [FilePath]
allFiles = do
  FileCacheTop top <- fileCacheTop
  dirs <- liftIOError $ listDirectory top
  concat <$> mapM (\dir -> let dir' = top </> dir in
                           fmap (dir' </>) <$> liftIOError (listDirectory dir')) dirs

-- | The full path name for the local cache of the file.
fileCachePath :: HasFileCacheTop m => File -> m FilePath
fileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> filePath file

oldFileCachePath :: HasFileCacheTop m => File -> m FilePath
oldFileCachePath file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> unpack (_fileChksum file)

fileCacheDir :: HasFileCacheTop m => File -> m FilePath
fileCacheDir file = fileCacheTop >>= \(FileCacheTop ver) -> return $ ver <++> fileDir file
