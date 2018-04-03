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
    ( FileError(..)
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

import Appraisal.Utils.ErrorWithIO (readCreateProcessWithExitCode')
import Control.Exception (ErrorCall(ErrorCallWithLocation), Exception(fromException), IOException, SomeException, throw, try)
import Control.Lens (_2, makeLenses, over, set, view)
import Control.Monad ( unless )
import "mtl" Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (mapReaderT, MonadReader(ask, local), ReaderT, runReaderT)
import Control.Monad.Trans (lift, MonadIO(..), MonadTrans)
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
import Data.String (IsString(fromString))
import Data.THUnify.Serialize ( deriveSerialize )
import Debug.Show (V(V))
import Language.Haskell.TH (ExpQ, Exp, location, Q)
import qualified Language.Haskell.TH.Lift as TH (deriveLiftMany, lift)
import Network.URI ( URI(..), URIAuth(..), parseRelativeReference, parseURI )
import System.Directory ( copyFile, createDirectoryIfMissing, doesFileExist, getDirectoryContents, renameFile )
import System.Exit ( ExitCode(..) )
import System.FilePath ( (</>) )
import System.FilePath.Extra ( writeFileReadable, makeReadableAndClose )
import System.IO ( openBinaryTempFile )
import System.Log.Logger ( logM, Priority(DEBUG, ERROR) )
import System.Process ( CreateProcess, proc, shell, showCommandForUser )
import System.Process.ListLike (readCreateProcessWithExitCode)
#if !MIN_VERSION_process(1,4,3)
import System.Process.ListLike (showCreateProcessForUser)
#endif
import System.Unix.FilePath ( (<++>) )
import Test.QuickCheck ( Arbitrary(..), oneof )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

data FileError
    = IOException IOException
    | ErrorCall ErrorCall
    | Command CreateProcess ExitCode
    | CommandInput P.ByteString FileError
    | CommandOut P.ByteString FileError
    | CommandErr P.ByteString FileError
    | FunctionName String FileError
    | Description String FileError
    | Failure String
    deriving (Show)

instance IsString FileError where
    fromString = Failure

#if !MIN_VERSION_process(1,4,3)
instance Show CreateProcess where
  show = showCreateProcessForUser
#endif

instance Show (V FileError) where show (V x) = show x

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- of a 'FileCache'.
-- paths do not.  So MonadIO is not a superclass here.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FileCacheTop

-- | This is the class for operations that do require IO.  Almost all
-- operations require IO, but you can build paths into the cache
-- without it.
newtype FileCacheT st e m a = FileCacheT {unFileCacheT :: ReaderT (st, FilePath) (ExceptT e m) a} deriving (Monad, Applicative, Functor)

type FileCache st e a = FileCacheT st FileError Identity a

instance MonadTrans (FileCacheT st FileError) where
    lift = FileCacheT . lift . lift

#if 0
instance MonadIO m => MonadIO (FileCacheT st FileError m) where
    liftIO = mapFileCacheT IOException . FileCacheT . liftIO
#else
-- ExceptT :: m (Either e a) -> ExceptT e m a
-- runExcept :: ExceptT e m a -> m (Either e a)

#if !MIN_VERSION_mtl(2,2,2)
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return
#endif

liftIOToF :: MonadIO m => IO a -> FileCacheT st FileError m a
#if 1
liftIOToF io = (FileCacheT . liftIO . runExceptT . withExceptT toFileError . ExceptT . logErrorCall . try $ io) >>= liftEither
    where
      logErrorCall :: IO (Either SomeException a) -> IO (Either SomeException a)
      logErrorCall x =
          x >>= either (\e -> case fromException e :: Maybe ErrorCall of
                                Just (ErrorCallWithLocation msg loc) -> logM "FileCache.hs" ERROR (show loc ++ ": " ++ msg) >> return (Left e)
                                _ -> return (Left e)) (return . Right)
      toFileError :: SomeException -> FileError
      toFileError e =
          maybe (throw e)
                id
                (msum [fmap IOException (fromException e :: Maybe IOException),
                       fmap ErrorCall (fromException e :: Maybe ErrorCall)])

#else
liftIOToF io = (f5 io) >>= liftEither
    where
      f0 :: IO a -> IO (Either SomeException a)
      f0 = try
      f1 :: IO (Either SomeException a) -> ExceptT SomeException IO a
      f1 = ExceptT
      f2 :: ExceptT SomeException IO a -> ExceptT FileError IO a
      f2 = withExceptT (\e -> maybe (throw e)
                                    id
                                    (msum [fmap IOException (fromException e :: Maybe IOException),
                                           fmap ErrorCall (fromException e :: Maybe ErrorCall)]))
      f3 :: ExceptT FileError IO a -> IO (Either FileError a)
      f3 = runExceptT
      f4 :: MonadIO m => IO (Either FileError a) -> m (Either FileError a)
      f4 = liftIO
      f5 :: MonadIO m => IO a -> FileCacheT st FileError m (Either FileError a)
      f5 = FileCacheT . f4 . f3 . f2 . f1 . logErrorCall . f0
      logErrorCall :: IO (Either SomeException a) -> IO (Either SomeException a)
      logErrorCall x =
          x >>= either (\e -> case fromException e :: Maybe ErrorCall of
                                Just (ErrorCallWithLocation msg loc) -> logM "FileCache.hs" ERROR (show loc ++ ": " ++ msg) >> return (Left e)
                                _ -> return (Left e)) (return . Right)
#endif
{-
liftIOToF :: MonadIO m => IO a -> FileCacheT  st FileError m (Either IOException a)
liftIOToF = mapFileCacheT IOException . FileCacheT . liftIO . runExceptT . ExceptT . try
#if 0
liftIOToF = mapFileCacheT IOException . f5
    where
      f1 :: IO a -> ExceptT IOException IO a
      f1 = ExceptT . try
      f3 :: ExceptT IOException IO a -> IO (Either IOException a)
      f3 = runExceptT
      f4 :: MonadIO m => IO (Either IOException a) -> m (Either IOException a)
      f4 = liftIO
      f5 :: MonadIO m => IO a -> FileCacheT st IOException m (Either IOException a)
      f5 = FileCacheT . f4 . f3 . f1
#endif
-}
#endif

instance (Monad m, MonadReader (st, FilePath) (FileCacheT st FileError m)) => HasFileCacheTop (FileCacheT st FileError m) where
    fileCacheTop = (FileCacheTop . view _2) <$> ask

instance (Monad m, e ~ FileError) => MonadError e (FileCacheT st e m) where
    throwError :: e -> FileCacheT st FileError m a
    throwError e = FileCacheT $ throwError e
    catchError :: FileCacheT st FileError m a -> (e -> FileCacheT st FileError m a) -> FileCacheT st FileError m a
    catchError (FileCacheT m) c = FileCacheT $ m `catchError` (unFileCacheT . c)

instance Monad m => MonadReader (st, FilePath) (FileCacheT st FileError m) where
    ask = FileCacheT ask
    local f action = FileCacheT (local f (unFileCacheT action))

runFileCacheT ::
       st
    -> FileCacheTop
    -> FileCacheT st FileError m a
    -> m (Either FileError a)
runFileCacheT fileAcidState (FileCacheTop fileCacheDir) action =
    runExceptT (runReaderT (unFileCacheT action) (fileAcidState, fileCacheDir))

runFileCacheTop ::
       FileCacheTop
    -> FileCacheT () e m a
    -> m (Either e a)
runFileCacheTop (FileCacheTop fileCacheDir) action =
    runExceptT (runReaderT (unFileCacheT action) ((), fileCacheDir))

runFileCache ::
       FileCacheTop
    -> FileCache () () a
    -> a
runFileCache (FileCacheTop fileCacheDir) action =
    (\(Right x) -> x) $ runIdentity (runExceptT (runReaderT (unFileCacheT action) ((), fileCacheDir)))

mapFileCacheT :: Functor m => (e -> e') -> FileCacheT st e m a -> FileCacheT st e' m a
mapFileCacheT f = FileCacheT . mapReaderT (withExceptT f) . unFileCacheT

ensureFileCacheTop :: MonadIO m => FileCacheT st FileError m ()
ensureFileCacheTop = fileCacheTop >>= liftIOToF . createDirectoryIfMissing True . unFileCacheTop

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
