-- | Maintain a cache of files.
--
-- A data structure representing a local cache of a data file.  The
-- cached file persists across runs of our application, and can be
-- accessed by name and passed to software which requires a file, for
-- example a document formatter such as LaTeX.  The original data can
-- be supplied as either a URI, a local file path, or as a ByteString.
-- The file is then downloaded and stored on the local machine at a
-- location based on the file's checksum.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Appraisal.FileCacheT
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
    ) where

#if MIN_VERSION_base(4,9,0)
import Control.Exception (ErrorCall(ErrorCallWithLocation))
#else
import Control.Exception (ErrorCall(ErrorCall))
#endif
import Control.Exception (Exception(fromException), IOException, SomeException, throw, try)
import Control.Lens (_2, view)
import Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (mapReaderT, MonadReader(ask, local), ReaderT, runReaderT)
import Control.Monad.Trans (lift, MonadIO(..), MonadTrans)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.String (IsString(fromString))
import Data.Serialize (Serialize)
import Debug.Show (V(V))
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing)
import System.Exit ( ExitCode(..) )
import System.Log.Logger ( logM, Priority(ERROR) )
import System.Process (CreateProcess)
#if !MIN_VERSION_process(1,4,3)
import System.Process.ListLike (showCreateProcessForUser)
#endif

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

logFileError :: String -> FileError -> IO ()
logFileError prefix (Description s e) = logM prefix ERROR (" - error description: " ++ s) >> logFileError prefix e
logFileError prefix (FunctionName n e) = logM prefix ERROR (" - error function " ++ n) >> logFileError prefix e
logFileError prefix (IOException e) = logM prefix ERROR (" - IO exception: " ++ show e)
logFileError prefix (Failure s) = logM prefix ERROR (" - failure: " ++ s)
logFileError prefix (Command cmd code) = logM prefix ERROR (" - shell command failed: " ++ show cmd ++ " -> " ++ show code)

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
liftIOToF io = (FileCacheT . liftIO . runExceptT . withExceptT toFileError . ExceptT . logErrorCall . try $ io) >>= liftEither
    where
      logErrorCall :: IO (Either SomeException a) -> IO (Either SomeException a)
      logErrorCall x =
          x >>= either (\e -> case fromException e :: Maybe ErrorCall of
#if MIN_VERSION_base(4,9,0)
                                Just (ErrorCallWithLocation msg loc) -> logM "FileCache.hs" ERROR (show loc ++ ": " ++ msg) >> return (Left e)
#else
                                Just (Control.Exception.ErrorCall msg) -> logM "FileCache.hs" ERROR msg >> return (Left e)
#endif
                                _ -> return (Left e)) (return . Right)
      toFileError :: SomeException -> FileError
      toFileError e =
          maybe (throw e)
                id
                (msum [fmap IOException (fromException e :: Maybe IOException),
                       fmap Appraisal.FileCacheT.ErrorCall (fromException e :: Maybe ErrorCall)])

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
