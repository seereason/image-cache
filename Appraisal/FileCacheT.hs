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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
    ( FileErrorT
    , runFileErrorT
    , FileCacheTop(..)
    , HasFileCacheTop(fileCacheTop)
    , MonadFileCache
    , ensureFileCacheTop
    , FileCacheT
    , runFileCacheT
    , runFileCacheTop
    , runFileCache
    -- , mapFileCacheT
    ) where

import Appraisal.FileError
import Control.Exception as E (ErrorCall, fromException, IOException, SomeException, throw, try)
import Control.Lens (_2, view)
import Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader ({-mapReaderT,-} MonadReader(ask, local), ReaderT, runReaderT)
import Control.Monad.Trans (lift, MonadIO(..), MonadTrans(lift))
--import Debug.Show (V)
import Data.Text (pack)
import System.Directory (createDirectoryIfMissing)

newtype FileErrorT m a = FileErrorT {unFileErrorT :: ExceptT FileError m a}
    deriving (Monad, Applicative, Functor)

instance MonadTrans FileErrorT where
    lift = FileErrorT . lift

instance Monad m => MonadError FileError (FileErrorT m) where
    throwError :: FileError -> FileErrorT m a
    throwError e = FileErrorT $ throwError e
    catchError :: FileErrorT m a -> (FileError -> FileErrorT m a) -> FileErrorT m a
    catchError (FileErrorT m) c = FileErrorT $ m `catchError` (unFileErrorT . c)

runFileErrorT :: FileErrorT m a -> m (Either FileError a)
runFileErrorT action = runExceptT (unFileErrorT action)

-- | Turn any caught IOException and ErrorCall into FileError.  Log
-- any intercepted IO errors.
instance MonadIO m => MonadIO (FileErrorT m) where
  liftIO =
    FileErrorT .
    (withExceptT toFileError :: ExceptT SomeException m a -> ExceptT FileError m a) .
    ((\action -> action >>= liftEither) :: ExceptT SomeException m (Either SomeException a) -> ExceptT SomeException m a) .
    logErrorCall .
    liftIO .
    try

toFileError :: SomeException -> FileError
toFileError e =
    maybe (throw e)
          id
          (msum [fmap (IOException . pack . show) (fromException e :: Maybe IOException),
                 fmap (Appraisal.FileError.ErrorCall . pack . show) (fromException e :: Maybe E.ErrorCall)])

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- of a 'FileCache'.  MonadIO is not a superclass here because
-- some FileCache operations (e.g. path construction) do not need it.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FileCacheTop

newtype FileCacheT st m a =
    FileCacheT {unFileCacheT :: ReaderT (st, FilePath) (FileErrorT m) a}
    deriving (Monad, Applicative, Functor)

type FileCache st a = FileCacheT st Identity a

instance MonadTrans (FileCacheT st) where
    lift = FileCacheT . lift . lift

instance MonadIO m => MonadIO (FileCacheT st m) where
    liftIO = lift . liftIO

type MonadFileCache m = (MonadIO m, MonadError FileError m, HasFileCacheTop m)

instance (Monad m, MonadReader (st, FilePath) (FileCacheT st m)) => HasFileCacheTop (FileCacheT st m) where
    fileCacheTop = (FileCacheTop . view _2) <$> ask

instance (Monad m, e ~ FileError) => MonadError e (FileCacheT st m) where
    throwError :: e -> FileCacheT st m a
    throwError e = FileCacheT $ throwError e
    catchError :: FileCacheT st m a -> (e -> FileCacheT st m a) -> FileCacheT st m a
    catchError (FileCacheT m) c = FileCacheT $ m `catchError` (unFileCacheT . c)

instance Monad m => MonadReader (st, FilePath) (FileCacheT st m) where
    ask = FileCacheT ask
    local f action = FileCacheT (local f (unFileCacheT action))

runFileCacheT ::
       st
    -> FileCacheTop
    -> FileCacheT st m a
    -> m (Either FileError a)
runFileCacheT fileAcidState (FileCacheTop fileCacheDir) action =
    runFileErrorT (runReaderT (unFileCacheT action) (fileAcidState, fileCacheDir))

runFileCacheTop ::
       FileCacheTop
    -> FileCacheT () m a
    -> m (Either FileError a)
runFileCacheTop (FileCacheTop fileCacheDir) action =
    runFileErrorT (runReaderT (unFileCacheT action) ((), fileCacheDir))

runFileCache ::
       FileCacheTop
    -> FileCache () a
    -> a
runFileCache (FileCacheTop fileCacheDir) action =
    (\(Right x) -> x) $ runIdentity (runFileErrorT (runReaderT (unFileCacheT action) ((), fileCacheDir)))

-- mapFileCacheT :: Functor m => (e -> e') -> FileCacheT st m a -> FileCacheT st m a
-- mapFileCacheT f = FileCacheT . mapReaderT (withExceptT f) . unFileCacheT

ensureFileCacheTop :: MonadIO m => FileCacheT st m ()
ensureFileCacheTop = fileCacheTop >>= lift . liftIO . createDirectoryIfMissing True . unFileCacheTop
