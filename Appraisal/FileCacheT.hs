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
    ( FileCacheTop(..)
    , HasFileCacheTop(fileCacheTop)
    , MonadFileCache
    , ensureFileCacheTop
    , FileCacheT
    , runFileCacheT
    , runFileCacheTop
    , runFileCache
    , mapFileCacheT
    ) where

import Appraisal.FileError
import Control.Lens (_2, view)
import Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Reader (mapReaderT, MonadReader(ask, local), ReaderT, runReaderT)
import Control.Monad.Trans (lift, MonadIO(..), MonadTrans)
import Debug.Show (V)
import System.Directory (createDirectoryIfMissing)

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- of a 'FileCache'.  MonadIO is not a superclass here because
-- some FileCache operations (e.g. path construction) do not need it.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FileCacheTop

newtype FileCacheT st e m a =
    FileCacheT {unFileCacheT :: ReaderT (st, FilePath) (ExceptT e m) a}
    deriving (Monad, Applicative, Functor)

type FileCache st e a = FileCacheT st e Identity a

instance MonadTrans (FileCacheT st e) where
    lift = FileCacheT . lift . lift

instance MonadIO m => MonadIO (FileCacheT st e m) where
    liftIO = lift . liftIO

type MonadFileCache e m = (MonadIO m, IsFileError e, Show e, Show (V e), MonadError e m, HasFileCacheTop m)

#if !MIN_VERSION_mtl(2,2,2)
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return
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
ensureFileCacheTop = fileCacheTop >>= lift . liftIO . createDirectoryIfMissing True . unFileCacheTop
