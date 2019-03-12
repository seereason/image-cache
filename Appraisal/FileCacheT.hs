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
    -- , MonadFileCache
    , ensureFileCacheTop
    , FileCacheT, FileCache
    , runFileCacheT'
    , runFileCacheT
    , execFileCacheT
    -- , runFileCacheTop  -- still used in AppraisalScope
    -- , runFileCache
    -- , mapFileCacheT
    ) where

--import Appraisal.FileError
import Control.Lens (_1, _2, view)
import Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader ({-mapReaderT,-} MonadReader(ask))
import Control.Monad.RWS
import Control.Monad.Trans (lift, MonadIO(..), MonadTrans(lift))
import Data.Generics (Proxy)
import System.Directory (createDirectoryIfMissing)

{-
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
-}

newtype FileCacheTop = FileCacheTop {unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- of a 'FileCache'.  MonadIO is not a superclass here because
-- some FileCache operations (e.g. path construction) do not need it.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FileCacheTop

type FileCacheT acid w s m = RWST (acid, FileCacheTop) w s m

type FileCache acid w s = FileCacheT acid w s Identity

-- type MonadFileCache m = (MonadIO m, {-MonadError FileError m,-} HasFileCacheTop m)

instance (Monad m, Monoid w) => HasFileCacheTop (RWST (acid, FileCacheTop) w s m) where
    fileCacheTop = view _2 <$> ask

instance HasFileCacheTop m => HasFileCacheTop (ExceptT e m) where
    fileCacheTop = lift fileCacheTop

runFileCacheT' ::
       Proxy w
    -> s
    -> acid
    -> FileCacheTop
    -> RWST (acid, FileCacheTop) w s m a
    -> m (a, s, w)
runFileCacheT' _ s0 r0 top action = runRWST action (r0, top) s0

runFileCacheT :: Monad m => Proxy w -> s -> acid -> FileCacheTop -> RWST (acid, FileCacheTop) w s m a -> m a
runFileCacheT w s0 r0 top action = view _1 <$> runFileCacheT' w s0 r0 top action

execFileCacheT :: Monad m => Proxy w -> s -> acid -> FileCacheTop -> RWST (acid, FileCacheTop) w s m a -> m s
execFileCacheT w s0 r0 top action = view _2 <$> runFileCacheT' w s0 r0 top action

{-
runFileCacheTop ::
     Monad m
    => FileCacheTop
    -> acid
    -- -> FileCacheT () () () (ExceptT FileError m) a
    -> RWST (acid, FileCacheTop) () () (ExceptT FileError m) a
    -> m (Either FileError a)
runFileCacheTop top acid action =
    runExceptT (runFileCacheT (Proxy :: Proxy w) () acid top action)
-}

-- mapFileCacheT :: Functor m => (e -> e') -> FileCacheT st m a -> FileCacheT st m a
-- mapFileCacheT f = FileCacheT . mapReaderT (withExceptT f) . unFileCacheT

ensureFileCacheTop :: (MonadIO m, Monoid w) => FileCacheT acid w s m ()
ensureFileCacheTop = fileCacheTop >>= lift . liftIO . createDirectoryIfMissing True . unFileCacheTop
