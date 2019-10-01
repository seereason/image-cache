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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Data.FileCache.FileCacheT
    ( ensureFileCacheTop
    , runFileCacheT'
    , runFileCacheT
    , execFileCacheT
    ) where

import Control.Lens (_1, _2, view)
import Control.Monad.Except -- (ExceptT(ExceptT), liftEither, MonadError(..), runExceptT, withExceptT)
import Control.Monad.Reader ({-mapReaderT,-} MonadReader(ask))
import Control.Monad.RWS
import Control.Monad.Trans (lift, MonadIO(..), MonadTrans(lift))
import Data.FileCache.Types (FileCacheTop, HasFileCacheTop(fileCacheTop))
import Data.Generics (Proxy)
import System.Directory (createDirectoryIfMissing)

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

ensureFileCacheTop :: (MonadIO m, Monoid w) => RWST (acid, FileCacheTop) w s m ()
ensureFileCacheTop = fileCacheTop >>= lift . liftIO . createDirectoryIfMissing True . unFileCacheTop
