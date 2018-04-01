-- | Instance of MonadFileCacheIO

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

module Appraisal.FileCacheInst
    ( FileCacheT(unFileCacheT)
    , runFileCache
    , runFileCacheT
    , runFileCacheIO
    ) where

import Appraisal.AcidCacheInst (runMonadCacheT)
import Appraisal.FileCache ( HasFileCacheTop(..), MonadFileCacheIO(..) )
import Appraisal.Utils.ErrorWithIO ()
import Control.Exception ( Exception, IOException )
import Control.Monad.Catch ( MonadCatch(catch), MonadThrow(throwM) )
import Control.Monad.Except ( catchError, ExceptT, MonadError, runExceptT, throwError )
import Control.Monad.Reader ( mapReaderT, MonadReader(ask, local), ReaderT(ReaderT), runReaderT )
import Control.Monad.Trans ( MonadIO, MonadTrans(lift), liftIO )
import Data.Acid ( AcidState )
import Data.Map ( Map )

-- | In order to avoid type ambiguities between different reader monads, we need
-- a newtype wrapper around this ReaderT FileCacheTop.
newtype FileCacheT m a = FileCacheT {unFileCacheT :: ReaderT FilePath m a} deriving (Monad, Applicative, Functor)

-- | Get 'fileCacheTop' from the wrapped reader monad.
instance (Monad m, Monad (FileCacheT m)) => HasFileCacheTop (FileCacheT m) where
    -- fileCacheTop :: FileCacheT m FilePath
    fileCacheTop = FileCacheT (ReaderT return)

mapFileCacheT :: (m a -> m a) -> FileCacheT m a -> FileCacheT m a
mapFileCacheT f = FileCacheT . mapReaderT f . unFileCacheT

instance MonadTrans FileCacheT where
    lift = FileCacheT . lift
instance MonadReader r m => MonadReader r (FileCacheT m) where
    ask = lift ask
    local = mapFileCacheT . local
instance MonadIO m => MonadIO (FileCacheT m) where
    liftIO = FileCacheT . liftIO
instance MonadThrow m => MonadThrow (FileCacheT m) where
    throwM e = lift $ throwM e
instance MonadCatch m => MonadCatch (FileCacheT m) where
    catch :: forall e a. Exception e => FileCacheT m a -> (e -> FileCacheT m a) -> FileCacheT m a
    catch (FileCacheT m) c = FileCacheT $ m `catch` (unFileCacheT . c)
-- Hmm, familiar...
instance MonadError e m => MonadError e (FileCacheT m) where
    throwError :: e -> FileCacheT m a
    throwError e = lift $ throwError e
    catchError :: FileCacheT m a -> (e -> FileCacheT m a) -> FileCacheT m a
    catchError (FileCacheT m) c = FileCacheT $ m `catchError` (unFileCacheT . c)
instance HasFileCacheTop m => HasFileCacheTop (ExceptT IOException m) where
    fileCacheTop = lift fileCacheTop

runFileCacheIO ::
    MonadFileCacheIO e (FileCacheT (ReaderT (AcidState (Map key val)) (ExceptT e m)))
    => AcidState (Map key val)
    -> FilePath
    -> FileCacheT (ReaderT (AcidState (Map key val)) (ExceptT e m)) a
    -> m (Either e a)
runFileCacheIO fileAcidState fileCacheDir action =
    runExceptT (runMonadCacheT (runReaderT (unFileCacheT (ensureFileCacheTop >> action)) fileCacheDir) fileAcidState)

-- | Like runFileCacheIO, but without the MonadIO superclass.  No acid
-- state value is passed because you need IO to use acid state.
-- Typical use is to construct paths to the file cache.
runFileCache :: FilePath -> FileCacheT m a -> m a
runFileCache fileCacheDir action = runReaderT (unFileCacheT action) fileCacheDir

runFileCacheT :: FilePath -> FileCacheT m a -> m a
runFileCacheT fileCacheDir action =
    runReaderT (unFileCacheT action) fileCacheDir
