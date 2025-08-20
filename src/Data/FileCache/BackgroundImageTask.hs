-- Unused code that once did image builds in the background

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.BackgroundImageTask
  ( queueImageTasks
  , doImageTask
  , ImageTaskKey(ImageTask)
  , E
  ) where

import Control.Exception (IOException)
import Control.Lens
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.FileCache.Background
import Data.FileCache.FileCacheTop
import Data.FileCache.Derive (cacheImageFile, cacheImageShape, getImageFiles, getImageShapes)
import Data.FileCache.FileError (CacheFlag, FileError)
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey (ImageKey, ImageShape)
import Data.Generics.Sum (_Ctor)
import Data.ListLike ( show )
import Data.Map.Strict as Map (filter, keysSet, Map, size, union)
import Data.Maybe (catMaybes)
import Data.Monoid ( (<>) )
import Data.Set as Set (Set, toList)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.Log (alog)
import SeeReason.Errors (Member, OneOf, throwMember)
import System.Log.Logger (Priority(..))

instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

-- | Enqueue a build for the 'ImageKey's that have an 'ImageShape'.
-- These will be inserted into the channel that is being polled by the
-- thread launched by startTaskQueue.
queueImageTasks ::
  forall a e m key. (MonadFileCache a e m, HasTaskQueue key a, HasCallStack)
  => (ImageKey -> ImageShape -> key)
  -> Set CacheFlag
  -> [ImageKey]
  -> m ()
queueImageTasks enq flags keys = do
  mapM (\key -> fromShape key <$> cacheImageShape flags key Nothing) keys >>= queueTasks . fmap (uncurry enq) . catMaybes
  where
    fromShape key (Right (ImageFileShape shape)) = Just (key, shape)
    fromShape _ _ = Nothing

type E = '[FileError, IOException]

-- | This is used to implement the image portion of doTask for
-- whatever the ultimate 'DoTask' sum type is.
doImageTask ::
  (MonadFileCacheWriter a E (ReaderT a (ExceptT (OneOf E) IO)), HasCallStack)
  => a -> ImageKey -> ImageShape -> IO ()
doImageTask a key shape =
  runExceptT (runReaderT (cacheImageFile key shape {- >> liftIO (threadDelay 5000000)-}) a) >>= \case
    Left (e :: OneOf E) -> alog ERROR ("error building " <> show key <> ": " ++ show e)
    Right (Left (e :: FileError)) -> alog ERROR ("error building " <> show key <> ": " ++ show e)
    Right (Right _file) -> alog INFO ("completed " <> show key)

-- * Example of a task key type

data ImageTaskKey where
  ImageTask :: ImageKey -> ImageShape -> ImageTaskKey
  -- DummyTask :: String -> TaskKey

instance (HasCacheAcid a, HasFileCacheTop a,
          MonadFileCacheWriter a E (ReaderT a (ExceptT (OneOf E) IO))
         ) => DoTask ImageTaskKey a () where
  doTaskInternal a (ImageTask key shape) = doImageTask a key shape
