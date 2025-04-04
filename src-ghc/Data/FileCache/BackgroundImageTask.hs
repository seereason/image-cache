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

module Data.FileCache.BackgroundImageTask
  ( queueImageTasks
  , doImageTask
  , testImageKeys
  , foregroundOrBackground
  , ImageTaskKey(ImageTask)
  ) where

import Control.Exception (IOException)
import Control.Lens
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Data.FileCache.Background
import Data.FileCache.FileCacheTop
import Data.FileCache.Derive (CacheFlag, cacheImageFile, cacheImageShape, getImageFiles, getImageShapes)
import Data.FileCache.FileError (FileError)
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey
import Data.Generics.Sum (_Ctor)
import Data.ListLike ( show )
import Data.Map.Strict as Map (filter, keysSet, Map, size, union)
import Data.Maybe (catMaybes)
import Data.Monoid ( (<>) )
import Data.Set as Set (Set, toList)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.LogServer (alog)
import SeeReason.Errors (Member, OneOf, throwMember)
import System.Log.Logger (Priority(..))

instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

-- | Enqueue a build for the 'ImageKey's that have an 'ImageShape'.
-- These will be inserted into the channel that is being polled by the
-- thread launched by startTaskQueue.
queueImageTasks ::
  forall r e m key. (MonadFileCache r e m, HasTaskQueue key r, HasCallStack)
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
  (HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => r -> ImageKey -> ImageShape -> IO ()
doImageTask r key shape =
  runExceptT (runReaderT (cacheImageFile key shape {- >> liftIO (threadDelay 5000000)-}) r) >>= \case
    Left (e :: OneOf E) -> alog ERROR ("error building " <> show key <> ": " ++ show e)
    Right (Left (e :: FileError)) -> alog ERROR ("error building " <> show key <> ": " ++ show e)
    Right (Right _file) -> alog INFO ("completed " <> show key)

-- | Throw an exception if there are more than 20 unavailable
-- images.  This sends the images to the background image
-- generator thread, aborts whatever we are doing, and puts up a
-- "come back later" message.
testImageKeys ::
  forall r e m.
  (MonadFileCache r e m,
   HasCallStack)
  => Set ImageKey
  -> m (Map ImageKey (Either FileError ImageFile), ImageStats)
testImageKeys ks = do
  shapes :: Map ImageKey (Either FileError ImageFile)
    <- getImageShapes mempty ks
  let ready = Map.filter (has (_Right . _Ctor @"ImageFileReady")) shapes
      -- Files that have been requested but not yet written out
      unready = Map.filter (has (_Right . _Ctor @"ImageFileShape")) shapes
      -- Files that were written out but have gone missing, or were
      -- recorded with the retired NoShapeOld error constructor.
      missing = Map.filter (\e -> has (_Left . _Ctor @"MissingDerivedEntry") e ||
                                  has (_Left . _Ctor @"NoShapeOld") e) shapes
      -- needed = Map.keysSet unready <> Map.keysSet missing
  let stats = ImageStats {_keys = Map.size shapes,
                          _ready = Map.size ready,
                          _shapes = Map.size unready,
                          _errors = Map.size missing }
  alog DEBUG ("#keys=" <> show (_keys stats))
  alog DEBUG ("#ready=" <> show (_ready stats))
  alog DEBUG ("#unready image shapes: " <> show (_shapes stats))
  alog DEBUG ("#errors=" <> show (_errors stats))
  pure (Map.union unready missing, stats)

-- | Decide whether there are enough images to be built that we
-- need to do them in the background
foregroundOrBackground ::
  forall key r e m.
  (MonadFileCache r e m,
   HasTaskQueue key r,
   Member ImageStats e,
   HasCallStack)
  => ([ImageKey] -> m ())
  -> Set ImageKey
  -> m ()
foregroundOrBackground enq ks = do
  (needed, stats) <- over _1 Map.keysSet <$> testImageKeys ks
  -- let needed = Map.keysSet mp
  view (to (taskQueue @key)) >>= \case
    Just _ | _shapes stats + _errors stats > 20 -> do
      alog DEBUG ("needed=" <> show needed)
      enq (Set.toList needed)
      throwMember stats
    _ -> do
      _ <- getImageFiles mempty needed
      alog DEBUG ("getImageFiles " <> show needed)
      pure ()

-- * Example of a task key type

data ImageTaskKey where
  ImageTask :: ImageKey -> ImageShape -> ImageTaskKey
  -- DummyTask :: String -> TaskKey

instance (HasCacheAcid r, HasFileCacheTop r) => DoTask ImageTaskKey r where
  doTaskInternal r (ImageTask key shape) = doImageTask r key shape
