-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, GADTs, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Background
  ( TaskChan
  , HasTaskQueue(taskQueue)
  , startTaskQueue
  , DoTask(doTask)

  , ImageTaskKey
  , queueImageTasks
  , testImageKeys
  ) where

import Control.Concurrent as IO (ThreadId{-, threadDelay-}, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Thread (forkIO, Result)
import Control.Exception (IOException)
import Control.Lens
import Control.Monad (forever, when)
import Control.Monad.Reader (liftIO, MonadReader(ask), runReaderT)
import Control.Monad.Except (runExceptT)
import Data.FileCache.FileCacheTop
import Data.FileCache.Derive (CacheFlag, cacheImageFile, cacheImageShape, getImageFiles, getImageShapes)
import Data.FileCache.FileError (FileError)
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey
import Data.Generics.Sum (_Ctor)
import Data.ListLike ( length, show )
import Data.Map.Strict as Map ( filter, fromList, keysSet, Map, size )
import Data.Maybe (mapMaybe)
import Data.Monoid ( (<>) )
import Data.Set as Set (Set, toList)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.LogServer (alog)
import SeeReason.Errors (Member, OneOf, throwMember)
import System.Log.Logger (Priority(..))

data ImageTaskKey where
  ImageTask :: ImageKey -> ImageShape -> ImageTaskKey
  -- DummyTask :: String -> TaskKey

type TaskChan key = Chan [key]

-- | Find the field containing the task queue
class HasTaskQueue key a where
  taskQueue :: a -> Maybe (TaskChan key, (ThreadId, IO (Result ())))
instance HasTaskQueue key (TaskChan key, (ThreadId, IO (Result ()))) where taskQueue = Just
instance HasTaskQueue key (a, b, (TaskChan key, (ThreadId, IO (Result ())))) where taskQueue = Just . view _3
instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

-- | Enqueue a build for the 'ImageKey's that have an 'ImageShape'.
queueImageTasks ::
  forall r e m. (MonadFileCache r e m, HasTaskQueue ImageTaskKey r, HasCallStack)
  => Set CacheFlag
  -> [ImageKey]
  -> m ()
queueImageTasks flags keys = do
  files :: [Either FileError ImageFile]
    <- mapM (\key -> cacheImageShape flags key Nothing) keys
  queueImageBuild (mapMaybe isShape (zip keys files))
  where isShape (key, Right (ImageFileShape shape)) = Just (ImageTask key shape)
        isShape _ = Nothing

-- | Insert an image build request into the channel that is being polled
-- by the thread launched in startCacheImageFileQueue.
queueImageBuild ::
  (MonadFileCache r e m, HasTaskQueue ImageTaskKey r, HasCallStack)
  => [ImageTaskKey]
  -> m ()
queueImageBuild pairs = do
  -- Write empty files into cache
  alog DEBUG ("queueImageBuild - requesting " ++ show (length pairs) ++ " images")
  (chan, _) <- maybe (error "Chan Is Missing") pure =<< (taskQueue <$> ask)
  liftIO (writeChan chan pairs)
  alog DEBUG ("queueImageBuild - requested " ++ show (length pairs) ++ " images")

type E = '[FileError, IOException]

-- | Class of types that represent tasks.
class DoTask key r where
  type TaskResult key
  doTask :: HasCallStack => r -> key -> IO (TaskResult key)

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startTaskQueue ::
  forall key r. (DoTask key r, HasCallStack)
  => r
  -> IO (TaskChan key, (ThreadId, IO (Result ())))
startTaskQueue r = do
  (chan :: TaskChan key) <- newChan
  alog DEBUG "Starting background task queue"
  (,) <$> pure chan <*> forkIO (task chan)
  where
    task :: TaskChan key -> IO ()
    task chan = forever $
      readChan chan >>= doTasks @key r

-- | This is the background task.
doTasks ::
  forall key r. (DoTask key r, HasCallStack)
  => r
  -> [key]
  -> IO ()
doTasks r tasks = do
  when (length tasks > 0) $ alog DEBUG ("performing " ++ show (length tasks) ++ " tasks")
  mapM_ (doTask r) tasks

instance (HasCacheAcid r, HasFileCacheTop r) => DoTask ImageTaskKey r where
  type TaskResult ImageTaskKey = ()
  doTask = doImageTask

-- | This is used to implement the image portion of doTask for
-- whatever the ultimate 'DoTask' sum type is.
doImageTask ::
  (HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => r -> ImageTaskKey -> IO ()
doImageTask r (ImageTask key shape) =
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
   HasTaskQueue ImageTaskKey r,
   Member ImageStats e,
   HasCallStack)
  => Set ImageKey
  -> m ()
testImageKeys ks = do
  shapes <- getImageShapes mempty ks
  let ready = Map.filter (has (_Right . _Ctor @"ImageFileReady")) shapes
      -- Files that have been requested but not yet written out
      unready = Map.filter (has (_Right . _Ctor @"ImageFileShape")) shapes
      -- Files that were written out but have gone missing, or were
      -- recorded with the retired NoShapeOld error constructor.
      missing = Map.filter (\e -> has (_Left . _Ctor @"MissingDerivedEntry") e ||
                                  has (_Left . _Ctor @"NoShapeOld") e) shapes
      needed = Map.keysSet unready <> Map.keysSet missing
  let stats = ImageStats {_keys = Map.size shapes,
                          _ready = Map.size ready,
                          _shapes = Map.size unready,
                          _errors = Map.size missing }
  alog DEBUG ("#keys=" <> show (_keys stats))
  alog DEBUG ("#ready=" <> show (_ready stats))
  alog DEBUG ("#unready image shapes: " <> show (_shapes stats))
  alog DEBUG ("#errors=" <> show (_errors stats))
  view (to (taskQueue @ImageTaskKey)) >>= \case
    Just _ | _shapes stats + _errors stats > 20 -> do
      alog DEBUG ("cacheDerivedImagesBackground " <> show needed)
      queueImageTasks mempty (Set.toList needed)
      throwMember stats
    _ -> do
      _ <- getImageFiles mempty needed
      alog DEBUG ("getImageFiles " <> show needed)
      pure ()
