-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, GADTs, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Background
  ( TaskChan
  , HasTaskQueue(taskQueue)
  , startTaskQueue
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

data TaskKey where
  ImageTask :: ImageKey -> ImageShape -> TaskKey
  -- DummyTask :: String -> TaskKey

type TaskChan = Chan [TaskKey]

class HasTaskQueue a where
  taskQueue :: a -> Maybe (TaskChan, (ThreadId, IO (Result ())))
instance HasTaskQueue (TaskChan, (ThreadId, IO (Result ()))) where taskQueue = Just
instance HasTaskQueue (a, b, (TaskChan, (ThreadId, IO (Result ())))) where taskQueue = Just . view _3
instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

queueImageTasks ::
  forall r e m. (MonadFileCache r e m, HasTaskQueue r, HasCallStack)
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
queueImageTasks flags keys = do
  files :: [Either FileError ImageFile]
    <- mapM (\key -> cacheImageShape flags key Nothing) keys
  let pairs = zip keys files
  let tasks = mapMaybe isShape pairs
  queueImageBuild tasks
  pure $ Map.fromList pairs
  where isShape (key, Right (ImageFileShape shape)) = Just (ImageTask key shape)
        isShape _ = Nothing

-- | Insert an image build request into the channel that is being polled
-- by the thread launched in startCacheImageFileQueue.
queueImageBuild ::
  (MonadFileCache r e m, HasTaskQueue r, HasCallStack)
  => [TaskKey]
  -> m ()
queueImageBuild pairs = do
  -- Write empty files into cache
  alog DEBUG ("queueImageBuild - requesting " ++ show (length pairs) ++ " images")
  (chan, _) <- maybe (error "Chan Is Missing") pure =<< (taskQueue <$> ask)
  liftIO (writeChan chan pairs)
  alog DEBUG ("queueImageBuild - requested " ++ show (length pairs) ++ " images")

type E = '[FileError, IOException]

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startTaskQueue ::
  forall r.
  (HasCacheAcid r,
   HasFileCacheTop r,
   HasCallStack)
  => r
  -> IO (TaskChan, (ThreadId, IO (Result ())))
startTaskQueue r = do
  (chan :: TaskChan) <- newChan
  alog DEBUG "Starting background task queue"
  (,) <$> pure chan <*> forkIO (task chan)
  where
    task :: TaskChan -> IO ()
    task chan = forever $
      readChan chan >>= doTasks r

-- | This is the background task.
doTasks ::
  forall r. (HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => r
  -> [TaskKey]
  -> IO ()
doTasks r tasks = do
  when (length tasks > 0) $ alog DEBUG ("performing " ++ show (length tasks) ++ " tasks")
  mapM_ (doTask r) tasks

doTask ::
  forall r. (HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => r
  -> TaskKey
  -> IO ()
doTask r task@(ImageTask key shape) =
  runExceptT (runReaderT (cacheImageFile key shape {- >> liftIO (threadDelay 5000000)-}) r) >>= \case
    Left (e :: OneOf E) -> alog ERROR ("error building " <> show key <> ": " ++ show e)
    Right (Left (e :: FileError)) -> alog ERROR ("error building " <> show key <> ": " ++ show e)
    Right (Right _file) -> alog INFO ("completed " <> show key)
-- doTask r task@(DummyTask message) =
--   alog INFO ("performed dummy task: " <> message)

-- | Throw an exception if there are more than 20 unavailable
-- images.  This sends the images to the background image
-- generator thread, aborts whatever we are doing, and puts up a
-- "come back later" message.
testImageKeys ::
  forall r e m.
  (MonadFileCache r e m,
   HasTaskQueue r,
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
  view (to taskQueue) >>= \case
    Just _ | _shapes stats + _errors stats > 20 -> do
      alog DEBUG ("cacheDerivedImagesBackground " <> show needed)
      queueImageTasks mempty (Set.toList needed)
      throwMember stats
    _ -> do
      _ <- getImageFiles mempty needed
      alog DEBUG ("getImageFiles " <> show needed)
      pure ()
