-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, FunctionalDependencies, GADTs, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}
{-# OPTIONS -Werror=unused-imports #-}

module Data.FileCache.Background
  ( TaskChan
  , TaskQueue(TaskQueue)
  , HasTaskSet(lookTasks, overTasks)
  , HasTaskQueue(taskQueue)
  , startTaskQueue
  , DoTask(doTask, pollTask)
  , TaskStatus(Incomplete, Complete)
  , checkTask
  , queueTasks
  ) where

import Control.Concurrent as IO (ThreadId{-, threadDelay-}, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Thread (forkIO, Result)
import Control.Lens
import Control.Monad (forever, unless)
import Control.Monad.Except (liftIO, {-MonadError,-} MonadIO)
import Control.Monad.Reader (ask, MonadReader)
import Data.ListLike ( show )
import Data.Set as Set (delete, difference, fromList, Set, size, toList, union)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.Log (alog)
import System.Log.Logger (Priority(..))

type TaskChan key = Chan [key]
data TaskQueue key = TaskQueue (TaskChan key) (ThreadId, IO (Result ()))

-- | Find the field containing the task queue
class (Ord key, Show key) => HasTaskQueue key queue where
  taskQueue :: queue -> Maybe (TaskQueue key)
instance (Ord key, Show key) => HasTaskQueue key (TaskQueue key) where taskQueue = Just
instance (Ord key, Show key) => HasTaskQueue key (a, b, TaskQueue key) where taskQueue = Just . view _3

-- Can we merge this with HasTaskQueue?
class HasTaskSet key m where
  lookTasks :: m (Set key)
  overTasks :: (Set key -> Set key) -> m ()

-- | We need to be able to determine whether a task has successfully
-- completed so we can abandon further effort to perform it.
data TaskStatus result = Incomplete | Complete result deriving Show

-- | Class of types that represent tasks.
class DoTask key queue result | key -> result where
  doTask :: HasCallStack => queue -> key -> IO result
  pollTask :: HasCallStack => queue -> key -> IO (TaskStatus result)
  pollTask _ _ = pure Incomplete

-- | Check whether the task still needs to be done and if so do it.
checkTask :: forall key queue result m. (DoTask key queue result, Ord key, HasTaskSet key m, MonadIO m, HasCallStack) => queue -> key -> m result
checkTask queue key =
  liftIO (pollTask queue key) >>= \case
    Incomplete -> liftIO (doTask queue key)
    Complete result -> do
      overTasks (Set.delete key)
      count <- Set.size <$> lookTasks @key
      alog INFO ("Remaining tasks: " <> show count)
      pure result

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startTaskQueue ::
  forall key queue result m. (DoTask key queue result, MonadIO m, HasCallStack)
  => queue
  -> m (TaskQueue key)
startTaskQueue queue = do
  (chan :: TaskChan key) <- liftIO newChan
  alog DEBUG "Starting background task queue"
  TaskQueue <$> pure chan <*> liftIO (forkIO (task chan))
  where
    -- This is the background task
    task :: TaskChan key -> IO ()
    task chan = forever $
      readChan chan >>= mapM_ (doTask @key queue)

queueTasks ::
  forall m r key.
  (MonadIO m,
   MonadReader r m,
   HasTaskSet key m,
   HasTaskQueue key r,
   HasCallStack)
  => [key]
  -> m ()
queueTasks tasks = do
  TaskQueue chan _ <- maybe (error "Chan Is Missing") pure =<< (taskQueue <$> ask)
  oldTasks <- lookTasks
  let newTasks = Set.difference taskSet oldTasks
  unless (null newTasks) $ do
    alog INFO ("Adding " ++ show (Set.size newTasks) ++ " tasks to queue of size " <> show (Set.size oldTasks))
    liftIO (writeChan chan (Set.toList newTasks))
    overTasks (Set.union newTasks)
  where
    taskSet :: Set key
    taskSet = Set.fromList tasks
    -- taskLens :: Lens' s (Set key)
    -- taskLens = hasLens @_ @(Set key)
