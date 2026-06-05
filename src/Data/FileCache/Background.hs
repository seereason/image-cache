-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, FunctionalDependencies, GADTs, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}
{-# OPTIONS -Werror=unused-imports #-}

module Data.FileCache.Background
  ( TaskChan
  , TaskQueue(TaskQueue)
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
import Control.Monad.State (MonadState)
import Data.ListLike ( show )
import Data.Set as Set (difference, fromList, Set, size, toList, union)
import Extra.Lens (HasLens(hasLens))
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

-- | We need to be able to determine whether a task has successfully
-- completed so we can abandon further effort to perform it.
data TaskStatus result = Incomplete | Complete result deriving Show

-- | Class of types that represent tasks.
class DoTask key queue result | key -> result where
  doTask :: HasCallStack => queue -> key -> IO result
  pollTask :: HasCallStack => queue -> key -> IO (TaskStatus result)
  pollTask _ _ = pure Incomplete

-- | Check whether the task still needs to be done and if so do it.
checkTask :: (DoTask key queue result, HasCallStack) => queue -> key -> IO result
checkTask queue key =
  pollTask queue key >>= \case
    Incomplete -> doTask queue key
    Complete result -> pure result

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startTaskQueue ::
  forall key queue result. (DoTask key queue result, HasCallStack)
  => queue
  -> IO (TaskQueue key)
startTaskQueue queue = do
  (chan :: TaskChan key) <- newChan
  alog DEBUG "Starting background task queue"
  TaskQueue <$> pure chan <*> forkIO (task chan)
  where
    -- This is the background task
    task :: TaskChan key -> IO ()
    task chan = forever $
      readChan chan >>= mapM_ (doTask @key queue)

queueTasks ::
  forall m s r key.
  (MonadIO m,
   MonadReader r m,
   MonadState s m,
   HasLens s (Set key),
   HasTaskQueue key r,
   HasCallStack)
  => [key]
  -> m ()
queueTasks tasks = do
  TaskQueue chan _ <- maybe (error "Chan Is Missing") pure =<< (taskQueue <$> ask)
  oldTasks <- use taskLens
  let newTasks = Set.difference taskSet oldTasks
  unless (null newTasks) $ do
    alog INFO ("Adding " ++ show (Set.size newTasks) ++ " tasks to queue of size " <> show (Set.size oldTasks))
    liftIO (writeChan chan (Set.toList newTasks))
    taskLens %= Set.union newTasks
  where
    taskSet :: Set key
    taskSet = Set.fromList tasks
    taskLens :: Lens' s (Set key)
    taskLens = hasLens @_ @(Set key)
