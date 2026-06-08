-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, FunctionalDependencies, GADTs, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Background
  ( TaskChan
  , TaskQueue(TaskQueue)
  , HasTaskSet(lookTasks, overTasks)
  , HasTaskQueue(taskQueue)
  , HasTasks
  , startTaskQueue
  , DoTask(doTask, pollTask, TaskResult)
  , TaskStatus(Incomplete, Complete)
  , checkTask
  , queueTasks
  , MonadFromIO
  ) where

import Control.Concurrent as IO (ThreadId{-, threadDelay-}, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Thread (forkIO, Result)
import Control.Exception (AsyncException(ThreadKilled), catch, throwIO)
import Control.Lens
import Control.Monad (forever, unless)
import Control.Monad.Catch (MonadCatch, SomeException)
import Control.Monad.Except (liftIO, MonadError, MonadIO)
import Control.Monad.Reader (ask, MonadReader)
import Data.ListLike ( show )
import Data.Set as Set (delete, difference, fromList, Set, size, toList, union)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.Errors (ConvertError, fromIO)
import SeeReason.Log (alog)
import System.Log.Logger (Priority(..))

-- | An enhanced MonadIO that catches synchronous exceptions and adds
-- them to the error monad.
type MonadFromIO e m =
  (MonadIO m,
   MonadCatch m,
   MonadError e m,
   ConvertError SomeException (Either SomeException e))

type TaskChan key = Chan [key]
data TaskQueue key = TaskQueue (TaskChan key) (ThreadId, IO (Result ()))

-- | We need to be able to determine whether a task has successfully
-- completed so we can abandon further effort to perform it.
data TaskStatus result = Incomplete | Complete result deriving Show

-- | Find the field containing the task queue
class (Ord key, Show key) => HasTaskQueue key queue | queue -> key where
  taskQueue :: queue -> TaskQueue key

-- | Maintain and monitor a task queue.
class MonadFromIO e m => HasTaskSet key e m where
  lookTasks :: m (Set key)
  overTasks :: (Set key -> Set key) -> m ()

type HasTasks key queue e m =
  (HasTaskSet key e m,
   HasTaskQueue key queue,
   DoTask key queue)

instance (Ord key, Show key) => HasTaskQueue key (TaskQueue key) where taskQueue = id
instance (Ord key, Show key) => HasTaskQueue key (a, b, TaskQueue key) where taskQueue = view _3

-- | Class of types that represent tasks.  The task monad is limited
-- to IO because it occurs inside of 'forkIO'.
class DoTask key queue | queue -> key where
  type TaskResult key
  doTask :: HasCallStack => queue -> key -> IO (TaskResult key)
  pollTask :: HasCallStack => queue -> key -> IO (TaskStatus (TaskResult key))

-- | Fork a thread into the background that loops forever reading task
-- keys from the channel and running the corresponding task.
startTaskQueue ::
  forall key queue. (DoTask key queue, HasCallStack)
  => queue
  -> IO (TaskQueue key)
startTaskQueue queue = do
  (chan :: TaskChan key) <- newChan
  alog INFO "Background task queue starting"
  TaskQueue <$> pure chan <*> forkIO (task chan `catch` handler)
  where
    -- This is the background task.  It is limited to IO by forkIO.
    task :: TaskChan key -> IO ()
    task chan = forever $
      readChan chan >>= mapM_ (doTask @key queue)
#if 1
    handler :: SomeException -> IO ()
    handler e = do
      alog INFO ("task queue: e=" <> show e)
      throwIO e
#else
    handler :: AsyncException -> IO ()
    handler ThreadKilled = alog INFO "task queue exiting"
    handler e = throwIO e
#endif

-- | Check whether the task still needs to be done and if so do it.
checkTask ::
  forall key queue e m.
  (DoTask key queue, Ord key,
   HasTaskSet key e m,
   HasCallStack) => queue -> key -> m (TaskResult key)
checkTask queue key =
  fromIO (pollTask queue key) >>= \case
    Incomplete -> fromIO (doTask queue key)
    Complete result -> do
      overTasks (Set.delete key)
      count <- Set.size <$> lookTasks @key
      alog INFO ("Remaining tasks: " <> show count)
      pure result

-- | Add some tasks to the task queue after using the task set to see
-- if they are already in progress.  If the task has already completed
-- it will not be in the task set, so it is important to make re-doing
-- a completed task an inexpensive operation.
queueTasks ::
  forall key r e m.
  (MonadFromIO e m,
   MonadReader r m,
   HasTasks key r e m,
   HasCallStack)
  => [key]
  -> m ()
queueTasks tasks = do
  TaskQueue chan _ <- taskQueue <$> ask
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
