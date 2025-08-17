-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, GADTs, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}
{-# OPTIONS -Werror=unused-imports #-}

module Data.FileCache.Background
  ( TaskChan
  , TaskQueue(TaskQueue)
  , HasTaskQueue(taskQueue)
  , startTaskQueue
  , DoTask(doTaskInternal, pollTask)
  , TaskStatus(Incomplete, Complete)
  , doTask
  , queueTasks
  ) where

import Control.Concurrent as IO (ThreadId{-, threadDelay-}, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Thread (forkIO, Result)
import Control.Lens
import Control.Monad (forever, when)
import Control.Monad.Except (liftIO, {-MonadError,-} MonadIO)
import Control.Monad.Reader (ask, MonadReader)
import Data.ListLike ( length, show )
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.Log (alog)
import System.Log.Logger (Priority(..))

type TaskChan key = Chan [key]
data TaskQueue key = TaskQueue (TaskChan key) (ThreadId, IO (Result ()))

-- | Find the field containing the task queue
class HasTaskQueue key a where
  taskQueue :: a -> Maybe (TaskQueue key)
instance HasTaskQueue key (TaskQueue key) where taskQueue = Just
instance HasTaskQueue key (a, b, TaskQueue key) where taskQueue = Just . view _3
-- instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

-- | We need to be able to determine whether a task has successfully
-- completed so we can abandon further effort to perform it.
data TaskStatus = Incomplete | Complete

-- | Class of types that represent tasks.
class DoTask key a where
  doTaskInternal :: HasCallStack => a -> key -> IO ()
  pollTask :: HasCallStack => a -> key -> IO TaskStatus
  pollTask _ _ = pure Incomplete

-- | Check whether the task still needs to be done and if so do it.
doTask :: (DoTask key a, HasCallStack) => a -> key -> IO ()
doTask a key =
  pollTask a key >>= \case Incomplete -> doTaskInternal a key; _ -> pure ()

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startTaskQueue ::
  forall key a. (DoTask key a, HasCallStack)
  => a
  -> IO (TaskQueue key)
startTaskQueue a = do
  (chan :: TaskChan key) <- newChan
  alog DEBUG "Starting background task queue"
  TaskQueue <$> pure chan <*> forkIO (task chan)
  where
    task :: TaskChan key -> IO ()
    task chan = forever $
      readChan chan >>= doTasks @key a

-- | This is the background task.
doTasks ::
  forall key a. (DoTask key a, HasCallStack)
  => a
  -> [key]
  -> IO ()
doTasks a tasks = do
  when (length tasks > 0) $ alog DEBUG ("performing " ++ show (length tasks) ++ " tasks")
  mapM_ (doTask a) tasks

queueTasks ::
  forall task a {-e-} m.
  (MonadIO m,
   MonadReader a m,
   -- MonadError (OneOf e) m,
   HasTaskQueue task a,
   HasCallStack)
  => [task]
  -> m ()
queueTasks tasks = do
  TaskQueue chan _ <- maybe (error "Chan Is Missing") pure =<< (taskQueue <$> ask)
  liftIO (writeChan chan tasks)
  alog DEBUG ("enqueuing " ++ show (length tasks) ++ " tasks")
