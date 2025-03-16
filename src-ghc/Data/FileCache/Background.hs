-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, GADTs, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}
{-# OPTIONS -Werror=unused-imports #-}

module Data.FileCache.Background
  ( TaskChan
  , HasTaskQueue(taskQueue)
  , startTaskQueue
  , DoTask(doTask)
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
-- import SeeReason.Errors (OneOf)
import SeeReason.LogServer (alog)
import System.Log.Logger (Priority(..))

type TaskChan key = Chan [key]

-- | Find the field containing the task queue
class HasTaskQueue key a where
  taskQueue :: a -> Maybe (TaskChan key, (ThreadId, IO (Result ())))
instance HasTaskQueue key (TaskChan key, (ThreadId, IO (Result ()))) where taskQueue = Just
instance HasTaskQueue key (a, b, (TaskChan key, (ThreadId, IO (Result ())))) where taskQueue = Just . view _3
-- instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

-- | Class of types that represent tasks.
class DoTask key r where
  doTask :: HasCallStack => r -> key -> IO ()

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

queueTasks ::
  forall task r {-e-} m.
  (MonadIO m,
   MonadReader r m,
   -- MonadError (OneOf e) m,
   HasTaskQueue task r,
   HasCallStack)
  => [task]
  -> m ()
queueTasks tasks = do
  (chan, _) <- maybe (error "Chan Is Missing") pure =<< (taskQueue <$> ask)
  liftIO (writeChan chan tasks)
  alog DEBUG ("enqueuing " ++ show (length tasks) ++ " tasks")
