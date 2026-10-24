{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Queue (queueTests) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.STM (atomically, writeTQueue)
import Control.Monad (when)
import Data.FileCache.Background (DoTask(..), TaskQueue(TaskQueue), TaskStatus(Incomplete), startTaskQueue)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Test.HUnit

-- | Tasks that record their key as they run.  Task 1 waits at the
-- gate, so that whatever is queued meanwhile is waiting when it ends.
data Recorder = Recorder (IORef [Int]) (MVar ())

instance DoTask Int Recorder where
  type TaskResult Int = ()
  doTask (Recorder done gate) k = do
    when (k == 1) (readMVar gate)
    modifyIORef done (k :)
  pollTask _ _ = pure Incomplete

-- | A batch queued while another is running gets the next turn, and
-- after that the two alternate.
queueTests :: Test
queueTests = TestCase $ do
  done <- newIORef []
  gate <- newEmptyMVar
  TaskQueue chan _ <- startTaskQueue (Recorder done gate)
  atomically $ writeTQueue chan [1, 2, 3, 4, 5]
  threadDelay 100000 -- the worker takes task 1 and waits at the gate
  atomically $ writeTQueue chan [101, 102]
  putMVar gate ()
  order <- finished done 7 (500 :: Int)
  assertEqual "batches take turns" [1, 101, 2, 102, 3, 4, 5] order
  where
    finished :: IORef [Int] -> Int -> Int -> IO [Int]
    finished done n tries = do
      ks <- readIORef done
      if length ks >= n || tries == 0
        then pure (reverse ks)
        else threadDelay 10000 >> finished done n (tries - 1)
