{-# LANGUAGE LambdaCase, OverloadedLists, PackageImports, RecordWildCards #-}

module Data.FileCache.WaitFor
  ( incomplete
  , WaitResult(..)
  , waitForTasks
  , TaskKeyResult(TaskKeyResult)
  ) where

import Control.Concurrent (threadDelay)
import Data.FileCache.Background (DoTask(pollTask, TaskResult), HasTasks, TaskStatus(Incomplete), MonadFromIO)
import Data.Foldable (foldlM)
import Data.Map as Map (filter, insert, keysSet, Map)
import Data.Set as Set (null, Set, size)
import GHC.Stack (HasCallStack)
import SeeReason.Errors (fromIO)
import SeeReason.Log (alog, {-alogDrop,-} Priority(DEBUG, INFO))

data TaskKeyResult = TaskKeyResult

data WaitResult =
    NumberIncomplete Int -- ^ n is guaranteed to be more than 0
  | Completed [Int] -- ^ Argument is the remaining timeout schedule

data WaitFor m result =
  WaitFor
  { ready :: m Bool -- ^ completion test
  , nth :: Int -> m () -- ^ Something to do right after the nth wait
  , done :: [Int] -> m result -- ^ Signal completion, argument is remaining ticks
  , timeout :: m result -- ^ Signal timeout
  , ticks :: [Int] -- ^ Timeout schedule (list of microsecond wait times)
  }

-- | Returns either the number of remaining incomplete tasks after
-- timeout or the remaining timeout schedule after completion.
waitForTasks ::
  forall task queue e m.
  (HasTasks task queue e m,
    HasCallStack)
  => queue -- ^ The queue the tasks have already been added to
  -> [Int] -- ^ Timeout schedule (a list of microsecond wait times)
  -> Set task -- ^ The tasks we are waiting for
  -> m WaitResult
waitForTasks queue ticks tasks =
  waitFor $ WaitFor
  { ready = Set.null <$> incomplete queue tasks
  , nth = \_n -> pure ()
  , done = \ticks' -> do
      alog INFO ("result, ticks=" <> show ticks')
      pure $ Completed ticks'
  , timeout = do
      incomplete queue tasks >>= \case
        [] -> do
          alog INFO "All tasks finished"
          pure $ Completed []
        tasks' -> do
          alog INFO ("timeout, " <> show (Set.size tasks') <> " tasks remaining")
          pure $ NumberIncomplete $ Set.size tasks'
  , ticks = ticks
  }

waitFor :: (MonadFromIO e m, HasCallStack) => WaitFor m WaitResult -> m WaitResult
waitFor WaitFor{..} =
  go (zip [1..] ticks)
  where
    go [] = timeout
    go ((n, tick) : more) =
      ifM ready
        (alog INFO "Tasks finished: " >> done (fmap snd more))
        (alog DEBUG ("tick " <> show n) >> fromIO (threadDelay tick) >> nth n >> go more)

incomplete ::
  forall task queue e m.
  (DoTask task queue, Ord task,
   MonadFromIO e m,
   HasCallStack)
  => queue
  -> Set task
  -> m (Set task)
incomplete queue tasks = do
  statuses :: Map task (TaskStatus (TaskResult task)) <- setMapM (fromIO . pollTask queue) tasks
  pure $ Map.keysSet $ Map.filter (\case Incomplete -> True; _ -> False) statuses

-- | 'foldlM' for sets.  Belongs in some Extra module.
setMapM :: (Monad m, Ord k) => (k -> m a) -> Set k -> m (Map k a)
setMapM f s =
  foldlM f' mempty s
  where
    f' mp k = Map.insert k <$> f k <*> pure mp

-- | Like @if@, but where the test can be monadic.
{-# INLINABLE ifM #-}
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM b t f = do b <- b; if b then t else f
