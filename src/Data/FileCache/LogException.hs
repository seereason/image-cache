{-# LANGUAGE TemplateHaskell #-}

module Data.FileCache.LogException
    ( logException
    , logExceptionV
    , logAndThrow
    , Loggable(logit)
    ) where

import Control.Monad.Except (MonadIO(liftIO), MonadError(catchError, throwError))
import Language.Haskell.TH (ExpQ, Exp, Loc(..), location, pprint, Q)
import Language.Haskell.TH.Instances ()
import qualified Language.Haskell.TH.Lift as TH (Lift(lift))
import System.Log.Logger (Priority, logM)

__LOC__ :: Q Exp
__LOC__ = TH.lift =<< location

logAndThrow :: (MonadError e m, MonadIO m, Show e) => String -> Priority -> e -> m b
logAndThrow m p e = liftIO (logM m p ("logAndThrow - " ++ show e)) >> throwError e

-- | Create an expression of type (Unexceptional m => Priority -> m a -> m a) that we can
-- apply to an expression so that it catches, logs, and rethrows any
-- exception.
logException :: ExpQ
logException =
    [| \priority action ->
         action `catchError` (\e -> do
                                liftIO (logM (loc_module $__LOC__)
                                              priority
                                              ("Logging exception: " <> (pprint $__LOC__) <> " -> " ++ show e))
                                throwError e) |]

logExceptionV :: ExpQ
logExceptionV =
    [| \priority action ->
         action `catchError` (\e -> do
                                liftIO (logM (loc_module $__LOC__)
                                              priority
                                              ("Logging exception: " <> (pprint $__LOC__) <> " -> " ++ show (V e)))
                                throwError e) |]

class Loggable a where
  logit :: Priority -> Loc -> a -> IO ()
