{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.FileCache.LogException
    ( logException
    , logExceptionV
    , logAndThrow
    , Loggable(logit)
    ) where

import Control.Monad.Except (MonadError(catchError, throwError))
import Control.Monad.Trans (MonadIO(liftIO))
import Language.Haskell.TH (ExpQ, Exp, Loc(..), location, pprint, Q)
-- There is a new package template-haskell-lift with the same module
-- name, I assume it is intended as a replacement, but I'm not ready
-- to investigate atm.
import Language.Haskell.TH.Instances ()
import qualified "th-lift" Language.Haskell.TH.Lift as TH (Lift(lift))
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
