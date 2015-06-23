{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Appraisal.Utils.ErrorWithIO
    ( ErrorWithIO
    , modify
    , prefix
    , mapIOErrorDescription
    , ensureLink
    , readCreateProcess'
    , readCreateProcessWithExitCode'
    -- , logExceptionM
    , logException
    ) where

import Control.Exception (throw)
import Control.Monad.Catch (catch, catchJust, SomeException)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import GHC.IO.Exception (IOException(ioe_description))
import Language.Haskell.TH
import Language.Haskell.TH.Instances ({- instance Lift Loc -})
import Language.Haskell.TH.Syntax
import Prelude hiding (error, undefined, log)
import System.Exit (ExitCode(..))
import System.IO.Error (isDoesNotExistError)
import System.Log.Logger (logM, Priority(DEBUG, ERROR))
import qualified System.Posix.Files as F
import System.Process
import System.Process.ListLike as LL (ListLikeProcessIO, ProcessOutput, readCreateProcessWithExitCode, readCreateProcess)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

type ErrorWithIO m = ExceptT IOError m

modify :: Monad m => (IOError -> IOError) -> ErrorWithIO m a -> ErrorWithIO m a
modify f action = ExceptT (runExceptT action >>= return . either (Left . f) Right)

-- | Add a prefix to an IOError's description.
prefix :: Monad m => String -> ErrorWithIO m a -> ErrorWithIO m a
prefix s action = modify (mapIOErrorDescription (s ++)) action

mapIOErrorDescription :: (String -> String) -> IOError -> IOError
mapIOErrorDescription f e = e {ioe_description = f (ioe_description e)}

catchDoesNotExist :: IO a -> (() -> IO a) -> IO a
catchDoesNotExist = catchJust (\ e -> if isDoesNotExistError e then Just () else Nothing)

ensureLink :: String -> FilePath -> IO ()
ensureLink file path = (F.getSymbolicLinkStatus path >> return ()) `catchDoesNotExist` (\ () -> F.createSymbolicLink file path)

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    logM "readCreateProcessWithExitCode" DEBUG (show (pPrint p)) >>
    LL.readCreateProcessWithExitCode p s

readCreateProcess' :: (ListLikeProcessIO a c, ProcessOutput a b) => CreateProcess -> a -> IO b
readCreateProcess' p s =
    logM "readCreateProcess" DEBUG (show (pPrint p)) >>
    LL.readCreateProcess p s

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)

{-
instance Lift Exp where
    lift (VarE name) =
        recConE 'VarE ...
    lift (ConE Name) =
        recConE 'ConE ...
    lift (LitE Lit) =
        recConE 'LitE ...
    lift (AppE Exp Exp) =
        recConE 'AppE ...
    lift (InfixE (Maybe Exp) Exp (Maybe Exp)) =
        recConE 'InfixE ...
    lift (UInfixE Exp Exp Exp) =
        recConE 'UInfixE ...
    lift (ParensE Exp) =
        recConE 'ParensE ...
    lift (LamE [Pat] Exp) =
        recConE 'LamE ..
    lift (LamCaseE [Match]) =
        recConE 'LamCaseE ..
    lift (TupE [Exp]) =
        recConE 'TupE ..
    lift (UnboxedTupE [Exp]) =
        recConE 'UnboxedTupE ..
    lift (CondE Exp Exp Exp) =
        recConE 'CondE ..
    lift (MultiIfE [(Guard, Exp)]) =
        recConE 'MultiIfE ..
    lift (LetE [Dec] Exp) =
        recConE 'LetE ..
    lift (CaseE Exp [Match]) =
        recConE 'CaseE ..
    lift (DoE [Stmt]) =
        recConE 'DoE ..
    lift (CompE [Stmt]) =
        recConE 'CompE ..
    lift (ArithSeqE Range) =
        recConE 'ArithSeqE ..
    lift (ListE [Exp]) =
        recConE 'ListE ..
    lift (SigE Exp Type) =
        recConE 'SigE ..
    lift (RecConE Name [FieldExp]) =
        recConE 'RecConE ..
    lift (RecUpdE Exp [FieldExp]) =
        recConE 'RecUpdE ..
-}

__LOC__ :: Q Exp
__LOC__ = lift =<< location

-- | Create an expression of type (MonadIO m => a -> m a) that we can
-- apply to an expression so that it catches, logs, and rethrows any
-- exception.
logException :: ExpQ
logException =
    [| \ action -> let f :: MonadIO m => SomeException -> m a
                       f e = liftIO (logM "logException" ERROR ("Logging exception: " ++ (show $__LOC__) ++ " -> " ++ show e)) >> throw e in
                   action `catch` f |]
