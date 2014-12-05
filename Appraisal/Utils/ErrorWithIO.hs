{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Appraisal.Utils.ErrorWithIO
    ( ErrorWithIO
    , modify
    , prefix
    , logExceptionM
    , mapIOErrorDescription
    , ensureLink
    , readCreateProcess'
    , readCreateProcessWithExitCode'
    ) where

import Control.Monad.Error (MonadError, ErrorT(ErrorT, runErrorT), catchError, throwError)
import Control.Monad.Trans (MonadIO, liftIO)
import GHC.IO.Exception (IOException(ioe_description))
import Prelude hiding (error, undefined, log)
import System.Exit (ExitCode(..))
import System.Log.Logger (logM, Priority(DEBUG, ERROR))
import qualified System.Posix.Files as F
import System.Process
import System.Process.ListLike (ListLikeProcessIO, ProcessOutput, readCreateProcessWithExitCode, readCreateProcess)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

type ErrorWithIO m = ErrorT IOError m

modify :: Monad m => (IOError -> IOError) -> ErrorWithIO m a -> ErrorWithIO m a
modify f action = ErrorT (runErrorT action >>= return . either (Left . f) Right)

-- | Add a prefix to an IOError's description.
prefix :: Monad m => String -> ErrorWithIO m a -> ErrorWithIO m a
prefix s action = modify (mapIOErrorDescription (s ++)) action

mapIOErrorDescription :: (String -> String) -> IOError -> IOError
mapIOErrorDescription f e = e {ioe_description = f (ioe_description e)}

-- | Add a log message about an exception.
logExceptionM :: (MonadError IOException m, MonadIO m) => String -> m a -> m a
logExceptionM tag action = action `catchError` (\ e -> liftIO (logM tag ERROR (show e)) >> throwError e)
--logExceptionM tag action = ErrorT (runErrorT action >>= either (\ e -> liftIO (logM tag ERROR (show e)) >> return (Left e)) (return . Right))

ensureLink :: (MonadError IOException m, MonadIO m) => String -> FilePath -> m ()
ensureLink file path =
    liftIO (-- trace ("ensureLink " ++ show (fileChksum file) ++ " " ++ show path) (return ()) >>
            F.getSymbolicLinkStatus path >> return ()) `catchError` (\ _ -> liftIO (F.createSymbolicLink file path))

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    logM "readCreateProcessWithExitCode" DEBUG (show (pPrint p)) >>
    readCreateProcessWithExitCode p s

readCreateProcess' :: (ListLikeProcessIO a c, ProcessOutput a b) => CreateProcess -> a -> IO b
readCreateProcess' p s =
    logM "readCreateProcess" DEBUG (show (pPrint p)) >>
    readCreateProcess p s

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)
