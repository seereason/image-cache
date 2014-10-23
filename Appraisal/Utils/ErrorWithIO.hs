{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Appraisal.Utils.ErrorWithIO
    ( ErrorWithIO
    , io
    , modify
    , throw
    , catch
    , prefix
    , logExceptionM
    , mapIOErrorDescription
    , ensureLink
    , readCreateProcess'
    , readCreateProcessWithExitCode'
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Error (ErrorT(ErrorT, runErrorT))
import Control.Monad.Trans (MonadIO, liftIO)
import GHC.IO.Exception (IOException(ioe_description))
import Prelude hiding (error, undefined, log)
import System.Exit (ExitCode(..))
import System.IO.Error (tryIOError)
import System.Log.Logger (logM, Priority(DEBUG, ERROR))
import qualified System.Posix.Files as F
import System.Process
import System.Process.ListLike (ListLikeLazyIO, readCreateProcessWithExitCode, readCreateProcess, unStdoutWrapper)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

type ErrorWithIO m = ErrorT IOError m

io :: IO a -> ErrorWithIO IO a
io = ErrorT . tryIOError

modify :: Monad m => (IOError -> IOError) -> ErrorWithIO m a -> ErrorWithIO m a
modify f action = ErrorT (runErrorT action >>= return . either (Left . f) Right)

throw :: Monad m => IOError -> ErrorWithIO m a
throw = ErrorT . return . Left

catch :: Monad m => ErrorWithIO m a -> (IOError -> ErrorWithIO m a) -> ErrorWithIO m a
catch action handle = ErrorT (runErrorT action >>= either (runErrorT . handle) (return . Right))

-- | Add a prefix to an IOError's description.
prefix :: Monad m => String -> ErrorWithIO m a -> ErrorWithIO m a
prefix s action = modify (mapIOErrorDescription (s ++)) action

mapIOErrorDescription :: (String -> String) -> IOError -> IOError
mapIOErrorDescription f e = e {ioe_description = f (ioe_description e)}

-- | Add a log message about an exception.
logExceptionM :: MonadIO m => String -> ErrorWithIO m a -> ErrorWithIO m a
logExceptionM tag action = ErrorT (runErrorT action >>= either (\ e -> liftIO (logM tag ERROR (show e)) >> return (Left e)) (return . Right))

ensureLink :: MonadIO m => String -> FilePath -> ErrorWithIO m ()
ensureLink file path =
    liftIO (-- trace ("ensureLink " ++ show (fileChksum file) ++ " " ++ show path) (return ()) >>
            F.getSymbolicLinkStatus path >> return ()) `catch` (\ _ -> liftIO (F.createSymbolicLink file path))

readCreateProcessWithExitCode' :: ListLikeLazyIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    logM "readCreateProcessWithExitCode" DEBUG (show (pPrint p)) >>
    readCreateProcessWithExitCode p s

readCreateProcess' :: ListLikeLazyIO a c => CreateProcess -> a -> IO a
readCreateProcess' p s =
    logM "readCreateProcess" DEBUG (show (pPrint p)) >>
    unStdoutWrapper <$> readCreateProcess p s

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)
