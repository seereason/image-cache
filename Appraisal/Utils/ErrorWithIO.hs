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

import Appraisal.Utils.Pretty
import Control.Monad.Error (ErrorT(ErrorT, runErrorT))
import Control.Monad.Trans (liftIO)
import GHC.IO.Exception (IOException(ioe_description))
import Prelude hiding (error, undefined, log)
import System.Exit (ExitCode(..))
import System.IO.Error (tryIOError)
import System.Log.Logger (logM, Priority(DEBUG, ERROR))
import qualified System.Posix.Files as F
import System.Process
import System.Process.ListLike (ListLikePlus, readCreateProcessWithExitCode, readCreateProcess)

type ErrorWithIO = ErrorT IOError IO

io :: IO a -> ErrorWithIO a
io = ErrorT . tryIOError

modify :: (IOError -> IOError) -> ErrorWithIO a -> ErrorWithIO a
modify f action = ErrorT (runErrorT action >>= return . either (Left . f) Right)

throw :: IOError -> ErrorWithIO a
throw = ErrorT . return . Left

catch :: ErrorWithIO a -> (IOError -> ErrorWithIO a) -> ErrorWithIO a
catch action handle = ErrorT (runErrorT action >>= either (runErrorT . handle) (return . Right))

-- | Add a prefix to an IOError's description.
prefix :: String -> ErrorWithIO a -> ErrorWithIO a
prefix s action = modify (mapIOErrorDescription (s ++)) action

mapIOErrorDescription :: (String -> String) -> IOError -> IOError
mapIOErrorDescription f e = e {ioe_description = f (ioe_description e)}

-- | Add a log message about an exception.
logExceptionM :: String -> ErrorWithIO a -> ErrorWithIO a
logExceptionM tag action = ErrorT (runErrorT action >>= either (\ e -> liftIO (logM tag ERROR (show e)) >> return (Left e)) (return . Right))

ensureLink :: String -> FilePath -> ErrorWithIO ()
ensureLink file path =
    io (-- trace ("ensureLink " ++ show (fileChksum file) ++ " " ++ show path) (return ()) >>
        F.getSymbolicLinkStatus path >> return ()) `catch` (\ _ -> io (F.createSymbolicLink file path))

readCreateProcessWithExitCode' :: ListLikePlus a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    logM "readCreateProcessWithExitCode" DEBUG (show (pretty p :: Doc)) >>
    readCreateProcessWithExitCode p s

readCreateProcess' :: ListLikePlus a c => CreateProcess -> a -> IO a
readCreateProcess' p s =
    logM "readCreateProcess" DEBUG (show (pretty p :: Doc)) >>
    readCreateProcess p s

instance Pretty Doc CreateProcess where
    pretty p = pretty (cmdspec p)

instance Pretty Doc CmdSpec where
    pretty (ShellCommand s) = text s
    pretty (RawCommand path args) = text (showCommandForUser path args)
