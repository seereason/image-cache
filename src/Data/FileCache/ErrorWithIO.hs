{-# LANGUAGE CPP, FlexibleContexts, MultiParamTypeClasses, ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Data.FileCache.ErrorWithIO
    ( ErrorWithIO
    , modify
    , prefix
    , mapIOErrorDescription
    , ensureLink
    , readCreateProcess'
    , readCreateProcessWithExitCode'
    ) where

import Control.Monad.Catch (catchJust)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.FileCache.LogException (logException)
import GHC.IO.Exception (IOException(ioe_description))
import Language.Haskell.TH.Instances ({- instance Lift Loc -})
import Prelude hiding (error, undefined, log)
import System.Exit (ExitCode(..))
import System.IO.Error (isDoesNotExistError)
import System.Log.Logger (Priority(ERROR))
import qualified System.Posix.Files as F
import System.Process
import System.Process.ListLike as LL (ListLikeProcessIO, ProcessResult, readCreateProcessWithExitCode, readCreateProcess)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

type ErrorWithIO m = ExceptT IOError m

modify :: Monad m => (IOError -> IOError) -> ErrorWithIO m a -> ErrorWithIO m a
modify f action = ExceptT (runExceptT action >>= return . either (Left . f) Right)

-- | Add a prefix to an IOError's description.
prefix :: Monad m => String -> ErrorWithIO m a -> ErrorWithIO m a
prefix s action = modify (mapIOErrorDescription (s ++)) action

mapIOErrorDescription :: (String -> String) -> IOError -> IOError
mapIOErrorDescription f e = e {ioe_description = f (ioe_description e)}

ensureLink :: String -> FilePath -> IO ()
ensureLink file path = (F.getSymbolicLinkStatus path >> return ()) `catchDoesNotExist` (\ () -> F.createSymbolicLink file path)

catchDoesNotExist :: IO a -> (() -> IO a) -> IO a
catchDoesNotExist = catchJust (\ e -> if isDoesNotExistError e then Just () else Nothing)

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    -- logM "Appraisal.Utils.ErrorWithIO" DEBUG ("readCreateProcessWithExitCode': " <> show (pPrint p)) >>
    $logException ERROR (LL.readCreateProcessWithExitCode p s)

readCreateProcess' :: (ListLikeProcessIO a c, ProcessResult a b) => CreateProcess -> a -> IO b
readCreateProcess' p s =
    -- logM "Appraisal.Utils.ErrorWithIO" DEBUG ("readCreateProcess': " <> show (pPrint p)) >>
    $logException ERROR (LL.readCreateProcess p s)

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)
