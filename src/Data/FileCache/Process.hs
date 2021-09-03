{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Process
  ( readCreateProcessWithExitCode'
  , pipeline
  ) where

import Control.Exception ( IOException )
import Control.Monad.Except ( MonadError )
import qualified Data.ByteString.Lazy as BS ( ByteString )
import Data.FileCache.Common ( FileError )
import Data.FileCache.LogException ( logException )
import Data.ListLike ( StringLike(show) )
import Data.String ( fromString )
import SeeReason.Errors ( liftUIO, throwMember, Member, NonIOException, OneOf )
import qualified SeeReason.Errors as Errors ()
import GHC.Stack ( HasCallStack )
import Prelude ( (++), Monad((>>=), (>>), return), IO )
import SeeReason.LogServer (alog)
import System.Exit ( ExitCode(..) )
import System.Log.Logger ( Priority(..) )
import qualified System.Process.ListLike as LL ( showCreateProcessForUser )
import System.Process ( showCommandForUser, CmdSpec(..), CreateProcess(cmdspec) )
import System.Process.ByteString.Lazy as LBS ()
import System.Process.ListLike as LL ( readCreateProcessWithExitCode, ListLikeProcessIO )
import Text.PrettyPrint.HughesPJClass ( text, Pretty(pPrint) )
import UnexceptionalIO.Trans ( Unexceptional )
import UnexceptionalIO.Trans as UIO ( unsafeFromIO )

-- * Orphan Instances

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)

-- * Processes and IO

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    $logException ERROR (LL.readCreateProcessWithExitCode p s)

pipeline ::
  forall e m. (Unexceptional m, Member FileError e, Member IOException e, Member NonIOException e, MonadError (OneOf e) m, HasCallStack)
  => [CreateProcess]
  -> BS.ByteString
  -> m BS.ByteString
pipeline [] bytes = return bytes
pipeline (p : ps) bytes =
  liftUIO (LL.readCreateProcessWithExitCode p bytes) >>= doResult
  where
    doResult :: (ExitCode, BS.ByteString, BS.ByteString) -> m BS.ByteString
    -- doResult (Left e) = unsafeFromIO (alog ERROR (LL.showCreateProcessForUser p ++ " -> " ++ show e)) >> throwError e
    doResult (ExitSuccess, out, _) = pipeline ps out
    doResult (code, _, err) =
      let message = (LL.showCreateProcessForUser p ++ " -> " ++ show code ++ " (" ++ show err ++ ")") in
        unsafeFromIO (alog ERROR message) >>
        -- Not actually an IOExeption, this is a process error exit
        throwMember (fromString message :: FileError)
