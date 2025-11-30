{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Process
  ( Result(Bytes, Temporary)
  , resultBytes
  , readCreateProcessWithExitCode'
  , pipeline
  ) where

import Control.Exception (IOException)
import Control.Monad.Except (MonadError, MonadIO(liftIO))
import qualified Data.ByteString.Lazy as BS ( ByteString, readFile )
import Data.FileCache.FileError ( FileError )
import Data.FileCache.LogException ( logException )
import Data.ListLike ( StringLike(show) )
import Data.String ( fromString )
import GHC.Stack ( HasCallStack )
import Prelude hiding (show)
import SeeReason.Log (alog)
import SeeReason.Errors ( throwMember, Member, OneOf )
import System.Exit ( ExitCode(..) )
import System.FilePath (FilePath)
import System.Log.Logger ( Priority(..) )
import qualified System.Process.ListLike as LL ( showCreateProcessForUser )
import System.Process ( showCommandForUser, CmdSpec(..), CreateProcess(cmdspec) )
import System.Process.ByteString.Lazy as LBS ()
import System.Process.ListLike as LL ( readCreateProcessWithExitCode, ListLikeProcessIO )
import Text.PrettyPrint.HughesPJClass ( text, Pretty(pPrint) )

-- * Orphan Instances

instance Pretty CreateProcess where
    pPrint p = pPrint (cmdspec p)

instance Pretty CmdSpec where
    pPrint (ShellCommand s) = text s
    pPrint (RawCommand path args) = text (showCommandForUser path args)

-- * Processes and IO

-- | File processing operations used to only produce a bytestring, now
-- they might also produce a temporary file.
data Result
  = Bytes BS.ByteString
  | Temporary FilePath

resultBytes :: MonadIO m => Result -> m BS.ByteString
resultBytes (Bytes bs) = pure bs
resultBytes (Temporary path) = liftIO $ BS.readFile path

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    $logException ERROR (LL.readCreateProcessWithExitCode p s)

pipeline ::
  forall e m. (MonadIO m, Member FileError e, Member IOException e, MonadError (OneOf e) m, HasCallStack)
  => [Result -> CreateProcess]
  -> Result
  -> m Result
pipeline [] input = return input
pipeline (p : ps) input =
  case input of
    Bytes bytes -> liftIO (LL.readCreateProcessWithExitCode (p input) bytes) >>= doResult
    Temporary path -> liftIO (LL.readCreateProcessWithExitCode (p input) "") >>= doResult
  where
    doResult :: (ExitCode, BS.ByteString, BS.ByteString) -> m Result
    -- doResult (Left e) = alog ERROR (LL.showCreateProcessForUser p ++ " -> " ++ show e) >> throwError e
    doResult (ExitSuccess, out, _) = pipeline ps (Bytes out)
    doResult (code, _, err) =
      let message = (LL.showCreateProcessForUser (p input) ++ " -> " ++ show code ++ " (" ++ show err ++ ")") in
        alog ERROR message >>
        -- Not actually an IOExeption, this is a process error exit
        throwMember (fromString message :: FileError)
