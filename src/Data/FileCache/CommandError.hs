-- | Serialize and SafeCopy instances for 'IOException' aka 'IOError',
-- and CreateProcess to allow us to embed these in our error union
-- types.  They clobber all Handle values because they are too exotic
-- and don't contain any information useful to error processing code.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.FileCache.CommandError
  ( CommandError
  , CommandInfo(..)
  , HasCommandError(fromCommandError)
  , ToCommandError(toCommandError)
  ) where

import Data.ByteString.Lazy (ByteString)
import Data.FileCache.Orphans ()
import Data.SafeCopy
import Data.Serialize (Serialize(..))
import Extra.Except ({-instances only-})
import GHC.Generics (Generic)
import System.Exit (ExitCode)
import System.Process -- (CmdSpec(..), CreateProcess(..))

-- | Information about a shell command that failed.
type CommandError = [CommandInfo]

data CommandInfo
    = CommandCreateProcess CreateProcess
    | CommandExitCode ExitCode
    | CommandInput ByteString -- ^ command input
    | CommandOut ByteString -- ^ stdout
    | CommandErr ByteString -- ^ stderr
    | StartedFrom String -- ^ The function that ran the command
    | Description String -- ^ free form description of what happened
    deriving (Eq, Ord, Generic)

deriving instance Show CommandInfo
instance SafeCopy CommandInfo where version = 1
instance Serialize CommandInfo where get = safeGet; put = safePut

data StdStream' = Inherit' | UseHandle' | CreatePipe' | NoStream'
  deriving (Eq, Ord, Generic, Serialize)

stdStream :: StdStream -> StdStream'
stdStream Inherit = Inherit'
stdStream (UseHandle _) = UseHandle'
stdStream CreatePipe = CreatePipe'
stdStream NoStream = NoStream'

stdStream' :: StdStream' -> StdStream
stdStream' Inherit' = Inherit
stdStream' UseHandle' = NoStream
stdStream' CreatePipe' = CreatePipe
stdStream' NoStream' = NoStream

class HasCommandError e where fromCommandError :: CommandError -> e
instance HasCommandError CommandError where fromCommandError = id
class ToCommandError a where toCommandError :: a -> CommandError
instance ToCommandError CommandError where toCommandError = id

instance ToCommandError (ExitCode, ByteString, ByteString) where
  toCommandError (code, out, err) =
    [CommandOut out, CommandErr err, CommandExitCode code]
