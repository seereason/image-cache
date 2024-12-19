-- | Serialize and SafeCopy instances for 'IOException' aka 'IOError',
-- and CreateProcess to allow us to embed these in our error union
-- types.  They clobber all Handle values because they are too exotic
-- and don't contain any information useful to error processing code.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS -Wno-orphans #-}
{-# OPTIONS -Werror=incomplete-patterns #-}
{-# OPTIONS -Werror=missing-fields #-}
{-# OPTIONS -Werror=missing-methods #-}
{-# OPTIONS -Werror=redundant-constraints #-}
{-# OPTIONS -Werror=unused-matches #-}
{-# OPTIONS -Werror=unused-top-binds #-}

module Data.FileCache.Orphans () where

import Control.Exception (ErrorCall(..))
import Data.ByteString.Lazy (ByteString)
import Data.Function (on)
import Data.SafeCopy
import Data.Serialize (Serialize(..))
import Extra.Except ({-instances only-})
import Foreign.C.Types (CInt(..))
import GHC.Generics (Generic)
import GHC.IO.Exception (IOException(IOError), IOErrorType(..))
import System.Exit (ExitCode)
import System.Posix (CGid(..), CUid(..))
import System.Process -- (CmdSpec(..), CreateProcess(..))

instance SafeCopy IOException where
  putCopy (IOError _ t l d e p) = putCopy (t, l, d, e, p)
  getCopy = contain $ do get1 <- getSafeGet
                         get2 <- getSafeGet
                         get3 <- getSafeGet
                         get4 <- getSafeGet
                         (t :: IOErrorType) <- get1
                         (l :: String) <- get2
                         (d :: String) <- get2
                         (e :: Maybe CInt) <- get3
                         (p :: Maybe FilePath) <- get4
                         return $ IOError Nothing t l d e p
instance Ord IOException where
  compare (IOError _ t1 l1 d1 e1 p1) (IOError _ t2 l2 d2 e2 p2) =
    compare (t1, l1, d1, e1, p1) (t2, l2, d2, e2, p2)

instance SafeCopy CInt
instance SafeCopy IOErrorType
deriving instance Ord IOErrorType

deriving instance Generic ErrorCall
instance SafeCopy ErrorCall

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

deriving instance Generic CreateProcess
deriving instance Generic CmdSpec
deriving instance Generic StdStream
deriving instance Generic CGid
deriving instance Generic CUid

deriving instance Ord CreateProcess
deriving instance Ord CmdSpec
instance Ord StdStream where
  compare = compare `on` stdStream

instance SafeCopy CreateProcess
instance SafeCopy CmdSpec
instance SafeCopy CGid
instance SafeCopy CUid
instance SafeCopy StdStream'
instance SafeCopy ExitCode

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

instance SafeCopy StdStream where
  putCopy = putCopy . stdStream
  getCopy = contain $ do f <- getSafeGet
                         (s :: StdStream') <- f
                         return $ stdStream' s
