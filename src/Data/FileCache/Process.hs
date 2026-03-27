{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Process
  ( readCreateProcessWithExitCode'
  ) where

import Data.FileCache.LogException ( logException )
import Data.FileCache.Orphans ()
import Prelude hiding (show)
import System.Exit ( ExitCode(..) )
import System.Log.Logger ( Priority(..) )
import System.Process ( CreateProcess )
import System.Process.ByteString.Lazy as LBS ()
import System.Process.ListLike as LL ( readCreateProcessWithExitCode, ListLikeProcessIO )

-- * Processes and IO

readCreateProcessWithExitCode' :: ListLikeProcessIO a c => CreateProcess -> a -> IO (ExitCode, a, a)
readCreateProcessWithExitCode' p s =
    $logException ERROR (LL.readCreateProcessWithExitCode p s)
