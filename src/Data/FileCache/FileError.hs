-- | Error type for cached file manipulation.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass, DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Data.FileCache.FileError
    ( FileError(..)
    , CommandInfo(..)
    , HasFileError(fromFileError)
    , logErrorCall
    ) where

import Control.Exception as E (ErrorCall(ErrorCallWithLocation), fromException, SomeException)
--import Control.Monad.Catch (try)
--import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Trans (MonadIO(liftIO))
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Data (Data)
import Data.FileCache.Except -- (HasIOException(fromIOException), liftIOError)
import Data.FileCache.LogException (Loggable(logit))
import Data.SafeCopy (SafeCopy(version), safeGet, safePut)
import Data.Serialize (Serialize(..))
import Data.Text (pack, Text, unpack)
import Extra.Serialize ({-instance Serialize Text-})
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Loc(loc_module))
import System.Log.Logger (logM, Priority(ERROR))

-- | It would be nice to store the actual IOException and E.ErrorCall,
-- but then the FileError type wouldn't be serializable.
data FileError
    = IOException {-IOException-} Text -- ^ Caught an IOException
    | ErrorCall {-E.ErrorCall-} Text -- ^ Caught a call to error
    | CommandFailure CommandInfo -- ^ A shell command failed
    | CacheDamage -- ^ The contents of a cache file are wrong
    deriving (Eq, Ord, Generic)

-- | This ensures that runExceptT catches IOException
instance HasIOException FileError where fromIOException = IOException . pack . show

class HasIOException e => HasFileError e where fromFileError :: FileError -> e
instance HasFileError FileError where fromFileError = id

-- | Information about a shell command that failed.  This is
-- recursive so we can include as much or as little as desired.
data CommandInfo
    = Command Text Text -- ^ CreateProcess and ExitCode
    | CommandInput P.ByteString CommandInfo -- ^ command input
    | CommandOut P.ByteString CommandInfo -- ^ stdout
    | CommandErr P.ByteString CommandInfo -- ^ stderr
    | FunctionName String CommandInfo -- ^ The function that ran the command
    | Description String CommandInfo -- ^ free form description of what happened
    deriving (Eq, Ord, Generic)

instance Loggable FileError where
  logit priority loc (IOException e) = (logM (loc_module loc) priority (" - IO exception: " <> unpack e))
  logit priority loc (ErrorCall e) = (logM (loc_module loc) priority (" - error call: " <> show e))
  logit priority loc (CommandFailure info) = (logM (loc_module loc) priority " - shell command failed:" >> logCommandInfo priority loc info)
  logit priority loc CacheDamage = logM (loc_module loc) priority " - file cache is damaged"

logCommandInfo :: Priority -> Loc -> CommandInfo -> IO ()
logCommandInfo priority loc (Description s e) = logM (loc_module loc) priority (" - error description: " <> s) >> logCommandInfo priority loc e
logCommandInfo priority loc (FunctionName n e) = logM (loc_module loc) priority (" - error function " <> n) >> logCommandInfo priority loc e
logCommandInfo priority loc (Command cmd code) = logM (loc_module loc) priority (" - command: " <> show cmd <> ", exit code: " <> show code)
logCommandInfo priority loc (CommandInput bs e) = logM (loc_module loc) priority (" - command input: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e
logCommandInfo priority loc (CommandOut bs e) = logM (loc_module loc) priority (" - command stdout: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e
logCommandInfo priority loc (CommandErr bs e) = logM (loc_module loc) priority (" - command stderr: " <> show (P.take 1000 bs)) >> logCommandInfo priority loc e

logErrorCall :: MonadIO m => m (Either SomeException a) -> m (Either SomeException a)
logErrorCall x =
    x >>= either (\e -> case fromException e :: Maybe E.ErrorCall of
                          Just (ErrorCallWithLocation msg loc) ->
                              liftIO (logM "Appraisal.FileError" ERROR (show loc ++ ": " ++ msg)) >> return (Left e)
                          _ -> return (Left e)) (return . Right)

#if 0
$(deriveSafeCopy 1 'base ''CommandInfo)
$(deriveSafeCopy 1 'base ''FileError)
#else
instance SafeCopy CommandInfo where version = 1
instance SafeCopy FileError where version = 1
#endif

instance Serialize CommandInfo where get = safeGet; put = safePut
instance Serialize FileError where get = safeGet; put = safePut

deriving instance Data FileError
deriving instance Data CommandInfo
deriving instance Show FileError
deriving instance Show CommandInfo
