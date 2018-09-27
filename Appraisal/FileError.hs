-- | Maintain a cache of files.
--
-- A data structure representing a local cache of a data file.  The
-- cached file persists across runs of our application, and can be
-- accessed by name and passed to software which requires a file, for
-- example a document formatter such as LaTeX.  The original data can
-- be supplied as either a URI, a local file path, or as a ByteString.
-- The file is then downloaded and stored on the local machine at a
-- location based on the file's checksum.

{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}

module Appraisal.FileError
    ( FileError(..)
    , FileErrorT
    , runFileErrorT
    , CommandInfo(..)
    , IsFileError(fromFileError)
    , logFileError
    , logErrorCall
    ) where

import Control.Exception as E (ErrorCall(ErrorCallWithLocation), fromException, IOException, SomeException, throw, try)
import Control.Monad (msum)
import Control.Monad.Except (ExceptT(ExceptT), MonadError(catchError, throwError), runExceptT)
import Control.Monad.Trans (MonadIO(liftIO), MonadTrans(lift))
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Data (Data)
import Debug.Show (V(V))
import Data.Text (pack, Text, unpack)
import System.Log.Logger ( logM, Priority(ERROR) )

-- | It would be nice to store the actual IOException and E.ErrorCall,
-- but then the FileError type wouldn't be serializable.
data FileError
    = IOException {-IOException-} Text -- ^ Caught an IOException
    | ErrorCall {-E.ErrorCall-} Text -- ^ Caught a call to error
    | CommandFailure CommandInfo -- ^ A shell command failed
    | CacheDamage -- ^ The contents of a cache file are wrong
    deriving (Data, Eq, Ord, Show)

-- | Information about a shell command that failed.  This is
-- recursive so we can include as much or as little as desired.
data CommandInfo
    = Command Text Text -- ^ CreateProcess and ExitCode
    | CommandInput P.ByteString CommandInfo -- ^ command input
    | CommandOut P.ByteString CommandInfo -- ^ stdout
    | CommandErr P.ByteString CommandInfo -- ^ stderr
    | FunctionName String CommandInfo -- ^ The function that ran the command
    | Description String CommandInfo -- ^ free form description of what happened
    deriving (Data, Eq, Ord, Show)

class IsFileError e where fromFileError :: FileError -> e
instance IsFileError FileError where fromFileError = id

instance Show (V FileError) where show (V x) = show x

logFileError :: String -> FileError -> IO ()
logFileError prefix (IOException e) = logM prefix ERROR (" - IO exception: " <> unpack e)
logFileError prefix (ErrorCall e) = logM prefix ERROR (" - error call: " <> show e)
logFileError prefix (CommandFailure info) = logM prefix ERROR " - shell command failed:" >> logCommandInfo prefix info
logFileError prefix CacheDamage = logM prefix ERROR " - file cache is damaged"

logCommandInfo :: String -> CommandInfo -> IO ()
logCommandInfo prefix (Description s e) = logM prefix ERROR (" - error description: " <> s) >> logCommandInfo prefix e
logCommandInfo prefix (FunctionName n e) = logM prefix ERROR (" - error function " <> n) >> logCommandInfo prefix e
logCommandInfo prefix (Command cmd code) = logM prefix ERROR (" - command: " <> show cmd <> ", exit code: " <> show code)
logCommandInfo prefix (CommandInput bs e) = logM prefix ERROR (" - command input: " <> show (P.take 1000 bs)) >> logCommandInfo prefix e
logCommandInfo prefix (CommandOut bs e) = logM prefix ERROR (" - command stdout: " <> show (P.take 1000 bs)) >> logCommandInfo prefix e
logCommandInfo prefix (CommandErr bs e) = logM prefix ERROR (" - command stderr: " <> show (P.take 1000 bs)) >> logCommandInfo prefix e

newtype FileErrorT m a = FileErrorT {unFileErrorT :: ExceptT FileError m a}
    deriving (Monad, Applicative, Functor)

instance MonadTrans FileErrorT where
    lift = FileErrorT . lift

instance Monad m => MonadError FileError (FileErrorT m) where
    throwError :: FileError -> FileErrorT m a
    throwError e = FileErrorT $ throwError e
    catchError :: FileErrorT m a -> (FileError -> FileErrorT m a) -> FileErrorT m a
    catchError (FileErrorT m) c = FileErrorT $ m `catchError` (unFileErrorT . c)

runFileErrorT :: FileErrorT m a -> m (Either FileError a)
runFileErrorT action = runExceptT (unFileErrorT action)

-- | Turn any caught IOException and ErrorCall into FileError.  Log
-- any intercepted IO errors.
instance MonadIO m => MonadIO (FileErrorT m) where
  liftIO =
    FileErrorT .
    (ExceptT :: m (Either FileError a) -> ExceptT FileError m a) .
    ((\io -> either (Left . toFileError) Right <$> io) :: m (Either SomeException a) -> m (Either FileError a)) .
    logErrorCall .
    liftIO .
    try

toFileError :: SomeException -> FileError
toFileError e =
    maybe (throw e)
          id
          (msum [fmap (IOException . pack . show) (fromException e :: Maybe IOException),
                 fmap (Appraisal.FileError.ErrorCall . pack . show) (fromException e :: Maybe E.ErrorCall)])

logErrorCall :: MonadIO m => m (Either SomeException a) -> m (Either SomeException a)
logErrorCall x =
    x >>= either (\e -> case fromException e :: Maybe E.ErrorCall of
                          Just (ErrorCallWithLocation msg loc) ->
                              liftIO (logM "Appraisal.FileError" ERROR (show loc ++ ": " ++ msg)) >> return (Left e)
                          _ -> return (Left e)) (return . Right)
