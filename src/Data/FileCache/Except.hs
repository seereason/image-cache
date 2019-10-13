-- | Force IO exceptions into ExceptT.

{-# LANGUAGE DefaultSignatures, TemplateHaskell, UndecidableInstances #-}

module Data.FileCache.Except where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State
--import Control.Monad.Trans
import Control.Monad.Writer
import Data.FileCache.Cache (CacheValue(Failed))
import Data.FileCache.LogException (logException)
import Data.Typeable (typeOf)
import Extra.Except (handleError, tryError)
import System.Log.Logger (logM, Priority(ERROR))

-- | In order to guarantee IOException is caught, do NOT include the
-- standard 'HasIOException' instance
--
-- > instance HasIOException IOException where fromIOException = id
--
-- Because there is an @instance MonadError IOException IO@ in
-- @Control.Monad.Except@, "thrown" IOexceptions are not be caught by
-- 'runExceptT':
--
-- >>> runExceptT (liftIO (readFile "/etc/nonexistant") :: ExceptT IOException IO String)
-- *** Exception: /etc/nonexistant: openFile: does not exist (No such file or directory)
--
--  (*** means the exception reached the top level.)  However, if we
-- use 'liftIOError' to implement a version of 'readFile' that has a
-- 'MonadIOError' constraint:
--
-- >>> let readFile' path = liftIOError (readFile path)
-- >>> :type readFile'
-- readFile' :: MonadIOError e m => FilePath -> m String
--
-- and then create a suitable error type
--
-- >>> newtype Error = Error IOException deriving Show
-- >>> instance HasIOException Error where fromIOException = Error
--
-- Now the thrown 'IOException' will always be caught and lifted into
-- the 'MonadError':
--
-- >>> runExceptT (readFile' "/etc/nonexistant" :: ExceptT Error IO String)
-- Left (Error /etc/nonexistant: openFile: does not exist (No such file or directory))
class HasIOException e where
  fromIOException :: IOException -> e

class (HasIOException e, MonadError e m) => MonadIOError e m where
  liftIOError :: IO a -> m a

tryIOError :: MonadIOError e m => IO a -> m (Either e a)
tryIOError = tryError . liftIOError

instance (HasIOException e, MonadIO m) => MonadIOError e (ExceptT e m) where
  liftIOError io = liftIO (try io) >>= either (throwError . fromIOException) return

#if 1
-- This overlaps the instance above, so...
instance {-# Overlappable #-} (MonadIOError e m, MonadError e (t m), MonadTrans t) => MonadIOError e (t m) where
  liftIOError = lift . liftIOError
  -- runIOError action = undefined
  -- mapIOError f action = undefined
#else
instance MonadIOError e m => MonadIOError e (ReaderT r m) where
  liftIOError = lift . liftIOError
instance MonadIOError e m => MonadIOError e (StateT s m) where
  liftIOError = lift . liftIOError
instance (MonadIOError e m, Monoid w) => MonadIOError e (WriterT w m) where
  liftIOError = lift . liftIOError
instance (MonadIOError e m, Monoid w) => MonadIOError e (RWST r w s m) where
  liftIOError = lift . liftIOError
#endif

newtype Error = Error IOException
instance Show Error where show (Error e) = "(Error " <> show (show e) <> ")"
instance HasIOException Error where fromIOException = Error

readFile' :: MonadIOError e m => FilePath -> m String
readFile' path = liftIOError (readFile path)

test :: IO ()
test = do
  r <- runExceptT (readFile' "/etc/nonexistant" :: ExceptT Error IO String)
  putStrLn (show r <> " :: " <> show (typeOf r))

#if 0
logIOError :: (MonadIOError e m, Show e) => String -> m a -> m a
logIOError modul action =
  mapIOError
    (either
      (\e -> liftIOError (logM modul ERROR (show e)) >> return (Left e))
      (return . Right))
    action
#endif

logIOError :: MonadIOError e m => m a -> m a
logIOError = handleError (\e -> liftIOError ($logException ERROR (pure e)) >> throwError e)
