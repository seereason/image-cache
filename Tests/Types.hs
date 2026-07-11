{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TupleSections, TypeApplications, TypeFamilies #-}

module Types where

import Control.Exception ( fromException, IOException, SomeException )
import Control.Monad (msum)
import Control.Monad.Catch ( bracket )
import Control.Monad.Except ( ExceptT, runExceptT )
import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.Trans (MonadIO(liftIO))
import Data.Acid ( AcidState, openLocalStateFrom, closeAcidState )
import Data.FileCache ( CacheMap(CacheMap), FileCacheTop(FileCacheTop), FileError )
import Data.Typeable (typeOf)
import SeeReason.Errors as Err ( ConvertError(..), OneOf, Put1(put1) )
import System.FilePath ( (</>) )
import System.IO (stderr)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger (rootLoggerName, setHandlers, setLevel, Priority, updateGlobalLogger)

type ES = '[IOException, FileError, SomeException]

instance ConvertError SomeException (Either SomeException (OneOf ES)) where
  convertError e =
    maybe (Left e) Right $
      msum @[] [fmap put1 (fromException e :: Maybe IOException),
                fmap put1 (fromException e :: Maybe FileError)]

-- | A monad for running the image cache acid state.
type AcidT m = ReaderT (AcidState CacheMap, FileCacheTop) (ExceptT (OneOf ES) m)

runAcidT :: (r ~ (AcidState CacheMap, FileCacheTop)) => r -> AcidT IO a -> IO (Either (OneOf ES) a)
runAcidT r m = runExceptT $ runReaderT m r

runAcidT_ :: (r ~ (AcidState CacheMap, FileCacheTop)) => r -> AcidT IO a -> IO a
runAcidT_ r m = runAcidT r m >>= either (\e -> error ("e=" <> show e <> " :: " <> show (typeOf e))) pure

-- | Open the image cache acid state.
withImageCache :: FilePath -> ((AcidState CacheMap, FileCacheTop) -> IO r) -> IO r
withImageCache cache f =
  bracket
    (openLocalStateFrom
      (cache </> "_state")
      (CacheMap mempty mempty))
    closeAcidState
    (f . (, FileCacheTop cache))

-- | Set up logging so it writes to stderr.
withLogging :: MonadIO m => Priority -> m a -> m a
withLogging lvl io = do
  applog <- liftIO $ streamHandler stderr lvl
  liftIO $ updateGlobalLogger rootLoggerName (setLevel lvl . setHandlers [applog])
  io
