#if !__GHCJS__ && !defined(javascript_HOST_ARCH)
{-# LANGUAGE ImplicitParams #-}

module Data.FileCache.Monads
  ( MonadFileCacheType
  , MonadFileCache
  , MonadFileCacheBG
  , MonadFileCacheWriter
  ) where

import Control.Exception (IOException, SomeException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (ExceptT, MonadError, MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.RWS.Strict (RWST)
import Data.FileCache.Background (HasTaskQueue)
import Data.FileCache.FileCacheTop (HasFileCacheTop)
import Data.FileCache.FileError (FileError)
import Data.FileCache.ImageKey (ImageKey)
import Data.Set (Set)
import Extra.Lens (HasLens)
import SeeReason.Errors (ConvertError, Member, OneOf)

import Data.FileCache.FileCacheTop (HasCacheAcid)

type MonadFileCacheType r e m =
  (MonadIO m,
   MonadCatch m,
   MonadError (OneOf e) m,
   ConvertError SomeException (Either SomeException (OneOf e)),
   Member IOException e,
   Member FileError e,
   MonadReader r m,
   HasCacheAcid r,
   HasFileCacheTop r)

class MonadFileCacheType r e m => MonadFileCache r e m

type MonadFileCacheBG r s e m task =
  (MonadFileCache r e m, ?task :: ImageKey -> task,
   MonadState s m,
   HasLens s (Set task),
    -- Storage where this server thread can record the status of tasks
    -- we are interested in.
   HasTaskQueue task r)

-- | For code that can add things to the cache
class MonadFileCache r e m => MonadFileCacheWriter r e m

instance (MonadIO m, MonadCatch m, ConvertError SomeException (Either SomeException (OneOf e)), Member FileError e, Member IOException e, HasCacheAcid r, HasFileCacheTop r) => MonadFileCache r e (ReaderT r (ExceptT (OneOf e) m))
instance (MonadIO m, MonadCatch m, ConvertError SomeException (Either SomeException (OneOf e)), Member FileError e, Member IOException e, HasCacheAcid r, HasFileCacheTop r) => MonadFileCacheWriter r e (ReaderT r (ExceptT (OneOf e) m))
instance (Monoid w, MonadIO m, MonadCatch m, ConvertError SomeException (Either SomeException (OneOf e)), Member FileError e, Member IOException e, HasCacheAcid r, HasFileCacheTop r) => MonadFileCache r e (RWST r w s (ExceptT (OneOf e) m))
#endif
