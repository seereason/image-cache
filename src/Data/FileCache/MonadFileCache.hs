{-# LANGUAGE DeriveAnyClass #-}

module Data.FileCache.MonadFileCache
  ( -- * Monad transformer
    FileCacheT, W(W), S(S)
  , ensureFileCacheTop
  , runFileCacheT, evalFileCacheT, execFileCacheT, writeFileCacheT
  , runFileCacheIOT, evalFileCacheIOT, execFileCacheIOT, writeFileCacheIOT
  , runFileCacheIOErrorT, evalFileCacheIOErrorT, execFileCacheIOErrorT, writeFileCacheIOErrorT
    -- * Monad class
  , MonadFileCache(askCacheAcid, buildCacheValue)
  , cacheInsert, cacheLook, cacheMap, cacheDelete, cacheMiss
  , FileCacheTop(..)
  ) where

import Control.Lens (_1, _2, _3, view)
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Trans (lift, MonadIO(..), MonadTrans(lift))
import Data.Acid
import Data.FileCache.Acid
import Data.FileCache.Cache (CacheMap, CacheValue, FileCacheTop(..), fileCacheTop, HasFileCacheTop)
import Data.FileCache.Except -- (MonadIOError(liftIOError))
import Data.FileCache.FileError (FileError, HasFileError)
import Data.Proxy (Proxy)
import Data.SafeCopy
import Data.Set (Set)
import Data.Typeable
import System.Directory (createDirectoryIfMissing)

type FileCacheT key val m = RWST (AcidState (CacheMap key val FileError), FileCacheTop) W S (ExceptT FileError m)

data W = W
instance Semigroup W where W <> W = W
instance Monoid W where mempty = W; mappend = (<>)
data S = S

runFileCacheT ::
     acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W S m a
  -> m (a, S, W)
runFileCacheT r0 top action = runRWST action (r0, top) S

evalFileCacheT r0 top action = view _1 <$> runFileCacheT r0 top action
execFileCacheT r0 top action = view _2 <$> runFileCacheT r0 top action
writeFileCacheT r0 top action = view _3 <$> runFileCacheT r0 top action

-- | runFileCacheT with a MonadIO constraint.
runFileCacheIOT ::
  (HasFileError e, MonadError e m, MonadIO m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W S m a
  -> m (a, S, W)
runFileCacheIOT r0 top action = runRWST action (r0, top) S
evalFileCacheIOT r0 top action = view _1 <$> runFileCacheIOT r0 top action
execFileCacheIOT r0 top action = view _2 <$> runFileCacheIOT r0 top action
writeFileCacheIOT r0 top action = view _3 <$> runFileCacheIOT r0 top action

-- | runFileCacheT with a MonadIOError constraint.
runFileCacheIOErrorT ::
  (HasFileError e, MonadIOError e m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W S m a
  -> m (a, S, W)
runFileCacheIOErrorT r0 top action = runRWST action (r0, top) S
evalFileCacheIOErrorT r0 top action = view _1 <$> runFileCacheIOErrorT r0 top action
execFileCacheIOErrorT r0 top action = view _2 <$> runFileCacheIOErrorT r0 top action
writeFileCacheIOErrorT r0 top action = view _3 <$> runFileCacheIOErrorT r0 top action

--instance MonadIOError FileError (ExceptT FileError m) where
--  liftIOError io = try 

ensureFileCacheTop :: MonadIOError e m => FileCacheT key val m ()
ensureFileCacheTop = do
  fileCacheTop >>= lift . lift . liftIOError . createDirectoryIfMissing True . _unFileCacheTop

-- No MonadIO constraint here - not all MonadFileCache operations require
-- MonadIO, and we might want to use MonadIOError instead.
class (HasFileCacheTop m,
       Ord key, SafeCopy key, Typeable key, Show key,
       SafeCopy val, Typeable val,
       SafeCopy err, Typeable err) => MonadFileCache key val err m where
    askCacheAcid :: m (AcidState (CacheMap key val err))
    buildCacheValue :: MonadIOError FileError m => key -> m (CacheValue err val)

-- | Call the build function on cache miss to build the value.
cacheInsert ::
  forall key val err m. (MonadFileCache key val err m, MonadIOError FileError m)
  => key -> m (Cached (CacheValue err val))
cacheInsert key = do
  st <- askCacheAcid
  liftIOError (query st (LookValue key)) >>= maybe (cacheMiss key) return

cacheMiss ::
  forall key val err m. (MonadFileCache key val err m, MonadIOError FileError m)
  => key -> m (Cached (CacheValue err val))
cacheMiss key = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val err))
  val <- buildCacheValue key
  liftIOError $ update st (PutValue key val)

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  (MonadFileCache key val err m, MonadIOError FileError m)
  => key -> m (Maybe (Cached (CacheValue err val)))
cacheLook key = do
  st <- askCacheAcid
  liftIOError $ query st (LookValue key)

cacheMap ::
  (MonadFileCache key val err m, MonadIOError FileError m)
  => m (Cached (CacheMap key val err))
cacheMap = do
  st <- askCacheAcid
  liftIOError $ query st LookMap

cacheDelete ::
  forall key val err m. (MonadFileCache key val err m, MonadIOError FileError m)
  => Proxy (val, err) -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val err)) <- askCacheAcid
  liftIOError $ update st (DeleteValues keys)
