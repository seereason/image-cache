{-# LANGUAGE DeriveAnyClass #-}

module Data.FileCache.MonadFileCache
  ( -- * Monad transformer
    FileCacheT, W(W), S(S)
  , ensureFileCacheTop
  , runFileCacheT, evalFileCacheT, execFileCacheT, writeFileCacheT
    -- * Monad class
  , MonadFileCache(askCacheAcid, buildCacheValue)
  , cacheInsert, cacheLook, cacheMap, cacheDelete, cacheMiss
  , FileCacheTop(..)
  ) where

import Control.Lens (_1, _2, _3, view)
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Trans (lift, MonadTrans(lift))
import Data.Acid
import Data.FileCache.Acid
import Data.FileCache.Cache (CacheMap, CacheValue, FileCacheTop(..), fileCacheTop, HasFileCacheTop)
import Data.FileCache.FileError (FileError)
import Data.Proxy (Proxy)
import Data.SafeCopy
import Data.Set (Set)
import Data.Typeable
import Extra.Except -- (MonadIOError(liftIOError))
import System.Directory (createDirectoryIfMissing)

type FileCacheT key val m = RWST (AcidState (CacheMap key val), FileCacheTop) W S (ExceptT FileError m)

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

--instance MonadIOError FileError (ExceptT FileError m) where
--  liftIOError io = try 

ensureFileCacheTop :: MonadIOError e m => FileCacheT key val m ()
ensureFileCacheTop = do
  fileCacheTop >>= lift . lift . liftIOError . createDirectoryIfMissing True . _unFileCacheTop

-- No MonadIO constraint here - not all MonadFileCache operations require
-- MonadIO, and we might want to use MonadIOError instead.
class (HasFileCacheTop m,
       Ord key, SafeCopy key, Typeable key, Show key,
       SafeCopy val, Typeable val) => MonadFileCache key val m where
    askCacheAcid :: m (AcidState (CacheMap key val))
    buildCacheValue :: MonadIOError FileError m => key -> m (CacheValue val)

-- | Call the build function on cache miss to build the value.
cacheInsert ::
  forall key val m. (MonadFileCache key val m, MonadIOError FileError m)
  => key -> m (Cached (CacheValue val))
cacheInsert key = do
  st <- askCacheAcid
  liftIOError (query st (LookValue key)) >>= maybe (cacheMiss key) return

cacheMiss ::
  forall key val m. (MonadFileCache key val m, MonadIOError FileError m)
  => key -> m (Cached (CacheValue val))
cacheMiss key = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val))
  val <- buildCacheValue key
  liftIOError $ update st (PutValue key val)

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  (MonadFileCache key val m, MonadIOError FileError m)
  => key -> m (Maybe (Cached (CacheValue val)))
cacheLook key = do
  st <- askCacheAcid
  liftIOError $ query st (LookValue key)

cacheMap ::
  (MonadFileCache key val m, MonadIOError FileError m)
  => m (Cached (CacheMap key val))
cacheMap = do
  st <- askCacheAcid
  liftIOError $ query st LookMap

cacheDelete ::
  forall key val m. (MonadFileCache key val m, MonadIOError FileError m)
  => Proxy (val, FileError) -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val)) <- askCacheAcid
  liftIOError $ update st (DeleteValues keys)
