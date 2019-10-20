{-# LANGUAGE DeriveAnyClass, TemplateHaskell #-}
{-# OPTIONS -ddump-minimal-imports #-}

module Data.FileCache.MonadFileCache
  ( -- * Monad transformer
    FileCacheT, W(W)
  , ensureFileCacheTop
  , runFileCacheT, evalFileCacheT, execFileCacheT, writeFileCacheT
    -- * Monad class
  , MonadFileCache(askCacheAcid, buildCacheValue)
  , cacheInsert, cacheLook, cacheMap, cacheDelete, cacheMiss
  , FileCacheTop(..)
  ) where

import Control.Lens ( _1, _2, _3, view )
import Control.Monad.Except ( ExceptT )
import Control.Monad.RWS ( RWST(runRWST) )
import Control.Monad.Trans ( MonadTrans(lift) )
import Data.Acid ( query, update, AcidState )
import Data.FileCache.Acid ( Cached, DeleteValues(DeleteValues), LookMap(LookMap), LookValue(LookValue), PutValue(PutValue) )
import Data.FileCache.Cache ( CacheMap, CacheValue, FileCacheTop(..), fileCacheTop, HasFileCacheTop )
import Data.FileCache.FileError ( FileError, HasFileError )
import Data.Proxy ( Proxy )
import Data.SafeCopy ( SafeCopy )
import Data.Set ( Set )
import Data.Typeable ( Typeable )
import Extra.Except ( MonadIOError(..) )
import System.Directory ( createDirectoryIfMissing )
import qualified Data.ByteString.Lazy as P ()
import qualified Data.ByteString.UTF8 as P ()

type FileCacheT key val s m = RWST (AcidState (CacheMap key val), FileCacheTop) W s (ExceptT FileError m)

data W = W
instance Semigroup W where W <> W = W
instance Monoid W where mempty = W; mappend = (<>)

runFileCacheT ::
     acid
  -> FileCacheTop
  -> s
  -> RWST (acid, FileCacheTop) W s m a
  -> m (a, s, W)
runFileCacheT r0 top s0 action = runRWST action (r0, top) s0

evalFileCacheT ::
  Functor m
  => acid
  -> FileCacheTop
  -> s
  -> RWST (acid, FileCacheTop) W s m a
  -> m a
evalFileCacheT r0 top s0 action = view _1 <$> runFileCacheT r0 top s0 action
execFileCacheT r0 top s0 action = view _2 <$> runFileCacheT r0 top s0 action
writeFileCacheT r0 top s0 action = view _3 <$> runFileCacheT r0 top s0 action

ensureFileCacheTop :: MonadIOError e m => FileCacheT key val s m ()
ensureFileCacheTop = do
  fileCacheTop >>= lift . lift . liftIOError . createDirectoryIfMissing True . _unFileCacheTop

-- No MonadIO constraint here - not all MonadFileCache operations require
-- MonadIO, and we might want to use MonadIOError instead.
class (HasFileCacheTop m,
       Ord key, SafeCopy key, Typeable key, Show key,
       SafeCopy val, Typeable val) => MonadFileCache key val m where
    askCacheAcid :: m (AcidState (CacheMap key val))
    buildCacheValue :: (MonadIOError e m, HasFileError e) => key -> m (CacheValue val)

-- | Call the build function on cache miss to build the value.
cacheInsert ::
  forall key val e m. (MonadFileCache key val m, MonadIOError e m, HasFileError e)
  => key -> m (Cached (CacheValue val))
cacheInsert key = do
  st <- askCacheAcid
  liftIOError (query st (LookValue key)) >>= maybe (cacheMiss key) return

cacheMiss ::
  forall key val e m. (MonadFileCache key val m, MonadIOError e m, HasFileError e)
  => key -> m (Cached (CacheValue val))
cacheMiss key = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val))
  val <- buildCacheValue key
  liftIOError $ update st (PutValue key val)

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  (MonadFileCache key val m, MonadIOError e m)
  => key -> m (Maybe (Cached (CacheValue val)))
cacheLook key = do
  st <- askCacheAcid
  liftIOError $ query st (LookValue key)

cacheMap ::
  (MonadFileCache key val m, MonadIOError e m)
  => m (Cached (CacheMap key val))
cacheMap = do
  st <- askCacheAcid
  liftIOError $ query st LookMap

cacheDelete ::
  forall key val e m. (MonadFileCache key val m, MonadIOError e m)
  => Proxy (val, FileError) -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val)) <- askCacheAcid
  liftIOError $ update st (DeleteValues keys)
