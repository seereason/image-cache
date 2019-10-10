module Data.FileCache.Monad
  ( FileCacheT
  , MonadFileCache(askCacheAcid, buildCacheValue)
  , cacheInsert, cacheLook, cacheMap, cacheDelete
  ) where

import Control.Monad.Reader
import Data.Acid
import Data.FileCache.Acid
import Data.FileCache.Cache (CacheMap, CacheValue)
import Data.FileCache.FileError (FileError)
import Data.FileCache.Types (FileCacheTop)
import Data.SafeCopy
import Data.Set (Set)
import Data.Typeable

type FileCacheT key val m = ReaderT (AcidState (CacheMap key val FileError), FileCacheTop) m

-- | Note that class 'HasCache' and the 'cacheInsert' function return
-- values containing a 'FileError', but the monad m only has the
-- constraint HasFileError.
class (Ord key, SafeCopy key, Typeable key, Show key,
       SafeCopy val, Typeable val,
       SafeCopy err, Typeable err, MonadIO m) => MonadFileCache key val err m where
    askCacheAcid :: m (AcidState (CacheMap key val err))
    buildCacheValue :: key -> m (CacheValue err val)

-- | Call the build function on cache miss to build the value.
cacheInsert :: forall key val err m. (MonadFileCache key val err m) => key -> m (CacheValue err val)
cacheInsert key = do
  st <- askCacheAcid
  liftIO (query st (LookValue key)) >>= maybe (cacheMiss key) return

cacheMiss :: forall key val err m. (MonadFileCache key val err m) => key -> m (CacheValue err val)
cacheMiss key = do
  st <- askCacheAcid :: m (AcidState (CacheMap key val err))
  val <- buildCacheValue key
  () <- liftIO $ update st (PutValue key val)
  return val

-- | Query the cache, but do nothing on cache miss.
cacheLook :: MonadFileCache key val err m => key -> m (Maybe (CacheValue err val))
cacheLook key = do
  st <- askCacheAcid
  liftIO $ query st (LookValue key)

cacheMap :: MonadFileCache key val err m => m (CacheMap key val err)
cacheMap = do
  st <- askCacheAcid
  liftIO $ query st LookMap

cacheDelete :: forall key val err m. (MonadFileCache key val err m) => Proxy (val, err) -> Set key -> m ()
cacheDelete _ keys = do
  (st :: AcidState (CacheMap key val err)) <- askCacheAcid
  liftIO $ update st (DeleteValues keys)
