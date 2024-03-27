{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}
{-# OPTIONS -ddump-minimal-imports #-}

module Data.FileCache.FileCache
  ( HasFilePath(toFilePath)
  , fileCachePath
  , fileCachePathIO
  -- , FileCacheT, runFileCacheT, evalFileCacheT, execFileCacheT
  , cacheLook, cacheDelete, cacheMap
  , cachePut, cachePut_
  ) where

-- import Control.Lens ( _1, _2, view )
-- import Control.Monad.RWS ( RWST(runRWST) )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Acid ( query, update, AcidState )
import Data.FileCache.FileCacheTop
import Data.FileCache.Acid
import Data.FileCache.CacheMap
import Data.FileCache.File
import Data.FileCache.FileError
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.Set as Set ( Set )
import Data.Text as T ( unpack )
import GHC.Stack (callStack, HasCallStack)
import SeeReason.UIO (liftUIO)
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), makeRelative, takeDirectory )
import Web.Routes ( toPathInfo )

-- * FileCacheTop

-- | The common suffix of the path to the image URI and its server
-- FilePath.
class HasFilePath a where
  toFilePath :: a -> FilePath
  toURIDir :: a -> FilePath

instance HasFilePath (Checksum, FileType) where
  toURIDir (csum, _typ) = take 2 $ unpack csum
  toFilePath p@(csum, typ) =
     toURIDir p </> unpack (csum <> fileExtension typ)

instance HasFilePath ImagePath where
  toURIDir (ImagePath (ImageOriginal csum typ)) = toURIDir (csum, typ)
  toURIDir (ImagePath (ImageUpright key)) = toURIDir (ImagePath key)
  toURIDir (ImagePath (ImageScaled _ _ key)) = toURIDir (ImagePath key)
  toURIDir (ImagePath (ImageCropped _ key)) = toURIDir (ImagePath key)
  -- for backwards compatibility, special case ImageOriginal
  toFilePath (ImagePath (ImageOriginal csum typ)) = toFilePath (csum, typ)
  toFilePath p = toURIDir p </> makeRelative "/" (unpack (toPathInfo p))

instance HasFilePath ImageCached where
  toURIDir c = toURIDir (imagePath c)
  toFilePath c@(ImageCached _ _) = toFilePath (imagePath c)

instance HasFilePath ImageKey where
  toURIDir key = toURIDir (ImagePath key)
  toFilePath key = toFilePath (ImagePath key)

-- | The full path name for the local cache of the file.
fileCachePath ::
  (HasFilePath a, MonadReader r m, HasFileCacheTop r, HasCallStack)
  => a -> m FilePath
fileCachePath file = do
  (FileCacheTop top) <- fileCacheTop <$> ask
  let path = toFilePath file
  return $ top </> makeRelative "/" path
  where _ = callStack

-- | Create any missing directories and evaluate 'fileCachePath'
fileCachePathIO ::
  (MonadFileCache r e m, HasFilePath a, HasCallStack)
  => a -> m FilePath
fileCachePathIO file = do
  path <- fileCachePath file
  let dir = takeDirectory path
  liftUIO (createDirectoryIfMissing True dir)
  return path

-- * FileCacheT

#if 0
type FileCacheT r s m = RWST r () s m

runFileCacheT :: r -> s -> FileCacheT r s m a -> m (a, s, ())
runFileCacheT r s0 action = runRWST action r s0

evalFileCacheT :: Functor m => r -> s -> FileCacheT r s m a -> m a
evalFileCacheT r s0 action = view _1 <$> runFileCacheT r s0 action
execFileCacheT :: Functor m => r -> s -> FileCacheT r s m a-> m s
execFileCacheT r s0 action = view _2 <$> runFileCacheT r s0 action
#endif

askCacheAcid :: (MonadReader r m, HasCacheAcid r, HasCallStack) => m CacheAcid
askCacheAcid = cacheAcid <$> ask
  where _ = callStack

-- | insert a (key, value) pair into the cache
cachePut ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => ImageKey -> Either FileError ImageFile -> m (Either FileError ImageFile)
cachePut key val = do
  st <- askCacheAcid
  liftUIO (update st (PutValue key val))
  return val

-- | insert a (key, value) pair into the cache, discard result.
cachePut_ ::
  (MonadFileCache r e m, HasCallStack)
  => ImageKey -> Either FileError ImageFile -> m ()
cachePut_ key val = do
  st <- askCacheAcid
  liftUIO (update st (PutValue key val))

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  forall e r m. (MonadFileCache r e m, HasCallStack)
  => ImageKey -> m (Maybe (Either FileError ImageFile))
cacheLook key = do
  st <- askCacheAcid
  {-either (Just . Left) (fmap id) <$>-}
  liftUIO $ query st (LookValue key)

cacheMap :: (MonadFileCache r e m, HasCallStack) => m CacheMap
cacheMap = do
  st <- askCacheAcid
  liftUIO (query st LookMap)

cacheDelete ::
  forall e r m. (MonadFileCache r e m, HasCallStack)
  => Proxy ImageFile -> Set ImageKey -> m ()
cacheDelete _ keys = do
  (st :: AcidState CacheMap) <- cacheAcid <$> ask
  liftUIO (update st (DeleteValues keys))
  where _ = callStack
