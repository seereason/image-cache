{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators, UndecidableInstances #-}

module Data.FileCache.FileCache
  ( HasImageFilePath(toFilePath)
  , fileCachePath
  , fileCachePathIO
  , FileCacheT
  , runFileCacheT, evalFileCacheT, execFileCacheT
  , cacheLook, cacheDelete, cacheMap
  , cachePut, cachePut_
  ) where

import Control.Exception ( IOException )
import Control.Lens ( _1, _2, view )
import Control.Monad.RWS ( RWST(runRWST) )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Acid ( query, update, AcidState )
import Data.FileCache.FileCacheTop
import Data.FileCache.Acid
import Data.FileCache.CacheMap
import Data.FileCache.Common
import Data.Monoid ( (<>) )
import Data.Proxy ( Proxy )
import Data.Set as Set ( Set )
import Data.Text as T ( unpack )
import Extra.Errors ( liftUIO, Member, OneOf )
import Extra.Except ( MonadError, NonIOException )
import System.Directory ( createDirectoryIfMissing )
import System.FilePath ( (</>), makeRelative, takeDirectory )
import UnexceptionalIO.Trans ( Unexceptional )
import Web.Routes ( toPathInfo )

-- * FileCacheTop

-- | The common suffix of the path to the image URI and its server
-- FilePath.
class HasImageFilePath a where
  toFilePath :: a -> FilePath
  toURIDir :: a -> FilePath

instance HasImageFilePath (Checksum, ImageType) where
  toURIDir (csum, _typ) = take 2 $ unpack csum
  toFilePath p@(csum, typ) =
     toURIDir p </> unpack (csum <> fileExtension typ)

instance HasImageFilePath ImagePath where
  toURIDir (ImagePath (ImageOriginal csum typ)) = toURIDir (csum, typ)
  toURIDir (ImagePath (ImageUpright key)) = toURIDir (ImagePath key)
  toURIDir (ImagePath (ImageScaled _ _ key)) = toURIDir (ImagePath key)
  toURIDir (ImagePath (ImageCropped _ key)) = toURIDir (ImagePath key)
  -- for backwards compatibility, special case ImageOriginal
  toFilePath (ImagePath (ImageOriginal csum typ)) = toFilePath (csum, typ)
  toFilePath p = toURIDir p </> makeRelative "/" (unpack (toPathInfo p))

instance HasImageFilePath ImageCached where
  toURIDir c = toURIDir (imagePath c)
  toFilePath c@(ImageCached _ _) = toFilePath (imagePath c)

instance HasImageFilePath (ImageKey, ImageFile) where
  toURIDir (key, ImageFileShape shape) = toURIDir (key, shape)
  toURIDir (key, ImageFileReady ready) = toURIDir (key, ready)
  toFilePath (key, ImageFileShape shape) = toFilePath (key, shape)
  toFilePath (key, ImageFileReady ready) = toFilePath (key, ready)

instance HasImageFilePath (ImageKey, ImageReady) where
  toURIDir (key, ImageReady _ shape) = toURIDir (key, shape)
  toFilePath (key, ImageReady _ shape) = toFilePath (key, shape)

instance HasImageFilePath (ImageKey, ImageShape) where
  toURIDir (key, shape) = toURIDir (ImagePath key)
  toFilePath (key, shape) = toFilePath (ImagePath key)

-- | The full path name for the local cache of the file.
fileCachePath :: (HasImageFilePath a, MonadReader r m, HasFileCacheTop r) => a -> m FilePath
fileCachePath file = do
  (FileCacheTop top) <- fileCacheTop <$> ask
  let path = toFilePath file
  return $ top </> makeRelative "/" path

-- | Create any missing directories and evaluate 'fileCachePath'
fileCachePathIO :: (HasImageFilePath a, MonadReader r m, HasFileCacheTop r, Unexceptional m,
                    Member NonIOException e, Member IOException e, MonadError (OneOf e) m) => a -> m FilePath
fileCachePathIO file = do
  path <- fileCachePath file
  let dir = takeDirectory path
  liftUIO (createDirectoryIfMissing True dir)
  return path

-- * FileCacheT

type FileCacheT r s m = RWST r () s m

runFileCacheT :: (HasFileCacheTop r, HasCacheAcid r) => r -> s -> FileCacheT r s m a -> m (a, s, ())
runFileCacheT r s0 action = runRWST action r s0

evalFileCacheT :: (HasFileCacheTop r, HasCacheAcid r) => Functor m => r -> s -> FileCacheT r s m a -> m a
evalFileCacheT r s0 action = view _1 <$> runFileCacheT r s0 action
execFileCacheT :: (HasFileCacheTop r, HasCacheAcid r) => Functor m => r -> s -> FileCacheT r s m a-> m s
execFileCacheT r s0 action = view _2 <$> runFileCacheT r s0 action

askCacheAcid :: (MonadReader r m, HasCacheAcid r) => m CacheAcid
askCacheAcid = cacheAcid <$> ask

-- | insert a (key, value) pair into the cache
cachePut ::
  forall r e m. (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r)
  => ImageKey -> Either FileError ImageFile -> m (Either FileError ImageFile)
cachePut key val = do
  st <- askCacheAcid
  liftUIO (update st (PutValue key val))
  return val

-- | insert a (key, value) pair into the cache, discard result.
cachePut_ ::
  (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e,MonadReader r m, HasCacheAcid r)
  => ImageKey -> Either FileError ImageFile -> m ()
cachePut_ key val = do
  st <- askCacheAcid
  liftUIO (update st (PutValue key val))

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r)
  => ImageKey -> m (Maybe (Either FileError ImageFile))
cacheLook key = do
  st <- askCacheAcid
  {-either (Just . Left) (fmap id) <$>-}
  liftUIO $ query st (LookValue key)

cacheMap :: (FileCacheErrors e m, MonadReader r m, HasCacheAcid r) => m CacheMap
cacheMap = do
  st <- askCacheAcid
  liftUIO (query st LookMap)

cacheDelete ::
  forall e r m. (FileCacheErrors e m, MonadReader r m, HasCacheAcid r)
  => Proxy ImageFile -> Set ImageKey -> m ()
cacheDelete _ keys = do
  (st :: AcidState CacheMap) <- cacheAcid <$> ask
  liftUIO (update st (DeleteValues keys))
