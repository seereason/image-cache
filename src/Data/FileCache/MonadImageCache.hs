{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS -Wall #-}

module Data.FileCache.MonadImageCache
  ( ImageCacheT
  , MonadImageCache
#if 0
  , runImageCacheT
  , evalImageCacheT
  , execImageCacheT
  , writeImageCacheT
#endif
  ) where

import Control.Lens (_1, view)
import Control.Monad.Except
import Control.Monad.RWS
import Data.Acid (AcidState)
import Data.FileCache.Cache
import Data.FileCache.FileError (FileError)
import Data.FileCache.Image (ImageFile, ImageKey(..), scaleFromDPI)
import Data.FileCache.ImageIO (uprightImage, scaleImage, editImage)
--import Data.FileCache.LogException (logException)
import Data.FileCache.MonadFileCache (FileCacheT, W, MonadFileCache(..)
                                     {-, evalFileCacheT, execFileCacheT, runFileCacheT, writeFileCacheT-})
import Data.Maybe (fromMaybe)
import Extra.Except (logIOError, MonadIOError)
--import Extra.Log (Priority(ERROR))
import Numeric (fromRat)

type ImageCacheT s m = FileCacheT ImageKey ImageFile s m
type MonadImageCache m = MonadFileCache ImageKey ImageFile m

#if 0
runImageCacheT ::
  (HasFileError e, MonadError e m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W s m a
  -> m (a, s, W)
runImageCacheT = runFileCacheT

evalImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m a
evalImageCacheT = evalFileCacheT
execImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m s
execImageCacheT = execFileCacheT
writeImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m W
writeImageCacheT = writeFileCacheT

runImageCacheIOT ::
  (HasFileError e, MonadIOError e m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W S m a
  -> m (a, S, W)
runImageCacheIOT = runFileCacheIOT

evalImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m a
evalImageCacheT = evalFileCacheT
execImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m S
execImageCacheT = execFileCacheT
writeImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m W
writeImageCacheT = writeFileCacheT
#endif

-- | Build and return the 'ImageFile' described by the 'ImageKey'.
buildImageFile :: (MonadIOError FileError m, HasFileCacheTop m) => ImageKey -> m (CacheValue ImageFile)
buildImageFile (ImageOriginal img) = return (Value img)
buildImageFile (ImageUpright key) =
  -- mapError (\m -> either (Left . fromFileError) Right <$> m) $
    buildImageFile key >>= overCached uprightImage
buildImageFile (ImageScaled sz dpi key) = do
  buildImageFile key >>= overCached (\img ->
                                        let scale = scaleFromDPI dpi sz img in
                                        logIOError $ scaleImage (fromRat (fromMaybe 1 scale)) img)
buildImageFile (ImageCropped crop key) = do
  buildImageFile key >>= overCached (editImage crop)

overCached :: Monad m => (a -> m (CacheValue a)) -> CacheValue a -> m (CacheValue a)
overCached f (Value a) = f a
overCached _ v = pure v

-- | 'MonadFileCache' instance for images on top of the 'RWST' monad run by
-- 'runFileCacheT'
instance (MonadError FileError m, acid ~ AcidState (CacheMap ImageKey ImageFile), top ~ FileCacheTop)
  => MonadFileCache ImageKey ImageFile (RWST (acid, top) W s m) where
    askCacheAcid = view _1 :: RWST (acid, top) W s m (AcidState (CacheMap ImageKey ImageFile))
    buildCacheValue = buildImageFile
