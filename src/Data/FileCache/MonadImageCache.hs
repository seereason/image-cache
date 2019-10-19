{-# LANGUAGE OverloadedStrings, TemplateHaskell, UndecidableInstances #-}
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
  , fromImageCacheT
  , fromImageCacheT'
  ) where

import Control.Lens (_1, _Left, over, view)
import Control.Monad.Except
import Control.Monad.RWS
import Data.Acid (AcidState)
import Data.FileCache.Acid (Cached(_unCached))
import Data.FileCache.Cache
import Data.FileCache.FileError (FileError(CacheDamage), HasFileError, fromFileError)
import Data.FileCache.Image (ImageFile, ImageKey(..), scaleFromDPI)
import Data.FileCache.ImageIO (uprightImage, scaleImage, editImage)
--import Data.FileCache.LogException (logException)
import Data.FileCache.MonadFileCache (cacheLook, evalFileCacheT, FileCacheT, W, MonadFileCache(..)
                                     {-, evalFileCacheT, execFileCacheT, runFileCacheT, writeFileCacheT-})
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import Extra.Except (liftIOError, logIOError, MonadIOError)
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
buildImageFile ::
  forall e m. (MonadImageCache m, MonadIOError e m, HasFileError e)
  => ImageKey
  -> m (CacheValue ImageFile)
buildImageFile key@(ImageOriginal _) = do
  -- This should already be in the cache
  (r :: Maybe (Cached (CacheValue ImageFile))) <- cacheLook key
  case r of
    -- Should we write this into the cache?  Probably not, if we leave
    -- it as it is the software could later corrected.
    Nothing -> return (Failed (CacheDamage ("Missing original: " <> pack (show key))))
    Just c -> return (_unCached c)
  -- maybe (throwError (fromFileError (CacheDamage ("Missing original: " <> pack (show key))) :: e)) (return . _unCached) r
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
instance (MonadError e m, acid ~ AcidState (CacheMap ImageKey ImageFile), top ~ FileCacheTop)
  => MonadFileCache ImageKey ImageFile (RWST (acid, top) W s m) where
    askCacheAcid = view _1 :: RWST (acid, top) W s m (AcidState (CacheMap ImageKey ImageFile))
    buildCacheValue = buildImageFile

-- mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
-- evalFileCacheT :: Functor m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m a
-- runExceptT :: ExceptT e m a -> m (Either e a)

-- Crazy function to turn FileError into e.
fromImageCacheT' ::
  forall e m a n. (HasFileError e, MonadIOError e m, MonadImageCache m, n ~ ImageCacheT () IO)
  => n (Either FileError a)
  -> m (Either e a)
fromImageCacheT' action1 = do
  acid <- askCacheAcid @ImageKey @ImageFile
  top <- fileCacheTop
  flattenExcept1 (evalFileCacheT acid top () action1)
  where
    flattenExcept1 :: ExceptT FileError IO (Either FileError a) -> m (Either e a)
    flattenExcept1 action = liftFileError (flattenEither <$> (runExceptT action))

    liftFileError :: IO (Either FileError a) -> m (Either e a)
    liftFileError action = over _Left fromFileError <$> liftIOError action

    flattenEither = either Left id

-- Crazy function to turn FileError into e.
fromImageCacheT ::
  forall e m a n. (HasFileError e, MonadIOError e m, MonadImageCache m, n ~ ImageCacheT () IO)
  => n a
  -> m a
fromImageCacheT action = do
  acid <- askCacheAcid @ImageKey @ImageFile
  top <- fileCacheTop
  flattenExcept (evalFileCacheT acid top () action)
  where
    flattenExcept :: ExceptT FileError IO a -> m a
    flattenExcept = liftFileError . runExceptT

    liftFileError :: IO (Either FileError a) -> m a
    liftFileError action' = liftIOError action' >>= either (throwError . fromFileError) return
