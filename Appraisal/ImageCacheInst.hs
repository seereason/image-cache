-- | Instance of MonadCache

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Appraisal.ImageCacheInst
    ( runImageCacheIO
    ) where

import Appraisal.AcidCache ( MonadCache(..) )
import Appraisal.FileCacheInst ( FileCacheT, runFileCacheIO )
import Appraisal.ImageCache ( uprightImage, scaleImage, editImage, MonadImageCacheIO, MonadImageCache )
import Appraisal.FileCache ( MonadFileCacheIO )
import Appraisal.Image ( ImageFile, scaleFromDPI, ImageCacheMap, ImageKey(..) )
import Appraisal.Utils.ErrorWithIO ( logException )
import Control.Exception ( IOException )
import Control.Monad.Except ( ExceptT )
import Control.Monad.Reader ( MonadReader(ask), ReaderT )
import Data.Acid ( AcidState )
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Numeric ( fromRat )

-- | Build a MonadCache instance for images on top of a MonadFileCache
-- instance and a reader for the acid state.
instance (MonadReader (AcidState ImageCacheMap) m, MonadFileCacheIO IOException m) => MonadCache ImageKey ImageFile m where
    askAcidState = ask
    build (ImageOriginal img) = return img
    build (ImageUpright key) = do
      img <- build key
      $logException $ uprightImage img
    build (ImageScaled sz dpi key) = do
      img <- build key
      let scale = scaleFromDPI dpi sz img
      $logException $ scaleImage (fromRat (fromMaybe 1 scale)) img
    build (ImageCropped crop key) = do
      img <- build key
      $logException $ editImage crop img

instance (MonadImageCache m, MonadFileCacheIO e m) => MonadImageCacheIO e m
instance (MonadCache ImageKey ImageFile m{-, MonadFileCache m-}) => MonadImageCache m

-- | Given a file cache monad and an opened image cache database,
-- perform an image cache action.  This is just 'runFileCache'
-- with its arguments reversed to match an older version of the
-- function.
runImageCacheIO ::
    (MonadImageCacheIO e (FileCacheT (ReaderT (AcidState (Map key val)) (ExceptT e m))))
    => FileCacheT (ReaderT (AcidState (Map key val)) (ExceptT e m)) a
    -> FilePath
    -> AcidState (Map key val)
    -> m (Either e a)
runImageCacheIO action fileCacheDir fileAcidState =
    runFileCacheIO fileAcidState fileCacheDir action
