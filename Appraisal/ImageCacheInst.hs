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
import Appraisal.FileCache (FileCacheT, FileError, runFileCacheT)
import Appraisal.ImageCache ( uprightImage, scaleImage, editImage, ImageCacheT)
--import Appraisal.FileCache ( MonadFileCacheIO )
import Appraisal.Image ( ImageFile, scaleFromDPI, ImageKey(..) )
import Appraisal.Utils.ErrorWithIO ( logException )
--import Control.Exception ( IOException )
import Control.Lens (_1, view)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (MonadReader(ask))
import Data.Acid ( AcidState )
import Data.Map ( Map )
import Data.Maybe ( fromMaybe )
import Numeric ( fromRat )

-- | Build a MonadCache instance for images on top of a MonadFileCache
-- instance and a reader for the acid state.
instance (MonadIO m, MonadCatch m) => MonadCache ImageKey ImageFile (FileCacheT (AcidState (Map ImageKey ImageFile)) FileError m) where
    askAcidState = view _1 <$> ask
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

-- | Given a file cache monad and an opened image cache database,
-- perform an image cache action.  This is just 'runFileCache'
-- with its arguments reversed to match an older version of the
-- function.
runImageCacheIO ::
    (MonadIO m)
    => AcidState (Map key val)
    -> FilePath
    -> FileCacheT (AcidState (Map key val)) e m a
    -> m (Either e a)
runImageCacheIO = runFileCacheT
