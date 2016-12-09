-- | Database transactions to manage a cache of image files.  This
-- allows us to find derived versions of an image (resized, cropped,
-- rotated) given the ImageKey for the desired version, which contains
-- the checksum of the original image and the desired transformation.
-- If the desired transformation is not in the cached it is produced
-- and added.
--
-- The 'ImageKey' type describes the 'ImageFile' we would like the
-- system to produce.  This is passed to the 'build' method (which may
-- use IO) of 'MonadCache', and if that 'ImageKey' is not already in
-- the cache the desired 'ImageFile' is generated, added to the cache,
-- and returned.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Appraisal.ImageCache
    ( ImageKey(..)
    , ImageCacheMap
    , ImageCacheState
    , ImageCacheIO
    , runImageCacheIO
    , fileCachePath'
    ) where

import Appraisal.Cache (MonadCache(..))
import Appraisal.File (MonadFileCache(fileCacheTop), FileCacheTop(..), fileCachePath)
import Appraisal.Image (ImageCrop, ImageSize, scaleFromDPI)
import Appraisal.ImageFile (ImageFile(imageFile), editImage, scaleImage, uprightImage)
import Appraisal.Map (CacheState, runMonadCacheT)
import Appraisal.Utils.ErrorWithIO (logException)
import Control.Exception (IOException)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError)
import Control.Monad.Reader (MonadReader(ask), MonadTrans(lift), ReaderT, runReaderT)
import Control.Monad.Trans (MonadIO)
import Data.Generics (Data, Typeable)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (defaultOptions)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

-- | Describes an ImageFile and, if it was derived from other image
-- files, how.
data ImageKey
    = ImageOriginal ImageFile
    -- ^ An unmodified upload
    | ImageCropped ImageCrop ImageKey
    -- ^ A cropped version of another image
    | ImageScaled ImageSize Double ImageKey
    -- ^ A resized version of another image
    | ImageUpright ImageKey
    -- ^ Image uprighted using the EXIF orientation code, see  "Appraisal.Exif"
    deriving (Eq, Ord, Show, Typeable, Data)

instance Pretty ImageKey where
    pPrint (ImageOriginal x) = pPrint x
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> text ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled size dpi x) = text "Scale (" <> pPrint size <> text " @" <> text (show dpi) <> text "dpi) (" <> pPrint x <> text ")"

-- | A map from 'ImageKey' to 'ImageFile'.
data ImageCacheMap = ImageCacheMap (Map ImageKey ImageFile) deriving (Eq, Ord, Show, Typeable, Data)

-- | The acidic version of 'ImageCacheMap'
type ImageCacheState = CacheState ImageKey ImageFile
-- | Reader monad providing 'ImageCacheState'
type ImageCacheIO p m = ReaderT p (ReaderT ImageCacheState m)

-- | Given the path to a cache of images and an opened image cache
-- database, perform an action in the 'ImageCacheIO' monad.
runImageCacheIO :: ImageCacheIO FileCacheTop m a -> FileCacheTop -> ImageCacheState -> m a
runImageCacheIO action p st = runMonadCacheT (runReaderT action p) st
-- runImageCacheIO action p st = runReaderT (runMonadCacheT action st) p

instance (MonadReader FileCacheTop m, MonadCatch m, MonadError IOException m, MonadIO m)
    => MonadFileCache m where
    fileCacheTop = unFileCacheTop <$> ask

-- | Base instance of MonadCache for 'ImageCacheIO'.
instance (MonadCatch m, MonadError IOException m, MonadIO m, Functor m)
    => MonadCache ImageKey ImageFile (ImageCacheIO FileCacheTop m) where
    askAcidState = lift ask
    build (ImageOriginal img) = return img
    build (ImageUpright key) = do
      img <- build key
      $logException $ uprightImage img
    build (ImageScaled sz dpi key) = do
      img <- build key
      let scale = scaleFromDPI dpi sz img
      $logException $ scaleImage (fromMaybe 1.0 scale) img
    build (ImageCropped crop key) = do
      img <- build key
      $logException $ editImage crop img

-- | Compute 'Appraisal.File.fileCachePath' for an ImageFile.
fileCachePath' :: MonadFileCache m => ImageFile -> m FilePath
fileCachePath' = fileCachePath . imageFile

$(deriveJSON defaultOptions ''ImageKey)

$(deriveSafeCopy 1 'base ''ImageKey)
$(deriveSafeCopy 1 'base ''ImageCacheMap)
