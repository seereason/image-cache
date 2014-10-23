-- | Database transactions to manage a cache of image files.  This
-- allows us to find derived versions of an image (resized, cropped,
-- rotated) given the ImageKey for the desired version, which contains
-- the checksum of the original image and the desired transformation.
-- If the desired transformation is not in the cached it is produced
-- and added.
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
{-# OPTIONS -Wall #-}
module Appraisal.ImageCache
    ( ImageKey(..)
    , ImageCacheMap
    , ImageCacheState
    , ImageCacheIO
    , runImageCacheIO
    , fileCachePath'
    ) where

import Appraisal.Cache (CacheState, MonadCache(..), runMonadCacheT)
import Appraisal.File (FileCacheTop(..), fileCachePath)
import Appraisal.Image (ImageCrop, ImageSize, scaleFromDPI)
import Appraisal.ImageFile (ImageFile(imageFile), editImage, scaleImage, uprightImage)
import Control.Exception (IOException)
import Control.Monad.Error (MonadError)
import Control.Monad.Reader (MonadReader(ask), MonadTrans(lift), ReaderT, runReaderT)
import Control.Monad.Trans (MonadIO)
import Data.Generics (Data, Typeable)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.SafeCopy (base, deriveSafeCopy)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

data ImageKey
    = ImageOriginal ImageFile
    | ImageCropped ImageCrop ImageKey
    | ImageScaled ImageSize Double ImageKey
    | ImageUpright ImageKey
    deriving (Eq, Ord, Show, Typeable, Data)

instance Pretty ImageKey where
    pPrint (ImageOriginal x) = pPrint x
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> text ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled size dpi x) = text "Scale (" <> pPrint size <> text " @" <> text (show dpi) <> text "dpi) (" <> pPrint x <> text ")"

$(deriveSafeCopy 1 'base ''ImageKey)

data ImageCacheMap = ImageCacheMap (Map ImageKey ImageFile) deriving (Eq, Ord, Show, Typeable, Data)

$(deriveSafeCopy 1 'base ''ImageCacheMap)

type ImageCacheState = CacheState ImageKey ImageFile
type ImageCacheIO p m = ReaderT p (ReaderT ImageCacheState m)

runImageCacheIO :: ImageCacheIO FileCacheTop m a -> FileCacheTop -> ImageCacheState -> m a
runImageCacheIO action p st = runMonadCacheT (runReaderT action p) st

instance (MonadError IOException m, MonadIO m, Functor m) => MonadCache ImageKey ImageFile (ImageCacheIO FileCacheTop m) where
    askAcidState = lift ask
    build (ImageOriginal img) = return img
    build (ImageUpright key) = do
      top <- ask
      img <- build key
      lift (lift (uprightImage top img))
    build (ImageScaled sz dpi key) = do
      top <- ask
      img <- build key
      let scale = scaleFromDPI dpi sz img
      lift (lift (scaleImage (fromMaybe 1.0 scale) top img))
    build (ImageCropped crop key) = do
      top <- ask
      img <- build key
      lift (lift (editImage crop top img))

-- | Compute 'Appraisal.File.fileCachePath' for an ImageFile.
fileCachePath' :: Monad m => ImageFile -> ImageCacheIO FileCacheTop m FilePath
fileCachePath' x =
    do ver <- ask
       return $ fileCachePath ver (imageFile x)
