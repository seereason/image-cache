-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the
-- 'buildCacheValue' method of the 'HasCache' instance, and stored
-- using acid-state.

module Data.FileCache
  (
    -- * Happstack re-exports
    ContentType(..),

    -- * IO and error types
    FileError(..), HasFileError(fileError), MyMonadIO, MyIOErrors, MonadFileIO, E, fromE, runFileIOT,

    -- * Running shell commands
    CommandError, CommandInfo(..), HasCommandError(fromCommandError), ToCommandError(toCommandError),

    -- * File and image file types
    File(..), FileSource(..), Checksum, Extension, HasFileChecksum(fileChecksum), HasFileExtension(fileExtension),

    -- * import Data.FileCache.ImageCrop
    ImageCrop(..), Rotation(..),

    -- * import Data.FileCache.ImageSize
    ImageSize(..), HasImageSize(imageSize), Dimension(..), Units(..),
    saneSize, SaneSize(..), defaultSize, inches,

    -- * import Data.FileCache.ImageRect
    ImageRect(_imageRectWidth, _imageRectHeight, _imageFileOrientation), makeImageRect,
    imageAspect, HasImageRect(imageRect), widthInInches, widthInInches', heightInInches,
    scaleImageRect, scaleFromDPI, cropImageRect, uprightImageRect,

    -- * ImageKey - describes the function from original to derived image
    ImageKey(..), HasImageKey(imageKey),
    OriginalKey(originalKey), UprightKey(uprightKey),
    EditedKey(editedKey), ScaledKey(scaledKey),

    -- ImagePath(ImagePath, _imagePathKey), HasImagePath(imagePath),

    FileType(..), -- GIF, HEIC, JPEG, PDF, PNG, PPM, TIFF, CSV, Unknown
    HasFileType(imageType), supportedFileTypes, supportedMimeTypes,
    ImageShape(ImageShape, _imageShapeType, _imageShapeRect),
    shapeFromKey,
    HasImageShapeM(imageShapeM), HasImageShape, imageShape,
    HasOriginalShape(originalShape),
    ImageStats(..),

    -- * import Data.FileCache.ImageFile
    ImageFile(ImageFileShape, ImageFileReady),
    ImageReady(ImageReady, _imageFile, _imageShape),
    printerDPI,

    -- * import Data.FileCache.CacheMap
    ImageCached(..), CacheMap(..),

#ifndef __GHCJS__
    -- * Image cache acid state repository
    FileCacheTop(FileCacheTop, _unFileCacheTop),
    MonadFileCache, HasCacheAcid(cacheAcid),
    HasFileCacheTop(fileCacheTop), runFileCacheT,

    -- * Image cache acid-state operations
    initCacheMap, openCache, PutValue(..), PutValues(..),
    LookValue(..), LookValues(..), LookMap(..), DeleteValue(..),
    DeleteValues(..), Replace(..), Request(..), Requested(..),
    Dequeue(..), Complete(..), cacheLook, cachePut,

    -- * Image directory structure
    fileCachePath, HasFilePath(toFilePath),

    -- * Upload a new image
    cacheOriginalFile,

    -- * Turn an ImageKey into an ImageFile
    buildImageFile, getImageFile, getImageFiles,

    -- * Background image building process
    HasTaskQueue(taskQueue), TaskChan, startTaskQueue,
    testImageKeys,
#endif

---------------------

  -- * module Data.FileCache.Rational
    (%),     -- re-export with improved error message
    fromRat, -- re-export with improved error message
    approx,
    micro,
    rationalIso,
    rationalPrism,
    readRationalMaybe,
    showRational,
    rsqrt
  ) where

import Data.FileCache.CacheMap (ImageCached(..), CacheMap(..))
import Data.FileCache.CommandError (CommandError, CommandInfo(..), HasCommandError(fromCommandError), ToCommandError(toCommandError))
import Data.FileCache.File (File(..), FileSource(..), Checksum, Extension, HasFileChecksum(fileChecksum), HasFileExtension(fileExtension))
import Data.FileCache.FileError (FileError(..), HasFileError(fileError), MyMonadIO, MyIOErrors, MonadFileIO, E, fromE, runFileIOT)
import Data.FileCache.Happstack (ContentType(..))
import Data.FileCache.ImageCrop (ImageCrop(..), Rotation(..))
import Data.FileCache.ImageFile (ImageFile(..), ImageReady(..), printerDPI)
import Data.FileCache.ImageKey
  (ImageKey(..), HasImageKey(imageKey), OriginalKey(originalKey), UprightKey(uprightKey), EditedKey(editedKey), ScaledKey(scaledKey),
   {-ImagePath(ImagePath, _imagePathKey), HasImagePath(imagePath),-} shapeFromKey,
   FileType(..), HasFileType(imageType), supportedFileTypes, supportedMimeTypes,
   ImageShape(..), HasImageShapeM(imageShapeM), HasImageShape, imageShape, HasOriginalShape(originalShape),
   ImageStats(..))
import Data.FileCache.ImageRect
  (ImageRect(_imageRectWidth, _imageRectHeight, _imageFileOrientation), makeImageRect,
   imageAspect, HasImageRect(imageRect), widthInInches, widthInInches', heightInInches,
   scaleImageRect, scaleFromDPI, cropImageRect, uprightImageRect)
import Data.FileCache.ImageSize
  (ImageSize(..), HasImageSize(imageSize), Dimension(..), Units(..),
   saneSize, SaneSize(..), defaultSize, inches)
import Data.FileCache.Rational
  ((%), fromRat, -- re-exports
   approx, micro, rationalIso, rationalPrism, readRationalMaybe, showRational, rsqrt)

#ifndef __GHCJS__
import Data.FileCache.Acid
  (initCacheMap, openCache, PutValue(..), PutValues(..),
   LookValue(..), LookValues(..), LookMap(..), DeleteValue(..),
   DeleteValues(..), Replace(..), Request(..), Requested(..),
   Dequeue(..), Complete(..))
import Data.FileCache.Background (HasTaskQueue(taskQueue), TaskChan, startTaskQueue, testImageKeys)
import Data.FileCache.Derive (buildImageFile, getImageFile, getImageFiles)
import Data.FileCache.FileCache (cacheLook, cachePut, fileCachePath, HasFilePath(toFilePath))
import Data.FileCache.FileCacheTop (FileCacheTop(FileCacheTop, _unFileCacheTop), HasCacheAcid, MonadFileCache, HasCacheAcid(cacheAcid), HasFileCacheTop(fileCacheTop), runFileCacheT)
import Data.FileCache.Upload (cacheOriginalFile)
#endif
