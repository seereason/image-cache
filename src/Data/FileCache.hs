-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the
-- 'buildCacheValue' method of the 'HasCache' instance, and stored
-- using acid-state.

module Data.FileCache
  ( module Data.FileCache.CacheMap
  , module Data.FileCache.CommandError
  , module Data.FileCache.File
  , module Data.FileCache.FileError
  , module Data.FileCache.Happstack
  , module Data.FileCache.ImageCrop
  , module Data.FileCache.ImageFile
  , module Data.FileCache.ImageKey
  , module Data.FileCache.ImageRect
  , module Data.FileCache.ImageSize
  , module Data.FileCache.Rational
  ) where

import Data.FileCache.CacheMap (ImageCached(..), CacheMap(..))
import Data.FileCache.CommandError (CommandError, CommandInfo(..), HasCommandError(fromCommandError), ToCommandError(toCommandError))
import Data.FileCache.File (File(..), FileSource(..), Checksum, Extension, HasFileChecksum(fileChecksum), HasFileExtension(fileExtension))
import Data.FileCache.FileError (FileError(..), CommandError, HasFileError(fileError), MyMonadIONew, MyIOErrors, MonadFileIONew, E, runFileIOT)
import Data.FileCache.Happstack (ContentType(..))
import Data.FileCache.ImageCrop (ImageCrop(..), Rotation(..))
import Data.FileCache.ImageFile (ImageFile(..), ImageReady(..), printerDPI)
import Data.FileCache.ImageKey
  (ImageKey(..), HasImageKey(imageKey), OriginalKey(originalKey), UprightKey(uprightKey), EditedKey(editedKey), ScaledKey(scaledKey),
   ImagePath(ImagePath, _imagePathKey), HasImagePath(imagePath), shapeFromKey,
   FileType(..), HasFileType(imageType), supportedFileTypes, supportedMimeTypes,
   ImageShape(..), HasImageShapeM(imageShapeM), HasImageShape, imageShape, HasOriginalShape(originalShape),
   scaleFromDPI, ImageStats(..))
import Data.FileCache.ImageRect
  (ImageRect(_imageRectWidth, _imageRectHeight, _imageFileOrientation), makeImageRect,
   imageAspect, HasImageRect(imageRect), widthInInches, widthInInches', heightInInches,
   scaleImageRect, scaleFromDPI, cropImageRect, uprightImageRect)
import Data.FileCache.ImageSize
  (ImageSize(..), HasImageSize(imageSize), Dimension(..), Units(..),
   saneSize, SaneSize(..), defaultSize, inches)
import Data.FileCache.Rational
  ((%), fromRat, -- re-exports
   approx, rationalIso, rationalPrism, readRationalMaybe, showRational, rsqrt)
