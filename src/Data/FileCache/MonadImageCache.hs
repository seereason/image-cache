module Data.FileCache.MonadImageCache
  ( MonadImageCache
  ) where

import Data.FileCache.FileError (FileError)
import Data.FileCache.Image (ImageFile, ImageKey)
import Data.FileCache.Monad (MonadFileCache)

type MonadImageCache m = MonadFileCache ImageKey ImageFile FileError m
