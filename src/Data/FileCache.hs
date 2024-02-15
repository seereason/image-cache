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
  , module Data.FileCache.ImageShape
  , module Data.FileCache.ImageSize
  , module Data.FileCache.Rational
  ) where

import Data.FileCache.CacheMap
import Data.FileCache.CommandError
import Data.FileCache.File
import Data.FileCache.FileError
import Data.FileCache.Happstack
import Data.FileCache.ImageCrop
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey
import Data.FileCache.ImageRect
import Data.FileCache.ImageShape
import Data.FileCache.ImageSize
import Data.FileCache.Rational
