-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the
-- 'buildCacheValue' method of the 'HasCache' instance, and stored
-- using acid-state.

module Data.FileCache
  ( module Data.FileCache.FileError
  , module Data.FileCache.Image
  , module Data.FileCache.LogException
  , module Data.FileCache.Types
#if !__GHCJS__
  , module Data.FileCache.AcidCache
  , module Data.FileCache.ErrorWithIO
  , module Data.FileCache.Exif
  , module Data.FileCache.FileCache
  , module Data.FileCache.FileCacheT
  , module Data.FileCache.ImageCache
  , module Data.FileCache.ImageFile
#endif
  ) where

import Data.FileCache.FileError
import Data.FileCache.Image
import Data.FileCache.LogException
import Data.FileCache.Types
#if !__GHCJS__
import Data.FileCache.AcidCache
import Data.FileCache.ErrorWithIO
import Data.FileCache.Exif
import Data.FileCache.FileCache
import Data.FileCache.FileCacheT
import Data.FileCache.ImageCache
import Data.FileCache.ImageFile
#endif
