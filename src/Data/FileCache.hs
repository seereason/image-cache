-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the
-- 'buildCacheValue' method of the 'HasCache' instance, and stored
-- using acid-state.

module Data.FileCache
  ( module Data.FileCache.Cache
  , module Data.FileCache.File
  , module Data.FileCache.FileError
  , module Data.FileCache.Image
  , module Data.FileCache.LogException
#if !__GHCJS__
  , module Data.FileCache.Acid
  , module Data.FileCache.ErrorWithIO
  , module Data.FileCache.Exif
  , module Data.FileCache.FileIO
  , module Data.FileCache.ImageIO
  , module Data.FileCache.ImageFile
  , module Data.FileCache.MonadFileCache
  , module Data.FileCache.MonadImageCache
#endif
  ) where

import Data.FileCache.Cache
import Data.FileCache.File
import Data.FileCache.FileError
import Data.FileCache.Image
import Data.FileCache.LogException
#if !__GHCJS__
import Data.FileCache.Acid
import Data.FileCache.ErrorWithIO
import Data.FileCache.Exif
import Data.FileCache.FileIO
import Data.FileCache.ImageIO
import Data.FileCache.ImageFile
import Data.FileCache.MonadFileCache
import Data.FileCache.MonadImageCache
#endif
