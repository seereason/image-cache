-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the
-- 'buildCacheValue' method of the 'HasCache' instance, and stored
-- using acid-state.

module Data.FileCache
  ( module Data.FileCache.Common
#if !__GHCJS__
  , module Data.FileCache.Server
#endif
  ) where

import Data.FileCache.Common
#if !__GHCJS__
import Data.FileCache.Server
#endif
