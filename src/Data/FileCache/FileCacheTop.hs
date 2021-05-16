-- Probably should merge into FileCache

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.FileCacheTop
  ( FileCacheTop(..)
  , HasFileCacheTop(fileCacheTop)
  , HasCacheAcid(cacheAcid)
  , CacheAcid
  ) where

import Control.Lens ( _1, view )
import Data.Acid ( AcidState )
import Data.FileCache.Common ( CacheMap )

newtype FileCacheTop = FileCacheTop {_unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- directory of a file cache.
class HasFileCacheTop a where fileCacheTop :: a -> FileCacheTop
instance HasFileCacheTop FileCacheTop where fileCacheTop = id
-- instance HasFileCacheTop (ImageAcid, FileCacheTop) where fileCacheTop = snd
-- instance HasFileCacheTop (ImageAcid, FileCacheTop, c) where fileCacheTop = view _2

type CacheAcid = AcidState CacheMap
class HasCacheAcid a where cacheAcid :: a -> AcidState CacheMap
instance  HasCacheAcid CacheAcid where cacheAcid = id
instance  HasCacheAcid (CacheAcid, top) where cacheAcid = fst
instance  HasCacheAcid (CacheAcid, a, b) where cacheAcid = view _1
