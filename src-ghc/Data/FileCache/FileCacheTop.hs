-- Probably should merge into FileCache

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.FileCacheTop
  ( FileCacheTop(..)
  , HasFileCacheTop(fileCacheTop)
#if !__GHCJS__
  , HasCacheAcid(cacheAcid)
  , CacheAcid
  , MonadFileCache
#endif
  , FileCacheT
  , runFileCacheT
  ) where

import Control.Lens ( _1, view )
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
#if !__GHCJS__
import Data.Acid ( AcidState )
#endif
import Data.FileCache.CacheMap ( CacheMap )
import Data.FileCache.FileError (E, MonadFileIO, runFileIOT)
import SeeReason.Errors (OneOf)

newtype FileCacheTop = FileCacheTop {_unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- directory of a file cache.
class HasFileCacheTop a where fileCacheTop :: a -> FileCacheTop
instance HasFileCacheTop FileCacheTop where fileCacheTop = id

#if !__GHCJS__
type CacheAcid = AcidState CacheMap
class HasCacheAcid a where cacheAcid :: a -> AcidState CacheMap
instance  HasCacheAcid CacheAcid where cacheAcid = id
instance  HasCacheAcid (CacheAcid, top) where cacheAcid = fst
instance  HasCacheAcid (CacheAcid, a, b) where cacheAcid = view _1

type MonadFileCache r e m =
  (MonadFileIO e m,
   MonadReader r m,
   HasCacheAcid r,
   HasFileCacheTop r)
#endif

-- | A simple type that is an instance of 'MonadFileCacheUIO'.
type FileCacheT r m = ReaderT r (ExceptT (OneOf E) m)

runFileCacheT ::
     FileCacheT r m a
  -> r
  -> m (Either (OneOf E) a)
runFileCacheT action r =
  runFileIOT (runReaderT action r)
