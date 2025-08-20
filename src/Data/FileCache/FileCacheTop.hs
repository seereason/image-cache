-- Probably should merge into FileCache

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.FileCacheTop
  ( FileCacheTop(..)
  , HasFileCacheTop(fileCacheTop)
#if !__GHCJS__
  , HasCacheAcid(cacheAcid)
  , CacheAcid
  , MonadFileCache
  , MonadFileCacheWriter
  , FileCacheT
  , runFileCacheT
#endif
  ) where

#if !__GHCJS__
import Control.Exception (IOException)
import Control.Lens ( _1, view )
import Control.Monad.Except (ExceptT, MonadError, MonadIO)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.RWS (RWST)
import Data.Acid ( AcidState )
import Data.FileCache.CacheMap ( CacheMap )
import Data.FileCache.FileError (E, FileError, runFileIOT)
import SeeReason.Errors (Member, OneOf)
#endif

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

class (MonadIO m,
       MonadError (OneOf e) m,
       Member IOException e,
       Member FileError e,
       MonadReader r m,
       HasCacheAcid r,
       HasFileCacheTop r)
      => MonadFileCache r e m

-- | For code that can add things to the cache
class MonadFileCache r e m => MonadFileCacheWriter r e m

instance (MonadIO m, Member IOException e, Member FileError e, HasCacheAcid r, HasFileCacheTop r
         ) => MonadFileCache r e (ReaderT r (ExceptT (OneOf e) m))
instance (Monoid w, MonadIO m, Member IOException e, Member FileError e, HasCacheAcid r, HasFileCacheTop r
         ) => MonadFileCache r e (RWST r w s (ExceptT (OneOf e) m))

-- | A simple type that is an instance of 'MonadFileCacheUIO'.
type FileCacheT r m = ReaderT r (ExceptT (OneOf E) m)

runFileCacheT ::
     FileCacheT r m a
  -> r
  -> m (Either (OneOf E) a)
runFileCacheT action r =
  runFileIOT (runReaderT action r)
#endif
