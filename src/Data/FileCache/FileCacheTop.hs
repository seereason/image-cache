-- Probably should merge into FileCache

{-# OPTIONS -Wno-unused-imports #-}

{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
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
  , MonadFileCacheType
  , MonadFileCache
  , MonadFileCacheBG
  , MonadFileCacheWriter
#endif
  ) where

#if !__GHCJS__
import Control.Exception (IOException, SomeException)
import Control.Lens ( _1, view )
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, runExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.State.Class (MonadState)
import Control.Monad.RWS.Strict (RWST)
import Data.Acid ( AcidState )
import Data.FileCache.Background (HasTaskQueue)
import Data.FileCache.CacheMap ( CacheMap )
import Data.FileCache.FileError (FileError)
import Data.FileCache.ImageKey (ImageKey)
import Data.Set (Set)
import Extra.Lens (HasLens)
import SeeReason.Errors (ConvertError, Member, OneOf)
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

type MonadFileCacheType r e m =
  (MonadIO m,
   MonadCatch m,
   MonadError (OneOf e) m,
   ConvertError SomeException (Either SomeException (OneOf e)),
   Member IOException e,
   Member FileError e,
   MonadReader r m,
   HasCacheAcid r,
   HasFileCacheTop r)

class MonadFileCacheType r e m => MonadFileCache r e m

type MonadFileCacheBG r s e m task =
  (MonadFileCache r e m, ?task :: ImageKey -> task,
   MonadState s m,
   HasLens s (Set task),
    -- Storage where this server thread can record the status of tasks
    -- we are interested in.
   HasTaskQueue task r)

-- | For code that can add things to the cache
class MonadFileCache r e m => MonadFileCacheWriter r e m

instance MonadFileCacheType r e m => MonadFileCache r e (ReaderT r (ExceptT (OneOf e) m))
instance MonadFileCacheType r e m => MonadFileCacheWriter r e (ReaderT r (ExceptT (OneOf e) m))
instance (MonadFileCacheType r e m, Monoid w) => MonadFileCache r e (RWST r w s (ExceptT (OneOf e) m))
#endif
