-- Probably should merge into FileCache

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.FileCacheTop
  ( FileCacheTop(..)
  , HasFileCacheTop(fileCacheTop)
  , HasCacheAcid(cacheAcid)
  , CacheAcid
  , MonadFileCacheUIO
  , MonadFileCacheNew
  , FileCacheT
  , runFileCacheT
  ) where

import Control.Lens ( _1, view )
import Control.Monad.Except (ExceptT)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Control.Monad.RWS (RWST)
import Data.Acid ( AcidState )
import Data.FileCache.CacheMap ( CacheMap )
import Data.FileCache.FileError (E, MonadFileUIO, MyMonadUIO, MonadFileIONew, MyMonadIONew, runFileIOT)
import SeeReason.Errors (OneOf)

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

class (MonadFileUIO e m,
       MonadReader r m,
       HasCacheAcid r,
       HasFileCacheTop r
      ) => MonadFileCacheUIO r e m

instance (MonadFileUIO e (ExceptT (OneOf e) m),
          HasCacheAcid r,
          HasFileCacheTop r
         ) => MonadFileCacheUIO r e (ReaderT r (ExceptT (OneOf e) m))

instance (MonadFileUIO e (ExceptT (OneOf e) m),
          HasCacheAcid r, HasFileCacheTop r
         ) => MonadFileCacheUIO r e (RWST r () s (ExceptT (OneOf e) m))

class (MonadFileIONew e m,
       MonadReader r m,
       HasCacheAcid r,
       HasFileCacheTop r
      ) => MonadFileCacheNew r e m

instance (MonadFileIONew e (ExceptT (OneOf e) m),
          HasCacheAcid r,
          HasFileCacheTop r
         ) => MonadFileCacheNew r e (ReaderT r (ExceptT (OneOf e) m))

instance (MonadFileIONew e (ExceptT (OneOf e) m),
          HasCacheAcid r, HasFileCacheTop r
         ) => MonadFileCacheNew r e (RWST r () s (ExceptT (OneOf e) m))

-- | A simple type that is an instance of 'MonadFileCacheUIO'.
type FileCacheT r m = ReaderT r (ExceptT (OneOf E) m)

runFileCacheT ::
     FileCacheT r m a
  -> r
  -> m (Either (OneOf E) a)
runFileCacheT action r =
  runFileIOT (runReaderT action r)
