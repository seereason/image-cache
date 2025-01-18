{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Werror=unused-imports #-}

module Data.FileCache.FileCache
  ( HasFilePath(toFilePath)
  , fileCachePath
  , fileCachePathIO
  -- , FileCacheT, runFileCacheT, evalFileCacheT, execFileCacheT
  , cacheLook, cacheDelete, cacheMap
  , cachePut, cachePut_
  , Classified(..)
  , collectGarbage
  ) where

import Control.Lens (at, ifoldlM, Lens', set)
-- import Control.Lens ( _1, _2, view )
-- import Control.Monad.RWS ( RWST(runRWST) )
import Control.Monad.Reader (liftIO, MonadIO, MonadReader(ask))
import Data.Acid (AcidState)
import Data.Acid.Advanced (query', update')
import Data.FileCache.FileCacheTop
import Data.FileCache.Acid
import Data.FileCache.CacheMap
import Data.FileCache.File
import Data.FileCache.FileError
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey
import Data.List (stripPrefix)
import Data.Monoid ( (<>) )
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Proxy ( Proxy )
import Data.Set as Set (fromList, partition, Set)
import Data.Text as T ( unpack )
-- import Debug.Trace (trace)
import GHC.Generics (Generic)
import GHC.Stack (callStack, HasCallStack)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ( (</>), makeRelative, takeDirectory )
import System.FilePath.Find as Find ((==?), always, fileType, find)
import qualified System.FilePath.Find as Find (FileType(RegularFile))
import Web.Routes ( toPathInfo )

-- * FileCacheTop

-- | The common suffix of the path to the image URI and its server
-- FilePath.
class HasFilePath a where
  toFilePath :: a -> FilePath
  toURIDir :: a -> FilePath

instance HasFilePath (Checksum, FileType) where
  toURIDir (csum, _typ) = take 2 $ unpack csum
  toFilePath p@(csum, typ) =
     toURIDir p </> unpack (csum <> fileExtension typ)

instance HasFilePath ImagePath where
  toURIDir (ImagePath (ImageOriginal csum typ)) = toURIDir (csum, typ)
  toURIDir (ImagePath (ImageUpright key)) = toURIDir (ImagePath key)
  toURIDir (ImagePath (ImageScaled _ _ key)) = toURIDir (ImagePath key)
  toURIDir (ImagePath (ImageCropped _ key)) = toURIDir (ImagePath key)
  -- for backwards compatibility, special case ImageOriginal
  toFilePath (ImagePath (ImageOriginal csum typ)) = toFilePath (csum, typ)
  toFilePath p = toURIDir p </> makeRelative "/" (unpack (toPathInfo p))

instance HasFilePath ImageCached where
  toURIDir c = toURIDir (imagePath c)
  toFilePath c@(ImageCached _ _) = toFilePath (imagePath c)

instance HasFilePath ImageKey where
  toURIDir key = toURIDir (ImagePath key)
  toFilePath key = toFilePath (ImagePath key)

-- | The full path name for the local cache of the file.
fileCachePath ::
  (HasFilePath a, MonadReader r m, HasFileCacheTop r, HasCallStack)
  => a -> m FilePath
fileCachePath file = do
  (FileCacheTop top) <- fileCacheTop <$> ask
  let path = toFilePath file
  return $ top </> makeRelative "/" path
  where _ = callStack

-- | Create any missing directories and evaluate 'fileCachePath'
fileCachePathIO ::
  (MonadFileCache r e m, HasFilePath a, HasCallStack)
  => a -> m FilePath
fileCachePathIO file = do
  path <- fileCachePath file
  let dir = takeDirectory path
  liftIO $ createDirectoryIfMissing True dir
  return path

-- * FileCacheT

#if 0
type FileCacheT r s m = RWST r () s m

runFileCacheT :: r -> s -> FileCacheT r s m a -> m (a, s, ())
runFileCacheT r s0 action = runRWST action r s0

evalFileCacheT :: Functor m => r -> s -> FileCacheT r s m a -> m a
evalFileCacheT r s0 action = view _1 <$> runFileCacheT r s0 action
execFileCacheT :: Functor m => r -> s -> FileCacheT r s m a-> m s
execFileCacheT r s0 action = view _2 <$> runFileCacheT r s0 action
#endif

askCacheAcid :: (MonadReader r m, HasCacheAcid r, HasCallStack) => m CacheAcid
askCacheAcid = cacheAcid <$> ask
  where _ = callStack

-- | insert a (key, value) pair into the cache
cachePut ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => ImageKey -> Either FileError ImageFile -> m (Either FileError ImageFile)
cachePut key val = do
  st <- askCacheAcid
  update' st (PutValue key val)
  return val

-- | insert a (key, value) pair into the cache, discard result.
cachePut_ ::
  (MonadFileCache r e m, HasCallStack)
  => ImageKey -> Either FileError ImageFile -> m ()
cachePut_ key val = do
  st <- askCacheAcid
  update' st (PutValue key val)

-- | Query the cache, but do nothing on cache miss.
cacheLook ::
  forall e r m. (MonadFileCache r e m, HasCallStack)
  => ImageKey -> m (Maybe (Either FileError ImageFile))
cacheLook key = do
  st <- askCacheAcid
  {-either (Just . Left) (fmap id) <$>-}
  query' st (LookValue key)

cacheMap :: (MonadFileCache r e m, HasCallStack) => m CacheMap
cacheMap = do
  st <- askCacheAcid
  query' st LookMap

cacheDelete ::
  forall e r m. (MonadFileCache r e m, HasCallStack)
  => Proxy ImageFile -> Set ImageKey -> m ()
cacheDelete _ keys = do
  (st :: AcidState CacheMap) <- cacheAcid <$> ask
  update' st (DeleteValues keys)
  where _ = callStack

-- To do - files that are in the image database but not referenced in
-- the appraisal database.
data Classified =
  Classified
  { orphans :: Set FilePath
    -- ^ Original files that are not in the database.  What happened?
  , orphansDerived :: Set FilePath
    -- ^ Derived files not in database. These can be deleted.
  , known :: Set (FilePath, ImageKey) -- ^ Files in the database.
  , knownDerived :: Set (FilePath, ImageKey) -- ^ Files in the database that are derived
  , errors :: Set (FilePath, ImageKey)  -- ^ Files marked as errors.
  } deriving (Generic, Show)

isDerived :: FilePath -> FilePath -> Bool
isDerived top path = do
  -- top="/home/dsf/appraisalscribe3-development/images"
  elem '/' $ drop 4 $ fromMaybe (error "Unexpected prefix: " <> show path) $ stripPrefix top path

collectGarbage ::
  forall r m. (MonadIO m, MonadReader r m, HasFileCacheTop r)
  => Map ImageKey (Either FileError ImageFile)
  -> m Classified
collectGarbage mp = do
  FileCacheTop top <- fileCacheTop <$> ask
  files <- Set.fromList <$> liftIO (find always (fileType ==? Find.RegularFile) top)
  r :: Classified
    <- ifoldlM f (Classified files mempty mempty mempty mempty) mp
  let (d1, o1) = Set.partition (isDerived top) (orphans r)
  let (d2, o2) = Set.partition (isDerived top . fst) (known r)
  pure $
    set #orphansDerived d1 $
    set #orphans o1 $
    set #knownDerived d2 $
    set #known o2 $
    r

  -- pure $ foldlWithKey (f files) (Classified files mempty mempty) mp
  where
    f :: ImageKey
      -> Classified
      -> Either FileError ImageFile
      -> m Classified
    f key classified file = do
      path :: FilePath
        <- fileCachePath key
      let m :: FilePath -> Lens' Classified (Maybe ())
          m k = #orphans . at k
      case file of
          Left _e ->
            pure $
            set (m path) Nothing $
            set (#errors . at (path, key)) (Just ()) classified
          Right _i -> liftIO (doesFileExist path) >>= \case
            True ->
              pure $
              set (#orphans . at path) Nothing $
              set (#known . at (path, key)) (Just ()) classified
            False -> pure classified
