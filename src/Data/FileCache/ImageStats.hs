{-# LANGUAGE AllowAmbiguousTypes, DeriveAnyClass, LambdaCase #-}
{-# OPTIONS -ddump-minimal-imports #-}

module Data.FileCache.ImageStats
  ( ImageStats(..)
  , imageStatsDefault
  , imageStatsError
#if !__GHCJS__
  , testImageKeys
  , foregroundOrBackground
#endif
  ) where

import Control.Lens.Path ( Value(..) )
import Data.SafeCopy (SafeCopy(version, kind), extension, Migrate(MigrateFrom, migrate), safeGet, safePut)
import Data.Serialize ( Serialize(..) )
import GHC.Generics ( Generic )
import GHC.Stack (CallStack, callStack, emptyCallStack, HasCallStack)

#if !__GHCJS__
import Control.Lens ( Field1(_1), has, to, view, _Left, _Right, over )
import Data.FileCache.Background ( HasTaskQueue(taskQueue) )
import Data.FileCache.Derive ( getImageFiles, getImageShapes )
import Data.FileCache.FileCacheTop ( MonadFileCache, MonadFileCacheWriter )
import Data.FileCache.FileError ( FileError )
import Data.FileCache.ImageFile ( ImageFile )
import Data.FileCache.ImageKey ( ImageKey )
import Data.Generics.Sum ( _Ctor )
import Data.Map.Strict as Map ( filter, keysSet, Map, size, toList, union )
import Data.Monoid ( (<>) )
import Data.Set as Set ( Set, toList )
import SeeReason.Errors ( throwMember, Member )
import SeeReason.Log ( alog, alogDrop )
import System.Log.Logger ( Priority(..) )
#endif

-- * ImageStats

-- Statistics about the server status of the images in this reports.
data ImageStats
  = ImageStats
    { _keys :: Int
    , _ready :: Int
    , _shapes :: Int
    , _errors :: [String]
    , _stack :: CallStack
    } deriving (Generic, Eq, Ord, Show)

instance Value ImageStats where hops _ = []
instance SafeCopy ImageStats where version = 1; kind = extension
instance Serialize ImageStats where get = safeGet; put = safePut

instance Migrate ImageStats where
  type MigrateFrom ImageStats = ImageStats_0
  migrate (ImageStats_0 a b c _) =  ImageStats a b c [] emptyCallStack

data ImageStats_0
  = ImageStats_0
    { _keys_0 :: Int
    , _ready_0 :: Int
    , _shapes_0 :: Int
    , _errors_0 :: Int
    } deriving (Generic, Eq, Ord, Show)

instance SafeCopy ImageStats_0
instance Serialize ImageStats_0 where get = safeGet; put = safePut

imageStatsDefault = ImageStats 0 0 0 [] emptyCallStack

-- | This is thrown if we get some timeout not releated to image
-- generation - i.e. generating the latex or pdf file took too long.
imageStatsError :: (Show e, HasCallStack) => e -> ImageStats
imageStatsError e = ImageStats 0 0 0 [show e] callStack


#if !__GHCJS__
-- | Throw an exception if there are more than 20 unavailable
-- images.  This sends the images to the background image
-- generator thread, aborts whatever we are doing, and puts up a
-- "come back later" message.
testImageKeys ::
  forall a e m. (MonadFileCache a e m, HasCallStack)
  => Set ImageKey
  -> m (Map ImageKey (Either FileError ImageFile), ImageStats)
testImageKeys ks = do
  shapes :: Map ImageKey (Either FileError ImageFile)
    <- getImageShapes mempty ks
  let ready = Map.filter (has (_Right . _Ctor @"ImageFileReady")) shapes
      -- Files that have been requested but not yet written out
      unready = Map.filter (has (_Right . _Ctor @"ImageFileShape")) shapes
      -- Files that were written out but have gone missing, or were
      -- recorded with the retired NoShapeOld error constructor.
      missing = Map.filter (\e -> has (_Left . _Ctor @"MissingDerivedEntry") e ||
                                  has (_Left . _Ctor @"NoShapeOld") e) shapes
      -- needed = Map.keysSet unready <> Map.keysSet missing
  let stats = ImageStats {_keys = Map.size shapes,
                          _ready = Map.size ready,
                          _shapes = Map.size unready,
                          _errors = fmap show (Map.toList missing),
                          _stack = callStack }
  alog DEBUG ("#keys=" <> show (_keys stats))
  alog DEBUG ("#ready=" <> show (_ready stats))
  alog DEBUG ("#unready image shapes: " <> show (_shapes stats))
  alogDrop id DEBUG ("#errors=" <> show (_errors stats))
  pure (Map.union unready missing, stats)

-- | Decide whether there are enough images to be built that we
-- need to do them in the background
foregroundOrBackground ::
  forall key a e m.
  (MonadFileCacheWriter a e m,
   HasTaskQueue key a,
   Member ImageStats e,
   HasCallStack)
  => ([ImageKey] -> m ())
  -> Set ImageKey
  -> m ()
foregroundOrBackground enq ks = do
  alogDrop id INFO ("#ks=" <> show (length ks))
  (needed, stats) <- over _1 Map.keysSet <$> testImageKeys ks
  -- let needed = Map.keysSet mp
  view (to (taskQueue @key)) >>= \case
    -- Number of errors does not seem to be a useful heuristic - can
    -- these errors be corrected by waiting?
    Just _ | _shapes stats + length (_errors stats) > 20 -> do
      alog DEBUG ("needed=" <> show needed)
      enq (Set.toList needed)
      throwMember stats
    _ -> do
      _ <- getImageFiles mempty needed
      alog DEBUG ("getImageFiles " <> show needed)
      pure ()
#endif
