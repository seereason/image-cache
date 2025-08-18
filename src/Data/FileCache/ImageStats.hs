{-# LANGUAGE AllowAmbiguousTypes, DeriveAnyClass, LambdaCase #-}
{-# OPTIONS -ddump-minimal-imports #-}

module Data.FileCache.ImageStats
  ( ImageStats(_keys, _ready, _shapes, _errors)
  , imageStatsTimeout
  , imageStatsDefault
#if !__GHCJS__
  , testImageKeys
  , foregroundOrBackground
#endif
  ) where

import Control.Lens.Path ( Value(..) )
import Data.SafeCopy ( SafeCopy )
import Data.Serialize ( Serialize(..) )
import GHC.Generics ( Generic )

#if !__GHCJS__
import Control.Lens ( Field1(_1), has, to, view, _Left, _Right, over )
import Data.FileCache.Background ( HasTaskQueue(taskQueue) )
import Data.FileCache.Derive ( getImageFiles, getImageShapes )
import Data.FileCache.FileCacheTop ( MonadFileCache )
import Data.FileCache.FileError ( FileError )
import Data.FileCache.ImageFile ( ImageFile )
import Data.FileCache.ImageKey ( ImageKey )
import Data.Generics.Sum ( _Ctor )
import Data.Map.Strict as Map ( filter, keysSet, Map, size, union )
import Data.Monoid ( (<>) )
import Data.Set as Set ( Set, toList )
import GHC.Stack ( HasCallStack )
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
    , _errors :: Int
    } deriving (Generic, Eq, Ord, Show, Serialize)

instance SafeCopy ImageStats
instance Value ImageStats where hops _ = []

imageStatsDefault = ImageStats 0 0 0 0

-- | This is thrown if we get some timeout not releated to image
-- generation - i.e. generating the latex or pdf file took too long.
imageStatsTimeout = ImageStats 0 0 0 1


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
                          _errors = Map.size missing }
  alog DEBUG ("#keys=" <> show (_keys stats))
  alog DEBUG ("#ready=" <> show (_ready stats))
  alog DEBUG ("#unready image shapes: " <> show (_shapes stats))
  alogDrop id DEBUG ("#errors=" <> show (_errors stats))
  pure (Map.union unready missing, stats)
-- | Decide whether there are enough images to be built that we
-- need to do them in the background
foregroundOrBackground ::
  forall key a e m.
  (MonadFileCache a e m,
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
    Just _ | _shapes stats + _errors stats > 20 -> do
      alog DEBUG ("needed=" <> show needed)
      enq (Set.toList needed)
      throwMember stats
    _ -> do
      _ <- getImageFiles mempty needed
      alog DEBUG ("getImageFiles " <> show needed)
      pure ()
#endif
