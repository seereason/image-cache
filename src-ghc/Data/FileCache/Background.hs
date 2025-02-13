-- Unused code that once did image builds in the background

{-# LANGUAGE DeriveAnyClass, DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Background
  ( ImageChan
  , HasImageBuilder(imageBuilder)
  , cacheDerivedImagesBackground
  , startImageBuilder
  -- , queueImageBuild
  , testImageKeys
  ) where

import Control.Concurrent as IO (ThreadId{-, threadDelay-}, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Concurrent.Thread (forkIO, Result)
import Control.Exception (IOException)
import Control.Lens
import Control.Monad (forever, when)
import Control.Monad.Reader (liftIO, MonadReader(ask), runReaderT)
import Control.Monad.Except (runExceptT)
import Data.FileCache.FileCacheTop
import Data.FileCache.Derive (CacheFlag, cacheImageFile, cacheImageShape, getImageFiles, getImageShapes)
import Data.FileCache.FileError (FileError)
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey
import Data.Generics.Sum (_Ctor)
import Data.ListLike ( length, show )
import Data.Map.Strict as Map ( filter, fromList, keysSet, Map, size )
import Data.Maybe (mapMaybe)
import Data.Monoid ( (<>) )
import Data.Set as Set (Set, toList)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.LogServer (alog)
import SeeReason.Errors (Member, OneOf, throwMember)
import System.Log.Logger (Priority(..))

{-
class HasSomeNonPseudoException e where
  someNonPseudoException :: Prism e SomeNonPseudoException

instance HasSomeNonPseudoException ReportError where
  someNonPseudoException = _Ctor @"FileError" . someNonPseudoException
-}

type ImageChan = Chan [(ImageKey, ImageShape)]
class HasImageBuilder a where
  imageBuilder :: a -> Maybe (ImageChan, (ThreadId, IO (Result ())))
instance HasImageBuilder (ImageChan, (ThreadId, IO (Result ()))) where imageBuilder = Just
instance HasImageBuilder (a, b, (ImageChan, (ThreadId, IO (Result ())))) where imageBuilder = Just . view _3
instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

cacheDerivedImagesBackground ::
  forall r e m. (MonadFileCache r e m, HasImageBuilder r, HasCallStack)
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
cacheDerivedImagesBackground flags keys =
  mapM (\key -> (key,) <$> cacheImageShape flags key Nothing) keys >>=
  backgroundBuilds >>= pure . Map.fromList

backgroundBuilds ::
  (MonadFileCache r e m, HasImageBuilder r, HasCallStack)
  => [(ImageKey, Either FileError ImageFile)]
  -> m [(ImageKey, Either FileError ImageFile)]
backgroundBuilds pairs =
  queueImageBuild (mapMaybe isShape pairs) >> return pairs
  where isShape (key, Right (ImageFileShape shape)) = Just (key, shape)
        isShape _ = Nothing

-- | Insert an image build request into the channel that is being polled
-- by the thread launched in startCacheImageFileQueue.
queueImageBuild ::
  (MonadFileCache r e m, HasImageBuilder r, HasCallStack)
  => [(ImageKey, ImageShape)]
  -> m ()
queueImageBuild pairs = do
  -- Write empty files into cache
  alog DEBUG ("queueImageBuild - requesting " ++ show (length pairs) ++ " images")
  (chan, _) <- maybe (error "Chan Is Missing") pure =<< (imageBuilder <$> ask)
  liftIO (writeChan chan pairs)
  alog DEBUG ("queueImageBuild - requested " ++ show (length pairs) ++ " images")

type E = '[FileError, IOException]

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startImageBuilder ::
  forall r.
  (HasCacheAcid r,
   HasFileCacheTop r,
   HasCallStack)
  => r
  -> IO (ImageChan, (ThreadId, IO (Result ())))
startImageBuilder r = do
  (chan :: ImageChan) <- newChan
  alog DEBUG "Starting background image builder"
  (,) <$> pure chan <*> forkIO (task chan)
  where
    task :: ImageChan -> IO ()
    task chan = forever $
      readChan chan >>= doImages r

-- | This is the background task.
doImages ::
  forall r. (HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => r
  -> [(ImageKey, ImageShape)]
  -> IO ()
doImages r pairs = do
  when (length pairs > 0) $ alog DEBUG ("doImages - building " ++ show (length pairs) ++ " images")
  -- the threadDelay is to test the behavior of the server for lengthy image builds
  files <- mapM doShape pairs
  mapM_ doFile (zip pairs files)
  where
    doShape :: (ImageKey, ImageShape) -> IO (Either (OneOf E) (Either FileError ImageFile))
    doShape (key, shape) = runExceptT (runReaderT (cacheImageFile key shape {- >> liftIO (threadDelay 5000000)-}) r)

    doFile :: ((ImageKey, ImageShape), Either (OneOf E) (Either FileError ImageFile)) -> IO ()
    doFile ((key, _shape), Left e) = alog ERROR ("doImages - error building " <> show key <> ": " ++ show e)
    doFile ((key, _shape), Right (Left e)) = alog ERROR ("doImages - error building " <> show key <> ": " ++ show e)
    doFile ((key, _shape), Right (Right _file)) = alog INFO ("doImages - completed " <> show key)

-- | Throw an exception if there are more than 20 unavailable
-- images.  This sends the images to the background image
-- generator thread, aborts whatever we are doing, and puts up a
-- "come back later" message.
testImageKeys ::
  forall r e m.
  (MonadFileCache r e m,
   HasImageBuilder r,
   Member ImageStats e,
   HasCallStack)
  => Set ImageKey
  -> m ()
testImageKeys ks = do
  shapes <- getImageShapes mempty ks
  let ready = Map.filter (has (_Right . _Ctor @"ImageFileReady")) shapes
      -- Files that have been requested but not yet written out
      unready = Map.filter (has (_Right . _Ctor @"ImageFileShape")) shapes
      -- Files that were written out but have gone missing, or were
      -- recorded with the retired NoShapeOld error constructor.
      missing = Map.filter (\e -> has (_Left . _Ctor @"MissingDerivedEntry") e ||
                                  has (_Left . _Ctor @"NoShapeOld") e) shapes
      needed = Map.keysSet unready <> Map.keysSet missing
  let stats = ImageStats {_keys = Map.size shapes,
                          _ready = Map.size ready,
                          _shapes = Map.size unready,
                          _errors = Map.size missing }
  alog DEBUG ("#keys=" <> show (_keys stats))
  alog DEBUG ("#ready=" <> show (_ready stats))
  alog DEBUG ("#unready image shapes: " <> show (_shapes stats))
  alog DEBUG ("#errors=" <> show (_errors stats))
  view (to imageBuilder) >>= \case
    Just _ | _shapes stats + _errors stats > 20 -> do
      alog DEBUG ("cacheDerivedImagesBackground " <> show needed)
      cacheDerivedImagesBackground mempty (Set.toList needed)
      throwMember stats
    _ -> do
      _ <- getImageFiles mempty needed
      alog DEBUG ("getImageFiles " <> show needed)
      pure ()
