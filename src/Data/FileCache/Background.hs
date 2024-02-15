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

import Control.Concurrent (ThreadId{-, threadDelay-}, newChan, readChan, writeChan)
import Control.Concurrent.Chan (Chan)
import Control.Exception (IOException)
import Control.Lens
import Control.Monad (forever, when)
import Control.Monad.Reader (MonadReader(ask), runReaderT)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Data.FileCache.FileCacheTop
import Data.FileCache.Derive (CacheFlag, cacheImageFile, cacheImageShape, getImageFiles, getImageShapes)
import Data.FileCache.FileError (FileCacheErrors, FileError(UnexpectedException))
import Data.FileCache.ImageFile
import Data.FileCache.ImageKey
import Data.FileCache.ImageShape
import Data.Generics.Sum (_Ctor)
import Data.ListLike ( length, show )
import Data.Map.Strict as Map ( filter, fromList, keysSet, Map, size )
import Data.Maybe (mapMaybe)
import Data.Monoid ( (<>) )
import Data.SafeCopy (SafeCopy)
import Data.Serialize (Serialize)
import Data.Set as Set (Set, toList)
import Data.Typeable (Typeable)
import Extra.Except (throwError)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Prelude hiding (length, show)
import SeeReason.LogServer (alog)
import SeeReason.Errors (Member, OneOf, throwMember)
import SeeReason.UIO (fork, liftUIO, NonIOException, UIO, Unexceptional, unsafeFromIO)
import System.Log.Logger (Priority(..))

{-
class HasSomeNonPseudoException e where
  someNonPseudoException :: Prism e SomeNonPseudoException

instance HasSomeNonPseudoException ReportError where
  someNonPseudoException = _Ctor @"FileError" . someNonPseudoException
-}

type ImageChan = Chan [(ImageKey, ImageShape)]
class HasImageBuilder a where imageBuilder :: a -> Maybe ImageChan
instance HasImageBuilder ImageChan where imageBuilder = Just
instance HasImageBuilder (a, b, ImageChan) where imageBuilder = Just . view _3
instance HasFileCacheTop top => HasFileCacheTop (CacheAcid, top) where fileCacheTop = fileCacheTop . snd

cacheDerivedImagesBackground ::
  forall r e m. (FileCacheErrors e m, MonadReader r m, HasCacheAcid r,
                 HasImageBuilder r, HasFileCacheTop r)
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
cacheDerivedImagesBackground flags keys =
  mapM (\key -> (key,) <$> cacheImageShape flags key Nothing) keys >>=
  runExceptT . backgroundBuilds >>=
  either throwError (return . Map.fromList)

backgroundBuilds ::
  (Unexceptional m, FileCacheErrors e m, MonadReader r m, HasImageBuilder r)
  => [(ImageKey, Either FileError ImageFile)]
  -> m [(ImageKey, Either FileError ImageFile)]
backgroundBuilds pairs =
  queueImageBuild (mapMaybe isShape pairs) >> return pairs
  where isShape (key, Right (ImageFileShape shape)) = Just (key, shape)
        isShape _ = Nothing

-- | Insert an image build request into the channel that is being polled
-- by the thread launched in startCacheImageFileQueue.
queueImageBuild ::
  (FileCacheErrors e m, MonadReader r m, HasImageBuilder r, HasCallStack)
  => [(ImageKey, ImageShape)]
  -> m ()
queueImageBuild pairs = do
  -- Write empty files into cache
  -- mapM (runExceptT . fileCachePathIO) pairs >>= mapM_ (either (throwError . review fileError) (liftUIO . flip writeFile mempty))
  unsafeFromIO $ alog DEBUG ("queueImageBuild - requesting " ++ show (length pairs) ++ " images")
  chan <- maybe (throwMember (UnexpectedException "Chan Is Missing")) pure =<< (imageBuilder <$> ask)
  liftUIO (writeChan chan pairs)
  unsafeFromIO $ alog DEBUG ("queueImageBuild - requested " ++ show (length pairs) ++ " images")

-- | Fork a thread into the background that loops forever reading
-- (key, shape) pairs from the channel and building the corresponding
-- image file.
startImageBuilder ::
  forall e m rd.
  (Unexceptional m, HasCacheAcid rd, HasFileCacheTop rd,
   MonadError (OneOf e) m, Member NonIOException e, Member IOException e,
   HasCallStack)
  => rd
  -> m (ImageChan, ThreadId)
startImageBuilder rd = do
  (chan :: ImageChan) <- liftUIO newChan
  unsafeFromIO $ alog DEBUG "Starting background image builder"
  (,) <$> pure chan <*> (fork (task chan >>= either (error . show) pure) :: m ThreadId)
  where
    task :: ImageChan -> UIO (Either (OneOf '[IOException, NonIOException]) ())
    task chan = forever (runExceptT (liftUIO (readChan chan) >>= doImages rd))
#if 0
  (,) <$> pure chan <*> liftUIO (fork (forever (runExceptT (fromIO (readChan chan) :: ExceptT SomeNonPseudoException m [(ImageKey, ImageShape)]) >>= (doImages @e rd))))
#endif

-- readChan :: Chan a -> IO a
-- liftUIO :: (Unexceptional m, Member NonIOException e, Member IOException e, MonadError (OneOf e) m) => IO a -> m a
-- fromIO :: Unexceptional m => IO a -> ExceptT SomeNonPseudoException m a
-- fork :: Unexceptional m => UIO () -> m ThreadId

-- doImages' :: forall e rd. (HasCacheAcid rd, HasFileCacheTop rd, Member IOException e, Member NonIOException e) => rd -> Either SomeNonPseudoException [(ImageKey, ImageShape)] -> ExceptT (OneOf e) UIO ()
-- doImages' = doImages

-- | This is the background task.
doImages ::
  forall rd. (HasCacheAcid rd, HasFileCacheTop rd)
  => rd
  -> [(ImageKey, ImageShape)]
  -> ExceptT (OneOf '[IOException, NonIOException]) UIO ()
doImages rd pairs = do
  unsafeFromIO $ when (length pairs > 0) $ alog DEBUG ("doImages - building " ++ show (length pairs) ++ " images")
  -- the threadDelay is to test the behavior of the server for lengthy image builds
  (r :: [Either FileError ImageFile]) <- mapM (\(key, shape) -> runReaderT (cacheImageFile key shape {- >> unsafeFromIO (threadDelay 5000000)-}) rd) pairs
  mapM_ (\case ((key, _shape), Left e) -> unsafeFromIO (alog ERROR ("doImages - error building " <> show key <> ": " ++ show e))
               -- ((key, shape), Right _file) -> unsafeFromIO (alog ERROR ("doImages - error in cache for " <> show key <> ": " ++ show e))
               ((key, _shape), Right _file) -> unsafeFromIO (alog ERROR ("doImages - completed " <> show key)))
    (zip pairs r)
{-
doImages _rd (Left e) = do
  unsafeFromIO $ alog DEBUG ("Failure in readChan: " <> show e)
-}

-- | Throw an exception if there are more than 20 unavailable
-- images.  This sends the images to the background image
-- generator thread, aborts whatever we are doing, and puts up a
-- "come back later" message.
testImageKeys ::
  forall r e m.
  (HasCacheAcid r,
   HasFileCacheTop r,
   HasImageBuilder r,
   Member IOException e,
   Member NonIOException e,
   Member FileError e,
   Member ImageStats e,
   MonadError (OneOf e) m,
   MonadReader r m,
   Show (OneOf e),
   Unexceptional m,
   Typeable e,
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
  unsafeFromIO $ do
    alog DEBUG ("#keys=" <> show (_keys stats))
    alog DEBUG ("#ready=" <> show (_ready stats))
    alog DEBUG ("#unready image shapes: " <> show (_shapes stats))
    alog DEBUG ("#errors=" <> show (_errors stats))
  case _shapes stats + _errors stats > 20 of
    True -> do
      unsafeFromIO $ alog DEBUG ("cacheDerivedImagesBackground " <> show needed)
      cacheDerivedImagesBackground mempty (Set.toList needed)
      throwMember stats
    False -> do
      _ <- getImageFiles mempty needed
      unsafeFromIO $ alog DEBUG ("getImageFiles " <> show needed)
      pure ()

