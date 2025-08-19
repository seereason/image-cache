-- | Create derived images

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedLists, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}
{-# OPTIONS -Wno-deprecations #-}

module Data.FileCache.Derive
  ( getImageFile
  , getImageFiles
  , getImageShapes
  , cacheImageFile
  , cacheImageShape
  , buildImageFile
  ) where

import Control.Lens ( _Right, over )
import Control.Monad.Reader (liftIO, unless, when)
import qualified Data.ByteString.Lazy as BS ( ByteString, readFile )
import Data.ByteString.UTF8 as UTF8 ()
import Data.Digest.Pure.MD5 ( md5 )
import Data.FileCache.CacheMap ( ImageCached(ImageCached) )
import Data.FileCache.File ( File(File, _fileExt, _fileMessages, _fileChksum, _fileSource), FileSource(Derived, ThePath), HasFileExtension(..) )
import Data.FileCache.FileCache ( cacheLook, cachePut, cachePut_, fileCachePath, fileCachePathIO, HasFilePath )
import Data.FileCache.FileCacheTop ( MonadFileCache )
import Data.FileCache.FileError
  ( FileError(NoShapeFromKey, DamagedOriginalFile, MissingOriginalFile, MissingDerivedEntry,
              CacheDamageMigrated, MissingOriginalEntry, UnexpectedException), CacheFlag(RetryErrors) )
import Data.FileCache.ImageFile ( ImageFile(..), ImageReady(ImageReady, _imageFile, _imageShape) )
import Data.FileCache.ImageIO ( editImage', scaleImage', uprightImage', MakeByteString(makeByteString) )
import Data.FileCache.ImageKey
  ( ImageKey(..), ImagePath(ImagePath), originalKey, shapeFromKey,
    HasFileType(imageType), FileType, imageShape, HasImageShapeM(imageShapeM),
    ImageShape(_imageShapeType) )
import Data.FileCache.ImageRect (HasImageRect(imageRect), scaleFromDPI)
import Data.FileCache.Rational (fromRat)
import Data.FileCache.Upload ( cacheOriginalFile )
import Data.ListLike ( ListLike(length) )
import Data.Map.Strict as Map ( Map, fromSet, mapWithKey )
import Data.Monoid ( (<>) )
import Data.Set as Set ( member, Set )
import Data.Text as T ( Text, pack )
import Debug.Trace
import GHC.Stack (callStack, HasCallStack)
import Prelude hiding (length)
import SeeReason.Errors ( throwMember, tryMember )
import SeeReason.Log ( alog )
import System.Directory ( doesFileExist )
import System.FilePath ()
import System.FilePath.Extra ( writeFileReadable )
import System.Log.Logger ( Priority(..) )
import System.Posix.Files (createLink, removeLink)
import Text.PrettyPrint.HughesPJClass ( prettyShow )

getImageFiles ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => Set CacheFlag
  -> Set ImageKey
  -> m (Map ImageKey (Either FileError ImageFile))
getImageFiles flags keys =
  getImageShapes flags keys >>= pure . mapWithKey f >>= sequence
  where
    f :: ImageKey -> Either FileError ImageFile -> m (Either FileError ImageFile)
    f key (Right (ImageFileShape shape)) = cacheImageFile key shape
    f _key (Right ready@(ImageFileReady _)) = pure (Right ready)
    f _key (Left e) = pure (Left e)

-- | This is just a wrapper around cacheDerivedImagesForeground.
getImageFile ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => Set CacheFlag
  -> ImageKey
  -> m (Either FileError ImageFile)
getImageFile flags key =
  cacheLook key >>= cacheImageShape flags key >>= either (pure . Left) (buildImage key)

-- mapWithKeyM :: forall k v m. (Ord k, Monad m) => (k -> v -> m (k, v)) -> Map k v -> m (Map k v)
-- mapWithKeyM f mp = fromList <$> mapM (uncurry f) (Map.toList mp :: [(k, v)])

getImageShapes ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => Set CacheFlag
  -> Set ImageKey
  -> m (Map ImageKey (Either FileError ImageFile))
getImageShapes flags keys =
  sequence $ fromSet (\key -> cacheLook key >>= (cacheImageShape flags key :: Maybe (Either FileError ImageFile) -> m (Either FileError ImageFile)) {-maybe (cacheImageShape flags key) pure-}) keys

-- | Compute the shapes of requested images
cacheImageShape ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => Set CacheFlag
  -> ImageKey
  -> Maybe (Either FileError ImageFile)
  -> m (Either FileError ImageFile)
cacheImageShape _ key Nothing = do
  -- alog DEBUG ("cacheImageShape key=" ++ prettyShow key ++ " (miss)")
  cachePut_ key (Left (NoShapeFromKey key))
  buildAndCache
    where
      buildAndCache :: m (Either FileError ImageFile)
      buildAndCache =
        tryMember @FileError (buildImageShape key) >>= cachePut key . over _Right ImageFileShape
cacheImageShape flags key (Just (Left _))
  | Set.member RetryErrors flags = do
      alog INFO ("cacheImageShape key=" ++ prettyShow key ++ " (retry)")
      buildAndCache
        where
          buildAndCache :: m (Either FileError ImageFile)
          buildAndCache =
            tryMember @FileError (buildImageShape key) >>= cachePut key . over _Right ImageFileShape
cacheImageShape flag key (Just (Left e)) = do
  alog INFO ("cacheImageShape key=" ++ prettyShow key ++ " (e=" <> show e <> ")")
  cacheImageShape flag key Nothing
cacheImageShape _ _ (Just (Right (ImageFileShape shape))) = do
  -- alog INFO ("cacheImageShape key=" ++ prettyShow key ++ " (shape)")
  -- This value shouldn't be here in normal operation
  return (Right (ImageFileShape shape))
cacheImageShape _ key (Just (Right (ImageFileReady img))) = do
  path <- fileCachePath (ImagePath key)
  liftIO (doesFileExist path) >>= \case
    False -> do
      alog WARNING ("missing cache file: " <> prettyShow key <> " -> " <> show path)
      pure $ Left $ MissingDerivedEntry key
    True -> do
      -- alog DEBUG ("cacheImageShape key=" ++ prettyShow key ++ " (hit)")
      pure $ Right $ ImageFileReady img

-- | These are meant to be inexpensive operations that determine the
-- shape of the desired image, with the actual work of building them
-- deferred and forked into the background.  Are they actually
-- inexpensive?  We read the original bytestring and call file(1) to
-- get the shape and orientation of that image, and the other
-- operations are pure.
buildImageShape ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => ImageKey
  -> m ImageShape
buildImageShape key0 =
  originalShape key0 >>= \shape -> pure $ shapeFromKey shape key0
  where
    originalShape key@(ImageOriginal csum typ) =
      cacheLook (originalKey key) >>=
      maybe
        (do alog ALERT ("Missing original: " <> show (originalKey key))
            path <- fileCachePath (csum, typ)
            (_key, file) <- cacheOriginalFile (Just (ThePath path)) path
            alog ALERT ("Missing original found: " <> show path)
            pure $ fileShape file)
        (either throwMember (pure . fileShape))
    originalShape key = originalShape (originalKey key)
    fileShape (ImageFileReady img) = _imageShape img
    fileShape (ImageFileShape s) = s

buildImage ::
  (MonadFileCache r e m, HasCallStack)
  => ImageKey
  -> ImageFile
  -> m (Either FileError ImageFile)
buildImage key (ImageFileShape shape) = cacheImageFile key shape
buildImage _ i@(ImageFileReady _) = pure (Right i)

-- | Look up the key in the cache, if a miss call 'buildImageFile' and
-- cache the result.
cacheImageFile ::
  (MonadFileCache r e m, HasCallStack)
  => ImageKey
  -> ImageShape
  -> m (Either FileError ImageFile)
cacheImageFile key _shape = do
  -- Check the cache one last time - this image might appear more than once in this request
  cacheLook key >>=
    maybe (pure (Left (UnexpectedException "Impossible cache miss")))
          (either (pure . Left)
            (\case file@(ImageFileReady (ImageReady {..})) -> do
                     path <- fileCachePathIO key
                     -- It appears it was already built and cached.
                     -- But is it actually there?
                     liftIO (doesFileExist (traceWith ("path=" <>) path)) >>= \case
                       True -> pure (Right file) -- smooth sailing
                       False -> do
                         -- Cache file is missing, rebuild it
                         tryMember @FileError (buildImageFile key _imageShape) >>= cachePut key
                         -- did that work?
                         liftIO (doesFileExist path) >>= \case
                           True -> alog NOTICE ("Missing image cache file re-created: " <> show path)
                           -- It did not - very bad situation, this will happen every time.
                           False -> alog EMERGENCY ("Unable to re-create missing image cache file: " <> show path)
                         pure (Right file)
                   (ImageFileShape shape') -> do
                     -- Proceed with the build
                     tryMember @FileError (buildImageFile key shape') >>= cachePut key))

traceWith :: (a -> String) -> a -> a
traceWith f a = trace (f a) a

-- | Like `traceWith`, but skip the trace if @f@ returns `Nothing`.
-- traceIf :: (a -> Maybe String) -> a -> a
-- traceIf f a = maybe a (\s -> trace s a) (f a)

-- | Given an 'ImageKey' and 'ImageShape', build the corresponding
-- 'ImageFile' and write the image file.  This can be used to repair
-- missing cache files.
buildImageFile ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => ImageKey -> ImageShape -> m ImageFile
buildImageFile key shape = do
  (key', bs) <- buildImageBytes Nothing key
  -- key' may differ from key due to removal of no-ops.  If so we hard
  -- link the corresponsing image files so both keys work.
  let file = File { _fileSource = Derived
                  , _fileChksum = T.pack $ show $ md5 bs
                  , _fileMessages = []
                  , _fileExt = fileExtension (_imageShapeType shape) }
  let img = ImageFileReady (ImageReady { _imageFile = file, _imageShape = shape })
  path <- fileCachePathIO (ImageCached key img) -- the rendered key
  liftIO (doesFileExist path) >>= \case
    False -> do
      alog INFO ("Writing new cache file: " <> show path)
      liftIO $ writeFileReadable path bs
    True -> do
      -- The cached file exists.
      bs' <- liftIO $ BS.readFile path
      case bs == bs' of
        False -> do
          -- Do we have to worry that this file is in the process of
          -- being written?  This needs review.  Assuming it is
          -- damaged because the contents do not match.
          alog WARNING ("Replacing damaged cache file: " <> show path <>
                                       " length " <> show (length bs') <>
                                       " -> " <> show (length bs))
          liftIO $ writeFileReadable path bs
        True ->
          -- The image file already exists and contains what we
          -- expected.  Is this worth a warning?
          alog WARNING ("Cache file for new key already exists: " <> show path)
  path' <- fileCachePathIO (ImageCached key' img) -- the equivalent file
  when (path /= path') $ do
    -- The key contained no-ops, so the returned key is different.
    -- Hard link the returned key to the image file.
    liftIO (doesFileExist path') >>= \case
      True -> do
        bs' <- liftIO $ BS.readFile path'
        unless (bs == bs') $ do
          liftIO (removeLink path')
          liftIO (createLink path path')
      False -> do
        liftIO (createLink path path')
  -- alog DEBUG ("added to cache: " <> prettyShow img)
  return img

-- | Retrieve the 'ByteString' associated with an 'ImageKey'.
buildImageBytes ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => Maybe FileSource -> ImageKey -> m (ImageKey, BS.ByteString)
buildImageBytes source key@(ImageOriginal csum typ) =
  cacheLook key >>=
  maybe ((key,) <$> buildImageBytesFromFile source key csum typ)
        (\img -> (key,) <$> either (rebuildImageBytes source key typ)
                                   (lookImageBytes . ImageCached key) img)
  where _ = callStack
buildImageBytes source key@(ImageUpright key') = do
  (key'', bs) <- buildImageBytes source key'
  uprightImage' bs >>= return . maybe (key'', bs) (key,)
buildImageBytes source key@(ImageScaled sz dpi key') = do
  (key'', bs) <- buildImageBytes source key'
  -- the buildImageBytes that just ran might have this info
  shape <- imageShapeM bs
  case either (const Nothing) (scaleFromDPI sz dpi) (imageRect shape) of
    Nothing -> return (key'', bs)
    Just sc ->
      maybe (key'', bs) (key,) <$> scaleImage' (fromRat sc) bs (imageType shape)
buildImageBytes source key@(ImageCropped crop key') = do
  (key'', bs) <- buildImageBytes source key'
  shape <- imageShapeM bs
  maybe (key'', bs) (key,) <$> editImage' crop bs (imageType shape) (imageShape shape)

-- | Look up the image FilePath and read the ByteString it contains.
lookImageBytes ::
  forall r e m a. (MonadFileCache r e m, HasFilePath a, HasCallStack)
  => a -> m BS.ByteString
lookImageBytes a = fileCachePath a >>= liftIO . BS.readFile
  where _ = callStack

-- | There is an error stored in the cache, maybe it can be repaired
-- now?  Be careful not to get into a loop doing this.
rebuildImageBytes ::
  forall e r m. (MonadFileCache r e m, HasCallStack)
  => Maybe FileSource -> ImageKey -> FileType -> FileError -> m BS.ByteString
rebuildImageBytes source key _typ e | retry e = do
  alog ALERT ("Retrying build of " ++ show key ++ " (e=" ++ show e ++ ")")
  path <- fileCachePath (ImagePath key)
  -- This and other operations like it may throw an
  -- IOException - I need LyftIO to make sure this is caught.
  bs <- liftIO (BS.readFile path)
  _cached <- cacheOriginalFile source bs
  return bs
    where
      retry (MissingOriginalEntry _) = True -- transient I think
      retry CacheDamageMigrated = True -- obsolete error type is obsolete
      retry (MissingDerivedEntry _) = False -- troubling if this happens
      retry _ = False
rebuildImageBytes _ key _typ e = do
  alog INFO ("Not retrying build of " ++ show key ++ " (e=" ++ show e ++ ")")
  throwMember e

-- There's a chance the file is just sitting on disk even though
-- it is not in the database - see if we can read it and verify
-- its checksum.
buildImageBytesFromFile ::
  forall r e m. (MonadFileCache r e m, HasCallStack)
  => Maybe FileSource -> ImageKey -> Text -> FileType -> m BS.ByteString
buildImageBytesFromFile source key csum _typ = do
  -- If we get a cache miss for an ImageOriginal key something
  -- has gone wrong.  Try to rebuild from the file if it exists.
  path <- fileCachePath (ImagePath key)
  exists <- liftIO (doesFileExist path)
  case exists of
    False -> do
      let e = MissingOriginalFile key path
      cachePut_ key (Left e :: Either FileError ImageFile)
      throwMember e
    True -> do
      bs <- makeByteString path
      let csum' = T.pack $ show $ md5 bs
      case csum' == csum of
        False -> do
          let e = DamagedOriginalFile key path
          alog ERROR ("Checksum mismatch - " <> show e)
          cachePut_ key (Left e :: Either FileError ImageFile)
          throwMember e
        True -> do
          alog ALERT ("recaching " ++ show key)
          _cached <- cacheOriginalFile source bs
          return bs

#if 0
-- | See if images are already in the cache
cacheLookImages ::
  (MonadFileCache r e m, HasCallStack)
  => Set ImageKey -> m (Map ImageKey (Maybe (Either FileError ImageFile)))
cacheLookImages keys = sequence $ fromSet cacheLook keys

foregroundBuilds ::
  (MonadFileCache r e m, HasCallStack)
  => [(ImageKey, Either FileError ImageFile)]
  -> m [(Either FileError ImageFile)]
foregroundBuilds pairs =
  mapM (uncurry doImage) pairs
  where
    doImage key (Right (ImageFileShape shape)) =
      cacheImageFile key shape
    doImage _ x = return x
#endif
