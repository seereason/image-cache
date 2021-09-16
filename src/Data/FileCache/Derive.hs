-- | Create derived images

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Derive
  ( CacheFlag(RetryErrors)
  , getImageFile
  , getImageFiles
  , getImageShapes
  , cacheImageFile
  , cacheImageShape
{-
  , cacheDerivedImagesForeground
  , buildImageShape
-}
  , cacheLookImages
  ) where

import Control.Exception ( IOException )
import Control.Lens ( _Right, over )
import Control.Monad.Reader ( MonadReader )
import qualified Data.ByteString.Lazy as BS ( ByteString, readFile )
import Data.Digest.Pure.MD5 ( md5 )
-- import Data.FileCache.Background
import Data.FileCache.CacheMap ( ImageCached(ImageCached) )
import Data.FileCache.FileCacheTop ( HasCacheAcid, HasFileCacheTop )
import Data.FileCache.ImageIO
import Data.FileCache.FileCache
import Data.FileCache.Common
import Data.FileCache.Upload ( cacheOriginalImage )
import Data.ListLike ( length, show )
import Data.Map.Strict as Map ( Map, fromList, toList )
import Data.Monoid ( (<>) )
import Data.Set as Set ( member, Set )
import Data.Text as T ( Text, pack )
import Data.Typeable ( Typeable, typeOf )
import SeeReason.Errors( liftUIO, Member, NonIOException, OneOf, throwMember, tryMember )
import Extra.Except ( MonadError )
import GHC.Stack ( HasCallStack )
import Numeric ( fromRat )
import Prelude hiding (show, length)
import SeeReason.LogServer (alog)
import System.Directory ( doesFileExist )
import System.FilePath.Extra ( writeFileReadable )
import System.Log.Logger ( Priority(..) )
import System.Posix.Files ( createLink )
import Text.PrettyPrint.HughesPJClass ( prettyShow )
import UnexceptionalIO.Trans ( Unexceptional )
import UnexceptionalIO.Trans as UIO ( unsafeFromIO )

data CacheFlag
  = RetryErrors -- ^ If the cache contains a FileError try the operation again
  | RetryShapes -- ^ Not used
  deriving (Eq, Ord, Show)

-- | This is just a wrapper around cacheDerivedImagesForeground.
getImageFile ::
  forall r e m. (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => Set CacheFlag
  -> ImageKey
  -> m (Either FileError ImageFile)
getImageFile flags key =
  cacheLook key >>= cacheImageShape flags key >>= either (pure . Left) (buildImage key)

getImageShapes ::
  forall r e m. (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => Set CacheFlag -> [ImageKey] -> m (Map ImageKey (Either FileError ImageFile))
getImageShapes flags keys =
  (Map.fromList . zip keys) <$> mapM (\key -> cacheLook key >>= (cacheImageShape flags key :: Maybe (Either FileError ImageFile) -> m (Either FileError ImageFile)) {-maybe (cacheImageShape flags key) pure-}) keys

getImageFiles ::
  forall r e m. (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => Set CacheFlag -> [ImageKey] -> m (Map ImageKey (Either FileError ImageFile))
getImageFiles flags keys =
  mapWithKeyM f =<< getImageShapes flags keys
  where
    f :: ImageKey -> Either FileError ImageFile -> m (ImageKey, Either FileError ImageFile)
    f key (Right (ImageFileShape shape)) = (key,) <$> cacheImageFile key shape
    f key (Right ready@(ImageFileReady _)) = pure $ (key, Right ready)
    f key (Left e) = pure (key, Left e)

mapWithKeyM :: forall k v m. (Ord k, Monad m) => (k -> v -> m (k, v)) -> Map k v -> m (Map k v)
mapWithKeyM f mp = fromList <$> mapM (uncurry f) (Map.toList mp :: [(k, v)])

-- | See if images are already in the cache
cacheLookImages ::
  (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r)
  => [ImageKey] -> m [Maybe (Either FileError ImageFile)]
cacheLookImages keys = mapM cacheLook keys

-- | Compute the shapes of requested images
cacheImageShape ::
  forall (e :: [*]) r m. (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, MonadReader r m, HasCallStack,
   HasCacheAcid r, HasFileCacheTop r)
  => Set CacheFlag
  -> ImageKey
  -> Maybe (Either FileError ImageFile)
  -> m (Either FileError ImageFile)
cacheImageShape _ key Nothing = do
  -- unsafeFromIO $ alog DEBUG ("cacheImageShape key=" ++ prettyShow key ++ " (miss)")
  cachePut_ key (noShape ("cacheImageShape " <> show key <> " :: " <> show (typeOf key)))
  buildAndCache
    where
      buildAndCache :: m (Either FileError ImageFile)
      buildAndCache =
        tryMember @FileError (buildImageShape key) >>= cachePut key . over _Right ImageFileShape
cacheImageShape flags key (Just (Left _))
  | Set.member RetryErrors flags = do
      unsafeFromIO $ alog INFO ("cacheImageShape key=" ++ prettyShow key ++ " (retry)")
      buildAndCache
        where
          buildAndCache :: m (Either FileError ImageFile)
          buildAndCache =
            tryMember @FileError (buildImageShape key) >>= cachePut key . over _Right ImageFileShape
cacheImageShape flag key (Just (Left e)) = do
  unsafeFromIO $ alog INFO ("cacheImageShape key=" ++ prettyShow key ++ " (e=" <> show e <> ")")
  cacheImageShape flag key Nothing
cacheImageShape _ _key (Just (Right (ImageFileShape shape))) = do
  -- unsafeFromIO $ alog INFO ("cacheImageShape key=" ++ prettyShow key ++ " (shape)")
  -- This value shouldn't be here in normal operation
  return (Right (ImageFileShape shape))
cacheImageShape _ _ (Just (Right (ImageFileReady img))) = do
  -- unsafeFromIO $ alog DEBUG ("cacheImageShape key=" ++ prettyShow key ++ " (hit)")
  return (Right (ImageFileReady img))

#if 0
foregroundBuilds ::
  (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => [(ImageKey, Either FileError ImageFile)]
  -> m [(Either FileError ImageFile)]
foregroundBuilds pairs =
  mapM (uncurry doImage) pairs
  where
    doImage key (Right (ImageFileShape shape)) =
      cacheImageFile key shape
    doImage _ x = return x
#endif

buildImage ::
  (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e,
   MonadReader r m, HasCacheAcid r, HasFileCacheTop r,
   Member NonIOException e, Member IOException e)
  => ImageKey
  -> ImageFile
  -> m (Either FileError ImageFile)
buildImage key (ImageFileShape shape) = cacheImageFile key shape
buildImage _ i@(ImageFileReady _) = pure (Right i)

noShape :: Text -> Either FileError ImageFile
noShape = Left . NoShape

cacheImageFile ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => ImageKey
  -> ImageShape
  -> m (Either FileError ImageFile)
cacheImageFile key shape = do
  -- Check the cache one last time - this image might appear more than once in this request
  cacheLook key >>=
    maybe (pure (Left (UnexpectedException "Impossible cache miss")))
          (either (pure . Left)
            (\case file@(ImageFileReady img) ->
                     pure (Right file) -- It was already built
                   file@(ImageFileShape shape') ->
                     -- Proceed with the build
                     tryMember @FileError (buildImageFile key shape') >>= cachePut key))

-- | These are meant to be inexpensive operations that determine the
-- shape of the desired image, with the actual work of building them
-- deferred and forked into the background.  Are they actually
-- inexpensive?  We read the original bytestring and call file(1) to
-- get the shape and orientation of that image, and the other
-- operations are pure.
buildImageShape ::
  forall (e :: [*]) r m.
  (Unexceptional m, MonadReader r m, HasCacheAcid r, HasFileCacheTop r,
   MonadError (OneOf e) m, Show (OneOf e), Typeable e,
   Member FileError e, Member NonIOException e, Member IOException e)
  => ImageKey -> m ImageShape
buildImageShape key@(ImageOriginal csum typ) =
  cacheLook key >>=
  maybe missingOriginal
        (either throwMember
                (\case ImageFileReady img -> return (_imageShape img)
                       ImageFileShape s -> return s))
  where
    missingOriginal = do
      unsafeFromIO $ alog ALERT ("Missing original: " <> show key)
      path <- fileCachePath (csum, typ)
      (_key, file) <- cacheOriginalImage (Just (ThePath path)) path
      unsafeFromIO $ alog ALERT ("Missing original found: " <> show (key, file))
      case file of
        ImageFileReady img -> return (_imageShape img)
        ImageFileShape s -> return s
buildImageShape (ImageUpright key) =
  -- unsafeFromIO (alog DEBUG ("buildImageShape (" <> show key' <> ")")) >>
  uprightImageShape <$> buildImageShape key
buildImageShape (ImageCropped crop key) =
  -- unsafeFromIO (alog DEBUG ("buildImageShape (" <> show key' <> ")")) >>
  cropImageShape crop <$> buildImageShape key
buildImageShape (ImageScaled sz dpi key) =
  -- unsafeFromIO (alog DEBUG ("buildImageShape (" <> show key' <> ")")) >>
  scaleImageShape sz dpi <$> buildImageShape key

uprightImageShape :: ImageShape -> ImageShape
uprightImageShape shape@(ImageShape {_imageFileOrientation = rot}) =
  case rot of
    ZeroHr -> shape
    SixHr -> shape
    ThreeHr -> shape
    NineHr -> shape

buildImageFile ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member FileError e, Member NonIOException e, Member IOException e,
                 MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => ImageKey -> ImageShape -> m ImageFile
buildImageFile key shape = do
  (key', bs) <- buildImageBytes Nothing key -- key' may differ from key due to removal of no-ops
  let file = File { _fileSource = Derived
                  , _fileChksum = T.pack $ show $ md5 bs
                  , _fileMessages = []
                  , _fileExt = fileExtension (_imageShapeType shape) }
  let img = ImageFileReady (ImageReady { _imageFile = file, _imageShape = shape })
  path <- fileCachePathIO (ImageCached key img) -- the rendered key
  exists <- liftUIO $ doesFileExist path
  path' <- fileCachePathIO (ImageCached key' img) -- the equivalent file
  -- path' should exist, hard link path to path'
  case exists of
    False -> do
      case key == key' of
        True -> do
          unsafeFromIO $ alog INFO ("Writing new cache file: " <> show path)
          liftUIO $ writeFileReadable path bs
        False -> do
          unsafeFromIO $ alog INFO ("Hard linking " <> show path' <> " -> " <> show path)
          liftUIO $ createLink path' path
    True -> do
      -- Don't mess with it if it exists, there is probably
      -- a process running that is writing it out.
      bs' <- liftUIO $ BS.readFile path
      case bs == bs' of
        False -> do
          unsafeFromIO $ alog WARNING ("Replacing damaged cache file: " <> show path <> " length " <> show (length bs') <> " -> " <> show (length bs))
          liftUIO $ writeFileReadable path bs
        True -> unsafeFromIO $ alog WARNING ("Cache file for new key already exists: " <> show path)
  unsafeFromIO $ alog DEBUG ("added to cache: " <> prettyShow img)
  return img

-- | Retrieve the 'ByteString' associated with an 'ImageKey'.
buildImageBytes ::
  forall r e m. (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, Member FileError e,
                 MonadReader r m, HasFileCacheTop r, HasCacheAcid r)
  => Maybe FileSource -> ImageKey -> m (ImageKey, BS.ByteString)
buildImageBytes source key@(ImageOriginal csum typ) =
  cacheLook key >>=
  maybe ((key,) <$> buildImageBytesFromFile source key csum typ)
        (\img -> (key,) <$> either (rebuildImageBytes source key typ)
                                   (lookImageBytes . ImageCached key) img)
buildImageBytes source key@(ImageUpright key') = do
  (key'', bs) <- buildImageBytes source key'
  uprightImage' bs >>= return . maybe (key'', bs) (key,)
buildImageBytes source key@(ImageScaled sz dpi key') = do
  (key'', bs) <- buildImageBytes source key'
  -- the buildImageBytes that just ran might have this info
  shape <- imageShapeM bs
  let scale' = scaleFromDPI sz dpi shape
  case scale' of
    Nothing -> return (key'', bs)
    Just sc ->
      maybe (key'', bs) (key,) <$> scaleImage' (fromRat sc) bs (imageType shape)
buildImageBytes source key@(ImageCropped crop key') = do
  (key'', bs) <- buildImageBytes source key'
  shape <- imageShapeM bs
  maybe (key'', bs) (key,) <$> editImage' crop bs (imageType shape) (imageShape shape)

-- There's a chance the file is just sitting on disk even though
-- it is not in the database - see if we can read it and verify
-- its checksum.
buildImageBytesFromFile ::
  (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m,
   MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => Maybe FileSource -> ImageKey -> Text -> ImageType -> m BS.ByteString
buildImageBytesFromFile source key csum _typ = do
  -- If we get a cache miss for an ImageOriginal key something
  -- has gone wrong.  Try to rebuild from the file if it exists.
  path <- fileCachePath (ImagePath key)
  exists <- liftUIO (doesFileExist path)
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
          unsafeFromIO (alog ERROR ("Checksum mismatch - " <> show e))
          cachePut_ key (Left e :: Either FileError ImageFile)
          throwMember e
        True -> do
          unsafeFromIO (alog ALERT ("recaching " ++ show key))
          _cached <- cacheOriginalImage source bs
          return bs

-- | Look up the image FilePath and read the ByteString it contains.
lookImageBytes ::
  (MonadReader r m, HasFileCacheTop r,
   Unexceptional m, Member NonIOException e, Member IOException e, MonadError (OneOf e) m,
   HasImageFilePath a)
  => a -> m BS.ByteString
lookImageBytes a = fileCachePath a >>= liftUIO . BS.readFile

-- | There is an error stored in the cache, maybe it can be repaired
-- now?  Be careful not to get into a loop doing this.
rebuildImageBytes ::
  forall e r m. (Unexceptional m, Member FileError e, Member NonIOException e, Member IOException e, MonadError (OneOf e) m,
                 MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack)
  => Maybe FileSource -> ImageKey -> ImageType -> FileError -> m BS.ByteString
rebuildImageBytes source key _typ e | retry e = do
  unsafeFromIO (alog ALERT ("Retrying build of " ++ show key ++ " (e=" ++ show e ++ ")"))
  path <- fileCachePath (ImagePath key)
  -- This and other operations like it may throw an
  -- IOException - I need LyftIO to make sure this is caught.
  bs <- liftUIO (BS.readFile path)
  _cached <- cacheOriginalImage source bs
  return bs
    where
      retry (MissingOriginalEntry _) = True -- transient I think
      retry CacheDamageMigrated = True -- obsolete error type is obsolete
      retry (MissingDerivedEntry _) = False -- troubling if this happens
      retry _ = False
rebuildImageBytes _ key _typ e = do
  unsafeFromIO (alog INFO ("Not retrying build of " ++ show key ++ " (e=" ++ show e ++ ")"))
  throwMember e
