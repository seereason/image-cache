-- | Create derived images

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Derive
  ( CacheFlag(RetryErrors)
  , E
  , imageSP
  , cacheMiss

  -- , getImageFiles
  , getImageShapes

  , cachedImage
  , cacheImageFile

  , cacheImageShape
  , getImageFile
  -- , cacheLookImages
  ) where

import Control.Exception ( IOException, SomeException, toException )
import Control.Lens ( _Right, over )
import Control.Monad.Catch (MonadCatch, try)
import Control.Monad.Reader (liftIO, MonadIO, MonadReader, MonadPlus, ReaderT, runReaderT)
import qualified Data.ByteString.Lazy as BS ( ByteString, readFile )
import Data.ByteString.UTF8 as UTF8 (fromString)
import Data.Digest.Pure.MD5 ( md5 )
-- import Data.FileCache.Background
import Data.FileCache.CacheMap ( ImageCached(ImageCached) )
import Data.FileCache.FileCacheTop ( HasCacheAcid, HasFileCacheTop )
import Data.FileCache.ImageIO
import Data.FileCache.FileCache
import Data.FileCache.Common
import Data.FileCache.Upload ( cacheOriginalImage )
import Data.ListLike (fromText, length)
import Data.Map.Strict as Map ( Map, fromList, lookup, toList )
import Data.Maybe (fromMaybe)
import Data.Monoid ( (<>) )
import Data.Set as Set ( member, Set )
import Data.Text as T ( Text, pack )
import Data.Typeable ( Typeable, typeOf )
import Extra.Except (ExceptT, MonadError, runExceptT )
import GHC.Stack ( HasCallStack )
import Happstack.Server
  (Response, mimeTypes, serveFile, internalServerError, notFound,
   dir, uriRest, FilterMonad, ServerMonad, ToMessage(toResponse))
import Numeric ( fromRat )
import Prelude hiding (length)
import SeeReason.LogServer (alog)
import System.Directory ( doesFileExist )
import System.FilePath ((</>))
import System.FilePath.Extra ( writeFileReadable )
import System.Log.Logger ( Priority(..) )
import System.Posix.Files ( createLink )
import Text.PrettyPrint.HughesPJClass ( prettyShow )
import SeeReason.Errors(Member, OneOf, throwMember, tryMember)
import SeeReason.UIO (liftUIO, NonIOException, run, Unexceptional, UIO, unsafeFromIO)
import Web.Routes (fromPathInfo)

data CacheFlag
  = RetryErrors -- ^ If the cache contains a FileError try the operation again
  | RetryShapes -- ^ Not used
  deriving (Eq, Ord, Show)

type E = '[FileError, IOException, NonIOException]

-- Orphan instance
instance ToMessage SomeException where
  toResponse e = toResponse $ "Exception in serveFile: " <> show e

notFound' :: (ToMessage a, Show a, MonadIO m, FilterMonad Response m) => a -> m Response
notFound' a = do
  alog DEBUG ("imageSP notFound - a=" <> show a)
  notFound (toResponse a)

#define MONAD(r,e,m) Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable e, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack

-- This should use the image-cache API.
imageSP ::
  forall r m.
  (MonadIO m,
   MonadPlus m,
   MonadCatch m,
   ServerMonad m,
   FilterMonad Response m,
   HasCacheAcid r,
   HasFileCacheTop r)
  => r
  -> FilePath
  -> FilePath
  -> m Response
imageSP r db p = do
  dir p $ uriRest $ \uripath ->
    case fromPathInfo (UTF8.fromString uripath) of
      Left e -> do
        notFound' ("fromPathInfo " <> show uripath <> " :: ImageKey failed:\n" <> e)
      Right key -> do
        alog DEBUG ("key=" <> prettyShow key)
        runCache (cacheLook key) r >>= \case
          Left (e :: OneOf E) -> do
            let msg = "imageSP - cache mechanism failed: " <> show e
            alog ERROR msg
            notFound' msg
          Right Nothing ->
            cacheMiss r db key
          Right (Just (Left (e :: FileError))) -> do
            alog WARNING ("cachedError: " <> show e)
            notFound' $ "imageSP cachedError e=" <> show e
          Right (Just (Right img)) ->
            cachedImage db key img

            -- runCache action = run (runExceptT (runReaderT action r))
{-
              -- alog DEBUG ("imageSP - key=" <> show key)
              -- Look up the metadata to see if we need to request
              -- a build.
              img <- run (runExceptT (runReaderT (cacheLook key :: ReaderT r (ExceptT SomeNonPseudoException UIO) (Maybe (Either FileError ImageFile))) r))
              case img of
                Right (Just (Right img')) -> do -- easy case
                  -- alog DEBUG ("imageSP - ImageKey=" <> show key)
                  -- alog DEBUG ("imageSP - FilePath=" <> show (toFilePath (ImageCached key img')))
                Right Nothing -> do -- hard case
                  -- alog DEBUG ("imageSP - miss")
                  mp <- run (runExceptT (runReaderT (toList <$> cacheDerivedImagesBackground mempty [key]) r))
                  case (mp :: Either SomeNonPseudoException [(ImageKey, Either FileError ImageFile)]) of
                    Right [(_, Left e')] -> do
                      -- alog DEBUG ("imageSP - e'=" <> show e')
                      notFound $ toResponse $ "imageSP e'=" <> show e'
                    Right [(_, Right img')] -> do
                      -- alog DEBUG ("imageSP - img'=" <> show img')
                      serveFile (guessContentTypeM mimeTypes) (db </> toFilePath (ImageCached key img'))
                Right (Just (Left e'')) -> do -- sad case
                  -- alog DEBUG ("imageSP - e''=" <> show e'')
                Left e''' -> do -- scary case
                  -- alog ALERT ("imageSP - cache failure key=" ++ show key)
                  notFound $ toResponse $ "imageSP e'''=" <> show e'''
-}

-- | The key is not in the image database.
cacheMiss ::
  forall r m.
  (MonadIO m,
   MonadPlus m,
   MonadCatch m,
   ServerMonad m,
   FilterMonad Response m,
   HasFileCacheTop r,
   HasCacheAcid r)
  => r
  -> FilePath
  -> ImageKey
  -> m Response
cacheMiss r db key = do
  alog DEBUG ("cache miss")
  -- out :: Either (OneOf E) [(ImageKey, Either FileError ImageFile)]
  runCache ((toList <$> getImageFiles mempty [key])) r >>= \case
    Right [(_, Left (e :: FileError))] -> do
      alog WARNING ("cachedError: " <> show e)
      notFound' $ "imageSP cachedError e=" <> show e
    Right [(_, Right img)] ->
      cachedImage db key img
    Right _ -> error "Pattern match failure"
    Left (e :: OneOf E) -> do
      let msg = "imageSP - cache mechanism failed: " <> show e
      alog ERROR msg
      notFound' msg

runCache :: MonadIO m => ReaderT r (ExceptT e UIO) a -> r -> m (Either e a)
runCache action r =
  run (runExceptT (runReaderT action r))

getImageFiles ::
  forall r e m. (MONAD(r,e,m))
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
getImageFiles flags keys =
  mapWithKeyM f =<< getImageShapes flags keys
  where
    f :: ImageKey -> Either FileError ImageFile -> m (ImageKey, Either FileError ImageFile)
    f key (Right (ImageFileShape shape)) = (key,) <$> cacheImageFile key shape
    f key (Right ready@(ImageFileReady _)) = pure $ (key, Right ready)
    f key (Left e) = pure (key, Left e)

mapWithKeyM :: forall k v m. (Ord k, Monad m) => (k -> v -> m (k, v)) -> Map k v -> m (Map k v)
mapWithKeyM f mp = fromList <$> mapM (uncurry f) (Map.toList mp :: [(k, v)])

getImageShapes ::
  forall r e m. (MONAD(r,e,m))
  => Set CacheFlag
  -> [ImageKey]
  -> m (Map ImageKey (Either FileError ImageFile))
getImageShapes flags keys =
  (Map.fromList . zip keys) <$> mapM (\key -> cacheLook key >>= (cacheImageShape flags key :: Maybe (Either FileError ImageFile) -> m (Either FileError ImageFile)) {-maybe (cacheImageShape flags key) pure-}) keys

-- | This is just a wrapper around cacheDerivedImagesForeground.
getImageFile ::
  forall r e m. (MONAD(r,e,m))
  => Set CacheFlag
  -> ImageKey
  -> m (Either FileError ImageFile)
getImageFile flags key =
  cacheLook key >>= cacheImageShape flags key >>= either (pure . Left) (buildImage key)

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
  cachePut_ key (noShape ("cacheImageShape " <> pack (show key <> " :: " <> show (typeOf key))))
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

-- | These are meant to be inexpensive operations that determine the
-- shape of the desired image, with the actual work of building them
-- deferred and forked into the background.  Are they actually
-- inexpensive?  We read the original bytestring and call file(1) to
-- get the shape and orientation of that image, and the other
-- operations are pure.
buildImageShape ::
  forall r e m. (MONAD(r,e,m), Member FileError e)
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

cachedImage ::
  forall m.
  (MonadIO m,
   MonadPlus m,
   MonadCatch m,
   ServerMonad m,
   FilterMonad Response m)
  => FilePath -> ImageKey -> ImageFile -> m Response
cachedImage db key img = do
  let path = db </> toFilePath (ImageCached key img)
      -- mimeFn :: forall m'. Monad m' => FilePath -> m' String
      mimeFn _ = imageMimeType img
  liftIO (doesFileExist path) >>= \case
    False -> do
      alog DEBUG ("cache file missing:" ++ show path)
      internalServerError $ toResponse (toException (userError "Missing cache file"))
    True -> do
      alog DEBUG ("cache hit path=" ++ show path)
      -- rq <- askRq
      try (serveFile mimeFn path) >>= \case
        Left (e :: SomeException) -> do
          alog ERROR ("serveFile e=" ++ show e)
          internalServerError $ toResponse e
        Right (response :: Response) -> pure response

-- | Compute the mime type of an image that doesn't yet exist
imageMimeType :: (HasFileExtension img, Monad m) => img -> m String
imageMimeType img =
  return $
  fromMaybe "application/octet-stream" $
  Map.lookup (dropWhile (== '.') (fromText (fileExtension img))) mimeTypes

cacheImageFile ::
  (MONAD(r,e,m))
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

buildImageFile ::
  forall r e m. (MONAD(r,e,m), Member FileError e)
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
  forall r e m. (MONAD(r,e,m), Member FileError e)
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

-- | Look up the image FilePath and read the ByteString it contains.
lookImageBytes ::
  forall r e m a. (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e, MonadReader r m, HasFileCacheTop r, HasImageFilePath a, HasCallStack)
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

-- There's a chance the file is just sitting on disk even though
-- it is not in the database - see if we can read it and verify
-- its checksum.
buildImageBytesFromFile ::
  forall r e m. (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasFileCacheTop r, HasCallStack, Member FileError e)
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

noShape :: Text -> Either FileError ImageFile
noShape = Left . NoShape

-- | See if images are already in the cache
cacheLookImages ::
  (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e, MonadReader r m, HasCacheAcid r, HasCallStack)
  => [ImageKey] -> m [Maybe (Either FileError ImageFile)]
cacheLookImages keys = mapM cacheLook keys


#if 0
foregroundBuilds ::
  (MONAD(r,e,m))
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
  (MONAD(r,e,m))
  => ImageKey
  -> ImageFile
  -> m (Either FileError ImageFile)
buildImage key (ImageFileShape shape) = cacheImageFile key shape
buildImage _ i@(ImageFileReady _) = pure (Right i)

uprightImageShape :: ImageShape -> ImageShape
uprightImageShape shape@(ImageShape {_imageFileOrientation = rot}) =
  case rot of
    ZeroHr -> shape
    SixHr -> shape
    ThreeHr -> shape
    NineHr -> shape
