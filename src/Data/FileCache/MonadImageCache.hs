{-# LANGUAGE OverloadedStrings, TemplateHaskell, TupleSections, UndecidableInstances #-}
{-# OPTIONS -Wall -ddump-minimal-imports #-}

module Data.FileCache.MonadImageCache
  ( ImageCacheT
  , MonadImageCache
#if 0
  , runImageCacheT
  , evalImageCacheT
  , execImageCacheT
  , writeImageCacheT
#endif
  , fromImageCacheT
  , fromImageCacheT'
  , imageFilePath
  , cacheImageOriginal
  , cacheImagesByKey
  ) where

import Control.Exception ( IOException, throw )
import Control.Lens ( _1, _Left, over, view )
import Control.Monad.Catch ( try )
import Control.Monad.Except ( ExceptT, MonadError(throwError) )
import Control.Monad.Except ( catchError )
import Control.Monad.Except ( runExceptT )
import Control.Monad.RWS ( RWST )
import Control.Monad.RWS ()
import Control.Monad.Trans ( liftIO )
import Data.Acid ( AcidState )
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Lazy as P ( fromStrict, toStrict )
import qualified Data.ByteString.UTF8 as P ( ByteString )
import Data.FileCache.Acid ( Cached(_unCached) )
import Data.FileCache.Cache ( CacheMap, CacheValue(..), FileCacheTop(..), fileCacheTop, HasFileCacheTop )
import Data.FileCache.Cache ( CacheValue(Value, Failed) )
import Data.FileCache.Exif ( normalizeOrientationCode )
import Data.FileCache.File ( File )
import Data.FileCache.FileError ( FileError(CacheDamage), HasFileError, fromFileError )
import Data.FileCache.FileError ( withFileError )
import Data.FileCache.FileError ()
import Data.FileCache.FileIO ( fileCachePath, fileFromBytes, fileFromPath, fileFromURI, fileFromCmd, loadBytesSafe )
import Data.FileCache.Image ( CacheImage, ImageCrop(rotation, bottomCrop, topCrop, rightCrop, leftCrop), ImageType(..), originalKey )
import Data.FileCache.Image ( ImageFile, ImageKey(..), scaleFromDPI )
import Data.FileCache.Image ( PixmapShape(pixmapHeight, pixmapWidth), approx, fileExtension )
import Data.FileCache.ImageFile ( getFileType )
import Data.FileCache.ImageIO ( imageFileFromType )
import Data.FileCache.LogException ( logException )
import Data.FileCache.MonadFileCache ( cacheInsert, cacheLook, cachePut, evalFileCacheT, FileCacheT, W, MonadFileCache(..) )
import Data.Generics.Product ( field )
import Data.List ( intercalate )
import Data.Map ( fromList, Map )
import Data.Maybe ( fromMaybe )
import Data.Text ( pack )
import Extra.Except ( liftIOError, logIOError, MonadIOError )
import Extra.Log (alog, Priority(DEBUG, ERROR))
import Network.URI ( URI, uriToString )
import Numeric ( fromRat )
import Numeric ( showFFloat )
import System.Exit ( ExitCode(..) )
import System.Process ( proc, showCommandForUser, CreateProcess )
import System.Process.ListLike ( readCreateProcessWithExitCode, showCreateProcessForUser )

type ImageCacheT s m = FileCacheT ImageKey ImageFile s m
type MonadImageCache m = MonadFileCache ImageKey ImageFile m

#if 0
runImageCacheT ::
  (HasFileError e, MonadError e m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W s m a
  -> m (a, s, W)
runImageCacheT = runFileCacheT

evalImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m a
evalImageCacheT = evalFileCacheT
execImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m s
execImageCacheT = execFileCacheT
writeImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W s m a -> m W
writeImageCacheT = writeFileCacheT

runImageCacheIOT ::
  (HasFileError e, MonadIOError e m)
  => acid
  -> FileCacheTop
  -> RWST (acid, FileCacheTop) W S m a
  -> m (a, S, W)
runImageCacheIOT = runFileCacheIOT

evalImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m a
evalImageCacheT = evalFileCacheT
execImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m S
execImageCacheT = execFileCacheT
writeImageCacheT :: Monad m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m W
writeImageCacheT = writeFileCacheT
#endif

-- | Build and return the 'ImageFile' described by the 'ImageKey'.
buildImageFile ::
  forall e m. (MonadImageCache m, MonadIOError e m, HasFileError e)
  => ImageKey
  -> m (CacheValue ImageFile)
buildImageFile key@(ImageOriginal _) = do
  -- This should already be in the cache
  (r :: Maybe (Cached (CacheValue ImageFile))) <- cacheLook key
  case r of
    -- Should we write this into the cache?  Probably not, if we leave
    -- it as it is the software could later corrected.
    Nothing -> return (Failed (CacheDamage ("Missing original: " <> pack (show key))))
    Just c -> return (_unCached c)
  -- maybe (throwError (fromFileError (CacheDamage ("Missing original: " <> pack (show key))) :: e)) (return . _unCached) r
buildImageFile (ImageUpright key) = do
  -- mapError (\m -> either (Left . fromFileError) Right <$> m) $
  buildImageFile key >>= overCached uprightImage
buildImageFile (ImageScaled sz dpi key) = do
  buildImageFile key >>= overCached (\img ->
                                        let scale = scaleFromDPI dpi sz img in
                                        logIOError $ scaleImage (fromRat (fromMaybe 1 scale)) img)
buildImageFile (ImageCropped crop key) = do
  buildImageFile key >>= overCached (editImage crop)

overCached :: Monad m => (a -> m (CacheValue a)) -> CacheValue a -> m (CacheValue a)
overCached f (Value a) = f a
overCached _ v = pure v

-- | 'MonadFileCache' instance for images on top of the 'RWST' monad run by
-- 'runFileCacheT'
instance (MonadError e m, acid ~ AcidState (CacheMap ImageKey ImageFile), top ~ FileCacheTop)
  => MonadFileCache ImageKey ImageFile (RWST (acid, top) W s m) where
    askCacheAcid = view _1 :: RWST (acid, top) W s m (AcidState (CacheMap ImageKey ImageFile))
    buildCacheValue = buildImageFile

-- mapError :: (MonadError e m, MonadError e' n) => (m (Either e a) -> n (Either e' b)) -> m a -> n b
-- evalFileCacheT :: Functor m => acid -> FileCacheTop -> RWST (acid, FileCacheTop) W S m a -> m a
-- runExceptT :: ExceptT e m a -> m (Either e a)

-- Crazy function to turn FileError into e.
fromImageCacheT' ::
  forall e m a n. (HasFileError e, MonadIOError e m, MonadImageCache m, n ~ ImageCacheT () IO)
  => n (Either FileError a)
  -> m (Either e a)
fromImageCacheT' action1 = do
  acid <- askCacheAcid @ImageKey @ImageFile
  top <- fileCacheTop
  flattenExcept1 (evalFileCacheT acid top () action1)
  where
    flattenExcept1 :: ExceptT FileError IO (Either FileError a) -> m (Either e a)
    flattenExcept1 action = liftFileError (flattenEither <$> (runExceptT action))

    liftFileError :: IO (Either FileError a) -> m (Either e a)
    liftFileError action = over _Left fromFileError <$> liftIOError action

    flattenEither = either Left id

-- Crazy function to turn FileError into e.
fromImageCacheT ::
  forall e m a n. (HasFileError e, MonadIOError e m, MonadImageCache m, n ~ ImageCacheT () IO)
  => n a
  -> m a
fromImageCacheT action = do
  acid <- askCacheAcid @ImageKey @ImageFile
  top <- fileCacheTop
  flattenExcept (evalFileCacheT acid top () action)
  where
    flattenExcept :: ExceptT FileError IO a -> m a
    flattenExcept = liftFileError . runExceptT

    liftFileError :: IO (Either FileError a) -> m a
    liftFileError action' = liftIOError action' >>= either (throwError . fromFileError) return

-- This creates the file in the image cache but doesn't add it to the
-- database.  Why?  I'm not entirely sure.
class MakeImageFile a where
  makeImageFile :: (MonadIOError e m, HasFileError e, HasFileCacheTop m) => a -> m CacheImage

instance MakeImageFile ByteString where
  makeImageFile bs =
    fileFromBytes (liftIOError . getFileType) fileExtension bs >>= makeImageFile
instance MakeImageFile URI where
  makeImageFile uri =
    fileFromURI (liftIOError . getFileType) fileExtension (uriToString id uri "") >>= makeImageFile
instance MakeImageFile FilePath where
  makeImageFile path =
    fileFromPath (liftIOError . getFileType) fileExtension path >>= makeImageFile
-- | Create an image file from a 'File'.  The existance of a 'File'
-- value implies that the image has been found in or added to the
-- acid-state cache.  Note that 'InProgress' is not a possible result
-- here, it will only occur for derived (scaled, cropped, etc.)
-- images.
instance MakeImageFile (File, ImageType) where
  makeImageFile (file, ityp) = do
    path <- fileCachePath file
    liftIOError $ liftIO $
      (either Failed Value <$> try ($logException ERROR (liftIO $ imageFileFromType path file ityp)))

-- | Return the local pathname of an image file.  The path will have a
-- suitable extension (e.g. .jpg) for the benefit of software that
-- depends on this, so the result might point to a symbolic link.
imageFilePath :: HasFileCacheTop m => ImageFile -> m FilePath
imageFilePath img = fileCachePath (view (field @"_imageFile") img)

-- | Find or create a version of some image with its orientation
-- corrected based on the EXIF orientation flag.  If the image is
-- already upright this will return the original ImageFile.
uprightImage ::
    (MonadIOError e m, HasFileError e, HasFileCacheTop m)
    => ImageFile
    -> m CacheImage
uprightImage orig = do
  bs <- loadBytesSafe (view (field @"_imageFile") orig)
  bs' <- liftIOError $ $logException ERROR $ normalizeOrientationCode (P.fromStrict bs)
  either
    (\_ -> return (Value orig))
    (\bs'' -> fileFromBytes (liftIOError . $logException ERROR . getFileType) fileExtension (P.toStrict bs'') >>= makeImageFile)
    bs'

-- | Find or create a cached image resized by decoding, applying
-- pnmscale, and then re-encoding.  The new image inherits attributes
-- of the old other than size.
scaleImage ::
  forall e m. (MonadIOError e m, HasFileError e, HasFileCacheTop m)
  => Double -> ImageFile -> m CacheImage
scaleImage scale orig | approx (toRational scale) == 1 = return (Value orig)
scaleImage scale orig = {- liftIOError $ $logException ERROR $ -} do
    path <- fileCachePath (view (field @"_imageFile") orig)
    let decoder = case view (field @"_imageFileType") orig of
                    JPEG -> showCommandForUser "jpegtopnm" [path]
                    PPM -> showCommandForUser "cat" [path]
                    GIF -> showCommandForUser "giftopnm" [path]
                    PNG -> showCommandForUser "pngtopnm" [path]
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
        -- To save space, build a jpeg here rather than the original file type.
        encoder = case view (field @"_imageFileType") orig of
                    JPEG -> showCommandForUser "cjpeg" []
                    PPM -> showCommandForUser {-"cat"-} "cjpeg" []
                    GIF -> showCommandForUser {-"ppmtogif"-} "cjpeg" []
                    PNG -> showCommandForUser {-"pnmtopng"-} "cjpeg" []
        cmd = pipe' [decoder, scaler, encoder]
    fileFromCmd (liftIOError . getFileType) fileExtension cmd >>= makeImageFile

pipeline :: [CreateProcess] -> P.ByteString -> IO P.ByteString
pipeline [] bytes = return bytes
pipeline (p : ps) bytes =
    (readCreateProcessWithExitCode p bytes >>= doResult)
      `catchError` (\ (e :: IOException) -> doException (showCreateProcessForUser p ++ " -> " ++ show e) e)
    where
      doResult (ExitSuccess, out, _) = pipeline ps out
      doResult (code, _, err) = let message = (showCreateProcessForUser p ++ " -> " ++ show code ++ " (" ++ show err ++ ")") in doException message (userError message)
      -- Is there any exception we should ignore here?
      doException message e = alog "Appraisal.ImageFile" ERROR message >> throw e

pipe' :: [String] -> String
pipe' = intercalate " | "

-- | Find or create a cached image which is a cropped version of
-- another.
editImage ::
    forall e m. (MonadIOError e m, HasFileError e, HasFileCacheTop m)
    => ImageCrop -> ImageFile -> m CacheImage
editImage crop file =
  logIOError $
    case commands of
      [] ->
          return (Value file)
      _ ->
          (loadBytesSafe (view (field @"_imageFile") file) >>=
           liftIOError . pipeline commands >>=
           fileFromBytes (liftIOError . getFileType) fileExtension >>=
           makeImageFile) `catchError` err
    where
      commands = buildPipeline (view (field @"_imageFileType") file) [cut, rotate] (latexImageFileType (view (field @"_imageFileType") file))
      -- We can only embed JPEG and PNG images in a LaTeX
      -- includegraphics command, so here we choose which one to use.
      latexImageFileType GIF = JPEG
      latexImageFileType PPM = JPEG
      latexImageFileType JPEG = JPEG
      latexImageFileType PNG = JPEG
      cut = case (leftCrop crop, rightCrop crop, topCrop crop, bottomCrop crop) of
              (0, 0, 0, 0) -> Nothing
              (l, r, t, b) -> Just (PPM, proc "pnmcut" ["-left", show l,
                                                        "-right", show (w - r - 1),
                                                        "-top", show t,
                                                        "-bottom", show (h - b - 1)], PPM)
      rotate = case rotation crop of
                 90 -> Just (JPEG, proc "jpegtran" ["-rotate", "90"], JPEG)
                 180 -> Just (JPEG, proc "jpegtran" ["-rotate", "180"], JPEG)
                 270 -> Just (JPEG, proc "jpegtran" ["-rotate", "270"], JPEG)
                 _ -> Nothing
      w = pixmapWidth file
      h = pixmapHeight file
      buildPipeline :: ImageType -> [Maybe (ImageType, CreateProcess, ImageType)] -> ImageType -> [CreateProcess]
      buildPipeline start [] end = convert start end
      buildPipeline start (Nothing : ops) end = buildPipeline start ops end
      buildPipeline start (Just (a, cmd, b) : ops) end | start == a = cmd : buildPipeline b ops end
      buildPipeline start (Just (a, cmd, b) : ops) end = convert start a ++ buildPipeline a (Just (a, cmd, b) : ops) end
      convert JPEG PPM = [proc "jpegtopnm" []]
      convert GIF PPM = [proc "giftpnm" []]
      convert PNG PPM = [proc "pngtopnm" []]
      convert PPM JPEG = [proc "cjpeg" []]
      convert PPM GIF = [proc "ppmtogif" []]
      convert PPM PNG = [proc "pnmtopng" []]
      convert PNG x = proc "pngtopnm" [] : convert PPM x
      convert GIF x = proc "giftopnm" [] : convert PPM x
      convert a b | a == b = []
      convert a b = error $ "Unknown conversion: " ++ show a ++ " -> " ++ show b
      err :: e -> m CacheImage
      err e = withFileError err' e
        where err' :: Maybe FileError -> m CacheImage
              err' (Just e') = return $ Failed $ e'
              -- err' (Just e') = return $ Failed $ ErrorCall $ "editImage Failure: file=" <> pack (show file) <> ", error=" <> pack (show e)
              err' Nothing = throwError e

-- | Build an original (not derived) ImageFile from a URI or a
-- ByteString, insert it into the cache, and return it.
cacheImageOriginal ::
    forall f e m. (MakeImageFile f, MonadIOError e m, HasFileError e, MonadImageCache m)
    => f
    -> m (ImageKey, Cached CacheImage)
cacheImageOriginal src = do
  (img' :: CacheImage) <- makeImageFile src
  case img' of
    InProgress -> error "The InProgress status must not occur for a non-derived image"
    -- We build the key from the original ImageFile, if that
    -- fails there's no point inserting anything.  Also we
    -- don't have a return value, so throw an error.
    Failed e -> throwError (fromFileError e)
    Value img -> do
      let key = originalKey img
      (key,) <$> cachePut key img

-- | Scan for ReportImage objects and ensure that one version of that
-- image has been added to the cache, adding it if necessary.
cacheImagesByKey ::
  forall a e m. (Ord a, MonadImageCache m, MonadIOError e m, HasFileError e)
  => (a -> ImageKey)
  -> [a]
  -> m (Map a (Cached CacheImage))
cacheImagesByKey keyfn keys =
  fromList <$> mapM (\img -> let key = keyfn img in cacheInsert key >>= \val -> return (img, val)) keys
