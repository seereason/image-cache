-- | Create original images

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators, UndecidableInstances #-}

module Data.FileCache.Upload
  ( -- * Create original and derived images
    cacheOriginalFile
  , cacheOriginalFile'
  , cacheOriginalFiles
  ) where

import Control.Monad ( unless )
import Control.Monad.RWS ( liftIO, MonadState, modify )
import qualified Data.ByteString.Lazy as BS (ByteString, empty)
import Data.Digest.Pure.MD5 ( md5 )
import Data.FileCache.CacheMap ( ImageCached(ImageCached) )
import Data.FileCache.File
import Data.FileCache.FileCache ( fileCachePath, fileCachePathIO, cachePut_ )
import Data.FileCache.FileCacheTop ( MonadFileCache )
import Data.FileCache.FileError
import Data.FileCache.FileInfo ({-instances-} fileInfoFromPath)
import Data.FileCache.ImageFile
import Data.FileCache.ImageIO ( MakeByteString(..) )
import Data.FileCache.ImageKey (HasImageShapeM, ImageKey(ImageOriginal), ImageShape(..), imageShapeM, FileType, imageType, originalKey)
import Data.ListLike ( StringLike(show) )
import Data.Map.Strict as Map ( Map, insert )
import Data.Maybe ( fromMaybe )
import Data.Text as T ( pack )
import GHC.Stack ( HasCallStack )
import Prelude hiding (show)
import SeeReason.LogServer(alog)
import System.Directory ( doesFileExist )
import System.FilePath.Extra ( writeFileReadable )
import System.Log.Logger ( Priority(..) )
import SeeReason.Errors (throwMember, tryMember)

instance (MonadFileCache r e m) => HasImageShapeM m (Checksum, FileType) where
  imageShapeM (csum, typ) = fileCachePath (csum, typ) >>= fileInfoFromPath (Just typ) . (, BS.empty)

instance (MonadFileCache r e m) => HasImageShapeM m (Maybe FileSource, BS.ByteString) where
  imageShapeM (Just (TheUpload (path, typ)), bytes) = fileInfoFromPath (Just (imageType typ)) (path, bytes)
  imageShapeM (Just (ThePath path), bytes) = fileInfoFromPath Nothing (path, bytes)
  imageShapeM (Just (TheURI uri), bytes) = fileInfoFromPath Nothing (uri, bytes)
  imageShapeM (_, bytes) = fileInfoFromPath Nothing ("-", bytes)

-- | Add some image files to an image repository - updates the acid
-- state image map and copies the file to a location determined by the
-- FileCacheTop and its checksum.
cacheOriginalFiles ::
  forall x e r m.
  (MakeByteString x, Ord x,
   MonadFileCache r e m,
   MonadState (Map x (Either FileError (ImageKey, ImageFile))) m,
   HasCallStack)
  => [(FileSource, x)] -> m ()
cacheOriginalFiles pairs =
  mapM_ doPair pairs
  where
    doPair :: (FileSource, x) -> m ()
    doPair (source, x) = tryMember @FileError (cacheOriginalFile (Just source) x) >>= modify . Map.insert x

-- | 'cacheOriginalFile' with the 'FileError' captured.
cacheOriginalFile' ::
  forall x e r m.
  (MakeByteString x, MonadFileCache r e m, HasCallStack)
  => Maybe FileSource
  -> x
  -> m (Either FileError (ImageKey, ImageFile))
cacheOriginalFile' source x =
  tryMember @FileError (cacheOriginalFile source x)

-- | Build an original (not derived) ImageFile from a URI or a
-- ByteString, insert it into the cache, and return it.
cacheOriginalFile ::
  forall x e r m.
  (MakeByteString x, MonadFileCache r e m, HasCallStack)
  => Maybe FileSource
  -> x
  -> m (ImageKey, ImageFile)
cacheOriginalFile source x = do
  img <- buildOriginalImage source x
  let key = originalKey img
      val = ImageFileReady img
  liftIO (alog DEBUG ("cachePut " ++ show key))
  cachePut_ key (Right val)
  return (key, val)

buildOriginalImage ::
  forall x r e m.
  (MakeByteString x, MonadFileCache r e m, HasCallStack)
  => Maybe FileSource
  -> x
  -> m ImageReady
buildOriginalImage source x = do
  bs <- makeByteString x
  let csum = T.pack $ show $ md5 bs
  imageShapeM (source, bs) >>= \case
#if 0
    -- Even if the rect is an error message the image is returned,
    -- the client can display the message
    ImageShape{_imageShapeRect = Left msg} -> do
      alog DEBUG ("msg=" <> show msg)
      throwMember (FromString msg)
#endif
    shape@ImageShape{..} -> do
      -- FIXME: The image-replace command in appraisalscope will pass
      -- Nothing to the source parameter.  Could the correct source
      -- possibly be found in by looking in the image database?
      let file = File { _fileSource = fromMaybe Missing source
                      , _fileChksum = csum
                      , _fileMessages = []
                      , _fileExt = fileExtension (imageType shape) }
      let img = ImageReady { _imageFile = file, _imageShape = shape }
      alog DEBUG ("img=" <> show img)
      path <- fileCachePathIO (ImageCached
                                (ImageOriginal csum _imageShapeType)
                                (ImageFileReady img))
      alog DEBUG ("path=" <> show path)
      exists <- liftIO $ doesFileExist path
      unless exists $ liftIO $ writeFileReadable path bs
      return img
