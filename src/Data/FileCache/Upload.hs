-- | Create original images

{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators, UndecidableInstances #-}

module Data.FileCache.Upload
  ( -- * Create original and derived images
    cacheOriginalImage
  , cacheOriginalImage'
  , cacheOriginalImages
  ) where

import Control.Exception ( IOException )
import Control.Monad ( unless )
import Control.Monad.RWS ( MonadState, modify )
import Control.Monad.Reader ( MonadReader )
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Trans.Except ( runExceptT )
import qualified Data.ByteString.Lazy as BS ( empty )
import Data.Digest.Pure.MD5 ( md5 )
import Data.FileCache.FileCacheTop ( HasCacheAcid, HasFileCacheTop )
import Data.FileCache.CacheMap ( ImageCached(ImageCached) )
import Data.FileCache.ImageIO ( MakeByteString(..) )
import Data.FileCache.FileCache ( fileCachePath, fileCachePathIO, cachePut_ )
import Data.FileCache.FileInfo ({-instances-} fileInfoFromPath)
import Data.FileCache.Common
import Data.FileCache.ImageShape (imageShapeM)
import Data.ListLike ( StringLike(show) )
import Data.Map.Strict as Map ( Map, insert )
import Data.Maybe ( fromMaybe )
import Data.Text as T ( pack )
import Extra.Except ( MonadError(throwError), ExceptT )
import GHC.Stack ( HasCallStack )
import Prelude hiding (show)
import SeeReason.LogServer(alog)
import System.Directory ( doesFileExist )
import System.FilePath.Extra ( writeFileReadable )
import System.Log.Logger ( Priority(..) )
import SeeReason.Errors (runOneOf, Member, OneOf)
import SeeReason.UIO (liftUIO, NonIOException, Unexceptional, unsafeFromIO )

instance (Unexceptional m, MonadError (OneOf e) m, Member FileError e, Member IOException e, Member NonIOException e,
          MonadReader r m, HasFileCacheTop r) => HasImageShapeM m (Checksum, ImageType) where
  imageShapeM (csum, typ) = fileCachePath (csum, typ) >>= fileInfoFromPath . (, BS.empty)

-- | Add some image files to an image repository - updates the acid
-- state image map and copies the file to a location determined by the
-- FileCacheTop and its checksum.
cacheOriginalImages ::
  forall x e r m. (MakeByteString x, Ord x,
                   Unexceptional m, MonadError (OneOf e) m,
                   Member NonIOException e, Member IOException e,
                   MonadReader r m, HasCacheAcid r, HasFileCacheTop r,
                   MonadState (Map x (Either FileError (ImageKey, ImageFile))) m)
  => [(FileSource, x)] -> m ()
cacheOriginalImages pairs =
  runExceptT (mapM_ doPair pairs) >>= either throwError return
  where
    doPair :: (FileSource, x) -> ExceptT (OneOf e) m ()
    doPair (source, x) = cacheOriginalImage' (Just source) x >>= lift . modify . Map.insert x

cacheOriginalImage' ::
  forall x e r m.
  (MakeByteString x, Unexceptional m, MonadError (OneOf e) m,
   Member NonIOException e, Member IOException e,
   MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => Maybe FileSource
  -> x
  -> m (Either FileError (ImageKey, ImageFile))
cacheOriginalImage' source x =
  runOneOf @(FileError ': e) (cacheOriginalImage source x)

-- | Build an original (not derived) ImageFile from a URI or a
-- ByteString, insert it into the cache, and return it.
cacheOriginalImage ::
  forall x e r m.
  (MakeByteString x, Unexceptional m, {-HasImageShapeM m ByteString,-}
   MonadError (OneOf e) m, Member FileError e, Member NonIOException e, Member IOException e,
   MonadReader r m, HasFileCacheTop r, HasCacheAcid r, HasCallStack)
  => Maybe FileSource
  -> x
  -> m (ImageKey, ImageFile)
cacheOriginalImage source x = do
  img <- buildOriginalImage source x
  let key = originalKey img
      val = ImageFileReady img
  unsafeFromIO (alog DEBUG ("cachePut " ++ show key))
  cachePut_ key (Right val)
  return (key, val)

buildOriginalImage ::
  forall x r e m.
  (MakeByteString x, Unexceptional m, {-HasImageShapeM m ByteString,-}
   MonadError (OneOf e) m, Member FileError e, Member NonIOException e, Member IOException e,
   MonadReader r m, HasFileCacheTop r)
  => Maybe FileSource
  -> x
  -> m ImageReady
buildOriginalImage source x = do
  bs <- makeByteString x
  let csum = T.pack $ show $ md5 bs
  shape@ImageShape {..} <- imageShapeM bs
  -- FIXME: The image-replace command in appraisalscope will pass
  -- Nothing to the source parameter.  Could the correct source
  -- possibly be found in by looking in the image database?
  let file = File { _fileSource = fromMaybe Missing source
                  , _fileChksum = csum
                  , _fileMessages = []
                  , _fileExt = fileExtension (imageType shape) }
  let img = ImageReady { _imageFile = file, _imageShape = shape }
  path <- fileCachePathIO (ImageCached (ImageOriginal csum _imageShapeType) (ImageFileReady img))
  exists <- liftUIO $ doesFileExist path
  unless exists $ liftUIO $ writeFileReadable path bs
  return img
