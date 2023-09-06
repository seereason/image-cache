{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Test
  ( fixImageShapes
  , validateImageKey
  , validateImageFile
  , test1
  , tests
  ) where

import Control.Exception ( IOException )
import Control.Monad ( when )
import Control.Monad.Reader ( MonadReader )
import qualified Data.ByteString.Lazy as BS ( ByteString, empty, readFile )
import Data.Digest.Pure.MD5 ( md5 )
import Data.FileCache.FileCacheTop ( HasCacheAcid, HasFileCacheTop )
import Data.FileCache.FileInfo ()
import Data.FileCache.CacheMap ( ImageCached(ImageCached, _imageCachedFile, _imageCachedKey) )
import Data.FileCache.ImageIO ( validateJPG )
import Data.FileCache.FileCache ( HasImageFilePath, fileCachePath, cacheLook )
import Data.FileCache.Common
import Data.ListLike ( StringLike(show) )
import Data.Map.Strict as Map ( Map, lookup, toList, fromList )
import Data.Ratio ( (%) )
import Data.Text as T ( pack, Text, unpack )
import Data.Text.Encoding ( encodeUtf8 )
import Extra.Except ( MonadError )
import Language.Haskell.TH.Instances ()
import Prelude hiding (show)
import System.IO ( hFlush, stdout )
import Test.HUnit ( assertEqual, Test(..) )
import Test.QuickCheck
    ( Arbitrary(arbitrary),
      Gen,
      elements,
      listOf1,
      oneof,
      withMaxSuccess,
      quickCheck )
import SeeReason.Errors (Member, OneOf, runOneOf, throwMember )
import SeeReason.UIO as UIO (liftUIO, NonIOException, Unexceptional, unsafeFromIO)
import Web.Routes ( fromPathInfo, toPathInfo )
import Web.Routes.QuickCheck ( pathInfoInverse_prop )

-- | Integrity testing
validateImageKey ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Member NonIOException e, Member IOException e, Member FileError e,
                 MonadReader r m, HasCacheAcid r, HasFileCacheTop r)
  => ImageKey -> m ()
validateImageKey key = do
  cacheLook key >>=
    maybe (throwMember (MissingDerivedEntry key))
          (either throwMember (validateImageFile key))

#if 1
validateImageFile ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Member FileError e, Member NonIOException e, Member IOException e,
                 MonadReader r m, HasFileCacheTop r)
  => ImageKey -> ImageFile -> m ()
validateImageFile _key (ImageFileShape _) = return ()
validateImageFile key (ImageFileReady i@(ImageReady {..})) = do
  path <- fileCachePath (ImagePath key)
  when (imageType i == JPEG)
    (liftUIO (validateJPG path) >>= either throwMember (\_ -> return ()))
  runOneOf @(FileError ': e) (liftUIO (BS.readFile path)) >>= checkFile _imageFile path
#else
validateImageFile ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Member FileError e, Member NonIOException e,
                 MonadReader r m, HasFileCacheTop r)
  => ImageKey -> ImageFile -> m ()
validateImageFile _key (ImageFileShape _) = return ()
validateImageFile key (ImageFileReady i@(ImageReady {..})) = do
  path <- fileCachePath (ImagePath key (imageType i))
  when (imageType i == JPEG)
    (liftUIO (validateJPG path) >>= either (throwError . review fileError) (\_ -> return ()))
  runFileError (liftUIO (BS.readFile path)) >>= checkFile _imageFile path
#endif
  where
    checkFile :: File -> FilePath -> Either FileError BS.ByteString -> m ()
    checkFile _file _path (Left e) =
      unsafeFromIO (putStrLn ("error loading " ++ show _imageFile ++ ": " ++ show e))
    checkFile file _path (Right bs)
      | T.pack (show (md5 bs)) /= (_fileChksum file) =
          unsafeFromIO (putStrLn ("checksum mismatch in file " ++ show file))
    checkFile _file _path _bs = return ()

tests :: Test
tests = TestList [ TestCase (assertEqual "lens_saneSize 1"
                               (SaneSize (ImageSize {_dim = TheHeight, _size = 0.25, _units = Inches}))
                               (saneSize (ImageSize {_dim = TheHeight, _size = 0.0, _units = Inches})))
                 ]

-- splits (splitAt 3) [1,2,3,4,5,6,7] -> [[1,2,3],[4,5,6],[7]]
splits :: ([a] -> ([a], [a])) -> [a] -> [[a]]
splits _ [] = []
splits f xs = let (lhs, rhs) = f xs in (lhs : splits f rhs)

-- | The migration of 'ImageKey' sets the 'ImageType' field to
-- 'Unknown' everywhere, this looks at the pairs in the cache map and
-- copies the 'ImageType' of the 'ImageFile' into the keys.  We can't
-- do this in the CacheMap migration above because the types are too
-- specific.  But this should be removed when 'ImageKey' version 2 is
-- removed.
--
-- Also recomputes the orientation field of ImageShape.
fixImageShapes ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Member FileError e, Member IOException e, Member NonIOException e,
                 MonadReader r m, HasFileCacheTop r)
  => Map ImageKey (Either FileError ImageFile)
  -> m (Map ImageKey (Either FileError ImageFile))
fixImageShapes mp =
  let mp' = fromList (fixOriginalKeys (changes mp) (toList mp)) in
    (fromList . concat) <$> mapM fixPairs (splits (splitAt 100) (toList mp'))
  where
    fixPairs :: [(ImageKey, Either FileError ImageFile)] -> m [(ImageKey, Either FileError ImageFile)]
    fixPairs pairs = unsafeFromIO (putStr "." >> hFlush stdout) >> mapM fixPair pairs
    fixPair :: (ImageKey, Either FileError ImageFile) -> m (ImageKey, Either FileError ImageFile)
    -- If already damaged use the result
    fixPair (key, Left e) = runOneOf @(FileError ': e) (fixImageCached (key, Left e)) >>= either (return . (key,) . Left) return
    -- fixPair (key, Left e) = runOneOf @(FileError ': e) (fixImageCached (key, Left e)) (either (\e -> return (key, Left e)) (return . (key,)))
    -- If not damaged only use the result only if it succeeds
    fixPair (key, Right val) = runOneOf @(FileError ': e) (fixImageCached (key, Right val)) >>= either (\(_ :: FileError) -> return (key, Right val)) return
    -- fixPair (key, Right val) = either (\_ -> (key, Right val)) id <$> runExceptT (runOneOf' @FileError (fixImageCached (key, Right val)))

fixImageCached ::
  forall e r m. (Unexceptional m, MonadError (OneOf e) m, Member FileError e, Member IOException e, Member NonIOException e,
                 MonadReader r m, HasFileCacheTop r)
  => (ImageKey, Either FileError ImageFile) -> m (ImageKey, Either FileError ImageFile)
fixImageCached (key@(ImageOriginal csum typ), Right (ImageFileShape _s)) =
  fixImageShape (csum, typ) >>= \s' -> return (key, Right (ImageFileShape s'))
fixImageCached (key@(ImageOriginal csum typ), Right (ImageFileReady (ImageReady f _s))) =
  fixImageShape (csum, typ) >>= \s' -> return (key, Right (ImageFileReady (ImageReady f s')))
fixImageCached x = return x

-- Actually we just compute it from scratch
fixImageShape ::
  forall e r m a. (Unexceptional m, MonadError (OneOf e) m, Member FileError e, Member IOException e, Member NonIOException e,
                   MonadReader r m, HasFileCacheTop r, HasImageFilePath a)
  => a -> m ImageShape
fixImageShape a = fileCachePath a >>= imageShapeM . (, BS.empty)

-- | The repairs we need to perform on the original keys during 'setImageFileTypes'.
changes ::
     Map ImageKey (Either FileError ImageFile)
  -> (ImageKey, Either FileError ImageFile)
  -> (ImageKey, Either FileError ImageFile)
changes mp = \(key, file) -> (maybe key id (Map.lookup key changeMap), file)
  where
    changeMap :: Map ImageKey ImageKey
    changeMap = fromList (fmap (\(key, img) -> (key, fixOriginalKey key img)) (toList mp))
    fixOriginalKey :: ImageKey -> (Either FileError ImageFile) -> ImageKey
    fixOriginalKey (ImageOriginal csum Unknown) (Right img) = ImageOriginal csum (imageType img)
    fixOriginalKey key _img = key

fixOriginalKeys ::
     ((ImageKey, Either FileError ImageFile) -> (ImageKey, Either FileError ImageFile))
  -> [(ImageKey, Either FileError ImageFile)]
  -> [(ImageKey, Either FileError ImageFile)]
fixOriginalKeys f pairs = fmap f pairs

instance Arbitrary ImageKey where
  arbitrary = scaled $ crop $ upright original
    where
      original :: Gen ImageKey
      original = ImageOriginal <$> (pack <$> listOf1 (elements "0123456789abcdef")) <*> arbitrary
      upright :: Gen ImageKey -> Gen ImageKey
      upright i = oneof [i, ImageUpright <$> i]
      crop :: Gen ImageKey -> Gen ImageKey
      crop i = oneof [i, ImageCropped <$> arbitrary <*> i]
      scaled :: Gen ImageKey -> Gen ImageKey
      scaled i = oneof [i, ImageScaled
                             <$> ((_unSaneSize . saneSize) <$> arbitrary)
                             <*> ((approx . abs) <$> arbitrary)
                             <*> i]

instance Arbitrary ImageSize where
  arbitrary = (ImageSize <$> arbitrary <*> ((approx . abs) <$> arbitrary) <*> arbitrary)

instance Arbitrary ImageCrop where
  arbitrary = ImageCrop <$> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> (abs <$> arbitrary) <*> arbitrary

instance Arbitrary ImageType where
  arbitrary = oneof [pure PPM, pure JPEG, pure GIF, pure PNG, pure PDF, pure Unknown]

instance Arbitrary Dimension where arbitrary = elements [minBound..maxBound]
instance Arbitrary Units where arbitrary = elements [minBound..maxBound]
instance Arbitrary Rotation where arbitrary = elements [ZeroHr, ThreeHr, SixHr, NineHr]

#if 1
test1 :: Test
test1 =
  TestCase (assertEqual "test1"
              -- This is not the path to the file on disk, its what is used in the URI.
              ("/image-scaled/image-size/the-area/15/1/inches/100/1/image-upright/c3bd1388b41fa5d956e4308ce518a8bd.png" :: Text
            -- "/image-path/image-scaled/image-size/the-area/15/1/inches/100/1/image-upright/image-original/c3bd1388b41fa5d956e4308ce518a8bd/i.png" :: Text
              )
              (toPathInfo (_imageCachedKey file)))
  where
    file = ImageCached
             {_imageCachedKey = ImageScaled
                                  (ImageSize {_dim = TheArea, _size = 15 % 1, _units = Inches})
                                  (100 % 1)
                                  (ImageUpright (ImageOriginal "c3bd1388b41fa5d956e4308ce518a8bd" PNG)),
              _imageCachedFile = ImageFileReady (ImageReady {_imageFile = File {_fileSource = Legacy, _fileChksum = "be04a29700b06072326364fa1ce45f39", _fileMessages = [], _fileExt = ".jpg"},
                                            _imageShape = ImageShape {_imageShapeType = JPEG, _imageShapeWidth = 885, _imageShapeHeight = 170, _imageFileOrientation = ZeroHr}})}

#endif

testKey :: ImageKey -> IO ()
testKey key = do
  putStrLn (show key)
  let path = toPathInfo key :: Text
  putStrLn (unpack path)
  case fromPathInfo (encodeUtf8 path) of
    Left s -> putStrLn s
    Right (key' :: ImageKey) -> do
      putStrLn (show key')
      putStrLn (unpack (toPathInfo key'))

-- This fails because approx actually isn't idempotent.  I'm not 100%
-- sure whether it should be or not.
--
-- > approx (1409154553389 % 1397579329319)
-- 121 % 120
-- > approx (121 % 120)
-- 120 % 119
-- > approx (120 % 119)
-- 119 % 118
approx_prop :: Rational -> Bool
approx_prop r = approx r == approx (approx r)

quickTests = do
  quickCheck (withMaxSuccess 1000 (pathInfoInverse_prop :: ImageKey -> Bool))
  -- quickCheck (approx_prop :: Rational -> Bool)

{-
> toPathInfo (ImageScaled (ImageSize {_dim = TheWidth, _size = 1 % 4, _units = Inches}) (142 % 163) (ImageCropped (ImageCrop {topCrop = 0, bottomCrop = 0, leftCrop = 1, rightCrop = 1, rotation = SixHr}) (ImageUpright (ImageOriginal "" PNG))))
"/image-scaled/image-size/the-width/1/4/inches/115/132/image-cropped/image-crop/0/0/1/1/six-hr/image-upright/image-original//i.png"
toPathSegments (ImageScaled (ImageSize {_dim = TheWidth, _size = 1 % 4, _units = Inches}) (142 % 163) (ImageCropped (ImageCrop {topCrop = 0, bottomCrop = 0, leftCrop = 1, rightCrop = 1, rotation = SixHr}) (ImageUpright (ImageOriginal "" PNG))))
["image-scaled","image-size","the-width","1","4","inches","115","132","image-cropped","image-crop","0","0","1","1","six-hr","image-upright","image-original","","i.png"]
% fromPathInfo "/image-scaled/image-size/the-width/1/4/inches/115/132/image-cropped/image-crop/0/0/1/1/six-hr/image-upright/image-original//i.png" :: Either String ImageKey
Right (ImageScaled (ImageSize {_dim = TheWidth, _size = 1 % 4, _units = Inches}) (115 % 132) (ImageCropped (ImageCrop {topCrop = 0, bottomCrop = 0, leftCrop = 1, rightCrop = 1, rotation = SixHr}) (ImageUpright (ImageOriginal "" PNG))))
-}
