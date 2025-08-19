{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators #-}

module Data.FileCache.Test
  ( fixImageShapes
  , validateImageKey
  , validateImageFile
  , test1
  , tests
  , testKey
  , quickTests
  , approx_prop
  ) where

import Control.Monad ( when )
import Control.Monad.Reader (liftIO)
import qualified Data.ByteString.Lazy as BS ( ByteString, empty, readFile )
import Data.Digest.Pure.MD5 ( md5 )
import Data.FileCache.CacheMap ( ImageCached(ImageCached, _imageCachedFile, _imageCachedKey) )
import Data.FileCache.File
import Data.FileCache.FileCache ( HasFilePath, fileCachePath, cacheLook )
import Data.FileCache.FileCacheTop (MonadFileCache)
import Data.FileCache.FileError
import Data.FileCache.FileInfo ()
import Data.FileCache.ImageCrop
import Data.FileCache.ImageFile
import Data.FileCache.ImageIO ( validateJPG )
import Data.FileCache.ImageKey
import Data.FileCache.ImageRect (makeImageRect)
import Data.FileCache.ImageSize
import Data.FileCache.Rational ((%), approx)
import Data.ListLike ( StringLike(show) )
import Data.Map.Strict as Map ( Map, lookup, toList, fromList )
import Data.Text as T ( pack, Text, unpack )
import Data.Text.Encoding ( encodeUtf8 )
import Language.Haskell.TH.Instances ()
import Prelude hiding (show)
import SeeReason.Errors (throwMember, tryMember)
import System.IO ( hFlush, stdout )
import Test.HUnit ( assertEqual, Test(..) )
import Test.QuickCheck ( Arbitrary(arbitrary), Gen, elements, listOf1, oneof, withMaxSuccess, quickCheck )
import Web.Routes ( fromPathInfo, toPathInfo )
import Web.Routes.QuickCheck ( pathInfoInverse_prop )

-- | Integrity testing
validateImageKey ::
  forall e r m. MonadFileCache r e m => ImageKey -> m ()
validateImageKey key = do
  cacheLook key >>=
    maybe (throwMember (MissingDerivedEntry key))
          (either throwMember (validateImageFile key))

validateImageFile :: forall e r m. MonadFileCache r e m => ImageKey -> ImageFile -> m ()
validateImageFile _key (ImageFileShape _) = return ()
validateImageFile key (ImageFileReady i@(ImageReady {..})) = do
  path <- fileCachePath (ImagePath key)
  when (imageType i == JPEG)
    (liftIO (validateJPG path) >>= either throwMember (\_ -> return ()))
  tryMember @FileError (liftIO (BS.readFile path)) >>= checkFile _imageFile path
  where
    checkFile :: File -> FilePath -> Either FileError BS.ByteString -> m ()
    checkFile _file _path (Left e) =
      liftIO $ putStrLn ("error loading " ++ show _imageFile ++ ": " ++ show e)
    checkFile file _path (Right bs)
      | T.pack (show (md5 bs)) /= (_fileChksum file) =
          liftIO (putStrLn ("checksum mismatch in file " ++ show file))
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

-- | The migration of 'ImageKey' sets the 'FileType' field to
-- 'Unknown' everywhere, this looks at the pairs in the cache map and
-- copies the 'FileType' of the 'ImageFile' into the keys.  We can't
-- do this in the CacheMap migration above because the types are too
-- specific.  But this should be removed when 'ImageKey' version 2 is
-- removed.
--
-- Also recomputes the orientation field of ImageShape.
fixImageShapes ::
  forall r e m. (MonadFileCache r e m)
  => Map ImageKey (Either FileError ImageFile)
  -> m (Map ImageKey (Either FileError ImageFile))
fixImageShapes mp =
  let mp' = fromList (fixOriginalKeys (changes mp) (toList mp)) in
    (fromList . concat) <$> mapM fixPairs (splits (splitAt 100) (toList mp'))
  where
    fixPairs :: [(ImageKey, Either FileError ImageFile)] -> m [(ImageKey, Either FileError ImageFile)]
    fixPairs pairs = liftIO (putStr "." >> hFlush stdout) >> mapM fixPair pairs
    fixPair :: (ImageKey, Either FileError ImageFile) -> m (ImageKey, Either FileError ImageFile)
    -- If already damaged use the result
    fixPair (key, Left e) = tryMember @FileError (fixImageCached (key, Left e)) >>= either (return . (key,) . Left) return
    -- If not damaged only use the result only if it succeeds
    fixPair (key, Right val) = tryMember @FileError (fixImageCached (key, Right val)) >>= either (\_ -> return (key, Right val)) return

fixImageCached ::
  forall e r m. (MonadFileCache r e m)
  => (ImageKey, Either FileError ImageFile) -> m (ImageKey, Either FileError ImageFile)
fixImageCached (key@(ImageOriginal csum typ), Right (ImageFileShape _s)) =
  fixImageShape (csum, typ) >>= \s' -> return (key, Right (ImageFileShape s'))
fixImageCached (key@(ImageOriginal csum typ), Right (ImageFileReady (ImageReady f _s))) =
  fixImageShape (csum, typ) >>= \s' -> return (key, Right (ImageFileReady (ImageReady f s')))
fixImageCached x = return x

-- Actually we just compute it from scratch
fixImageShape ::
  forall e r m a. (MonadFileCache r e m, HasFilePath a)
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

instance Arbitrary FileType where
  arbitrary = oneof [pure PPM, pure JPEG, pure GIF, pure PNG, pure PDF, pure CSV, pure Unknown]

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
              _imageCachedFile = ImageFileReady (ImageReady {_imageFile = File {_fileSource = Legacy,
                                                                                _fileChksum = "be04a29700b06072326364fa1ce45f39",
                                                                                _fileMessages = [],
                                                                                _fileExt = ".jpg"},
                                                             _imageShape = ImageShape{_imageShapeType = JPEG,
                                                                                      _imageShapeRect = Right (makeImageRect 885 170 ZeroHr)}})}

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

quickTests :: IO ()
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
