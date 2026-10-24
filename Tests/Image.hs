{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedLists, OverloadedStrings, RankNTypes #-}
{-# LANGUAGE RecordWildCards, ScopedTypeVariables, TupleSections, TypeFamilies #-}

module Image where

import Control.Exception (IOException)
import Control.Lens (_Left, over)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Reader
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Acid.Abstract (query')
import Data.FileCache (CacheMap(CacheMap),
                       File(..),
                       FileCacheTop(FileCacheTop),
                       FileSource(Derived, ThePath),
                       FileType(PNG, PDF, JPEG),
                       HasCacheAcid,
                       HasFileCacheTop(fileCacheTop),
                       ImageFile(ImageFileReady),
                       ImageKey(ImageOriginal, ImageScaled, ImageUpright),
                       ImageSize(ImageSize), Dimension(TheArea), Units(Inches),
                       ImageCached(ImageCached),
                       HasImageRect(imageRect),
                       ImageReady(..),
                       ImageShape(..),
                       makeImageRect,
                       MonadFileCache,
                       Rotation(ZeroHr))
import Data.FileCache.CacheMap (CacheMap(CacheMap, _unCacheMap, _requested))
import Data.FileCache.FileCache (cachePut, collectGarbage, FileCacheT, fileCachePathIO, knownDerived, runFileCacheT)
import Data.FileCache.FileCacheTop (CacheAcid)
import Data.FileCache.FileInfo (fileInfoFromBytes)
import Data.FileCache.ImageKey (thumbKey)
import Data.FileCache.Server (makeByteString, LookMap(LookMap), originalKey, getImageFile)
import Data.FileCache.Derive (testImageKeys)
import qualified Data.Map as Map (keys)
import Data.FileCache.Upload (cacheOriginalFile)
import Data.Map as Map (size)
import Data.Proxy (Proxy(Proxy))
import Data.Set as Set (filter)
import Extra.Exceptionless (Exceptionless, runExceptionless)
import GHC.Stack (HasCallStack)
import SeeReason.Errors (catchMember, ConvertError, Member, OneOf, liftMember, throwMember)
import System.FilePath ((</>))
import System.Log.Logger (Priority(DEBUG))
import Test.HUnit

import Types (ES, AcidT, runAcidT_, withLogging)

tests :: (AcidState CacheMap, FileCacheTop) -> Test
tests r@(acid, top) =
  TestCase $
    runAcidT_ r $
      withLogging DEBUG $
        upload "Tests/data/APR_Logo_Symbol_Black.png"

instance HasFileCacheTop (AcidState CacheMap, FileCacheTop) where
  fileCacheTop = snd

upload ::
  forall r m.
  (r ~ (AcidState CacheMap, FileCacheTop),
   -- HasCacheAcid r,
   -- HasFileCacheTop r,
   MonadIO m,
   MonadCatch m,
   -- ConvertError SomeException (Either SomeException (OneOf ES))
   HasCallStack)
  => FilePath -> ReaderT r (ExceptT (OneOf ES) m) ()
upload path = do
  (key, file) <- cacheOriginalFile (Just (ThePath path)) path
  cachePut key (Right file)
  img <- derive (thumbKey key)
  liftIO $ assertEqual "cache 1"
    (ImageOriginal "a5bf499452b0dcdb203dd67ae0e0cf6e" PNG,
     ImageFileReady
      (ImageReady
        {_imageFile =
            File {_fileSource = ThePath "Tests/data/APR_Logo_Symbol_Black.png",
                  _fileChksum = "a5bf499452b0dcdb203dd67ae0e0cf6e",
                  _fileMessages = [], _fileExt = ".png"},
         _imageShape = ImageShape {_imageShapeType = PNG,
                                   _imageShapeRect = Right (makeImageRect 240 240 ZeroHr)}}),
     ImageFileReady
      (ImageReady
        {_imageFile =
            File {_fileSource = Derived,
                  -- This was "5353b35ca554264eea56f57e3bf1930b" on the older vips, something changed.
                  _fileChksum = "5b4d88f82c87c5f0ae25474dcb70cb4a",
                  _fileMessages = [],
                  _fileExt = ".jpg" -- wrong
                 },
         _imageShape = ImageShape {_imageShapeType = JPEG, -- wrong
                                   _imageShapeRect = Right (makeImageRect 100 100 ZeroHr)}}))
    (key, file, img)
  pure ()

derive ::
  forall r m.
  (r ~ (AcidState CacheMap, FileCacheTop),
   -- HasCacheAcid r,
   -- HasFileCacheTop r,
   MonadIO m,
   MonadCatch m,
   -- ConvertError SomeException (Either SomeException (OneOf ES))
   HasCallStack)
  => ImageKey -> ReaderT r (ExceptT (OneOf ES) m) ImageFile
derive key = getImageFile [] key >>= either throwMember pure

imageTests :: FilePath -> AcidState CacheMap -> Test
imageTests top acid =
  TestList
    [ test1 top
    , TestCase $ do
        CacheMap{..} <- query' acid LookMap
        assertEqual "map size" 2 (Map.size _unCacheMap)
    , TestCase $ do
        CacheMap{..} <- query' acid LookMap
        r <- runReaderT (collectGarbage _unCacheMap) (FileCacheTop "Tests/cache")
        -- writeFile "/tmp/gc" (show r)
        let originalIsPNG :: (FilePath, ImageKey) -> Bool
            originalIsPNG (_, key) = case originalKey key of
                               ImageOriginal _ PNG -> True
                               _ -> False
        assertEqual "gc" "fromList []" (show (Set.filter originalIsPNG (knownDerived r)))
{-      assertEqual "gc" "" (show (Set.size (orphans r),
                                   Set.size (orphansDerived r),
                                   Set.size (known r),
                                   Set.size (knownDerived r),
                                   Set.size (FileCache.errors r))) -}
    ]

test1 :: FilePath -> Test
test1 top = TestCase $ do
  (shape :: Either String ImageShape) <- over _Left show <$> runExceptT action2
  assertEqual "fileInfoFromBytes" (Right (ImageShape PDF (Left "PDF"))) shape
  where
    action2 :: ExceptT (OneOf ES) IO ImageShape
    action2 = runExceptionless throwMember action
    action :: Exceptionless (ExceptT (OneOf ES) IO) ImageShape
    action = catchMember (makeByteString pdf) (\(Proxy :: Proxy ES) (e :: IOException) -> throwMember e) >>= fileInfoFromBytes
    pdf :: FilePath
    pdf = top </> "data/fbddca395b0912cdfa710f84ab09f317.pdf"

-- | A record left from when scaling could enlarge: it says the screen
-- size of a 240x240 original at 600 dpi is 2400x2400.  Looking it up
-- has to rebuild it, which now leaves the original's pixels alone.
enlargedTests :: (AcidState CacheMap, FileCacheTop) -> Test
enlargedTests r =
  TestCase $
    runAcidT_ r $
      withLogging DEBUG $ do
        (key, _file) <- cacheOriginalFile (Just (ThePath path)) path
        let big = ImageScaled (ImageSize TheArea 15 Inches) 600 key
            stale = ImageFileReady
                      (ImageReady
                        { _imageFile = File { _fileSource = Derived
                                            , _fileChksum = "stale"
                                            , _fileMessages = []
                                            , _fileExt = ".png" }
                        , _imageShape = ImageShape { _imageShapeType = PNG
                                                   , _imageShapeRect = Right (makeImageRect 2400 2400 ZeroHr) } })
        stalePath <- fileCachePathIO (ImageCached big stale)
        liftIO $ writeFile stalePath "stale"
        cachePut big (Right stale)
        img <- derive big
        liftIO $ assertEqual "enlarged record rebuilt at the original size"
          (Right (makeImageRect 240 240 ZeroHr)) (imageRect img)
        liftIO $ assertEqual "enlarged record rebuilt from the original's bytes"
          (Just "a5bf499452b0dcdb203dd67ae0e0cf6e") (fileChksum img)
  where
    path = "Tests/data/APR_Logo_Symbol_Black.png"
    fileChksum (ImageFileReady ImageReady{..}) = Just (_fileChksum _imageFile)
    fileChksum _ = Nothing

-- | Once an image is built, looking it up again finds it ready.
stableTests :: (AcidState CacheMap, FileCacheTop) -> Test
stableTests r =
  TestCase $
    runAcidT_ r $
      withLogging DEBUG $ do
        (key, _file) <- cacheOriginalFile (Just (ThePath path)) path
        let small = ImageScaled (ImageSize TheArea (1/2) Inches) 100 key
        _built <- derive small
        (needed, _stats) <- testImageKeys [small]
        liftIO $ assertEqual "built image stays ready" [] (Map.keys needed)
  where
    path = "Tests/data/APR_Logo_Symbol_Black.png"

-- | The same for the kind of key a report's figures use: a JPEG,
-- uprighted, then scaled down.
stableJpegTests :: (AcidState CacheMap, FileCacheTop) -> Test
stableJpegTests r =
  TestCase $
    runAcidT_ r $
      withLogging DEBUG $ do
        (key, _file) <- cacheOriginalFile (Just (ThePath path)) path
        let fig = ImageScaled (ImageSize TheArea (1/2) Inches) 100 (ImageUpright key)
        _built <- derive fig
        (needed, _stats) <- testImageKeys [fig]
        liftIO $ assertEqual "built JPEG figure stays ready" [] (Map.keys needed)
  where
    path = "Tests/data/APR_Logo_Symbol_Black.jpg"
