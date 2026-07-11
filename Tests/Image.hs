{-# LANGUAGE FlexibleInstances, LambdaCase, OverloadedLists, OverloadedStrings, RankNTypes, RecordWildCards, TupleSections, TypeFamilies #-}

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
                       ImageKey(ImageOriginal),
                       ImageReady(..),
                       ImageShape(..),
                       makeImageRect,
                       MonadFileCache,
                       Rotation(ZeroHr))
import Data.FileCache.CacheMap (CacheMap(CacheMap, _unCacheMap, _requested))
import Data.FileCache.FileCache (cachePut, collectGarbage, FileCacheT, knownDerived, runFileCacheT)
import Data.FileCache.FileCacheTop (CacheAcid)
import Data.FileCache.FileInfo (fileInfoFromBytes)
import Data.FileCache.ImageKey (thumbKey)
import Data.FileCache.Server (makeByteString, LookMap(LookMap), originalKey, getImageFile)
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
                  _fileChksum = "5353b35ca554264eea56f57e3bf1930b",
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
        r <- runReaderT (collectGarbage _unCacheMap) (FileCacheTop "/home/dsf/appraisalscribe3-development/images")
        writeFile "/tmp/gc" (show r)
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

#if MIN_VERSION_sr_errors(1,19,0)
type R = (AcidState CacheMap, FileCacheTop)
#endif

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

-- instance MonadFileCache (AcidState CacheMap, FileCacheTop) ES (RWST R () () (ExceptT (OneOf ES) IO))

{-
uploadTest :: IO ()
uploadTest = do
  -- withLogging DEBUG $
    withTestCache (run action) >>= \case
      Left e -> putStrLn ("e=" <> show e)
      Right ((key, file), (), ()) -> do
        putStrLn ("key=" <> show key)
        putStrLn ("file=" <> show file)
  where
    action :: FileCacheT R () () (ExceptT (OneOf ES) IO) (ImageKey, ImageFile)
    action = cacheOriginalFile @FilePath Nothing "sample2.heic"
    run action acid = runExceptT @(OneOf ES) (runFileCacheT acid () action)
-}
