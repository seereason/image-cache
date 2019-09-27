-- | Maintain a cache of image files.
--
-- Database transactions to manage a cache of image files.  This
-- allows us to find derived versions of an image (resized, cropped,
-- rotated) given the ImageKey for the desired version, which contains
-- the checksum of the original image and the desired transformation.
-- If the desired transformation is not in the cached it is produced
-- and added.
--
-- The 'ImageKey' type describes the 'ImageFile' we would like the
-- system to produce.  This is passed to the 'build' method (which may
-- use IO) of 'MonadCache', and if that 'ImageKey' is not already in
-- the cache the desired 'ImageFile' is generated, added to the cache,
-- and returned.

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall -Wredundant-constraints #-}

module Data.FileCache.ImageCache
    ( -- * Image cache monad
      ImageCacheT
    , runImageCacheT
    , execImageCacheT
    -- * ImageFile upload
    , imageFileFromBytes
    , imageFileFromURI
    , imageFileFromPath
    -- * ImageFile query
    , imageFilePath
    -- * Deriving new ImageFiles
    , uprightImage
    , scaleImage
    , editImage
    ) where

import Control.Exception (IOException, throw, try)
import Control.Lens (_1, makeLensesFor, view)
import Control.Monad.Except (catchError, MonadError)
--import Control.Monad.Reader (MonadReader(ask))
import Control.Monad.RWS (RWST)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Acid (AcidState)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as P (fromStrict, toStrict)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString.UTF8 as P
import qualified Data.ByteString as P
#endif
import Data.FileCache.AcidCache (HasCache(..))
import Data.FileCache.Exif (normalizeOrientationCode)
import Data.FileCache.FileCache ({-fileChksum,-} fileCachePath, fileFromBytes, fileFromPath, fileFromURI,
                            fileFromCmd, loadBytesSafe)
import Data.FileCache.FileCacheT (execFileCacheT, FileCacheT, FileCacheTop, HasFileCacheTop, runFileCacheT)
import Data.FileCache.FileError (FileError(..), HasFileError)
import Data.FileCache.Image (ImageCrop(..), ImageFile(..), ImageType(..), ImageKey(..),
                        fileExtension, PixmapShape(..), scaleFromDPI, approx)
import Data.FileCache.ImageFile (getFileType)
import Data.FileCache.LogException (logException)
import Data.FileCache.Types (CacheMap, CacheValue(..), File(..))
import Data.Generics (Proxy)
import Data.Generics.Product (field)
import Data.List (intercalate)
import Data.Maybe ( fromMaybe )
import Data.Text (pack)
import Extra.Except
import Network.URI (URI, uriToString)
import Numeric (fromRat, showFFloat)
import System.Exit (ExitCode(..))
import System.Log.Logger (logM, Priority(ERROR))
import System.Process (CreateProcess(..), CmdSpec(..), proc, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode, showCreateProcessForUser)
import "regex-compat-tdfa" Text.Regex (mkRegex, matchRegex)

-- | Return the local pathname of an image file.  The path will have a
-- suitable extension (e.g. .jpg) for the benefit of software that
-- depends on this, so the result might point to a symbolic link.
imageFilePath :: HasFileCacheTop m => ImageFile -> m FilePath
imageFilePath img = fileCachePath (view (field @"_imageFile") img)

-- | Find or create a cached image matching this ByteString.
imageFileFromBytes ::
    forall e m. (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m)
    => ByteString
    -> m (CacheValue FileError ImageFile)
imageFileFromBytes bs = fileFromBytes (liftIOError . getFileType) fileExtension bs >>= makeImageFile

imageFileFromURI ::
    (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m)
    => URI
    -> m (CacheValue FileError ImageFile)
imageFileFromURI uri = fileFromURI (liftIOError . getFileType) fileExtension (uriToString id uri "") >>= makeImageFile

-- | Find or create a cached image file by reading from local file.
imageFileFromPath ::
    (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m)
    => FilePath
    -> m (CacheValue FileError ImageFile)
imageFileFromPath path = fileFromPath (liftIOError . getFileType) fileExtension path >>= makeImageFile

-- | Create an image file from a 'File'.  An ImageFile value implies
-- that the image has been found in or added to the acid-state cache.
-- Note that 'InProgress' is not a possible result here, it will only
-- occur for derived (scaled, cropped, etc.) images.
makeImageFile ::
    forall e m. (MonadIOError e m, HasFileCacheTop m)
    => (File, ImageType)
    -> m (CacheValue FileError ImageFile)
makeImageFile (file, ityp) = do
    path <- fileCachePath file
    (r :: Either IOException ImageFile) <- liftIO (try ($logException ERROR (imageFileFromType path file ityp)))
    return $ either (Failed . fromIOException) Cached r

-- | Helper function to build an image once its type is known - JPEG,
-- GIF, etc.
imageFileFromType :: FilePath -> File -> ImageType -> IO ImageFile
imageFileFromType path file typ = do
  -- logM "Appraisal.ImageFile.imageFileFromType" DEBUG ("Appraisal.ImageFile.imageFileFromType - typ=" ++ show typ) >>
  let cmd = case typ of
              JPEG -> pipe [proc "jpegtopnm" [path], proc "pnmfile" []]
              PPM ->  (proc "pnmfile" [])
              GIF -> pipe [proc "giftopnm" [path], proc "pnmfile" []]
              PNG -> pipe [proc "pngtopnm" [path], proc "pnmfile" []]
  -- err may contain "Output file write error --- out of disk space?"
  -- because pnmfile closes the output descriptor of the decoder
  -- process early.  This can be ignored.
  (code, out, _err) <- liftIO $ readCreateProcessWithExitCode cmd P.empty
  case code of
    ExitSuccess -> imageFileFromPnmfileOutput file typ out
    ExitFailure _ -> error $ "Failure building image file:\n " ++ showCmdSpec (cmdspec cmd) ++ " -> " ++ show code

-- | Helper function to load a PNM file.
imageFileFromPnmfileOutput :: File -> ImageType -> P.ByteString -> IO ImageFile
imageFileFromPnmfileOutput file typ out =
        case matchRegex pnmFileRegex (P.toString out) of
          Just [width, height, _, maxval] ->
            return $ ImageFile { _imageFile = file
                               , _imageFileType = typ
                               , _imageFileWidth = read width
                               , _imageFileHeight = read height
                               , _imageFileMaxVal = if maxval == "" then 1 else read maxval }
          _ -> error $ "Unexpected output from pnmfile: " ++ show out
  where
      pnmFileRegex = mkRegex "^stdin:\tP[PGB]M raw, ([0-9]+) by ([0-9]+)([ ]+maxval ([0-9]+))?$"

-- | The image file names are just checksums.  This makes sure a link
-- with a suitable extension (.jpg, .gif) also exists.
-- ensureExtensionLink :: MonadFileCacheIO st IOException m => File -> String -> m ()
-- ensureExtensionLink file ext = fileCachePath file >>= \ path -> liftIO $ ensureLink (view fileChksum file) (path ++ ext)

-- | Find or create a version of some image with its orientation
-- corrected based on the EXIF orientation flag.  If the image is
-- already upright this will return the original ImageFile.
uprightImage ::
    (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e)
    => ImageFile
    -> m (CacheValue FileError ImageFile)
uprightImage orig = do
  bs <- $logException ERROR (loadBytesSafe (view (field @"_imageFile") orig))
  bs' <- $logException ERROR (liftIOError (normalizeOrientationCode (P.fromStrict bs)))
  either (const (return (Cached orig))) (\bs'' -> (fileFromBytes (liftIOError . $logException ERROR . getFileType) fileExtension (P.toStrict bs'')) >>= makeImageFile) bs'

-- | Find or create a cached image resized by decoding, applying
-- pnmscale, and then re-encoding.  The new image inherits attributes
-- of the old other than size.
scaleImage :: forall e m. (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e) => Double -> ImageFile -> m (CacheValue FileError ImageFile)
scaleImage scale orig | approx (toRational scale) == 1 = return (Cached orig)
scaleImage scale orig = $logException ERROR $ do
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
    fileFromCmd (liftIOError . getFileType) fileExtension cmd >>= buildImage
    -- fileFromCmdViaTemp cmd >>= buildImage
    where
      buildImage :: (File, ImageType) -> m (CacheValue FileError ImageFile)
      buildImage file = makeImageFile file `catchError` (\ e -> fail $ "scaleImage - makeImageFile failed: " ++ show e)

-- | Find or create a cached image which is a cropped version of
-- another.
editImage ::
    forall e m. (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e)
    => ImageCrop -> ImageFile -> m (CacheValue FileError ImageFile)
editImage crop file = $logException ERROR $
    case commands of
      [] ->
          return (Cached file)
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
      err :: e -> m (CacheValue FileError ImageFile)
      err e = return $ Failed $ ErrorCall $ "editImage Failure: file=" <> pack (show file) <> ", error=" <> pack (show e)

pipeline :: [CreateProcess] -> P.ByteString -> IO P.ByteString
pipeline [] bytes = return bytes
pipeline (p : ps) bytes =
    (readCreateProcessWithExitCode p bytes >>= doResult)
      `catchError` (\ (e :: IOException) -> doException (showCreateProcessForUser p ++ " -> " ++ show e) e)
    where
      doResult (ExitSuccess, out, _) = pipeline ps out
      doResult (code, _, err) = let message = (showCreateProcessForUser p ++ " -> " ++ show code ++ " (" ++ show err ++ ")") in doException message (userError message)
      -- Is there any exception we should ignore here?
      doException message e = logM "Appraisal.ImageFile" ERROR message >> throw e

{-
pipelineWithExitCode :: [(String, [String])] -> B.ByteString -> IO (ExitCode, B.ByteString, [B.ByteString])
pipelineWithExitCode cmds inp =
    pipeline' cmds inp (ExitSuccess, [])
    where
      pipeline' _ bytes (code@(ExitFailure _), errs) = return (code, bytes, errs)
      pipeline' [] bytes (code, errs) = return (code, bytes, reverse errs)
      pipeline' ((cmd, args) : rest) bytes (code, errs) =
          do (code, out, err) <- readProcessWithExitCode cmd args bytes
             pipeline' rest out (code, err : errs)

showPipelineForUser :: [(String, [String])] -> String
showPipelineForUser ((cmd, args) : rest) =
    showCommandForUser cmd args ++
    case rest of 
      [] -> ""
      _ -> " | " ++ showPipelineForUser rest
-}

showCmdSpec :: CmdSpec -> String
showCmdSpec (ShellCommand s) = s
showCmdSpec (RawCommand p ss) = showCommandForUser p ss

pipe :: [CreateProcess] -> CreateProcess
pipe xs = foldl1 pipe2 xs

pipe2 :: CreateProcess -> CreateProcess -> CreateProcess
pipe2 a b =
    if cwd a == cwd b &&
       env a == env b &&
       close_fds a == close_fds b &&
       create_group a == create_group b
    then a {cmdspec = ShellCommand (showCmdSpec (cmdspec a) ++ " | " ++ showCmdSpec (cmdspec b))}
    else error $ "Pipeline of incompatible commands: " ++ showCreateProcessForUser a ++ " | " ++ showCreateProcessForUser b

pipe' :: [String] -> String
pipe' = intercalate " | "

$(makeLensesFor [("imageFile", "imageFileL")] ''ImageFile)

type ImageCacheT w s e m = FileCacheT (AcidState (CacheMap ImageKey ImageFile FileError)) w s m

runImageCacheT ::
    (top ~ FileCacheTop, Monad m)
    => Proxy w -> s -> acid -> top -> RWST (acid, top) w s m a -> m a
runImageCacheT = runFileCacheT

execImageCacheT ::
    (top ~ FileCacheTop, Monad m)
    => Proxy w -> s -> acid -> top -> RWST (acid, top) w s m a -> m s
execImageCacheT = execFileCacheT

-- | Build a MonadCache instance for images on top of a MonadFileCache
-- instance and a reader for the acid state.
instance (MonadIOError e m, HasFileError e, Monoid w, m' ~ ImageCacheT w s e m, Show e)
  => HasCache ImageKey ImageFile FileError m' where
    askCacheAcid = view _1 :: m' (AcidState (CacheMap ImageKey ImageFile FileError))
    buildCacheValue = buildImageFile

-- | Build and return the 'ImageFile' described by the 'ImageKey'.
-- Note that this does not insert the new 'ImageFile' into the acid
-- state.
buildImageFile :: (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e)
  => ImageKey -> m (CacheValue FileError ImageFile)
buildImageFile (ImageOriginal img) = return (Cached img)
buildImageFile (ImageUpright key) =
  buildImageFile key >>= overCached uprightImage
buildImageFile (ImageScaled sz dpi key) = do
  buildImageFile key >>= overCached (\img -> do
                                        let scale = scaleFromDPI dpi sz img
                                        $logException ERROR $ scaleImage (fromRat (fromMaybe 1 scale)) img)
buildImageFile (ImageCropped crop key) = do
  buildImageFile key >>= overCached (editImage crop)

overCached :: Monad m => (a -> m (CacheValue e a)) -> CacheValue e a -> m (CacheValue e a)
overCached f (Cached a) = f a
overCached _ v = pure v
