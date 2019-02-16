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
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS -Wall #-}
module Appraisal.ImageCache
    ( -- * Image cache monad
      ImageCacheT
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
    , foo
    ) where

import Appraisal.Exif (normalizeOrientationCode)
import Appraisal.AcidCache ( MonadCache(..) )
import Appraisal.FileCache (File(..), {-fileChksum,-} fileCachePath, fileFromBytes, fileFromPath, fileFromURI,
                            fileFromCmd, loadBytesSafe)
import Appraisal.FileCacheT (FileCacheTop, HasFileCacheTop)
import Appraisal.FileError (FileError(..), HasFileError(fromFileError))
import Appraisal.Image (getFileType, ImageCrop(..), ImageFile(..), imageFile, ImageType(..), ImageKey(..),
                        fileExtension, imageFileType, PixmapShape(..), scaleFromDPI, approx)
import Appraisal.LogException (logException)
import Control.Exception (IOException, throw)
import Control.Lens (_1, _Left, makeLensesFor, over, view)
import Control.Monad.Except (catchError, ExceptT, liftEither, MonadError, runExceptT)
import Control.Monad.Reader (MonadReader(ask))
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
import Data.List (intercalate)
import Data.Maybe ( fromMaybe )
import Data.Map (Map)
import Extra.Except
--import Debug.Show (V)
--import Language.Haskell.TH (pprint)
import Network.URI (URI, uriToString)
import Numeric (fromRat, showFFloat)
import System.Exit (ExitCode(..))
import System.Log.Logger (logM, Priority(ERROR))
import System.Process (CreateProcess(..), CmdSpec(..), proc, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode, showCreateProcessForUser)
import Text.Regex (mkRegex, matchRegex)

-- foo :: forall m a. (Monad m, Monad m') => ExceptT FileError m a -> m a
foo :: (MonadError e m, HasFileError e) => ExceptT FileError m a -> m a
foo action = (liftEither . over _Left fromFileError) =<< runExceptT action

-- | Return the local pathname of an image file.  The path will have a
-- suitable extension (e.g. .jpg) for the benefit of software that
-- depends on this, so the result might point to a symbolic link.
imageFilePath :: HasFileCacheTop m => ImageFile -> m FilePath
imageFilePath img = fileCachePath (view imageFile img)

-- | Find or create a cached image matching this ByteString.
imageFileFromBytes ::
    forall e m. (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e)
    => ByteString
    -> m ImageFile
imageFileFromBytes bs = fileFromBytes (liftIOError . getFileType) fileExtension bs >>= makeImageFile

-- | Find or create a cached image file by downloading from this URI.
imageFileFromURI :: (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e) => URI -> m ImageFile
imageFileFromURI uri = fileFromURI (liftIOError . getFileType) fileExtension (uriToString id uri "") >>= makeImageFile

-- | Find or create a cached image file by reading from local file.
imageFileFromPath :: (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e) => FilePath -> m ImageFile
imageFileFromPath path = fileFromPath (liftIOError . getFileType) fileExtension path >>= makeImageFile

-- | Create an image file from a 'File'.  An ImageFile value implies
-- that the image has been found in or added to the acid-state cache.
makeImageFile :: forall e m. (MonadIO m, HasFileCacheTop m, MonadError e m, Show e) => (File, ImageType) -> m ImageFile
makeImageFile (file, ityp) = do
    -- logM "Appraisal.ImageFile.makeImageFile" INFO ("Appraisal.ImageFile.makeImageFile - INFO file=" ++ show file) >>
    path <- fileCachePath file
    $logException ERROR $ imageFileFromType path file ityp

-- | Helper function to build an image once its type is known - JPEG,
-- GIF, etc.
imageFileFromType ::
    (MonadIO m)
    => FilePath -> File -> ImageType -> m ImageFile
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
imageFileFromPnmfileOutput :: MonadIO m => File -> ImageType -> P.ByteString -> m ImageFile
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
uprightImage :: (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e) => ImageFile -> m ImageFile
uprightImage orig = do
  -- path <- _fileCachePath (imageFile orig)
  bs <- $logException ERROR (loadBytesSafe (view imageFile orig))
  bs' <- $logException ERROR (liftIOError (normalizeOrientationCode (P.fromStrict bs)))
  either (const (return orig)) (\bs'' -> $logException ERROR (fileFromBytes (liftIOError . getFileType) fileExtension (P.toStrict bs'')) >>= makeImageFile) bs'

-- | Find or create a cached image resized by decoding, applying
-- pnmscale, and then re-encoding.  The new image inherits attributes
-- of the old other than size.
scaleImage :: forall e m. (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e) => Double -> ImageFile -> m ImageFile
scaleImage scale orig | approx (toRational scale) == 1 = return orig
scaleImage scale orig = $logException ERROR $ do
    path <- fileCachePath (view imageFile orig)
    let decoder = case view imageFileType orig of
                    JPEG -> showCommandForUser "jpegtopnm" [path]
                    PPM -> showCommandForUser "cat" [path]
                    GIF -> showCommandForUser "giftopnm" [path]
                    PNG -> showCommandForUser "pngtopnm" [path]
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
        -- To save space, build a jpeg here rather than the original file type.
        encoder = case view imageFileType orig of
                    JPEG -> showCommandForUser "cjpeg" []
                    PPM -> showCommandForUser {-"cat"-} "cjpeg" []
                    GIF -> showCommandForUser {-"ppmtogif"-} "cjpeg" []
                    PNG -> showCommandForUser {-"pnmtopng"-} "cjpeg" []
        cmd = pipe' [decoder, scaler, encoder]
    fileFromCmd (liftIOError . getFileType) fileExtension cmd >>= buildImage
    -- fileFromCmdViaTemp cmd >>= buildImage
    where
      buildImage :: (File, ImageType) -> m ImageFile
      buildImage file = makeImageFile file `catchError` (\ e -> fail $ "scaleImage - makeImageFile failed: " ++ show e)

-- | Find or create a cached image which is a cropped version of
-- another.
editImage :: (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e) => ImageCrop -> ImageFile -> m ImageFile
editImage crop file = $logException ERROR $
    case commands of
      [] ->
          return file
      _ ->
          (loadBytesSafe (view imageFile file) >>=
           liftIO . pipeline commands >>=
           fileFromBytes (liftIO . getFileType) fileExtension >>=
           makeImageFile) `catchError` err
    where
      commands = buildPipeline (view imageFileType file) [cut, rotate] (latexImageFileType (view imageFileType file))
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
      err e = $logException ERROR $ fail $ "editImage Failure: file=" ++ show file ++ ", error=" ++ show e

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

type ImageCacheT w s e m = RWST (AcidState (Map ImageKey ImageFile), FileCacheTop) w s m

-- | Build a MonadCache instance for images on top of a MonadFileCache
-- instance and a reader for the acid state.
instance (MonadIO m, HasFileError e, MonadError e m, Monoid w, Show e)
      => MonadCache ImageKey ImageFile e
           (RWST (AcidState (Map ImageKey ImageFile), FileCacheTop) w s m) where
    askAcidState = view _1 <$> ask
    build = buildImageFile

-- | Adds ExceptT.
{-
instance (MonadIO m, Monoid w, HasFileError e, acid ~ (AcidState (Map ImageKey ImageFile)))
      => MonadCache ImageKey ImageFile e
           (ExceptT e (RWST (acid, FilePath) w (Map ImageKey (Either e ImageFile)) m)) where
    askAcidState = view _1 <$> ask
    build = withExceptT fromFileError . buildImageFile
-}

-- | Build and return the 'ImageFile' described by the 'ImageKey'.
-- Note that this does not insert the new 'ImageFile' into the acid
-- state.
buildImageFile ::
    (MonadIO m, HasFileCacheTop m, HasFileError e, MonadError e m, Show e)
    => ImageKey
    -> m ImageFile
buildImageFile (ImageOriginal img) = return img
buildImageFile (ImageUpright key) =
  buildImageFile key >>= $logException ERROR . {-withExceptT fromFileError .-} uprightImage
buildImageFile (ImageScaled sz dpi key) = do
  img <- buildImageFile key
  let scale = scaleFromDPI dpi sz img
  $logException ERROR $ {-withExceptT fromFileError $-} scaleImage (fromRat (fromMaybe 1 scale)) img
buildImageFile (ImageCropped crop key) = do
  buildImageFile key >>= $logException ERROR . {-withExceptT fromFileError .-} editImage crop
