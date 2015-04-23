{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
-- |Support for a 'File' that contains an image.
module Appraisal.ImageFile
    ( ImageFile(..)
    , ImageType(..)
    , extension
    -- * Build original image files
    , imageFileFromBytes
    , imageFileFromURI
    , imageFileFromPath
    -- * Image file queries
    , imageFileArea
    , imageFilePath
    -- * Build derived image files
    , uprightImage
    , scaleImage
    , editImage
    ) where

import Appraisal.Exif (normalizeOrientationCode)
import Appraisal.File (MonadFileCacheTop, File(..), fileCachePath, loadBytes, fileFromBytes, fileFromPath, fileFromURI, {-fileFromFile, fileFromCmd,-} fileFromCmdViaTemp)
import Appraisal.Image (PixmapShape(..), ImageCrop(..))
import Appraisal.Utils.ErrorWithIO (logException, ensureLink)
import Control.Exception (catch, IOException, SomeException, throw)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Except (MonadError, catchError)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.Generics (Data(..), Typeable)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base)
import qualified Data.ByteString.Lazy as P (fromStrict, toStrict)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString.UTF8 as P
import qualified Data.ByteString as P
#endif
import Network.URI (URI, uriToString)
import Numeric (showFFloat)
import System.Exit (ExitCode(..))
import System.Log.Logger (logM, Priority(ERROR))
import System.Process (CreateProcess(..), CmdSpec(..), proc, showCommandForUser, StdStream)
import System.Process.ListLike (readCreateProcessWithExitCode, readProcessWithExitCode, showCreateProcessForUser)
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)
import Text.Regex (mkRegex, matchRegex)

data ImageFile
    = ImageFile
      { imageFile :: File
      , imageFileType :: ImageType
      , imageFileWidth :: Int
      , imageFileHeight :: Int
      , imageFileMaxVal :: Int
      } deriving (Show, Read, Eq, Ord, Data, Typeable)

data ImageType = PPM | JPEG | GIF | PNG deriving (Show, Read, Eq, Ord, Typeable, Data)

instance PixmapShape ImageFile where
    pixmapHeight = imageFileHeight
    pixmapWidth = imageFileWidth
    pixmapMaxVal = imageFileMaxVal

instance Pretty ImageFile where
    pPrint (ImageFile f typ w h _mx) = text "ImageFile(" <> pPrint f <> text (" " <> show w <> "x" <> show h <> " " <> show typ <> ")")

extension :: ImageType -> String
extension JPEG = ".jpg"
extension PPM = ".ppm"
extension GIF = ".gif"
extension PNG = ".png"

-- |Return the local pathname of an image file with an appropriate extension (e.g. .jpg).
imageFilePath :: MonadFileCacheTop m => ImageFile -> m FilePath
imageFilePath img = fileCachePath (imageFile img) >>= \ path -> return $ path ++ extension (imageFileType img)

imageFileFromBytes :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) => ByteString -> m ImageFile
imageFileFromBytes bs = fileFromBytes bs >>= makeImageFile

-- | Find or create a cached image file from a URI.
imageFileFromURI :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) => URI -> m ImageFile
imageFileFromURI uri = fileFromURI (uriToString id uri "") >>= makeImageFile . fst

imageFileFromPath :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) => FilePath -> m ImageFile
imageFileFromPath path = fileFromPath path >>= makeImageFile . fst

-- |Create an image file from a 'File'.
makeImageFile :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) => File -> m ImageFile
makeImageFile file = $logException $ do
    -- logM "Appraisal.ImageFile.makeImageFile" INFO ("Appraisal.ImageFile.makeImageFile - INFO file=" ++ show file) >>
    path <- fileCachePath file
    (getFileType path >>= $logException . imageFileFromType path file) `catchError` handle
    where
      handle :: (MonadCatch m, MonadError IOException m, MonadIO m) => IOError -> m ImageFile
      handle e =
          $logException $ fail $ "Failure making image file " ++ show file ++ ": " ++ show e

imageFileFromType :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m) => FilePath -> File -> ImageType -> m ImageFile
imageFileFromType path file typ = do
  -- logM "Appraisal.ImageFile.imageFileFromType" DEBUG ("Appraisal.ImageFile.imageFileFromType - typ=" ++ show typ) >>
  let cmd = case typ of
              JPEG -> pipe [proc "/usr/bin/jpegtopnm" [path], proc "/usr/bin/pnmfile" []]
              PPM ->  (proc "/usr/bin/pnmfile" [])
              GIF -> pipe [proc "/usr/bin/giftopnm" [path], proc "/usr/bin/pnmfile" []]
              PNG -> pipe [proc "/usr/bin/pngtopnm" [path], proc "/usr/bin/pnmfile" []]
  -- err may contain "Output file write error --- out of disk space?"
  -- because pnmfile closes the output descriptor of the decoder
  -- process early.  This can be ignored.
  (code, out, _err) <- liftIO $ readCreateProcessWithExitCode cmd P.empty
  case code of
    ExitSuccess -> imageFileFromPnmfileOutput file typ out
    ExitFailure _ -> error $ "Failure building image file:\n " ++ showCmdSpec (cmdspec cmd) ++ " -> " ++ show code

imageFileFromPnmfileOutput :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m) => File -> ImageType -> P.ByteString -> m ImageFile
imageFileFromPnmfileOutput file typ out =
        case matchRegex pnmFileRegex (P.toString out) of
          Just [width, height, _, maxval] ->
            ensureExtensionLink file (extension typ) >>=
            (const . return $ ImageFile { imageFile = file
                                        , imageFileType = typ
                                        , imageFileWidth = read width
                                        , imageFileHeight = read height
                                        , imageFileMaxVal = if maxval == "" then 1 else read maxval })
          _ -> error $ "Unexpected output from pnmfile: " ++ show out
  where
      pnmFileRegex = mkRegex "^stdin:\tP[PGB]M raw, ([0-9]+) by ([0-9]+)([ ]+maxval ([0-9]+))?$"

ensureExtensionLink :: (MonadFileCacheTop m, MonadError IOException m, MonadIO m) => File -> String -> m ()
ensureExtensionLink file ext = fileCachePath file >>= \ path -> liftIO $ ensureLink (fileChksum file) (path ++ ext)

-- |Run @file -b@ and convert the output to an 'ImageType'.
getFileType :: (MonadCatch m, MonadError IOException m, MonadIO m) => FilePath -> m ImageType
getFileType path =
    liftIO (readProcessWithExitCode cmd args P.empty) `catchError` err >>= return . test . (\ (_, out, _) -> out)
    where
      cmd = "file"
      args = ["-b", path]
      err (e :: IOError) =
          $logException $ fail ("getFileType Failure: " ++ showCommandForUser cmd args ++ " -> " ++ show e)
      test :: P.ByteString -> ImageType
      test s = maybe (error $ "ImageFile.getFileType - Not an image: " ++ path ++ "(Ident string: " ++ show s ++ ")") id (foldr (testre (P.toString s)) Nothing tests)
      testre _ _ (Just result) = Just result
      testre s (re, typ) Nothing = maybe Nothing (const (Just typ)) (matchRegex re s)
      -- Any more?
      tests = [(mkRegex "Netpbm P[BGPP]M \"rawbits\" image data$", PPM)
              ,(mkRegex "JPEG image data", JPEG)
              ,(mkRegex "PNG image data", PNG)
              ,(mkRegex "GIF image data", GIF)]

-- |Return the area of an image in square pixels.
imageFileArea :: ImageFile -> Int
imageFileArea image = imageFileWidth image * imageFileHeight image

-- | Build a version of the image with its orientation fixed based on
-- the EXIF orientation flag.  If the image is already upright it will
-- return the original ImageFile.
uprightImage :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) => ImageFile -> m ImageFile
uprightImage orig = do
  -- path <- fileCachePath (imageFile orig)
  bs <- $logException $ loadBytes (imageFile orig)
  bs' <- $logException $ liftIO (normalizeOrientationCode (P.fromStrict bs))
  maybe (return orig) (\ bs'' -> $logException (fileFromBytes (P.toStrict bs'')) >>= makeImageFile) bs'

-- |Use a decoder, pnmscale, and an encoder to change the size of an
-- |image file.  The new image inherits the home directory of the old.
scaleImage :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m, Functor m, RealFloat f) => f -> ImageFile -> m ImageFile
scaleImage scale orig | scale == 1.0 = return orig
scaleImage scale orig = $logException $ do
    path <- fileCachePath (imageFile orig)
    let decoder = case imageFileType orig of
                    JPEG -> showCommandForUser "/usr/bin/jpegtopnm" [path]
                    PPM -> showCommandForUser "cat" [path]
                    GIF -> showCommandForUser "/usr/bin/giftopnm" [path]
                    PNG -> showCommandForUser "/usr/bin/pngtopnm" [path]
        scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
        -- Probably we should always build a png here rather than
        -- whatever the original file type was?
        encoder = case imageFileType orig of
                    JPEG -> showCommandForUser "/usr/bin/cjpeg" []
                    PPM -> showCommandForUser "cat" []
                    GIF -> showCommandForUser "ppmtogif" []
                    PNG -> showCommandForUser "pnmtopng" []
        cmd = pipe' [decoder, scaler, encoder]
    -- fileFromCmd ver cmd >>= buildImage
    fileFromCmdViaTemp cmd >>= buildImage
    where
      buildImage :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) => File -> m ImageFile
      buildImage file = makeImageFile file `catchError` (\ e -> fail $ "scaleImage - makeImageFile failed: " ++ show e)

-- |Crop an image.
editImage :: (MonadCatch m, MonadFileCacheTop m, MonadError IOException m, MonadIO m) => ImageCrop -> ImageFile -> m ImageFile
editImage crop file = $logException $
    case commands of
      [] ->
          return file
      _ ->
          (loadBytes (imageFile file) >>=
           liftIO . pipeline commands >>=
           fileFromBytes >>=
           makeImageFile) `catchError` err
    where
      commands = buildPipeline (imageFileType file) [cut, rotate] (latexImageFileType (imageFileType file))
      -- We can only embed JPEG and PNG images in a LaTeX
      -- includegraphics command, so here we choose which one to use.
      latexImageFileType GIF = PNG
      latexImageFileType PPM = PNG
      latexImageFileType JPEG = JPEG
      latexImageFileType PNG = PNG
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
      convert JPEG PPM = [proc "/usr/bin/jpegtopnm" []]
      convert GIF PPM = [proc "giftpnm" []]
      convert PNG PPM = [proc "/usr/bin/pngtopnm" []]
      convert PPM JPEG = [proc "/usr/bin/cjpeg" []]
      convert PPM GIF = [proc "ppmtogif" []]
      convert PPM PNG = [proc "pnmtopng" []]
      convert PNG x = proc "pngtopnm" [] : convert PPM x
      convert GIF x = proc "/usr/bin/giftopnm" [] : convert PPM x
      convert a b | a == b = []
      convert a b = error $ "Unknown conversion: " ++ show a ++ " -> " ++ show b
      err e = $logException $ fail $ "editImage Failure: file=" ++ show file ++ ", error=" ++ show e

pipeline :: [CreateProcess] -> P.ByteString -> IO P.ByteString
pipeline [] bytes = return bytes
pipeline (p : ps) bytes =
    (readCreateProcessWithExitCode p bytes >>= doResult)
      `catch` (\ (e :: SomeException) -> doException (showCreateProcessForUser p ++ " -> " ++ show e) e)
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

instance Show StdStream where
    show _ = "<StdStream>"

deriving instance Show CmdSpec
deriving instance Show CreateProcess

pipe :: [CreateProcess] -> CreateProcess
pipe xs = foldl1 pipe2 xs

pipe2 :: CreateProcess -> CreateProcess -> CreateProcess
pipe2 a b =
    if cwd a == cwd b &&
       env a == env b &&
       close_fds a == close_fds b &&
       create_group a == create_group b
    then a {cmdspec = ShellCommand (showCmdSpec (cmdspec a) ++ " | " ++ showCmdSpec (cmdspec b))}
    else error $ "Pipeline of incompatible commands: " ++ show (a, b)

pipe' :: [String] -> String
pipe' = intercalate " | "

$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''ImageFile)
