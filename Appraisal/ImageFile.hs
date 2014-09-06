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

import Appraisal.Config (Paths)
import Appraisal.Exif (normalizeOrientationCode)
import Appraisal.File (File(..), fileCachePath, loadBytes, fileFromBytes, fileFromPath, fileFromURI, {-fileFromFile, fileFromCmd,-} fileFromCmdViaTemp)
import Appraisal.Image (PixmapShape(..), ImageCrop(..))
import Appraisal.Utils.ErrorWithIO (ErrorWithIO, io, catch, logExceptionM, ensureLink, readCreateProcess')
import Appraisal.Utils.Prelude
import Appraisal.Utils.Pretty
import Data.ByteString (ByteString)
import Data.Generics (Data(..), Typeable)
import Data.List (intercalate)
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
import System.Process (CreateProcess(..), CmdSpec(..), proc, showCommandForUser, StdStream)
import System.Process.ListLike (readCreateProcessWithExitCode, readCreateProcess)
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

instance Pretty Doc ImageFile where
    pretty (ImageFile f typ w h _mx) = text "ImageFile(" <> pretty f <> text (" " <> show w <> "x" <> show h <> " " <> show typ <> ")")

extension :: ImageType -> String
extension JPEG = ".jpg"
extension PPM = ".ppm"
extension GIF = ".gif"
extension PNG = ".png"

-- |Return the local pathname of an image file with an appropriate extension (e.g. .jpg).
imageFilePath :: Paths a => a -> ImageFile -> FilePath
imageFilePath ver img = fileCachePath ver (imageFile img) ++ extension (imageFileType img)

imageFileFromBytes :: Paths p => p -> ByteString -> ErrorWithIO ImageFile
imageFileFromBytes top bs = fileFromBytes top bs >>= makeImageFile top

-- | Find or create a cached image file from a URI.
imageFileFromURI :: Paths p => p -> URI -> ErrorWithIO ImageFile
imageFileFromURI top uri = fileFromURI top (uriToString id uri "") >>= makeImageFile top . fst

imageFileFromPath :: Paths p => p -> FilePath -> ErrorWithIO ImageFile
imageFileFromPath top path = fileFromPath top path >>= makeImageFile top . fst

-- |Create an image file from a 'File'.
makeImageFile :: Paths a => a -> File -> ErrorWithIO ImageFile
makeImageFile ver file = logExceptionM "Appraisal.ImageFile.makeImageFile" $ do
    -- logM "Appraisal.ImageFile.makeImageFile" INFO ("Appraisal.ImageFile.makeImageFile - INFO file=" ++ show file) >>
    (getFileType path >>= imageFileFromType ver path file) `catch` handle
    where
      path = fileCachePath ver file
      handle :: IOError -> ErrorWithIO ImageFile
      handle e =
          logExceptionM "Appraisal.ImageFile.makeImageFile" $ fail $ "Failure making image file " ++ show file ++ ": " ++ show e

imageFileFromType :: Paths p => p -> FilePath -> File -> ImageType -> ErrorWithIO ImageFile
imageFileFromType ver path file typ = do
  -- logM "Appraisal.ImageFile.imageFileFromType" DEBUG ("Appraisal.ImageFile.imageFileFromType - typ=" ++ show typ) >>
  let cmd = case typ of
              JPEG -> pipe [proc "/usr/bin/djpeg" [path], proc "/usr/bin/pnmfile" []]
              PPM ->  (proc "/usr/bin/pnmfile" [])
              GIF -> pipe [proc "/usr/bin/giftopnm" [path], proc "/usr/bin/pnmfile" []]
              PNG -> pipe [proc "/usr/bin/pngtopnm" [path], proc "/usr/bin/pnmfile" []]
  -- err may contain "Output file write error --- out of disk space?"
  -- because pnmfile closes the output descriptor of the decoder
  -- process early.  This can be ignored.
  (code, out, _err) <- io $ readCreateProcessWithExitCode cmd P.empty
  case code of
    ExitSuccess -> imageFileFromPnmfileOutput ver file typ out
    ExitFailure _ -> myerror $ "Failure building image file:\n " ++ showCmdSpec (cmdspec cmd) ++ " -> " ++ show code

imageFileFromPnmfileOutput :: Paths p => p -> File -> ImageType -> P.ByteString -> ErrorWithIO ImageFile
imageFileFromPnmfileOutput ver file typ out =
        case matchRegex pnmFileRegex (P.toString out) of
          Just [width, height, _, maxval] ->
            ensureExtensionLink ver file (extension typ) >>=
            (const . return $ ImageFile { imageFile = file
                                        , imageFileType = typ
                                        , imageFileWidth = read width
                                        , imageFileHeight = read height
                                        , imageFileMaxVal = if maxval == "" then 1 else read maxval })
          _ -> myerror $ "Unexpected output from pnmfile: " ++ show out
  where
      pnmFileRegex = mkRegex "^stdin:\tP[PGB]M raw, ([0-9]+) by ([0-9]+)([ ]+maxval ([0-9]+))?$"

ensureExtensionLink :: Paths p => p -> File -> String -> ErrorWithIO ()
ensureExtensionLink ver file ext = ensureLink (fileChksum file) (fileCachePath ver file ++ ext)

-- |Run @file -b@ and convert the output to an 'ImageType'.
getFileType :: FilePath -> ErrorWithIO ImageType
getFileType path =
    io (readCreateProcess (proc cmd args) P.empty) `catch` err >>= return . test
    where
      cmd = "file"
      args = ["-b", path]
      err (e :: IOError) =
          logExceptionM "Appraisal.ImageFile.getFileType" $ fail ("getFileType Failure: " ++ showCommandForUser cmd args ++ " -> " ++ show e)
      test :: P.ByteString -> ImageType
      test s = maybe (myerror $ "ImageFile.getFileType - Not an image: " ++ path ++ "(Ident string: " ++ show s ++ ")") id (foldr (testre (P.toString s)) Nothing tests)
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
uprightImage :: Paths a => a -> ImageFile -> ErrorWithIO ImageFile
uprightImage ver orig = do
  bs <- loadBytes ver (imageFile orig)
  bs' <- io (normalizeOrientationCode (P.fromStrict bs))
  maybe (return orig) (\ bs'' -> fileFromBytes ver (P.toStrict bs'') >>= makeImageFile ver) bs'

-- |Use a decoder, pnmscale, and an encoder to change the size of an
-- |image file.  The new image inherits the home directory of the old.
scaleImage :: (Paths a, RealFloat f) => f -> a -> ImageFile -> ErrorWithIO ImageFile
scaleImage scale _ orig | scale == 1.0 = return orig
scaleImage scale ver orig = logExceptionM "Appraisal.ImageFile.scaleImage" $ do
    let cmd = pipe' [decoder, scaler, encoder]
    -- fileFromCmd ver cmd >>= buildImage
    fileFromCmdViaTemp ver cmd >>= buildImage
    where
      decoder = case imageFileType orig of
                  JPEG -> showCommandForUser "/usr/bin/djpeg" [path]
                  PPM -> showCommandForUser "cat" [path]
                  GIF -> showCommandForUser "/usr/bin/giftopnm" [path]
                  PNG -> showCommandForUser "/usr/bin/pngtopnm" [path]
      path = fileCachePath ver (imageFile orig)
      scaler = showCommandForUser "pnmscale" [showFFloat (Just 6) scale ""]
      -- Probably we should always build a png here rather than
      -- whatever the original file type was?
      encoder = case imageFileType orig of
                  JPEG -> showCommandForUser "/usr/bin/cjpeg" []
                  PPM -> showCommandForUser "cat" []
                  GIF -> showCommandForUser "ppmtogif" []
                  PNG -> showCommandForUser "pnmtopng" []
      buildImage :: File -> ErrorWithIO ImageFile
      buildImage file = makeImageFile ver file `catch` (\ e -> fail $ "scaleImage - makeImageFile failed: " ++ show e)

-- |Crop an image.
editImage :: Paths a => ImageCrop -> a -> ImageFile -> ErrorWithIO ImageFile
editImage crop ver file = logExceptionM "Appraisal.ImageFile.editImage" $
    case commands of
      [] ->
          return file
      _ ->
          (loadBytes ver (imageFile file) >>=
           io . pipeline commands >>=
           fileFromBytes ver >>=
           makeImageFile ver) `catch` err
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
              (l, r, t, b) -> Just (PPM, ("pnmcut", ["-left", show l,
                                                     "-right", show (w - r - 1),
                                                     "-top", show t,
                                                     "-bottom", show (h - b - 1)]), PPM)
      rotate = case rotation crop of
                 90 -> Just (JPEG, ("jpegtran", ["-rotate", "90"]), JPEG)
                 180 -> Just (JPEG, ("jpegtran", ["-rotate", "180"]), JPEG)
                 270 -> Just (JPEG, ("jpegtran", ["-rotate", "270"]), JPEG)
                 _ -> Nothing
      w = pixmapWidth file
      h = pixmapHeight file
      buildPipeline :: ImageType -> [Maybe (ImageType, (String, [String]), ImageType)] -> ImageType -> [(String, [String])]
      buildPipeline start [] end = convert start end
      buildPipeline start (Nothing : ops) end = buildPipeline start ops end
      buildPipeline start (Just (a, cmd, b) : ops) end | start == a = cmd : buildPipeline b ops end
      buildPipeline start (Just (a, cmd, b) : ops) end = convert start a ++ buildPipeline a (Just (a, cmd, b) : ops) end
      convert JPEG PPM = [("/usr/bin/djpeg", [])]
      convert GIF PPM = [("giftpnm", [])]
      convert PNG PPM = [("/usr/bin/pngtopnm", [])]
      convert PPM JPEG = [("/usr/bin/cjpeg", [])]
      convert PPM GIF = [("ppmtogif", [])]
      convert PPM PNG = [("pnmtopng", [])]
      convert PNG x = [("pngtopnm", [])] ++ convert PPM x
      convert GIF x = [("/usr/bin/giftopnm", [])] ++ convert PPM x
      convert a b | a == b = []
      convert a b = myerror $ "Unknown conversion: " ++ show a ++ " -> " ++ show b
      err e = logExceptionM "Appraisal.ImageFile.editImage" $ fail $ "editImage Failure: file=" ++ show file ++ ", error=" ++ show e

pipeline :: [(String, [String])] -> P.ByteString -> IO P.ByteString
pipeline [] bytes = return bytes
pipeline ((cmd, args) : rest) bytes = readCreateProcess' (proc cmd args) bytes >>= pipeline rest

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
    else myerror $ "Pipeline of incompatible commands: " ++ show (a, b)

pipe' :: [String] -> String
pipe' = intercalate " | "

$(deriveSafeCopy 0 'base ''ImageType)
$(deriveSafeCopy 0 'base ''ImageFile)
