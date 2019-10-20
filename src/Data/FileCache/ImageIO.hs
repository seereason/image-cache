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
-- system to produce.  This is passed to the 'buildCacheValue' method
-- (which may use IO) of 'MonadCache', and if that 'ImageKey' is not
-- already in the cache the desired 'ImageFile' is generated, added to
-- the cache, and returned.

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

module Data.FileCache.ImageIO
  ( imageFileFromType
  ) where

import Control.Exception (IOException, throw)
import Control.Lens (makeLensesFor, view)
import Control.Monad.Catch (try)
import Control.Monad.Except (catchError)
import Control.Monad.Trans (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as P (fromStrict, toStrict)
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString.UTF8 as P
import qualified Data.ByteString as P
#endif
import Data.FileCache.Cache (CacheValue(..), HasFileCacheTop)
import Data.FileCache.Exif (normalizeOrientationCode)
import Data.FileCache.File (File(..))
import Data.FileCache.FileIO (fileCachePath, fileFromBytes, fileFromPath, fileFromURI,
                              fileFromCmd, loadBytesSafe)
import Data.FileCache.FileError (FileError(..), HasFileError, withFileError)
import Data.FileCache.Image (ImageCrop(..), ImageFile(..), ImageType(..), fileExtension, PixmapShape(..), approx)
import Data.FileCache.ImageFile (getFileType)
import Data.Generics.Product (field)
import Data.List (intercalate)
--import Data.Text (pack)
import Extra.Except
import Network.URI (URI, uriToString)
import Numeric (showFFloat)
import System.Exit (ExitCode(..))
import System.Log.Logger (logM, Priority(ERROR))
import System.Process (CreateProcess(..), CmdSpec(..), proc, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode, showCreateProcessForUser)
import "regex-compat-tdfa" Text.Regex (mkRegex, matchRegex)

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
  (code, out, _err) <- readCreateProcessWithExitCode cmd P.empty
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

$(makeLensesFor [("imageFile", "imageFileL")] ''ImageFile)
