module Cache where

-- | Load an image cache state

import Appraisal.File (ImageCacheTop)
import Appraisal.ImageCache (ImageCacheIO, ImageCacheMap, ImageCacheState, runImageCacheIO, fileCachePath')
import Appraisal.Utils.UUID (fromString)
import Control.Exception (bracket)
import Control.Monad.Error
import Data.Acid (openLocalStateFrom, closeAcidState)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import System.FilePath ((</>))
import Test.HUnit
import Text.LaTeX (render)

loadImageCache :: FilePath -> ImageCacheTop -> IO ImageCacheState
loadImageCache top p =
    bracket (openLocalStateFrom (top </> "imageCache") (error $ "loadImageCache " ++ top </> "imageCache"))
            closeAcidState $ \ imageCache ->
    return imageCache
