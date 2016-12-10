module Cache where

-- | Load an image cache state

import Appraisal.ImageCache
import Control.Exception (bracket)
import Data.Acid (openLocalStateFrom, closeAcidState)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import System.FilePath ((</>))
import Test.HUnit
import Text.LaTeX (render)

loadImageCache :: FilePath -> IO ImageCacheState
loadImageCache top =
    bracket (openLocalStateFrom (top </> "imageCache") (error $ "loadImageCache " ++ top </> "imageCache"))
            closeAcidState $ \ imageCache ->
    return imageCache
