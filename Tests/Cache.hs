{-# LANGUAGE FlexibleContexts #-}

module Cache where

-- | Load an image cache state

import Appraisal.Image
import Appraisal.ImageCache
import Control.Exception (bracket)
import Data.Acid (AcidState, IsAcidic, openLocalStateFrom, closeAcidState)
import Data.Map (Map)
import Data.Maybe (fromJust)
import Data.Text (unpack)
import System.FilePath ((</>))
import Test.HUnit
import Text.LaTeX (render)

loadImageCache :: IsAcidic (Map ImageKey ImageFile) => FilePath -> IO (AcidState (Map ImageKey ImageFile))
loadImageCache top =
    bracket (openLocalStateFrom (\flag path -> appendFile (path <> "/locktag") ("Appraisal.Cache.loadImageCache " <> show flag <> "\n")) (top </> "imageCache") (error $ "loadImageCache " ++ top </> "imageCache"))
            closeAcidState $ \ imageCache ->
    return imageCache
