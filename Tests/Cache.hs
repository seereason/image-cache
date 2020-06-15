{-# LANGUAGE FlexibleContexts #-}

module Cache where

-- | Load an image cache state

import Data.FileCache (ImageFile, ImageKey)
import Control.Exception (bracket)
import Data.Acid (AcidState, IsAcidic, openLocalStateFrom, closeAcidState)
import Data.Map (Map)
import System.FilePath ((</>))

loadImageCache :: IsAcidic (Map ImageKey ImageFile) => FilePath -> IO (AcidState (Map ImageKey ImageFile))
loadImageCache top =
    bracket (openLocalStateFrom (top </> "imageCache") (error $ "loadImageCache " ++ top </> "imageCache"))
            closeAcidState $ \ imageCache ->
    return imageCache
