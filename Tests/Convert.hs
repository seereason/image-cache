-- | Convert from old to new image cache format.  Also, remove all
-- derived files.  This will become a function that runs at server
-- startup if it notices the new image directory doesn't exist.

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Appraisal.AcidCache
import Appraisal.FileCache
import Appraisal.Image
import Control.Exception (bracket)
import Control.Lens (view)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Acid (AcidState, {-closeAcidState,-} openLocalStateFrom)
import Data.Acid.Advanced (query', {-update',-} groupUpdates)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Either
import Data.Map as Map (Map, toList)
import System.Exit (exitWith, ExitCode(ExitSuccess))
-- import System.Unix.FilePath ((<++>))
import Data.Time (getCurrentTime, diffUTCTime, getCurrentTime, NominalDiffTime)

db :: FilePath
db = "/srv/appraisalscribe-development/_state/imageCache"

files :: FileCacheTop
files = FileCacheTop "/srv/appraisalscribe-development/images"

main :: IO ()
main =
  bracket (openLocalStateFrom db (error $ "loadImageCache " ++ db)) createCheckpointAndClose $ \cache ->
  timeComputation (go cache) >>= \(_, t) ->
  putStrLn ("Elapsed: " ++ show t) >>
  exitWith ExitSuccess

go :: AcidState (Map ImageKey ImageFile) -> IO ()
go cache = runFileCacheIO cache files $ do
  mp <- query' cache LookMap
  events <- concat <$> mapM convert (Map.toList mp)
  let (dels, puts) = partitionEithers events
  liftIO $ groupUpdates cache dels -- dels first or they may clobber subsequent puts
  liftIO $ groupUpdates cache puts

convert :: MonadFileCacheIO m => (ImageKey, ImageFile) -> m [Either (DeleteValues ImageKey ImageFile) (PutValue ImageKey ImageFile)]
convert (key@(ImageOriginal img), img')
    | img /= img' =
        error $ "Expected equal ImageFiles:\n  " ++ show img ++ "\n  " ++ show img'
    | otherwise = do
        oldPath <- oldFileCachePath (view imageFile img)
        newFile <- fileFromPathViaCopy (fileExtension (view imageFileType img)) oldPath
        let newImage = img {_imageFile = newFile}
        return $ [Left (DeleteValues [key]), Right (PutValue (ImageOriginal newImage) newImage)]
convert (key, _img) = return $ [Left (DeleteValues [key])]

-- | Perform an IO operation and return the elapsed time along with the result.
timeComputation :: MonadIO m => m r -> m (r, NominalDiffTime)
timeComputation a = do
  !start <- liftIO getCurrentTime
  !r <- a
  !end <- liftIO getCurrentTime
  return (r, diffUTCTime end start)
