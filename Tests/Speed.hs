-- | Test the speed of the File creation function.

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Appraisal.AcidCache
import Appraisal.FileCache
import Appraisal.Image
import Control.Exception (bracket)
import Control.Lens (view)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Acid (AcidState, closeAcidState, openLocalStateFrom)
import Data.Acid.Advanced (query', {-update',-} groupUpdates)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Map as Map (Map, toList)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import Data.Time (getCurrentTime, diffUTCTime, getCurrentTime, NominalDiffTime)

db :: FilePath
db = "/srv/appraisalscribe-development/_state/imageCache"
files :: FileCacheTop
files = FileCacheTop "/srv/appraisalscribe-development/images"

testdb :: FilePath
testdb = "test/db"

testFiles:: FileCacheTop
testFiles = FileCacheTop "test/files"

main :: IO ()
main =
  bracket (openLocalStateFrom db (error $ "loadImageCache " ++ db)) closeAcidState $ \imageCache ->
  bracket (openLocalStateFrom testdb mempty) createCheckpointAndClose $ \testCache ->
  timeComputation (go imageCache testCache) >>= \(_, t) ->
  putStrLn ("Elapsed: " ++ show t) >>
  exitWith ExitSuccess

go :: AcidState (Map ImageKey ImageFile) -> AcidState (Map ImageKey File) -> IO ()
go imageCache testCache = do
  (triples :: [(FilePath, ImageKey, ImageFile)])
      <- runFileCacheIO imageCache files
           (do mp <- query' imageCache LookMap
               let prs = take 1000 $ Map.toList mp
               mapM (\(key, img) -> fileCachePath (view imageFile img) >>= \source -> return (source, key, img)) prs)
  -- let events = map (\(source, key, img) -> PutValue key (view imageFile img)) triples
  events <- runFileCacheIO testCache testFiles $ mapM (\(source, key, img) -> liftIO (copyImage testCache (source, key, img)) >>= \file -> return $ PutValue key file) triples
  groupUpdates testCache events

copyImage :: AcidState (Map ImageKey File) -> (FilePath, ImageKey, ImageFile) -> IO File
copyImage testCache (source, _key,  img) = runFileCacheIO testCache testFiles $ do
  -- fst <$> fileFromPath source -- 16.18s
  fileFromPathViaCopy source -- 6.00s
  -- fileFromPathViaRename source
  -- return $ view imageFile img -- 0.63s

-- | Perform an IO operation and return the elapsed time along with the result.
timeComputation :: MonadIO m => m r -> m (r, NominalDiffTime)
timeComputation a = do
  !start <- liftIO getCurrentTime
  !r <- a
  !end <- liftIO getCurrentTime
  return (r, diffUTCTime end start)
