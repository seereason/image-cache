-- | Convert from old to new image cache format.  Also, remove all
-- derived files.  This will become a function that runs at server
-- startup if it notices the new image directory doesn't exist.

{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS -Wall #-}

import Control.Monad (MonadPlus, msum, when)
import Data.Generics (Data, Typeable, listify)

import Appraisal.AcidCache
import Appraisal.FileCache
import Appraisal.Image
import AppraisalDef (AppraisalData(..), ReportMap(..))
import AppraisalData (QueryAppraisalData(..))
import Control.Exception (bracket)
import Control.Lens (view)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Acid (AcidState, {-closeAcidState,-} openLocalStateFrom)
import Data.Acid.Advanced (query', {-update',-} groupUpdates)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Either
import Data.Map as Map (elems, Map, toList)
import Data.Set as Set (delete, difference, fromList, insert, member, Set, toList)
import Text.PrettyPrint.HughesPJClass (prettyShow)
import System.Directory (getDirectoryContents)
import System.Exit (exitWith, ExitCode(ExitSuccess))
import System.FilePath (dropExtension)
import Data.Time (getCurrentTime, diffUTCTime, getCurrentTime, NominalDiffTime)

db :: FilePath
db = "/srv/appraisalscribe-development/_state/imageCache"

adb :: FilePath
adb = "/srv/appraisalscribe-development/_state/appraisalData"

files :: FileCacheTop
files = FileCacheTop "/srv/appraisalscribe-development/images"

main :: IO ()
main =
  bracket (openLocalStateFrom db (error $ "loadImageCache " ++ db)) createCheckpointAndClose $ \cache ->
  bracket (openLocalStateFrom adb (error $ "loadImageCache " ++ adb)) createCheckpointAndClose $ \appraisals ->
  timeComputation ({-convert-} dump cache files appraisals) >>= \(_, t) ->
  putStrLn ("Elapsed: " ++ show t) >>
  exitWith ExitSuccess

convert :: AcidState (Map ImageKey ImageFile) -> IO ()
convert cache = runFileCacheIO cache files $ do
  mp <- query' cache LookMap
  events <- concat <$> mapM convert1 (Map.toList mp)
  let (dels, puts) = partitionEithers events
  liftIO $ groupUpdates cache dels -- dels first or they may clobber subsequent puts
  liftIO $ groupUpdates cache puts

dump :: AcidState (Map ImageKey ImageFile) -> FileCacheTop -> AcidState AppraisalData -> IO ()
dump cache top appraisals = runFileCacheIO cache top $ do
  appraisals' <- liftIO $ query' appraisals QueryAppraisalData
  let (usedImgs :: Set ImageFile) = Set.fromList (gFind (_unReportMap (_reportMap appraisals'), _unReportMap (_trashCan appraisals')) :: [ImageFile])
  imagefiles <- liftIO $ (Set.delete "." . Set.delete ".." . Set.fromList) <$> getDirectoryContents (unFileCacheTop top)
  -- mapM_ (\name -> liftIO $ putStrLn $ "File: " ++ name) (Set.toList imagefiles)
  let imagefiles' = foldr (addRegular imagefiles) mempty (Set.toList imagefiles)
  mp <- query' cache LookMap
  let unlisted = Set.toList $ Set.difference imagefiles' (Set.fromList (fmap (view (imageFile . fileChksum)) (Map.elems mp)))
  mapM_ (\name -> liftIO $ putStrLn $ "Unlisted: " ++ name) unlisted
  -- mapM_ (\name -> liftIO $ putStrLn $ "Entries: " ++ name) unlisted
  mapM_ (\(key, img) -> liftIO $ putStrLn $ prettyShow key ++ " -> " ++ prettyShow img) (Map.toList mp)
    where
      addRegular images file r =
          let stripped = dropExtension file in
          if stripped /= file && Set.member stripped images then r else Set.insert file r

convert1 :: MonadFileCacheIO m => (ImageKey, ImageFile) -> m [Either (DeleteValues ImageKey ImageFile) (PutValue ImageKey ImageFile)]
convert1 (key@(ImageOriginal img), img')
    | img /= img' =
        error $ "Expected equal ImageFiles:\n  " ++ show img ++ "\n  " ++ show img'
    | otherwise = do
        oldPath <- oldFileCachePath (view imageFile img)
        newFile <- fileFromPathViaCopy (fileExtension (view imageFileType img)) oldPath
        let newImage = img {_imageFile = newFile}
        return $ [Left (DeleteValues [key]), Right (PutValue (ImageOriginal newImage) newImage)]
convert1 (key, img) = do
  liftIO $ putStrLn $ "derived: " ++ show key ++ " -> " ++ show (filePath (view imageFile img))
  return $ [Left (DeleteValues [key])]

-- | Perform an IO operation and return the elapsed time along with the result.
timeComputation :: MonadIO m => m r -> m (r, NominalDiffTime)
timeComputation a = do
  !start <- liftIO getCurrentTime
  !r <- a
  !end <- liftIO getCurrentTime
  return (r, diffUTCTime end start)


gFind :: (MonadPlus m, Data a, Typeable b) => a -> m b
gFind = msum . map return . listify (const True)
