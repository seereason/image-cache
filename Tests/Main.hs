{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Appraisal.AcidCache
import Appraisal.FileCache
import Appraisal.FileCacheT
import Control.Exception (IOException)
import Control.Monad.Reader (ask, ReaderT)
import Control.Monad.Trans (lift)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Acid (AcidState)
import Data.ByteString (ByteString, pack)
import Data.Char (ord)
import Data.Map (fromList, Map)
import qualified Exif (tests)
import qualified LaTeX
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import System.FilePath.Extra3 (removeRecursiveSafely)
import Test.HUnit (assertEqual, Test(TestList, TestCase), runTestTT, Counts(errors, failures))

main =
    do counts <- runTestTT Main.tests
       putStrLn ("Test results: " ++ show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure 1)
       putStrLn (unlines ["Other Things that should be tested:",
                          "FileCache: fileFromPath, fileFromCmd, fileFromCmdViaTemp, fileFromURI, fileURI, fileCacheURI",
                          "ImageCache: getFileType, uprightImage, scaleImage, editImage"
                         ])

tests :: Test
tests = TestList [Exif.tests, LaTeX.tests, acid1, file1]

-- The directory that holds the acid state event logs and checkpoints.
acidDir = "Tests/acid"
fileAcidDir = "Tests/fileacid"
fileCacheDir' = FileCacheTop "Tests/filecache"

-- The oldest file in /usr/share/doc
oldfile :: FilePath
oldfile = "/usr/share/doc/cron/THANKS"

type AcidM = ReaderT (AcidState (Map String String)) IO
type FileM = FileCacheT (AcidState (Map String String)) IOException IO

-- | A simple cache - its builder simply reverses the key.  The
-- IO monad is required to query and update the acid state database.
instance MonadCache String String AcidM where
    askAcidState = ask
    build = return . reverse

instance MonadCache String String m => MonadCache String String (ReaderT FilePath m) where
    askAcidState = lift askAcidState
    build = lift . build

acid1 :: Test
acid1 = TestCase $ do
          removeRecursiveSafely acidDir
          value1 <- withValueCache acidDir (runMonadCacheT (cacheLook "Hello, world!" :: AcidM (Maybe String)))
          value2 <- withValueCache acidDir (runMonadCacheT (cacheMap :: AcidM (Map String String)))
          value3 <- withValueCache acidDir (runMonadCacheT (cacheInsert "Hello, world!" :: AcidM String))
          value4 <- withValueCache acidDir (runMonadCacheT (cacheLook "Hello, world!" :: AcidM (Maybe String)))
          value5 <- withValueCache acidDir (runMonadCacheT (cacheMap :: AcidM (Map String String)))
          assertEqual "acid1"
                          (Nothing, fromList [], "!dlrow ,olleH", Just "!dlrow ,olleH", fromList [("Hello, world!","!dlrow ,olleH")])
                          (value1, value2, value3, value4, value5)

-- runEitherT :: EitherT e m a -> m (Either e a)

file1 :: Test
file1 = TestCase $ do
          removeRecursiveSafely fileAcidDir
          Right value1 <- either Left (either Left Right) <$> withValueCache fileAcidDir (runExceptT . f) :: IO (Either FileError (File, ByteString))
          assertEqual "file1" expected value1
    where
      f :: AcidState (Map String String) -> ExceptT FileError IO (Either FileError (File, ByteString))
      f fileAcidState =
          runFileCacheT fileAcidState fileCacheDir'
            (fileFromPath return (pure "") oldfile :: FileCacheT st FileError (ExceptT FileError IO) (File, ByteString))
             {-liftIO (try (fileFromPath return (pure "") oldfile) >>= either (throwError . IOException) return)-}
      expected :: (File, ByteString)
      expected = (File {_fileSource = Just (ThePath "/usr/share/doc/cron/THANKS"),
                         _fileChksum = "8f57348732b9755b264ef1c15b0e6485",
                         _fileMessages = [],
                         _fileExt = "" },
                  (pack $ map (fromIntegral . ord) $ unlines
                   ["15 January 1990",
                    "Paul Vixie",
                    "",
                    "Many people have contributed to cron.  Many more than I can remember, in fact.",
                    "Rich Salz and Carl Gutekunst were each of enormous help to me in V1; Carl for",
                    "helping me understand UNIX well enough to write it, and Rich for helping me",
                    "get the features right.",
                    "",
                    "John Gilmore wrote me a wonderful review of V2, which took me a whole year to",
                    "answer even though it made me clean up some really awful things in the code.",
                    "(According to John the most awful things are still in here, of course.)",
                    "",
                    "Paul Close made a suggestion which led to /etc/crond.pid and the mutex locking",
                    "on it.  Kevin Braunsdorf of Purdue made a suggestion that led to @reboot and",
                    "its brothers and sisters; he also sent some diffs that lead cron toward compil-",
                    "ability with System V, though without at(1) capabilities, this cron isn't going",
                    "to be that useful on System V.  Bob Alverson fixed a silly bug in the line",
                    "number counting.  Brian Reid made suggestions which led to the run queue and",
                    "the source-file labelling in installed crontabs.",
                    "",
                    "Scott Narveson ported V2 to a Sequent, and sent in the most useful single batch",
                    "of diffs I got from anybody.  Changes attributable to Scott are:",
                    "\t-> sendmail won't time out if the command is slow to generate output",
                    "\t-> day-of-week names aren't off by one anymore",
                    "\t-> crontab says the right thing if you do something you shouldn't do",
                    "\t-> crontab(5) man page is longer and more informative",
                    "\t-> misc changes related to the side effects of fclose()",
                    "\t-> Sequent \"universe\" support added (may also help on Pyramids)",
                    "\t-> null pw_shell is dealt with now; default is /bin/sh"]))
