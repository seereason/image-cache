{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main (main) where

import Data.FileCache
import Control.Exception (bracket, IOException, SomeException)
import Control.Lens (over, _Left)
import Control.Monad.Except (ExceptT, runExceptT)
import Data.Acid (AcidState, openLocalStateFrom, closeAcidState)
import Data.Acid.Abstract (query')
import Data.FileCache.Acid (LookMap(LookMap))
import Data.FileCache.CacheMap (CacheMap(CacheMap, _unCacheMap, _requested))
import Data.FileCache.FileInfo (fileInfoFromBytes)
import Data.FileCache.ImageKey (ImageShape)
import Data.FileCache.Test (tests)
import Data.Map (size)
import Extra.Exceptionless (Exceptionless, runExceptionless)
import qualified LaTeX
import SeeReason.Errors as Err (catchMember, throwMember, OneOf)
import System.Exit (exitSuccess, exitFailure)
import System.FilePath ((</>))
import Test.HUnit (assertEqual, Test(TestList, TestCase), runTestTT, Counts(errors, failures))
import Data.FileCache.Server (makeByteString)

main =
  bracket
    (openLocalStateFrom
      (top </> "imageCache")
      (error $ "Could not open " <> top </> "imageCache"))
    closeAcidState
    (\acid -> runTestTTAndExit $
      TestList [LaTeX.tests,
                Data.FileCache.Test.tests,
                imageTests acid])
  where
    top = "/home/dsf/appraisalscribe3-development/_state"

runTestTTAndExit :: Test -> IO ()
runTestTTAndExit test = do
  c <- runTestTT test
  if (errors c == 0) && (failures c == 0)
    then exitSuccess
    else exitFailure

-- The directory that holds the acid state event logs and checkpoints.
-- acidDir = "Tests/acid"
-- fileAcidDir = "Tests/fileacid"
-- fileCacheDir' = FileCacheTop "Tests/filecache"

-- The oldest file in /usr/share/doc
-- oldfile :: FilePath
-- oldfile = "/usr/share/doc/cron/THANKS"

-- type AcidM m = RWST (AcidState CacheMap) () () m
-- type FileM m = {-FileCacheT (AcidState CacheMap)-} Monad m

-- | A simple cache - its builder simply reverses the key.  The
-- IO monad is required to query and update the acid state database.
{-
instance (MonadIO m, MonadCatch m, MonadError FileError m) => HasCacheAcid (AcidM m ()) where
    cacheAcid = ask
    -- buildCacheValue = return . Cached . reverse

instance HasCacheAcid (m :: * -> *) => HasCacheAcid (ReaderT FilePath m) where
    cacheAcid = lift cacheAcid

runMonadCacheT ::
    Monad m
    => RWST (AcidState (CacheMap key val err)) () s m a
    -> s
    -> AcidState (CacheMap key val err)
    -> m a
runMonadCacheT action s acid = fst <$> evalRWST action acid s

acid1 :: Test
acid1 = TestCase $ do
          removeRecursiveSafely acidDir
          (value1 :: Either FileError (Maybe (String))) <- runExceptT $ withCache acidDir (runMonadCacheT (cacheLook "Hello, world!" :: AcidM (ExceptT FileError IO) (Maybe (String))) ())
          (value2 :: Either FileError (CacheMap)) <- runExceptT $ withCache acidDir (runMonadCacheT (cacheMap :: AcidM (ExceptT FileError IO) (CacheMap)) ())
          value3 <- runExceptT $ withCache acidDir (runMonadCacheT (cacheInsert "Hello, world!" :: AcidM (ExceptT FileError IO) (String)) ())
          value4 <- runExceptT $ withCache acidDir (runMonadCacheT (cacheLook "Hello, world!" :: AcidM (ExceptT FileError IO) (Maybe (String))) ())
          value5 <- runExceptT $ withCache acidDir (runMonadCacheT (cacheMap :: AcidM (ExceptT FileError IO) (CacheMap)) ())
          assertEqual "acid1"
                          (Right Nothing,
                           Right (CacheMap (fromList [])),
                           Right (Cached "!dlrow ,olleH"),
                           Right (Just (Cached "!dlrow ,olleH")),
                           Right (CacheMap (fromList [("Hello, world!",Cached "!dlrow ,olleH")])))
                          (value1, value2, value3, value4, value5)

-- runEitherT :: EitherT e m a -> m (Either e a)

file1 :: Test
file1 = TestCase $ do
          removeRecursiveSafely fileAcidDir
          Right value1 <- runExceptT (withCache fileAcidDir f) :: IO (Either FileError (File, ByteString))
          assertEqual "file1" expected value1
    where
      f :: AcidState (CacheMap String String FileError) -> ExceptT FileError IO (File, ByteString)
      f fileAcidState =
          runFileCacheT Proxy () fileAcidState fileCacheDir'
            (fileFromPath return (pure "") oldfile :: FileCacheT st () () (ExceptT FileError IO) (File, ByteString))
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
-}

imageTests :: AcidState CacheMap -> Test
imageTests acid =
  TestList
    [ test1
    , TestCase $ do
        CacheMap{..} <- query' acid LookMap
        assertEqual "map size" 81877 (size _unCacheMap)
    ]
  where
    -- foo :: Either SomeException (Either SomeException ImageShape) -> Either SomeException ImageShape
    -- foo = either Left (either Left Right)
    -- handle :: SomeException -> IO (Either SomeException ImageShape)
    -- handle e = undefined

type ES = OneOf '[IOException, FileError, SomeException]

test1 :: Test
test1 = TestCase $ do
  (shape :: Either String ImageShape) <- over _Left show <$> runExceptT action2
  assertEqual "fileInfoFromBytes" (Right (ImageShape PDF (Left "PDF"))) shape
  where
    action2 :: ExceptT ES IO ImageShape
    action2 = runExceptionless throwMember action
    action :: Exceptionless (ExceptT ES IO) ImageShape
    action = catchMember (makeByteString pdf) (\(e :: IOException) -> throwMember e) >>= fileInfoFromBytes
    pdf :: FilePath
    pdf = "/home/dsf/git/happstack-ghcjs.alpha/happstack-ghcjs-server/test-top/images/fb/fbddca395b0912cdfa710f84ab09f317.pdf"
