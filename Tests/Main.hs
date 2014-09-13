module Main where

import Appraisal.Utils.ErrorWithIO (ErrorWithIO)
import Cache (loadImageCache)
import qualified Exif (tests)
import qualified LaTeX
import System.Exit (exitWith, ExitCode(ExitSuccess, ExitFailure))
import Test.HUnit (Test(TestList), runTestTT, Counts(errors, failures))

main =
    do counts <- runTestTT Main.tests
       putStrLn ("Test results: " ++ show counts)
       case (errors counts + failures counts) of
         0 -> exitWith ExitSuccess
         _ -> exitWith (ExitFailure 1)

tests :: Test
tests = TestList [Exif.tests, LaTeX.tests]
