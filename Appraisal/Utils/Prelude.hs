{-# LANGUAGE NoImplicitPrelude #-}
module Appraisal.Utils.Prelude
    ( module Prelude
    , myerror
    , logerror
    , uncurry3
    ) where

import GHC.Exception (ErrorCall(..), throw)
import Prelude hiding (error, undefined)
import System.IO.Unsafe (unsafePerformIO)
import System.Log.Logger (logM, Priority(ERROR))

myerror :: String -> a
myerror = throw . ErrorCall . unsafePerformIO . logg
  where logg t = logM "MyPredule.myerror" ERROR t >> return ("MyPrelude.myerror:" ++ t)

logerror :: String -> a -> a
logerror s x = unsafePerformIO (logg s x)
  where logg t v = logM "MyPrelude.logerror" ERROR t >> return v

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
