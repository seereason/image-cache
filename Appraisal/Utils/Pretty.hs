{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TypeSynonymInstances #-}
module Appraisal.Utils.Pretty
    ( Pretty(pretty)
    , (<>)
    , module Text.PrettyPrint
    ) where

import Data.Monoid ((<>))
import Text.PrettyPrint hiding ((<>))

class Pretty doc a where
    pretty :: a -> doc
