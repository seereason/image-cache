-- | Choose which deriveSerialize function to use everywhere.

module Appraisal.Serialize
    ( Data.THUnify.GHCGenerics.deriveSerialize
    ) where

-- This one produces smaller code under ghcjs
import qualified Data.THUnify.Serialize (deriveSerialize)
-- This one compilers faster in many cases under ghcjs
import qualified Data.THUnify.GHCGenerics (deriveSerialize)
