-- | Choose which deriveSerialize function to use everywhere.

module Appraisal.Serialize
    (
      -- This one produces smaller code under ghcjs
      -- Data.THUnify.GHCGenerics.deriveSerialize
      -- This one compilers faster in many cases under ghcjs
      Data.THUnify.Serialize.deriveSerialize
    ) where

import qualified Data.THUnify.Serialize (deriveSerialize)
import qualified Data.THUnify.GHCGenerics (deriveSerialize)
