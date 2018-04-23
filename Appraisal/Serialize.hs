-- | Choose which deriveSerialize function to use everywhere.

{-# LANGUAGE CPP, TemplateHaskell #-}

module Appraisal.Serialize
    (
      -- Data.THUnify.GHCGenerics.deriveSerialize
      -- This one compilers faster in many cases under ghcjs
      Appraisal.Serialize.deriveSerialize
    ) where

import Data.SafeCopy (SafeCopy, safeGet, safePut)
import Data.Serialize (Serialize(..))
-- import Data.THUnify.SerializeViaSafeCopy
import Language.Haskell.TH (Dec, TypeQ, Q)

deriveSerialize :: TypeQ -> Q [Dec]
deriveSerialize typ =
    [d|instance {-SafeCopy $typ =>-} Serialize $typ where
          get = safeGet
          put = safePut|]

#if 0
-- This one produces larger .js_o files.
import qualified Data.THUnify.GHCGenerics (deriveSerialize)

deriveSerialize = Data.THUnify.GHCGenerics.deriveSerialize
#endif

#if 0
-- This one produces smaller code under ghcjs.  It compiles reasonably fast
-- if you have 32GB.  If you only have 16GB it may never complete.
import Data.THUnify.Serialize (deriveSerialize)
-- This one prints debugging output
import Data.THUnify.Serialize (deriveSerializeVerbose)

deriveSerialize = deriveSerializeVerbose 2
#endif
