{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- This may be set by the .ghci file, otherwise it gets set here.
#ifndef HAVE_HAPPSTACK
#if __GHCJS__
#define HAVE_HAPPSTACK 0
#else
-- #define HAVE_HAPPSTACK 1
-- I can't seem to build this package with the happstack-server dependency
#define HAVE_HAPPSTACK 0
#endif
#endif

module Data.FileCache.Happstack
  (
    -- * Happstack
#if HAVE_HAPPSTACK
    Real.ContentType(..)
#else
    ContentType(..)
#endif
  ) where

import Control.Lens.Path (HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(hops))
import Control.Monad.Except (throwError)
import Data.Data ( Data )
import Data.SafeCopy ( SafeCopy )
import Data.Serialize ( Serialize(..) )
import Data.Typeable (typeRep)
import GHC.Generics ( Generic )
-- import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ( Lift )

#if HAVE_HAPPSTACK
import Happstack.Server as Real (ContentType(..))
deriving instance Generic Real.ContentType
deriving instance Serialize Real.ContentType
#endif

-- * Happstack Types
#if !HAVE_HAPPSTACK
data ContentType =
        ContentType {
                     -- | The top-level media type, the general type
                     --   of the data. Common examples are
                     --   \"text\", \"image\", \"audio\", \"video\",
                     --   \"multipart\", and \"application\".
                     ctType :: String,
                     -- | The media subtype, the specific data format.
                     --   Examples include \"plain\", \"html\",
                     --   \"jpeg\", \"form-data\", etc.
                     ctSubtype :: String,
                     -- | Media type parameters. On common example is
                     --   the charset parameter for the \"text\"
                     --   top-level type, e.g. @(\"charset\",\"ISO-8859-1\")@.
                     ctParameters :: [(String, String)]
                    }
    deriving (Show, Read, Eq, Ord, Generic, Serialize)

-- Happstack does not create a SafeCopy instance for ContentType.
-- This gives it version 0.
#endif

$(concat <$>
  sequence
  [ pathInstances [FIELDS] =<< [t|ContentType|]
  ])

deriving instance Data ContentType
deriving instance Lift ContentType
instance SafeCopy ContentType
instance Value ContentType where hops _ = [RecType, CtorType]
