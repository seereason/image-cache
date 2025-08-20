-- | Provide a copy of the Happstack ContentType type if we are in an
-- environment where it is not available (i.e. ghcjs.)

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.Happstack
  ( ContentType(..)
  , showContentType
  ) where

import Control.Lens.Path (HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(hops))
import Control.Monad.Except (throwError)
import Data.Data ( Data )
import Data.SafeCopy ( SafeCopy )
import Data.Serialize ( Serialize(..) )
import Data.Typeable (typeRep)
import GHC.Generics ( Generic )
import GHC.Stack (callStack)
import Language.Haskell.TH.Lift as TH ( Lift )

#ifdef MIN_VERSION_happstack_server
import Happstack.Server as Real (ContentType(..))
import Happstack.Server.Internal.RFC822Headers (showContentType)
deriving instance Generic Real.ContentType
deriving instance Serialize Real.ContentType
#endif

-- * Happstack Types
#ifndef MIN_VERSION_happstack_server
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

showContentType :: ContentType -> String
showContentType (ContentType x y ps) = x ++ "/" ++ y ++ showParameters ps

showParameters :: [(String,String)] -> String
showParameters = concatMap f
    where f (n,v) = "; " ++ n ++ "=\"" ++ concatMap esc v ++ "\""
          esc '\\' = "\\\\"
          esc '"'  = "\\\""
          esc c | c `elem` ['\\','"'] = '\\':[c]
                | otherwise = [c]

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
