{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageType
  (
    -- * ImageType
    ImageType(..)
  , Extension
  , HasFileExtension(fileExtension)
  , HasImageType(imageType)
  , Checksum
  , HasFileChecksum(fileChecksum)
  , supportedMimeTypes
  ) where

import Data.Data ( Data )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, safeGet, safePut, SafeCopy(kind, version) )
import Data.Serialize ( Serialize(..) )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Text.Parsec ( (<|>) )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Web.Routes ( PathInfo(..), segment )

-- * ImageType and Checksum

data ImageType = PPM | JPEG | GIF | PNG | PDF | Unknown deriving (Generic, Eq, Ord, Enum, Bounded)

deriving instance Data ImageType
deriving instance Read ImageType
deriving instance Show ImageType
deriving instance Typeable ImageType
instance Serialize ImageType where get = safeGet; put = safePut
instance SafeCopy ImageType where version = 1; kind = base
instance Pretty ImageType where pPrint = text . show

type Extension = Text

class HasImageType a where imageType :: a -> ImageType

class HasFileExtension a where fileExtension :: a -> Extension
instance HasFileExtension Extension where fileExtension = id

instance HasFileExtension ImageType where
  fileExtension JPEG = ".jpg"
  fileExtension PPM = ".ppm"
  fileExtension GIF = ".gif"
  fileExtension PNG = ".png"
  fileExtension PDF = ".pdf"
  fileExtension Unknown = ".xxx"

type MimeType = (String, String)
class HasMimeType a where
  mimeType :: a -> MimeType
  mimeType' :: a -> String
  mimeType' = snd . mimeType

instance HasMimeType ImageType where
  mimeType JPEG = ("jpg","image/jpeg")
  mimeType PPM = ("ppm","image/ppm")
  mimeType GIF = ("gif","image/gif")
  mimeType PNG = ("png","image/png")
  mimeType PDF = ("pdf","image/pdf")
  mimeType Unknown = ("xxx","application/unknown")

supportedMimeTypes :: [String]
supportedMimeTypes = map mimeType' allOf
  where allOf = filter (/= Unknown) [minBound..maxBound] :: [ImageType]


-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = Text

class HasFileChecksum a where fileChecksum :: a -> Checksum

-- This instance gives the paths conventional file extensions.
instance PathInfo ImageType where
  toPathSegments inp = ["i" <> fileExtension inp]
  fromPathSegments =
    (segment ("i" <> fileExtension PPM) >> return PPM) <|>
    (segment ("i" <> fileExtension JPEG) >> return JPEG) <|>
    (segment ("i" <> fileExtension GIF) >> return GIF) <|>
    (segment ("i" <> fileExtension PNG) >> return PNG) <|>
    (segment ("i" <> fileExtension PDF) >> return PDF) <|>
    (segment ("i" <> fileExtension Unknown) >> return Unknown)
