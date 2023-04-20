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
  , supportedImageTypes
  ) where

import Data.Data ( Data )
import Data.Monoid ( (<>) )
import Data.SafeCopy (base, extension, Migrate(..), SafeCopy(kind, version), safeGet, safePut)
import Data.Serialize ( Serialize(..) )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Text.Parsec ( (<|>) )
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(pPrint), comma, hsep, punctuate, text )
import Web.Routes ( PathInfo(..), segment )

-- * ImageType and Checksum

-- HEIC, should also have HEIF?

data ImageType = GIF | HEIC | JPEG | PDF | PNG | PPM | TIFF | Unknown deriving (Generic, Eq, Ord, Enum, Bounded)

deriving instance Data ImageType
deriving instance Read ImageType
deriving instance Show ImageType
deriving instance Typeable ImageType
instance Serialize ImageType where get = safeGet; put = safePut
instance SafeCopy ImageType_1 where version = 1; kind = base
instance SafeCopy ImageType where version = 2; kind = extension
instance Migrate ImageType where
  type MigrateFrom ImageType = ImageType_1
  migrate old =
    case old of
      PPM_1 -> PPM
      JPEG_1 -> JPEG
      GIF_1 -> GIF
      PNG_1 -> PNG
      PDF_1 -> PDF
      Unknown_1 -> Unknown
data ImageType_1 = PPM_1 | JPEG_1 | GIF_1 | PNG_1 | PDF_1 | Unknown_1 deriving (Generic, Eq, Ord, Enum, Bounded)

instance Pretty ImageType where pPrint = text . noQuotes . show

noQuotes :: String -> String
noQuotes = filter (/= '"')

type Extension = Text

class HasImageType a where imageType :: a -> ImageType

class HasFileExtension a where fileExtension :: a -> Extension
instance HasFileExtension Extension where fileExtension = id

instance HasFileExtension ImageType where
  fileExtension GIF = ".gif"
  fileExtension HEIC = ".heic"
  fileExtension JPEG = ".jpg"
  fileExtension PDF = ".pdf"
  fileExtension PNG = ".png"
  fileExtension PPM = ".ppm"
  fileExtension TIFF = ".tiff"
  fileExtension Unknown = ".xxx"

type MimeType = String
class HasMimeType a where
  mimeType :: a -> MimeType

instance HasMimeType ImageType where
  mimeType GIF = "image/gif"
  mimeType HEIC = "image/heif"
  mimeType JPEG = "image/jpeg"
  mimeType PPM = "image/ppm"
  mimeType PNG = "image/png"
  mimeType PDF = "application/pdf"
  mimeType TIFF = "image/tiff"
  mimeType Unknown = "application/unknown"

allSupported :: [ImageType]
allSupported = filter works [minBound..maxBound]
  where works :: ImageType -> Bool
        works it = not (it `elem` [HEIC, TIFF, Unknown])
  

-- | A Pretty comma-separated list of ImageTypes supported by this library.
supportedImageTypes :: Doc
supportedImageTypes = commas allSupported

commas :: Pretty a => [a] -> Doc
commas = hsep . punctuate comma . map pPrint

supportedMimeTypes :: [String]
supportedMimeTypes = map mimeType allSupported

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
