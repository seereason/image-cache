{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

module Data.FileCache.ImageType
  (
    -- * ImageType
    ImageType(..)
  , Extension
  , HasFileExtension(fileExtension)
  , HasImageType(imageType)
  , HasMimeType(mimeType)
  , Checksum
  , HasFileChecksum(fileChecksum)
  , ppImageTypes
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

data ImageType = GIF | HEIC | JPEG | PDF | PNG | PPM | TIFF | CSV | Unknown deriving (Generic, Eq, Ord, Enum, Bounded)

deriving instance Data ImageType
deriving instance Read ImageType
deriving instance Show ImageType
deriving instance Typeable ImageType
instance Serialize ImageType where get = safeGet; put = safePut

instance SafeCopy ImageType_1 where version = 1; kind = base
instance SafeCopy ImageType_2 where version = 2; kind = extension
instance SafeCopy ImageType where version = 3; kind = extension

instance Migrate ImageType where
  type MigrateFrom ImageType = ImageType_2
  migrate old =
    case old of
      GIF_2 -> GIF
      HEIC_2 -> HEIC
      JPEG_2 -> JPEG
      PDF_2 -> PDF
      PNG_2 -> PNG
      PPM_2 -> PPM
      TIFF_2 -> TIFF
      Unknown_2 -> Unknown

instance Migrate ImageType_2 where
  type MigrateFrom ImageType_2 = ImageType_1
  migrate old =
    case old of
      PPM_1 -> PPM_2
      JPEG_1 -> JPEG_2
      GIF_1 -> GIF_2
      PNG_1 -> PNG_2
      PDF_1 -> PDF_2
      Unknown_1 -> Unknown_2

data ImageType_2 = GIF_2 | HEIC_2 | JPEG_2 | PDF_2 | PNG_2 | PPM_2 | TIFF_2 | Unknown_2 deriving (Generic, Eq, Ord, Enum, Bounded)
data ImageType_1 = PPM_1 | JPEG_1 | GIF_1 | PNG_1 | PDF_1 | Unknown_1 deriving (Generic, Eq, Ord, Enum, Bounded)

instance Pretty ImageType where pPrint = text . noQuotes . show

noQuotes :: String -> String
noQuotes = filter (/= '"')

type Extension = Text

class HasImageType a where imageType :: a -> ImageType

instance HasImageType Extension where
  imageType ext = case filter ((== ext) . fileExtension) ts of
    [x] -> x
    [] -> Unknown
    es -> error ("Data.FileCache.ImageType HasImageType Extension matched " <> show ext <> " to multiple ImageTypes: " <> show es)
    where ts = [minBound .. maxBound] :: [ImageType]

class HasFileExtension a where fileExtension :: a -> Extension
instance HasFileExtension Extension where fileExtension = id

-- FIXME  This should be a list of file extensions for each ImageType
instance HasFileExtension ImageType where
  fileExtension GIF = ".gif"
  fileExtension HEIC = ".heic"
  fileExtension JPEG = ".jpg"
  fileExtension PDF = ".pdf"
  fileExtension PNG = ".png"
  fileExtension PPM = ".ppm"
  fileExtension TIFF = ".tiff"
  fileExtension CSV = ".csv"
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
  mimeType CSV = "text/csv"
  mimeType Unknown = "application/unknown"

supportedFileTypes :: [ImageType]
supportedFileTypes = filter works [minBound..maxBound]
  where works :: ImageType -> Bool
        works it = not (it `elem` [HEIC, TIFF, Unknown])

-- These are the actual supported image types.
supportedImageTypes :: [ImageType]
supportedImageTypes = filter works [minBound..maxBound]
  where works :: ImageType -> Bool
        works it = not (it `elem` [HEIC, PDF, TIFF, CSV, Unknown])
  
-- | A Pretty comma-separated list of filetypes [ImageType].
ppImageTypes :: [ImageType] -> Doc
ppImageTypes fs = commas fs

commas :: Pretty a => [a] -> Doc
commas = hsep . punctuate comma . map pPrint

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = Text

class HasFileChecksum a where fileChecksum :: a -> Checksum

-- This instance gives the paths conventional file extensions.
instance PathInfo ImageType where
  toPathSegments inp = ["i" <> fileExtension inp]
  fromPathSegments =
    (segment ("i" <> fileExtension GIF) >> return GIF) <|>
    (segment ("i" <> fileExtension HEIC) >> return HEIC) <|>
    (segment ("i" <> fileExtension JPEG) >> return JPEG) <|>
    (segment ("i" <> fileExtension PDF) >> return PDF) <|>
    (segment ("i" <> fileExtension PNG) >> return PNG) <|>
    (segment ("i" <> fileExtension PPM) >> return PPM) <|>
    (segment ("i" <> fileExtension TIFF) >> return TIFF) <|>
    (segment ("i" <> fileExtension CSV) >> return CSV) <|>
    (segment ("i" <> fileExtension Unknown) >> return Unknown)
