-- | Size, orientation, and format of an image.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageShape
  ( -- * ImageType
    ImageType(..)
  , HasImageType(imageType)
  , supportedMimeTypes
  , supportedImageTypes
    -- * ImageShape, HasImageShape
  , ImageShape(..)
  , HasImageShapeM(imageShapeM)
  , HasImageShape, imageShape
  , HasOriginalShape(originalShape)
  , scaleFromDPI
  , scaleImageShape
  , cropImageShape
  , rotateImageShape
  , uprightImageShape
  , ImageStats(..)
  ) where

import Control.Lens (Identity(runIdentity), _Just, over)
import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.FileCache.File (Extension, HasFileExtension(fileExtension))
import Data.FileCache.ImageCrop ( Rotation(..), ImageCrop(..) )
import Data.FileCache.ImageRect (HasImageRect(imageRect), ImageRect, makeImageRect, scaleImageRect, scaleFromDPI, rotateImageRect, cropImageRect, uprightImageRect)
import Data.FileCache.ImageSize (ImageSize)
import Data.FileCache.Rational (approx)
import Data.Generics.Labels ()
import Data.Monoid ( (<>) )
import Data.SafeCopy (base, extension, Migrate(..), safeGet, safePut, Migrate(..), SafeCopy(version, kind) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Text.Parsec ( (<|>) )
import Text.PrettyPrint.HughesPJClass ( Doc, Pretty(pPrint), comma, empty, hsep, punctuate, text )
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

class HasImageType a where imageType :: a -> ImageType

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

-- * ImageShape

class HasOriginalShape a where
  originalShape :: a -> ImageShape

-- * HasImageShape

-- | A class whose primary (only?) instance is ImageFile.  Access to
-- the original dimensions of the image, so we can compute the aspect
-- ratio.
class HasImageShapeM m a where
  imageShapeM :: a -> m ImageShape

type HasImageShape a = HasImageShapeM Identity a
imageShape :: HasImageShape a => a -> ImageShape
imageShape = runIdentity . imageShapeM

instance HasImageShapeM Identity ImageShape where
  imageShapeM s = pure s

data ImageShape_1
  = ImageShape_1
      { _imageShapeType_1 :: ImageType
      , _imageShapeWidth_1 :: Int
      , _imageShapeHeight_1 :: Int
      , _imageFileOrientation_1 :: Rotation
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Serialize ImageShape_1 where get = safeGet; put = safePut
instance SafeCopy ImageShape_1 where version = 1; kind = base
instance Migrate ImageShape where
  type MigrateFrom ImageShape = ImageShape_1
  migrate (ImageShape_1 typ w h rot) =
    ImageShape { _imageShapeType = typ,
                 _imageShapeRect =
                   case typ of
                     -- Unknown -> Nothing
                     PDF -> Nothing
                     _ -> Just (makeImageRect w h rot) }

instance HasImageRect ImageShape where imageRect = _imageShapeRect

data ImageShape
  = ImageShape
    { _imageShapeType :: ImageType
    , _imageShapeRect :: Maybe ImageRect
      -- ^ this could be safer, there should be different constructors
      -- for image types that have or do not have an ImageRect.
    } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Serialize ImageShape where get = safeGet; put = safePut
instance SafeCopy ImageShape where version = 2; kind = extension

instance Pretty ImageShape where
  pPrint (ImageShape typ mrect) =
    text "ImageShape (" <> pPrint typ <>
    maybe empty (\rect -> text " " <> pPrint rect) mrect <>
    text ")"

instance HasImageType ImageShape where imageType = _imageShapeType

scaleImageShape :: ImageSize -> Rational -> ImageShape -> ImageShape
scaleImageShape sz dpi shape =
  case _imageShapeRect shape of
    Nothing -> shape
    Just rect | scale rect == 1 -> shape
    Just rect ->
      shape { _imageShapeType = JPEG -- the scaling pipeline results in a jpeg file
            , _imageShapeRect = Just (scaleImageRect sz dpi rect) }
  where
    scale :: ImageRect -> Rational
    scale rect = maybe 1 approx $ scaleFromDPI sz dpi rect

cropImageShape :: ImageCrop -> ImageShape -> ImageShape
cropImageShape crop shape | crop == def = shape
cropImageShape _ shape@ImageShape{_imageShapeRect = Nothing} = shape
cropImageShape crop shape@ImageShape{_imageShapeRect = Just rect} =
  shape {_imageShapeRect = Just (cropImageRect crop rect)}

rotateImageShape :: Rotation -> ImageShape -> ImageShape
rotateImageShape ZeroHr shape = shape
rotateImageShape _ shape@ImageShape{_imageShapeRect = Nothing} = shape
rotateImageShape rot shape@ImageShape{_imageShapeRect = Just rect} =
  -- Note that the image type is changed to JPEG because the tool we
  -- currently use to rotate images returns a JPEG image.
  shape {_imageShapeType = JPEG,
         _imageShapeRect = Just (rotateImageRect rot rect)}

-- | This seems to have evolved into a no-op.
uprightImageShape :: ImageShape -> ImageShape
uprightImageShape shape = over (#_imageShapeRect . _Just) uprightImageRect shape

-- Statistics about the server status of the images in this reports.
data ImageStats
  = ImageStats
    { _keys :: Int
    , _ready :: Int
    , _shapes :: Int
    , _errors :: Int
    } deriving (Generic, Eq, Ord, Show, Serialize)

instance SafeCopy ImageStats
