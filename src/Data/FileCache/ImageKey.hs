-- | An 'ImageKey' is a value that describes how to derive a modified
-- 'ImageFile' from an existing one.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Werror=incomplete-patterns #-}

module Data.FileCache.ImageKey
  ( ImageKey(..)
  , HasImageKey(imageKey)
  , OriginalKey(originalKey)
  , UprightKey(uprightKey)
  , EditedKey(editedKey)
  , ScaledKey(scaledKey)

  , ImagePath(ImagePath, _imagePathKey)
  , HasImagePath(imagePath)
  , shapeFromKey

  , FileType(..)
  , HasFileType(imageType)
  , supportedFileTypes
  , allSupported
  , omitHEICAndTIFF, omitTIFF

  , supportedMimeTypes
  , MimeType

  , ImageShape(..)
  , HasImageShapeM(imageShapeM)
  , HasImageShape, imageShape
  , HasOriginalShape(originalShape)
  , ImageStats(..)
  ) where

import Control.Lens ( Identity(runIdentity) )
import Control.Lens.Path ( HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(hops) )
import Control.Lens.Path ()
import Control.Monad ( ap )
import Control.Monad.Except ( throwError )
import Data.Data ( Data )
import Data.Default ( def )
import Data.FileCache.File ( Checksum, File(_fileChksum), HasFileExtension(fileExtension) )
import Data.FileCache.Happstack (ContentType(..))
import Data.FileCache.ImageCrop ( ImageCrop(..), Rotation )
import Data.FileCache.ImageRect ( HasImageRect(imageRect), ImageRect, makeImageRect, scaleImageRect, scaleFromDPI, cropImageRect, uprightImageRect )
import Data.FileCache.ImageSize ( HasImageSize(..), ImageSize(..) )
import Data.FileCache.Rational ( approx, showRational, fromRat )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( SafeCopy(kind), base, extension, Migrate(..), safeGet, safePut, SafeCopy(version) )
import Data.Serialize ( Serialize(..) )
import Data.Text ( pack, span, Text, unpack )
import Data.Typeable ( Typeable, typeRep )
import GHC.Generics ( Generic )
import GHC.Stack (callStack, HasCallStack)
import Language.Haskell.TH.Lift as TH ()
import Prelude hiding (span)
import Text.Parsec ( (<|>) )
import Text.PrettyPrint.HughesPJClass ( Doc, comma, hsep, punctuate, Pretty(pPrint), text )
import Web.Routes ( PathInfo(..), segment )
import Web.Routes.TH ( derivePathInfo )

#if __GHCJS__
import Control.Lens (ReifiedLens(Lens), ReifiedPrism(Prism), ReifiedTraversal(Traversal))
import Control.Lens.Path (ConstructorPosTuple(..), WithFieldOptic(..))
import Control.Lens.Path.Error (PathError(PathError))
import Control.Lens.Path.PathTypes
import Control.Lens.Path.ReifiedOptic (ReifiedOptic(..))
import Data.Generics.Product
import Data.Generics.Sum
import Data.Proxy (Proxy(Proxy))
import Web.Routes (PathInfo(..), segment)
#else
import Extra.THIO (spliceModule)
#endif

-- * MimeType

type MimeType = String

class HasMimeType a where
  mimeType :: a -> MimeType

instance HasMimeType FileType where
  mimeType GIF = "image/gif"
  mimeType HEIC = "image/heif"
  mimeType JPEG = "image/jpeg"
  mimeType PPM = "image/ppm"
  mimeType PNG = "image/png"
  mimeType PDF = "application/pdf"
  mimeType TIFF = "image/tiff"
  mimeType CSV = "application/csv"
  mimeType Unknown = "application/unknown"

instance HasFileType ContentType where
  imageType (ContentType{..}) | ctSubtype == "csv" = CSV
  imageType (ContentType{..}) | ctSubtype == "gif" = GIF
  imageType (ContentType{..}) | ctSubtype == "heif" = HEIC
  imageType (ContentType{..}) | elem ctSubtype ["jpeg", "jpg"] = JPEG
  imageType (ContentType{..}) | ctSubtype == "pdf" = PDF
  imageType (ContentType{..}) | ctSubtype == "png" = PNG
  imageType (ContentType{..}) | ctSubtype == "ppm" = PPM
  imageType (ContentType{..}) | ctSubtype == "tiff" = TIFF
  imageType (ContentType{..}) = Unknown

supportedMimeTypes :: (FileType -> Bool) -> [MimeType]
supportedMimeTypes omit = map mimeType (allSupported omit)

-- * FileType

-- HEIC, should also have HEIF?

-- Should FileType be replaced by ContentType?
data FileType = GIF | HEIC | JPEG | PDF | PNG | PPM | TIFF | CSV | Unknown deriving (Generic, Eq, Ord, Enum, Bounded)

instance SafeCopy FileType where version = 3; kind = extension
instance Serialize FileType where get = safeGet; put = safePut
instance Migrate FileType where
  type MigrateFrom FileType = FileType_2
  migrate old =
    case old of
      PPM_2 -> PPM
      JPEG_2 -> JPEG
      GIF_2 -> GIF
      PNG_2 -> PNG
      PDF_2 -> PDF
      TIFF_2 -> TIFF
      HEIC_2 -> HEIC
      Unknown_2 -> Unknown

data FileType_2 = GIF_2 | HEIC_2 | JPEG_2 | PDF_2 | PNG_2 | PPM_2 | TIFF_2 | Unknown_2 deriving (Generic, Eq, Ord, Enum, Bounded)

instance SafeCopy FileType_1 where version = 1; kind = base

instance SafeCopy FileType_2 where version = 2; kind = extension
instance Value FileType where hops _ = []

instance Migrate FileType_2 where
  type MigrateFrom FileType_2 = FileType_1
  migrate old =
    case old of
      PPM_1 -> PPM_2
      JPEG_1 -> JPEG_2
      GIF_1 -> GIF_2
      PNG_1 -> PNG_2
      PDF_1 -> PDF_2
      Unknown_1 -> Unknown_2

data FileType_1 = PPM_1 | JPEG_1 | GIF_1 | PNG_1 | PDF_1 | Unknown_1 deriving (Generic, Eq, Ord, Enum, Bounded)

instance Pretty FileType where pPrint = text . noQuotes . show
deriving instance Data FileType
deriving instance Read FileType
deriving instance Show FileType
deriving instance Typeable FileType

noQuotes :: String -> String
noQuotes = filter (/= '"')

-- * HasFileType

class HasFileType a where imageType :: HasCallStack => a -> FileType

instance HasFileExtension FileType where
  fileExtension GIF = ".gif"
  fileExtension HEIC = ".heic"
  fileExtension JPEG = ".jpg"
  fileExtension PDF = ".pdf"
  fileExtension PNG = ".png"
  fileExtension PPM = ".ppm"
  fileExtension TIFF = ".tiff"
  fileExtension CSV = ".csv"
  fileExtension Unknown = ".xxx"

allSupported :: (FileType -> Bool) -> [FileType]
allSupported omit = filter (not . omit) [minBound..maxBound]

omitHEICAndTIFF :: FileType -> Bool
omitHEICAndTIFF typ = elem typ [HEIC, TIFF, Unknown]

omitTIFF :: FileType -> Bool
omitTIFF typ = elem typ [TIFF, Unknown]

-- | A Pretty comma-separated list of FileTypes supported by this library.
supportedFileTypes :: (FileType -> Bool) -> Doc
supportedFileTypes omit = commas (allSupported omit)

commas :: Pretty a => [a] -> Doc
commas = hsep . punctuate comma . map pPrint

-- This instance gives the paths conventional file extensions.
instance PathInfo FileType where
  toPathSegments inp = ["i" <> fileExtension inp]
  fromPathSegments =
    (segment ("i" <> fileExtension PPM) >> return PPM) <|>
    (segment ("i" <> fileExtension JPEG) >> return JPEG) <|>
    (segment ("i" <> fileExtension HEIC) >> return HEIC) <|>
    (segment ("i" <> fileExtension GIF) >> return GIF) <|>
    (segment ("i" <> fileExtension PNG) >> return PNG) <|>
    (segment ("i" <> fileExtension PDF) >> return PDF) <|>
    (segment ("i" <> fileExtension Unknown) >> return Unknown)

-- * ImageShape

data ImageShape
  = ImageShape
    { _imageShapeType :: FileType
    , _imageShapeRect :: Either String ImageRect
      -- ^ this could be safer, there should be different constructors
      -- for image types that have or do not have an ImageRect.
    } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Serialize ImageShape where get = safeGet; put = safePut
instance SafeCopy ImageShape where version = 3; kind = extension
instance Value ImageShape where hops _ = [RecType, CtorType]

instance Pretty ImageShape where
  pPrint (ImageShape typ mrect) =
    text "ImageShape (" <> pPrint typ <>
    either (text . ("missing: " <>)) (\rect -> text " " <> pPrint rect) mrect <>
    text ")"

instance HasFileType ImageShape where imageType = _imageShapeType

instance HasFileExtension ImageShape where
  fileExtension = fileExtension . _imageShapeType

instance HasImageRect ImageShape where imageRect = _imageShapeRect

scaleImageShape :: ImageSize -> Rational -> ImageShape -> ImageShape
scaleImageShape sz dpi shape =
  case _imageShapeRect shape of
    Left _ -> shape
    Right rect | scale rect == 1 -> shape
    Right rect ->
      shape { _imageShapeType = JPEG -- the scaling pipeline results in a jpeg file
            , _imageShapeRect = Right (scaleImageRect sz dpi rect) }
  where
    scale :: ImageRect -> Rational
    scale rect = maybe 1 approx $ scaleFromDPI sz dpi rect

cropImageShape :: ImageCrop -> ImageShape -> ImageShape
cropImageShape crop shape | crop == def = shape
cropImageShape _ shape@ImageShape{_imageShapeRect = Left _} = shape
cropImageShape crop shape@ImageShape{_imageShapeRect = Right rect} =
  shape {_imageShapeRect = Right (cropImageRect crop rect)}

-- | Does this change _imageShapeType?  Should it?
uprightImageShape :: ImageShape -> ImageShape
uprightImageShape shape =
  shape {_imageShapeType = JPEG,
         _imageShapeRect = fmap uprightImageRect (_imageShapeRect shape)}

instance Migrate ImageShape_2 where
  type MigrateFrom ImageShape_2 = ImageShape_1
  migrate (ImageShape_1 typ w h rot) =
    ImageShape_2 { _imageShapeType_2 = typ,
                   _imageShapeRect_2 =
                     case typ of
                       Unknown -> Nothing
                       PDF -> Nothing
                       _ -> Just (makeImageRect w h rot) }

data ImageShape_2
  = ImageShape_2
    { _imageShapeType_2 :: FileType
    , _imageShapeRect_2 :: Maybe ImageRect
      -- ^ this could be safer, there should be different constructors
      -- for image types that have or do not have an ImageRect.
    } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Serialize ImageShape_2 where get = safeGet; put = safePut
instance SafeCopy ImageShape_2 where version = 2; kind = extension
instance Migrate ImageShape where
  type MigrateFrom ImageShape = ImageShape_2
  migrate (ImageShape_2 a b) =
    ImageShape a (maybe (Left "") Right b)

data ImageShape_1
  = ImageShape_1
      { _imageShapeType_1 :: FileType
      , _imageShapeWidth_1 :: Int
      , _imageShapeHeight_1 :: Int
      , _imageFileOrientation_1 :: Rotation
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Serialize ImageShape_1 where get = safeGet; put = safePut
instance SafeCopy ImageShape_1 where version = 1; kind = base

-- * ImageKey - Describes an ImageFile and, if it was derived from
-- other image files, how.

data ImageKey
    = ImageOriginal Checksum FileType
    -- ^ An unmodified upload, the info lets us construct an URL
    | ImageCropped ImageCrop ImageKey
    -- ^ A cropped version of another image
    | ImageScaled ImageSize Rational ImageKey
    -- ^ A resized version of another image
    | ImageUpright ImageKey
    -- ^ Image uprighted using the EXIF orientation code, see  "Appraisal.Exif"
    deriving (Generic, Eq, Ord)

deriving instance Data ImageKey
deriving instance Read ImageKey
deriving instance Show ImageKey
deriving instance Typeable ImageKey
instance Serialize ImageKey where get = safeGet; put = safePut
-- This is not an extension of ImageKey_2, it is a new type
-- created by the migration of ImageCache.
instance SafeCopy ImageKey where version = 4

instance Value ImageKey where hops _ = [RecType, CtorType]

instance Pretty ImageKey where
    pPrint (ImageOriginal csum typ) = text (take 7 (unpack csum)) <> text (unpack (fileExtension typ))
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled sz dpi x) = text "Scale (" <> pPrint sz <> text " @" <> text (showRational dpi) <> text " dpi) (" <> pPrint x <> text ")"

-- | Modified PathInfo instance that omits the "image-original" tag,
-- if none of the other tags appear it is assumed.  This matches the
-- structure of the image cache.
instance PathInfo ImageKey where
  toPathSegments inp =
    case inp of
      ImageCropped arg_a9peM arg_a9peN -> [pack "image-cropped"] <> toPathSegments arg_a9peM <> toPathSegments arg_a9peN
      ImageScaled arg_a9peO arg_a9peP arg_a9peQ -> [pack "image-scaled"] <> toPathSegments arg_a9peO <> toPathSegments arg_a9peP <> toPathSegments arg_a9peQ
      ImageUpright arg_a9peR -> [pack "image-upright"] <> toPathSegments arg_a9peR
      ImageOriginal csum ityp -> {-[pack "image-original"] <>-} [csum <> fileExtension ityp]
  fromPathSegments =
    ap (ap (segment (pack "image-cropped") >> return ImageCropped) fromPathSegments) fromPathSegments <|>
    ap (ap (ap (segment (pack "image-scaled") >> return ImageScaled) fromPathSegments) fromPathSegments) fromPathSegments <|>
    ap (segment (pack "image-upright") >> return ImageUpright) fromPathSegments <|>
    (parseOriginal <$> fromPathSegments)
    -- ap (ap ({-segment (pack "image-original") >>-} return ImageOriginal) fromPathSegments) fromPathSegments
    where
      parseOriginal :: Text -> ImageKey
      parseOriginal t = let (name, ext) = span (/= '.') t in
                          ImageOriginal name (case ext of
                                                 ".jpg" -> JPEG
                                                 ".ppm" -> PPM
                                                 ".png" -> PNG
                                                 ".gif" -> GIF
                                                 ".pdf" -> PDF
                                                 ".csv" -> CSV
                                                 ".heic" -> HEIC
                                                 _ -> Unknown)

-- | Compute a derived image shape from the original image shape and the key.
shapeFromKey :: ImageShape -> ImageKey -> ImageShape
shapeFromKey original (ImageOriginal _csum _typ) =
  original
shapeFromKey original (ImageUpright key) =
  uprightImageShape $ shapeFromKey original key
shapeFromKey original (ImageCropped crop key) =
  cropImageShape crop $ shapeFromKey original key
shapeFromKey original (ImageScaled sz dpi key) =
  scaleImageShape sz dpi $ shapeFromKey original key

-- * HasImageKey

-- | The 'imageKey' method implements ways of constructing an 'ImageKey'
class HasImageKey a where
  imageKey :: a -> ImageKey
-- | The trivial 'HasImageKey' instance.
instance HasImageKey ImageKey where
  imageKey = id

-- | In order to build an image's path or url we need the
-- ImageKey, which tells us how the image is transformed, and we need
-- the Imagetype so we can append the correct file extension.  We could
-- also get it from the File record in ImageReady, but that isn't
-- available until the image has been fully generated by the server.
newtype ImagePath =
  ImagePath { _imagePathKey :: ImageKey
            -- , _imagePathType :: FileType
            } deriving (Generic, Eq, Ord)

class HasImagePath a where imagePath :: a -> ImagePath
instance HasImagePath ImagePath where imagePath = id
instance HasImageKey ImagePath where imageKey (ImagePath x) = imageKey x

-- * OriginalKey

-- | Various ways to build an OriginalKey.
class OriginalKey a where
  originalKey :: a -> ImageKey

instance OriginalKey (Checksum, FileType) where -- danger - Checksum is just String
  originalKey = uncurry ImageOriginal

--instance OriginalKey ImageFile where
--  originalKey (ImageFileReady i) = originalKey i
--  originalKey (ImageFileShape i) = originalKey i

-- | The basic original key is just the file's checksum and format.
instance OriginalKey (File, FileType) where
  originalKey (f, typ) = ImageOriginal (_fileChksum f) typ

instance OriginalKey (File, ImageShape) where
  originalKey (f, shape) = originalKey (f, _imageShapeType shape)

-- | Traverse into an 'ImageKey' to find the key of the original
-- image.
instance OriginalKey ImageKey where
  originalKey key@(ImageOriginal _ _) = key
  originalKey (ImageUpright key) = originalKey key
  originalKey (ImageScaled _ _ key) = originalKey key
  originalKey (ImageCropped _ key) = originalKey key

-- * UprightKey

class UprightKey a where
  uprightKey :: a -> ImageKey
instance UprightKey ImageKey where
  uprightKey (ImageScaled _ _ key) = uprightKey key
  uprightKey (ImageCropped _ key) = uprightKey key
  uprightKey (ImageUpright key) = uprightKey key
  uprightKey key@(ImageOriginal _ _) = ImageUpright key

-- * EditedKey

class EditedKey a where
  editedKey :: a -> ImageKey
instance EditedKey ImageKey where
  editedKey (ImageScaled _ _ key) = editedKey key
  editedKey key@(ImageCropped _ _) = key
  editedKey key@(ImageUpright _) = key
  editedKey key@(ImageOriginal _ _) = ImageUpright key

-- * ScaledKey

-- | Compute an image key that has a certain size at the given dpi
class HasImageSize size => ScaledKey size a where
  scaledKey :: size -> Rational -> a -> ImageKey
instance ScaledKey ImageSize ImageKey where
  scaledKey size dpi x = ImageScaled size dpi (editedKey x)

-- * HasOriginalShape

class HasOriginalShape a where
  originalShape :: a -> ImageShape

-- * HasImageShapeM, HasImageShape

-- | A class whose primary (only?) instance is ImageFile.  Access to
-- the original dimensions of the image, so we can compute the aspect
-- ratio.
class HasImageShapeM m a where
  imageShapeM :: HasCallStack => a -> m ImageShape

type HasImageShape a = HasImageShapeM Identity a

imageShape :: (HasImageShape a, HasCallStack) => a -> ImageShape
imageShape = runIdentity . imageShapeM

instance HasImageShapeM Identity ImageShape where
  imageShapeM s = pure s

-- | We can infer the file type from the key using insider info on how
-- the various IO operations work.  E.g. all the cropping operations
-- other than the identity crop result in a JPEG.
instance HasImageShape a => HasFileType (ImageKey, a) where
  imageType (ImageOriginal _ typ, _) = typ
  imageType (ImageUpright key, shape) = imageType (key, shape)
  imageType (ImageCropped crop key, shape) =
    if crop == def then imageType (key, shape) else JPEG
  imageType (ImageScaled sz dpi key, shape) =
    case _imageShapeRect (imageShape shape) of
      Left _ -> imageType (key, shape)
      Right rect ->
        let sc' = scaleFromDPI sz dpi rect
            sc :: Double
            sc = fromRat (fromMaybe 1 sc') in
          if approx (toRational sc) == 1 then imageType (key, shape) else JPEG

-- * ImageStats

-- Statistics about the server status of the images in this reports.
data ImageStats
  = ImageStats
    { _keys :: Int
    , _ready :: Int
    , _shapes :: Int
    , _errors :: Int
    } deriving (Generic, Eq, Ord, Show, Serialize)

instance SafeCopy ImageStats
instance Value ImageStats where hops _ = []

#if __GHCJS__
#include "SplicesForImageKey.hs"
#else
$(spliceModule "-- Generated by spliceModule\n" =<< mconcat <$> sequence
  [ derivePathInfo ''ImagePath
  , pathInstances [FIELDS] =<< [t|ImageKey|]
  , pathInstances [FIELDS] =<< [t|ImageShape|]
  ])
#endif
