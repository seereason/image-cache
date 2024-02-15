-- | An 'ImageKey' is a value that describes how to derive a modified
-- 'ImageFile' from an existing one.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageKey
  ( -- * ImageKey
    ImageKey(..)
  , HasImageKey(imageKey)
  , OriginalKey(originalKey)
  , UprightKey(uprightKey)
  , EditedKey(editedKey)
  , ScaledKey(scaledKey)

  , ImagePath(ImagePath, _imagePathKey)
  , HasImagePath(imagePath)
  , shapeFromKey
  ) where

import Control.Lens.Path (HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(hops))
import Control.Monad.Except (throwError)
import Control.Monad ( ap )
import Data.Data ( Data )
import Data.Default (def)
import Data.FileCache.Rational ( approx, showRational )
import Data.FileCache.File (Checksum, File(_fileChksum), HasFileExtension(fileExtension))
import Data.FileCache.ImageCrop ( ImageCrop(..) )
import Data.FileCache.ImageShape (cropImageShape, HasImageShape, HasImageType(imageType), ImageShape(_imageShapeType, _imageShapeRect), imageShape, ImageType(..), scaleFromDPI, scaleImageShape, uprightImageShape)
import Data.FileCache.ImageSize ( HasImageSize(..), ImageSize(..) )
import Data.FileCache.Rational ( fromRat )
import Data.Maybe (fromMaybe)
import Data.Monoid ( (<>) )
import Data.SafeCopy ( safeGet, safePut, SafeCopy(version) )
import Data.Serialize ( Serialize(..) )
import Data.Text ( pack, span, Text, unpack )
import Data.Typeable ( Typeable, typeRep )
import GHC.Generics ( Generic )
-- import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ()
import Prelude hiding (span)
import Text.Parsec ( (<|>) )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Web.Routes ( PathInfo(..), segment )
import Web.Routes.TH ( derivePathInfo )

-- * ImageKey

-- | Describes an ImageFile and, if it was derived from other image
-- files, how.
data ImageKey
    = ImageOriginal Checksum ImageType
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

instance Pretty ImageKey where
    pPrint (ImageOriginal csum typ) = text (take 7 (unpack csum)) <> text (unpack (fileExtension typ))
    pPrint (ImageUpright x) = text "Upright (" <> pPrint x <> ")"
    pPrint (ImageCropped crop x) = text "Crop (" <> pPrint crop <> text ") (" <> pPrint x <> text ")"
    pPrint (ImageScaled sz dpi x) = text "Scale (" <> pPrint sz <> text " @" <> text (showRational dpi) <> text " dpi) (" <> pPrint x <> text ")"

class HasImageKey a where
  imageKey :: a -> ImageKey
instance HasImageKey ImageKey where
  imageKey = id

-- | We can infer the file type from the key using insider info on how
-- the various IO operations work.  E.g. all the cropping operations
-- other than the identity crop result in a JPEG.
instance HasImageShape a => HasImageType (ImageKey, a) where
  imageType (ImageOriginal _ typ, _) = typ
  imageType (ImageUpright key, shape) = imageType (key, shape)
  imageType (ImageCropped crop key, shape) =
    if crop == def then imageType (key, shape) else JPEG
  imageType (ImageScaled sz dpi key, shape) =
    case _imageShapeRect (imageShape shape) of
      Nothing -> imageType (key, shape)
      Just rect ->
        let sc' = scaleFromDPI sz dpi rect
            sc :: Double
            sc = fromRat (fromMaybe 1 sc') in
          if approx (toRational sc) == 1 then imageType (key, shape) else JPEG

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

-- | Various ways to build an OriginalKey.
class OriginalKey a where
  originalKey :: a -> ImageKey

instance OriginalKey (Checksum, ImageType) where -- danger - Checksum is just String
  originalKey = uncurry ImageOriginal

--instance OriginalKey ImageFile where
--  originalKey (ImageFileReady i) = originalKey i
--  originalKey (ImageFileShape i) = originalKey i

instance OriginalKey (File, ImageType) where
  originalKey (f, typ) = ImageOriginal (_fileChksum f) typ

instance OriginalKey (File, ImageShape) where
  originalKey (f, shape) = originalKey (f, _imageShapeType shape)

instance OriginalKey ImageKey where
  originalKey key@(ImageOriginal _ _) = key
  originalKey (ImageUpright key) = originalKey key
  originalKey (ImageScaled _ _ key) = originalKey key
  originalKey (ImageCropped _ key) = originalKey key

class UprightKey a where
  uprightKey :: a -> ImageKey
instance UprightKey ImageKey where
  uprightKey (ImageScaled _ _ key) = uprightKey key
  uprightKey (ImageCropped _ key) = uprightKey key
  uprightKey (ImageUpright key) = uprightKey key
  uprightKey key@(ImageOriginal _ _) = ImageUpright key

class EditedKey a where
  editedKey :: a -> ImageKey
instance EditedKey ImageKey where
  editedKey (ImageScaled _ _ key) = editedKey key
  editedKey key@(ImageCropped _ _) = key
  editedKey key@(ImageUpright _) = key
  editedKey key@(ImageOriginal _ _) = ImageUpright key

-- | Compute an image key that has a certain size at the given dpi
class HasImageSize size => ScaledKey size a where
  scaledKey :: size -> Rational -> a -> ImageKey
instance ScaledKey ImageSize ImageKey where
  scaledKey size dpi x = ImageScaled size dpi (editedKey x)

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
                                                 _ -> Unknown)

-- | In order to build an image's path or url we need the ImageKey,
-- which tells us how the image is transformed, and we need the
-- Imagetype so we can append the correct file extension.  We could
-- also get it from the File record in ImageReady, but that isn't
-- available until the image has been fully generated by the server.
newtype ImagePath =
  ImagePath { _imagePathKey :: ImageKey
            -- , _imagePathType :: ImageType
            } deriving (Generic, Eq, Ord)

class HasImagePath a where imagePath :: a -> ImagePath
instance HasImagePath ImagePath where imagePath = id
instance HasImageKey ImagePath where imageKey (ImagePath x) = imageKey x

#if MIN_VERSION_template_haskell(2,17,0)
instance PathInfo ImagePath where
      toPathSegments inp_aAxt
        = case inp_aAxt of {
            ImagePath arg_aAxu
              -> ((++) [pack "image-path"]) (toPathSegments arg_aAxu) }
      fromPathSegments
        = (ap (segment (pack "image-path") >> return ImagePath))
            fromPathSegments
#else
$(concat <$>
  sequence
  [ derivePathInfo ''ImagePath
  , pathInstances [FIELDS] =<< [t|ImageKey|]
  ])
#endif

instance Value ImageKey where hops _ = [RecType, CtorType]
