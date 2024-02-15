-- | An 'ImageFile' is an 'ImageShape' and the path to the image file.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageFile
  ( ImageFile(..)
  , ImageReady(..)
  ) where

import Control.Lens ( Identity, iso )
import Control.Lens.Path ( View(..) )
import Data.Data ( Data )
import Data.FileCache.File (File, HasFileExtension(..))
import Data.FileCache.ImageKey ( ScaledKey(..), EditedKey(..), UprightKey(..), OriginalKey(..),
                                 ImageKey(ImageUpright, ImageScaled), shapeFromKey )
import Data.FileCache.ImageRect (HasImageRect(imageRect))
import Data.FileCache.ImageShape (HasImageShape, imageShape, HasImageType(..), HasImageShapeM(..), ImageShape(_imageShapeType, _imageShapeRect))
import Data.FileCache.ImageSize ( HasImageSize(imageSize), ImageSize, SaneSize(SaneSize) )
import Data.FileCache.Happstack ()
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, safeGet, safePut, SafeCopy(kind, version) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Lift as TH ()
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Text.Read ( readMaybe )

-- * ImageFile

data ImageFile
  = ImageFileShape ImageShape
  | ImageFileReady ImageReady
  deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

data ImageReady
  = ImageReady {_imageFile :: File, _imageShape :: ImageShape}
  deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance SafeCopy ImageFile where version = 3; kind = base
instance SafeCopy ImageReady where version = 1

instance HasFileExtension ImageFile where
  fileExtension (ImageFileShape x) = fileExtension x
  fileExtension (ImageFileReady x) = fileExtension x

instance HasImageRect ImageReady where imageRect = imageRect . _imageShape
instance HasImageRect ImageFile where
  imageRect (ImageFileReady a) = imageRect a
  imageRect (ImageFileShape a) = imageRect a

instance HasImageRect (SaneSize ImageSize, ImageReady) where
  imageRect (SaneSize size, ready) = imageRect (size, ready)
instance HasImageRect (ImageSize, ImageReady) where
  imageRect a = _imageShapeRect (imageShape a)

instance HasImageShapeM Identity (ImageSize, ImageReady) where
  imageShapeM (size, ready@ImageReady{..}) =
    pure $ shapeFromKey _imageShape (scaledKey size printerDPI ready)
{-
instance HasImageRect (ImageSize, Maybe ImageRect) where
  imageRect (size, Nothing) = makeImageRect (100, 100, ZeroHr)
  imageRect (size, Just rect) =
-}

printerDPI = 600.0 :: Rational

instance HasFileExtension ImageReady where
  fileExtension = fileExtension . _imageFile

instance HasFileExtension ImageShape where
  fileExtension = fileExtension . _imageShapeType

instance Serialize ImageFile where get = safeGet; put = safePut
instance Serialize ImageReady where get = safeGet; put = safePut

instance Pretty ImageFile where
  pPrint (ImageFileShape s) = text "ImageFileShape (" <> pPrint s <> text ")"
  pPrint (ImageFileReady f) = text "ImageFileReady (" <> pPrint f <> text ")"
instance Pretty ImageReady where
  pPrint (ImageReady f s) = text "ImageReady (" <> pPrint f <> text ") (" <> pPrint s <> text ")"
instance HasImageType ImageFile where
  imageType (ImageFileReady f) = imageType f
  imageType (ImageFileShape s) = imageType s
instance HasImageType ImageReady where
  imageType = imageType . _imageShape

instance View (Maybe ImageFile) where
  type ViewType (Maybe ImageFile) = String
  _View = iso (maybe "" show) readMaybe

instance OriginalKey ImageReady where
  originalKey i = originalKey (_imageFile i, _imageShape i)
instance UprightKey ImageReady where
  uprightKey img = ImageUpright (originalKey img)
instance EditedKey ImageReady where
  editedKey img = uprightKey img
instance ScaledKey ImageSize ImageReady where
  scaledKey size dpi x = ImageScaled (imageSize size) dpi (editedKey x)

instance HasImageShapeM Identity ImageFile where
  imageShapeM (ImageFileReady f) = imageShapeM f
  imageShapeM (ImageFileShape f) = imageShapeM f

instance HasImageShapeM Identity ImageReady where
  imageShapeM = imageShapeM . _imageShape
