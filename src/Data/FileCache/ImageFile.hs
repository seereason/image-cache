-- | An 'ImageFile' is an 'ImageShape' and the path to the image file.

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageFile
  ( ImageFile(..)
  , ImageReady(..)
  , printerDPI
  ) where

import Control.Lens ( Identity, iso )
import Control.Lens.Path (HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(hops), View(_View, ViewType))
import Control.Monad.Except (throwError)
import Data.Data ( Data )
import Data.FileCache.File (File, HasFileExtension(..))
import Data.FileCache.ImageKey ( ScaledKey(..), EditedKey(..), UprightKey(..), OriginalKey(..),
                                 ImageKey(ImageUpright, ImageScaled), shapeFromKey )
import Data.FileCache.ImageRect (HasImageRect(imageRect))
import Data.FileCache.ImageShape (imageShape, HasImageType(..), HasImageShapeM(..), ImageShape(_imageShapeRect))
import Data.FileCache.ImageSize ( HasImageSize(imageSize), ImageSize, SaneSize(SaneSize) )
import Data.FileCache.Happstack ()
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, safeGet, safePut, SafeCopy(kind, version) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable ( Typeable, typeRep )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Lift as TH ()
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Text.Read ( readMaybe )

-- * ImageFile

data ImageFile
  = ImageFileShape ImageShape
  | ImageFileReady ImageReady
  deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance SafeCopy ImageFile where version = 3; kind = base
instance HasFileExtension ImageFile where
  fileExtension (ImageFileShape x) = fileExtension x
  fileExtension (ImageFileReady x) = fileExtension x
instance HasImageRect ImageFile where
  imageRect (ImageFileReady a) = imageRect a
  imageRect (ImageFileShape a) = imageRect a
instance Serialize ImageFile where get = safeGet; put = safePut
instance Pretty ImageFile where
  pPrint (ImageFileShape s) = text "ImageFileShape (" <> pPrint s <> text ")"
  pPrint (ImageFileReady f) = text "ImageFileReady (" <> pPrint f <> text ")"
instance HasImageType ImageFile where
  imageType (ImageFileReady f) = imageType f
  imageType (ImageFileShape s) = imageType s
instance View (Maybe ImageFile) where
  type ViewType (Maybe ImageFile) = String
  _View = iso (maybe "" show) readMaybe
instance HasImageShapeM Identity ImageFile where
  imageShapeM (ImageFileReady f) = imageShapeM f
  imageShapeM (ImageFileShape f) = imageShapeM f
instance Value ImageFile where hops _ = [RecType, CtorType]

-- * ImageReady

-- | An uploaded 'File' whose 'ImageShape' has been determined.
data ImageReady
  = ImageReady {_imageFile :: File, _imageShape :: ImageShape}
  deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance SafeCopy ImageReady where version = 1
instance HasImageRect ImageReady where imageRect = imageRect . _imageShape
instance HasImageRect (SaneSize ImageSize, ImageReady) where
  imageRect (SaneSize size, ready) = imageRect (size, ready)
instance HasImageRect (ImageSize, ImageReady) where
  imageRect a = _imageShapeRect (imageShape a)
instance HasImageShapeM Identity (ImageSize, ImageReady) where
  imageShapeM (size, ready@ImageReady{..}) =
    pure $ shapeFromKey _imageShape (scaledKey size printerDPI ready)

printerDPI :: Rational
printerDPI = 600.0

instance HasFileExtension ImageReady where
  fileExtension = fileExtension . _imageFile
instance Serialize ImageReady where get = safeGet; put = safePut
instance Pretty ImageReady where
  pPrint (ImageReady f s) = text "ImageReady (" <> pPrint f <> text ") (" <> pPrint s <> text ")"
instance HasImageType ImageReady where
  imageType = imageType . _imageShape
instance OriginalKey ImageReady where
  originalKey i = originalKey (_imageFile i, _imageShape i)
instance EditedKey ImageReady where
  editedKey img = uprightKey img
instance ScaledKey ImageSize ImageReady where
  scaledKey size dpi x = ImageScaled (imageSize size) dpi (editedKey x)
instance UprightKey ImageReady where
  uprightKey img = ImageUpright (originalKey img)
instance HasImageShapeM Identity ImageReady where
  imageShapeM = imageShapeM . _imageShape
instance Value ImageReady where hops _ = [RecType, CtorType]

$(concat <$>
  sequence
  [ pathInstances [FIELDS] =<< [t|ImageFile|]
  , pathInstances [FIELDS] =<< [t|ImageReady|]
  ])
