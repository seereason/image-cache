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
import Data.FileCache.File ( File )
import Data.FileCache.ImageKey ( ScaledKey(..), EditedKey(..), UprightKey(..), OriginalKey(..),
                                 ImageKey(ImageUpright, ImageScaled) )
import Data.FileCache.ImageType ( HasFileExtension(..), HasImageType(..), ImageType )
import Data.FileCache.ImageShape ( HasImageShapeM(..), ImageShape(_imageShapeType) )
import Data.FileCache.ImageCrop ( ImageCrop )
import Data.FileCache.ImageSize ( HasImageSize(imageSize), ImageSize )
import Data.FileCache.Happstack ()
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, safeGet, safePut, Migrate(..), SafeCopy(kind, version) )
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
