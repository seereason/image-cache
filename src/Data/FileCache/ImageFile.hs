{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageFile
  (
    -- * ImageFile, ImageReady
    ImageFile(..)
  , ImageReady(..)

  , ImageKey_2(..)
  ) where

import Control.Lens ( Identity, iso )
import Control.Lens.Path ( View(..) )
import Data.Data ( Data )
import Data.FileCache.File ( File )
import Data.FileCache.ImageKey ( ScaledKey(..), EditedKey(..), UprightKey(..), OriginalKey(..),
                                 ImageKey(ImageUpright, ImageScaled) )
import Data.FileCache.ImageType ( HasFileExtension(..), HasImageType(..), ImageType )
import Data.FileCache.ImageShape ( HasImageShapeM(..), ImageShape(_imageShapeType), ImageShape_0(ImageShape_0) )
import Data.FileCache.ImageCrop ( ImageCrop )
import Data.FileCache.ImageSize ( HasImageSize(imageSize), ImageSize )
import Data.FileCache.Happstack ()
import Data.Monoid ( (<>) )
import Data.SafeCopy ( extension, safeGet, safePut, Migrate(..), SafeCopy(kind, version) )
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

instance SafeCopy ImageFile where version = 3; kind = extension
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


-- MIGRATIONS

-- | A file containing an image plus meta info.
data ImageFile_2
    = ImageFile_2
      { _imageFile_2 :: File
      , _imageFileShape_2 :: ImageShape
      } deriving (Generic, Eq, Ord, Data, Typeable, Read, Show)

instance Migrate ImageFile where
  type MigrateFrom ImageFile = ImageFile_2
  migrate (ImageFile_2 f s) = ImageFileReady (ImageReady f s)

instance SafeCopy ImageFile_2 where kind = extension; version = 2

-- 1 Nov 2019
instance Migrate ImageFile_2 where
  type MigrateFrom ImageFile_2 = ImageFile_1
  migrate (ImageFile_1 f t w h _) = ImageFile_2 f (migrate (ImageShape_0 t w h))

data ImageFile_1
    = ImageFile_1
      { _imageFile_1 :: File
      , _imageFileType_1 :: ImageType
      , _imageFileWidth_1 :: Int
      , _imageFileHeight_1 :: Int
      , _imageFileMaxVal_1 :: Int
      } deriving (Generic, Eq, Ord)

instance SafeCopy ImageFile_1 where version = 1

-- When this is removed the 'setImageFileTypes' function should also
-- be removed.
data ImageKey_2
    = ImageOriginal_2 ImageFile
    | ImageCropped_2 ImageCrop ImageKey_2
    | ImageScaled_2 ImageSize Rational ImageKey_2
    | ImageUpright_2 ImageKey_2
    deriving (Generic, Eq, Ord)

instance SafeCopy ImageKey_2 where version = 2
-- instance SafeCopy ImageKey_3 where kind = extension; version = 3
