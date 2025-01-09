{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.ImageCrop
  ( -- * ImageCrop
    ImageCrop(..)
  , Rotation(..)
  ) where

import Control.Applicative ((<$>), (<|>))
import Control.Lens.Path ( HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(..) )
import Control.Monad (ap)
import Control.Monad.Except (throwError)
import Data.Data ( Data )
import Data.Default ( Default(def) )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, safeGet, safePut, SafeCopy(kind, version) )
import Data.Serialize ( Serialize(..) )
import Data.Typeable (Typeable, typeRep)
import GHC.Generics ( Generic )
-- import Prelude ( (++), ($), Eq, Ord, Read, Show(show), Int )
import Data.Text (pack)
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )
import Web.Routes
import Web.Routes.TH ( derivePathInfo )

-- * ImageCrop

-- |This describes the cropping and rotation of an image.
data ImageCrop
    = ImageCrop
      { topCrop :: Int
      , bottomCrop :: Int
      , leftCrop :: Int
      , rightCrop :: Int
      , rotation :: Rotation
      } deriving (Generic, Eq, Ord, Data, Read, Show, Typeable)

instance Default ImageCrop where def = ImageCrop 0 0 0 0 ZeroHr
instance Serialize ImageCrop where get = safeGet; put = safePut
instance SafeCopy ImageCrop where kind = base; version = 1
instance Value ImageCrop where hops _ = [RecType, CtorType]
instance Pretty ImageCrop where
    pPrint (ImageCrop 0 0 0 0 ZeroHr) = text "(no crop)"
    pPrint (ImageCrop t b l r ZeroHr) = text $ "(crop " <> show (b, l) <> " -> " <> show (t, r) <> ")"
    pPrint (ImageCrop t b l r rot) = text $ "(crop " <> show (b, l) <> " -> " <> show (t, r) <> ", rot " ++ show rot ++ ")"

-- * Rotation

data Rotation = ZeroHr | ThreeHr | SixHr | NineHr
  deriving (Generic, Eq, Ord, Show, Read, Data, Typeable)

instance Default Rotation where def = ZeroHr
instance SafeCopy Rotation where version = 0
instance Serialize Rotation where get = safeGet; put = safePut
instance Value Rotation where hops _ = []

#if MIN_VERSION_template_haskell(2,17,0)
instance PathInfo ImageCrop where
      toPathSegments inp_aAxw
        = case inp_aAxw of {
            ImageCrop arg_aAxx arg_aAxy arg_aAxz arg_aAxA arg_aAxB
              -> ((++) [pack "image-crop"])
                   (((++) (toPathSegments arg_aAxx))
                      (((++) (toPathSegments arg_aAxy))
                         (((++) (toPathSegments arg_aAxz))
                            (((++) (toPathSegments arg_aAxA)) (toPathSegments arg_aAxB))))) }
      fromPathSegments
        = (ap
             ((ap
                 ((ap
                     ((ap
                         ((ap
                             (segment (pack "image-crop")
                                >> return ImageCrop))
                            fromPathSegments))
                        fromPathSegments))
                    fromPathSegments))
                fromPathSegments))
            fromPathSegments
instance PathInfo Rotation where
      toPathSegments inp_aAyb
        = case inp_aAyb of
            ZeroHr -> [pack "zero-hr"]
            ThreeHr -> [pack "three-hr"]
            SixHr -> [pack "six-hr"]
            NineHr -> [pack "nine-hr"]
      fromPathSegments
        = ((<|>)
             (((<|>)
                 (((<|>)
                     (segment (pack "zero-hr")
                        >> return ZeroHr))
                    (segment (pack "three-hr")
                       >> return ThreeHr)))
                (segment (pack "six-hr")
                   >> return SixHr)))
            (segment (pack "nine-hr")
               >> return NineHr)
#else
$(concat <$>
  sequence
  [ derivePathInfo ''ImageCrop
  , derivePathInfo ''Rotation
  , pathInstances [FIELDS] =<< [t|ImageCrop|]
  ])
#endif
