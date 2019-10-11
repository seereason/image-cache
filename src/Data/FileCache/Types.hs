{-# LANGUAGE CPP, DataKinds #-}
{-# LANGUAGE DeriveAnyClass, DeriveFunctor, DeriveGeneric, DeriveLift #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wall -Wredundant-constraints -fno-warn-orphans #-}

module Data.FileCache.Types
    ( FileCacheTop(..)
    , HasFileCacheTop(fileCacheTop)
      -- * Open cache
    ) where

import Control.Lens (_2, over, view)
import Control.Monad.Except (ExceptT)
import Control.Lens.Path (HOP(FIELDS), makePathInstances)
import Control.Monad.RWS (lift, RWST)
import qualified Data.ByteString.Lazy.Char8 as Lazy ( fromChunks )
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Data (Data)
import Data.Digest.Pure.MD5 ( md5 )
import Data.Generics.Product (field)
import Data.Map.Strict as Map (Map)
import Data.Serialize (label, Serialize(..))
import Data.SafeCopy -- (deriveSafeCopy, extension, Migrate(..), SafeCopy)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift as TH (Lift)
import Network.URI ( URI(..), parseRelativeReference, parseURI )
import System.FilePath (makeRelative, (</>))
#if ARBITRARY
import Test.QuickCheck ( Arbitrary(..), oneof )
#endif
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

newtype FileCacheTop = FileCacheTop {_unFileCacheTop :: FilePath} deriving Show

-- | Class of monads with a 'FilePath' value containing the top
-- directory of a file cache.
class Monad m => HasFileCacheTop m where
    fileCacheTop :: m FileCacheTop

instance (Monad m, Monoid w) => HasFileCacheTop (RWST (acid, FileCacheTop) w s m) where
    fileCacheTop = view _2

instance HasFileCacheTop m => HasFileCacheTop (ExceptT e m) where
    fileCacheTop = lift fileCacheTop
