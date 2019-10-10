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
    , Checksum
    , FileSource(..)
    , File(..)
    , fileURI
    , filePath
    , fileDir
    , addMessage
    , md5'
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

{-
$(concat <$>
  sequence
  [ makePrisms ''CacheValue
  , makeLenses ''CacheMap
  ])
-}

(<++>) :: FilePath -> FilePath -> FilePath
a <++> b = a </> (makeRelative "" b)

-- |The original source if the file is saved, in case
-- the cache needs to be reconstructed.  However, we don't
-- store the original ByteString if that is all we began
-- with, that would be redundant and wasteful.
data FileSource
    = TheURI String
    | ThePath FilePath
    deriving (Generic, Eq, Ord)

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = String

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           , _fileExt :: String                  -- ^ Name is formed by appending this to checksum
           } deriving (Generic, Eq, Ord)

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File(" <> show (cksum <> ext) <> ")")

instance SafeCopy FileSource where version = 1
instance SafeCopy File where version = 2
instance Serialize FileSource where get = safeGet; put = safePut
instance Serialize File where get = safeGet; put = safePut

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: File -> Maybe URI
fileURI file = case _fileSource file of
                 Just (TheURI uri) -> maybe (parseRelativeReference uri) Just (parseURI uri)
                 _ -> Nothing

-- |Add a message to the file message list.
addMessage :: String -> File -> File
addMessage message file = over (field @"_fileMessages") (++ [message]) file

md5' :: P.ByteString -> String
#ifdef LAZYIMAGES
md5' = show . md5
#else
md5' = show . md5 . Lazy.fromChunks . (: [])
#endif

filePath :: File -> FilePath
filePath file = fileDir file <++> _fileChksum file <> _fileExt file

fileDir :: File -> FilePath
fileDir file = take 2 (_fileChksum file)

deriving instance Show FileSource
deriving instance Read FileSource
deriving instance Data FileSource
deriving instance Typeable FileSource
deriving instance Lift FileSource

deriving instance Show File
deriving instance Read File
deriving instance Data File
deriving instance Typeable File
deriving instance Lift File

$(makePathInstances [FIELDS] ''File)
$(makePathInstances [FIELDS] ''FileSource)

#if ARBITRARY
instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure [] <*> arbitrary

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]
#endif
