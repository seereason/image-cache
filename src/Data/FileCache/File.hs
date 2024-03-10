{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoOverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.File
  (
    -- * File
    File(..)
  , FileSource(..)
  , Checksum
  , Extension
  , HasFileChecksum(fileChecksum)
  , HasFileExtension(fileExtension)
  ) where

import Control.Lens.Path ( HOP(FIELDS), HopType(CtorType, RecType), pathInstances, Value(..) )
import Control.Monad.Except (throwError)
import Data.Data ( Data )
import Data.FileCache.Happstack ( ContentType(..) )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( base, safeGet, safePut, SafeCopy(kind, version) )
import Data.Serialize ( Serialize(..) )
import Data.Text (Text, unpack)
import Data.Typeable ( Typeable, typeRep )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ( Lift )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- * File

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = Text

type Extension = Text

instance HasFileExtension Extension where fileExtension = id

class HasFileChecksum a where fileChecksum :: a -> Checksum
class HasFileExtension a where fileExtension :: a -> Extension

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: FileSource           -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           , _fileExt :: Extension               -- ^ Name is formed by appending this to checksum
           } deriving (Generic, Eq, Ord)

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File " <> take 7 (unpack cksum) <> unpack ext)
instance SafeCopy File where version = 3; kind = base
instance Serialize File where get = safeGet; put = safePut
deriving instance Show File
deriving instance Read File
deriving instance Data File
deriving instance Typeable File
deriving instance Lift File
instance HasFileChecksum File where fileChecksum = _fileChksum
instance HasFileExtension File where fileExtension = _fileExt

-- |The original source if the file is saved, in case
-- the cache needs to be reconstructed.  However, we don't
-- store the original ByteString if that is all we began
-- with, that would be redundant and wasteful.
data FileSource
    = TheURI String
    | ThePath FilePath
    | TheUpload (FilePath, ContentType)
    | Derived
    | Legacy
    | Missing
    deriving (Generic, Eq, Ord)

instance SafeCopy FileSource where version = 1
instance Serialize FileSource where get = safeGet; put = safePut
deriving instance Show FileSource
deriving instance Read FileSource
deriving instance Data FileSource
deriving instance Typeable FileSource
deriving instance Lift FileSource

#if ARBITRARY
instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure [] <*> arbitrary

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]
#endif

$(concat <$>
  sequence
  [ pathInstances [FIELDS] =<< [t|File|]
  , pathInstances [FIELDS] =<< [t|FileSource|]
  ])

instance Value File where hops _ = [RecType, CtorType]
instance Value FileSource where hops _ = [RecType, CtorType]
