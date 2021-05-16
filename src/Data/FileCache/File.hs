{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.File
  (
    -- * File
    File(..)
  , FileSource(..)
  , Checksum
  , HasFileChecksum(fileChecksum)
  ) where

import Data.Data ( Data )
import Data.FileCache.ImageType ( HasFileChecksum(..), Checksum, HasFileExtension(..), Extension )
import Data.FileCache.Happstack ( ContentType(..) )
import Data.Maybe ( fromMaybe )
import Data.Monoid ( (<>) )
import Data.SafeCopy ( extension, safeGet, safePut, Migrate(..), SafeCopy(kind, version) )
import Data.Serialize ( Serialize(..) )
import Data.Text ( pack, unpack )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ( Lift )
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- * File

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: FileSource           -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           , _fileExt :: Extension               -- ^ Name is formed by appending this to checksum
           } deriving (Generic, Eq, Ord)

instance HasFileChecksum File where fileChecksum = _fileChksum
instance HasFileExtension File where fileExtension = _fileExt

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File " <> take 7 (unpack cksum) <> unpack ext)
instance SafeCopy File where version = 3; kind = extension
instance Serialize File where get = safeGet; put = safePut
deriving instance Show File
deriving instance Read File
deriving instance Data File
deriving instance Typeable File
deriving instance Lift File

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

instance Migrate File where
  type MigrateFrom File = File_2
  migrate (File_2 src cksum msgs ext) =
    File {_fileSource = fromMaybe Legacy src, _fileChksum = pack cksum, _fileMessages = msgs, _fileExt = pack ext}

-- |A local cache of a file obtained from a 'FileSource'.
data File_2
    = File_2 { _fileSource_2 :: Maybe FileSource
             , _fileChksum_2 :: String
             , _fileMessages_2 :: [String]
             , _fileExt_2 :: String
             } deriving (Generic, Eq, Ord)

instance SafeCopy File_2 where version = 2
