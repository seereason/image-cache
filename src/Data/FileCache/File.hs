{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.File
  ( File(..)
  , FileSource(..)
  , Checksum
  , Extension
  , fileURI
  , filePath
  , fileDir
  , addMessage
  , md5'
  ) where

import Control.Lens (over)
import Control.Lens.Path (HOP(FIELDS), makePathInstances)
import qualified Data.ByteString.Lazy.Char8 as Lazy ( fromChunks )
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Data (Data)
import Data.Digest.Pure.MD5 ( md5 )
import Data.Generics.Product (field)
import Data.Serialize (Serialize(..))
import Data.SafeCopy -- (deriveSafeCopy, extension, Migrate(..), SafeCopy)
import Data.Text as T (pack, Text, unpack)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Haskell.TH.Lift as TH (Lift)
import Network.URI ( URI(..), parseRelativeReference, parseURI )
import System.FilePath (makeRelative, (</>))
#if ARBITRARY
import Test.QuickCheck ( Arbitrary(..), oneof )
#endif
import Text.PrettyPrint.HughesPJClass ( Pretty(pPrint), text )

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           , _fileExt :: Extension               -- ^ Name is formed by appending this to checksum
           } deriving (Generic, Eq, Ord)

instance Migrate File where
  type MigrateFrom File = File_2
  migrate (File_2 src cksum msgs ext) =
    File {_fileSource = src, _fileChksum = pack cksum, _fileMessages = msgs, _fileExt = pack ext}

-- |A local cache of a file obtained from a 'FileSource'.
data File_2
    = File_2 { _fileSource_2 :: Maybe FileSource
             , _fileChksum_2 :: String
             , _fileMessages_2 :: [String]
             , _fileExt_2 :: String
             } deriving (Generic, Eq, Ord)

-- | A type to represent a checksum which (unlike MD5Digest) is an instance of Data.
type Checksum = Text
type Extension = Text

instance Pretty File where
    pPrint (File _ cksum _ ext) = text ("File(" <> show (cksum <> ext) <> ")")
instance SafeCopy File_2 where version = 2
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
    deriving (Generic, Eq, Ord)

instance SafeCopy FileSource where version = 1
instance Serialize FileSource where get = safeGet; put = safePut
deriving instance Show FileSource
deriving instance Read FileSource
deriving instance Data FileSource
deriving instance Typeable FileSource
deriving instance Lift FileSource

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
filePath file = fileDir file <++> unpack (_fileChksum file) <> unpack (_fileExt file)

(<++>) :: FilePath -> FilePath -> FilePath
a <++> b = a </> (makeRelative "" b)

fileDir :: File -> FilePath
fileDir file = take 2 (unpack (_fileChksum file))

$(concat <$>
  sequence
  [ makePathInstances [FIELDS] ''File
  , makePathInstances [FIELDS] ''FileSource ])

#if ARBITRARY
instance Arbitrary File where
    arbitrary = File <$> arbitrary <*> arbitrary <*> pure [] <*> arbitrary

instance Arbitrary FileSource where
    arbitrary = oneof [TheURI <$> arbitrary, ThePath <$> arbitrary]
#endif
