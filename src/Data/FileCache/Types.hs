-- | Maintain a cache of key/value pairs in acid state, where the
-- values are monadically obtained from the keys using the 'build'
-- method of the MonadCache instance, and stored using acid-state.

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
    ( -- * Open cache
      CacheMap(..)
    , CacheValue(..){-, _InProgress, _Cached, _Failed-}
    , Checksum
    , FileSource(..), fileSource, fileChksum, fileMessages, fileExt
    , File(..)
    , fileURI
    , filePath
    , fileDir
    , addMessage
    , md5'
    ) where

import Control.Lens (makeLenses, over, view)
import Control.Lens.Path (HOP(FIELDS), makePathInstances)
import qualified Data.ByteString.Lazy.Char8 as Lazy ( fromChunks )
#ifdef LAZYIMAGES
import qualified Data.ByteString.Lazy as P
#else
import qualified Data.ByteString as P
#endif
import Data.Data (Data)
import Data.Digest.Pure.MD5 ( md5 )
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

data CacheValue err val
    = InProgress
    | Cached val
    | Failed err
    deriving (Generic, Eq, Ord, Functor)

-- Later we could make FileError a type parameter, but right now its
-- tangled with the MonadError type.
data CacheMap key val err =
    CacheMap {_unCacheMap :: Map key (CacheValue err val)}
    deriving (Generic, Eq, Ord)

#if 1
$(deriveSafeCopy 1 'base ''CacheValue)
instance (Ord key, SafeCopy key, SafeCopy val, SafeCopy err) => SafeCopy (CacheMap key val err) where
      putCopy (CacheMap a)
        = contain
            (do safeput <- getSafePut
                safeput a
                return ())
      getCopy
        = contain
            ((label "Appraisal.AcidCache.CacheMap:")
               (do safeget <- getSafeGet @(Map key (CacheValue err val))
                   (return CacheMap <*> safeget)))
      version = 2
      kind = extension
      errorTypeName _ = "Appraisal.AcidCache.CacheMap"
#else
instance (SafeCopy err, SafeCopy val) => SafeCopy (CacheValue err val) where version = 1
instance (Ord key, SafeCopy key, SafeCopy val, SafeCopy err) => SafeCopy (CacheMap key val err) where
  version = 2
  kind = extension
#endif

instance (Ord key, SafeCopy key, SafeCopy val) => Migrate (CacheMap key val err) where
    type MigrateFrom (CacheMap key val err) = Map key val
    migrate mp = CacheMap (fmap Cached mp)

{-
$(concat <$>
  sequence
  [ makePrisms ''CacheValue
  , makeLenses ''CacheMap
  ])
-}

deriving instance (Data err, Data val) => Data (CacheValue err val)
deriving instance (Ord key, Data key, Data val, Data err) => Data (CacheMap key val err)
deriving instance (Show err, Show val) => Show (CacheValue err val)
deriving instance (Show key, Show val, Show err) => Show (CacheMap key val err)

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

$(makeLenses ''File)

instance SafeCopy FileSource where version = 1
instance SafeCopy File where version = 2
instance Serialize FileSource where get = safeGet; put = safePut
instance Serialize File where get = safeGet; put = safePut

-- |Return the remote URI if the file resulted from downloading a URI.
fileURI :: File -> Maybe URI
fileURI file = case view fileSource file of
                 Just (TheURI uri) -> maybe (parseRelativeReference uri) Just (parseURI uri)
                 _ -> Nothing

-- |Add a message to the file message list.
addMessage :: String -> File -> File
addMessage message file = over fileMessages (++ [message]) file

md5' :: P.ByteString -> String
#ifdef LAZYIMAGES
md5' = show . md5
#else
md5' = show . md5 . Lazy.fromChunks . (: [])
#endif

filePath :: File -> FilePath
filePath file = fileDir file <++> view fileChksum file <> view fileExt file

fileDir :: File -> FilePath
fileDir file = take 2 (view fileChksum file)

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
