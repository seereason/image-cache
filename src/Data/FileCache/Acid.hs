{-# LANGUAGE DeriveLift, LambdaCase, OverloadedStrings, PackageImports, RecordWildCards, TemplateHaskell, TupleSections, TypeOperators, UndecidableInstances #-}

module Data.FileCache.Acid
  ( initCacheMap
  , openCache
  , PutValue(..)
  , PutValues(..)
  , LookValue(..)
  , LookValues(..)
  , LookMap(..)
  , DeleteValue(..)
  , DeleteValues(..)
  , Replace(..)
  ) where

import Control.Lens ( view, (%=), At(at) )
import Control.Monad.RWS ( MonadState(put) )
import Control.Monad.Reader ( MonadReader(ask) )
import Data.Acid ( openLocalStateFrom, makeAcidic, AcidState, Query, Update )
import Data.FileCache.CacheMap ( CacheMap(CacheMap) )
import Data.FileCache.Common ( ImageKey, ImageFile, FileError )
import Data.Generics.Product ( field )
import Data.Map.Strict as Map ( Map, union, delete, difference, intersection, fromSet, insert )
import Data.Set as Set ( Set )

-- * Events

-- | Install a key/value pair into the cache.
putValue ::
     ImageKey
  -> Either FileError ImageFile
  -> Update CacheMap ()
putValue key img = do
  field @"_unCacheMap" %= Map.insert key img

-- | Install several key/value pairs into the cache.
putValues :: Map ImageKey (Either FileError ImageFile) -> Update CacheMap ()
putValues pairs = do
  field @"_unCacheMap" %= Map.union pairs

-- | Look up a key.
lookValue :: ImageKey -> Query CacheMap (Maybe (Either FileError ImageFile))
lookValue key = view (field @"_unCacheMap" . at key)

-- | Look up several keys.
lookValues :: Set ImageKey -> Query CacheMap (Map ImageKey (Either FileError ImageFile))
lookValues keys = Map.intersection <$> view (field @"_unCacheMap") <*> pure (Map.fromSet (const ()) keys)

-- | Return the entire cache.  (Despite what ghc says, this constraint
-- isn't redundant, without it the makeAcidic call has a missing Ord
-- key instance.)
lookMap :: Query CacheMap CacheMap
lookMap = ask

-- | Remove values from the database.
deleteValue :: ImageKey -> Update CacheMap ()
deleteValue key = field @"_unCacheMap" %= Map.delete key

deleteValues :: Set ImageKey -> Update CacheMap ()
deleteValues keys = field @"_unCacheMap" %= (`Map.difference` (Map.fromSet (const ()) keys))

replace :: CacheMap  -> Update CacheMap ()
replace = put

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues, 'replace])

openCache :: FilePath -> IO (AcidState CacheMap)
openCache path = openLocalStateFrom path initCacheMap

initCacheMap :: CacheMap
initCacheMap = CacheMap mempty
