{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -Wredundant-constraints -fno-warn-orphans #-}

module Data.FileCache.Acid
    ( initCacheMap
    , openCache
    , withCache
    -- * Cached map events
    , PutValue(..)
    , PutValues(..)
    , LookValue(..)
    , LookValues(..)
    , LookMap(..)
    , DeleteValue(..)
    , DeleteValues(..)
    ) where

import Data.Map.Strict as Map (Map)
import Data.SafeCopy -- (deriveSafeCopy, extension, Migrate(..), SafeCopy)
import Control.Lens ((%=), at, view)
import Control.Monad.Catch (bracket, {-MonadCatch,-} MonadMask)
import Control.Monad.Reader (MonadReader(ask))
import Data.Acid (AcidState, makeAcidic, openLocalStateFrom, Query, Update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Generics (Typeable)
import Data.FileCache.Cache (CacheMap(..), CacheValue(..))
import Data.Generics.Product (field)
import Data.Map.Strict as Map (delete, difference, fromSet, insert, intersection, union)
import Data.Set as Set (Set)
import Extra.Except (liftIOError, MonadIOError)

-- | Install a key/value pair into the cache.
putValue :: Ord key => key -> CacheValue err val -> Update (CacheMap key val err) ()
putValue key img = field @"_unCacheMap" %= Map.insert key img

-- | Install several key/value pairs into the cache.
putValues :: Ord key => Map key (CacheValue err val) -> Update (CacheMap key val err) ()
putValues pairs = field @"_unCacheMap" %= Map.union pairs

-- | Look up a key.
lookValue :: Ord key => key -> Query (CacheMap key val err) (Maybe (CacheValue err val))
lookValue key = view (field @"_unCacheMap" . at key)

-- | Look up several keys.
lookValues :: Ord key => Set key -> Query (CacheMap key val err) (Map key (CacheValue err val))
lookValues keys = Map.intersection <$> view (field @"_unCacheMap") <*> pure (Map.fromSet (const ()) keys)

-- | Return the entire cache.  (Despite what ghc says, this constraint
-- isn't redundant, without it the makeAcidic call has a missing Ord
-- key instance.)
lookMap :: {-Ord key =>-} Query (CacheMap key val err) (CacheMap key val err)
lookMap = ask

-- | Remove values from the database.
deleteValue :: (Ord key{-, Serialize key, Serialize val, Serialize e-}) => key -> Update (CacheMap key val err) ()
deleteValue key = field @"_unCacheMap" %= Map.delete key

deleteValues :: Ord key => Set key -> Update (CacheMap key val err) ()
deleteValues keys = field @"_unCacheMap" %= (`Map.difference` (Map.fromSet (const ()) keys))

initCacheMap :: Ord key => CacheMap key val err
initCacheMap = CacheMap mempty

openCache :: (SafeCopy key, Typeable key, Ord key,
              SafeCopy err, Typeable err,
              SafeCopy val, Typeable val) => FilePath -> IO (AcidState (CacheMap key val err))
openCache path = openLocalStateFrom path initCacheMap

-- | In theory the MonadError type e1 might differ from the error type
-- stored in the map e2.  But I'm not sure if it would work in practice.
withCache :: (MonadIOError e m, MonadMask m,
              SafeCopy val, Typeable val, SafeCopy err, Typeable err,
              Ord key, Typeable key, SafeCopy key) => FilePath -> (AcidState (CacheMap key val err) -> m b) -> m b
withCache path f = bracket (liftIOError (openCache path)) (liftIOError . createCheckpointAndClose) $ f

$(makeAcidic ''CacheMap ['putValue, 'putValues, 'lookValue, 'lookValues, 'lookMap, 'deleteValue, 'deleteValues])
