{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.FileError
  ( -- * FileError
    FileError(..)
  , CommandError
  , HasFileError(fileError)
--  , logErrorCall
  , FileCacheErrors
  , FileCacheErrors2
  , FileCacheErrors3
  ) where

import Control.Exception as E ( Exception, ErrorCall, IOException )
import Control.Lens ( Prism' )
import Data.FileCache.CommandError ( CommandError )
import Data.FileCache.ImageKey ( ImageKey )
import Data.FileCache.ImageFile ( ImageReady )
import Data.SafeCopy ( base, safeGet, safePut, Migrate(..), SafeCopy(version, kind) )
import Data.Serialize ( Serialize(..) )
import Data.String ( IsString(fromString) )
import Data.Text ( Text )
import Data.Typeable ( Typeable )
import SeeReason.Errors ( Member, NonIOException, OneOf )
import Extra.Except ( MonadError, HasErrorCall(..) )
import GHC.Generics ( Generic )
--import Language.Haskell.TH.Instances ()
import UnexceptionalIO.Trans ( Unexceptional )

-- * FileError, CommandInfo

data FileError
    = IOException IOError -- ^ Caught an IOException
    | ErrorCall E.ErrorCall -- ^ Caught a call to error
    | FromString String -- ^ FileError created via IsString(fromstring)
    | UnexpectedException String
      -- ^ Something unanticipated, not an IOException.  Because we
      -- derive Eq we can't put a SomeException here, so its a string.
    | CommandFailure CommandError -- ^ A shell command failed
    | CacheDamageMigrated -- ^ A CacheDamage value was migrated
    | MissingOriginalEntry ImageKey -- ^ An original image is missing from the cache
    | MissingOriginalFile ImageKey FilePath -- ^ An original image file is missing
    | MissingDerivedEntry ImageKey -- ^ cacheDerivedImagesForeground returned a map without this key
    | DamagedOriginalFile ImageKey FilePath -- ^ An original image had a bad checksum
    | ImageBuildFailure ImageKey -- ^ Some command in the image build failed
    | InvalidJPEG ImageReady
    | ExtractBBFailed FilePath Text
    | InvalidBoundingBox FilePath Text Text
    | UnexpectedPnmfileOutput Text
    -- | CacheDamageUnknownOriginal ImageKey
    | NoShape Text
      -- ^ Could not determine the dimensions of an image.  This comes
      -- from failed attempt to parse the output of the unix file(1)
      -- command, or attempts to scale or edit inappropriate file
      -- types such as pdf.
    deriving (Eq, Ord, Generic)

-- Dubious instance, but omitting makes other things more dubious.
instance IsString FileError where fromString = FromString

instance Exception FileError
instance SafeCopy FileError where version = 3; kind = base
instance Serialize FileError where get = safeGet; put = safePut

--deriving instance Data FileError
--deriving instance Data CommandInfo
deriving instance Show FileError

-- | This ensures that runExceptT catches IOException
--instance HasIOException FileError where ioException = _Ctor @"IOException"
instance HasErrorCall FileError where fromErrorCall = ErrorCall

-- These superclasses are due to types embedded in FileError.
-- they ought to be unbundled and removed going forward.
class HasFileError e where fileError :: Prism' e FileError
instance HasFileError FileError where fileError = id

-- | Constraints typical of the functions in this package.  They occur
-- when an IO operation is lifted into 'Unexceptional' by 'lyftIO',
-- which splits the resulting 'SomeNonPseudoException' into @IO@ and
-- @NonIO@ parts.  It also has FileException, an error type defined in
-- this package.

type FileCacheErrors e m =  (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable (OneOf e), Typeable e, Member NonIOException e, Member IOException e, Member FileError e)
-- A little looser
type FileCacheErrors2 e m = (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable (OneOf e), Typeable e, Member NonIOException e, Member FileError e)
type FileCacheErrors3 e m = (Unexceptional m, MonadError (OneOf e) m, Show (OneOf e), Typeable (OneOf e), Typeable e, Member NonIOException e, Member IOException e)
