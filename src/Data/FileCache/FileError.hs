{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.FileCache.FileError
  ( -- * FileError
    FileError(..)
  , CommandError
  , HasFileError(fileError)
  , MyMonadIO, MyIOErrors
  , MonadFileIO
  , E, fromE
  , runFileIOT
  ) where

import Control.Exception as E ( Exception, ErrorCall, IOException )
import Control.Lens ( Prism' )
import Control.Lens.Path ( Value(..) )
import Control.Monad.IO.Class (MonadIO)
import Data.FileCache.CommandError ( CommandError )
import Data.FileCache.ImageFile (ImageReady)
import Data.FileCache.ImageKey (FileType, ImageKey)
import Data.SafeCopy ( base, extension, Migrate(..), safeGet, safePut, SafeCopy(version, kind) )
import Data.Serialize ( Serialize(..) )
import Data.String ( IsString(fromString) )
import Data.Text ( Text )
import Extra.Except ( ExceptT, MonadError, HasErrorCall(..), runExceptT )
import SeeReason.Errors as Errors ( Member, OneOf(..), oneOf, put1)
import GHC.Generics ( Generic )

-- * FileError, CommandInfo

data FileError_3
    = IOException_3 IOError -- ^ Caught an IOException
    | ErrorCall_3 E.ErrorCall -- ^ Caught a call to error
    | FromString_3 String -- ^ FileError created via IsString(fromstring)
    | UnexpectedException_3 String
      -- ^ Something unanticipated, not an IOException.  Because we
      -- derive Eq we can't put a SomeException here, so its a string.
    | CommandFailure_3 CommandError -- ^ A shell command failed
    | CacheDamageMigrated_3 -- ^ A CacheDamage value was migrated
    | MissingOriginalEntry_3 ImageKey -- ^ An original image is missing from the cache
    | MissingOriginalFile_3 ImageKey FilePath -- ^ An original image file is missing
    | MissingDerivedEntry_3 ImageKey -- ^ cacheDerivedImagesForeground returned a map without this key
    | DamagedOriginalFile_3 ImageKey FilePath -- ^ An original image had a bad checksum
    | ImageBuildFailure_3 ImageKey -- ^ Some command in the image build failed
    | InvalidJPEG_3 ImageReady
    | ExtractBBFailed_3 FilePath Text
    | InvalidBoundingBox_3 FilePath Text Text
    | UnexpectedPnmfileOutput_3 Text
    -- | CacheDamageUnknownOriginal ImageKey
    | NoShape_3 Text
      -- ^ Could not determine the dimensions of an image.  This comes
      -- from failed attempt to parse the output of the unix file(1)
      -- command, or attempts to scale or edit inappropriate file
      -- types such as pdf.
    deriving (Eq, Ord, Generic)

instance SafeCopy FileError_3 where version = 3; kind = base
instance Serialize FileError_3 where get = safeGet; put = safePut
instance Migrate FileError where
  type MigrateFrom FileError = FileError_3
  migrate (IOException_3 a) = IOException a
  migrate (ErrorCall_3 a) = ErrorCall a
  migrate (FromString_3 a) = FromString a
  migrate (UnexpectedException_3 a) = UnexpectedException a
  migrate (CommandFailure_3 a) = CommandFailure a
  migrate CacheDamageMigrated_3 = CacheDamageMigrated
  migrate (MissingOriginalEntry_3 key) = MissingOriginalEntry key
  migrate (MissingOriginalFile_3 key path) = MissingOriginalFile key path
  migrate (MissingDerivedEntry_3 key) = MissingDerivedEntry key
  migrate (DamagedOriginalFile_3 key path) = DamagedOriginalFile key path
  migrate (ImageBuildFailure_3 key) = ImageBuildFailure key
  migrate (InvalidJPEG_3 a) = InvalidJPEG a
  migrate (ExtractBBFailed_3 path t) = ExtractBBFailed path t
  migrate (InvalidBoundingBox_3 path t1 t2) = InvalidBoundingBox path t1 t2
  migrate (UnexpectedPnmfileOutput_3 t) = UnexpectedPnmfileOutput t
  migrate (NoShape_3 t) = NoShapeOld t

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
    | NoShapeFromKey ImageKey
    | NoShapeFromPath FilePath Text
    | NoShapeOld Text
      -- ^ Could not determine the dimensions of an image.  This comes
      -- from failed attempt to parse the output of the unix file(1)
      -- command, or attempts to scale or edit inappropriate file
      -- types such as pdf.
    | CannotScale FileType
    | CannotCrop FileType
    deriving (Eq, Ord, Generic)

-- Dubious instance, but omitting makes other things more dubious.
instance IsString FileError where fromString = FromString

instance Exception FileError
instance SafeCopy FileError where version = 4; kind = extension
instance Serialize FileError where get = safeGet; put = safePut

--deriving instance Data FileError
--deriving instance Data CommandInfo
deriving instance Show FileError

instance Value FileError where hops _ = []

-- | This ensures that runExceptT catches IOException
--instance HasIOException FileError where ioException = _Ctor @"IOException"
instance HasErrorCall FileError where fromErrorCall = ErrorCall

-- These superclasses are due to types embedded in FileError.
-- they ought to be unbundled and removed going forward.
class HasFileError e where fileError :: Prism' e FileError
instance HasFileError FileError where fileError = id

instance Member FileError e => HasFileError (OneOf e) where fileError = Errors.oneOf

type MyMonadIO e m = (MonadIO m, MonadError (OneOf e) m, MyIOErrors e)

type MyIOErrors e = (Member IOException e)

-- | Constraints typical of the functions in this package.
type MonadFileIO e m = (MyMonadIO e m, Member FileError e)

type FileIOT e m = ExceptT (OneOf e) m

runFileIOT :: FileIOT e m a -> m (Either (OneOf e) a)
runFileIOT action = runExceptT action

-- | A minimal set of errors to run FileIOT
type E = '[FileError, IOException]

-- | Convert an 'E' into some e
fromE :: forall e. (Member FileError e, MyIOErrors e) => OneOf E -> OneOf e
fromE (Val e) = Errors.put1 (e :: FileError)
fromE (NoVal (Val e)) = Errors.put1 (e :: IOException)
fromE _ = error "Impossible"
