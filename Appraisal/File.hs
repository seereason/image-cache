-- | Our one instance of CacheFile.

{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses,
             ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS -Wall -fno-warn-orphans #-}

module Appraisal.File
    ( File(..)
    ) where

import Appraisal.FileCache
import Control.Lens (view)
import Control.Monad.Trans (liftIO)
import Data.Aeson.TH (deriveJSON)
import Data.Aeson.Types (defaultOptions)
import Data.Generics (Data(..), Typeable)
import Data.Monoid ((<>))
import Data.SafeCopy (deriveSafeCopy, base)
import Language.Haskell.TH.Lift (deriveLiftMany)
import System.Directory (doesFileExist, renameFile)
import System.FilePath.Extra (writeFileReadable)
import System.Log.Logger (logM, Priority(DEBUG))
import System.Process (shell, showCommandForUser)
import System.Process.ListLike (readCreateProcessWithExitCode)
import System.Unix.FilePath ((<++>))
import Text.PrettyPrint.HughesPJClass (Pretty(pPrint), text)

-- |A local cache of a file obtained from a 'FileSource'.
data File
    = File { _fileSource :: Maybe FileSource     -- ^ Where the file's contents came from
           , _fileChksum :: Checksum             -- ^ The checksum of the file's contents
           , _fileMessages :: [String]           -- ^ Messages received while manipulating the file
           } deriving (Show, Read, Eq, Ord, Data, Typeable)

instance Pretty File where
    pPrint (File _ cksum _) = text ("File(" <> show cksum <> ")")

instance CacheFile File where
    fileSource = \f (File x1 x2 x3) -> fmap (\y -> File y x2 x3) (f x1)
    fileChksum = \f (File x1 x2 x3) -> fmap (\y -> File x1 y x3) (f x2)
    fileMessages = \f (File x1 x2 x3) -> fmap (\y -> File x1 x2 y) (f x3)
    fileCachePath file = fileCacheTop >>= \ver -> return $ ver <++> view fileChksum file
    fileFromFile path = do
      cksum <- (\(_, out, _) -> take 32 out) <$> liftIO (readCreateProcessWithExitCode (shell ("md5sum < " ++ showCommandForUser path [])) "")
      let file = File { _fileSource = Just (ThePath path)
                      , _fileChksum = cksum
                      , _fileMessages = [] }
      dest <- fileCachePath file
      liftIO (logM "fileFromFile" DEBUG ("renameFile " <> path <> " " <> dest) >>
              renameFile path dest)
      return file
    fileFromBytes bytes =
      do let file = File { _fileSource = Nothing
                         , _fileChksum = md5' bytes
                         , _fileMessages = [] }
         path <- fileCachePath file
         exists <- liftIO $ doesFileExist path
         case exists of
           True -> return file
           False -> liftIO (writeFileReadable path bytes) >> return file

$(deriveSafeCopy 1 'base ''File)
$(deriveLiftMany [''File])
$(deriveJSON defaultOptions ''File)
