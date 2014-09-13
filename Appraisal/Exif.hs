{-# OPTIONS_GHC -Wall #-}
module Appraisal.Exif
    ( normalizeOrientationCode
    , getEXIFOrientationCode
    ) where

import Control.Exception (ErrorCall, try, evaluate)
import Control.Monad (when)
import Data.ByteString.Lazy (ByteString, pack, unpack, take, drop, concat)
import Data.Binary.Get (getLazyByteString, Get, skip, bytesRead, runGet,
                        getWord16be, getWord32be, getWord16le, getWord32le)
import Data.Word (Word16, Word32)
import GHC.Int (Int64)
import Prelude hiding (take, drop, concat)
import System.Exit (ExitCode(..))
import System.Process (proc, showCommandForUser)
import System.Process.ByteString.Lazy (readCreateProcessWithExitCode)

-- | Given a bytestring containing a JPEG file, examine the EXIF
-- orientation flag and if it is something other than 1 transform the
-- image into the "normal" orientation and change the orientation flag
-- to 1.  The result is the new bytestring.  If the old bytestring was
-- already normalized, or absolutely anything else goes wrong, the
-- result is Nothing.  This means the original bytestring should be used.
normalizeOrientationCode :: ByteString -> IO (Maybe ByteString)
normalizeOrientationCode bs = do
  result <- try (evaluate $ runGet getEXIFOrientationCode bs) :: IO (Either ErrorCall (Int, Int64, Bool))
  case result of
    Right (2, pos, flag) -> transform ["-flip", "horizontal"] pos flag
    Right (3, pos, flag) -> transform ["-rotate", "180"] pos flag
    Right (4, pos, flag) -> transform ["-flip", "vertical"] pos flag
    Right (5, pos, flag) -> transform ["-transpose"] pos flag
    Right (6, pos, flag) -> transform ["-rotate", "90"] pos flag
    Right (7, pos, flag) -> transform ["-transverse"] pos flag
    Right (8, pos, flag) -> transform ["-rotate", "270"] pos flag
    _ -> return Nothing
    -- x -> error $ "Invalid orientation code: " ++ show x
    where
      transform :: [String] -> Int64 -> Bool -> IO (Maybe ByteString)
      transform args pos isMotorola = do
        let cmd = "jpegtran"
            args' = (["-copy", "all"] ++ args)
            hd = take pos bs            -- everything before the orientation code
            flag = pack (if isMotorola then [0x0, 0x1] else [0x1, 0x0]) -- orientation code 1
            tl = drop (pos + 2) bs      -- everything after the orientation code
            bs' = concat [hd, flag, tl]
        (result, out, err) <- readCreateProcessWithExitCode (proc cmd args') bs'
        case result of
          ExitSuccess -> return (Just out)
          ExitFailure n -> error (showCommandForUser cmd args' ++ " -> " ++ show n ++ "\n error output: " ++ show err)

-- | Read the orientation code of a JPEG file, returning its value,
-- the offset of the two byte code in the file, and the "isMotorola"
-- flag which determines something about the endianness.
getEXIFOrientationCode :: Get (Int, Int64, Bool)
getEXIFOrientationCode = do
  getLazyByteString 4 >>= doJPEGHeader
  headerLength <- getWord16be >>= return . markerParameterLength
  getLazyByteString 6 >>= testEXIFHead
  isMotorola <- getLazyByteString 2 >>= discoverByteOrder
  getLazyByteString 2 >>= checkTagMark isMotorola
  offset <- getWord32Motorola isMotorola >>= testIFDOffset headerLength
  skip (fromIntegral offset - 8)
  numberOfTags <- getWord16Motorola isMotorola >>= testNumberOfTags
  findOrientationTag isMotorola (headerLength - 8) numberOfTags (offset + 2)
    where
      doJPEGHeader :: Monad m => ByteString -> m ()
      doJPEGHeader x = when (unpack x /= [0xff, 0xd8, 0xff, 0xe1] && unpack x /= [0xff, 0xd8, 0xff, 0xe0]) (error $ "Invalid JPEG header: " ++ show (unpack x))

      markerParameterLength :: Word16 -> Int64
      markerParameterLength w
          | w < 8 = error $ "Length field much too short: " ++ show w
          | w < 20 = error $ "Length field too short: " ++ show w
          | otherwise = fromIntegral $ w - 8

      testEXIFHead :: Monad m => ByteString -> m ()
      testEXIFHead x = when (unpack x /= [0x45, 0x78, 0x69, 0x66, 0x0, 0x0]) (error $ "Invalid EXIF header: " ++ show (unpack x))

      discoverByteOrder :: Monad m => ByteString -> m Bool
      discoverByteOrder x =
          case unpack x of
            [0x49, 0x49] -> return True
            [0x4d, 0x4d] -> return False
            s -> error $ "Invalid byte order: " ++ show s

      checkTagMark :: Monad m => Bool -> ByteString -> m ()
      checkTagMark True x = when (unpack x /= [0x2a, 0x0]) (error $ "Invalid tag mark True: " ++ show (unpack x))
      checkTagMark False x = when (unpack x /= [0x0, 0x2a]) (error $ "Invalid tag mark False: " ++ show (unpack x))

      testIFDOffset :: Monad m => Int64 -> Word32 -> m Word32
      testIFDOffset len x = if x > 0xffff || fromIntegral x > len - 2 then error ("Invalid IFD offset: " ++ show x) else return x

      testNumberOfTags :: Monad m => Word16 -> m Word16
      testNumberOfTags n = if n > 0 then return n else error "No tags"

findOrientationTag :: Bool -> Int64 -> Word16 -> Word32 -> Get (Int, Int64, Bool)
findOrientationTag isMotorola headerLength numberOfTags offset = do
    when  (fromIntegral offset > headerLength - 12 || numberOfTags < 1) (error "No orientation tag")
    tagnum <- getWord16Motorola isMotorola
    case tagnum of
      0x0112 -> do
        skip 6
        pos <- bytesRead
        flag <- getWord16Motorola isMotorola
        if flag < 1 || flag > 8 then error "Invalid orientation flag" else return (fromIntegral flag, pos, isMotorola)
      _ -> do
        skip 10
        findOrientationTag isMotorola headerLength (numberOfTags - 1) (offset + 12)

getWord16Motorola :: Bool -> Get Word16
getWord16Motorola True = getWord16le
getWord16Motorola False = getWord16be

getWord32Motorola :: Bool -> Get Word32
getWord32Motorola True = getWord32le
getWord32Motorola False = getWord32be

{-
-- Test

dir = "/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images"

main = do
  names <- getDirectoryContents dir >>= return . filter (not . (`elem` [".", ".."])) . filter (not . isSuffixOf ".jpg")
  mapM_ (uncurry doName) (zip [1..] names)

doName :: Int -> FilePath -> IO ()
doName count path = Data.ByteString.Lazy.readFile (dir </> path) >>= handle (\ (e :: SomeException) -> return ()) . doCode count (dir </> path)

doCode :: Int -> FilePath -> ByteString -> IO ()
doCode count path bs =
    case getEXIFOrientationCode bs of
      (1, _, _) -> return ()
      result -> putStrLn (show count ++ ". " ++ show path ++ ": " ++ show result)

Images with valid orientation codes other than 1:

"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/6aa7ea4bd79f39c3092b6e4251b3b073": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/9624fd5634a5b6257725b42fbad1cbde": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b7e8f8e5cf18926e839b3cf6083d4932": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/62368114d7b71b5c6b6899f92651b12e": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/198ee040f479b85490b92fb2a2804ecf": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/72cc1a950694cc2409c3a8f4bc71301f": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/2a504ec035e8868c2b95e9a7d9c3c69e": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/304837ab54c63738fecf556f0ed0ba2a": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b41611aa15441535aead51769f801620": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/1a4b46c6272d45223cea4b2fd85882ff": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/12b4e042540ca1e1c8499bce228652e3": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0197efa1312688937c7dfe80c1cd5a08": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/34711b918bea81cf6d7f4481b72c017b": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/32004c379d003ca780a6345fa79050c8": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/d0276f1a87d4617bd21664a894daab5a": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b46817cecb5348ef82643b7654e01326": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/f8ed3f915cb9424f20b371f1667b2ecb": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3a391f9f15e49c2d6f506556e08b45dd": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/e855b29fee68fcea34152c8ab6f05df7": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/fe08b904b41a159a469c519e9666325c": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0d27c6db4f72952e134212b5eb56947d": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b31797cf58d461ddf57af90a54bc9bda": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0d423f15b9d129a923c63aac41b652b8": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/7c324add279ed38272ac1a03c490fa9b": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3077253364f0a682d9c654bf0e087246": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/ae344bc17e1c6b92bf783ea7d3adeda3": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/6da21a666fbfd29e16aca6602fdc8478": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b66b7a09b5ea7adcd0b9d4a4fbcb90ae": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3cd5da3d4568095e6c15bd992a676d1c": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/22d628676e6b561f2aaba590dacf63e7": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/9172d8098bdf0c4ff9360801339d632d": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/144177d35e36e0a93e269af46ad01346": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/96a961a0f288e7e05b243889a1d06c14": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/4aa04ca61174a6d5bdfc30278ad04e9a": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/918a82dff691604601f21c4b4932c5bc": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3337b1f7388639d2fc53f74a7bf834fa": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/e39a014def56a031b78c2e5e96d1f7c5": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0bf267e6a880422d4479ef256d0836e9": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/135bf2de2dd4c2fa94ae05f117867b5e": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/5c100432de39d6f4fe911b69523cb22c": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/0aeecd29b06ed4738a72f554288ffc47": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/20a29b7d21ef647cd2e4841cf67656fa": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/c0419800d504d32c51f5c5ed6bb362cb": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/b619922e4856ba6e0f443ffb49d488f6": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/01e6856e1091b02642840a59386236e3": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/1a7f39310a5cfc3c38ab4f58c2d1a688": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/e04b93bf3d3183f8e6073857626fdda6": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/45842965ab1e4032625ca14309ee35b4": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/254f665f816cb484e4feb0d8197fe642": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/ae51018932436f9494ce087e2ec628f5": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/7231ae84a7c45da7f022da2edb22470c": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/4485d40fc73aeb5b3b4a59e56a03cad6": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/d8ab2be3f85b2033ab5ce6e798ee2a6a": (8,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/a41ab6f5b45888755eeae2edcdecb626": (6,54,True)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/dd0d1578040c42da93633b4ed7efd1ff": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/1fba9c9a78ba2e8284c2b7cc804feaa8": (8,54,False)
"/srv/backups/appraisalscribe-production/current/appraisalscribe-production/images/3aeb2f1b415291ab5a90349dc21e2620": (8,54,False)
-}
