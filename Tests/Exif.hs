{-# LANGUAGE CPP #-}
module Exif (tests) where

import Appraisal.Exif
import Control.Exception (ErrorCall(ErrorCall), try, evaluate)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (pack)
import Data.Binary.Get (Get, runGet)
import GHC.IO.Exception
import GHC.Int (Int64)
import Test.HUnit

test' :: Int -> ByteString -> (Either ErrorCall (Int, Int64, Bool)) -> Test
test' n bs expected = do
  TestCase (try (evaluate (runGet getEXIFOrientationCode bs)) >>= \ r ->
            assertEqual ("Exif test " ++ show n) expected r)

tests :: Test
tests =
    TestList
      [ test' 1 (pack "") (Left (ErrorCall "Data.Binary.Get.runGet at position 0: demandInput: not enough bytes"))
      , test' 7 (pack "abcd") (Left (ErrorCall "Invalid JPEG header: [97,98,99,100]"))
      , test' 2 (pack "\255\216\255\225") (Left (ErrorCall "Data.Binary.Get.runGet at position 4: demandInput: not enough bytes"))
      , test' 3 (pack "\255\216\255\225}\254") (Left (ErrorCall "Data.Binary.Get.runGet at position 6: demandInput: not enough bytes"))
      , test' 8 (pack "\255\216\255\225}\254Exim\NUL\NUL") (Left (ErrorCall "Invalid EXIF header: [69,120,105,109,0,0]"))
      , test' 4 (pack "\255\216\255\225}\254Exif\NUL\NUL") (Left (ErrorCall {-"Invalid byte order: []"-} "Data.Binary.Get.runGet at position 12: demandInput: not enough bytes"))
      , test' 9 (pack "\255\216\255\225}\254Exif\NUL\NULMI") (Left (ErrorCall "Invalid byte order: [77,73]"))
      , test' 5 (pack "\255\216\255\225}\254Exif\NUL\NULII*\NUL\b\NUL\NUL\NUL\r\NUL\SI\SOH\STX\NUL\n\NUL\NUL\NUL\170\NUL\NUL\NUL\DLE\SOH\STX\NUL\t\NUL\NUL\NUL\180\NUL\NUL\NUL\DC2\SOH\ETX\NUL\SOH\NUL\NUL\NUL\b\NUL\NUL\NUL\SUB\SOH\ENQ\NUL\SOH\NUL\NUL\NUL\190\NUL\NUL\NUL\ESC\SOH\ENQ\NUL\SOH\NUL\NUL\NUL\198\NUL\NUL\NUL(\SOH\ETX\NUL\SOH\NUL\NUL\NUL\STX\NUL\NUL\NUL1\SOH\STX\NUL\n\NUL\NUL\NUL\206\NUL\NUL\NUL2\SOH\STX\NUL\DC4\NUL\NUL\NUL\216\NUL\NUL\NUL\DC3\STX\ETX\NUL\SOH\NUL\NUL\NUL\STX\NUL\NUL\NULi\135\EOT\NUL\SOH\NUL\NUL\NUL|\STX\NUL\NUL\165\196\a\NUL\208\NUL\NUL\NUL\236\NUL\NUL\NUL\210\198\a\NUL@\NUL\NUL\NUL\188\SOH\NUL\NUL\211\198\a\NUL\128\NUL\NUL\NUL\252\SOH\NUL\NUL\FS*\NUL\NULPanasonic\NULDMC-FZ47\NUL\NUL\180\NUL\NUL\NUL\SOH\NUL\NUL\NUL\180\NUL\NUL\NUL\SOH\NUL\NUL\NULVer.1.0  \NUL2013:11:01 12:09:38\NULPrintIM\NUL0250\NUL\NUL\SO\NUL\SOH\NUL\SYN\NUL\SYN\NUL\STX\NUL\NUL\NUL\NUL\NUL\ETX\NULd\NUL\NUL\NUL\a\NUL\NUL\NUL\NUL\NUL\b\NUL\NUL\NUL\NUL\NUL\t\NUL\NUL\NUL\NUL\NUL\n\NUL\NUL\NUL\NUL\NUL\v\NUL\172\NUL\NUL\NUL\f\NUL\NUL\NUL\NUL\NUL\r\NUL\NUL\NUL\NUL\NUL\SO\NUL\196\NUL\NUL\NUL\NUL\SOH\ENQ\NUL\NUL\NUL\SOH\SOH\SOH\NUL\NUL\NUL") (Right (8,54,True))
      , test' 6 (pack "\255\216\255\224\NUL\DLEJFIF\NUL\SOH\SOH\NUL\NUL\SOH\NUL\SOH\NUL\NUL\255\219\NULC\NUL\b\ACK\ACK\a\ACK\ENQ\b\a\a\a\t\t\b\n\f\DC4\r\f\v\v\f\EM\DC2\DC3\SI\DC4\GS\SUB\US\RS\GS\SUB\FS\FS $.' \",#\FS\FS(7),01444\US'9=82<.342\255\219\NULC\SOH\t\t\t\f\v\f\CAN\r\r\CAN2!\FS!22222222222222222222222222222222222222222222222222\255\192\NUL\DC1\b\NULd\NUL\133\ETX\SOH\"\NUL\STX\DC1\SOH\ETX\DC1\SOH\255\196\NUL\US\NUL\NUL\SOH\ENQ\SOH\SOH\SOH\SOH\SOH\SOH\NUL\NUL\NUL\NUL\NUL\NUL\NUL\NUL\SOH\STX\ETX\EOT\ENQ\ACK\a\b\t\n\v\255\196\NUL\181\DLE\NUL\STX\SOH\ETX\ETX\STX\EOT\ETX\ENQ\ENQ\EOT\EOT\NUL\NUL\SOH}\SOH\STX\ETX\NUL\EOT\DC1\ENQ\DC2!1A\ACK\DC3Qa\a\"q\DC42\129\145\161\b#B\177\193\NAKR\209\240$3br\130\t\n\SYN\ETB\CAN\EM\SUB%&'()*456789:CDEFGHIJSTUVWXYZcdefghijstuvwxyz\131\132\133\134\135\136\137\138\146\147\148\149\150\151\152\153\154\162\163\164\165\166\167\168\169\170\178\179\180\181\182\183\184\185\186\194\195\196\197\198\199\200\201\202\210\211\212\213\214\215\216\217\218\225\226\227\228\229\230\231\232\233\234\241\242\243\244\245\246\247\248\249\250\255\196\NUL\US\SOH\NUL\ETX\SOH\SOH\SOH\SOH\SOH\SOH") (Left (ErrorCall "Invalid EXIF header: [74,70,73,70,0,1]")) ]
