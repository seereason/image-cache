{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Data.FileCache.Rational
  (
    -- * Rational
    (%)
  , fromRat
  , approx
  , rationalIso
  , rationalPrism
  , readRationalMaybe
  , showRational
  , rsqrt
  , micro
  ) where

import Control.Lens (Iso', iso, preview, Prism', prism', review)
import Control.Monad.Fail (MonadFail)
import Data.Fixed (E6, Fixed, showFixed)
import Data.ListLike (fromString, toString)
import Data.Monoid ((<>))
import Data.Ratio (approxRational, denominator, numerator, Ratio)
import qualified Data.Ratio ((%))
import Data.Text (pack, Text, strip)
import GHC.Stack (HasCallStack)
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ()
import Numeric (readSigned, readFloat, showSigned, showFFloat)
import qualified Numeric (fromRat)
import SeeReason.Log (compactStack, getStack)
import Web.Routes (PathInfo(..))

-- * Rational

-- | Wrapper for (%) with the Debug attribute
(%) :: (Integral a, HasCallStack) => a -> a -> Ratio a
_ % d | d == 0 = error (compactStack getStack)
n % d = n Data.Ratio.% d

-- | Wrapper for (%) with the Debug attribute
fromRat :: (RealFloat a, HasCallStack) => Rational -> a
fromRat r | denominator r == 0 = error ("fromRat " <> show r <> " (" <> compactStack getStack <> ")")
fromRat r = Numeric.fromRat r

-- | Simplify the ratio to avoid a long representation:
--
-- > λ> toRational 0.123456
-- > 8895942329546431 % 72057594037927936
-- > λ> approxRational (toRational 0.123456) (1 % 10000)
-- > 10 % 81
-- > λ> 10 / 81
-- > 0.12345679012345678   (wow, that's wierd)
--
-- This is important for values that might become part of a path,
-- we don't want them to be too long or subject to rounding errors.
approx :: Rational -> Rational
approx x = approxRational x (1 % 10000)

rationalIso :: Iso' (Either Text Rational) Text
rationalIso = iso (either id (review rationalPrism)) (\text -> maybe (Left text) Right (preview rationalPrism text))

-- | Every Rational has a Text equivalent, some Text strings have a
-- Rational equivalant.  'readShowLens' is not a good choice for
-- rational numbers, because it only understands strings like "15 % 4",
-- not "15" or "3.5".  Trims whitespace from strings.
rationalPrism :: Prism' Text Rational
-- rationalPrism = iso (toString . strip) fromString . prism' showRational readRationalMaybe
rationalPrism = prism' (fromString . showRational) (readRationalMaybe . toString . strip)

-- | Show a rational using decimal notation.  May lose precision.
showRational :: Rational -> String
showRational x = showSigned (showFFloat Nothing) 0 (fromRat x :: Double) ""

-- | Read a rational in decimal notation.
readRationalMaybe :: MonadFail m => String -> m Rational
readRationalMaybe s =
    case (map fst $ filter (null . snd) $ readSigned readFloat s) of
      [r] -> return r
      [] -> fail $ "readRationalMaybe " ++ s
      _rs -> fail $ "readRationalMaybe " ++ s

rsqrt :: Rational -> Rational
rsqrt = toRational . (sqrt :: Double -> Double) . fromRat

#if 0
-- Danger, not a proper Iso.
instance View Rational where
  type ViewType Rational = Text
  _View = rationalIso . iso pack unpack
#endif

instance PathInfo Rational where
  toPathSegments r = toPathSegments (numerator r) <> toPathSegments (denominator r)
  fromPathSegments = (%) <$> fromPathSegments <*> fromPathSegments

-- mapRatio :: (Integral a, Integral b) => (a -> b) -> Ratio a -> Ratio b
-- mapRatio f r = f (numerator r) % f (denominator r)

-- | Convert a real to 'Micro' and render as 'Text'
micro :: (Real a, Fractional a) => a -> Text
micro x = pack (showFixed True (realToFrac (x + 0.5e-6) :: Fixed E6))
