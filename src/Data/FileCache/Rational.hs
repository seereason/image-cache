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
    approx
  -- , rationalIso
  -- , rationalLens
  , rationalPrism
  , readRationalMaybe
  , showRational
  , rsqrt
  ) where

import Control.Lens ( Lens', Iso', iso, lens, Prism', prism'  )
import Data.ListLike (fromString, toString)
import Data.Monoid ( (<>) )
import Data.Ratio ( (%), approxRational, denominator, numerator )
import Data.Text ( Text, pack, strip, unpack )
import Language.Haskell.TH.Instances ()
import Language.Haskell.TH.Lift as TH ()
import Numeric ( fromRat, readSigned, readFloat, showSigned, showFFloat )
import Web.Routes ( PathInfo(..) )

-- * Rational

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

#if 0
rationalLens :: Lens' Rational String
rationalLens = lens showRational (\r s -> either (const r) id (readRationalMaybe s))
#endif

#if 0
rationalIso :: Iso' Rational String
rationalIso = iso showRational (readRational 0)
    where
      readRational :: Rational -> String -> Rational
      readRational d = either (const d) id . readRationalMaybe
#endif

-- | Every Rational has a Text equivalent, some Text strings have a
-- Rational equivalant.  'readShowLens' is not a good choice for
-- rational numbers, because it only understands strings like "15 % 4",
-- not "15" or "3.5".  Trims whitespace from strings.
rationalPrism :: Prism' Text Rational
rationalPrism = iso (toString . strip) fromString . prism' showRational readRationalMaybe

-- | Show a rational using decimal notation.  May lose precision.
showRational :: Rational -> String
showRational x = showSigned (showFFloat Nothing) 0 (fromRat x :: Double) ""

-- | Read a rational in decimal notation.
readRationalMaybe :: Monad m => String -> m Rational
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

