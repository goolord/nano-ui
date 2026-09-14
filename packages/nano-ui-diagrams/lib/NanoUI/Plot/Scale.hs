{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Scale
  ( domainToPlot
  , plotToDomain
  , niceTicks
  , formatTick
  , domainExtent
  , domainExtentBy
  , mergeDomains
  , padDomain
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Plot.Types (Domain (..), Range (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Vector.Generic as GV

domainToPlot :: Domain -> Range -> Double -> Double
domainToPlot (Domain lo hi) (Range rLo rHi) v =
  let dSpan = max 1e-9 (hi - lo)
      t = (v - lo) / dSpan
   in rLo + t * (rHi - rLo)

plotToDomain :: Domain -> Range -> Double -> Double
plotToDomain (Domain lo hi) (Range rLo rHi) v =
  let dSpan = max 1e-9 (hi - lo)
      t = (v - rLo) / max 1e-9 (rHi - rLo)
   in lo + t * dSpan

-- Works with Data.Vector, Data.Vector.Unboxed, or Data.Vector.Storable
domainExtent :: (GV.Vector v Double) => v Double -> Domain
domainExtent = domainExtentBy id
{-# INLINE domainExtent #-}

-- | Project while reducing, without materialising a mapped numeric vector.
domainExtentBy :: GV.Vector v a => (a -> Double) -> v a -> Domain
domainExtentBy project xs = case GV.uncons xs of
  Nothing -> Domain 0 1
  Just (first, rest) ->
    -- Single-pass fold to find both min and max simultaneously
    let !initial = project first
        !(!lo, !hi) = GV.foldl' (\(!mn, !mx) value -> let !x = project value in (min mn x, max mx x))
                               (initial, initial)
                               rest
     in if lo == hi
          then Domain (lo - 0.5) (hi + 0.5)
           else Domain lo hi
{-# INLINE domainExtentBy #-}

mergeDomains :: Domain -> Domain -> Domain
mergeDomains (Domain a b) (Domain c d) = Domain (min a c) (max b d)

padDomain :: Double -> Domain -> Domain
padDomain frac (Domain lo hi) =
  let dSpan = max 1e-9 (hi - lo)
      pad = dSpan * frac
   in Domain (lo - pad) (hi + pad)

finite :: Double -> Bool
finite x = not (isNaN x || isInfinite x)

-- Heckbert-style nice tick step.
niceStep :: Double -> Double
niceStep raw =
  let exp10 = floor (logBase 10 raw) :: Int
      f = raw / (10 ** fromIntegral exp10)
      nf
        | f <= 1 = 1
        | f <= 2 = 2
        | f <= 5 = 5
        | otherwise = 10
   in nf * (10 ** fromIntegral exp10)

niceTicks :: Int -> Domain -> [Double]
niceTicks maxTicks (Domain lo hi)
  | maxTicks <= 0 || not (finite lo && finite hi) || hi < lo = []
  | lo == hi = [lo]
  | not (finite dSpan) = []
  | not (finite step) || step <= 0 || not (finite (lo / step)) = []
  | otherwise = go start 0 []
  where
    dSpan = max 1e-9 (hi - lo)
    step = niceStep (dSpan / fromIntegral (max 2 maxTicks))
    start = fromIntegral (ceiling (lo / step - 1e-9) :: Integer) * step
    go !v !count acc
      | not (finite v) || v > hi + step * 0.001 = reverse acc
      | otherwise =
          let !accepted = v >= lo - step * 0.001
              acc' = if accepted then v : acc else acc
              !next = v + step
           in if count >= maxTicks || next <= v
                then reverse acc'
                else go next (count + 1) acc'

formatTick :: Double -> Text
formatTick v
  | not (finite v) = T.empty
  | otherwise =
      let snapped = snapNoise v
       in if abs snapped >= 1e6 || (abs snapped > 0 && abs snapped < 1e-6)
            then render (TB.formatRealFloat TB.Exponent (Just 3) snapped)
            else
              let n = round snapped :: Integer
               in if abs (snapped - fromIntegral n) < 1e-6
                    then render (TB.decimal n)
                    else stripZeros (render (TB.formatRealFloat TB.Fixed (Just 6) snapped))
  where
    render = TL.toStrict . TB.toLazyText

snapNoise :: Double -> Double
snapNoise v =
  let s = 1e10
      scaled = v * s
   in if finite scaled then fromIntegral (round scaled :: Integer) / s else v

stripZeros :: Text -> Text
stripZeros s =
  let t = T.dropWhileEnd (== '0') s
   in case T.unsnoc t of
        Just (rest, '.') -> rest
        _ -> t
