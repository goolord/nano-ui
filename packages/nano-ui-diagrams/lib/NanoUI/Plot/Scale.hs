{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Scale
  ( domainToPlot
  , plotToDomain
  , niceTicks
  , formatTick
  , domainExtent
  , mergeDomains
  , padDomain
  ) where

import Data.List (dropWhileEnd)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Plot.Types (Domain (..), Range (..))
import Numeric (showEFloat, showFFloat)
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
domainExtent xs = case GV.uncons xs of
  Nothing -> Domain 0 1
  Just (first, rest) ->
    -- Single-pass fold to find both min and max simultaneously
    let !(!lo, !hi) = GV.foldl' (\(!mn, !mx) !x -> (min mn x, max mx x))
                               (first, first)
                               rest
     in if lo == hi
          then Domain (lo - 0.5) (hi + 0.5)
          else Domain lo hi

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
niceTicks maxTicks (Domain lo hi) =
  if not (finite lo && finite hi)
    then []
    else
      let dSpan = max 1e-9 (hi - lo)
          rawStep = dSpan / fromIntegral (max 2 maxTicks)
          step = niceStep rawStep
          start = fromIntegral (ceiling (lo / step - 1e-9) :: Integer) * step
          go v acc
            | v > hi + step * 0.001 = reverse acc
            | length acc >= maxTicks + 1 = reverse acc
            | v >= lo - step * 0.001 = go (v + step) (v : acc)
            | otherwise = go (v + step) acc
       in go start []

formatTick :: Double -> Text
formatTick v
  | not (finite v) = T.empty
  | otherwise =
      let snapped = snapNoise v
       in if abs snapped >= 1e6 || (abs snapped > 0 && abs snapped < 1e-6)
            then T.pack (showEFloat (Just 3) snapped "")
            else
              let n = round snapped :: Integer
               in if abs (snapped - fromIntegral n) < 1e-6
                    then T.pack (show n)
                    else T.pack (stripZeros (showFFloat (Just 6) snapped ""))

snapNoise :: Double -> Double
snapNoise v =
  let s = 1e10
   in fromIntegral (round (v * s) :: Integer) / s

stripZeros :: String -> String
stripZeros s =
  let t = dropWhileEnd (== '0') s
   in case reverse t of
        '.' : rest -> reverse rest
        _ -> t
