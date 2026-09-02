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

domainToPlot :: Domain -> Range -> Double -> Double
domainToPlot (Domain lo hi) (Range rLo rHi) v =
  let dSpan = max 1e-9 (hi - lo)
      t = (v - lo) / dSpan
   in rLo + t * (rHi - rLo)

plotToDomain :: Domain -> Range -> Double -> Double
plotToDomain dom@(Domain lo _) rng v =
  let dSpan = max 1e-9 (domainSpan dom)
      t = (v - rangeLo rng) / max 1e-9 (rangeSpan rng)
   in lo + t * dSpan

domainSpan :: Domain -> Double
domainSpan (Domain lo hi) = hi - lo

rangeLo :: Range -> Double
rangeLo (Range lo _) = lo

rangeSpan :: Range -> Double
rangeSpan (Range lo hi) = hi - lo

domainExtent :: [Double] -> Domain
domainExtent [] = Domain 0 1
domainExtent [x] = Domain (x - 0.5) (x + 0.5)
domainExtent xs =
  let lo = minimum xs
      hi = maximum xs
   in if lo == hi then Domain (lo - 0.5) (hi + 0.5) else Domain lo hi

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
      nf =
        if f <= 1
          then 1
          else
            if f <= 2
              then 2
              else
                if f <= 5
                  then 5
                  else 10
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
  | abs v >= 1000 || (abs v > 0 && abs v < 0.001) = T.pack (show v)
  | abs (v - fromIntegral (round v :: Integer)) < 1e-9 = T.pack (show (round v :: Integer))
  | otherwise =
      let s = show v
          trimmed = dropWhileEnd (== '0') (dropWhileEnd (/= '.') s)
       in T.pack (case reverse trimmed of
            '.' : rest -> reverse rest
            _ -> trimmed)
