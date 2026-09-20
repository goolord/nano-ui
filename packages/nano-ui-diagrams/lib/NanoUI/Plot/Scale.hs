-- | Mapping between data domains and plot coordinates, tick placement and
-- labels, and domain extents.
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
import NanoUI.Plot.Types (Domain (..))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TB
import qualified Data.Text.Lazy.Builder.Int as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB
import qualified Data.Vector.Generic as GV

-- | Map a data value to plot coordinates without clamping. For a domain span
-- of at least 1e-9, the lower bound maps to 0 and the upper bound to 1.
-- Smaller or reversed spans use a denominator of 1e-9.
domainToPlot :: Domain -> Double -> Double
domainToPlot (Domain lo hi) v = (v - lo) / max 1e-9 (hi - lo)

-- | Inverse of 'domainToPlot'.
plotToDomain :: Domain -> Double -> Double
plotToDomain (Domain lo hi) v = lo + v * max 1e-9 (hi - lo)

-- | Bounds of a numeric vector. Empty input gives 0-1; constant input gets
-- half a unit of padding on each side. Values must be finite.
domainExtent :: (GV.Vector v Double) => v Double -> Domain
domainExtent = domainExtentBy id
{-# INLINE domainExtent #-}

-- | 'domainExtent' after a projection, without allocating a mapped vector.
domainExtentBy :: GV.Vector v a => (a -> Double) -> v a -> Domain
domainExtentBy project xs
  | lo > hi = Domain 0 1
  | lo == hi = Domain (lo - 0.5) (hi + 0.5)
  | otherwise = extent
  where
    extent@(Domain lo hi) =
      GV.foldl' (\(Domain mn mx) value -> let !x = project value in Domain (min mn x) (max mx x)) (Domain (1 / 0) (-1 / 0)) xs
{-# INLINE domainExtentBy #-}

-- | Smallest domain containing both inputs.
mergeDomains :: Domain -> Domain -> Domain
mergeDomains (Domain a b) (Domain c d) = Domain (min a c) (max b d)

-- | Extend each end by a fraction of the span, using at least 1e-9 for the span.
-- A fraction of 0.05 adds 5% at each end.
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

-- | Readable ticks using 1, 2, or 5 times a power of ten. The requested count
-- guides spacing; endpoints can add a tick. Invalid domains or non-positive
-- counts return an empty list, and a constant domain returns its one value.
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

-- | Format a finite tick with trailing zeros removed, using scientific notation
-- for very small or large magnitudes. Non-finite values produce empty text.
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
