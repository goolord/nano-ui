-- | Point reduction for long series: largest-triangle-three-buckets and
-- per-bucket minimum and maximum.
module NanoUI.Plot.Decimate
  ( lttb
  , minMaxDecimate
  ) where

import Control.Monad.ST (runST)
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic as V
import qualified Data.Vector.Generic.Mutable as MV
import qualified Data.Vector.Unboxed as U

-- | Largest-Triangle-Three-Buckets sampling. Returns at most the requested
-- number of points in input order, preserving both endpoints for budgets >= 2.
{-# INLINABLE lttb #-}
{-# SPECIALIZE lttb :: Int -> U.Vector (Double, Double) -> U.Vector (Double, Double) #-}
lttb :: Vector v (Double, Double) => Int -> v (Double, Double) -> v (Double, Double)
lttb k0 pts
  | k0 <= 0 = V.empty
  | n <= k0 = pts
  | k0 == 1 = V.take 1 pts
  | k0 == 2 = V.fromList [V.head pts, V.last pts]
  | otherwise = V.create $ do
      out <- MV.new k0
      let !k = k0
          !bucketSize = fromIntegral (n - 2) / (fromIntegral (k - 2) :: Double)
          !firstPt = pts V.! 0
          !lastPt = pts V.! (n - 1)
          go !i !prevIdx
            | i >= k - 2 = MV.write out (k - 1) lastPt
            | otherwise =
                let !rangeStart = floor (fromIntegral i * bucketSize) + 1
                    !rangeEnd = min (n - 1) (floor (fromIntegral (i + 1) * bucketSize) + 1)
                    !avgStart = rangeEnd
                    !avgEnd = min n (floor (fromIntegral (i + 2) * bucketSize) + 1)
                    (!avgX, !avgY) = bucketAvg pts avgStart avgEnd
                    !prevPt = pts V.! prevIdx
                    findBest !j !bestIdx !bestArea
                      | j >= rangeEnd = bestIdx
                      | otherwise =
                          let !area = triArea prevPt (pts V.! j) (avgX, avgY)
                           in if area > bestArea
                                then findBest (j + 1) j area
                                else findBest (j + 1) bestIdx bestArea
                    -- An empty range keeps rangeStart; areas are never negative.
                    !best = findBest rangeStart rangeStart (-1)
                 in do
                   MV.write out (i + 1) (pts V.! best)
                   go (i + 1) best
      MV.write out 0 firstPt
      go 0 0
      pure out
  where
    !n = V.length pts

{-# INLINE bucketAvg #-}
bucketAvg :: Vector v (Double, Double) => v (Double, Double) -> Int -> Int -> (Double, Double)
bucketAvg pts !start !end
  | start >= end = (0, 0)
  | otherwise =
      let !len = end - start
          !denom = fromIntegral len :: Double
          go !i !sx !sy
            | i >= end = (sx / denom, sy / denom)
            | otherwise =
                let (!x, !y) = pts V.! i
                 in go (i + 1) (sx + x) (sy + y)
       in go start 0 0

triArea :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
triArea (!x0, !y0) (!x1, !y1) (!x2, !y2) =
  abs ((x0 - x2) * (y1 - y0) - (x0 - x1) * (y2 - y0)) * 0.5

-- | Split into at most @k@ buckets and retain each bucket's Y extrema in
-- input order. The output has at most @2*k@ points; non-positive budgets
-- return an empty vector. A point selected as both extrema is emitted once.
{-# INLINABLE minMaxDecimate #-}
{-# SPECIALIZE minMaxDecimate :: Int -> U.Vector (Double, Double) -> U.Vector (Double, Double) #-}
minMaxDecimate :: Vector v (Double, Double) => Int -> v (Double, Double) -> v (Double, Double)
minMaxDecimate k pts
  | k <= 0 = V.empty
  | len <= k = pts
  | otherwise = runST $ do
      out <- MV.new (min len (2 * numChunks))
      let chunks !start !written
            | start >= len = V.freeze (MV.slice 0 written out)
            | otherwise = do
                let !end = start + min bucket (len - start)
                    extrema !i !lowIdx !highIdx
                      | i >= end = (lowIdx, highIdx)
                      | otherwise =
                          let !y = snd (pts V.! i)
                              !lo' = if y < snd (pts V.! lowIdx) then i else lowIdx
                              !hi' = if y > snd (pts V.! highIdx) then i else highIdx
                           in extrema (i + 1) lo' hi'
                    (!lo, !hi) = extrema (start + 1) start start
                MV.write out written (pts V.! min lo hi)
                if lo == hi
                  then chunks end (written + 1)
                  else do
                    MV.write out (written + 1) (pts V.! max lo hi)
                    chunks end (written + 2)
      chunks 0 0
  where
    !len = V.length pts
    bucket = (len - 1) `div` k + 1
    numChunks = (len - 1) `div` bucket + 1
