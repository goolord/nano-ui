{-# LANGUAGE BangPatterns #-}

module NanoUI.Plot.Decimate
  ( lttb
  , minMaxDecimate
  ) where

import Data.Ord (comparing)
import Data.Vector (Vector)
import qualified Data.Vector as V

-- | Largest-Triangle-Three-Buckets sampling. Returns at most the requested
-- number of points in input order, preserving both endpoints for budgets >= 2.
lttb :: Int -> Vector (Double, Double) -> Vector (Double, Double)
lttb k0 pts
  | k0 <= 0 = V.empty
  | n <= k0 = pts
  | k0 == 1 = V.take 1 pts
  | k0 == 2 = V.fromList [V.head pts, V.last pts]
  | otherwise =
      let !k = k0
          !bucketSize = fromIntegral (n - 2) / (fromIntegral (k - 2) :: Double)
          !firstPt = pts V.! 0
          !lastPt = pts V.! (n - 1)
          go acc !i !prevIdx
            | i >= k - 2 =
                V.fromListN k (reverse (lastPt : acc))
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
                    !initIdx = rangeStart
                    !initArea = if rangeStart < rangeEnd
                                  then triArea prevPt (pts V.! initIdx) (avgX, avgY)
                                  else 0
                    !best = if rangeStart < rangeEnd
                              then findBest (rangeStart + 1) initIdx initArea
                              else rangeStart
                 in go (pts V.! best : acc) (i + 1) best
       in go [firstPt] 0 0
  where
    !n = V.length pts

bucketAvg :: Vector (Double, Double) -> Int -> Int -> (Double, Double)
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
minMaxDecimate :: Int -> Vector (Double, Double) -> Vector (Double, Double)
minMaxDecimate k pts
  | k <= 0 = V.empty
  | len <= k = pts
  | otherwise = V.concatMap extrema (V.generate numChunks chunk)
  where
    !len = V.length pts
    bucket = (len - 1) `div` k + 1
    numChunks = (len - 1) `div` bucket + 1
    chunk i = V.slice (i * bucket) (min bucket (len - i * bucket)) pts
    extrema xs =
      let lo = V.minIndexBy (comparing snd) xs
          hi = V.maxIndexBy (comparing snd) xs
       in if lo == hi
            then V.singleton (xs V.! lo)
            else V.fromList [xs V.! min lo hi, xs V.! max lo hi]
