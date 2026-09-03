{-# LANGUAGE BangPatterns #-}

module NanoUI.Plot.Decimate
  ( lttb
  , minMaxDecimate
  ) where

import Data.Vector (Vector)
import Data.Vector qualified as V

lttb :: Int -> Vector (Double, Double) -> Vector (Double, Double)
lttb k0 pts
  | V.length pts <= k0 = pts
  | k0 < 3 = V.take k0 pts
  | otherwise =
      let k = k0
          n = V.length pts
          bucketSize = fromIntegral (n - 2) / (fromIntegral (k - 2) :: Double)
          firstPt = pts V.! 0
          lastPt = pts V.! (n - 1)
          go acc i prevIdx
            | i >= k - 1 =
                V.fromList (reverse (lastPt : acc))
            | otherwise =
                let rangeStart = floor (fromIntegral i * bucketSize) + 1
                    rangeEnd = min (n - 1) (floor (fromIntegral (i + 1) * bucketSize) + 1)
                    avgStart = min (n - 1) (floor (fromIntegral (i + 1) * bucketSize) + 1)
                    avgEnd = min n (floor (fromIntegral (i + 2) * bucketSize) + 1)
                    (avgX, avgY) = bucketAvg pts avgStart avgEnd
                    candidates = [rangeStart .. max rangeStart (rangeEnd - 1)]
                    bestIdx = case candidates of
                      j : js ->
                        foldl
                          ( \best j' ->
                              if triArea (pts V.! prevIdx) (pts V.! j') (avgX, avgY)
                                > triArea (pts V.! prevIdx) (pts V.! best) (avgX, avgY)
                                then j'
                                else best
                          )
                          j
                          js
                      [] -> rangeStart
                 in go (pts V.! bestIdx : acc) (i + 1) bestIdx
       in V.cons firstPt (go [] 1 0)

bucketAvg :: Vector (Double, Double) -> Int -> Int -> (Double, Double)
bucketAvg pts start end
  | start >= end = (0, 0)
  | otherwise =
      let len = end - start
          slice = V.slice start len pts
          denom = fromIntegral len :: Double
       in (V.sum (V.map fst slice) / denom, V.sum (V.map snd slice) / denom)

triArea :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
triArea (x0, y0) (x1, y1) (x2, y2) =
  abs ((x0 - x2) * (y1 - y0) - (x0 - x1) * (y2 - y0)) * 0.5

minMaxDecimate :: Int -> Vector (Double, Double) -> Vector (Double, Double)
minMaxDecimate k pts
  | V.length pts <= k = pts
  | otherwise =
      let bucket = max 1 (V.length pts `div` k)
       in V.fromList
            ( concat
                [ [loPt, hiPt]
                | chunk <- chunksOf bucket (V.toList pts)
                , not (null chunk)
                , let (loPt, hiPt) = minMaxPair chunk
                ]
            )

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (h, t) = splitAt n xs in h : chunksOf n t

minMaxPair :: [(Double, Double)] -> ((Double, Double), (Double, Double))
minMaxPair (x : xs) =
  let lo = foldl (\a b -> if snd a <= snd b then a else b) x xs
      hi = foldl (\a b -> if snd a >= snd b then a else b) x xs
   in (lo, hi)
minMaxPair [] = ((0, 0), (0, 0))
