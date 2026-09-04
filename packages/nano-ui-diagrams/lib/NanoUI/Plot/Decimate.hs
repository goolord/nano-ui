{-# LANGUAGE BangPatterns #-}

module NanoUI.Plot.Decimate
  ( lttb
  , minMaxDecimate
  ) where

import Data.Bits ((.&.))
import Data.Vector (Vector)
import qualified Data.Vector as V

lttb :: Int -> Vector (Double, Double) -> Vector (Double, Double)
lttb k0 pts
  | n <= k0 = pts
  | k0 < 3 = V.take k0 pts
  | otherwise =
      let !k = k0
          !bucketSize = fromIntegral (n - 2) / (fromIntegral (k - 2) :: Double)
          !firstPt = pts V.! 0
          !lastPt = pts V.! (n - 1)
          go acc !i !prevIdx
            | i >= k - 1 =
                V.fromList (reverse (lastPt : acc))
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
       in V.cons firstPt (go [] 1 0)
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

minMaxDecimate :: Int -> Vector (Double, Double) -> Vector (Double, Double)
minMaxDecimate k pts
  | len <= k = pts
  | otherwise =
      let !bucket = max 1 (len `div` k)
          !numChunks = (len + bucket - 1) `div` bucket
          findMinMax !start !end =
            let !firstPt = pts V.! start
                !firstY = snd firstPt
                go !i !loPt !hiPt !loY !hiY
                  | i + 4 <= end =
                      let !p0 = pts V.! i
                          !p1 = pts V.! (i + 1)
                          !p2 = pts V.! (i + 2)
                          !p3 = pts V.! (i + 3)
                          !y0 = snd p0
                          !y1 = snd p1
                          !y2 = snd p2
                          !y3 = snd p3
                          (!l0, !ly0) = if y0 < loY then (p0, y0) else (loPt, loY)
                          (!h0, !hy0) = if y0 > hiY then (p0, y0) else (hiPt, hiY)
                          (!l1, !ly1) = if y1 < ly0 then (p1, y1) else (l0, ly0)
                          (!h1, !hy1) = if y1 > hy0 then (p1, y1) else (h0, hy0)
                          (!l2, !ly2) = if y2 < ly1 then (p2, y2) else (l1, ly1)
                          (!h2, !hy2) = if y2 > hy1 then (p2, y2) else (h1, hy1)
                          (!l3, !ly3) = if y3 < ly2 then (p3, y3) else (l2, ly2)
                          (!h3, !hy3) = if y3 > hy2 then (p3, y3) else (h2, hy2)
                       in go (i + 4) l3 h3 ly3 hy3
                  | i >= end = (loPt, hiPt)
                  | otherwise =
                      let !pt = pts V.! i
                          !y = snd pt
                          (!l', !ly') = if y < loY then (pt, y) else (loPt, loY)
                          (!h', !hy') = if y > hiY then (pt, y) else (hiPt, hiY)
                       in go (i + 1) l' h' ly' hy'
             in go (start + 1) firstPt firstPt firstY firstY
       in V.generate (numChunks * 2) $ \idx ->
            let !chunkIdx = idx `div` 2
                !isHi = (idx .&. 1) /= 0
                !start = chunkIdx * bucket
                !end = min len (start + bucket)
                (!lo, !hi) = findMinMax start end
             in if isHi then hi else lo
  where
    !len = V.length pts
