{-# LANGUAGE BangPatterns #-}

module NanoUI.Diagrams.Tessellation
  ( triangulatePolygon
  , fillPolygon
  , strokePolyline
  , flattenCubic
  , bezierTolerance
  ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import NanoUI (Color, DrawOp (..), Rect (..))

bezierTolerance :: Float
bezierTolerance = 0.5

triangulatePolygon :: [(Float, Float)] -> [((Float, Float), (Float, Float), (Float, Float))]
triangulatePolygon [] = []
triangulatePolygon [_] = []
triangulatePolygon pts0 =
  let pts = stripClosed pts0
    in earClip (U.fromList pts)

stripClosed :: [(Float, Float)] -> [(Float, Float)]
stripClosed [] = []
stripClosed [p] = [p]
stripClosed (p : rest) =
  case reverse rest of
    q : _ | p == q -> p : init rest
    _ -> p : rest

signedArea :: U.Vector (Float, Float) -> Float
signedArea vs =
  U.ifoldl' (\acc i a -> acc + cross a (vs U.! ((i + 1) `mod` U.length vs)) / 2) 0 vs

cross :: (Float, Float) -> (Float, Float) -> Float
cross (x0, y0) (x1, y1) = x0 * y1 - x1 * y0

diff :: (Float, Float) -> (Float, Float) -> (Float, Float)
diff (x0, y0) (x1, y1) = (x1 - x0, y1 - y0)

isConvex :: Bool -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
isConvex ccw a b c =
  let ab = diff a b
      bc = diff b c
   in if ccw then cross ab bc >= 0 else cross ab bc <= 0

-- Fan-fill leftover only when every vertex turns the same way. A concave
-- remainder fanned from vertex 0 can cover area outside the polygon.
leftoverConvex :: Bool -> U.Vector (Float, Float) -> Bool
leftoverConvex ccw vs =
  let n = U.length vs
      at i = vs U.! (i `mod` n)
   in n >= 3 && U.ifoldr (\i cur rest -> isConvex ccw (at (i - 1)) cur (at (i + 1)) && rest) True vs

pointInTri :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
pointInTri p a b c =
  let sign (p1, p2, p3) = cross (diff p1 p3) (diff p2 p3)
      d1 = sign (p, a, b)
      d2 = sign (p, b, c)
      d3 = sign (p, c, a)
   in not ((d1 < 0 || d2 < 0 || d3 < 0) && (d1 > 0 || d2 > 0 || d3 > 0))

isEar :: Bool -> U.Vector (Float, Float) -> Int -> Bool
isEar ccw vs i
  | i < 0 || i >= n = False
  | otherwise =
      let prevIdx = (i - 1 + n) `mod` n
          nextIdx = (i + 1) `mod` n
          prev = vs U.! prevIdx
          cur = vs U.! i
          next = vs U.! nextIdx
          outside j p rest =
            (j == prevIdx || j == i || j == nextIdx || not (pointInTri p prev cur next)) && rest
       in isConvex ccw prev cur next && U.ifoldr outside True vs
  where
    n = U.length vs

earClip :: U.Vector (Float, Float) -> [((Float, Float), (Float, Float), (Float, Float))]
earClip vs
  | U.length vs < 3 = []
  | U.length vs == 3 = [(vs U.! 0, vs U.! 1, vs U.! 2)]
  | otherwise =
      let ccw = signedArea vs >= 0
          go remaining idx tries tris
            | nRem < 3 = tris
            | nRem == 3 = (remaining U.! 0, remaining U.! 1, remaining U.! 2) : tris
            | tries >= nRem =
                if nRem >= 3 && leftoverConvex ccw remaining
                  then
                    let origin = remaining U.! 0
                     in tris
                          ++ [ (origin, remaining U.! i, remaining U.! (i + 1))
                             | i <- [1 .. nRem - 2]
                             ]
                  else tris
            | isEar ccw remaining idx =
                let prev = remaining U.! ((idx - 1 + nRem) `mod` nRem)
                    cur = remaining U.! idx
                    next = remaining U.! ((idx + 1) `mod` nRem)
                    newRem = U.take idx remaining U.++ U.drop (idx + 1) remaining
                 in go newRem 0 0 ((prev, cur, next) : tris)
            | otherwise = go remaining ((idx + 1) `mod` nRem) (tries + 1) tris
            where
              nRem = U.length remaining
       in reverse (go vs 0 0 [])

fillPolygon :: Color -> [(Float, Float)] -> [DrawOp]
fillPolygon col pts =
  case axisAlignedRect pts of
    Just r -> [FillRect r col]
    Nothing ->
      [ FillTriangle x0 y0 x1 y1 x2 y2 col
      | ((x0, y0), (x1, y1), (x2, y2)) <- triangulatePolygon pts
      ]

axisAlignedRect :: [(Float, Float)] -> Maybe Rect
axisAlignedRect pts =
  case stripClosed pts of
    [(x0, y0), (x1, y1), (x2, y2), (x3, y3)]
      | near y0 y1 && near x1 x2 && near y2 y3 && near x3 x0 ->
          Just (Rect (min x0 x3) (min y0 y2) (abs (x1 - x0)) (abs (y2 - y0)))
    [(x0, y0), (x1, y1), (x2, y2), (x3, y3)]
      | near x0 x1 && near y1 y2 && near x2 x3 && near y3 y0 ->
          Just (Rect (min x0 x2) (min y0 y1) (abs (x2 - x0)) (abs (y1 - y0)))
    _ -> Nothing
  where
    near a b = abs (a - b) <= 1e-3


strokePolyline :: Color -> Float -> Bool -> [(Float, Float)] -> [DrawOp]
strokePolyline _ _ _ [] = []
strokePolyline _ _ _ [_] = []
strokePolyline col w closed pts0 =
  let pts = if closed && length pts0 > 2 then stripClosed pts0 else pts0
      hw = w / 2
      !n = length pts
   in if n < 2
        then []
        else
          let !vPts = V.fromList pts
              !segCount = if closed then n else n - 1
              !segNormals = V.generate segCount $ \i ->
                let !(p0x, p0y) = vPts V.! i
                    !(p1x, p1y) = vPts V.! ((i + 1) `mod` n)
                    dx = p1x - p0x
                    dy = p1y - p0y
                    nx = -dy
                    ny = dx
                    d = sqrt (nx * nx + ny * ny)
                 in if d <= 1e-9 then (0, 0) else (nx / d, ny / d)
              joinNormal !i
                | not closed && i <= 0 = segNormals V.! 0
                | not closed && i >= n - 1 = segNormals V.! (segCount - 1)
                | otherwise =
                    let (ax, ay) = segNormals V.! ((i - 1 + segCount) `mod` segCount)
                        (bx, by) = segNormals V.! (i `mod` segCount)
                        sx = ax + bx
                        sy = ay + by
                        d = sqrt (sx * sx + sy * sy)
                     in if d <= 1e-9 then (0, 0) else (sx / d, sy / d)
              offset !i =
                let (!px, !py) = vPts V.! i
                    (!nx, !ny) = joinNormal i
                 in ((px + hw * nx, py + hw * ny), (px - hw * nx, py - hw * ny))
              buildQuads !i
                | i >= segCount = []
                | otherwise =
                    let !j = if closed then (i + 1) `mod` n else i + 1
                        ((!x0a, !y0a), (!x0b, !y0b)) = offset i
                        ((!x1a, !y1a), (!x1b, !y1b)) = offset j
                     in FillTriangle x0a y0a x1a y1a x1b y1b col
                          : FillTriangle x0a y0a x1b y1b x0b y0b col
                          : buildQuads (i + 1)
           in buildQuads 0

flattenCubic ::
  (Float, Float) ->
  (Float, Float) ->
  (Float, Float) ->
  (Float, Float) ->
  [(Float, Float)]
flattenCubic p0 c1 c2 p1 = go p0 c1 c2 p1
  where
    go a b c d =
      let mid (p, q) = ((fst p + fst q) / 2, (snd p + snd q) / 2)
          ab = mid (a, b)
          bc = mid (b, c)
          cd = mid (c, d)
          abbc = mid (ab, bc)
          bccd = mid (bc, cd)
          mid12 = mid (abbc, bccd)
          flat =
            let (dx, dy) = diff d a
                len = sqrt (dx * dx + dy * dy)
                dist =
                  if len <= 1e-9
                    then 0
                    else abs (cross (diff b a) (dx, dy)) / len
             in dist <= bezierTolerance
       in if flat
            then [a, d]
            else
              let rest = go mid12 bccd cd d
               in go a ab abbc mid12 ++ drop 1 rest
