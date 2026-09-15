{-# LANGUAGE BangPatterns #-}

module NanoUI.Diagrams.Tessellation
  ( triangulatePolygon
  , fillPolygon
  , strokePolyline
  , flattenCubic
  , bezierTolerance
  ) where

import Control.Monad.ST (runST)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
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
stripClosed (p : rest)
  | p == last rest = p : init rest
  | otherwise = p : rest

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

pointInTri :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
pointInTri p a b c =
  let sign (p1, p2, p3) = cross (diff p1 p3) (diff p2 p3)
      d1 = sign (p, a, b)
      d2 = sign (p, b, c)
      d3 = sign (p, c, a)
   in not ((d1 < 0 || d2 < 0 || d3 < 0) && (d1 > 0 || d2 > 0 || d3 > 0))

earClip :: U.Vector (Float, Float) -> [((Float, Float), (Float, Float), (Float, Float))]
earClip vs
  | U.length vs < 3 = []
  | U.length vs == 3 = [(vs U.! 0, vs U.! 1, vs U.! 2)]
  | otherwise = runST $ do
      let !n = U.length vs
          !ccw = signedArea vs >= 0
          at = (vs U.!)
      -- Coordinates never move. Remove an ear by relinking two neighbours,
      -- instead of copying the remaining coordinate vector at every step.
      prevs <- U.thaw (U.generate n (\i -> (i - 1 + n) `mod` n))
      nexts <- U.thaw (U.generate n (\i -> (i + 1) `mod` n))
      let triangle i = do
            p <- M.read prevs i
            q <- M.read nexts i
            pure (p, q, (at p, at i, at q))
          isEarAt first count i p q (a, b, c)
            | not (isConvex ccw a b c) = pure False
            | otherwise = outside first count
            where
              outside !_ 0 = pure True
              outside !j !left
                | j /= p && j /= i && j /= q && pointInTri (at j) a b c = pure False
                | otherwise = do
                    next <- M.read nexts j
                    outside next (left - 1)
          convex !_ 0 = pure True
          convex !i !left = do
            (_, q, (a, b, c)) <- triangle i
            if isConvex ccw a b c then convex q (left - 1) else pure False
          fan origin i left
            | left <= 0 = pure []
            | otherwise = do
                q <- M.read nexts i
                rest <- fan origin q (left - 1)
                pure ((at origin, at i, at q) : rest)
          go !first !count !idx !tries tris
            | count == 3 = do
                second <- M.read nexts first
                third <- M.read nexts second
                pure ((at first, at second, at third) : tris)
            | tries >= count = do
                isConvexRing <- convex first count
                if isConvexRing then do
                  second <- M.read nexts first
                  rest <- fan first second (count - 2)
                  pure (tris ++ rest)
                else pure tris
            | otherwise = do
                (p, q, tri) <- triangle idx
                ear <- isEarAt first count idx p q tri
                if ear then do
                  M.write nexts p q
                  M.write prevs q p
                  let !first' = if idx == first then q else first
                  go first' (count - 1) first' 0 (tri : tris)
                else go first count q (tries + 1) tris
      reverse <$> go 0 n 0 0 []

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
      !vPts = U.fromList pts
      !n = U.length vPts
   in if n < 2
        then []
        else
          let !segCount = if closed then n else n - 1
              !segNormals = U.generate segCount $ \i ->
                let !(p0x, p0y) = vPts U.! i
                    !(p1x, p1y) = vPts U.! ((i + 1) `mod` n)
                    dx = p1x - p0x
                    dy = p1y - p0y
                    nx = -dy
                    ny = dx
                    d = sqrt (nx * nx + ny * ny)
                 in if d <= 1e-9 then (0, 0) else (nx / d, ny / d)
              joinNormal !i
                | not closed && i <= 0 = segNormals U.! 0
                | not closed && i >= n - 1 = segNormals U.! (segCount - 1)
                | otherwise =
                    let (ax, ay) = segNormals U.! ((i - 1 + segCount) `mod` segCount)
                        (bx, by) = segNormals U.! (i `mod` segCount)
                        sx = ax + bx
                        sy = ay + by
                        d = sqrt (sx * sx + sy * sy)
                     in if d <= 1e-9 then (0, 0) else (sx / d, sy / d)
              -- Adjacent quads share a vertex, so offset each vertex once.
              !offsets = U.generate n $ \i ->
                let (!px, !py) = vPts U.! i
                    (!nx, !ny) = joinNormal i
                 in ((px + hw * nx, py + hw * ny), (px - hw * nx, py - hw * ny))
              buildQuads !i
                | i >= segCount = []
                | otherwise =
                    let !j = if closed then (i + 1) `mod` n else i + 1
                        ((!x0a, !y0a), (!x0b, !y0b)) = offsets U.! i
                        ((!x1a, !y1a), (!x1b, !y1b)) = offsets U.! j
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
