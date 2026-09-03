{-# LANGUAGE BangPatterns #-}

module NanoUI.Diagrams.Tessellation
  ( triangulatePolygon
  , fillPolygon
  , strokePolyline
  , flattenCubic
  , bezierTolerance
  ) where

import NanoUI (Color, DrawOp (..), Rect (..))

bezierTolerance :: Float
bezierTolerance = 0.5

triangulatePolygon :: [(Float, Float)] -> [((Float, Float), (Float, Float), (Float, Float))]
triangulatePolygon [] = []
triangulatePolygon [_] = []
triangulatePolygon pts0 =
  let pts = stripClosed pts0
   in if length pts < 3 then [] else earClip pts

stripClosed :: [(Float, Float)] -> [(Float, Float)]
stripClosed [] = []
stripClosed [p] = [p]
stripClosed (p : rest) =
  case reverse rest of
    q : _ | p == q -> p : init rest
    _ -> p : rest

signedArea :: [(Float, Float)] -> Float
signedArea [] = 0
signedArea vs =
  sum [cross a b / 2 | (a, b) <- take (length vs) (zip vs (drop 1 (cycle vs)))]

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
leftoverConvex :: Bool -> [(Float, Float)] -> Bool
leftoverConvex ccw vs =
  let n = length vs
      at i = vs !! (i `mod` n)
   in n >= 3 && and [isConvex ccw (at (i - 1)) (at i) (at (i + 1)) | i <- [0 .. n - 1]]

pointInTri :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
pointInTri p a b c =
  let sign (p1, p2, p3) = cross (diff p1 p3) (diff p2 p3)
      d1 = sign (p, a, b)
      d2 = sign (p, b, c)
      d3 = sign (p, c, a)
   in not ((d1 < 0 || d2 < 0 || d3 < 0) && (d1 > 0 || d2 > 0 || d3 > 0))

isEar :: Bool -> [(Float, Float)] -> Int -> Bool
isEar ccw vs i
  | i < 0 || i >= n = False
  | otherwise =
      let prev = vs !! ((i - 1 + n) `mod` n)
          cur = vs !! i
          next = vs !! ((i + 1) `mod` n)
          others =
            [ p
            | (j, p) <- zip [0 ..] vs
            , j /= (i - 1 + n) `mod` n
            , j /= i
            , j /= (i + 1) `mod` n
            ]
       in isConvex ccw prev cur next && not (any (pointInTri cur prev next) others)
  where
    n = length vs

earClip :: [(Float, Float)] -> [((Float, Float), (Float, Float), (Float, Float))]
earClip vs
  | length vs < 3 = []
  | length vs == 3 = [(vs !! 0, vs !! 1, vs !! 2)]
  | otherwise =
      let ccw = signedArea vs >= 0
          go remaining idx tries tris
            | nRem < 3 = tris
            | nRem == 3 = (remaining !! 0, remaining !! 1, remaining !! 2) : tris
            | tries >= nRem =
                if nRem >= 3 && leftoverConvex ccw remaining
                  then
                    let origin = remaining !! 0
                     in tris
                          ++ [ (origin, remaining !! i, remaining !! (i + 1))
                             | i <- [1 .. nRem - 2]
                             ]
                  else tris
            | isEar ccw remaining idx =
                let prev = remaining !! ((idx - 1 + nRem) `mod` nRem)
                    cur = remaining !! idx
                    next = remaining !! ((idx + 1) `mod` nRem)
                    newRem = take idx remaining ++ drop (idx + 1) remaining
                 in go newRem 0 0 ((prev, cur, next) : tris)
            | otherwise = go remaining ((idx + 1) `mod` nRem) (tries + 1) tris
            where
              nRem = length remaining
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

norm :: (Float, Float) -> (Float, Float)
norm (x, y) =
  let d = sqrt (x * x + y * y)
   in if d <= 1e-9 then (0, 0) else (x / d, y / d)

perp :: (Float, Float) -> (Float, Float)
perp (x, y) = (-y, x)

addV :: (Float, Float) -> (Float, Float) -> (Float, Float)
addV (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

scaleV :: Float -> (Float, Float) -> (Float, Float)
scaleV s (x, y) = (s * x, s * y)

strokePolyline :: Color -> Float -> Bool -> [(Float, Float)] -> [DrawOp]
strokePolyline _ _ _ [] = []
strokePolyline _ _ _ [_] = []
strokePolyline col w closed pts0 =
  let pts = if closed && length pts0 > 2 then stripClosed pts0 else pts0
      hw = w / 2
      n = length pts
      segCount = if closed then n else n - 1
      segNormals =
        [ norm (perp (diff (pts !! ((i + 1) `mod` n)) (pts !! i))) | i <- [0 .. segCount - 1] ]
      joinNormal i =
        if not closed && (i <= 0 || i >= n - 1)
          then
            if i <= 0
              then segNormals !! 0
              else segNormals !! (segCount - 1)
          else
            let a = segNormals !! ((i - 1 + segCount) `mod` segCount)
                b = segNormals !! (i `mod` segCount)
             in norm (addV a b)
      offset i =
        let (px, py) = pts !! i
            (nx, ny) = scaleV hw (joinNormal i)
         in ((px + nx, py + ny), (px - nx, py - ny))
   in concat
        [ quadFill col (x0a, y0a) (x1a, y1a) (x1b, y1b) (x0b, y0b)
        | i <- [0 .. segCount - 1]
        , let j = if closed then (i + 1) `mod` n else i + 1
              ((x0a, y0a), (x0b, y0b)) = offset i
              ((x1a, y1a), (x1b, y1b)) = offset j
        ]
  where
    quadFill c (x0, y0) (x1, y1) (x2, y2) (x3, y3) =
      [ FillTriangle x0 y0 x1 y1 x2 y2 c
      , FillTriangle x0 y0 x2 y2 x3 y3 c
      ]

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
