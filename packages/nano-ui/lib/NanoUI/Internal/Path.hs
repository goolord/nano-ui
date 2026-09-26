-- | Paths and affine transforms, and the geometry that turns them into what
-- the draw ops take: curves flattened into rings of points, the centre form
-- of an SVG arc, and a ring triangulated for 'FillPolygon'. The canvas
-- ("NanoUI.Path", "NanoUI.Widgets.Custom"), the SVG rasterizer
-- ("NanoUI.Svg") and nano-ui-diagrams share it.
module NanoUI.Internal.Path
  ( -- * Paths
    Path (..)
  , Segment (..)
  , LineCap (..)
    -- * Transforms
  , Transform (..)
  , applyTransform
    -- * Rings
  , Rings (..)
  , ringCount
  , buildRings
  , cleanRings
    -- * Flattening
  , curveTolerance
  , flattenPath
  , cubicPoints
    -- * Triangulation
  , triangulate
    -- * Draw ops
  , fillPathOps
  , strokePathOps
  , transformOp
    -- * Shapes
  , rect
  , roundedRect
  , ellipse
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.Primitive.PrimArray
  ( PrimArray
  , emptyPrimArray
  , generatePrimArray
  , indexPrimArray
  , newPrimArray
  , primArrayFromList
  , readPrimArray
  , runPrimArray
  , setPrimArray
  , shrinkMutablePrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import NanoUI.Internal.Draw.Types (DrawOp (..))
import NanoUI.Internal.Types (Color, Rect (..), V2 (..))

--------------------------------------------------------------------------------
-- Paths
--------------------------------------------------------------------------------

-- | Subpaths of lines, curves and arcs, in the coordinates of whatever
-- draws them. Paths join with '<>', one after the other.
newtype Path = Path [Segment]
  deriving (Eq, Show)
  deriving newtype (Semigroup, Monoid)

-- | One step of a 'Path'. A line, curve or 'SegArcTo' with no subpath open
-- starts one at the current point: where the last subpath closed, or the
-- origin.
data Segment
  = SegMove {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  -- ^ Start a subpath at a point.
  | SegLine {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  -- ^ A straight line to a point.
  | SegQuad
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  -- ^ A quadratic Bezier: control point, end.
  | SegCubic
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  -- ^ A cubic Bezier: two control points, end.
  | SegArc
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  -- ^ An elliptical arc by its centre: centre, radii, the ellipse's
  -- rotation, start angle and sweep, in radians. With a subpath open a line
  -- runs to its start; otherwise a subpath starts there.
  | SegArcTo
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      !Bool
      !Bool
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  -- ^ An SVG arc to a point: radii, the ellipse's rotation in radians, the
  -- large-arc and sweep flags, end.
  | SegClose
  -- ^ Close the subpath back to where it started.
  deriving (Eq, Show)

-- | How a stroke ends an open subpath.
data LineCap
  = ButtCap
  -- ^ Cut square at the end point.
  | SquareCap
  -- ^ Cut square half the width past the end point.
  | RoundCap
  -- ^ A half disc past the end point.
  deriving (Eq, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Transforms
--------------------------------------------------------------------------------

-- | An affine map of the plane, as SVG's @matrix(a b c d e f)@: @(x, y)@
-- goes to @(a x + c y + e, b x + d y + f)@. @s <> t@ applies @t@ first, as
-- function composition does, and 'mempty' is the identity.
data Transform
  = Transform
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
      {-# UNPACK #-} !Float
  deriving (Eq, Show)

instance Semigroup Transform where
  Transform a b c d e f <> Transform a' b' c' d' e' f' =
    Transform
      (a * a' + c * b')
      (b * a' + d * b')
      (a * c' + c * d')
      (b * c' + d * d')
      (a * e' + c * f' + e)
      (b * e' + d * f' + f)

instance Monoid Transform where
  mempty = Transform 1 0 0 1 0 0

-- | Map a point through a transform.
{-# INLINE applyTransform #-}
applyTransform :: Transform -> Float -> Float -> (Float, Float)
applyTransform (Transform a b c d e f) x y = (a * x + c * y + e, b * x + d * y + f)

-- | Every entry is a number: neither NaN nor infinite.
transformFinite :: Transform -> Bool
transformFinite (Transform a b c d e f) = all finite [a, b, c, d, e, f]

{-# INLINE finite #-}
finite :: Float -> Bool
finite v = not (isNaN v || isInfinite v)

-- | The most a transform lengthens anything: the larger singular value of
-- its linear part.
maxStretch :: Transform -> Float
maxStretch (Transform a b c d _ _) =
  let s = a * a + b * b + c * c + d * d
      det = a * d - b * c
   in sqrt ((s + sqrt (max 0 (s * s - 4 * det * det))) / 2)

-- | The square root of the transform's area scale: the scale of a uniform
-- one, and a geometric mean of the axes' scales otherwise.
averageStretch :: Transform -> Float
averageStretch (Transform a b c d _ _) = sqrt (abs (a * d - b * c))

--------------------------------------------------------------------------------
-- Rings
--------------------------------------------------------------------------------

-- | Point lists in flat arrays: ring @i@ is points @starts[i]@ up to
-- @starts[i + 1]@, stored x then y, with a number the builder tagged it with.
data Rings = Rings !(PrimArray Float) !(PrimArray Int) !(PrimArray Int)

-- | How many rings there are.
ringCount :: Rings -> Int
ringCount (Rings _ starts _) = sizeofPrimArray starts - 1

-- | Rings from a walk that calls @point x y@ for each point and @end tag@
-- after each ring's last. The walk runs twice, to size the arrays and then
-- to fill them, so it must visit the same points both times.
{-# INLINE buildRings #-}
buildRings :: (forall s. (Float -> Float -> ST s ()) -> (Int -> ST s ()) -> ST s ()) -> Rings
buildRings walk = runST $ do
  counts <- newPrimArray 2
  setPrimArray counts 0 2 (0 :: Int)
  let bump k = readPrimArray counts k >>= writePrimArray counts k . (+ 1)
  walk (\_ _ -> bump 0) (\_ -> bump 1)
  nPoints <- readPrimArray counts 0
  nRings <- readPrimArray counts 1
  points <- newPrimArray (2 * nPoints)
  starts <- newPrimArray (nRings + 1)
  tags <- newPrimArray nRings
  writePrimArray starts 0 0
  setPrimArray counts 0 2 0
  walk
    ( \x y -> do
        k <- readPrimArray counts 0
        writePrimArray points (2 * k) x
        writePrimArray points (2 * k + 1) y
        writePrimArray counts 0 (k + 1)
    )
    ( \tag -> do
        r <- readPrimArray counts 1
        readPrimArray counts 0 >>= writePrimArray starts (r + 1)
        writePrimArray tags r tag
        writePrimArray counts 1 (r + 1)
    )
  Rings <$> unsafeFreezePrimArray points <*> unsafeFreezePrimArray starts <*> unsafeFreezePrimArray tags

--------------------------------------------------------------------------------
-- Flattening
--------------------------------------------------------------------------------

-- | How far a flattened curve may stray from the true one, in logical
-- pixels, for a display of @scale@ device pixels to the logical one: a
-- quarter of a device pixel.
curveTolerance :: Float -> Float
curveTolerance scale
  | scale > 0 && scale < 1 / 0 = 0.25 / scale
  | otherwise = 0.25

-- | Flatten a path through a transform into rings of points, each tagged 1
-- when 'SegClose' ended it. Curves are split until they are within @tol@ of
-- their chords once transformed, so a scaled-up curve gets more points and
-- a scaled-down one fewer; an arc is cut into @steps r sweep@ chords, @r@
-- its larger radius once transformed ('arcSteps' for the canvas). A segment
-- with a coordinate that is NaN or infinite is skipped, and a transform with
-- one gives no rings.
flattenPath :: Float -> (Float -> Float -> Int) -> Transform -> Path -> Rings
flattenPath tol steps t (Path segs)
  | transformFinite t = buildRings (pathWalk tol steps t segs)
  | otherwise = buildRings (\_ _ -> pure ())

{-# INLINE pathWalk #-}
pathWalk :: Float -> (Float -> Float -> Int) -> Transform -> [Segment] -> (Float -> Float -> ST s ()) -> (Int -> ST s ()) -> ST s ()
pathWalk tol steps t segs0 point end = go segs0 False 0 0 0 0
  where
    emit x y = let (x', y') = applyTransform t x y in point x' y'
    finish open closed = when open (end (if closed then 1 else 0))
    -- A drawing segment with no subpath open starts one at the current point.
    begin open cx cy = unless open (emit cx cy)
    stretch = maxStretch t
    -- The points after the start of an elliptical arc, the last its end
    -- @(x1, y1)@ exactly, so a whole turn closes on its start.
    arcPoints ax ay rx ry rot a0 sweep x1 y1 = do
      let n = steps (stretch * max (abs rx) (abs ry)) sweep
      forM_ [1 .. n - 1] $ \i ->
        uncurry emit (ellipsePoint ax ay rx ry rot (a0 + sweep * fromIntegral i / fromIntegral n))
      emit x1 y1
    cubic x0 y0 x1 y1 x2 y2 x3 y3 = do
      let (tx0, ty0) = applyTransform t x0 y0
          (tx1, ty1) = applyTransform t x1 y1
          (tx2, ty2) = applyTransform t x2 y2
          (tx3, ty3) = applyTransform t x3 y3
      cubicPoints tol point tx0 ty0 tx1 ty1 tx2 ty2 tx3 ty3
    go [] open _ _ _ _ = finish open False
    go (seg : rest) open cx cy sx sy
      | not (segmentFinite seg) = go rest open cx cy sx sy
      | otherwise = case seg of
          SegMove x y -> finish open False >> emit x y >> go rest True x y x y
          SegLine x y -> begin open cx cy >> emit x y >> go rest True x y sx sy
          SegQuad qx qy x y -> do
            begin open cx cy
            -- The quadratic as a cubic, its control points two thirds of
            -- the way from each end to the quadratic's.
            cubic cx cy (cx + 2 / 3 * (qx - cx)) (cy + 2 / 3 * (qy - cy)) (x + 2 / 3 * (qx - x)) (y + 2 / 3 * (qy - y)) x y
            go rest True x y sx sy
          SegCubic x1 y1 x2 y2 x y -> do
            begin open cx cy
            cubic cx cy x1 y1 x2 y2 x y
            go rest True x y sx sy
          SegArc ax ay rx ry rot a0 sweep0 -> do
            -- A sweep past a whole turn draws the whole ellipse once.
            let sweep = max (-2 * pi) (min (2 * pi) sweep0)
                (x0, y0) = ellipsePoint ax ay rx ry rot a0
                (x1, y1)
                  | abs sweep >= 2 * pi = (x0, y0)
                  | otherwise = ellipsePoint ax ay rx ry rot (a0 + sweep)
            -- A line to the arc's start, or the first point of a new subpath.
            emit x0 y0
            arcPoints ax ay rx ry rot a0 sweep x1 y1
            if open then go rest True x1 y1 sx sy else go rest True x1 y1 x0 y0
          SegArcTo rx ry rot large sweepFlag x y -> do
            begin open cx cy
            case arcCentre cx cy rx ry rot large sweepFlag x y of
              Nothing -> emit x y
              Just (ax, ay, arx, ary, a0, sweep) -> arcPoints ax ay arx ary rot a0 sweep x y
            go rest True x y sx sy
          SegClose -> finish open True >> go rest False sx sy sx sy

-- | Every coordinate of the segment is a number.
segmentFinite :: Segment -> Bool
segmentFinite seg = case seg of
  SegMove x y -> finite x && finite y
  SegLine x y -> finite x && finite y
  SegQuad a b x y -> all finite [a, b, x, y]
  SegCubic a b c d x y -> all finite [a, b, c, d, x, y]
  SegArc a b c d e f g -> all finite [a, b, c, d, e, f, g]
  SegArcTo a b c _ _ x y -> all finite [a, b, c, x, y]
  SegClose -> True

-- | The point at angle @a@ on an ellipse: centre, radii (a negative one
-- counting as its size) and rotation.
{-# INLINE ellipsePoint #-}
ellipsePoint :: Float -> Float -> Float -> Float -> Float -> Float -> (Float, Float)
ellipsePoint cx cy rx ry rot a =
  let ex = abs rx * cos a
      ey = abs ry * sin a
      cr = cos rot
      sr = sin rot
   in (cx + cr * ex - sr * ey, cy + sr * ex + cr * ey)

-- | Chords for @sweep@ radians of an ellipse whose larger radius is @r@
-- once transformed, each within @tol@ of the curve: at least one a quarter
-- turn, at most 'maxArcSteps'.
arcSteps :: Float -> Float -> Float -> Int
arcSteps tol r sweep
  | not (abs sweep <= 4 * pi) = maxArcSteps
  | r > tol && ideal < fromIntegral maxArcSteps = max quarters (ceiling ideal)
  | r > tol = maxArcSteps
  | otherwise = quarters
  where
    quarters = max 1 (ceiling (abs sweep / (pi / 2)))
    -- A chord of angle @da@ strays @r (1 - cos (da / 2))@, which is
    -- @2 r sin (da / 4) ^ 2@, from the circle. Solved in that form, since
    -- @1 - tol / r@ rounds to 1 for a radius millions of times the
    -- tolerance, and a nearly straight arc would get every chord allowed.
    ideal = abs sweep / (4 * asin (sqrt (tol / (2 * r))))

-- | The most chords one arc is cut into.
maxArcSteps :: Int
maxArcSteps = 1024

-- | The points after the start of a cubic Bezier from @(x0, y0)@, with
-- control points @(x1, y1)@ and @(x2, y2)@, to @(x3, y3)@. The curve is split
-- in halves until each piece's control points bound it within @tol@ of its
-- chord, at most ten times over.
{-# INLINE cubicPoints #-}
cubicPoints :: Monad m => Float -> (Float -> Float -> m ()) -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> m ()
cubicPoints tol point = go (0 :: Int)
  where
    go depth x0 y0 x1 y1 x2 y2 x3 y3
      | depth >= 10 || flat = point x3 y3
      | otherwise = do
          let mx01 = (x0 + x1) / 2
              my01 = (y0 + y1) / 2
              mx12 = (x1 + x2) / 2
              my12 = (y1 + y2) / 2
              mx23 = (x2 + x3) / 2
              my23 = (y2 + y3) / 2
              mx012 = (mx01 + mx12) / 2
              my012 = (my01 + my12) / 2
              mx123 = (mx12 + mx23) / 2
              my123 = (my12 + my23) / 2
              mx = (mx012 + mx123) / 2
              my = (my012 + my123) / 2
          go (depth + 1) x0 y0 mx01 my01 mx012 my012 mx my
          go (depth + 1) mx my mx123 my123 mx23 my23 x3 y3
      where
        ux = 3 * x1 - 2 * x0 - x3
        uy = 3 * y1 - 2 * y0 - y3
        vx = 3 * x2 - 2 * x3 - x0
        vy = 3 * y2 - 2 * y3 - y0
        flat = max (ux * ux) (vx * vx) + max (uy * uy) (vy * vy) <= 16 * tol * tol

-- | The centre form of an SVG arc from @(x1, y1)@ to @(x2, y2)@ with radii
-- @rx@ and @ry@, the ellipse rotated @phi@ radians, by the conversion in the
-- SVG specification: its centre, radii (scaled up if they cannot reach),
-- start angle and sweep. 'Nothing' when a radius is zero or the ends
-- coincide, for which SVG draws a straight line or nothing, and when radii
-- too large or too small for a 'Float' give a centre that is not a number,
-- which callers draw as a straight line too: an arc on a huge radius is
-- nearly one.
arcCentre :: Float -> Float -> Float -> Float -> Float -> Bool -> Bool -> Float -> Float -> Maybe (Float, Float, Float, Float, Float, Float)
arcCentre x1 y1 rx0 ry0 phi large sweep x2 y2
  | rx0 == 0 || ry0 == 0 || (x1 == x2 && y1 == y2) = Nothing
  | not (all finite [cx, cy, rx, ry, theta1, dtheta]) = Nothing
  | otherwise = Just (cx, cy, rx, ry, theta1, dtheta)
  where
    cosP = cos phi
    sinP = sin phi
    dx = (x1 - x2) / 2
    dy = (y1 - y2) / 2
    x1' = cosP * dx + sinP * dy
    y1' = negate sinP * dx + cosP * dy
    lambda = (x1' * x1') / (rx0 * rx0) + (y1' * y1') / (ry0 * ry0)
    grow = if lambda > 1 then sqrt lambda else 1
    rx = abs rx0 * grow
    ry = abs ry0 * grow
    -- The specification's (rx² ry² - rx² y1'² - ry² x1'²) / (rx² y1'² +
    -- ry² x1'²), divided through by rx² ry² so that a large radius does not
    -- overflow it.
    reach = (x1' / rx) * (x1' / rx) + (y1' / ry) * (y1' / ry)
    coef = (if large == sweep then -1 else 1) * sqrt (max 0 (1 / reach - 1))
    cx' = coef * rx * y1' / ry
    cy' = coef * negate (ry * x1' / rx)
    cx = cosP * cx' - sinP * cy' + (x1 + x2) / 2
    cy = sinP * cx' + cosP * cy' + (y1 + y2) / 2
    angle ux uy vx vy = atan2 (ux * vy - uy * vx) (ux * vx + uy * vy)
    theta1 = angle 1 0 ((x1' - cx') / rx) ((y1' - cy') / ry)
    dtheta0 = angle ((x1' - cx') / rx) ((y1' - cy') / ry) ((negate x1' - cx') / rx) ((negate y1' - cy') / ry)
    dtheta
      | not sweep && dtheta0 > 0 = dtheta0 - 2 * pi
      | sweep && dtheta0 < 0 = dtheta0 + 2 * pi
      | otherwise = dtheta0

--------------------------------------------------------------------------------
-- Triangulation
--------------------------------------------------------------------------------

-- | Index triples, three a triangle, into the points of a simple polygon
-- (x/y pairs, in either winding, the first point not repeated) that cover
-- it. A convex polygon is fanned from its first point and any other ear
-- clipped. Fewer than three points give none; holes and self-intersections
-- are unsupported and may leave part of the polygon uncovered.
triangulate :: PrimArray Float -> PrimArray Int
triangulate vs
  | n < 3 = emptyPrimArray
  | convexRing vs = generatePrimArray (3 * (n - 2)) fan
  | otherwise = primArrayFromList [i | (a, b, c) <- earClip vs, i <- [a, b, c]]
  where
    n = sizeofPrimArray vs `div` 2
    fan k = case k `mod` 3 of
      0 -> 0
      1 -> k `div` 3 + 1
      _ -> k `div` 3 + 2

-- | Whether the ring turns one way at every corner, straight on aside, and
-- goes round once: its x and y directions each reverse at most twice, which
-- a star that turns one way but winds twice does not.
convexRing :: PrimArray Float -> Bool
convexRing vs = go 0 0 0 0 0 0
  where
    n = sizeofPrimArray vs `div` 2
    at = pointAt vs
    go :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
    go !i !turn !xs !ys !lastX !lastY
      | i >= n = True
      | otherwise =
          let (x0, y0) = at i
              (x1, y1) = at ((i + 1) `mod` n)
              (x2, y2) = at ((i + 2) `mod` n)
              c = cross (x1 - x0, y1 - y0) (x2 - x1, y2 - y1)
              turn' = if c > 0 then 1 else if c < 0 then -1 else turn
              sx = signum' (x1 - x0)
              sy = signum' (y1 - y0)
              xs' = if sx /= 0 && lastX /= 0 && sx /= lastX then xs + 1 else xs
              ys' = if sy /= 0 && lastY /= 0 && sy /= lastY then ys + 1 else ys
           in if turn /= 0 && turn' /= turn || xs' > 2 || ys' > 2
                then False
                else go (i + 1) turn' xs' ys' (if sx /= 0 then sx else lastX) (if sy /= 0 then sy else lastY)
    signum' v = if v > 0 then 1 else if v < 0 then -1 else 0 :: Int

{-# INLINE pointAt #-}
pointAt :: PrimArray Float -> Int -> (Float, Float)
pointAt vs i = (indexPrimArray vs (2 * i), indexPrimArray vs (2 * i + 1))

signedArea :: PrimArray Float -> Float
signedArea vs =
  let n = sizeofPrimArray vs `div` 2
   in foldl' (\acc i -> acc + cross (pointAt vs i) (pointAt vs ((i + 1) `mod` n)) / 2) 0 [0 .. n - 1]

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

-- | Index triples into the points of @vs@, a ring of more than three.
earClip :: PrimArray Float -> [(Int, Int, Int)]
earClip vs = runST $ do
      let !ccw = signedArea vs >= 0
      -- Coordinates never move. Remove an ear by relinking two neighbours,
      -- instead of copying the remaining coordinates at every step.
      prevs <- newPrimArray n
      nexts <- newPrimArray n
      forM_ [0 .. n - 1] $ \i -> do
        writePrimArray prevs i ((i - 1 + n) `mod` n)
        writePrimArray nexts i ((i + 1) `mod` n)
      let triangle i = do
            p <- readPrimArray prevs i
            q <- readPrimArray nexts i
            pure (p, q, (at p, at i, at q))
          isEarAt first count i p q (a, b, c)
            | not (isConvex ccw a b c) = pure False
            | otherwise = outside first count
            where
              outside !_ 0 = pure True
              outside !j !left
                | j /= p && j /= i && j /= q && pointInTri (at j) a b c = pure False
                | otherwise = do
                    next <- readPrimArray nexts j
                    outside next (left - 1)
          convex !_ 0 = pure True
          convex !i !left = do
            (_, q, (a, b, c)) <- triangle i
            if isConvex ccw a b c then convex q (left - 1) else pure False
          fan origin i left
            | left <= 0 = pure []
            | otherwise = do
                q <- readPrimArray nexts i
                rest <- fan origin q (left - 1)
                pure ((origin, i, q) : rest)
          go !first !count !idx !tries tris
            | count == 3 = do
                second <- readPrimArray nexts first
                third <- readPrimArray nexts second
                pure ((first, second, third) : tris)
            | tries >= count = do
                isConvexRing <- convex first count
                if isConvexRing then do
                  second <- readPrimArray nexts first
                  rest <- fan first second (count - 2)
                  pure (tris ++ rest)
                else pure tris
            | otherwise = do
                (p, q, tri) <- triangle idx
                ear <- isEarAt first count idx p q tri
                if ear then do
                  writePrimArray nexts p q
                  writePrimArray prevs q p
                  let !first' = if idx == first then q else first
                  go first' (count - 1) first' 0 ((p, idx, q) : tris)
                else go first count q (tries + 1) tris
      reverse <$> go 0 n 0 0 []
  where
    n = sizeofPrimArray vs `div` 2
    at = pointAt vs

--------------------------------------------------------------------------------
-- Draw ops
--------------------------------------------------------------------------------

-- | Each ring's points with repeats dropped, and whether it closed, for the
-- rings with nothing but numbers in them. A point within a ten-thousandth
-- of a pixel of the one before it repeats it; with @closing@, or when the
-- ring closed, so does a last point on the first.
cleanRings :: Bool -> Rings -> [(PrimArray Float, Bool)]
cleanRings closing rings@(Rings pts starts tags) =
  [ (kept, closed)
  | r <- [0 .. ringCount rings - 1]
  , let from = indexPrimArray starts r
        to = indexPrimArray starts (r + 1)
        closed = indexPrimArray tags r /= 0
  , all (\k -> finite (indexPrimArray pts k)) [2 * from .. 2 * to - 1]
  , let kept = dedupe (closing || closed) from to
  ]
  where
    near ax ay bx by = let dx = ax - bx; dy = ay - by in dx * dx + dy * dy < 1e-8
    dedupe wrap from to = runPrimArray $ do
      out <- newPrimArray (2 * (to - from))
      let copy !k !m
            | k >= to = pure m
            | otherwise = do
                let x = indexPrimArray pts (2 * k)
                    y = indexPrimArray pts (2 * k + 1)
                repeated <-
                  if m == 0
                    then pure False
                    else near x y <$> readPrimArray out (2 * m - 2) <*> readPrimArray out (2 * m - 1)
                if repeated
                  then copy (k + 1) m
                  else do
                    writePrimArray out (2 * m) x
                    writePrimArray out (2 * m + 1) y
                    copy (k + 1) (m + 1)
      m <- copy from 0
      m' <-
        if wrap && m > 1
          then do
            onFirst <- near <$> readPrimArray out 0 <*> readPrimArray out 1 <*> readPrimArray out (2 * m - 2) <*> readPrimArray out (2 * m - 1)
            pure (if onFirst then m - 1 else m)
          else pure m
      shrinkMutablePrimArray out (2 * m')
      pure out

-- | The ops that fill a path flattened through @t@ within @tol@: a
-- 'FillPolygon' for each subpath, closed or not, on its own. A subpath
-- inside another fills over it rather than cutting a hole.
fillPathOps :: Float -> Transform -> Path -> Color -> [DrawOp]
fillPathOps tol t path col =
  [ FillPolygon pts tris col
  | (pts, _) <- cleanRings True (flattenPath tol (arcSteps tol) t path)
  , abs (signedArea pts) > 1e-6
  , let tris = triangulate pts
  , sizeofPrimArray tris > 0
  ]

-- | The ops that stroke a path flattened through @t@ within @tol@, @w@
-- wide before the transform scales it by its 'averageStretch': a
-- 'StrokePolyline' for each subpath, with @cap@ at an open one's ends. A
-- round cap is a disc over the end, so a translucent line shows darker
-- where the two overlap.
strokePathOps :: Float -> Transform -> LineCap -> Path -> Float -> Color -> [DrawOp]
strokePathOps tol t cap path w col
  | not (width > 0) = []
  | otherwise = concatMap ring (cleanRings False (flattenPath tol (arcSteps tol) t path))
  where
    width = w * averageStretch t
    hw = width / 2
    ring (pts, closed)
      | n < 2 = []
      -- A closed subpath of two points is a line with no caps.
      | closed = [StrokePolyline pts width (n > 2) col]
      | otherwise = case cap of
          ButtCap -> [StrokePolyline pts width False col]
          SquareCap -> [StrokePolyline (extendEnds hw pts) width False col]
          RoundCap ->
            [ StrokePolyline pts width False col
            , FillCircle (px 0) (py 0) hw col
            , FillCircle (px (n - 1)) (py (n - 1)) hw col
            ]
      where
        n = sizeofPrimArray pts `div` 2
        px i = indexPrimArray pts (2 * i)
        py i = indexPrimArray pts (2 * i + 1)

-- | The polyline's first and last points moved @d@ further out along
-- their segments.
extendEnds :: Float -> PrimArray Float -> PrimArray Float
extendEnds d pts = generatePrimArray (2 * n) at
  where
    n = sizeofPrimArray pts `div` 2
    px i = indexPrimArray pts (2 * i)
    py i = indexPrimArray pts (2 * i + 1)
    out i j =
      let dx = px i - px j
          dy = py i - py j
          len = sqrt (dx * dx + dy * dy)
       in if len > 0 then (px i + dx / len * d, py i + dy / len * d) else (px i, py i)
    at k
      | k < 2 = pick (out 0 1)
      | k >= 2 * n - 2 = pick (out (n - 1) (n - 2))
      | otherwise = indexPrimArray pts k
      where
        pick (x, y) = if even k then x else y

-- | An op drawn under transform @t@, flattening what it has to within
-- @tol@. A rect keeps its op under a transform that keeps its sides level
-- and upright (a scale, a flip, a quarter turn), and a rounded rect under
-- one of those that scales both ways alike; a circle keeps its op under a
-- transform that keeps it round. Otherwise they become polygons. Lines,
-- polygons and triangles move their points, their widths scaled by the
-- transform's 'averageStretch'. Gradients and images fill the bounding box
-- of their transformed rect: a gradient's corners take the colours of the
-- corners that land nearest them, and an image turns over with a flip, but
-- neither turns. Text moves its anchor, its glyphs neither scaled nor
-- turned. A transform with a NaN or infinite entry draws nothing, as does
-- a rounded rect or circle with no size.
transformOp :: Float -> Transform -> DrawOp -> [DrawOp]
transformOp tol t@(Transform a b c d _ _) op
  | not (transformFinite t) = []
  | otherwise = case op of
      FillRect r col
        | keepsRects -> [FillRect (box r) col]
        | otherwise -> fillPathOps tol t (rect r) col
      FillRoundedRect r rad col
        | empty r -> []
        | keepsRects && uniform -> [FillRoundedRect (box r) (rad * k) col]
        | otherwise -> fillPathOps tol t (roundedRect r rad) col
      FillTriangle x0 y0 x1 y1 x2 y2 col ->
        let (x0', y0') = applyTransform t x0 y0
            (x1', y1') = applyTransform t x1 y1
            (x2', y2') = applyTransform t x2 y2
         in [FillTriangle x0' y0' x1' y1' x2' y2' col]
      FillCircle cx cy rad col
        | not (rad > 0) -> []
        | conformal -> let (cx', cy') = applyTransform t cx cy in [FillCircle cx' cy' (rad * k) col]
        | otherwise -> fillPathOps tol t (ellipse (V2 cx cy) (V2 rad rad)) col
      Stroke x0 y0 x1 y1 w col -> line Stroke x0 y0 x1 y1 w col
      StrokeRoundedRect r rad w col
        | empty r -> []
        | keepsRects && uniform -> [StrokeRoundedRect (box r) (rad * k) (w * k) col]
        | otherwise ->
            -- The border lies inside the rect, so its centre line is the
            -- rect inset by half the width.
            let Rect x y rw rh = r
                bw = min w (min rw rh / 2)
             in strokePathOps tol t ButtCap (roundedRect (Rect (x + bw / 2) (y + bw / 2) (rw - bw) (rh - bw)) (rad - bw / 2)) bw col
      StrokeCircle cx cy rad w col
        | not (rad > 0) -> []
        | conformal -> let (cx', cy') = applyTransform t cx cy in [StrokeCircle cx' cy' (rad * k) (w * k) col]
        | otherwise ->
            let bw = min w rad
             in strokePathOps tol t ButtCap (ellipse (V2 cx cy) (V2 (rad - bw / 2) (rad - bw / 2))) bw col
      StrokeLineAA x0 y0 x1 y1 w col -> line StrokeLineAA x0 y0 x1 y1 w col
      FillPolygon pts tris col -> [FillPolygon (points pts) tris col]
      StrokePolyline pts w closed col -> [StrokePolyline (points pts) (w * k) closed col]
      FillQuadGradient r tl tr br bl ->
        let Rect bx by bw bh = box r
            from = zip (corners r) [tl, tr, br, bl]
            nearest (px, py) = snd (minimumOn (\((qx, qy), _) -> (qx - px) * (qx - px) + (qy - py) * (qy - py)) from)
         in [ FillQuadGradient
                (Rect bx by bw bh)
                (nearest (bx, by))
                (nearest (bx + bw, by))
                (nearest (bx + bw, by + bh))
                (nearest (bx, by + bh))
            ]
      DrawImageRect r tex u0 v0 u1 v1 col ->
        let (u0', u1') = if levelAxes && a < 0 then (u1, u0) else (u0, u1)
            (v0', v1') = if levelAxes && d < 0 then (v1, v0) else (v0, v1)
         in [DrawImageRect (box r) tex u0' v0' u1' v1' col]
      DrawText x y ax ay txt col -> let (x', y') = applyTransform t x y in [DrawText x' y' ax ay txt col]
      DrawTextStyled x y font txt col -> let (x', y') = applyTransform t x y in [DrawTextStyled x' y' font txt col]
  where
    k = averageStretch t
    -- A rect with no size, for which a rounded rect's own op draws nothing.
    empty (Rect _ _ w h) = not (w > 0 && h > 0)
    -- Zero but for rounding: a half turn's sine is not quite 0.
    small v = abs v <= 1e-6 * maximum (map abs [a, b, c, d])
    -- No rotation or skew.
    levelAxes = small b && small c
    keepsRects = levelAxes || (small a && small d)
    uniform = abs (abs a + abs b - abs c - abs d) <= 1e-6 * maximum (map abs [a, b, c, d])
    -- Rotation and a uniform scale, perhaps with a flip.
    conformal = (a == d && b == negate c) || (a == negate d && b == c)
    line mk x0 y0 x1 y1 w col =
      let (x0', y0') = applyTransform t x0 y0
          (x1', y1') = applyTransform t x1 y1
       in [mk x0' y0' x1' y1' (w * k) col]
    points pts = generatePrimArray (sizeofPrimArray pts) $ \i ->
      let (x, y) = applyTransform t (indexPrimArray pts (i - i `mod` 2)) (indexPrimArray pts (i - i `mod` 2 + 1))
       in if even i then x else y
    -- The rect's corners, top left and on clockwise, where the transform
    -- takes them.
    corners (Rect x y w h) = [applyTransform t px py | (px, py) <- [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]]
    -- The bounding box of the rect's transformed corners.
    box r =
      let xs = map fst (corners r)
          ys = map snd (corners r)
       in Rect (minimum xs) (minimum ys) (maximum xs - minimum xs) (maximum ys - minimum ys)
    minimumOn f = foldr1 (\p q -> if f p <= f q then p else q)

-- | A rectangle's outline, clockwise on screen from its top left corner.
rect :: Rect -> Path
rect (Rect x y w h) =
  Path [SegMove x y, SegLine (x + w) y, SegLine (x + w) (y + h), SegLine x (y + h), SegClose]

-- | A rounded rectangle's outline and its corner radius, kept within half
-- the shorter side, clockwise on screen from the top left corner's end.
roundedRect :: Rect -> Float -> Path
roundedRect box@(Rect x y w h) radius
  | r <= 0 = rect box
  | otherwise =
      Path
        [ SegMove (x + r) y
        , SegArc (x + w - r) (y + r) r r 0 (-pi / 2) (pi / 2)
        , SegArc (x + w - r) (y + h - r) r r 0 0 (pi / 2)
        , SegArc (x + r) (y + h - r) r r 0 (pi / 2) (pi / 2)
        , SegArc (x + r) (y + r) r r 0 pi (pi / 2)
        , SegClose
        ]
  where
    r = max 0 (min radius (min (abs w) (abs h) / 2))

-- | An ellipse: its centre and its x and y radii. Rotate one with
-- 'NanoUI.Path.ellipticalArc' or a transform.
ellipse :: V2 -> V2 -> Path
ellipse (V2 cx cy) (V2 rx ry) = Path [SegMove (cx + abs rx) cy, SegArc cx cy rx ry 0 0 (2 * pi), SegClose]
