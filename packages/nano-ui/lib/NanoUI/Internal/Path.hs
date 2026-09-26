-- | Paths and affine transforms, and the geometry that turns them into what
-- the draw ops take: curves flattened into rings of points, the centre form
-- of an SVG arc, rings nested into polygons with holes and triangulated for
-- 'FillPolygon', dashes, miters and gradients. The canvas ("NanoUI.Path",
-- "NanoUI.Widgets.Custom"), the SVG rasterizer ("NanoUI.Svg") and
-- nano-ui-diagrams share it.
module NanoUI.Internal.Path
  ( -- * Paths
    Path (..)
  , Segment (..)
  , moveTo
  , lineTo
  , quadTo
  , cubicTo
  , arc
  , ellipticalArc
  , arcTo
  , close
    -- * Shapes
  , rect
  , roundedRect
  , roundedRectCorners
  , circle
  , ellipse
  , polygon
  , polyline
    -- * Transforms
  , Transform (..)
  , applyTransform
  , translate
  , rotate
  , rotateAround
  , scale
  , affine
  , transformPoint
  , invert
    -- * Strokes, fills and paints
  , Stroke (..)
  , stroke
  , LineCap (..)
  , LineJoin (..)
  , miterOffset
  , FillRule (..)
  , fillsWinding
  , Paint (..)
  , Shade (..)
    -- * Rings
  , Rings (..)
  , ringCount
  , shoelace
  , buildRings
  , cleanRings
    -- * Flattening
  , curveTolerance
  , flattenPath
  , cubicPoints
    -- * Triangulation
  , triangulate
  , triangulateRings
    -- * Draw ops
  , fillPathOps
  , strokePathOps
  , transformOp
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.List (sortOn)
import Data.Primitive.PrimArray
  ( PrimArray
  , emptyPrimArray
  , generatePrimArray
  , indexPrimArray
  , newPrimArray
  , primArrayFromList
  , primArrayFromListN
  , primArrayToList
  , readPrimArray
  , runPrimArray
  , setPrimArray
  , shrinkMutablePrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.SmallArray (indexSmallArray, smallArrayFromListN)
import Data.Word (Word32)
import NanoUI.Internal.Draw.Types (DrawOp (..), LineCap (..), LineJoin (..), Shade (..), defaultTextFont)
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..), lerpColor)

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

-- | Start a subpath at a point.
moveTo :: V2 -> Path
moveTo (V2 x y) = Path [SegMove x y]

-- | A straight line to a point.
lineTo :: V2 -> Path
lineTo (V2 x y) = Path [SegLine x y]

-- | A quadratic Bezier curve to a point: its control point, then its end.
quadTo :: V2 -> V2 -> Path
quadTo (V2 qx qy) (V2 x y) = Path [SegQuad qx qy x y]

-- | A cubic Bezier curve to a point: its two control points, then its end.
cubicTo :: V2 -> V2 -> V2 -> Path
cubicTo (V2 x1 y1) (V2 x2 y2) (V2 x y) = Path [SegCubic x1 y1 x2 y2 x y]

-- | A circular arc: its centre, radius, start angle and sweep. With a
-- subpath open a line runs from its current point to the arc's start;
-- otherwise the arc starts one. A sweep of a whole turn or more draws the
-- whole circle, and a negative one turns anticlockwise. A negative radius
-- counts as its size.
--
-- > P.moveTo c <> P.arc c r 0 (pi / 3) <> P.close  -- a pie slice
arc :: V2 -> Float -> Float -> Float -> Path
arc c r = ellipticalArc c (V2 r r) 0

-- | An arc of an ellipse: its centre, its x and y radii, how far the
-- ellipse is rotated, and the start angle and sweep around it, which are
-- angles before the ellipse is stretched and rotated. Joins its subpath as
-- 'arc' does.
ellipticalArc :: V2 -> V2 -> Float -> Float -> Float -> Path
ellipticalArc (V2 cx cy) (V2 rx ry) rot start sweep = Path [SegArc cx cy rx ry rot start sweep]

-- | An arc to a point, as SVG's @A@ command draws one: the ellipse's x and
-- y radii and rotation, whether to take the larger of the two arcs that
-- fit, whether to go clockwise (on screen) from the current point, and the
-- end. Radii too small to reach the end grow until they do; a zero radius
-- draws a straight line. It is not HTML canvas's @arcTo@, which rounds the
-- corner between two lines: draw that with 'arc' about the corner's centre.
arcTo :: V2 -> Float -> Bool -> Bool -> V2 -> Path
arcTo (V2 rx ry) rot large clockwise (V2 x y) = Path [SegArcTo rx ry rot large clockwise x y]

-- | Close the subpath with a line back to its start. A stroke joins a
-- closed subpath's ends instead of capping them.
close :: Path
close = Path [SegClose]

--------------------------------------------------------------------------------
-- Shapes
--------------------------------------------------------------------------------

-- | A rectangle's outline, clockwise on screen from its top left corner.
rect :: Rect -> Path
rect (Rect x y w h) =
  Path [SegMove x y, SegLine (x + w) y, SegLine (x + w) (y + h), SegLine x (y + h), SegClose]

-- | A rounded rectangle's outline and its corner radius, kept within half
-- the shorter side, clockwise on screen from the top left corner's end.
roundedRect :: Rect -> Float -> Path
roundedRect box radius = roundedRectCorners box radius radius radius radius

-- | A rectangle with a radius for each corner: top left, top right, bottom
-- right and bottom left, as CSS's @border-radius@ lists them. Where two
-- corners on a side would overlap, every radius shrinks by the same factor
-- until they meet, as CSS shrinks them; a negative radius is square.
roundedRectCorners :: Rect -> Float -> Float -> Float -> Float -> Path
roundedRectCorners box@(Rect x y w h) tl0 tr0 br0 bl0
  | tl <= 0 && tr <= 0 && br <= 0 && bl <= 0 = rect box
  | otherwise =
      Path
        [ SegMove (x + tl) y
        , corner (x + w - tr) (y + tr) tr (-pi / 2)
        , corner (x + w - br) (y + h - br) br 0
        , corner (x + bl) (y + h - bl) bl (pi / 2)
        , corner (x + tl) (y + tl) tl pi
        , SegClose
        ]
  where
    positive r = if r > 0 then r else 0
    aw = abs w
    ah = abs h
    -- How far a side's two radii shrink to fit along it.
    fitSide side a b = let both = positive a + positive b in if both > side then side / both else 1
    shrink = min (min (fitSide aw tl0 tr0) (fitSide ah tr0 br0)) (min (fitSide aw br0 bl0) (fitSide ah bl0 tl0))
    fit r = positive r * shrink
    tl = fit tl0
    tr = fit tr0
    br = fit br0
    bl = fit bl0
    -- A quarter turn round a corner's centre, or a line to its point.
    corner cx cy r a0
      | r > 0 = SegArc cx cy r r 0 a0 (pi / 2)
      | otherwise = SegLine cx cy

-- | A circle: its centre and radius.
circle :: V2 -> Float -> Path
circle c r = ellipse c (V2 r r)

-- | An ellipse: its centre and its x and y radii. Rotate one with
-- 'NanoUI.Path.ellipticalArc' or a transform.
ellipse :: V2 -> V2 -> Path
ellipse (V2 cx cy) (V2 rx ry) = Path [SegMove (cx + abs rx) cy, SegArc cx cy rx ry 0 0 (2 * pi), SegClose]

-- | A closed polygon through the points.
polygon :: [V2] -> Path
polygon pts = polyline pts <> if null pts then mempty else close

-- | An open line through the points.
polyline :: [V2] -> Path
polyline [] = mempty
polyline (V2 x y : rest) = Path (SegMove x y : [SegLine px py | V2 px py <- rest])

--------------------------------------------------------------------------------
-- Strokes, fills and paints
--------------------------------------------------------------------------------

-- | How to stroke a path: 'stroke' with its width, and a record update for
-- the rest.
--
-- > (P.stroke 3) {P.strokeCap = P.RoundCap, P.strokeDash = [6, 4]}
data Stroke = StrokeStyle
  { strokeWidth :: !Float
  -- ^ The line's width. A transform scales it, as it scales the path.
  , strokeCap :: !LineCap
  -- ^ How an open subpath's ends, and every dash's, are capped (default
  -- 'ButtCap').
  , strokeJoin :: !LineJoin
  -- ^ How its corners are joined (default 'MiterJoin').
  , strokeMiterLimit :: !Float
  -- ^ How long a miter may be, as a multiple of the width, before its
  -- corner is beveled instead (default 4, as SVG's @stroke-miterlimit@).
  , strokeDash :: ![Float]
  -- ^ Lengths of dash and gap, one after the other, repeated along each
  -- subpath; an odd number of them is repeated twice over (default @[]@, a
  -- solid line). A zero-length dash with round or square caps is a dot.
  -- Lengths that are negative, or all zero, draw a solid line, and so does
  -- a pattern that would cut a subpath into more than 4096 dashes. A
  -- transform scales them as it scales the width.
  , strokeDashOffset :: !Float
  -- ^ How far into the dash pattern each subpath starts (default 0).
  }
  deriving (Eq, Show)

-- | A solid stroke this wide, cut square at open ends, with miter joins.
stroke :: Float -> Stroke
stroke w = StrokeStyle w ButtCap MiterJoin 4 [] 0

-- | The miter at a corner between two edges with unit normals @(ax, ay)@
-- and @(bx, by)@: the offset along which moving the corner by one moves
-- both edges by one. Its length is the miter's length over the line's
-- width; past @limit@ the corner is beveled and there is 'Nothing'. The
-- canvas's strokes and the SVG rasterizer join corners by it.
{-# INLINE miterOffset #-}
miterOffset :: Float -> Float -> Float -> Float -> Float -> Maybe (Float, Float)
miterOffset limit ax ay bx by
  | not (d2 * limit * limit >= 1) = Nothing
  | otherwise = Just (mx / d2, my / d2)
  where
    mx = (ax + bx) / 2
    my = (ay + by) / 2
    d2 = mx * mx + my * my

-- | Which points a fill covers, by how many times the path winds round
-- them, counting a turn one way as 1 and the other as -1.
data FillRule
  = NonZero
  -- ^ Every point it winds round other than as many times one way as the
  -- other: a subpath inside another is filled over, unless it runs the
  -- other way, which cuts it out.
  | EvenOdd
  -- ^ Every point it winds round an odd number of times: a subpath inside
  -- another is cut out of it, and one inside that filled again.
  deriving (Eq, Show, Enum, Bounded)

-- | Whether a point a path winds round @w@ times is inside its fill. The
-- canvas's fills and the SVG rasterizer both count by it.
{-# INLINE fillsWinding #-}
fillsWinding :: FillRule -> Int -> Bool
fillsWinding NonZero w = w /= 0
fillsWinding EvenOdd w = odd w

-- | What a fill or a stroke is painted with.
data Paint
  = Solid !Color
  -- ^ One colour.
  | Linear !V2 !V2 ![(Float, Color)]
  -- ^ A linear gradient from its first point, where it is at 0, to its
  -- second, where it is at 1, and colour stops at offsets along it. Past
  -- the first and last stops it keeps their colours. Offsets are kept
  -- within 0 and 1, and one less than the one before counts as that one,
  -- so two stops at one offset change colour sharply there. It is laid out
  -- in the coordinates of the block that draws it, so it turns and scales
  -- with a transform. A stroke takes the colour at its centre line across
  -- its width.
  deriving (Eq, Show)

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

-- | Move by an offset.
translate :: Float -> Float -> Transform
translate dx dy = Transform 1 0 0 1 dx dy

-- | Turn about the origin, clockwise on screen for a positive angle.
rotate :: Float -> Transform
rotate a =
  let s = sin a
      c = cos a
   in Transform c s (negate s) c 0 0

-- | Turn about a point.
rotateAround :: V2 -> Float -> Transform
rotateAround (V2 x y) a = translate x y <> rotate a <> translate (negate x) (negate y)

-- | Scale about the origin, x and y by their own factors. A negative one
-- flips that axis.
scale :: Float -> Float -> Transform
scale sx sy = Transform sx 0 0 sy 0 0

-- | A transform by its matrix, as SVG's @matrix(a b c d e f)@ gives it:
-- @(x, y)@ goes to @(a x + c y + e, b x + d y + f)@.
affine :: Float -> Float -> Float -> Float -> Float -> Float -> Transform
affine = Transform

-- | Where a transform takes a point.
transformPoint :: Transform -> V2 -> V2
transformPoint t (V2 x y) = let (x', y') = applyTransform t x y in V2 x' y'

-- | The transform that undoes this one, as for taking a point under the
-- pointer back into a drawing's own coordinates. 'Nothing' for one that
-- flattens the plane onto a line or a point, or has an entry that is not a
-- number.
invert :: Transform -> Maybe Transform
invert (Transform a b c d e f)
  | det == 0 || not (transformFinite inv) = Nothing
  | otherwise = Just inv
  where
    det = a * d - b * c
    inv = Transform (d / det) (negate b / det) (negate c / det) (a / det) ((c * f - d * e) / det) ((b * e - a * f) / det)

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
curveTolerance s
  | s > 0 && s < 1 / 0 = 0.25 / s
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
  | otherwise = earClip vs (generatePrimArray n id)
  where
    n = sizeofPrimArray vs `div` 2
    fan k = case k `mod` 3 of
      0 -> 0
      1 -> k `div` 3 + 1
      _ -> k `div` 3 + 2

-- | Index triples that cover a polygon with holes: @vs@ holds its rings'
-- points, one ring after another, and @rings@ where each ring starts and
-- the last ends. The first ring is its outline, and the rest are holes
-- inside it that touch neither it nor each other. A lone outline may wind
-- either way; with holes, the outline winds with a positive 'ringArea' and
-- the holes negative, as 'fillPathOps' winds them. Each hole is joined to
-- the outline by a cut to a point it can see, as in Eberly's
-- \"Triangulation by Ear Clipping\" and mapbox's earcut, and the one ring
-- that makes is ear clipped.
triangulateRings :: PrimArray Float -> PrimArray Int -> PrimArray Int
triangulateRings vs rings
  | sizeofPrimArray rings <= 2 = triangulate vs
  | otherwise = earClip vs (primArrayFromList (bridgeHoles vs (ringAt 0) (map ringAt [1 .. sizeofPrimArray rings - 2])))
  where
    ringAt r = [indexPrimArray rings r .. indexPrimArray rings (r + 1) - 1]

-- | The outline's point indices with each hole spliced in, leftmost hole
-- first: from the outline point a cut from the hole's leftmost point can
-- reach, round the hole and back along the cut. A hole with no such point
-- is left out.
bridgeHoles :: PrimArray Float -> [Int] -> [[Int]] -> [Int]
bridgeHoles vs outline holes = foldl' join outline (map snd (sortOn fst (map leftmostFirst (filter (not . null) holes))))
  where
    at = pointAt vs
    -- The hole from its leftmost point, and that point.
    leftmostFirst h =
      let (p, m) = minimum [(at i, k) | (k, i) <- zip [0 :: Int ..] h]
       in (p, drop m h ++ take m h)
    join ring h@(hi : _) = case bridgeFrom ring (at hi) of
      Nothing -> ring
      Just k ->
        let (before, after) = splitAt (k + 1) ring
         in before ++ h ++ [hi, ring !! k] ++ after
    join ring [] = ring
    -- The position in the ring of the point a cut from @(hx, hy)@ goes to:
    -- a ray to the left meets the ring's nearest edge, and the cut goes to
    -- that edge's left end, or to the point of the ring inside the triangle
    -- that makes with the ray at the least angle to it, which the ray
    -- cannot otherwise see past.
    bridgeFrom ring (hx, hy) =
      let n = length ring
          ix = primArrayFromListN n ring
          node k = indexPrimArray ix (k `mod` n)
          pt k = at (node k)
          hits =
            [ (x, k')
            | k <- [0 .. n - 1]
            , let (ax, ay) = pt k
                  (bx, by) = pt (k + 1)
            , hy <= ay && hy >= by && by /= ay
            , let x = ax + (hy - ay) * (bx - ax) / (by - ay)
            , x <= hx
            , let k' = if ax < bx then k else (k + 1) `mod` n
            ]
       in case hits of
            [] -> Nothing
            _ ->
              let (qx, m) = maximum hits
                  (mx, my) = pt m
                  inTriangle (px, py) =
                    let (ax, ay, cx, cy) = if hy < my then (hx, hy, qx, hy) else (qx, hy, hx, hy)
                     in (cx - px) * (ay - py) >= (ax - px) * (cy - py)
                          && (ax - px) * (my - py) >= (mx - px) * (ay - py)
                          && (mx - px) * (cy - py) >= (cx - px) * (my - py)
                  -- The cut from the hole's point to point @k@ leaves the
                  -- ring's corner there on its inside.
                  locallyInside k =
                    let a = pt k
                        prev = pt (k - 1)
                        next = pt (k + 1)
                        h = (hx, hy)
                     in if turn prev a next < 0
                          then turn a h next >= 0 && turn a prev h >= 0
                          else turn a h prev < 0 || turn a next h < 0
                  better (tanBest, kBest) k =
                    let (px, py) = pt k
                        t = abs (hy - py) / (hx - px)
                     in if hx >= px && px >= mx && hx /= px && inTriangle (px, py) && locallyInside k
                          && (t < tanBest || (t == tanBest && px > fst (pt kBest)))
                          then (t, k)
                          else (tanBest, kBest)
               in if qx == hx then Just m else Just (snd (foldl' better (1 / 0, m) [0 .. n - 1]))

-- | Twice the signed area of a triangle, in earcut's sense: negative where
-- it turns as a ring with a positive 'signedArea' turns at a convex corner.
turn :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
turn (px, py) (qx, qy) (rx, ry) = (qy - py) * (rx - qx) - (qx - px) * (ry - qy)

-- | Whether the ring turns one way at every corner, straight on aside, and
-- goes round once: its x and y directions each reverse at most twice, which
-- a star that turns one way but winds twice does not.
convexRing :: PrimArray Float -> Bool
convexRing vs = go 0 0 0 0 0 0
  where
    n = sizeofPrimArray vs `div` 2
    at = pointAt vs
    go :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
    go !i !turn' !xs !ys !lastX !lastY
      | i >= n = True
      | otherwise =
          let (x0, y0) = at i
              (x1, y1) = at ((i + 1) `mod` n)
              (x2, y2) = at ((i + 2) `mod` n)
              c = cross (x1 - x0, y1 - y0) (x2 - x1, y2 - y1)
              turn'' = if c > 0 then 1 else if c < 0 then -1 else turn'
              sx = signum' (x1 - x0)
              sy = signum' (y1 - y0)
              xs' = if sx /= 0 && lastX /= 0 && sx /= lastX then xs + 1 else xs
              ys' = if sy /= 0 && lastY /= 0 && sy /= lastY then ys + 1 else ys
           in if turn' /= 0 && turn'' /= turn' || xs' > 2 || ys' > 2
                then False
                else go (i + 1) turn'' xs' ys' (if sx /= 0 then sx else lastX) (if sy /= 0 then sy else lastY)
    signum' v = if v > 0 then 1 else if v < 0 then -1 else 0 :: Int

{-# INLINE pointAt #-}
pointAt :: PrimArray Float -> Int -> (Float, Float)
pointAt vs i =
  let !x = indexPrimArray vs (2 * i)
      !y = indexPrimArray vs (2 * i + 1)
   in (x, y)

-- | The shoelace area of the points of @vs@ that @ix@ lists, in order:
-- positive for a ring clockwise on screen.
signedArea :: PrimArray Float -> PrimArray Int -> Float
signedArea vs ix = shoelace (sizeofPrimArray ix) (pointAt vs . indexPrimArray ix)

-- | 'signedArea' of a ring's points in their own order.
ringArea :: PrimArray Float -> Float
ringArea vs = shoelace (sizeofPrimArray vs `div` 2) (pointAt vs)

-- | The shoelace area of @n@ points, the @k@th at @at k@: positive for a
-- ring clockwise on screen. The polygon emitter tells a ring's winding by
-- it.
{-# INLINE shoelace #-}
shoelace :: Int -> (Int -> (Float, Float)) -> Float
shoelace n at = go 0 0
  where
    go !k !acc
      | k >= n = acc / 2
      | otherwise = go (k + 1) (acc + cross (at k) (at (if k + 1 >= n then 0 else k + 1)))

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

-- | Index triples into the points of @vs@ covering the ring that @ix@ lists
-- by their indices, which a cut to a hole lists twice over. A point on one
-- of an ear's corners does not stop it being cut off.
earClip :: PrimArray Float -> PrimArray Int -> PrimArray Int
earClip vs ix = primArrayFromList [indexPrimArray ix k | (a, b, c) <- clipped, k <- [a, b, c]]
  where
    clipped = runST $ do
      let !ccw = signedArea vs ix >= 0
      -- Coordinates never move. Remove an ear by relinking two neighbours,
      -- instead of copying the remaining coordinates at every step.
      prevs <- newPrimArray n
      nexts <- newPrimArray n
      forM_ [0 .. n - 1] $ \i -> do
        writePrimArray prevs i ((i - 1 + n) `mod` n)
        writePrimArray nexts i ((i + 1) `mod` n)
      -- An ear's corners, read strictly: returned lazily they are thunks,
      -- two for every corner of every ear tried.
      let {-# INLINE triangle #-}
          triangle i = do
            p <- readPrimArray prevs i
            q <- readPrimArray nexts i
            let !a = at p
                !b = at i
                !c = at q
            pure (p, q, (a, b, c))
          isEarAt first count i p q (a, b, c)
            | not (isConvex ccw a b c) = pure False
            | otherwise = outside first count
            where
              outside !_ 0 = pure True
              outside !j !left
                | j /= p && j /= i && j /= q && blocks (at j) = pure False
                | otherwise = do
                    next <- readPrimArray nexts j
                    outside next (left - 1)
              -- A point inside the ear stops it, unless it is on a corner:
              -- the other end of a cut to a hole.
              blocks pj = pointInTri pj a b c && pj /= a && pj /= b && pj /= c
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
            | count < 3 = pure tris
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
    n = sizeofPrimArray ix
    at k = pointAt vs (indexPrimArray ix k)

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

-- | The ops that fill a path flattened through @t@ within @tol@, by the
-- fill rule: a 'FillPolygon' for each outline, closed or not, with the
-- holes cut out of it that the rule makes. A subpath is a hole in the one
-- round it when the rule leaves it unfilled; subpaths that cross each other
-- fill on their own, and one that crosses itself may fill only in part.
fillPathOps :: Float -> Transform -> FillRule -> Path -> Paint -> [DrawOp]
fillPathOps tol t rule path paint = concatMap polygonOp (fillComponents rule rings)
  where
    rings =
      [ pts
      | (pts, _) <- cleanRings True (flattenPath tol (arcSteps tol) t path)
      , abs (ringArea pts) > 1e-6
      ]
    polygonOp (outline, holes) = case devicePaint t paint of
      DeviceSolid col
        | [] <- holes -> let tris = triangulate outline in [FillPolygon outline (ringStarts [outline]) tris (Flat col) | nonEmpty tris]
        | otherwise ->
            let pts = concatPoints (outline : holes)
                starts = ringStarts (outline : holes)
                tris = triangulateRings pts starts
             in [FillPolygon pts starts tris (Flat col) | nonEmpty tris]
      DeviceRamp ramp ->
        -- Each ring gets a point where it crosses a stop, and each triangle
        -- is cut along the stops, so every piece's colour is a blend of its
        -- corners'.
        let rs = map (\r -> splitEdges (rampAt ramp) (rampSplits ramp) True r) (outline : holes)
            outlinePts = concatPoints rs
            starts = ringStarts rs
            (pts, tris) = splitTriangles (rampAt ramp) (rampSplits ramp) outlinePts (triangulateRings outlinePts starts)
         in [FillPolygon pts starts tris (Shaded (rampShades ramp pts)) | nonEmpty tris]
    nonEmpty a = sizeofPrimArray a > 0

-- | The ops that stroke a path flattened through @t@ within @tol@, as the
-- 'Stroke' says, its width and dashes scaled by the transform's
-- 'averageStretch': a 'StrokePolyline' for each subpath, or each dash of it.
strokePathOps :: Float -> Transform -> Stroke -> Path -> Paint -> [DrawOp]
strokePathOps tol t st path paint
  | not (width > 0) = []
  | otherwise = concatMap ring (cleanRings False (flattenPath tol (arcSteps tol) t path))
  where
    k = averageStretch t
    width = strokeWidth st * k
    shade = devicePaint t paint
    ring (pts, closed)
      | n < 2 = []
      | not (null (strokeDash st))
      , Just pieces <- dashes (map (* k) (strokeDash st)) (strokeDashOffset st * k) (closed && n > 2) pts =
          map (line False (strokeCap st)) pieces
      -- A closed subpath of two points is a line with no caps.
      | closed && n == 2 = [line False ButtCap pts]
      | otherwise = [line closed (strokeCap st) pts]
      where
        n = sizeofPrimArray pts `div` 2
    line closed cap pts = case shade of
      DeviceSolid col -> StrokePolyline pts width closed cap (strokeJoin st) (strokeMiterLimit st) (Flat col)
      DeviceRamp ramp ->
        let pts' = splitEdges (rampAt ramp) (rampSplits ramp) closed pts
         in StrokePolyline pts' width closed cap (strokeJoin st) (strokeMiterLimit st) (Shaded (rampShades ramp pts'))

-- | Rings' points one ring after another.
concatPoints :: [PrimArray Float] -> PrimArray Float
concatPoints [r] = r
concatPoints rs = primArrayFromList (concatMap primArrayToList rs)

-- | Where each ring starts in 'concatPoints', and where the last ends.
ringStarts :: [PrimArray Float] -> PrimArray Int
ringStarts [r] = runPrimArray $ do
  starts <- newPrimArray 2
  writePrimArray starts 0 0
  writePrimArray starts 1 (sizeofPrimArray r `div` 2)
  pure starts
ringStarts rs = primArrayFromList (scanl (+) 0 [sizeofPrimArray r `div` 2 | r <- rs])

-- | The rings' outlines, each with the holes the fill rule cuts in it:
-- where there are holes, the outline wound with a positive 'ringArea' and
-- the holes the other way. A ring is inside another when all of its points
-- are; the path winds round the points just inside a ring once more, one
-- way or the other by the ring's winding, than round those just outside,
-- and the rule says which of them it fills. A filled ring with nothing
-- filled round it is an outline, and an unfilled ring inside one, or inside
-- filled rings inside one, a hole in it. Rings that cross, being neither
-- inside nor outside each other, each fill on their own.
fillComponents :: FillRule -> [PrimArray Float] -> [(PrimArray Float, [PrimArray Float])]
fillComponents _ [r] = [(r, [])]
fillComponents rule rs = [(orient True i, map (orient False) (holesOf i)) | i <- ids, outline i]
  where
    count = length rs
    ids = [0 .. count - 1]
    rings = smallArrayFromListN count rs
    ring = indexSmallArray rings
    areas = primArrayFromListN count (map ringArea rs)
    areaOf i = indexPrimArray areas i
    boxes = smallArrayFromListN count (map bounds rs)
    boxOf = indexSmallArray boxes
    -- The smallest ring round ring @i@.
    parents = primArrayFromListN count (map parentOf ids)
    parentOf i =
      case [(abs (areaOf j), j) | j <- ids, j /= i, abs (areaOf j) > abs (areaOf i), boxInside (boxOf i) (boxOf j), all (inside (ring j)) (points (ring i))] of
        [] -> -1
        cs -> snd (minimum cs)
    parent i = let p = indexPrimArray parents i in if p < 0 then Nothing else Just p
    -- How many times the path winds round the points just inside ring @i@.
    -- Boxed and lazy: a ring's winding is its parent's and one more.
    windings = smallArrayFromListN count (map windingOf ids)
    windingOf i = maybe 0 (indexSmallArray windings) (parent i) + (if areaOf i > 0 then 1 else -1) :: Int
    filled i = fillsWinding rule (indexSmallArray windings i)
    outline i = filled i && maybe True (not . filled) (parent i)
    children i = [j | j <- ids, parent j == Just i]
    holesOf i = concat [if filled c then holesOf c else [c] | c <- children i]
    orient positive i
      | (areaOf i >= 0) == positive = ring i
      | otherwise = reversePoints (ring i)
    points r = [pointAt r i | i <- [0 .. sizeofPrimArray r `div` 2 - 1]]
    bounds r =
      let xs = [x | (x, _) <- points r]
          ys = [y | (_, y) <- points r]
       in (minimum xs, minimum ys, maximum xs, maximum ys)
    boxInside (ax0, ay0, ax1, ay1) (bx0, by0, bx1, by1) = ax0 >= bx0 && ay0 >= by0 && ax1 <= bx1 && ay1 <= by1
    -- Whether a point is inside a ring, by the rays it crosses.
    inside r (px, py) =
      let m = sizeofPrimArray r `div` 2
          crosses k =
            let (ax, ay) = pointAt r k
                (bx, by) = pointAt r ((k + 1) `mod` m)
             in (ay > py) /= (by > py) && px < ax + (py - ay) * (bx - ax) / (by - ay)
       in odd (length (filter crosses [0 .. m - 1]))

-- | A ring's points in the other order.
reversePoints :: PrimArray Float -> PrimArray Float
reversePoints r =
  let m = sizeofPrimArray r `div` 2
   in generatePrimArray (2 * m) (\i -> indexPrimArray r (2 * (m - 1 - i `div` 2) + i `mod` 2))

--------------------------------------------------------------------------------
-- Gradients
--------------------------------------------------------------------------------

-- | A paint where it lands on the display.
data DevicePaint
  = DeviceSolid !Color
  | DeviceRamp !Ramp

-- | A linear gradient where it lands: its value at @(x, y)@ is
-- @gx x + gy y + g0@, and its stops' offsets rise within 0 and 1.
data Ramp = Ramp !Float !Float !Float ![(Float, Color)]

-- | A paint laid out in the coordinates a transform takes to the display's.
-- A gradient's value at a point is its value where the transform's inverse
-- takes the point back, which is again a linear function of the point.
devicePaint :: Transform -> Paint -> DevicePaint
devicePaint _ (Solid c) = DeviceSolid c
devicePaint t@(Transform a b c d _ _) (Linear (V2 x0 y0) (V2 x1 y1) stops0) = case stops of
  [] -> DeviceSolid (Color 0)
  [(_, col)] -> DeviceSolid col
  _
    | len2 > 0 && det /= 0 && all finite [gx, gy, g0] -> DeviceRamp (Ramp gx gy g0 stops)
    | otherwise -> DeviceSolid (snd (last stops))
  where
    stops = rising 0 stops0
    rising lo ((o, col) : rest) = let o' = max lo (min 1 o) in (o', col) : rising o' rest
    rising _ [] = []
    dx = x1 - x0
    dy = y1 - y0
    len2 = dx * dx + dy * dy
    det = a * d - b * c
    -- The gradient's direction over its length squared, through the
    -- inverse transpose of the transform's linear part.
    gx = (d * dx - b * dy) / det / len2
    gy = (a * dy - c * dx) / det / len2
    (px, py) = applyTransform t x0 y0
    g0 = negate (gx * px + gy * py)

-- | A gradient's value at a point.
{-# INLINE rampAt #-}
rampAt :: Ramp -> Float -> Float -> Float
rampAt (Ramp gx gy g0 _) x y = gx * x + gy * y + g0

-- | The values a gradient's colour bends at: its stops' offsets.
rampSplits :: Ramp -> [Float]
rampSplits (Ramp _ _ _ stops) = dedupe (map fst stops)
  where
    dedupe (v : w : rest) | v == w = dedupe (w : rest)
    dedupe (v : rest) = v : dedupe rest
    dedupe [] = []

-- | The gradient's colour at each point.
rampShades :: Ramp -> PrimArray Float -> PrimArray Word32
rampShades ramp@(Ramp _ _ _ stops) pts =
  generatePrimArray (sizeofPrimArray pts `div` 2) $ \i ->
    let (x, y) = pointAt pts i
        Color w = rampColor stops (rampAt ramp x y)
     in w

-- | The colour at a value among rising stops.
rampColor :: [(Float, Color)] -> Float -> Color
rampColor stops v = case stops of
  (o0, c0) : _ | v <= o0 -> c0
  _ -> go stops
  where
    go ((o0, c0) : rest@((o1, c1) : _))
      | v < o1 = lerpColor c0 c1 ((v - o0) / (o1 - o0))
      | otherwise = go rest
    go [(_, c)] = c
    go [] = Color 0

-- | The points of a polyline, @closed@ or not, with a point added wherever
-- an edge crosses a value of @g@ in @vs@, in order along the edge.
splitEdges :: (Float -> Float -> Float) -> [Float] -> Bool -> PrimArray Float -> PrimArray Float
splitEdges g vs closed pts = primArrayFromList (concat [x : y : cuts i | i <- [0 .. m - 1], let (x, y) = pointAt pts i])
  where
    m = sizeofPrimArray pts `div` 2
    cuts i
      | i == m - 1 && not closed = []
      | otherwise =
          let (ax, ay) = pointAt pts i
              (bx, by) = pointAt pts ((i + 1) `mod` m)
              ga = g ax ay
              gb = g bx by
              us = sortOn id [(v - ga) / (gb - ga) | v <- vs, min ga gb < v, v < max ga gb]
           in concat [[ax + u * (bx - ax), ay + u * (by - ay)] | u <- us]

-- | Triangles cut along the lines where @g@ takes a value in @vs@ (rising),
-- so that no piece crosses one: the points with the corners the cuts add
-- after them, and the pieces' index triples. A cut through an edge is
-- worked out from the edge's lower-numbered end, so the two triangles
-- either side of it put the corner in the same place.
splitTriangles :: (Float -> Float -> Float) -> [Float] -> PrimArray Float -> PrimArray Int -> (PrimArray Float, PrimArray Int)
splitTriangles g vs pts tris = (primArrayFromList (primArrayToList pts ++ concat [[x, y] | (x, y) <- reverse added]), primArrayFromList (reverse out))
  where
    n0 = sizeofPrimArray pts `div` 2
    corner i = let (x, y) = pointAt pts i in (i, x, y, g x y)
    (_, added, out) = foldl' triangle (n0, [], []) [(indexPrimArray tris (3 * t), indexPrimArray tris (3 * t + 1), indexPrimArray tris (3 * t + 2)) | t <- [0 .. sizeofPrimArray tris `div` 3 - 1]]
    triangle acc (i, j, k) =
      let poly = map corner [i, j, k]
          gs = [v | (_, _, _, v) <- poly]
          cutsHere = [v | v <- vs, minimum gs < v, v < maximum gs]
          (acc', rest) = foldl' cut (acc, poly) cutsHere
       in fanOut acc' rest
    -- The part of the polygon below the value is done; the part above goes on.
    cut ((next, adds, tris'), poly) v =
      let edges = zip poly (drop 1 poly ++ take 1 poly)
          step (nx, ads, lower, upper) (p@(_, _, _, gp), q@(_, _, _, gq))
            | (gp < v && v < gq) || (gq < v && v < gp) =
                let (a, b) = if first p q then (p, q) else (q, p)
                    (_, ax, ay, ga) = a
                    (_, bx, by, gb) = b
                    u = (v - ga) / (gb - ga)
                    c = (nx, ax + u * (bx - ax), ay + u * (by - ay), v)
                    (_, cx, cy, _) = c
                 in (nx + 1, (cx, cy) : ads, c : keepBelow p lower, c : keepAbove p upper)
            | otherwise = (nx, ads, keepBelow p lower, keepAbove p upper)
          keepBelow p@(_, _, _, gp) acc = if gp <= v then p : acc else acc
          keepAbove p@(_, _, _, gp) acc = if gp >= v then p : acc else acc
          (next', adds', belowR, aboveR) = foldl' step (next, adds, [], []) edges
          -- The crossing goes after its edge's first end.
          below = reverse belowR
          above = reverse aboveR
          (next'', adds'', tris'') = fanOut (next', adds', tris') below
       in ((next'', adds'', tris''), above)
    first (i, _, _, _) (j, _, _, _) = i <= j
    fanOut acc@(nx, ads, ts) poly = case poly of
      (a, _, _, _) : rest@(_ : _ : _) -> (nx, ads, foldl' (\acc' ((b, _, _, _), (c, _, _, _)) -> c : b : a : acc') ts (zip rest (drop 1 rest)))
      _ -> acc

--------------------------------------------------------------------------------
-- Dashes
--------------------------------------------------------------------------------

-- | The dashes of a polyline, @closed@ or not, for a dash pattern and how far
-- into it the line starts: each an open polyline, a zero-length one a hair
-- long so that its caps have a direction. 'Nothing' for a solid line: no
-- pattern, or one with a negative or infinite length or all zero, or one
-- that would cut the line into more than 'maxDashes' dashes.
--
-- Kept out of line: inlined, its bindings are allocated for every stroke,
-- dashed or not.
{-# NOINLINE dashes #-}
dashes :: [Float] -> Float -> Bool -> PrimArray Float -> Maybe [PrimArray Float]
dashes pattern0 offset closed pts
  | null pattern0 || any (\v -> not (v >= 0) || isInfinite v) pattern0 || not (period > 0) || isInfinite period = Nothing
  | not (finite offset) || fromIntegral (length pattern `div` 2) * (total / period + 1) > (fromIntegral maxDashes :: Float) = Nothing
  | otherwise = case path of
      p0 : rest -> Just [primArrayFromList (concat [[x, y] | (x, y) <- d]) | d <- walk k0 left0 [p0 | even k0] (0, 0) p0 rest]
      [] -> Nothing
  where
    pattern = if odd (length pattern0) then pattern0 ++ pattern0 else pattern0
    plen = length pattern
    entries = primArrayFromListN plen pattern
    entry k = indexPrimArray entries (k `mod` plen)
    period = sum pattern
    m = sizeofPrimArray pts `div` 2
    path = [pointAt pts i | i <- [0 .. m - 1]] ++ [pointAt pts 0 | closed]
    total = sum (zipWith dist path (drop 1 path))
    dist (ax, ay) (bx, by) = sqrt ((bx - ax) * (bx - ax) + (by - ay) * (by - ay))
    -- The entry the line starts in, and how much of it is left. A dash of
    -- no length at the very start is kept, as a dot.
    (k0, left0) = skip 0 (offset - period * fromIntegral (floor (offset / period) :: Int))
    skip k o
      | k < 2 * plen && o >= entry k && (entry k > 0 || o > 0) = skip (k + 1) (o - entry k)
      | otherwise = (k, entry k - o)
    -- In entry @k@ with @left@ of it to go, at point @p@ heading along
    -- direction @dir@, with the dash so far reversed in @cur@ while in one.
    walk :: Int -> Float -> [(Float, Float)] -> (Float, Float) -> (Float, Float) -> [(Float, Float)] -> [[(Float, Float)]]
    walk k left cur dir p qs = case qs of
      []
        | even k -> [dash dir cur | not (null cur)]
        -- A gap that ends with the line, before a dash of no length: a dot
        -- at the end.
        | left <= 0 && entry (k + 1) == 0 -> [dash dir [p]]
        | otherwise -> []
      q : rest
        | len <= left -> walk k (left - len) (if even k then q : cur else cur) dir' q rest
        | otherwise ->
            let u = left / len
                mid = (fst p + u * (fst q - fst p), snd p + u * (snd q - snd p))
             in if even k
                  then dash dir' (mid : cur) : walk (k + 1) (entry (k + 1)) [] dir' mid qs
                  else walk (k + 1) (entry (k + 1)) [mid] dir' mid qs
        where
          len = dist p q
          dir' = if len > 0 then ((fst q - fst p) / len, (snd q - snd p) / len) else dir
    -- A dash's points in order, repeats dropped; a single point gets a
    -- second a hair along the way the line runs.
    dash (dx, dy) ds = case dropRepeats (reverse ds) of
      [(x, y)] -> [(x, y), (x + 1e-3 * dx, y + 1e-3 * dy)]
      kept -> kept
    dropRepeats (a : b : rest)
      | dist a b < 1e-4 = dropRepeats (a : rest)
      | otherwise = a : dropRepeats (b : rest)
    dropRepeats xs = xs

-- | The most dashes a subpath is cut into; past that it is drawn solid.
maxDashes :: Int
maxDashes = 4096

--------------------------------------------------------------------------------
-- Ops under a transform
--------------------------------------------------------------------------------

-- | An op drawn under transform @t@, flattening what it has to within
-- @tol@. A rect keeps its op under a transform that keeps its sides level
-- and upright (a scale, a flip, a quarter turn), and a rounded rect under
-- one of those that scales both ways alike; a circle keeps its op under a
-- transform that keeps it round. Otherwise they become polygons. Lines,
-- polygons and triangles move their points, their widths scaled by the
-- transform's 'averageStretch'. A gradient keeps its op under a transform
-- that keeps rects, filling the transformed rect, its corners taking the
-- colours of the corners that land nearest them; under any other it is a
-- polygon with a colour at each corner, drawn as it would be unturned. An
-- unturned image keeps its rect under a transform with no rotation or skew,
-- turning over with a flip, and otherwise turns, as a turned image always
-- does: its centre moves, it scales and it turns with the transform. Text
-- moves its anchor, and a transform that scales it (by its
-- 'averageStretch') scales its font; its glyphs do not turn. A clip is the
-- bounding box of its transformed rect. A transform with a NaN or infinite
-- entry draws nothing, as does a rounded rect or circle with no size.
transformOp :: Float -> Transform -> DrawOp -> [DrawOp]
transformOp tol t@(Transform a b c d _ _) op
  | not (transformFinite t) = []
  | otherwise = case op of
      FillRect r col
        | keepsRects -> [FillRect (box r) col]
        | otherwise -> fillPathOps tol t NonZero (rect r) (Solid col)
      FillRoundedRect r rad col
        | empty r -> []
        | keepsRects && uniform -> [FillRoundedRect (box r) (rad * k) col]
        | otherwise -> fillPathOps tol t NonZero (roundedRect r rad) (Solid col)
      FillTriangle x0 y0 x1 y1 x2 y2 col ->
        let (x0', y0') = applyTransform t x0 y0
            (x1', y1') = applyTransform t x1 y1
            (x2', y2') = applyTransform t x2 y2
         in [FillTriangle x0' y0' x1' y1' x2' y2' col]
      FillCircle cx cy rad col
        | not (rad > 0) -> []
        | conformal -> let (cx', cy') = applyTransform t cx cy in [FillCircle cx' cy' (rad * k) col]
        | otherwise -> fillPathOps tol t NonZero (ellipse (V2 cx cy) (V2 rad rad)) (Solid col)
      Stroke x0 y0 x1 y1 w col -> line Stroke x0 y0 x1 y1 w col
      StrokeRoundedRect r rad w col
        | empty r -> []
        | keepsRects && uniform -> [StrokeRoundedRect (box r) (rad * k) (w * k) col]
        | otherwise ->
            -- The border lies inside the rect, so its centre line is the
            -- rect inset by half the width.
            let Rect x y rw rh = r
                bw = min w (min rw rh / 2)
             in strokePathOps tol t (stroke bw) (roundedRect (Rect (x + bw / 2) (y + bw / 2) (rw - bw) (rh - bw)) (rad - bw / 2)) (Solid col)
      StrokeCircle cx cy rad w col
        | not (rad > 0) -> []
        | conformal -> let (cx', cy') = applyTransform t cx cy in [StrokeCircle cx' cy' (rad * k) (w * k) col]
        | otherwise ->
            let bw = min w rad
             in strokePathOps tol t (stroke bw) (ellipse (V2 cx cy) (V2 (rad - bw / 2) (rad - bw / 2))) (Solid col)
      StrokeLineAA x0 y0 x1 y1 w col -> line StrokeLineAA x0 y0 x1 y1 w col
      FillPolygon pts rings tris sh -> [FillPolygon (points pts) rings tris sh]
      StrokePolyline pts w closed cap join limit sh -> [StrokePolyline (points pts) (w * k) closed cap join limit sh]
      FillQuadGradient r tl tr br bl
        | not keepsRects ->
            -- Its corners where the transform takes them, in the order
            -- and with the two triangles the unturned quad is drawn with.
            let quad = primArrayFromList (concat [[x, y] | (x, y) <- corners r])
             in [ FillPolygon quad (primArrayFromListN 2 [0, 4]) (primArrayFromListN 6 [0, 1, 2, 0, 2, 3]) (Shaded (primArrayFromListN 4 [w | Color w <- [tl, tr, br, bl]]))
                | abs (a * d - b * c) > 0
                ]
        | otherwise ->
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
      DrawImage r angle tex u0 v0 u1 v1 col
        | angle == 0 && levelAxes ->
            let (u0', u1') = if a < 0 then (u1, u0) else (u0, u1)
                (v0', v1') = if d < 0 then (v1, v0) else (v0, v1)
             in [DrawImage (box r) 0 tex u0' v0' u1' v1' col]
        | otherwise -> [turnedImage r angle tex u0 v0 u1 v1 col]
      DrawText x y ax ay txt col
        | scaled -> [DrawTextAligned x' y' ax ay k defaultTextFont txt col]
        | otherwise -> [DrawText x' y' ax ay txt col]
        where
          (x', y') = applyTransform t x y
      DrawTextStyled x y font txt col
        | scaled -> [DrawTextAligned x' y' 0 1 k font txt col]
        | otherwise -> [DrawTextStyled x' y' font txt col]
        where
          (x', y') = applyTransform t x y
      DrawTextAligned x y ax ay s font txt col ->
        let (x', y') = applyTransform t x y in [DrawTextAligned x' y' ax ay (if scaled then s * k else s) font txt col]
      PushClip r -> [PushClip (box r)]
      PopClip -> [PopClip]
  where
    k = averageStretch t
    -- Text keeps its size under a transform that barely scales it, as a
    -- rotation's rounding does, so that it keeps its font.
    scaled = abs (k - 1) > 1e-3
    -- A rect with no size, for which a rounded rect's own op draws nothing.
    empty (Rect _ _ w h) = not (w > 0 && h > 0)
    -- Zero but for rounding: a half turn's sine is not quite 0.
    rounding = 1e-6 * maximum (map abs [a, b, c, d])
    small v = abs v <= rounding
    -- No rotation or skew.
    levelAxes = small b && small c
    keepsRects = levelAxes || (small a && small d)
    uniform = small (abs a + abs b - abs c - abs d)
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
      let cs = corners r
          xs = map fst cs
          ys = map snd cs
       in Rect (minimum xs) (minimum ys) (maximum xs - minimum xs) (maximum ys - minimum ys)
    minimumOn f = foldr1 (\p q -> if f p <= f q then p else q)
    -- An image turned by @angle@ about its rect's centre, under the
    -- transform: the centre goes where the transform takes it, each side
    -- stretches as the transform stretches the turned image's axis along it,
    -- and the image turns as its x axis does, over on its v axis when the
    -- transform flips. That is exact while the transform keeps the turned
    -- axes square, as a rotation, a uniform scale and a flip do; under a
    -- skew the image stays a rect rather than a parallelogram.
    turnedImage (Rect x y w h) angle tex u0 v0 u1 v1 col =
      let (cx, cy) = applyTransform t (x + w / 2) (y + h / 2)
          cs = cos angle
          sn = sin angle
          exX = a * cs + c * sn
          exY = b * cs + d * sn
          eyX = c * cs - a * sn
          eyY = d * cs - b * sn
          w' = w * sqrt (exX * exX + exY * exY)
          h' = h * sqrt (eyX * eyX + eyY * eyY)
          (v0', v1') = if a * d - b * c < 0 then (v1, v0) else (v0, v1)
       in DrawImage (Rect (cx - w' / 2) (cy - h' / 2) w' h') (atan2 exY exX) tex u0 v0' u1 v1' col
