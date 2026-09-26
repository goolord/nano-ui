-- | Paths and transforms for canvas drawing, for qualified import:
--
-- > import NanoUI.Path qualified as P
-- >
-- > sketch :: Rect -> CanvasM ()
-- > sketch (Rect x y w h) = do
-- >   let c = V2 (x + w / 2) (y + h / 2)
-- >       slice a0 sweep col = drawPath (P.moveTo c <> P.arc c 40 a0 sweep <> P.close) col
-- >   slice 0 (pi / 2) (colorRGBA 220 80 60 255)
-- >   slice (pi / 2) (3 * pi / 2) (colorRGBA 60 120 220 255)
-- >   withTransform (P.rotateAround c (pi / 8)) $
-- >     drawStrokePath (P.rect (Rect (x + 10) (y + 10) 40 40)) 2 (colorRGBA 30 30 30 255)
--
-- A 'Path' is a list of steps joined with '<>'. 'moveTo' starts a subpath;
-- 'lineTo', 'quadTo', 'cubicTo', 'arc' and 'arcTo' extend it from its
-- current point, and 'close' runs a line back to its start. A step with no
-- subpath open starts one where the last one closed, or at the origin. The
-- shapes ('rect', 'roundedRect', 'circle', 'ellipse', 'polygon', 'polyline')
-- are subpaths of their own, so they join onto nothing before them.
--
-- Angles are in radians, and with y pointing down a positive angle turns
-- clockwise on screen.
--
-- 'NanoUI.Widgets.Custom.drawPath' fills a path and
-- 'NanoUI.Widgets.Custom.drawStrokePath' strokes it, flattening its curves
-- to within a quarter of a device pixel. A fill fills each subpath on its
-- own, as if closed: a subpath inside another is drawn over it, not cut out
-- of it, and one that crosses itself may fill only in part. Draw a ring as a
-- thick stroke, or its hole over it in the background colour. A stroke's
-- corners are mitered, a very sharp one cut short.
module NanoUI.Path
  ( -- * Paths
    Path
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
  , circle
  , ellipse
  , polygon
  , polyline
    -- * Stroke ends
  , LineCap (..)
    -- * Transforms
  , Transform
  , translate
  , rotate
  , rotateAround
  , scale
  , affine
  , transformPoint
  ) where

import NanoUI.Internal.Path
  ( LineCap (..)
  , Path (..)
  , Segment (..)
  , Transform (..)
  , applyTransform
  , ellipse
  , rect
  , roundedRect
  )
import NanoUI.Internal.Types (V2 (..))

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
arc (V2 cx cy) r = ellipticalArc (V2 cx cy) (V2 r r) 0

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
-- draws a straight line.
arcTo :: V2 -> Float -> Bool -> Bool -> V2 -> Path
arcTo (V2 rx ry) rot large clockwise (V2 x y) = Path [SegArcTo rx ry rot large clockwise x y]

-- | Close the subpath with a line back to its start. A stroke joins a
-- closed subpath's ends instead of capping them.
close :: Path
close = Path [SegClose]

-- | A circle: its centre and radius.
circle :: V2 -> Float -> Path
circle c r = ellipse c (V2 r r)

-- | A closed polygon through the points.
polygon :: [V2] -> Path
polygon pts = polyline pts <> if null pts then mempty else close

-- | An open line through the points.
polyline :: [V2] -> Path
polyline [] = mempty
polyline (V2 x y : rest) = Path (SegMove x y : [SegLine px py | V2 px py <- rest])

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
