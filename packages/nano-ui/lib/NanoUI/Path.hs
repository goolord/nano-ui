-- | Paths, transforms and paints for canvas drawing. Import qualified:
--
-- > import NanoUI.Path qualified as P
-- >
-- > sketch :: Rect -> CanvasM ()
-- > sketch (Rect x y w h) = do
-- >   let c = V2 (x + w / 2) (y + h / 2)
-- >       slice a0 sweep col = drawPath (P.moveTo c <> P.arc c 40 a0 sweep <> P.close) col
-- >   slice 0 (pi / 2) (colorRGBA 220 80 60 255)
-- >   slice (pi / 2) (3 * pi / 2) (colorRGBA 60 120 220 255)
-- >   -- A ring: the inner circle is a hole in the outer one.
-- >   drawPathWith P.EvenOdd (P.circle c 30 <> P.circle c 20) (P.Solid (colorRGBA 30 30 30 255))
-- >   withTransform (P.rotateAround c (pi / 8)) $
-- >     drawStrokePathWith (P.stroke 2) {P.strokeJoin = P.RoundJoin, P.strokeDash = [6, 3]}
-- >       (P.rect (Rect (x + 10) (y + 10) 40 40))
-- >       (P.Linear (V2 x y) (V2 (x + 60) y) [(0, colorRGBA 30 30 30 255), (1, colorRGBA 60 120 220 255)])
--
-- A 'Path' is a sequence of steps joined with '<>'. 'moveTo' starts a
-- subpath; 'lineTo', 'quadTo', 'cubicTo', 'arc' and 'arcTo' extend it from
-- the current point; 'close' draws a line back to its start. A step with no
-- open subpath starts one where the last subpath closed, or at the origin.
-- Shapes ('rect', 'roundedRect', 'circle', 'ellipse', 'polygon', 'polyline')
-- are separate subpaths and never join onto what precedes them.
--
-- Angles are in radians. Since y points down, positive angles turn clockwise
-- on screen.
--
-- 'NanoUI.Widgets.Custom.drawPath' and 'NanoUI.Widgets.Custom.drawPathWith'
-- fill a path; 'NanoUI.Widgets.Custom.drawStrokePath' and
-- 'NanoUI.Widgets.Custom.drawStrokePathWith' stroke it. Curves are flattened
-- to within a quarter of a device pixel.
--
-- A fill closes every subpath and fills the inside given by its 'FillRule'.
-- A nested subpath is a hole when the rule leaves it unfilled: with
-- 'EvenOdd', or with 'NonZero' when the two subpaths wind in opposite
-- directions. Intersections are not resolved: crossing subpaths fill
-- independently, and a self-intersecting one (a pentagram in one line) may
-- fill only partly.
--
-- A stroke is centred on its path; 'Stroke' sets its width, caps, joins and
-- dashes.
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
  , roundedRectCorners
  , circle
  , ellipse
  , polygon
  , polyline
    -- * Strokes
  , Stroke (strokeWidth, strokeCap, strokeJoin, strokeMiterLimit, strokeDash, strokeDashOffset)
  , stroke
  , LineCap (..)
  , LineJoin (..)
    -- * Fills and paints
  , FillRule (..)
  , Paint (..)
    -- * Transforms
  , Transform
  , translate
  , rotate
  , rotateAround
  , scale
  , affine
  , transformPoint
  , invert
  ) where

import NanoUI.Internal.Path
