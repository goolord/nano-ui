-- | Paths, transforms and the ways to paint them, for canvas drawing and
-- for qualified import:
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
-- 'NanoUI.Widgets.Custom.drawPath' and 'NanoUI.Widgets.Custom.drawPathWith'
-- fill a path, and 'NanoUI.Widgets.Custom.drawStrokePath' and
-- 'NanoUI.Widgets.Custom.drawStrokePathWith' stroke it, flattening its
-- curves to within a quarter of a device pixel. A fill closes every subpath
-- and fills what its 'FillRule' says is inside: a subpath inside another is
-- a hole in it where the rule leaves it unfilled, as the inner circle of a
-- ring drawn with 'EvenOdd', or with 'NonZero' and the two circles going
-- opposite ways. Subpaths that cross each other each fill on their own, and
-- one that crosses itself (a pentagram drawn in one line) may fill only in
-- part: a fill does not work out where paths cross. A stroke is centred on
-- its path, as 'Stroke' says: its width, how its ends are capped, how its
-- corners join, and its dashes.
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
