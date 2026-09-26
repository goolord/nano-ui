-- | The canvas: 'CanvasM', the block a custom widget draws with, and what
-- it draws. "NanoUI.Widgets.Custom" exports the API; this module also has
-- the representation, for tools that build draw ops of their own.
module NanoUI.Internal.Canvas
  ( -- * Canvas blocks
    CanvasM (..)
  , CanvasEnv (..)
  , runCanvas
  , runCanvasFor
  , runCanvasScaled
  , emitOp
  , emitWith
  , drawContext
    -- * Shapes
  , drawRect
  , drawRoundedRect
  , drawCircle
  , drawStroke
  , drawStrokeRoundedRect
  , drawStrokeCircle
  , drawStrokeAA
  , drawQuadGradient
  , drawLinearGradientH
  , drawLinearGradientV
  , drawImage
  , drawImageUV
  , drawImageRotated
  , drawText
  , drawTextWith
  , drawCheckbox
    -- * Paths, clips and transforms
  , drawPath
  , drawPathWith
  , drawStrokePath
  , drawStrokePathWith
  , withTransform
  , withClip
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.State.Strict qualified as State
import Data.Maybe (fromMaybe)
import Data.Primitive.SmallArray (SmallArray, smallArrayFromList)
import Data.Text (Text)
import NanoUI.Internal.Context.Types (CustomDrawContext (..))
import NanoUI.Internal.Draw (DrawOp (..), TextFont, checkboxOps)
import NanoUI.Internal.Font (FontMetrics (fmSnapScale), monospaceMetrics)
import NanoUI.Internal.Path (FillRule (..), Paint (..), Path, Stroke, Transform, curveTolerance, fillPathOps, stroke, strokePathOps, transformOp)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Style (..), Theme (..), defaultTheme)
import NanoUI.Internal.Types (Color, ImageId (..), Rect (..), V2 (..))

-- | A block of drawing: a monad that collects draw ops, run with
-- 'runCanvasFor'.
newtype CanvasM a = CanvasM (Reader.ReaderT CanvasEnv (State.State ([DrawOp] -> [DrawOp])) a)
  deriving (Functor, Applicative, Monad)

-- | What a canvas block draws under: the transform 'withTransform' set, if
-- any; how far a flattened curve may stray from the true one, in logical
-- pixels, which a block that draws no curve never works out; and the
-- widget's draw context, which a block that does not ask for it never
-- builds.
data CanvasEnv = CanvasEnv
  { ceTransform :: !(Maybe Transform)
  , ceTolerance :: Float
  , ceContext :: CustomDrawContext
  }

-- | Compile a 'CanvasM' block into an immutable 'SmallArray DrawOp'. It
-- does not know the display's scale, so it flattens curves finely enough
-- for two device pixels to the logical one, and its 'drawContext' is a
-- widget neither hovered nor pressed in 'defaultTheme'.
{-# DEPRECATED runCanvas "runCanvas guesses the display's scale; use runCanvasFor with the widget's draw context, or canvas" #-}
{-# INLINE runCanvas #-}
runCanvas :: CanvasM a -> SmallArray DrawOp
runCanvas =
  runCanvasScaled
    (CustomDrawContext False False False False False defaultTheme (monospaceMetrics 16) {fmSnapScale = 2})
    2

-- | Run a custom widget's drawing, flattening curves to within a quarter
-- of a device pixel on the display its draw context is for, and handing
-- the block that context ('drawContext'):
--
-- > widgetDraw = \cdc rect -> runCanvasFor cdc (drawPath (P.circle (V2 20 20) 12) accent)
{-# INLINE runCanvasFor #-}
runCanvasFor :: CustomDrawContext -> CanvasM a -> SmallArray DrawOp
runCanvasFor cdc = runCanvasScaled cdc (fmSnapScale (cdcFont cdc))

-- | Run a block for a draw context on a display of this many device
-- pixels to the logical one.
{-# INLINE runCanvasScaled #-}
runCanvasScaled :: CustomDrawContext -> Float -> CanvasM a -> SmallArray DrawOp
runCanvasScaled cdc scale (CanvasM m) =
  smallArrayFromList (State.execState (Reader.runReaderT m (CanvasEnv Nothing (curveTolerance scale) cdc)) id [])

-- | Draw an op, through the block's transform.
{-# INLINE emitOp #-}
emitOp :: DrawOp -> CanvasM ()
emitOp op = CanvasM $ do
  env <- Reader.ask
  lift $ case ceTransform env of
    Nothing -> State.modify (. (op :))
    Just t -> emitOps (transformOp (ceTolerance env) t op)

{-# INLINE emitOps #-}
emitOps :: [DrawOp] -> State.State ([DrawOp] -> [DrawOp]) ()
emitOps ops = State.modify (. (ops ++))

-- | Ops built from the block's transform, the identity for none, and its
-- curve tolerance.
{-# INLINE emitWith #-}
emitWith :: (Transform -> Float -> [DrawOp]) -> CanvasM ()
emitWith build = CanvasM $ do
  env <- Reader.ask
  lift (emitOps (build (fromMaybe mempty (ceTransform env)) (ceTolerance env)))

-- | The draw context of the widget the block draws: whether it is hovered,
-- pressed or focused, its theme, and the font it measures text with.
--
-- > canvas (fixedWH 40 40) $ \r -> do
-- >   cdc <- drawContext
-- >   drawRoundedRect r 6 (if cdcHovered cdc then themeAccent (cdcTheme cdc) else themeMuted (cdcTheme cdc))
{-# INLINE drawContext #-}
drawContext :: CanvasM CustomDrawContext
drawContext = CanvasM (Reader.asks ceContext)

-- | Fill a solid rectangle.
drawRect :: Rect -> Color -> CanvasM ()
drawRect r c = emitOp (FillRect r c)

-- | Fill a rounded rectangle with given corner radius.
drawRoundedRect :: Rect -> Float -> Color -> CanvasM ()
drawRoundedRect r radius c = emitOp (FillRoundedRect r radius c)

-- | Fill a solid circle at center with given radius.
drawCircle :: V2 -> Float -> Color -> CanvasM ()
drawCircle (V2 cx cy) radius c = emitOp (FillCircle cx cy radius c)

-- | A straight line between two points, this thick, its edges not
-- anti-aliased: slanted, they are jagged. 'drawStrokeAA' draws the same line
-- smooth.
drawStroke :: V2 -> V2 -> Float -> Color -> CanvasM ()
drawStroke (V2 x0 y0) (V2 x1 y1) thickness c = emitOp (Stroke x0 y0 x1 y1 thickness c)

-- | Stroke a rounded rectangle's border, this wide, with the given corner
-- radius. The border lies inside the rectangle, as a panel's does, where a
-- path's stroke ('drawStrokePath') is centred on the path.
drawStrokeRoundedRect :: Rect -> Float -> Float -> Color -> CanvasM ()
drawStrokeRoundedRect r radius thickness c = emitOp (StrokeRoundedRect r radius thickness c)

-- | Stroke a circle's outline, this wide, inside the circle, where a
-- path's stroke is centred on the path.
drawStrokeCircle :: V2 -> Float -> Float -> Color -> CanvasM ()
drawStrokeCircle (V2 cx cy) radius thickness c = emitOp (StrokeCircle cx cy radius thickness c)

-- | An anti-aliased straight line between two points, this thick, cut
-- square at its ends.
drawStrokeAA :: V2 -> V2 -> Float -> Color -> CanvasM ()
drawStrokeAA (V2 x0 y0) (V2 x1 y1) thickness c = emitOp (StrokeLineAA x0 y0 x1 y1 thickness c)

-- | Four-corner gradient fill (top-left, top-right, bottom-right,
-- bottom-left), blended across the two triangles from the top left corner.
-- For a gradient along any line, or over a path, use 'drawPathWith' and a
-- 'NanoUI.Path.Linear' paint.
drawQuadGradient :: Rect -> Color -> Color -> Color -> Color -> CanvasM ()
drawQuadGradient r tl tr br bl = emitOp (FillQuadGradient r tl tr br bl)

-- | Horizontal 2-color linear gradient fill (left to right).
drawLinearGradientH :: Rect -> Color -> Color -> CanvasM ()
drawLinearGradientH r leftCol rightCol = emitOp (FillQuadGradient r leftCol rightCol rightCol leftCol)

-- | Vertical 2-color linear gradient fill (top to bottom).
drawLinearGradientV :: Rect -> Color -> Color -> CanvasM ()
drawLinearGradientV r topCol botCol = emitOp (FillQuadGradient r topCol topCol botCol botCol)

-- | Draw a textured image stretched over given rectangle.
drawImage :: Rect -> ImageId -> Color -> CanvasM ()
drawImage r (ImageId tid) c = emitOp (DrawImageRect r tid 0 0 1 1 c)

-- | Draw a sub-region of a textured image with explicit UV texture coordinates.
drawImageUV :: Rect -> ImageId -> Float -> Float -> Float -> Float -> Color -> CanvasM ()
drawImageUV r (ImageId tid) u0 v0 u1 v1 c = emitOp (DrawImageRect r tid u0 v0 u1 v1 c)

-- | 'drawImage' turned about the rectangle's centre by an angle in radians,
-- clockwise on screen. Whatever leaves the canvas is clipped.
drawImageRotated :: Rect -> Float -> ImageId -> Color -> CanvasM ()
drawImageRotated r angle (ImageId tid) c = emitOp (DrawImageRotated r angle tid 0 0 1 1 c)

-- | Draw text in the context's font, placed on a point by its horizontal
-- and vertical alignment: 'AlignStart' puts the point at the text's left,
-- 'AlignTop' at its line's top, and 'AlignBaseline' on its baseline.
drawText :: V2 -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawText (V2 x y) alignX alignY txt col = emitOp (DrawText x y (alignXFrac alignX) (alignYFrac alignY) txt col)

-- | 'drawText' in a font of its own: a size, a variant such as 'FontMono',
-- a weight, a slant and a decoration, as a label's layout picks them.
--
-- > drawTextWith defaultTextFont {textFontSize = 20, textFontWeight = WeightBold} (V2 cx cy) AlignCenter AlignMiddle "42" ink
--
-- Under a transform the point moves and the font scales as the transform
-- scales (by the square root of its area scale); the glyphs do not turn.
drawTextWith :: TextFont -> V2 -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawTextWith font (V2 x y) alignX alignY txt col = emitOp (DrawTextAligned x y (alignXFrac alignX) (alignYFrac alignY) 1 font txt col)

-- | Where across the text an alignment puts its point, as 'DrawText'
-- takes it.
alignXFrac :: AlignX -> Float
alignXFrac = \case AlignStart -> 0; AlignCenter -> 0.5; AlignEnd -> 1

-- | Where up the line an alignment puts its point; below zero for the
-- baseline.
alignYFrac :: AlignY -> Float
alignYFrac = \case AlignTop -> 1; AlignMiddle -> 0.5; AlignBottom -> 0; AlignBaseline -> -1

-- | A checkbox's box as the checkbox widget draws it, in the square at the
-- rect's top-left corner as wide as the rect's shorter side: the theme's
-- accent with a check mark when checked, otherwise an input well with the
-- theme's button border. The widget draws it 'NanoUI.Widgets.Custom.checkboxBoxSize' wide.
drawCheckbox :: Theme -> Rect -> Bool -> CanvasM ()
drawCheckbox theme (Rect x y w h) = checkboxOps emitOp theme (styleBorder (themeButton theme)) x y (min w h)

-- | Fill a path built with "NanoUI.Path" in a colour, covering what winds
-- round it at all ('NanoUI.Path.NonZero').
drawPath :: Path -> Color -> CanvasM ()
drawPath path col = drawPathWith NonZero path (Solid col)

-- | Fill a path by a fill rule with a paint. Each subpath is closed; one
-- inside another is a hole in it where the rule leaves it unfilled. The
-- fill's outline is anti-aliased, its holes' too.
--
-- > drawPathWith P.EvenOdd (P.circle c 20 <> P.circle c 12) (P.Solid ink)  -- a ring
drawPathWith :: FillRule -> Path -> Paint -> CanvasM ()
drawPathWith rule path paint = emitWith (\t tol -> fillPathOps tol t rule path paint)

-- | Stroke a path built with "NanoUI.Path" this wide in a colour, centred
-- on the path: 'drawStrokePathWith' and 'NanoUI.Path.stroke'.
drawStrokePath :: Path -> Float -> Color -> CanvasM ()
drawStrokePath path w col = drawStrokePathWith (stroke w) path (Solid col)

-- | Stroke a path, centred on it, as a 'NanoUI.Path.Stroke' says: its
-- width, caps, joins and dashes. Each subpath, or each dash, is one
-- anti-aliased line, so a translucent one does not darken where it meets
-- itself at a corner or a cap.
--
-- > drawStrokePathWith (P.stroke 3) {P.strokeCap = P.RoundCap, P.strokeJoin = P.RoundJoin} path (P.Solid ink)
drawStrokePathWith :: Stroke -> Path -> Paint -> CanvasM ()
drawStrokePathWith st path paint = emitWith (\t tol -> strokePathOps tol t st path paint)

-- | Draw a block through a transform from "NanoUI.Path", inside any it is
-- already in, which applies after it: @withTransform (P.translate 40 40 <>
-- P.rotate a)@ turns what the block draws by @a@ about the origin, then
-- moves it 40 right and 40 down.
--
-- Paths are transformed before they are flattened, so a curve scaled up
-- stays smooth, and a stroke's width and dashes scale with the transform
-- (by the square root of its area scale, for one that scales x and y
-- apart), as does a gradient. The other ops follow as far as their shapes
-- allow. Rects, rounded rects, circles and their outlines keep their own
-- ops while they keep their shape, and otherwise become paths: a rect
-- turned other than by quarter turns is a polygon, and a circle scaled on
-- one axis an ellipse. Lines and triangles move their points, and a
-- four-corner gradient turns with its rect. An image turns and scales with
-- the transform, and turns over with a flip; under a skew it stays a rect.
-- Text moves its point, and its font scales as the transform does; its
-- glyphs do not turn. A clip ('withClip') is the box round its turned rect.
withTransform :: Transform -> CanvasM a -> CanvasM a
withTransform t (CanvasM m) =
  CanvasM (Reader.local (\env -> env {ceTransform = Just (maybe t (<> t) (ceTransform env))}) m)

-- | Draw a block clipped to a rect, inside the clip it is already drawn in
-- (the widget's own rect, at most). Under a transform that turns the rect
-- other than by quarter turns, the clip is the box round the turned rect.
--
-- > withClip (Rect x y w 20) (drawText (V2 x y) AlignStart AlignTop longTitle ink)
withClip :: Rect -> CanvasM a -> CanvasM a
withClip r body = emitOp (PushClip r) *> body <* emitOp PopClip
