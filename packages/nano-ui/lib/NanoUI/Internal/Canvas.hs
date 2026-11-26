{-# OPTIONS_HADDOCK not-home #-}

-- | 'CanvasM', the monad custom widgets draw with. "NanoUI.Widgets.Custom"
-- exports the API; this module also exposes the representation for tools
-- that build their own draw ops.
module NanoUI.Internal.Canvas
  ( -- * Canvas blocks
    CanvasM (..)
  , CanvasEnv (..)
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
  , ImageDraw (..)
  , imageDraw
  , drawImageWith
  , drawText
  , drawTextWith
  , drawTextClipped
  , drawTextClippedWith
  , drawCheckbox
    -- * Paths, clips and transforms
  , drawPath
  , drawPathWith
  , drawStrokePath
  , drawStrokePathWith
  , withTransform
  , withClip
  ) where

import Control.Monad (when)
import Control.Monad.ST (ST, runST)
import Data.Maybe (fromMaybe)
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Primitive.SmallArray
  ( SmallArray
  , SmallMutableArray
  , copySmallMutableArray
  , emptySmallArray
  , getSizeofSmallMutableArray
  , newSmallArray
  , shrinkSmallMutableArray
  , unsafeFreezeSmallArray
  , writeSmallArray
  )
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context.Types (CustomDrawContext (..))
import NanoUI.Internal.Draw (DrawOp (..), TextFont (..), checkboxOps, defaultTextFont)
import NanoUI.Internal.Font (FontMetrics (fmAscent, fmSnapScale), tabularFontMetrics, truncateTextPure)
import NanoUI.Internal.Image (ImageDraw (..), imageDraw, imageDrawOp)
import NanoUI.Internal.Path (FillRule (..), Paint (..), Path, Stroke, Transform, curveTolerance, fillPathOps, stroke, strokePathOps, transformOp)
import NanoUI.Internal.Style (AlignX (..), AlignY (..), Style (..), TextEllipsis (..), Theme (..), alignXFraction)
import NanoUI.Internal.Types (Color, ImageId, Rect (..), V2 (..))

-- | Growable op buffer written directly during 'runCanvasScaled' without
-- intermediate lists or difference-list closures.
data CanvasBuf s
  = CanvasBuf
      {-# UNPACK #-} !(STRef s (SmallMutableArray s DrawOp))
      {-# UNPACK #-} !(PrimVar s Int)

-- | A drawing block that appends draw ops directly into a growable array.
-- Run it with 'runCanvasFor'.
newtype CanvasM a = CanvasM (forall s. CanvasEnv -> CanvasBuf s -> ST s a)

instance Functor CanvasM where
  {-# INLINE fmap #-}
  fmap f (CanvasM m) = CanvasM $ \env buf -> f <$> m env buf

instance Applicative CanvasM where
  {-# INLINE pure #-}
  pure a = CanvasM $ \_ _ -> pure a
  {-# INLINE (<*>) #-}
  CanvasM mf <*> CanvasM ma = CanvasM $ \env buf -> mf env buf <*> ma env buf
  {-# INLINE (*>) #-}
  CanvasM ma *> CanvasM mb = CanvasM $ \env buf -> ma env buf *> mb env buf
  {-# INLINE (<*) #-}
  CanvasM ma <* CanvasM mb = CanvasM $ \env buf -> ma env buf <* mb env buf

instance Monad CanvasM where
  {-# INLINE (>>=) #-}
  CanvasM ma >>= f = CanvasM $ \env buf -> do
    a <- ma env buf
    case f a of
      CanvasM mb -> mb env buf

-- | The environment of a canvas block: the current 'withTransform'
-- transform, the curve flattening tolerance in logical pixels, and the
-- widget's draw context. The last two are lazy, so a block that draws no
-- curve or never calls 'drawContext' does not compute them.
data CanvasEnv = CanvasEnv
  { ceTransform :: !(Maybe Transform)
  , ceTolerance :: Float
  , ceContext :: CustomDrawContext
  }

-- | Run a custom widget's drawing. Curves are flattened to within a quarter
-- of a device pixel at the context's display scale, and 'drawContext'
-- returns the context:
--
-- > widgetDraw = \cdc rect -> runCanvasFor cdc (drawPath (P.circle (V2 20 20) 12) accent)
{-# INLINE runCanvasFor #-}
runCanvasFor :: CustomDrawContext -> CanvasM a -> SmallArray DrawOp
runCanvasFor cdc = runCanvasScaled cdc (fmSnapScale (cdcFont cdc))

-- | Run a block at an explicit display scale (device pixels per logical
-- pixel).
{-# INLINE runCanvasScaled #-}
runCanvasScaled :: CustomDrawContext -> Float -> CanvasM a -> SmallArray DrawOp
runCanvasScaled cdc scale (CanvasM m) = runST $ do
  arr0 <- newSmallArray 16 PopClip
  arrRef <- newSTRef arr0
  lenVar <- newPrimVar 0
  _ <- m (CanvasEnv Nothing (curveTolerance scale) cdc) (CanvasBuf arrRef lenVar)
  n <- readPrimVar lenVar
  if n <= 0
    then pure emptySmallArray
    else do
      arr <- readSTRef arrRef
      cap <- getSizeofSmallMutableArray arr
      when (n < cap) (shrinkSmallMutableArray arr n)
      unsafeFreezeSmallArray arr

{-# NOINLINE growCanvasBuf #-}
growCanvasBuf :: STRef s (SmallMutableArray s DrawOp) -> Int -> ST s (SmallMutableArray s DrawOp)
growCanvasBuf ref needed = do
  arr <- readSTRef ref
  cap <- getSizeofSmallMutableArray arr
  let !newCap = max needed (cap * 2)
  grown <- newSmallArray newCap PopClip
  copySmallMutableArray grown 0 arr 0 cap
  writeSTRef ref grown
  pure grown

{-# INLINE pushRawOp #-}
pushRawOp :: CanvasBuf s -> DrawOp -> ST s ()
pushRawOp (CanvasBuf arrRef lenVar) !op = do
  n <- readPrimVar lenVar
  arr0 <- readSTRef arrRef
  cap <- getSizeofSmallMutableArray arr0
  arr <- if n < cap then pure arr0 else growCanvasBuf arrRef (n + 1)
  writeSmallArray arr n op
  writePrimVar lenVar (n + 1)

-- | Emit an op, applying the block's transform.
{-# INLINE emitOp #-}
emitOp :: DrawOp -> CanvasM ()
emitOp op = CanvasM $ \env buf ->
  case ceTransform env of
    Nothing -> pushRawOp buf op
    Just t -> pushList buf (transformOp (ceTolerance env) t op)

{-# INLINE pushList #-}
pushList :: CanvasBuf s -> [DrawOp] -> ST s ()
pushList buf = go
  where
    go [] = pure ()
    go (y : ys) = pushRawOp buf y >> go ys

-- | Emit ops built from the block's transform (identity if none) and curve
-- tolerance.
{-# INLINE emitWith #-}
emitWith :: (Transform -> Float -> [DrawOp]) -> CanvasM ()
emitWith build = CanvasM $ \env buf ->
  pushList buf (build (fromMaybe mempty (ceTransform env)) (ceTolerance env))

-- | The draw context of the widget being drawn: hover, press and focus
-- state, theme, and text font.
--
-- > canvas (fixedWH 40 40) $ \r -> do
-- >   cdc <- drawContext
-- >   drawRoundedRect r 6 (if cdcHovered cdc then themeAccent (cdcTheme cdc) else themeMuted (cdcTheme cdc))
{-# INLINE drawContext #-}
drawContext :: CanvasM CustomDrawContext
drawContext = CanvasM $ \env _ -> pure (ceContext env)

-- | Fill a solid rectangle.
{-# INLINE drawRect #-}
drawRect :: Rect -> Color -> CanvasM ()
drawRect r c = emitOp (FillRect r c)

-- | Fill a rounded rectangle with given corner radius.
{-# INLINE drawRoundedRect #-}
drawRoundedRect :: Rect -> Float -> Color -> CanvasM ()
drawRoundedRect r radius c = emitOp (FillRoundedRect r radius c)

-- | Fill a solid circle at center with given radius.
{-# INLINE drawCircle #-}
drawCircle :: V2 -> Float -> Color -> CanvasM ()
drawCircle (V2 cx cy) radius c = emitOp (FillCircle cx cy radius c)

-- | A straight line of the given thickness. It is not anti-aliased, so
-- slanted lines look jagged; 'drawStrokeAA' draws them smooth.
{-# INLINE drawStroke #-}
drawStroke :: V2 -> V2 -> Float -> Color -> CanvasM ()
drawStroke (V2 x0 y0) (V2 x1 y1) thickness c = emitOp (Stroke x0 y0 x1 y1 thickness c)

-- | Stroke a rounded rectangle's border with the given corner radius and
-- width. The border lies inside the rectangle, unlike 'drawStrokePath',
-- which centres the stroke on the path.
{-# INLINE drawStrokeRoundedRect #-}
drawStrokeRoundedRect :: Rect -> Float -> Float -> Color -> CanvasM ()
drawStrokeRoundedRect r radius thickness c = emitOp (StrokeRoundedRect r radius thickness c)

-- | Stroke a circle's outline with the given width. Like
-- 'drawStrokeRoundedRect', the outline lies inside the circle.
{-# INLINE drawStrokeCircle #-}
drawStrokeCircle :: V2 -> Float -> Float -> Color -> CanvasM ()
drawStrokeCircle (V2 cx cy) radius thickness c = emitOp (StrokeCircle cx cy radius thickness c)

-- | An anti-aliased straight line of the given thickness, with square ends.
{-# INLINE drawStrokeAA #-}
drawStrokeAA :: V2 -> V2 -> Float -> Color -> CanvasM ()
drawStrokeAA (V2 x0 y0) (V2 x1 y1) thickness c = emitOp (StrokeLineAA x0 y0 x1 y1 thickness c)

-- | Four-corner gradient fill (top-left, top-right, bottom-right,
-- bottom-left), blended over two triangles split from the top-left corner.
-- For a gradient along any line or over a path, use 'drawPathWith' with a
-- 'NanoUI.Path.Linear' paint.
{-# INLINE drawQuadGradient #-}
drawQuadGradient :: Rect -> Color -> Color -> Color -> Color -> CanvasM ()
drawQuadGradient r tl tr br bl = emitOp (FillQuadGradient r tl tr br bl)

-- | Horizontal 2-color linear gradient fill (left to right).
{-# INLINE drawLinearGradientH #-}
drawLinearGradientH :: Rect -> Color -> Color -> CanvasM ()
drawLinearGradientH r leftCol rightCol = emitOp (FillQuadGradient r leftCol rightCol rightCol leftCol)

-- | Vertical 2-color linear gradient fill (top to bottom).
{-# INLINE drawLinearGradientV #-}
drawLinearGradientV :: Rect -> Color -> Color -> CanvasM ()
drawLinearGradientV r topCol botCol = emitOp (FillQuadGradient r topCol topCol botCol botCol)

-- | Draw an image stretched over a rectangle, tinted. See 'drawImageWith'.
{-# INLINE drawImage #-}
drawImage :: Rect -> ImageId -> Color -> CanvasM ()
drawImage r iid c = drawImageWith (imageDraw r iid) {imageTint = c}

-- | Draw the region of an image from UV @(u0, v0)@ to @(u1, v1)@. See
-- 'drawImageWith'.
{-# INLINE drawImageUV #-}
drawImageUV :: Rect -> ImageId -> Float -> Float -> Float -> Float -> Color -> CanvasM ()
drawImageUV r iid u0 v0 u1 v1 c = drawImageWith (imageDraw r iid) {imageUV = Rect u0 v0 (u1 - u0) (v1 - v0), imageTint = c}

-- | Draw an image with a source region ('imageUV'), rotation about the
-- rect's centre ('imageAngle'), tint and opacity. Anything outside the
-- canvas is clipped, and a fully transparent image draws nothing:
--
-- > drawImageWith (imageDraw r photo) {imageAngle = pi / 8, imageOpacity = 0.6}
{-# INLINE drawImageWith #-}
drawImageWith :: ImageDraw -> CanvasM ()
drawImageWith = mapM_ emitOp . imageDrawOp

-- | Draw text in the context's font, anchored to a point by alignment:
-- 'AlignStart' puts the point at the text's left edge, 'AlignTop' at the
-- line's top, and 'AlignBaseline' on the baseline.
{-# INLINE drawText #-}
drawText :: V2 -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawText (V2 x y) alignX alignY txt col = emitOp (DrawText x y (alignXFraction alignX) (alignYFrac alignY) txt col)

-- | 'drawText' with an explicit font: size, variant (such as 'NanoUI.FontMono'),
-- weight, slant and decoration.
--
-- > drawTextWith defaultTextFont {textFontSize = 20, textFontWeight = WeightBold} (V2 cx cy) AlignCenter AlignMiddle "42" ink
--
-- Under a transform the point moves and the font scales by the square root
-- of the transform's area scale. Glyphs do not rotate.
{-# INLINE drawTextWith #-}
drawTextWith :: TextFont -> V2 -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawTextWith font (V2 x y) alignX alignY txt col = emitOp (DrawTextAligned x y (alignXFraction alignX) (alignYFrac alignY) 1 font txt col)

-- | Draw single-line text in the default font inside @r@, truncating with a
-- trailing ellipsis and clipping to @r@ when narrower than the text's width.
drawTextClipped :: Rect -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawTextClipped r alignX alignY txt col = do
  cdc <- drawContext
  drawTextClippedWith (cdcFont cdc) defaultTextFont EllipsisEnd r alignX alignY txt col

-- | Draw single-line text inside @r@ using explicit 'FontMetrics' (obtained
-- from 'NanoUI.resolveFontUi' or 'NanoUI.uiFontMetrics'), 'TextFont', and
-- 'TextEllipsis' mode. Truncates against @fm@ (applying tabular digit widths
-- when 'textFontTabular' is set) and clips to @r@.
drawTextClippedWith :: FontMetrics -> TextFont -> TextEllipsis -> Rect -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawTextClippedWith fm0 font ell r@(Rect rx ry rw rh) alignX alignY txt col =
  when (rw > 0 && rh > 0 && not (T.null txt)) $ do
    let fm = if textFontTabular font then tabularFontMetrics fm0 else fm0
        cut = truncateTextPure ell fm rw txt
        ax = rx + rw * alignXFraction alignX
        ay = case alignY of
          AlignTop -> ry
          AlignMiddle -> ry + rh * 0.5
          AlignBottom -> ry + rh
          AlignBaseline -> ry + fmAscent fm
    when (not (T.null cut)) $
      withClip r (drawTextWith font (V2 ax ay) alignX alignY cut col)

-- | Anchor position up the line: 0 is the bottom, 1 the top, and a negative
-- value the baseline.
alignYFrac :: AlignY -> Float
alignYFrac = \case AlignTop -> 1; AlignMiddle -> 0.5; AlignBottom -> 0; AlignBaseline -> -1

-- | The checkbox widget's box, drawn in a square at the rect's top-left
-- sized to the rect's shorter side. Checked, it is the theme accent with a
-- check mark; unchecked, an input well with the button border. The widget
-- itself draws it 'NanoUI.Widgets.Custom.checkboxBoxSize' wide.
drawCheckbox :: Theme -> Rect -> Bool -> CanvasM ()
drawCheckbox theme (Rect x y w h) = checkboxOps emitOp theme (styleBorder (themeButton theme)) x y (min w h)

-- | Fill a "NanoUI.Path" path with a colour, using the
-- 'NanoUI.Path.NonZero' fill rule.
drawPath :: Path -> Color -> CanvasM ()
drawPath path col = drawPathWith NonZero path (Solid col)

-- | Fill a path with a fill rule and paint. Each subpath is closed, and
-- regions the rule leaves unfilled are holes. All edges, including hole
-- edges, are anti-aliased.
--
-- > drawPathWith P.EvenOdd (P.circle c 20 <> P.circle c 12) (P.Solid ink)  -- a ring
drawPathWith :: FillRule -> Path -> Paint -> CanvasM ()
drawPathWith rule path paint = emitWith (\t tol -> fillPathOps tol t rule path paint)

-- | Stroke a "NanoUI.Path" path with a width and colour, centred on the
-- path. Shorthand for 'drawStrokePathWith' with 'NanoUI.Path.stroke'.
drawStrokePath :: Path -> Float -> Color -> CanvasM ()
drawStrokePath path w col = drawStrokePathWith (stroke w) path (Solid col)

-- | Stroke a path, centred on it, with a 'NanoUI.Path.Stroke' (width, caps,
-- joins, dashes). Each subpath or dash is one anti-aliased shape, so a
-- translucent stroke does not darken where it overlaps itself at a corner
-- or cap.
--
-- > drawStrokePathWith (P.stroke 3) {P.strokeCap = P.RoundCap, P.strokeJoin = P.RoundJoin} path (P.Solid ink)
drawStrokePathWith :: Stroke -> Path -> Paint -> CanvasM ()
drawStrokePathWith st path paint = emitWith (\t tol -> strokePathOps tol t st path paint)

-- | Draw a block through a "NanoUI.Path" transform. Nested transforms
-- compose, with the inner one applied first.
-- @withTransform (P.translate 40 40 <> P.rotate a)@ rotates the block by
-- @a@ about the origin, then moves it 40 right and 40 down.
--
-- Paths are transformed before flattening, so scaled-up curves stay
-- smooth. Stroke widths, dashes and gradients scale with the transform (by
-- the square root of its area scale when x and y scale differently). Other
-- ops:
--
-- * Rects, rounded rects, circles and their outlines keep their own ops
--   while the transform preserves their shape, and otherwise become paths.
--   A rect rotated by other than quarter turns becomes a polygon; a circle
--   scaled on one axis becomes an ellipse.
-- * Lines and triangles transform their points. A four-corner gradient
--   rotates with its rect.
-- * Images rotate, scale and flip, but stay rectangular under a skew.
-- * Text moves its anchor point and scales its font. Glyphs do not rotate.
-- * A clip ('withClip') becomes the bounding box of its transformed rect.
withTransform :: Transform -> CanvasM a -> CanvasM a
withTransform t (CanvasM m) =
  CanvasM $ \env -> m (env {ceTransform = Just (maybe t (<> t) (ceTransform env))})

-- | Draw a block clipped to a rect, intersected with the current clip
-- (which is at most the widget's rect).
--
-- > withClip (Rect x y w 20) (drawText (V2 x y) AlignStart AlignTop longTitle ink)
withClip :: Rect -> CanvasM a -> CanvasM a
withClip r body = emitOp (PushClip r) *> body <* emitOp PopClip
