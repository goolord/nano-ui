-- | Custom widgets and the reference widgets built on them.
--
-- 'customWidget' takes a 'CustomWidgetSpec': a layout, optional measurement,
-- drawing that sees hover and press state, an optional content key, a cursor,
-- and damage slop.
-- 'canvas' is the short form for drawing into a laid-out rectangle with
-- 'CanvasM', which fills and strokes paths from "NanoUI.Path" as well as
-- rects, circles, lines, images and text, and draws through transforms.
-- 'useDrag2DOn' and 'useWheelDeltaOn' are gesture hooks for your own
-- controls, fed the widget's 'Response'; 'knob' and 'toggleSwitch' show how
-- they fit together. Each reference
-- widget comes as @x@, at its default size, and as @xWith'@, which takes a
-- layout modifier and a size and also returns the widget's 'Response'.
module NanoUI.Widgets.Custom
  ( -- * Custom widgets
    CustomWidgetSpec (..)
  , defaultCustomWidgetSpec
  , customWidget
  , customWidgetWithId
  , contentKey
  , contentKeyOf
  , KeyPart
  , keyPart
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomDrawBuild
    -- * Canvas
  , CanvasM
  , runCanvas
  , runCanvasFor
  , canvas
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
  , drawCheckbox
  , checkboxBoxSize
    -- * Paths and transforms

    -- | Build a path with "NanoUI.Path", imported qualified, and fill or
    -- stroke it here. 'withTransform' moves, turns and scales what a block
    -- draws.
  , drawPath
  , drawStrokePath
  , drawStrokePathCapped
  , withTransform
    -- * Gestures
  , useDrag2DOn
  , Drag2D (..)
  , useWheelDeltaOn
  , useDrag2D
  , useWheelDelta
    -- * Reference widgets
  , knob
  , knobWith'
  , toggleSwitch
  , toggleSwitchWith'
  , circularProgress
  , circularProgressWith'
  , spinner
  , spinnerWith'
  , progressBar
  , progressBarWith'
  , sparkline
  , sparklineWith'
  ) where

import Control.Monad (void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader qualified as Reader
import Control.Monad.Trans.State.Strict qualified as State
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, smallArrayFromList)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawOp (..), checkboxOps)
import Data.Word (Word64)
import Data.Hashable (Hashable, hash)
import Data.Maybe (fromMaybe)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Font (FontMetrics (fmSnapScale), checkboxBoxSize)
import NanoUI.Internal.Id (WidgetId, mix64)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Internal.Monad (Ui, askContext, askInput, freshWidget, nextId, uiIO, uiTime)
import NanoUI.Internal.Path (LineCap (..), Path, Transform, curveTolerance, fillPathOps, strokePathOps, transformOp)
import NanoUI.Path qualified as P
import NanoUI.Internal.Store
import NanoUI.Internal.Style
import NanoUI.Internal.Types
import NanoUI.Internal.Widgets.Behavior (navStep, useKeyNav)
import NanoUI.Internal.Widgets.Combinators (finishInput, finishToggle)
import NanoUI.Internal.Widgets.Node
import NanoUI.Internal.Widgets.Animate (keepAnimating)
import NanoUI.Internal.Widgets.Custom (customDrawContext)

-- -----------------------------------------------------------------------------
-- Canvas Monad
-- -----------------------------------------------------------------------------

-- | Monadic canvas builder that collects 'DrawOp' vector operations efficiently.
newtype CanvasM a = CanvasM (Reader.ReaderT CanvasEnv (State.State ([DrawOp] -> [DrawOp])) a)
  deriving (Functor, Applicative, Monad)

-- | What a canvas block draws under: the transform 'withTransform' set, if
-- any, and how far a flattened curve may stray from the true one, in
-- logical pixels, which a block that draws no curve never works out.
data CanvasEnv = CanvasEnv
  { ceTransform :: !(Maybe Transform)
  , ceTolerance :: Float
  }

-- | Compile a 'CanvasM' block into an immutable 'SmallArray DrawOp'. It
-- does not know the display's scale, so it flattens curves finely enough
-- for two device pixels to the logical one; 'runCanvasFor' flattens them
-- for the display the widget is on.
{-# INLINE runCanvas #-}
runCanvas :: CanvasM a -> SmallArray DrawOp
runCanvas = runCanvasScaled 2

-- | 'runCanvas' for a custom widget's drawing, flattening curves to within
-- a quarter of a device pixel on the display its draw context is for:
--
-- > widgetDraw = \cdc rect -> runCanvasFor cdc (drawPath (P.circle (V2 20 20) 12) accent)
{-# INLINE runCanvasFor #-}
runCanvasFor :: CustomDrawContext -> CanvasM a -> SmallArray DrawOp
runCanvasFor cdc = runCanvasScaled (fmSnapScale (cdcFont cdc))

{-# INLINE runCanvasScaled #-}
runCanvasScaled :: Float -> CanvasM a -> SmallArray DrawOp
runCanvasScaled scale (CanvasM m) =
  smallArrayFromList (State.execState (Reader.runReaderT m (CanvasEnv Nothing (curveTolerance scale))) id [])

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

-- | Fill a solid rectangle.
drawRect :: Rect -> Color -> CanvasM ()
drawRect r c = emitOp (FillRect r c)

-- | Fill a rounded rectangle with given corner radius.
drawRoundedRect :: Rect -> Float -> Color -> CanvasM ()
drawRoundedRect r radius c = emitOp (FillRoundedRect r radius c)

-- | Fill a solid circle at center with given radius.
drawCircle :: V2 -> Float -> Color -> CanvasM ()
drawCircle (V2 cx cy) radius c = emitOp (FillCircle cx cy radius c)

-- | Stroke a straight segment between two points with thickness.
drawStroke :: V2 -> V2 -> Float -> Color -> CanvasM ()
drawStroke (V2 x0 y0) (V2 x1 y1) thickness c = emitOp (Stroke x0 y0 x1 y1 thickness c)

-- | Stroke a rounded rectangle border with given radius and stroke width.
drawStrokeRoundedRect :: Rect -> Float -> Float -> Color -> CanvasM ()
drawStrokeRoundedRect r radius thickness c = emitOp (StrokeRoundedRect r radius thickness c)

-- | Stroke a circular outline at center with given radius and stroke width.
drawStrokeCircle :: V2 -> Float -> Float -> Color -> CanvasM ()
drawStrokeCircle (V2 cx cy) radius thickness c = emitOp (StrokeCircle cx cy radius thickness c)

-- | Antialiased smooth stroke line between two points.
drawStrokeAA :: V2 -> V2 -> Float -> Color -> CanvasM ()
drawStrokeAA (V2 x0 y0) (V2 x1 y1) thickness c = emitOp (StrokeLineAA x0 y0 x1 y1 thickness c)

-- | Four-corner bilinear gradient fill (top-left, top-right, bottom-right, bottom-left).
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

-- | Draw text positioned at a reference point with horizontal and vertical alignment.
drawText :: V2 -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawText (V2 x y) alignX alignY txt col =
  let ax = case alignX of AlignStart -> 0; AlignCenter -> 0.5; AlignEnd -> 1
      ay = case alignY of AlignTop -> 1; AlignMiddle -> 0.5; AlignBottom -> 0; AlignBaseline -> -1
   in emitOp (DrawText x y ax ay txt col)

-- | A checkbox's box as the checkbox widget draws it, in the square at the
-- rect's top-left corner as wide as the rect's shorter side: the theme's
-- accent with a check mark when checked, otherwise an input well with the
-- theme's button border. The widget draws it 'checkboxBoxSize' wide.
drawCheckbox :: Theme -> Rect -> Bool -> CanvasM ()
drawCheckbox theme (Rect x y w h) = checkboxOps emitOp theme (styleBorder (themeButton theme)) x y (min w h)

-- | Fill a path built with "NanoUI.Path". Each subpath fills on its own,
-- as if closed, as one anti-aliased 'FillPolygon': one inside another is
-- drawn over it rather than cut out of it, and one that crosses itself may
-- fill only in part.
drawPath :: Path -> Color -> CanvasM ()
drawPath path col = emitWith (\t tol -> fillPathOps tol t path col)

-- | Stroke a path built with "NanoUI.Path", this wide, as one anti-aliased
-- 'StrokePolyline' a subpath. Its corners are mitered, a very sharp one cut
-- short, and an open subpath's ends are cut square at its end points.
drawStrokePath :: Path -> Float -> Color -> CanvasM ()
drawStrokePath = drawStrokePathCapped ButtCap

-- | 'drawStrokePath' with the given ends on open subpaths. A round cap is a
-- disc drawn over the end, so a translucent line shows darker where the two
-- overlap.
drawStrokePathCapped :: LineCap -> Path -> Float -> Color -> CanvasM ()
drawStrokePathCapped cap path w col = emitWith (\t tol -> strokePathOps tol t cap path w col)

-- | Draw a block through a transform from "NanoUI.Path", inside any it is
-- already in, which applies after it: @withTransform (P.translate 40 40 <>
-- P.rotate a)@ turns what the block draws by @a@ about the origin, then
-- moves it 40 right and 40 down.
--
-- Paths are transformed before they are flattened, so a curve scaled up
-- stays smooth, and a stroke's width scales with the transform (by the
-- square root of its area scale, for one that scales x and y apart). The
-- other ops follow as far as their shapes allow. Rects, rounded rects,
-- circles and their outlines keep their own ops while they keep their
-- shape, and otherwise become paths: a rect turned other than by quarter
-- turns is a polygon, and a circle scaled on one axis an ellipse. Lines and
-- triangles move their points. A gradient fills the bounding box of its
-- transformed rect, its corners taking the colours of the corners that land
-- nearest them, and does not turn. An image turns and scales with the
-- transform, and turns over with a flip: under a rotation or a skew it is
-- drawn as a 'DrawImageRotated', and a skew leaves it a rect. Text moves its
-- anchor; its glyphs are neither scaled nor turned.
withTransform :: Transform -> CanvasM a -> CanvasM a
withTransform t (CanvasM m) =
  CanvasM (Reader.local (\env -> env {ceTransform = Just (maybe t (<> t) (ceTransform env))}) m)

-- -----------------------------------------------------------------------------
-- Custom Widget Specification
-- -----------------------------------------------------------------------------

-- | Complete specification for defining a custom widget.
data CustomWidgetSpec a = CustomWidgetSpec
  { widgetLayout     :: !Layout
    -- ^ Flex layout constraints (width/height sizing, min/max, alignment, padding).
  , widgetMeasure    :: !(Maybe CustomMeasureFn)
    -- ^ Optional intrinsic measurement hook for 'Fit' or dynamic sizing.
  , widgetDraw       :: !CustomDrawBuild
    -- ^ Vector drawing procedure receiving interaction context and layout rect.
  , widgetContent    :: !Int
    -- ^ Content key: a number that changes whenever 'widgetDraw' would draw
    -- something different ('contentKey' hashes numbers into one). A frame
    -- whose key, size, interaction state and metrics are unchanged neither
    -- rebuilds the ops nor repaints. The default 0 means no key: the ops are
    -- rebuilt and compared every frame. A stale key draws stale pixels, so
    -- derive it from everything the drawing reads: it is believed even while
    -- the widget animates.
  , widgetCursor     :: !(Maybe (CustomDrawContext -> UiCursorKind))
    -- ^ Optional custom mouse cursor when pointer is over the widget.
  , widgetFocusable  :: !Bool
    -- ^ Whether this widget accepts tab/keyboard focus.
  , widgetDamageSlop :: !Float
    -- ^ Padding added to dirty rectangles (for shadows, glow, or drag handles).
  , widgetTrackPointer :: !Bool
    -- ^ Run a frame for every pointer move over the widget, for one that
    -- draws what is under the pointer inside itself (default 'False': only
    -- moves onto another widget run a frame).
  , widgetInteract   :: !(Response -> CustomDrawContext -> Input -> (Response, a))
    -- ^ Interaction hook: from the resolved 'Response', the draw context and
    -- the input, the final response and value.
  }

-- | Default configuration for a custom widget with standard hover/press/click behavior.
defaultCustomWidgetSpec :: CustomWidgetSpec ()
defaultCustomWidgetSpec = CustomWidgetSpec
  { widgetLayout     = defaultLayout
  , widgetMeasure    = Nothing
  , widgetDraw       = \_ _ -> emptySmallArray
  , widgetContent    = 0
  , widgetCursor     = Nothing
  , widgetFocusable  = False
  , widgetDamageSlop = defaultDamageSlop
  , widgetTrackPointer = False
  , widgetInteract   = \resp _ _ -> (resp, ())
  }

-- | The default spec at a fixed @w@ by @h@, the caller's layout modifier
-- applied, as the reference widgets below are built.
fixedSizeSpec :: (Layout -> Layout) -> Float -> Float -> CustomWidgetSpec ()
fixedSizeSpec f w h =
  defaultCustomWidgetSpec {widgetLayout = fixedWH w h (f defaultLayout), widgetMeasure = Just $ \_ _ -> (w, h)}

-- | A 'widgetContent' key for a drawing whose output follows these numbers.
-- Pass every value the drawing reads; @0@ means "no key", so a hash that lands
-- there becomes 1.
{-# INLINE contentKey #-}
contentKey :: [Float] -> Int
contentKey vs = contentKeyOf [KeyPart (fromIntegral (castFloatToWord32 v)) | v <- vs]

-- | A 'widgetContent' key over values of any 'Hashable' types, mixed in
-- order; an 'Int' or a 'Double' is hashed whole where 'contentKey' would
-- round it to a 'Float'.
--
-- > widgetContent = contentKeyOf [keyPart version, keyPart scrollY, keyPart query, keyPart selected]
{-# INLINE contentKeyOf #-}
contentKeyOf :: [KeyPart] -> Int
contentKeyOf parts =
  let raw = foldl' (\acc (KeyPart v) -> mix64 acc v) 0x9E3779B97F4A7C15 parts
      k = fromIntegral raw
   in if k == 0 then 1 else k

-- | One value of a 'contentKeyOf' key, made by 'keyPart'.
newtype KeyPart = KeyPart Word64

-- | A value's part of a content key: its 'hash', which a list, a 'Maybe' or
-- a tuple of values has as well, and a type of the app's own has by deriving
-- 'Hashable'.
{-# INLINE keyPart #-}
keyPart :: Hashable a => a -> KeyPart
keyPart = KeyPart . fromIntegral . hash

-- | Instantiates a custom widget using an existing 'WidgetId'.
customWidgetWithId :: (Ui :> es) => WidgetId -> CustomWidgetSpec a -> Eff es (Response, a)
customWidgetWithId wid spec = do
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    when (widgetFocusable spec) $ registerFocusable ctx wid
    mapM_ (registerCustomMeasure ctx wid) (widgetMeasure spec)
    registerCustomEntry ctx wid $
      CustomDrawingEntry
        (widgetContent spec)
        (widgetDraw spec)
        (widgetCursor spec)
        (widgetDamageSlop spec)
        (widgetTrackPointer spec)
  resp0 <- addWidget wid NodeDrawing T.empty 0 (widgetLayout spec)
  cdc <- uiIO (customDrawContext ctx (ctxFontMetrics ctx) wid (respHovered resp0) (respPressed resp0))
  pure (widgetInteract spec resp0 cdc inp)

-- | Instantiates a custom widget from a 'CustomWidgetSpec'.
customWidget :: (Ui :> es) => CustomWidgetSpec a -> Eff es (Response, a)
customWidget spec = do
  wid <- nextId
  customWidgetWithId wid spec

-- | Draw into a rectangle sized by the layout modifier, curves flattened
-- for the display it is on ('runCanvasFor'). Use 'customWidget' when the
-- drawing needs hover or press state.
canvas :: (Ui :> es) => (Layout -> Layout) -> (Rect -> CanvasM ()) -> Eff es Response
canvas f drawAction =
  fst <$> customWidget defaultCustomWidgetSpec
    { widgetLayout = f defaultLayout
    , widgetDraw   = \cdc rect -> runCanvasFor cdc (drawAction rect)
    }

-- -----------------------------------------------------------------------------
-- Common Gesture & Behavior Helpers
-- -----------------------------------------------------------------------------

-- | Result of a 2D drag gesture.
data Drag2D = Drag2D
  { dragPosition :: !V2
    -- ^ Current dragged pointer position clamped within bounds.
  , dragActive   :: !Bool
    -- ^ True while pointer is pressed and dragging is active.
  , dragDelta    :: !V2
    -- ^ Movement delta since previous frame.
  }
  deriving (Eq, Show)

-- | A drag of the left button that starts with a press on the widget, and
-- lasts until the button comes up wherever the pointer goes, for a colour
-- picker, a joystick or a panned canvas. The position is clamped to the
-- widget's rect. The press is the widget's own ('respPressed'), so a press
-- on something drawn over it, or on the part a scroller has clipped off, or
-- on it disabled, starts nothing. Call it every frame, after the widget:
--
-- > (resp, ()) <- customWidget spec
-- > drag <- useDrag2DOn resp
-- > when (dragActive drag) (setPan (dragPosition drag))
useDrag2DOn :: (Ui :> es, HasResponse r) => r -> Eff es Drag2D
useDrag2DOn r = do
  inp <- askInput
  drag2DFrom (respRect r) (respPressed r && buttonPressed MouseLeft inp)

-- | 'useDrag2DOn' over a rect: the drag starts with a press anywhere in it,
-- whatever is drawn there.
useDrag2D :: (Ui :> es) => Rect -> Eff es Drag2D
useDrag2D bounds = do
  inp <- askInput
  drag2DFrom bounds (buttonPressed MouseLeft inp && rectContains bounds (inputMousePos inp))
{-# DEPRECATED useDrag2D "Use useDrag2DOn with the widget's Response, which respects what is drawn over it" #-}

-- | A drag of the left button that @starts@ this frame or started before,
-- clamped to @bounds@.
drag2DFrom :: (Ui :> es) => Rect -> Bool -> Eff es Drag2D
drag2DFrom bounds starts = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  -- The drag flag is quiet bookkeeping; the last pointer position is a point
  -- slot.
  let dragK = slotKey SlotDrag (intKey wid)
      mouse = inputMousePos inp
  store <- uiIO (getStore ctx)
  let active0 = quietFlag dragK store
      active = buttonHeld MouseLeft inp && (active0 || starts)
      prev = uncurry V2 (findSlot fieldPoint (v2X mouse, v2Y mouse) dragK store)
      delta = if active && active0 then v2Sub mouse prev else V2 0 0
      clampedMouse =
        V2
          (clamp (rectX bounds) (rectX bounds + rectW bounds) (v2X mouse))
          (clamp (rectY bounds) (rectY bounds + rectH bounds) (v2Y mouse))
  when (active || active0) $
    uiIO . modifyStore ctx $
      setQuietFlag dragK active
        . (if active then insertSlot fieldPoint dragK (v2X mouse, v2Y mouse) else deleteSlot fieldPoint dragK)
  pure Drag2D { dragPosition = clampedMouse, dragActive = active, dragDelta = delta }

-- | This frame's wheel turn while the pointer is on the widget
-- ('respHovered'): not where something is drawn over it, nor where a
-- scroller has clipped it off, nor while it is disabled.
useWheelDeltaOn :: (Ui :> es, HasResponse r) => r -> Eff es (Float, Float)
useWheelDeltaOn r = wheelIf (respHovered r)

-- | 'useWheelDeltaOn' over a rect: the wheel wherever the pointer is in it.
useWheelDelta :: (Ui :> es) => Rect -> Eff es (Float, Float)
useWheelDelta bounds = wheelIf . rectContains bounds . inputMousePos =<< askInput
{-# DEPRECATED useWheelDelta "Use useWheelDeltaOn with the widget's Response, which respects what is drawn over it" #-}

-- | This frame's wheel turn when @on@, else none.
wheelIf :: (Ui :> es) => Bool -> Eff es (Float, Float)
wheelIf on = do
  V2 x y <- inputScroll <$> askInput
  pure (if on then (x, y) else (0, 0))

-- -----------------------------------------------------------------------------
-- Reference Custom Widgets
-- -----------------------------------------------------------------------------

-- | Rotary knob over @[minV, maxV]@, 36 px across. Drag vertically, scroll,
-- or use the arrow keys. Pass the current value; the result is the value
-- after this frame.
{-# INLINE knob #-}
knob :: Ui :> es => Float -> Float -> Float -> Eff es Float
knob minV maxV value = snd <$> knobWith' id 36 minV maxV value

-- | 'knob' with a layout modifier and a diameter in pixels, returning the
-- response and the updated value.
knobWith' ::
  Ui :> es =>
  (Layout -> Layout)
  -> Float
  -> Float
  -> Float
  -> Float
  -> Eff es (Response, Float)
knobWith' f diameter minV maxV value = do
  (wid, ctx) <- freshWidget
  -- NaN would be adopted afresh, dirtying the frame, every frame.
  current <- uiIO $ adoptSlot fieldFloat ctx wid (if isNaN value then minV else value)
  let
    range = maxV - minV
    frac = if range > 0 then clamp01 ((current - minV) / range) else 0
  (resp, ()) <-
    customWidgetWithId
      wid
      (fixedSizeSpec f diameter diameter)
        { widgetCursor = Just (\_ -> UiCursorNsResize)
        , widgetFocusable = True
        , widgetContent = contentKey [frac]
        , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $ do
            let
              cx = x + w / 2
              cy = y + h / 2
              r = min (w / 2) (h / 2) - 2
              button = themeButton (cdcTheme cdc)
              bgCol
                | cdcPressed cdc = styleActiveBg button
                | cdcHovered cdc = styleHoverBg button
                | otherwise = styleBg button
              angle = (135 + frac * 270) * (pi / 180)
              tip = V2 (cx + cos angle * (r * 0.75)) (cy + sin angle * (r * 0.75))
            drawCircle (V2 cx cy) r bgCol
            drawStrokeCircle (V2 cx cy) r 1.5 (styleBorder button)
            drawStrokeAA (V2 cx cy) tip 2.5 (themeAccent (cdcTheme cdc))
        }
  -- A drag reports no movement while it is not held.
  drag <- useDrag2DOn resp
  (_, scrollY) <- useWheelDeltaOn resp
  nav <- useKeyNav wid
  let
    deltaNorm = -v2Y (dragDelta drag) / 120 + scrollY * 2 / 60 + fromIntegral (navStep nav) * 0.05
    finalVal
      | range > 0 && deltaNorm /= 0 = clamp minV maxV (current + deltaNorm * range)
      | otherwise = current
  finishInput fieldFloat ctx wid current resp finalVal

-- | On/off switch. Pass the current state; the result is the state after
-- this frame's click or Space/Enter.
{-# INLINE toggleSwitch #-}
toggleSwitch :: Ui :> es => Bool -> Eff es Bool
toggleSwitch on = snd <$> toggleSwitchWith' id on

-- | 'toggleSwitch' with a layout modifier, returning the response and the
-- updated flag.
toggleSwitchWith' ::
  Ui :> es => (Layout -> Layout) -> Bool -> Eff es (Response, Bool)
toggleSwitchWith' f on = do
  (wid, ctx) <- freshWidget
  current <- intBool <$> uiIO (adoptSlot fieldInt ctx wid (boolInt on))
  let
    pillW = 44.0
    pillH = 24.0
  (resp, ()) <-
    customWidgetWithId
      wid
      (fixedSizeSpec f pillW pillH)
        { widgetCursor = Just (\_ -> UiCursorPointer)
        , widgetFocusable = True
        , widgetContent = contentKey [if current then 1 else 0]
        , widgetDraw = \cdc rect@(Rect x y w h) -> runCanvasFor cdc $ do
            let
              theme = cdcTheme cdc
              r = h / 2
              thumbX = if current then x + w - r else x + r
            drawRoundedRect rect r (if current then themeAccent theme else styleBg (themeButton theme))
            drawStrokeRoundedRect rect r 1 (styleBorder (themeButton theme))
            drawCircle (V2 thumbX (y + r)) (r - 3) (themeOnAccent theme)
        }
  finishToggle ctx wid current resp

-- | Progress ring for a fraction in @[0, 1]@, 32 px across.
{-# INLINE circularProgress #-}
circularProgress :: Ui :> es => Float -> Eff es ()
circularProgress frac = void (circularProgressWith' id 32 frac)

-- | 'circularProgress' with a layout modifier and a diameter in pixels,
-- returning its response.
circularProgressWith' :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Eff es Response
circularProgressWith' f diameter frac =
  fst <$> customWidget (fixedSizeSpec f diameter diameter)
    { widgetContent = contentKey [clamp01 frac]
    , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $ do
        let centre = V2 (x + w / 2) (y + h / 2)
            r = min (w / 2) (h / 2) - 2
            theme = cdcTheme cdc
            clampedFrac = clamp01 frac
        drawStrokeCircle centre r 2.0 (styleBorder (themeButton theme))
        when (clampedFrac > 0) $
          drawCircle centre (r * clampedFrac) (themeAccent theme)
    }

-- | An indeterminate loading indicator, 18 px across: an accent arc turning
-- over a faint ring. It keeps frames coming while on screen.
{-# INLINE spinner #-}
spinner :: Ui :> es => Eff es ()
spinner = void (spinnerWith' id 18)

-- | 'spinner' with a layout modifier and a diameter in pixels, returning its
-- response. It requests animation frames while declared.
spinnerWith' :: Ui :> es => (Layout -> Layout) -> Float -> Eff es Response
spinnerWith' f diameter = do
  t <- uiTime
  let !d = max 4 diameter
      -- One turn every 0.8 s, in 48 steps: the step is the content key, so
      -- frames within a step reuse the ops.
      !step = floor (t * 48 / 0.8) `mod` 48 :: Int
  resp <-
    fst <$> customWidget (fixedSizeSpec f d d)
      { widgetContent = step + 1
      , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $ do
          let theme = cdcTheme cdc
              thick = max 1.5 (d / 9)
              r = min w h / 2 - thick / 2
              cx = x + w / 2
              cy = y + h / 2
              start = 2 * pi * fromIntegral step / 48
          drawStrokeCircle (V2 cx cy) r thick (fadeAlpha (themeAccent theme) 48)
          drawStrokePath (P.arc (V2 cx cy) r start (pi / 2)) thick (themeAccent theme)
      }
  keepAnimating resp
  pure resp

-- | Horizontal progress bar for a fraction in @[0, 1]@. It fills the
-- available width at a fixed height.
{-# INLINE progressBar #-}
progressBar :: Ui :> es => Float -> Eff es ()
progressBar frac = void (progressBarWith' id progressBarDefaultHeight frac)

-- | 'progressBar' with a layout modifier and a height in pixels, returning
-- its response.
progressBarWith' :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Eff es Response
progressBarWith' f height frac =
  let !barH = max 0 height
   in fst <$> customWidget defaultCustomWidgetSpec
        { widgetLayout = fillW (fixedH barH (f defaultLayout))
        , widgetMeasure = Just $ \_ _ -> (progressBarDefaultWidth, barH)
        , widgetContent = contentKey [clamp01 frac]
        , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $ do
            let theme = cdcTheme cdc
                barW = max 0 w
                barH' = max 0 h
                rad = barH' / 2
                fillWpx = barW * clamp01 frac
            drawRoundedRect (Rect x y barW barH') rad (styleBg (themeButton theme))
            when (fillWpx > 0) $
              drawRoundedRect (Rect x y fillWpx barH') (min rad (fillWpx / 2)) (themeAccent theme)
            drawStrokeRoundedRect (Rect x y barW barH') rad 1 (styleBorder (themeButton theme))
        }

-- | Default height and minimum content width for 'progressBar'.
progressBarDefaultHeight, progressBarDefaultWidth :: Float
progressBarDefaultHeight = 12.0
progressBarDefaultWidth = 120.0

-- | A small line chart of the values, 80 by 24 px, scaled to their range.
{-# INLINE sparkline #-}
sparkline :: Ui :> es => [Float] -> Eff es ()
sparkline values = void (sparklineWith' id 80 24 values)

-- | 'sparkline' with a layout modifier and a preferred size, returning its
-- response.
sparklineWith' :: Ui :> es => (Layout -> Layout) -> Float -> Float -> [Float] -> Eff es Response
sparklineWith' f prefW prefH values =
  fst <$> customWidget (fixedSizeSpec f prefW prefH)
    { widgetContent = contentKey values
    , widgetDraw = \cdc rect@(Rect x y rw rh) -> runCanvasFor cdc $ do
        let accent = themeAccent (cdcTheme cdc)
        drawRoundedRect rect 3.0 (styleBg (themePanel (cdcTheme cdc)))
        case values of
          [] -> pure ()
          [_] -> drawCircle (V2 (x + rw / 2) (y + rh / 2)) 2.0 accent
          vs -> do
            let minV = minimum vs
                maxV = maximum vs
                range = if maxV > minV then maxV - minV else 1.0
                pad = 4.0
                plotW = max 1.0 (rw - 2 * pad)
                plotH = max 1.0 (rh - 2 * pad)
                n = length vs
                stepX = plotW / fromIntegral (max 1 (n - 1))
                pts = [ V2 (x + pad + fromIntegral i * stepX)
                           (y + rh - pad - ((v - minV) / range) * plotH)
                      | (i, v) <- zip [0 :: Int ..] vs
                      ]
            drawStrokePath (P.polyline pts) 1.5 accent
            drawCircle (last pts) 2.5 accent
    }
