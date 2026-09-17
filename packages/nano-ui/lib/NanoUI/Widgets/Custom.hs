{-# LANGUAGE OverloadedStrings #-}

-- | Custom widgets and the reference widgets built on them.
--
-- 'customWidget' takes a 'CustomWidgetSpec': a layout, optional measurement,
-- drawing that sees hover and press state, an optional content key, a cursor,
-- and damage slop.
-- 'canvas' is the short form for drawing into a laid-out rectangle with
-- 'CanvasM'. 'useDrag2D' and 'useWheelDelta' are gesture hooks for your own
-- controls; 'knob' and 'toggleSwitch' show how they fit together.
module NanoUI.Widgets.Custom
  ( -- * Custom widgets
    CustomWidgetSpec (..)
  , defaultCustomWidgetSpec
  , customWidget
  , customWidgetWithId
  , contentKey
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomDrawBuild
  , mkCustomDrawContext
    -- * Canvas
  , CanvasM
  , runCanvas
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
  , drawText
    -- * Gestures
  , useDrag2D
  , Drag2D (..)
  , useWheelDelta
    -- * Reference widgets
  , knob
  , knob'
  , knobWith
  , knobWith'
  , toggleSwitch
  , toggleSwitch'
  , toggleSwitchWith
  , toggleSwitchWith'
  , circularProgress
  , circularProgress'
  , circularProgressWith
  , circularProgressWith'
  , spinner
  , spinner'
  , spinnerWith
  , spinnerWith'
  , progressBar
  , progressBar'
  , progressBarWith
  , progressBarWith'
  , sparkline
  , sparkline'
  , sparklineWith
  , sparklineWith'
  ) where

import Control.Monad (forM_, void, when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.SmallArray (SmallArray, emptySmallArray, smallArrayFromList)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomMeasureFn
  , adoptStoreFloat
  , adoptStoreInt
  , getFocusId
  , getHotId
  , getStore
  , intKey
  , isDisabled
  , recordStoreFloat
  , recordStoreInt
  , registerCustomCursor
  , registerCustomDamageSlop
  , registerCustomDrawing
  , registerCustomMeasure
  , registerFocusable
  , writeStoreBool
  , writeStoreFloat
  , widgetTheme
  , modifyStore
  )
import NanoUI.Draw (DrawOp (..))
import NanoUI.Font (FontMetrics)
import GHC.Float (castFloatToWord32)
import NanoUI.Id (WidgetId, mix64)
import NanoUI.Input
  ( Input (..)
  , UiCursorKind (..)
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputScroll
  )
import NanoUI.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, uiTime)
import NanoUI.Store (WidgetStore (..), boolInt, intBool, Slot (..), slotKey)
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Layout
  , defaultLayout
  , fillW
  , fixedH
  , fixedWH
  , styleActiveBg
  , styleBg
  , styleBorder
  , styleHoverBg
  , themeAccent
  , themeButton
  , themePanel
  , themeOnAccent
  , fadeAlpha
  )
import NanoUI.Types
  ( Color
  , ImageId (..)
  , Rect (..)
  , V2 (..)
  , clamp
  , clamp01
  , defaultDamageSlop
  , rectContains
  , v2X
  , v2Y
  )
import NanoUI.Widgets.Behavior (KeyNav (..), keyActivated, useKeyNav)
import NanoUI.Widgets.Node
  ( Response
  , addWidget
  , respClicked
  , respHovered
  , respPressed
  , respRect
  , setChanged
  )
import NanoUI.Widgets.Animate (keepAnimating)

-- -----------------------------------------------------------------------------
-- Canvas Monad
-- -----------------------------------------------------------------------------

-- | Monadic canvas builder that collects 'DrawOp' vector operations efficiently.
newtype CanvasM a = CanvasM { runCanvasM :: ([DrawOp] -> [DrawOp]) -> (a, [DrawOp] -> [DrawOp]) }

instance Functor CanvasM where
  fmap f (CanvasM m) = CanvasM $ \s ->
    case m s of (a, s') -> (f a, s')

instance Applicative CanvasM where
  pure a = CanvasM $ \s -> (a, s)
  CanvasM mf <*> CanvasM mx = CanvasM $ \s ->
    case mf s of
      (f, s1) -> case mx s1 of
        (x, s2) -> (f x, s2)

instance Monad CanvasM where
  CanvasM m >>= f = CanvasM $ \s ->
    case m s of (a, s') -> runCanvasM (f a) s'

-- | Compile a 'CanvasM' block into an immutable 'SmallArray DrawOp'.
runCanvas :: CanvasM a -> SmallArray DrawOp
runCanvas (CanvasM m) =
  let (_, diff) = m id
   in smallArrayFromList (diff [])

emitOp :: DrawOp -> CanvasM ()
emitOp op = CanvasM $ \diff -> ((), diff . (op :))

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

-- | Draw text positioned at a reference point with horizontal and vertical alignment.
drawText :: V2 -> AlignX -> AlignY -> Text -> Color -> CanvasM ()
drawText (V2 x y) alignX alignY txt col =
  let ax = case alignX of AlignStart -> 0; AlignCenter -> 0.5; AlignEnd -> 1
      ay = case alignY of AlignTop -> 1; AlignMiddle -> 0.5; AlignBottom -> 0
   in emitOp (DrawText x y ax ay txt col)

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
    -- something different from the state it reads (a value, a flag, a model
    -- revision; 'contentKey' hashes numbers into one). A frame whose key,
    -- size, interaction state and metrics are unchanged neither rebuilds the
    -- ops nor repaints the widget (a widget that only moved has its ops
    -- translated), so key a drawing whose ops are expensive to build. The default 0 means no key: the ops are rebuilt every frame and
    -- compared, which repaints correctly whatever the drawing reads but pays
    -- for the rebuild. A stale key draws stale pixels, so derive it from
    -- everything the drawing reads, an animated value included: a key is
    -- believed while the widget animates, as a versioned drawing's version is.
  , widgetCursor     :: !(Maybe (CustomDrawContext -> UiCursorKind))
    -- ^ Optional custom mouse cursor when pointer is over the widget.
  , widgetFocusable  :: !Bool
    -- ^ Whether this widget accepts tab/keyboard focus.
  , widgetDamageSlop :: !Float
    -- ^ Padding added to dirty rectangles (for shadows, glow, or drag handles).
  , widgetInteract   :: !(Response -> CustomDrawContext -> Input -> (Response, a))
    -- ^ Interaction hook. It receives the widget's resolved 'Response' (hover,
    -- press, right-click, and clicks including one queued from a previous
    -- frame), the draw context and the input, and returns the final response
    -- and value.
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
  , widgetInteract   = \resp _ _ -> (resp, ())
  }

-- | A 'widgetContent' key for a drawing whose output follows these numbers.
-- Pass every value the drawing reads; @0@ means "no key", so a hash that lands
-- there becomes 1.
{-# INLINE contentKey #-}
contentKey :: [Float] -> Int
contentKey vs =
  let raw = foldl' (\acc v -> mix64 acc (fromIntegral (castFloatToWord32 v))) 0x9E3779B97F4A7C15 vs
      k = fromIntegral raw
   in if k == 0 then 1 else k

-- | Build the draw context a custom widget sees, resolving hover/press/focus
-- state for @wid@ from the ambient context. One policy for state masking.
mkCustomDrawContext :: Context -> FontMetrics -> WidgetId -> IO CustomDrawContext
mkCustomDrawContext ctx fm wid = do
  hot <- getHotId ctx
  active <- readIORef (ctxActiveId ctx)
  customDrawContext ctx fm wid (hot == wid) (active == wid)

-- | Draw context for @wid@ with the given hover and press state; a disabled
-- widget is never hovered or pressed.
customDrawContext :: Context -> FontMetrics -> WidgetId -> Bool -> Bool -> IO CustomDrawContext
customDrawContext ctx fm wid hovered pressed = do
  disabled <- isDisabled ctx wid
  focused <- (== wid) <$> getFocusId ctx
  active <- readIORef (ctxActiveId ctx)
  theme <- widgetTheme ctx wid
  pure
    CustomDrawContext
      { cdcHovered = hovered && not disabled
      , cdcPressed = pressed && not disabled
      , cdcFocused = focused
      , cdcActive = active == wid
      , cdcDisabled = disabled
      , cdcTheme = theme
      , cdcFont = fm
      }

-- | Instantiates a custom widget using an existing 'WidgetId'.
customWidgetWithId :: (Ui :> es) => WidgetId -> CustomWidgetSpec a -> Eff es (Response, a)
customWidgetWithId wid spec = do
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    when (widgetFocusable spec) $ registerFocusable ctx wid
    mapM_ (registerCustomMeasure ctx wid) (widgetMeasure spec)
    registerCustomDrawing ctx wid (widgetContent spec) (widgetDraw spec)
    mapM_ (registerCustomCursor ctx wid) (widgetCursor spec)
    when (widgetDamageSlop spec > 0) $
      registerCustomDamageSlop ctx wid (widgetDamageSlop spec)
  resp0 <- addWidget wid NodeDrawing T.empty 0 (widgetLayout spec)
  cdc <- uiIO (customDrawContext ctx (ctxFontMetrics ctx) wid (respHovered resp0) (respPressed resp0))
  pure (widgetInteract spec resp0 cdc inp)

-- | Instantiates a custom widget from a 'CustomWidgetSpec'.
--
-- Connects the widget into:
-- - The two-pass layout arena (respecting 'widgetMeasure' or layout constraints).
-- - Off-heap vector drawing pipeline. Without a 'widgetContent' key the draw
--   function runs once a frame and the widget repaints when its ops change, so
--   it may read anything; with one, an unchanged key skips both.
-- - Interactive hit-testing, focus management, and custom cursor resolution.
-- - Accurate damage region tracking with 'widgetDamageSlop'.
customWidget :: (Ui :> es) => CustomWidgetSpec a -> Eff es (Response, a)
customWidget spec = do
  wid <- nextId
  customWidgetWithId wid spec

-- | Draw into a rectangle sized by the layout modifier. Use 'customWidget'
-- when the drawing needs hover or press state.
canvas :: (Ui :> es) => (Layout -> Layout) -> (Rect -> CanvasM ()) -> Eff es Response
canvas f drawAction =
  fst <$> customWidget defaultCustomWidgetSpec
    { widgetLayout = f defaultLayout
    , widgetDraw   = \_ rect -> runCanvas (drawAction rect)
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

-- | Tracks pointer dragging across a 2D area (e.g. for color pickers, joysticks, canvas panning).
useDrag2D ::
  (Ui :> es) =>
  Rect ->
  Eff es Drag2D
useDrag2D bounds = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  -- The drag flag lives in 'storeInt' and the last pointer position in
  -- 'storePoint', both under the widget's drag slot.
  let dragK = slotKey SlotDrag (intKey wid)
      mouse = inputMousePos inp
  store <- uiIO (getStore ctx)
  let active0 = IM.findWithDefault 0 dragK (storeInt store) /= 0
      active = inputMouseDown inp && (active0 || (inputMousePressed inp && rectContains bounds mouse))
      (prevX, prevY) = IM.findWithDefault (v2X mouse, v2Y mouse) dragK (storePoint store)
      delta =
        if active && active0
          then V2 (v2X mouse - prevX) (v2Y mouse - prevY)
          else V2 0 0
      clampedMouse =
        V2
          (clamp (rectX bounds) (rectX bounds + rectW bounds) (v2X mouse))
          (clamp (rectY bounds) (rectY bounds + rectH bounds) (v2Y mouse))
  when (active || active0) $
    uiIO $
      modifyStore ctx $ \st ->
        if active
          then
            st
              { storeInt = IM.insert dragK 1 (storeInt st)
              , storePoint = IM.insert dragK (v2X mouse, v2Y mouse) (storePoint st)
              }
          else
            st
              { storeInt = IM.delete dragK (storeInt st)
              , storePoint = IM.delete dragK (storePoint st)
              }
  pure Drag2D { dragPosition = clampedMouse, dragActive = active, dragDelta = delta }

-- | Inspects mouse wheel scroll delta when pointer is hovering over bounds.
useWheelDelta :: (Ui :> es) => Rect -> Eff es (Float, Float)
useWheelDelta bounds = do
  inp <- askInput
  let mouse = inputMousePos inp
  if rectContains bounds mouse
    then pure (v2X (inputScroll inp), v2Y (inputScroll inp))
    else pure (0, 0)

-- -----------------------------------------------------------------------------
-- Reference Custom Widgets
-- -----------------------------------------------------------------------------

-- | Rotary knob over @[minV, maxV]@, 36 px across. Drag vertically, scroll,
-- or use the arrow keys. Pass the current value; the result is the value
-- after this frame.
{-# INLINE knob #-}
knob :: Ui :> es => Float -> Float -> Float -> Eff es Float
knob minV maxV value = snd <$> knobWith' id 36 minV maxV value

{-# INLINE knob' #-}
knob' :: Ui :> es => Float -> Float -> Float -> Eff es (Response, Float)
knob' = knobWith' id 36

-- | 'knob' with a layout modifier and a diameter in pixels.
{-# INLINE knobWith #-}
knobWith :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Float -> Float -> Eff es Float
knobWith f diameter minV maxV value = snd <$> knobWith' f diameter minV maxV value

knobWith' ::
  Ui :> es =>
  (Layout -> Layout) -> Float -> Float -> Float -> Float -> Eff es (Response, Float)
knobWith' f diameter minV maxV value = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  current <- uiIO $ adoptStoreFloat ctx wid key value
  let range = maxV - minV
      frac = if range > 0 then clamp01 ((current - minV) / range) else 0
  (resp, ()) <- customWidgetWithId wid defaultCustomWidgetSpec
    { widgetLayout = fixedWH diameter diameter (f defaultLayout)
    , widgetMeasure = Just $ \_ _ -> (diameter, diameter)
    , widgetCursor = Just (\_ -> UiCursorNsResize)
    , widgetFocusable = True
    , widgetContent = contentKey [frac]
    , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
        let cx = x + w / 2
            cy = y + h / 2
            r = min (w / 2) (h / 2) - 2
            theme = cdcTheme cdc
            hover = cdcHovered cdc
            pressed = cdcPressed cdc
            bgCol =
              if pressed
                then styleActiveBg (themeButton theme)
                else if hover
                  then styleHoverBg (themeButton theme)
                  else styleBg (themeButton theme)
            accent = themeAccent theme
            borderCol = styleBorder (themeButton theme)
            angle = (135 + frac * 270) * (pi / 180)
            ix = cx + cos angle * (r * 0.75)
            iy = cy + sin angle * (r * 0.75)
        drawCircle (V2 cx cy) r bgCol
        drawStrokeCircle (V2 cx cy) r 1.5 borderCol
        drawStrokeAA (V2 cx cy) (V2 ix iy) 2.5 accent
    }
  let bounds = respRect resp
  drag <- useDrag2D bounds
  (_scrollX, scrollY) <- useWheelDelta bounds
  nav <- useKeyNav wid
  let isDragging = dragActive drag
      dy = if isDragging then - v2Y (dragDelta drag) else 0
      dScroll = scrollY * 2.0
      dKey =
        (if knRight nav || knUp nav then 1 else 0 :: Int)
          - (if knLeft nav || knDown nav then 1 else 0)
      deltaNorm =
        if range > 0
          then (dy / 120.0) + (dScroll / 60.0) + fromIntegral dKey * 0.05
          else 0
      finalVal =
        if deltaNorm /= 0
          then clamp minV maxV (current + deltaNorm * range)
          else current
  uiIO $ do
    writeStoreFloat ctx wid key finalVal
    recordStoreFloat ctx key finalVal
  pure (setChanged (finalVal /= current) resp, finalVal)

-- | On/off switch. Pass the current state; the result is the state after
-- this frame's click or Space/Enter.
{-# INLINE toggleSwitch #-}
toggleSwitch :: Ui :> es => Bool -> Eff es Bool
toggleSwitch on = snd <$> toggleSwitchWith' id on

{-# INLINE toggleSwitch' #-}
toggleSwitch' :: Ui :> es => Bool -> Eff es (Response, Bool)
toggleSwitch' = toggleSwitchWith' id

-- | 'toggleSwitch' with a layout modifier.
{-# INLINE toggleSwitchWith #-}
toggleSwitchWith :: Ui :> es => (Layout -> Layout) -> Bool -> Eff es Bool
toggleSwitchWith f on = snd <$> toggleSwitchWith' f on

toggleSwitchWith' :: Ui :> es => (Layout -> Layout) -> Bool -> Eff es (Response, Bool)
toggleSwitchWith' f on = do
  wid <- nextId
  ctx <- askContext
  let key = intKey wid
  current <- intBool <$> uiIO (adoptStoreInt ctx wid key (boolInt on))
  let pillW = 44.0
      pillH = 24.0
  (resp, ()) <- customWidgetWithId wid defaultCustomWidgetSpec
    { widgetLayout = fixedWH pillW pillH (f defaultLayout)
    , widgetMeasure = Just $ \_ _ -> (pillW, pillH)
    , widgetCursor = Just (\_ -> UiCursorPointer)
    , widgetFocusable = True
    , widgetContent = contentKey [if current then 1 else 0]
    , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
        let theme = cdcTheme cdc
            r = h / 2
            accent = themeAccent theme
            mutedCol = styleBg (themeButton theme)
            bgCol = if current then accent else mutedCol
            thumbR = r - 3
            thumbX = if current then (x + w - r) else (x + r)
            thumbY = y + r
            thumbCol = themeOnAccent theme
        drawRoundedRect (Rect x y w h) r bgCol
        drawStrokeRoundedRect (Rect x y w h) r 1 (styleBorder (themeButton theme))
        drawCircle (V2 thumbX thumbY) thumbR thumbCol
    }
  keyClick <- keyActivated wid
  let clicked = respClicked resp || keyClick
      newVal = current /= clicked
  uiIO $ do
    writeStoreBool ctx wid newVal
    recordStoreInt ctx key (boolInt newVal)
  pure (setChanged clicked resp, newVal)

-- | Progress ring for a fraction in @[0, 1]@, 32 px across.
{-# INLINE circularProgress #-}
circularProgress :: Ui :> es => Float -> Eff es ()
circularProgress frac = void (circularProgressWith' id 32 frac)

{-# INLINE circularProgress' #-}
circularProgress' :: Ui :> es => Float -> Eff es Response
circularProgress' = circularProgressWith' id 32

-- | 'circularProgress' with a layout modifier and a diameter in pixels.
{-# INLINE circularProgressWith #-}
circularProgressWith :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Eff es ()
circularProgressWith f diameter frac = void (circularProgressWith' f diameter frac)

circularProgressWith' :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Eff es Response
circularProgressWith' f diameter frac =
  fst <$> customWidget defaultCustomWidgetSpec
    { widgetLayout = fixedWH diameter diameter (f defaultLayout)
    , widgetMeasure = Just $ \_ _ -> (diameter, diameter)
    , widgetContent = contentKey [clamp01 frac]
    , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
        let cx = x + w / 2
            cy = y + h / 2
            r = min (w / 2) (h / 2) - 2
            theme = cdcTheme cdc
            trackCol = styleBorder (themeButton theme)
            accent = themeAccent theme
            clampedFrac = clamp01 frac
        drawStrokeCircle (V2 cx cy) r 2.0 trackCol
        when (clampedFrac > 0) $
          drawCircle (V2 cx cy) (r * clampedFrac) accent
    }

-- | An indeterminate loading indicator: a short accent arc turning over a
-- faint ring, 18 px across. It keeps the frame loop running while it is on
-- screen and repaints only its own rect.
{-# INLINE spinner #-}
spinner :: Ui :> es => Eff es ()
spinner = void (spinnerWith' id 18)

{-# INLINE spinner' #-}
spinner' :: Ui :> es => Eff es Response
spinner' = spinnerWith' id 18

-- | 'spinner' with a layout modifier and a diameter in pixels.
{-# INLINE spinnerWith #-}
spinnerWith :: Ui :> es => (Layout -> Layout) -> Float -> Eff es ()
spinnerWith f diameter = void (spinnerWith' f diameter)

spinnerWith' :: Ui :> es => (Layout -> Layout) -> Float -> Eff es Response
spinnerWith' f diameter = do
  t <- uiTime
  let !d = max 4 diameter
      -- One turn every 0.8 s, in 48 steps: the step is the content key, so
      -- frames within a step reuse the ops.
      !step = floor (t * 48 / 0.8) `mod` 48 :: Int
  resp <-
    fst <$> customWidget defaultCustomWidgetSpec
      { widgetLayout = fixedWH d d (f defaultLayout)
      , widgetMeasure = Just $ \_ _ -> (d, d)
      , widgetContent = step + 1
      , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
          let theme = cdcTheme cdc
              thick = max 1.5 (d / 9)
              r = min w h / 2 - thick / 2
              cx = x + w / 2
              cy = y + h / 2
              start = 2 * pi * fromIntegral step / 48
              at a = V2 (cx + r * cos a) (cy + r * sin a)
              segments = 8 :: Int
              sweep = pi / 2
          drawStrokeCircle (V2 cx cy) r thick (fadeAlpha (themeAccent theme) 48)
          forM_ [0 .. segments - 1] $ \i -> do
            let a0 = start + sweep * fromIntegral i / fromIntegral segments
                a1 = start + sweep * fromIntegral (i + 1) / fromIntegral segments
            drawStrokeAA (at a0) (at a1) thick (themeAccent theme)
      }
  keepAnimating resp
  pure resp

-- | Horizontal progress bar for a fraction in @[0, 1]@. It fills the
-- available width at a fixed height.
{-# INLINE progressBar #-}
progressBar :: Ui :> es => Float -> Eff es ()
progressBar frac = void (progressBarWith' id progressBarDefaultHeight frac)

{-# INLINE progressBar' #-}
progressBar' :: Ui :> es => Float -> Eff es Response
progressBar' = progressBarWith' id progressBarDefaultHeight

-- | 'progressBar' with a layout modifier and a bar height in pixels.
{-# INLINE progressBarWith #-}
progressBarWith :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Eff es ()
progressBarWith f height frac = void (progressBarWith' f height frac)

progressBarWith' :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Eff es Response
progressBarWith' f height frac =
  let !barH = max 0 height
   in fst <$> customWidget defaultCustomWidgetSpec
        { widgetLayout = fillW (fixedH barH (f defaultLayout))
        , widgetMeasure = Just $ \_ _ -> (progressBarDefaultWidth, barH)
        , widgetContent = contentKey [clamp01 frac]
        , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
            let theme = cdcTheme cdc
                trackCol = styleBg (themeButton theme)
                borderCol = styleBorder (themeButton theme)
                fillCol = themeAccent theme
                barW = max 0 w
                barH' = max 0 h
                rad = barH' / 2
                clamped = clamp01 frac
                fillWpx = barW * clamped
                fillRad = if barH' <= 0 then 0 else min rad (fillWpx / 2)
            drawRoundedRect (Rect x y barW barH') rad trackCol
            when (clamped > 0 && fillWpx > 0) $
              drawRoundedRect (Rect x y fillWpx barH') fillRad fillCol
            drawStrokeRoundedRect (Rect x y barW barH') rad 1 borderCol
        }

-- | Default height and minimum content width for 'progressBar'.
progressBarDefaultHeight, progressBarDefaultWidth :: Float
progressBarDefaultHeight = 12.0
progressBarDefaultWidth = 120.0

-- | A small line chart of the values, 80 by 24 px, scaled to their range.
{-# INLINE sparkline #-}
sparkline :: Ui :> es => [Float] -> Eff es ()
sparkline values = void (sparklineWith' id 80 24 values)

{-# INLINE sparkline' #-}
sparkline' :: Ui :> es => [Float] -> Eff es Response
sparkline' = sparklineWith' id 80 24

-- | 'sparkline' with a layout modifier and a width and height in pixels.
{-# INLINE sparklineWith #-}
sparklineWith :: Ui :> es => (Layout -> Layout) -> Float -> Float -> [Float] -> Eff es ()
sparklineWith f prefW prefH values = void (sparklineWith' f prefW prefH values)

sparklineWith' :: Ui :> es => (Layout -> Layout) -> Float -> Float -> [Float] -> Eff es Response
sparklineWith' f prefW prefH values =
  fst <$> customWidget defaultCustomWidgetSpec
    { widgetLayout = fixedWH prefW prefH (f defaultLayout)
    , widgetMeasure = Just $ \_ _ -> (prefW, prefH)
    , widgetContent = contentKey values
    , widgetDraw = \cdc (Rect x y rw rh) -> runCanvas $ do
        let theme = cdcTheme cdc
            accent = themeAccent theme
            bg = styleBg (themePanel theme)
        drawRoundedRect (Rect x y rw rh) 3.0 bg
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
                drawSegments [] = pure ()
                drawSegments [_] = pure ()
                drawSegments (p1 : p2 : rest) = do
                  drawStrokeAA p1 p2 1.5 accent
                  drawSegments (p2 : rest)
            drawSegments pts
            case pts of
              [] -> pure ()
              _ -> drawCircle (last pts) 2.5 accent
    }
