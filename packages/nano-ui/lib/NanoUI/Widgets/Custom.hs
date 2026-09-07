{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

-- | First-class custom widget definition system for nano-ui.
--
-- Provides:
-- 1. 'CustomWidgetSpec' & 'customWidget': A unified specification for leaf/interactive widgets
--    supporting custom layout measurement, interaction-aware drawing, custom cursors, and damage slop.
-- 2. 'CanvasM': A fast, declarative monadic canvas builder emitting vector 'DrawOp's.
-- 3. 'canvas' & 'canvasWith': Ergonomic one-line helpers for custom graphics and interactive visual components.
-- 4. Reusable 2D gesture hooks ('useDrag2D', 'useWheelDelta', 'useClickGesture').
module NanoUI.Widgets.Custom
  ( -- * Custom Widget Specification
    CustomWidgetSpec (..)
  , defaultCustomWidgetSpec
  , customWidget
  , customWidget_
  , customWidgetWithId
  , CustomDrawContext (..)
  , CustomMeasureFn
  , CustomDrawBuild
    -- * Canvas Monad & Drawing
  , CanvasM
  , runCanvas
  , canvas
  , canvasWith
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
    -- * Common Gesture & Behavior Helpers
  , useDrag2D
  , Drag2D (..)
  , useWheelDelta
  , useClickGesture
  , ClickGesture (..)
    -- * Reference Custom Widgets
  , knob
  , knobWith
  , toggleSwitch
  , toggleSwitchWith
  , circularProgress
  , circularProgressWith
  , progressBar
  , progressBarWith
  , sparkline
  , sparklineWith
  ) where

import Control.Monad (void, when)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , CustomDrawBuild
  , CustomDrawContext (..)
  , CustomMeasureFn
  , getFocusId
  , getStore
  , intKey
  , isDisabled
  , markDirty
  , registerCustomCursor
  , registerCustomDamageSlop
  , registerCustomDrawing
  , registerCustomMeasure
  , registerFocusable
  , setStore
  )
import NanoUI.Draw (DrawOp (..))
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , UiCursorKind (..)
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputScroll
  )
import NanoUI.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Store (WidgetStore (..), boolInt, intBool, slotDrag, slotKey)
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
  )
import NanoUI.Types
  ( Color
  , ImageId (..)
  , Rect (..)
  , V2 (..)
  , colorRGBA
  , defaultDamageSlop
  , rectContains
  , rectH
  , rectW
  , rectX
  , rectY
  , v2X
  , v2Y
  )
import NanoUI.Widgets.Node
  ( Responding (..)
  , Response
  , addWidget
  , mkResponse
  , setChanged
  )

-- -----------------------------------------------------------------------------
-- Canvas Monad
-- -----------------------------------------------------------------------------

-- | Monadic canvas builder that collects 'DrawOp' vector operations efficiently.
newtype CanvasM a = CanvasM { runCanvasM :: ([DrawOp] -> [DrawOp]) -> (a, [DrawOp] -> [DrawOp]) }

instance Functor CanvasM where
  fmap f (CanvasM m) = CanvasM $ \s ->
    let (a, s') = m s in (f a, s')

instance Applicative CanvasM where
  pure a = CanvasM $ \s -> (a, s)
  CanvasM mf <*> CanvasM mx = CanvasM $ \s ->
    let (f, s1) = mf s
        (x, s2) = mx s1
     in (f x, s2)

instance Monad CanvasM where
  CanvasM m >>= f = CanvasM $ \s ->
    let (a, s') = m s in runCanvasM (f a) s'

-- | Compile a 'CanvasM' block into an immutable 'Vector DrawOp'.
runCanvas :: CanvasM a -> Vector DrawOp
runCanvas (CanvasM m) =
  let (_, diff) = m id
   in V.fromList (diff [])

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
  , widgetCursor     :: !(Maybe (CustomDrawContext -> UiCursorKind))
    -- ^ Optional custom mouse cursor when pointer is over the widget.
  , widgetFocusable  :: !Bool
    -- ^ Whether this widget accepts tab/keyboard focus.
  , widgetDamageSlop :: !Float
    -- ^ Padding added to dirty rectangles (for shadows, glow, or drag handles).
  , widgetInteract   :: !(WidgetId -> Rect -> CustomDrawContext -> Input -> (Response, a))
    -- ^ Interaction response and value evaluation hook.
  }

-- | Default configuration for a custom widget with standard hover/press/click behavior.
defaultCustomWidgetSpec :: CustomWidgetSpec ()
defaultCustomWidgetSpec = CustomWidgetSpec
  { widgetLayout     = defaultLayout
  , widgetMeasure    = Nothing
  , widgetDraw       = \_ _ -> V.empty
  , widgetCursor     = Nothing
  , widgetFocusable  = False
  , widgetDamageSlop = defaultDamageSlop
  , widgetInteract   = \wid r cdc inp ->
      let hovered = cdcHovered cdc
          pressed = cdcPressed cdc
          clicked = hovered && inputMouseReleased inp
       in (mkResponse wid r hovered pressed clicked False, ())
  }

-- | Instantiates a custom widget using an existing 'WidgetId'.
customWidgetWithId :: (Ui :> es) => WidgetId -> CustomWidgetSpec a -> Eff es (Response, a)
customWidgetWithId wid spec = do
  ctx <- askContext
  inp <- askInput
  uiIO $ do
    when (widgetFocusable spec) $ registerFocusable ctx wid
    case widgetMeasure spec of
      Just mFn -> registerCustomMeasure ctx wid mFn
      Nothing  -> pure ()
    registerCustomDrawing ctx wid (widgetDraw spec)
    case widgetCursor spec of
      Just cFn -> registerCustomCursor ctx wid cFn
      Nothing  -> pure ()
    when (widgetDamageSlop spec > 0) $
      registerCustomDamageSlop ctx wid (widgetDamageSlop spec)
  resp0 <- addWidget wid NodeDrawing T.empty 0 (widgetLayout spec)
  (resp, val) <- uiIO $ do
    disabled <- isDisabled ctx wid
    focused <- (== wid) <$> getFocusId ctx
    active <- readIORef (ctxActiveId ctx)
    theme <- readIORef (ctxTheme ctx)
    let cdc =
          CustomDrawContext
            { cdcHovered  = respHovered resp0
            , cdcPressed  = respPressed resp0
            , cdcFocused  = focused
            , cdcActive   = active == wid
            , cdcDisabled = disabled
            , cdcTheme    = theme
            , cdcHost     = ctxHostProfile ctx
            , cdcFont     = ctxFontMetrics ctx
            }
    pure (widgetInteract spec wid (respRect resp0) cdc inp)
  pure (resp, val)

-- | Instantiates a custom widget from a 'CustomWidgetSpec'.
--
-- Connects the widget into:
-- - The two-pass layout arena (respecting 'widgetMeasure' or layout constraints).
-- - Off-heap vector drawing pipeline (cached with automatic interaction-change invalidation).
-- - Interactive hit-testing, focus management, and custom cursor resolution.
-- - Accurate damage region tracking with 'widgetDamageSlop'.
customWidget :: (Ui :> es) => CustomWidgetSpec a -> Eff es (Response, a)
customWidget spec = do
  wid <- nextId
  customWidgetWithId wid spec

-- | Convenient variant of 'customWidget' when no custom value is returned.
customWidget_ :: (Ui :> es) => CustomWidgetSpec () -> Eff es Response
customWidget_ spec = fmap fst (customWidget spec)

-- | Declarative canvas widget for rendering custom shapes, diagrams, or graphics.
canvas :: (Ui :> es) => Layout -> (Rect -> CanvasM ()) -> Eff es Response
canvas lay drawAction =
  customWidget_ defaultCustomWidgetSpec
    { widgetLayout = lay
    , widgetDraw   = \_ rect -> runCanvas (drawAction rect)
    }

-- | Interactive canvas widget that receives hover, press, focus, and theme states.
canvasWith ::
  (Ui :> es) =>
  Layout ->
  (CustomDrawContext -> Rect -> CanvasM a) ->
  Eff es (Response, a)
canvasWith lay drawAction =
  customWidget defaultCustomWidgetSpec
    { widgetLayout   = lay
    , widgetDraw     = \cdc rect -> runCanvas (void (drawAction cdc rect))
    , widgetInteract = \wid r cdc inp ->
        let hovered = cdcHovered cdc
            pressed = cdcPressed cdc
            clicked = hovered && inputMouseReleased inp
            val     = fst (runCanvasM (drawAction cdc r) id)
         in (mkResponse wid r hovered pressed clicked False, val)
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
  let key = intKey wid
      dragK = slotKey slotDrag key
      slotPosX = dragK + 1
      slotPosY = dragK + 2
      mouse = inputMousePos inp
      down = inputMouseDown inp
      press = inputMousePressed inp
  store <- uiIO (getStore ctx)
  let active0 = IM.findWithDefault 0 dragK (storeInt store) /= 0
      hit = rectContains bounds mouse
      active = down && (active0 || (press && hit))
      prevX = IM.findWithDefault (v2X mouse) slotPosX (storeFloat store)
      prevY = IM.findWithDefault (v2Y mouse) slotPosY (storeFloat store)
      delta =
        if active && active0
          then V2 (v2X mouse - prevX) (v2Y mouse - prevY)
          else V2 0 0
      clampedMouse =
        V2
          (max (rectX bounds) (min (rectX bounds + rectW bounds) (v2X mouse)))
          (max (rectY bounds) (min (rectY bounds + rectH bounds) (v2Y mouse)))
  uiIO $ do
    st <- getStore ctx
    let sInt =
          if active
            then IM.insert dragK 1 (storeInt st)
            else IM.delete dragK (storeInt st)
        sFloat =
          if active
            then IM.insert slotPosX (v2X mouse) (IM.insert slotPosY (v2Y mouse) (storeFloat st))
            else IM.delete slotPosX (IM.delete slotPosY (storeFloat st))
    setStore ctx (st { storeInt = sInt, storeFloat = sFloat })
  pure Drag2D { dragPosition = clampedMouse, dragActive = active, dragDelta = delta }

-- | Inspects mouse wheel scroll delta when pointer is hovering over bounds.
useWheelDelta :: (Ui :> es) => Rect -> Eff es (Float, Float)
useWheelDelta bounds = do
  inp <- askInput
  let mouse = inputMousePos inp
  if rectContains bounds mouse
    then pure (v2X (inputScroll inp), v2Y (inputScroll inp))
    else pure (0, 0)

-- | Gesture classification for mouse clicks.
data ClickGesture
  = ClickNone
  | ClickSingle
  deriving (Eq, Show)

-- | Determines click gestures on a widget response.
useClickGesture :: Response -> ClickGesture
useClickGesture resp
  | respClicked resp = ClickSingle
  | otherwise        = ClickNone

-- -----------------------------------------------------------------------------
-- Reference Custom Widgets
-- -----------------------------------------------------------------------------

-- | Rotary dial / knob control.
-- Draggable vertically to adjust value between min and max bounds.
-- Mouse wheel over the knob allows fine-tuning.
knob
  :: (Ui :> es)
  => Float                -- ^ Minimum value
  -> Float                -- ^ Maximum value
  -> Float                -- ^ Initial / default value
  -> Eff es (Response, Float)
knob = knobWith defaultLayout 36.0

-- | Rotary knob with custom layout and diameter.
knobWith
  :: (Ui :> es)
  => Layout
  -> Float                -- ^ Diameter in pixels
  -> Float                -- ^ Min value
  -> Float                -- ^ Max value
  -> Float                -- ^ Initial / default value
  -> Eff es (Response, Float)
knobWith layout diameter minV maxV initial = do
  wid <- nextId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let key = intKey wid
      current = IM.findWithDefault initial key (storeFloat store)
      range = maxV - minV
      frac = if range > 0 then max 0 (min 1 ((current - minV) / range)) else 0
  (resp, ()) <- customWidgetWithId wid defaultCustomWidgetSpec
    { widgetLayout = fixedWH diameter diameter layout
    , widgetMeasure = Just $ \_ _ _ -> (diameter, diameter)
    , widgetCursor = Just (\_ -> UiCursorNsResize)
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
  let isDragging = dragActive drag
      dy = if isDragging then - v2Y (dragDelta drag) else 0
      dScroll = scrollY * 2.0
      deltaNorm = if range > 0 then (dy / 120.0) + (dScroll / 60.0) else 0
      finalVal =
        if deltaNorm /= 0
          then max minV (min maxV (current + deltaNorm * range))
          else current
  when (finalVal /= current) $ do
    uiIO $ do
      st <- getStore ctx
      setStore ctx (st { storeFloat = IM.insert key finalVal (storeFloat st) })
      markDirty ctx
  pure (setChanged (finalVal /= current) resp, finalVal)

-- | iOS-style toggle pill switch.
toggleSwitch
  :: (Ui :> es)
  => Bool                 -- ^ Initial state
  -> Eff es (Response, Bool)
toggleSwitch = toggleSwitchWith defaultLayout

-- | Toggle switch with custom layout constraints.
toggleSwitchWith
  :: (Ui :> es)
  => Layout
  -> Bool
  -> Eff es (Response, Bool)
toggleSwitchWith layout initial = do
  wid <- nextId
  ctx <- askContext
  store <- uiIO (getStore ctx)
  let key = intKey wid
      current = intBool (IM.findWithDefault (boolInt initial) key (storeInt store))
      pillW = 44.0
      pillH = 24.0
  (resp, ()) <- customWidgetWithId wid defaultCustomWidgetSpec
    { widgetLayout = fixedWH pillW pillH layout
    , widgetMeasure = Just $ \_ _ _ -> (pillW, pillH)
    , widgetCursor = Just (\_ -> UiCursorPointer)
    , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
        let theme = cdcTheme cdc
            r = h / 2
            accent = themeAccent theme
            mutedCol = styleBg (themeButton theme)
            bgCol = if current then accent else mutedCol
            thumbR = r - 3
            thumbX = if current then (x + w - r) else (x + r)
            thumbY = y + r
            thumbCol = colorRGBA 255 255 255 255
        drawRoundedRect (Rect x y w h) r bgCol
        drawStrokeRoundedRect (Rect x y w h) r 1 (styleBorder (themeButton theme))
        drawCircle (V2 thumbX thumbY) thumbR thumbCol
    }
  let clicked = respClicked resp
      newVal = if clicked then not current else current
  when clicked $ do
    uiIO $ do
      st <- getStore ctx
      setStore ctx (st { storeInt = IM.insert key (boolInt newVal) (storeInt st) })
      markDirty ctx
  pure (setChanged clicked resp, newVal)

-- | Circular progress ring indicator (clamped between 0.0 and 1.0).
circularProgress
  :: (Ui :> es)
  => Float                -- ^ Progress fraction (0.0 to 1.0)
  -> Eff es Response
circularProgress = circularProgressWith defaultLayout 32.0

-- | Circular progress ring with custom layout and diameter.
circularProgressWith
  :: (Ui :> es)
  => Layout
  -> Float                -- ^ Diameter in pixels
  -> Float                -- ^ Progress fraction (0.0 to 1.0)
  -> Eff es Response
circularProgressWith layout diameter frac = do
  customWidget_ defaultCustomWidgetSpec
    { widgetLayout = fixedWH diameter diameter layout
    , widgetMeasure = Just $ \_ _ _ -> (diameter, diameter)
    , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
        let cx = x + w / 2
            cy = y + h / 2
            r = min (w / 2) (h / 2) - 2
            theme = cdcTheme cdc
            trackCol = styleBorder (themeButton theme)
            accent = themeAccent theme
            clampedFrac = max 0 (min 1 frac)
        drawStrokeCircle (V2 cx cy) r 2.0 trackCol
        when (clampedFrac > 0) $
          drawCircle (V2 cx cy) (r * clampedFrac) accent
    }

-- | Horizontal linear progress bar. The fraction is clamped to 0..1; the bar
-- fills the available width at a fixed height (like a slider track).
progressBar
  :: (Ui :> es)
  => Float                -- ^ Progress fraction (0.0 to 1.0)
  -> Eff es Response
progressBar = progressBarWith defaultLayout progressBarDefaultHeight

-- | Horizontal linear progress bar with a custom layout and bar height.
progressBarWith
  :: (Ui :> es)
  => Layout
  -> Float                -- ^ Bar height in pixels
  -> Float                -- ^ Progress fraction (0.0 to 1.0)
  -> Eff es Response
progressBarWith layout height frac =
  let !barH = max 0 height
   in customWidget_ defaultCustomWidgetSpec
        { widgetLayout = fillW (fixedH barH layout)
        , widgetMeasure = Just $ \_ _ _ -> (progressBarDefaultWidth, barH)
        , widgetDraw = \cdc (Rect x y w h) -> runCanvas $ do
            let theme = cdcTheme cdc
                trackCol = styleBg (themeButton theme)
                borderCol = styleBorder (themeButton theme)
                fillCol = themeAccent theme
                barW = max 0 w
                barH' = max 0 h
                rad = barH' / 2
                clamped = max 0 (min 1 frac)
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

-- | Compact inline data chart drawing smooth anti-aliased polyline segments.
sparkline
  :: (Ui :> es)
  => [Float]              -- ^ Data point sequence
  -> Eff es Response
sparkline = sparklineWith defaultLayout 80.0 24.0

-- | Sparkline chart with custom layout and dimensions.
sparklineWith
  :: (Ui :> es)
  => Layout
  -> Float                -- ^ Preferred width
  -> Float                -- ^ Preferred height
  -> [Float]              -- ^ Data point sequence
  -> Eff es Response
sparklineWith layout prefW prefH values = do
  customWidget_ defaultCustomWidgetSpec
    { widgetLayout = fixedWH prefW prefH layout
    , widgetMeasure = Just $ \_ _ _ -> (prefW, prefH)
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
            case reverse pts of
              (lastPt : _) -> drawCircle lastPt 2.5 accent
              _            -> pure ()
    }
