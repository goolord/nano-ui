-- | Custom widgets and the reference widgets built on them.
--
-- 'customWidget' takes a 'CustomWidgetSpec': a layout, optional measurement,
-- drawing that sees hover and press state, an optional content key, a cursor,
-- and damage slop.
-- 'canvas' is the short form for drawing into a laid-out rectangle with
-- 'CanvasM', which fills and strokes paths from "NanoUI.Path" as well as
-- rects, circles, lines, images and text, clips, and draws through
-- transforms; 'canvasConfigured' adds a content key, a cursor and pointer
-- tracking, and 'drawContext' hands the drawing hover and press state.
-- 'useDrag2D' and 'useWheelDelta' are gesture hooks for your own controls;
-- 'knob' and 'toggleSwitch' show how they fit together. Each reference
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
  , canvas
  , CanvasConfig (..)
  , defaultCanvasConfig
  , canvasConfigured
  , runCanvasFor
  , drawContext
  , runCanvas
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
  , checkboxBoxSize
    -- * Paths, clips and transforms

    -- | Build a path with "NanoUI.Path", imported qualified, and fill or
    -- stroke it here, in a colour or a 'NanoUI.Path.Paint'. 'withTransform'
    -- moves, turns and scales what a block draws, and 'withClip' clips it.
  , drawPath
  , drawPathWith
  , drawStrokePath
  , drawStrokePathWith
  , withTransform
  , withClip
    -- * Gestures
  , useDrag2D
  , Drag2D (..)
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
import Data.Text qualified as T
import Data.Primitive.SmallArray (emptySmallArray)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Canvas
import NanoUI.Internal.Context
import Data.Word (Word64)
import Data.Hashable (Hashable, hash)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Font (checkboxBoxSize)
import NanoUI.Internal.Id (WidgetId, mix64)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Internal.Monad (Ui, askContext, askInput, freshWidget, nextId, uiIO, uiTime)
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
-- for the display it is on ('runCanvasFor'). Its ops are rebuilt and
-- compared every frame; 'canvasConfigured' takes a content key that saves
-- that, and a cursor.
canvas :: (Ui :> es) => (Layout -> Layout) -> (Rect -> CanvasM ()) -> Eff es Response
canvas f = canvasConfigured defaultCanvasConfig {canvasLayout = f defaultLayout}

-- | What 'canvasConfigured' draws with besides its drawing.
data CanvasConfig = CanvasConfig
  { canvasLayout :: !Layout
    -- ^ Its size and place, as a custom widget's 'widgetLayout' (default
    -- 'defaultLayout').
  , canvasContent :: !Int
    -- ^ A content key, as 'widgetContent': a number that changes whenever
    -- the drawing would draw something different, made with 'contentKey'
    -- or 'contentKeyOf' (default 0, no key: the drawing runs every frame).
  , canvasTrackPointer :: !Bool
    -- ^ A frame for every pointer move over it, for a drawing of what is
    -- under the pointer ('widgetTrackPointer'; default 'False').
  , canvasCursor :: !(Maybe (CustomDrawContext -> UiCursorKind))
    -- ^ The pointer's shape over it ('widgetCursor'; default 'Nothing').
  }

-- | A canvas laid out by 'defaultLayout', with no content key, cursor or
-- pointer tracking.
defaultCanvasConfig :: CanvasConfig
defaultCanvasConfig =
  CanvasConfig
    { canvasLayout = defaultLayout
    , canvasContent = 0
    , canvasTrackPointer = False
    , canvasCursor = Nothing
    }

-- | 'canvas' as a 'CanvasConfig' says. The drawing reads its hover and
-- press state and theme with 'drawContext':
--
-- > canvasConfigured defaultCanvasConfig {canvasLayout = fixedWH 120 24 defaultLayout, canvasContent = contentKey [level]} $ \r -> do
-- >   cdc <- drawContext
-- >   drawRoundedRect r 4 (if cdcHovered cdc then themeAccent (cdcTheme cdc) else themeMuted (cdcTheme cdc))
canvasConfigured :: (Ui :> es) => CanvasConfig -> (Rect -> CanvasM ()) -> Eff es Response
canvasConfigured cfg drawAction =
  fst <$> customWidget defaultCustomWidgetSpec
    { widgetLayout = canvasLayout cfg
    , widgetContent = canvasContent cfg
    , widgetTrackPointer = canvasTrackPointer cfg
    , widgetCursor = canvasCursor cfg
    , widgetDraw = \cdc rect -> runCanvasFor cdc (drawAction rect)
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
  (wid, ctx) <- freshWidget
  inp <- askInput
  -- The drag flag is quiet bookkeeping; the last pointer position is a point
  -- slot.
  let dragK = slotKey SlotDrag (intKey wid)
      mouse = inputMousePos inp
  store <- uiIO (getStore ctx)
  let active0 = quietFlag dragK store
      active = inputMouseDown inp && (active0 || (inputMousePressed inp && rectContains bounds mouse))
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

-- | Inspects mouse wheel scroll delta when pointer is hovering over bounds.
useWheelDelta :: (Ui :> es) => Rect -> Eff es (Float, Float)
useWheelDelta bounds = do
  inp <- askInput
  let V2 x y = inputScroll inp
  pure (if rectContains bounds (inputMousePos inp) then (x, y) else (0, 0))

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
  drag <- useDrag2D (respRect resp)
  (_, scrollY) <- useWheelDelta (respRect resp)
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
