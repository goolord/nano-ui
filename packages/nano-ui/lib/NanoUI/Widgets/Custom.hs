-- | Custom widgets and the reference widgets built on them.
--
-- 'customWidget' takes a 'CustomWidgetSpec': a layout, optional measurement,
-- drawing that sees hover and press state, an optional content key, a cursor,
-- and damage slop.
-- 'canvas' is the short form: it draws into a laid-out rectangle with
-- 'CanvasM' (rects, circles, lines, images, text, "NanoUI.Path" paths, clips
-- and transforms). 'canvasConfigured' adds a content key, a cursor and
-- pointer tracking; 'drawContext' gives the drawing hover and press state.
-- 'useDrag2DOn' and 'useWheelDeltaOn' are gesture hooks that take the
-- widget's 'Response'; 'knob' and 'toggleSwitch' show them in use. Each
-- reference widget comes as @x@ at its default size, and as @xWith'@, which
-- takes a layout modifier and a size and also returns the 'Response'.
module NanoUI.Widgets.Custom
  ( -- * Custom widgets
    CustomWidgetSpec (..)
  , KeyClaim (..)
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
  , ImageDraw (..)
  , imageDraw
  , drawImageWith
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
    -- something different ('contentKey' hashes numbers into one). If the key,
    -- size, interaction state and metrics are unchanged, the frame skips
    -- rebuilding the ops and repainting. The default 0 means no key: ops are
    -- rebuilt and compared every frame. A stale key draws stale pixels, so
    -- derive it from everything the drawing reads. The key is trusted even
    -- while the widget animates.
  , widgetCursor     :: !(Maybe (CustomDrawContext -> Rect -> V2 -> UiCursorKind))
    -- ^ Pointer shape over the widget, given the draw context, the widget's
    -- rect and the pointer position, so different parts can show different
    -- shapes. It is also queried during a drag that started on the widget,
    -- so the drag keeps its shape. 'UiCursorDefault' defers to an enclosing
    -- 'NanoUI.withCursorShape', or the arrow.
    --
    -- > widgetCursor = Just $ \_ (Rect x _ w _) (V2 px _) ->
    -- >   if px > x + w - 6 then UiCursorEwResize else UiCursorDefault
  , widgetFocusable  :: !Bool
    -- ^ Whether this widget accepts tab/keyboard focus.
  , widgetKeys       :: !KeyClaim
    -- ^ Keys the widget handles itself while focused; shortcuts and
    -- 'NanoUI.keyPressed' ignore them. Default 'KeysNavigate'. A terminal or
    -- an editor with its own chords uses 'KeysAll'.
  , widgetDamageSlop :: !Float
    -- ^ Padding added to dirty rectangles (for shadows, glow, or drag handles).
  , widgetTrackPointer :: !Bool
    -- ^ Run a frame on every pointer move over the widget, for drawings that
    -- depend on the pointer position. Default 'False': a move runs a frame
    -- only when it crosses onto another widget.
  , widgetInteract   :: !(Response -> CustomDrawContext -> Input -> (Response, a))
    -- ^ Interaction hook: maps the resolved 'Response', draw context and
    -- input to the final response and value.
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
  , widgetKeys       = KeysNavigate
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

-- | A 'widgetContent' key over 'Hashable' values, mixed in order. Unlike
-- 'contentKey', an 'Int' or 'Double' is hashed whole, not rounded to 'Float'.
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
  resp0 <- addWidgetStyled wid NodeDrawing T.empty 0 (widgetLayout spec) (fromEnum (widgetKeys spec))
  cdc <- uiIO (customDrawContext ctx (ctxFontMetrics ctx) wid (respHovered resp0) (respPressed resp0))
  pure (widgetInteract spec resp0 cdc inp)

-- | Instantiates a custom widget from a 'CustomWidgetSpec'.
customWidget :: (Ui :> es) => CustomWidgetSpec a -> Eff es (Response, a)
customWidget spec = do
  wid <- nextId
  customWidgetWithId wid spec

-- | Draw into a rectangle sized by the layout modifier. Curves are flattened
-- for the current display ('runCanvasFor'). Ops are rebuilt and compared
-- every frame; use 'canvasConfigured' for a content key or a cursor.
canvas :: (Ui :> es) => (Layout -> Layout) -> (Rect -> CanvasM ()) -> Eff es Response
canvas f = canvasConfigured defaultCanvasConfig {canvasLayout = f defaultLayout}

-- | Options for 'canvasConfigured'.
data CanvasConfig = CanvasConfig
  { canvasLayout :: !Layout
    -- ^ As 'widgetLayout' (default 'defaultLayout').
  , canvasContent :: !Int
    -- ^ As 'widgetContent'; build it with 'contentKey' or 'contentKeyOf'.
    -- Default 0 (no key): the drawing runs every frame.
  , canvasTrackPointer :: !Bool
    -- ^ As 'widgetTrackPointer' (default 'False').
  , canvasCursor :: !(Maybe (CustomDrawContext -> Rect -> V2 -> UiCursorKind))
    -- ^ As 'widgetCursor' (default 'Nothing').
  }

-- | 'defaultLayout', with no content key, cursor or pointer tracking.
defaultCanvasConfig :: CanvasConfig
defaultCanvasConfig =
  CanvasConfig
    { canvasLayout = defaultLayout
    , canvasContent = 0
    , canvasTrackPointer = False
    , canvasCursor = Nothing
    }

-- | 'canvas' with a 'CanvasConfig'. The drawing reads hover, press state and
-- theme with 'drawContext':
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

-- | A left-button drag that starts with a press on the widget and lasts
-- until release, wherever the pointer goes. For colour pickers, joysticks or
-- panned canvases. The position is clamped to the widget's rect. Only the
-- widget's own press ('respPressed') starts a drag, so nothing starts when
-- the press lands on an overlay, a scrolled-off part, or a disabled widget.
-- Call it every frame, after the widget:
--
-- > (resp, ()) <- customWidget spec
-- > drag <- useDrag2DOn resp
-- > when (dragActive drag) (setPan (dragPosition drag))
useDrag2DOn :: (Ui :> es, HasResponse r) => r -> Eff es Drag2D
useDrag2DOn r = drag2DFrom (respRect r) (respPressed r)

-- | 'useDrag2DOn' over a rect: a press anywhere in it starts the drag, even
-- on something drawn over it.
useDrag2D :: (Ui :> es) => Rect -> Eff es Drag2D
useDrag2D bounds = drag2DFrom bounds . rectContains bounds . inputMousePos =<< askInput
{-# DEPRECATED useDrag2D "Use useDrag2DOn with the widget's Response, which respects what is drawn over it" #-}

-- | A left-button drag already under way, or starting with a press this
-- frame when @onIt@; the position is clamped to @bounds@.
drag2DFrom :: (Ui :> es) => Rect -> Bool -> Eff es Drag2D
drag2DFrom bounds onIt = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  -- The drag flag is quiet bookkeeping (it causes no damage); the last
  -- pointer position goes in a point slot.
  let dragK = slotKey SlotDrag (intKey wid)
      mouse = inputMousePos inp
  store <- uiIO (getStore ctx)
  let active0 = quietFlag dragK store
      active = heldIn MouseLeft inp && (active0 || (pressedIn MouseLeft inp && onIt))
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

-- | This frame's wheel delta while the widget is hovered ('respHovered').
-- Overlays, scrolled-off parts and a disabled widget get none.
useWheelDeltaOn :: (Ui :> es, HasResponse r) => r -> Eff es (Float, Float)
useWheelDeltaOn r = wheelIf (respHovered r)

-- | 'useWheelDeltaOn' over a rect, regardless of what is drawn over it.
useWheelDelta :: (Ui :> es) => Rect -> Eff es (Float, Float)
useWheelDelta bounds = wheelIf . rectContains bounds . inputMousePos =<< askInput
{-# DEPRECATED useWheelDelta "Use useWheelDeltaOn with the widget's Response, which respects what is drawn over it" #-}

-- | This frame's wheel delta when @on@, else zero.
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
  -- NaN never equals itself, so it would be re-adopted, dirtying every frame.
  current <- uiIO $ adoptSlot fieldFloat ctx wid (if isNaN value then minV else value)
  let
    range = maxV - minV
    frac = if range > 0 then clamp01 ((current - minV) / range) else 0
  (resp, ()) <-
    customWidgetWithId
      wid
      (fixedSizeSpec f diameter diameter)
        { widgetCursor = Just (\_ _ _ -> UiCursorNsResize)
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
        { widgetCursor = Just (\_ _ _ -> UiCursorPointer)
        , widgetFocusable = True
        , widgetKeys = KeysActivate
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
-- over a faint ring. It keeps the frame loop running while on screen.
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
