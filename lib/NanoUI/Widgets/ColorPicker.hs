{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.ColorPicker
  ( ColorPickerGeom (..)
  , colorPickerDefaultColor
  , colorPickerMinWidth
  , colorPickerSvH
  , colorPickerExtraH
  , widgetStoreColor
  , widgetStoreBaseColor
  , widgetStoreHue
  , widgetStoreSv
  , clampHue
  , hueTrackContainsX
  , colorPickerGeom
  , colorPickerMeasureSize
  , svFromMouse
  , hueFromMouse
  , colorPickerHueHitRect
  , colorPickerDragSv
  , colorPickerDragHue
  , drawColorPickerPanel
  , colorPicker
  ) where

import Control.Monad (unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word (Word64)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getStore
  , intKey
  , menuPointerGestureActive
  , registerFocusable
  , setStore
  )
import NanoUI.Draw
  ( DrawArena
  , pushQuadGradient
  , pushRoundedRect
  , pushRoundedStroke
  )
import NanoUI.Font
  ( FontMetrics (..)
  , labelContentInset
  , layoutLineHeight
  , sliderTrackHeight
  , widgetContentInset
  )
import NanoUI.Types
  ( Color (..)
  , HostProfile
  , Rect (..)
  , V2 (..)
  , clamp01
  , colorFromWord32
  , colorRGBA
  , colorToWord32
  , hsvToRgb
  , isCellHost
  , rectH
  , rectW
  , rectX
  , rectY
  , rgbToHsv
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputMouseDown, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Style (Style (..), defaultLayout, fillW)
import NanoUI.WidgetText
  ( colorPickerCurrentLabel
  , colorPickerLabelText
  , colorPickerNewLabel
  , colorPickerToHex
  )
import NanoUI.Store (slotKey)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Widgets.Behavior (DragAxis (..), KeyNav (..), keyedDragHeld, useDrag1D, useKeyNav)
import NanoUI.Widgets.Node (Response (..), addWidget, setChanged)

colorPickerDefaultColor :: Color
colorPickerDefaultColor = colorRGBA 128 128 128 255

colorPickerMinWidth :: Float
colorPickerMinWidth = 220

colorPickerGap :: Float
colorPickerGap = 6

colorPickerHexGap :: Float
colorPickerHexGap = 4

colorPickerSwatchH :: Float
colorPickerSwatchH = 28

colorPickerSwatchGap :: Float
colorPickerSwatchGap = 4

-- Fixed SV height. Hue, previews, and hex sit below it and add their own extraH.
colorPickerSvH :: Float
colorPickerSvH = 120

colorPickerHueH :: Float
colorPickerHueH = sliderTrackHeight

-- Live RGB in storeInt at the widget key. Opening color stays in this slot.
slotColorBase :: Word64
slotColorBase = 0x4355524300000001

data ColorPickerGeom = ColorPickerGeom
  { cpgLabelH :: !Float
  , cpgSv :: !Rect
  , cpgHue :: !Rect
  , cpgCurrent :: !Rect
  , cpgNew :: !Rect
  , cpgPreviewLabelY :: !Float
  , cpgHexY :: !Float
  , cpgHexH :: !Float
  }
  deriving (Eq, Show)

storeColorAt :: WidgetStore -> Int -> Color -> Color
storeColorAt store key fallback =
  colorFromWord32
    ( fromIntegral
        ( IM.findWithDefault
            (fromIntegral (colorToWord32 fallback))
            key
            (storeInt store)
        )
    )

widgetStoreColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreColor store wid fallback = storeColorAt store (intKey wid) fallback

widgetStoreBaseColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreBaseColor store wid fallback =
  storeColorAt store (slotKey slotColorBase (intKey wid)) (widgetStoreColor store wid fallback)

-- RGB cannot tell hue 0 from 360. Keep the slider end the user last set.
widgetStoreHue :: WidgetStore -> WidgetId -> Color -> Float
widgetStoreHue store wid fallback =
  let (h0, _, _) = rgbToHsv (widgetStoreColor store wid fallback)
   in IM.findWithDefault h0 (intKey wid) (storeFloat store)

-- Black collapses S in RGB. Keep the last mouse S/V so the marker does not jitter.
widgetStoreSv :: WidgetStore -> WidgetId -> Color -> (Float, Float)
widgetStoreSv store wid fallback =
  let (_, s0, v0) = rgbToHsv (widgetStoreColor store wid fallback)
   in fromMaybe (s0, v0) (IM.lookup (intKey wid) (storePoint store))

-- Hue slider is a line, not a wheel.
clampHue :: Float -> Float
clampHue h = max 0 (min 360 h)

-- extraH: SV + hue track + Current/New row + hex line + gaps. Same pattern as slider trackExtra.
colorPickerExtraH :: Float -> Float
colorPickerExtraH previewLabelH =
  colorPickerGap
    + colorPickerSvH
    + colorPickerGap
    + colorPickerHueH
    + colorPickerGap
    + previewLabelH
    + colorPickerSwatchGap
    + colorPickerSwatchH
    + colorPickerHexGap
    + previewLabelH

colorPickerGeom :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> ColorPickerGeom
colorPickerGeom host fm x y w _h =
  let (ix, iy) =
        if isCellHost host
          then widgetContentInset host fm
          else labelContentInset host fm
      innerX = x + ix
      innerW = max 0 (w - 2 * ix)
      labelH = layoutLineHeight host fm
      hexH = layoutLineHeight host fm
      hueH = colorPickerHueH
      svY = y + iy + labelH + colorPickerGap
      svH = colorPickerSvH
      hueY = svY + svH + colorPickerGap
      previewLabelY = hueY + hueH + colorPickerGap
      swatchY = previewLabelY + hexH + colorPickerSwatchGap
      swatchW = max 0 ((innerW - colorPickerGap) / 2)
      hexY = swatchY + colorPickerSwatchH + colorPickerHexGap
   in ColorPickerGeom
        { cpgLabelH = labelH
        , cpgSv = Rect innerX svY innerW svH
        , cpgHue = Rect innerX hueY innerW hueH
        , cpgCurrent = Rect innerX swatchY swatchW colorPickerSwatchH
        , cpgNew = Rect (innerX + swatchW + colorPickerGap) swatchY swatchW colorPickerSwatchH
        , cpgPreviewLabelY = previewLabelY
        , cpgHexY = hexY
        , cpgHexH = hexH
        }

colorPickerMeasureSize :: HostProfile -> FontMetrics -> (Text -> IO (Float, Float)) -> Text -> IO (Float, Float, Float)
colorPickerMeasureSize host fm measure lbl =
  if isCellHost host
    then do
      (mw, mh) <- measure (lbl <> ": #000000")
      pure (mw, mh, 0)
    else do
      (lw, lh) <- measure lbl
      (hw, _) <- measure (colorPickerToHex colorPickerDefaultColor)
      (cw, _) <- measure colorPickerCurrentLabel
      (nw, _) <- measure colorPickerNewLabel
      let previewW = cw + colorPickerGap + nw
          contentW = max colorPickerMinWidth (max lw (max hw previewW))
      pure (contentW, lh, colorPickerExtraH (layoutLineHeight host fm))

-- Clamp each axis on its own so a corner is S/V 0 or 1, not a frozen mid value.
svFromMouse :: Rect -> V2 -> (Float, Float)
svFromMouse rect (V2 mx my) =
  ( clamp01 ((mx - rectX rect) / max (rectW rect) 1)
  , clamp01 (1 - (my - rectY rect) / max (rectH rect) 1)
  )

hueFromMouse :: Rect -> V2 -> Float
hueFromMouse rect (V2 mx _) =
  clamp01 ((mx - rectX rect) / max (rectW rect) 1) * 360

-- Past either end: freeze hue. Circling around does not jump to the other end.
hueTrackContainsX :: Rect -> V2 -> Bool
hueTrackContainsX rect (V2 mx _) =
  mx >= rectX rect && mx <= rectX rect + rectW rect

-- Taller than the painted track so the handle is easy to grab.
colorPickerHueHitRect :: Rect -> Rect
colorPickerHueHitRect (Rect x y w h) =
  let pad = 6
   in Rect x (y - pad) w (h + pad * 2)

colorPickerDragSv :: Int
colorPickerDragSv = 1

colorPickerDragHue :: Int
colorPickerDragHue = 2

drawSvField :: DrawArena -> Rect -> Float -> IO ()
drawSvField da rect hue = do
  let white = colorRGBA 255 255 255 255
      hueCol = hsvToRgb hue 1 1
      clear = colorRGBA 0 0 0 0
      black = colorRGBA 0 0 0 255
  -- Horizontal: white to hue. Vertical overlay: fade to black (alpha over).
  pushQuadGradient da rect white hueCol hueCol white
  pushQuadGradient da rect clear clear black black

drawHueBar :: DrawArena -> Rect -> IO ()
drawHueBar da rect =
  let stops = (6 :: Int)
      cellW = rectW rect / fromIntegral stops
      stopCol i = hsvToRgb (360 * fromIntegral i / fromIntegral stops) 1 1
   in mapM_
        ( \i ->
            let cell = Rect (rectX rect + fromIntegral i * cellW) (rectY rect) cellW (rectH rect)
             in pushQuadGradient da cell (stopCol i) (stopCol (i + 1)) (stopCol (i + 1)) (stopCol i)
        )
        [0 .. stops - 1]

drawColorPickerPanel ::
  HostProfile ->
  FontMetrics ->
  DrawArena ->
  WidgetStore ->
  WidgetId ->
  Style ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
drawColorPickerPanel host fm da store wid style x y w h = do
  unless (isCellHost host) $ do
    let geom = colorPickerGeom host fm x y w h
        hue = widgetStoreHue store wid colorPickerDefaultColor
        (sat, val) = widgetStoreSv store wid colorPickerDefaultColor
        sv = cpgSv geom
        hueRect = cpgHue geom
        currentCol = widgetStoreBaseColor store wid colorPickerDefaultColor
        newCol = widgetStoreColor store wid colorPickerDefaultColor
        border = styleBorder style
        marker = 6
        mx = rectX sv + sat * rectW sv
        my = rectY sv + (1 - val) * rectH sv
        hx = rectX hueRect + (hue / 360) * rectW hueRect
        handleW = 6
        handle = Rect (hx - handleW / 2) (rectY hueRect) handleW (rectH hueRect)
        handleInner = Rect (rectX handle + 1) (rectY handle + 1) (handleW - 2) (rectH handle - 2)
    drawSvField da sv hue
    pushRoundedStroke da sv 4 1 border
    drawHueBar da hueRect
    pushRoundedStroke da hueRect 3 1 border
    pushRoundedRect da (cpgCurrent geom) 4 currentCol
    pushRoundedStroke da (cpgCurrent geom) 4 1 border
    pushRoundedRect da (cpgNew geom) 4 newCol
    pushRoundedStroke da (cpgNew geom) 4 1 border
    pushRoundedRect da (Rect (mx - marker / 2) (my - marker / 2) marker marker) (marker / 2) (colorRGBA 255 255 255 255)
    pushRoundedStroke da (Rect (mx - marker / 2) (my - marker / 2) marker marker) (marker / 2) 1 (colorRGBA 0 0 0 180)
    pushRoundedRect da handleInner 1 (colorRGBA 255 255 255 255)
    pushRoundedStroke da handle 2 1 (colorRGBA 0 0 0 180)

colorPicker :: (Ui :> es) => Text -> Color -> Eff es (Response, Color)
colorPicker lbl initial = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ registerFocusable ctx wid
  let key = intKey wid
  store0 <- uiIO (getStore ctx)
  when (not (IM.member key (storeInt store0))) $
    uiIO $
      let (hInit, sInit, vInit) = rgbToHsv initial
          packed = fromIntegral (colorToWord32 initial)
       in setStore
            ctx
            ( store0
                { storeInt =
                    IM.insert (slotKey slotColorBase key) packed $
                      IM.insert key packed (storeInt store0)
                , storeFloat = IM.insert key (clampHue hInit) (storeFloat store0)
                , storePoint = IM.insert key (sInit, vInit) (storePoint store0)
                }
            )
  resp <- addWidget wid NodeColorPicker (colorPickerLabelText lbl) 0 (fillW defaultLayout)
  store <- uiIO (getStore ctx)
  let current = widgetStoreColor store wid initial
      host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
  active <- uiIO (readIORef (ctxActiveId ctx))
  mrect <- uiIO (scrollHitRect ctx wid)
  blocked <- uiIO (readIORef (ctxLastPointerBlocked ctx))
  gesture <- uiIO (menuPointerGestureActive ctx)
  hueHeld0 <- keyedDragHeld ("hue" :: Text)
  svHeld0 <- do
    s <- keyedDragHeld ("s" :: Text)
    v <- keyedDragHeld ("v" :: Text)
    pure (s || v)
  let geom = maybe (colorPickerGeom host fm 0 0 0 0) (\(Rect x y w h) -> colorPickerGeom host fm x y w h) mrect
      empty = Rect 0 0 0 0
      isActive = active == wid
      heldByOther =
        inputMouseDown inp
          && not (inputMousePressed inp)
          && hashWidgetId active /= 0
          && not isActive
      svRect =
        if blocked || heldByOther || hueHeld0 || gesture
          then empty
          else cpgSv geom
      hueRect =
        if blocked || heldByOther || svHeld0 || gesture
          then empty
          else colorPickerHueHitRect (cpgHue geom)
      h0 = widgetStoreHue store wid initial
      (s0, v0) = widgetStoreSv store wid initial
  (sDrag, sA) <- withKey ("s" :: Text) (useDrag1D DragAxisX 0 1 s0 svRect)
  (vDrag, vA) <- withKey ("v" :: Text) (useDrag1D DragAxisY 1 0 v0 svRect)
  let svA = sA || vA
  (hDrag, hA) <-
    withKey ("hue" :: Text) (useDrag1D DragAxisX 0 360 h0 (if svA then empty else hueRect))
  let dragging = svA || hA
      nextHue = if hA then hDrag else h0
      nextS = if sA then sDrag else s0
      nextV = if vA then vDrag else v0
      dragged = hsvToRgb nextHue nextS nextV
  when (dragging && not isActive) $ uiIO $ writeIORef (ctxActiveId ctx) wid
  when ((not dragging || blocked) && isActive) $ uiIO $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  when (dragging && (dragged /= current || nextHue /= h0 || nextS /= s0 || nextV /= v0)) $
    uiIO $ do
      st <- getStore ctx
      setStore
        ctx
        ( st
            { storeInt = IM.insert key (fromIntegral (colorToWord32 dragged)) (storeInt st)
            , storeFloat = IM.insert key nextHue (storeFloat st)
            , storePoint = IM.insert key (nextS, nextV) (storePoint st)
            }
        )
  nav <- useKeyNav wid
  let keyMoved = knLeft nav || knRight nav || knUp nav || knDown nav
  when keyMoved $
    uiIO $ applyColorPickerKeys ctx wid current nav
  store1 <- uiIO (getStore ctx)
  let final = widgetStoreColor store1 wid initial
      releasedDrag = (hueHeld0 || svHeld0) && not dragging
  when (releasedDrag || keyMoved) $
    uiIO $ commitColorPickerCurrent ctx wid final
  pure (setChanged (final /= initial) resp, final)

commitColorPickerCurrent :: Context -> WidgetId -> Color -> IO ()
commitColorPickerCurrent ctx wid col = do
  st <- getStore ctx
  let packed = fromIntegral (colorToWord32 col)
      k = slotKey slotColorBase (intKey wid)
      old = IM.findWithDefault packed k (storeInt st)
  when (old /= packed) $
    setStore ctx (st {storeInt = IM.insert k packed (storeInt st)})

applyColorPickerKeys :: Context -> WidgetId -> Color -> KeyNav -> IO ()
applyColorPickerKeys ctx wid current nav = do
  store <- getStore ctx
  let h = widgetStoreHue store wid current
      (s, v) = widgetStoreSv store wid current
      stepHue = if knLeft nav then -6 else if knRight nav then 6 else 0
      stepVal = if knUp nav then 0.05 else if knDown nav then -0.05 else 0
      nextHue = clampHue (if stepHue /= 0 then h + stepHue else h)
      nextV = clamp01 (v + stepVal)
      next = hsvToRgb nextHue s nextV
  when (next /= current || nextHue /= h || nextV /= v) $
    setStore
      ctx
      ( store
          { storeInt = IM.insert (intKey wid) (fromIntegral (colorToWord32 next)) (storeInt store)
          , storeFloat = IM.insert (intKey wid) nextHue (storeFloat store)
          , storePoint = IM.insert (intKey wid) (s, nextV) (storePoint store)
          }
      )
