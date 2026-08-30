{-# LANGUAGE OverloadedStrings #-}

module NanoUI.ColorPicker
  ( ColorPickerGeom (..)
  , colorPickerDefaultColor
  , colorPickerMinWidth
  , colorPickerSvH
  , colorPickerExtraH
  , widgetStoreColor
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
  ) where

import Control.Monad (unless)
import Data.Text (Text)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context (intKey)
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
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.Id (WidgetId (..))
import NanoUI.Store (WidgetStore (..))
import NanoUI.Style (Style (..))
import NanoUI.Types
  ( Color (..)
  , Rect (..)
  , V2 (..)
  , clamp01
  , colorFromWord32
  , colorRGBA
  , colorToWord32
  , hsvToRgb
  , rectH
  , rectW
  , rectX
  , rectY
  , rgbToHsv
  )
import NanoUI.WidgetText (colorPickerToHex)

colorPickerDefaultColor :: Color
colorPickerDefaultColor = colorRGBA 128 128 128 255

colorPickerMinWidth :: Float
colorPickerMinWidth = 180

colorPickerGap :: Float
colorPickerGap = 6

colorPickerHexGap :: Float
colorPickerHexGap = 4

-- Fixed SV height. Hue and hex sit below it and add their own extraH.
colorPickerSvH :: Float
colorPickerSvH = 120

colorPickerHueH :: Float
colorPickerHueH = sliderTrackHeight

data ColorPickerGeom = ColorPickerGeom
  { cpgLabelH :: !Float
  , cpgSv :: !Rect
  , cpgHue :: !Rect
  , cpgHexY :: !Float
  , cpgHexH :: !Float
  }
  deriving (Eq, Show)

widgetStoreColor :: WidgetStore -> WidgetId -> Color -> Color
widgetStoreColor store wid fallback =
  colorFromWord32 (IM.findWithDefault (colorToWord32 fallback) (intKey wid) (storeColor store))

-- RGB cannot tell hue 0 from 360. Keep the slider end the user last set.
widgetStoreHue :: WidgetStore -> WidgetId -> Color -> Float
widgetStoreHue store wid fallback =
  let (h0, _, _) = rgbToHsv (widgetStoreColor store wid fallback)
   in IM.findWithDefault h0 (intKey wid) (storeColorHue store)

-- Black collapses S in RGB. Keep the last mouse S/V so the marker does not jitter.
widgetStoreSv :: WidgetStore -> WidgetId -> Color -> (Float, Float)
widgetStoreSv store wid fallback =
  let (_, s0, v0) = rgbToHsv (widgetStoreColor store wid fallback)
   in IM.findWithDefault (s0, v0) (intKey wid) (storeColorSv store)

-- Hue slider is a line, not a wheel.
clampHue :: Float -> Float
clampHue h = max 0 (min 360 h)

-- extraH: SV + hue track + hex line + gaps. Same pattern as slider trackExtra.
colorPickerExtraH :: Float -> Float
colorPickerExtraH hexH =
  colorPickerGap + colorPickerSvH + colorPickerGap + colorPickerHueH + colorPickerHexGap + hexH

colorPickerGeom :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> ColorPickerGeom
colorPickerGeom host fm x y w h =
  let (ix, iy) =
        if isCellHost host
          then widgetContentInset host fm
          else labelContentInset host fm
      innerX = x + ix
      innerW = max 0 (w - 2 * ix)
      labelH = layoutLineHeight host fm
      hexH = layoutLineHeight host fm
      hueH = colorPickerHueH
      hexY = y + h - iy - hexH
      hueY = hexY - colorPickerHexGap - hueH
      svY = y + iy + labelH + colorPickerGap
      svH = max 0 (hueY - colorPickerGap - svY)
   in ColorPickerGeom
        { cpgLabelH = labelH
        , cpgSv = Rect innerX svY innerW svH
        , cpgHue = Rect innerX hueY innerW hueH
        , cpgHexY = hexY
        , cpgHexH = hexH
        }

colorPickerMeasureSize :: HostProfile -> FontMetrics -> (Text -> IO (Float, Float)) -> Text -> IO (Float, Float, Float)
colorPickerMeasureSize host _fm measure lbl =
  if isCellHost host
    then do
      (mw, mh) <- measure (lbl <> ": #000000")
      pure (mw, mh, 0)
    else do
      (lw, lh) <- measure lbl
      (hw, hh) <- measure (colorPickerToHex colorPickerDefaultColor)
      let contentW = max colorPickerMinWidth (max lw hw)
      pure (contentW, lh, colorPickerExtraH hh)

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
        border = styleBorder style
        marker = 6
        mx = rectX sv + sat * rectW sv
        my = rectY sv + (1 - val) * rectH sv
        hx = rectX hueRect + (hue / 360) * rectW hueRect
        handleW = 6
        handle = Rect (hx - handleW / 2) (rectY hueRect) handleW (rectH hueRect)
    drawSvField da sv hue
    pushRoundedStroke da sv 4 1 border
    drawHueBar da hueRect
    pushRoundedStroke da hueRect 3 1 border
    pushRoundedRect da (Rect (mx - marker / 2) (my - marker / 2) marker marker) (marker / 2) (colorRGBA 255 255 255 255)
    pushRoundedStroke da (Rect (mx - marker / 2) (my - marker / 2) marker marker) (marker / 2) 1 (colorRGBA 0 0 0 180)
    pushRoundedRect da handle 2 (colorRGBA 255 255 255 255)
    pushRoundedStroke da handle 2 1 (colorRGBA 0 0 0 180)
