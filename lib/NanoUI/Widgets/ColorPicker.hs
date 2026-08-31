{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Widgets.ColorPicker
  ( colorPicker
  ) where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import qualified Data.IntMap.Strict as IM
import NanoUI.ColorPicker
  ( ColorPickerGeom (..)
  , colorPickerGeom
  , colorPickerHueHitRect
  , clampHue
  , widgetStoreColor
  , widgetStoreHue
  , widgetStoreSv
  )
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getStore
  , intKey
  , registerFocusable
  , setStore
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputMouseDown, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Style (defaultLayout, fillW)
import NanoUI.Types (Color (..), Rect (..), clamp01, colorToWord32, hsvToRgb, rgbToHsv)
import NanoUI.WidgetText (colorPickerLabelText)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Widgets.Behavior (DragAxis (..), KeyNav (..), useDrag1D, useKeyNav)
import NanoUI.Widgets.Node (Response (..), addWidget, setChanged)

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
       in setStore
            ctx
            ( store0
                { storeInt = IM.insert key (fromIntegral (colorToWord32 initial)) (storeInt store0)
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
  let geom = maybe (colorPickerGeom host fm 0 0 0 0) (\(Rect x y w h) -> colorPickerGeom host fm x y w h) mrect
      empty = Rect 0 0 0 0
      isActive = active == wid
      heldByOther =
        inputMouseDown inp
          && not (inputMousePressed inp)
          && hashWidgetId active /= 0
          && not isActive
      svRect = if blocked || heldByOther then empty else cpgSv geom
      hueRect = if blocked || heldByOther then empty else colorPickerHueHitRect (cpgHue geom)
      h0 = widgetStoreHue store wid initial
      (s0, v0) = widgetStoreSv store wid initial
  (sDrag, sA) <- withKey ("s" :: Text) (useDrag1D DragAxisX 0 1 s0 svRect)
  (vDrag, vA) <- withKey ("v" :: Text) (useDrag1D DragAxisY 1 0 v0 svRect)
  let svA = sA || vA
  (hDrag, hA) <- withKey ("hue" :: Text) (useDrag1D DragAxisX 0 360 h0 (if svA then empty else hueRect))
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
  when (knLeft nav || knRight nav || knUp nav || knDown nav) $
    uiIO $ applyColorPickerKeys ctx wid current nav
  store1 <- uiIO (getStore ctx)
  let final = widgetStoreColor store1 wid initial
  pure (setChanged (final /= initial) resp, final)

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
