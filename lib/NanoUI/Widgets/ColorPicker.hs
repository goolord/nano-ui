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
  , colorPickerDragHue
  , colorPickerDragSv
  , colorPickerGeom
  , colorPickerHueHitRect
  , clampHue
  , hueFromMouse
  , svFromMouse
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
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , foldInputKeys
  , inputKeys
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  )
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO)
import NanoUI.Style (defaultLayout, fillW)
import NanoUI.Types (Color (..), Rect (..), clamp01, colorToWord32, hsvToRgb, rectContains, rgbToHsv)
import NanoUI.WidgetText (colorPickerLabelText)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Widgets.Node (Response (..), addWidget, setChanged)

colorPicker :: (Ui :> es) => Text -> Color -> Eff es (Response, Color)
colorPicker lbl initial = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ registerFocusable ctx wid
  let key = intKey wid
  store0 <- uiIO (getStore ctx)
  when (not (IM.member key (storeColor store0))) $
    uiIO $
      let (hInit, sInit, vInit) = rgbToHsv initial
       in setStore
            ctx
            ( store0
                { storeColor = IM.insert key (colorToWord32 initial) (storeColor store0)
                , storeColorHue = IM.insert key (clampHue hInit) (storeColorHue store0)
                , storeColorSv = IM.insert key (sInit, vInit) (storeColorSv store0)
                }
            )
  let nodeText = colorPickerLabelText lbl
  resp <- addWidget wid NodeColorPicker nodeText 0 (fillW defaultLayout)
  store <- uiIO (getStore ctx)
  let current = widgetStoreColor store wid initial
      host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
      drag0 = IM.findWithDefault 0 key (storeColorDrag store)
  active <- uiIO (readIORef (ctxActiveId ctx))
  focus <- uiIO (readIORef (ctxFocusId ctx))
  mrect <- uiIO (scrollHitRect ctx wid)
  blocked <- uiIO (readIORef (ctxLastPointerBlocked ctx))
  let mouse = inputMousePos inp
      geom =
        case mrect of
          Just (Rect x y w h) -> colorPickerGeom host fm x y w h
          Nothing -> colorPickerGeom host fm 0 0 0 0
      sv = cpgSv geom
      hueRect = cpgHue geom
      svHit = not blocked && rectContains sv mouse
      hueHit = not blocked && rectContains (colorPickerHueHitRect hueRect) mouse
      isActive = active == wid
      heldByOther =
        inputMouseDown inp
          && not (inputMousePressed inp)
          && hashWidgetId active /= 0
          && not isActive
      down = inputMouseDown inp
      drag =
        if not down || blocked
          then 0
          else
            if drag0 /= 0
              then drag0
              else
                if heldByOther
                  then 0
                  else
                    if svHit
                      then colorPickerDragSv
                      else if hueHit then colorPickerDragHue else 0
      h0 = widgetStoreHue store wid initial
      (s0, v0) = widgetStoreSv store wid initial
      (draggedHue, draggedS, draggedV) =
        if drag == colorPickerDragSv
          then
            let (s, v) = svFromMouse sv mouse
             in (h0, s, v)
          else
            if drag == colorPickerDragHue
              then (hueFromMouse hueRect mouse, s0, v0)
              else (h0, s0, v0)
      dragged = hsvToRgb draggedHue draggedS draggedV
  when (down && drag /= 0 && not isActive) $
    uiIO $ writeIORef (ctxActiveId ctx) wid
  when ((not down || blocked) && isActive) $
    uiIO $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  when (drag /= drag0) $
    uiIO $ do
      st <- getStore ctx
      setStore
        ctx
        ( st
            { storeColorDrag =
                if drag == 0
                  then IM.delete key (storeColorDrag st)
                  else IM.insert key drag (storeColorDrag st)
            }
        )
  when (dragged /= current || draggedHue /= h0 || draggedS /= s0 || draggedV /= v0) $
    uiIO $ do
      st <- getStore ctx
      setStore
        ctx
        ( st
            { storeColor = IM.insert key (colorToWord32 dragged) (storeColor st)
            , storeColorHue = IM.insert key draggedHue (storeColorHue st)
            , storeColorSv = IM.insert key (draggedS, draggedV) (storeColorSv st)
            }
        )
  let next = if drag /= 0 then dragged else current
  when (focus == wid) $
    uiIO $ applyColorPickerKeys ctx wid next inp
  store1 <- uiIO (getStore ctx)
  let final = widgetStoreColor store1 wid initial
  pure (setChanged (final /= initial) resp, final)

applyColorPickerKeys :: Context -> WidgetId -> Color -> Input -> IO ()
applyColorPickerKeys ctx wid current inp =
  let keys = inputKeys inp
      (wantLeft, wantRight, wantUp, wantDown) =
        foldInputKeys
          ( \(l, r, u, d) k ->
              ( l || k == KeyLeft
              , r || k == KeyRight
              , u || k == KeyUp
              , d || k == KeyDown
              )
          )
          (False, False, False, False)
          keys
   in when (wantLeft || wantRight || wantUp || wantDown) $ do
        store <- getStore ctx
        let h = widgetStoreHue store wid current
            (s, v) = widgetStoreSv store wid current
            stepHue = if wantLeft then -6 else if wantRight then 6 else 0
            stepVal = if wantUp then 0.05 else if wantDown then -0.05 else 0
            nextHue = clampHue (if stepHue /= 0 then h + stepHue else h)
            nextV = clamp01 (v + stepVal)
            next = hsvToRgb nextHue s nextV
        when (next /= current || nextHue /= h || nextV /= v) $
          setStore
            ctx
            ( store
                { storeColor = IM.insert (intKey wid) (colorToWord32 next) (storeColor store)
                , storeColorHue = IM.insert (intKey wid) nextHue (storeColorHue store)
                , storeColorSv = IM.insert (intKey wid) (s, nextV) (storeColorSv store)
                }
            )
