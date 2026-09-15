-- | Horizontal slider control.
module NanoUI.Widgets.Slider
  ( slider
  , sliderWith
  , sliderEx
  )
where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context (Context (..), getLastPointerBlocked, getStore, intKey, registerFocusable, writeStoreFloat)
import NanoUI.Font (sliderHandleSlack, sliderTrackBounds)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (inputMouseDown, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Store (WidgetStore (..))
import NanoUI.Style (Layout, defaultLayout, fillW)
import NanoUI.Types (Rect (..), clamp)
import NanoUI.Widgets.Behavior (DragAxis (..), KeyNav (..), useDrag1D, useKeyNav)
import NanoUI.Widgets.Node (Response, addWidget, setChanged)

-- | Slider control with default layout ('fillW'). Returns @(response, currentValue)@.
slider ::
  Ui :> es => Float -> Float -> Float -> Eff es (Response, Float)
slider = sliderEx (fillW defaultLayout)

-- | Slider with a custom layout modifier function.
--
-- Example:
--
-- @
-- (resp, val) <- sliderWith (fixedW 200) 0 100 currentVol
-- @
{-# INLINE sliderWith #-}
sliderWith ::
  Ui :> es =>
  (Layout -> Layout) -> Float -> Float -> Float -> Eff es (Response, Float)
sliderWith f = sliderEx (f defaultLayout)

sliderEx ::
  Ui :> es =>
  Layout -> Float -> Float -> Float -> Eff es (Response, Float)
sliderEx layout minV maxV initial = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ registerFocusable ctx wid
  store <- uiIO (getStore ctx)
  let
    key = intKey wid
    current = IM.findWithDefault initial key (storeFloat store)
    frac = if maxV > minV then (current - minV) / (maxV - minV) else 0
    fm = ctxFontMetrics ctx
  resp <- addWidget wid NodeSlider "" frac layout
  active <- uiIO (readIORef (ctxActiveId ctx))
  blocked <- uiIO (getLastPointerBlocked ctx)
  mrect <- uiIO (scrollHitRect ctx wid)
  let
    isActive = active == wid
    heldByOther =
      inputMouseDown inp
        && not (inputMousePressed inp)
        && hashWidgetId active /= 0
        && not isActive
    track0 =
      case mrect of
        Just (Rect x y w h) ->
          let tr = sliderTrackBounds fm x y w h
           in Rect (rectX tr) (rectY tr - sliderHandleSlack) (rectW tr) (rectH tr + 2 * sliderHandleSlack)
        Nothing -> Rect 0 0 0 0
    track = if blocked || heldByOther then Rect 0 0 0 0 else track0
  (dragged, dragging) <- withKey ("drag" :: Text) (useDrag1D DragAxisX minV maxV current track)
  when (dragging && not isActive) $ uiIO $ writeIORef (ctxActiveId ctx) wid
  when ((not dragging || blocked) && isActive) $
    uiIO $ writeIORef (ctxActiveId ctx) (WidgetId 0)
  nav <- useKeyNav wid
  let
    range = maxV - minV
    step = if range > 0 then range / 100 else 0
    navStep =
      (if knRight nav || knUp nav then 1 else 0 :: Int)
        - (if knLeft nav || knDown nav then 1 else 0)
    baseVal = if dragging then dragged else current
    finalVal = clamp minV maxV (baseVal + fromIntegral navStep * step)
  when (finalVal /= current) $
    uiIO $ writeStoreFloat ctx wid key finalVal
  pure (setChanged (finalVal /= current) resp, finalVal)
