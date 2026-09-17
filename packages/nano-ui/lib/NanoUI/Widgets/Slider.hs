-- | Horizontal slider control.
module NanoUI.Widgets.Slider
  ( slider
  , slider'
  , sliderWith
  , sliderWith'
  )
where

import Control.Monad (when)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , adoptStoreFloat
  , intKey
  , recordStoreFloat
  , registerFocusable
  , writeStoreFloat
  , getsOverlay
  , OverlayState (..)
  )
import NanoUI.Font (sliderHandleSlack, sliderTrackBounds)
import NanoUI.Frame.Hit (scrollHitRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (inputMouseDown, inputMousePressed)
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Style (Layout, defaultLayout, fillW)
import NanoUI.Types (Rect (..), clamp)
import NanoUI.Widgets.Behavior (DragAxis (..), KeyNav (..), useDrag1D, useKeyNav)
import NanoUI.Widgets.Node (Response, addWidget, setChanged)

-- | Slider over @[minV, maxV]@ that fills the available width. Pass the
-- current value; the result is the value after this frame's drag or arrow
-- keys.
{-# INLINE slider #-}
slider :: Ui :> es => Float -> Float -> Float -> Eff es Float
slider minV maxV value = snd <$> sliderWith' id minV maxV value

{-# INLINE slider' #-}
slider' :: Ui :> es => Float -> Float -> Float -> Eff es (Response, Float)
slider' = sliderWith' id

-- | 'slider' with a layout modifier.
--
-- @
-- volume' <- sliderWith (fixedW 200) 0 100 volume
-- @
{-# INLINE sliderWith #-}
sliderWith :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Float -> Eff es Float
sliderWith f minV maxV value = snd <$> sliderWith' f minV maxV value

sliderWith' ::
  Ui :> es =>
  (Layout -> Layout) -> Float -> Float -> Float -> Eff es (Response, Float)
sliderWith' f minV maxV value = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO $ registerFocusable ctx wid
  let key = intKey wid
  current <- uiIO $ adoptStoreFloat ctx wid key value
  let
    frac = if maxV > minV then (current - minV) / (maxV - minV) else 0
  resp <- addWidget wid NodeSlider "" frac (f (fillW defaultLayout))
  active <- uiIO (readIORef (ctxActiveId ctx))
  blocked <- uiIO (getsOverlay ctx osLastPointerBlocked)
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
          let tr = sliderTrackBounds x y w h
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
  uiIO $ do
    writeStoreFloat ctx wid key finalVal
    recordStoreFloat ctx key finalVal
  pure (setChanged (finalVal /= current) resp, finalVal)
