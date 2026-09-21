-- | Horizontal slider control.
module NanoUI.Internal.Widgets.Slider
  ( slider
  , slider'
  , sliderWith
  , sliderWith'
  )
where

import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( adoptSlot
  , intKey
  , registerFocusable
  )
import NanoUI.Internal.Font (sliderHandleSlack, sliderTrackBounds)
import NanoUI.Internal.Frame.Hit (scrollHitRect)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, askContext, nextId, uiIO, withKey)
import NanoUI.Internal.Style (Layout, defaultLayout, fillW)
import NanoUI.Internal.Store (fieldFloat)
import NanoUI.Internal.Types (Rect (..), clamp)
import NanoUI.Internal.Widgets.Behavior (DragAxis (..), holdActiveWhile, navStep, useDrag1D, useKeyNav)
import NanoUI.Internal.Widgets.Combinators (finishInput)
import NanoUI.Internal.Widgets.Node (Response, addWidget)

-- | Slider over @[minV, maxV]@ that fills the available width. Pass the
-- current value; the result is the value after this frame's drag or arrow
-- keys.
{-# INLINE slider #-}
slider :: Ui :> es => Float -> Float -> Float -> Eff es Float
slider minV maxV value = snd <$> sliderWith' id minV maxV value

{-# INLINE slider' #-}
-- | 'slider' returning @(response, updatedValue)@ for the supplied bounds and value.
slider' :: Ui :> es => Float -> Float -> Float -> Eff es (Response, Float)
slider' = sliderWith' id

-- | 'slider' with a layout modifier.
--
-- > volume' <- sliderWith (fixedW 200) 0 100 volume
{-# INLINE sliderWith #-}
sliderWith :: Ui :> es => (Layout -> Layout) -> Float -> Float -> Float -> Eff es Float
sliderWith f minV maxV value = snd <$> sliderWith' f minV maxV value

-- | 'sliderWith' returning the response and updated value.
sliderWith' ::
  Ui :> es =>
  (Layout -> Layout) -> Float -> Float -> Float -> Eff es (Response, Float)
sliderWith' f minV maxV value = do
  wid <- nextId
  ctx <- askContext
  uiIO $ registerFocusable ctx wid
  let
    key = intKey wid
  current <- uiIO $ adoptSlot fieldFloat ctx wid key value
  let
    frac = if maxV > minV then (current - minV) / (maxV - minV) else 0
  resp <- addWidget wid NodeSlider "" frac (f (fillW defaultLayout))
  mrect <- uiIO (scrollHitRect ctx wid)
  let
    track =
      case mrect of
        Just (Rect x y w h) ->
          let
            tr = sliderTrackBounds x y w h
           in
            Rect
              (rectX tr)
              (rectY tr - sliderHandleSlack)
              (rectW tr)
              (rectH tr + 2 * sliderHandleSlack)
        Nothing -> Rect 0 0 0 0
  (dragged, dragging) <-
    withKey ("drag" :: Text) (useDrag1D DragAxisX minV maxV current track)
  holdActiveWhile wid dragging
  nav <- useKeyNav wid
  let
    range = maxV - minV
    step = if range > 0 then range / 100 else 0
    baseVal = if dragging then dragged else current
    finalVal = clamp minV maxV (baseVal + fromIntegral (navStep nav) * step)
  finishInput fieldFloat ctx wid key current resp finalVal
