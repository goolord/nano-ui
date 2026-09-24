-- | Horizontal slider control.
module NanoUI.Internal.Widgets.Slider
  ( slider
  , slider'
  , sliderWith
  , sliderWith'
  )
where

import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Font (sliderHitBounds)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (Ui, freshWidget, scope, uiIO)
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
  (wid, ctx) <- freshWidget
  uiIO $ registerFocusable ctx wid
  -- Adopt the value the slider can show, so one outside the range (or NaN)
  -- is not adopted afresh, and written back clamped, on every frame.
  let given = if isNaN value then minV else clamp minV maxV value
  current <- uiIO $ adoptSlot fieldFloat ctx wid given
  let
    range = maxV - minV
    frac = if range > 0 then (current - minV) / range else 0
  resp <- addWidget wid NodeSlider "" frac (f (fillW defaultLayout))
  mrect <- uiIO (getPrevRect ctx wid)
  let
    track = maybe (Rect 0 0 0 0) (\(Rect x y w h) -> sliderHitBounds x y w h) mrect
  -- An idle drag hands back the value it was given. Its scope follows this
  -- slider's position, so sibling sliders keep separate drags.
  (dragged, dragging, _) <- scope (useDrag1D DragAxisX minV maxV current track)
  holdActiveWhile wid dragging
  nav <- useKeyNav wid
  let
    step = if range > 0 then range / 100 else 0
    finalVal = clamp minV maxV (dragged + fromIntegral (navStep nav) * step)
  finishInput fieldFloat ctx wid current resp finalVal
