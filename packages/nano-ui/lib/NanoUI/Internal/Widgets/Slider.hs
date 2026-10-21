-- | Horizontal slider control.
module NanoUI.Internal.Widgets.Slider
  ( slider
  , slider'
  , sliderWith
  , sliderWith'
  )
where

import Control.Monad (void, when)
import NanoUI.Internal.Context
import NanoUI.Internal.Equality (sameFloat)
import NanoUI.Internal.Font (sliderHitBounds)
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Internal.Monad (NanoUI, freshWidget, scope, liftIO)
import NanoUI.Internal.Style (Layout, defaultLayout, fillW)
import NanoUI.Internal.Store (fieldFloat)
import NanoUI.Internal.Types (Rect (..), clamp)
import NanoUI.Internal.Var (Binding (..), bindVarBy)
import NanoUI.Internal.Var.Types (Var)
import NanoUI.Internal.Widgets.Behavior (DragAxis (..), holdActiveWhile, navStep, useDrag1D, useKeyNav)
import NanoUI.Internal.Widgets.Combinators (finishInput)
import NanoUI.Internal.Widgets.Node (Response, addWidget, setWidgetValue)

-- | Slider over @[minV, maxV]@ that fills the available width, bound to
-- mutable state ('Var'): a drag or the arrow keys update it in place.
--
-- > slider 0 100 app.volume
{-# INLINE slider #-}
slider :: Float -> Float -> Var Float -> NanoUI ()
slider minV maxV v = void (sliderWith' id minV maxV v)

-- | 'slider' returning its 'Response'.
{-# INLINE slider' #-}
slider' :: Float -> Float -> Var Float -> NanoUI Response
slider' = sliderWith' id

-- | 'slider' with a layout modifier.
--
-- > sliderWith (fixedW 200) 0 100 volume
{-# INLINE sliderWith #-}
sliderWith :: (Layout -> Layout) -> Float -> Float -> Var Float -> NanoUI ()
sliderWith f minV maxV v = void (sliderWith' f minV maxV v)

-- | 'sliderWith' returning its 'Response'.
{-# INLINE sliderWith' #-}
sliderWith' :: (Layout -> Layout) -> Float -> Float -> Var Float -> NanoUI Response
sliderWith' f minV maxV v = bindVarBy sameFloat RenderFinal v (sliderCore f minV maxV)

-- | 'sliderWith' on a plain value: the response and the value after this
-- frame's drag or arrow keys.
sliderCore ::
  (Layout -> Layout) -> Float -> Float -> Float -> NanoUI (Response, Float)
sliderCore f minV maxV value
  -- A NaN bound gives way to 0 or the other bound, and a reversed range
  -- reads low to high, so neither leaves the value NaN or pinned to a bound.
  | isNaN minV = sliderCore f 0 maxV value
  | isNaN maxV = sliderCore f minV minV value
  | minV > maxV = sliderCore f maxV minV value
sliderCore f minV maxV value = do
  (wid, ctx) <- freshWidget
  liftIO $ registerFocusable ctx wid
  -- Adopt the value the slider can show, so one outside the range (or NaN)
  -- is not adopted afresh, and written back clamped, on every frame.
  let given = if isNaN value then minV else clamp minV maxV value
  current <- liftIO $ adoptSlot fieldFloat ctx wid given
  let
    range = maxV - minV
    fracOf x = if range > 0 then (x - minV) / range else 0
  resp <- addWidget wid NodeSlider "" (fracOf current) (f (fillW defaultLayout))
  mrect <- liftIO (getPrevRect ctx wid)
  let
    track = maybe (Rect 0 0 0 0) (\(Rect x y w h) -> sliderHitBounds x y w h) mrect
  -- Scope the drag to this slider's position so sibling sliders keep separate drag state.
  (dragged, dragging, _) <- scope (useDrag1D DragAxisX wid minV maxV current track)
  holdActiveWhile wid dragging
  nav <- useKeyNav wid
  let
    step = if range > 0 then range / 100 else 0
    finalVal = clamp minV maxV (dragged + fromIntegral (navStep nav) * step)
  -- The node was added before the drag was read: show where it ended, so
  -- this pass draws the value it returns.
  when (finalVal /= current) . liftIO $ setWidgetValue ctx wid (fracOf finalVal)
  finishInput fieldFloat ctx wid current resp finalVal
