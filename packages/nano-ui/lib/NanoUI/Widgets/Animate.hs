module NanoUI.Widgets.Animate
  ( animate
  , animateEase
  , animateEaseDelay
  , animateTo
  , animateToEase
  , animateToEaseDelay
  , animateToSpring
  , animateToA
  , animateToSpringA
  , pulse
  , keepAnimating
  )
where

import Effectful (Eff, type (:>))
import NanoUI.Animatable (Animatable (..))
import NanoUI.Animation (SpringParams)
import NanoUI.Context
  ( Ease (..)
  , approxEq
  , easeSameSpec
  , getAnimationValue
  , lookupAnimation
  , startAnimation
  , startAnimationEaseDelay
  , startSpring
  )
import NanoUI.Monad (Ui, askContext, nextId, scope, uiIO, uiTime, withKey)
import NanoUI.Widgets.Node (Response (..), respId)

animate :: Ui :> es => Float -> Float -> Float -> Eff es Float
animate = animateEase EaseLinear

animateEase :: Ui :> es => Ease -> Float -> Float -> Float -> Eff es Float
animateEase ease from to dur = animateEaseDelay ease from to dur 0

animateEaseDelay ::
  Ui :> es => Ease -> Float -> Float -> Float -> Float -> Eff es Float
animateEaseDelay ease from to dur delay = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    startAnimationEaseDelay ctx wid from to dur ease delay
    getAnimationValue ctx wid

animateTo :: Ui :> es => Float -> Float -> Eff es Float
animateTo = animateToEase EaseLinear

animateToEase :: Ui :> es => Ease -> Float -> Float -> Eff es Float
animateToEase ease target dur = animateToEaseDelay ease target dur 0

animateToEaseDelay ::
  Ui :> es => Ease -> Float -> Float -> Float -> Eff es Float
animateToEaseDelay ease target dur delay = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    cur <- getAnimationValue ctx wid
    manim <- lookupAnimation ctx wid
    case manim of
      Just a | easeSameSpec a ease dur delay target -> pure cur
      Nothing | approxEq cur target -> pure cur
      _ -> do
        startAnimationEaseDelay ctx wid cur target dur ease delay
        getAnimationValue ctx wid

animateToSpring :: Ui :> es => SpringParams -> Float -> Eff es Float
animateToSpring params target = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    startSpring ctx wid params target
    getAnimationValue ctx wid

animateToA :: (Animatable a, Ui :> es) => Ease -> Float -> a -> Eff es a
animateToA ease dur = animateComponents (\value -> animateToEase ease value dur)

animateToSpringA :: (Animatable a, Ui :> es) => SpringParams -> a -> Eff es a
animateToSpringA params = animateComponents (animateToSpring params)

-- Component keys are local to one composite value, not its parent widget.
animateComponents ::
  (Animatable a, Ui :> es) => (Float -> Eff es Float) -> a -> Eff es a
animateComponents animateComponent target = scope $ do
  components <-
    mapM
      (\(index, value) -> withKey (index :: Int) (animateComponent value))
      (zip [0 ..] (toComponents target))
  pure (fromComponents components)

-- | A smoothly oscillating value in @[0,1]@ driven by the real-time clock, with
-- the given period in seconds (e.g. @pulse 6@ sweeps once every six seconds).
-- The time is captured in 'Double' (see 'NanoUI.Monad.uiTime'), so the sweep
-- stays sub-frame smooth even on long-running processes. The value is
-- re-evaluated each frame, like 'animate'.
pulse :: Ui :> es => Float -> Eff es Float
pulse periodSec = do
  t <- uiTime
  let
    period = max 0.001 (realToFrac periodSec :: Double)
  pure (realToFrac (0.5 + 0.5 * sin (2 * pi * t / period)) :: Float)

-- | Keep a widget's response animating indefinitely so the frame loop never
-- idles. Some widgets are driven by the wall clock (see 'pulse', or draw from
-- 'NanoUI.Monad.uiTime' directly) rather than by a frame-counted animation; a
-- page containing only those would otherwise go idle after animating settles
-- and stop repainting. Returns the response unchanged for composition, e.g.
--
-- > progResp <- progressBar =<< pulse 6
-- > keepAnimating progResp
keepAnimating :: Ui :> es => Response -> Eff es Response
keepAnimating resp = do
  ctx <- askContext
  uiIO (startAnimation ctx (respId resp) 0 1 1e9)
  pure resp
