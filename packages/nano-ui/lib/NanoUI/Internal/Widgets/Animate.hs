-- | View-level tween and spring hooks, continuous animation, and timed frame requests.
module NanoUI.Internal.Widgets.Animate
  ( Transition (..)
  , animate
  , animateTo
  , animateToA
  , pulse
  , keepAnimating
  , wakeAfter
  )
where

import Control.Monad (when)
import Data.Maybe (isNothing)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Animatable (Animatable (..))
import NanoUI.Internal.Animation (SpringParams)
import NanoUI.Internal.Context
  ( Ease (..)
  , approxEq
  , easeSameSpec
  , getAnimationValue
  , lookupAnimation
  , setAnimationValue
  , keepAnimationAlive
  , requestWakeAfter
  , startAnimationEaseDelay
  , startSpring
  )
import NanoUI.Internal.Monad (Ui, askContext, nextId, scope, uiIO, uiTime, withContext, withKey)
import NanoUI.Internal.Widgets.Node (HasResponse, respId)

-- | How an animated value moves.
data Transition
  = -- | Eased tween: duration and start delay, in seconds.
    Tween !Ease !Float !Float
  | -- | Damped spring; retargets from its current position and velocity.
    Spring !SpringParams

-- | Animate from @from@ to @to@. It starts over from @from@ once it has
-- finished (a tween completes, a spring settles) or its tween changes, so
-- calling it every frame cycles.
animate :: Ui :> es => Transition -> Float -> Float -> Eff es Float
animate transition from to = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    case transition of
      Tween ease dur delay -> startAnimationEaseDelay ctx wid from to dur ease delay
      Spring params -> do
        running <- lookupAnimation ctx wid
        when (isNothing running) (setAnimationValue ctx wid from)
        startSpring ctx wid params to
    getAnimationValue ctx wid

-- | Animate from the current value toward @target@. An unchanged target keeps
-- the running animation; a new one retargets from wherever the value is.
animateTo :: Ui :> es => Transition -> Float -> Eff es Float
animateTo transition target = do
  wid <- nextId
  ctx <- askContext
  uiIO $ do
    case transition of
      Tween ease dur delay -> do
        cur <- getAnimationValue ctx wid
        manim <- lookupAnimation ctx wid
        case manim of
          Just a | easeSameSpec a ease dur delay target -> pure ()
          Nothing | approxEq cur target -> pure ()
          _ -> startAnimationEaseDelay ctx wid cur target dur ease delay
      Spring params -> startSpring ctx wid params target
    getAnimationValue ctx wid

-- | 'animateTo' for every component of a composite value.
animateToA :: (Animatable a, Ui :> es) => Transition -> a -> Eff es a
animateToA transition = animateComponents (animateTo transition)

-- Component keys are local to one composite value, not its parent widget.
animateComponents ::
  (Animatable a, Ui :> es) => (Float -> Eff es Float) -> a -> Eff es a
animateComponents animateComponent target =
  scope $
    traverseChannels (\index value -> withKey index (animateComponent value)) target

-- | A smoothly oscillating value in @[0,1]@ driven by the real-time clock, with
-- the given period in seconds (e.g. @pulse 6@ sweeps once every six seconds).
-- The time is captured in 'Double' (see 'NanoUI.Internal.Monad.uiTime'), so the sweep
-- stays sub-frame smooth even on long-running processes. The value is
-- re-evaluated each frame, like 'animate'.
pulse :: Ui :> es => Float -> Eff es Float
pulse periodSec = do
  t <- uiTime
  let
    period = max 0.001 (realToFrac periodSec :: Double)
  pure (realToFrac (0.5 + 0.5 * sin (2 * pi * t / period)) :: Float)

-- | Keep a widget animating for as long as this is called, so the frame loop
-- does not idle. Widgets driven by the wall clock ('pulse', or drawing from
-- 'NanoUI.Internal.Monad.uiTime') rather than by a frame-counted animation would
-- otherwise stop repainting once other animations settle. Call it every
-- frame the widget is shown: the loop goes back to sleep on the first frame
-- that leaves it out.
--
-- > bar <- progressBar' =<< pulse 6
-- > keepAnimating bar
keepAnimating :: (HasResponse r, Ui :> es) => r -> Eff es ()
keepAnimating resp = withContext (\ctx -> keepAnimationAlive ctx (respId resp))

-- | Ask for another frame after this many seconds, even if no input arrives.
-- The loop sleeps until then. Call it on every frame that still needs the
-- later one, as a clock label would to tick once a second:
--
-- > clock = do
-- >   label =<< currentTimeText
-- >   wakeAfter 1
--
-- Use it instead of 'keepAnimating' when a view changes on a schedule and not
-- continuously: 'keepAnimating' runs a frame for every display refresh.
wakeAfter :: Ui :> es => Double -> Eff es ()
wakeAfter sec = withContext (\ctx -> requestWakeAfter ctx sec)
