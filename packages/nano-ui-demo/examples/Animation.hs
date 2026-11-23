-- | Animation: tweens, springs, animated colors, hover effects, and a looping indicator.
--
-- An animation returns this frame's interpolated value, which you use just
-- like any other number in your view. 'animateTo' moves toward a target:
-- keeping the target constant continues the running transition, while giving
-- it a new target redirects from the current value so flipping a toggle
-- mid-flight reverses smoothly. A 'Tween' follows an 'Ease' curve over a
-- fixed duration; a 'Spring' preserves velocity on retargeting and can
-- overshoot ('presetBouncy') or settle cleanly ('presetStiff'). 'animateToA'
-- animates every channel of an 'Animatable' value such as 'Color' or 'V2'.
--
-- Animations consume a widget ID like widgets, so call them in consistent
-- order on every frame. Wrapping each one in 'withKey' ties its state to a
-- stable key rather than position, so adding widgets above will not restart it.
--
-- Idle cost: frames only run while something is moving. Tweens and springs
-- request frames automatically until they settle, after which the loop sleeps
-- until the next input. 'pulse' reads the clock without waking the loop on its
-- own, so call 'keepAnimating' on every frame the indicator is visible.
-- Uncheck "Run" and the app goes back to sleep. See Theming.hs for theme colors.
--
-- Run with @cabal run nano-ui-example-animation@.
module Main (main) where

import Control.Monad (when)
import NanoUI
import NanoUI.Backend.Sdl (SdlOptions (..), defaultSdlOptions, runSdlApp)
import qualified Data.Text as T

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Animation", wsSize = Size 720 640}
      }
    view

-- | 'withKey' at one key type, so the string literals need no annotation.
keyed :: T.Text -> NanoUI a -> NanoUI a
keyed = withKey

view :: NanoUI ()
view = do
  open <- useVar False
  right <- useVar False
  warm <- useVar False
  running <- useVar False
  theme <- uiTheme
  scrollWith (padAll 20 . grow) $
    columnWith (tight . gap 18 . fillW) $ do
      -- 1. Expand and collapse: a tween from 0 to 1 scales the panel's
      -- height and fades its text in. The panel clips what does not fit yet.
      checkbox "Show details" open
      isOpen <- readVar open
      openT <- keyed "details" (animateTo (Tween EaseOutCubic 0.3 0) (if isOpen then 1 else 0))
      -- The panel is only declared while openT > 0.005, so 'scope' keeps
      -- sibling IDs below it stable whether or not it appears.
      scope . when (openT > 0.005) $
        panelWith (fillW . fixedH (96 * openT) . padXY 12 10 . gap 6) $ do
          let ink = withAlpha (styleFg (themePanel theme)) openT
          labelWith (tight . fontColor ink . fontMedium) "Details"
          labelWith (tight . fontColor ink) "Height and text opacity both follow the same tween."
          labelWith (tight . fontColor ink) "Uncheck the box mid-way to see it reverse smoothly."

      -- 2. Springs: same target, two feels. Click "Move" rapidly to watch
      -- velocity carry smoothly through a retarget.
      rowWith (tight . gap 12 . alignMid) $ do
        whenM (button "Move") (modifyVar right not)
        muted "Bouncy overshoots; stiff settles cleanly."
      isRight <- readVar right
      bouncy <- keyed "bouncy" (animateTo (Spring presetBouncy) (if isRight then 1 else 0))
      stiff <- keyed "stiff" (animateTo (Spring presetStiff) (if isRight then 1 else 0))
      rail "Bouncy" bouncy
      rail "Stiff" stiff

      -- 3. Animated color: 'animateToA' interpolates each RGBA channel.
      -- Animating one Float and blending with 'lerpColor' works too.
      isWarm <- readVar warm
      swatch <- keyed "swatch" (animateToA (Tween EaseInOutCubic 0.6 0) (if isWarm then themeOrange theme else themeAccent theme))
      rowWith (tight . gap 12 . alignMid) $ do
        whenM (button (if isWarm then "Cool down" else "Warm up")) (modifyVar warm not)
        box (fixedWH 120 36) swatch

      -- 4. Hover: the button's response says whether the pointer is over
      -- it, and a short tween turns that into a bar that grows beneath it.
      columnWith (tight . gap 4) $ do
        resp <- button' "Hover me"
        hoverT <- keyed "hover" (animateTo (Tween EaseOutQuad 0.15 0) (if respHovered resp then 1 else 0))
        box (fixedWH (8 + 112 * hoverT) 3) (themeAccent theme)

      -- 5. A loop that runs only while asked to. 'pulse' sweeps 0..1 from
      -- the clock; 'keepAnimating' on the bar keeps the frames coming, and
      -- leaving the scope out (Run off) lets the loop sleep again.
      checkbox "Run" running
      isRunning <- readVar running
      scope . when isRunning $
        rowWith (tight . gap 12 . alignMid . fillW) $ do
          p <- pulse 1.2
          box (fixedWH 14 14) (withAlpha (themeAccent theme) (0.25 + 0.75 * p))
          bar <- progressBarWith' id 8 p
          keepAnimating bar

-- | A knob on a track at fraction @t@ of the way along. The slack on either
-- side leaves room for a bouncy spring to overshoot past 0 and 1.
rail :: T.Text -> Float -> NanoUI ()
rail name t =
  keyed name $
    rowWith (tight . gap 12 . alignMid) $ do
      labelWith (tight . fixedW 56) name
      panelWith (fixedWH (travel + 2 * slack + knobSize) (knobSize + 8) . padXY 0 4) $
        rowWith (tight . alignMid) $ do
          spacer (Fixed (max 0 (slack + t * travel))) Fit
          box (fixedWH knobSize knobSize) (colorRGB 214 51 132)
  where
    travel = 300
    slack = 60
    knobSize = 20
