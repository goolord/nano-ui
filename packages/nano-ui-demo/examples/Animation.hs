-- | Animation: tweens, springs, animated colours, hover effects and a looping indicator.
--
-- An animation hook returns this frame's value; you build the view from it
-- like any other number. 'animateTo' moves toward a target: an unchanged
-- target keeps the running animation, a new one retargets from wherever the
-- value is now, so flipping a toggle mid-flight reverses smoothly. A 'Tween'
-- follows an 'Ease' curve over a fixed time; a 'Spring' carries its velocity
-- into a retarget and may overshoot ('presetBouncy') or not ('presetStiff').
-- 'animateToA' animates every channel of an 'Animatable' value, such as a
-- 'Color' or a 'V2'.
--
-- Animation hooks take a widget id like any widget, so they must run on
-- every frame in the same order. Each one here runs under 'withKey': its
-- state then follows the key rather than its position, and a widget added
-- above it later does not restart it.
--
-- Idle cost: the loop draws frames only while something moves. A tween or
-- spring asks for frames until it lands, then the loop sleeps until the
-- next input. 'pulse' is different: it reads the clock and asks for nothing,
-- so 'keepAnimating' keeps frames coming, and only on frames that call it.
-- Untick "Run" and the app goes back to sleep. See Theming.hs for colours
-- taken from the theme.
--
-- Run it with @cabal run nano-ui-example-animation@.
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
  (open, setOpen) <- useFlag False
  (right, setRight) <- useFlag False
  (warm, setWarm) <- useFlag False
  (running, setRunning) <- useFlag False
  theme <- uiTheme
  scrollWith (padAll 20 . grow) $
    columnWith (tight . gap 18 . fillW) $ do
      -- 1. Expand and collapse: a tween from 0 to 1 scales the panel's
      -- height and fades its text in. The panel clips what does not fit yet.
      openT <- keyed "details" (animateTo (Tween EaseOutCubic 0.3 0) (if open then 1 else 0))
      setOpen =<< checkbox "Show details" open
      -- The panel exists only while any of it shows, so 'scope' keeps the
      -- ids after it stable whether or not it is declared.
      scope . when (openT > 0.005) $
        panelWith (fillW . fixedH (96 * openT) . padXY 12 10 . gap 6) $ do
          let ink = withAlpha (styleFg (themePanel theme)) openT
          labelWith (tight . fontColor ink . fontMedium) "Details"
          labelWith (tight . fontColor ink) "The height and the text alpha follow one tween."
          labelWith (tight . fontColor ink) "Untick the box mid-way: it reverses from where it is."

      -- 2. Springs: the same target, two feels. Click "Move" quickly a few
      -- times: a spring keeps its speed when retargeted.
      bouncy <- keyed "bouncy" (animateTo (Spring presetBouncy) (if right then 1 else 0))
      stiff <- keyed "stiff" (animateTo (Spring presetStiff) (if right then 1 else 0))
      rowWith (tight . gap 12 . alignMid) $ do
        whenM (button "Move") (setRight (not right))
        muted "Bouncy overshoots; stiff settles without."
      rail "Bouncy" bouncy
      rail "Stiff" stiff

      -- 3. A colour: 'animateToA' tweens each RGBA channel. To blend between
      -- two fixed colours, animating one Float and using 'lerpColor' works too.
      swatch <- keyed "swatch" (animateToA (Tween EaseInOutCubic 0.6 0) (if warm then themeOrange theme else themeAccent theme))
      rowWith (tight . gap 12 . alignMid) $ do
        whenM (button (if warm then "Cool down" else "Warm up")) (setWarm (not warm))
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
      setRunning =<< checkbox "Run" running
      scope . when running $
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
