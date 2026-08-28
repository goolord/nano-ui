{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, void, when)
import NanoUI
import NanoUI.Backend.Sdl (newSdlContext, runSdlAppWithQuit)
import qualified Data.Text as T
import Text.Printf (printf)

main :: IO ()
main = do
  ctx <- newSdlContext
  runSdlAppWithQuit
    (withTheme ctx gateTheme)
    (\inp -> KeyEscape `elem` inputKeys inp)
    animUi

-- Optical printer desk: warm gate, filament shuttle, cyan registration.
gateTheme :: Theme
gateTheme =
  let panelStyle =
        Style
          { styleBg = colorRGBA 44 38 28 255
          , styleFg = colorRGBA 240 230 210 255
          , styleBorder = colorRGBA 92 82 68 255
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 44 38 28 255
          , styleActiveBg = colorRGBA 38 32 24 255
          }
   in defaultTheme
        { themeWindow = colorRGBA 22 19 16 255
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
        , themeButton =
            Style
              { styleBg = colorRGBA 62 54 42 255
              , styleFg = colorRGBA 240 230 210 255
              , styleBorder = colorRGBA 110 96 78 255
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 82 70 52 255
              , styleActiveBg = colorRGBA 48 42 32 255
              }
        , themeInput =
            Style
              { styleBg = colorRGBA 16 14 12 255
              , styleFg = colorRGBA 240 230 210 255
              , styleBorder = colorRGBA 92 82 68 255
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 24 20 16 255
              , styleActiveBg = colorRGBA 12 10 8 255
              }
        , themeSeparator = colorRGBA 110 96 78 255
        , themeAccent = filament
        , themeMuted = colorRGBA 168 152 128 255
        }

filament, registration, leader, gateSteel, safeRed :: Color
filament = colorRGBA 255 106 42 255
registration = colorRGBA 46 196 182 255
leader = colorRGBA 240 230 210 255
gateSteel = colorRGBA 168 148 112 255
safeRed = colorRGBA 148 36 28 255

animUi :: NanoUI ()
animUi = do
  (exposed, setExposed) <- useFlag False
  (rewinding, setRewinding) <- useFlag False
  (lampOn, setLamp) <- useFlag False
  (bellowsOpen, setBellows) <- useFlag False
  lampT <-
    if lampOn
      then withKey ("lamp" :: String) (animateEase EaseInOutCubic 0 1 1.6)
      else pure 0
  wash <-
    if lampOn
      then withKey ("wash" :: String) (animateToEase EaseInOutCubic 1 0.85)
      else pure 0
  bellowsT <-
    if bellowsOpen
      then withKey ("bellows" :: String) (animateToEase EaseOutCubic 1 0.45)
      else pure 0
  clock <-
    if exposed
      then withKey ("clock" :: String) (animateEase EaseLinear 0 1 3.2)
      else
        if rewinding
          then withKey ("clock" :: String) (animateTo 0 0.35)
          else pure 0
  let lampGlow = sin (lampT * pi)
      frames = floor (clock * 128) :: Int
      footage = T.pack (printf "%d+%02d" (frames `div` 16) (frames `mod` 16))
  scroll (tight (grow defaultLayout)) $
    column (padAll 12 . gap 10 . fillW $ defaultLayout) $ do
      panel (padXY 14 10 . gap 8 . fillW $ defaultLayout) $
        toolbar $ do
          column (tight . gap 4 $ defaultLayout) $ do
            heading "Gate"
            muted "Optical printer. Expose loops the pull-down."
          flex
          clickButton "Expose" (setExposed True >> setRewinding False)
          clickButton "Rewind" (setExposed False >> setRewinding True)
          clickButton (if lampOn then "Lamp off" else "Lamp on") (setLamp (not lampOn))
      throwSec <-
        panel (padXY 14 10 . gap 6 . fillW $ defaultLayout) $ do
          (_, throwRaw) <- slider "Throw" 35 140 75
          let sec = throwRaw / 100
          row (tight . gap 16 . wrap . fillW $ defaultLayout) $ do
            kv "Throw" (T.pack (printf "%.2fs" sec))
            kv "Footage" footage
            kv "Lamp" (if lampOn then "print" else "safe")
            kv "Bellows" (if bellowsOpen then "open" else "home")
          pure sec
      let cycleLen = pullCycleLen throwSec
      pullPhase <-
        if exposed
          then withKey ("pulldown" :: String) (animateEase EaseLinear 0 1 cycleLen)
          else pure 0
      let time = pullPhase * cycleLen
      leaderT <-
        if exposed
          then pure (laneT EaseLinear throwSec time)
          else
            if rewinding
              then withKey ("Leader" :: String) (animateToEase EaseLinear 0 throwSec)
              else pure 0
      when (rewinding && not exposed && abs leaderT < 0.001 && abs clock < 0.001) (setRewinding False)
      panel (padXY 14 12 . gap 10 . fillW $ defaultLayout) $ do
        heading "Pull-down"
        muted "All lanes throw together. One second hold at home and at full throw."
        sep
        lane exposed rewinding throwSec time "Leader" EaseLinear leader
        lane exposed rewinding throwSec time "Gate" EaseInCubic gateSteel
        lane exposed rewinding throwSec time "Shuttle" EaseOutCubic filament
        lane exposed rewinding throwSec time "Reg" EaseInOutCubic registration
        lane exposed rewinding throwSec time "Claw" EaseOutBack (colorRGBA 232 196 72 255)
      panel (padXY 14 12 . gap 8 . fillW $ defaultLayout) $ do
        row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
          heading "Stock"
          flex
          muted footage
        muted "Looping sprocket. Phase is per-hole, not a second clock."
        sprockets clock
      panel (padXY 14 12 . gap 10 . fillW $ defaultLayout) $ do
        row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
          heading "Lamp house"
          flex
          lampCell lampOn lampGlow
        muted "Looping pulse on the lamp. Hold tween on the wash."
        void
          ( box
              (fixedH 12 . fillW $ defaultLayout)
              (lerpColor safeRed filament wash)
          )
      panel (padXY 14 10 . gap 8 . fixedW (240 + 200 * bellowsT) $ defaultLayout) $ do
        heading "Bellows"
        muted "Width and iris hold after the tween settles."
        row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
          clickButton
            (if bellowsOpen then "Collapse" else "Extend")
            (setBellows (not bellowsOpen))
          flex
          let iris = 10 + 28 * bellowsT
          void (box (fixedWH iris iris defaultLayout) (lerpColor gateSteel registration bellowsT))
        when (bellowsT > 0.2) $
          muted "Focus throw follows EaseOutCubic."

pullHoldSec :: Float
pullHoldSec = 1

-- Hold at 0, throw out, hold at 1, throw back, hold at 0.
pullCycleLen :: Float -> Float
pullCycleLen throwSec = 3 * pullHoldSec + 2 * throwSec

laneT :: Ease -> Float -> Float -> Float
laneT ease throwSec time =
  let hold = pullHoldSec
      out0 = hold
      out1 = out0 + throwSec
      topHold1 = out1 + hold
      in0 = topHold1
      in1 = in0 + throwSec
   in if time < out0
        then 0
        else if time < out1
          then applyEase ease ((time - out0) / throwSec)
          else if time < in0
            then 1
            else if time < in1
              then 1 - applyEase ease ((time - in0) / throwSec)
              else 0

lane :: Bool -> Bool -> Float -> Float -> T.Text -> Ease -> Color -> NanoUI ()
lane exposed rewinding throwSec time name ease col = do
  t <-
    if exposed
      then pure (laneT ease throwSec time)
      else
        if rewinding
          then withKey name (animateToEase ease 0 throwSec)
          else pure 0
  row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (tight . fixedW 72 $ defaultLayout) name)
    panel (tight . padAll 8 . fillW $ defaultLayout) $
      row (tight . alignMid . fillW $ defaultLayout) $ do
        void (spacer (Fixed (max 0 (t * 420))) Fit)
        void (box (fixedWH 26 26 defaultLayout) col)
        flex
    muted (T.pack (printf "%.2f" t))

sprockets :: Float -> NanoUI ()
sprockets clock =
  row (tight . gap 6 . alignMid . fillW $ defaultLayout) $
    forM_ [0 .. 11 :: Int] $ \i -> do
      let phase = fromIntegral i * 0.45
          pulse = 0.5 + 0.5 * sin (clock * 2 * pi + phase)
          h = 8 + 14 * pulse
          col = lerpColor (colorRGBA 56 48 36 255) filament pulse
      void (box (fixedWH 16 h defaultLayout) col)

lampCell :: Bool -> Float -> NanoUI ()
lampCell on glow = do
  let dim = colorRGBA 72 48 28 255
      col = if on then lerpColor dim filament glow else dim
      s = 14 + 10 * glow
  row (tight . gap 8 . alignMid $ defaultLayout) $ do
    void (box (fixedWH s s defaultLayout) col)
    muted (if on then "lamp" else "lamp idle")
