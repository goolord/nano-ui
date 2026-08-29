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
    (withTheme ctx benchTheme)
    (\inp -> KeyEscape `elem` inputKeys inp)
    animUi

-- Ink chassis, pewter metal, ivory stock, tungsten lamp.
benchTheme :: Theme
benchTheme =
  let panelStyle =
        Style
          { styleBg = colorRGBA 30 34 42 255
          , styleFg = colorRGBA 236 234 228 255
          , styleBorder = colorRGBA 16 18 22 255
          , styleBorderWidth = 1
          , styleCornerRadius = 2
          , styleHoverBg = colorRGBA 40 46 56 255
          , styleActiveBg = colorRGBA 24 28 34 255
          }
   in defaultTheme
        { themeWindow = colorRGBA 12 14 18 255
        , themePanel = panelStyle
        , themeFloatingWindow = panelStyle
        , themeButton =
            Style
              { styleBg = colorRGBA 44 50 62 255
              , styleFg = colorRGBA 240 238 232 255
              , styleBorder = colorRGBA 110 122 142 255
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 58 68 84 255
              , styleActiveBg = colorRGBA 32 36 46 255
              }
        , themeInput =
            Style
              { styleBg = colorRGBA 20 22 28 255
              , styleFg = colorRGBA 236 234 228 255
              , styleBorder = colorRGBA 78 88 104 255
              , styleBorderWidth = 1
              , styleCornerRadius = 2
              , styleHoverBg = colorRGBA 28 32 40 255
              , styleActiveBg = colorRGBA 14 16 20 255
              }
        , themeSeparator = colorRGBA 78 88 104 255
        , themeAccent = tungsten
        , themeMuted = colorRGBA 148 156 170 255
        }

film, paper, tungsten, hole :: Color
film = colorRGBA 8 10 14 255
paper = colorRGBA 244 240 232 255
tungsten = colorRGBA 232 186 110 255
hole = colorRGBA 12 14 18 255

animUi :: NanoUI ()
animUi = do
  (exposed, setExposed) <- useFlag False
  (rewinding, setRewinding) <- useFlag False
  (lampOn, setLamp) <- useFlag False
  (bellowsOpen, setBellows) <- useFlag False
  (tossed, setTossed) <- useFlag False
  (stiffSpring, setStiffSpring) <- useFlag False
  tossT <-
    withKey ("toss" :: String)
      ( animateToSpring
          (if stiffSpring then presetStiff else presetBouncy)
          (if tossed then 1 else 0)
      )
  lampT <-
    if lampOn
      then withKey ("lamp" :: String) (animateEase EaseInOutCubic 0 1 1.6)
      else pure 0
  wash <-
    withKey ("wash" :: String) (animateToEase EaseInOutCubic (if lampOn then 1 else 0) 0.85)
  bellowsT <-
    withKey ("bellows" :: String) (animateToEase EaseOutCubic (if bellowsOpen then 1 else 0) 0.45)
  clock <-
    if exposed
      then withKey ("clock" :: String) (animateEase EaseLinear 0 1 3.2)
      else
        if rewinding
          then withKey ("clock" :: String) (animateTo 0 0.35)
          else pure 0
  let frames = floor (clock * 128) :: Int
      footage = T.pack (printf "%d+%02d" (frames `div` 16) (frames `mod` 16))
      lampGlow = sin (lampT * pi)
  scroll (tight (grow defaultLayout)) $
    column (padAll 22 . gap 16 . fillW $ defaultLayout) $ do
      row (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
        heading "16mm"
        muted footage
        flex
        clickButton "Expose" (setExposed True >> setRewinding False)
        clickButton "Rewind" (setExposed False >> setRewinding True)
        clickButton (if lampOn then "Lamp off" else "Lamp on") (setLamp (not lampOn))
        clickButton (if tossed then "Catch" else "Toss") (setTossed (not tossed))
        clickButton (if stiffSpring then "Stiff" else "Bouncy") (setStiffSpring (not stiffSpring))
      throwSec <- do
        (_, throwRaw) <- slider "Throw" 35 140 75
        pure (throwRaw / 100)
      cycleThrow <- lockThrow exposed throwSec
      let cycleLen = pullCycleLen cycleThrow
      pullPhase <-
        if exposed
          then withKey ("pulldown" :: String) (animateEase EaseLinear 0 1 cycleLen)
          else pure 0
      let time = pullPhase * cycleLen
      tossRail tossT
      laneTs <- transport exposed rewinding cycleThrow time wash lampGlow
      when
        (rewinding && not exposed && abs clock < 0.001 && all settled laneTs)
        (setRewinding False)
      panel (padXY 12 10 . gap 8 . fixedW (220 + 180 * bellowsT) $ defaultLayout) $ do
        row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
          clickButton
            (if bellowsOpen then "Collapse" else "Extend")
            (setBellows (not bellowsOpen))
          flex
          let iris = 12 + 22 * bellowsT
          void (box (fixedWH iris iris defaultLayout) (lerpColor film paper bellowsT))

settled :: Float -> Bool
settled x = abs x < 0.001

lockThrow :: Bool -> Float -> NanoUI Float
lockThrow exposed throwSec = withKey ("cycleThrow" :: String) $ do
  ctx <- askContext
  wid <- currentId
  uiIO $ do
    cur <- getAnimationValue ctx wid
    if exposed
      then
        if cur < 0.05
          then setAnimationValue ctx wid throwSec >> pure throwSec
          else pure cur
      else setAnimationValue ctx wid 0 >> pure throwSec

transport :: Bool -> Bool -> Float -> Float -> Float -> Float -> NanoUI [Float]
transport exposed rewinding throwSec time wash glow = do
  let washCol = lerpColor film tungsten (wash * (0.2 + 0.55 * glow))
  panel (padXY 0 0 . gap 0 . fillW $ defaultLayout) $
    column (tight . gap 0 . fillW $ defaultLayout) $ do
      perfs
      void (box (fixedH 8 . fillW $ defaultLayout) washCol)
      ts <-
        column (padXY 14 10 . gap 8 . fillW $ defaultLayout) $
          sequence
            [ lane exposed rewinding throwSec time "Leader" EaseLinear
            , lane exposed rewinding throwSec time "Gate" EaseInCubic
            , lane exposed rewinding throwSec time "Shuttle" EaseOutCubic
            , lane exposed rewinding throwSec time "Reg" EaseInOutCubic
            , lane exposed rewinding throwSec time "Claw" EaseOutBack
            , lane exposed rewinding throwSec time "Bezier" (EaseCubicBezier 0.33 0 0.2 1)
            ]
      void (box (fixedH 8 . fillW $ defaultLayout) washCol)
      perfs
      pure ts

perfs :: NanoUI ()
perfs =
  row (tight . gap 10 . padXY 10 4 . fillW $ defaultLayout) $
    forM_ [0 .. 17 :: Int] $ \_ ->
      void (box (fixedWH 7 5 defaultLayout) hole)

pullHoldSec :: Float
pullHoldSec = 1

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

tossRail :: Float -> NanoUI ()
tossRail t =
  row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (tight . fixedW 64 $ defaultLayout) "Spring")
    void (box (fixedWH 3 16 defaultLayout) film)
    column (tight . gap 0 . fillW $ defaultLayout) $ do
      travel <- railTravel "Spring"
      row (tight . alignMid . fillW $ defaultLayout) $ do
        void (spacer (Fixed (max 0 (t * travel))) Fit)
        void (box (fixedWH 18 18 defaultLayout) tungsten)
        flex
    void (box (fixedWH 3 16 defaultLayout) film)

lane :: Bool -> Bool -> Float -> Float -> T.Text -> Ease -> NanoUI Float
lane exposed rewinding throwSec time name ease = do
  t <-
    if exposed
      then pure (laneT ease throwSec time)
      else
        if rewinding
          then withKey name (animateToEase ease 0 throwSec)
          else pure 0
  row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
    void (labelEx (tight . fixedW 64 $ defaultLayout) name)
    void (box (fixedWH 3 16 defaultLayout) film)
    column (tight . gap 0 . fillW $ defaultLayout) $ do
      travel <- railTravel name
      row (tight . alignMid . fillW $ defaultLayout) $ do
        void (spacer (Fixed (max 0 (t * travel))) Fit)
        void (box (fixedWH 18 18 defaultLayout) paper)
        flex
    void (box (fixedWH 3 16 defaultLayout) film)
  pure t

railTravel :: T.Text -> NanoUI Float
railTravel name = withKey (name <> "-rail") $ do
  ctx <- askContext
  wid <- currentId
  mrect <- uiIO (getPrevRect ctx wid)
  void (box (fillW . fixedH 2 $ defaultLayout) film)
  let w = maybe 418 rectW mrect
  pure (max 0 (w - 18))
