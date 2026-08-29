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

-- Steenbeck flatbed: putty Formica, black 16mm path, cream frame, ruby rec.
benchTheme :: Theme
benchTheme =
  let plate =
        Style
          { styleBg = colorRGBA 214 208 196 255
          , styleFg = colorRGBA 28 26 22 255
          , styleBorder = colorRGBA 92 86 76 255
          , styleBorderWidth = 1
          , styleCornerRadius = 1
          , styleHoverBg = colorRGBA 222 216 204 255
          , styleActiveBg = colorRGBA 196 190 178 255
          }
   in defaultTheme
        { themeWindow = formica
        , themePanel = plate
        , themeFloatingWindow = plate
        , themeButton =
            Style
              { styleBg = colorRGBA 196 190 176 255
              , styleFg = colorRGBA 28 26 22 255
              , styleBorder = colorRGBA 74 68 58 255
              , styleBorderWidth = 1
              , styleCornerRadius = 1
              , styleHoverBg = colorRGBA 228 222 208 255
              , styleActiveBg = colorRGBA 168 162 148 255
              }
        , themeInput =
            Style
              { styleBg = colorRGBA 176 170 156 255
              , styleFg = colorRGBA 28 26 22 255
              , styleBorder = colorRGBA 74 68 58 255
              , styleBorderWidth = 1
              , styleCornerRadius = 1
              , styleHoverBg = colorRGBA 186 180 166 255
              , styleActiveBg = colorRGBA 158 152 138 255
              }
        , themeSeparator = colorRGBA 92 86 76 255
        , themeAccent = ruby
        , themeMuted = colorRGBA 90 84 74 255
        , themeOverlayDim = colorRGBA 40 36 30 160
        }

formica, film, rail, paper, ruby, lamp, punch :: Color
formica = colorRGBA 184 176 162 255
film = colorRGBA 18 16 14 255
rail = colorRGBA 42 38 34 255
paper = colorRGBA 244 236 220 255
ruby = colorRGBA 154 42 36 255
lamp = colorRGBA 255 196 92 255
punch = colorRGBA 232 224 208 255

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
    column (padAll 28 . gap 18 . fillW $ defaultLayout) $ do
      row (tight . gap 10 . alignMid . fillW $ defaultLayout) $ do
        heading "16mm"
        flex
        withKey ("footage" :: String) (muted footage)
      row (tight . gap 8 . alignMid . fillW $ defaultLayout) $ do
        clickButton "Expose" (setExposed True >> setRewinding False)
        clickButton "Rewind" (setExposed False >> setRewinding True)
        clickButton (if lampOn then "Lamp off" else "Lamp on") (setLamp (not lampOn))
        flex
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
              irisCol = lerpColor film (lerpColor paper ruby 0.18) bellowsT
          void (box (fixedWH iris iris defaultLayout) irisCol)

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
  let washCol = lerpColor film lamp (wash * (0.45 + 0.55 * glow))
  column (tight . gap 0 . fillW $ defaultLayout) $ do
    perfs
    withKey ("washTop" :: String) (void (box (fixedH 10 . fillW $ defaultLayout) washCol))
    ts <-
      column (padXY 0 10 . gap 10 . fillW $ defaultLayout) $
        sequence
          [ lane exposed rewinding throwSec time "Leader" EaseLinear
          , lane exposed rewinding throwSec time "Gate" EaseInCubic
          , lane exposed rewinding throwSec time "Shuttle" EaseOutCubic
          , lane exposed rewinding throwSec time "Reg" EaseInOutCubic
          , lane exposed rewinding throwSec time "Claw" EaseOutBack
          , lane exposed rewinding throwSec time "Bezier" (EaseCubicBezier 0.33 0 0.2 1)
          ]
    withKey ("washBot" :: String) (void (box (fixedH 10 . fillW $ defaultLayout) washCol))
    perfs
    pure ts

perfs :: NanoUI ()
perfs =
  row (tight . gap 0 . alignMid . fillW $ defaultLayout) $ do
    withKey ("perfL" :: String) (void (box (fixedWH 12 18 defaultLayout) film))
    forM_ [0 .. 16 :: Int] $ \i ->
      withKey i $ do
        void (box (fixedWH 6 18 defaultLayout) film)
        void (box (fixedWH 7 6 defaultLayout) punch)
        void (box (fixedWH 3 18 defaultLayout) film)
    withKey ("perfR" :: String) (void (box (fillW . fixedH 18 $ defaultLayout) film))

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
tossRail t = trackRow "Spring" t ruby

lane :: Bool -> Bool -> Float -> Float -> T.Text -> Ease -> NanoUI Float
lane exposed rewinding throwSec time name ease = do
  t <-
    if exposed
      then pure (laneT ease throwSec time)
      else
        if rewinding
          then withKey name (animateToEase ease 0 throwSec)
          else pure 0
  trackRow name t paper
  pure t

trackRow :: T.Text -> Float -> Color -> NanoUI ()
trackRow name t shuttle =
  withKey name $
    row (tight . gap 12 . alignMid . fillW $ defaultLayout) $ do
      void (labelEx (tight . fixedW 72 $ defaultLayout) name)
      column (tight . gap 0 . fillW $ defaultLayout) $ do
        travel <- railTravel name
        row (tight . alignMid . fillW $ defaultLayout) $ do
          void (box (fixedWH 4 22 defaultLayout) film)
          void (spacer (Fixed (max 0 (t * travel))) Fit)
          void (box (fixedWH 16 16 defaultLayout) shuttle)
          flex
          void (box (fixedWH 4 22 defaultLayout) film)

railTravel :: T.Text -> NanoUI Float
railTravel name = withKey (name <> "-rail") $ do
  ctx <- askContext
  wid <- currentId
  mrect <- uiIO (getPrevRect ctx wid)
  void (box (fillW . fixedH 3 $ defaultLayout) rail)
  let w = maybe 418 rectW mrect
  pure (max 0 (w - 24))
