{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Bits (shiftR, (.&.))
import System.Exit (exitFailure)

import NanoUI
  ( colorRGBA
  , Direction (..)
  , Sizing (..)
  , Padding (..)
  , AlignX (..)
  , AlignY (..)
  )
import NanoUI.Layout.Arena
  ( NodeType (..)
  , newNodeArena
  , addNode
  , getRect
  )
import NanoUI.Rgfw.Font.Cozette
  ( CozetteFont (..)
  , getCozetteFont
  , charToGlyphId
  , cozetteCharAdvance
  , cozetteLineHeight
  )
import NanoUI.Rgfw.Layout (solveSinglePassLayout)
import NanoUI.Rgfw.Surface (packColor)
import NanoUI.Rgfw.Theme
  ( RgfwTheme (..)
  , tomorrowMidnightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  )
import qualified RGFW.Raw as R

assert :: String -> Bool -> IO ()
assert name True  = putStrLn ("[PASS] " ++ name)
assert name False = do
  putStrLn ("[FAIL] " ++ name)
  exitFailure

testCozette :: IO ()
testCozette = do
  let font = getCozetteFont
  assert "Cozette loads 921 embedded glyphs" (cfNumGlyphs font == 921)
  assert "Cozette space maps to glyph 1" (charToGlyphId font ' ' == 1)
  assert "Cozette '!' maps to glyph 2" (charToGlyphId font '!' == 2)
  assert "Cozette 'A' maps to glyph 34" (charToGlyphId font 'A' == 34)
  assert "Cozette '~' maps to glyph 95" (charToGlyphId font '~' == 95)
  assert "Cozette advance is 6px" (cozetteCharAdvance == 6)
  assert "Cozette line height is 13px" (cozetteLineHeight == 13)

testPackColor :: IO ()
testPackColor = do
  let c = colorRGBA 0x12 0x34 0x56 0x78
      w = packColor c
      a = (w `shiftR` 24) .&. 0xFF
      r = (w `shiftR` 16) .&. 0xFF
      g = (w `shiftR` 8) .&. 0xFF
      b = w .&. 0xFF
  assert "Color byte packing - Alpha" (a == 0x78)
  assert "Color byte packing - Red" (r == 0x12)
  assert "Color byte packing - Green" (g == 0x34)
  assert "Color byte packing - Blue" (b == 0x56)

testTomorrowThemes :: IO ()
testTomorrowThemes = do
  -- Tomorrow Min Light
  assert "Tomorrow Min Light background is white (#FFF)" (thBackground tomorrowMinLightTheme == colorRGBA 255 255 255 255)
  assert "Tomorrow Min Light primary is Tomorrow Blue (#5286BC)" (thPrimary tomorrowMinLightTheme == colorRGBA 82 134 188 255)
  assert "Tomorrow Min Light text is dark gray (#60605F)" (thText tomorrowMinLightTheme == colorRGBA 96 96 95 255)

  -- Tomorrow Night Min Dark
  assert "Tomorrow Night Min background is teal (#2596BE)" (thBackground tomorrowNightMinDarkTheme == colorRGBA 37 150 190 255)
  assert "Tomorrow Night Min primary is Tomorrow Gold (#F0C674)" (thPrimary tomorrowNightMinDarkTheme == colorRGBA 240 198 116 255)
  assert "Tomorrow Night Min text is white (#FFF)" (thText tomorrowNightMinDarkTheme == colorRGBA 255 255 255 255)

  -- Tomorrow at Midnight Min
  assert "Tomorrow at Midnight Min background is pitch black (#000)" (thBackground tomorrowMidnightMinDarkTheme == colorRGBA 0 0 0 255)
  assert "Tomorrow at Midnight Min text is near-white (#EEE)" (thText tomorrowMidnightMinDarkTheme == colorRGBA 238 238 238 255)

testMouseConstants :: IO ()
testMouseConstants = do
  assert "RGFW mouseLeft is 0 (RGFW.h match)" (R.rgfw_mouseLeft == 0)
  assert "RGFW mouseMiddle is 1 (RGFW.h match)" (R.rgfw_mouseMiddle == 1)
  assert "RGFW mouseRight is 2 (RGFW.h match)" (R.rgfw_mouseRight == 2)

testSinglePassColumnLayout :: IO ()
testSinglePassColumnLayout = do
  na <- newNodeArena
  -- Root container: 800x600, Column, pad=20, gap=10
  root <- addNode na NodeContainer (-1) Column (Fixed 800) (Fixed 600) (Padding 20 20 20 20) 10 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Child 1: fixed 100x30
  c1 <- addNode na NodeButton root Column (Fixed 100) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Child 2: fixed 120x40
  c2 <- addNode na NodeButton root Column (Fixed 120) (Fixed 40) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na 800 600

  rRoot <- getRect na root
  assert "Root rect is (0, 0, 800, 600)" (rRoot == (0, 0, 800, 600))

  r1 <- getRect na c1
  assert "Child 1 rect is (20, 20, 100, 30)" (r1 == (20, 20, 100, 30))

  r2 <- getRect na c2
  -- Child 2 should be at y = 20 + 30 + 10 = 60
  assert "Child 2 rect is (20, 60, 120, 40)" (r2 == (20, 60, 120, 40))

testSinglePassRowLayout :: IO ()
testSinglePassRowLayout = do
  na <- newNodeArena
  -- Root container: 500x300, Row, pad=15, gap=8
  root <- addNode na NodeContainer (-1) Row (Fixed 500) (Fixed 300) (Padding 15 15 15 15) 8 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Child 1: fixed 60x25
  c1 <- addNode na NodeButton root Column (Fixed 60) (Fixed 25) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Child 2: fixed 80x25
  c2 <- addNode na NodeButton root Column (Fixed 80) (Fixed 25) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na 500 300

  r1 <- getRect na c1
  assert "Row Child 1 rect is (15, 15, 60, 25)" (r1 == (15, 15, 60, 25))

  r2 <- getRect na c2
  -- Child 2 should be at x = 15 + 60 + 8 = 83
  assert "Row Child 2 rect is (83, 15, 80, 25)" (r2 == (83, 15, 80, 25))

testLayoutClampingAndBounds :: IO ()
testLayoutClampingAndBounds = do
  na <- newNodeArena
  -- Window 600x400
  -- Main row: fixed 600x400, Row, pad=0, gap=16
  mainRow <- addNode na NodeContainer (-1) Row (Fixed 600) (Fixed 400) (Padding 0 0 0 0) 16 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Left panel: fixedW 200, fillH
  leftP <- addNode na NodePanel mainRow Column (Fixed 200) (Grow 1.0) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Right panel: fillW, fillH
  rightP <- addNode na NodePanel mainRow Column (Grow 1.0) (Grow 1.0) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na 600 400

  (rx0, _, rw0, rh0) <- getRect na leftP
  assert "Left panel starts at x=0, w=200, h=400" (rx0 == 0 && rw0 == 200 && rh0 == 400)

  (rx1, _, rw1, rh1) <- getRect na rightP
  -- Right panel starts at x = 200 + 16 = 216
  -- Remaining width in 600px row is 600 - 216 = 384
  assert "Right panel starts at x=216, w=384, h=400" (rx1 == 216 && rw1 == 384 && rh1 == 400)
  assert "Right panel does not exceed parent width (216 + 384 == 600)" (rx1 + rw1 <= 600)

testNestedContainerHeight :: IO ()
testNestedContainerHeight = do
  na <- newNodeArena
  -- Panel 400x400
  p <- addNode na NodePanel (-1) Column (Fixed 400) (Fixed 400) (Padding 10 10 10 10) 8 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Nested column with 3 labels (each h=13, gap=6)
  col <- addNode na NodeContainer p Column Fit Fit (Padding 0 0 0 0) 6 0 0 1e9 1e9 0 AlignStart AlignTop False
  _ <- addNode na NodeText col Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  _ <- addNode na NodeText col Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  _ <- addNode na NodeText col Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Next item after column in panel: separator (h=2)
  sepNode <- addNode na NodeSeparator p Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na 400 400

  -- col should be 3 * 13 + 2 * 6 = 39 + 12 = 51 px high
  (_, _, _, ch) <- getRect na col
  assert "Nested column height is 51px" (ch == 51)
  -- sepNode should be placed after col + gap 8 -> 10 + 51 + 8 = 69
  (_, sy, _, _) <- getRect na sepNode
  assert "Separator starts after nested column at y=69 (no overlap)" (sy == 69)

main :: IO ()
main = do
  putStrLn "=== Running nano-ui-rgfw Unit Tests ==="
  testCozette
  testPackColor
  testTomorrowThemes
  testMouseConstants
  testSinglePassColumnLayout
  testSinglePassRowLayout
  testLayoutClampingAndBounds
  testNestedContainerHeight
  putStrLn "=== All tests passed successfully! ==="
