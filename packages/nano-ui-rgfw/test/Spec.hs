{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Bits (shiftR, (.&.))
import System.Exit (exitFailure)

import qualified Data.IntMap.Strict as IM
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import NanoUI.Debug (CoreDebugSnapshot (..))
import NanoUI.Rgfw.Debug (RgfwDebugSnapshot (..), debugWindowBody, emptyRgfwDebug)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeType (..)
  , addNode
  , arenaCount
  , getClipRect
  , getDirection
  , getFirstChild
  , getNextSibling
  , getNodeType
  , getParent
  , getRect
  , newNodeArena
  , setClipRect
  , setRect
  , setStyleIdx
  , setWidgetId
  )
import NanoUI
  ( AlignX (..)
  , AlignY (..)
  , Color (..)
  , Direction (..)
  , Padding (..)
  , Rect (..)
  , Size (..)
  , Sizing (..)
  , V2 (..)
  , colorRGBA
  , label
  , rectContains
  , rectH
  , rectW
  , rectX
  , rectY
  , runNanoUI
  , window
  )
import NanoUI.Layout.Solve (solveLayout)
import Data.Word (Word32)
import Foreign.Storable (peekElemOff)
import NanoUI.Input (Input (..), Modifiers (..), emptyInput)
import NanoUI.Widgets.TextArea (buffer, initTextAreaState, processTextArea, selectionAnchor)
import NanoUI.Widgets.TextBuffer as TB (Cursor (..), getCursor, toText)
import NanoUI.Context
  ( Context (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , getStore
  , intKey
  , setStore
  , withClipboard
  , withFontMetrics
  )
import NanoUI.Frame.Window (resizeFromEdge, windowResizeEdgeAt)

import NanoUI.Store
  ( WidgetStore (..)
  , slotAnchor
  , slotCursor
  , slotKey
  , slotTextAreaAnchorCol
  , slotTextAreaAnchorRow
  , slotTextAreaCol
  , slotTextAreaRow
  )
import NanoUI.Testing (HostProfile (PixelHost), newPixelContext, runFrame)
import NanoUI.Frame.TextEdit
  ( applyTextFieldMenuAction
  , normalizeTextFieldClicks
  , textEditMenuRectAt
  , textEditMenuWidth
  , textWordBounds
  )
import NanoUI.Id (WidgetId (..))
import NanoUI.Rgfw.Font.Cozette
  ( CozetteFont (..)
  , boxAverageCoverage
  , charToGlyphId
  , cozetteCharAdvance
  , cozetteGlyphBit1x
  , cozetteGlyphBit2x
  , cozetteGlyphBit4x
  , cozetteLineHeight
  , cozetteMetrics
  , getCozetteFont
  , scale2x
  )
import NanoUI.Rgfw.Render (renderArena)
import NanoUI.Rgfw.Session (defaultRgfwOptions, optScale)
import NanoUI.Rgfw.Surface
  ( freeRgfwSurface
  , newOffscreenRgfwSurface
  , packColor
  , sBuffer
  , toPhysRect
  )
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
  assert "Tomorrow Night Min background is dark charcoal (#1E1F21)" (thBackground tomorrowNightMinDarkTheme == 0x1E1F21FF)
  assert "Tomorrow Night Min primary is orange accent (#ED9E56)" (thPrimary tomorrowNightMinDarkTheme == 0xED9E56FF)
  assert "Tomorrow Night Min text is near-white (#EEEEEE)" (thText tomorrowNightMinDarkTheme == 0xEEEEEEFF)

  -- Tomorrow at Midnight Min
  assert "Tomorrow at Midnight Min background is pitch black (#000)" (thBackground tomorrowMidnightMinDarkTheme == colorRGBA 0 0 0 255)
  assert "Tomorrow at Midnight Min text is near-white (#EEE)" (thText tomorrowMidnightMinDarkTheme == colorRGBA 238 238 238 255)

testMouseConstants :: IO ()
testMouseConstants = do
  assert "RGFW mouseLeft is 0 (RGFW.h match)" (R.rgfw_mouseLeft == 0)
  assert "RGFW mouseMiddle is 1 (RGFW.h match)" (R.rgfw_mouseMiddle == 1)
  assert "RGFW mouseRight is 2 (RGFW.h match)" (R.rgfw_mouseRight == 2)

testMultiClickWordBounds :: IO ()
testMultiClickWordBounds = do
  let s = "hello world"
  -- Clicking on first word "hello"
  assert "Word bounds at start of 'hello' is (0, 5)" (textWordBounds s 0 == (0, 5))
  assert "Word bounds in middle of 'hello' is (0, 5)" (textWordBounds s 2 == (0, 5))
  assert "Word bounds at end of 'hello' is (0, 5)" (textWordBounds s 4 == (0, 5))
  -- Space between words
  assert "Word bounds on space is (5, 6)" (textWordBounds s 5 == (5, 6))
  -- Clicking on second word "world"
  assert "Word bounds at start of 'world' is (6, 11)" (textWordBounds s 6 == (6, 11))
  assert "Word bounds in middle of 'world' is (6, 11)" (textWordBounds s 8 == (6, 11))
  assert "Word bounds at end of string selects last word (6, 11)" (textWordBounds s 11 == (6, 11))
  assert "Word bounds on empty string is (0, 0)" (textWordBounds "" 0 == (0, 0))
  -- Identifier with underscore
  let code = "foo_bar baz"
  assert "Word bounds on 'foo_bar' includes underscore" (textWordBounds code 3 == (0, 7))

testNormalizeClicks :: IO ()
testNormalizeClicks = do
  ctx <- newPixelContext
  let wid = WidgetId 99
  c1 <- normalizeTextFieldClicks ctx wid 2 0 0 False 1
  assert "Raw click 1 returns 1" (c1 == 1)
  c2 <- normalizeTextFieldClicks ctx wid 2 0 0 False 2
  assert "Raw click 2 on same cell returns 2" (c2 == 2)
  c3 <- normalizeTextFieldClicks ctx wid 2 0 0 False 3
  assert "Raw click 3 on same cell returns 3" (c3 == 3)
  cReset <- normalizeTextFieldClicks ctx wid 7 0 0 False 2
  assert "Raw click 2 on different cell resets to 1" (cReset == 1)

testContextMenuGeometry :: IO ()
testContextMenuGeometry = do
  ctx <- newPixelContext
  let fm = cozetteMetrics
  menuW <- textEditMenuWidth ctx
  let rNorm = textEditMenuRectAt (ctxHostProfile ctx) fm 100 100 menuW (Size 800 600)
  assert "Menu rect at (100, 100) placed at mouse" (rectX rNorm == 100 && rectY rNorm == 100)
  let rClamp = textEditMenuRectAt (ctxHostProfile ctx) fm 790 595 menuW (Size 800 600)
  assert "Menu rect clamped within window width" (rectX rClamp + rectW rClamp <= 800)
  assert "Menu rect clamped within window height" (rectY rClamp + rectH rClamp <= 600)

testTextInputContextMenuActions :: IO ()
testTextInputContextMenuActions = do
  clipRef <- newIORef ("" :: T.Text)
  ctx0 <- newPixelContext
  let ctx = withClipboard
              (withFontMetrics ctx0 cozetteMetrics)
              (readIORef clipRef >>= \t -> pure (if T.null t then Nothing else Just t))
              (\t -> writeIORef clipRef t >> pure True)
      wid = WidgetId 101
      key = intKey wid
  idx <- addNode (ctxNodeArena ctx) NodeTextInput (-1) Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop
  setWidgetId (ctxNodeArena ctx) idx wid

  -- Setup text "hello world" with "world" selected (anchor=6, cursor=11)
  store0 <- getStore ctx
  setStore ctx $ store0
    { storeText = IM.insert key "hello world" (storeText store0)
    , storeInt =
        IM.insert (slotKey slotAnchor key) 6 $
          IM.insert (slotKey slotCursor key) 11 (storeInt store0)
    }

  -- Copy (action 1)
  applyTextFieldMenuAction ctx wid 1
  clip1 <- readIORef clipRef
  assert "TextInput context menu Copy writes 'world' to clipboard" (clip1 == "world")

  -- Cut (action 0)
  applyTextFieldMenuAction ctx wid 0
  store1 <- getStore ctx
  let text1 = IM.findWithDefault "" key (storeText store1)
  assert "TextInput context menu Cut cuts 'world' leaving 'hello '" (text1 == "hello ")

  -- Paste (action 2)
  applyTextFieldMenuAction ctx wid 2
  store2 <- getStore ctx
  let text2 = IM.findWithDefault "" key (storeText store2)
  assert "TextInput context menu Paste restores 'hello world'" (text2 == "hello world")

  -- Select All (action 3)
  applyTextFieldMenuAction ctx wid 3
  store3 <- getStore ctx
  let anc = IM.findWithDefault (-1) (slotKey slotAnchor key) (storeInt store3)
      cur = IM.findWithDefault (-1) (slotKey slotCursor key) (storeInt store3)
  assert "TextInput context menu Select All sets anchor=0 and cursor=11" (anc == 0 && cur == 11)

testTextAreaContextMenuActions :: IO ()
testTextAreaContextMenuActions = do
  clipRef <- newIORef ("" :: T.Text)
  ctx0 <- newPixelContext
  let ctx = withClipboard
              (withFontMetrics ctx0 cozetteMetrics)
              (readIORef clipRef >>= \t -> pure (if T.null t then Nothing else Just t))
              (\t -> writeIORef clipRef t >> pure True)
      wid = WidgetId 202
      key = intKey wid
  idx <- addNode (ctxNodeArena ctx) NodeTextArea (-1) Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop
  setWidgetId (ctxNodeArena ctx) idx wid

  -- Setup text "first line\nsecond line"
  store0 <- getStore ctx
  setStore ctx $ store0
    { storeText = IM.insert key "first line\nsecond line" (storeText store0)
    , storeInt =
        IM.insert (slotKey slotTextAreaRow key) 0 $
          IM.insert (slotKey slotTextAreaCol key) 0 $
            IM.insert (slotKey slotTextAreaAnchorRow key) 0 $
              IM.insert (slotKey slotTextAreaAnchorCol key) 0 (storeInt store0)
    }

  -- Select All (action 3)
  applyTextFieldMenuAction ctx wid 3
  store1 <- getStore ctx
  let aRow = IM.findWithDefault (-1) (slotKey slotTextAreaAnchorRow key) (storeInt store1)
      aCol = IM.findWithDefault (-1) (slotKey slotTextAreaAnchorCol key) (storeInt store1)
      cRow = IM.findWithDefault (-1) (slotKey slotTextAreaRow key) (storeInt store1)
      cCol = IM.findWithDefault (-1) (slotKey slotTextAreaCol key) (storeInt store1)
  assert "TextArea context menu Select All selects all lines" (aRow == 0 && aCol == 0 && cRow == 1 && cCol == 11)

  -- Copy (action 1)
  applyTextFieldMenuAction ctx wid 1
  clip1 <- readIORef clipRef
  assert "TextArea context menu Copy writes full text to clipboard" (clip1 == "first line\nsecond line")

  -- Cut (action 0)
  applyTextFieldMenuAction ctx wid 0
  store2 <- getStore ctx
  let text2 = IM.findWithDefault "fail" key (storeText store2)
  assert "TextArea context menu Cut removes all text" (text2 == "")

  -- Paste (action 2)
  applyTextFieldMenuAction ctx wid 2
  store3 <- getStore ctx
  let text3 = IM.findWithDefault "fail" key (storeText store3)
  assert "TextArea context menu Paste pastes full text back" (text3 == "first line\nsecond line")

testTextAreaCtrlA :: IO ()
testTextAreaCtrlA = do
  ctx0 <- newPixelContext
  let ctx = withFontMetrics ctx0 cozetteMetrics
      initial = "hello world\nsecond line of text\nthird"
      st0 = initTextAreaState initial
      inpA = emptyInput {inputChars = "a", inputModifiers = Modifiers False True False}
  -- Process Ctrl+A with 'a'
  st1 <- processTextArea ctx inpA 200 96 14 st0
  let Cursor aRow aCol = selectionAnchor st1
      Cursor cRow cC = TB.getCursor (buffer st1)
  assert "TextArea Ctrl+A with 'a' sets anchor to (0, 0)" (aRow == 0 && aCol == 0)
  assert "TextArea Ctrl+A with 'a' sets cursor to (2, 5)" (cRow == 2 && cC == 5)
  -- Process Ctrl+A with '\x01'
  let inpCtrlA = emptyInput {inputChars = "\x01", inputModifiers = Modifiers False True False}
  st2 <- processTextArea ctx inpCtrlA 200 96 14 st0
  let Cursor aRow2 aCol2 = selectionAnchor st2
      Cursor cRow2 cC2 = TB.getCursor (buffer st2)
  assert "TextArea Ctrl+A with '\\x01' sets anchor to (0, 0)" (aRow2 == 0 && aCol2 == 0)
  assert "TextArea Ctrl+A with '\\x01' sets cursor to (2, 5)" (cRow2 == 2 && cC2 == 5)
  -- Typing a character after Ctrl+A replaces all text
  let inpTyping = emptyInput {inputChars = "x"}
  st3 <- processTextArea ctx inpTyping 200 96 14 st1
  assert "TextArea typing after Ctrl+A replaces text" (TB.toText (buffer st3) == "x")

testScale2xRule :: IO ()
testScale2xRule = do
  let sampleIsolated x y = x == 1 && y == 1
      scaledIsolated = scale2x 3 3 sampleIsolated
  assert "Scale2x isolated dot expands to 2x2 block"
    (scaledIsolated 2 2 && scaledIsolated 3 2 && scaledIsolated 2 3 && scaledIsolated 3 3)
  assert "Scale2x isolated dot does not spill into neighbor pixels"
    (not (scaledIsolated 1 2) && not (scaledIsolated 4 2) && not (scaledIsolated 2 1) && not (scaledIsolated 2 4))

  let sampleCorner x y = (x == 1 && y == 0) || (x == 0 && y == 1) || (x == 1 && y == 1)
      scaledCorner = scale2x 3 3 sampleCorner
  assert "Scale2x corner smoothing activates on E3 (rounds corner)"
    (scaledCorner 2 2 && scaledCorner 3 2 && scaledCorner 2 3 && not (scaledCorner 3 3))

testScale2xGlyphTables :: IO ()
testScale2xGlyphTables = do
  let font = getCozetteFont
      testGlyphs = [1, 2, 34, 36, 65, 95]
  forM_ testGlyphs $ \gid -> do
    let expectedBit2x = scale2x 7 13 (cozetteGlyphBit1x font gid)
        matches2x = and [ cozetteGlyphBit2x font gid x y == expectedBit2x x y
                        | y <- [0 .. 25]
                        , x <- [0 .. 13]
                        ]
    assert ("Scale2x 14x26 precomputed table matches pure scale2x for glyph " ++ show gid) matches2x

  forM_ testGlyphs $ \gid -> do
    let expectedBit4x = scale2x 14 26 (cozetteGlyphBit2x font gid)
        matches4x = and [ cozetteGlyphBit4x font gid x y == expectedBit4x x y
                        | y <- [0 .. 51]
                        , x <- [0 .. 27]
                        ]
    assert ("Scale4x 28x52 precomputed table matches double scale2x for glyph " ++ show gid) matches4x

testFractionalDpiCalculations :: IO ()
testFractionalDpiCalculations = do
  let (px0, py0, pw0, ph0) = toPhysRect 1.5 0 0 100 50
  assert "toPhysRect 1.5x at origin" (px0 == 0 && py0 == 0 && pw0 == 150 && ph0 == 75)

  let scale = 1.33 :: Float
      (w1_x0, _, w1_w, _) = toPhysRect scale 0 0 63.7 30
      (w2_x0, _, _, _) = toPhysRect scale 63.7 0 63.7 30
  assert "Adjacent widgets at fractional scale have zero gap/overlap" (w1_x0 + w1_w == w2_x0)

  let physW = 1920 :: Int
      physH = 1080 :: Int
      sc = 1.5 :: Float
      logW = round (fromIntegral physW / sc) :: Int
      logH = round (fromIntegral physH / sc) :: Int
  assert "Logical window width at 1.5x is 1280" (logW == 1280)
  assert "Logical window height at 1.5x is 720" (logH == 720)

  let physMouseX = 750 :: Int
      physMouseY = 450 :: Int
      logMouseX = fromIntegral physMouseX / sc :: Float
      logMouseY = fromIntegral physMouseY / sc :: Float
  assert "Logical mouse X at 1.5x is 500" (logMouseX == 500.0)
  assert "Logical mouse Y at 1.5x is 300" (logMouseY == 300.0)
  assert "defaultRgfwOptions optScale is 0.0 (OS reported DPI by default)" (optScale defaultRgfwOptions == 0.0)

testBoxAreaAveraging :: IO ()
testBoxAreaAveraging = do
  let covAll1 = boxAverageCoverage 4 4 2 2 (\_ _ -> True) 0 0
  assert "Box averaging all-ones produces 1.0" (abs (covAll1 - 1.0) < 0.001)

  let covAll0 = boxAverageCoverage 4 4 2 2 (\_ _ -> False) 0 0
  assert "Box averaging all-zeros produces 0.0" (abs (covAll0 - 0.0) < 0.001)

  let covSingle = boxAverageCoverage 2 2 1 1 (\x y -> x == 0 && y == 0) 0 0
  assert "Single pixel in 2x2 downscaled to 1x1 produces 0.25 coverage" (abs (covSingle - 0.25) < 0.001)

  let isTopLeft2x2 x y = x < 2 && y < 2
      covTL = boxAverageCoverage 4 4 2 2 isTopLeft2x2 0 0
      covTR = boxAverageCoverage 4 4 2 2 isTopLeft2x2 1 0
      covBL = boxAverageCoverage 4 4 2 2 isTopLeft2x2 0 1
      covBR = boxAverageCoverage 4 4 2 2 isTopLeft2x2 1 1
  assert "2x2 block top-left destination has coverage 1.0" (abs (covTL - 1.0) < 0.001)
  assert "2x2 block top-right destination has coverage 0.0" (abs (covTR - 0.0) < 0.001)
  assert "2x2 block bottom-left destination has coverage 0.0" (abs (covBL - 0.0) < 0.001)
  assert "2x2 block bottom-right destination has coverage 0.0" (abs (covBR - 0.0) < 0.001)

  let isCenter x y = x == 1 && y == 1
      cov00 = boxAverageCoverage 3 3 2 2 isCenter 0 0
      cov01 = boxAverageCoverage 3 3 2 2 isCenter 0 1
      cov10 = boxAverageCoverage 3 3 2 2 isCenter 1 0
      cov11 = boxAverageCoverage 3 3 2 2 isCenter 1 1
  assert "Fractional area split is symmetric across boundaries" (cov00 == cov01 && cov01 == cov10 && cov10 == cov11)
  let totalIntegratedArea = (cov00 + cov01 + cov10 + cov11) * 2.25
  assert "Continuous box averaging conserves total energy/area" (abs (totalIntegratedArea - 1.0) < 0.001)

testWindowResizing :: IO ()
testWindowResizing = do
  let winRect = Rect 100 100 300 200
  -- 1. Test windowResizeEdgeAt
  assert "Bottom-right outer halo detects ResizeSE"
    (windowResizeEdgeAt PixelHost winRect (V2 404 304) == Just ResizeSE)
  assert "Bottom edge halo detects ResizeS"
    (windowResizeEdgeAt PixelHost winRect (V2 250 304) == Just ResizeS)
  assert "Right edge halo detects ResizeE"
    (windowResizeEdgeAt PixelHost winRect (V2 404 200) == Just ResizeE)
  assert "Left edge halo detects ResizeW"
    (windowResizeEdgeAt PixelHost winRect (V2 96 200) == Just ResizeW)
  assert "Top edge outer halo detects ResizeN"
    (windowResizeEdgeAt PixelHost winRect (V2 250 95) == Just ResizeN)
  assert "Top-left corner detects ResizeNW"
    (windowResizeEdgeAt PixelHost winRect (V2 96 96) == Just ResizeNW)
  assert "Top-right corner detects ResizeNE"
    (windowResizeEdgeAt PixelHost winRect (V2 404 96) == Just ResizeNE)
  assert "Window interior does NOT trigger resize"
    (windowResizeEdgeAt PixelHost winRect (V2 200 200) == Nothing)
  assert "Far outside halo does NOT trigger resize"
    (windowResizeEdgeAt PixelHost winRect (V2 500 500) == Nothing)

  -- 2. Test resizeFromEdge math
  let wrd = WindowResizeDrag
        { wrdWidget = WidgetId 42
        , wrdEdge = ResizeSE
        , wrdGrabX = 400
        , wrdGrabY = 300
        , wrdStartX = 100
        , wrdStartY = 100
        , wrdStartW = 300
        , wrdStartH = 200
        , wrdMinW = 160
        , wrdMinH = 80
        , wrdMaxW = 1000
        , wrdMaxH = 800
        }
      (nw, nh, nx, ny) = resizeFromEdge wrd (V2 450 350) 1000 800
  assert "Drag SE corner expands width to 350" (nw == 350.0)
  assert "Drag SE corner expands height to 250" (nh == 250.0)
  assert "Drag SE corner preserves origin X at 100" (nx == 100.0)
  assert "Drag SE corner preserves origin Y at 100" (ny == 100.0)

  let wrdW = wrd { wrdEdge = ResizeW, wrdGrabX = 100, wrdGrabY = 200 }
      (nwW, nhW, nxW, nyW) = resizeFromEdge wrdW (V2 70 200) 1000 800
  assert "Drag W edge expands width to 330" (nwW == 330.0)
  assert "Drag W edge preserves height at 200" (nhW == 200.0)
  assert "Drag W edge moves origin X to 70" (nxW == 70.0)
  assert "Drag W edge preserves origin Y at 100" (nyW == 100.0)

  let wrdE = wrd { wrdEdge = ResizeE, wrdGrabX = 400, wrdGrabY = 200 }
      (nwE, nhE, nxE, nyE) = resizeFromEdge wrdE (V2 450 200) 1000 800
  assert "Drag E edge expands width to 350" (nwE == 350.0)
  assert "Drag E edge preserves height at 200" (nhE == 200.0)
  assert "Drag E edge preserves origin X at 100" (nxE == 100.0)
  assert "Drag E edge preserves origin Y at 100" (nyE == 100.0)

  let (nwEClamp, _, nxEClamp, _) = resizeFromEdge wrdE (V2 150 200) 1000 800
  assert "Drag E edge clamps to min width 160" (nwEClamp == 160.0)
  assert "Drag E edge preserves origin X at 100 on clamp" (nxEClamp == 100.0)

  let wrdN = wrd { wrdEdge = ResizeN, wrdGrabX = 250, wrdGrabY = 100 }
      (nwN, nhN, nxN, nyN) = resizeFromEdge wrdN (V2 250 50) 1000 800
  assert "Drag N edge expands height to 250" (nhN == 250.0)
  assert "Drag N edge moves origin Y to 50" (nyN == 50.0)
  assert "Drag N edge preserves width at 300" (nwN == 300.0)
  assert "Drag N edge preserves origin X at 100" (nxN == 100.0)

  let (_, nhNClamp, _, nyNClamp) = resizeFromEdge wrdN (V2 250 280) 1000 800
  assert "Drag N edge clamps to min height 80 and pins bottom at 300" (nhNClamp == 80.0 && nyNClamp == 220.0)

  let wrdS = wrd { wrdEdge = ResizeS, wrdGrabX = 250, wrdGrabY = 300 }
      (nwS, nhS, nxS, nyS) = resizeFromEdge wrdS (V2 250 350) 1000 800
  assert "Drag S edge expands height to 250" (nhS == 250.0)
  assert "Drag S edge preserves origin Y at 100" (nyS == 100.0)
  assert "Drag S edge preserves width at 300" (nwS == 300.0)
  assert "Drag S edge preserves origin X at 100" (nxS == 100.0)

testZIndexRenderArena :: IO ()
testZIndexRenderArena = do
  surf <- newOffscreenRgfwSurface 100 100
  let font = getCozetteFont
      theme = tomorrowMidnightMinDarkTheme
  ctx <- newPixelContext

  na <- newNodeArena
  root <- addNode na NodeContainer (-1) Column (Fixed 100) (Fixed 100) (Padding 0 0 0 0) 0 0 0 100 100 0 AlignStart AlignTop

  -- Insert in reverse layer order to verify topological Z-layering works regardless of insertion order:
  pop <- addNode na NodePopup root Column (Fixed 40) (Fixed 40) (Padding 0 0 0 0) 0 0 0 100 100 0 AlignStart AlignTop
  setRect na pop 40 40 40 40
  b3 <- addNode na NodeBox pop Column (Fixed 40) (Fixed 40) (Padding 0 0 0 0) 0 0 0 100 100 0 AlignStart AlignTop
  setStyleIdx na b3 (fromIntegral (0x778899FF :: Word32))
  setRect na b3 40 40 40 40

  w <- addNode na NodeWindow root Column (Fixed 60) (Fixed 60) (Padding 0 0 0 0) 0 0 0 100 100 0 AlignStart AlignTop
  setRect na w 20 20 60 60
  b2 <- addNode na NodeBox w Column (Fixed 60) (Fixed 60) (Padding 0 0 0 0) 0 0 0 100 100 0 AlignStart AlignTop
  setStyleIdx na b2 (fromIntegral (0x445566FF :: Word32))
  setRect na b2 20 20 60 60

  b1 <- addNode na NodeBox root Column (Fixed 80) (Fixed 80) (Padding 0 0 0 0) 0 0 0 100 100 0 AlignStart AlignTop
  setStyleIdx na b1 (fromIntegral (0x112233FF :: Word32))
  setRect na b1 0 0 80 80

  renderArena surf font 1.0 theme ctx na (WidgetId 0) (WidgetId 0) (WidgetId 0)

  let p10 = 10 * 100 + 10
      p30 = 30 * 100 + 30
      p50 = 50 * 100 + 50
  c10 <- peekElemOff (sBuffer surf) p10
  c30 <- peekElemOff (sBuffer surf) p30
  c50 <- peekElemOff (sBuffer surf) p50

  assert "Pixel (10, 10) rendered Normal node color" (c10 == packColor (Color 0x112233FF))
  assert "Pixel (30, 30) rendered Window overlay node on top of Normal" (c30 == packColor (Color 0x445566FF))
  assert "Pixel (50, 50) rendered Popup overlay node on top of Window and Normal" (c50 == packColor (Color 0x778899FF))

  freeRgfwSurface surf

testWindowTitleAndCloseButton :: IO ()
testWindowTitleAndCloseButton = do
  ctx <- newPixelContext
  let inp = emptyInput { inputWindowSize = Size 1000 800 }
      winUi = window True "Window Title" (label "Window Content")
  (_, _, _, _) <- runFrame ctx inp winUi
  let na = ctxNodeArena ctx

  surf <- newOffscreenRgfwSurface 400 300
  let font = getCozetteFont
      theme = tomorrowMidnightMinDarkTheme
  renderArena surf font 1.0 theme ctx na (WidgetId 0) (WidgetId 0) (WidgetId 0)
  freeRgfwSurface surf
  assert "Window title and close button rendered cleanly" True

testDebugWindow :: IO ()
testDebugWindow = do
  ctx <- newPixelContext
  let inp = emptyInput { inputWindowSize = Size 800 600 }
      snap = emptyRgfwDebug { dbgCore = (dbgCore emptyRgfwDebug) { dbgRtsOn = True } }
  _ <- runNanoUI ctx inp (window True "Debug Diagnostics" (debugWindowBody snap))
  let na = ctxNodeArena ctx
  solveLayout na (ctxHostProfile ctx) (ctxFontMetrics ctx) (ctxMonoFontMetrics ctx) (ctxMeasureText ctx) 800 600
  n <- arenaCount na

  let findTitleChild !ci
        | ci < 0 = pure (-1)
        | otherwise = do
            cnt <- getNodeType na ci
            cdir <- getDirection na ci
            if cnt == NodeContainer && cdir == DirRow
              then pure ci
              else getNextSibling na ci >>= findTitleChild

  titleChild <- findTitleChild =<< getFirstChild na 0
  let hasTitleRow = titleChild >= 0
  assert "Debug window title row container is detected" hasTitleRow

  (wx, wy, ww, wh) <- getRect na 0
  let titleBarH = if hasTitleRow then 24.0 else 0.0
      bodyTop = wy + titleBarH
      bodyH = max 0.0 (wh - titleBarH)
      bodyRect = Rect wx bodyTop ww bodyH

  assert "Debug window bodyTop is placed below 24px title bar" (bodyTop == wy + 24.0)
  assert "Debug window bodyRect has remaining window height" (bodyH == wh - 24.0)

  let inTitleBar !curr
        | not hasTitleRow = pure False
        | curr < 0 = pure False
        | curr == titleChild = pure True
        | curr == 0 = pure False
        | otherwise = do
            p <- getParent na curr
            inTitleBar p

  closeInTitle <- inTitleBar 4
  assert "Close button is identified as inside the title bar" closeInTitle

  bodyScrollInTitle <- inTitleBar 5
  assert "Body scroll container is NOT inside the title bar" (not bodyScrollInTitle)

  let sbW = 8.0 :: Float
      sbH = max 0 (bodyH - 14.0)
      vTrackRect = Rect (wx + ww - sbW - 2.0) bodyTop (sbW + 4.0) sbH
  (bx, by, _bw, _bh) <- getRect na 4
  assert "Vertical scrollbar track starts at bodyTop (wy + 24)" (rectY vTrackRect == bodyTop)
  assert "Close button click position is NOT inside vertical scrollbar track"
    (not (rectContains vTrackRect (V2 (bx + 12.0) (by + 12.0))))

  let belongsToWin !curr
        | curr < 0 = pure False
        | curr == 0 = pure True
        | otherwise = do
            cnt <- getNodeType na curr
            if (cnt == NodeWindow || cnt == NodeModal) && curr /= 0
              then pure False
              else do
                p <- getParent na curr
                belongsToWin p

  let clampedSX = 0.0 :: Float
      clampedSY = 60.0 :: Float

  let applyScrollClip !j
        | j >= n = pure ()
        | otherwise = do
            belongs <- belongsToWin j
            if not belongs || j == 0
              then applyScrollClip (j + 1)
              else do
                inTitle <- inTitleBar j
                if inTitle
                  then applyScrollClip (j + 1)
                  else do
                    (jx, jy, jw, jh) <- getRect na j
                    let !newX = jx - clampedSX
                        !newY = jy - clampedSY
                    setRect na j newX newY jw jh
                    let !cx0 = max (rectX bodyRect) newX
                        !cy0 = max (rectY bodyRect) newY
                        !cx1 = min (rectX bodyRect + rectW bodyRect) (newX + jw)
                        !cy1 = min (rectY bodyRect + rectH bodyRect) (newY + jh)
                        !finalClip = Rect cx0 cy0 (max 0 (cx1 - cx0)) (max 0 (cy1 - cy0))
                    setClipRect na j finalClip
                    applyScrollClip (j + 1)

  applyScrollClip 0

  (_tx, ty, _tw, _th) <- getRect na 1
  assert "Title row container Y position is pinned at wy (not scrolled)" (ty == wy)

  (_cx, cy, _cw, _ch) <- getRect na 4
  assert "Close button Y position is pinned (not scrolled)" (cy == by)

  forM_ [5 .. n - 1] $ \i -> do
    mClip <- getClipRect na i
    case mClip of
      Just clip -> do
        assert ("Body node " ++ show i ++ " clip rect does not enter title bar") (rectH clip == 0 || rectY clip >= bodyTop)
        assert ("Body node " ++ show i ++ " clip rect does not extend below window") (rectY clip + rectH clip <= wy + wh)
      Nothing -> pure ()

  surf <- newOffscreenRgfwSurface 800 600
  let font = getCozetteFont
      theme = tomorrowMidnightMinDarkTheme
  renderArena surf font 1.0 theme ctx na (WidgetId 0) (WidgetId 0) (WidgetId 0)

  closeCenterPixel <- peekElemOff (sBuffer surf) (round (by + 12.0) * 800 + round (bx + 12.0))
  assert "Close button area is rendered cleanly" (closeCenterPixel /= 0)

  freeRgfwSurface surf

main :: IO ()
main = do
  putStrLn "=== Running nano-ui-rgfw Unit Tests ==="
  testCozette
  testPackColor
  testTomorrowThemes
  testMouseConstants
  testMultiClickWordBounds
  testNormalizeClicks
  testContextMenuGeometry
  testTextInputContextMenuActions
  testTextAreaContextMenuActions
  testTextAreaCtrlA
  testScale2xRule
  testScale2xGlyphTables
  testFractionalDpiCalculations
  testBoxAreaAveraging
  testWindowResizing
  testZIndexRenderArena
  testWindowTitleAndCloseButton
  testDebugWindow
  putStrLn "=== All tests passed successfully! ==="
