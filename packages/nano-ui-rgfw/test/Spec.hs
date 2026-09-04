{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Data.Bits (shiftR, (.&.))
import System.Exit (exitFailure)

import qualified Data.IntMap.Strict as IM
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Text as T

import NanoUI
  ( colorRGBA
  , Direction (..)
  , Sizing (..)
  , Padding (..)
  , AlignX (..)
  , AlignY (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  )
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
import NanoUI.Frame.Window (resizeFromEdge)

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
import NanoUI.Testing (newPixelContext)
import NanoUI.Frame.TextEdit
  ( normalizeTextFieldClicks
  , textEditMenuRectAt
  , textEditMenuWidth
  , textWordBounds
  )
import NanoUI.Id (WidgetId (..))
import NanoUI.Layout.Arena
  ( NodeType (..)
  , newNodeArena
  , addNode
  , getRect
  , setGridCols
  , setNodeText
  , setWidgetId
  )
import NanoUI.Rgfw.Font.Cozette
  ( CozetteFont (..)
  , getCozetteFont
  , charToGlyphId
  , cozetteCharAdvance
  , cozetteLineHeight
  , cozetteMetrics
  , cozetteGlyphBit1x
  , cozetteGlyphBit2x
  , cozetteGlyphBit4x
  , scale2x
  , boxAverageCoverage
  )
import NanoUI.Rgfw.Layout (getContentHeight, getContentWidth, solveSinglePassLayout, solveSinglePassLayoutWith)
import NanoUI.Rgfw.Session (defaultRgfwOptions, detectWindowResizeEdge, optScale)
import NanoUI.Rgfw.Surface (packColor, toPhysRect)
import NanoUI.Rgfw.Theme
  ( RgfwTheme (..)
  , tomorrowMidnightMinDarkTheme
  , tomorrowMinLightTheme
  , tomorrowNightMinDarkTheme
  )
import NanoUI.Widgets.TextEdit (applyTextFieldMenuAction)
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

testHorizontalOverflowAndScroll :: IO ()
testHorizontalOverflowAndScroll = do
  na <- newNodeArena
  -- Root container: 400x300, Row, pad=10, gap=10
  root <- addNode na NodeContainer (-1) Row (Fixed 400) (Fixed 300) (Padding 10 10 10 10) 10 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Child 1: fixed 250x40
  c1 <- addNode na NodeButton root Column (Fixed 250) (Fixed 40) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  -- Child 2: fixed 300x40
  c2 <- addNode na NodeButton root Column (Fixed 300) (Fixed 40) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na 400 300

  (rx1, _, rw1, _) <- getRect na c1
  assert "Row Child 1 starts at x=10 with w=250" (rx1 == 10 && rw1 == 250)

  (rx2, _, rw2, _) <- getRect na c2
  -- Child 2 starts at x = 10 + 250 + 10 = 270 with w=300
  assert "Row Child 2 starts at x=270 with w=300" (rx2 == 270 && rw2 == 300)

  -- Total content right extent: 270 + 300 = 570
  contentW <- getContentWidth na
  assert "Total content width is 570px (> 400px viewport)" (contentW == 570)

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
  idx <- addNode (ctxNodeArena ctx) NodeTextInput (-1) Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
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
  idx <- addNode (ctxNodeArena ctx) NodeTextArea (-1) Column Fit Fit (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
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
  -- Pattern 1: Isolated single pixel in 3x3
  -- . . .
  -- . 1 .
  -- . . .
  let sampleIsolated x y = x == 1 && y == 1
      scaledIsolated = scale2x 3 3 sampleIsolated
  -- Center pixel at (1, 1) expands to 2x2 at (2, 2), (3, 2), (2, 3), (3, 3)
  -- Since B=0, H=0, D=0, F=0: B==H, so all E0..E3 = E (True)
  assert "Scale2x isolated dot expands to 2x2 block"
    (scaledIsolated 2 2 && scaledIsolated 3 2 && scaledIsolated 2 3 && scaledIsolated 3 3)
  assert "Scale2x isolated dot does not spill into neighbor pixels"
    (not (scaledIsolated 1 2) && not (scaledIsolated 4 2) && not (scaledIsolated 2 1) && not (scaledIsolated 2 4))

  -- Pattern 2: Corner smoothing where Scale2x rule actively activates:
  -- 0 1 0
  -- 1 1 0
  -- 0 0 0
  -- At center E=(1, 1): B=(1,0)=True, D=(0,1)=True, F=(2,1)=False, H=(1,2)=False
  -- B /= H (True /= False), D /= F (True /= False)
  -- E0 = (D == B ? D : E) = True
  -- E1 = (B == F ? F : E) = True == False ? False : True -> True
  -- E2 = (D == H ? D : E) = True == False ? True : True -> True
  -- E3 = (H == F ? F : E) = False == False ? False : True -> False!
  -- Thus bottom-right pixel E3 (at 3, 3) becomes False (smooths/rounds corner)!
  let sampleCorner x y = (x == 1 && y == 0) || (x == 0 && y == 1) || (x == 1 && y == 1)
      scaledCorner = scale2x 3 3 sampleCorner
  assert "Scale2x corner smoothing activates on E3 (rounds corner)"
    (scaledCorner 2 2 && scaledCorner 3 2 && scaledCorner 2 3 && not (scaledCorner 3 3))

testScale2xGlyphTables :: IO ()
testScale2xGlyphTables = do
  let font = getCozetteFont
      testGlyphs = [1, 2, 34, 36, 65, 95] -- ' ', '!', 'A', 'C', etc.
  -- Verify cfGlyphData2x precomputed table matches pure scale2x on all 14x26 bits for test glyphs
  forM_ testGlyphs $ \gid -> do
    let expectedBit2x = scale2x 7 13 (cozetteGlyphBit1x font gid)
        matches2x = and [ cozetteGlyphBit2x font gid x y == expectedBit2x x y
                        | y <- [0 .. 25]
                        , x <- [0 .. 13]
                        ]
    assert ("Scale2x 14x26 precomputed table matches pure scale2x for glyph " ++ show gid) matches2x

  -- Verify cfGlyphData4x precomputed table matches applying scale2x twice (28x52)
  forM_ testGlyphs $ \gid -> do
    let expectedBit4x = scale2x 14 26 (cozetteGlyphBit2x font gid)
        matches4x = and [ cozetteGlyphBit4x font gid x y == expectedBit4x x y
                        | y <- [0 .. 51]
                        , x <- [0 .. 27]
                        ]
    assert ("Scale4x 28x52 precomputed table matches double scale2x for glyph " ++ show gid) matches4x

testFractionalDpiCalculations :: IO ()
testFractionalDpiCalculations = do
  -- 1.5x scale
  let (px0, py0, pw0, ph0) = toPhysRect 1.5 0 0 100 50
  assert "toPhysRect 1.5x at origin" (px0 == 0 && py0 == 0 && pw0 == 150 && ph0 == 75)

  -- Fractional scale adjacent widgets: zero gap and zero overlap
  -- Widget 1 from x=0 to x=63.7; Widget 2 from x=63.7 to x=127.4
  let scale = 1.33 :: Float
      (w1_x0, _, w1_w, _) = toPhysRect scale 0 0 63.7 30
      (w2_x0, _, _, _) = toPhysRect scale 63.7 0 63.7 30
  assert "Adjacent widgets at fractional scale have zero gap/overlap" (w1_x0 + w1_w == w2_x0)

  -- Logical coordinate mapping for mouse and window
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
  -- 1. Full 1s input produces coverage 1.0
  let covAll1 = boxAverageCoverage 4 4 2 2 (\_ _ -> True) 0 0
  assert "Box averaging all-ones produces 1.0" (abs (covAll1 - 1.0) < 0.001)

  -- 2. Full 0s input produces coverage 0.0
  let covAll0 = boxAverageCoverage 4 4 2 2 (\_ _ -> False) 0 0
  assert "Box averaging all-zeros produces 0.0" (abs (covAll0 - 0.0) < 0.001)

  -- 3. Single pixel (0,0) in 2x2 downscaled to 1x1 produces 0.25
  let covSingle = boxAverageCoverage 2 2 1 1 (\x y -> x == 0 && y == 0) 0 0
  assert "Single pixel in 2x2 downscaled to 1x1 produces 0.25 coverage" (abs (covSingle - 0.25) < 0.001)

  -- 4. 2x2 block in 4x4 downscaled to 2x2 produces 1.0 for top-left, 0.0 for others
  let isTopLeft2x2 x y = x < 2 && y < 2
      covTL = boxAverageCoverage 4 4 2 2 isTopLeft2x2 0 0
      covTR = boxAverageCoverage 4 4 2 2 isTopLeft2x2 1 0
      covBL = boxAverageCoverage 4 4 2 2 isTopLeft2x2 0 1
      covBR = boxAverageCoverage 4 4 2 2 isTopLeft2x2 1 1
  assert "2x2 block top-left destination has coverage 1.0" (abs (covTL - 1.0) < 0.001)
  assert "2x2 block top-right destination has coverage 0.0" (abs (covTR - 0.0) < 0.001)
  assert "2x2 block bottom-left destination has coverage 0.0" (abs (covBL - 0.0) < 0.001)
  assert "2x2 block bottom-right destination has coverage 0.0" (abs (covBR - 0.0) < 0.001)

  -- 5. Non-integer fractional scale area preservation (3x3 to 2x2)
  -- Pixel at (1, 1) is set. It overlaps both X and Y splits evenly (0.5 in X, 0.5 in Y).
  -- So each of the 4 destination pixels gets 0.5 * 0.5 = 0.25 of source pixel area (1.0).
  -- Destination boxArea = 1.5 * 1.5 = 2.25.
  -- Each destination coverage = 0.25 / 2.25 = 1/9.
  -- Total area = 4 * 0.25 = 1.0 = total source area!
  let isCenter x y = x == 1 && y == 1
      cov00 = boxAverageCoverage 3 3 2 2 isCenter 0 0
      cov01 = boxAverageCoverage 3 3 2 2 isCenter 0 1
      cov10 = boxAverageCoverage 3 3 2 2 isCenter 1 0
      cov11 = boxAverageCoverage 3 3 2 2 isCenter 1 1
  assert "Fractional area split is symmetric across boundaries" (cov00 == cov01 && cov01 == cov10 && cov10 == cov11)
  let totalIntegratedArea = (cov00 + cov01 + cov10 + cov11) * 2.25
  assert "Continuous box averaging conserves total energy/area" (abs (totalIntegratedArea - 1.0) < 0.001)

testMultilineTextLayout :: IO ()
testMultilineTextLayout = do
  na <- newNodeArena
  root <- addNode na NodeContainer (-1) Column Fit Fit (Padding 0 0 0 0) 0 0 0 800 600 0 AlignStart AlignTop False
  txtNode <- addNode na NodeText root Column Fit Fit (Padding 0 0 0 0) 0 0 0 800 600 0 AlignStart AlignTop False
  setNodeText na txtNode "Line 1\nLine 2 is longer\nLine 3"
  solveSinglePassLayout na 800 600
  (_, _, tw, th) <- getRect na txtNode
  assert "Multiline text with 3 lines has height 39px (3 * 13)" (th == 39.0)
  assert "Multiline text width equals longest line width (16 * 6 = 96)" (tw == 96.0)

testFloatingWindowLayout :: IO ()
testFloatingWindowLayout = do
  na <- newNodeArena
  root <- addNode na NodeContainer (-1) Column Fit Fit (Padding 10 10 10 10) 8 0 0 1000 800 0 AlignStart AlignTop False
  btn <- addNode na NodeButton root Column Fit Fit (Padding 4 4 4 4) 0 0 0 1000 800 0 AlignStart AlignTop False
  setNodeText na btn "In-flow button"
  win <- addNode na NodeWindow root Column Fit Fit (Padding 8 8 8 8) 6 0 0 1000 800 0 AlignStart AlignTop False
  setWidgetId na win (WidgetId 42)
  setNodeText na win "Debug Window"
  solveSinglePassLayoutWith na 1000 800 (\_ -> pure Nothing) (\_ -> pure Nothing) (\_ -> pure Nothing)
  (winX, winY, winW, _) <- getRect na win
  contentH <- getContentHeight na
  assert "Floating window placed at default top-right" (abs (winX - (1000 - winW - 16)) < 1.0 && winY == 32.0)
  assert "Floating window does not inflate contentHeight" (contentH <= 50.0)

  let storedPos (WidgetId 42) = pure (Just (150.0, 250.0))
      storedPos _             = pure Nothing
  solveSinglePassLayoutWith na 1000 800 (\_ -> pure Nothing) storedPos (\_ -> pure Nothing)
  (draggedX, draggedY, _, _) <- getRect na win
  assert "Floating window placed at stored/dragged position (150, 250)" (draggedX == 150.0 && draggedY == 250.0)

testCompactContextMenuLayout :: IO ()
testCompactContextMenuLayout = do
  na <- newNodeArena
  root <- addNode na NodeContainer (-1) Column Fit Fit (Padding 0 0 0 0) 0 0 0 800 600 0 AlignStart AlignTop False
  popup <- addNode na NodePopup root Column Fit Fit (Padding 0 0 0 0) 0 0 0 800 600 0 AlignStart AlignTop False
  setWidgetId na popup (WidgetId 99)
  btn1 <- addNode na NodeButton popup Column Fit Fit (Padding 0 0 0 0) 0 0 0 800 600 0 AlignStart AlignTop False
  setNodeText na btn1 "Cut"
  btn2 <- addNode na NodeButton popup Column Fit Fit (Padding 0 0 0 0) 0 0 0 800 600 0 AlignStart AlignTop False
  setNodeText na btn2 "Copy"
  solveSinglePassLayout na 800 600
  (_, _, _, h1) <- getRect na btn1
  (_, _, _, h2) <- getRect na btn2
  assert "Context menu button inside popup has compact height 17px" (h1 == 17.0 && h2 == 17.0)

testWindowResizing :: IO ()
testWindowResizing = do
  let winRect = Rect 100 100 300 200
  -- 1. Test detectWindowResizeEdge
  assert "Bottom-right inner corner grip detects ResizeSE"
    (detectWindowResizeEdge winRect (V2 395 295) == Just ResizeSE)
  assert "Bottom-right outer halo detects ResizeSE"
    (detectWindowResizeEdge winRect (V2 404 304) == Just ResizeSE)
  assert "Bottom edge detects ResizeS"
    (detectWindowResizeEdge winRect (V2 250 298) == Just ResizeS)
  assert "Right edge detects ResizeE"
    (detectWindowResizeEdge winRect (V2 398 200) == Just ResizeE)
  assert "Left edge detects ResizeW"
    (detectWindowResizeEdge winRect (V2 98 200) == Just ResizeW)
  assert "Top edge outer halo detects ResizeN"
    (detectWindowResizeEdge winRect (V2 250 95) == Just ResizeN)
  assert "Top-left corner detects ResizeNW"
    (detectWindowResizeEdge winRect (V2 96 96) == Just ResizeNW)
  assert "Top-right corner detects ResizeNE"
    (detectWindowResizeEdge winRect (V2 404 96) == Just ResizeNE)
  assert "Close button area does NOT trigger resize"
    (detectWindowResizeEdge winRect (V2 390 110) == Nothing)
  assert "Window interior does NOT trigger resize"
    (detectWindowResizeEdge winRect (V2 200 200) == Nothing)
  assert "Far outside halo does NOT trigger resize"
    (detectWindowResizeEdge winRect (V2 500 500) == Nothing)

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

  -- 3. Test single pass layout with resized window size
  na <- newNodeArena
  root <- addNode na NodeContainer (-1) Column Fit Fit (Padding 0 0 0 0) 0 0 0 1000 800 0 AlignStart AlignTop False
  win <- addNode na NodeWindow root Column Fit Fit (Padding 8 8 8 8) 6 0 0 1000 800 0 AlignStart AlignTop False
  setWidgetId na win (WidgetId 42)
  setNodeText na win "Resizable Window"
  let storedSz (WidgetId 42) = pure (Just (450.0, 320.0))
      storedSz _             = pure Nothing
      storedPos (WidgetId 42) = pure (Just (80.0, 120.0))
      storedPos _             = pure Nothing
  solveSinglePassLayoutWith na 1000 800 (\_ -> pure Nothing) storedPos storedSz
  (rx, ry, rw, rh) <- getRect na win
  assert "Resized floating window placed at stored width 450" (rw == 450.0)
  assert "Resized floating window placed at stored height 320" (rh == 320.0)
  assert "Resized floating window placed at stored X 80" (rx == 80.0)
  assert "Resized floating window placed at stored Y 120" (ry == 120.0)

testGridLayout :: IO ()
testGridLayout = do
  -- 1. Uniform 2-column Grid
  na1 <- newNodeArena
  grid1 <- addNode na1 NodeContainer (-1) Row (Fixed 600) (Fixed 400) (Padding 10 10 10 10) 10 0 0 1e9 1e9 0 AlignStart AlignTop False
  setGridCols na1 grid1 2
  c0 <- addNode na1 NodeButton grid1 Column (Fixed 285) (Fixed 40) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  c1 <- addNode na1 NodeButton grid1 Column (Fixed 285) (Fixed 50) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  c2 <- addNode na1 NodeButton grid1 Column (Fixed 285) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  c3 <- addNode na1 NodeButton grid1 Column (Fixed 285) (Fixed 60) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na1 600 400

  r0 <- getRect na1 c0
  r1 <- getRect na1 c1
  r2 <- getRect na1 c2
  r3 <- getRect na1 c3

  assert "Grid 2-col child 0 at (10, 10, 285, 40)" (r0 == (10, 10, 285, 40))
  assert "Grid 2-col child 1 at (305, 10, 285, 50)" (r1 == (305, 10, 285, 50))
  assert "Grid 2-col child 2 at (10, 70, 285, 30)" (r2 == (10, 70, 285, 30))
  assert "Grid 2-col child 3 at (305, 70, 285, 60)" (r3 == (305, 70, 285, 60))

  -- 2. Mixed Fixed + Grow columns (2-column layout)
  na2 <- newNodeArena
  grid2 <- addNode na2 NodeContainer (-1) Row (Fixed 600) (Fixed 400) (Padding 0 0 0 0) 16 0 0 1e9 1e9 0 AlignStart AlignTop False
  setGridCols na2 grid2 2
  leftP <- addNode na2 NodePanel grid2 Column (Fixed 200) (Grow 1.0) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  rightP <- addNode na2 NodePanel grid2 Column (Grow 1.0) (Grow 1.0) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na2 600 400

  (gx0, _, gw0, gh0) <- getRect na2 leftP
  (gx1, _, gw1, gh1) <- getRect na2 rightP
  assert "Grid left panel starts at x=0, w=200, h=400" (gx0 == 0 && gw0 == 200 && gh0 == 400)
  assert "Grid right panel starts at x=216, w=384, h=400" (gx1 == 216 && gw1 == 384 && gh1 == 400)
  assert "Grid panels span parent width (216 + 384 == 600)" (gx1 + gw1 == 600)

  -- 3. 3-column multi-row grid (3x3 items)
  na3 <- newNodeArena
  grid3 <- addNode na3 NodeContainer (-1) Row (Fixed 320) (Fixed 200) (Padding 10 10 10 10) 10 0 0 1e9 1e9 0 AlignStart AlignTop False
  setGridCols na3 grid3 3
  k0 <- addNode na3 NodeButton grid3 Column (Fixed 80) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k1 <- addNode na3 NodeButton grid3 Column (Fixed 100) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k2 <- addNode na3 NodeButton grid3 Column (Fixed 100) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k3 <- addNode na3 NodeButton grid3 Column (Fixed 80) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k4 <- addNode na3 NodeButton grid3 Column (Fixed 100) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k5 <- addNode na3 NodeButton grid3 Column (Fixed 100) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k6 <- addNode na3 NodeButton grid3 Column (Fixed 80) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k7 <- addNode na3 NodeButton grid3 Column (Fixed 100) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  k8 <- addNode na3 NodeButton grid3 Column (Fixed 100) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na3 320 200

  rk0 <- getRect na3 k0
  rk1 <- getRect na3 k1
  rk2 <- getRect na3 k2
  rk3 <- getRect na3 k3
  rk4 <- getRect na3 k4
  rk5 <- getRect na3 k5
  rk6 <- getRect na3 k6
  rk7 <- getRect na3 k7
  rk8 <- getRect na3 k8

  assert "Grid 3x3 item (0,0) at (10, 10, 80, 30)" (rk0 == (10, 10, 80, 30))
  assert "Grid 3x3 item (1,0) at (100, 10, 100, 30)" (rk1 == (100, 10, 100, 30))
  assert "Grid 3x3 item (2,0) at (210, 10, 100, 30)" (rk2 == (210, 10, 100, 30))
  assert "Grid 3x3 item (0,1) at (10, 50, 80, 30)" (rk3 == (10, 50, 80, 30))
  assert "Grid 3x3 item (1,1) at (100, 50, 100, 30)" (rk4 == (100, 50, 100, 30))
  assert "Grid 3x3 item (2,1) at (210, 50, 100, 30)" (rk5 == (210, 50, 100, 30))
  assert "Grid 3x3 item (0,2) at (10, 90, 80, 30)" (rk6 == (10, 90, 80, 30))
  assert "Grid 3x3 item (1,2) at (100, 90, 100, 30)" (rk7 == (100, 90, 100, 30))
  assert "Grid 3x3 item (2,2) at (210, 90, 100, 30)" (rk8 == (210, 90, 100, 30))

  -- 4. Grid Pass 1 intrinsic size calculation (nested in column)
  na4 <- newNodeArena
  root4 <- addNode na4 NodeContainer (-1) Column (Fixed 800) (Fixed 600) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  grid4 <- addNode na4 NodeContainer root4 Row Fit Fit (Padding 5 5 5 5) 6 0 0 1e9 1e9 0 AlignStart AlignTop False
  setGridCols na4 grid4 2
  _ <- addNode na4 NodeButton grid4 Column (Fixed 70) (Fixed 20) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  _ <- addNode na4 NodeButton grid4 Column (Fixed 90) (Fixed 40) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  _ <- addNode na4 NodeButton grid4 Column (Fixed 60) (Fixed 30) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False
  _ <- addNode na4 NodeButton grid4 Column (Fixed 80) (Fixed 25) (Padding 0 0 0 0) 0 0 0 1e9 1e9 0 AlignStart AlignTop False

  solveSinglePassLayout na4 800 600

  rGrid4 <- getRect na4 grid4
  assert "Grid intrinsic content rect is (0, 0, 176, 86)" (rGrid4 == (0, 0, 176, 86))

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
  testHorizontalOverflowAndScroll
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
  testMultilineTextLayout
  testFloatingWindowLayout
  testCompactContextMenuLayout
  testWindowResizing
  testGridLayout
  putStrLn "=== All tests passed successfully! ==="
