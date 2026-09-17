{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_)
import Data.Bits ((.&.))
import System.Exit (exitFailure)

import Data.Primitive.PrimArray (mapPrimArray)
import NanoUI.Layout.Arena
  ( NodeType (..)
  , arenaCount
  , getNodeType
  , getRect
  )
import NanoUI
  ( DrawOp (..)
  , Rect (..)
  , Size (..)
  , V2 (..)
  , box
  , button
  , checkbox
  , colorRGBA
  , column
  , drawing
  , fixedWH
  , grow
  , label
  , Style (..)
  , Theme (..)
  , tomorrowNightMinDarkTheme
  , window
  )
import Foreign.Marshal.Alloc (allocaBytes, callocBytes, free)
import Foreign.Storable (peekByteOff, peekElemOff)
import NanoUI.Input (Input (..), Modifiers (..), emptyInput)
import NanoUI.Context (Context (..))
import NanoUI.Testing
  ( DrawCmd (..)
  , DrawData (..)
  , collectRasterSpans
  , newPixelContext
  , runFrame
  )
import NanoUI.Rgfw.Font.Cozette
  ( CozetteFont (..)
  , charToGlyphId
  , cozetteGlyphBit1x
  , cozetteGlyphBit2x
  , cozetteGlyphBit4x
  , getCozetteFont
  , renderGlyphScaledToBuffer
  )
import NanoUI.Rgfw.Context (newRgfwContext)
import NanoUI.Rgfw.Gl (GlyphAtlas (..), atlasCell, bakeGlyphAtlas, glyphAtlasFor, toPhysRect, writeSpanQuads)
import NanoUI.Rgfw.Render (renderArena)
import NanoUI.Rgfw.Session (applyRgfwEvent, decodeRgfwEvents)
import RGFW (Event (..))
import NanoUI.Rgfw.Surface
  ( clearScreen
  , fillRect
  , freeRgfwSurface
  , newOffscreenRgfwSurface
  , packColor
  , sBuffer
  , sWidth
  , sHeight
  )
import qualified RGFW.Raw as R

assert :: String -> Bool -> IO ()
assert name True  = putStrLn ("[PASS] " ++ name)
assert name False = do
  putStrLn ("[FAIL] " ++ name)
  exitFailure

testPackColor :: IO ()
testPackColor =
  assert "Color byte packing is ARGB" (packColor (colorRGBA 0x12 0x34 0x56 0x78) == 0x78123456)

testSurfaceAllocation :: IO ()
testSurfaceAllocation = do
  bracket (newOffscreenRgfwSurface 0 (-1)) freeRgfwSurface $ \surface -> do
    assert "Empty surface dimensions produce a usable pixel" (sWidth surface == 1 && sHeight surface == 1)
    clearScreen surface 0x12345678
    pixel <- peekElemOff (sBuffer surface) 0
    assert "Minimum surface buffer can be cleared" (pixel == 0x12345678)
  -- This product wraps to zero on machine Int arithmetic. Reject it before
  -- allocating a tiny buffer for what claims to be a huge surface.
  oversized <- try (bracket
    (newOffscreenRgfwSurface (maxBound `div` 2 + 1) 2)
    freeRgfwSurface
    (const (pure ()))) :: IO (Either IOException ())
  assert "Surface dimensions cannot overflow the buffer allocation" (either (const True) (const False) oversized)
  -- Odd row strides and short spans exercise the shared fill kernel's aligned
  -- pairs and scalar tails. Clearing must cover the entire buffer exactly.
  forM_ [1 .. 19] $ \w ->
    bracket (newOffscreenRgfwSurface w 3) freeRgfwSurface $ \surface -> do
      clearScreen surface 0x12345678
      fillRect surface 1 1 (max 0 (w - 2)) 1 0xABCDEF01
      pixels <- mapM (peekElemOff (sBuffer surface)) [0 .. w * 3 - 1]
      let expected =
            [ if y == 1 && x >= 1 && x < w - 1 then 0xABCDEF01 else 0x12345678
            | y <- [0 .. 2 :: Int], x <- [0 .. w - 1]
            ]
      assert ("Shared pixel fill handles stride " ++ show w) (pixels == expected)

-- | Reference EPX (Scale2x). Given width W, height H and a pixel query
-- (col -> row -> Bool), returns the query for the 2W x 2H result.
scale2x :: Int -> Int -> (Int -> Int -> Bool) -> (Int -> Int -> Bool)
scale2x !w !h getPixel = \ !c2 !r2 ->
  if c2 < 0 || c2 >= w * 2 || r2 < 0 || r2 >= h * 2
    then False
    else
      let !c = c2 `div` 2
          !r = r2 `div` 2
          at x y = x >= 0 && x < w && y >= 0 && y < h && getPixel x y
          above = at c (r - 1)
          left = at (c - 1) r
          e = getPixel c r
          right = at (c + 1) r
          below = at c (r + 1)
          (tl, tr, bl, br)
            | above /= below && left /= right =
                ( if left == above then left else e
                , if above == right then right else e
                , if left == below then left else e
                , if below == right then right else e
                )
            | otherwise = (e, e, e, e)
       in case (c2 .&. 1, r2 .&. 1) of
            (0, 0) -> tl
            (1, 0) -> tr
            (0, 1) -> bl
            _ -> br

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

testZOrderRenderArena :: IO ()
testZOrderRenderArena = do
  ctx <- newPixelContext
  let inp = emptyInput {inputWindowSize = Size 100 100}
      boxCol = colorRGBA 0x11 0x22 0x33 255
      ui = do
        box grow boxCol
        window True "Z" (label "hi")
  (_, _, draw, _) <- runFrame ctx inp ui
  (baseSpans, overlaySpans) <- collectRasterSpans ctx inp

  surf <- newOffscreenRgfwSurface 100 100
  clearScreen surf 0
  renderArena surf (getCozetteFont) 1.0 draw baseSpans overlaySpans

  -- placeWindows pins floating windows to the top-right corner; probe its
  -- title bar center and a bottom-left box pixel far from the window.
  let na = ctxNodeArena ctx
  nNodes <- arenaCount na
  let findWindow !i
        | i >= nNodes = pure Nothing
        | otherwise = do
            nt <- getNodeType na i
            if nt == NodeWindow
              then do
                (x, y, w, h) <- getRect na i
                pure (Just (x, y, w, h))
              else findWindow (i + 1)
  mWin <- findWindow 0
  assert "Overlay text spans collected for floating window chrome" (not (null overlaySpans))
  case mWin of
    Nothing -> assert "Floating window node present in arena" False
    Just (wx, wy, ww, _wh) -> do
      let !winProbeX = round (wx + ww / 2)
          !winProbeY = round wy + 4
      cWin <- peekElemOff (sBuffer surf) (winProbeY * 100 + winProbeX)
      assert "Window paints above in-flow content" (cWin /= 0 && cWin /= packColor boxCol)
      cBox <- peekElemOff (sBuffer surf) (95 * 100 + 5)
      assert "In-flow box painted beneath the window layer" (cBox == packColor boxCol)

  freeRgfwSurface surf

-- Check coverage and clipping pixel-for-pixel, including empty iteration
-- bounds and reversed winding in the numeric raster loops.
testTriangleRaster :: IO ()
testTriangleRaster =
  bracket (newOffscreenRgfwSurface 8 8) freeRgfwSurface $ \surf -> do
    let full = Rect 0 0 8 8
        clipped = Rect 2 0 3 4
        triangle = (1, 1, 5, 1, 1, 5)
        reversed = (1, 5, 5, 1, 1, 1)
        inside x y = x >= 1 && y >= 1 && x + y <= 5
        red = packColor (colorRGBA 255 0 0 255)
        cases =
          [ ("normal", full, triangle, inside)
          , ("reversed", full, reversed, inside)
          , ("clipped", clipped, triangle, \x y -> inside x y && x >= 2 && x < 5 && y < 4)
          , ("outside", Rect 6 6 2 2, triangle, \_ _ -> False)
          , ("empty clip", Rect 0 0 0 0, triangle, \_ _ -> False)
          , ("degenerate", full, (1, 1, 3, 3, 5, 5), \_ _ -> False)
          ]
    forM_ cases $ \(name, clip, (ax, ay, bx, by, cx, cy), covered) -> do
      clearScreen surf 0
      ctx <- newPixelContext
      (_, _, draw, _) <- runFrame ctx (emptyInput {inputWindowSize = Size 8 8}) $
        drawing (fixedWH 8 8) $ \_ ->
          pure (FillTriangle ax ay bx by cx cy (colorRGBA 255 0 0 255))
      let Rect clipX clipY clipW clipH = clip
          clippedDraw = draw
            { drawCommands = mapPrimArray (\cmd -> cmd {cmdClipX = clipX, cmdClipY = clipY, cmdClipW = clipW, cmdClipH = clipH}) (drawCommands draw)
            }
      renderArena surf getCozetteFont 1 clippedDraw [] []
      pixels <- mapM (peekElemOff (sBuffer surf)) [0 .. 63]
      let expected = [if covered x y then red else 0 | y <- [0 .. 7 :: Int], x <- [0 .. 7]]
      assert ("triangle raster " ++ name) (pixels == expected)

-- | An RGFW context renders square, themed widgets: button corners are the
-- border colour, fills come from the theme, and label text is stamped
-- glyphs rather than solid per-character boxes.
testSquareThemedRaster :: IO ()
testSquareThemedRaster = do
  let theme = tomorrowNightMinDarkTheme
      w = 240
      h = 120
      inp = emptyInput {inputWindowSize = Size (fromIntegral w) (fromIntegral h), inputMousePos = V2 (-100) (-100)}
      ui = column $ do
        _ <- button "Button"
        _ <- checkbox "Checkbox label" True
        pure ()
  ctx <- newRgfwContext theme
  (_, _, draw, _) <- runFrame ctx inp ui
  (baseSpans, overlaySpans) <- collectRasterSpans ctx inp
  surf <- newOffscreenRgfwSurface w h
  clearScreen surf (packColor (themeWindow theme))
  renderArena surf getCozetteFont 1.0 draw baseSpans overlaySpans
  let na = ctxNodeArena ctx
      pixel x y = peekElemOff (sBuffer surf) (y * w + x)
  n <- arenaCount na
  rects <- mapM (\i -> (,) <$> getNodeType na i <*> getRect na i) [0 .. n - 1]
  case [r | (NodeButton, r) <- rects] of
    ((bx, by, bw, bh) : _) -> do
      let x0 = round bx
          y0 = round by
          x1 = round (bx + bw) - 1
          y1 = round (by + bh) - 1
      corners <- mapM (uncurry pixel) [(x0, y0), (x1, y0), (x0, y1), (x1, y1)]
      assert "Button corners are square (border colour)" (all (== packColor (styleBorder (themeButton theme))) corners)
      fillPx <- pixel (x0 + 2) (y0 + 2)
      assert "Button fill uses the theme" (fillPx == packColor (styleBg (themeButton theme)))
    [] -> assert "Button node present" False
  case [(r, fg) | (r, t, fg, _, _) <- baseSpans, t == "Checkbox label"] of
    ((Rect sx sy sw sh, fg) : _) -> do
      px <- sequence [pixel x y | y <- [round sy .. round (sy + sh) - 1], x <- [round sx .. round (sx + sw) - 1]]
      let lit = length (filter (== packColor fg) px)
      assert "Checkbox label is glyphs, not solid boxes" (lit > 0 && lit * 2 < length px)
    [] -> assert "Checkbox label span collected" False
  freeRgfwSurface surf

-- | The OpenGL glyph atlas holds every glyph exactly as the software blitter
-- stamps it at that scale: the grid fits the font, and a cell equals a
-- standalone render with nothing bleeding in from its neighbours.
testGlyphAtlas :: IO ()
testGlyphAtlas = do
  let font = getCozetteFont
  forM_ [1.0, 1.5, 2.0, 3.0, 4.0] $ \scale -> do
    let ga = glyphAtlasFor font scale
        cw = gaCellW ga
        ch = gaCellH ga
        label' = " at scale " ++ show scale
    assert ("glyph atlas grid fits every glyph" ++ label')
      (gaCols ga * (gaHeight ga `div` ch) >= cfNumGlyphs font && gaWidth ga == gaCols ga * cw)
    bakeGlyphAtlas font ga $ \atlas ->
      forM_ ("A@#|" :: String) $ \c -> do
        let gid = fromIntegral (charToGlyphId font c)
            (ax, ay) = atlasCell ga gid
        bracket (callocBytes (cw * ch * 4)) free $ \solo -> do
          renderGlyphScaledToBuffer solo cw 0 0 cw ch scale 0 0 0xFFFFFFFF font (fromIntegral gid)
          cell <- mapM (\(x, y) -> peekElemOff atlas ((ay + y) * gaWidth ga + ax + x)) [(x, y) | y <- [0 .. ch - 1], x <- [0 .. cw - 1]]
          expected <- mapM (peekElemOff solo) [0 .. cw * ch - 1]
          assert ("glyph atlas cell " ++ show c ++ " matches the blitter" ++ label')
            (cell == expected && any (/= 0) expected)

-- | Span glyph quads land on the software pen positions (spaces skipped,
-- newlines reset the column) and clipping trims positions and UVs together.
testSpanQuads :: IO ()
testSpanQuads = do
  let font = getCozetteFont
      ga = glyphAtlasFor font 2.0
      red = colorRGBA 255 0 0 255
      aw = fromIntegral (gaWidth ga) :: Float
      ah = fromIntegral (gaHeight ga) :: Float
      (ax, ay) = atlasCell ga (fromIntegral (charToGlyphId font 'A'))
      quads txt clip = allocaBytes (64 * 32) $ \buf -> do
        n <- writeSpanQuads ga font 200 100 buf 0 (Rect 10 5 0 0, txt, red, red, clip)
        vs <- mapM (\i -> mapM (\o -> peekByteOff buf (i * 32 + o)) [0, 4, 24, 28]) [0 .. n - 1]
        pure (n, vs :: [[Float]])
  (n1, v1) <- quads "A B\nC" (Rect 0 0 100 50)
  assert "span quads: one quad per visible glyph" (n1 == 18)
  assert "span quads: first glyph at the scaled pen" (take 1 v1 == [[20, 10, fromIntegral ax / aw, fromIntegral ay / ah]])
  assert "span quads: newline resets the column" (map (take 2) (take 1 (drop 12 v1)) == [[20, 36]])
  (n2, v2) <- quads "A" (Rect 12 0 100 50)
  assert "span quads: clip trims position and UV"
    (n2 == 6 && take 1 v2 == [[24, 10, fromIntegral (ax + 4) / aw, fromIntegral ay / ah]])
  (n3, _) <- quads "A" (Rect 150 80 10 10)
  assert "span quads: clip outside the framebuffer emits nothing" (n3 == 0)

-- | RGFW keyboard translation: repeated letters all type, and one Ctrl+letter
-- keystroke types its letter once, whichever of its key-char and key-press
-- events RGFW queues first.
testRgfwTyping :: IO ()
testRgfwTyping = do
  let typed = foldl' applyRgfwEvent emptyInput . decodeRgfwEvents 1
      chars = inputChars . typed
      ctrlHeld = modCtrl . inputModifiers . typed
      keyL = fromIntegral (fromEnum 'l')
      plainL = [EventKeyChar 'l', EventKeyPress keyL 0]
      ctrlLCharFirst = [EventKeyChar '\x0c', EventKeyPress keyL R.rgfw_modControl]
      ctrlLPressFirst = [EventKeyPress keyL R.rgfw_modControl, EventKeyChar '\x0c']
  assert "RGFW typing: repeated key-char events all type" (chars [EventKeyChar 'l', EventKeyChar 'l'] == "ll")
  assert "RGFW typing: repeated keystrokes all type" (chars (plainL ++ plainL) == "ll")
  assert "RGFW typing: Ctrl+L queued char-first types once" (chars ctrlLCharFirst == "l" && ctrlHeld ctrlLCharFirst)
  assert "RGFW typing: Ctrl+L queued press-first types once" (chars ctrlLPressFirst == "l" && ctrlHeld ctrlLPressFirst)
  assert "RGFW typing: a Ctrl+L press without a char types" (chars [EventKeyPress keyL R.rgfw_modControl] == "l")
  assert "RGFW typing: Ctrl+L twice types twice"
    (chars (ctrlLCharFirst ++ ctrlLCharFirst) == "ll" && chars (ctrlLPressFirst ++ ctrlLPressFirst) == "ll")

-- | Wheel events queued in one batch add up rather than keeping the last.
testRgfwScroll :: IO ()
testRgfwScroll = do
  let scrolled = inputScroll (foldl' applyRgfwEvent emptyInput (decodeRgfwEvents 1 [EventMouseScroll 0 1, EventMouseScroll 0.5 2]))
  assert "RGFW scroll: a batch of wheel events accumulates" (scrolled == V2 0.5 3)

main :: IO ()
main = do
  putStrLn "=== Running nano-ui-rgfw Unit Tests ==="
  testRgfwTyping
  testRgfwScroll
  testPackColor
  testSurfaceAllocation
  testScale2xGlyphTables
  testFractionalDpiCalculations
  testZOrderRenderArena
  testTriangleRaster
  testSquareThemedRaster
  testGlyphAtlas
  testSpanQuads
  putStrLn "=== All tests passed successfully! ==="
