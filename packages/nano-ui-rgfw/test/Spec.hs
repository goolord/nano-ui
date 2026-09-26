module Main (main) where

import Control.Exception (IOException, bracket, try)
import Control.Monad (forM_)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List (nub)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word32)
import Foreign.Marshal.Alloc (allocaBytes, callocBytes, free)
import Foreign.Storable (peekByteOff, peekElemOff)
import NanoUI
  ( DrawOp (..), ImageConfig (..), ImageId (..), NanoUI, Rect (..), Rotation (..), Style (..), Theme (..)
  , UiCursorKind (..), V2 (..), box, button, checkbox, colorRGBA, column, defaultImageConfig, defaultLayout, drawing
  , fixedWH, grow, imageConfigured', label, respRect, tomorrowNightMinDarkTheme, window
  )
import NanoUI.Input (Input (..), Key (..), Modifiers (..), MouseButton (..), buttonHeld, buttonPressed, buttonReleased, buttonsFromList, buttonsToList, emptyInput, noModifiers)
import NanoUI.Internal.Context (Context (..), setDrawSquareGeometry)
import NanoUI.Internal.Layout.Arena (NodeType (..), arenaCount, getNodeRect, getNodeType)
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette (CozetteFont (..), charToGlyphId, cozetteGlyphBit, getCozetteFont, renderGlyphScaledToBuffer)
import NanoUI.Rgfw.Internal.Gl (GlyphAtlas (..), atlasCell, bakeGlyphAtlas, glyphAtlasFor, toPhysRect, writeSpanQuads)
import NanoUI.Rgfw.Internal.Session (applyRgfwEvent, decodeRgfwEvents, mapRgfwCursor)
import NanoUI.Rgfw.Render (renderArena)
import NanoUI.Rgfw.Surface (clearScreen, fillRect, freeRgfwSurface, newOffscreenRgfwSurface, packColor, sBuffer, sHeight, sWidth)
import NanoUI.Rgfw.Wake (testRgfwWake)
import NanoUI.Rgfw.Window (testGlWindow, testSessionWindow)
import NanoUI.Testing (DrawCmd (..), DrawData (..), collectRasterSpans, newPixelContext, registerImage, runFrame)
import NanoUI.Testing.Assert (run2Frames, withInput)
import NanoUI.Testing.Harness (DemoSpan, withInputOff)
import RGFW (Event (..))
import RGFW.Raw qualified as R
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert name True = putStrLn ("[PASS] " ++ name)
assert name False = putStrLn ("[FAIL] " ++ name) >> exitFailure

-- | Draw a view's second frame, with the pointer off the window, onto a
-- software surface of the window's size cleared to @bg@; then look at it.
raster :: Context -> Int -> Int -> Word32 -> NanoUI a -> (a -> ([DemoSpan], [DemoSpan]) -> (Int -> Int -> IO Word32) -> IO b) -> IO b
raster ctx w h bg ui k = do
  let inp = withInputOff (fromIntegral w) (fromIntegral h)
  (a, _, draw, _) <- run2Frames ctx inp ui
  (base, overlay) <- collectRasterSpans ctx inp
  bracket (newOffscreenRgfwSurface w h) freeRgfwSurface $ \surf -> do
    clearScreen surf bg
    renderArena surf getCozetteFont 1 draw base overlay
    k a (base, overlay) (\x y -> peekElemOff (sBuffer surf) (y * w + x))

-- | The rects of the frame's layout nodes of a type, in arena order.
nodesOf :: NodeType -> Context -> IO [Rect]
nodesOf t ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  map snd . filter ((== t) . fst) <$> mapM (\i -> (,) <$> getNodeType na i <*> getNodeRect na i) [0 .. n - 1]

-- | The input a batch of RGFW events adds up to.
applied :: [Event] -> Input
applied = foldl' applyRgfwEvent emptyInput . decodeRgfwEvents 1

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
  oversized <- try (bracket (newOffscreenRgfwSurface (maxBound `div` 2 + 1) 2) freeRgfwSurface (const (pure ())))
  assert "Surface dimensions cannot overflow the buffer allocation" (isLeft (oversized :: Either IOException ()))
  -- Odd row strides and short spans exercise the shared fill kernel's aligned
  -- pairs and scalar tails. Clearing must cover the entire buffer exactly.
  forM_ [1 .. 19] $ \w ->
    bracket (newOffscreenRgfwSurface w 3) freeRgfwSurface $ \surface -> do
      clearScreen surface 0x12345678
      fillRect surface 1 1 (max 0 (w - 2)) 1 0xABCDEF01
      pixels <- mapM (peekElemOff (sBuffer surface)) [0 .. w * 3 - 1]
      let expected = [if y == 1 && x >= 1 && x < w - 1 then 0xABCDEF01 else 0x12345678 | y <- [0 .. 2 :: Int], x <- [0 .. w - 1]]
      assert ("Shared pixel fill handles stride " ++ show w) (pixels == expected)

-- | Reference EPX (Scale2x): given width W, height H and a pixel query
-- (col -> row -> Bool), the query for the 2W x 2H result. Where a pixel's
-- opposite neighbours differ both ways, a quarter takes the value its nearer
-- vertical and horizontal neighbours share; otherwise it is the pixel's.
scale2x :: Int -> Int -> (Int -> Int -> Bool) -> Int -> Int -> Bool
scale2x w h get c2 r2
  | c2 < 0 || c2 >= w * 2 || r2 < 0 || r2 >= h * 2 = False
  | above /= below && left /= right && vertical == horizontal = vertical
  | otherwise = get c r
  where
    (c, r) = (c2 `div` 2, r2 `div` 2)
    at x y = x >= 0 && x < w && y >= 0 && y < h && get x y
    (above, below, left, right) = (at c (r - 1), at c (r + 1), at (c - 1) r, at (c + 1) r)
    vertical = if even r2 then above else below
    horizontal = if even c2 then left else right

-- | The 14x26 table is Scale2x of the 7x13 glyphs, and the 28x52 one Scale2x
-- of the 14x26 table.
testScale2xGlyphTables :: IO ()
testScale2xGlyphTables =
  forM_ [(2, 1, 7, 13), (4, 2, 14, 26)] $ \(scale, from, w, h) -> forM_ [1, 2, 34, 36, 65, 95] $ \gid ->
    assert ("Scale" ++ show scale ++ "x precomputed table matches scale2x for glyph " ++ show gid) $
      and [cozetteGlyphBit font scale gid x y == scale2x w h (cozetteGlyphBit font from gid) x y | y <- [0 .. 2 * h - 1], x <- [0 .. 2 * w - 1]]
  where
    font = getCozetteFont

testFractionalDpiCalculations :: IO ()
testFractionalDpiCalculations = do
  assert "toPhysRect 1.5x at origin" (toPhysRect 1.5 0 0 100 50 == (0, 0, 150, 75))
  let (w1_x0, _, w1_w, _) = toPhysRect 1.33 0 0 63.7 30
      (w2_x0, _, _, _) = toPhysRect 1.33 63.7 0 63.7 30
  assert "Adjacent widgets at fractional scale have zero gap/overlap" (w1_x0 + w1_w == w2_x0)

-- | A floating window, which placeFloatingNodes pins to the top-right
-- corner, paints over in-flow content: probe its title bar centre and a
-- bottom-left box pixel far from it.
testZOrderRenderArena :: IO ()
testZOrderRenderArena = do
  ctx <- newPixelContext
  let boxCol = colorRGBA 0x11 0x22 0x33 255
  raster ctx 100 100 0 (box grow boxCol >> window True "Z" (label "hi")) $ \_ (_, overlay) px -> do
    assert "Overlay text spans collected for floating window chrome" (not (null overlay))
    nodesOf NodeWindow ctx >>= \case
      Rect wx wy ww _ : _ -> do
        cWin <- px (round (wx + ww / 2)) (round wy + 4)
        assert "Window paints above in-flow content" (cWin /= 0 && cWin /= packColor boxCol)
      [] -> assert "Floating window node present in arena" False
    cBox <- px 5 95
    assert "In-flow box painted beneath the window layer" (cBox == packColor boxCol)

-- Check coverage and clipping pixel-for-pixel, including empty iteration
-- bounds and reversed winding in the numeric raster loops. Square geometry
-- leaves out the triangle's anti-aliased fringe, which this rasteriser would
-- fill flat in its corners' average colour.
testTriangleRaster :: IO ()
testTriangleRaster =
  bracket (newOffscreenRgfwSurface 8 8) freeRgfwSurface $ \surf -> do
    let full = Rect 0 0 8 8
        triangle = (1, 1, 5, 1, 1, 5)
        inside x y = x >= 1 && y >= 1 && x + y <= 5
        red = colorRGBA 255 0 0 255
        cases =
          [ ("normal", full, triangle, inside)
          , ("reversed", full, (1, 5, 5, 1, 1, 1), inside)
          , ("clipped", Rect 2 0 3 4, triangle, \x y -> inside x y && x >= 2 && x < 5 && y < 4)
          , ("outside", Rect 6 6 2 2, triangle, \_ _ -> False)
          , ("empty clip", Rect 0 0 0 0, triangle, \_ _ -> False)
          , ("degenerate", full, (1, 1, 3, 3, 5, 5), \_ _ -> False)
          ]
    forM_ cases $ \(name, Rect clipX clipY clipW clipH, (ax, ay, bx, by, cx, cy), covered) -> do
      clearScreen surf 0
      ctx <- newPixelContext
      setDrawSquareGeometry ctx True
      (_, _, draw, _) <- runFrame ctx (withInput 8 8) (drawing (fixedWH 8 8) (\_ -> pure (FillTriangle ax ay bx by cx cy red)))
      let clip cmd = cmd {cmdClipX = clipX, cmdClipY = clipY, cmdClipW = clipW, cmdClipH = clipH}
      renderArena surf getCozetteFont 1 draw {drawCommands = U.map clip (drawCommands draw)} [] []
      pixels <- mapM (peekElemOff (sBuffer surf)) [0 .. 63]
      assert ("triangle raster " ++ name) (pixels == [if covered x y then packColor red else 0 | y <- [0 .. 7 :: Int], x <- [0 .. 7]])

-- | An RGFW context renders square, themed widgets: button corners are the
-- border colour, fills come from the theme, and label text is stamped
-- glyphs rather than solid per-character boxes.
testSquareThemedRaster :: IO ()
testSquareThemedRaster = do
  let theme = tomorrowNightMinDarkTheme
  ctx <- newRgfwContext theme
  raster ctx 240 120 (packColor (themeWindow theme)) (column (button "Button" >> checkbox "Checkbox label" True)) $ \_ (base, _) px -> do
    nodesOf NodeButton ctx >>= \case
      Rect bx by bw bh : _ -> do
        let (x0, y0, x1, y1) = (round bx, round by, round (bx + bw) - 1, round (by + bh) - 1)
        corners <- mapM (uncurry px) [(x0, y0), (x1, y0), (x0, y1), (x1, y1)]
        assert "Button corners are square (border colour)" (all (== packColor (styleBorder (themeButton theme))) corners)
        fillPx <- px (x0 + 2) (y0 + 2)
        assert "Button fill uses the theme" (fillPx == packColor (styleBg (themeButton theme)))
      [] -> assert "Button node present" False
    case [(r, fg) | (r, t, fg, _, _) <- base, t == "Checkbox label"] of
      (Rect sx sy sw sh, fg) : _ -> do
        ps <- sequence [px x y | y <- [round sy .. round (sy + sh) - 1], x <- [round sx .. round (sx + sw) - 1]]
        let lit = length (filter (== packColor fg) ps)
        assert "Checkbox label is glyphs, not solid boxes" (lit > 0 && lit * 2 < length ps)
      [] -> assert "Checkbox label span collected" False

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
          assert ("glyph atlas cell " ++ show c ++ " matches the blitter" ++ label') (cell == expected && any (/= 0) expected)

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
        n <- writeSpanQuads ga font (0, 0, 200, 100) buf 0 (Rect 10 5 0 0, txt, red, red, clip)
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

-- | RGFW keyboard translation: repeated letters all type, one Ctrl+letter
-- keystroke is a key chord that types nothing, whichever of its key-char and
-- key-press events RGFW queues first, and keys come up and repeat as they
-- should.
testRgfwTyping :: IO ()
testRgfwTyping = do
  let chars = inputChars . applied
      keys = toList . inputKeys . applied
      keyL = fromIntegral (fromEnum 'l')
      plainL = [EventKeyChar 'l', EventKeyPress keyL 0]
      ctrlLCharFirst = [EventKeyChar '\x0c', EventKeyPress keyL R.rgfw_modControl]
      ctrlLPressFirst = [EventKeyPress keyL R.rgfw_modControl, EventKeyChar '\x0c']
      chord evs = chars evs == "" && keys evs == [KeyChar 'l'] && modCtrl (inputModifiers (applied evs))
      released = applied [EventKeyPress keyL 0, EventKeyRelease keyL 0]
  assert "RGFW typing: repeated key-char events all type" (chars [EventKeyChar 'l', EventKeyChar 'l'] == "ll")
  assert "RGFW typing: repeated keystrokes all type" (chars (plainL ++ plainL) == "ll")
  assert "RGFW typing: a keystroke is its character key as well" (keys plainL == [KeyChar 'l'])
  assert "RGFW typing: Ctrl+L queued char-first is a chord" (chord ctrlLCharFirst)
  assert "RGFW typing: Ctrl+L queued press-first is a chord" (chord ctrlLPressFirst)
  assert "RGFW typing: Ctrl+L twice presses twice" (keys (ctrlLCharFirst ++ ctrlLPressFirst) == [KeyChar 'l', KeyChar 'l'])
  assert "RGFW keys: a release is reported and leaves nothing held"
    (toList (inputKeysReleased released) == [KeyChar 'l'] && null (inputKeysHeld released))
  assert "RGFW keys: a held key is held" (toList (inputKeysHeld (applied [EventKeyPress keyL 0])) == [KeyChar 'l'])
  assert "RGFW keys: letters repeat, Enter does not" $
    keys [EventKeyPress keyL 0, EventKeyRepeat keyL 0] == [KeyChar 'l', KeyChar 'l']
      && keys [EventKeyPress R.rgfw_keyReturn 0, EventKeyRepeat R.rgfw_keyReturn 0] == [KeyEnter]
  assert "RGFW keys: function keys, paging and Super" $
    keys [EventKeyPress (R.rgfw_keyF1 + 4) 0, EventKeyPress R.rgfw_keyPageDown 0] == [KeyF 5, KeyPageDown]
      && inputModifiers (applied [EventKeyPress keyL R.rgfw_modSuper]) == noModifiers {modSuper = True}
  assert "RGFW keys: the keypad types with Num Lock and navigates without" $
    keys [EventKeyPress (R.rgfw_keyPad1 + 1) R.rgfw_modNumLock] == [KeyChar '2']
      && keys [EventKeyPress (R.rgfw_keyPad1 + 1) 0] == [KeyDown]
      && keys [EventKeyPress R.rgfw_keyPadReturn 0] == [KeyEnter]

-- | Wheel events queued in one batch add up rather than keeping the last.
-- The middle button is held and clicks like the others; the side buttons
-- are back and forward, and the misc buttons past them are the extra ones.
-- The pointer leaving the window moves it off every widget.
testRgfwPointer :: IO ()
testRgfwPointer = do
  let middle = applied [EventMouseButton R.rgfw_mouseMiddle True]
      middleUp = applied [EventMouseButton R.rgfw_mouseMiddle True, EventMouseButton R.rgfw_mouseMiddle False]
      pressedBy b = buttonsToList (inputButtonsPressed (applied [EventMouseButton b True]))
      gone = applied [EventMouseMotion 30 40, EventOther R.rgfw_mouseLeave]
  assert "RGFW scroll: a batch of wheel events accumulates" (inputScroll (applied [EventMouseScroll 0 1, EventMouseScroll 0.5 2]) == V2 0.5 3)
  assert "RGFW buttons: the middle button goes down" (buttonHeld MouseMiddle middle && buttonPressed MouseMiddle middle)
  assert "RGFW buttons: the middle button comes up" (not (buttonHeld MouseMiddle middleUp) && buttonReleased MouseMiddle middleUp)
  assert "RGFW buttons: the middle button is not the left" (inputButtonsHeld middle == buttonsFromList [MouseMiddle])
  assert "RGFW buttons: misc 1 is back" (pressedBy R.rgfw_mouseMisc1 == [MouseBack])
  assert "RGFW buttons: misc 2 is forward" (pressedBy R.rgfw_mouseMisc2 == [MouseForward])
  assert "RGFW buttons: misc 3 to 5 are the next buttons" (concatMap pressedBy [R.rgfw_mouseMisc2 + 1 .. R.rgfw_mouseMisc2 + 3] == map MouseOther [6, 7, 8])
  assert "RGFW pointer: leaving the window moves the pointer off it" (let V2 x y = inputMousePos gone in x < -1000 && y < -1000)

-- | A turned image reaches the rasteriser as a turned quad, clipped to its
-- widget: turned an eighth, a 40 by 20 image covers its rect's top-left and
-- bottom-right corners and leaves the other two, which unturned it covers.
testTurnedImageRaster :: IO ()
testTurnedImageRaster = do
  let probe angle = do
        ctx <- newRgfwContext tomorrowNightMinDarkTheme
        _ <- registerImage ctx (ImageId 1) 8 4 (BS.replicate (8 * 4 * 4) 255)
        let cfg = defaultImageConfig {icLayout = fixedWH 40 20 defaultLayout, icRotation = RotateFloating angle}
        raster ctx 60 40 0 (column (imageConfigured' cfg (ImageId 1))) $ \resp _ px -> do
          let Rect x y w h = respRect resp
              white (u, v) = (== packColor (colorRGBA 255 255 255 255)) <$> px (round u) (round v)
          mapM white [(x + w / 2, y + h / 2), (x + 1, y + 1), (x + w - 2, y + 1), (x + 1, y + h - 2), (x + w - 2, y + h - 2)]
  turned <- probe (pi / 4)
  assert "Turned image covers its centre and the corners its long axis reaches" (turned == [True, True, False, False, True])
  flat <- probe 0
  assert "Unturned image covers its whole rect" (and flat)

-- | Each cursor kind RGFW has a cursor for shows that cursor, no two of them
-- the same one, and every other kind shows the nearest one RGFW has.
testRgfwCursors :: IO ()
testRgfwCursors = do
  let native =
        [ (UiCursorDefault, R.rgfw_mouseArrow), (UiCursorPointer, R.rgfw_mousePointingHand), (UiCursorText, R.rgfw_mouseIbeam)
        , (UiCursorNsResize, R.rgfw_mouseResizeNS), (UiCursorEwResize, R.rgfw_mouseResizeEW)
        , (UiCursorNwseResize, R.rgfw_mouseResizeNWSE), (UiCursorNeswResize, R.rgfw_mouseResizeNESW)
        , (UiCursorNotAllowed, R.rgfw_mouseNotAllowed), (UiCursorWait, R.rgfw_mouseWait), (UiCursorProgress, R.rgfw_mouseProgress)
        , (UiCursorCrosshair, R.rgfw_mouseCrosshair), (UiCursorMove, R.rgfw_mouseResizeAll), (UiCursorNResize, R.rgfw_mouseResizeN)
        , (UiCursorNeResize, R.rgfw_mouseResizeNE), (UiCursorEResize, R.rgfw_mouseResizeE), (UiCursorSeResize, R.rgfw_mouseResizeSE)
        , (UiCursorSResize, R.rgfw_mouseResizeS), (UiCursorSwResize, R.rgfw_mouseResizeSW), (UiCursorWResize, R.rgfw_mouseResizeW)
        , (UiCursorNwResize, R.rgfw_mouseResizeNW)
        ]
      fallback =
        [ (UiCursorGrab, R.rgfw_mouseResizeAll), (UiCursorGrabbing, R.rgfw_mouseResizeAll), (UiCursorAllScroll, R.rgfw_mouseResizeAll)
        , (UiCursorCell, R.rgfw_mouseCrosshair), (UiCursorColResize, R.rgfw_mouseResizeEW), (UiCursorRowResize, R.rgfw_mouseResizeNS)
        ]
          ++ map (,R.rgfw_mouseArrow) [UiCursorHelp, UiCursorCopy, UiCursorAlias, UiCursorContextMenu, UiCursorZoomIn, UiCursorZoomOut]
      -- The session hides the pointer for this kind rather than showing an
      -- icon; the arrow is what the mapping gives it.
      hidden = [(UiCursorHidden, R.rgfw_mouseArrow)]
      shows' = all (\(k, icon) -> mapRgfwCursor k == icon)
  assert "RGFW cursors: each native shape shows its own cursor" (shows' native && length (nub (map snd native)) == length native)
  assert "RGFW cursors: the other shapes fall back" (shows' (fallback ++ hidden))
  assert "RGFW cursors: every kind is mapped" (all (`elem` map fst (native ++ fallback ++ hidden)) [minBound .. maxBound])

main :: IO ()
main = do
  putStrLn "=== Running nano-ui-rgfw Unit Tests ==="
  testRgfwTyping
  testRgfwPointer
  testRgfwCursors
  testPackColor
  testSurfaceAllocation
  testScale2xGlyphTables
  testFractionalDpiCalculations
  testZOrderRenderArena
  testTriangleRaster
  testSquareThemedRaster
  testGlyphAtlas
  testSpanQuads
  testTurnedImageRaster
  testGlWindow assert
  testSessionWindow assert
  testRgfwWake
  putStrLn "=== All tests passed successfully! ==="
