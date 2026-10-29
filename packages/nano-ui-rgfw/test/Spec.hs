module Main (main) where

import Control.Exception (bracket)
import Control.Monad (filterM, forM_, void)
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.Foldable (toList)
import Data.List (nub)
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign.Marshal.Alloc (allocaBytes, callocBytes, free)
import Foreign.Storable (peekByteOff, peekElemOff)
import NanoUI
  ( AlignX (..), AlignY (..), ImageConfig (..), ImageId (..), NanoUI, Rect (..), Rotation (..), Style (..)
  , Theme (..), UiCursorKind (..), V2 (..), button, canvas, checkbox, colorRGBA, column, constVar
  , defaultImageConfig, drawText, fixedWH, fontColor, fontMono, fontSize, imageConfigured', inlineWith, labelWith
  , rectIntersect, respRect, richText, roundHalfUp, tomorrowNightMinDarkTheme, window', withClip, withTransform
  )
import NanoUI.Input
  ( Input (..), Key (..), Modifiers (..), MouseButton (..), Pressable (..), buttonsFromList, buttonsToList
  , emptyInput, noModifiers
  )
import NanoUI.Internal.Layout.Arena (NodeType (..))
import NanoUI.Path qualified as P
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette
  ( CozetteFont (..), charToGlyphId, cozetteGlyphBit, cozetteGlyphFootprint, getCozetteFont
  , renderGlyphScaledToBuffer
  )
import NanoUI.Rgfw.Internal.Gl
  (GlyphAtlas (..), atlasCell, bakeGlyphAtlas, glyphAtlasFor, toPhysRect, writeSpanQuads)
import NanoUI.Rgfw.Internal.Session (applyRgfwEvent, decodeRgfwEvents, mapRgfwCursor)
import NanoUI.Rgfw.Render (RgfwSurface (..), packColor, renderArena, withSurface)
import NanoUI.Rgfw.Wake (testRgfwWake)
import NanoUI.Rgfw.Window (testGlWindow, testSessionWindow)
import NanoUI.Testing (Context, collectRasterSpans, registerImage)
import NanoUI.Testing.Assert (run2Frames)
import NanoUI.Testing.Harness (DemoSpan, nodesOfType, withInputOff)
import RGFW (Event (..))
import RGFW.Raw qualified as R
import System.Exit (exitFailure)

assert :: String -> Bool -> IO ()
assert name True = putStrLn ("[PASS] " ++ name)
assert name False = putStrLn ("[FAIL] " ++ name) >> exitFailure

-- | Each named check, under a common prefix.
asserts :: String -> [(String, Bool)] -> IO ()
asserts prefix = mapM_ (\(name, ok) -> assert (prefix ++ name) ok)

type Pixels = Int -> Int -> IO Word32

-- | Draw a view's second frame (pointer outside the window) onto a software
-- surface cleared to @bg@, then pass the spans and a pixel reader to @k@.
raster :: Context -> Int -> Int -> Word32 -> NanoUI a -> (a -> ([DemoSpan], [DemoSpan]) -> Pixels -> IO b) -> IO b
raster ctx w h bg ui k = do
  let inp = withInputOff (fromIntegral w) (fromIntegral h)
  (a, draw, _) <- run2Frames ctx inp ui
  spans@(base, overlay) <- collectRasterSpans ctx inp
  withSurface w h bg $ \surf -> do
    renderArena surf getCozetteFont 1 draw base overlay
    k a spans (\x y -> peekElemOff (sBuffer surf) (y * w + x))

-- | The pixels of @r@ (rounded) whose colour is @c@, out of all of them.
inkIn :: Pixels -> Word32 -> Rect -> IO (Int, Int)
inkIn px c (Rect x y w h) = do
  let box = [(i, j) | j <- [round y .. round (y + h) - 1], i <- [round x .. round (x + w) - 1]]
  lit <- length <$> filterM (\(i, j) -> (== c) <$> px i j) box
  pure (lit, length box)

-- | Some, but not most, of a span's box is ink: glyphs, not a solid box.
glyphs :: (Int, Int) -> Bool
glyphs (lit, total) = lit > 0 && lit * 2 < total

-- | Fold a batch of RGFW events into one 'Input'.
applied :: [Event] -> Input
applied = foldl' applyRgfwEvent emptyInput . decodeRgfwEvents 1

-- | Reference EPX (Scale2x) of a @w@ by @h@ bit query.
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

-- | The scaled glyph tables, the OpenGL glyph atlas, span quads, and the
-- physical rects of fractional scales.
testGlyphs :: IO ()
testGlyphs = do
  let font = getCozetteFont
  -- The 14x26 table is Scale2x of the 7x13 one, and 28x52 of 14x26.
  forM_ [(2, 1, 7, 13), (4, 2, 14, 26)] $ \(scale, from, w, h) -> forM_ [1, 2, 34, 36, 65, 95] $ \gid ->
    assert ("Scale" ++ show scale ++ "x table matches scale2x for glyph " ++ show gid) $
      and
        [ cozetteGlyphBit font scale gid x y == scale2x w h (cozetteGlyphBit font from gid) x y
        | y <- [0 .. 2 * h - 1]
        , x <- [0 .. 2 * w - 1]
        ]
  let (x1, _, w1, _) = toPhysRect 1.33 0 0 63.7 30
      (x2, _, _, _) = toPhysRect 1.33 63.7 0 63.7 30
  assert "Adjacent widgets at fractional scale have zero gap/overlap" (x1 + w1 == x2)
  -- The atlas holds every glyph as the software blitter stamps it at that
  -- scale; below 1x the pens close up, so the glyphs shrink with them.
  assert "glyphs below 1x shrink with the pen grid" $
    map cozetteGlyphFootprint [0.5, 0.75, 1.0] == [(4, 6), (5, 10), (7, 13)]
  forM_ [0.5, 1.0, 1.5, 2.0, 3.0, 4.0] $ \scale -> do
    let ga = glyphAtlasFor font scale
        (cw, ch) = (gaCellW ga, gaCellH ga)
        at = " at scale " ++ show scale
    assert ("glyph atlas grid fits every glyph" ++ at)
      (gaCols ga * (gaHeight ga `div` ch) >= cfNumGlyphs font && gaWidth ga == gaCols ga * cw)
    bakeGlyphAtlas font ga $ \atlas -> forM_ ("A@#|" :: String) $ \c -> do
      let gid = fromIntegral (charToGlyphId font c)
          (ax, ay) = atlasCell ga gid
      bracket (callocBytes (cw * ch * 4)) free $ \solo -> do
        renderGlyphScaledToBuffer solo cw 0 0 cw ch scale 0 0 0xFFFFFFFF font (fromIntegral gid)
        cell <- sequence
          [peekElemOff atlas (row * gaWidth ga + col) | row <- [ay .. ay + ch - 1], col <- [ax .. ax + cw - 1]]
        expected <- mapM (peekElemOff solo) [0 .. cw * ch - 1]
        assert ("glyph atlas cell " ++ show c ++ " matches the blitter" ++ at) $
          cell == expected && any (/= 0) expected
  -- Span quads land on the software pen positions (spaces skipped, newlines
  -- reset the column) and clipping trims positions and UVs together.
  let ga = glyphAtlasFor font 2.0
      red = colorRGBA 255 0 0 255
      (aw, ah) = (fromIntegral (gaWidth ga), fromIntegral (gaHeight ga)) :: (Float, Float)
      (ax, ay) = atlasCell ga (fromIntegral (charToGlyphId font 'A'))
      uv u v = [fromIntegral u / aw, fromIntegral v / ah]
      quads txt clip = allocaBytes (64 * 32) $ \buf -> do
        n <- writeSpanQuads ga font (0, 0, 200, 100) buf 0 (Rect 10 5 0 0, txt, red, red, clip)
        vs <- mapM (\i -> mapM (\o -> peekByteOff buf (i * 32 + o)) [0, 4, 24, 28]) [0 .. n - 1]
        pure (n, vs :: [[Float]])
  (n1, v1) <- quads "A B\nC" (Rect 0 0 100 50)
  (n2, v2) <- quads "A" (Rect 12 0 100 50)
  (n3, _) <- quads "A" (Rect 150 80 10 10)
  asserts "span quads: "
    [ ("one quad per visible glyph", n1 == 18)
    , ("first glyph at the scaled pen", take 1 v1 == [[20, 10] ++ uv ax ay])
    , ("newline resets the column", map (take 2) (take 1 (drop 12 v1)) == [[20, 36]])
    , ("clip trims position and UV", n2 == 6 && take 1 v2 == [[24, 10] ++ uv (ax + 4) ay])
    , ("clip outside the framebuffer emits nothing", n3 == 0)
    ]

-- | An RGFW context renders square, themed widgets, and stamps widget,
-- sized, monospace, canvas and rich text as glyphs in the Cozette cell.
testThemedRaster :: IO ()
testThemedRaster = do
  let theme = tomorrowNightMinDarkTheme
      ink = colorRGBA 0x12 0xEE 0x34 255
      ui = column $ do
        void (button "Button")
        checkbox "Checkbox label" (constVar True)
        labelWith (fontSize 26) "Title"
        labelWith fontMono "1234"
        _ <- canvas (fixedWH 200 20) (\(Rect x y _ _) -> drawText (V2 x y) AlignStart AlignTop "Label" ink)
        richText [inlineWith (fontColor ink) "a rich text paragraph"]
  ctx <- newRgfwContext theme
  raster ctx 240 200 (packColor (themeWindow theme)) ui $ \_ (base, _) px -> do
    nodesOfType NodeButton ctx >>= \case
      (_, Rect bx by bw bh) : _ -> do
        let (x0, y0, x1, y1) = (round bx, round by, round (bx + bw) - 1, round (by + bh) - 1)
        corners <- mapM (uncurry px) [(x0, y0), (x1, y0), (x0, y1), (x1, y1)]
        fill <- px (x0 + 2) (y0 + 2)
        assert "Button corners are square (border colour)" $
          all (== packColor (styleBorder (themeButton theme))) corners
        assert "Button fill uses the theme" (fill == packColor (styleBg (themeButton theme)))
      [] -> assert "Button node present" False
    labelInk <- sequence [inkIn px (packColor fg) r | (r, "Checkbox label", fg, _, _) <- base]
    assert "Checkbox label is glyphs, not solid boxes" (map glyphs labelInk == [True])
    -- Cozette stamps every span from its one cell, so each span is as wide
    -- as the glyphs drawn in it.
    assert "Sized and monospace text spans measure the Cozette cell" $
      [(t, w, h) | (Rect _ _ w h, t, _, _, _) <- base, t `elem` ["Title", "1234"]]
        == [("Title", 30, 13), ("1234", 24, 13)]
    drawings <- map snd <$> nodesOfType NodeDrawing ctx
    let inkSpans = [r | (r, _, fg, _, _) <- base, fg == ink]
        inside (Rect rx ry rw rh) (x, y) =
          fromIntegral x >= rx && fromIntegral x < rx + rw && fromIntegral y >= ry && fromIntegral y < ry + rh
    inked <- filterM (\(x, y) -> (== packColor ink) <$> px x y) [(x, y) | y <- [0 .. 199], x <- [0 .. 239]]
    drawingInk <- mapM (inkIn px (packColor ink)) inkSpans
    assert "Canvas and rich text are glyphs, not solid boxes" (length inkSpans >= 2 && all glyphs drawingInk)
    assert "No drawing text outside its drawing" (all (\p -> any (`inside` p) drawings) inked)

-- | A floating window paints over the page: its chrome is the theme's, the
-- page's drawing text under it is hidden, and its own drawing text shows
-- over its background.
testWindowLayer :: IO ()
testWindowLayer = do
  let theme = tomorrowNightMinDarkTheme
      under = colorRGBA 0x12 0xEE 0x34 255
      over = colorRGBA 0xEE 0x12 0x34 255
      text c (Rect x y _ _) = drawText (V2 x y) AlignStart AlignTop c
      page = void $ canvas (fixedWH 240 120) $ \(Rect x y w h) ->
        forM_ [0 .. 8 :: Int] $ \i -> text (T.replicate 40 "#") (Rect x (y + 13 * fromIntegral i) w h) under
      win = window' (constVar True) "Z" (canvas (fixedWH 60 20) (\r -> text "over" r over))
      inkAt col px = filterM (\(x, y) -> (== packColor col) <$> px x y)
  alone <- newRgfwContext theme
  pageInk <- raster alone 240 120 0 page $ \_ _ px ->
    inkAt under px [(x, y) | y <- [0 .. 119], x <- [0 .. 239]]
  ctx <- newRgfwContext theme
  raster ctx 240 120 0 (page >> win) $ \_ (_, overlay) px ->
    nodesOfType NodeWindow ctx >>= \case
      (_, Rect wx wy ww wh) : _ -> do
        -- A pixel in from the window's edges, which may be fractional.
        let inWindow (x, y) =
              fromIntegral x >= wx + 1 && fromIntegral x < wx + ww - 1
                && fromIntegral y >= wy + 1 && fromIntegral y < wy + wh - 1
            covered = filter inWindow pageInk
        shown <- inkAt under px covered
        chrome <- px (round (wx + ww / 2)) (round wy + 4)
        overInk <- mapM (inkIn px (packColor over)) [r | (r, _, fg, _, _) <- overlay, fg == over]
        assert "A floating window paints its theme's chrome over the page" $
          chrome == packColor (styleBg (themeFloatingWindow theme))
        assert "A floating window covers the page's drawing text" (not (null covered) && null shown)
        assert "Drawing text in a floating window shows over its background" (map glyphs overInk == [True])
      [] -> assert "Floating window node present in arena" False

-- | A canvas transform moves its text's span, and a canvas clip clips it:
-- translated by (30, 10), the span is the plain one moved that far, and its
-- clip is the clip rect cut to the canvas. At 1.5x its first glyph quad
-- starts at the span's origin scaled and rounded half up.
testDrawingTextTransform :: IO ()
testDrawingTextTransform = do
  ctx <- newRgfwContext tomorrowNightMinDarkTheme
  let ink = colorRGBA 0x12 0xEE 0x34 255
      inp = withInputOff 240 80
      clipR = Rect 20 5 300 30
      at (Rect x y _ _) = V2 (x + 5) (y + 4)
      ui = canvas (fixedWH 200 60) $ \r -> do
        drawText (at r) AlignStart AlignTop "plain" ink
        withClip clipR (withTransform (P.translate 30 10) (drawText (at r) AlignStart AlignTop "moved" ink))
  (resp, _, _) <- run2Frames ctx inp ui
  (base, _) <- collectRasterSpans ctx inp
  case ([r | (r, "plain", _, _, _) <- base], [(r, c) | (r, "moved", _, _, c) <- base]) of
    ([Rect px py _ _], [(moved@(Rect mx my _ _), clip)]) -> do
      assert "A canvas transform moves drawing text" ((mx, my) == (px + 30, py + 10))
      assert "A canvas clip clips drawing text" (Just clip == rectIntersect clipR (respRect resp))
      let ga = glyphAtlasFor getCozetteFont 1.5
          pen v = fromIntegral (roundHalfUp (v * 1.5)) :: Float
      first <- allocaBytes (64 * 32) $ \buf -> do
        n <- writeSpanQuads ga getCozetteFont (0, 0, 400, 200) buf 0 (moved, "moved", ink, ink, clip)
        if n > 0 then Just <$> mapM (peekByteOff buf) [0, 4] else pure Nothing
      assert "Drawing text's first glyph lands on its rounded pen at 1.5x" (first == Just [pen mx, pen my])
    _ -> assert "Transformed and plain drawing text spans collected" False

-- | A rotated image reaches the rasteriser as a rotated quad, clipped to its
-- widget: turned 45 degrees, a 40 by 20 image covers its rect's centre and
-- top-left and bottom-right corners only; unturned, all of it.
testTurnedImageRaster :: IO ()
testTurnedImageRaster = do
  let probe angle = do
        ctx <- newRgfwContext tomorrowNightMinDarkTheme
        _ <- registerImage ctx (ImageId 1) 8 4 (BS.replicate (8 * 4 * 4) 255)
        let cfg = defaultImageConfig {icLayout = fixedWH 40 20, icRotation = RotateFloating angle}
        raster ctx 60 40 0 (column (imageConfigured' cfg (ImageId 1))) $ \resp _ px -> do
          let Rect x y w h = respRect resp
              (r, b) = (x + w - 2, y + h - 2)
              white (u, v) = (== packColor (colorRGBA 255 255 255 255)) <$> px (round u) (round v)
          mapM white [(x + w / 2, y + h / 2), (x + 1, y + 1), (r, y + 1), (x + 1, b), (r, b)]
  turned <- probe (pi / 4)
  flat <- probe 0
  assert "Turned image covers its centre and the corners its long axis reaches" $
    turned == [True, True, False, False, True]
  assert "Unturned image covers its whole rect" (and flat)

-- | RGFW keyboard translation. A Ctrl+letter keystroke is a chord that
-- types nothing, whichever of its key-char and key-press events RGFW queues
-- first.
testRgfwKeys :: IO ()
testRgfwKeys = do
  let chars = inputChars . applied
      keys = toList . inputKeys . applied
      mods = inputModifiers . applied
      keyL = fromIntegral (fromEnum 'l')
      plainL = [EventKeyChar 'l', EventKeyPress keyL 0]
      ctrlLCharFirst = [EventKeyChar '\x0c', EventKeyPress keyL R.rgfw_modControl]
      ctrlLPressFirst = [EventKeyPress keyL R.rgfw_modControl, EventKeyChar '\x0c']
      chord evs = chars evs == "" && keys evs == [KeyChar 'l'] && modCtrl (mods evs)
      released = applied [EventKeyPress keyL 0, EventKeyRelease keyL 0]
      enterHeld = applied [EventKeyPress R.rgfw_keyReturn 0, EventKeyRepeat R.rgfw_keyReturn 0]
      blurred = applied [EventKeyPress keyL R.rgfw_modControl, EventOther R.rgfw_windowFocusOut]
      pad1 = R.rgfw_keyPad1 + 1
      shift = noModifiers {modShift = True}
  asserts "RGFW keys: "
    [ ("repeated key-char events all type", chars [EventKeyChar 'l', EventKeyChar 'l'] == "ll")
    , ( "a joiner types, and macOS arrow-key characters do not"
      , chars [EventKeyChar '\x200C', EventKeyChar '\xF702'] == "\x200C"
      )
    , ( "private-use characters past the macOS function keys type"
      , chars [EventKeyChar '\xF747', EventKeyChar '\xF748', EventKeyChar '\xF8FF'] == "\xF748\xF8FF"
      )
    , ("repeated keystrokes all type", chars (plainL ++ plainL) == "ll")
    , ("a keystroke is its character key as well", keys plainL == [KeyChar 'l'])
    , ("Ctrl+L queued char-first is a chord", chord ctrlLCharFirst)
    , ("Ctrl+L queued press-first is a chord", chord ctrlLPressFirst)
    , ("Ctrl+L twice presses twice", keys (ctrlLCharFirst ++ ctrlLPressFirst) == [KeyChar 'l', KeyChar 'l'])
    , ( "a release is reported and leaves nothing held"
      , toList (inputKeysReleased released) == [KeyChar 'l'] && null (inputKeysHeld released)
      )
    , ("a held key is held", toList (inputKeysHeld (applied [EventKeyPress keyL 0])) == [KeyChar 'l'])
    , ( "every key repeats, and a repeat is no new press"
      , keys [EventKeyPress keyL 0, EventKeyRepeat keyL 0] == [KeyChar 'l', KeyChar 'l']
          && toList (inputKeys enterHeld) == [KeyEnter, KeyEnter]
          && toList (inputKeysNew enterHeld) == [KeyEnter]
      )
    , ( "losing the keyboard lets go of the keys held"
      , null (inputKeysHeld blurred) && pressedIn (KeyChar 'l') blurred && releasedIn (KeyChar 'l') blurred
          && inputModifiers blurred == noModifiers
      )
    , ( "function keys, paging and Super"
      , keys [EventKeyPress (R.rgfw_keyF1 + 4) 0, EventKeyPress R.rgfw_keyPageDown 0] == [KeyF 5, KeyPageDown]
          && mods [EventKeyPress keyL R.rgfw_modSuper] == noModifiers {modSuper = True}
      )
    , ( "the keypad types with Num Lock and navigates without"
      , keys [EventKeyPress pad1 R.rgfw_modNumLock] == [KeyChar '2']
          && keys [EventKeyPress pad1 0] == [KeyDown]
          && keys [EventKeyPress R.rgfw_keyPadReturn 0] == [KeyEnter]
      )
    , -- On X11 and Windows a modifier key's own events carry the bits from
      -- before them: Shift's press lacks Shift and its release still has it.
      ( "a modifier is held from its own press to its own release"
      , map mods
          [ [EventKeyPress R.rgfw_keyShiftL 0]
          , [EventKeyPress R.rgfw_keyControlR R.rgfw_modShift]
          , [EventKeyPress R.rgfw_keyShiftL 0, EventKeyRelease R.rgfw_keyShiftL R.rgfw_modShift]
          , [EventKeyRelease R.rgfw_keyAltR (R.rgfw_modAlt .|. R.rgfw_modSuper)]
          ]
          == [shift, shift {modCtrl = True}, noModifiers, noModifiers {modSuper = True}]
      )
    ]

-- | RGFW pointer translation, window repaints, and cursor shapes.
testRgfwPointer :: IO ()
testRgfwPointer = do
  let middle = applied [EventMouseButton R.rgfw_mouseMiddle True]
      middleUp = applied [EventMouseButton R.rgfw_mouseMiddle True, EventMouseButton R.rgfw_mouseMiddle False]
      pressedBy b = buttonsToList (inputButtonsPressed (applied [EventMouseButton b True]))
      gone = applied [EventMouseMotion 30 40, EventOther R.rgfw_mouseLeave]
      -- A drag's motion outside the window still comes, so a leave then
      -- would drop a dragged slider or scrollbar to the far corner.
      dragOut =
        applied [EventMouseButton R.rgfw_mouseLeft True, EventMouseMotion 30 40, EventOther R.rgfw_mouseLeave]
      redraws evs = inputWindowRedraw (applied evs)
      wheel = inputScroll . applied . map (uncurry EventMouseScroll)
  asserts "RGFW pointer: "
    [ -- RGFW's wheel is positive up and left, the input's down and right.
      ("a batch of wheel events accumulates", wheel [(0, 1), (0.5, 2)] == V2 (-0.5) (-3))
    , ("the wheel turned down and right scrolls down and right", wheel [(-1, -1)] == V2 1 1)
    , ("the middle button goes down", heldIn MouseMiddle middle && pressedIn MouseMiddle middle)
    , ("the middle button comes up", not (heldIn MouseMiddle middleUp) && releasedIn MouseMiddle middleUp)
    , ("the middle button is not the left", inputButtonsHeld middle == buttonsFromList [MouseMiddle])
    , ("misc 1 is back", pressedBy R.rgfw_mouseMisc1 == [MouseBack])
    , ("misc 2 is forward", pressedBy R.rgfw_mouseMisc2 == [MouseForward])
    , ( "misc 3 to 5 are the next buttons"
      , concatMap pressedBy [R.rgfw_mouseMisc2 + 1 .. R.rgfw_mouseMisc2 + 3] == map MouseOther [6, 7, 8]
      )
    , ("leaving the window moves the pointer off it", let V2 x y = inputMousePos gone in x < -1000 && y < -1000)
    , ("leaving with a button held keeps the pointer", inputMousePos dragOut == V2 30 40)
    , ( "an uncovered or restored window repaints in full"
      , redraws [EventOther R.rgfw_windowRefresh] && redraws [EventOther R.rgfw_windowRestored]
          && not (redraws [EventMouseMotion 1 1])
      )
    ]
  -- Each kind with a native RGFW cursor shows its own.
  let native =
        [ (UiCursorDefault, R.rgfw_mouseArrow), (UiCursorPointer, R.rgfw_mousePointingHand)
        , (UiCursorText, R.rgfw_mouseIbeam), (UiCursorNsResize, R.rgfw_mouseResizeNS)
        , (UiCursorEwResize, R.rgfw_mouseResizeEW), (UiCursorNwseResize, R.rgfw_mouseResizeNWSE)
        , (UiCursorNeswResize, R.rgfw_mouseResizeNESW), (UiCursorNotAllowed, R.rgfw_mouseNotAllowed)
        , (UiCursorWait, R.rgfw_mouseWait), (UiCursorProgress, R.rgfw_mouseProgress)
        , (UiCursorCrosshair, R.rgfw_mouseCrosshair), (UiCursorMove, R.rgfw_mouseResizeAll)
        , (UiCursorNResize, R.rgfw_mouseResizeN), (UiCursorNeResize, R.rgfw_mouseResizeNE)
        , (UiCursorEResize, R.rgfw_mouseResizeE), (UiCursorSeResize, R.rgfw_mouseResizeSE)
        , (UiCursorSResize, R.rgfw_mouseResizeS), (UiCursorSwResize, R.rgfw_mouseResizeSW)
        , (UiCursorWResize, R.rgfw_mouseResizeW), (UiCursorNwResize, R.rgfw_mouseResizeNW)
        ]
  asserts "RGFW cursors: "
    [ ( "each native shape shows its own cursor"
      , all (\(k, icon) -> mapRgfwCursor k == icon) native && length (nub (map snd native)) == length native
      )
    , ("every other kind falls back to a native one", all ((`elem` map snd native) . mapRgfwCursor) [minBound ..])
    ]

main :: IO ()
main = do
  putStrLn "=== Running nano-ui-rgfw Unit Tests ==="
  testRgfwKeys
  testRgfwPointer
  testGlyphs
  testThemedRaster
  testWindowLayer
  testDrawingTextTransform
  testTurnedImageRaster
  testGlWindow assert
  testSessionWindow assert
  testRgfwWake
  putStrLn "=== All tests passed successfully! ==="
