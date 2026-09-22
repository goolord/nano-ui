-- | Shared helpers for integration tests: input gestures, spans, scroll checks.
module NanoUI.Testing.Harness
  ( clickPair
  , rightClickPair
  , pressAt
  , holdAt
  , releaseAt
  , keyInp
  , tabInp
  , withInputOff
  , withDelta
  , centerOf
  , warmup
  , warmup2
  , warmupDraw
  , warmupFocused
  , held
  , runClick
  , assertSpansHas
  , spanYOf
  , spanXOf
  , spanRect
  , spanRectOf
  , covers
  , clipCovers
  , assertScrollGutterPad
  , assertWheelTitlePinned
  , findGrabHover
  , dragWindowEdge
  , vertUv
  , checkLabelAlignEndInk
  , checkIdleFullDamage
  , windowTitleGrab
  , runDragFrom
  , DemoSpan
  , spanCenter
  , hasText
  , spanLabel
  , findExact
  , findHeader
  , findRightmost
  , requireSpan
  , expectText
  , clickPos
  , clickTab
  , dragPos
  , drawQuads
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_, unless, void, when)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.List (maximumBy)
import Data.Maybe (listToMaybe)
import Data.Ord (comparing)
import Data.Text qualified as T
import Data.Word (Word32, Word8)
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (peekByteOff)
import GHC.Stack (HasCallStack)
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Font (alignedTextPen, textInkEnd)
import NanoUI.Internal.Types (clamp)
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertLt, bump, evalUi, run2Frames, withInput)

-- | Text bounds, text, foreground, background, and clip in logical window coordinates.
type DemoSpan = (Rect, T.Text, Color, Color, Rect)

foreign import ccall unsafe "string.h memcpy" c_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO ()

-- | Decode the quads a frame actually rasterised: one @(rect, color)@ per
-- six-index quad, in draw order. Span and arena queries cannot see chrome
-- (scroller wells, scrollbar lanes); this can. Use only with quad-packed
-- output, such as square-geometry test contexts. Triangle and rounded-shape
-- output is not supported. Commands with counts not divisible by six throw.
drawQuads :: DrawData -> IO [(Rect, Color)]
drawQuads dd =
  fmap concat $
    forM (drawCmdElems dd) $ \c -> do
      let ioff = fromIntegral (cmdIndexOffset c)
          icnt = fromIntegral (cmdIndexCount c)
      when (icnt `rem` 6 /= 0) $
        error ("drawQuads: draw command packs " ++ show icnt ++ " indices; not quad-packed")
      sequence
        [ decodeQuad (ioff + q)
        | q <- [0, 6 .. icnt - 1]
        ]
  where
    verts = drawVertices dd
    idxs = drawIndices dd
    peekWord32 :: Ptr Word8 -> IO Word32
    peekWord32 off = allocaBytes 4 $ \tmp -> do
      c_memcpy tmp off 4
      peekByteOff tmp 0
    peekVertex :: Ptr Word8 -> Int -> IO (Float, Float, Float, Float, Float, Float)
    peekVertex vp vi =
      allocaBytes vertexSize $ \tmp -> do
        c_memcpy tmp (vp `plusPtr` (vi * vertexSize)) (fromIntegral vertexSize)
        x <- peekByteOff tmp 0
        y <- peekByteOff tmp 4
        r <- peekByteOff tmp 8
        g <- peekByteOff tmp 12
        b <- peekByteOff tmp 16
        a <- peekByteOff tmp 20
        pure (x, y, r, g, b, a)
    decodeQuad iStart =
      withForeignPtr verts $ \vp ->
        withForeignPtr idxs $ \ip -> do
          vis <-
            forM [iStart .. iStart + 3] $ \ii -> do
              vi <- fromIntegral <$> peekWord32 (ip `plusPtr` (ii * indexSize))
              peekVertex vp vi
          case vis of
            [] -> pure (Rect 0 0 0 0, colorRGBA 0 0 0 0)
            (x0, y0, r0, g0, b0, a0) : rest -> do
              let xs = x0 : map (\(x, _, _, _, _, _) -> x) rest
                  ys = y0 : map (\(_, y, _, _, _, _) -> y) rest
                  toW8 f = clamp 0 255 (round (f * 255))
              pure
                ( Rect (minimum xs) (minimum ys) (maximum xs - minimum xs) (maximum ys - minimum ys)
                , colorRGBA (toW8 r0) (toW8 g0) (toW8 b0) (toW8 a0)
                )

-- | Midpoint of a logical rectangle, without clipping it.
spanCenter :: Rect -> V2
spanCenter (Rect x y w h) = V2 (x + w / 2) (y + h / 2)

-- | Whether any span contains the text as a substring.
hasText :: T.Text -> [(Rect, T.Text, a, b, c)] -> Bool
hasText needle = any (\(_, txt, _, _, _) -> needle `T.isInfixOf` txt)

-- Blank-glyph markers some span labels carry in front of their text (blanked
-- sort arrows, flags, sort-reserve padding).
dropSpanMarkers :: T.Text -> T.Text
dropSpanMarkers = T.dropWhile (`elem` ['\x01', '\x02', '\x05'])

-- | Trim surrounding whitespace and remove leading internal blank-glyph markers.
spanLabel :: T.Text -> T.Text
spanLabel txt = dropSpanMarkers (T.strip txt)

-- | Centre of the rightmost nontrivial span with an exact normalised label.
-- Returns 'Nothing' when none matches.
findExact :: T.Text -> [DemoSpan] -> Maybe V2
findExact needle spans =
  pickRight
    [ (x, spanCenter r)
    | (r@(Rect x _ w h), txt, _, _, _) <- spans
    , w > 1 && h > 1
    , spanLabel txt == needle
    ]

-- | Find a table-header centre, preferring its reserved sort padding over an
-- ordinary label with the same text. Ties choose the rightmost match.
findHeader :: T.Text -> [DemoSpan] -> Maybe V2
findHeader needle spans =
  -- Header spans keep their sort-reserve padding ("Name   ", or "   Age" in
  -- a right-aligned column, the arrow glyph blanked), while every other
  -- "Name" label is trimmed. Match the raw, untrimmed text so the header wins
  -- over right-aligned kv values that happen to repeat the column name.
  let marked =
        [ (x, spanCenter r)
        | (r@(Rect x _ w h), txt, _, _, _) <- spans
        , w > 1 && h > 1
        , let raw = dropSpanMarkers txt
        , T.isPrefixOf (needle <> " ") raw || T.isSuffixOf (" " <> needle) raw
        ]
   in pickRight marked <|> findExact needle spans

-- | Centre of the rightmost span containing the substring, or 'Nothing'.
findRightmost :: T.Text -> [DemoSpan] -> Maybe V2
findRightmost needle spans =
  pickRight [(x, spanCenter r) | (r@(Rect x _ _ _), txt, _, _, _) <- spans, needle `T.isInfixOf` txt]

pickRight :: [(Float, V2)] -> Maybe V2
pickRight [] = Nothing
-- maximumBy keeps the later of equal elements, so ties choose the rightmost.
pickRight ps = Just (snd (maximumBy (comparing fst) ps))

-- | Return a located point or throw an IO error with the supplied diagnostic.
requireSpan :: String -> Maybe V2 -> IO V2
requireSpan msg = maybe (fail msg) pure

-- | Fail with @msg@ unless a span contains @needle@.
expectText :: String -> T.Text -> [(Rect, T.Text, a, b, c)] -> IO ()
expectText msg needle spans = unless (hasText needle spans) (fail msg)

-- | Press, hold and release at @pos@, then two idle frames.
clickPos :: (Input -> IO ()) -> Input -> V2 -> IO ()
clickPos drawFrame base pos = dragPos drawFrame base pos pos

-- | Locate a named tab from text spans and drive a click. Fails if absent.
clickTab :: (Context -> IO [DemoSpan]) -> (Input -> IO ()) -> Context -> Input -> T.Text -> IO ()
clickTab getSpans drawFrame ctx base name = do
  spans <- getSpans ctx
  pos <- requireSpan ("selftest: tab " <> T.unpack name) (findExact name spans)
  clickPos drawFrame base pos

-- | Press at @from@, hold at @to@ and release there, then two idle frames.
dragPos :: (Input -> IO ()) -> Input -> V2 -> V2 -> IO ()
dragPos drawFrame base from to = do
  let press = pressAt base from
      hold = holdAt base to
  mapM_ drawFrame [press, hold, releaseAt hold, base, base]

-- | Left-button press and release at a point, retaining other base-input fields.
clickPair :: Input -> V2 -> (Input, Input)
clickPair inp pos =
  let
    press = pressAt inp pos
    release = releaseAt press
   in
    (press, release)

-- | Right-button press and release at a point. Supply event-free base input.
rightClickPair :: Input -> V2 -> (Input, Input)
rightClickPair inp pos =
  let
    press =
      inp
        { inputMousePos = pos
        , inputMouseRightDown = True
        , inputMouseRightPressed = True
        }
    release =
      press
        { inputMouseRightDown = False
        , inputMouseRightPressed = False
        , inputMouseRightReleased = True
        }
   in
    (press, release)

-- | Set pointer position and left-button press/held flags, clearing its release flag.
pressAt :: Input -> V2 -> Input
pressAt inp pos =
  inp
    { inputMousePos = pos
    , inputMouseDown = True
    , inputMousePressed = True
    , inputMouseReleased = False
    }

-- | The button still down from an earlier 'pressAt', with the pointer at @pos@.
holdAt :: Input -> V2 -> Input
holdAt inp pos = (pressAt inp pos) {inputMousePressed = False}

-- | Release the left button at its current position, clearing its press/held flags.
releaseAt :: Input -> Input
releaseAt press =
  press
    { inputMouseDown = False
    , inputMousePressed = False
    , inputMouseReleased = True
    }

-- | A single key-down frame.
keyInp :: Key -> Input -> Input
keyInp k inp = inp {inputKeys = inputKeysFromList [k]}

-- | Step the tab focus to the next focusable.
tabInp :: Input -> Input
tabInp = keyInp KeyTab

-- | Event-free input with the requested window size and pointer at (-10,-10).
withInputOff :: Float -> Float -> Input
withInputOff w h =
  let inp = withInput w h
   in inp {inputMousePos = V2 (-10) (-10)}

-- | Event-free input with logical width/height and elapsed time in seconds.
withDelta :: Float -> Float -> Float -> Input
withDelta w h dt =
  let inp = withInput w h
   in inp {inputDeltaTime = dt}

-- | Centre of the response rectangle. Warm up the view before using it as a target.
centerOf :: Response -> V2
centerOf = spanCenter . respRect

-- | Run one frame to establish previous geometry. Supply event-free input.
warmup :: Context -> Input -> NanoUI a -> IO ()
warmup ctx inp ui = void (runFrame ctx inp ui)

-- | Run two frames and return the second result, whose responses can use solved
-- geometry from the first. Supply event-free input.
warmup2 :: Context -> Input -> NanoUI a -> IO a
warmup2 ctx inp ui = warmup ctx inp ui >> evalUi ctx inp ui

-- | Two-frame warmup returning the second result and borrowed drawing buffers.
warmupDraw :: Context -> Input -> NanoUI a -> IO (a, DrawData)
warmupDraw ctx inp ui = do
  (a, _, draw, _) <- run2Frames ctx inp ui
  pure (a, draw)

-- | Warm the view up, then Tab onto its first focusable.
warmupFocused :: Context -> Input -> NanoUI a -> IO ()
warmupFocused ctx inp ui = warmup2 ctx inp ui >> void (runFrame ctx (tabInp inp) ui)

-- | Drive a controlled input the way an application does: pass the value held
-- in the test's 'IORef' and store the widget's result for the next frame. Not
-- a hook: a hook write makes the frame run the view again without input, and
-- the frame then returns that pass's result without its click or change flags.
held :: Ui :> es => IORef a -> (a -> Eff es (r, a)) -> Eff es (r, a)
held ref widget = do
  result <- widget =<< uiIO (readIORef ref)
  uiIO (writeIORef ref (snd result))
  pure result

-- | Run a press frame and a release frame at @pos@ ('clickPair'), returning
-- the release frame's result.
runClick :: Context -> Input -> NanoUI a -> V2 -> IO a
runClick ctx inp0 ui pos =
  let
    (press, release) = clickPair inp0 pos
   in
    warmup ctx press ui >> evalUi ctx release ui

-- | Count a failure unless some span contains the substring.
assertSpansHas :: HasCallStack => IORef Int -> T.Text -> [(Rect, T.Text, a, b, c)] -> IO ()
assertSpansHas failed needle spans = assert failed (hasText needle spans)

-- | The rect of the first span whose text contains @needle@.
spanRect :: T.Text -> [(Rect, T.Text, a, b, c)] -> Maybe Rect
spanRect needle spans = listToMaybe [r | (r, txt, _, _, _) <- spans, needle `T.isInfixOf` txt]

-- | The rect of the first span whose text is exactly @lbl@.
spanRectOf :: T.Text -> [(Rect, T.Text, a, b, c)] -> Maybe Rect
spanRectOf lbl spans = listToMaybe [r | (r, txt, _, _, _) <- spans, txt == lbl]

-- | Whether the first rect contains the second.
covers :: Rect -> Rect -> Bool
covers (Rect cx cy cw ch) (Rect x y w h) =
  cx <= x && cy <= y && cx + cw >= x + w && cy + ch >= y + h

-- | Whether a frame's damage was a clip, and one that covers @rect@.
clipCovers :: Damage -> Rect -> Bool
clipCovers (DamageClip clip) rect = covers clip rect
clipCovers DamageFull _ = False

-- | Y origins of spans whose unmodified text exactly matches the label.
spanYOf :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanYOf lbl spans = [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == lbl]

-- | X origins of spans whose unmodified text exactly matches the label.
spanXOf :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanXOf lbl spans = [x | (Rect x _ _ _, txt, _, _, _) <- spans, txt == lbl]

-- | Check that a child's right edge ends at the scroller's content edge,
-- given gutter width and end padding in logical pixels.
assertScrollGutterPad ::
  HasCallStack
  => IORef Int
  -> Context
  -> WidgetId
  -> Response
  -> Float
  -> Float
  -> IO ()
assertScrollGutterPad failed ctx sid child gutter endPad = do
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> assert failed False
    Just (Rect sx _ sw _) -> do
      let
        Rect cx _ cw _ = respRect child
        contentRight = sx + sw - endPad - gutter
      assert failed (cx + cw >= contentRight - 0.5)
      assert failed (cx + cw <= contentRight + 0.01)

-- | Scroll an overlay by one wheel step and require its title to stay fixed
-- while its first body line moves upward. Optionally bound all spans' bottom edges.
assertWheelTitlePinned ::
  HasCallStack
  => IORef Int
  -> Context
  -> Input
  -> NanoUI a
  -> T.Text
  -> T.Text
  -> V2
  -> Maybe Float
  -> IO ()
assertWheelTitlePinned failed ctx inp0 ui title line1 wheelAt mClipMax = do
  spans0 <- collectOverlayTextSpans ctx inp0
  let
    titleYs0 = spanYOf title spans0
    line1Ys0 = spanYOf line1 spans0
  assert failed (not (null titleYs0))
  case line1Ys0 of
    [] -> assert failed False
    b0 : _ -> do
      let
        wheel = inp0 {inputMousePos = wheelAt, inputScroll = V2 0 1}
      _ <- runFrame ctx wheel ui
      spans1 <- collectOverlayTextSpans ctx wheel
      let
        titleYs1 = spanYOf title spans1
        line1Ys1 = spanYOf line1 spans1
      case (titleYs0, titleYs1) of
        (y0 : _, y1 : _) -> assertEq failed y1 y0
        _ -> assert failed False
      forM_ (listToMaybe line1Ys1) $ \b1 -> assertLt failed b1 b0
      forM_ mClipMax $ \maxY ->
        assert failed (not (any (\(Rect _ y _ h, _, _, _, _) -> y < 0 || y + h > maxY) spans1))

-- | Probe candidate y positions at a fixed x, running a frame for each, and
-- return the first input that produces a grab cursor.
findGrabHover ::
  Context -> NanoUI a -> Input -> Float -> [Float] -> IO (Maybe Input)
findGrabHover ctx ui inp0 thumbX = go
 where
  go [] = pure Nothing
  go (y : ys) = do
    let
      hover = inp0 {inputMousePos = V2 thumbX y}
    _ <- runFrame ctx hover ui
    kind <- uiCursorKind ctx hover
    if kind == UiCursorGrab then pure (Just hover) else go ys

-- | Press and move a resize handle, then run two button-up frames. Returns
-- the window's recorded bounds, or 'Nothing' if its node is absent.
dragWindowEdge ::
  Context
  -> Input
  -> NanoUI Response
  -> V2
  -> V2
  -> IO (Maybe Rect)
dragWindowEdge ctx inp0 ui grab dest = do
  runDragFrom ctx inp0 ui grab dest
  win <- warmup2 ctx (inp0 {inputMousePos = dest}) ui
  getPrevRect ctx (respId win)

-- | Read UV coordinates of a zero-based vertex. The index must be below
-- 'drawVertexCount' and the borrowed draw buffers must still be valid.
vertUv :: DrawData -> Int -> IO (Float, Float)
vertUv dd i =
  withForeignPtr (drawVertices dd) $ \p -> do
    let
      off = i * vertexSize
    u <- peekByteOff p (off + 24) :: IO Float
    v <- peekByteOff p (off + 28) :: IO Float
    pure (u, v)

-- | Require the transition to idle input to need a frame, then require that
-- frame's damage to cover the whole window.
checkIdleFullDamage ::
  HasCallStack => IORef Int -> Context -> Input -> Input -> NanoUI a -> IO ()
checkIdleFullDamage failed ctx inpAfter inpIdle ui = do
  need <- needsRedraw ctx inpAfter inpIdle
  assert failed need
  _ <- runFrame ctx inpIdle ui
  dmg <- takeDamage ctx
  assert failed (dmg == DamageFull)

-- | Check that right-aligned labels share an ink edge despite different glyph advances.
-- Lives here rather than with its test case because the pen and ink helpers
-- are internal to the library.
checkLabelAlignEndInk :: IORef Int -> IO ()
checkLabelAlignEndInk failed = do
  let
    gq xoff gw =
      GlyphQuad
        { gqX = xoff
        , gqY = 0
        , gqW = gw
        , gqH = 10
        , gqU0 = 0
        , gqV0 = 0
        , gqU1 = 1
        , gqV1 = 1
        }
    fm =
      (monospaceMetrics 10)
        { fmAdvance = \c -> case c of
            'i' -> 4
            '.' -> 4
            _ -> 10
        , fmGlyph = \c -> case c of
            'i' -> Just (gq 0.5 3)
            '.' -> Just (gq 1 1)
            _ -> Just (gq 1 8)
        }
    boxW = 100
    visualRight txt =
      let (tx, _) = alignedTextPen AlignEnd 0 boxW 0 fm txt
       in tx + textInkEnd fm txt
    r0 = visualRight "10"
    ri = visualRight "1i"
    rd = visualRight "1."
  when (abs (r0 - boxW) > 0.01) $ bump failed
  when (abs (ri - boxW) > 0.01) $ bump failed
  when (abs (rd - boxW) > 0.01) $ bump failed

-- | Point inside the standard floating-window title bar, away from its close button.
windowTitleGrab :: Rect -> V2
windowTitleGrab (Rect x0 y0 _ _) = V2 (x0 + 24) (y0 + padT windowPad + 19.5)

-- | Run press and held-move frames from one point to another. Leaves the button
-- held; the test supplies the release frame when needed.
runDragFrom :: Context -> Input -> NanoUI a -> V2 -> V2 -> IO ()
runDragFrom ctx inp0 ui grab dest = do
  let
    press = pressAt inp0 grab
  _ <- runFrame ctx press ui
  void (runFrame ctx (holdAt inp0 dest) ui)
