-- | Shared helpers for integration tests: input gestures, spans, scroll checks.
module NanoUI.Testing.Harness
  ( clickPair
  , rightClickPair
  , middleClickPair
  , pressAt
  , holdAt
  , releaseAt
  , keyInp
  , chordInp
  , keyUpInp
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
  , runClickReduce
  , assertSpansHas
  , spanYOf
  , spanXOf
  , spanRect
  , spanRectOf
  , covers
  , clipCovers
  , damageCovers
  , assertScrollGutterPad
  , assertWheelTitlePinned
  , findGrabHover
  , cursorOver
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
import Data.Typeable (Typeable)
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import GHC.Stack (HasCallStack)
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Font (alignedTextPen, textInkEnd)
import NanoUI.Internal.Types (clamp)
import NanoUI.Shortcut (Shortcut (..))
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertJustM, assertLt, bump, evalUi, run2Frames, withInput)

-- | Text bounds, text, foreground, background, and clip in logical window coordinates.
type DemoSpan = (Rect, T.Text, Color, Color, Rect)

-- | Decode the quads a frame actually rasterised: one @(rect, color)@ per
-- six-index quad, in draw order. Span and arena queries cannot see chrome
-- (scroller wells, scrollbar lanes); this can. Use only with quad-packed
-- output, such as square-geometry test contexts. Triangle and rounded-shape
-- output is not supported. Commands with counts not divisible by six throw.
drawQuads :: DrawData -> IO [(Rect, Color)]
drawQuads dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip ->
      fmap concat $
        forM (drawCmdElems dd) $ \c -> do
          let ioff = fromIntegral (cmdIndexOffset c)
              icnt = fromIntegral (cmdIndexCount c)
          when (icnt `rem` 6 /= 0) $
            error ("drawQuads: draw command packs " ++ show icnt ++ " indices; not quad-packed")
          forM [ioff, ioff + 6 .. ioff + icnt - 1] $ \q -> do
            -- Each corner as its x, y, r, g, b, a floats.
            corners <-
              forM [q .. q + 3] $ \ii -> do
                vi <- fromIntegral <$> (peekByteOff ip (ii * indexSize) :: IO Word32)
                forM [0 .. 5] $ \k -> peekByteOff vp (vi * vertexSize + 4 * k) :: IO Float
            let xs = [x | x : _ <- corners]
                ys = [y | _ : y : _ <- corners]
                toW8 f = clamp 0 255 (round (f * 255))
            pure $ case corners of
              [_, _, r, g, b, a] : _ ->
                ( Rect (minimum xs) (minimum ys) (maximum xs - minimum xs) (maximum ys - minimum ys)
                , colorRGBA (toW8 r) (toW8 g) (toW8 b) (toW8 a)
                )
              _ -> (Rect 0 0 0 0, colorRGBA 0 0 0 0)

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
clickPair inp pos = let press = pressAt inp pos in (press, releaseAt press)

-- | Right- or middle-button press and release at a point. Supply event-free
-- base input.
rightClickPair, middleClickPair :: Input -> V2 -> (Input, Input)
rightClickPair = buttonPair MouseRight (\i -> i {inputMouseRightPressed = False})
middleClickPair = buttonPair MouseMiddle (\i -> i {inputMouseMiddlePressed = False})

buttonPair :: MouseButton -> (Input -> Input) -> Input -> V2 -> (Input, Input)
buttonPair mouseButton unpress inp pos =
  let press = applyMouseButton mouseButton True inp {inputMousePos = pos}
   in (press, applyMouseButton mouseButton False (unpress press))

-- | Set pointer position and left-button press/held flags, clearing its release flag.
pressAt :: Input -> V2 -> Input
pressAt inp pos =
  (applyMouseButton MouseLeft True inp {inputMousePos = pos}) {inputMouseReleased = False}

-- | The button still down from an earlier 'pressAt', with the pointer at @pos@.
holdAt :: Input -> V2 -> Input
holdAt inp pos = (pressAt inp pos) {inputMousePressed = False}

-- | Release the left button at its current position, clearing its press/held flags.
releaseAt :: Input -> Input
releaseAt press = applyMouseButton MouseLeft False press {inputMousePressed = False}

-- | A single key-down frame.
keyInp :: Key -> Input -> Input
keyInp k inp = inp {inputKeys = inputKeysFromList [k]}

-- | A frame pressing a chord, such as @ctrl <> key 'a'@: its key goes down
-- with exactly the chord's modifiers held.
chordInp :: Shortcut -> Input -> Input
chordInp (Shortcut k mods) inp =
  (maybe id (`applyKey` True) k inp {inputKeys = mempty}) {inputModifiers = mods}

-- | A frame releasing a key, which leaves the held keys.
keyUpInp :: Key -> Input -> Input
keyUpInp k inp = applyKey k False inp {inputKeys = mempty, inputKeysReleased = mempty}

-- | Step the tab focus to the next focusable.
tabInp :: Input -> Input
tabInp = keyInp KeyTab

-- | Event-free input with the requested window size and pointer at (-10,-10).
withInputOff :: Float -> Float -> Input
withInputOff w h = (withInput w h) {inputMousePos = V2 (-10) (-10)}

-- | Event-free input with logical width/height and elapsed time in seconds.
withDelta :: Float -> Float -> Float -> Input
withDelta w h dt = (withInput w h) {inputDeltaTime = dt}

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
warmupDraw ctx inp ui = (\(a, _, draw, _) -> (a, draw)) <$> run2Frames ctx inp ui

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
runClick ctx inp0 ui pos = let (press, release) = clickPair inp0 pos in warmup ctx press ui >> evalUi ctx release ui

-- | Run left press and release frames ('clickPair') through a reducer. Returns
-- the final model, release-frame messages, and release-frame dirty flag.
runClickReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> NanoUI Response)
  -> V2
  -> IO (model, [msg], Bool)
runClickReduce reduce ctx inp0 model0 view pos = do
  let (press, release) = clickPair inp0 pos
  (_, modelP, _, _, _) <- runFrameReduce reduce ctx press model0 view
  (_, modelR, msgs, _, dirty) <- runFrameReduce reduce ctx release modelP view
  pure (modelR, msgs, dirty)

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

-- | Whether a frame's damage covers @rect@, the whole window included.
damageCovers :: Damage -> Rect -> Bool
damageCovers DamageFull _ = True
damageCovers dmg rect = clipCovers dmg rect

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
assertScrollGutterPad failed ctx sid child gutter endPad =
  assertJustM failed (getPrevRect ctx sid) $ \(Rect sx _ sw _) -> do
    let
      Rect cx _ cw _ = respRect child
      contentRight = sx + sw - endPad - gutter
    assert failed (cx + cw >= contentRight - 0.5)
    assert failed (cx + cw <= contentRight + 0.01)

-- | Scroll an overlay by one wheel step and require its title to stay fixed
-- while its first body line moves upward (if it is still shown).
assertWheelTitlePinned ::
  HasCallStack
  => IORef Int
  -> Context
  -> Input
  -> NanoUI a
  -> T.Text
  -> T.Text
  -> V2
  -> IO ()
assertWheelTitlePinned failed ctx inp0 ui title line1 wheelAt = do
  let wheel = inp0 {inputMousePos = wheelAt, inputScroll = V2 0 1}
  spans0 <- collectOverlayTextSpans ctx inp0
  _ <- runFrame ctx wheel ui
  spans1 <- collectOverlayTextSpans ctx wheel
  case (spanYOf title spans0, spanYOf title spans1, spanYOf line1 spans0) of
    (y0 : _, y1 : _, b0 : _) -> do
      assertEq failed y1 y0
      forM_ (listToMaybe (spanYOf line1 spans1)) $ \b1 -> assertLt failed b1 b0
    _ -> assert failed False

-- | Run a frame with the pointer at @pos@ and return the cursor it asks for.
cursorOver :: Context -> Input -> NanoUI a -> V2 -> IO UiCursorKind
cursorOver ctx inp ui pos = do
  let hover = inp {inputMousePos = pos}
  _ <- runFrame ctx hover ui
  uiCursorKind ctx hover

-- | Probe candidate y positions at a fixed x, running a frame for each, and
-- return the first input that produces a grab cursor.
findGrabHover ::
  Context -> NanoUI a -> Input -> Float -> [Float] -> IO (Maybe Input)
findGrabHover ctx ui inp0 thumbX = go
 where
  go [] = pure Nothing
  go (y : ys) = do
    kind <- cursorOver ctx inp0 ui (V2 thumbX y)
    if kind == UiCursorGrab then pure (Just inp0 {inputMousePos = V2 thumbX y}) else go ys

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
  withForeignPtr (drawVertices dd) $ \p ->
    (,) <$> peekByteOff p (i * vertexSize + 24) <*> peekByteOff p (i * vertexSize + 28)

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
checkLabelAlignEndInk failed =
  forM_ ["10", "1i", "1."] $ \txt -> do
    let (tx, _) = alignedTextPen AlignEnd 0 boxW 0 fm txt
    when (abs (tx + textInkEnd fm txt - boxW) > 0.01) $ bump failed
  where
    boxW = 100
    -- Narrow 'i' and '.' advances, with ink offset inside each glyph box.
    glyph xoff gw = Just (GlyphQuad xoff 0 gw 10 0 0 1 1)
    fm =
      (monospaceMetrics 10)
        { fmAdvance = \c -> if c == 'i' || c == '.' then 4 else 10
        , fmGlyph = \c -> case c of
            'i' -> glyph 0.5 3
            '.' -> glyph 1 1
            _ -> glyph 1 8
        }

-- | Point inside the standard floating-window title bar, away from its close button.
windowTitleGrab :: Rect -> V2
windowTitleGrab (Rect x0 y0 _ _) = V2 (x0 + 24) (y0 + padT windowPad + 19.5)

-- | Run press and held-move frames from one point to another. Leaves the button
-- held; the test supplies the release frame when needed.
runDragFrom :: Context -> Input -> NanoUI a -> V2 -> V2 -> IO ()
runDragFrom ctx inp0 ui grab dest =
  forM_ [pressAt inp0 grab, holdAt inp0 dest] $ \inp -> runFrame ctx inp ui
