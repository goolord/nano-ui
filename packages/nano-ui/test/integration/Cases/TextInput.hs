module Cases.TextInput (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import NanoUI.Internal.Context (intKey)
import NanoUI.Internal.Frame.TextArea
  ( TextAreaBars (..)
  , TextAreaHit (..)
  , resolveTextAreaFont
  , textAreaContentMetrics
  , textAreaBarLane
  , textAreaBarLayouts
  , textAreaBars
  , textAreaLineHeight
  , textAreaHitForWidget
  )
import NanoUI.Internal.Store
  ( Slot (..)
  , WidgetStore (..)
  , slotKey
  )
import NanoUI.Internal.Widgets.TextArea
  ( buffer
  , loadTextAreaState
  , selectionAnchor
  )
import NanoUI.Shortcut
import NanoUI.Widgets.TextBuffer
  ( fromText
  , getCursor
  , lineAt
  , toLines
  , toText
  )

tests :: [Spec]
tests =
  [ spec "refresh-forces-redraw" runRefreshRedrawTest
  , spec "text-undo" runTextUndoTest
  , spec "text-area-width-tracking" runTextAreaWidthTrackingTest
  , spec "text-area-document" runTextAreaDocumentTest
  , spec "text-area-changed-means-edited" runTextAreaChangedTest
  , spec "text-area-spans" runTextAreaSpansTest
  , spec "text-input-cursor" runTextInputCursorTest
  , spec "text-input-batch" runTextInputBatchTest
  , spec "text-input-selection" runTextInputSelectionTest
  , spec "text-input-mouse-selection" runTextInputMouseSelectionTest
  , spec "text-input-click-select" runTextInputClickSelectTest
  , spec "text-input-word-keys" runTextInputWordKeysTest
  , spec "text-input-line-break" runTextInputLineBreakTest
  , spec "text-input-cut-clears-selection" runTextInputCutClearsSelectionTest
  , spec "text-input-clipboard" runTextInputClipboardTest
  , spec "text-input-password" runTextInputPasswordTest
  , spec "text-input-menu" runTextInputMenuTest
  , spec "text-input-ff-caret" runTextInputFfCaretTest
  , pixelSpec "text-input-focus-sdl" runTextInputFocusSdlTest
  , pixelSpec "text-input-scroll" runTextInputScrollTest
  , spec "text-input-font-scroll" runTextInputFontScrollTest
  , spec "text-input-collapse-untouched" runTextInputCollapseUntouchedTest
  , spec "text-input-font-inset" runTextInputFontInsetTest
  , spec "text-input-dirty" runTextInputDirtyTest
  , spec "text-input-drag-wake" runTextInputDragWakeTest
  , spec "text-area-hold-still" runTextAreaHoldStillTest
  , spec "text-area-cut-clears-selection" runTextAreaCutClearsSelectionTest
  , pixelSpec "text-area-scroll-wheel" runTextAreaScrollWheelTest
  , pixelSpec "text-area-zoom-scroll" runTextAreaZoomScrollTest
  , pixelSpec "text-area-remount-scroll" runTextAreaRemountScrollTest
  , pixelSpec "text-area-menu-pulse" runTextAreaMenuPulseTest
  , pixelSpec "text-area-menu-select-all" runTextAreaMenuSelectAllTest
  , spec "text-command-focus" runTextCommandFocusTest
  , pixelSpec "text-area-scroll-drag" runTextAreaScrollDragTest
  , pixelSpec "text-area-cursor-on-scrollbar" runTextAreaCursorOnScrollBarTest
  , pixelSpec "text-area-hscroll-wheel" runTextAreaHScrollWheelTest
  , pixelSpec "text-area-hscroll-drag" runTextAreaHScrollDragTest
  , pixelSpec "text-area-2d-scroll" runTextArea2DScrollTest
  , pixelSpec "text-area-scroll-cursor-leaves-viewport" runTextAreaScrollCursorLeavesViewportTest
  , pixelSpec "text-area-caret-follow" runTextAreaCaretFollowTest
  , pixelSpec "text-area-caret-above-bar" runTextAreaCaretAboveBarTest
  , pixelSpec "text-area-snapped-rows" runTextAreaSnappedRowsTest
  ]

runTextInputBatchTest :: Context -> IORef Int -> IO ()
runTextInputBatchTest ctx failed = do
  let
    inp = withInput 320 120
    ui = column (withValue "aOLDz" textInput')
    left = keyInp KeyLeft inp
    step event = runFrame ctx event ui
  (resp, _) <- warmup2 ctx inp ui
  _ <- step (tabInp inp)
  _ <- step left
  replicateM_ 3 (step (left {inputModifiers = Modifiers True False False False}))
  let
    checkSelection cursor anchor = do
      ints <- storeInt <$> getStore ctx
      let slot = intKey (respId resp)
      assertEq failed (IM.lookup (slotKey SlotCursor slot) ints) (Just cursor)
      assertEq failed (IM.lookup (slotKey SlotAnchor slot) ints) (Just anchor)
  checkSelection 1 4
  -- An event filtered to nothing must not delete the current selection.
  ((_, unchanged), _, _) <- step (inp {inputChars = "\n\t"})
  assertEq failed unchanged "aOLDz"
  checkSelection 1 4
  -- Navigation follows text insertion within a frame.
  ((_, committed), _, _) <- step (left {inputChars = "é\n世界\t"})
  assertEq failed committed "aé世界z"
  checkSelection 3 3

-- | A text area's rows are text spans, for hosts that draw text themselves.
runTextAreaSpansTest :: Context -> IORef Int -> IO ()
runTextAreaSpansTest ctx failed = do
  let ui = column (labeledArea "Notes" "alpha\nbeta")
  _ <- warmup2 ctx (withInput 400 300) ui
  spans <- collectTextSpans ctx
  assert failed (hasText "alpha" spans && hasText "beta" spans)

-- | A text area under a separate label ('labeledInput' for a text input).
labeledArea :: T.Text -> T.Text -> NanoUI (Response, T.Text)
labeledArea lbl initial = label lbl >> withValue initial textArea'

labeledInput :: T.Text -> T.Text -> NanoUI (Response, T.Text)
labeledInput lbl initial =
  label lbl >> withValue initial (textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Enter " <> lbl})

-- | Forty numbered lines, taller than a text area.
fortyLines :: T.Text
fortyLines = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]

-- | The top middle of each span whose text passes the test.
spanTop :: (T.Text -> Bool) -> [(Rect, T.Text, a, b, c)] -> [V2]
spanTop ok spans = [V2 (rectX r + rectW r / 2) (rectY r + 0.5) | (r, txt, _, _, _) <- spans, ok txt]

runTextInputCursorTest :: Context -> IORef Int -> IO ()
runTextInputCursorTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column (labeledInput "Name" "")
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case (spanTop (== "Name") spans, spanTop ("Enter" `T.isInfixOf`) spans) of
    ([labelPos], [fieldPos]) -> do
      assertEq failed UiCursorDefault =<< cursorOver ctx inp0 ui labelPos
      assertEq failed UiCursorText =<< cursorOver ctx inp0 ui fieldPos
      assertEq failed UiCursorText =<< cursorOver ctx (pressAt inp0 fieldPos) ui fieldPos
    _ -> assert failed False

runTextInputCutClearsSelectionTest :: Context -> IORef Int -> IO ()
runTextInputCutClearsSelectionTest ctx failed = do
  textRef <- newVar "hello"
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  let
    inp0 = withInput 320 120
    ui = column (held textRef textInput')
  warmupFocused ctx' inp0 ui
  _ <- runFrame ctx' (chordInp (shift <> key KeyLeft) inp0) ui
  _ <- runFrame ctx' (chordInp (ctrl <> key 'x') inp0) ui
  assertEq failed (Just "o") =<< readIORef clipRef
  assertEq failed "hellz" . snd =<< evalUi ctx' (inp0 {inputChars = "z"}) ui

-- | A field whose value holds a line break keeps its caret as an offset into
-- the whole value: Left from the end, then typing, inserts before the last
-- character, and Select All selects every character.
runTextInputLineBreakTest :: Context -> IORef Int -> IO ()
runTextInputLineBreakTest ctx failed = do
  textRef <- newVar "ab\ncd"
  let
    inp0 = withInput 320 120
    ui = column (held textRef textInput')
  (resp, _) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- runFrame ctx (keyInp KeyLeft inp0) ui
  assertEq failed "ab\ncXd" . snd =<< evalUi ctx (inp0 {inputChars = "X"}) ui
  _ <- runFrame ctx (chordInp (ctrl <> key 'a') inp0) ui
  ints <- storeInt <$> getStore ctx
  assertEq failed (Just 6) (IM.lookup (slotKey SlotCursor (intKey (respId resp))) ints)

-- Word-wise editing keys (Ctrl or Alt + Backspace/Delete/Left/Right) work in
-- the single-line text input like they do in the text area.
runTextInputWordKeysTest :: Context -> IORef Int -> IO ()
runTextInputWordKeysTest ctx failed = do
  textRef <- newVar "hello world"
  let
    inp0 = withInput 320 120
    ui = column (held textRef textInput')
    -- Run the chord's frame, then return the text a frame later.
    chordThen chord next = runFrame ctx (chordInp chord inp0) ui >> snd <$> evalUi ctx next ui
  warmupFocused ctx inp0 ui
  -- Ctrl+Backspace deletes the word before the cursor ("world").
  assertEq failed "hello " =<< chordThen (ctrl <> key KeyBackspace) inp0
  -- Nothing right of the cursor at end of text: Ctrl+Delete is a no-op.
  assertEq failed "hello " =<< chordThen (ctrl <> key KeyDelete) inp0
  -- Ctrl+Left jumps to the start; typing there proves the cursor moved.
  assertEq failed "Xhello " =<< chordThen (ctrl <> key KeyLeft) inp0 {inputChars = "X"}
  -- Ctrl+Delete removes the word after the cursor ("hello").
  assertEq failed "X " =<< chordThen (ctrl <> key KeyDelete) inp0

runTextAreaCutClearsSelectionTest :: Context -> IORef Int -> IO ()
runTextAreaCutClearsSelectionTest ctx failed = do
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  textRef <- newVar "hello"
  let
    inp0 = withInput 320 220
    ui = column (label "Notes" >> held textRef textArea')
  warmupFocused ctx' inp0 ui
  _ <- runFrame ctx' (chordInp (ctrl <> key 'a') inp0) ui
  (_, cutVal) <- evalUi ctx' (chordInp (ctrl <> key 'x') inp0) ui
  assertEq failed (Just "hello") =<< readIORef clipRef
  assertEq failed cutVal ""
  assertEq failed "z" . snd =<< evalUi ctx' (inp0 {inputChars = "z"}) ui

-- A text area's respChanged reports an edit to its text, as a text input's
-- does: moving the caret or selecting leaves it unset.
runTextAreaChangedTest :: Context -> IORef Int -> IO ()
runTextAreaChangedTest ctx failed = do
  textRef <- newVar "hello"
  let inp0 = withInput 320 220
      ui = column (fst <$> held textRef textArea')
  warmupFocused ctx inp0 ui
  assert failed . not . respChanged =<< evalUi ctx (keyInp KeyLeft inp0) ui
  assert failed . not . respChanged =<< evalUi ctx (chordInp (ctrl <> key 'a') inp0) ui
  assert failed . respChanged =<< evalUi ctx (inp0 {inputChars = "z"}) ui
  assertEq failed "z" =<< readVarIO textRef

runTextInputSelectionTest :: Context -> IORef Int -> IO ()
runTextInputSelectionTest ctx failed = do
  textRef <- newVar "hello"
  let
    inp0 = withInput 320 120
    ui = column (button "Other" >> held textRef textInput')
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- warmup2 ctx (chordInp (shift <> key KeyLeft) inp0) ui
  assertEq failed "helX" . snd =<< evalUi ctx (inp0 {inputChars = "X"}) ui
  -- Ctrl+A selects all and types nothing, even when the backend also sends
  -- the letter as text.
  _ <- runFrame ctx (inp0 {inputChars = "abc"}) ui
  _ <- runFrame ctx ((chordInp (ctrl <> key 'a') inp0) {inputChars = "a"}) ui
  assertEq failed "" . snd =<< evalUi ctx (keyInp KeyBackspace inp0) ui

runTextInputMouseSelectionTest :: Context -> IORef Int -> IO ()
runTextInputMouseSelectionTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column (withValue "hello" textInput')
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "hello" spans) $ \(Rect fx fy fw fh) -> do
    let
      fieldY = fy + fh / 2
      dragged = holdAt inp0 (V2 (fx + fw - 1) fieldY)
    mapM_ (\i -> runFrame ctx i ui) [pressAt inp0 (V2 (fx + 1) fieldY), dragged, releaseAt dragged]
    assertEq failed "z" . snd =<< evalUi ctx (inp0 {inputChars = "z"}) ui

-- Double-click selects a word and triple-click the whole line.
runTextInputClickSelectTest :: Context -> IORef Int -> IO ()
runTextInputClickSelectTest ctx failed = do
  allCtx <- newContext
  let
    inp0 = withInput 320 120
    -- Click n times at the start of the field showing txt, then Backspace.
    clicksThenBackspace c txt n = do
      let
        ui = column (withValue txt textInput')
      _ <- warmup2 c inp0 ui
      spans <- collectTextSpans c
      forM (spanRectOf txt spans) $ \(Rect fx fy _ fh) -> do
        forM_ [1 .. n] $ \k ->
          runFrame c (pressAt inp0 (V2 (fx + 1) (fy + fh / 2))) {inputMouseClicks = k} ui
        snd <$> evalUi c (keyInp KeyBackspace inp0) ui
  assertEq failed (Just " world") =<< clicksThenBackspace ctx "hello world" 2
  assertEq failed (Just "") =<< clicksThenBackspace allCtx "hello" 3

runTextInputClipboardTest :: Context -> IORef Int -> IO ()
runTextInputClipboardTest ctx failed = do
  textRef <- newVar "hello"
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  let
    inp0 = withInput 320 120
    ui = column (held textRef textInput')
  warmupFocused ctx' inp0 ui
  let chord c = runFrame ctx' (chordInp c inp0) ui
  _ <- chord (ctrl <> key 'a') >> chord (ctrl <> key 'c')
  assertEq failed (Just "hello") =<< readIORef clipRef
  _ <- chord (ctrl <> key 'a') >> runFrame ctx' (keyInp KeyBackspace inp0) ui
  assertEq failed "hello" . snd =<< evalUi ctx' (chordInp (ctrl <> key 'v') inp0) ui

-- A password field displays one mask character per character, and Ctrl+C
-- leaves the clipboard untouched while the field keeps its real value.
runTextInputPasswordTest :: Context -> IORef Int -> IO ()
runTextInputPasswordTest ctx failed = do
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  let
    inp0 = withInput 320 120
    ui = column (withValue "hunter2" (textInputConfigured' defaultTextInputConfig {ticPassword = True}))
  _ <- warmup2 ctx' inp0 ui
  spans <- collectTextSpans ctx'
  assert failed (not (hasText "hunter2" spans))
  assertSpansHas failed "*******" spans
  _ <- runFrame ctx' (tabInp inp0) ui
  _ <- runFrame ctx' (chordInp (ctrl <> key 'a') inp0) ui
  (_, val) <- evalUi ctx' (chordInp (ctrl <> key 'c') inp0) ui
  assertEq failed Nothing =<< readIORef clipRef
  assertEq failed val "hunter2"

-- The text input's context menu offers Paste even while unfocused, and its
-- Paste and Cut entries edit the field through the clipboard. The menu edits
-- the field between frames, so each edit must survive the caller passing back
-- the result of the frame before it.
runTextInputMenuTest :: Context -> IORef Int -> IO ()
runTextInputMenuTest ctx failed = do
  textRef <- newVar "hello"
  (ctx', clipRef) <- memoryClipboard (Just "pasted") ctx
  let
    inp0 = withInput 320 160
    ui = column (held textRef textInput')
  _ <- warmup2 ctx' inp0 ui
  spans <- collectTextSpans ctx'
  assertJust failed (spanRectOf "hello" spans) $ \(Rect fx fy _ fh) -> do
    let
      menuOpen = fst (rightClickPair inp0 (V2 (fx + 1) (fy + fh / 2)))
      pick entry = do
        _ <- runFrame ctx' menuOpen ui
        overlays <- collectOverlayTextSpans ctx' menuOpen
        forM (spanRectOf entry overlays) $ \r -> do
          _ <- runClick ctx' inp0 ui (spanCenter r)
          snd <$> evalUi ctx' inp0 ui
    assertEq failed (Just "hellopasted") =<< pick "Paste"
    cut <- pick "Cut"
    assertEq failed (Just "hellopasted") =<< readIORef clipRef
    assertEq failed cut (Just "")
    overlaysAfter <- collectOverlayTextSpans ctx' inp0
    assert failed (null (spanYOf "Cut" overlaysAfter))

runTextInputFocusSdlTest :: Context -> IORef Int -> IO ()
runTextInputFocusSdlTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column (labeledInput "Name" "")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case spanTop ("Enter" `T.isInfixOf`) spans of
    [pos] -> do
      _ <- runFrame ctx (pressAt inp0 pos) ui
      assertEq failed (respId resp) =<< getFocusId ctx
      samples <- replicateM 5 (runFrame ctx inp0 {inputMousePos = pos} ui >> getFocusId ctx)
      assertEq failed samples (replicate 5 (respId resp))
    _ -> assert failed False

runTextInputDirtyTest :: Context -> IORef Int -> IO ()
runTextInputDirtyTest ctx failed = do
  let
    ui = column (withValue "" textInput')
    inp0 = (withInput 200 100) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry _ _ = respRect resp
    (_, release) = clickPair inp0 (V2 (rx + 1) (ry + 0.5))
    idle = release {inputButtonsReleased = noButtons, inputDeltaTime = 1}
  _ <- runClick ctx inp0 ui (V2 (rx + 1) (ry + 0.5))
  _ <- warmup2 ctx idle ui
  -- Keyboard focus by itself asks for nothing: the caret does not blink, and
  -- typing arrives as input. A focused field must let the loop sleep.
  assert failed . not =<< needsRedraw ctx idle idle
  assertEq failed 0 =<< getWakeAt ctx
  assert failed =<< needsRedraw ctx idle idle {inputChars = "x"}

-- A selection drag held still past the field's edge keeps asking for frames,
-- so the text goes on scrolling under it. Inside the field, and once the
-- button is up, nothing is asked for and the loop sleeps.
runTextInputDragWakeTest :: Context -> IORef Int -> IO ()
runTextInputDragWakeTest ctx failed = do
  let
    ui = column (withValue "some text to select" textInput')
    inp0 = (withInput 200 100) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry rw _ = respRect resp
    outside = V2 (rx + rw + 40) (ry + 4)
    (press, release) = clickPair inp0 (V2 (rx + 4) (ry + 4))
    hold = press {inputButtonsPressed = noButtons}
  _ <- runFrame ctx press ui
  _ <- runFrame ctx hold ui
  assertEq failed 0 =<< getWakeAt ctx
  _ <- runFrame ctx hold {inputMousePos = outside} ui
  assert failed . (> 0) =<< getWakeAt ctx
  _ <- runFrame ctx release {inputMousePos = outside} ui
  _ <- runFrame ctx release {inputMousePos = outside, inputButtonsReleased = noButtons} ui
  assertEq failed 0 =<< getWakeAt ctx

-- | A button held still in a text area after a click selects nothing new, so
-- its frames ask for no other frame.
runTextAreaHoldStillTest :: Context -> IORef Int -> IO ()
runTextAreaHoldStillTest ctx failed = do
  let
    ui = column (withValue "some text\nto select" textArea')
    inp0 = (withInput 320 220) {inputMousePos = V2 300 200}
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry _ _ = respRect resp
    press = pressAt inp0 (V2 (rx + 20) (ry + 10))
    hold = press {inputButtonsPressed = noButtons}
  _ <- runFrame ctx press ui
  _ <- runFrame ctx hold ui
  (_, _, dirty) <- runFrame ctx hold ui
  assert failed (not dirty)
  assertEq failed 0 =<< getWakeAt ctx

runTextInputFfCaretTest :: Context -> IORef Int -> IO ()
runTextInputFfCaretTest ctx failed = do
  let
    fm = ctxFontMetrics ctx
    fs = T.replicate 6 "f"
    adv = fmAdvance fm 'f'
  assertEq failed (lineWidth fm fs) (6 * adv)
  assertEq failed (textIndexAtX fm fs (lineWidth fm fs)) 6
  assertEq failed (textIndexAtX fm fs (lineWidth fm (T.take 3 fs))) 3
  let
    inp0 = withInput 320 120
    ui = column (withValue fs textInput')
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  assertSpansHas failed fs spans
  assertJust failed (spanRectOf fs spans) $ \(Rect fx fy _ fh) -> do
    let pos = V2 (fx + lineWidth fm (T.take 3 fs)) (fy + fh / 2)
    _ <- runClick ctx inp0 ui pos
    assertEq failed "fffxfff" . snd =<< evalUi ctx (inp0 {inputMousePos = pos, inputChars = "x"}) ui

runTextInputScrollTest :: Context -> IORef Int -> IO ()
runTextInputScrollTest ctx failed = do
  let
    longText = "VeryLongTextEnteredIntoTheFieldThatExceedsTheWidth"
    inp0 = withInput 200 120
    ui = column (withValue longText textInput')
  (resp, _) <- warmup2 ctx inp0 ui
  spans0 <- collectTextSpans ctx
  assertJust failed (spanRectOf longText spans0) $ \(Rect fx fy _ fh) -> do
    _ <- runClick ctx inp0 ui (V2 (fx + 50) (fy + fh / 2))
    _ <- runFrame ctx (keyInp KeyEnd inp0) ui
    assertLt failed 0 =<< fieldScrollX ctx resp
    _ <- runFrame ctx (keyInp KeyHome inp0) ui
    assertEq failed 0 =<< fieldScrollX ctx resp

-- | A text field's horizontal scroll offset.
fieldScrollX :: Context -> Response -> IO Float
fieldScrollX ctx resp =
  IM.findWithDefault 0 (slotKey SlotTextInputScroll (intKey (respId resp))) . storeFloat <$> getStore ctx

-- | A field in its own, larger font scrolls its caret into view by that font's
-- widths, which draw it: ten 32 px cells put the end caret 320 px in, past a
-- 200 px field, where the 12 px base font would put it at 120 px and not scroll.
runTextInputFontScrollTest :: Context -> IORef Int -> IO ()
runTextInputFontScrollTest base failed = do
  let
    large = monospaceMetrics 32
    ctx =
      withFontResolver
        (withFontMetrics base (monospaceMetrics 12))
        (\_ _ _ _ -> pure (large, False))
        (\_ _ _ _ txt -> pure (32 * fromIntegral (T.length txt), 32))
    inp0 = withInput 400 200
    cfg = defaultTextInputConfig {ticLayout = fontSize 32 (fixedW 200 (ticLayout defaultTextInputConfig))}
    ui = column (withValue "HgHgHgHgHg" (textInputConfigured' cfg))
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry _ rh = respRect resp
  _ <- runClick ctx inp0 ui (V2 (rx + 20) (ry + rh / 2))
  _ <- runFrame ctx (keyInp KeyEnd inp0) ui
  scrollX <- fieldScrollX ctx resp
  assert failed (scrollX >= 320 + 1 - 200)

-- | A field focused without being edited or clicked, by Tab, stores no caret:
-- its caret reads as the end of its text. A press elsewhere collapses its
-- selection onto that caret, so Tab back and typing appends, where a collapse
-- that read the missing caret as the start stored the whole text selected and
-- the typing replaced it.
runTextInputCollapseUntouchedTest :: Context -> IORef Int -> IO ()
runTextInputCollapseUntouchedTest ctx failed = do
  let
    inp0 = withInputOff 400 200
    ui = column (withValue "hello" textInput')
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- runClick ctx inp0 ui (V2 390 190)
  _ <- runFrame ctx (tabInp inp0) ui
  assertEq failed "helloX" . snd =<< evalUi ctx (inp0 {inputChars = "X"}) ui

-- | A field in its own, larger font starts its caret and hit-testing where it
-- draws its glyphs: at the content inset of its font. In 24 px cells over a
-- 12 px base font, "abcdef" draws from 30 px into the field. A press just past
-- the start of the "c" puts the caret before it, where an inset taken from the
-- base font would have put the caret one cell later.
runTextInputFontInsetTest :: Context -> IORef Int -> IO ()
runTextInputFontInsetTest base failed = do
  let
    ctx = withMonospaceFonts 12 24 base
    inp0 = withInputOff 400 200
    cfg = defaultTextInputConfig {ticLayout = fontSize 24 (fixedW 300 (ticLayout defaultTextInputConfig))}
    ui = column (withValue "abcdef" (textInputConfigured' cfg))
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case [r | (r, "abcdef", _, _, _) <- spans] of
    [Rect penX penY _ penH] -> do
      assertEq failed (rectX (respRect resp) + 24 * 1.25) penX
      _ <- runClick ctx inp0 ui (V2 (penX + 2 * 24 + 4) (penY + penH / 2))
      assertEq failed "abXcdef" . snd =<< evalUi ctx (inp0 {inputChars = "X"}) ui
    found -> do
      putStrLn ("text-input-font-inset: value spans " <> show found)
      assert failed False

runTextAreaScrollWheelTest :: Context -> IORef Int -> IO ()
runTextAreaScrollWheelTest ctx failed = do
  let
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" fortyLines)
    uiShort = column (withKey (1 :: Int) (labeledArea "Short" "Line 1\nLine 2"))
    -- Scroll by @notches@ over the field; return its scroll offset.
    wheel view resp r notches = do
      _ <- runFrame ctx (inp0 {inputMousePos = spanCenter r, inputScroll = V2 0 notches}) view
      getScrollOffset ctx (respId resp)
  -- Text that fits the viewport does not wheel-scroll.
  (respShort, _) <- warmup2 ctx inp0 uiShort
  assertJustM failed (getPrevRect ctx (respId respShort)) $ \r ->
    assertEq failed 0 =<< wheel uiShort respShort r 1
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \r -> do
    assertEq failed 0 =<< getScrollOffset ctx (respId resp)
    off1 <- wheel ui resp r 1
    assertGt failed off1 0
    off2 <- wheel ui resp r 3
    assertGt failed off2 off1
    -- Scrolling up past the top clamps to 0.
    assertEq failed 0 =<< wheel ui resp r (-10)
    -- The text is untouched.
    st <- (`loadTextAreaState` intKey (respId resp)) <$> getStore ctx
    assertEq failed (toText (buffer st)) fortyLines

runTextAreaZoomScrollTest :: Context -> IORef Int -> IO ()
runTextAreaZoomScrollTest ctx failed = do
  let
    inp0 = withInput 320 220
    ui = column $ withValue fortyLines (textAreaWith' (fontSize 32))
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
    fm <- resolveTextAreaFont ctx (tahNodeIdx hit)
    let
      field = tahFieldRect hit
      lineH = tahLineH hit
      lineCount = max 1 (length (toLines (fromText fortyLines)))
      contentH = fromIntegral lineCount * lineH
      contentW = maximum (0 : [lineWidth fm l | l <- T.lines fortyLines])
      (ix, iy) = widgetContentInset fm
      innerW = rectW field - 2 * ix
      innerH = rectH field - 2 * iy
      hasV0 = contentH > innerH
      hasH = contentW > (if hasV0 then max 0 (innerW - textAreaBarLane) else innerW)
      availH = if hasH then max 0 (innerH - textAreaBarLane) else innerH
      expectedMaxY = max 0 (contentH - availH)
    _ <- runFrame ctx inp0 {inputMousePos = spanCenter field, inputScroll = V2 0 100} ui
    off <- getScrollOffset ctx (respId resp)
    assert failed (abs (off - expectedMaxY) < 0.5)

runTextAreaScrollDragTest, runTextAreaHScrollDragTest :: Context -> IORef Int -> IO ()
runTextAreaScrollDragTest = textAreaThumbDragTest False
runTextAreaHScrollDragTest = textAreaThumbDragTest True

-- | Dragging a text area's vertical (or, when @horizontal@, horizontal)
-- thumb scrolls it without selecting or editing the text.
textAreaThumbDragTest :: Bool -> Context -> IORef Int -> IO ()
textAreaThumbDragTest horizontal ctx failed = do
  let
    txt
      | horizontal = T.replicate 15 "0123456789"
      | otherwise = fortyLines
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" txt)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    fm = ctxFontMetrics ctx
    offset = (if horizontal then v2X else v2Y) <$> getScrollOffset2D ctx (respId resp)
  assertJustM failed (getPrevRect ctx (respId resp)) $ \field -> do
    off0 <- offset
    assertEq failed off0 0
    let
      bar
        | horizontal = snd (textAreaBarLayouts field (textAreaBars fm field (lineWidth fm txt) 0) off0 0)
        | otherwise = fst (textAreaBarLayouts field (textAreaBars fm field 0 (40 * textAreaLineHeight fm)) 0 off0)
    assertJust failed bar $ \layout -> do
      let
        V2 cx cy = spanCenter (sbThumb layout)
        press = pressAt inp0 (V2 cx cy)
        -- 30 pixels along the track.
        drag = holdAt press (if horizontal then V2 (cx + 30) cy else V2 cx (cy + 30))
      _ <- runFrame ctx press ui
      _ <- runFrame ctx drag ui
      off1 <- offset
      assertGt failed off1 off0
      _ <- runFrame ctx (applyMouseButton MouseLeft False drag) ui
      -- Dragging the scrollbar must not start a selection or edit the text.
      st <- (`loadTextAreaState` intKey (respId resp)) <$> getStore ctx
      assertEq failed (toText (buffer st)) txt
      assert failed (selectionAnchor st == getCursor (buffer st))

runTextAreaCursorOnScrollBarTest :: Context -> IORef Int -> IO ()
runTextAreaCursorOnScrollBarTest ctx failed = do
  let
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" fortyLines)
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  mRect <- getPrevRect ctx (respId resp)
  case (spanTop (== "Notes") spans, mRect) of
    ([labelPos], Just field) -> do
      let fm = ctxFontMetrics ctx
      assertEq failed UiCursorDefault =<< cursorOver ctx inp0 ui labelPos
      assertEq failed UiCursorText =<< cursorOver ctx inp0 ui (V2 (rectX field + 20) (rectY field + 20))
      assertJust failed (fst (textAreaBarLayouts field (textAreaBars fm field 0 (40 * textAreaLineHeight fm)) 0 0)) $
        checkThumbGrab failed ctx inp0 ui
    _ -> assert failed False

-- | A scrollbar thumb shows the grab cursor on hover and grabbing when
-- pressed.
checkThumbGrab :: IORef Int -> Context -> Input -> NanoUI a -> ScrollBarLayout -> IO ()
checkThumbGrab failed ctx inp ui layout = do
  let thumb = spanCenter (sbThumb layout)
  assertEq failed UiCursorGrab =<< cursorOver ctx inp ui thumb
  _ <- runFrame ctx (pressAt inp thumb) ui
  assert failed =<< cursorKindIs ctx (pressAt inp thumb) UiCursorGrabbing

runTextAreaHScrollWheelTest :: Context -> IORef Int -> IO ()
runTextAreaHScrollWheelTest ctx failed = do
  let
    longLine = T.replicate 15 "0123456789"
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" longLine)
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \field -> do
    let
      fm = ctxFontMetrics ctx
      wheel dx = do
        _ <- runFrame ctx inp0 {inputMousePos = spanCenter field, inputScroll = V2 dx 0} ui
        getScrollOffset2D ctx (respId resp)
    assertEq failed (V2 0 0) =<< getScrollOffset2D ctx (respId resp)
    V2 offX1 offY1 <- wheel 1
    assertGt failed offX1 0
    assertEq failed offY1 0
    V2 offX2 _ <- wheel 3
    assertGt failed offX2 offX1
    -- A click in the scrolled text lands on the column under the pointer,
    -- counting the horizontal offset.
    let
      (ix, iy) = widgetContentInset fm
      clickX = 30
    _ <- runClick ctx inp0 ui (V2 (rectX field + ix + clickX) (rectY field + iy + 5))
    store <- getStore ctx
    let Cursor _ col = getCursor (buffer (loadTextAreaState store (intKey (respId resp))))
    assert failed (abs (fromIntegral col - (offX2 + clickX) / fmAdvance fm '0') <= 1)
    assertEq failed 0 . v2X =<< wheel (-10)
    assertJust failed (snd (textAreaBarLayouts field (textAreaBars fm field (lineWidth fm longLine) 0) 0 0)) $
      checkThumbGrab failed ctx inp0 ui

runTextArea2DScrollTest :: Context -> IORef Int -> IO ()
runTextArea2DScrollTest ctx failed = do
  let
    lines2D =
      [ T.pack (show (i :: Int)) <> " - " <> T.replicate 10 "abcdefghij"
      | i <- [1 .. 40]
      ]
    text2D = T.unlines lines2D
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" text2D)
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \field -> do
    let
      fm = ctxFontMetrics ctx
      contentH = 40 * textAreaLineHeight fm
      contentW = maximum (0 : [lineWidth fm l | l <- lines2D])
      (mV, mH) = textAreaBarLayouts field (textAreaBars fm field contentW contentH) 0 0
    assertJust failed ((,) <$> mV <*> mH) $ \(vLayout, hLayout) -> do
      let
        vTrack = sbTrack vLayout
        hTrack = sbTrack hLayout
      assert failed (rectY vTrack + rectH vTrack <= rectY field + rectH field - textAreaBarLane + 1)
      assert failed (rectX hTrack + rectW hTrack <= rectX field + rectW field - textAreaBarLane + 1)
      _ <- runFrame ctx inp0 {inputMousePos = spanCenter field, inputScroll = V2 2 3} ui
      V2 offX offY <- getScrollOffset2D ctx (respId resp)
      assertGt failed offX 0
      assertGt failed offY 0

-- | Typing past a text area's right edge scrolls it across, keeping the caret
-- at the end of its widest line in view, and Home scrolls back.
runTextAreaCaretFollowTest :: Context -> IORef Int -> IO ()
runTextAreaCaretFollowTest ctx failed = do
  ref <- newVar ""
  let
    inp0 = withInput 320 220
    ui = column (fst <$> held ref (textAreaWith' (fixedW 200)))
  resp <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  typed <- forM [1 .. 60 :: Int] $ \_ -> runFrame ctx inp0 {inputChars = "x"} ui >> caretInView ctx (respId resp)
  assert failed (and typed)
  assertEq failed 60 . T.length =<< readVarIO ref
  V2 offX _ <- getScrollOffset2D ctx (respId resp)
  assertGt failed offX 0
  _ <- runFrame ctx (keyInp KeyHome inp0) ui
  assertEq failed 0 . v2X =<< getScrollOffset2D ctx (respId resp)

-- | With the horizontal bar showing, the caret moved to the last row sits
-- above the bar's lane, not under it.
runTextAreaCaretAboveBarTest :: Context -> IORef Int -> IO ()
runTextAreaCaretAboveBarTest ctx failed = do
  let
    inp0 = withInput 320 220
    ui = column (fst <$> withValue (T.replicate 30 "wide " <> "\n" <> fortyLines) textArea')
  resp <- warmup2 ctx inp0 ui
  _ <- runFrame ctx inp0 (runTextCommand (respId resp) SelectAll)
  _ <- runFrame ctx inp0 ui
  assert failed =<< caretInView ctx (respId resp)

-- | Keys keep the caret in view by the rows as painted: on the device pixel
-- grid, 12.8 px apart for a 13 px font at 125%, not 13.
runTextAreaSnappedRowsTest :: Context -> IORef Int -> IO ()
runTextAreaSnappedRowsTest base failed = do
  ref <- newVar (T.replicate 59 "\n")
  let
    fm = (monospaceMetrics 13) {fmSnapScale = 1.25}
    ctx = withFontResolver base (\_ _ _ _ -> pure (fm, False)) (\_ _ _ _ txt -> pure (13 * fromIntegral (T.length txt), 13))
    inp0 = withInput 320 220
    ui = column (fst <$> held ref (textAreaWith' (fontSize 13)))
  resp <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  inView <- forM (replicate 59 KeyDown ++ replicate 20 KeyUp) $ \k -> runFrame ctx (keyInp k inp0) ui >> caretInView ctx (respId resp)
  assert failed (and inView)

-- | Whether text area @wid@'s caret, at its stored scroll, lies inside the
-- clip its rows are painted in: its content box, short of the lane of each
-- scrollbar that shows.
caretInView :: Context -> WidgetId -> IO Bool
caretInView ctx wid = do
  st <- (`loadTextAreaState` intKey wid) <$> getStore ctx
  V2 sx sy <- getScrollOffset2D ctx wid
  textAreaHitForWidget ctx wid >>= \case
    Nothing -> pure False
    Just (TextAreaHit idx field lineH) -> do
      fm <- resolveTextAreaFont ctx idx
      (contentW, contentH) <- textAreaContentMetrics ctx idx
      let
        bars = textAreaBars fm field contentW contentH
        (ix, iy) = widgetContentInset fm
        lane shown = if shown then textAreaBarLane else 0
        viewW = rectW field - 2 * ix - lane (tabVertical bars)
        viewH = rectH field - 2 * iy - lane (tabHorizontal bars)
        Cursor caretRow caretCol = getCursor (buffer st)
        left = lineWidth fm (T.take caretCol (lineAt caretRow (buffer st))) - sx
        top = fromIntegral caretRow * lineH - sy
      pure (left >= -0.5 && left + 1 <= viewW + 0.5 && top >= -0.5 && top + lineH <= viewH + 0.5)

runTextAreaScrollCursorLeavesViewportTest :: Context -> IORef Int -> IO ()
runTextAreaScrollCursorLeavesViewportTest ctx failed = do
  let
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" fortyLines)
  (resp, _) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \field -> do
    let
      caret = getCursor . buffer . (`loadTextAreaState` intKey (respId resp)) <$> getStore ctx
      offset = getScrollOffset ctx (respId resp)
    assertEq failed (Cursor 0 0) =<< caret
    assertEq failed 0 =<< offset
    -- The wheel scrolls a focused area without moving the caret, and an idle
    -- frame does not snap back.
    _ <- runFrame ctx inp0 {inputMousePos = spanCenter field, inputScroll = V2 0 5} ui
    off1 <- offset
    assertGt failed off1 0
    _ <- runFrame ctx inp0 ui
    assertEq failed off1 =<< offset
    assertEq failed (Cursor 0 0) =<< caret
    -- Typing brings the caret, now at (0, 1), back into view.
    _ <- runFrame ctx inp0 {inputChars = "!"} ui
    assertEq failed 0 =<< offset
    assertEq failed (Cursor 0 1) =<< caret

-- | A backend redraw request needs a frame even when user input is unchanged.
runRefreshRedrawTest :: Context -> IORef Int -> IO ()
runRefreshRedrawTest ctx failed = do
  let
    idle = emptyInput {inputWindowSize = Size 320 200}
    refreshed = idle {inputWindowRedraw = True}
  assert failed =<< needsRedraw ctx idle refreshed

-- | A context-menu Cut/Paste edits the document without any keys or chars on
-- the frame, so the change must surface as a 'respChanged' pulse through the
-- text-area store flag, or callers (the notepad's dirty tracking) never learn
-- the document changed. Covers the hadInput guard in 'textAreaWith'. The
-- caller holds the text, so the cut must also survive the
-- release frame, where the caller still passes back the pre-cut result.
runTextAreaMenuPulseTest :: Context -> IORef Int -> IO ()
runTextAreaMenuPulseTest ctx failed = do
  textRef <- newVar "abc"
  let
    inp0 = withInput 320 220
    ui = column (held textRef (textAreaWith' grow))
  (resp0, initial) <- warmup2 ctx inp0 ui
  assertEq failed initial "abc"
  assertJustM failed (textAreaHitForWidget ctx (respId resp0)) $ \hit -> do
    let
      mid = spanCenter (tahFieldRect hit)
      menuOpen = fst (rightClickPair inp0 mid)
    -- Focus the editor, as a menu pick would.
    _ <- runClick ctx inp0 ui mid
    -- Selection-only actions do not pulse: no text delta.
    _ <- runFrame ctx inp0 (runTextCommand (respId resp0) SelectAll)
    (respSel, valSel) <- evalUi ctx inp0 ui
    assert failed (not (respChanged respSel))
    assertEq failed valSel "abc"
    _ <- runFrame ctx menuOpen ui
    overlays <- collectOverlayTextSpans ctx menuOpen
    assertJust failed (spanRectOf "Cut" overlays) $ \r -> do
      -- Cut runs on the press frame; the release frame reports the change
      -- and the emptied text, and the next frame reports no change.
      (resp, val) <- runClick ctx inp0 ui (spanCenter r)
      assert failed (respChanged resp)
      assertEq failed val ""
      (respIdle, valIdle) <- evalUi ctx inp0 ui
      assert failed (not (respChanged respIdle))
      assertEq failed valIdle ""

-- | A command run from a button elsewhere (an app's Edit menu) focuses the
-- field it edits, so Select All followed by typing replaces the text.
runTextCommandFocusTest :: Context -> IORef Int -> IO ()
runTextCommandFocusTest ctx failed = do
  ref <- newVar "abc"
  let
    inp = withInput 320 220
    ui = column $ do
      (area, _) <- held ref (textAreaWith' (fixedH 80))
      selectAll <- button' "Select All"
      when (respClicked selectAll) (runTextCommand (respId area) SelectAll)
      pure selectAll
  selectAll <- warmup2 ctx inp ui
  _ <- runClick ctx inp ui (centerOf selectAll)
  mapM_ (\i -> runFrame ctx i ui) [inp, inp {inputChars = "Z"}, inp]
  assertEq failed "Z" =<< readVarIO ref

-- | An editor mounted under a fresh key scrolls on wheel hover without needing focus.
runTextAreaRemountScrollTest :: Context -> IORef Int -> IO ()
runTextAreaRemountScrollTest ctx failed = do
  let
    inp0 = withInput 320 220
    mkUi k = column $ withKey k $ withValue fortyLines (textAreaWith' grow)
  _ <- warmup2 ctx inp0 (mkUi (1 :: Int))
  (resp, _) <- warmup2 ctx inp0 (mkUi (2 :: Int))
  assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
    -- Park the pointer over the editor with 1 s frames so hover animations
    -- settle; otherwise a hot-widget change on the wheel frame would damage
    -- the area and mask a missing scroll repaint.
    let hover = inp0 {inputMousePos = spanCenter (tahFieldRect hit), inputDeltaTime = 1}
    replicateM_ 4 (runFrame ctx hover (mkUi (2 :: Int)))
    dmgIdle <- takeDamage ctx
    assert failed (damageIsEmpty dmgIdle)
    off0 <- getScrollOffset ctx (respId resp)
    _ <- runFrame ctx hover {inputScroll = V2 0 1} (mkUi (2 :: Int))
    off1 <- getScrollOffset ctx (respId resp)
    assertGt failed off1 off0
    -- The scroll offset must also damage the editor, or nothing repaints.
    dmg <- takeDamage ctx
    assert failed (not (damageIsEmpty dmg))

-- | Ctrl+Z and Ctrl+Shift+Z undo and redo typing in a field; commands run from
-- outside the frame edit it and pulse 'respChanged'; replacing the value the
-- field is passed clears its history.
runTextUndoTest :: Context -> IORef Int -> IO ()
runTextUndoTest ctx failed = do
  ref <- newVar ""
  commands <- newIORef []
  let
    inp = withInput 320 120
    -- Commands queued by the test run after the field, as an app's menu
    -- would; the field's undo state is read the same way.
    ui = column $ do
      (resp, _) <- held ref textInput'
      pending <- liftIO (readIORef commands)
      liftIO (writeIORef commands [])
      mapM_ (runTextCommand (respId resp)) pending
      (,) resp <$> textCanUndo (respId resp)
    frame i = evalUi ctx i ui
  _ <- warmup2 ctx inp ui
  _ <- frame (tabInp inp)
  mapM_ (\c -> frame inp {inputChars = T.singleton c}) ("red fox" :: String)
  assertEq failed "red fox" =<< readVarIO ref
  (_, canUndo) <- frame inp
  assert failed canUndo
  _ <- frame (chordInp (ctrl <> key 'z') inp)
  assertEq failed "red " =<< readVarIO ref
  _ <- frame (chordInp (ctrl <> key 'z') inp)
  assertEq failed "" =<< readVarIO ref
  _ <- frame (chordInp (ctrl <> shift <> key 'z') inp)
  assertEq failed "red " =<< readVarIO ref
  _ <- frame (chordInp (ctrl <> key 'y') inp)
  assertEq failed "red fox" =<< readVarIO ref
  -- A command from outside the field's frame edits it and pulses it once.
  writeIORef commands [InsertText "!"]
  _ <- frame inp
  (pulsed, _) <- frame inp
  assert failed (respChanged pulsed)
  assertEq failed "red fox!" =<< readVarIO ref
  (quiet, _) <- frame inp
  assert failed (not (respChanged quiet))
  writeIORef commands [Undo]
  _ <- frame inp
  _ <- frame inp
  assertEq failed "red fox" =<< readVarIO ref
  -- The caller replacing the value drops the history recorded against the
  -- old text.
  writeVarIO ref "something else"
  _ <- frame inp
  (_, stillUndoable) <- frame inp
  assert failed (not stillUndoable)

-- | A text area over a 'TextDocument' returns the input document unchanged
-- while nothing edits it (including on cursor movement), returns a new one
-- on edits, and supports undo and external replacement like the 'Text' one.
runTextAreaDocumentTest :: Context -> IORef Int -> IO ()
runTextAreaDocumentTest ctx failed = do
  let original = T.intercalate "\n" ["line " <> T.pack (show i) | i <- [0 .. 999 :: Int]]
  ref <- newVar (textDocument original)
  let
    inp = withInput 320 220
    ui = column $ do
      (resp, doc) <- held ref textAreaDocument'
      (,) (resp, doc) <$> textCanUndo (respId resp)
    frame i = do
      before <- readVarIO ref
      ((resp, after), undoable) <- evalUi ctx i ui
      pure (before, resp, after, undoable)
  warmupFocused ctx inp ui
  (idleIn, idleResp, idleOut, _) <- frame inp
  assert failed (sameDocument idleIn idleOut)
  assert failed (not (respChanged idleResp))
  (moveIn, moveResp, moveOut, _) <- frame (keyInp KeyDown inp)
  -- A caret move is no edit: the document is the one passed in.
  assert failed (not (respChanged moveResp))
  assert failed (sameDocument moveIn moveOut)
  (typedIn, typedResp, typedOut, undoable) <- frame inp {inputChars = "x"}
  assert failed (respChanged typedResp)
  assert failed (not (sameDocument typedIn typedOut))
  assertEq failed (documentLine 1 typedOut) "xline 1"
  assertEq failed (documentLineCount typedOut) 1000
  assert failed undoable
  (againIn, _, againOut, _) <- frame inp
  assert failed (sameDocument againIn againOut)
  _ <- frame (chordInp (ctrl <> key 'z') inp)
  undone <- readVarIO ref
  assertEq failed (documentText undone) original
  -- Replacing the document drops the history recorded against the old one.
  writeVarIO ref (textDocument "fresh")
  _ <- frame inp
  (_, _, fresh, stillUndoable) <- frame inp
  assertEq failed fresh (textDocument "fresh")
  assert failed (not stillUndoable)
  -- A caller that builds an equal document every frame lets the loop sleep.
  copyCtx <- newContext
  source <- newIORef original
  let
    -- Split inside the frame, so each frame passes a new copy.
    copies = column (textAreaDocument . constVar =<< liftIO (textDocument <$> readIORef source))
    idle = inp {inputDeltaTime = 1}
  _ <- warmup2 copyCtx inp copies
  _ <- warmup2 copyCtx idle copies
  assert failed . not =<< needsRedraw copyCtx idle idle

-- | The content width a text area keeps up to date line by line matches a
-- fresh measurement after edits that widen, move and shorten its widest line.
runTextAreaWidthTrackingTest :: Context -> IORef Int -> IO ()
runTextAreaWidthTrackingTest ctx failed = do
  ref <- newVar (T.intercalate "\n" (replicate 200 "short line" ++ ["the widest line of them all"] ++ replicate 200 "short line"))
  let
    inp = withInput 400 300
    ui = column (held ref (textAreaWith' grow))
    frame i = evalUi ctx i ui
    check = do
      (resp, _) <- frame inp
      assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
        (tracked, _) <- textAreaContentMetrics ctx (tahNodeIdx hit)
        text <- readVarIO ref
        fm <- resolveTextAreaFont ctx (tahNodeIdx hit)
        widths <- mapM (lineWidthIO fm) (T.splitOn "\n" text)
        assertEq failed (maximum widths) tracked
  _ <- warmup2 ctx inp ui
  _ <- frame (tabInp inp)
  check
  -- Widen a short line past the widest.
  mapM_ (\_ -> frame (keyInp KeyDown inp)) [1 .. 10 :: Int]
  mapM_ (\c -> frame inp {inputChars = T.singleton c}) (replicate 40 'x')
  check
  -- Shorten it again, so the old widest line wins.
  mapM_ (\_ -> frame (keyInp KeyBackspace inp)) [1 .. 40 :: Int]
  check
  -- Delete the widest line itself.
  mapM_ (\_ -> frame (keyInp KeyDown inp)) [1 .. 190 :: Int]
  _ <- frame inp {inputKeys = inputKeysFromList [KeyHome, KeyEnd], inputModifiers = Modifiers True False False False}
  _ <- frame (keyInp KeyBackspace inp)
  check
  -- Undo brings it back.
  _ <- frame (chordInp (ctrl <> key 'z') inp)
  check

-- | The context with a clipboard kept in memory, starting with @initial@, and
-- a reference to its contents.
memoryClipboard :: Maybe T.Text -> Context -> IO (Context, IORef (Maybe T.Text))
memoryClipboard initial ctx = do
  clipRef <- newIORef initial
  pure (withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True), clipRef)

-- | Select All from the context menu keeps the whole document selected once
-- the button comes up. The menu gesture must not move the underlying caret
-- or start a text-selection drag.
runTextAreaMenuSelectAllTest :: Context -> IORef Int -> IO ()
runTextAreaMenuSelectAllTest ctx failed = do
  let
    lastRow = 200
    lastLine = "the last line"
    original = T.intercalate "\n" (["line " <> T.pack (show i) | i <- [0 .. lastRow - 1]] ++ [lastLine])
    inp0 = withInput 400 300
    ui = column (withValue original (textAreaWith' grow))
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
    let
      field = tahFieldRect hit
      mid = V2 (rectX field + rectW field / 3) (rectY field + rectH field / 3)
      menuOpen = fst (rightClickPair inp0 mid)
    _ <- runClick ctx inp0 ui mid
    _ <- runFrame ctx menuOpen ui
    overlays <- collectOverlayTextSpans ctx menuOpen
    assertJust failed (findExact "Select All" overlays) $ \pos -> do
      _ <- runClick ctx inp0 ui pos
      store <- getStore ctx
      let st = loadTextAreaState store (intKey (respId resp))
      assertEq failed (selectionAnchor st) (Cursor 0 0)
      assertEq failed (getCursor (buffer st)) (Cursor lastRow (T.length lastLine))
