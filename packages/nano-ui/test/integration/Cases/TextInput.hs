module Cases.TextInput
  ( runTextInputClickSelectTest
  , runTextInputClipboardTest
  , runTextInputPasswordTest
  , runTextInputCursorTest
  , runTextAreaCutClearsSelectionTest
  , runTextInputCutClearsSelectionTest
  , runTextInputDirtyTest
  , runTextInputDragWakeTest
  , runTextInputFocusSdlTest
  , runTextInputMenuTest
  , runTextInputMouseSelectionTest
  , runTextInputSelectionTest
  , runTextInputFfCaretTest
  , runTextInputScrollTest
  , runTextInputWordKeysTest
  , runTextInputBatchTest
  , runTextUndoTest
  , runTextAreaWidthTrackingTest
  , runTextAreaDocumentTest
  , runTextAreaScrollWheelTest
  , runTextAreaZoomScrollTest
  , runTextAreaScrollDragTest
  , runTextAreaCursorOnScrollBarTest
  , runTextAreaHScrollWheelTest
  , runTextAreaHScrollDragTest
  , runTextArea2DScrollTest
  , runTextAreaScrollCursorLeavesViewportTest
  , runRefreshRedrawTest
  , runTextAreaMenuPulseTest
  , runTextAreaMenuSelectAllTest
  , runTextCommandFocusTest
  , runTextAreaRemountScrollTest
  )
where

import Control.Monad (forM_, replicateM, replicateM_, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import NanoUI
import NanoUI.Context (intKey)
import NanoUI.Frame.TextEdit
  ( TextAreaHit (..)
  , TextAreaScrollBarLayouts (..)
  , resolveTextAreaFont
  , textAreaContentMetrics
  , textAreaBarLane
  , textAreaLineHeight
  , textAreaHScrollBarLayout
  , textAreaHitForWidget
  , textAreaScrollBarLayout
  , textAreaScrollBarLayouts
  )
import NanoUI.Store
  ( Slot (..)
  , WidgetStore (..)
  , slotKey
  )
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, assertJust, assertJustM, withInput)
import NanoUI.Testing.Harness
  ( assertSpansHas
  , centerOf
  , clickPair
  , findExact
  , held
  , holdAt
  , keyInp
  , pressAt
  , releaseAt
  , rightClickPair
  , runClick
  , spanCenter
  , spanRectOf
  , tabInp
  , warmup2
  , warmupFocused
  )
import NanoUI.Widgets.TextArea
  ( buffer
  , loadTextAreaState
  , selectionAnchor
  )
import NanoUI.Widgets.TextBuffer
  ( fromText
  , getCursor
  , toLines
  , toText
  )

runTextInputBatchTest :: Context -> IORef Int -> IO ()
runTextInputBatchTest ctx failed = do
  let
    inp = withInput 320 120
    ui = column (textInput' "aOLDz")
    left = keyInp KeyLeft inp
    step event = runFrame ctx event ui
  (resp, _) <- warmup2 ctx inp ui
  _ <- step (tabInp inp)
  _ <- step left
  replicateM_ 3 (step (left {inputModifiers = Modifiers True False False}))
  let
    checkSelection cursor anchor = do
      store <- getStore ctx
      let
        key = intKey (respId resp)
      assertEq
        failed
        (IM.lookup (slotKey SlotCursor key) (storeInt store))
        (Just cursor)
      assertEq
        failed
        (IM.lookup (slotKey SlotAnchor key) (storeInt store))
        (Just anchor)
  checkSelection 1 4
  -- An event filtered to nothing must not delete the current selection.
  ((_, unchanged), _, _, _) <- step (inp {inputChars = "\n\t"})
  assertEq failed unchanged "aOLDz"
  checkSelection 1 4
  -- Navigation follows text insertion within a frame.
  ((_, committed), _, _, _) <- step (left {inputChars = "é\n世界\t"})
  assertEq failed committed "aé世界z"
  checkSelection 3 3

-- | Caption-less text area with a separate label above it (the old labelled
-- field kept the label span and geometry the tests assert against).
labeledArea :: T.Text -> T.Text -> NanoUI (Response, T.Text)
labeledArea lbl initial = do
  label lbl
  textArea' initial

labeledInput :: Ui :> es => T.Text -> T.Text -> Eff es (Response, T.Text)
labeledInput lbl initial = do
  label lbl
  textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Enter " <> lbl} initial

runTextInputCursorTest :: Context -> IORef Int -> IO ()
runTextInputCursorTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column (labeledInput "Name" "")
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let
    labelPos =
      [ (rectX r + rectW r / 2, rectY r + 0.5)
      | (r, txt, _, _, _) <- spans
      , txt == "Name"
      ]
    fieldPos =
      [ (rectX r + rectW r / 2, rectY r + 0.5)
      | (r, txt, _, _, _) <- spans
      , "Enter" `T.isInfixOf` txt
      ]
  case (labelPos, fieldPos) of
    ([(lx, ly)], [(fx, fy)]) -> do
      let
        labelHover = inp0 {inputMousePos = V2 lx ly}
      _ <- runFrame ctx labelHover ui
      labelKind <- uiCursorKind ctx labelHover
      assertEq failed labelKind UiCursorDefault
      let
        fieldHover = inp0 {inputMousePos = V2 fx fy}
      _ <- runFrame ctx fieldHover ui
      fieldKind <- uiCursorKind ctx fieldHover
      assertEq failed fieldKind UiCursorText
      let
        click =
          fieldHover
            { inputMouseDown = True
            , inputMousePressed = True
            , inputMouseReleased = False
            }
      _ <- runFrame ctx click ui
      clickKind <- uiCursorKind ctx click
      assertEq failed clickKind UiCursorText
    _ -> assert failed False

runTextInputCutClearsSelectionTest :: Context -> IORef Int -> IO ()
runTextInputCutClearsSelectionTest ctx failed = do
  textRef <- newIORef "hello"
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  let
    inp0 = withInput 320 120
    ui = column (held textRef textInput')
  warmupFocused ctx' inp0 ui
  let
    shiftLeft =
      inp0
        { inputKeys = inputKeysFromList [KeyLeft]
        , inputModifiers = Modifiers True False False
        }
  _ <- runFrame ctx' shiftLeft ui
  _ <-
    runFrame
      ctx'
      (inp0 {inputChars = "x", inputModifiers = Modifiers False True False})
      ui
  clip <- readIORef clipRef
  assertEq failed clip (Just "o")
  ((_, val), _, _, _) <- runFrame ctx' (inp0 {inputChars = "z"}) ui
  assertEq failed val "hellz"

-- Word-wise editing keys (Ctrl or Alt + Backspace/Delete/Left/Right) work in
-- the single-line text input like they do in the text area.
runTextInputWordKeysTest :: Context -> IORef Int -> IO ()
runTextInputWordKeysTest ctx failed = do
  textRef <- newIORef "hello world"
  let
    inp0 = withInput 320 120
    ui = column (held textRef textInput')
    ctrlMods = Modifiers False True False
  warmupFocused ctx inp0 ui
  -- Ctrl+Backspace deletes the word before the cursor ("world").
  _ <-
    runFrame
      ctx
      (inp0 {inputKeys = inputKeysFromList [KeyBackspace], inputModifiers = ctrlMods})
      ui
  ((_, v1), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed v1 "hello "
  -- Nothing right of the cursor at end of text: Ctrl+Delete is a no-op.
  _ <-
    runFrame
      ctx
      (inp0 {inputKeys = inputKeysFromList [KeyDelete], inputModifiers = ctrlMods})
      ui
  ((_, v2), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed v2 "hello "
  -- Ctrl+Left jumps to the start; typing there proves the cursor moved.
  _ <-
    runFrame
      ctx
      (inp0 {inputKeys = inputKeysFromList [KeyLeft], inputModifiers = ctrlMods})
      ui
  ((_, v3), _, _, _) <- runFrame ctx (inp0 {inputChars = "X"}) ui
  assertEq failed v3 "Xhello "
  -- Ctrl+Delete removes the word after the cursor ("hello").
  _ <-
    runFrame
      ctx
      (inp0 {inputKeys = inputKeysFromList [KeyDelete], inputModifiers = ctrlMods})
      ui
  ((_, v4), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed v4 "X "

runTextAreaCutClearsSelectionTest :: Context -> IORef Int -> IO ()
runTextAreaCutClearsSelectionTest ctx failed = do
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  textRef <- newIORef "hello"
  let
    inp0 = withInput 320 220
    ui = column (label "Notes" >> held textRef textArea')
  warmupFocused ctx' inp0 ui
  _ <-
    runFrame
      ctx'
      (inp0 {inputChars = "\x01", inputModifiers = Modifiers False True False})
      ui
  ((_, cutVal), _, _, _) <-
    runFrame
      ctx'
      (inp0 {inputChars = "x", inputModifiers = Modifiers False True False})
      ui
  clip <- readIORef clipRef
  assertEq failed clip (Just "hello")
  assertEq failed cutVal ""
  ((_, val), _, _, _) <- runFrame ctx' (inp0 {inputChars = "z"}) ui
  assertEq failed val "z"

runTextInputSelectionTest :: Context -> IORef Int -> IO ()
runTextInputSelectionTest ctx failed = do
  textRef <- newIORef "hello"
  let
    inp0 = withInput 320 120
    ui = column (button "Other" >> held textRef textInput')
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  let
    shiftLeft =
      inp0
        { inputKeys = inputKeysFromList [KeyLeft]
        , inputModifiers = Modifiers True False False
        }
  _ <- warmup2 ctx shiftLeft ui
  ((_, valReplace), _, _, _) <- runFrame ctx (inp0 {inputChars = "X"}) ui
  assertEq failed valReplace "helX"
  -- Ctrl+A selects all whether it arrives as 'a' or as the \x01 control char.
  forM_ ["\x01", "a"] $ \selectAll -> do
    _ <- runFrame ctx (inp0 {inputChars = "abc"}) ui
    _ <-
      runFrame
        ctx
        (inp0 {inputChars = selectAll, inputModifiers = Modifiers False True False})
        ui
    ((_, valClear), _, _, _) <-
      runFrame ctx (keyInp KeyBackspace inp0) ui
    assertEq failed valClear ""

runTextInputMouseSelectionTest :: Context -> IORef Int -> IO ()
runTextInputMouseSelectionTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column (textInput' "hello")
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "hello" spans) $ \(Rect fx fy fw fh) -> do
    let
      fieldY = fy + fh / 2
      dragged = holdAt inp0 (V2 (fx + fw - 1) fieldY)
    _ <- runFrame ctx (pressAt inp0 (V2 (fx + 1) fieldY)) ui
    _ <- runFrame ctx dragged ui
    _ <- runFrame ctx (releaseAt dragged) ui
    ((_, val), _, _, _) <- runFrame ctx (inp0 {inputChars = "z"}) ui
    assertEq failed val "z"

-- Double-click selects a word and triple-click the whole line.
runTextInputClickSelectTest :: Context -> IORef Int -> IO ()
runTextInputClickSelectTest ctx failed = do
  allCtx <- newContext
  let
    inp0 = withInput 320 120
    -- Click n times at the start of the field showing txt, then Backspace.
    clicksThenBackspace c txt n = do
      let
        ui = column (textInput' txt)
      _ <- warmup2 c inp0 ui
      spans <- collectTextSpans c
      case [r | (r, t, _, _, _) <- spans, t == txt] of
        (Rect fx fy _ fh : _) -> do
          let
            pos = V2 (fx + 1) (fy + fh / 2)
          forM_ [1 .. n] $ \k ->
            runFrame
              c
              ( inp0
                  { inputMousePos = pos
                  , inputMouseDown = True
                  , inputMousePressed = True
                  , inputMouseClicks = k
                  }
              )
              ui
          ((_, val), _, _, _) <-
            runFrame c (keyInp KeyBackspace inp0) ui
          pure (Just val)
        _ -> pure Nothing
  word <- clicksThenBackspace ctx "hello world" 2
  assertEq failed word (Just " world")
  line <- clicksThenBackspace allCtx "hello" 3
  assertEq failed line (Just "")

runTextInputClipboardTest :: Context -> IORef Int -> IO ()
runTextInputClipboardTest ctx failed = do
  textRef <- newIORef "hello"
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  let
    inp0 = withInput 320 120
    ui = column (held textRef textInput')
  warmupFocused ctx' inp0 ui
  let
    selectAll = inp0 {inputChars = "a", inputModifiers = Modifiers False True False}
    copy = inp0 {inputChars = "c", inputModifiers = Modifiers False True False}
    clear = keyInp KeyBackspace inp0
    paste = inp0 {inputChars = "v", inputModifiers = Modifiers False True False}
  _ <- runFrame ctx' selectAll ui
  _ <- runFrame ctx' copy ui
  clip <- readIORef clipRef
  assertEq failed clip (Just "hello")
  _ <- runFrame ctx' selectAll ui >> runFrame ctx' clear ui
  ((_, val), _, _, _) <- runFrame ctx' paste ui
  assertEq failed val "hello"

-- A password field displays one mask character per character, and Ctrl+C
-- leaves the clipboard untouched while the field keeps its real value.
runTextInputPasswordTest :: Context -> IORef Int -> IO ()
runTextInputPasswordTest ctx failed = do
  (ctx', clipRef) <- memoryClipboard Nothing ctx
  let
    inp0 = withInput 320 120
    ui = column (textInputConfigured' defaultTextInputConfig {ticPassword = True} "hunter2")
  _ <- warmup2 ctx' inp0 ui
  spans <- collectTextSpans ctx'
  assert failed (not (any (\(_, txt, _, _, _) -> "hunter2" `T.isInfixOf` txt) spans))
  assertSpansHas failed "*******" spans
  _ <- runFrame ctx' (tabInp inp0) ui
  let
    selectAll = inp0 {inputChars = "a", inputModifiers = Modifiers False True False}
    copy = inp0 {inputChars = "c", inputModifiers = Modifiers False True False}
  _ <- runFrame ctx' selectAll ui
  ((_, val), _, _, _) <- runFrame ctx' copy ui
  clip <- readIORef clipRef
  assertEq failed clip Nothing
  assertEq failed val "hunter2"

-- The text input's context menu offers Paste even while unfocused, and its
-- Paste and Cut entries edit the field through the clipboard. The menu edits
-- the field between frames, so each edit must survive the caller passing back
-- the result of the frame before it.
runTextInputMenuTest :: Context -> IORef Int -> IO ()
runTextInputMenuTest ctx failed = do
  textRef <- newIORef "hello"
  (ctx', clipRef) <- memoryClipboard (Just "pasted") ctx
  let
    inp0 = withInput 320 160
    ui = column (held textRef textInput')
  _ <- warmup2 ctx' inp0 ui
  spans <- collectTextSpans ctx'
  assertJust failed (spanRectOf "hello" spans) $ \(Rect fx fy _ fh) -> do
    let
      menuOpen =
        inp0
          { inputMousePos = V2 (fx + 1) (fy + fh / 2)
          , inputMouseRightDown = True
          , inputMouseRightPressed = True
          }
      pick entry = do
        _ <- runFrame ctx' menuOpen ui
        overlays <- collectOverlayTextSpans ctx' menuOpen
        case [r | (r, txt, _, _, _) <- overlays, txt == entry] of
          (r : _) -> do
            let
              (pickPress, pickRelease) = clickPair inp0 (spanCenter r)
            _ <- runFrame ctx' pickPress ui >> runFrame ctx' pickRelease ui
            ((_, val), _, _, _) <- runFrame ctx' inp0 ui
            pure (Just val)
          _ -> pure Nothing
    pasted <- pick "Paste"
    assertEq failed pasted (Just "hellopasted")
    cut <- pick "Cut"
    clip <- readIORef clipRef
    assertEq failed clip (Just "hellopasted")
    assertEq failed cut (Just "")
    overlaysAfter <- collectOverlayTextSpans ctx' inp0
    assert failed (not (any (\(_, txt, _, _, _) -> txt == "Cut") overlaysAfter))

runTextInputFocusSdlTest :: Context -> IORef Int -> IO ()
runTextInputFocusSdlTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column (labeledInput "Name" "")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case [ (rectX r + rectW r / 2, rectY r + 0.5)
       | (r, txt, _, _, _) <- spans
       , "Enter" `T.isInfixOf` txt
       ] of
    [(fx, fy)] -> do
      let
        inp1 =
          pressAt inp0 (V2 fx fy)
      _ <- runFrame ctx inp1 ui
      focus <- getFocusId ctx
      assertEq failed focus (respId resp)
      let
        idle = inp0 {inputMousePos = V2 fx fy}
      samples <- replicateM 5 (runFrame ctx idle ui >> getFocusId ctx)
      assertEq failed samples (replicate 5 (respId resp))
    _ -> assert failed False

runTextInputDirtyTest :: Context -> IORef Int -> IO ()
runTextInputDirtyTest ctx failed = do
  let
    ui = column (textInput' "")
    inp0 = (withInput 200 100) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry _ _ = respRect resp
    (press, release) = clickPair inp0 (V2 (rx + 1) (ry + 0.5))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  let
    idle = release {inputMouseReleased = False, inputDeltaTime = 1}
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
    ui = column (textInput' "some text to select")
    inp0 = (withInput 200 100) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry rw _ = respRect resp
    outside = V2 (rx + rw + 40) (ry + 4)
    (press, release) = clickPair inp0 (V2 (rx + 4) (ry + 4))
    hold = press {inputMousePressed = False}
  _ <- runFrame ctx press ui
  _ <- runFrame ctx hold ui
  assertEq failed 0 =<< getWakeAt ctx
  _ <- runFrame ctx hold {inputMousePos = outside} ui
  assert failed . (> 0) =<< getWakeAt ctx
  _ <- runFrame ctx release {inputMousePos = outside} ui
  _ <- runFrame ctx release {inputMousePos = outside, inputMouseReleased = False} ui
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
    ui = column (textInput' fs)
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  assertSpansHas failed fs spans
  assertJust failed (spanRectOf fs spans) $ \(Rect fx fy _ fh) -> do
    let
      pos = V2 (fx + lineWidth fm (T.take 3 fs)) (fy + fh / 2)
      (press, release) = clickPair inp0 pos
    _ <- runFrame ctx press ui
    _ <- runFrame ctx release ui
    ((_, val), _, _, _) <-
      runFrame ctx (inp0 {inputMousePos = pos, inputChars = "x"}) ui
    assertEq failed val "fffxfff"

runTextInputScrollTest :: Context -> IORef Int -> IO ()
runTextInputScrollTest ctx failed = do
  let
    longText = "VeryLongTextEnteredIntoTheFieldThatExceedsTheWidth"
    inp0 = withInput 200 120
    ui = column (textInput' longText)
  (resp, _) <- warmup2 ctx inp0 ui
  spans0 <- collectTextSpans ctx
  assertJust failed (spanRectOf longText spans0) $ \(Rect fx fy _ fh) -> do
    let
      pos = V2 (fx + 50) (fy + fh / 2)
      (press, release) = clickPair inp0 pos
    _ <- runFrame ctx press ui
    _ <- runFrame ctx release ui
    let
      atEnd = keyInp KeyEnd inp0
    _ <- runFrame ctx atEnd ui
    store <- getStore ctx
    let
      key = intKey (respId resp)
      scrollEnd = IM.findWithDefault 0 (slotKey SlotTextInputScroll key) (storeFloat store)
    assert failed (scrollEnd > 0)
    let
      atHome = keyInp KeyHome inp0
    _ <- runFrame ctx atHome ui
    storeHome <- getStore ctx
    let
      scrollHome = IM.findWithDefault 0 (slotKey SlotTextInputScroll key) (storeFloat storeHome)
    assertEq failed scrollHome 0

runTextAreaScrollWheelTest :: Context -> IORef Int -> IO ()
runTextAreaScrollWheelTest ctx failed = do
  let
    longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" longText)
    uiShort = column (keyed (1 :: Int) (labeledArea "Short" "Line 1\nLine 2"))
    fieldCenter = pure . spanCenter
  -- Text that fits the viewport does not wheel-scroll.
  (respShort, _) <- warmup2 ctx inp0 uiShort
  assertJustM failed (getPrevRect ctx (respId respShort)) $ \r -> do
    pos <- fieldCenter r
    _ <- runFrame ctx (inp0 {inputMousePos = pos, inputScroll = V2 0 1}) uiShort
    offShort <- getScrollOffset ctx (respId respShort)
    assertEq failed offShort 0
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \r -> do
    pos <- fieldCenter r
    let
      wheelDown = inp0 {inputMousePos = pos, inputScroll = V2 0 1}
    off0 <- getScrollOffset ctx (respId resp)
    assertEq failed off0 0
    -- Scroll down 1 notch
    _ <- runFrame ctx wheelDown ui
    off1 <- getScrollOffset ctx (respId resp)
    assertGt failed off1 off0
    -- Scroll down 3 more notches
    let
      wheelDownMore = inp0 {inputMousePos = pos, inputScroll = V2 0 3}
    _ <- runFrame ctx wheelDownMore ui
    off2 <- getScrollOffset ctx (respId resp)
    assertGt failed off2 off1
    -- Scroll up beyond top to check clamping to 0
    let
      wheelUp = inp0 {inputMousePos = pos, inputScroll = V2 0 (-10)}
    _ <- runFrame ctx wheelUp ui
    off3 <- getScrollOffset ctx (respId resp)
    assertEq failed off3 0
    -- Text buffer should remain completely unmodified
    store <- getStore ctx
    let
      key = intKey (respId resp)
      st = loadTextAreaState store key
    assertEq failed (toText (buffer st)) longText

runTextAreaZoomScrollTest :: Context -> IORef Int -> IO ()
runTextAreaZoomScrollTest ctx failed = do
  let
    longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
    inp0 = withInput 320 220
    ui = column $ textAreaWith' (fontSize 32) longText
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
    fm <- resolveTextAreaFont ctx (tahNodeIdx hit)
    let
      field = tahFieldRect hit
      lineH = tahLineH hit
      lineCount = max 1 (length (toLines (fromText longText)))
      contentH = fromIntegral lineCount * lineH
      contentW = maximum (0 : [lineWidth fm l | l <- T.lines longText])
      (ix, iy) = widgetContentInset fm
      innerW = rectW field - 2 * ix
      innerH = rectH field - 2 * iy
      barLaneW = textAreaBarLane
      barLaneH = textAreaBarLane
      hasV0 = contentH > innerH
      hasH = contentW > (if hasV0 then max 0 (innerW - barLaneW) else innerW)
      availH = if hasH then max 0 (innerH - barLaneH) else innerH
      expectedMaxY = max 0 (contentH - availH)
      pos = spanCenter field
      wheelDown = inp0 {inputMousePos = pos, inputScroll = V2 0 100}
    _ <- runFrame ctx wheelDown ui
    off <- getScrollOffset ctx (respId resp)
    assert failed (abs (off - expectedMaxY) < 0.5)

runTextAreaScrollDragTest :: Context -> IORef Int -> IO ()
runTextAreaScrollDragTest ctx failed = do
  let
    longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \(Rect rx ry rw rh) -> do
    let
      fm = ctxFontMetrics ctx
      field = Rect rx ry rw rh
      contentH = 40 * textAreaLineHeight fm
    off0 <- getScrollOffset ctx (respId resp)
    assertEq failed off0 0
    assertJust failed (textAreaScrollBarLayout fm field contentH off0) $ \layout -> do
      let
        thumb = sbThumb layout
        thumbCenter = spanCenter thumb
        press = pressAt inp0 thumbCenter
      _ <- runFrame ctx press ui
      -- Drag the thumb down by 30 pixels
      let
        drag = holdAt press (V2 (v2X thumbCenter) (v2Y thumbCenter + 30))
      _ <- runFrame ctx drag ui
      off1 <- getScrollOffset ctx (respId resp)
      assertGt failed off1 off0
      -- Release the mouse
      let
        release = drag {inputMouseDown = False, inputMouseReleased = True}
      _ <- runFrame ctx release ui
      -- Clicking/dragging scrollbar must not initiate text selection or alter buffer
      store <- getStore ctx
      let
        key = intKey (respId resp)
        st = loadTextAreaState store key
      assertEq failed (toText (buffer st)) longText
      assert failed (selectionAnchor st == getCursor (buffer st))

runTextAreaCursorOnScrollBarTest :: Context -> IORef Int -> IO ()
runTextAreaCursorOnScrollBarTest ctx failed = do
  let
    longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let
    labelPos =
      [ (rectX r + rectW r / 2, rectY r + 0.5)
      | (r, txt, _, _, _) <- spans
      , txt == "Notes"
      ]
  mRect <- getPrevRect ctx (respId resp)
  case (labelPos, mRect) of
    ([(lx, ly)], Just (Rect rx ry rw rh)) -> do
      let
        fm = ctxFontMetrics ctx
        field = Rect rx ry rw rh
        contentH = 40 * textAreaLineHeight fm
      -- Hover over label -> UiCursorDefault
      let
        labelHover = inp0 {inputMousePos = V2 lx ly}
      _ <- runFrame ctx labelHover ui
      labelKind <- uiCursorKind ctx labelHover
      assertEq failed labelKind UiCursorDefault

      -- Hover over text field area (left side) -> UiCursorText
      let
        textHover = inp0 {inputMousePos = V2 (rectX field + 20) (rectY field + 20)}
      _ <- runFrame ctx textHover ui
      textKind <- uiCursorKind ctx textHover
      assertEq failed textKind UiCursorText

      -- Hover over scrollbar thumb -> UiCursorGrab
      assertJust failed (textAreaScrollBarLayout fm field contentH 0) $ \layout -> do
        let
          thumb = sbThumb layout
          thumbCenter = spanCenter thumb
          thumbHover = inp0 {inputMousePos = thumbCenter}
        _ <- runFrame ctx thumbHover ui
        thumbKind <- uiCursorKind ctx thumbHover
        assertEq failed thumbKind UiCursorGrab

        -- Press down on scrollbar thumb -> UiCursorGrabbing
        let
          thumbPress = thumbHover {inputMouseDown = True, inputMousePressed = True}
        _ <- runFrame ctx thumbPress ui
        grabbing <- cursorKindIs ctx thumbPress UiCursorGrabbing
        assert failed grabbing
    _ -> assert failed False

runTextAreaHScrollWheelTest :: Context -> IORef Int -> IO ()
runTextAreaHScrollWheelTest ctx failed = do
  let
    longLine = T.replicate 15 "0123456789"
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" longLine)
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \(Rect rx ry rw rh) -> do
    let
      fm = ctxFontMetrics ctx
      field = Rect rx ry rw rh
      pos = spanCenter field
      wheelRight = inp0 {inputMousePos = pos, inputScroll = V2 1 0}
    V2 offX0 offY0 <- getScrollOffset2D ctx (respId resp)
    assertEq failed offX0 0
    assertEq failed offY0 0
    _ <- runFrame ctx wheelRight ui
    V2 offX1 offY1 <- getScrollOffset2D ctx (respId resp)
    assertGt failed offX1 offX0
    assertEq failed offY1 0
    let
      wheelRightMore = inp0 {inputMousePos = pos, inputScroll = V2 3 0}
    _ <- runFrame ctx wheelRightMore ui
    V2 offX2 _ <- getScrollOffset2D ctx (respId resp)
    assertGt failed offX2 offX1
    -- A click in the scrolled text lands on the column under the pointer,
    -- counting the horizontal offset.
    let
      (ix, iy) = widgetContentInset fm
      clickX = 30
      textClick = pressAt inp0 (V2 (rectX field + ix + clickX) (rectY field + iy + 5))
    _ <- runFrame ctx textClick ui
    _ <- runFrame ctx textClick {inputMouseDown = False, inputMousePressed = False, inputMouseReleased = True} ui
    store <- getStore ctx
    let
      key = intKey (respId resp)
      Cursor _ col = getCursor (buffer (loadTextAreaState store key))
    assert failed (abs (fromIntegral col - (offX2 + clickX) / fmAdvance fm '0') <= 1)
    let
      wheelLeft = inp0 {inputMousePos = pos, inputScroll = V2 (-10) 0}
    _ <- runFrame ctx wheelLeft ui
    V2 offX3 _ <- getScrollOffset2D ctx (respId resp)
    assertEq failed offX3 0
    -- The horizontal thumb shows the grab cursors.
    assertJust failed (textAreaHScrollBarLayout fm field (lineWidth fm longLine) 0) $ \layout -> do
      let
        thumb = sbThumb layout
        thumbHover = inp0 {inputMousePos = spanCenter thumb}
      _ <- runFrame ctx thumbHover ui
      thumbKind <- uiCursorKind ctx thumbHover
      assertEq failed thumbKind UiCursorGrab
      let
        thumbPress = thumbHover {inputMouseDown = True, inputMousePressed = True}
      _ <- runFrame ctx thumbPress ui
      grabbing <- cursorKindIs ctx thumbPress UiCursorGrabbing
      assert failed grabbing

runTextAreaHScrollDragTest :: Context -> IORef Int -> IO ()
runTextAreaHScrollDragTest ctx failed = do
  let
    longLine = T.replicate 15 "0123456789"
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" longLine)
  (resp, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \(Rect rx ry rw rh) -> do
    let
      fm = ctxFontMetrics ctx
      field = Rect rx ry rw rh
      contentW = lineWidth fm longLine
    V2 offX0 _ <- getScrollOffset2D ctx (respId resp)
    assertEq failed offX0 0
    assertJust failed (textAreaHScrollBarLayout fm field contentW offX0) $ \layout -> do
      let
        thumb = sbThumb layout
        thumbCenter = spanCenter thumb
        press = pressAt inp0 thumbCenter
      _ <- runFrame ctx press ui
      let
        drag = holdAt press (V2 (v2X thumbCenter + 30) (v2Y thumbCenter))
      _ <- runFrame ctx drag ui
      V2 offX1 _ <- getScrollOffset2D ctx (respId resp)
      assertGt failed offX1 offX0
      let
        release = drag {inputMouseDown = False, inputMouseReleased = True}
      _ <- runFrame ctx release ui
      store <- getStore ctx
      let
        key = intKey (respId resp)
        st = loadTextAreaState store key
      assertEq failed (toText (buffer st)) longLine
      assert failed (selectionAnchor st == getCursor (buffer st))

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
  assertJustM failed (getPrevRect ctx (respId resp)) $ \(Rect rx ry rw rh) -> do
    let
      fm = ctxFontMetrics ctx
      field = Rect rx ry rw rh
      contentH = 40 * textAreaLineHeight fm
      contentW = maximum (0 : [lineWidth fm l | l <- lines2D])
      barLaneW = textAreaBarLane
      barLaneH = textAreaBarLane
      layouts = textAreaScrollBarLayouts fm field contentW contentH 0 0
    case (tasbVertical layouts, tasbHorizontal layouts) of
      (Just vLayout, Just hLayout) -> do
        let
          vTrack = sbTrack vLayout
          hTrack = sbTrack hLayout
        assert
          failed
          (rectY vTrack + rectH vTrack <= rectY field + rectH field - barLaneH + 1)
        assert
          failed
          (rectX hTrack + rectW hTrack <= rectX field + rectW field - barLaneW + 1)
        let
          pos = spanCenter field
          wheel2D = inp0 {inputMousePos = pos, inputScroll = V2 2 3}
        _ <- runFrame ctx wheel2D ui
        V2 offX offY <- getScrollOffset2D ctx (respId resp)
        assertGt failed offX 0
        assertGt failed offY 0
      _ -> assert failed False

runTextAreaScrollCursorLeavesViewportTest :: Context -> IORef Int -> IO ()
runTextAreaScrollCursorLeavesViewportTest ctx failed = do
  let
    longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
    inp0 = withInput 320 220
    ui = column (labeledArea "Notes" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  -- Focus the textarea via Tab
  _ <- runFrame ctx (tabInp inp0) ui
  assertJustM failed (getPrevRect ctx (respId resp)) $ \(Rect rx ry rw rh) -> do
    let
      field = Rect rx ry rw rh
      pos = spanCenter field
      key = intKey (respId resp)

    -- Verify cursor is at top (line 0, col 0) and scroll is 0
    store0 <- getStore ctx
    let
      st0 = loadTextAreaState store0 key
      Cursor r0 c0 = getCursor (buffer st0)
    assertEq failed (r0, c0) (0, 0)
    off0 <- getScrollOffset ctx (respId resp)
    assertEq failed off0 0

    -- Scroll down while focused: caret stays at line 0, but viewport scrolls down
    let
      wheelDown = inp0 {inputMousePos = pos, inputScroll = V2 0 5}
    _ <- runFrame ctx wheelDown ui
    off1 <- getScrollOffset ctx (respId resp)
    assertGt failed off1 0

    -- Run an idle frame while textarea remains focused to ensure scroll offset does not snap back!
    _ <- runFrame ctx inp0 ui
    offIdle <- getScrollOffset ctx (respId resp)
    assertEq failed offIdle off1

    -- Verify caret in buffer is still at (0, 0) even though viewport scrolled down
    store1 <- getStore ctx
    let
      st1 = loadTextAreaState store1 key
      Cursor r1 c1 = getCursor (buffer st1)
    assertEq failed (r1, c1) (0, 0)

    -- Now send keyboard input: typing or navigating MUST bring the cursor back into view!
    let
      typeChar = inp0 {inputChars = "!"}
    _ <- runFrame ctx typeChar ui
    offAfterKey <- getScrollOffset ctx (respId resp)
    -- Cursor is at (0, 1), so ensureCaretVisible brings scroll offset back to 0
    assertEq failed offAfterKey 0
    store2 <- getStore ctx
    let
      st2 = loadTextAreaState store2 key
      Cursor r2 c2 = getCursor (buffer st2)
    assertEq failed (r2, c2) (0, 1)

-- | A backend redraw request needs a frame even when user input is unchanged.
runRefreshRedrawTest :: Context -> IORef Int -> IO ()
runRefreshRedrawTest ctx failed = do
  let
    idle = emptyInput {inputWindowSize = Size 320 200}
    refreshed = idle {inputWindowRedraw = True}
  need <- needsRedraw ctx idle refreshed
  assert failed need

-- | A context-menu Cut/Paste edits the document without any keys or chars on
-- the frame, so the change must surface as a 'respChanged' pulse through the
-- text-area store flag, or callers (the notepad's dirty tracking) never learn
-- the document changed. Covers the hadInput guard in 'textAreaWith'. The
-- caller holds the text, so the cut must also survive the
-- release frame, where the caller still passes back the pre-cut result.
runTextAreaMenuPulseTest :: Context -> IORef Int -> IO ()
runTextAreaMenuPulseTest ctx failed = do
  textRef <- newIORef "abc"
  let
    inp0 = withInput 320 220
    ui = column (held textRef (textAreaWith' grow))
  (resp0, initial) <- warmup2 ctx inp0 ui
  assertEq failed initial "abc"
  assertJustM failed (textAreaHitForWidget ctx (respId resp0)) $ \hit -> do
    let
      field = tahFieldRect hit
      mid = spanCenter field
    -- Focus the editor, as a menu pick would.
    let
      (focusPress, focusRelease) = clickPair inp0 mid
    _ <- runFrame ctx focusPress ui >> runFrame ctx focusRelease ui
    -- Selection-only actions do not pulse: no text delta.
    _ <- runFrame ctx inp0 (runTextCommand (respId resp0) SelectAll)
    ((respSel, valSel), _, _, _) <- runFrame ctx inp0 ui
    assert failed (not (respChanged respSel))
    assertEq failed valSel "abc"
    let
      menuOpen =
        inp0
          { inputMousePos = mid
          , inputMouseRightDown = True
          , inputMouseRightPressed = True
          }
    _ <- runFrame ctx menuOpen ui
    overlays <- collectOverlayTextSpans ctx menuOpen
    assertJust failed (spanRectOf "Cut" overlays) $ \r -> do
      -- The Cut runs through the field's command path on the press
      -- frame; the very next frame (release) must deliver the pulse and
      -- the emptied text, then go quiet again.
      let
        (pickPress, pickRelease) = clickPair inp0 (spanCenter r)
      _ <- runFrame ctx pickPress ui
      ((resp, val), _, _, _) <- runFrame ctx pickRelease ui
      assert failed (respChanged resp)
      assertEq failed val ""
      ((respIdle, valIdle), _, _, _) <- runFrame ctx inp0 ui
      assert failed (not (respChanged respIdle))
      assertEq failed valIdle ""

-- | A command run from a button elsewhere (an app's Edit menu) focuses the
-- field it edits, so Select All followed by typing replaces the text.
runTextCommandFocusTest :: Context -> IORef Int -> IO ()
runTextCommandFocusTest ctx failed = do
  ref <- newIORef "abc"
  let
    inp = withInput 320 220
    ui = column $ do
      (area, _) <- held ref (textAreaWith' (fixedH 80))
      selectAll <- button' "Select All"
      when (respClicked selectAll) (runTextCommand (respId area) SelectAll)
      pure selectAll
  selectAll <- warmup2 ctx inp ui
  let
    (press, release) = clickPair inp (centerOf selectAll)
  mapM_ (\i -> runFrame ctx i ui) [press, release, inp, inp {inputChars = "Z"}, inp]
  assertEq failed "Z" =<< readIORef ref

-- | An editor mounted under a fresh key scrolls on wheel hover without needing focus.
runTextAreaRemountScrollTest :: Context -> IORef Int -> IO ()
runTextAreaRemountScrollTest ctx failed = do
  let
    longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
    inp0 = withInput 320 220
    mkUi k = column $ keyed k $ textAreaWith' grow longText
  _ <- warmup2 ctx inp0 (mkUi (1 :: Int))
  (resp, _) <- warmup2 ctx inp0 (mkUi (2 :: Int))
  assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
    let
      field = tahFieldRect hit
      pos = spanCenter field
      -- Large delta so hover animations settle within the warm-up frames
      -- and the idle frame below reports no damage of its own.
      settleDt = 1.0
      hover = inp0 {inputMousePos = pos, inputDeltaTime = settleDt}
      warmupFrames = 4 :: Int
    -- Park the pointer over the editor first, so the wheel frame does not
    -- also change the hot widget (whose damage would mask a missing scroll
    -- repaint).
    replicateM_ warmupFrames (runFrame ctx hover (mkUi (2 :: Int)))
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
  ref <- newIORef ""
  commands <- newIORef []
  let
    inp = withInput 320 120
    -- Commands queued by the test run after the field, as an app's menu
    -- would; the field's undo state is read the same way.
    ui = column $ do
      (resp, _) <- held ref textInput'
      pending <- uiIO (readIORef commands)
      uiIO (writeIORef commands [])
      mapM_ (runTextCommand (respId resp)) pending
      (,) resp <$> textCanUndo (respId resp)
    ctrl = Modifiers False True False
    ctrlShift = Modifiers True True False
    frame i = (\(a, _, _, _) -> a) <$> runFrame ctx i ui
  _ <- warmup2 ctx inp ui
  _ <- frame (tabInp inp)
  mapM_ (\c -> frame inp {inputChars = T.singleton c}) ("red fox" :: String)
  assertEq failed "red fox" =<< readIORef ref
  (_, canUndo) <- frame inp
  assert failed canUndo
  _ <- frame inp {inputChars = "z", inputModifiers = ctrl}
  assertEq failed "red " =<< readIORef ref
  _ <- frame inp {inputChars = "z", inputModifiers = ctrl}
  assertEq failed "" =<< readIORef ref
  _ <- frame inp {inputChars = "z", inputModifiers = ctrlShift}
  assertEq failed "red " =<< readIORef ref
  _ <- frame inp {inputChars = "y", inputModifiers = ctrl}
  assertEq failed "red fox" =<< readIORef ref
  -- A command from outside the field's frame edits it and pulses it once.
  writeIORef commands [InsertText "!"]
  _ <- frame inp
  (pulsed, _) <- frame inp
  assert failed (respChanged pulsed)
  assertEq failed "red fox!" =<< readIORef ref
  (quiet, _) <- frame inp
  assert failed (not (respChanged quiet))
  writeIORef commands [Undo]
  _ <- frame inp
  _ <- frame inp
  assertEq failed "red fox" =<< readIORef ref
  -- The caller replacing the value drops the history recorded against the
  -- old text.
  writeIORef ref "something else"
  _ <- frame inp
  (_, stillUndoable) <- frame inp
  assert failed (not stillUndoable)

-- | A text area over a 'TextDocument' hands back the document it was passed
-- while nothing edits it (a cursor move included), a new one when typing
-- does, and passes undo and replacement through like the 'Text' one.
runTextAreaDocumentTest :: Context -> IORef Int -> IO ()
runTextAreaDocumentTest ctx failed = do
  let original = T.intercalate "\n" ["line " <> T.pack (show i) | i <- [0 .. 999 :: Int]]
  ref <- newIORef (textDocument original)
  let
    inp = withInput 320 220
    ui = column $ do
      (resp, doc) <- held ref textAreaDocument'
      (,) (resp, doc) <$> textCanUndo (respId resp)
    ctrl = Modifiers False True False
    frame i = do
      before <- readIORef ref
      ((resp, after), undoable) <- (\(a, _, _, _) -> a) <$> runFrame ctx i ui
      pure (before, resp, after, undoable)
  warmupFocused ctx inp ui
  (idleIn, idleResp, idleOut, _) <- frame inp
  assert failed (sameDocument idleIn idleOut)
  assert failed (not (respChanged idleResp))
  (moveIn, moveResp, moveOut, _) <- frame (keyInp KeyDown inp)
  assert failed (respChanged moveResp)
  assert failed (sameDocument moveIn moveOut)
  (typedIn, typedResp, typedOut, undoable) <- frame inp {inputChars = "x"}
  assert failed (respChanged typedResp)
  assert failed (not (sameDocument typedIn typedOut))
  assertEq failed (documentLine 1 typedOut) "xline 1"
  assertEq failed (documentLineCount typedOut) 1000
  assert failed undoable
  (againIn, _, againOut, _) <- frame inp
  assert failed (sameDocument againIn againOut)
  _ <- frame inp {inputChars = "z", inputModifiers = ctrl}
  undone <- readIORef ref
  assertEq failed (documentText undone) original
  -- Replacing the document drops the history recorded against the old one.
  writeIORef ref (textDocument "fresh")
  _ <- frame inp
  (_, _, fresh, stillUndoable) <- frame inp
  assertEq failed fresh (textDocument "fresh")
  assert failed (not stillUndoable)
  -- A caller that builds an equal document every frame lets the loop sleep.
  copyCtx <- newContext
  source <- newIORef original
  let
    -- Split inside the frame, so each frame passes a new copy.
    copies = column (void (textAreaDocument =<< uiIO (textDocument <$> readIORef source)))
    idle = inp {inputDeltaTime = 1}
  _ <- warmup2 copyCtx inp copies
  _ <- warmup2 copyCtx idle copies
  assert failed . not =<< needsRedraw copyCtx idle idle

-- | The content width a text area keeps up to date line by line matches a
-- fresh measurement after edits that widen, move and shorten its widest line.
runTextAreaWidthTrackingTest :: Context -> IORef Int -> IO ()
runTextAreaWidthTrackingTest ctx failed = do
  ref <- newIORef (T.intercalate "\n" (replicate 200 "short line" ++ ["the widest line of them all"] ++ replicate 200 "short line"))
  let
    inp = withInput 400 300
    ui = column (held ref (textAreaWith' grow))
    frame i = (\(a, _, _, _) -> a) <$> runFrame ctx i ui
    ctrl = Modifiers False True False
    check = do
      (resp, _) <- frame inp
      assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
        (tracked, _) <- textAreaContentMetrics ctx (tahNodeIdx hit)
        text <- readIORef ref
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
  _ <- frame inp {inputKeys = inputKeysFromList [KeyHome, KeyEnd], inputModifiers = Modifiers True False False}
  _ <- frame (keyInp KeyBackspace inp)
  check
  -- Undo brings it back.
  _ <- frame inp {inputChars = "z", inputModifiers = ctrl}
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
    ui = column (textAreaWith' grow original)
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
