module Cases.TextInput
  ( runButtonHoverAnimTest
  , runButtonPressReleaseHoverTest
  , runTextInputClickSelectTest
  , runTextInputClipboardTest
  , runTextInputCtrlATest
  , runTextInputCursorTest
  , runTextAreaCursorTest
  , runTextAreaCutClearsSelectionTest
  , runTextAreaCtrlATest
  , runTextFieldHoverBoundaryTest
  , runTextInputCutClearsSelectionTest
  , runTextInputCutMenuTest
  , runTextInputDirtyTest
  , runTextInputFocusSdlTest
  , runTextInputFocusTest
  , runTextInputMenuTest
  , runTextInputMenuUnfocusedTest
  , runTextInputMouseSelectionTest
  , runTextInputSelectionTest
  , runTextInputSpanTest
  , runTextInputFfCaretTest
  , runTextInputScrollTest
  , runTextInputWordKeysTest
  , runKvMultilineHeightTest
  , runTextAreaScrollbarVisibilityTest
  , runTextAreaScrollWheelTest
  , runTextAreaScrollDragTest
  , runTextAreaCursorOnScrollBarTest
  , runTextAreaHScrollbarVisibilityTest
  , runTextAreaHScrollWheelTest
  , runTextAreaHScrollDragTest
  , runTextArea2DScrollTest
  , runTextAreaHScrollCursorClickTest
  , runTextAreaScrollCursorLeavesViewportTest
  ) where

import Control.Monad (forM_, replicateM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import NanoUI
import NanoUI.Context (getScrollOffset2D, intKey)
import NanoUI.Frame.TextEdit
  ( TextAreaGeom (..)
  , TextAreaScrollBarLayouts (..)
  , textAreaBarLanes
  , textAreaGeom
  , textAreaHScrollBarLayout
  , textAreaScrollBarLayout
  , textAreaScrollBarLayouts
  )
import NanoUI.Store (WidgetStore (..), slotKey, slotTextInputScroll)
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, withInput)
import NanoUI.Testing.Harness (assertSpansHas, clickPair, spanYOf, warmup2, withDelta)
import NanoUI.Widgets.TextArea (buffer, loadTextAreaState, selectionAnchor)
import NanoUI.Widgets.TextBuffer (Cursor (..), getCursor, toText)

runTextInputCursorTest :: Context -> IORef Int -> IO ()
runTextInputCursorTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column (textInput "Name" "")
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let labelPos = [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, txt == "Name"]
      fieldPos = [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, "Enter" `T.isInfixOf` txt]
  case (labelPos, fieldPos) of
    ([(lx, ly)], [(fx, fy)]) -> do
      let labelHover = inp0 {inputMousePos = V2 lx ly}
      _ <- runFrame ctx labelHover ui
      labelKind <- uiCursorKind ctx labelHover
      assertEq failed labelKind UiCursorDefault
      let fieldHover = inp0 {inputMousePos = V2 fx fy}
      _ <- runFrame ctx fieldHover ui
      fieldKind <- uiCursorKind ctx fieldHover
      assertEq failed fieldKind UiCursorText
      let click = fieldHover {inputMouseDown = True, inputMousePressed = True, inputMouseReleased = False}
      _ <- runFrame ctx click ui
      clickKind <- uiCursorKind ctx click
      assertEq failed clickKind UiCursorText
    _ -> assert failed False

runTextAreaCursorTest :: Context -> IORef Int -> IO ()
runTextAreaCursorTest ctx failed = do
  let inp0 = withInput 320 220
      ui = column (textArea "Notes" "Edit me.\nSecond line.")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let labelPos = [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, txt == "Notes"]
      Rect rx ry rw rh = respRect resp
      emptyField = V2 (rx + rw * 0.5) (ry + rh * 0.5)
  case labelPos of
    [(lx, ly)] -> do
      let labelHover = inp0 {inputMousePos = V2 lx ly}
      _ <- runFrame ctx labelHover ui
      labelKind <- uiCursorKind ctx labelHover
      assertEq failed labelKind UiCursorDefault
      let fieldHover = inp0 {inputMousePos = emptyField}
      _ <- runFrame ctx fieldHover ui
      fieldKind <- uiCursorKind ctx fieldHover
      assertEq failed fieldKind UiCursorText
      let click = fieldHover {inputMouseDown = True, inputMousePressed = True, inputMouseReleased = False}
      _ <- runFrame ctx click ui
      clickKind <- uiCursorKind ctx click
      assertEq failed clickKind UiCursorText
    _ -> assert failed False

runTextFieldHoverBoundaryTest :: Context -> IORef Int -> IO ()
runTextFieldHoverBoundaryTest ctx failed = do
  let inp0 = withInput 320 220
      ui = column (textArea "Notes" "Edit me.\nSecond line.")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let labelPos = [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, txt == "Notes"]
      Rect rx ry rw rh = respRect resp
      fieldPos = V2 (rx + rw * 0.5) (ry + rh * 0.5)
  case labelPos of
    [(lx, ly)] -> do
      let labelHover = inp0 {inputMousePos = V2 lx ly}
          fieldHover = inp0 {inputMousePos = fieldPos}
      _ <- runFrame ctx fieldHover ui
      fieldKind <- uiCursorKind ctx fieldHover
      assertEq failed fieldKind UiCursorText
      _ <- runFrame ctx labelHover ui
      labelKind <- uiCursorKind ctx labelHover
      assertEq failed labelKind UiCursorDefault
      _ <- runFrame ctx fieldHover ui
      fieldKind2 <- uiCursorKind ctx fieldHover
      assertEq failed fieldKind2 UiCursorText
    _ -> assert failed False

runTextInputCutClearsSelectionTest :: Context -> IORef Int -> IO ()
runTextInputCutClearsSelectionTest ctx failed = do
  clipRef <- newIORef (Nothing :: Maybe T.Text)
  let ctx' = withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True)
      inp0 = withInput 320 120
      ui = column (textInput "Name" "hello")
  _ <- warmup2 ctx' inp0 ui
  _ <- runFrame ctx' (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  let shiftLeft =
        inp0
          { inputKeys = inputKeysFromList [KeyLeft]
          , inputModifiers = Modifiers True False False
          }
  _ <- runFrame ctx' shiftLeft ui
  _ <- runFrame ctx' (inp0 {inputChars = "x", inputModifiers = Modifiers False True False}) ui
  clip <- readIORef clipRef
  assertEq failed clip (Just "o")
  ((_, val), _, _, _) <- runFrame ctx' (inp0 {inputChars = "z"}) ui
  assertEq failed val "hellz"

-- Word-wise editing keys (Ctrl or Alt + Backspace/Delete/Left/Right) work in
-- the single-line text input like they do in the text area.
runTextInputWordKeysTest :: Context -> IORef Int -> IO ()
runTextInputWordKeysTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column (textInput "Name" "hello world")
      ctrlMods = Modifiers False True False
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  -- Ctrl+Backspace deletes the word before the cursor ("world").
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyBackspace], inputModifiers = ctrlMods}) ui
  ((_, v1), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed v1 "hello "
  -- Nothing right of the cursor at end of text: Ctrl+Delete is a no-op.
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDelete], inputModifiers = ctrlMods}) ui
  ((_, v2), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed v2 "hello "
  -- Ctrl+Left jumps to the start; typing there proves the cursor moved.
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyLeft], inputModifiers = ctrlMods}) ui
  ((_, v3), _, _, _) <- runFrame ctx (inp0 {inputChars = "X"}) ui
  assertEq failed v3 "Xhello "
  -- Ctrl+Delete removes the word after the cursor ("hello").
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDelete], inputModifiers = ctrlMods}) ui
  ((_, v4), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed v4 "X "

runTextAreaCutClearsSelectionTest :: Context -> IORef Int -> IO ()
runTextAreaCutClearsSelectionTest ctx failed = do
  clipRef <- newIORef (Nothing :: Maybe T.Text)
  let ctx' = withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True)
      inp0 = withInput 320 220
      ui = column (textArea "Notes" "hello")
  (resp, _) <- warmup2 ctx' inp0 ui
  _ <- runFrame ctx' (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx' (inp0 {inputChars = "\x01", inputModifiers = Modifiers False True False}) ui
  ((_, cutVal), _, _, _) <-
    runFrame ctx' (inp0 {inputChars = "x", inputModifiers = Modifiers False True False}) ui
  clip <- readIORef clipRef
  assertEq failed clip (Just "hello")
  assertEq failed cutVal ""
  store <- getStore ctx'
  let key = fromIntegral (hashWidgetId (respId resp))
      st = loadTextAreaState store key ""
  assertEq failed (toText (buffer st)) ""
  assert failed (selectionAnchor st == getCursor (buffer st))
  ((_, val), _, _, _) <- runFrame ctx' (inp0 {inputChars = "z"}) ui
  assertEq failed val "z"

runTextInputSelectionTest :: Context -> IORef Int -> IO ()
runTextInputSelectionTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column (button "Other" >> textInput "Name" "hello")
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  let shiftLeft = inp0 {inputKeys = inputKeysFromList [KeyLeft], inputModifiers = Modifiers True False False}
  _ <- warmup2 ctx shiftLeft ui
  ((_, valReplace), _, _, _) <- runFrame ctx (inp0 {inputChars = "X"}) ui
  assertEq failed valReplace "helX"
  _ <- runFrame ctx (inp0 {inputChars = "a", inputModifiers = Modifiers False True False}) ui
  ((_, valClear), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyBackspace]}) ui
  assertEq failed valClear ""

runTextInputCtrlATest :: Context -> IORef Int -> IO ()
runTextInputCtrlATest ctx failed = do
  term <- newCellContext
  let inp0 = withInput 320 120
      ui = column (textInput "Name" "hello")
  forM_ [ctx, term] $ \c -> do
    _ <- runFrame c inp0 ui
    _ <- runFrame c (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
    _ <- runFrame c (inp0 {inputChars = "\x01", inputModifiers = Modifiers False True False}) ui
    ((_, valClear), _, _, _) <- runFrame c (inp0 {inputKeys = inputKeysFromList [KeyBackspace]}) ui
    assertEq failed valClear ""

runTextAreaCtrlATest :: Context -> IORef Int -> IO ()
runTextAreaCtrlATest ctx failed = do
  term <- newCellContext
  let initial = "line one\nline two\nline three"
      inp0 = withInput 320 220
      ui = column (textArea "Notes" initial)
  forM_ [ctx, term] $ \c -> do
    (resp, _) <- warmup2 c inp0 ui
    -- Tab into textarea to gain focus
    _ <- runFrame c (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
    -- Ctrl+A with "a"
    _ <- runFrame c (inp0 {inputChars = "a", inputModifiers = Modifiers False True False}) ui
    store1 <- getStore c
    let key = fromIntegral (hashWidgetId (respId resp))
        st1 = loadTextAreaState store1 key initial
        Cursor curR curC = getCursor (buffer st1)
        Cursor ancR ancC = selectionAnchor st1
    assertEq failed (ancR, ancC) (0, 0)
    assertEq failed (curR, curC) (2, 10)
    -- Backspace deletes all selected text
    ((_, valClear), _, _, _) <- runFrame c (inp0 {inputKeys = inputKeysFromList [KeyBackspace]}) ui
    assertEq failed valClear ""

  -- Test "\x01" and text replacement on fresh contexts
  pix2 <- newContext
  cell2 <- newCellContext
  let initial2 = "abc\ndef"
      ui2 = column (textArea "Notes2" initial2)
  forM_ [pix2, cell2] $ \c -> do
    (resp2, _) <- warmup2 c inp0 ui2
    _ <- runFrame c (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui2
    -- Ctrl+A with "\x01"
    _ <- runFrame c (inp0 {inputChars = "\x01", inputModifiers = Modifiers False True False}) ui2
    store2 <- getStore c
    let key2 = fromIntegral (hashWidgetId (respId resp2))
        st2 = loadTextAreaState store2 key2 initial2
        Cursor curR2 curC2 = getCursor (buffer st2)
        Cursor ancR2 ancC2 = selectionAnchor st2
    assertEq failed (ancR2, ancC2) (0, 0)
    assertEq failed (curR2, curC2) (1, 3)
    -- Typing a character replaces all text
    ((_, valReplace), _, _, _) <- runFrame c (inp0 {inputChars = "z"}) ui2
    assertEq failed valReplace "z"

runTextInputMouseSelectionTest :: Context -> IORef Int -> IO ()
runTextInputMouseSelectionTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column (textInput "Name" "hello")
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy fw fh : _) -> do
      let fieldY = fy + fh / 2
      _ <- runFrame ctx (inp0 {inputMousePos = V2 (fx + 1) fieldY, inputMouseDown = True, inputMousePressed = True}) ui
      _ <- runFrame ctx (inp0 {inputMousePos = V2 (fx + fw - 1) fieldY, inputMouseDown = True}) ui
      _ <- runFrame ctx (inp0 {inputMousePos = V2 (fx + fw - 1) fieldY, inputMouseDown = False, inputMouseReleased = True}) ui
      ((_, val), _, _, _) <- runFrame ctx (inp0 {inputChars = "z"}) ui
      assertEq failed val "z"
    _ -> assert failed False

runTextInputClickSelectTest :: Context -> IORef Int -> IO ()
runTextInputClickSelectTest _ failed = do
  wordCtx <- newContext
  allCtx <- newContext
  let inp0 = withInput 320 120
      uiWord = column (textInput "Name" "hello world")
      uiAll = column (textInput "Name" "hello")
  _ <- warmup2 wordCtx inp0 uiWord
  spans <- collectTextSpans wordCtx
  case [r | (r, txt, _, _, _) <- spans, txt == "hello world"] of
    (Rect fx fy _ fh : _) -> do
      let pos = V2 (fx + 1) (fy + fh / 2)
          click1 = inp0 {inputMousePos = pos, inputMouseDown = True, inputMousePressed = True, inputMouseClicks = 1}
          click2 = inp0 {inputMousePos = pos, inputMouseDown = True, inputMousePressed = True, inputMouseClicks = 2}
      _ <- runFrame wordCtx click1 uiWord
      _ <- runFrame wordCtx click2 uiWord
      ((_, val), _, _, _) <- runFrame wordCtx (inp0 {inputKeys = inputKeysFromList [KeyBackspace]}) uiWord
      assertEq failed val " world"
    _ -> assert failed False
  _ <- warmup2 allCtx inp0 uiAll
  spansAll <- collectTextSpans allCtx
  case [r | (r, txt, _, _, _) <- spansAll, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let pos = V2 (fx + 1) (fy + fh / 2)
          click1 = inp0 {inputMousePos = pos, inputMouseDown = True, inputMousePressed = True, inputMouseClicks = 1}
          click2 = inp0 {inputMousePos = pos, inputMouseDown = True, inputMousePressed = True, inputMouseClicks = 2}
          click3 = inp0 {inputMousePos = pos, inputMouseDown = True, inputMousePressed = True, inputMouseClicks = 3}
      _ <- runFrame allCtx click1 uiAll
      _ <- runFrame allCtx click2 uiAll
      _ <- runFrame allCtx click3 uiAll
      ((_, val), _, _, _) <- runFrame allCtx (inp0 {inputKeys = inputKeysFromList [KeyBackspace]}) uiAll
      assertEq failed val ""
    _ -> assert failed False

runTextInputClipboardTest :: Context -> IORef Int -> IO ()
runTextInputClipboardTest ctx failed = do
  clipRef <- newIORef (Nothing :: Maybe T.Text)
  let ctx' = withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True)
      inp0 = withInput 320 120
      ui = column (textInput "Name" "hello")
  _ <- warmup2 ctx' inp0 ui
  _ <- runFrame ctx' (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  let selectAll = inp0 {inputChars = "a", inputModifiers = Modifiers False True False}
      copy = inp0 {inputChars = "c", inputModifiers = Modifiers False True False}
      clear = inp0 {inputKeys = inputKeysFromList [KeyBackspace]}
      paste = inp0 {inputChars = "v", inputModifiers = Modifiers False True False}
  _ <- runFrame ctx' selectAll ui
  _ <- runFrame ctx' copy ui
  clip <- readIORef clipRef
  assertEq failed clip (Just "hello")
  _ <- runFrame ctx' selectAll ui >> runFrame ctx' clear ui
  ((_, val), _, _, _) <- runFrame ctx' paste ui
  assertEq failed val "hello"

runTextInputCutMenuTest :: Context -> IORef Int -> IO ()
runTextInputCutMenuTest ctx failed = do
  clipRef <- newIORef (Nothing :: Maybe T.Text)
  let ctx' = withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True)
      inp0 = withInput 320 160
      ui = column (textInput "Name" "hello")
  _ <- warmup2 ctx' inp0 ui
  _ <- runFrame ctx' (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  spans <- collectTextSpans ctx'
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let menuOpen =
            inp0
              { inputMousePos = V2 (fx + 1) (fy + fh / 2)
              , inputMouseRightDown = True
              , inputMouseRightPressed = True
              }
      _ <- runFrame ctx' menuOpen ui
      overlays <- collectOverlayTextSpans ctx' menuOpen
      case [r | (r, txt, _, _, _) <- overlays, txt == "Cut"] of
        (Rect px py pw ph : _) -> do
          let (pickPress, pickRelease) = clickPair inp0 (V2 (px + pw / 2) (py + ph / 2))
          _ <- runFrame ctx' pickPress ui >> runFrame ctx' pickRelease ui
          clip <- readIORef clipRef
          assertEq failed clip (Just "hello")
          ((_, val), _, _, _) <- runFrame ctx' inp0 ui
          assertEq failed val ""
          overlaysAfter <- collectOverlayTextSpans ctx' inp0
          assert failed (not (any (\(_, txt, _, _, _) -> txt == "Cut") overlaysAfter))
        _ -> assert failed False
    _ -> assert failed False

runTextInputMenuTest :: Context -> IORef Int -> IO ()
runTextInputMenuTest ctx failed = do
  clipRef <- newIORef (Just "pasted")
  let ctx' = withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True)
      inp0 = withInput 320 160
      ui = column (textInput "Name" "hello")
  _ <- warmup2 ctx' inp0 ui
  _ <- runFrame ctx' (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  spans <- collectTextSpans ctx'
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let menuOpen = inp0 {inputMousePos = V2 (fx + 1) (fy + fh / 2), inputMouseRightDown = True, inputMouseRightPressed = True}
      _ <- runFrame ctx' menuOpen ui
      overlays <- collectOverlayTextSpans ctx' menuOpen
      case [r | (r, txt, _, _, _) <- overlays, txt == "Paste"] of
        (Rect px py pw ph : _) -> do
          let (pickPress, pickRelease) = clickPair inp0 (V2 (px + pw / 2) (py + ph / 2))
          _ <- runFrame ctx' pickPress ui >> runFrame ctx' pickRelease ui
          ((_, val), _, _, _) <- runFrame ctx' inp0 ui
          assertEq failed val "hellopasted"
        _ -> assert failed False
    _ -> assert failed False

runTextInputMenuUnfocusedTest :: Context -> IORef Int -> IO ()
runTextInputMenuUnfocusedTest ctx failed = do
  clipRef <- newIORef (Just "pasted")
  let ctx' = withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True)
      inp0 = withInput 320 160
      ui = column (textInput "Name" "hello")
  _ <- warmup2 ctx' inp0 ui
  spans <- collectTextSpans ctx'
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let menuOpen = inp0 {inputMousePos = V2 (fx + 1) (fy + fh / 2), inputMouseRightDown = True, inputMouseRightPressed = True}
      _ <- runFrame ctx' menuOpen ui
      overlays <- collectOverlayTextSpans ctx' menuOpen
      assert failed (any (\(_, txt, _, _, _) -> txt == "Paste") overlays)
    _ -> assert failed False

runTextInputSpanTest :: Context -> IORef Int -> IO ()
runTextInputSpanTest ctx failed = do
  _ <- runFrame ctx (withInput 320 120) (column (textInput "Name" "hello"))
  spans <- collectTextSpans ctx
  assertSpansHas failed "hello" spans

runTextInputFocusSdlTest :: Context -> IORef Int -> IO ()
runTextInputFocusSdlTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column (textInput "Name" "")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, "Enter" `T.isInfixOf` txt] of
    [(fx, fy)] -> do
      let inp1 = inp0 {inputMousePos = V2 fx fy, inputMouseDown = True, inputMousePressed = True}
      _ <- runFrame ctx inp1 ui
      focus <- getFocusId ctx
      assertEq failed focus (respId resp)
      spans' <- collectTextSpans ctx
      assert failed (any (\(_, txt, _, _, _) -> txt == "Name") spans')
      let idle = inp0 {inputMousePos = V2 fx fy}
      samples <- replicateM 5 $ do
        _ <- runFrame ctx idle ui
        focusN <- getFocusId ctx
        spansN <- collectTextSpans ctx
        let emptyRs = [r | (r, txt, _, _, _) <- spansN, T.null txt]
        pure (focusN, emptyRs)
      case samples of
        [] -> assert failed False
        (f0, rs0) : rest -> do
          assertEq failed f0 (respId resp)
          assert failed (all (\(f, rs) -> f == respId resp && rs == rs0) rest)
    _ -> assert failed False

runButtonHoverAnimTest :: Context -> IORef Int -> IO ()
runButtonHoverAnimTest ctx failed = do
  let inp0 = withDelta 200 100 0.016
      ui = column (button "Hover")
  _ <- runFrame ctx inp0 ui
  let inp1 = inp0 {inputMousePos = V2 10 10}
  vals <- replicateM 5 (runFrame ctx inp1 ui >> getHotId ctx >>= getAnimationValue ctx)
  let decreases = case vals of
        [] -> False
        _ -> any (uncurry (\a b -> b + 0.001 < a)) (zip vals (drop 1 vals))
  assert failed (not decreases)
  assert failed (last vals >= 0.4)

runButtonPressReleaseHoverTest :: Context -> IORef Int -> IO ()
runButtonPressReleaseHoverTest ctx failed = do
  let inp0 = withDelta 200 100 0.016
      ui = column (button "Hover")
      (press, release) = clickPair inp0 (V2 10 10)
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  hot <- getHotId ctx
  val <- getAnimationValue ctx hot
  assert failed (hashWidgetId hot /= 0)
  assert failed (val >= 0.99)

runTextInputFocusTest :: Context -> IORef Int -> IO ()
runTextInputFocusTest _ failed = do
  ctx <- newCellContext
  let inp0 = withInput 200 100
      ui = column (textInput "Name" "")
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      inp1 = inp0 {inputMousePos = V2 (rx + 1) (ry + 0.5), inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx inp1 ui
  spans <- collectTextSpans ctx
  assert failed (any (\(_, txt, _, _, _) -> T.isInfixOf "\x2502" txt) spans)

runTextInputDirtyTest :: Context -> IORef Int -> IO ()
runTextInputDirtyTest _ failed = do
  ctx <- newCellContext
  let ui = column (textInput "Name" "")
      inp0 = (withInput 200 100) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      (press, release) = clickPair inp0 (V2 (rx + 1) (ry + 0.5))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  let idle = release {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  needFocus <- needsRedraw ctx idle idle
  assert failed needFocus
  _ <- runFrame ctx (idle {inputChars = "ab"}) ui
  dmg <- takeDamage ctx
  assert failed (not (damageIsEmpty dmg))

runTextInputFfCaretTest :: Context -> IORef Int -> IO ()
runTextInputFfCaretTest ctx failed = do
  let fm = ctxFontMetrics ctx
      host = ctxHostProfile ctx
      fs = T.replicate 6 "f"
      adv = fmAdvance fm 'f'
  assertEq failed (lineWidth fm fs) (6 * adv)
  assertEq failed (textIndexAtX host fm fs (lineWidth fm fs)) 6
  assertEq failed (textIndexAtX host fm fs (lineWidth fm (T.take 3 fs))) 3
  let inp0 = withInput 320 120
      ui = column (textInput "Name" fs)
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  assertSpansHas failed fs spans
  case [r | (r, txt, _, _, _) <- spans, txt == fs] of
    (Rect fx fy _ fh : _) -> do
      let pos = V2 (fx + lineWidth fm (T.take 3 fs)) (fy + fh / 2)
          (press, release) = clickPair inp0 pos
      _ <- runFrame ctx press ui
      _ <- runFrame ctx release ui
      ((_, val), _, _, _) <- runFrame ctx (inp0 {inputMousePos = pos, inputChars = "x"}) ui
      assertEq failed val "fffxfff"
    _ -> assert failed False

runTextInputScrollTest :: Context -> IORef Int -> IO ()
runTextInputScrollTest ctx failed = do
  let longText = "VeryLongTextEnteredIntoTheFieldThatExceedsTheWidth"
      inp0 = withInput 200 120
      ui = column (textInput "Name" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  spans0 <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans0, txt == longText] of
    (Rect fx fy _ fh : _) -> do
      let pos = V2 (fx + 50) (fy + fh / 2)
          (press, release) = clickPair inp0 pos
      _ <- runFrame ctx press ui
      _ <- runFrame ctx release ui
      let atEnd = inp0 {inputKeys = inputKeysFromList [KeyEnd]}
      _ <- runFrame ctx atEnd ui
      store <- getStore ctx
      let key = intKey (respId resp)
          scrollEnd = IM.findWithDefault 0 (slotKey slotTextInputScroll key) (storeFloat store)
      assert failed (scrollEnd > 0)
      let atHome = inp0 {inputKeys = inputKeysFromList [KeyHome]}
      _ <- runFrame ctx atHome ui
      storeHome <- getStore ctx
      let scrollHome = IM.findWithDefault 0 (slotKey slotTextInputScroll key) (storeFloat storeHome)
      assertEq failed scrollHome 0
    _ -> assert failed False

runKvMultilineHeightTest :: Context -> IORef Int -> IO ()
runKvMultilineHeightTest ctx failed = do
  let inp0 = withInput 320 400
      ui = column $ do
        card $ do
          kv "Notes" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5"
          kv "Tree" "0"
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  case (spanYOf "Line 5" spans, spanYOf "Tree" spans) of
    ([line5Y], [treeY]) ->
      assert failed (treeY > line5Y)
    _ -> assert failed False

runTextAreaScrollbarVisibilityTest :: Context -> IORef Int -> IO ()
runTextAreaScrollbarVisibilityTest ctx failed = do
  -- Short text fits within viewport: no scrollbar
  let shortText = "Line 1\nLine 2"
      inp0 = withInput 320 220
      uiShort = column (textArea "NotesShort" shortText)
  (respShort, _) <- warmup2 ctx inp0 uiShort
  offShort0 <- getScrollOffset ctx (respId respShort)
  assertEq failed offShort0 0
  mRectShort <- getPrevRect ctx (respId respShort)
  case mRectShort of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          fieldCenter = V2 (rectX field + rectW field / 2) (rectY field + rectH field / 2)
          wheelShort = inp0 {inputMousePos = fieldCenter, inputScroll = V2 0 1}
      _ <- runFrame ctx wheelShort uiShort
      offShort1 <- getScrollOffset ctx (respId respShort)
      assertEq failed offShort1 0
      let contentH = 2 * tagLineHeight geom
      assertEq failed (textAreaScrollBarLayout host fm field contentH 0) Nothing
    _ -> assert failed False

  -- Long text overflowing viewport: scrollbar layout exists and wheel scrolls
  ctxLong <- newPixelContext
  let longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 30]]
      uiLong = column (textArea "NotesLong" longText)
  (respLong, _) <- warmup2 ctxLong inp0 uiLong
  mRectLong <- getPrevRect ctxLong (respId respLong)
  case mRectLong of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctxLong
          host = ctxHostProfile ctxLong
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentH = 30 * tagLineHeight geom
          mLayout = textAreaScrollBarLayout host fm field contentH 0
      case mLayout of
        Nothing -> assert failed False
        Just layout -> do
          assertGt failed (sbMaxOff layout) 0
          assertGt failed (rectH (sbThumb layout)) 0
          let fieldCenter = V2 (rectX field + rectW field / 2) (rectY field + rectH field / 2)
              wheelLong = inp0 {inputMousePos = fieldCenter, inputScroll = V2 0 1}
          offLong0 <- getScrollOffset ctxLong (respId respLong)
          _ <- runFrame ctxLong wheelLong uiLong
          offLong1 <- getScrollOffset ctxLong (respId respLong)
          assertGt failed offLong1 offLong0
    _ -> assert failed False

runTextAreaScrollWheelTest :: Context -> IORef Int -> IO ()
runTextAreaScrollWheelTest ctx failed = do
  let longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
      inp0 = withInput 320 220
      ui = column (textArea "Notes" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  mRect <- getPrevRect ctx (respId resp)
  case mRect of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          pos = V2 (rectX field + rectW field / 2) (rectY field + rectH field / 2)
          wheelDown = inp0 {inputMousePos = pos, inputScroll = V2 0 1}
      off0 <- getScrollOffset ctx (respId resp)
      assertEq failed off0 0
      -- Scroll down 1 notch
      _ <- runFrame ctx wheelDown ui
      off1 <- getScrollOffset ctx (respId resp)
      assertGt failed off1 off0
      -- Scroll down 3 more notches
      let wheelDownMore = inp0 {inputMousePos = pos, inputScroll = V2 0 3}
      _ <- runFrame ctx wheelDownMore ui
      off2 <- getScrollOffset ctx (respId resp)
      assertGt failed off2 off1
      -- Scroll up beyond top to check clamping to 0
      let wheelUp = inp0 {inputMousePos = pos, inputScroll = V2 0 (-10)}
      _ <- runFrame ctx wheelUp ui
      off3 <- getScrollOffset ctx (respId resp)
      assertEq failed off3 0
      -- Text buffer should remain completely unmodified
      store <- getStore ctx
      let key = intKey (respId resp)
          st = loadTextAreaState store key longText
      assertEq failed (toText (buffer st)) longText
    _ -> assert failed False

runTextAreaScrollDragTest :: Context -> IORef Int -> IO ()
runTextAreaScrollDragTest ctx failed = do
  let longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
      inp0 = withInput 320 220
      ui = column (textArea "Notes" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  mRect <- getPrevRect ctx (respId resp)
  case mRect of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentH = 40 * tagLineHeight geom
      off0 <- getScrollOffset ctx (respId resp)
      assertEq failed off0 0
      case textAreaScrollBarLayout host fm field contentH off0 of
        Nothing -> assert failed False
        Just layout -> do
          let thumb = sbThumb layout
              thumbCenter = V2 (rectX thumb + rectW thumb / 2) (rectY thumb + rectH thumb / 2)
              press = inp0 {inputMousePos = thumbCenter, inputMouseDown = True, inputMousePressed = True}
          _ <- runFrame ctx press ui
          -- Drag the thumb down by 30 pixels
          let drag = press {inputMousePressed = False, inputMousePos = V2 (v2X thumbCenter) (v2Y thumbCenter + 30)}
          _ <- runFrame ctx drag ui
          off1 <- getScrollOffset ctx (respId resp)
          assertGt failed off1 off0
          -- Release the mouse
          let release = drag {inputMouseDown = False, inputMouseReleased = True}
          _ <- runFrame ctx release ui
          -- Clicking/dragging scrollbar must not initiate text selection or alter buffer
          store <- getStore ctx
          let key = intKey (respId resp)
              st = loadTextAreaState store key longText
          assertEq failed (toText (buffer st)) longText
          assert failed (selectionAnchor st == getCursor (buffer st))
    _ -> assert failed False

runTextAreaCursorOnScrollBarTest :: Context -> IORef Int -> IO ()
runTextAreaCursorOnScrollBarTest ctx failed = do
  let longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
      inp0 = withInput 320 220
      ui = column (textArea "Notes" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let labelPos = [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, txt == "Notes"]
  mRect <- getPrevRect ctx (respId resp)
  case (labelPos, mRect) of
    ([(lx, ly)], Just (Rect rx ry rw rh)) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentH = 40 * tagLineHeight geom
      -- Hover over label -> UiCursorDefault
      let labelHover = inp0 {inputMousePos = V2 lx ly}
      _ <- runFrame ctx labelHover ui
      labelKind <- uiCursorKind ctx labelHover
      assertEq failed labelKind UiCursorDefault

      -- Hover over text field area (left side) -> UiCursorText
      let textHover = inp0 {inputMousePos = V2 (rectX field + 20) (rectY field + 20)}
      _ <- runFrame ctx textHover ui
      textKind <- uiCursorKind ctx textHover
      assertEq failed textKind UiCursorText

      -- Hover over scrollbar thumb -> UiCursorGrab
      case textAreaScrollBarLayout host fm field contentH 0 of
        Nothing -> assert failed False
        Just layout -> do
          let thumb = sbThumb layout
              thumbCenter = V2 (rectX thumb + rectW thumb / 2) (rectY thumb + rectH thumb / 2)
              thumbHover = inp0 {inputMousePos = thumbCenter}
          _ <- runFrame ctx thumbHover ui
          thumbKind <- uiCursorKind ctx thumbHover
          assertEq failed thumbKind UiCursorGrab

          -- Press down on scrollbar thumb -> UiCursorGrabbing
          let thumbPress = thumbHover {inputMouseDown = True, inputMousePressed = True}
          _ <- runFrame ctx thumbPress ui
          grabbing <- cursorKindIs ctx thumbPress UiCursorGrabbing
          assert failed grabbing
    _ -> assert failed False

runTextAreaHScrollbarVisibilityTest :: Context -> IORef Int -> IO ()
runTextAreaHScrollbarVisibilityTest ctx failed = do
  let shortText = "Short"
      longText = T.replicate 10 "0123456789"
      inp0 = withInput 320 220
      uiShort = column (textArea "Notes" shortText)
      uiLong = column (textArea "Notes" longText)
  (respS, _) <- warmup2 ctx inp0 uiShort
  mRectS <- getPrevRect ctx (respId respS)
  case mRectS of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentW = textDisplayWidth host fm shortText
          mLayout = textAreaHScrollBarLayout host fm field contentW 0
      assertEq failed mLayout Nothing
    _ -> assert failed False

  (respL, _) <- warmup2 ctx inp0 uiLong
  mRectL <- getPrevRect ctx (respId respL)
  case mRectL of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentW = textDisplayWidth host fm longText
          mLayout = textAreaHScrollBarLayout host fm field contentW 0
      case mLayout of
        Nothing -> assert failed False
        Just layout -> do
          let track = sbTrack layout
              thumb = sbThumb layout
          assertGt failed (rectW track) 0
          assertGt failed (rectW thumb) 0
          assertGt failed (sbMaxOff layout) 0
    _ -> assert failed False

runTextAreaHScrollWheelTest :: Context -> IORef Int -> IO ()
runTextAreaHScrollWheelTest ctx failed = do
  let longLine = T.replicate 15 "0123456789"
      inp0 = withInput 320 220
      ui = column (textArea "Notes" longLine)
  (resp, _) <- warmup2 ctx inp0 ui
  mRect <- getPrevRect ctx (respId resp)
  case mRect of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          pos = V2 (rectX field + rectW field / 2) (rectY field + rectH field / 2)
          wheelRight = inp0 {inputMousePos = pos, inputScroll = V2 1 0}
      V2 offX0 offY0 <- getScrollOffset2D ctx (respId resp)
      assertEq failed offX0 0
      assertEq failed offY0 0
      _ <- runFrame ctx wheelRight ui
      V2 offX1 offY1 <- getScrollOffset2D ctx (respId resp)
      assertGt failed offX1 offX0
      assertEq failed offY1 0
      let wheelRightMore = inp0 {inputMousePos = pos, inputScroll = V2 3 0}
      _ <- runFrame ctx wheelRightMore ui
      V2 offX2 _ <- getScrollOffset2D ctx (respId resp)
      assertGt failed offX2 offX1
      let wheelLeft = inp0 {inputMousePos = pos, inputScroll = V2 (-10) 0}
      _ <- runFrame ctx wheelLeft ui
      V2 offX3 _ <- getScrollOffset2D ctx (respId resp)
      assertEq failed offX3 0
    _ -> assert failed False

runTextAreaHScrollDragTest :: Context -> IORef Int -> IO ()
runTextAreaHScrollDragTest ctx failed = do
  let longLine = T.replicate 15 "0123456789"
      inp0 = withInput 320 220
      ui = column (textArea "Notes" longLine)
  (resp, _) <- warmup2 ctx inp0 ui
  mRect <- getPrevRect ctx (respId resp)
  case mRect of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentW = textDisplayWidth host fm longLine
      V2 offX0 _ <- getScrollOffset2D ctx (respId resp)
      assertEq failed offX0 0
      case textAreaHScrollBarLayout host fm field contentW offX0 of
        Nothing -> assert failed False
        Just layout -> do
          let thumb = sbThumb layout
              thumbCenter = V2 (rectX thumb + rectW thumb / 2) (rectY thumb + rectH thumb / 2)
              press = inp0 {inputMousePos = thumbCenter, inputMouseDown = True, inputMousePressed = True}
          _ <- runFrame ctx press ui
          let drag = press {inputMousePressed = False, inputMousePos = V2 (v2X thumbCenter + 30) (v2Y thumbCenter)}
          _ <- runFrame ctx drag ui
          V2 offX1 _ <- getScrollOffset2D ctx (respId resp)
          assertGt failed offX1 offX0
          let release = drag {inputMouseDown = False, inputMouseReleased = True}
          _ <- runFrame ctx release ui
          store <- getStore ctx
          let key = intKey (respId resp)
              st = loadTextAreaState store key longLine
          assertEq failed (toText (buffer st)) longLine
          assert failed (selectionAnchor st == getCursor (buffer st))
    _ -> assert failed False

runTextArea2DScrollTest :: Context -> IORef Int -> IO ()
runTextArea2DScrollTest ctx failed = do
  let lines2D = [T.pack (show (i :: Int)) <> " - " <> T.replicate 10 "abcdefghij" | i <- [1 .. 40]]
      text2D = T.unlines lines2D
      inp0 = withInput 320 220
      ui = column (textArea "Notes" text2D)
  (resp, _) <- warmup2 ctx inp0 ui
  mRect <- getPrevRect ctx (respId resp)
  case mRect of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentH = 40 * tagLineHeight geom
          contentW = maximum (0 : [textDisplayWidth host fm l | l <- lines2D])
          (barLaneW, barLaneH) = textAreaBarLanes host fm
          layouts = textAreaScrollBarLayouts host fm field contentW contentH 0 0
      case (tasbVertical layouts, tasbHorizontal layouts) of
        (Just vLayout, Just hLayout) -> do
          let vTrack = sbTrack vLayout
              hTrack = sbTrack hLayout
          assert failed (rectY vTrack + rectH vTrack <= rectY field + rectH field - barLaneH + 1)
          assert failed (rectX hTrack + rectW hTrack <= rectX field + rectW field - barLaneW + 1)
          let pos = V2 (rectX field + rectW field / 2) (rectY field + rectH field / 2)
              wheel2D = inp0 {inputMousePos = pos, inputScroll = V2 2 3}
          _ <- runFrame ctx wheel2D ui
          V2 offX offY <- getScrollOffset2D ctx (respId resp)
          assertGt failed offX 0
          assertGt failed offY 0
        _ -> assert failed False
    _ -> assert failed False

runTextAreaHScrollCursorClickTest :: Context -> IORef Int -> IO ()
runTextAreaHScrollCursorClickTest ctx failed = do
  let longLine = T.replicate 15 "0123456789"
      inp0 = withInput 320 220
      ui = column (textArea "Notes" longLine)
  (resp, _) <- warmup2 ctx inp0 ui
  mRect <- getPrevRect ctx (respId resp)
  case mRect of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          contentW = textDisplayWidth host fm longLine
      case textAreaHScrollBarLayout host fm field contentW 0 of
        Nothing -> assert failed False
        Just layout -> do
          let thumb = sbThumb layout
              thumbCenter = V2 (rectX thumb + rectW thumb / 2) (rectY thumb + rectH thumb / 2)
              thumbHover = inp0 {inputMousePos = thumbCenter}
          _ <- runFrame ctx thumbHover ui
          thumbKind <- uiCursorKind ctx thumbHover
          assertEq failed thumbKind UiCursorGrab

          let thumbPress = thumbHover {inputMouseDown = True, inputMousePressed = True}
          _ <- runFrame ctx thumbPress ui
          grabbing <- cursorKindIs ctx thumbPress UiCursorGrabbing
          assert failed grabbing

          let release = thumbPress {inputMouseDown = False, inputMouseReleased = True}
          _ <- runFrame ctx release ui

          let pos = V2 (rectX field + rectW field / 2) (rectY field + rectH field / 2)
              wheelRight = inp0 {inputMousePos = pos, inputScroll = V2 5 0}
          _ <- runFrame ctx wheelRight ui
          V2 offX _ <- getScrollOffset2D ctx (respId resp)
          assertGt failed offX 0

          let (ix, iy) = widgetContentInset host fm
              clickPos = V2 (rectX field + ix + 30) (rectY field + iy + 5)
              textClick = inp0 {inputMousePos = clickPos, inputMouseDown = True, inputMousePressed = True}
          _ <- runFrame ctx textClick ui
          let textRelease = textClick {inputMouseDown = False, inputMouseReleased = True}
          _ <- runFrame ctx textRelease ui

          store <- getStore ctx
          let key = intKey (respId resp)
              st = loadTextAreaState store key longLine
              Cursor _ col = getCursor (buffer st)
          assertGt failed col 0
    _ -> assert failed False

runTextAreaScrollCursorLeavesViewportTest :: Context -> IORef Int -> IO ()
runTextAreaScrollCursorLeavesViewportTest ctx failed = do
  let longText = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]
      inp0 = withInput 320 220
      ui = column (textArea "Notes" longText)
  (resp, _) <- warmup2 ctx inp0 ui
  -- Focus the textarea via Tab
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  mRect <- getPrevRect ctx (respId resp)
  case mRect of
    Just (Rect rx ry rw rh) -> do
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          geom = textAreaGeom host fm rx ry rw rh
          field = tagFieldRect geom
          pos = V2 (rectX field + rectW field / 2) (rectY field + rectH field / 2)
          key = intKey (respId resp)

      -- Verify cursor is at top (line 0, col 0) and scroll is 0
      store0 <- getStore ctx
      let st0 = loadTextAreaState store0 key longText
          Cursor r0 c0 = getCursor (buffer st0)
      assertEq failed (r0, c0) (0, 0)
      off0 <- getScrollOffset ctx (respId resp)
      assertEq failed off0 0

      -- Scroll down while focused: caret stays at line 0, but viewport scrolls down
      let wheelDown = inp0 {inputMousePos = pos, inputScroll = V2 0 5}
      _ <- runFrame ctx wheelDown ui
      off1 <- getScrollOffset ctx (respId resp)
      assertGt failed off1 0

      -- Run an idle frame while textarea remains focused to ensure scroll offset does not snap back!
      _ <- runFrame ctx inp0 ui
      offIdle <- getScrollOffset ctx (respId resp)
      assertEq failed offIdle off1

      -- Verify caret in buffer is still at (0, 0) even though viewport scrolled down
      store1 <- getStore ctx
      let st1 = loadTextAreaState store1 key longText
          Cursor r1 c1 = getCursor (buffer st1)
          lineH = tagLineHeight geom
          caretViewportY = fromIntegral r1 * lineH - offIdle
      assertEq failed (r1, c1) (0, 0)
      -- Caret is above the visible viewport:
      assert failed (caretViewportY < 0)

      -- Now send keyboard input: typing or navigating MUST bring the cursor back into view!
      let typeChar = inp0 {inputChars = "!"}
      _ <- runFrame ctx typeChar ui
      offAfterKey <- getScrollOffset ctx (respId resp)
      -- Cursor is at (0, 1), so ensureCaretVisible brings scroll offset back to 0
      assertEq failed offAfterKey 0
      store2 <- getStore ctx
      let st2 = loadTextAreaState store2 key longText
          Cursor r2 c2 = getCursor (buffer st2)
      assertEq failed (r2, c2) (0, 1)
    _ -> assert failed False





