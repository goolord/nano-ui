module Cases.TextInput
  ( runButtonHoverAnimTest
  , runButtonPressReleaseHoverTest
  , runTextInputClickSelectTest
  , runTextInputClipboardTest
  , runTextInputCtrlATest
  , runTextInputCursorTest
  , runTextAreaCursorTest
  , runTextAreaCutClearsSelectionTest
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
  ) where

import Control.Monad (forM_, replicateM)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness (assertSpansHas, clickPair, warmup2, withDelta)
import NanoUI.Widgets.TextArea (buffer, loadTextAreaState, selectionAnchor)
import NanoUI.Widgets.TextBuffer (getCursor, toText)

runTextInputCursorTest :: Context -> IORef Int -> IO ()
runTextInputCursorTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column defaultLayout (textInput "Name" "")
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
      ui = column defaultLayout (textArea "Notes" "Edit me.\nSecond line.")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let labelPos = [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, txt == "Notes"]
      Rect rx ry rw rh = respRect resp
      emptyField = V2 (rx + rw * 0.85) (ry + rh - 10)
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
      ui = column defaultLayout (textArea "Notes" "Edit me.\nSecond line.")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let labelPos = [(rectX r + rectW r / 2, rectY r + 0.5) | (r, txt, _, _, _) <- spans, txt == "Notes"]
      Rect rx ry rw rh = respRect resp
      fieldPos = V2 (rx + rw * 0.85) (ry + rh - 10)
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
      ui = column defaultLayout (textInput "Name" "hello")
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

runTextAreaCutClearsSelectionTest :: Context -> IORef Int -> IO ()
runTextAreaCutClearsSelectionTest ctx failed = do
  clipRef <- newIORef (Nothing :: Maybe T.Text)
  let ctx' = withClipboard ctx (readIORef clipRef) (\s -> writeIORef clipRef (Just s) >> pure True)
      inp0 = withInput 320 220
      ui = column defaultLayout (textArea "Notes" "hello")
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
      ui = column defaultLayout (button "Other" >> textInput "Name" "hello")
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
      ui = column defaultLayout (textInput "Name" "hello")
  forM_ [ctx, term] $ \c -> do
    _ <- runFrame c inp0 ui
    _ <- runFrame c (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
    _ <- runFrame c (inp0 {inputChars = "\x01", inputModifiers = Modifiers False True False}) ui
    ((_, valClear), _, _, _) <- runFrame c (inp0 {inputKeys = inputKeysFromList [KeyBackspace]}) ui
    assertEq failed valClear ""

runTextInputMouseSelectionTest :: Context -> IORef Int -> IO ()
runTextInputMouseSelectionTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column defaultLayout (textInput "Name" "hello")
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
      uiWord = column defaultLayout (textInput "Name" "hello world")
      uiAll = column defaultLayout (textInput "Name" "hello")
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
      ui = column defaultLayout (textInput "Name" "hello")
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
      ui = column defaultLayout (textInput "Name" "hello")
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
      ui = column defaultLayout (textInput "Name" "hello")
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
      ui = column defaultLayout (textInput "Name" "hello")
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
  _ <- runFrame ctx (withInput 320 120) (column defaultLayout (textInput "Name" "hello"))
  spans <- collectTextSpans ctx
  assertSpansHas failed "hello" spans

runTextInputFocusSdlTest :: Context -> IORef Int -> IO ()
runTextInputFocusSdlTest ctx failed = do
  let inp0 = withInput 320 120
      ui = column defaultLayout (textInput "Name" "")
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
      ui = column defaultLayout (button "Hover")
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
      ui = column defaultLayout (button "Hover")
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
      ui = column defaultLayout (textInput "Name" "")
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      inp1 = inp0 {inputMousePos = V2 (rx + 1) (ry + 0.5), inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx inp1 ui
  spans <- collectTextSpans ctx
  assert failed (any (\(_, txt, _, _, _) -> T.isInfixOf "\x2502" txt) spans)

runTextInputDirtyTest :: Context -> IORef Int -> IO ()
runTextInputDirtyTest _ failed = do
  ctx <- newCellContext
  let ui = column defaultLayout (textInput "Name" "")
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
      ui = column defaultLayout (textInput "Name" fs)
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



