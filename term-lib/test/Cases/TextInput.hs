module Cases.TextInput
  ( runButtonHoverAnimTest
  , runButtonPressReleaseHoverTest
  , runTextInputClickSelectTest
  , runTextInputClipboardTest
  , runTextInputCtrlATest
  , runTextInputCursorTest
  , runTextInputDirtyTest
  , runTextInputFocusSdlTest
  , runTextInputFocusTest
  , runTextInputMenuTest
  , runTextInputMouseSelectionTest
  , runTextInputSelectionTest
  , runTextInputSpanTest
  ) where

import Control.Monad (forM_, replicateM, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, withInput)
import NanoUI.Testing.Harness (spansHas, warmup2, withDelta)
import NanoUI.Testing.Term (newAdaptiveTerminalContext)
runTextInputCursorTest :: Context -> IORef Int -> IO ()
runTextInputCursorTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column defaultLayout (textInput "Name" "")
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
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
      when (labelKind /= UiCursorDefault) $ bump failed
      let
        fieldHover = inp0 {inputMousePos = V2 fx fy}
      _ <- runFrame ctx fieldHover ui
      fieldKind <- uiCursorKind ctx fieldHover
      when (fieldKind /= UiCursorText) $ bump failed
      let
        click =
          fieldHover
            { inputMouseDown = True
            , inputMousePressed = True
            , inputMouseReleased = False
            }
      _ <- runFrame ctx click ui
      clickKind <- uiCursorKind ctx click
      when (clickKind /= UiCursorText) $ bump failed
    _ -> bump failed

runTextInputSelectionTest :: Context -> IORef Int -> IO ()
runTextInputSelectionTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui =
      column defaultLayout $ do
        _ <- button "Other"
        textInput "Name" "hello"
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  let
    tab1 = inp0 {inputKeys = inputKeysFromList [KeyTab]}
  _ <- runFrame ctx tab1 ui
  let
    tab2 = inp0 {inputKeys = inputKeysFromList [KeyTab]}
  _ <- runFrame ctx tab2 ui
  let
    shiftLeft1 =
      inp0
        { inputKeys = inputKeysFromList [KeyLeft]
        , inputModifiers = Modifiers True False False
        }
  _ <- runFrame ctx shiftLeft1 ui
  let
    shiftLeft2 = shiftLeft1 {inputKeys = inputKeysFromList [KeyLeft]}
  _ <- runFrame ctx shiftLeft2 ui
  let
    typed =
      inp0
        { inputChars = "X"
        , inputModifiers = Modifiers False False False
        }
  ((_, valReplace), _, _, _) <- runFrame ctx typed ui
  when (valReplace /= "helX") $ bump failed
  let
    selectAll =
      inp0
        { inputChars = "a"
        , inputModifiers = Modifiers False True False
        }
  _ <- runFrame ctx selectAll ui
  let
    deleteSel = inp0 {inputKeys = inputKeysFromList [KeyBackspace]}
  ((_, valClear), _, _, _) <- runFrame ctx deleteSel ui
  when (valClear /= "") $ bump failed

runTextInputCtrlATest :: Context -> IORef Int -> IO ()
runTextInputCtrlATest ctx failed = do
  term <- newAdaptiveTerminalContext
  let
    inp0 = withInput 320 120
    ui = column defaultLayout (textInput "Name" "hello")
  forM_ [ctx, term] $ \c -> do
    _ <- runFrame c inp0 ui
    let
      tabInp = inp0 {inputKeys = inputKeysFromList [KeyTab]}
    _ <- runFrame c tabInp ui
    let
      selectAll =
        inp0
          { inputChars = "\x01"
          , inputModifiers = Modifiers False True False
          }
    _ <- runFrame c selectAll ui
    let
      deleteSel = inp0 {inputKeys = inputKeysFromList [KeyBackspace]}
    ((_, valClear), _, _, _) <- runFrame c deleteSel ui
    when (valClear /= "") $ bump failed

runTextInputMouseSelectionTest :: Context -> IORef Int -> IO ()
runTextInputMouseSelectionTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy fw fh : _) -> do
      let
        fieldY = fy + fh / 2
        leftX = fx + 1
        rightX = fx + fw - 1
        focusPress =
          inp0
            { inputMousePos = V2 leftX fieldY
            , inputMouseDown = True
            , inputMousePressed = True
            }
      _ <- runFrame ctx focusPress ui
      let
        dragMove =
          inp0
            { inputMousePos = V2 rightX fieldY
            , inputMouseDown = True
            }
      _ <- runFrame ctx dragMove ui
      let
        dragRelease =
          dragMove
            { inputMouseDown = False
            , inputMouseReleased = True
            }
      _ <- runFrame ctx dragRelease ui
      let
        typed = inp0 {inputChars = "z"}
      ((_, val), _, _, _) <- runFrame ctx typed ui
      when (val /= "z") $ bump failed
    _ -> bump failed

runTextInputClickSelectTest :: Context -> IORef Int -> IO ()
runTextInputClickSelectTest _ failed = do
  wordCtx <- newContext
  allCtx <- newContext
  let
    inp0 = withInput 320 120
    wordUi = column defaultLayout (textInput "Name" "hello world")
    allUi = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame wordCtx inp0 wordUi
  _ <- runFrame wordCtx inp0 wordUi
  spans <- collectTextSpans wordCtx
  case [r | (r, txt, _, _, _) <- spans, txt == "hello world"] of
    (Rect fx fy _ fh : _) -> do
      let
        click =
          inp0
            { inputMousePos = V2 (fx + 1) (fy + fh / 2)
            , inputMouseDown = True
            , inputMousePressed = True
            , inputMouseClicks = 2
            }
      _ <- runFrame wordCtx click wordUi
      let
        del = inp0 {inputKeys = inputKeysFromList [KeyBackspace]}
      ((_, val), _, _, _) <- runFrame wordCtx del wordUi
      when (val /= " world") $ bump failed
    _ -> bump failed
  _ <- runFrame allCtx inp0 allUi
  _ <- runFrame allCtx inp0 allUi
  spansAll <- collectTextSpans allCtx
  case [r | (r, txt, _, _, _) <- spansAll, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let
        click =
          inp0
            { inputMousePos = V2 (fx + 1) (fy + fh / 2)
            , inputMouseDown = True
            , inputMousePressed = True
            , inputMouseClicks = 3
            }
      _ <- runFrame allCtx click allUi
      let
        del = inp0 {inputKeys = inputKeysFromList [KeyBackspace]}
      ((_, val), _, _, _) <- runFrame allCtx del allUi
      when (val /= "") $ bump failed
    _ -> bump failed

runTextInputClipboardTest :: Context -> IORef Int -> IO ()
runTextInputClipboardTest ctx failed = do
  clipRef <- newIORef (Nothing :: Maybe T.Text)
  let
    ctx' =
      withClipboard
        ctx
        (readIORef clipRef)
        (\s -> writeIORef clipRef (Just s) >> pure True)
  let
    inp0 = withInput 320 120
    ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx' inp0 ui
  _ <- runFrame ctx' inp0 ui
  let
    tabInp = inp0 {inputKeys = inputKeysFromList [KeyTab]}
  _ <- runFrame ctx' tabInp ui
  let
    selectAll =
      inp0
        { inputChars = "a"
        , inputModifiers = Modifiers False True False
        }
  _ <- runFrame ctx' selectAll ui
  let
    copy =
      inp0
        { inputChars = "c"
        , inputModifiers = Modifiers False True False
        }
  _ <- runFrame ctx' copy ui
  clip <- readIORef clipRef
  when (clip /= Just "hello") $ bump failed
  _ <- runFrame ctx' selectAll ui
  let
    clear = inp0 {inputKeys = inputKeysFromList [KeyBackspace]}
  _ <- runFrame ctx' clear ui
  let
    paste =
      inp0
        { inputChars = "v"
        , inputModifiers = Modifiers False True False
        }
  ((_, val), _, _, _) <- runFrame ctx' paste ui
  when (val /= "hello") $ bump failed

runTextInputMenuTest :: Context -> IORef Int -> IO ()
runTextInputMenuTest ctx failed = do
  clipRef <- newIORef (Just "pasted")
  let
    ctx' =
      withClipboard
        ctx
        (readIORef clipRef)
        (\s -> writeIORef clipRef (Just s) >> pure True)
  let
    inp0 = withInput 320 160
    ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx' inp0 ui
  _ <- runFrame ctx' inp0 ui
  let
    tabInp = inp0 {inputKeys = inputKeysFromList [KeyTab]}
  _ <- runFrame ctx' tabInp ui
  spans <- collectTextSpans ctx'
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let
        fieldClick = V2 (fx + 1) (fy + fh / 2)
        menuOpen =
          inp0
            { inputMousePos = fieldClick
            , inputMouseRightDown = True
            , inputMouseRightPressed = True
            }
      _ <- runFrame ctx' menuOpen ui
      overlays <- collectOverlayTextSpans ctx' menuOpen
      let
        pasteRows = [r | (r, txt, _, _, _) <- overlays, txt == "Paste"]
      case pasteRows of
        (Rect px py pw ph : _) -> do
          let
            pick =
              inp0
                { inputMousePos = V2 (px + pw / 2) (py + ph / 2)
                , inputMouseDown = True
                , inputMousePressed = True
                }
          _ <- runFrame ctx' pick ui
          ((_, val), _, _, _) <- runFrame ctx' inp0 ui
          when (val /= "hellopasted") $ bump failed
        _ -> bump failed
    _ -> bump failed

runTextInputSpanTest :: Context -> IORef Int -> IO ()
runTextInputSpanTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let
    hasHello = spansHas (T.pack "hello") spans
  failWhen failed (not hasHello)

runTextInputFocusSdlTest :: Context -> IORef Int -> IO ()
runTextInputFocusSdlTest ctx failed = do
  let
    inp0 = withInput 320 120
    ui = column defaultLayout (textInput "Name" "")
  (resp, _) <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  let
    fieldPos =
      [ (rectX r + rectW r / 2, rectY r + 0.5)
      | (r, txt, _, _, _) <- spans
      , "Enter" `T.isInfixOf` txt
      ]
  case fieldPos of
    [(fx, fy)] -> do
      let
        click = V2 fx fy
        inp1 =
          inp0
            { inputMousePos = click
            , inputMouseDown = True
            , inputMousePressed = True
            , inputMouseReleased = False
            }
      _ <- runFrame ctx inp1 ui
      focus <- getFocusId ctx
      when (focus /= respId resp) $ bump failed
      spans' <- collectTextSpans ctx
      let
        hasLabel = any (\(_, txt, _, _, _) -> txt == "Name") spans'
      failWhen failed (not hasLabel)
    _ -> bump failed

runButtonHoverAnimTest :: Context -> IORef Int -> IO ()
runButtonHoverAnimTest ctx failed = do
  let
    inp0 = withDelta 200 100 0.016
    ui = column defaultLayout (button "Hover")
  _ <- runFrame ctx inp0 ui
  let
    inp1 = inp0 {inputMousePos = V2 10 10}
  vals <- replicateM 5 $ do
    _ <- runFrame ctx inp1 ui
    hot <- getHotId ctx
    getAnimationValue ctx hot
  let
    decreases =
      case vals of
        [] -> False
        _ -> any (uncurry (\a b -> b + 0.001 < a)) (zip vals (drop 1 vals))
  when decreases $ bump failed
  when (last vals < 0.4) $ bump failed

runButtonPressReleaseHoverTest :: Context -> IORef Int -> IO ()
runButtonPressReleaseHoverTest ctx failed = do
  let
    inp0 = withDelta 200 100 0.016
    ui = column defaultLayout (button "Hover")
  _ <- runFrame ctx inp0 ui
  let
    click = V2 10 10
    inpPress =
      inp0
        { inputMousePos = click
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx inpPress ui
  let
    inpRelease =
      inpPress
        { inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx inpRelease ui
  hot <- getHotId ctx
  val <- getAnimationValue ctx hot
  when (hashWidgetId hot == 0) $ bump failed
  when (val < 0.99) $ bump failed

runTextInputFocusTest :: Context -> IORef Int -> IO ()
runTextInputFocusTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    inp0 = withInput 200 100
    ui = column defaultLayout (textInput "Name" "")
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry _ _ = respRect resp
    click = V2 (rx + 1) (ry + 0.5)
  let
    inp1 =
      inp0
        { inputMousePos = click
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  (_, _, _, _) <- runFrame ctx inp1 ui
  spans <- collectTextSpans ctx
  let
    hasCursor = any (\(_, txt, _, _, _) -> T.isInfixOf "\x2502" txt) spans
  failWhen failed (not hasCursor)

runTextInputDirtyTest :: Context -> IORef Int -> IO ()
runTextInputDirtyTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    ui = column defaultLayout (textInput "Name" "")
    inp0 = (withInput 200 100) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry _ _ = respRect resp
    click = V2 (rx + 1) (ry + 0.5)
    press =
      inp0
        { inputMousePos = click
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx press ui
  let
    release =
      press
        { inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx release ui
  let
    idle = release {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  needFocus <- needsRedraw ctx idle idle
  failWhen failed (not needFocus)
  let
    typed = idle {inputChars = "ab"}
  _ <- runFrame ctx typed ui
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed



