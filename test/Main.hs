module Main (main) where

import Control.Monad (forM_, replicateM, void, when)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import Data.List (isInfixOf, nub)
import NanoUI
import NanoUI.Term.Ansi (frameBytes)
import NanoUI.Term.Cells (cellRows, narrowChar, rasterize)
import NanoUI.Term.Event (MouseAction (..), MouseBtn (..), TermEvent (..), noMods)
import NanoUI.Term.Vt (decode, flushPending)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

main :: IO ()
main = do
  failed <- newIORef 0
  ctx <- newContext
  sdlCtx <- newSdlContext

  let run name test = do
        before <- readIORef failed
        test ctx failed
        after <- readIORef failed
        when (after > before) $ putStrLn ("FAIL: " ++ name)

  let runSdl name test = do
        before <- readIORef failed
        test sdlCtx failed
        after <- readIORef failed
        when (after > before) $ putStrLn ("FAIL: " ++ name)

  run "id-stability" runIdStabilityTest
  run "id-uniqueness" runIdUniquenessTest
  run "fit-sizing" runFitSizingTest
  run "with-key" runWithKeyTest
  run "layout" runLayoutTest
  run "draw" runDrawTest
  run "overlay" runOverlayTest
  run "interaction" runInteractionTest
  run "hover" runHoverTest
  run "pointer-cursor" runPointerCursorTest
  run "pointer-cursor-checkbox" runPointerCursorCheckboxTest
  run "text-input-cursor" runTextInputCursorTest
  run "text-input-selection" runTextInputSelectionTest
  run "text-input-ctrl-a" runTextInputCtrlATest
  run "text-input-mouse-selection" runTextInputMouseSelectionTest
  run "text-input-click-select" runTextInputClickSelectTest
  run "modal-overlay" runModalOverlayTest
  run "modal-no-phantom-scroll" runModalNoPhantomScrollTest
  run "image" runImageTest
  run "text-input-clipboard" runTextInputClipboardTest
  run "text-input-menu" runTextInputMenuTest
  run "select-dropdown-cursor" runSelectDropdownCursorTest
  run "slider-cursor" runSliderCursorTest
  run "scroll-thumb-cursor" runScrollThumbCursorTest
  runSdl "text-input-span" runTextInputSpanTest
  runSdl "text-input-focus-sdl" runTextInputFocusSdlTest
  run "button-hover-anim" runButtonHoverAnimTest
  run "button-press-release-hover" runButtonPressReleaseHoverTest
  run "text-input-focus" runTextInputFocusTest
  run "idle" runIdleTest
  run "animation-idle" runAnimationIdleTest
  run "ascii" runAsciiTest
  run "vt-decode" runVtTest
  run "cells-and-diff" runCellsTest
  run "checkbox-toggle" runCheckboxTest
  run "slider-store" runSliderTest
  run "scroll-wheel" runScrollTest
  run "nested-scroll" runNestedScrollTest
  run "nested-scroll-focus" runNestedScrollFocusTest
  run "scroll-hover-clip" runScrollHoverClipTest
  run "scroll-hit-offset" runScrollHitOffsetTest
  run "tab-focus" runTabFocusTest
  run "select-initial" runSelectTest
  run "select-dropdown" runSelectDropdownTest
  run "select-dropdown-hover" runSelectDropdownHoverTest
  run "select-pick-low" runSelectPickLowTest
  run "select-keyboard" runSelectKeyboardTest
  run "text-wrap" runTextWrapTest
  run "text-wrap-width" runTextWrapAssignedTest
  run "flex-wrap" runFlexWrapTest
  run "flex-shrink" runFlexShrinkTest
  run "grow-fits-window" runGrowFitsWindowTest
  run "grow-wrap-sibling" runGrowWrapPushesSiblingTest
  run "scroll-bar-gutter" runScrollBarGutterTest
  run "use-flag-click" runUseFlagClickTest
  run "panel-paints" runPanelPaintsTest
  run "separator-span" runSeparatorSpanTest
  run "header-top-pad" runHeaderTopPadTest
  run "fit-header-no-shrink" runFitHeaderNoShrinkTest
  run "window-overlay" runWindowOverlayTest
  run "window-drag" runWindowDragTest

  n <- readIORef failed
  if n == 0
    then putStrLn "All tests passed."
    else do
      putStrLn $ show n ++ " test(s) failed."
      fail "tests failed"

bump :: IORef Int -> IO ()
bump r = modifyIORef r (+ 1)

findGrabHover :: Context -> UI a -> Input -> Float -> [Float] -> IO (Maybe Input)
findGrabHover ctx ui inp0 thumbX = go
  where
    go [] = pure Nothing
    go (y : ys) = do
      let hover = inp0 {inputMousePos = V2 thumbX y}
      _ <- runFrame ctx hover ui
      kind <- uiCursorKind ctx hover
      if kind == UiCursorGrab then pure (Just hover) else go ys

modifyIORef :: IORef Int -> (Int -> Int) -> IO ()
modifyIORef r f = readIORef r >>= writeIORef r . f

runIdStabilityTest :: Context -> IORef Int -> IO ()
runIdStabilityTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  (ids, _, _, _) <-
    runFrame
      ctx
      inp
      (column defaultLayout (replicateM 3 currentId))
  case ids of
    [a, b, c] -> when (a /= b || b /= c) $ bump failed
    _ -> bump failed

-- Widgets at different call sites must not collapse onto one id, or they all
-- share hover, focus and stored state.
runIdUniquenessTest :: Context -> IORef Int -> IO ()
runIdUniquenessTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  (ids, _, _, _) <-
    runFrame
      ctx
      inp
      ( column defaultLayout $ do
          a <- currentId
          b <- currentId
          c <- currentId
          pure [a, b, c]
      )
  case ids of
    [a, b, c] -> when (a == b || b == c || a == c) $ bump failed
    _ -> bump failed

-- Fit sizing must follow intrinsic content size, not the available width.
runFitSizingTest :: Context -> IORef Int -> IO ()
runFitSizingTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 400 100}
      -- respRect reports the previous frame's rect, so each case needs two runs.
      measure ui = do
        _ <- runFrame ctx inp ui
        (resp, _, _, _) <- runFrame ctx inp ui
        pure (rectW (respRect resp))
  w1 <- measure (column defaultLayout (label "hi"))
  w2 <- measure (column defaultLayout (label "a much longer label"))
  when (w1 <= 0 || w1 >= 400) $ bump failed
  when (w2 <= w1) $ bump failed

runWithKeyTest :: Context -> IORef Int -> IO ()
runWithKeyTest ctx _failed = do
  let inp = emptyInput {inputWindowSize = Size 200 200}
  (_, _, _, _) <-
    runFrame
      ctx
      inp
      ( withKey (0 :: Int) $ do
          _ <- button "A"
          withKey (1 :: Int) $ button "A"
      )
  pure ()

runLayoutTest :: Context -> IORef Int -> IO ()
runLayoutTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 400 300}
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      ( column
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutGap = 8})
          ( do
              _ <-
                row
                  (defaultLayout {layoutWidth = Grow 1})
                  ( do
                      _ <- spacer (Grow 1) Fit
                      label "grow test"
                  )
              label "nested"
          )
      )
  when (drawVertexCount draw <= 0) $ bump failed

runDrawTest :: Context -> IORef Int -> IO ()
runDrawTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100}
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      (column defaultLayout (label "draw"))
  when (drawIndexCount draw < 6) $ bump failed
  when (null (drawCommands draw)) $ bump failed

-- Widget identity is the call site, so every frame must run the same `ui`
-- binding; re-typing the widget on another line makes a different widget.
runOverlayTest :: Context -> IORef Int -> IO ()
runOverlayTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 80}
      ui = column defaultLayout (button "Hover" >>= tooltip "tip")
  (_, _, _, _) <- runFrame ctx inp0 ui
  let inp1 =
        inp0
          { inputMousePos = V2 10 10
          , inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = False
          }
  (_, _, draw, _) <- runFrame ctx inp1 ui
  let hasOverlay = any ((== LayerOverlay) . cmdLayer) (drawCommands draw)
  when (not hasOverlay) $ bump failed

runInteractionTest :: Context -> IORef Int -> IO ()
runInteractionTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (button "Click")
  -- Frame 1: layout, store prev rects
  _ <- runFrame ctx inp0 ui
  -- Frame 2: press on button
  let inpPress =
        inp0
          { inputMousePos = V2 10 10
          , inputMousePressed = True
          , inputMouseDown = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx inpPress ui
  -- Frame 3: release => click
  let inpRelease =
        inpPress
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  (_, msgs, _, _) <- runFrame ctx inpRelease ui
  when (null msgs) $ bump failed

-- Hover uses solved layout rects after the first frame stores prev positions.
runHoverTest :: Context -> IORef Int -> IO ()
runHoverTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (button "Hover")
  _ <- runFrame ctx inp0 ui
  let inp1 =
        inp0
          { inputMousePos = V2 10 10
          , inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = False
          }
  _ <- runFrame ctx inp1 ui
  hot <- getHotId ctx
  when (hashWidgetId hot == 0) $ bump failed

runPointerCursorTest :: Context -> IORef Int -> IO ()
runPointerCursorTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (button "Click")
  _ <- runFrame ctx inp0 ui
  let inp1 = inp0 {inputMousePos = V2 10 10}
  _ <- runFrame ctx inp1 ui
  want <- pointerCursorWanted ctx inp1
  when (not want) $ bump failed
  let inp2 = inp0 {inputMousePos = V2 (-1) (-1)}
  _ <- runFrame ctx inp2 ui
  want2 <- pointerCursorWanted ctx inp2
  when want2 $ bump failed

runPointerCursorCheckboxTest :: Context -> IORef Int -> IO ()
runPointerCursorCheckboxTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (checkbox "Feature" False)
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      hover = inp0 {inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)}
  _ <- runFrame ctx hover ui
  want <- pointerCursorWanted ctx hover
  when (not want) $ bump failed
  let click =
        hover
          { inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx click ui
  wantClick <- pointerCursorWanted ctx click
  when (not wantClick) $ bump failed

runTextInputCursorTest :: Context -> IORef Int -> IO ()
runTextInputCursorTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      ui = column defaultLayout (textInput "Name" "")
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let labelPos =
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
      let labelHover = inp0 {inputMousePos = V2 lx ly}
      _ <- runFrame ctx labelHover ui
      labelKind <- uiCursorKind ctx labelHover
      when (labelKind /= UiCursorDefault) $ bump failed
      let fieldHover = inp0 {inputMousePos = V2 fx fy}
      _ <- runFrame ctx fieldHover ui
      fieldKind <- uiCursorKind ctx fieldHover
      when (fieldKind /= UiCursorText) $ bump failed
      let click =
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
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      ui =
        column defaultLayout $ do
          _ <- button "Other"
          textInput "Name" "hello"
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  let tab1 = inp0 {inputKeys = [KeyTab]}
  _ <- runFrame ctx tab1 ui
  let tab2 = inp0 {inputKeys = [KeyTab]}
  _ <- runFrame ctx tab2 ui
  let shiftLeft1 =
        inp0
          { inputKeys = [KeyLeft]
          , inputModifiers = Modifiers True False False
          }
  _ <- runFrame ctx shiftLeft1 ui
  let shiftLeft2 = shiftLeft1 {inputKeys = [KeyLeft]}
  _ <- runFrame ctx shiftLeft2 ui
  let typed =
        inp0
          { inputChars = ['X']
          , inputModifiers = Modifiers False False False
          }
  ((_, valReplace), _, _, _) <- runFrame ctx typed ui
  when (valReplace /= "helX") $ bump failed
  let selectAll =
        inp0
          { inputChars = ['a']
          , inputModifiers = Modifiers False True False
          }
  _ <- runFrame ctx selectAll ui
  let deleteSel = inp0 {inputKeys = [KeyBackspace]}
  ((_, valClear), _, _, _) <- runFrame ctx deleteSel ui
  when (valClear /= "") $ bump failed

runTextInputCtrlATest :: Context -> IORef Int -> IO ()
runTextInputCtrlATest ctx failed = do
  term <- newTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      ui = column defaultLayout (textInput "Name" "hello")
  forM_ [ctx, term] $ \c -> do
    _ <- runFrame c inp0 ui
    let tab = inp0 {inputKeys = [KeyTab]}
    _ <- runFrame c tab ui
    let selectAll =
          inp0
            { inputChars = ['\x01']
            , inputModifiers = Modifiers False True False
            }
    _ <- runFrame c selectAll ui
    let deleteSel = inp0 {inputKeys = [KeyBackspace]}
    ((_, valClear), _, _, _) <- runFrame c deleteSel ui
    when (valClear /= "") $ bump failed

runTextInputMouseSelectionTest :: Context -> IORef Int -> IO ()
runTextInputMouseSelectionTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy fw fh : _) -> do
      let fieldY = fy + fh / 2
          leftX = fx + 1
          rightX = fx + fw - 1
          focusPress =
            inp0
              { inputMousePos = V2 leftX fieldY
              , inputMouseDown = True
              , inputMousePressed = True
              }
      _ <- runFrame ctx focusPress ui
      let dragMove =
            inp0
              { inputMousePos = V2 rightX fieldY
              , inputMouseDown = True
              }
      _ <- runFrame ctx dragMove ui
      let dragRelease =
            dragMove
              { inputMouseDown = False
              , inputMouseReleased = True
              }
      _ <- runFrame ctx dragRelease ui
      let typed = inp0 {inputChars = ['z']}
      ((_, val), _, _, _) <- runFrame ctx typed ui
      when (val /= "z") $ bump failed
    _ -> bump failed

runTextInputClickSelectTest :: Context -> IORef Int -> IO ()
runTextInputClickSelectTest _ failed = do
  wordCtx <- newContext
  allCtx <- newContext
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      wordUi = column defaultLayout (textInput "Name" "hello world")
      allUi = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame wordCtx inp0 wordUi
  _ <- runFrame wordCtx inp0 wordUi
  spans <- collectTextSpans wordCtx
  case [r | (r, txt, _, _, _) <- spans, txt == "hello world"] of
    (Rect fx fy _ fh : _) -> do
      let click =
            inp0
              { inputMousePos = V2 (fx + 1) (fy + fh / 2)
              , inputMouseDown = True
              , inputMousePressed = True
              , inputMouseClicks = 2
              }
      _ <- runFrame wordCtx click wordUi
      let del = inp0 {inputKeys = [KeyBackspace]}
      ((_, val), _, _, _) <- runFrame wordCtx del wordUi
      when (val /= " world") $ bump failed
    _ -> bump failed
  _ <- runFrame allCtx inp0 allUi
  _ <- runFrame allCtx inp0 allUi
  spansAll <- collectTextSpans allCtx
  case [r | (r, txt, _, _, _) <- spansAll, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let click =
            inp0
              { inputMousePos = V2 (fx + 1) (fy + fh / 2)
              , inputMouseDown = True
              , inputMousePressed = True
              , inputMouseClicks = 3
              }
      _ <- runFrame allCtx click allUi
      let del = inp0 {inputKeys = [KeyBackspace]}
      ((_, val), _, _, _) <- runFrame allCtx del allUi
      when (val /= "") $ bump failed
    _ -> bump failed

runModalOverlayTest :: Context -> IORef Int -> IO ()
runModalOverlayTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 200}
      ui =
        column defaultLayout $ do
          outside <- button "Outside"
          (dlg, mInside) <-
            modal True "Title" $ do
              button "Inside"
          pure (outside, dlg, mInside)
      closedUi =
        column defaultLayout $ do
          _ <- button "Outside"
          (dlg, mInside) <-
            modal False "Title" $ do
              button "Inside"
          pure (dlg, mInside)
  do
    ((dlg, mInside), _, _, _) <- runFrame ctx inp0 closedUi
    when (respClicked dlg) $ bump failed
    case mInside of
      Nothing -> pure ()
      Just _ -> bump failed
    closedSpans <- collectOverlayTextSpans ctx inp0
    let closedTitle = any (\(_, txt, _, _, _) -> "Title" `T.isInfixOf` txt) closedSpans
    when closedTitle $ bump failed
  _ <- runFrame ctx inp0 ui
  ((outside0, _, mInside0), _, _, _) <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  let hasTitle = any (\(_, txt, _, _, _) -> "Title" `T.isInfixOf` txt) overlays
      hasInside = any (\(_, txt, _, _, _) -> "Inside" `T.isInfixOf` txt) overlays
  when (not (hasTitle && hasInside)) $ bump failed
  case mInside0 of
    Just inside -> do
      let Rect ix iy iw ih = respRect inside
          clickIn =
            inp0
              { inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)
              , inputMouseDown = True
              , inputMousePressed = True
              }
      _ <- runFrame ctx clickIn ui
      let releaseIn =
            clickIn
              { inputMouseDown = False
              , inputMousePressed = False
              , inputMouseReleased = True
              }
      ((_, _, mClicked), _, _, _) <- runFrame ctx releaseIn ui
      case mClicked of
        Just r -> when (not (respClicked r)) $ bump failed
        Nothing -> bump failed
      let Rect ox oy ow oh = respRect outside0
          clickOut =
            inp0
              { inputMousePos = V2 (ox + ow / 2) (oy + oh / 2)
              , inputMouseDown = True
              , inputMousePressed = True
              }
      ((outsideHit, _, _), _, _, _) <- runFrame ctx clickOut ui
      when (respClicked outsideHit) $ bump failed
      let backdrop =
            inp0
              { inputMousePos = V2 4 4
              , inputMouseDown = True
              , inputMousePressed = True
              }
      ((_, dlg, _), _, _, _) <- runFrame ctx backdrop ui
      when (not (respClicked dlg)) $ bump failed
      let esc = inp0 {inputKeys = [KeyEscape]}
      ((_, dlgEsc, _), _, _, _) <- runFrame ctx esc ui
      when (not (respClicked dlgEsc)) $ bump failed
      consumed <- overlayConsumesQuit ctx esc
      when (not consumed) $ bump failed
      _ <- runFrame ctx esc closedUi
      leftover <- overlayConsumesQuit ctx esc
      when leftover $ bump failed
    Nothing -> bump failed
  let tallUi =
        modal True "Tall" $ do
          mapM_ (\i -> label (T.pack ("Row " <> show (i :: Int)))) [1 .. 40]
          button "Close"
  _ <- runFrame ctx inp0 tallUi
  ((dlgTall, _), _, _, _) <- runFrame ctx inp0 tallUi
  let Rect _ _ _ mh = respRect dlgTall
  when (mh > 200) $ bump failed

-- A short padded modal must not treat its own padding as overflow.
runModalNoPhantomScrollTest :: Context -> IORef Int -> IO ()
runModalNoPhantomScrollTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 400 300}
      ui =
        modal True "About" $ do
          _ <- label "Immediate-mode GUI for Haskell."
          row
            (defaultLayout {layoutWidth = Grow 1})
            $ do
              _ <- spacer (Grow 1) Fit
              _ <- button "Close"
              pure ()
  _ <- runFrame ctx inp0 ui
  ((dlg, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect mx my mw mh = respRect dlg
  when (mw <= 0 || mh <= 0) $ bump failed
  off0 <- getScrollOffset ctx (respId dlg)
  let wheel =
        inp0
          { inputMousePos = V2 (mx + mw / 2) (my + mh / 2)
          , inputScroll = V2 0 1
          }
  _ <- runFrame ctx wheel ui
  off1 <- getScrollOffset ctx (respId dlg)
  when (off0 /= 0 || off1 /= 0) $ bump failed

runImageTest :: Context -> IORef Int -> IO ()
runImageTest ctx failed = do
  let px a b c = BS.pack (concat (replicate 16 [a, b, c, 255]))
  ok1 <- registerImage ctx (ImageId 1) 4 4 (px 255 0 0)
  ok7 <- registerImage ctx (ImageId 7) 4 4 (px 0 0 255)
  when (not (ok1 && ok7)) $ bump failed
  let inp0 = emptyInput {inputWindowSize = Size 320 200}
      ui =
        row defaultLayout $ do
          _ <-
            image
              ( defaultLayout
                  { layoutWidth = Fixed 40
                  , layoutHeight = Fixed 24
                  }
              )
              (ImageId 1)
          image
            ( defaultLayout
                { layoutWidth = Fixed 40
                , layoutHeight = Fixed 24
                }
            )
            (ImageId 7)
  _ <- runFrame ctx inp0 ui
  (resp, _, drawData, _) <- runFrame ctx inp0 ui
  let Rect _ _ w h = respRect resp
  when (abs (w - 40) > 0.5 || abs (h - 24) > 0.5) $ bump failed
  let texCmds = filter (\c -> cmdTextureId c == atlasTextureId) (drawCommands drawData)
  when (length texCmds /= 1) $ bump failed
  when (not (any (\c -> cmdIndexCount c == 12) texCmds)) $ bump failed
  (u0, _) <- vertUv drawData 0
  (u4, _) <- vertUv drawData 4
  when (abs (u0 - u4) < 1e-6) $ bump failed
  let missing =
        image
          ( defaultLayout
              { layoutWidth = Fixed 40
              , layoutHeight = Fixed 24
              }
          )
          (ImageId 0)
  _ <- runFrame ctx inp0 missing
  (_, _, missingData, _) <- runFrame ctx inp0 missing
  when (any (\c -> cmdTextureId c > 0) (drawCommands missingData)) $ bump failed

vertUv :: DrawData -> Int -> IO (Float, Float)
vertUv dd i =
  withForeignPtr (drawVertices dd) $ \p -> do
    let off = i * vertexSize
    u <- peekByteOff p (off + 8)
    v <- peekByteOff p (off + 12)
    pure (u, v)

runTextInputClipboardTest :: Context -> IORef Int -> IO ()
runTextInputClipboardTest ctx failed = do
  clipRef <- newIORef (Nothing :: Maybe String)
  let ctx' =
        withClipboard
          ctx
          (readIORef clipRef)
          (\s -> writeIORef clipRef (Just s) >> pure True)
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx' inp0 ui
  _ <- runFrame ctx' inp0 ui
  let tab = inp0 {inputKeys = [KeyTab]}
  _ <- runFrame ctx' tab ui
  let selectAll =
        inp0
          { inputChars = ['a']
          , inputModifiers = Modifiers False True False
          }
  _ <- runFrame ctx' selectAll ui
  let copy =
        inp0
          { inputChars = ['c']
          , inputModifiers = Modifiers False True False
          }
  _ <- runFrame ctx' copy ui
  clip <- readIORef clipRef
  when (clip /= Just "hello") $ bump failed
  _ <- runFrame ctx' selectAll ui
  let clear = inp0 {inputKeys = [KeyBackspace]}
  _ <- runFrame ctx' clear ui
  let paste =
        inp0
          { inputChars = ['v']
          , inputModifiers = Modifiers False True False
          }
  ((_, val), _, _, _) <- runFrame ctx' paste ui
  when (val /= "hello") $ bump failed

runTextInputMenuTest :: Context -> IORef Int -> IO ()
runTextInputMenuTest ctx failed = do
  clipRef <- newIORef (Just "pasted")
  let ctx' =
        withClipboard
          ctx
          (readIORef clipRef)
          (\s -> writeIORef clipRef (Just s) >> pure True)
  let inp0 = emptyInput {inputWindowSize = Size 320 160}
      ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx' inp0 ui
  _ <- runFrame ctx' inp0 ui
  let tab = inp0 {inputKeys = [KeyTab]}
  _ <- runFrame ctx' tab ui
  spans <- collectTextSpans ctx'
  case [r | (r, txt, _, _, _) <- spans, txt == "hello"] of
    (Rect fx fy _ fh : _) -> do
      let fieldClick = V2 (fx + 1) (fy + fh / 2)
          menuOpen =
            inp0
              { inputMousePos = fieldClick
              , inputMouseRightDown = True
              , inputMouseRightPressed = True
              }
      _ <- runFrame ctx' menuOpen ui
      overlays <- collectOverlayTextSpans ctx' menuOpen
      let pasteRows = [r | (r, txt, _, _, _) <- overlays, txt == "Paste"]
      case pasteRows of
        (Rect px py pw ph : _) -> do
          let pick =
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

runSelectDropdownCursorTest :: Context -> IORef Int -> IO ()
runSelectDropdownCursorTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 200}
      ui = column defaultLayout (select "Quality" ["Low", "High"] 0)
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect sx sy sw sh = respRect resp
      btn = V2 (sx + sw / 2) (sy + sh / 2)
      openPress =
        inp0
          { inputMousePos = btn
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx openPress ui
  let openRelease =
        openPress
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx openRelease ui
  overlaysOpen <- collectOverlayTextSpans ctx openRelease
  let lowYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt]
  case lowYs of
    (lowY : _) -> do
      let hover =
            inp0
              { inputMousePos = V2 (sx + sw / 2) (lowY + 0.5)
              , inputMouseReleased = False
              , inputMousePressed = False
              , inputMouseDown = False
              }
      _ <- runFrame ctx hover ui
      kind <- uiCursorKind ctx hover
      when (kind /= UiCursorPointer) $ bump failed
      let press =
            hover
              { inputMouseDown = True
              , inputMousePressed = True
              }
      _ <- runFrame ctx press ui
      pressKind <- uiCursorKind ctx press
      when (pressKind /= UiCursorPointer) $ bump failed
    _ -> bump failed

runSliderCursorTest :: Context -> IORef Int -> IO ()
runSliderCursorTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 300 80}
      ui = column defaultLayout (slider "Volume" 0 100 50)
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      trackRect = sliderTrackRect rx ry rw rh
      track = V2 (rx + rw / 2) (rectY trackRect + rectH trackRect / 2)
      labelPos = V2 (rx + 4) (ry + 4)
  let hoverTrack = inp0 {inputMousePos = track}
  _ <- runFrame ctx hoverTrack ui
  hoverKind <- uiCursorKind ctx hoverTrack
  when (hoverKind /= UiCursorGrab) $ bump failed
  let pressTrack =
        hoverTrack
          { inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx pressTrack ui
  grabbing <- cursorKindIs ctx pressTrack UiCursorGrabbing
  when (not grabbing) $ bump failed
  let dragOff =
        pressTrack
          { inputMousePos = labelPos
          }
  _ <- runFrame ctx dragOff ui
  grabbingOff <- cursorKindIs ctx dragOff UiCursorGrabbing
  when (not grabbingOff) $ bump failed
  let hoverLabel = inp0 {inputMousePos = labelPos}
  _ <- runFrame ctx hoverLabel ui
  labelKind <- uiCursorKind ctx hoverLabel
  when (labelKind /= UiCursorDefault) $ bump failed

runScrollThumbCursorTest :: Context -> IORef Int -> IO ()
runScrollThumbCursorTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 120}
      ui =
        scrollArea
          (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
          ( column defaultLayout $ do
              _ <- replicateM 8 (label "scroll line")
              pure ()
          )
  _ <- runFrame ctx inp0 ui
  ((sid, ()), _, _, _) <- runFrame ctx inp0 ui
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> bump failed
    Just (Rect rx ry rw rh) -> do
      let barW = 8
          barMargin = 3
          thumbX = rx + rw - barW - barMargin + barW / 2
          tryYs = [ry + rh * n / 8 | n <- [1 .. 7]]
      mHover <- findGrabHover ctx ui inp0 thumbX tryYs
      case mHover of
        Nothing -> bump failed
        Just hover -> do
          kind <- uiCursorKind ctx hover
          when (kind /= UiCursorGrab) $ bump failed
          let press =
                hover
                  { inputMouseDown = True
                  , inputMousePressed = True
                  }
          _ <- runFrame ctx press ui
          grabbing <- cursorKindIs ctx press UiCursorGrabbing
          when (not grabbing) $ bump failed

-- Overflowing vertical scroll reserves a right gutter so children do not sit under the bar.
runScrollBarGutterTest :: Context -> IORef Int -> IO ()
runScrollBarGutterTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 120}
      ui = do
        (sid, child) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60})
            ( column (defaultLayout {layoutWidth = Grow 1}) $ do
                r <- button "Wide"
                _ <- replicateM 8 (label "scroll line")
                pure r
            )
        pure (sid, child)
  _ <- runFrame ctx inp0 ui
  ((sid, child), _, _, _) <- runFrame ctx inp0 ui
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> bump failed
    Just (Rect sx _ sw _) -> do
      let Rect cx _ cw _ = respRect child
          gutter = 8 + 3
      when (cx + cw > sx + sw - gutter + 0.01) $ bump failed

runTextInputSpanTest :: Context -> IORef Int -> IO ()
runTextInputSpanTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let hasHello = any (\(_, txt, _, _, _) -> "hello" `T.isInfixOf` txt) spans
  when (not hasHello) $ bump failed

runTextInputFocusSdlTest :: Context -> IORef Int -> IO ()
runTextInputFocusSdlTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 120}
      ui = column defaultLayout (textInput "Name" "")
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let fieldPos =
        [ (rectX r + rectW r / 2, rectY r + 0.5)
        | (r, txt, _, _, _) <- spans
        , "Enter" `T.isInfixOf` txt
        ]
  case fieldPos of
    [(fx, fy)] -> do
      let click = V2 fx fy
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
      let hasLabel = any (\(_, txt, _, _, _) -> txt == "Name") spans'
      when (not hasLabel) $ bump failed
    _ -> bump failed

-- Hover animation must advance frame-to-frame without restarting at zero.
runButtonHoverAnimTest :: Context -> IORef Int -> IO ()
runButtonHoverAnimTest ctx failed = do
  let inp0 =
        emptyInput
          { inputWindowSize = Size 200 100
          , inputDeltaTime = 0.016
          }
      ui = column defaultLayout (button "Hover")
  _ <- runFrame ctx inp0 ui
  let inp1 = inp0 {inputMousePos = V2 10 10}
  vals <- replicateM 5 $ do
    _ <- runFrame ctx inp1 ui
    hot <- getHotId ctx
    getAnimationValue ctx hot
  let decreases =
        case vals of
          [] -> False
          _ -> any (uncurry (\a b -> b + 0.001 < a)) (zip vals (drop 1 vals))
  when decreases $ bump failed
  when (last vals < 0.4) $ bump failed

-- Release over a button while still hovered should land at full hover, not flash base.
runButtonPressReleaseHoverTest :: Context -> IORef Int -> IO ()
runButtonPressReleaseHoverTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100, inputDeltaTime = 0.016}
      ui = column defaultLayout (button "Hover")
  _ <- runFrame ctx inp0 ui
  let click = V2 10 10
      inpPress =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx inpPress ui
  let inpRelease =
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

-- Text input focus is finalized against solved rects on first press.
runTextInputFocusTest :: Context -> IORef Int -> IO ()
runTextInputFocusTest _ failed = do
  ctx <- newTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (textInput "Name" "")
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      click = V2 (rx + 1) (ry + 0.5)
  let inp1 =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  (_, _, _, _) <- runFrame ctx inp1 ui
  spans <- collectTextSpans ctx
  let hasCursor = any (\(_, txt, _, _, _) -> T.isInfixOf "\x2502" txt) spans
  when (not hasCursor) $ bump failed

runIdleTest :: Context -> IORef Int -> IO ()
runIdleTest _ failed = do
  ctx <- newContext
  let inp =
        emptyInput
          { inputWindowSize = Size 100 100
          , inputMousePos = V2 (-1) (-1)
          }
  _ <- runFrame ctx inp (label "idle")
  need <- needsRedraw ctx inp inp
  when need $ bump failed

runAnimationIdleTest :: Context -> IORef Int -> IO ()
runAnimationIdleTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.05}
  _ <- runFrame ctx inp (label "anim")
  startAnimation ctx (WidgetId 42) 0 1 0.5
  need <- needsRedraw ctx inp inp
  when (not need) $ bump failed

runAsciiTest :: Context -> IORef Int -> IO ()
runAsciiTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 40 10}
  (_, _, draw, _) <-
    runFrame
      ctx
      inp
      (column (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "snap"))
  let ascii = renderASCII 40 10 draw
  when (length ascii /= 10) $ bump failed
  when (all (all (== ' ')) ascii) $ bump failed

-- The terminal backend owns its input parsing because vty could not report
-- hover at all, and discarded pending bytes when it met a report it did not
-- recognise, which swallowed clicks. Both properties are pinned here.
runVtTest :: Context -> IORef Int -> IO ()
runVtTest _ failed = do
  let ck cond = when (not cond) (bump failed)
      evs s = fst (decode (BS8.pack s))
      leftover s = snd (decode (BS8.pack s))
  -- Bare motion with no button held: hover. Coordinates are one-based on the
  -- wire and zero-based in the event.
  ck (evs "\ESC[<35;10;5M" == [EvMouse MouseMove 9 4 noMods])
  -- The regression: a click arriving in the same read as a motion report must
  -- still be delivered.
  ck
    ( evs "\ESC[<35;10;5M\ESC[<0;12;6M"
        == [ EvMouse MouseMove 9 4 noMods
           , EvMouse (MousePress BtnLeft) 11 5 noMods
           ]
    )
  -- An unrecognised sequence consumes only itself.
  ck (evs "\ESC[?1;2p\ESC[<0;1;1M" == [EvMouse (MousePress BtnLeft) 0 0 noMods])
  ck (evs "\ESC[<0;12;6m" == [EvMouse (MouseRelease (Just BtnLeft)) 11 5 noMods])
  ck (evs "\ESC[<32;3;4M" == [EvMouse (MouseDrag BtnLeft) 2 3 noMods])
  ck (evs "\ESC[<64;3;4M" == [EvMouse MouseScrollUp 2 3 noMods])
  ck (evs "\ESC[<65;3;4M" == [EvMouse MouseScrollDown 2 3 noMods])
  ck (evs "\ESC[<66;3;4M" == [])
  ck (evs "\ESC[<67;3;4M" == [])
  -- X10 fallback, for terminals that ignore the SGR request.
  ck (evs "\ESC[MC*%" == [EvMouse MouseMove 9 4 noMods])
  ck (evs "\ESC[M *%" == [EvMouse (MousePress BtnLeft) 9 4 noMods])
  ck (evs "\ESC[M#*%" == [EvMouse (MouseRelease Nothing) 9 4 noMods])
  -- A sequence split across reads is held whole, never half-read.
  ck (evs "\ESC[<35;10" == [])
  ck (leftover "\ESC[<35;10" == BS8.pack "\ESC[<35;10")
  -- A lone ESC stays ambiguous until input goes idle.
  ck (evs "\ESC" == [])
  ck (leftover "\ESC" == BS8.pack "\ESC")
  ck (flushPending (BS8.pack "\ESC") == [EvKey KeyEscape noMods])
  ck (evs "\ESC[A" == [EvKey KeyUp noMods])
  ck (evs "\ESCOB" == [EvKey KeyDown noMods])
  ck (evs "\ESC[3~" == [EvKey KeyDelete noMods])
  ck (evs "\ESC[H" == [EvKey KeyHome noMods])
  ck (evs "hi" == [EvChar 'h' noMods, EvChar 'i' noMods])
  ck (evs "\r" == [EvKey KeyEnter noMods])
  ck (evs "\DEL" == [EvKey KeyBackspace noMods])
  -- Multi-byte input decodes as one character.
  ck (evs "\xc3\xa9" == [EvChar '\233' noMods])
  ck (evs "\xc3" == [])

runCellsTest :: Context -> IORef Int -> IO ()
runCellsTest ctx failed = do
  let ck cond = when (not cond) (bump failed)
      inp = emptyInput {inputWindowSize = Size 200 80}
      ui = column (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1}) (label "hello")
  (_, _, draw, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  cells <- rasterize 40 10 draw spans
  let rows = cellRows cells
  ck (length rows == 10)
  ck (any (isInfixOf "hello") rows)
  -- An unchanged frame must produce no output, or hover would repaint the
  -- whole screen on every pointer movement.
  ck (BL.null (toLazyByteString (frameBytes (Just cells) cells)))
  ck (not (BL.null (toLazyByteString (frameBytes Nothing cells))))
  -- Box-drawing and block glyphs used by sliders and the text-input caret
  -- must survive the narrow-char filter.
  ck (narrowChar '\x2502')
  ck (narrowChar '\x2588')
  ck (narrowChar '\x2591')
  ck (not (narrowChar '\x4E00'))

runCheckboxTest :: Context -> IORef Int -> IO ()
runCheckboxTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 100}
      ui = column defaultLayout (checkbox "Opt" False)
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      click = V2 (rx + 1) (ry + 0.5)
  let inpPress =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx inpPress ui
  let inpRelease = inpPress {inputMouseDown = False, inputMousePressed = False, inputMouseReleased = True}
  ((_, checked), _, _, _) <- runFrame ctx inpRelease ui
  when (not checked) $ bump failed

runSliderTest :: Context -> IORef Int -> IO ()
runSliderTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 300 80}
      ui = column defaultLayout (slider "Vol" 0 100 10)
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      trackRect = sliderTrackRect rx ry rw rh
      drag = V2 (rx + rw * 0.75) (rectY trackRect + rectH trackRect / 2)
  let inpDrag =
        inp0
          { inputMousePos = drag
          , inputMouseDown = True
          , inputMousePressed = True
          }
  ((_, val), _, _, _) <- runFrame ctx inpDrag ui
  when (val <= 10) $ bump failed

runScrollTest :: Context -> IORef Int -> IO ()
runScrollTest ctx failed = do
  let inp0 =
        emptyInput
          { inputWindowSize = Size 200 120
          , inputMousePos = V2 20 20
          }
      ui = do
        (sid, _) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60})
            ( column defaultLayout $ do
                _ <- replicateM 8 (label "scroll line")
                pure ()
            )
        pure sid
  _ <- runFrame ctx inp0 ui
  (sid, _, _, _) <- runFrame ctx inp0 ui
  off0 <- getScrollOffset ctx sid
  let inpScroll = inp0 {inputScroll = V2 0 1}
  (_, _, _, _) <- runFrame ctx inpScroll ui
  off1 <- getScrollOffset ctx sid
  when (off1 <= off0) $ bump failed

-- Inner list takes the wheel only while hovered. At its limit the page stays put.
runNestedScrollTest :: Context -> IORef Int -> IO ()
runNestedScrollTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 200}
      ui = do
        (outer, inner) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90})
            ( column defaultLayout $ do
                (inner, ()) <-
                  scrollArea
                    (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40})
                    ( column defaultLayout $ do
                        mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12]
                    )
                mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 12]
                pure inner
            )
        pure (outer, inner)
  _ <- runFrame ctx inp0 ui
  ((outer, inner), _, _, _) <- runFrame ctx inp0 ui
  mInner <- getPrevRect ctx inner
  mOuter <- getPrevRect ctx outer
  case (mInner, mOuter) of
    (Just (Rect ix iy iw ih), Just (Rect _ oy _ oh))
      | iw > 0 && ih > 0 -> do
          let hoverInner = inp0 {inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)}
              wheelInner = hoverInner {inputScroll = V2 0 1}
          offI0 <- getScrollOffset ctx inner
          offO0 <- getScrollOffset ctx outer
          _ <- runFrame ctx wheelInner ui
          offI1 <- getScrollOffset ctx inner
          offO1 <- getScrollOffset ctx outer
          when (offI1 <= offI0) $ bump failed
          when (offO1 /= offO0) $ bump failed
          let pumpInner = do
                before <- getScrollOffset ctx inner
                _ <- runFrame ctx wheelInner ui
                after <- getScrollOffset ctx inner
                when (after > before) pumpInner
          pumpInner
          offO2 <- getScrollOffset ctx outer
          when (offO2 /= offO1) $ bump failed
          let hoverOuterY = min (oy + oh - 4) (iy + ih + 8)
              wheelOuter =
                inp0
                  { inputMousePos = V2 (ix + iw / 2) hoverOuterY
                  , inputScroll = V2 0 1
                  }
          offO3 <- getScrollOffset ctx outer
          _ <- runFrame ctx wheelOuter ui
          offO4 <- getScrollOffset ctx outer
          when (offO4 <= offO3) $ bump failed
    _ -> bump failed

-- A nested scroller below the parent viewport must not take wheel hover.
runScrollHoverClipTest :: Context -> IORef Int -> IO ()
runScrollHoverClipTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 200}
      ui = do
        (outer, inner) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80})
            ( column defaultLayout $ do
                mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
                (inner, ()) <-
                  scrollArea
                    (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 36})
                    ( column defaultLayout $ do
                        mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 8]
                    )
                pure inner
            )
        pure (outer, inner)
  _ <- runFrame ctx inp0 ui
  ((_, inner), _, _, _) <- runFrame ctx inp0 ui
  mInner <- getPrevRect ctx inner
  case mInner of
    Just (Rect ix iy iw ih)
      | iw > 0 && ih > 0 -> do
          let hoverHidden =
                inp0
                  { inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)
                  , inputScroll = V2 0 1
                  }
          offI0 <- getScrollOffset ctx inner
          _ <- runFrame ctx hoverHidden ui
          offI1 <- getScrollOffset ctx inner
          when (offI1 > offI0) $ bump failed
    _ -> bump failed

-- Wheel hit-test must use post-scroll rects. Above the shifted viewport is not a hit.
runScrollHitOffsetTest :: Context -> IORef Int -> IO ()
runScrollHitOffsetTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 200}
      ui = do
        (outer, inner) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90})
            ( column defaultLayout $ do
                (inner, ()) <-
                  scrollArea
                    (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 40})
                    ( column defaultLayout $ do
                        mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 12]
                    )
                mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 12]
                pure inner
            )
        pure (outer, inner)
  _ <- runFrame ctx inp0 ui
  ((_, inner), _, _, _) <- runFrame ctx inp0 ui
  mInner0 <- getPrevRect ctx inner
  case mInner0 of
    Just (Rect ix iy iw ih)
      | iw > 0 && ih > 0 -> do
          let wheelInner =
                inp0
                  { inputMousePos = V2 (ix + iw / 2) (iy + ih / 2)
                  , inputScroll = V2 0 1
                  }
          forM_ [(1 :: Int) .. 6] $ \_ -> void (runFrame ctx wheelInner ui)
          mInner1 <- getPrevRect ctx inner
          case mInner1 of
            Just (Rect ix1 iy1 iw1 _) -> do
              off0 <- getScrollOffset ctx inner
              when (off0 <= 0) $ bump failed
              let hoverAbove =
                    inp0
                      { inputMousePos = V2 (ix1 + iw1 / 2) (iy1 - 6)
                      , inputScroll = V2 0 1
                      }
              _ <- runFrame ctx hoverAbove ui
              off1 <- getScrollOffset ctx inner
              when (off1 > off0) $ bump failed
            _ -> bump failed
    _ -> bump failed

-- Wheel with the pointer outside every scroller still moves the nested list
-- when a widget inside it is focused.
runNestedScrollFocusTest :: Context -> IORef Int -> IO ()
runNestedScrollFocusTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 240 220}
      ui = do
        (outer, (inner, btn)) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 90})
            ( column defaultLayout $ do
                pair <-
                  scrollArea
                    (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 50})
                    ( column defaultLayout $ do
                        b <- button "In"
                        mapM_ (\i -> label (T.pack ("in " <> show (i :: Int)))) [1 .. 10]
                        pure b
                    )
                mapM_ (\i -> label (T.pack ("out " <> show (i :: Int)))) [1 .. 10]
                pure pair
            )
        pure (outer, inner, btn)
  _ <- runFrame ctx inp0 ui
  ((_, inner, _), _, _, _) <- runFrame ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = [KeyTab]}) ui
  focus <- getFocusId ctx
  when (focus == WidgetId 0) $ bump failed
  offI0 <- getScrollOffset ctx inner
  let away =
        inp0
          { inputMousePos = V2 230 210
          , inputScroll = V2 0 1
          }
  _ <- runFrame ctx away ui
  offI1 <- getScrollOffset ctx inner
  when (offI1 <= offI0) $ bump failed

runTabFocusTest :: Context -> IORef Int -> IO ()
runTabFocusTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 120}
      ui =
        column defaultLayout $ do
          _ <- button "One"
          _ <- button "Two"
          pure ()
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = [KeyTab]}) ui
  focus1 <- getFocusId ctx
  _ <- runFrame ctx (inp0 {inputKeys = [KeyTab]}) ui
  focus2 <- getFocusId ctx
  when (focus1 == WidgetId 0 || focus2 == WidgetId 0 || focus1 == focus2) $ bump failed

runSelectTest :: Context -> IORef Int -> IO ()
runSelectTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 80}
      ui = column defaultLayout (select "Quality" ["Low", "Medium", "High"] 1)
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let hasMedium =
        any (\(_, txt, _, _, _) -> "Quality: Medium" `T.isInfixOf` txt) spans
  when (not hasMedium) $ bump failed

-- Open select dropdown and check overlay rows render option text.
runSelectDropdownTest :: Context -> IORef Int -> IO ()
runSelectDropdownTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 80}
      ui = column defaultLayout (select "Quality" ["Low", "High"] 0)
  _ <- runFrame ctx inp0 ui
  let click = V2 10 10
      inpPress =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx inpPress ui
  let inpRelease =
        inpPress
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx inpRelease ui
  overlays <- collectOverlayTextSpans ctx inpRelease
  let hasLow = any (\(_, txt, _, _, _) -> "Low" `T.isInfixOf` txt) overlays
      hasHigh = any (\(_, txt, _, _, _) -> "High" `T.isInfixOf` txt) overlays
  when (not (hasLow && hasHigh)) $ bump failed

runSelectDropdownHoverTest :: Context -> IORef Int -> IO ()
runSelectDropdownHoverTest _ failed = do
  ctx <- newTerminalContext
  let layout = defaultLayout {layoutPadding = Padding 0 0 0 0, layoutGap = 0}
      inp0 = emptyInput {inputWindowSize = Size 40 6}
      ui = column layout (select "Quality" ["Low", "High"] 0)
  _ <- runFrame ctx inp0 ui
  let click =
        inp0
          { inputMousePos = V2 1 0.5
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx click ui
  let open =
        click
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx open ui
  let hoverBase =
        open
          { inputMouseReleased = False
          , inputMousePressed = False
          , inputMouseDown = False
          }
  overlaysOpen <- collectOverlayTextSpans ctx hoverBase
  let highYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "High" `T.isInfixOf` txt]
  case highYs of
    (highY : _) -> do
      let hoverHigh = hoverBase {inputMousePos = V2 1 (highY + 0.5)}
      _ <- runFrame ctx hoverHigh ui
      overlaysHigh <- collectOverlayTextSpans ctx hoverHigh
      let bgFor needle spans = [bg | (_, txt, _, bg, _) <- spans, needle `T.isInfixOf` txt]
      case (bgFor "Low" overlaysHigh, bgFor "High" overlaysHigh) of
        ([lowBg], [highBg]) -> when (lowBg == highBg) $ bump failed
        _ -> bump failed
      let lowYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt]
      case (lowYs, bgFor "High" overlaysHigh) of
        ((lowY : _), [highHoverBg]) -> do
          let hoverLow = hoverBase {inputMousePos = V2 1 (lowY + 0.5)}
          _ <- runFrame ctx hoverLow ui
          overlaysLow <- collectOverlayTextSpans ctx hoverLow
          case bgFor "Low" overlaysLow of
            [lowHoverBg]
              | lowHoverBg == highHoverBg -> pure ()
              | otherwise -> bump failed
            _ -> bump failed
        _ -> bump failed
    _ -> bump failed

runSelectPickLowTest :: Context -> IORef Int -> IO ()
runSelectPickLowTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 200}
      ui = select "Quality" ["Low", "Medium", "High"] 1
  _ <- runFrame ctx inp0 ui
  ((resp, idx0), _, _, _) <- runFrame ctx inp0 ui
  when (idx0 /= 1) $ bump failed
  let Rect sx sy sw sh = respRect resp
      btn = V2 (sx + sw / 2) (sy + sh / 2)
      openPress =
        inp0
          { inputMousePos = btn
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx openPress ui
  let openRelease =
        openPress
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx openRelease ui
  overlaysOpen <- collectOverlayTextSpans ctx openRelease
  let lowYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt]
  case lowYs of
    (lowY : _) -> do
      let pickPos = V2 (sx + sw / 2) (lowY + 0.5)
          pickPress =
            inp0
              { inputMousePos = pickPos
              , inputMouseDown = True
              , inputMousePressed = True
              , inputMouseReleased = False
              }
      _ <- runFrame ctx pickPress ui
      focusAfterPick <- getFocusId ctx
      when (hashWidgetId focusAfterPick == 0) $ bump failed
      let pickRelease =
            pickPress
              { inputMousePressed = False
              , inputMouseDown = False
              , inputMouseReleased = True
              }
      ((_, idx1), _, _, _) <- runFrame ctx pickRelease ui
      when (idx1 /= 0) $ bump failed
      spans <- collectTextSpans ctx
      let hasLow =
            any (\(_, txt, _, _, _) -> "Quality: Low" `T.isInfixOf` txt) spans
      when (not hasLow) $ bump failed
    _ -> bump failed

runSelectKeyboardTest :: Context -> IORef Int -> IO ()
runSelectKeyboardTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 200}
      ui = select "Quality" ["Low", "Medium", "High"] 1
  _ <- runFrame ctx inp0 ui
  ((resp, idx0), _, _, _) <- runFrame ctx inp0 ui
  when (idx0 /= 1) $ bump failed
  let Rect sx sy sw sh = respRect resp
      btn = V2 (sx + sw / 2) (sy + sh / 2)
      openPress =
        inp0
          { inputMousePos = btn
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
  _ <- runFrame ctx openPress ui
  let openRelease =
        openPress
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx openRelease ui
  _ <- runFrame ctx (openRelease {inputKeys = [KeyDown]}) ui
  ((_, idx1), _, _, _) <- runFrame ctx openRelease ui
  when (idx1 /= 2) $ bump failed
  _ <- runFrame ctx (openRelease {inputKeys = [KeyUp]}) ui
  ((_, idx2), _, _, _) <- runFrame ctx openRelease ui
  when (idx2 /= 1) $ bump failed
  _ <- runFrame ctx (openRelease {inputKeys = [KeyEscape]}) ui
  _ <- runFrame ctx openRelease ui
  overlays <- collectOverlayTextSpans ctx openRelease
  let dropdownOpen =
        any
          (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"])
          overlays
  when dropdownOpen $ bump failed
  -- Focused with menu closed: arrows change value without opening the list.
  _ <- runFrame ctx (inp0 {inputKeys = [KeyTab]}) ui
  focus <- getFocusId ctx
  when (focus == WidgetId 0) $ bump failed
  _ <- runFrame ctx (inp0 {inputKeys = [KeyRight]}) ui
  ((_, idx3), _, _, _) <- runFrame ctx inp0 ui
  when (idx3 /= 2) $ bump failed
  closedOverlays <- collectOverlayTextSpans ctx inp0
  let closedMenuOpen =
        any
          (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"])
          closedOverlays
  when closedMenuOpen $ bump failed
  _ <- runFrame ctx (inp0 {inputKeys = [KeyLeft]}) ui
  ((_, idx4), _, _, _) <- runFrame ctx inp0 ui
  when (idx4 /= 1) $ bump failed

runTextWrapTest :: Context -> IORef Int -> IO ()
runTextWrapTest _ failed = do
  ctx <- newTerminalContext
  let inp = emptyInput {inputWindowSize = Size 40 10}
      long = T.replicate 24 (T.pack "x")
      ui = labelEx (defaultLayout {layoutMaxW = 8}) long
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  when (length spans < 3) $ bump failed

-- Grow labels wrap to the assigned column width without an explicit maxW.
runTextWrapAssignedTest :: Context -> IORef Int -> IO ()
runTextWrapAssignedTest _ failed = do
  ctx <- newTerminalContext
  let inp = emptyInput {inputWindowSize = Size 20 12}
      long = T.replicate 24 (T.pack "x")
      ui =
        column
          ( defaultLayout
              { layoutWidth = Fixed 8
              , layoutPadding = Padding 0 0 0 0
              , layoutGap = 0
              }
          )
          $ labelEx (defaultLayout {layoutWidth = Grow 1}) long
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  when (length spans < 3) $ bump failed

runFlexWrapTest :: Context -> IORef Int -> IO ()
runFlexWrapTest _ failed = do
  ctx <- newTerminalContext
  let inp = emptyInput {inputWindowSize = Size 30 10}
      ui =
        row
          (defaultLayout {layoutWrap = True, layoutWidth = Fixed 10, layoutGap = 0})
          ( do
              _ <- label "AA"
              _ <- label "BB"
              _ <- label "CC"
              _ <- label "DD"
              pure ()
          )
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let ys = nub (map (\(Rect _ y _ _, _, _, _, _) -> (round y :: Int)) spans)
  when (length ys < 2) $ bump failed

runFlexShrinkTest :: Context -> IORef Int -> IO ()
runFlexShrinkTest _ failed = do
  ctx <- newTerminalContext
  let inp = emptyInput {inputWindowSize = Size 20 10}
      ui =
        row
          ( defaultLayout
              { layoutWidth = Fixed 5
              , layoutGap = 0
              , layoutPadding = Padding 0 0 0 0
              }
          )
          ( do
              _ <- labelEx (defaultLayout {layoutWidth = Shrink 1}) "AA"
              _ <- labelEx (defaultLayout {layoutWidth = Shrink 1}) "BB"
              _ <- labelEx (defaultLayout {layoutWidth = Shrink 1}) "CC"
              pure ()
          )
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  when (length spans /= 3) $ bump failed
  let lastX = maximum (map (\(Rect x _ _ _, _, _, _, _) -> x) spans)
  when (lastX > 3.5) $ bump failed

-- Grow children must shrink when the window is narrower than content.
runGrowFitsWindowTest :: Context -> IORef Int -> IO ()
runGrowFitsWindowTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 10 10}
      ui =
        row
          ( defaultLayout
              { layoutWidth = Grow 1
              , layoutHeight = Grow 1
              , layoutPadding = Padding 0 0 0 0
              , layoutGap = 0
              }
          )
          $ do
            a <- spacer (Grow 1) (Fixed 8)
            b <- spacer (Grow 1) (Fixed 8)
            pure (a, b)
  _ <- runFrame ctx inp ui
  ((ra, rb), _, _, _) <- runFrame ctx inp ui
  let Rect x1 _ w1 _ = respRect ra
      Rect x2 _ w2 _ = respRect rb
  when (w1 <= 0 || w2 <= 0) $ bump failed
  when (x1 < -0.01 || x2 + w2 > 10.01) $ bump failed
  when (abs (w1 - w2) > 0.5) $ bump failed

-- Grow wrap must remasure height so the next sibling sits below wrapped lines.
runGrowWrapPushesSiblingTest :: Context -> IORef Int -> IO ()
runGrowWrapPushesSiblingTest _ failed = do
  ctx <- newTerminalContext
  let inp = emptyInput {inputWindowSize = Size 6 20}
      ui =
        column
          ( defaultLayout
              { layoutWidth = Grow 1
              , layoutHeight = Grow 1
              , layoutPadding = Padding 0 0 0 0
              , layoutGap = 0
              }
          )
          $ do
            row
              ( defaultLayout
                  { layoutWidth = Grow 1
                  , layoutWrap = True
                  , layoutPadding = Padding 0 0 0 0
                  , layoutGap = 0
                  }
              )
              $ do
                _ <- label "AAAA"
                _ <- label "BBBB"
                pure ()
            label "BELOW"
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let ysFor t = [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == t]
  case (ysFor "BBBB", ysFor "BELOW") of
    ([by], [sy]) -> when (sy < by + 0.5) $ bump failed
    _ -> bump failed

-- App state lives in the widget store, so clicks persist without IORefs.
runUseFlagClickTest :: Context -> IORef Int -> IO ()
runUseFlagClickTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 240 120}
      ui = do
        (open, setOpen) <- useFlag False
        (note, setNote) <- useText ""
        resp <- button "Go"
        onClick resp $ do
          setOpen True
          setNote "hi"
        pure (open, note, resp)
  _ <- runFrame ctx inp0 ui
  ((open0, note0, resp), _, _, _) <- runFrame ctx inp0 ui
  when (open0 || note0 /= "") $ bump failed
  let Rect x y w h = respRect resp
      pos = V2 (x + w / 2) (y + h / 2)
      press =
        inp0
          { inputMousePos = pos
          , inputMouseDown = True
          , inputMousePressed = True
          , inputMouseReleased = False
          }
      release =
        press
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  ((open1, note1, _), _, _, _) <- runFrame ctx inp0 ui
  when (not open1 || note1 /= "hi") $ bump failed

-- panel paints chrome; a fat-padded column does not.
runPanelPaintsTest :: Context -> IORef Int -> IO ()
runPanelPaintsTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 200 200}
      fat = padAll 16 (fillW defaultLayout)
  (_, _, colDraw, _) <- runFrame ctx inp (column fat (label "x"))
  (_, _, panDraw, _) <- runFrame ctx inp (panel fat (label "x"))
  when (drawVertexCount panDraw <= drawVertexCount colDraw) $ bump failed

-- Page chrome sits below window padding so the header outline is not on y=0.
runHeaderTopPadTest :: Context -> IORef Int -> IO ()
runHeaderTopPadTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 800 600}
      ui =
        column (padAll 12 . gap 8 . grow $ defaultLayout) $
          panel (padXY 16 12 . fillW $ defaultLayout) $
            label "nano-ui SDL3 demo"
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  let Rect _ y _ _ = respRect resp
  when (y < 24) $ bump failed

-- A Fit header beside a Grow scroll keeps its content height when the
-- window is shorter than the scroll content.
runFitHeaderNoShrinkTest :: Context -> IORef Int -> IO ()
runFitHeaderNoShrinkTest ctx failed = do
  let header = panel (padXY 16 12 . fillW $ defaultLayout) (label "nano-ui SDL3 demo")
      only =
        column (padAll 12 . grow $ defaultLayout) header
      withBody = do
        r <-
          column (padAll 12 . gap 8 . grow $ defaultLayout) $ do
            h <- header
            scroll (tight (grow defaultLayout)) $
              column (fillW defaultLayout) $
                mapM_ (label_ . T.pack . show) [1 .. 40 :: Int]
            pure h
        pure r
      tall = emptyInput {inputWindowSize = Size 400 800}
      short = emptyInput {inputWindowSize = Size 400 200}
  _ <- runFrame ctx tall only
  (r0, _, _, _) <- runFrame ctx tall only
  _ <- runFrame ctx short withBody
  (r1, _, _, _) <- runFrame ctx short withBody
  when (rectH (respRect r1) + 0.5 < rectH (respRect r0)) $ bump failed

-- Floating windows draw on the overlay, ignore backdrop clicks, and close on X.
runWindowOverlayTest :: Context -> IORef Int -> IO ()
runWindowOverlayTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 640 400}
      ui = do
        outside <- button "Outside"
        (win, mBody) <-
          window True "Debug" $ do
            label "Body"
        pure (outside, win, mBody)
      closedUi = do
        _ <- button "Outside"
        (win, mBody) <- window False "Debug" (label "Body")
        pure (win, mBody)
  do
    ((win, mBody), _, _, _) <- runFrame ctx inp0 closedUi
    when (respClicked win) $ bump failed
    case mBody of
      Nothing -> pure ()
      Just _ -> bump failed
    closedSpans <- collectOverlayTextSpans ctx inp0
    when (any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) closedSpans) $ bump failed
  _ <- runFrame ctx inp0 ui
  ((outside0, win0, mBody0), _, _, _) <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  let hasTitle = any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) overlays
      hasBody = any (\(_, txt, _, _, _) -> "Body" `T.isInfixOf` txt) overlays
  when (not (hasTitle && hasBody)) $ bump failed
  let Rect wx wy ww wh = respRect win0
  when (ww < 100 || wh < 20) $ bump failed
  case mBody0 of
    Nothing -> bump failed
    Just _ -> pure ()
  let clickOut =
        inp0
          { inputMousePos = V2 (rectX (respRect outside0) + 8) (rectY (respRect outside0) + 8)
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx clickOut ui
  let releaseOut =
        clickOut
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
  ((outsideHit, _, _), _, _, _) <- runFrame ctx releaseOut ui
  when (not (respClicked outsideHit)) $ bump failed
  let mid = V2 (wx + ww / 2) (wy + wh * 0.7)
      clickWin =
        inp0
          { inputMousePos = mid
          , inputMouseDown = True
          , inputMousePressed = True
          }
  ((outsideMid, _, _), _, _, _) <- runFrame ctx clickWin ui
  when (respClicked outsideMid) $ bump failed
  let esc = inp0 {inputKeys = [KeyEscape]}
  ((_, winEsc, _), _, _, _) <- runFrame ctx esc ui
  when (respClicked winEsc) $ bump failed

runWindowDragTest :: Context -> IORef Int -> IO ()
runWindowDragTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 640 400}
      ui = do
        (win, _) <- window True "Debug" (label "Body")
        pure win
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 24) (y0 + 10)
      press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let moved =
        press
          { inputMousePos = V2 (x0 + 24 - 50) (y0 + 10 + 30)
          , inputMousePressed = False
          }
  _ <- runFrame ctx moved ui
  (win1, _, _, _) <- runFrame ctx moved ui
  let Rect x1 y1 _ _ = respRect win1
  when (x1 >= x0 - 10) $ bump failed
  when (y1 <= y0 + 10) $ bump failed

-- A column separator spans the parent width and stays a 1px hairline.
runSeparatorSpanTest :: Context -> IORef Int -> IO ()
runSeparatorSpanTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 200 120}
      ui =
        column (fillW defaultLayout) $ do
          label_ "A"
          resp <- separator
          label_ "B"
          pure resp
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  let Rect _ _ w h = respRect resp
  when (w < 100) $ bump failed
  when (h > 2) $ bump failed
