module Main (main) where

import Control.Monad (forM_, replicateM, void, when)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import Data.List (isInfixOf, nub, sort)
import Effectful.State.Static.Local (State, evalState, get, modify)
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Term
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

main :: IO ()
main = do
  failed <- newIORef 0
  ctx <- newContext
  -- Pixel-host metrics only (headless); no SDL3 libs required for these tests.
  sdlCtx <- newPixelContext

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
  runSdl "slider-cursor" runSliderCursorTest
  run "scroll-thumb-cursor" runScrollThumbCursorTest
  runSdl "text-input-span" runTextInputSpanTest
  runSdl "text-input-focus-sdl" runTextInputFocusSdlTest
  run "button-hover-anim" runButtonHoverAnimTest
  run "button-press-release-hover" runButtonPressReleaseHoverTest
  run "text-input-focus" runTextInputFocusTest
  run "idle" runIdleTest
  run "hover-skip" runHoverSkipTest
  run "hover-damage" runHoverDamageTest
  run "scroll-damage" runScrollDamageTest
  run "select-overlay-damage" runSelectOverlayDamageTest
  run "text-input-dirty" runTextInputDirtyTest
  run "modal-close-damage" runModalCloseDamageTest
  run "modal-open-damage" runModalOpenDamageTest
  run "window-close-damage" runWindowCloseDamageTest
  run "window-drag-damage" runWindowDragDamageTest
  run "overlay-panel-live" runOverlayPanelLiveTest
  run "animation-idle" runAnimationIdleTest
  run "animation-settle" runAnimationSettleTest
  run "animation-ease" runAnimationEaseTest
  run "animation-hold" runAnimationHoldTest
  run "animation-damage" runAnimationDamageTest
  run "animation-delay" runAnimationDelayTest
  run "animation-stagger" runAnimationStaggerTest
  run "animation-shared-ctx" runAnimationSharedCtxTest
  run "animation-bezier" runAnimationBezierTest
  run "animation-spring" runAnimationSpringTest
  run "animation-spring-retarget" runAnimationSpringRetargetTest
  run "animation-spring-dt" runAnimationSpringDtTest
  run "animation-spring-hold" runAnimationSpringHoldTest
  run "animation-spring-a" runAnimationSpringATest
  run "ascii" runAsciiTest
  run "vt-decode" runVtTest
  run "cells-and-diff" runCellsTest
  run "checkbox-toggle" runCheckboxTest
  runSdl "slider-store" runSliderTest
  runSdl "slider-fill-width" runSliderFillWidthTest
  run "scroll-wheel" runScrollTest
  run "nested-scroll" runNestedScrollTest
  run "nested-scroll-focus" runNestedScrollFocusTest
  run "scroll-hover-clip" runScrollHoverClipTest
  run "scroll-hit-offset" runScrollHitOffsetTest
  run "tab-focus" runTabFocusTest
  run "select-initial" runSelectTest
  run "select-dropdown" runSelectDropdownTest
  run "select-dropdown-hover" runSelectDropdownHoverTest
  runSdl "select-drop-flush" runSelectDropFlushTest
  run "select-pick-low" runSelectPickLowTest
  run "select-keyboard" runSelectKeyboardTest
  run "text-wrap" runTextWrapTest
  run "text-wrap-width" runTextWrapAssignedTest
  run "text-multiline" runTextMultilineTest
  run "flex-wrap" runFlexWrapTest
  run "flex-shrink" runFlexShrinkTest
  run "grow-fits-window" runGrowFitsWindowTest
  run "percent-layout" runPercentLayoutTest
  run "aspect-layout" runAspectLayoutTest
  run "label-align-end" runLabelAlignEndTest
  run "grow-wrap-sibling" runGrowWrapPushesSiblingTest
  run "terminal-default-gap" runTerminalDefaultGapTest
  run "terminal-slider-track" runTerminalSliderTrackTest
  run "terminal-text-input" runTerminalTextInputDisplayTest
  run "terminal-modal-overlay" runTerminalModalOverlayTest
  run "terminal-modal-scroll" runTerminalModalScrollTest
  run "terminal-modal-tight" runTerminalModalTightTest
  run "terminal-modal-open-redraw" runTerminalModalOpenRedrawTest
  run "terminal-window-overlay" runTerminalWindowOverlayTest
  run "terminal-window-drag" runTerminalWindowDragTest
  run "terminal-window-drag-icons" runTerminalWindowDragIconTest
  run "terminal-close-button" runTerminalCloseButtonTest
  run "icon-set" runIconSetTest
  run "terminal-icon-chrome" runTerminalIconChromeTest
  run "terminal-icon-close" runTerminalIconCloseTest
  run "terminal-button-brackets" runTerminalButtonBracketTest
  run "terminal-wide-clear-bracket" runTerminalWideClearBracketTest
  run "terminal-wide-cursor-cup" runTerminalWideCursorCupTest
  run "terminal-wide-transitions" runTerminalWideTransitionTest
  run "terminal-wide-pairs" runTerminalWidePairTest
  run "terminal-theme-contrast" runTerminalThemeContrastTest
  run "scroll-bar-gutter" runScrollBarGutterTest
  runSdl "scroll-bar-gutter-grow" runGrowScrollGutterTest
  runSdl "scroll-bar-gutter-panel" runPanelGrowScrollGutterTest
  runSdl "window-scroll-gutter" runWindowScrollGutterTest
  run "use-flag-click" runUseFlagClickTest
  run "host-slot" runHostSlotTest
  run "host-profile-gap" runHostProfileGapTest
  run "host-profile-measure" runHostProfileMeasureTest
  run "compact-host" runCompactHostTest
  run "embed-state" runEmbedStateTest
  run "reduce-messages" runReduceMessagesTest
  run "reduce-updates" runReduceUpdatesTest
  run "reduce-click" runReduceClickTest
  run "reduce-identity" runReduceIdentityTest
  run "widget-no-string-emit" runWidgetNoStringEmitTest
  run "panel-paints" runPanelPaintsTest
  run "separator-span" runSeparatorSpanTest
  run "terminal-separator-span" runTerminalSeparatorSpanTest
  run "header-top-pad" runHeaderTopPadTest
  run "fit-header-no-shrink" runFitHeaderNoShrinkTest
  run "window-overlay" runWindowOverlayTest
  run "window-drag" runWindowDragTest
  run "window-scroll-wheel" runWindowScrollWheelTest
  run "window-resize" runWindowResizeTest
  run "window-resize-halo-hit" runWindowResizeHaloHitTest

  n <- readIORef failed
  if n == 0
    then putStrLn "All tests passed."
    else do
      putStrLn $ show n ++ " test(s) failed."
      fail "tests failed"

bump :: IORef Int -> IO ()
bump r = modifyIORef r (+ 1)

-- HostProfile drives cell vs pixel layout even when font metrics suggest the other host.
runHostProfileGapTest :: Context -> IORef Int -> IO ()
runHostProfileGapTest _ failed = do
  let defaultGap = layoutGap defaultLayout
      cellGap = resolveLayoutGap CellHost (monospaceMetrics 1) defaultGap
      pixelGap = resolveLayoutGap PixelHost (monospaceMetrics 16) defaultGap
  when (cellGap /= 1) $ bump failed
  when (pixelGap /= defaultGap) $ bump failed

runHostProfileMeasureTest :: Context -> IORef Int -> IO ()
runHostProfileMeasureTest _ failed = do
  let txt = "abcde"
      fmCell = monospaceMetrics 1
      fmPixel = monospaceMetrics 16
      cellW = textDisplayWidth CellHost fmCell txt
      pixelW = textDisplayWidth PixelHost fmPixel txt
  when (cellW /= fromIntegral (terminalPaintColumns txt)) $ bump failed
  when (pixelW /= fromIntegral (T.length txt) * fmAdvance fmPixel ' ') $ bump failed

findGrabHover :: Context -> NanoUI a -> Input -> Float -> [Float] -> IO (Maybe Input)
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
  (resp, msgs, _, _) <- runFrame ctx inpRelease ui
  when (not (respClicked resp) || not (null msgs)) $ bump failed

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
  term <- newAdaptiveTerminalContext
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
      hasCloseGlyph = any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays
  when (not (hasTitle && hasInside)) $ bump failed
  when hasCloseGlyph $ bump failed
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
      track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Volume" rx ry rw rh
      trackMid = V2 (rectX track + rectW track / 2) (rectY track + rectH track / 2)
      labelPos = V2 (rx + 4) (ry + 4)
  let hoverTrack = inp0 {inputMousePos = trackMid}
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
      let barW = scrollBarWidth
          thumbX = rx + rw - scrollBarListExtra - barW / 2
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

-- Overflowing list scroll reserves a right gutter. Bar stays inside the well.
runScrollBarGutterTest :: Context -> IORef Int -> IO ()
runScrollBarGutterTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 200 120}
      ui = do
        (sid, child) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60})
            ( do
                r <- labelEx (fillW defaultLayout) "Wide"
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
          endPad = padR (layoutPadding defaultLayout)
          gutter = scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarListExtra
          contentRight = sx + sw - endPad - gutter
      when (cx + cw < contentRight - 0.5) $ bump failed
      when (cx + cw > contentRight + 0.01) $ bump failed

-- Grow/Grow page scroll reserves bar width plus a small right inset.
runGrowScrollGutterTest :: Context -> IORef Int -> IO ()
runGrowScrollGutterTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 240 140}
      ui = do
        (sid, child) <-
          scrollArea
            (tight (grow defaultLayout))
            ( do
                r <- labelEx (fillW defaultLayout) "Wide"
                _ <- replicateM 20 (label "scroll line")
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
          fm = ctxFontMetrics ctx
          gutter = scrollBarGutter (ctxHostProfile ctx) fm + scrollBarPageExtra
          contentRight = sx + sw - gutter
      when (cx + cw < contentRight - 0.5) $ bump failed
      when (cx + cw > contentRight + 0.01) $ bump failed

-- Grow/Grow inside a panel is a list well, not page chrome.
runPanelGrowScrollGutterTest :: Context -> IORef Int -> IO ()
runPanelGrowScrollGutterTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 240 140}
      ui = do
        (sid, child) <-
          panel (grow defaultLayout) $
            scrollArea (tight (grow defaultLayout)) $ do
              r <- labelEx (fillW defaultLayout) "Wide"
              _ <- replicateM 20 (label "scroll line")
              pure r
        pure (sid, child)
  _ <- runFrame ctx inp0 ui
  ((sid, child), _, _, _) <- runFrame ctx inp0 ui
  mrect <- getPrevRect ctx sid
  case mrect of
    Nothing -> bump failed
    Just (Rect sx _ sw _) -> do
      let Rect cx _ cw _ = respRect child
          gutter = scrollBarGutter (ctxHostProfile ctx) (ctxFontMetrics ctx) + scrollBarListExtra
          contentRight = sx + sw - gutter
      when (cx + cw < contentRight - 0.5) $ bump failed
      when (cx + cw > contentRight + 0.01) $ bump failed

-- Window body keeps full inner width. Bar hangs into the window pad.
runWindowScrollGutterTest :: Context -> IORef Int -> IO ()
runWindowScrollGutterTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 640 360}
      long = T.pack (replicate 48 'M')
      ui = do
        (win, mwide) <-
          window True "GutterWin" $ do
            wide <- labelEx (fillW defaultLayout) "WWWW"
            kv "Key" long
            mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
            pure wide
        pure (win, mwide)
  _ <- runFrame ctx inp0 ui
  ((win, mwide), _, _, _) <- runFrame ctx inp0 ui
  let Rect wx _ ww _ = respRect win
      contentRight = wx + ww - padR windowPad
  spans <- collectOverlayTextSpans ctx inp0
  let titleYs = [rectY r | (r, txt, _, _, _) <- spans, "GutterWin" `T.isInfixOf` txt]
  when (null titleYs) $ bump failed
  case mwide of
    Nothing -> bump failed
    Just wide -> do
      let Rect cx _ cw _ = respRect wide
      when (cx + cw < contentRight - 0.5) $ bump failed
      when (cx + cw > contentRight + 0.01) $ bump failed

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
  ctx <- newAdaptiveTerminalContext
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

runHoverSkipTest :: Context -> IORef Int -> IO ()
runHoverSkipTest _ failed = do
  ctx <- newContext
  let ui = column defaultLayout (button "OK")
      inp0 =
        emptyInput
          { inputWindowSize = Size 240 80
          , inputMousePos = V2 (-10) (-10)
          }
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      inside = V2 (rx + rw / 2) (ry + rh / 2)
      inside2 = V2 (rx + rw / 2 + 1) (ry + rh / 2)
      inp1 = inp0 {inputMousePos = inside}
      inp2 = inp0 {inputMousePos = inside2}
  needEnter <- needsRedraw ctx inp0 inp1
  when (not needEnter) $ bump failed
  _ <- runFrame ctx inp1 ui
  let drain = inp1 {inputDeltaTime = 1}
  _ <- runFrame ctx drain ui
  needStay <- needsRedraw ctx drain inp2
  when needStay $ bump failed
  let inpClick = inp1 {inputMouseDown = True, inputMousePressed = True}
  needClick <- needsRedraw ctx drain inpClick
  when (not needClick) $ bump failed

runHoverDamageTest :: Context -> IORef Int -> IO ()
runHoverDamageTest _ failed = do
  ctx <- newContext
  let ui = column defaultLayout (button "OK")
      inp0 =
        emptyInput
          { inputWindowSize = Size 240 80
          , inputMousePos = V2 (-10) (-10)
          }
  _ <- runFrame ctx inp0 ui
  d0 <- takeDamage ctx
  when (d0 /= DamageFull) $ bump failed
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      inside = V2 (rx + rw / 2) (ry + rh / 2)
      inp1 = inp0 {inputMousePos = inside}
  _ <- runFrame ctx inp1 ui
  d1 <- takeDamage ctx
  case d1 of
    DamageFull -> bump failed
    DamageClip (Rect _ _ w h) ->
      when (w * h >= 240 * 80 * 0.5) $ bump failed
  let inpClick = inp1 {inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx inpClick ui
  d2 <- takeDamage ctx
  when (d2 /= DamageFull) $ bump failed

runScrollDamageTest :: Context -> IORef Int -> IO ()
runScrollDamageTest _ failed = do
  ctx <- newContext
  let ui = do
        (sid, _) <-
          scrollArea
            (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 60})
            ( column defaultLayout $ do
                _ <- replicateM 8 (label "scroll line")
                pure ()
            )
        pure sid
      inp0 =
        emptyInput
          { inputWindowSize = Size 200 120
          , inputMousePos = V2 (-10) (-10)
          }
  (_, _, _, _) <- runFrame ctx inp0 ui
  let inpHover = inp0 {inputMousePos = V2 20 20}
  _ <- runFrame ctx inpHover ui
  dHover <- takeDamage ctx
  case dHover of
    DamageFull -> bump failed
    DamageClip {} -> pure ()
  let inpScroll = inpHover {inputScroll = V2 0 1}
  _ <- runFrame ctx inpScroll ui
  dScroll <- takeDamage ctx
  when (dScroll /= DamageFull) $ bump failed

-- Open dropdown: motion over the menu must redraw, and damage must be full.
runSelectOverlayDamageTest :: Context -> IORef Int -> IO ()
runSelectOverlayDamageTest _ failed = do
  ctx <- newContext
  let ui = column defaultLayout (select "Quality" ["Low", "Medium", "High"] 0)
      inp0 = emptyInput {inputWindowSize = Size 320 160, inputMousePos = V2 20 20}
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect sx sy sw sh = respRect resp
      click = V2 (sx + sw / 2) (sy + sh / 2)
      press =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let open =
        press
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx open ui
  let idle = open {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  let highYs = [rectY r | (r, txt, _, _, _) <- overlays, "High" `T.isInfixOf` txt]
  case highYs of
    [] -> bump failed
    (highY : _) -> do
      let overMenu = idle {inputMousePos = V2 (sx + sw / 2) (highY + 0.5)}
      need <- needsRedraw ctx idle overMenu
      when (not need) $ bump failed
      _ <- runFrame ctx overMenu ui
      dmg <- takeDamage ctx
      when (dmg /= DamageFull) $ bump failed

-- Focused text field must stay live so typed bytes are not delayed until
-- the next unrelated wake, and store text changes force a full redraw.
runTextInputDirtyTest :: Context -> IORef Int -> IO ()
runTextInputDirtyTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let ui = column defaultLayout (textInput "Name" "")
      inp0 = emptyInput {inputWindowSize = Size 200 100, inputMousePos = V2 20 20}
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry _ _ = respRect resp
      click = V2 (rx + 1) (ry + 0.5)
      press =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let release =
        press
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx release ui
  let idle = release {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  needFocus <- needsRedraw ctx idle idle
  when (not needFocus) $ bump failed
  let typed = idle {inputChars = "ab"}
  _ <- runFrame ctx typed ui
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed

-- Esc dismisses the modal this frame; the next idle frame must still redraw
-- the dim and panel away (full damage).
runModalCloseDamageTest :: Context -> IORef Int -> IO ()
runModalCloseDamageTest _ failed = do
  ctx <- newContext
  let ui = do
        (open, setOpen) <- useFlag True
        (resp, _) <- modal open "Title" (label "body")
        onClick resp (setOpen False)
      inp0 = emptyInput {inputWindowSize = Size 320 240, inputMousePos = V2 1 1}
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  let esc = inp0 {inputKeys = [KeyEscape]}
  _ <- runFrame ctx esc ui
  let idle = inp0 {inputDeltaTime = 1}
  need <- needsRedraw ctx idle idle
  when (not need) $ bump failed
  _ <- runFrame ctx idle ui
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed

-- Click opens the modal next frame. That idle frame must still redraw the dim.
runModalOpenDamageTest :: Context -> IORef Int -> IO ()
runModalOpenDamageTest _ failed = do
  ctx <- newContext
  let ui = do
        (open, setOpen) <- useFlag False
        resp <- button "Open"
        onClick resp (setOpen True)
        _ <- modal open "Title" (label "body")
        pure resp
      inp0 = emptyInput {inputWindowSize = Size 320 240, inputMousePos = V2 (-10) (-10)}
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      click = V2 (rx + rw / 2) (ry + rh / 2)
      press =
        inp0
          { inputMousePos = click
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let release =
        press
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx release ui
  let idle = inp0 {inputDeltaTime = 1}
  need <- needsRedraw ctx idle idle
  when (not need) $ bump failed
  _ <- runFrame ctx idle ui
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed

-- Closing a floating window must redraw the content it covered.
runWindowCloseDamageTest :: Context -> IORef Int -> IO ()
runWindowCloseDamageTest _ failed = do
  ctx <- newContext
  let ui open = void (window open "Debug" (label "Body"))
      inp0 = emptyInput {inputWindowSize = Size 640 400}
  _ <- runFrame ctx inp0 (ui True)
  _ <- runFrame ctx inp0 (ui True)
  _ <- runFrame ctx inp0 (ui False)
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed
  let idle = inp0 {inputDeltaTime = 1}
  need <- needsRedraw ctx inp0 idle
  when (not need) $ bump failed

-- Dragging a window must not leave partial clips that skip the old footprint.
runWindowDragDamageTest :: Context -> IORef Int -> IO ()
runWindowDragDamageTest _ failed = do
  ctx <- newContext
  let inp0 = emptyInput {inputWindowSize = Size 640 400}
      ui = do
        (win, _) <- window True "Debug" (label "Body")
        pure win
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 24) (y0 + 22)
      press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let moved =
        press
          { inputMousePos = V2 (x0 + 24 - 50) (y0 + 22 + 30)
          , inputMousePressed = False
          }
  _ <- runFrame ctx moved ui
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed

-- Modals with static content idle without redraw. Floating windows tick live.
runOverlayPanelLiveTest :: Context -> IORef Int -> IO ()
runOverlayPanelLiveTest _ failed = do
  let inp = emptyInput {inputWindowSize = Size 320 240, inputMousePos = V2 (-10) (-10)}
      checkStatic ui = do
        ctx <- newContext
        _ <- runFrame ctx inp ui
        _ <- runFrame ctx inp ui
        need <- needsRedraw ctx inp inp
        when need $ bump failed
        _ <- runFrame ctx inp ui
        dmg <- takeDamage ctx
        when (not (damageIsEmpty dmg)) $ bump failed
      checkWindowLive ui = do
        ctx <- newContext
        _ <- runFrame ctx inp ui
        _ <- runFrame ctx inp ui
        need <- needsRedraw ctx inp inp
        when (not need) $ bump failed
        _ <- runFrame ctx inp ui
        dmg <- takeDamage ctx
        when (dmg /= DamageFull) $ bump failed
      checkDirtyWake ui = do
        ctx <- newContext
        _ <- runFrame ctx inp ui
        markDirty ctx
        need <- needsRedraw ctx inp inp
        when (not need) $ bump failed
        _ <- runFrame ctx inp ui
        dmg <- takeDamage ctx
        when (dmg /= DamageFull) $ bump failed
  checkWindowLive (void (window True "Debug" (label "fps 0")))
  checkStatic (void (modal True "About" (label "body")))
  checkDirtyWake (void (modal True "About" (label "body")))

runAnimationIdleTest :: Context -> IORef Int -> IO ()
runAnimationIdleTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.05}
  _ <- runFrame ctx inp (label "anim")
  startAnimation ctx (WidgetId 42) 0 1 0.5
  need <- needsRedraw ctx inp inp
  when (not need) $ bump failed

-- Finished tweens keep the end value and stop waking the loop.
runAnimationSettleTest :: Context -> IORef Int -> IO ()
runAnimationSettleTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.1}
      wid = WidgetId 99
  startAnimation ctx wid 0 1 0.25
  void $ replicateM 4 (runFrame ctx inp (label "settle"))
  val <- getAnimationValue ctx wid
  when (abs (val - 1) > 0.01) $ bump failed
  live <- anyAnimating ctx
  when live $ bump failed
  need <- needsRedraw ctx inp inp
  when need $ bump failed

-- EaseOutCubic at halfway is well above linear 0.5.
runAnimationEaseTest :: Context -> IORef Int -> IO ()
runAnimationEaseTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.5}
      wid = WidgetId 100
  startAnimationEase ctx wid 0 1 1 EaseOutCubic
  _ <- runFrame ctx inp (label "ease")
  val <- getAnimationValue ctx wid
  when (val < 0.8) $ bump failed

-- animateTo holds after the duration elapses.
runAnimationHoldTest :: Context -> IORef Int -> IO ()
runAnimationHoldTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 200 100, inputDeltaTime = 0.1}
      ui = do
        t <- animateTo 1 0.2
        label_ (T.pack (show t))
  void $ replicateM 5 (runFrame ctx inp ui)
  (_, _, _, _) <- runFrame ctx inp ui
  live <- anyAnimating ctx
  when live $ bump failed
  spans <- collectTextSpans ctx
  let shown = [txt | (_, txt, _, _, _) <- spans]
  when (not (any (\t -> t == "1.0" || "1.0" `T.isPrefixOf` t) shown)) $ bump failed
  startAnimationEase ctx (WidgetId 101) 0 1 0.2 EaseLinear
  void $ replicateM 5 (runFrame ctx inp (label "hold"))
  val <- getAnimationValue ctx (WidgetId 101)
  when (abs (val - 1) > 0.01) $ bump failed
  void $ replicateM 3 (runFrame ctx inp (label "hold"))
  val2 <- getAnimationValue ctx (WidgetId 101)
  when (abs (val2 - 1) > 0.01) $ bump failed
  live2 <- anyAnimating ctx
  when live2 $ bump failed

-- Layout tweens clip the moved spacer (old+new). Drain newContext Full
-- first so the assertion is a mid-tween frame, not the first paint.
-- Also cover start-and-finish in one tick (dt > dur).
runAnimationDamageTest :: Context -> IORef Int -> IO ()
runAnimationDamageTest _ failed = do
  ctx <- newContext
  let idleInp = emptyInput {inputWindowSize = Size 200 100, inputDeltaTime = 0}
      idle = label_ "anim"
      tweenInp = idleInp {inputDeltaTime = 0.05}
      ui = do
        t <- animateTo 1 0.4
        void (spacer (Fixed (20 + 80 * t)) Fit)
        label_ "anim"
      hasMove dmg = case dmg of
        DamageFull -> True
        DamageClip r -> rectW r > 0 && rectH r > 0
  _ <- runFrame ctx idleInp idle
  _ <- runFrame ctx idleInp idle
  dIdle <- takeDamage ctx
  when (dIdle == DamageFull) $ bump failed
  _ <- runFrame ctx tweenInp ui
  dMid <- takeDamage ctx
  when (not (hasMove dMid)) $ bump failed
  ctx2 <- newContext
  let fastInp = idleInp {inputDeltaTime = 0.5}
      uiFast = do
        t <- animateTo 1 0.2
        void (spacer (Fixed (20 + 80 * t)) Fit)
        label_ "anim"
  _ <- runFrame ctx2 idleInp idle
  _ <- runFrame ctx2 idleInp idle
  _ <- runFrame ctx2 fastInp uiFast
  dFast <- takeDamage ctx2
  when (not (hasMove dFast)) $ bump failed

-- Delay holds the start value, then leftover dt applies after the wait.
runAnimationDelayTest :: Context -> IORef Int -> IO ()
runAnimationDelayTest _ failed = do
  ctx <- newContext
  let inp0 = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.1}
      wid = WidgetId 202
  startAnimationEaseDelay ctx wid 0 1 0.2 EaseLinear 0.15
  _ <- runFrame ctx inp0 (label "delay")
  v0 <- getAnimationValue ctx wid
  when (abs v0 > 0.01) $ bump failed
  live0 <- anyAnimating ctx
  when (not live0) $ bump failed
  _ <- runFrame ctx inp0 (label "delay")
  v1 <- getAnimationValue ctx wid
  when (abs (v1 - 0.25) > 0.03) $ bump failed

-- Staggered delays must count down through animateToEaseDelay, not reset.
runAnimationStaggerTest :: Context -> IORef Int -> IO ()
runAnimationStaggerTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 200 100, inputDeltaTime = 0.02}
      ui = do
        _ <- withKey ("lead" :: String) (animateToEaseDelay EaseLinear 1 0.4 0)
        t <- withKey ("trail" :: String) (animateToEaseDelay EaseLinear 1 0.4 0.08)
        label_ (T.pack ("t=" ++ show t))
      trailVal = do
        spans <- collectTextSpans ctx
        let shown = [txt | (_, txt, _, _, _) <- spans]
            tagged = [T.drop 2 txt | txt <- shown, "t=" `T.isPrefixOf` txt]
        case tagged of
          (raw : _) ->
            case reads (T.unpack raw) of
              [(n, "")] -> pure (n :: Float)
              _ -> bump failed >> pure 0
          _ -> bump failed >> pure 0
  void $ replicateM 3 (runFrame ctx inp ui)
  early <- trailVal
  when (early > 0.01) $ bump failed
  void $ replicateM 10 (runFrame ctx inp ui)
  late <- trailVal
  when (late < 0.15) $ bump failed

-- CubicBezier (0,0,1,1) matches linear. A standard ease-out sits above it.
runAnimationBezierTest :: Context -> IORef Int -> IO ()
runAnimationBezierTest _ failed = do
  let lin = applyEase (EaseCubicBezier 0 0 1 1) 0.5
      out = applyEase (EaseCubicBezier 0 0 0.58 1) 0.5
  when (abs (lin - 0.5) > 0.01) $ bump failed
  when (out <= 0.5) $ bump failed
  when (abs (applyEase EaseInQuad 0.5 - 0.25) > 0.01) $ bump failed
  when (abs (applyEase (EaseCubicBezier 0.33 0 0.2 1) 0) > 0.001) $ bump failed
  when (abs (applyEase (EaseCubicBezier 0.33 0 0.2 1) 1 - 1) > 0.001) $ bump failed

-- Smooth spring reaches the target and drops the wake flag.
runAnimationSpringTest :: Context -> IORef Int -> IO ()
runAnimationSpringTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.05}
      wid = WidgetId 401
  startSpring ctx wid presetSmooth 1
  void $ replicateM 80 (runFrame ctx inp (label "spring"))
  val <- getAnimationValue ctx wid
  when (abs (val - 1) > 0.02) $ bump failed
  live <- anyAnimating ctx
  when live $ bump failed
  need <- needsRedraw ctx inp inp
  when need $ bump failed

-- Retarget keeps the current position. Value must not jump to the new start.
runAnimationSpringRetargetTest :: Context -> IORef Int -> IO ()
runAnimationSpringRetargetTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 0.02}
      wid = WidgetId 402
  startSpring ctx wid presetBouncy 1
  void $ replicateM 5 (runFrame ctx inp (label "retarget"))
  v1 <- getAnimationValue ctx wid
  when (v1 < 0.02 || v1 > 0.98) $ bump failed
  startSpring ctx wid presetBouncy 0
  v2 <- getAnimationValue ctx wid
  when (abs (v2 - v1) > 0.02) $ bump failed
  live <- anyAnimating ctx
  when (not live) $ bump failed

-- One large dt stays finite after substeps.
runAnimationSpringDtTest :: Context -> IORef Int -> IO ()
runAnimationSpringDtTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 100 100, inputDeltaTime = 2}
      wid = WidgetId 403
  startSpring ctx wid presetStiff 1
  _ <- runFrame ctx inp (label "dt")
  val <- getAnimationValue ctx wid
  when (isNaN val || isInfinite val || val < 0 || val > 1.5) $ bump failed

-- animateToSpring holds after settle, same as animateTo.
runAnimationSpringHoldTest :: Context -> IORef Int -> IO ()
runAnimationSpringHoldTest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 200 100, inputDeltaTime = 0.05}
      ui = do
        t <- animateToSpring presetSmooth 1
        label_ (T.pack (show t))
  void $ replicateM 80 (runFrame ctx inp ui)
  (_, _, _, _) <- runFrame ctx inp ui
  live <- anyAnimating ctx
  when live $ bump failed
  spans <- collectTextSpans ctx
  let shown = [txt | (_, txt, _, _, _) <- spans]
  when (not (any (\t -> t == "1.0" || "1.0" `T.isPrefixOf` t) shown)) $ bump failed

-- animateToSpringA springs each V2 component and holds.
runAnimationSpringATest :: Context -> IORef Int -> IO ()
runAnimationSpringATest _ failed = do
  ctx <- newContext
  let inp = emptyInput {inputWindowSize = Size 200 100, inputDeltaTime = 0.05}
      ui = do
        V2 x y <- withKey ("vec" :: String) (animateToSpringA presetSmooth (V2 1 2))
        label_ (T.pack (show x ++ "," ++ show y))
  void $ replicateM 80 (runFrame ctx inp ui)
  live <- anyAnimating ctx
  when live $ bump failed
  spans <- collectTextSpans ctx
  let shown = [txt | (_, txt, _, _, _) <- spans]
      ok t =
        case break (== ',') (T.unpack t) of
          (xs, ',' : ys) ->
            case (reads xs, reads ys) of
              ([(x, "")], [(y, "")]) -> abs (x - 1 :: Float) < 0.05 && abs (y - 2 :: Float) < 0.05
              _ -> False
          _ -> False
  when (not (any ok shown)) $ bump failed

-- Settled tweens on a shared Context must not leave dirty or wake idle loops.
runAnimationSharedCtxTest :: Context -> IORef Int -> IO ()
runAnimationSharedCtxTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 80 80, inputDeltaTime = 0.1}
      wid = WidgetId 777
  startAnimation ctx wid 0 1 0.1
  void $ replicateM 3 (runFrame ctx inp (label "shared"))
  val <- getAnimationValue ctx wid
  when (abs (val - 1) > 0.01) $ bump failed
  need <- needsRedraw ctx inp inp
  when need $ bump failed
  (_, _, _, dirty) <- runFrame ctx inp (label "idle")
  when dirty $ bump failed

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
  -- Nerd font / Font Awesome icons live in the private use area and must pass
  -- the same filter, or the glyph tier would render blanks.
  ck (all narrowChar (concatMap T.unpack (glyphIconTexts glyphIcons)))
  ck (terminalTextColumns "\xf046" == 2)
  ck (terminalPaintColumns "\xf046" == 1)
  ck (terminalPaintColumns (iconClose glyphIcons) == 1)
  ck (terminalTextColumns (iconClose glyphIcons) == 2)
  ck (terminalTextColumns (iconChecked glyphIcons) == 3)
  ck (terminalPaintColumns (iconChecked glyphIcons) == 3)
  ck (terminalTextColumns (iconUnchecked glyphIcons) == 3)

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
      track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Vol" rx ry rw rh
      drag = V2 (rectX track + rectW track * 0.75) (rectY track + rectH track / 2)
  let inpDrag =
        inp0
          { inputMousePos = drag
          , inputMouseDown = True
          , inputMousePressed = True
          }
  ((_, val), _, _, _) <- runFrame ctx inpDrag ui
  when (val <= 10) $ bump failed

-- Grow sliders take the column width; drag maps to the painted track span.
runSliderFillWidthTest :: Context -> IORef Int -> IO ()
runSliderFillWidthTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 400 120}
      ui = column (fillW defaultLayout) (slider "Vol" 0 100 0)
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
  when (rw < 300) $ bump failed
  let track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Vol" rx ry rw rh
      endDrag =
        V2 (rectX track + rectW track - 2) (rectY track + rectH track / 2)
      inpDrag =
        inp0
          { inputMousePos = endDrag
          , inputMouseDown = True
          , inputMousePressed = True
          }
  ((_, val), _, _, _) <- runFrame ctx inpDrag ui
  when (val < 90) $ bump failed

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
  ctx <- newAdaptiveTerminalContext
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

-- The option list hangs off the select bottom; only the menu pad separates them.
runSelectDropFlushTest :: Context -> IORef Int -> IO ()
runSelectDropFlushTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 200}
      ui = select "Quality" ["Low", "High"] 1
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect sx sy sw sh = respRect resp
      press =
        inp0
          { inputMousePos = V2 (sx + sw / 2) (sy + sh / 2)
          , inputMouseDown = True
          , inputMousePressed = True
          }
      release =
        press
          { inputMousePressed = False
          , inputMouseDown = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  overlays <- collectOverlayTextSpans ctx release
  -- 6 menu pad plus text centering in the 28px row. A gap would add 6 more.
  let maxOffset = 12
  case [rectY r | (r, txt, _, _, _) <- overlays, "Low" `T.isInfixOf` txt] of
    (lowY : _) -> when (lowY - (sy + sh) > maxOffset) $ bump failed
    [] -> bump failed

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
  ctx <- newAdaptiveTerminalContext
  let inp = emptyInput {inputWindowSize = Size 40 10}
      long = T.replicate 24 (T.pack "x")
      ui = labelEx (defaultLayout {layoutMaxW = 8}) long
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  when (length spans < 3) $ bump failed

-- Grow labels wrap to the assigned column width without an explicit maxW.
runTextWrapAssignedTest :: Context -> IORef Int -> IO ()
runTextWrapAssignedTest _ failed = do
  ctx <- newAdaptiveTerminalContext
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

-- Explicit newlines layout as stacked lines; marker does not add width.
runTextMultilineTest :: Context -> IORef Int -> IO ()
runTextMultilineTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp = emptyInput {inputWindowSize = Size 40 10}
      ui = labelEx (tight defaultLayout) (monoFontMarker <> "aa\nbb\ncc")
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let rows = sort (map (\(Rect _ y _ _, txt, _, _, _) -> (round y :: Int, txt)) spans)
  when (map snd rows /= ["aa", "bb", "cc"]) $ bump failed
  case map fst rows of
    [a, b, c] -> when (b /= a + 1 || c /= b + 1) $ bump failed
    _ -> bump failed

runFlexWrapTest :: Context -> IORef Int -> IO ()
runFlexWrapTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp = emptyInput {inputWindowSize = Size 30 10}
      ui =
        row
          (defaultLayout {layoutWrap = True, layoutWidth = Fixed 4, layoutGap = 0, layoutPadding = Padding 0 0 0 0})
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
  ctx <- newAdaptiveTerminalContext
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

-- Percent sizing is a fraction of the parent's inner size.
runPercentLayoutTest :: Context -> IORef Int -> IO ()
runPercentLayoutTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 200 80}
      ui =
        row (fixedW 200 . tight . gap 0 $ defaultLayout) $ do
          a <- labelEx (percent 25 . tight $ defaultLayout) "A"
          b <- labelEx (percent 75 . tight $ defaultLayout) "B"
          pure (a, b)
  _ <- runFrame ctx inp ui
  ((a, b), _, _, _) <- runFrame ctx inp ui
  let Rect _ _ wa _ = respRect a
      Rect _ _ wb _ = respRect b
  when (abs (wa - 50) > 1) $ bump failed
  when (abs (wb - 150) > 1) $ bump failed

-- Right-aligned label glyphs sit on the content-box right edge.
runLabelAlignEndTest :: Context -> IORef Int -> IO ()
runLabelAlignEndTest _ failed = do
  checkLabelAlignEnd failed =<< newAdaptiveTerminalContext
  checkLabelAlignEnd failed =<< newPixelContext

checkLabelAlignEnd :: IORef Int -> Context -> IO ()
checkLabelAlignEnd failed ctx = do
  let fm = ctxFontMetrics ctx
      (ix, _) = labelContentInset (ctxHostProfile ctx) fm
      tw = fmAdvance fm ' ' * 2
      boxW = tw + 2 * ix + 4
      inp = emptyInput {inputWindowSize = Size (boxW + 8) 8}
      ui =
        row (fixedW boxW . tight . gap 0 $ defaultLayout) $
          labelEx (fillW . alignEnd . tight $ defaultLayout) "ab"
  _ <- runFrame ctx inp ui
  (lab, _, _, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let Rect bx _ bw _ = respRect lab
      hits = [r | (r, txt, _, _, _) <- spans, T.isInfixOf (T.pack "ab") txt]
  case hits of
    [] -> bump failed
    Rect x _ w _ : _ -> do
      when (abs ((x + w) - (bx + bw - ix)) > 0.6) $ bump failed
      when (abs (w - tw) > 0.6) $ bump failed

-- Aspect locks height to width / ratio after width is known.
runAspectLayoutTest :: Context -> IORef Int -> IO ()
runAspectLayoutTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 320 240}
      ui =
        column (fixedW 160 . tight $ defaultLayout) $
          labelEx (fillW . aspect 2 . tight $ defaultLayout) "X"
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  let Rect _ _ w h = respRect resp
  when (abs (w - 160) > 1) $ bump failed
  when (abs (h - 80) > 1) $ bump failed

-- Grow wrap must remasure height so the next sibling sits below wrapped lines.
runGrowWrapPushesSiblingTest :: Context -> IORef Int -> IO ()
runGrowWrapPushesSiblingTest _ failed = do
  ctx <- newAdaptiveTerminalContext
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

-- defaultLayout gap/pad are pixel-sized; terminal scales them to cells.
runTerminalDefaultGapTest :: Context -> IORef Int -> IO ()
runTerminalDefaultGapTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let fm = ctxFontMetrics ctx
      expectedStep = fmLineHeight fm + resolveLayoutGap (ctxHostProfile ctx) fm (layoutGap defaultLayout)
      inp = emptyInput {inputWindowSize = Size 20 10}
      ui =
        column defaultLayout $ do
          _ <- label "A"
          _ <- label "B"
          pure ()
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let ys = sort [y | (Rect _ y _ _, txt, _, _, _) <- spans, txt == "A" || txt == "B"]
  case ys of
    yA : yB : _ -> when (yB - yA > expectedStep + 0.25) $ bump failed
    _ -> bump failed

-- Terminal slider drag maps to the inline [bar], not the grow node width.
runTerminalSliderTrackTest :: Context -> IORef Int -> IO ()
runTerminalSliderTrackTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 60 10}
      ui = column (fillW defaultLayout) (slider "Vol" 0 100 0)
  _ <- runFrame ctx inp0 ui
  ((resp, _), _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      fm = ctxFontMetrics ctx
      track = sliderTrackBounds (ctxHostProfile ctx) fm "Vol" rx ry rw rh
  when (rectW track >= rw - 1) $ bump failed
  when (rectW track < 10) $ bump failed
  let endDrag =
        inp0
          { inputMousePos = V2 (rectX track + rectW track - 0.5) (rectY track + rectH track / 2)
          , inputMouseDown = True
          , inputMousePressed = True
          }
  ((_, val), _, _, _) <- runFrame ctx endDrag ui
  when (val < 90) $ bump failed

-- Modal/window chrome is pixel-authored. Terminal must scale it to cells
-- so title and body stay on the 80x24 grid instead of clipping away.
runTerminalModalOverlayTest :: Context -> IORef Int -> IO ()
runTerminalModalOverlayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 80 24}
      ui =
        column defaultLayout $ do
          _ <- label "Behind"
          (dlg, _) <-
            modal True "About" $ do
              heading "nano-ui"
              muted "Immediate-mode GUI for Haskell."
              muted "Terminal backend demo."
              row (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fit}) $ do
                flex
                clickButton "Close" (pure ())
              pure ()
          pure dlg
  _ <- runFrame ctx inp0 ui
  (dlg, _, drawData, _) <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  base <- collectTextSpans ctx
  let hasTitle = any (\(_, txt, _, _, _) -> "About" `T.isInfixOf` txt) overlays
      hasBody = any (\(_, txt, _, _, _) -> "Immediate-mode" `T.isInfixOf` txt) overlays
      hasClose = any (\(_, txt, _, _, _) -> "Close" `T.isInfixOf` txt) overlays
      inGrid (Rect x y w h, _, _, _, _) =
        x >= -0.5 && y >= -0.5 && x + w <= 80.5 && y + h <= 24.5
  when (not (hasTitle && hasBody && hasClose)) $ bump failed
  when (any (not . inGrid) overlays) $ bump failed
  let Rect _ _ mw mh = respRect dlg
  when (mw > 80 || mh > 24 || mw < 8 || mh < 2) $ bump failed
  cells <- rasterizeLayered 80 24 drawData base overlays
  let blob = concat (cellRows cells)
  when (not ("About" `isInfixOf` blob)) $ bump failed
  when (not ("Immediate-mode" `isInfixOf` blob)) $ bump failed
  when (not ("Behind" `isInfixOf` blob)) $ bump failed
  when (not ('\x2500' `elem` blob)) $ bump failed
  when (not (any (\c -> cmdTextureId c == backdropDimTextureId) (drawCommands drawData))) $
    bump failed

-- TUI About modal: title + sep + 4 body lines + float pad/gap (see modal/2).
terminalAboutModalMaxH :: HostProfile -> FontMetrics -> Float
terminalAboutModalMaxH host fm =
  let pad = resolveLayoutPadding host fm (Padding 4 4 4 4)
      modalGap = resolveLayoutGap host fm 8
      bodyGap = resolveLayoutGap host fm (layoutGap defaultLayout)
      line = fmLineHeight fm
      titleH = if host == CellHost then 1 else 28
      sepH = 1
      bodyRows = (4 :: Int)
      bodyH =
        fromIntegral bodyRows * line
          + bodyGap * fromIntegral (pred bodyRows)
      chromeH = titleH + sepH + bodyH + modalGap * 2
   in padT pad + padB pad + chromeH + 0.5

terminalAboutModalMaxFooter :: HostProfile -> FontMetrics -> Float
terminalAboutModalMaxFooter host fm =
  let pad = resolveLayoutPadding host fm (Padding 4 4 4 4)
   in padB pad + fmLineHeight fm

-- Title stays pinned. Body clips and scrolls inside the modal.
runTerminalModalScrollTest :: Context -> IORef Int -> IO ()
runTerminalModalScrollTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 80 16}
      line1 = T.pack "line 1"
      ui = do
        (dlg, _) <-
          modal True "About" $
            column defaultLayout $
              mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
        pure dlg
  _ <- runFrame ctx inp0 ui
  (dlg, _, _, _) <- runFrame ctx inp0 ui
  let Rect mx _ mw mh = respRect dlg
  when (mw <= 0 || mh <= 0 || mh > 16) $ bump failed
  spans0 <- collectOverlayTextSpans ctx inp0
  let titleYs0 = spanYs (T.pack "About") spans0
      line1Ys0 = spanLabelYs line1 spans0
  when (null titleYs0) $ bump failed
  case line1Ys0 of
    [] -> bump failed
    b0 : _ -> do
      let wheelAt = V2 (mx + mw / 2) (b0 + 0.5)
          wheel =
            inp0
              { inputMousePos = wheelAt
              , inputScroll = V2 0 1
              }
      _ <- runFrame ctx wheel ui
      spans1 <- collectOverlayTextSpans ctx wheel
      let titleYs1 = spanYs (T.pack "About") spans1
          line1Ys1 = spanLabelYs line1 spans1
      case (titleYs0, titleYs1) of
        (y0 : _, y1 : _) -> when (y1 /= y0) $ bump failed
        _ -> bump failed
      case line1Ys1 of
        [] -> pure ()
        b1 : _ -> when (b1 >= b0) $ bump failed
      when (any (\(Rect _ y _ h, _, _, _, _) -> y < 0 || y + h > 16.5) spans1) $
        bump failed

-- About body should sit on the modal, not a tall empty footer under Close.
runTerminalModalTightTest :: Context -> IORef Int -> IO ()
runTerminalModalTightTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 80 24}
      ui = do
        (dlg, _) <-
          modal True "About" $ do
            heading "nano-ui"
            muted "Immediate-mode GUI for Haskell."
            muted "Terminal backend demo."
            row (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fit}) $ do
              flex
              clickButton "Close" (pure ())
            pure ()
        pure dlg
  _ <- runFrame ctx inp0 ui
  (dlg, _, _, _) <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  let fm = ctxFontMetrics ctx
      Rect _ my _ mh = respRect dlg
      maxH = terminalAboutModalMaxH (ctxHostProfile ctx) fm
      maxFooter = terminalAboutModalMaxFooter (ctxHostProfile ctx) fm
  case closeSpanBottom overlays of
    Nothing -> bump failed
    Just bottom ->
      let footer = my + mh - bottom
       in when (mh > maxH || footer > maxFooter) $ bump failed

-- Flag open must redraw on the next idle frame without waiting for input.
runTerminalModalOpenRedrawTest :: Context -> IORef Int -> IO ()
runTerminalModalOpenRedrawTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 80 24, inputMousePos = V2 (-10) (-10)}
      ui = do
        (open, setOpen) <- useFlag False
        resp <- button "Open"
        onClick resp (setOpen True)
        _ <- modal open "About" (label "body")
        pure resp
  _ <- runFrame ctx inp0 ui
  (resp, _, _, _) <- runFrame ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      press =
        inp0
          { inputMousePos = V2 (rx + rw / 2) (ry + rh / 2)
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let release =
        press
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
  _ <- runFrame ctx release ui
  let idle = inp0 {inputDeltaTime = 0}
  need <- needsRedrawIdle ctx release idle
  when (not need) $ bump failed
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  let hasAbout = any (\(_, txt, _, _, _) -> "About" `T.isInfixOf` txt) overlays
  when (not hasAbout) $ bump failed
  dmg <- takeDamage ctx
  when (damageIsEmpty dmg) $ bump failed

runTerminalWindowOverlayTest :: Context -> IORef Int -> IO ()
runTerminalWindowOverlayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 80 24}
      ui =
        window True "Debug" $ do
          _ <- label "Floating window overlay."
          pure ()
  _ <- runFrame ctx inp0 ui
  ((win, _), _, drawData, _) <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  base <- collectTextSpans ctx
  let hasTitle = any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) overlays
      hasBody = any (\(_, txt, _, _, _) -> "Floating window" `T.isInfixOf` txt) overlays
      inGrid (Rect x y w h, _, _, _, _) =
        x >= -0.5 && y >= -0.5 && x + w <= 80.5 && y + h <= 24.5
  when (not (hasTitle && hasBody)) $ bump failed
  when (any (not . inGrid) overlays) $ bump failed
  let Rect _ _ ww wh = respRect win
  when (ww > 80 || wh > 24 || ww < 8 || wh < 2) $ bump failed
  cells <- rasterizeLayered 80 24 drawData base overlays
  let blob = concat (cellRows cells)
  when (not ("Debug" `isInfixOf` blob)) $ bump failed
  when (not ("Floating window" `isInfixOf` blob)) $ bump failed
  when (not ('\x2500' `elem` blob)) $ bump failed

runTerminalWindowDragTest :: Context -> IORef Int -> IO ()
runTerminalWindowDragTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 80 24}
      ui = do
        (win, _) <- window True "Debug" (label "Body")
        pure win
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 4) (y0 + 1.5)
      press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let moved =
        press
          { inputMousePos = V2 (x0 + 4 - 8) (y0 + 1.5 + 4)
          , inputMousePressed = False
          }
  _ <- runFrame ctx moved ui
  (win1, _, _, _) <- runFrame ctx moved ui
  let Rect x1 y1 _ _ = respRect win1
  when (x1 >= x0 - 2) $ bump failed
  when (y1 <= y0 + 1) $ bump failed

-- Dragging with nerd-font close icons must clear both terminal columns the
-- glyph occupied; otherwise the trail cell leaves a ghost on the screen.
runTerminalWindowDragIconTest :: Context -> IORef Int -> IO ()
runTerminalWindowDragIconTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp0 = emptyInput {inputWindowSize = Size 80 24}
      ui = do
        (win, _) <- window True "Debug" (label "Body")
        pure win
  _ <- runFrame ctx inp0 ui
  (_, _, draw0, _) <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 4) (y0 + 1.5)
      press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let moved =
        press
          { inputMousePos = V2 (x0 + 4 - 10) (y0 + 1.5 + 4)
          , inputMousePressed = False
          }
  (_, _, draw1, _) <- runFrame ctx moved ui
  overlays1 <- collectOverlayTextSpans ctx moved
  let Size tw th = inputWindowSize inp0
  cells0 <- rasterizeLayered (round tw) (round th) draw0 [] overlays0
  cells1 <- rasterizeLayered (round tw) (round th) draw1 [] overlays1
  case closeSpanPos overlays0 of
    Nothing -> bump failed
    Just (closeCol, rowY) -> do
      let maxX =
            case cellRows cells1 of
              (r : _) -> length r - 1
              [] -> -1
          trailGhost =
            any
              ( \x ->
                  x >= 0
                    && x <= maxX
                    && rowY >= 0
                    && rowY < cellsH cells1
                    && let c1 = cellChar cells1 x rowY
                        in fontAwesomeIcon c1 || c1 == wideTrailChar
              )
              [closeCol, closeCol + 1]
      case closeSpanStart overlays1 of
        Nothing -> bump failed
        Just x1 -> do
          when (x1 >= closeCol - 2) $ bump failed
          when (x1 < closeCol - 2 && trailGhost) $ bump failed
  let bytes = toLazyByteString (frameBytes (Just cells0) cells1)
  when (BL.null bytes) $ bump failed

-- Lone FA paint spans (close, scroll carets) reserve one cell, not a pair.
oneColFaOrigins :: [(Rect, T.Text, a, b, c)] -> [(Int, Int)]
oneColFaOrigins spans =
  [ (round (rectX r), round (rectY r))
  | (r, txt, _, _, _) <- spans
  , rectW r < 2
  , loneFontAwesome (T.strip txt)
  ]

closeSpanPos :: [(Rect, T.Text, Color, Color, Rect)] -> Maybe (Int, Int)
closeSpanPos spans =
  case [(round (rectX r), round (rectY r)) | (r, txt, _, _, _) <- spans, T.strip txt == iconClose glyphIcons] of
    (p : _) -> Just p
    [] -> Nothing

closeSpanStart :: [(Rect, T.Text, Color, Color, Rect)] -> Maybe Int
closeSpanStart spans = fmap fst (closeSpanPos spans)

-- Wide icon trail cells must not appear inside button bracket spans, and close
-- hover must still produce a frame diff.
runTerminalButtonBracketTest :: Context -> IORef Int -> IO ()
runTerminalButtonBracketTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp0 = emptyInput {inputWindowSize = Size 50 20}
      ui = do
        (dlg, _) <-
          modal True "Long modal title for clip" $ do
            row defaultLayout $ do
              void (button "OK")
              void (button "Cancel")
            label_ "Body"
        pure dlg
      bracketSpans spans =
        [ r
        | (r, txt, _, _, _) <- spans
        , T.isPrefixOf "[ " txt
        ]
      badSpan cells (Rect x y w h) =
        let x0 = max 0 (round x)
            y0 = max 0 (round y)
            x1 = min (gridW cells - 1) (round (x + w - 1))
            y1 = min (cellsH cells - 1) (round (y + h - 1))
         in any
              ( \cy ->
                  any (\cx -> cellChar cells cx cy == wideTrailChar) [x0 .. x1]
              )
              [y0 .. y1]
      gridW cells =
        case cellRows cells of
          (r : _) -> length r
          [] -> 0
  _ <- runFrame ctx inp0 ui
  (_, _, draw0, _) <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  let Size tw th = inputWindowSize inp0
      spans0 = bracketSpans overlays0
  when (null spans0) $ bump failed
  cells0 <- rasterizeLayered (round tw) (round th) draw0 [] overlays0
  when (any (badSpan cells0) spans0) $ bump failed
  case closeSpanPos overlays0 of
    Nothing -> bump failed
    Just (closeCol, closeY) -> do
      let hover =
            inp0
              { inputMousePos = V2 (fromIntegral closeCol + 0.5) (fromIntegral closeY + 0.5)
              , inputMouseDown = False
              }
      (_, _, draw1, _) <- runFrame ctx hover ui
      overlays1 <- collectOverlayTextSpans ctx hover
      cells1 <- rasterizeLayered (round tw) (round th) draw1 [] overlays1
      when (any (badSpan cells1) (bracketSpans overlays1)) $ bump failed
  let pageUi =
        column defaultLayout $
          row defaultLayout $ do
            void (button "OK")
            void (button "Cancel")
            void (checkbox "Feature" False)
  _ <- runFrame ctx inp0 pageUi
  (_, _, drawP, _) <- runFrame ctx inp0 pageUi
  baseP <- collectTextSpans ctx
  cellsP <- rasterizeLayered (round tw) (round th) drawP baseP []
  let pageBrackets = bracketSpans baseP
  when (null pageBrackets) $ bump failed
  when (any (badSpan cellsP) pageBrackets) $ bump failed

-- A wide glyph landing on "[OK]" must emit spaces for both columns. Windows
-- Terminal keeps the old second cell if we only write the lead codepoint.
runTerminalWideClearBracketTest :: Context -> IORef Int -> IO ()
runTerminalWideClearBracketTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 2 1}
      clip = Rect 0 0 2 1
      fg = colorRGBA 220 220 220 255
      bg = colorRGBA 20 20 24 255
  (_, _, draw, _) <- runFrame ctx inp (pure ())
  cellsA <- rasterize 2 1 draw [(clip, "[O", fg, bg, clip)]
  cellsB <- rasterize 2 1 draw [(clip, iconClose glyphIcons, fg, bg, clip)]
  let bytes = toLazyByteString (frameBytes (Just cellsA) cellsB)
      packed = BL.unpack bytes
  when (cellChar cellsA 0 0 /= '[') $ bump failed
  when (cellChar cellsA 1 0 /= 'O') $ bump failed
  when (not (fontAwesomeIcon (cellChar cellsB 0 0))) $ bump failed
  when (cellChar cellsB 1 0 /= wideTrailChar) $ bump failed
  when (BL.null bytes) $ bump failed
  when (0x20 `notElem` packed) $ bump failed
  when (not (BS.isInfixOf "\ESC[1;1H" (BL.toStrict bytes))) $ bump failed

-- After a wide glyph the writer must CUP to column x+2 so a following '['
-- cannot land on the glyph's second cell when the console advanced only one.
runTerminalWideCursorCupTest :: Context -> IORef Int -> IO ()
runTerminalWideCursorCupTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 4 1}
      clip = Rect 0 0 4 1
      fg = colorRGBA 220 220 220 255
      bg = colorRGBA 20 20 24 255
  (_, _, draw, _) <- runFrame ctx inp (pure ())
  cells <-
    rasterize
      4
      1
      draw
      [ (Rect 0 0 2 1, iconClose glyphIcons, fg, bg, clip)
      , (Rect 2 0 2 1, "[O", fg, bg, clip)
      ]
  when (not (fontAwesomeIcon (cellChar cells 0 0))) $ bump failed
  when (cellChar cells 1 0 /= wideTrailChar) $ bump failed
  when (cellChar cells 2 0 /= '[') $ bump failed
  let bytes = BL.toStrict (toLazyByteString (frameBytes Nothing cells))
  -- 1-based CUP to column 3 (grid x=2), after the 2-col icon.
  when (not (BS.isInfixOf "\ESC[1;3H" bytes)) $ bump failed

-- Window drag, button hover, and modal open must keep FA+trail pairs and '['.
runTerminalWideTransitionTest :: Context -> IORef Int -> IO ()
runTerminalWideTransitionTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp0 = emptyInput {inputWindowSize = Size 80 24}
      page = do
        void (button "OK")
        void (button "Cancel")
        void (checkbox "Feature" False)
      windowUi = do
        page
        (win, _) <- window True "Debug" (label_ "Body")
        pure win
      modalUi open = do
        page
        (dlg, _) <- modal open "About" (label_ "Body")
        pure dlg
      gridW cells =
        case cellRows cells of
          (r : _) -> length r
          [] -> 0
      pairsOk cells spans =
        let skip = oneColFaOrigins spans
         in all
              ( \(x, y) ->
                  let c = cellChar cells x y
                   in not (fontAwesomeIcon c)
                        || (x, y) `elem` skip
                        || ( x + 1 < gridW cells
                               && cellChar cells (x + 1) y == wideTrailChar
                           )
              )
              [ (x, y)
              | y <- [0 .. cellsH cells - 1]
              , x <- [0 .. gridW cells - 1]
              ]
      bracketsOk cells spans =
        all
          ( \(Rect x y w h) ->
              let x0 = max 0 (round x)
                  y0 = max 0 (round y)
                  x1 = min (gridW cells - 1) (round (x + w - 1))
                  y1 = min (cellsH cells - 1) (round (y + h - 1))
               in all
                    ( \cy ->
                        all
                          (\cx -> cellChar cells cx cy /= wideTrailChar)
                          [x0 .. x1]
                    )
                    [y0 .. y1]
          )
          [ r
          | (r, txt, _, _, _) <- spans
          , T.isPrefixOf "[ " txt
          ]
  -- Button hover on the page row (checkbox FA sits on the same wrap row).
  _ <- runFrame ctx inp0 page
  (_, _, draw0, _) <- runFrame ctx inp0 page
  base0 <- collectTextSpans ctx
  let Size tw th = inputWindowSize inp0
  cells0 <- rasterizeLayered (round tw) (round th) draw0 base0 []
  when (not (pairsOk cells0 base0)) $ bump failed
  when (not (bracketsOk cells0 base0)) $ bump failed
  case [ (round (rectX r), round (rectY r))
       | (r, txt, _, _, _) <- base0
       , T.isPrefixOf "[ " txt
       ] of
    [] -> bump failed
    ((bx, by) : _) -> do
      let hover =
            inp0
              { inputMousePos = V2 (fromIntegral bx + 1.5) (fromIntegral by + 0.5)
              }
      (_, _, drawH, _) <- runFrame ctx hover page
      baseH <- collectTextSpans ctx
      cellsHov <- rasterizeLayered (round tw) (round th) drawH baseH []
      when (not (pairsOk cellsHov baseH)) $ bump failed
      when (not (bracketsOk cellsHov baseH)) $ bump failed
      when (cellChar cellsHov bx by /= '[') $ bump failed
  -- Modal open: close icon is present, page brackets stay clean.
  _ <- runFrame ctx inp0 (void (modalUi False))
  (_, _, drawC, _) <- runFrame ctx inp0 (void (modalUi False))
  (baseC, overC) <- collectRasterSpans ctx inp0
  cellsC <- rasterizeLayered (round tw) (round th) drawC baseC overC
  _ <- runFrame ctx inp0 (void (modalUi True))
  (_, _, drawM, _) <- runFrame ctx inp0 (void (modalUi True))
  (baseM, overM) <- collectRasterSpans ctx inp0
  cellsM <- rasterizeLayered (round tw) (round th) drawM baseM overM
  when (not (pairsOk cellsM (baseM ++ overM))) $ bump failed
  when (closeSpanPos overM == Nothing) $ bump failed
  when (not (bracketsOk cellsC baseC)) $ bump failed
  -- Window drag: old close columns must not keep FA/trail.
  _ <- runFrame ctx inp0 windowUi
  (_, _, _, _) <- runFrame ctx inp0 windowUi
  overW0 <- collectOverlayTextSpans ctx inp0
  (win0, _, _, _) <- runFrame ctx inp0 windowUi
  let Rect wx wy _ _ = respRect win0
      grab = V2 (wx + 4) (wy + 1.5)
      press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press windowUi
  let moved =
        press
          { inputMousePos = V2 (wx + 4 - 10) (wy + 1.5 + 4)
          , inputMousePressed = False
          }
  (_, _, drawW1, _) <- runFrame ctx moved windowUi
  overW1 <- collectOverlayTextSpans ctx moved
  cellsW1 <- rasterizeLayered (round tw) (round th) drawW1 [] overW1
  when (not (pairsOk cellsW1 overW1)) $ bump failed
  case closeSpanPos overW0 of
    Nothing -> bump failed
    Just (cx, cy) ->
      case closeSpanStart overW1 of
        Nothing -> bump failed
        Just x1 -> do
          when (x1 >= cx - 2) $ bump failed
          let leftover =
                any
                  ( \x ->
                      x >= 0
                        && x < gridW cellsW1
                        && cy >= 0
                        && cy < cellsH cellsW1
                        && let c1 = cellChar cellsW1 x cy
                            in fontAwesomeIcon c1 || c1 == wideTrailChar
                  )
                  [cx, cx + 1]
          when leftover $ bump failed

-- Font Awesome leads that reserve two columns must keep a trail cell.
-- Lone FA spans paint one cell (rectW < 2) and have no trail.
runTerminalWidePairTest :: Context -> IORef Int -> IO ()
runTerminalWidePairTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp = emptyInput {inputWindowSize = Size 80 24}
      ui = do
        _ <- button "OK"
        _ <- button "Cancel"
        _ <- checkbox "Feature" False
        (win, _) <- window True "Debug" (label_ "Body")
        pure win
  _ <- runFrame ctx inp ui
  (_, _, draw, _) <- runFrame ctx inp ui
  (base, overlay) <- collectRasterSpans ctx inp
  cells <- rasterizeLayered 80 24 draw base overlay
  let skip = oneColFaOrigins (base ++ overlay)
      rows = cellRows cells
      broken =
        [ (r, c)
        | (r, rowChars) <- zip [0 :: Int ..] rows
        , (c, ch) <- zip [0 ..] rowChars
        , fontAwesomeIcon ch
        , (c, r) `notElem` skip
        , let nextOk =
                c + 1 < length rowChars
                  && rowChars !! (c + 1) == wideTrailChar
        , not nextOk
        ]
  when (not (null broken)) $ bump failed

runTerminalCloseButtonTest :: Context -> IORef Int -> IO ()
runTerminalCloseButtonTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp0 = emptyInput {inputWindowSize = Size 80 24}
      modalUi =
        column defaultLayout $ do
          (dlg, _) <- modal True "About" (label_ "Body")
          pure dlg
      windowUi = do
        (win, _) <- window True "Debug" (label_ "Body")
        pure win
      testClose ui = do
        _ <- runFrame ctx inp0 ui
        _ <- runFrame ctx inp0 ui
        overlays <- collectOverlayTextSpans ctx inp0
        case closeSpanCenter overlays of
          Nothing -> bump failed
          Just (V2 cx cy) -> do
            -- Left edge of the 3-cell close slot, not the centered glyph.
            let edge = V2 (cx - 1.0) cy
                (press, release) = clickAt inp0 edge
            _ <- runFrame ctx press ui
            (outer, _, _, _) <- runFrame ctx release ui
            when (not (respClicked outer)) $ bump failed
  testClose modalUi
  testClose windowUi

glyphIconTexts :: Icons -> [T.Text]
glyphIconTexts icons =
  [ iconChecked icons
  , iconUnchecked icons
  , iconClose icons
  , iconSelectOpen icons
  , iconSelectClosed icons
  , iconScrollUp icons
  , iconScrollDown icons
  , iconWindowTitle icons
  , iconModalTitle icons
  ]

runIconSetTest :: Context -> IORef Int -> IO ()
runIconSetTest _ failed = do
  let ck cond = when (not cond) (bump failed)
  ck (parseIconSet "nerd" == Just IconsNerd)
  ck (parseIconSet "FontAwesome" == Just IconsFontAwesome)
  ck (parseIconSet " ascii " == Just IconsAscii)
  ck (parseIconSet "auto" == Nothing)
  ck (iconsFor IconsAscii == asciiIcons)
  ck (iconsFor IconsNerd == glyphIcons)
  ck (iconsFor IconsFontAwesome == glyphIcons)
  ck (checkboxMark glyphIcons True == iconChecked glyphIcons)
  -- Prefixes must share a cell width, or toggling would reflow the row.
  ck (terminalTextColumns (iconChecked glyphIcons) == terminalTextColumns (iconUnchecked glyphIcons))

-- Glyph tier: checkbox, select caret and scrollbar caps come from the icon
-- table, and a re-rendered checkbox keeps exactly one prefix.
runTerminalIconChromeTest :: Context -> IORef Int -> IO ()
runTerminalIconChromeTest _ failed = do
  let ck cond = when (not cond) (bump failed)
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp = emptyInput {inputWindowSize = Size 40 30}
      rows = column (fillW defaultLayout) (forM_ [1 .. 40 :: Int] (label_ . T.pack . show))
      ui = column (fillW defaultLayout) $ do
        _ <- checkbox "Feature" False
        _ <- select "Quality" ["Low", "High"] 0
        _ <- scrollArea ((fillW defaultLayout) {layoutHeight = Fixed 20}) rows
        pure ()
  _ <- runFrame ctx inp ui
  (_, _, drawData, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let texts = [txt | (_, txt, _, _, _) <- spans]
      hasGlyph g = any (T.isInfixOf g) texts
  ck (length (filter (== iconUnchecked glyphIcons <> "Feature") texts) == 1)
  ck (hasGlyph (iconSelectClosed glyphIcons))
  -- Scroll caps fit a 1-cell track (lone FA paints one column).
  let Size tw th = inputWindowSize inp
  cells <- rasterizeLayered (round tw) (round th) drawData spans []
  let blob = concat (cellRows cells)
  ck (any (`elem` blob) (concatMap T.unpack [iconChecked glyphIcons]))

-- The close glyph follows the tier; the ASCII default stays "X".
runTerminalIconCloseTest :: Context -> IORef Int -> IO ()
runTerminalIconCloseTest _ failed = do
  term <- newAdaptiveTerminalContext
  let ctx = withIcons term IconsNerd
      inp = emptyInput {inputWindowSize = Size 60 20}
      ui = do
        (win, _) <- window True "Debug" (label_ "Body")
        pure win
  _ <- runFrame ctx inp ui
  _ <- runFrame ctx inp ui
  overlays <- collectOverlayTextSpans ctx inp
  let texts = [T.strip txt | (_, txt, _, _, _) <- overlays]
  when (iconClose glyphIcons `notElem` texts) $ bump failed
  when (not (any (T.isPrefixOf (iconWindowTitle glyphIcons)) texts)) $ bump failed

closeSpanCenter :: [(Rect, T.Text, Color, Color, Rect)] -> Maybe V2
closeSpanCenter spans =
  case [Rect x y w h | (Rect x y w h, txt, _, _, _) <- spans, T.strip txt == "X"] of
    (Rect x y w h : _) -> Just (V2 (x + w / 2) (y + h / 2))
    [] -> Nothing

clickAt :: Input -> V2 -> (Input, Input)
clickAt base pos =
  let press =
        base
          { inputMousePos = pos
          , inputMouseDown = True
          , inputMousePressed = True
          }
      release =
        press
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
   in (press, release)

runTerminalThemeContrastTest :: Context -> IORef Int -> IO ()
runTerminalThemeContrastTest _ failed = do
  checkThemeContrast
    "terminalTheme-fallback"
    (terminalThemeFromColors terminalDefaultFg terminalDefaultBg)
    failed
  checkThemeContrast
    "terminalTheme-light"
    (terminalThemeFromColors (colorRGBA 0 0 0 255) (colorRGBA 255 255 255 255))
    failed
  (fg, bg) <- queryTerminalColors
  checkThemeContrast
    "terminalTheme-adaptive"
    (terminalThemeFromColors fg bg)
    failed
  checkThemeContrast "defaultTheme" defaultTheme failed

checkThemeContrast :: String -> Theme -> IORef Int -> IO ()
checkThemeContrast themeName theme failed =
  mapM_ check (themeContrastPairs theme)
  where
    aa = 4.5
    check (pairName, fg, bg) = do
      let ratio = contrastRatio fg bg
      when (ratio < aa) $ do
        putStrLn $
          "  contrast "
            ++ themeName
            ++ " "
            ++ pairName
            ++ ": "
            ++ show ratio
            ++ " < "
            ++ show aa
        bump failed

-- Each text colour against every fill it is painted on.
themeContrastPairs :: Theme -> [(String, Color, Color)]
themeContrastPairs theme =
  concat
    [ styleStates "panel" (themePanel theme)
    , styleStates "floating-window" (themeFloatingWindow theme)
    , styleStates "button" (themeButton theme)
    , styleStates "input" (themeInput theme)
    , [ ("panel-fg/window", styleFg (themePanel theme), themeWindow theme)
      , ("accent/panel", themeAccent theme, styleBg (themePanel theme))
      , ("accent/window", themeAccent theme, themeWindow theme)
      , ("muted/panel", themeMuted theme, styleBg (themePanel theme))
      , ("muted/window", themeMuted theme, themeWindow theme)
      , ("modal-fg/dim", styleFg (themePanel theme), themeOverlayDim theme)
      , ("modal-muted/dim", themeMuted theme, themeOverlayDim theme)
      , ("modal-accent/dim", themeAccent theme, themeOverlayDim theme)
      , ( "scroll-thumb/floating"
        , scrollBarThumbColor (themeFloatingWindow theme) theme True
        , styleBg (themeFloatingWindow theme)
        )
      , ( "scroll-thumb/track"
        , scrollBarThumbColor (themeFloatingWindow theme) theme True
        , scrollBarTrackColor (themeFloatingWindow theme) theme True
        )
      ]
    ]
  where
    styleStates name style =
      [ (name ++ "-fg/bg", styleFg style, styleBg style)
      , (name ++ "-fg/hover", styleFg style, styleHoverBg style)
      , (name ++ "-fg/active", styleFg style, styleActiveBg style)
      ]

-- Label stays in node text; display must not accumulate "Name: ...: ...".
runTerminalTextInputDisplayTest :: Context -> IORef Int -> IO ()
runTerminalTextInputDisplayTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp = emptyInput {inputWindowSize = Size 40 10}
      ui = column defaultLayout (textInput "Name" "hello")
  _ <- runFrame ctx inp ui
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let shown = [txt | (_, txt, _, _, _) <- spans, "Name:" `T.isPrefixOf` txt]
  case shown of
    [one] -> do
      when (one /= "Name: hello") $ bump failed
      when ("Name: hello: hello" `T.isInfixOf` one) $ bump failed
    _ -> bump failed

runHostSlotTest :: Context -> IORef Int -> IO ()
runHostSlotTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 80 80}
  (miss, _, _, _) <- runFrame ctx inp (askHost :: NanoUI (Maybe String))
  setHost ctx ("ok" :: String)
  setHost ctx (1 :: Int)
  (hitS, _, _, _) <- runFrame ctx inp (askHost :: NanoUI (Maybe String))
  (hitI, _, _, _) <- runFrame ctx inp (askHost :: NanoUI (Maybe Int))
  when (miss /= Nothing || hitS /= Just "ok" || hitI /= Just 1) $ bump failed

-- Large read-heavy state lives in a compact region. GC sees one block.
runCompactHostTest :: Context -> IORef Int -> IO ()
runCompactHostTest ctx failed = do
  let payload = [0 .. 9999] :: [Int]
  _ <- compactHost ctx payload
  let inp = emptyInput {inputWindowSize = Size 80 80}
  (got, _, _, _) <- runFrame ctx inp (askCompact :: NanoUI (Maybe [Int]))
  case got of
    Just xs | length xs == 10000 && last xs == 9999 -> pure ()
    _ -> bump failed

runEmbedStateTest :: Context -> IORef Int -> IO ()
runEmbedStateTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 80 80}
      ui :: Eff '[Ui, State Int, IOE] Int
      ui = do
        modify (+ (1 :: Int))
        modify (+ (1 :: Int))
        get
  (n, _, _, _) <- runFrameEff (runEff . evalState (0 :: Int)) ctx inp ui
  when (n /= 2) $ bump failed

data CounterMsg = Inc | Dec
  deriving (Eq, Show)

data Counter = Counter {counterN :: Int}
  deriving (Eq, Show)

updateCounter :: CounterMsg -> Counter -> Counter
updateCounter Inc m = m {counterN = counterN m + 1}
updateCounter Dec m = m {counterN = counterN m - 1}

-- emit collects typed messages; reduceMessages applies them after the view.
runReduceMessagesTest :: Context -> IORef Int -> IO ()
runReduceMessagesTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 80 80}
      model0 = Counter 0
      view _ = do
        emit Inc
        emit Dec
        emit Inc
        emit ("noise" :: String)
  ((), model1, msgs, _, dirty) <- runFrameReduce updateCounter ctx inp model0 view
  when (msgs /= [Inc, Dec, Inc] || model1 /= Counter 1 || not dirty) $ bump failed

-- Reducer functions themselves are the payload.
runReduceUpdatesTest :: Context -> IORef Int -> IO ()
runReduceUpdatesTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 80 80}
      ui = do
        emit (updateCounter Inc)
        emit (updateCounter Dec)
        emit (updateCounter Inc)
  (_, msgs, _, _) <- runFrame ctx inp ui
  let model1 = reduceUpdates (Counter 0) msgs
  when (model1 /= Counter 1) $ bump failed

-- Click emits Inc; the next view sees the reduced model.
runReduceClickTest :: Context -> IORef Int -> IO ()
runReduceClickTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 240 120}
      view m = do
        resp <- button "Go"
        onClick resp (emit Inc)
        label_ (T.pack (show (counterN m)))
        pure resp
  (resp, model0, _, _, _) <- runFrameReduce updateCounter ctx inp0 (Counter 0) view
  when (model0 /= Counter 0) $ bump failed
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
  (_, modelP, _, _, _) <- runFrameReduce updateCounter ctx press model0 view
  (_, modelR, msgs, _, dirty) <- runFrameReduce updateCounter ctx release modelP view
  when (modelP /= Counter 0 || msgs /= [Inc] || modelR /= Counter 1 || not dirty) $
    bump failed
  (_, model1, _, _, _) <- runFrameReduce updateCounter ctx inp0 modelR view
  when (model1 /= Counter 1) $ bump failed

-- Identity update must not mark dirty (no extra idle frame).
runReduceIdentityTest :: Context -> IORef Int -> IO ()
runReduceIdentityTest ctx failed = do
  let inp = emptyInput {inputWindowSize = Size 80 80}
      view _ = do
        emit Inc
        emit Dec
  ((), model1, msgs, _, dirty) <- runFrameReduce updateCounter ctx inp (Counter 0) view
  when (msgs /= [Inc, Dec] || model1 /= Counter 0 || dirty) $ bump failed

-- Widgets no longer dump String tags onto the app message queue.
runWidgetNoStringEmitTest :: Context -> IORef Int -> IO ()
runWidgetNoStringEmitTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 240 120}
      ui = button "Go"
  (resp, _, _, _) <- runFrame ctx inp0 ui
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
  (_, msgs, _, _) <- runFrame ctx release ui
  when (not (null (decodeMessages msgs :: [String]))) $ bump failed

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

-- Floating windows draw on the overlay, ignore backdrop clicks, and close on the title icon.
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
      hasCloseGlyph = any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays
  when (not (hasTitle && hasBody)) $ bump failed
  when hasCloseGlyph $ bump failed
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
  let closeAt = V2 (wx + ww - padR windowPad - 14) (wy + padT windowPad + 14)
      clickClose =
        inp0
          { inputMousePos = closeAt
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx clickClose ui
  let releaseClose =
        clickClose
          { inputMouseDown = False
          , inputMousePressed = False
          , inputMouseReleased = True
          }
  ((_, winClose, _), _, _, _) <- runFrame ctx releaseClose ui
  when (not (respClicked winClose)) $ bump failed

runWindowDragTest :: Context -> IORef Int -> IO ()
runWindowDragTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 640 400}
      ui = do
        (win, _) <- window True "Debug" (label "Body")
        pure win
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 24) (y0 + 22)
      press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let moved =
        press
          { inputMousePos = V2 (x0 + 24 - 50) (y0 + 22 + 30)
          , inputMousePressed = False
          }
  _ <- runFrame ctx moved ui
  (win1, _, _, _) <- runFrame ctx moved ui
  let Rect x1 y1 _ _ = respRect win1
  when (x1 >= x0 - 10) $ bump failed
  when (y1 <= y0 + 10) $ bump failed

spanYs :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanYs needle spans = [rectY r | (r, txt, _, _, _) <- spans, needle `T.isInfixOf` txt]

spanLabelYs :: T.Text -> [(Rect, T.Text, a, b, c)] -> [Float]
spanLabelYs needle spans = [rectY r | (r, txt, _, _, _) <- spans, txt == needle]

closeSpanBottom :: [(Rect, T.Text, a, b, c)] -> Maybe Float
closeSpanBottom spans =
  case
    [ rectY r + rectH r
    | (r, txt, _, _, _) <- spans
    , "Close" `T.isInfixOf` txt
    , T.strip txt /= "X"
    ]
  of
    [] -> Nothing
    bs -> Just (maximum bs)

-- Wheel over an open floating window must scroll overflowing body content.
-- The title bar stays pinned and does not move with the body.
runWindowScrollWheelTest :: Context -> IORef Int -> IO ()
runWindowScrollWheelTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 320 220}
      line1 = T.pack "line 1"
      ui = do
        (win, _) <-
          window True "Scroll" $
            column defaultLayout $
              mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
        pure win
  _ <- runFrame ctx inp0 ui
  (win, _, _, _) <- runFrame ctx inp0 ui
  let Rect wx _ ww wh = respRect win
  when (ww <= 0 || wh <= 0) $ bump failed
  spans0 <- collectOverlayTextSpans ctx inp0
  let titleYs0 = spanYs (T.pack "Scroll") spans0
      line1Ys0 = spanLabelYs line1 spans0
  when (null titleYs0) $ bump failed
  case line1Ys0 of
    [] -> bump failed
    b0 : _ -> do
      let wheelAt = V2 (wx + ww / 2) (b0 + 2)
          wheel =
            inp0
              { inputMousePos = wheelAt
              , inputScroll = V2 0 1
              }
      _ <- runFrame ctx wheel ui
      spans1 <- collectOverlayTextSpans ctx wheel
      let titleYs1 = spanYs (T.pack "Scroll") spans1
          line1Ys1 = spanLabelYs line1 spans1
      case (titleYs0, titleYs1) of
        (y0 : _, y1 : _) -> when (y1 /= y0) $ bump failed
        _ -> bump failed
      case line1Ys1 of
        [] -> pure ()
        b1 : _ -> when (b1 >= b0) $ bump failed

dragWindowEdge ::
  Context ->
  Input ->
  NanoUI Response ->
  V2 ->
  V2 ->
  IO (Maybe Rect)
dragWindowEdge ctx inp0 ui grab dest = do
  let press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let dragged =
        press
          { inputMousePos = dest
          , inputMousePressed = False
          }
  _ <- runFrame ctx dragged ui
  let idle = inp0 {inputMousePos = dest}
  _ <- runFrame ctx idle ui
  (win, _, _, _) <- runFrame ctx idle ui
  getPrevRect ctx (respId win)

-- Resize from every edge and corner; hover uses directional arrows.
runWindowResizeTest :: Context -> IORef Int -> IO ()
runWindowResizeTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 640 400}
      ui = do
        (win, _) <- window True "Resize" (label "Body")
        pure win
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  mrect0 <- getPrevRect ctx (respId win0)
  case mrect0 of
    Nothing -> bump failed
    Just (Rect x0 y0 w0 h0) -> do
      when (w0 <= 0 || h0 <= 0) $ bump failed
      let hoverAt p = inp0 {inputMousePos = p}
          expectCursor p kind = do
            k <- uiCursorKind ctx (hoverAt p)
            when (k /= kind) $ bump failed
      expectCursor (V2 (x0 + w0 + 4) (y0 + h0 + 4)) UiCursorNwseResize
      expectCursor (V2 (x0 - 4) (y0 - 4)) UiCursorNwseResize
      expectCursor (V2 (x0 + w0 + 4) (y0 - 4)) UiCursorNeswResize
      expectCursor (V2 (x0 - 4) (y0 + h0 + 4)) UiCursorNeswResize
      expectCursor (V2 (x0 + w0 / 2) (y0 - 4)) UiCursorNsResize
      expectCursor (V2 (x0 + w0 / 2) (y0 + h0 + 4)) UiCursorNsResize
      expectCursor (V2 (x0 - 4) (y0 + h0 / 2)) UiCursorEwResize
      expectCursor (V2 (x0 + w0 + 4) (y0 + h0 / 2)) UiCursorEwResize
      expectCursor (V2 (x0 + w0 - 5) (y0 + h0 / 2)) UiCursorEwResize
      insideKind <- uiCursorKind ctx (hoverAt (V2 (x0 + w0 - padR windowPad - 4) (y0 + h0 / 2)))
      when (insideKind == UiCursorEwResize) $ bump failed
      mSe <-
        dragWindowEdge
          ctx
          inp0
          ui
          (V2 (x0 + w0 + 4) (y0 + h0 + 4))
          (V2 (x0 + w0 + 40) (y0 + h0 + 30))
      case mSe of
        Nothing -> bump failed
        Just (Rect x1 y1 w1 h1) -> do
          when (w1 <= w0 + 20) $ bump failed
          when (h1 <= h0 + 15) $ bump failed
          mW <-
            dragWindowEdge
              ctx
              inp0
              ui
              (V2 (x1 - 4) (y1 + h1 / 2))
              (V2 (x1 - 36) (y1 + h1 / 2))
          case mW of
            Nothing -> bump failed
            Just (Rect xw yw ww hw) -> do
              when (ww <= w1 + 15) $ bump failed
              when (xw >= x1 - 10) $ bump failed
              mN <-
                dragWindowEdge
                  ctx
                  inp0
                  ui
                  (V2 (xw + ww / 2) (yw - 4))
                  (V2 (xw + ww / 2) (yw - 20))
              case mN of
                Nothing -> bump failed
                Just (Rect xn yn wn hn) -> do
                  when (hn <= hw + 8) $ bump failed
                  when (yn >= yw - 5) $ bump failed
                  let minTitleH = padT windowPad + 28 + padB windowPad
                  mShort <-
                    dragWindowEdge
                      ctx
                      inp0
                      ui
                      (V2 (xn + wn / 2) (yn + hn + 4))
                      (V2 (xn + wn / 2) (yn + 4))
                  case mShort of
                    Nothing -> bump failed
                    Just (Rect _ _ _ hMin) ->
                      when (hMin + 0.01 < minTitleH) $ bump failed

-- Page widgets in the outside halo keep their hit. Resize does not steal them.
runWindowResizeHaloHitTest :: Context -> IORef Int -> IO ()
runWindowResizeHaloHitTest ctx failed = do
  let inp0 = emptyInput {inputWindowSize = Size 640 400}
      ui = do
        btn <- button "Hit"
        (win, _) <- window True "Resize" (label "Body")
        pure (btn, win)
  _ <- runFrame ctx inp0 ui
  ((btn0, win0), _, _, _) <- runFrame ctx inp0 ui
  let Rect bx by bw bh = respRect btn0
      Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 24) (y0 + 22)
      destX = bx + bw + 4
      press =
        inp0
          { inputMousePos = grab
          , inputMouseDown = True
          , inputMousePressed = True
          }
  _ <- runFrame ctx press ui
  let moved =
        press
          { inputMousePos = V2 (destX + 24) (y0 + 22)
          , inputMousePressed = False
          }
  _ <- runFrame ctx moved ui
  ((_, win1), _, _, _) <- runFrame ctx (inp0 {inputMousePos = V2 destX (y0 + 22)}) ui
  let Rect x1 y1 _ h1 = respRect win1
      hit = V2 (bx + bw - 2) (by + bh - 2)
      inHalo =
        let s = 12
         in (fst2 hit < x1 && fst2 hit >= x1 - s)
              && snd2 hit >= y1 - s
              && snd2 hit <= y1 + h1 + s
      isResize k =
        k == UiCursorEwResize
          || k == UiCursorNsResize
          || k == UiCursorNwseResize
          || k == UiCursorNeswResize
  kind <- uiCursorKind ctx (inp0 {inputMousePos = hit})
  when (abs (x1 - destX) > 8) $ bump failed
  when (not inHalo) $ bump failed
  when (isResize kind) $ bump failed
  where
    fst2 (V2 x _) = x
    snd2 (V2 _ y) = y

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

-- Terminal separators are box-drawing glyphs, not filled hairline quads.
runTerminalSeparatorSpanTest :: Context -> IORef Int -> IO ()
runTerminalSeparatorSpanTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let inp = emptyInput {inputWindowSize = Size 40 8}
      ui =
        column (fillW defaultLayout) $ do
          label_ "A"
          resp <- separator
          label_ "B"
          pure resp
  _ <- runFrame ctx inp ui
  (resp, _, drawData, _) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  let Rect _ _ w h = respRect resp
  when (w < 20) $ bump failed
  when (h > 2) $ bump failed
  let Size tw th = inputWindowSize inp
  cells <- rasterizeLayered (round tw) (round th) drawData spans []
  let blob = concat (cellRows cells)
  when (not ('\x2500' `elem` blob)) $ bump failed
  when (not ("A" `isInfixOf` blob && "B" `isInfixOf` blob)) $ bump failed
