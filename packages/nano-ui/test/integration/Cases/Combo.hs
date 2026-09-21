module Cases.Combo (tests) where

import Spec
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "combo-filter" runComboFilterTest
  , spec "combo-keyboard-pick" runComboKeyboardPickTest
  , spec "combo-mouse-pick" runComboMousePickTest
  , spec "combo-blur-commit" runComboBlurCommitTest
  , spec "combo-escape-revert" runComboEscapeRevertTest
  , spec "combo-hover-highlight" runComboHoverHighlightTest
  , spec "combo-scrollbar-drag" runComboScrollbarDragTest
  , spec "combo-wheel-scroll" runComboWheelScrollTest
  ]

comboOpts :: [T.Text]
comboOpts = ["Alpha Sans", "Beta Serif", "Gamma Mono", "Delta Round"]

-- Enough options to overflow the dropdown's visible window (8 rows). Names
-- are zero-padded so no name is a substring of another.
comboLongOpts :: [T.Text]
comboLongOpts =
  [ "Fam " <> (if i < 10 then "0" else "") <> T.pack (show i)
  | i <- [1 .. 12 :: Int]
  ]

-- Typing filters the suggestion list case-insensitively and never selects
-- anything on its own: Enter with no highlight leaves the typed text alone.
runComboFilterTest :: Context -> IORef Int -> IO ()
runComboFilterTest ctx failed = do
  textRef <- newIORef ""
  let inp0 = withInput 320 100
      ui = held textRef (comboBox' "Font" comboOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputChars = "ga"}) ui
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (hasText "Gamma Mono" overlays)
  assert failed (not (hasText "Alpha Sans" overlays))
  ((_, t), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assertEq failed t "ga"

-- Up/Down move the keyboard highlight (Down from nothing selects the first
-- row), and Enter commits it.
runComboKeyboardPickTest :: Context -> IORef Int -> IO ()
runComboKeyboardPickTest ctx failed = do
  let inp0 = withInput 320 100
      ui = comboBox' "Font" comboOpts ""
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  ((r, t), _, _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed (respChanged r)
  assertEq failed t "Beta Serif"
  spans <- collectTextSpans ctx
  assert failed (hasText "Beta Serif" spans)

-- Clicking a suggestion row commits its option text.
runComboMousePickTest :: Context -> IORef Int -> IO ()
runComboMousePickTest ctx failed = do
  let inp0 = withInput 320 200
      ui = comboBox' "Font" comboOpts ""
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Beta Serif" overlays) $ \rowRect -> do
    let cx = rectX rowRect + rectW rowRect / 2
        cy = rectY rowRect + rectH rowRect / 2
        (press, release) = clickPair inp0 (V2 cx cy)
    _ <- runFrame ctx press ui
    ((r, t), _, _, _) <- runFrame ctx release ui
    assert failed (respChanged r)
    assertEq failed t "Beta Serif"
    -- Picking defocuses the field: the dropdown is visible exactly while
    -- focused, so the menu disappears with the pick.
    focus <- getFocusId ctx
    assertEq failed focus (WidgetId 0)
    overlaysClosed <- collectOverlayTextSpans ctx release
    assert failed (not (hasText "Alpha Sans" overlaysClosed))

-- Hovering a suggestion row highlights it (hover paint, becomes the Enter
-- target) but never commits by itself; Enter then commits the hovered row.
runComboHoverHighlightTest :: Context -> IORef Int -> IO ()
runComboHoverHighlightTest ctx failed = do
  let inp0 = withInput 320 200
      ui = comboBox' "Font" comboOpts ""
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Delta Round" overlays) $ \rowRect -> do
    let hover =
          inp0
            { inputMousePos = spanCenter rowRect
            }
    _ <- runFrame ctx hover ui
    -- Hover alone must not commit anything.
    ((r0, t0), _, _, _) <- runFrame ctx hover ui
    assert failed (not (respChanged r0) && T.null t0)
    -- Menu rows show the pointer cursor while hovered.
    ptr <- cursorKindIs ctx hover UiCursorPointer
    assert failed ptr
    -- The hovered row carries the hover background, the others do not.
    overlaysHover <- collectOverlayTextSpans ctx hover
    let bgFor needle = [bg | (_, txt, _, bg, _) <- overlaysHover, needle `T.isInfixOf` txt]
    case (bgFor "Delta Round", bgFor "Alpha Sans") of
      ([dBg], [aBg]) -> assert failed (dBg /= aBg)
      _ -> assert failed False
    -- Enter commits the hovered row.
    ((r1, t1), _, _, _) <- runFrame ctx (keyInp KeyEnter hover) ui
    assert failed (respChanged r1)
    assertEq failed t1 "Delta Round"

-- Dragging the vertical scrollbar thumb scrolls the list, and releasing the
-- drag over a row must not commit it.
runComboScrollbarDragTest :: Context -> IORef Int -> IO ()
runComboScrollbarDragTest ctx failed = do
  let inp0 = withInput 320 300
      ui = comboBox' "Fonts" comboLongOpts ""
  (resp, _) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  assert failed (hasText "Fam 01" overlays0)
  let Rect rx ry rw rh = respRect resp
      -- Mirrors the overlay geometry: gap 4, item 28, lane 10 wide, rows
      -- flush at the drop rect's top (no outer margin).
      dropY = ry + rh + 4
      trackX = rx + rw - 5
      press = pressAt inp0 (V2 trackX (dropY + 200))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx press {inputMousePressed = False} ui
  -- Release over a row position (bottom of the list): must not pick.
  let release = press {inputMouseDown = False, inputMouseReleased = True}
  ((r, t), _, _, _) <- runFrame ctx release ui
  assert failed (T.null t && not (respChanged r))
  overlays1 <- collectOverlayTextSpans ctx inp0
  assert failed (not (hasText "Fam 01" overlays1))
  assert failed (hasText "Fam 12" overlays1)

-- Typing edits the live text without committing, word-wise editing keys
-- (Ctrl+Backspace) work like in the plain text input, and losing focus
-- commits the text.
runComboBlurCommitTest :: Context -> IORef Int -> IO ()
runComboBlurCommitTest ctx failed = do
  textRef <- newIORef ""
  let inp0 = withInput 320 200
      ui = column (held textRef (comboBox' "Font" comboOpts))
  warmupFocused ctx inp0 ui
  ((rA, tA), _, _, _) <- runFrame ctx (inp0 {inputChars = "N"}) ui
  assertEq failed tA "N"
  assert failed (not (respChanged rA))
  ((rB, tB), _, _, _) <- runFrame ctx (inp0 {inputChars = "o"}) ui
  assertEq failed tB "No"
  assert failed (not (respChanged rB))
  _ <- runFrame ctx (inp0 {inputChars = " bar"}) ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyBackspace], inputModifiers = Modifiers False True False}) ui
  ((rW, tW), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed tW "No "
  assert failed (not (respChanged rW))
  _ <- runFrame ctx (keyInp KeyBackspace inp0) ui
  -- Click far away: focus clears after the UI pass, and the frame after the
  -- blur commits the typed text.
  let away = pressAt inp0 (V2 310 5)
  _ <- runFrame ctx away ui
  ((rC, tC), _, _, _) <- runFrame ctx inp0 {inputMouseReleased = True} ui
  assertEq failed tC "No"
  assert failed (respChanged rC)
  ((rD, _), _, _, _) <- runFrame ctx inp0 ui
  assert failed (not (respChanged rD))

-- Unfocused, the combo is just a search field: the value is visible and no
-- dropdown overlay exists. Escape cancels an edit: the live text reverts to
-- the last committed value without a commit pulse, and the dropdown closes.
runComboEscapeRevertTest :: Context -> IORef Int -> IO ()
runComboEscapeRevertTest ctx failed = do
  textRef <- newIORef "Inter"
  let inp0 = withInput 320 200
      ui = held textRef (comboBox' "Font" comboOpts)
  (r0, t0) <- warmup2 ctx inp0 ui
  assertEq failed t0 "Inter"
  assert failed (not (respChanged r0))
  overlays0 <- collectOverlayTextSpans ctx inp0
  assert failed (not (hasText "Alpha Sans" overlays0))
  spans0 <- collectTextSpans ctx
  assert failed (hasText "Inter" spans0)
  _ <- runFrame ctx (tabInp inp0) ui
  _ <- runFrame ctx (inp0 {inputChars = "No"}) ui
  ((r, t), _, _, _) <- runFrame ctx (keyInp KeyEscape inp0) ui
  assert failed (not (respChanged r))
  assertEq failed t "Inter"
  focus <- getFocusId ctx
  assertEq failed focus (WidgetId 0)
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (not (hasText "Alpha Sans" overlays))

-- The wheel scrolls the suggestion list while the pointer is over the open
-- dropdown: a horizontal wheel shifts rows that overflow the dropdown width
-- (clamped), and a vertical notch slides the window past the first rows.
runComboWheelScrollTest :: Context -> IORef Int -> IO ()
runComboWheelScrollTest ctx failed = do
  let inp0 = withInput 200 260
      long = "A Very Long Font Family Name That Overflows"
      ui = comboBox' "Fonts" (comboLongOpts ++ [long]) ""
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  assert failed (hasText "Fam 01" overlays0)
  assert failed (not (hasText "Fam 09" overlays0))
  assertJust failed (spanRect "Fam 01" overlays0) $ \rowRect -> do
    let overList = inp0 {inputMousePos = V2 (rectX rowRect + 4) (rectY rowRect + rectH rowRect / 2)}
    _ <- runFrame ctx overList ui
    _ <- runFrame ctx overList {inputScroll = V2 5 0} ui
    overlaysX <- collectOverlayTextSpans ctx overList
    assertJust failed (spanRect "Fam 01" overlaysX) $ \after -> assert failed (rectX after < rectX rowRect - 50)
    -- The x-shift is clamped: a huge wheel does not push rows out of reach.
    _ <- runFrame ctx overList {inputScroll = V2 1000 0} ui
    overlaysClamped <- collectOverlayTextSpans ctx overList
    assert failed (hasText "Fam 01" overlaysClamped)
    _ <- runFrame ctx overList {inputScroll = V2 0 1} ui
    overlays1 <- collectOverlayTextSpans ctx overList
    -- One wheel notch scrolls three rows past "Fam 01".
    assert failed (not (hasText "Fam 01" overlays1))
    assert failed (hasText "Fam 04" overlays1)
    assert failed (hasText "Fam 11" overlays1)
