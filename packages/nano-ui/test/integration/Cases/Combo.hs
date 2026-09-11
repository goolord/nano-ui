module Cases.Combo
  ( runComboBlurCommitTest
  , runComboEscapeRevertTest
  , runComboFilterTest
  , runComboFocusedDropdownTest
  , runComboHoverHighlightTest
  , runComboInitialTest
  , runComboKeyboardPickTest
  , runComboMousePickTest
  , runComboScrollbarDragTest
  , runComboWheelScrollTest
  , runComboWheelXTest
  , runComboWordKeysTest
  ) where

import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness (clickPair, warmup2)

comboOpts :: [T.Text]
comboOpts = ["Alpha Sans", "Beta Serif", "Gamma Mono", "Delta Round"]

-- Enough options to overflow the dropdown's visible window (8 rows). Names
-- are zero-padded so no name is a substring of another.
comboLongOpts :: [T.Text]
comboLongOpts =
  [ "Fam " <> (if i < 10 then "0" else "") <> T.pack (show i)
  | i <- [1 .. 12 :: Int]
  ]

-- Unfocused, the combo is just a search field: the value is visible, nothing
-- is highlighted, and no dropdown overlay exists.
runComboInitialTest :: Context -> IORef Int -> IO ()
runComboInitialTest ctx failed = do
  let inp0 = withInput 320 100
      ui = comboBox "Font" comboOpts "Inter"
  _ <- runFrame ctx inp0 ui
  ((r, t), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed t "Inter"
  assert failed (not (respChanged r))
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> "Alpha Sans" `T.isInfixOf` txt) overlays))
  spans <- collectTextSpans ctx
  assert failed (any (\(_, txt, _, _, _) -> "Inter" `T.isInfixOf` txt) spans)

-- The dropdown is visible exactly while the field holds focus: Tab opens it,
-- Escape dismisses it by clearing focus.
runComboFocusedDropdownTest :: Context -> IORef Int -> IO ()
runComboFocusedDropdownTest ctx failed = do
  let inp0 = withInput 320 100
      ui = comboBox "Font" comboOpts ""
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx inp0 ui
  overlaysOpen <- collectOverlayTextSpans ctx inp0
  assert failed (any (\(_, txt, _, _, _) -> "Alpha Sans" `T.isInfixOf` txt) overlaysOpen)
  assert failed (any (\(_, txt, _, _, _) -> "Delta Round" `T.isInfixOf` txt) overlaysOpen)
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyEscape]}) ui
  overlaysClosed <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> "Alpha Sans" `T.isInfixOf` txt) overlaysClosed))
  focus <- getFocusId ctx
  assertEq failed focus (WidgetId 0)

-- Typing filters the suggestion list case-insensitively and never selects
-- anything on its own: Enter with no highlight leaves the typed text alone.
runComboFilterTest :: Context -> IORef Int -> IO ()
runComboFilterTest ctx failed = do
  let inp0 = withInput 320 100
      ui = comboBox "Font" comboOpts ""
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx (inp0 {inputChars = "ga"}) ui
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (any (\(_, txt, _, _, _) -> "Gamma Mono" `T.isInfixOf` txt) overlays)
  assert failed (not (any (\(_, txt, _, _, _) -> "Alpha Sans" `T.isInfixOf` txt) overlays))
  ((_, t), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyEnter]}) ui
  assertEq failed t "ga"

-- Up/Down move the keyboard highlight (Down from nothing selects the first
-- row), and Enter commits it.
runComboKeyboardPickTest :: Context -> IORef Int -> IO ()
runComboKeyboardPickTest ctx failed = do
  let inp0 = withInput 320 100
      ui = comboBox "Font" comboOpts ""
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDown]}) ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDown]}) ui
  ((r, t), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyEnter]}) ui
  assert failed (respChanged r)
  assertEq failed t "Beta Serif"
  spans <- collectTextSpans ctx
  assert failed (any (\(_, txt, _, _, _) -> "Beta Serif" `T.isInfixOf` txt) spans)

-- Clicking a suggestion row commits its option text.
runComboMousePickTest :: Context -> IORef Int -> IO ()
runComboMousePickTest ctx failed = do
  let inp0 = withInput 320 200
      ui = comboBox "Font" comboOpts ""
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  case [r | (r, txt, _, _, _) <- overlays, "Beta Serif" `T.isInfixOf` txt] of
    (rowRect : _) -> do
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
      assert failed (not (any (\(_, txt, _, _, _) -> "Alpha Sans" `T.isInfixOf` txt) overlaysClosed))
    _ -> assert failed False

-- Hovering a suggestion row highlights it (hover paint, becomes the Enter
-- target) but never commits by itself; Enter then commits the hovered row.
runComboHoverHighlightTest :: Context -> IORef Int -> IO ()
runComboHoverHighlightTest ctx failed = do
  let inp0 = withInput 320 200
      ui = comboBox "Font" comboOpts ""
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  case [r | (r, txt, _, _, _) <- overlays, "Delta Round" `T.isInfixOf` txt] of
    (rowRect : _) -> do
      let hover =
            inp0
              { inputMousePos = V2 (rectX rowRect + rectW rowRect / 2) (rectY rowRect + rectH rowRect / 2)
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
      ((r1, t1), _, _, _) <- runFrame ctx (hover {inputKeys = inputKeysFromList [KeyEnter]}) ui
      assert failed (respChanged r1)
      assertEq failed t1 "Delta Round"
    _ -> assert failed False

-- Dragging the vertical scrollbar thumb scrolls the list, and releasing the
-- drag over a row must not commit it.
runComboScrollbarDragTest :: Context -> IORef Int -> IO ()
runComboScrollbarDragTest ctx failed = do
  let inp0 = withInput 320 300
      ui = comboBox "Fonts" comboLongOpts ""
  (resp, _) <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  assert failed (any (\(_, txt, _, _, _) -> "Fam 01" `T.isInfixOf` txt) overlays0)
  let Rect rx ry rw rh = respRect resp
      -- Mirrors the overlay geometry: gap 4, item 28, lane 10 wide, rows
      -- flush at the drop rect's top (no outer margin).
      dropY = ry + rh + 4
      trackX = rx + rw - 5
      press = inp0 {inputMousePos = V2 trackX (dropY + 200), inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx press ui
  spansDrag <- collectTextSpans ctx
  _ <- runFrame ctx press {inputMousePressed = False} ui
  -- Release over a row position (bottom of the list): must not pick.
  let release = press {inputMouseDown = False, inputMouseReleased = True}
  ((r, t), _, _, _) <- runFrame ctx release ui
  assert failed (T.null t && not (respChanged r))
  overlays1 <- collectOverlayTextSpans ctx inp0
  case [r' | (r', txt, _, _, _) <- overlays1, "Fam 01" `T.isInfixOf` txt] of
    [] -> assert failed (any (\(_, txt, _, _, _) -> "Fam 12" `T.isInfixOf` txt) overlays1)
    (_ : _) -> assert failed (not (null spansDrag))

-- Horizontal wheel scrolls the widest rows that overflow the dropdown width.
runComboWheelXTest :: Context -> IORef Int -> IO ()
runComboWheelXTest ctx failed = do
  let inp0 = withInput 200 160
      long = "A Very Long Font Family Name That Overflows"
      ui = comboBox "Fonts" (comboLongOpts ++ [long]) ""
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  row0 <- case [r | (r, txt, _, _, _) <- overlays0, "Fam 01" `T.isInfixOf` txt] of
    (r : _) -> pure r
    _ -> assert failed False >> pure (Rect 0 0 0 0)
  let overList = inp0 {inputMousePos = V2 (rectX row0 + 4) (rectY row0 + rectH row0 / 2)}
  _ <- runFrame ctx overList ui
  _ <- runFrame ctx overList {inputScroll = V2 5 0} ui
  overlays1 <- collectOverlayTextSpans ctx overList
  case ([r | (r, txt, _, _, _) <- overlays1, "Fam 01" `T.isInfixOf` txt], [r | (r, txt, _, _, _) <- overlays0, "Fam 01" `T.isInfixOf` txt]) of
    ((after : _), (before : _)) -> assert failed (rectX after < rectX before - 50)
    _ -> assert failed False
  -- The x-shift is clamped: a huge wheel does not push rows out of reach.
  _ <- runFrame ctx overList {inputScroll = V2 1000 0} ui
  overlays2 <- collectOverlayTextSpans ctx overList
  assert failed (any (\(_, txt, _, _, _) -> "Fam 01" `T.isInfixOf` txt) overlays2)

-- Word-wise editing keys (Ctrl+Backspace etc.) work in the combo field like
-- in the plain text input, and never commit.
runComboWordKeysTest :: Context -> IORef Int -> IO ()
runComboWordKeysTest ctx failed = do
  let inp0 = withInput 320 200
      ui = comboBox "Font" comboOpts ""
      ctrlMods = Modifiers False True False
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx (inp0 {inputChars = "foo bar"}) ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyBackspace], inputModifiers = ctrlMods}) ui
  ((r, t), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed t "foo "
  assert failed (not (respChanged r))

-- Typing edits the live text without committing; losing focus commits it.
runComboBlurCommitTest :: Context -> IORef Int -> IO ()
runComboBlurCommitTest ctx failed = do
  let inp0 = withInput 320 200
      ui = column (comboBox "Font" comboOpts "")
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  ((rA, tA), _, _, _) <- runFrame ctx (inp0 {inputChars = "N"}) ui
  assertEq failed tA "N"
  assert failed (not (respChanged rA))
  ((rB, tB), _, _, _) <- runFrame ctx (inp0 {inputChars = "o"}) ui
  assertEq failed tB "No"
  assert failed (not (respChanged rB))
  -- Click far away: focus clears after the UI pass, and the frame after the
  -- blur commits the typed text.
  let away = inp0 {inputMousePos = V2 310 5, inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx away ui
  ((rC, tC), _, _, _) <- runFrame ctx inp0 {inputMouseReleased = True} ui
  assertEq failed tC "No"
  assert failed (respChanged rC)
  ((rD, _), _, _, _) <- runFrame ctx inp0 ui
  assert failed (not (respChanged rD))

-- Escape cancels: the live text reverts to the last committed value without
-- a commit pulse, and the dropdown closes.
runComboEscapeRevertTest :: Context -> IORef Int -> IO ()
runComboEscapeRevertTest ctx failed = do
  let inp0 = withInput 320 200
      ui = comboBox "Font" comboOpts "Inter"
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx (inp0 {inputChars = "No"}) ui
  ((r, t), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyEscape]}) ui
  assert failed (not (respChanged r))
  assertEq failed t "Inter"
  focus <- getFocusId ctx
  assertEq failed focus (WidgetId 0)
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> "Alpha Sans" `T.isInfixOf` txt) overlays))

-- The wheel scrolls the suggestion list while the pointer is over the open
-- dropdown: the visible window slides past the first rows.
runComboWheelScrollTest :: Context -> IORef Int -> IO ()
runComboWheelScrollTest ctx failed = do
  let inp0 = withInput 320 260
      ui = comboBox "Fonts" comboLongOpts ""
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  _ <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  assert failed (any (\(_, txt, _, _, _) -> "Fam 01" `T.isInfixOf` txt) overlays0)
  assert failed (not (any (\(_, txt, _, _, _) -> "Fam 09" `T.isInfixOf` txt) overlays0))
  case [r | (r, txt, _, _, _) <- overlays0, "Fam 01" `T.isInfixOf` txt] of
    (rowRect : _) -> do
      let overList =
            inp0
              { inputMousePos = V2 (rectX rowRect + rectW rowRect / 2) (rectY rowRect + rectH rowRect / 2)
              }
      _ <- runFrame ctx overList ui
      _ <- runFrame ctx overList {inputScroll = V2 0 1} ui
      overlays1 <- collectOverlayTextSpans ctx overList
      -- One wheel notch scrolls three rows past "Fam 01".
      assert failed (not (any (\(_, txt, _, _, _) -> "Fam 01" `T.isInfixOf` txt) overlays1))
      assert failed (any (\(_, txt, _, _, _) -> "Fam 04" `T.isInfixOf` txt) overlays1)
      assert failed (any (\(_, txt, _, _, _) -> "Fam 11" `T.isInfixOf` txt) overlays1)
    _ -> assert failed False
