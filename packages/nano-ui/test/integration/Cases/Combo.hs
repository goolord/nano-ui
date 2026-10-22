module Cases.Combo (tests) where

import Spec
import NanoUI.Shortcut
import NanoUI.Internal.Input (onMac)
import NanoUI.Widgets.Combo (ComboInput (..), ComboState (..), ComboStep (..), comboStep)
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "combo-filter" runComboFilterTest
  , spec "combo-filter-case-fold" runComboFilterCaseFoldTest
  , spec "combo-keyboard-pick" runComboKeyboardPickTest
  , spec "combo-mouse-pick" runComboMousePickTest
  , spec "combo-dropdown-flips" runComboDropdownFlipsTest
  , spec "combo-dropdown-off-field" runComboDropdownOffFieldTest
  , spec "combo-dropdown-scrolled-away" runComboDropdownScrolledAwayTest
  , spec "combo-blur-commit" runComboBlurCommitTest
  , spec "combo-app-write-no-commit" runComboAppWriteNoCommitTest
  , spec "combo-escape-revert" runComboEscapeRevertTest
  , spec "combo-escape-owned" runComboEscapeOwnedTest
  , spec "combo-hover-highlight" runComboHoverHighlightTest
  , spec "combo-keys-over-still-pointer" runComboKeysOverStillPointerTest
  , spec "combo-scrollbar-drag" runComboScrollbarDragTest
  , spec "combo-track-press-holds" runComboTrackPressHoldsTest
  , spec "combo-wheel-scroll" runComboWheelScrollTest
  , spec "combo-slow-wheel-scroll" runComboSlowWheelScrollTest
  , spec "combo-wheel-at-end-idle" runComboWheelAtEndIdleTest
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
  textRef <- newVar ""
  let inp0 = withInput 320 100
      ui = held textRef (comboBox' "Font" comboOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputChars = "ga"}) ui
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (hasText "Gamma Mono" overlays)
  assert failed (not (hasText "Alpha Sans" overlays))
  assertEq failed "ga" . snd =<< evalUi ctx (keyInp KeyEnter inp0) ui

-- | The filter folds case rather than lowercasing, so "STRASSE" finds
-- "Straße".
runComboFilterCaseFoldTest :: Context -> IORef Int -> IO ()
runComboFilterCaseFoldTest ctx failed = do
  let inp0 = withInput 320 100
      ui = withValue "" (comboBox' "Font" ["Straße Sans", "Beta Serif"])
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputChars = "STRASSE"}) ui
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (hasText "Straße Sans" overlays)
  assert failed (not (hasText "Beta Serif" overlays))

-- Up/Down move the keyboard highlight (Down from nothing selects the first
-- row), and Enter commits it.
runComboKeyboardPickTest :: Context -> IORef Int -> IO ()
runComboKeyboardPickTest ctx failed = do
  let inp0 = withInput 320 100
      ui = withValue "" (comboBox' "Font" comboOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  ((r, t), _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed (respChanged r)
  assertEq failed t "Beta Serif"
  spans <- collectTextSpans ctx
  assert failed (hasText "Beta Serif" spans)

-- Clicking a suggestion row commits its option text.
runComboMousePickTest :: Context -> IORef Int -> IO ()
runComboMousePickTest ctx failed = do
  let inp0 = withInput 320 200
      ui = withValue "" (comboBox' "Font" comboOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Beta Serif" overlays) $ \rowRect -> do
    let release = snd (clickPair inp0 (spanCenter rowRect))
    (r, t) <- runClick ctx inp0 ui (spanCenter rowRect)
    assert failed (respChanged r)
    assertEq failed t "Beta Serif"
    -- Picking defocuses the field: the dropdown is visible exactly while
    -- focused, so the menu disappears with the pick.
    assertEq failed (WidgetId 0) =<< getFocusId ctx
    overlaysClosed <- collectOverlayTextSpans ctx release
    assert failed (not (hasText "Alpha Sans" overlaysClosed))

-- | A combo near the window's bottom edge lists its options above itself,
-- where they fit, and a click there picks the row under it.
runComboDropdownFlipsTest :: Context -> IORef Int -> IO ()
runComboDropdownFlipsTest ctx failed = do
  let inp0 = withInput 320 200
      ui = column (replicateM_ 6 (label "line") >> withValue "" (comboBox' "Font" comboOpts))
  warmupFocused ctx inp0 ui
  (resp, _) <- evalUi ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Beta Serif" overlays) $ \rowRect -> do
    assert failed (rectY rowRect + rectH rowRect <= rectY (respRect resp))
    assertEq failed "Beta Serif" . snd =<< runClick ctx inp0 ui (spanCenter rowRect)

-- | A list that fits on neither side of its combo is not slid back over the
-- field, so the click that focuses the combo commits no suggestion.
runComboDropdownOffFieldTest :: Context -> IORef Int -> IO ()
runComboDropdownOffFieldTest ctx failed = do
  let inp0 = withInput 320 300
      ui = column (replicateM_ 4 (label "line") >> withValue "" (comboBox' "Font" comboLongOpts))
  (resp, _) <- warmup2 ctx inp0 ui
  assertEq failed "" . snd =<< runClick ctx inp0 ui (centerOf resp)
  assertEq failed (respId resp) =<< getFocusId ctx

-- | A focused combo whose field scrolls out of view shows no list, and the
-- pointer where the list would be highlights no row for Enter to commit.
runComboDropdownScrolledAwayTest :: Context -> IORef Int -> IO ()
runComboDropdownScrolledAwayTest ctx failed = do
  let inp0 = withInput 320 300
      ui = column $ do
        label "toolbar"
        scrollArea (fixedWH 300 120) (withValue "" (comboBox' "Font" comboOpts) <* replicateM_ 30 (label "line"))
  (sid, (resp, _)) <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf resp)
  shown <- collectOverlayTextSpans ctx inp0
  -- Scrolled just past its field, the list would lie in the window.
  let past = rectH (respRect resp) + 20
  setScrollOffset ctx sid past
  replicateM_ 2 (runFrame ctx inp0 ui)
  assert failed . not . hasText "Beta Serif" =<< collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Beta Serif" shown) $ \item -> do
    let V2 cx cy = spanCenter item
        hover = inp0 {inputMousePos = V2 cx (cy - past)}
    _ <- runFrame ctx hover ui
    assertEq failed "" . snd . snd =<< evalUi ctx (keyInp KeyEnter hover) ui

-- Hovering a suggestion row highlights it (hover paint, becomes the Enter
-- target) but never commits by itself; Enter then commits the hovered row.
runComboHoverHighlightTest :: Context -> IORef Int -> IO ()
runComboHoverHighlightTest ctx failed = do
  let inp0 = withInput 320 200
      ui = withValue "" (comboBox' "Font" comboOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Delta Round" overlays) $ \rowRect -> do
    let hover = inp0 {inputMousePos = spanCenter rowRect}
    _ <- runFrame ctx hover ui
    -- Hover alone must not commit anything.
    ((r0, t0), _, _) <- runFrame ctx hover ui
    assert failed (not (respChanged r0) && T.null t0)
    -- Menu rows show the pointer cursor while hovered.
    assert failed =<< cursorKindIs ctx hover UiCursorPointer
    -- The hovered row carries the hover background, the others do not.
    overlaysHover <- collectOverlayTextSpans ctx hover
    let bgFor needle = [bg | (_, txt, _, bg, _) <- overlaysHover, needle `T.isInfixOf` txt]
    case (bgFor "Delta Round", bgFor "Alpha Sans") of
      ([dBg], [aBg]) -> assert failed (dBg /= aBg)
      _ -> assert failed False
    -- Enter commits the hovered row.
    ((r1, t1), _, _) <- runFrame ctx (keyInp KeyEnter hover) ui
    assert failed (respChanged r1)
    assertEq failed t1 "Delta Round"

-- Up/Down move the highlight while the pointer rests on a row: a pointer
-- that has not moved does not pull the highlight back to its row, and Enter
-- commits the row the keys reached.
runComboKeysOverStillPointerTest :: Context -> IORef Int -> IO ()
runComboKeysOverStillPointerTest ctx failed = do
  let inp0 = withInput 320 200
      ui = withValue "" (comboBox' "Font" comboOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Beta Serif" overlays) $ \rowRect -> do
    let hover = inp0 {inputMousePos = spanCenter rowRect}
    _ <- runFrame ctx hover ui
    _ <- runFrame ctx hover ui
    _ <- runFrame ctx (keyInp KeyDown hover) ui
    _ <- runFrame ctx hover ui
    ((r, t), _, _) <- runFrame ctx (keyInp KeyEnter hover) ui
    assert failed (respChanged r)
    assertEq failed t "Gamma Mono"

-- Dragging the vertical scrollbar thumb scrolls the list, and releasing the
-- drag over a row must not commit it.
runComboScrollbarDragTest :: Context -> IORef Int -> IO ()
runComboScrollbarDragTest ctx failed = do
  let inp0 = withInput 320 300
      ui = withValue "" (comboBox' "Fonts" comboLongOpts)
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
      -- From the thumb at the lane's top down to the list's bottom.
      drag = holdAt inp0 (V2 trackX (dropY + 200))
  _ <- runFrame ctx (pressAt inp0 (V2 trackX (dropY + 10))) ui
  _ <- runFrame ctx drag ui
  -- Release over a row position (bottom of the list): must not pick.
  ((r, t), _, _) <- runFrame ctx (releaseAt drag) ui
  assert failed (T.null t && not (respChanged r))
  overlays1 <- collectOverlayTextSpans ctx inp0
  assert failed (not (hasText "Fam 01" overlays1))
  assert failed (hasText "Fam 12" overlays1)

-- | A press on the scrollbar track below the thumb jumps the list there, and
-- holding the button still keeps the list where the jump put it.
runComboTrackPressHoldsTest :: Context -> IORef Int -> IO ()
runComboTrackPressHoldsTest _ failed = do
  let fresh =
        ComboState
          { csHighlight = -1
          , csWindow = 0
          , csScrollX = 0
          , csContentW = 50
          , csDrag = 0
          , csDragOff = 0
          , csWheelRem = 0
          , csCommitted = ""
          , csLive = ""
          , csFocused = True
          , csNavPointer = Nothing
          }
      step inp =
        stepState
          . comboStep
            ComboInput
              { ciFocused = True
              , ciEdited = False
              , ciText = ""
              , ciRows = [T.pack (show i) | i <- [1 .. 100 :: Int]]
              , ciContentW = 50
              , ciField = Rect 0 0 200 30
              , ciInput = inp
              }
      -- The dropdown opens 4 px below the field; its scrollbar lane is its
      -- right-most 10 px, and the thumb starts at the lane's top.
      onTrack = V2 195 (34 + 100)
      pressed = step (pressAt (withInput 400 400) onTrack) fresh
      stillHeld = step (holdAt (withInput 400 400) onTrack) pressed
  assertGt failed (csWindow pressed) 0
  assertEq failed (csWindow pressed) (csWindow stillHeld)

-- Typing edits the live text without committing, word-wise editing keys
-- (Ctrl+Backspace, Option+Backspace on macOS) work like in the plain text
-- input, and losing focus commits the text.
runComboBlurCommitTest :: Context -> IORef Int -> IO ()
runComboBlurCommitTest ctx failed = do
  textRef <- newVar ""
  let inp0 = withInput 320 200
      ui = column (held textRef (comboBox' "Font" comboOpts))
  warmupFocused ctx inp0 ui
  ((rA, tA), _, _) <- runFrame ctx (inp0 {inputChars = "N"}) ui
  assertEq failed tA "N"
  assert failed (not (respChanged rA))
  ((rB, tB), _, _) <- runFrame ctx (inp0 {inputChars = "o"}) ui
  assertEq failed tB "No"
  assert failed (not (respChanged rB))
  _ <- runFrame ctx (inp0 {inputChars = " bar"}) ui
  _ <- runFrame ctx (chordInp ((if onMac then alt else ctrl) <> key KeyBackspace) inp0) ui
  ((rW, tW), _, _) <- runFrame ctx inp0 ui
  assertEq failed tW "No "
  assert failed (not (respChanged rW))
  _ <- runFrame ctx (keyInp KeyBackspace inp0) ui
  -- Click far away: focus clears after the UI pass, and the frame after the
  -- blur commits the typed text.
  let away = pressAt inp0 (V2 310 5)
  _ <- runFrame ctx away ui
  ((rC, tC), _, _) <- runFrame ctx (applyMouseButton MouseLeft False inp0) ui
  assertEq failed tC "No"
  assert failed (respChanged rC)
  ((rD, _), _, _) <- runFrame ctx inp0 ui
  assert failed (not (respChanged rD))

-- | A value the application writes shows in the field, but it is not the
-- user's commit: respChanged stays off.
runComboAppWriteNoCommitTest :: Context -> IORef Int -> IO ()
runComboAppWriteNoCommitTest ctx failed = do
  textRef <- newVar ""
  let inp0 = withInput 320 200
      ui = column (held textRef (comboBox' "Font" comboOpts))
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  ((picked, _), _, _) <- runFrame ctx (keyInp KeyEnter inp0) ui
  assert failed (respChanged picked)
  writeVarIO textRef "Reset"
  ((r, t), _, _) <- runFrame ctx inp0 ui
  assertEq failed "Reset" t
  assert failed (not (respChanged r))

-- Unfocused, the combo is just a search field: the value is visible and no
-- dropdown overlay exists. Escape cancels an edit: the live text reverts to
-- the last committed value without a commit pulse, and the dropdown closes.
runComboEscapeRevertTest :: Context -> IORef Int -> IO ()
runComboEscapeRevertTest ctx failed = do
  textRef <- newVar "Inter"
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
  ((r, t), _, _) <- runFrame ctx (keyInp KeyEscape inp0) ui
  assert failed (not (respChanged r))
  assertEq failed t "Inter"
  assertEq failed (WidgetId 0) =<< getFocusId ctx
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (not (hasText "Alpha Sans" overlays))

-- | Escape on a focused combo showing its list cancels the combo and nothing
-- else, wherever another Escape consumer is declared: a popup's dismiss, a
-- shortcut or a key listener, before it or after it, leaves it alone. So do
-- the listeners while the Escape is held, repeating, and on its release.
runComboEscapeOwnedTest :: Context -> IORef Int -> IO ()
runComboEscapeOwnedTest _ failed =
  forM_ [(which, before) | which <- ["popup", "shortcut", "keyPressed", "keyReleased", "keyHeld"], before <- [True, False]] $ \(which, before) -> do
    ctx <- newContext
    fired <- newIORef False
    popupOpen <- newVar True
    let inp0 = withInput 320 200
        listen k = whenM (k KeyEscape) (liftIO (writeIORef fired True))
        other :: NanoUI ()
        other = case which of
          "popup" -> popup popupOpen (defaultPopupConfig (AnchorPoint (V2 300 180))) (label "Panel")
          "shortcut" -> whenM (shortcut (key KeyEscape)) (liftIO (writeIORef fired True))
          "keyPressed" -> listen keyPressed
          "keyReleased" -> listen keyReleased
          _ -> listen keyHeld
        ui = column $ do
          when before other
          r <- withValue "" (comboBox' "Font" comboOpts)
          unless before other
          pure r
        down = chordInp (key KeyEscape) inp0
    warmupFocused ctx inp0 ui
    _ <- runFrame ctx (inp0 {inputChars = "a"}) ui
    _ <- runFrame ctx down ui
    _ <- runFrame ctx (keyRepeatInp KeyEscape down) ui
    _ <- runFrame ctx (keyUpInp KeyEscape down) ui
    open <- readVarIO popupOpen
    acted <- readIORef fired
    assertEq failed (which :: T.Text, before, False) (which, before, acted || not open)
    assertEq failed (WidgetId 0) =<< getFocusId ctx

-- The wheel scrolls the suggestion list while the pointer is over the open
-- dropdown: a horizontal wheel shifts rows that overflow the dropdown width
-- (clamped), and a vertical notch slides the window past the first rows.
runComboWheelScrollTest :: Context -> IORef Int -> IO ()
runComboWheelScrollTest ctx failed = do
  let inp0 = withInput 200 260
      long = "A Very Long Font Family Name That Overflows"
      ui = withValue "" (comboBox' "Fonts" (comboLongOpts ++ [long]))
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

-- | A trackpad's small wheel steps add up: eight eighths of a notch, each
-- under half a row, scroll the list as far as one notch does.
runComboSlowWheelScrollTest :: Context -> IORef Int -> IO ()
runComboSlowWheelScrollTest ctx failed = do
  let inp0 = withInput 200 360
      ui = withValue "" (comboBox' "Fonts" comboLongOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Fam 01" overlays0) $ \rowRect -> do
    let overList = inp0 {inputMousePos = spanCenter rowRect}
    replicateM_ 8 (runFrame ctx overList {inputScroll = V2 0 0.125} ui)
    overlays1 <- collectOverlayTextSpans ctx overList
    assert failed (not (hasText "Fam 03" overlays1))
    assert failed (hasText "Fam 04" overlays1)

-- | A wheel over a list already scrolled to its end moves nothing, and asks
-- for no further frame.
runComboWheelAtEndIdleTest :: Context -> IORef Int -> IO ()
runComboWheelAtEndIdleTest ctx failed = do
  let inp0 = withInput 200 360
      ui = withValue "" (comboBox' "Fonts" comboLongOpts)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  overlays0 <- collectOverlayTextSpans ctx inp0
  assertJust failed (spanRect "Fam 01" overlays0) $ \rowRect -> do
    let overList = inp0 {inputMousePos = spanCenter rowRect}
    _ <- runFrame ctx overList {inputScroll = V2 0 10} ui
    _ <- runFrame ctx overList ui
    (_, _, dirty) <- runFrame ctx overList {inputScroll = V2 0 1} ui
    assert failed (not dirty)
    assert failed . hasText "Fam 12" =<< collectOverlayTextSpans ctx overList
