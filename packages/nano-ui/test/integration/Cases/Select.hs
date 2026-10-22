module Cases.Select (tests) where

import Spec
import Data.Primitive.SmallArray qualified as SA
import NanoUI.Internal.Store (anySelectOpen)

tests :: [Spec]
tests =
  [ pixelSpec "slider-cursor" runSliderCursorTest
  , spec "select-drag-to-select" runSelectDragToSelectTest
  , spec "select-keyboard" runSelectKeyboardTest
  , spec "select-keyboard-jumps" runSelectKeyboardJumpsTest
  , spec "select-closes-under-modal" runSelectClosesUnderModalTest
  , spec "select-change-once" runSelectChangeOnceTest
  , spec "select-close-keeps-focus" runSelectCloseKeepsFocusTest
  , spec "select-overlay-damage" runSelectOverlayDamageTest
  , spec "select-dropdown-span-colors" runSelectDropdownSpanColorsTest
  , spec "select-dropdown-flips" runSelectDropdownFlipsTest
  , spec "select-dropdown-off-field" runSelectDropdownOffFieldTest
  , spec "select-dropdown-scrolled-away" runSelectDropdownScrolledAwayTest
  , spec "select-keys-after-focus-moves" runSelectKeysAfterFocusMovesTest
  , spec "select-dropdown-clips-rows" runSelectDropdownClipsRowsTest
  , spec "choices-keep-unreached-index" runChoicesKeepUnreachedIndexTest
  , spec "choices-adopt-shown-pick" runChoicesAdoptShownPickTest
  , spec "select-end-adopts-shown" runSelectEndAdoptsShownTest
  , spec "tree-select" runTreeSelectTest
  , spec "tree-keyboard" runTreeKeyboardTest
  , spec "tree-late-items-expanded" runTreeLateItemsExpandedTest
  , spec "radio-click-marks" runRadioClickMarksTest
  , spec "sliders-drag-apart" runSlidersDragApartTest
  , spec "radio-groups-apart" runRadioGroupsApartTest
  , spec "slider-out-of-range-settles" runSliderOutOfRangeSettlesTest
  , spec "slider-bad-bounds-settle" runSliderBadBoundsSettleTest
  , spec "slider-stale-drag" (runStaleDragTest (slider' 0 100) (40, 0))
  , spec "knob-stale-drag" (runStaleDragTest (knob' 0 100) (0, -30))
  ]

-- | Sibling sliders keep separate drags: dragging the second leaves the
-- first where it was.
runSlidersDragApartTest :: Context -> IORef Int -> IO ()
runSlidersDragApartTest ctx failed = do
  let inp0 = withInput 300 120
      ui = column ((,) <$> withValue 20 (slider' 0 100) <*> withValue 20 (slider' 0 100))
  (_, (second, _)) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect second
      Rect tx ty tw th = sliderTrackBounds rx ry rw rh
      press = pressAt inp0 (V2 (tx + tw * 0.25) (ty + th / 2))
      drag = press {inputButtonsPressed = noButtons, inputMousePos = V2 (tx + tw * 0.75) (ty + th / 2)}
  _ <- runFrame ctx press ui
  (((_, v1), (_, v2)), _, _) <- runFrame ctx drag ui
  assertEq failed v1 20
  assertGt failed v2 50

-- | Radio groups declared side by side keep separate selections, and a
-- steady frame leaves nothing to redraw.
runRadioGroupsApartTest :: Context -> IORef Int -> IO ()
runRadioGroupsApartTest ctx failed = do
  aRef <- newVar (0 :: Int)
  bRef <- newVar (2 :: Int)
  let inp0 = withInput 200 240
      ui = column ((,) <$> held aRef (radio' ["x", "y"]) <*> held bRef (radio' ["p", "q", "r"]))
  _ <- warmup2 ctx inp0 ui
  (_, _, dirty) <- runFrame ctx inp0 ui
  assert failed (not dirty)
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "q" spans) $ \r -> do
    _ <- runClick ctx inp0 ui (spanCenter r)
    assertEq failed 0 =<< readVarIO aRef
    assertEq failed 1 =<< readVarIO bRef

-- | A slider given a value outside its range shows it clamped and settles,
-- rather than adopting the value afresh on every frame.
runSliderOutOfRangeSettlesTest :: Context -> IORef Int -> IO ()
runSliderOutOfRangeSettlesTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column (withValue 1.2 (slider' 0 1))
  _ <- warmup2 ctx inp0 ui
  ((resp, v), _, dirty) <- runFrame ctx inp0 ui
  assertEq failed v 1
  assert failed (not (respChanged resp))
  assert failed (not dirty)

-- | A slider given a NaN bound, equal bounds or a reversed range settles, and
-- a reversed range keeps a value inside it.
runSliderBadBoundsSettleTest :: Context -> IORef Int -> IO ()
runSliderBadBoundsSettleTest _ failed = do
  let inp0 = withInput 300 80
      nan = 0 / 0
  forM_ [(nan, 1, 0.5), (0, nan, 0.5), (5, 5, 5), (100, 0, 50)] $ \(lo, hi, given) -> do
    ctx <- newContext
    let ui = column (withValue given (slider' lo hi))
    _ <- warmup2 ctx inp0 ui
    ((resp, v), _, dirty) <- runFrame ctx inp0 ui
    assert failed (not (respChanged resp) && not dirty)
    when (lo > hi) $ assertEq failed given v

-- | A drag released while its slider, or its knob (whose drag 'useDrag2DOn'
-- runs), is not built stays over: a later press elsewhere, held while the
-- widget comes back, leaves its value alone. The drag moves the pointer by
-- @(dx, dy)@.
runStaleDragTest :: (Var Float -> NanoUI Response) -> (Float, Float) -> Context -> IORef Int -> IO ()
runStaleDragTest widget (dx, dy) ctx failed = do
  shownRef <- newIORef True
  let inp0 = withInput 300 200
      ui = column $ do
        shown <- liftIO (readIORef shownRef)
        if shown then snd <$> withValue 50 widget else pure 50
      frameShown shown i = writeIORef shownRef shown >> evalUi ctx i ui
  (resp, _) <- warmup2 ctx inp0 (column (withValue 50 widget))
  let V2 x y = centerOf resp
      hold = holdAt inp0 (V2 (x + dx) (y + dy))
      elsewhere = pressAt inp0 (V2 200 150)
  _ <- frameShown True (pressAt inp0 (V2 x y))
  dragged <- frameShown True hold
  assertGt failed dragged 50
  _ <- frameShown False hold
  _ <- frameShown False (releaseAt hold)
  _ <- frameShown False elsewhere
  assertEq failed 50 =<< frameShown True (holdAt elsewhere (V2 200 100))

runSliderCursorTest :: Context -> IORef Int -> IO ()
runSliderCursorTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column (withValue 50 (slider' 0 100))
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      trackMid = spanCenter (sliderTrackBounds rx ry rw rh)
      offPos = V2 (rx + rw + 20) (ry + rh + 20)
      pressTrack = pressAt inp0 trackMid
  assertEq failed UiCursorGrab =<< cursorOver ctx inp0 ui trackMid
  _ <- runFrame ctx pressTrack ui
  assert failed =<< cursorKindIs ctx pressTrack UiCursorGrabbing
  assertEq failed UiCursorGrabbing =<< cursorOver ctx (holdAt inp0 offPos) ui offPos
  assertEq failed UiCursorDefault =<< cursorOver ctx inp0 ui offPos

runSelectOverlayDamageTest :: Context -> IORef Int -> IO ()
runSelectOverlayDamageTest ctx failed = do
  let ui = column (withValue 0 (select' ["Low", "Medium", "High"]))
      inp0 = (withInput 640 480) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let pos = centerOf resp
      open = snd (clickPair inp0 pos)
  _ <- runClick ctx inp0 ui pos
  let idle = open {inputButtonsReleased = noButtons, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  assertJust failed (rectY <$> spanRect "High" overlays) $ \highY -> do
    let overMenu = idle {inputMousePos = V2 (v2X pos) (highY + 0.5)}
    assert failed =<< needsRedraw ctx idle overMenu
    _ <- runFrame ctx overMenu ui
    dmg <- takeDamage ctx
    -- The hover moved inside the dropdown, which the arena does not hold:
    -- the clip repaints the whole dropdown rather than the whole window.
    forM_ overlays $ \(_, _, _, _, dropRect) -> assert failed (clipCovers dmg dropRect)

-- | An open dropdown's text spans carry the colours it is painted in: the
-- picked row's text in the accent colour, the others in the menu's.
runSelectDropdownSpanColorsTest :: Context -> IORef Int -> IO ()
runSelectDropdownSpanColorsTest ctx failed = do
  theme <- getTheme ctx
  let ui = column (withValue 0 (select' ["Low", "Medium", "High"]))
      inp0 = (withInput 640 480) {inputMousePos = V2 600 400}
  (resp, _) <- warmup2 ctx inp0 ui
  let open = snd (clickPair inp0 (centerOf resp))
  _ <- runClick ctx inp0 ui (centerOf resp)
  let away = open {inputButtonsReleased = noButtons, inputMousePos = V2 600 400}
  _ <- runFrame ctx away ui
  overlays <- collectOverlayTextSpans ctx away
  let fgOf lbl = [fg | (_, txt, fg, _, _) <- overlays, txt == lbl]
  assertEq failed (fgOf "Low") [themeAccent theme]
  assertEq failed (length (fgOf "Medium")) 1
  assert failed (themeAccent theme `notElem` fgOf "Medium")

-- | A select near the window's bottom edge opens its dropdown above itself,
-- where it fits, and a click there picks the row under it.
runSelectDropdownFlipsTest :: Context -> IORef Int -> IO ()
runSelectDropdownFlipsTest ctx failed = do
  let inp0 = withInput 320 140
      ui = column (replicateM_ 4 (label "line") >> withValue 1 (select' ["Low", "Medium", "High"]))
  (resp, _) <- warmup2 ctx inp0 ui
  let open = snd (clickPair inp0 (centerOf resp))
  _ <- runClick ctx inp0 ui (centerOf resp)
  overlays <- collectOverlayTextSpans ctx open
  assertJust failed (spanRect "Low" overlays) $ \low -> do
    assert failed (rectY low + rectH low <= rectY (respRect resp))
    assertEq failed 0 . snd =<< runClick ctx open ui (spanCenter low)

-- | A dropdown that fits on neither side of its select is not slid back over
-- the field, so the click that opens it leaves the value alone.
runSelectDropdownOffFieldTest :: Context -> IORef Int -> IO ()
runSelectDropdownOffFieldTest ctx failed = do
  let inp0 = withInput 320 300
      opts = ["One", "Two", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"]
      ui = column (replicateM_ 4 (label "line") >> withValue 3 (select' opts))
  (resp, _) <- warmup2 ctx inp0 ui
  assertEq failed 3 . snd =<< runClick ctx inp0 ui (centerOf resp)
  assert failed . anySelectOpen =<< getStore ctx

-- | A dropdown goes with its field when the field scrolls out of view: it is
-- not drawn, and a press where it would be picks nothing.
runSelectDropdownScrolledAwayTest :: Context -> IORef Int -> IO ()
runSelectDropdownScrolledAwayTest ctx failed = do
  let inp0 = withInput 320 300
      ui = column $ do
        label "toolbar"
        scrollArea (fixedWH 300 120) (withValue 0 (selectWith' (fixedW 100) ["Low", "Medium", "High"]) <* replicateM_ 30 (label "line"))
  (sid, (resp, _)) <- warmup2 ctx inp0 ui
  let open = snd (clickPair inp0 (centerOf resp))
      -- Over the scroller, beside the dropdown.
      wheel = open {inputButtonsReleased = noButtons, inputMousePos = V2 250 (rectY (respRect resp) + 10), inputScroll = V2 0 5}
      idle = wheel {inputScroll = V2 0 0}
  _ <- runClick ctx inp0 ui (centerOf resp)
  shown <- collectOverlayTextSpans ctx open
  assert failed (hasText "Medium" shown)
  _ <- runFrame ctx wheel ui
  replicateM_ 3 (runFrame ctx idle ui)
  after <- collectOverlayTextSpans ctx idle
  assert failed (not (hasText "Medium" after))
  -- Pressed open and held while its field scrolls just past the top, the
  -- dropdown would lie in the window: a release on its middle row there
  -- picks nothing.
  let past = rectH (respRect resp) + 20
  setScrollOffset ctx sid 0
  _ <- runClick ctx inp0 ui (V2 250 280)
  _ <- runFrame ctx (pressAt inp0 (centerOf resp)) ui
  assert failed . anySelectOpen =<< getStore ctx
  setScrollOffset ctx sid past
  assertJust failed (spanRect "Medium" shown) $ \item -> do
    let V2 cx cy = spanCenter item
        holding = holdAt inp0 (V2 cx (cy - past))
    _ <- runFrame ctx holding ui
    assertEq failed 0 . snd . snd =<< evalUi ctx (releaseAt holding) ui

-- | Keys reach an open select only while it has focus: a right-click on a
-- text field closes it, and a field that takes focus by 'holdFocus' keeps
-- the arrows to itself.
runSelectKeysAfterFocusMovesTest :: Context -> IORef Int -> IO ()
runSelectKeysAfterFocusMovesTest ctx failed = do
  grabRef <- newIORef False
  let inp0 = withInput 320 300
      -- The field above the select, clear of its dropdown.
      ui = column $ do
        (t, _) <- withValue "text" textInput'
        (s, v) <- withValue 0 (select' ["Low", "Medium", "High"])
        grab <- liftIO (readIORef grabRef)
        when grab (holdFocus (respId t))
        pure (s, t, v)
  (s, t, _) <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf s)
  _ <- runRightClick ctx inp0 ui (centerOf t)
  assert failed . not . anySelectOpen =<< getStore ctx
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed 0 . (\(_, _, v) -> v) =<< evalUi ctx inp0 ui
  _ <- runFrame ctx (keyInp KeyEscape inp0) ui
  _ <- runClick ctx inp0 ui (centerOf s)
  writeIORef grabRef True
  _ <- runFrame ctx inp0 ui
  assertEq failed (respId t) =<< getFocusId ctx
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed 0 . (\(_, _, v) -> v) =<< evalUi ctx inp0 ui

-- | A select's dropdown is as wide as the select, and an option wider than
-- that is clipped to the dropdown rather than drawn over the page, where
-- closing the dropdown would not repaint it.
runSelectDropdownClipsRowsTest :: Context -> IORef Int -> IO ()
runSelectDropdownClipsRowsTest ctx failed = do
  let long = "A rather long option label"
      ui = column (withValue 0 (selectWith' (fixedW 80) [long, "Short"]))
      inp0 = (withInput 640 480) {inputMousePos = V2 600 400}
  (resp, _) <- warmup2 ctx inp0 ui
  let open = snd (clickPair inp0 (centerOf resp))
  _ <- runClick ctx inp0 ui (centerOf resp)
  let away = open {inputButtonsReleased = noButtons}
  (_, dd, _) <- runFrame ctx away ui
  overlays <- collectOverlayTextSpans ctx away
  case [(r, dropRect) | (r, txt, _, _, dropRect) <- overlays, txt == long] of
    (r, dropRect) : _ -> do
      assertGt failed (rectX r + rectW r) (rectX dropRect + rectW dropRect)
      let clipOf c = Rect (cmdClipX c) (cmdClipY c) (cmdClipW c) (cmdClipH c)
          overlayCmds = [c | c <- drawCmdElems dd, cmdLayer c == LayerOverlay, cmdIndexCount c > 0]
      assert failed (any ((rectInflate 1 dropRect `covers`) . clipOf) overlayCmds)
    [] -> assert failed False

-- | A select, a radio group and a tree given an index their options do not
-- reach show it clamped but leave the caller's index alone, so a choice
-- restored before the options load is still there when they do.
runChoicesKeepUnreachedIndexTest :: Context -> IORef Int -> IO ()
runChoicesKeepUnreachedIndexTest ctx failed = do
  loadedRef <- newIORef False
  selRef <- newVar 2
  radioRef <- newVar 2
  treeRef <- newVar 2
  let refs = [selRef, radioRef, treeRef]
      inp0 = withInput 320 400
      ui = do
        loaded <- liftIO (readIORef loadedRef)
        let opts = if loaded then ["Low", "Medium", "High"] else []
        column $ do
          select opts selRef
          radio opts radioRef
          tree "t" [TreeItem o [] | o <- opts] treeRef
  _ <- warmup2 ctx inp0 ui
  assertEq failed [2, 2, 2] =<< mapM readVarIO refs
  writeIORef loadedRef True
  _ <- warmup2 ctx inp0 ui
  assertEq failed [2, 2, 2] =<< mapM readVarIO refs
  -- The radio's options, then the tree's rows.
  assertEq failed [0, 0, 1, 0, 0, 1] =<< buttonValues ctx

-- | Picking the option an unreached index shows is still a pick: the
-- select, the radio group and the tree each hand that option back.
runChoicesAdoptShownPickTest :: Context -> IORef Int -> IO ()
runChoicesAdoptShownPickTest ctx failed = do
  selRef <- newVar 5
  radioRef <- newVar 5
  treeRef <- newVar 5
  let inp0 = withInput 320 400
      ui = column $ do
        resp <- select' ["Low", "Medium", "High"] selRef
        radio ["One", "Two", "Three"] radioRef
        tree "t" [TreeItem o [] | o <- ["Red", "Green", "Blue"]] treeRef
        pure resp
  resp <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  forM_ ["Three", "Blue"] $ \lbl ->
    assertJust failed (spanRectOf lbl spans) (void . runClick ctx inp0 ui . spanCenter)
  let open = snd (clickPair inp0 (centerOf resp))
  _ <- runClick ctx inp0 ui (centerOf resp)
  overlays <- collectOverlayTextSpans ctx open
  assertJust failed (spanRectOf "High" overlays) (void . runClick ctx open ui . spanCenter)
  assertEq failed [2, 2, 2] =<< mapM readVarIO [selRef, radioRef, treeRef]

-- | End on a select whose index its options do not reach picks the last
-- option, the one it shows, and reports the change.
runSelectEndAdoptsShownTest :: Context -> IORef Int -> IO ()
runSelectEndAdoptsShownTest ctx failed = do
  selRef <- newVar 5
  let inp0 = withInput 320 200
      ui = column (select' ["Low", "Medium", "High"] selRef)
  warmupFocused ctx inp0 ui
  _ <- runFrame ctx (keyInp KeyEnd inp0) ui
  assert failed . respChanged =<< evalUi ctx inp0 ui
  assertEq failed 2 =<< readVarIO selRef

runTreeSelectTest :: Context -> IORef Int -> IO ()
runTreeSelectTest ctx failed = do
  -- Tall enough to show the clicked row: one past the window's edge is
  -- clipped away and takes no click.
  let inp0 = withInput 200 120
      items = [TreeItem "alpha" [], TreeItem "beta" []]
      ui = column (withValue 0 (tree' "t" items))
  (resp, sel0) <- warmup2 ctx inp0 ui
  assertEq failed sel0 0
  let Rect rx ry _rh rh = respRect resp
  (_, sel) <- runClick ctx inp0 ui (V2 (rx + 1) (ry + rh * 0.75))
  assertEq failed sel 1
  -- The click's frame draws the clicked row selected, though the rows were
  -- added before the click was read.
  assertEq failed [0, 1] =<< buttonValues ctx

-- | A click moves a radio group's mark on the frame it lands.
runRadioClickMarksTest :: Context -> IORef Int -> IO ()
runRadioClickMarksTest ctx failed = do
  selRef <- newVar (0 :: Int)
  let inp0 = withInput 200 120
      ui = column (held selRef (radio' ["one", "two", "three"]))
  _ <- warmup2 ctx inp0 ui
  assertEq failed [1, 0, 0] =<< buttonValues ctx
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "three" spans) $ \r -> do
    (_, sel) <- runClick ctx inp0 ui (spanCenter r)
    assertEq failed sel 2
    assertEq failed [0, 0, 1] =<< buttonValues ctx

-- A tree renders expanded, moves its selection with the arrow keys, and
-- Enter collapses the selected parent.
runTreeKeyboardTest :: Context -> IORef Int -> IO ()
runTreeKeyboardTest ctx failed = do
  selectedRef <- newVar 0
  let items = SA.smallArrayFromList [TreeItem "root" [TreeItem "child" []], TreeItem "leaf" []]
      ui = column (held selectedRef (tree' "k" items))
      inp0 = withInput 40 12
  _ <- warmup2 ctx inp0 ui
  spans0 <- collectTextSpans ctx
  assert failed (hasText "root" spans0 && hasText "child" spans0 && hasText "leaf" spans0)
  _ <- runFrame ctx (tabInp inp0) ui
  assertEq failed 1 . snd =<< evalUi ctx (keyInp KeyDown inp0) ui
  assertEq failed [0, 1, 0] =<< buttonValues ctx
  assertEq failed 0 . snd =<< evalUi ctx (keyInp KeyUp inp0) ui
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed 0 . snd =<< evalUi ctx (keyInp KeyLeft inp0) ui
  _ <- runFrame ctx (keyInp KeyEnter inp0) ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  assert failed (not (hasText "child" spans))
  assertEq failed 2 . snd =<< evalUi ctx (keyInp KeyDown inp0) ui

-- | A tree whose parents arrive after its first frame, empty or holding a
-- placeholder leaf, opens every parent, as one built with them does.
runTreeLateItemsExpandedTest :: Context -> IORef Int -> IO ()
runTreeLateItemsExpandedTest ctx failed = do
  itemsRef <- newIORef []
  let inp0 = withInput 200 120
      ui = do
        items <- liftIO (readIORef itemsRef)
        column (withValue 0 (tree' "late" items))
  _ <- warmup2 ctx inp0 ui
  writeIORef itemsRef [TreeItem "Loading" []]
  _ <- warmup2 ctx inp0 ui
  writeIORef itemsRef [TreeItem "root" [TreeItem "child" []]]
  _ <- warmup2 ctx inp0 ui
  assert failed . hasText "child" =<< collectTextSpans ctx

-- Open dropdown rows show the pointer cursor on hover and press, and
-- respChanged fires on the frame the selection changes and not on later
-- frames. Reducer adapters must therefore emit only once per selection change.
runSelectChangeOnceTest :: Context -> IORef Int -> IO ()
runSelectChangeOnceTest ctx failed = do
  indexRef <- newVar 1
  let
    inp0 = withInput 320 200
    ui = held indexRef (select' ["Low", "Medium", "High"])
  (resp, _) <- warmup2 ctx inp0 ui
  let openRelease = snd (clickPair inp0 (centerOf resp))
  _ <- runClick ctx inp0 ui (centerOf resp)
  overlays <- collectOverlayTextSpans ctx openRelease
  assertJust failed (rectY <$> spanRect "Low" overlays) $ \lowY -> do
    let
      lowPos = V2 (v2X (centerOf resp)) (lowY + 0.5)
      (pickPress, pickRelease) = clickPair inp0 lowPos
      frame inp = (\((r, i), _, _) -> (respChanged r, i)) <$> runFrame ctx inp ui
    assertEq failed UiCursorPointer =<< cursorOver ctx inp0 ui lowPos
    pressed <- frame pickPress
    assertEq failed UiCursorPointer =<< uiCursorKind ctx pickPress
    rest <- mapM frame [pickRelease, inp0, inp0, inp0]
    let
      results = pressed : rest
    assertEq failed (map snd rest) [0, 0, 0, 0]
    assertEq failed (length (filter fst results)) 1
    assertEq failed (map fst (drop 1 rest)) [False, False, False]

runSelectDragToSelectTest :: Context -> IORef Int -> IO ()
runSelectDragToSelectTest ctx failed = do
  let inp0 = withInput 320 200
      ui = withValue 1 (select' ["Low", "Medium", "High"])
  (resp, idx0) <- warmup2 ctx inp0 ui
  assertEq failed idx0 1
  let Rect sx sy sw _ = respRect resp
      btnMid = V2 (sx + sw / 2) (sy + 10)
      press = pressAt inp0 btnMid
  -- 1. On mousedown, the menu should show up immediately
  _ <- runFrame ctx press ui
  overlaysPress <- collectOverlayTextSpans ctx press
  assert failed (hasText "Low" overlaysPress)
  assert failed (hasText "High" overlaysPress)
  assertJust failed (rectY <$> spanRect "Low" overlaysPress) $ \lowY -> do
    -- 2. Move mouse over an item while still pressed
    let drag = inp0 {inputMousePos = V2 (sx + sw / 2) (lowY + 0.5), inputButtonsHeld = buttonsFromList [MouseLeft]}
    _ <- runFrame ctx drag ui
    overlaysDrag <- collectOverlayTextSpans ctx drag
    assert failed (hasText "Low" overlaysDrag)
    assertEq failed UiCursorPointer =<< uiCursorKind ctx drag
    -- 3. Mouseup over the item selects it and closes the menu
    let release = applyMouseButton MouseLeft False drag
    assertEq failed 0 . snd =<< evalUi ctx release ui
    overlaysClosed <- collectOverlayTextSpans ctx release
    assert failed (not (hasText "Low" overlaysClosed))
    spans <- collectTextSpans ctx
    assertSpansHas failed "Low" spans

runSelectKeyboardTest :: Context -> IORef Int -> IO ()
runSelectKeyboardTest ctx failed = do
  indexRef <- newVar 1
  let inp0 = withInput 320 200
      ui = column (held indexRef (select' (SA.smallArrayFromList ["Low", "Medium", "High"])))
  (resp, idx0) <- warmup2 ctx inp0 ui
  assertEq failed idx0 1
  let openRelease = snd (clickPair inp0 (centerOf resp))
  _ <- runClick ctx inp0 ui (centerOf resp)
  _ <- runFrame ctx (keyInp KeyDown openRelease) ui
  assertEq failed 2 . snd =<< evalUi ctx openRelease ui
  _ <- runFrame ctx (keyInp KeyUp openRelease) ui
  assertEq failed 1 . snd =<< evalUi ctx openRelease ui
  _ <- runFrame ctx (keyInp KeyEscape openRelease {inputButtonsReleased = noButtons}) ui
  let idleAfterOpen = openRelease {inputButtonsReleased = noButtons}
  _ <- runFrame ctx idleAfterOpen ui
  overlays <- collectOverlayTextSpans ctx idleAfterOpen
  assert failed (not (any (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"]) overlays))
  _ <- runFrame ctx (tabInp inp0) ui
  focus <- getFocusId ctx
  assert failed (focus /= WidgetId 0)
  _ <- runFrame ctx (keyInp KeyRight inp0) ui
  assertEq failed 2 . snd =<< evalUi ctx inp0 ui
  closedOverlays <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"]) closedOverlays))
  _ <- runFrame ctx (keyInp KeyLeft inp0) ui
  assertEq failed 1 . snd =<< evalUi ctx inp0 ui

-- | A focused select acts on every key it keeps from shortcuts: Home and
-- Page Up pick the first option, End and Page Down the last, and Space or
-- Enter opens the dropdown and closes it again.
runSelectKeyboardJumpsTest :: Context -> IORef Int -> IO ()
runSelectKeyboardJumpsTest ctx failed = do
  indexRef <- newVar 1
  let inp0 = withInput 320 200
      ui = column (held indexRef (select' ["Low", "Medium", "High"]))
  warmupFocused ctx inp0 ui
  forM_ [(KeyEnd, 2), (KeyHome, 0), (KeyPageDown, 2), (KeyPageUp, 0)] $ \(k, want) -> do
    _ <- runFrame ctx (keyInp k inp0) ui
    assertEq failed (k, want) . (k,) . snd =<< evalUi ctx inp0 ui
  forM_ [(KeySpace, True), (KeySpace, False), (KeyEnter, True), (KeyEnter, False)] $ \(k, open) -> do
    _ <- runFrame ctx (keyInp k inp0) ui
    assertEq failed (k, open) . (k,) . anySelectOpen =<< getStore ctx

-- | A modal opened over an open select closes its dropdown, which the modal
-- hides, so Escape then reaches the modal.
runSelectClosesUnderModalTest :: Context -> IORef Int -> IO ()
runSelectClosesUnderModalTest ctx failed = do
  dialog <- newVar False
  let inp0 = withInput 320 200
      ui = column $ do
        (resp, _) <- withValue 0 (select' ["Low", "Medium", "High"])
        modal dialog "Title" (label "body")
        pure resp
  resp <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf resp)
  assert failed . anySelectOpen =<< getStore ctx
  writeVarIO dialog True
  _ <- runFrame ctx inp0 ui
  assert failed . not . anySelectOpen =<< getStore ctx
  _ <- runFrame ctx (keyInp KeyEscape inp0) ui
  assertEq failed False =<< readVarIO dialog

-- Clicking an open select's own field closes the dropdown and keeps the
-- select focused.
runSelectCloseKeepsFocusTest :: Context -> IORef Int -> IO ()
runSelectCloseKeepsFocusTest ctx failed = do
  indexRef <- newVar 1
  let inp0 = withInput 320 200
      ui = column (held indexRef (select' (SA.smallArrayFromList ["Low", "Medium", "High"])))
  (resp, _) <- warmup2 ctx inp0 ui
  let mid = centerOf resp
      openRelease = snd (clickPair inp0 mid)
      closeRelease = snd (clickPair openRelease mid)
  _ <- runClick ctx inp0 ui mid
  assert failed . hasText "Low" =<< collectOverlayTextSpans ctx openRelease
  _ <- runClick ctx openRelease ui mid
  let idle = closeRelease {inputButtonsReleased = noButtons}
  _ <- runFrame ctx idle ui
  assert failed . not . hasText "Low" =<< collectOverlayTextSpans ctx idle
  assertEq failed (respId resp) =<< getFocusId ctx
