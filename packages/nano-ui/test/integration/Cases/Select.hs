module Cases.Select (tests) where

import Spec
import Data.Primitive.SmallArray qualified as SA

tests :: [Spec]
tests =
  [ pixelSpec "slider-cursor" runSliderCursorTest
  , spec "select-drag-to-select" runSelectDragToSelectTest
  , spec "select-keyboard" runSelectKeyboardTest
  , spec "select-change-once" runSelectChangeOnceTest
  , spec "select-close-keeps-focus" runSelectCloseKeepsFocusTest
  , spec "select-overlay-damage" runSelectOverlayDamageTest
  , spec "select-dropdown-span-colors" runSelectDropdownSpanColorsTest
  , spec "tree-select" runTreeSelectTest
  , spec "tree-keyboard" runTreeKeyboardTest
  , spec "radio-click-marks" runRadioClickMarksTest
  , spec "sliders-drag-apart" runSlidersDragApartTest
  , spec "radio-groups-apart" runRadioGroupsApartTest
  , spec "slider-out-of-range-settles" runSliderOutOfRangeSettlesTest
  ]

-- | Sibling sliders keep separate drags: dragging the second leaves the
-- first where it was.
runSlidersDragApartTest :: Context -> IORef Int -> IO ()
runSlidersDragApartTest ctx failed = do
  let inp0 = withInput 300 120
      ui = column ((,) <$> slider' 0 100 20 <*> slider' 0 100 20)
  (_, (second, _)) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect second
      Rect tx ty tw th = sliderTrackBounds rx ry rw rh
      press = pressAt inp0 (V2 (tx + tw * 0.25) (ty + th / 2))
      drag = press {inputMousePressed = False, inputMousePos = V2 (tx + tw * 0.75) (ty + th / 2)}
  _ <- runFrame ctx press ui
  (((_, v1), (_, v2)), _, _, _) <- runFrame ctx drag ui
  assertEq failed v1 20
  assertGt failed v2 50

-- | Radio groups declared side by side keep separate selections, and a
-- steady frame leaves nothing to redraw.
runRadioGroupsApartTest :: Context -> IORef Int -> IO ()
runRadioGroupsApartTest ctx failed = do
  aRef <- newIORef (0 :: Int)
  bRef <- newIORef (2 :: Int)
  let inp0 = withInput 200 240
      ui = column ((,) <$> held aRef (radio' ["x", "y"]) <*> held bRef (radio' ["p", "q", "r"]))
  _ <- warmup2 ctx inp0 ui
  (_, _, _, dirty) <- runFrame ctx inp0 ui
  assert failed (not dirty)
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "q" spans) $ \r -> do
    _ <- runClick ctx inp0 ui (spanCenter r)
    assertEq failed 0 =<< readIORef aRef
    assertEq failed 1 =<< readIORef bRef

-- | A slider given a value outside its range shows it clamped and settles,
-- rather than adopting the value afresh on every frame.
runSliderOutOfRangeSettlesTest :: Context -> IORef Int -> IO ()
runSliderOutOfRangeSettlesTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column (slider' 0 1 1.2)
  _ <- warmup2 ctx inp0 ui
  ((resp, v), _, _, dirty) <- runFrame ctx inp0 ui
  assertEq failed v 1
  assert failed (not (respChanged resp))
  assert failed (not dirty)

runSliderCursorTest :: Context -> IORef Int -> IO ()
runSliderCursorTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column (slider' 0 100 50)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      trackMid = spanCenter (sliderTrackBounds rx ry rw rh)
      offPos = V2 (rx + rw + 20) (ry + rh + 20)
      pressTrack = pressAt inp0 trackMid
  assertEq failed UiCursorGrab =<< cursorOver ctx inp0 ui trackMid
  _ <- runFrame ctx pressTrack ui
  assert failed =<< cursorKindIs ctx pressTrack UiCursorGrabbing
  assertEq failed UiCursorGrabbing =<< cursorOver ctx pressTrack ui offPos
  assertEq failed UiCursorDefault =<< cursorOver ctx inp0 ui offPos

runSelectOverlayDamageTest :: Context -> IORef Int -> IO ()
runSelectOverlayDamageTest ctx failed = do
  let ui = column (select' ["Low", "Medium", "High"] 0)
      inp0 = (withInput 640 480) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let pos = centerOf resp
      open = snd (clickPair inp0 pos)
  _ <- runClick ctx inp0 ui pos
  let idle = open {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  assertJust failed (rectY <$> spanRect "High" overlays) $ \highY -> do
    let overMenu = idle {inputMousePos = V2 (v2X pos) (highY + 0.5)}
    assert failed =<< needsRedraw ctx idle overMenu
    _ <- runFrame ctx overMenu ui
    dmg <- takeDamage ctx
    -- The hover moved inside the dropdown, which the arena does not hold:
    -- the clip repaints the whole dropdown rather than the whole window.
    assert failed (not (null overlays))
    forM_ overlays $ \(_, _, _, _, dropRect) -> assert failed (clipCovers dmg dropRect)

-- | An open dropdown's text spans carry the colours it is painted in: the
-- picked row's text in the accent colour, the others in the menu's.
runSelectDropdownSpanColorsTest :: Context -> IORef Int -> IO ()
runSelectDropdownSpanColorsTest ctx failed = do
  theme <- getTheme ctx
  let ui = column (select' ["Low", "Medium", "High"] 0)
      inp0 = (withInput 640 480) {inputMousePos = V2 600 400}
  (resp, _) <- warmup2 ctx inp0 ui
  let open = snd (clickPair inp0 (centerOf resp))
  _ <- runClick ctx inp0 ui (centerOf resp)
  let away = open {inputMouseReleased = False, inputMousePos = V2 600 400}
  _ <- runFrame ctx away ui
  overlays <- collectOverlayTextSpans ctx away
  let fgOf lbl = [fg | (_, txt, fg, _, _) <- overlays, txt == lbl]
  assertEq failed (fgOf "Low") [themeAccent theme]
  assertEq failed (length (fgOf "Medium")) 1
  assert failed (themeAccent theme `notElem` fgOf "Medium")

runTreeSelectTest :: Context -> IORef Int -> IO ()
runTreeSelectTest ctx failed = do
  let inp0 = withInput 40 12
      items = [TreeItem "alpha" [], TreeItem "beta" []]
      ui = column (tree' "t" items 0)
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
  selRef <- newIORef (0 :: Int)
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
  selectedRef <- newIORef 0
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

-- Open dropdown rows show the pointer cursor on hover and press, and
-- respChanged fires on the frame the selection changes and not on later
-- frames. Reducer adapters must therefore emit only once per selection change.
runSelectChangeOnceTest :: Context -> IORef Int -> IO ()
runSelectChangeOnceTest ctx failed = do
  indexRef <- newIORef 1
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
      frame inp = (\((r, i), _, _, _) -> (respChanged r, i)) <$> runFrame ctx inp ui
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
      ui = select' ["Low", "Medium", "High"] 1
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
    let drag = inp0 {inputMousePos = V2 (sx + sw / 2) (lowY + 0.5), inputMouseDown = True}
    _ <- runFrame ctx drag ui
    overlaysDrag <- collectOverlayTextSpans ctx drag
    assert failed (hasText "Low" overlaysDrag)
    assertEq failed UiCursorPointer =<< uiCursorKind ctx drag
    -- 3. Mouseup over the item selects it and closes the menu
    let release = drag {inputMouseDown = False, inputMouseReleased = True}
    assertEq failed 0 . snd =<< evalUi ctx release ui
    overlaysClosed <- collectOverlayTextSpans ctx release
    assert failed (not (hasText "Low" overlaysClosed))
    spans <- collectTextSpans ctx
    assertSpansHas failed "Low" spans

runSelectKeyboardTest :: Context -> IORef Int -> IO ()
runSelectKeyboardTest ctx failed = do
  indexRef <- newIORef 1
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
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyEscape], inputMouseReleased = False}) ui
  let idleAfterOpen = openRelease {inputMouseReleased = False}
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

-- Clicking an open select's own field closes the dropdown and keeps the
-- select focused.
runSelectCloseKeepsFocusTest :: Context -> IORef Int -> IO ()
runSelectCloseKeepsFocusTest ctx failed = do
  indexRef <- newIORef 1
  let inp0 = withInput 320 200
      ui = column (held indexRef (select' (SA.smallArrayFromList ["Low", "Medium", "High"])))
      listed spans = any (\(_, txt, _, _, _) -> txt == "Low") spans
  (resp, _) <- warmup2 ctx inp0 ui
  let mid = centerOf resp
      openRelease = snd (clickPair inp0 mid)
      closeRelease = snd (clickPair openRelease mid)
  _ <- runClick ctx inp0 ui mid
  assert failed . listed =<< collectOverlayTextSpans ctx openRelease
  _ <- runClick ctx openRelease ui mid
  let idle = closeRelease {inputMouseReleased = False}
  _ <- runFrame ctx idle ui
  assert failed . not . listed =<< collectOverlayTextSpans ctx idle
  assertEq failed (respId resp) =<< getFocusId ctx
