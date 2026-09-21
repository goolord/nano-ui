module Cases.Select (tests) where

import Spec
import Data.Text qualified as T
import Data.Primitive.SmallArray qualified as SA

tests :: [Spec]
tests =
  [ pixelSpec "slider-cursor" runSliderCursorTest
  , spec "select-drag-to-select" runSelectDragToSelectTest
  , spec "select-keyboard" runSelectKeyboardTest
  , spec "select-change-once" runSelectChangeOnceTest
  , spec "select-close-keeps-focus" runSelectCloseKeepsFocusTest
  , spec "select-overlay-damage" runSelectOverlayDamageTest
  , spec "tree-select" runTreeSelectTest
  , spec "tree-keyboard" runTreeKeyboardTest
  ]

runSliderCursorTest :: Context -> IORef Int -> IO ()
runSliderCursorTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column (slider' 0 100 50)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      track = sliderTrackBounds rx ry rw rh
      trackMid = spanCenter track
      offPos = V2 (rx + rw + 20) (ry + rh + 20)
      hoverTrack = inp0 {inputMousePos = trackMid}
  _ <- runFrame ctx hoverTrack ui
  hoverKind <- uiCursorKind ctx hoverTrack
  assertEq failed hoverKind UiCursorGrab
  let pressTrack = hoverTrack {inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx pressTrack ui
  grabbing <- cursorKindIs ctx pressTrack UiCursorGrabbing
  assert failed grabbing
  let dragOff = pressTrack {inputMousePos = offPos}
  _ <- runFrame ctx dragOff ui
  grabbingOff <- cursorKindIs ctx dragOff UiCursorGrabbing
  assert failed grabbingOff
  let hoverOff = inp0 {inputMousePos = offPos}
  _ <- runFrame ctx hoverOff ui
  isDefault <- cursorKindIs ctx hoverOff UiCursorDefault
  assert failed isDefault

runSelectOverlayDamageTest :: Context -> IORef Int -> IO ()
runSelectOverlayDamageTest ctx failed = do
  let ui = column (select' ["Low", "Medium", "High"] 0)
      inp0 = (withInput 320 160) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let pos = centerOf resp
      open = snd (clickPair inp0 pos)
  _ <- runClick ctx inp0 ui pos
  let idle = open {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  assertJust failed (rectY <$> spanRect "High" overlays) $ \highY -> do
    let overMenu = idle {inputMousePos = V2 (v2X pos) (highY + 0.5)}
    need <- needsRedraw ctx idle overMenu
    assert failed need
    _ <- runFrame ctx overMenu ui
    dmg <- takeDamage ctx
    assertEq failed dmg DamageFull

runTreeSelectTest :: Context -> IORef Int -> IO ()
runTreeSelectTest ctx failed = do
  let inp0 = withInput 40 12
      items = [TreeItem "alpha" [], TreeItem "beta" []]
      ui = column (tree' "t" items 0)
  (resp, sel0) <- warmup2 ctx inp0 ui
  assertEq failed sel0 0
  let Rect rx ry _rh rh = respRect resp
      (press, release) = clickPair inp0 (V2 (rx + 1) (ry + rh * 0.75))
  _ <- runFrame ctx press ui
  ((_, sel), _, _, _) <- runFrame ctx release ui
  assertEq failed sel 1

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
  ((_, sel1), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed sel1 1
  ((_, sel0), _, _, _) <- runFrame ctx (keyInp KeyUp inp0) ui
  assertEq failed sel0 0
  _ <- runFrame ctx (keyInp KeyDown inp0) ui
  ((_, parentSel), _, _, _) <- runFrame ctx (keyInp KeyLeft inp0) ui
  assertEq failed parentSel 0
  _ <- runFrame ctx (keyInp KeyEnter inp0) ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  assert failed (not (hasText "child" spans))
  ((_, afterCollapsed), _, _, _) <- runFrame ctx (keyInp KeyDown inp0) ui
  assertEq failed afterCollapsed 2

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
  let
    (openPress, openRelease) = clickPair inp0 (centerOf resp)
  _ <- runFrame ctx openPress ui
  _ <- runFrame ctx openRelease ui
  overlays <- collectOverlayTextSpans ctx openRelease
  assertJust failed (rectY <$> spanRect "Low" overlays) $ \lowY -> do
    let
      lowPos = V2 (v2X (centerOf resp)) (lowY + 0.5)
      hover = inp0 {inputMousePos = lowPos}
      (pickPress, pickRelease) = clickPair inp0 lowPos
      frame inp = (\((r, i), _, _, _) -> (respChanged r, i)) <$> runFrame ctx inp ui
    _ <- runFrame ctx hover ui
    hoverKind <- uiCursorKind ctx hover
    assertEq failed hoverKind UiCursorPointer
    pressed <- frame pickPress
    pressKind <- uiCursorKind ctx pickPress
    assertEq failed pressKind UiCursorPointer
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
  assert failed (any (\(_, txt, _, _, _) -> "Low" `T.isInfixOf` txt) overlaysPress)
  assert failed (any (\(_, txt, _, _, _) -> "High" `T.isInfixOf` txt) overlaysPress)
  assertJust failed (rectY <$> spanRect "Low" overlaysPress) $ \lowY -> do
    -- 2. Move mouse over an item while still pressed
    let drag = inp0 {inputMousePos = V2 (sx + sw / 2) (lowY + 0.5), inputMouseDown = True}
    _ <- runFrame ctx drag ui
    overlaysDrag <- collectOverlayTextSpans ctx drag
    assert failed (any (\(_, txt, _, _, _) -> "Low" `T.isInfixOf` txt) overlaysDrag)
    kind <- uiCursorKind ctx drag
    assertEq failed kind UiCursorPointer
    -- 3. Mouseup over the item selects it and closes the menu
    let release = drag {inputMouseDown = False, inputMouseReleased = True}
    ((_, idx1), _, _, _) <- runFrame ctx release ui
    assertEq failed idx1 0
    overlaysClosed <- collectOverlayTextSpans ctx release
    assert failed (not (any (\(_, txt, _, _, _) -> "Low" `T.isInfixOf` txt) overlaysClosed))
    spans <- collectTextSpans ctx
    assertSpansHas failed "Low" spans

runSelectKeyboardTest :: Context -> IORef Int -> IO ()
runSelectKeyboardTest ctx failed = do
  indexRef <- newIORef 1
  let inp0 = withInput 320 200
      ui = column (held indexRef (select' (SA.smallArrayFromList ["Low", "Medium", "High"])))
  (resp, idx0) <- warmup2 ctx inp0 ui
  assertEq failed idx0 1
  let (openPress, openRelease) = clickPair inp0 (centerOf resp)
  _ <- runFrame ctx openPress ui
  _ <- runFrame ctx openRelease ui
  _ <- runFrame ctx (keyInp KeyDown openRelease) ui
  ((_, idx1), _, _, _) <- runFrame ctx openRelease ui
  assertEq failed idx1 2
  _ <- runFrame ctx (keyInp KeyUp openRelease) ui
  ((_, idx2), _, _, _) <- runFrame ctx openRelease ui
  assertEq failed idx2 1
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyEscape], inputMouseReleased = False}) ui
  let idleAfterOpen = openRelease {inputMouseReleased = False}
  _ <- runFrame ctx idleAfterOpen ui
  overlays <- collectOverlayTextSpans ctx idleAfterOpen
  assert failed (not (any (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"]) overlays))
  _ <- runFrame ctx (tabInp inp0) ui
  focus <- getFocusId ctx
  assert failed (focus /= WidgetId 0)
  _ <- runFrame ctx (keyInp KeyRight inp0) ui
  ((_, idx3), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed idx3 2
  closedOverlays <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"]) closedOverlays))
  _ <- runFrame ctx (keyInp KeyLeft inp0) ui
  ((_, idx4), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed idx4 1

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
  focus <- getFocusId ctx
  assertEq failed focus (respId resp)
