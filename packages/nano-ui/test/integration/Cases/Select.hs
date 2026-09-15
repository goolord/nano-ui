module Cases.Select
  ( runSelectDragToSelectTest
  , runSelectKeyboardTest
  , runSelectOverlayDamageTest
  , runSelectChangeOnceTest
  , runSliderCursorTest
  , runTreeKeyboardTest
  , runTreeSelectTest
  ) where

import Data.IORef (IORef, newIORef)
import Data.Text qualified as T
import NanoUI
import Data.Vector qualified as V
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness
  ( assertSpansHas
  , clickPair
  , hasText
  , held
  , runClickRelease
  , warmup2
  )

runSliderCursorTest :: Context -> IORef Int -> IO ()
runSliderCursorTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column (slider' 0 100 50)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      track = sliderTrackBounds (ctxFontMetrics ctx) rx ry rw rh
      trackMid = V2 (rectX track + rectW track / 2) (rectY track + rectH track / 2)
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
  let Rect sx sy sw sh = respRect resp
  open <- runClickRelease ctx inp0 ui (V2 (sx + sw / 2) (sy + sh / 2))
  let idle = open {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  case [rectY r | (r, txt, _, _, _) <- overlays, "High" `T.isInfixOf` txt] of
    (highY : _) -> do
      let overMenu = idle {inputMousePos = V2 (sx + sw / 2) (highY + 0.5)}
      need <- needsRedraw ctx idle overMenu
      assert failed need
      _ <- runFrame ctx overMenu ui
      dmg <- takeDamage ctx
      assertEq failed dmg DamageFull
    [] -> assert failed False

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
  let items = V.fromList [TreeItem "root" [TreeItem "child" []], TreeItem "leaf" []]
      ui = column (held selectedRef (tree' "k" items))
      inp0 = withInput 40 12
  _ <- warmup2 ctx inp0 ui
  spans0 <- collectTextSpans ctx
  assert failed (hasText "root" spans0 && hasText "child" spans0 && hasText "leaf" spans0)
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  ((_, sel1), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDown]}) ui
  assertEq failed sel1 1
  ((_, sel0), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyUp]}) ui
  assertEq failed sel0 0
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDown]}) ui
  ((_, parentSel), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyLeft]}) ui
  assertEq failed parentSel 0
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyEnter]}) ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  assert failed (not (hasText "child" spans))
  ((_, afterCollapsed), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDown]}) ui
  assertEq failed afterCollapsed 2

-- Open dropdown rows show the pointer cursor on hover and press, and
-- respChanged fires on the frame the selection changes and not on later
-- frames (regression: it compared the index against the initial one, so it
-- stayed set, and Emit.select emitted, every frame after a pick).
runSelectChangeOnceTest :: Context -> IORef Int -> IO ()
runSelectChangeOnceTest ctx failed = do
  indexRef <- newIORef 1
  let inp0 = withInput 320 200
      ui = held indexRef (select' ["Low", "Medium", "High"])
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect sx sy sw sh = respRect resp
      (openPress, openRelease) = clickPair inp0 (V2 (sx + sw / 2) (sy + sh / 2))
  _ <- runFrame ctx openPress ui
  _ <- runFrame ctx openRelease ui
  overlays <- collectOverlayTextSpans ctx openRelease
  case [rectY r | (r, txt, _, _, _) <- overlays, "Low" `T.isInfixOf` txt] of
    (lowY : _) -> do
      let lowPos = V2 (sx + sw / 2) (lowY + 0.5)
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
      let results = pressed : rest
      assertEq failed (map snd rest) [0, 0, 0, 0]
      assertEq failed (length (filter fst results)) 1
      assertEq failed (map fst (drop 1 rest)) [False, False, False]
    [] -> assert failed False

runSelectDragToSelectTest :: Context -> IORef Int -> IO ()
runSelectDragToSelectTest ctx failed = do
  let inp0 = withInput 320 200
      ui = select' ["Low", "Medium", "High"] 1
  (resp, idx0) <- warmup2 ctx inp0 ui
  assertEq failed idx0 1
  let Rect sx sy sw _ = respRect resp
      btnMid = V2 (sx + sw / 2) (sy + 10)
      press = inp0 {inputMousePos = btnMid, inputMouseDown = True, inputMousePressed = True}
  -- 1. On mousedown, the menu should show up immediately
  _ <- runFrame ctx press ui
  overlaysPress <- collectOverlayTextSpans ctx press
  assert failed (any (\(_, txt, _, _, _) -> "Low" `T.isInfixOf` txt) overlaysPress)
  assert failed (any (\(_, txt, _, _, _) -> "High" `T.isInfixOf` txt) overlaysPress)
  case [rectY r | (r, txt, _, _, _) <- overlaysPress, "Low" `T.isInfixOf` txt] of
    (lowY : _) -> do
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
    _ -> assert failed False

runSelectKeyboardTest :: Context -> IORef Int -> IO ()
runSelectKeyboardTest ctx failed = do
  indexRef <- newIORef 1
  let inp0 = withInput 320 200
      ui = column (held indexRef (select' (V.fromList ["Low", "Medium", "High"])))
  (resp, idx0) <- warmup2 ctx inp0 ui
  assertEq failed idx0 1
  let Rect sx sy sw sh = respRect resp
      (openPress, openRelease) = clickPair inp0 (V2 (sx + sw / 2) (sy + sh / 2))
  _ <- runFrame ctx openPress ui
  _ <- runFrame ctx openRelease ui
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyDown]}) ui
  ((_, idx1), _, _, _) <- runFrame ctx openRelease ui
  assertEq failed idx1 2
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyUp]}) ui
  ((_, idx2), _, _, _) <- runFrame ctx openRelease ui
  assertEq failed idx2 1
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyEscape], inputMouseReleased = False}) ui
  let idleAfterOpen = openRelease {inputMouseReleased = False}
  _ <- runFrame ctx idleAfterOpen ui
  overlays <- collectOverlayTextSpans ctx idleAfterOpen
  assert failed (not (any (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"]) overlays))
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus <- getFocusId ctx
  assert failed (focus /= WidgetId 0)
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyRight]}) ui
  ((_, idx3), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed idx3 2
  closedOverlays <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"]) closedOverlays))
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyLeft]}) ui
  ((_, idx4), _, _, _) <- runFrame ctx inp0 ui
  assertEq failed idx4 1
