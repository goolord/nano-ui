module Cases.Select
  ( runSelectDropdownCursorTest
  , runSelectDropdownHoverTest
  , runSelectDropdownTest
  , runSelectDropFlushTest
  , runSelectKeyboardTest
  , runSelectOverlayDamageTest
  , runSelectPickLowTest
  , runSelectTest
  , runSliderCursorTest
  , runTreeExpandDamageTest
  , runTreeInitialTest
  , runTreeKeyboardTest
  , runTreeSelectTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, withInput)
import NanoUI.Testing.Harness
  ( assertSpansHas
  , clickPair
  , runClickRelease
  , warmup2
  , withInputOff
  )
import NanoUI.Testing.Term (newAdaptiveTerminalContext)

runSelectDropdownCursorTest :: Context -> IORef Int -> IO ()
runSelectDropdownCursorTest ctx failed = do
  let inp0 = withInput 320 200
      ui = column defaultLayout (select "Quality" ["Low", "High"] 0)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect sx sy sw sh = respRect resp
      (press, release) = clickPair inp0 (V2 (sx + sw / 2) (sy + sh / 2))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  overlaysOpen <- collectOverlayTextSpans ctx release
  case [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt] of
    (lowY : _) -> do
      let hover = inp0 {inputMousePos = V2 (sx + sw / 2) (lowY + 0.5)}
      _ <- runFrame ctx hover ui
      kind <- uiCursorKind ctx hover
      assertEq failed kind UiCursorPointer
      let press' = hover {inputMouseDown = True, inputMousePressed = True}
      _ <- runFrame ctx press' ui
      pressKind <- uiCursorKind ctx press'
      assertEq failed pressKind UiCursorPointer
    _ -> assert failed False

runSliderCursorTest :: Context -> IORef Int -> IO ()
runSliderCursorTest ctx failed = do
  let inp0 = withInput 300 80
      ui = column defaultLayout (slider "Volume" 0 100 50)
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect rx ry rw rh = respRect resp
      track = sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Volume" rx ry rw rh
      trackMid = V2 (rectX track + rectW track / 2) (rectY track + rectH track / 2)
      labelPos = V2 (rx + 4) (ry + 4)
      hoverTrack = inp0 {inputMousePos = trackMid}
  _ <- runFrame ctx hoverTrack ui
  hoverKind <- uiCursorKind ctx hoverTrack
  assertEq failed hoverKind UiCursorGrab
  let pressTrack = hoverTrack {inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx pressTrack ui
  grabbing <- cursorKindIs ctx pressTrack UiCursorGrabbing
  assert failed grabbing
  let dragOff = pressTrack {inputMousePos = labelPos}
  _ <- runFrame ctx dragOff ui
  grabbingOff <- cursorKindIs ctx dragOff UiCursorGrabbing
  assert failed grabbingOff
  let hoverLabel = inp0 {inputMousePos = labelPos}
  _ <- runFrame ctx hoverLabel ui
  isDefault <- cursorKindIs ctx hoverLabel UiCursorDefault
  assert failed isDefault

runSelectOverlayDamageTest :: Context -> IORef Int -> IO ()
runSelectOverlayDamageTest _ failed = do
  ctx <- newContext
  let ui = column defaultLayout (select "Quality" ["Low", "Medium", "High"] 0)
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

runSelectTest :: Context -> IORef Int -> IO ()
runSelectTest ctx failed = do
  _ <- runFrame ctx (withInput 320 80) (column defaultLayout (select "Quality" ["Low", "Medium", "High"] 1))
  spans <- collectTextSpans ctx
  assertSpansHas failed "Quality: Medium" spans

runSelectDropdownTest :: Context -> IORef Int -> IO ()
runSelectDropdownTest ctx failed = do
  let inp0 = withInput 320 80
      ui = column defaultLayout (select "Quality" ["Low", "High"] 0)
      (press, release) = clickPair inp0 (V2 10 10)
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  overlays <- collectOverlayTextSpans ctx release
  assert failed (any (\(_, txt, _, _, _) -> "Low" `T.isInfixOf` txt) overlays)
  assert failed (any (\(_, txt, _, _, _) -> "High" `T.isInfixOf` txt) overlays)

runTreeInitialTest :: Context -> IORef Int -> IO ()
runTreeInitialTest _ failed = do
  ctx <- newContext
  let items = [TreeItem "root" [TreeItem "child" []], TreeItem "leaf" []]
  _ <- runFrame ctx (withInput 40 12) (column defaultLayout (void (tree "t" items 0)))
  spans <- collectTextSpans ctx
  let texts = [txt | (_, txt, _, _, _) <- spans]
  assert failed (any ("root" `T.isInfixOf`) texts && any ("child" `T.isInfixOf`) texts && any ("leaf" `T.isInfixOf`) texts)

runTreeSelectTest :: Context -> IORef Int -> IO ()
runTreeSelectTest _ failed = do
  ctx <- newContext
  let inp0 = withInput 40 12
      items = [TreeItem "alpha" [], TreeItem "beta" []]
      ui = column defaultLayout (tree "t" items 0)
  (resp, sel0) <- warmup2 ctx inp0 ui
  assertEq failed sel0 0
  let Rect rx ry _rh rh = respRect resp
      (press, release) = clickPair inp0 (V2 (rx + 1) (ry + rh * 0.75))
  _ <- runFrame ctx press ui
  ((_, sel), _, _, _) <- runFrame ctx release ui
  assertEq failed sel 1

runTreeExpandDamageTest :: Context -> IORef Int -> IO ()
runTreeExpandDamageTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let items = [TreeItem "root" [TreeItem "child" []], TreeItem "leaf" []]
      ui = column defaultLayout (void (tree "t" items 0))
      inp0 = withInputOff 40 12
  _ <- runFrame ctx inp0 ui >> takeDamage ctx
  _ <- runFrame ctx inp0 ui >> takeDamage ctx
  _ <- runFrame ctx inp0 ui
  dIdle <- takeDamage ctx
  assert failed (dIdle /= DamageFull)
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "root" `T.isInfixOf` txt] of
    (Rect x y _w h : _) -> do
      let (press, release) = clickPair inp0 (V2 (x + 0.5) (y + h / 2))
      _ <- runFrame ctx press ui
      _ <- runFrame ctx release ui
      spansClick <- collectTextSpans ctx
      assert failed (any (\(_, t, _, _, _) -> "child" `T.isInfixOf` t) spansClick)
      dClick <- takeDamage ctx
      case dClick of
        DamageFull -> assert failed False
        DamageClip r -> assert failed (not (damageIsEmpty dClick) && rectW r < 40 && rectH r < 12)
      _ <- runFrame ctx press ui
      _ <- runFrame ctx release ui
      spansNext <- collectTextSpans ctx
      assert failed (not (any (\(_, t, _, _, _) -> "child" `T.isInfixOf` t) spansNext))
      assert failed (any (\(_, t, _, _, _) -> "root" `T.isInfixOf` t) spansNext)
      dNext <- takeDamage ctx
      case dNext of
        DamageFull -> assert failed False
        DamageClip r -> assert failed (not (damageIsEmpty dNext) && rectW r < 40 && rectH r < 12)
      _ <- runFrame ctx inp0 ui
      _ <- runFrame ctx inp0 ui
      dSettled <- takeDamage ctx
      assert failed (dSettled /= DamageFull)
    _ -> assert failed False

runTreeKeyboardTest :: Context -> IORef Int -> IO ()
runTreeKeyboardTest _ failed = do
  ctx <- newContext
  let items = [TreeItem "root" [TreeItem "child" []], TreeItem "leaf" []]
      ui = column defaultLayout (tree "k" items 0)
      inp0 = withInput 40 12
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  ((_, sel1), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyDown]}) ui
  assertEq failed sel1 1
  ((_, sel0), _, _, _) <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyUp]}) ui
  assertEq failed sel0 0
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyEnter]}) ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  assert failed (not (any (\(_, t, _, _, _) -> "child" `T.isInfixOf` t) spans))

runSelectDropdownHoverTest :: Context -> IORef Int -> IO ()
runSelectDropdownHoverTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let layout = defaultLayout {layoutPadding = Padding 0 0 0 0, layoutGap = 0}
      inp0 = withInput 40 6
      ui = column layout (select "Quality" ["Low", "High"] 0)
      (press, release) = clickPair inp0 (V2 1 0.5)
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  let hoverBase = release {inputMouseReleased = False, inputMousePressed = False, inputMouseDown = False}
  overlaysOpen <- collectOverlayTextSpans ctx hoverBase
  case [rectY r | (r, txt, _, _, _) <- overlaysOpen, "High" `T.isInfixOf` txt] of
    (highY : _) -> do
      let hoverHigh = hoverBase {inputMousePos = V2 1 (highY + 0.5)}
      _ <- runFrame ctx hoverHigh ui
      overlaysHigh <- collectOverlayTextSpans ctx hoverHigh
      let bgFor needle spans = [bg | (_, txt, _, bg, _) <- spans, needle `T.isInfixOf` txt]
      case (bgFor "Low" overlaysHigh, bgFor "High" overlaysHigh) of
        ([lowBg], [highBg]) -> assert failed (lowBg /= highBg)
        _ -> assert failed False
      case ([rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt], bgFor "High" overlaysHigh) of
        ((lowY : _), [highHoverBg]) -> do
          let hoverLow = hoverBase {inputMousePos = V2 1 (lowY + 0.5)}
          _ <- runFrame ctx hoverLow ui
          overlaysLow <- collectOverlayTextSpans ctx hoverLow
          case bgFor "Low" overlaysLow of
            [lowHoverBg] -> assertEq failed lowHoverBg highHoverBg
            _ -> assert failed False
        _ -> assert failed False
    _ -> assert failed False

runSelectDropFlushTest :: Context -> IORef Int -> IO ()
runSelectDropFlushTest ctx failed = do
  let inp0 = withInput 320 200
      ui = select "Quality" ["Low", "High"] 1
  (resp, _) <- warmup2 ctx inp0 ui
  let Rect sx sy sw sh = respRect resp
      (press, release) = clickPair inp0 (V2 (sx + sw / 2) (sy + sh / 2))
  _ <- runFrame ctx press ui
  _ <- runFrame ctx release ui
  overlays <- collectOverlayTextSpans ctx release
  case [rectY r | (r, txt, _, _, _) <- overlays, "Low" `T.isInfixOf` txt] of
    (lowY : _) -> assert failed (lowY - (sy + sh) <= 12)
    [] -> assert failed False

runSelectPickLowTest :: Context -> IORef Int -> IO ()
runSelectPickLowTest ctx failed = do
  let inp0 = withInput 320 200
      ui = select "Quality" ["Low", "Medium", "High"] 1
  (resp, idx0) <- warmup2 ctx inp0 ui
  assertEq failed idx0 1
  let Rect sx sy sw _ = respRect resp
      (openPress, openRelease) = clickPair inp0 (V2 (sx + sw / 2) (sy + 10))
  _ <- runFrame ctx openPress ui
  _ <- runFrame ctx openRelease ui
  overlaysOpen <- collectOverlayTextSpans ctx openRelease
  case [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt] of
    (lowY : _) -> do
      let (pickPress, pickRelease) = clickPair inp0 (V2 (sx + sw / 2) (lowY + 0.5))
      _ <- runFrame ctx pickPress ui
      focusAfterPick <- getFocusId ctx
      assert failed (hashWidgetId focusAfterPick /= 0)
      ((_, idx1), _, _, _) <- runFrame ctx pickRelease ui
      assertEq failed idx1 0
      spans <- collectTextSpans ctx
      assertSpansHas failed "Quality: Low" spans
    _ -> assert failed False

runSelectKeyboardTest :: Context -> IORef Int -> IO ()
runSelectKeyboardTest ctx failed = do
  let inp0 = withInput 320 200
      ui = column defaultLayout (select "Quality" ["Low", "Medium", "High"] 1)
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
