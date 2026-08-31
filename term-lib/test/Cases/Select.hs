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

import Control.Monad (void, when)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, withInput)
import NanoUI.Testing.Harness
  ( runClickRelease
  , spansHas
  , warmup2
  , withInputOff
  )
import NanoUI.Testing.Term (newAdaptiveTerminalContext)
runSelectDropdownCursorTest :: Context -> IORef Int -> IO ()
runSelectDropdownCursorTest ctx failed = do
  let
    inp0 = withInput 320 200
    ui = column defaultLayout (select "Quality" ["Low", "High"] 0)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect sx sy sw sh = respRect resp
    btn = V2 (sx + sw / 2) (sy + sh / 2)
    openPress =
      inp0
        { inputMousePos = btn
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx openPress ui
  let
    openRelease =
      openPress
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx openRelease ui
  overlaysOpen <- collectOverlayTextSpans ctx openRelease
  let
    lowYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt]
  case lowYs of
    (lowY : _) -> do
      let
        hover =
          inp0
            { inputMousePos = V2 (sx + sw / 2) (lowY + 0.5)
            , inputMouseReleased = False
            , inputMousePressed = False
            , inputMouseDown = False
            }
      _ <- runFrame ctx hover ui
      kind <- uiCursorKind ctx hover
      when (kind /= UiCursorPointer) $ bump failed
      let
        press =
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
  let
    inp0 = withInput 300 80
    ui = column defaultLayout (slider "Volume" 0 100 50)
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect rx ry rw rh = respRect resp
    track =
      sliderTrackBounds (ctxHostProfile ctx) (ctxFontMetrics ctx) "Volume" rx ry rw rh
    trackMid = V2 (rectX track + rectW track / 2) (rectY track + rectH track / 2)
    labelPos = V2 (rx + 4) (ry + 4)
  let
    hoverTrack = inp0 {inputMousePos = trackMid}
  _ <- runFrame ctx hoverTrack ui
  hoverKind <- uiCursorKind ctx hoverTrack
  when (hoverKind /= UiCursorGrab) $ bump failed
  let
    pressTrack =
      hoverTrack
        { inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx pressTrack ui
  grabbing <- cursorKindIs ctx pressTrack UiCursorGrabbing
  failWhen failed (not grabbing)
  let
    dragOff =
      pressTrack
        { inputMousePos = labelPos
        }
  _ <- runFrame ctx dragOff ui
  grabbingOff <- cursorKindIs ctx dragOff UiCursorGrabbing
  failWhen failed (not grabbingOff)
  let
    hoverLabel = inp0 {inputMousePos = labelPos}
  _ <- runFrame ctx hoverLabel ui
  labelKind <- uiCursorKind ctx hoverLabel
  when (labelKind /= UiCursorDefault) $ bump failed

runSelectOverlayDamageTest :: Context -> IORef Int -> IO ()
runSelectOverlayDamageTest _ failed = do
  ctx <- newContext
  let
    ui = column defaultLayout (select "Quality" ["Low", "Medium", "High"] 0)
    inp0 = (withInput 320 160) {inputMousePos = V2 20 20}
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect sx sy sw sh = respRect resp
    click = V2 (sx + sw / 2) (sy + sh / 2)
  open <- runClickRelease ctx inp0 ui click
  let
    idle = open {inputMouseReleased = False, inputDeltaTime = 1}
  _ <- runFrame ctx idle ui
  overlays <- collectOverlayTextSpans ctx idle
  let
    highYs = [rectY r | (r, txt, _, _, _) <- overlays, "High" `T.isInfixOf` txt]
  case highYs of
    [] -> bump failed
    (highY : _) -> do
      let
        overMenu = idle {inputMousePos = V2 (sx + sw / 2) (highY + 0.5)}
      need <- needsRedraw ctx idle overMenu
      failWhen failed (not need)
      _ <- runFrame ctx overMenu ui
      dmg <- takeDamage ctx
      when (dmg /= DamageFull) $ bump failed

runSelectTest :: Context -> IORef Int -> IO ()
runSelectTest ctx failed = do
  let
    inp0 = withInput 320 80
    ui = column defaultLayout (select "Quality" ["Low", "Medium", "High"] 1)
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let
    hasMedium =
      spansHas (T.pack "Quality: Medium") spans
  failWhen failed (not hasMedium)

runSelectDropdownTest :: Context -> IORef Int -> IO ()
runSelectDropdownTest ctx failed = do
  let
    inp0 = withInput 320 80
    ui = column defaultLayout (select "Quality" ["Low", "High"] 0)
  _ <- runFrame ctx inp0 ui
  let
    click = V2 10 10
    inpPress =
      inp0
        { inputMousePos = click
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx inpPress ui
  let
    inpRelease =
      inpPress
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx inpRelease ui
  overlays <- collectOverlayTextSpans ctx inpRelease
  let
    hasLow = any (\(_, txt, _, _, _) -> "Low" `T.isInfixOf` txt) overlays
    hasHigh = any (\(_, txt, _, _, _) -> "High" `T.isInfixOf` txt) overlays
  when (not (hasLow && hasHigh)) $ bump failed

runTreeInitialTest :: Context -> IORef Int -> IO ()
runTreeInitialTest _ failed = do
  ctx <- newContext
  let
    inp0 = withInput 40 12
    items =
      [ TreeItem "root" [TreeItem "child" []]
      , TreeItem "leaf" []
      ]
    ui = column defaultLayout (void (tree "t" items 0))
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  let
    texts = [txt | (_, txt, _, _, _) <- spans]
    hasRoot = any ("root" `T.isInfixOf`) texts
    hasChild = any ("child" `T.isInfixOf`) texts
    hasLeaf = any ("leaf" `T.isInfixOf`) texts
  when (not (hasRoot && hasChild && hasLeaf)) $ bump failed

runTreeSelectTest :: Context -> IORef Int -> IO ()
runTreeSelectTest _ failed = do
  ctx <- newContext
  let
    inp0 = withInput 40 12
    items = [TreeItem "alpha" [], TreeItem "beta" []]
    ui = column defaultLayout (tree "t" items 0)
  (resp, sel0) <- warmup2 ctx inp0 ui
  when (sel0 /= 0) $ bump failed
  let
    Rect rx ry _rw rh = respRect resp
    -- Second row: below mid-height of the merged tree rect.
    click = V2 (rx + 1) (ry + rh * 0.75)
    inpPress =
      inp0
        { inputMousePos = click
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx inpPress ui
  let
    inpRelease =
      inpPress
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  ((_, sel), _, _, _) <- runFrame ctx inpRelease ui
  when (sel /= 1) $ bump failed

runTreeExpandDamageTest :: Context -> IORef Int -> IO ()
runTreeExpandDamageTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    items =
      [ TreeItem "root" [TreeItem "child" []]
      , TreeItem "leaf" []
      ]
    ui = column defaultLayout (void (tree "t" items 0))
    inp0 = withInputOff 40 12
  _ <- runFrame ctx inp0 ui
  _ <- takeDamage ctx
  _ <- runFrame ctx inp0 ui
  _ <- takeDamage ctx
  _ <- runFrame ctx inp0 ui
  dIdle <- takeDamage ctx
  when (dIdle == DamageFull) $ bump failed
  spans <- collectTextSpans ctx
  case [r | (r, txt, _, _, _) <- spans, "root" `T.isInfixOf` txt] of
    (Rect x y _w h : _) -> do
      let
        click = V2 (x + 0.5) (y + h / 2)
        press =
          inp0
            { inputMousePos = click
            , inputMouseDown = True
            , inputMousePressed = True
            }
        release =
          press
            { inputMouseDown = False
            , inputMousePressed = False
            , inputMouseReleased = True
            }
      _ <- runFrame ctx press ui
      _ <- runFrame ctx release ui
      spansClick <- collectTextSpans ctx
      when (not (any (\(_, t, _, _, _) -> "child" `T.isInfixOf` t) spansClick)) $
        bump failed
      dClick <- takeDamage ctx
      when (dClick /= DamageFull) $ bump failed
      _ <- runFrame ctx inp0 ui
      spansNext <- collectTextSpans ctx
      when (any (\(_, t, _, _, _) -> "child" `T.isInfixOf` t) spansNext) $ bump failed
      when (not (any (\(_, t, _, _, _) -> "root" `T.isInfixOf` t) spansNext)) $
        bump failed
      dNext <- takeDamage ctx
      when (dNext /= DamageFull) $ bump failed
      _ <- runFrame ctx inp0 ui
      dSettled <- takeDamage ctx
      when (dSettled == DamageFull) $ bump failed
    _ -> bump failed

runTreeKeyboardTest :: Context -> IORef Int -> IO ()
runTreeKeyboardTest _ failed = do
  ctx <- newContext
  let
    items =
      [ TreeItem "root" [TreeItem "child" []]
      , TreeItem "leaf" []
      ]
    ui = column defaultLayout (tree "k" items 0)
    inp0 = withInput 40 12
  _ <- runFrame ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  let
    tabInp = inp0 {inputKeys = inputKeysFromList [KeyTab]}
  _ <- runFrame ctx tabInp ui
  let
    downInp = inp0 {inputKeys = inputKeysFromList [KeyDown]}
  ((_, sel1), _, _, _) <- runFrame ctx downInp ui
  when (sel1 /= 1) $ bump failed
  let
    upInp = inp0 {inputKeys = inputKeysFromList [KeyUp]}
  ((_, sel0), _, _, _) <- runFrame ctx upInp ui
  when (sel0 /= 0) $ bump failed
  let
    enterInp = inp0 {inputKeys = inputKeysFromList [KeyEnter]}
  _ <- runFrame ctx enterInp ui
  _ <- runFrame ctx inp0 ui
  spans <- collectTextSpans ctx
  when (any (\(_, t, _, _, _) -> "child" `T.isInfixOf` t) spans) $ bump failed

runSelectDropdownHoverTest :: Context -> IORef Int -> IO ()
runSelectDropdownHoverTest _ failed = do
  ctx <- newAdaptiveTerminalContext
  let
    layout = defaultLayout {layoutPadding = Padding 0 0 0 0, layoutGap = 0}
    inp0 = withInput 40 6
    ui = column layout (select "Quality" ["Low", "High"] 0)
  _ <- runFrame ctx inp0 ui
  let
    click =
      inp0
        { inputMousePos = V2 1 0.5
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx click ui
  let
    open =
      click
        { inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx open ui
  let
    hoverBase =
      open
        { inputMouseReleased = False
        , inputMousePressed = False
        , inputMouseDown = False
        }
  overlaysOpen <- collectOverlayTextSpans ctx hoverBase
  let
    highYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "High" `T.isInfixOf` txt]
  case highYs of
    (highY : _) -> do
      let
        hoverHigh = hoverBase {inputMousePos = V2 1 (highY + 0.5)}
      _ <- runFrame ctx hoverHigh ui
      overlaysHigh <- collectOverlayTextSpans ctx hoverHigh
      let
        bgFor needle spans = [bg | (_, txt, _, bg, _) <- spans, needle `T.isInfixOf` txt]
      case (bgFor "Low" overlaysHigh, bgFor "High" overlaysHigh) of
        ([lowBg], [highBg]) -> when (lowBg == highBg) $ bump failed
        _ -> bump failed
      let
        lowYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt]
      case (lowYs, bgFor "High" overlaysHigh) of
        ((lowY : _), [highHoverBg]) -> do
          let
            hoverLow = hoverBase {inputMousePos = V2 1 (lowY + 0.5)}
          _ <- runFrame ctx hoverLow ui
          overlaysLow <- collectOverlayTextSpans ctx hoverLow
          case bgFor "Low" overlaysLow of
            [lowHoverBg]
              | lowHoverBg == highHoverBg -> pure ()
              | otherwise -> bump failed
            _ -> bump failed
        _ -> bump failed
    _ -> bump failed

runSelectDropFlushTest :: Context -> IORef Int -> IO ()
runSelectDropFlushTest ctx failed = do
  let
    inp0 = withInput 320 200
    ui = select "Quality" ["Low", "High"] 1
  (resp, _) <- warmup2 ctx inp0 ui
  let
    Rect sx sy sw sh = respRect resp
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
  let
    maxOffset = 12
  case [rectY r | (r, txt, _, _, _) <- overlays, "Low" `T.isInfixOf` txt] of
    (lowY : _) -> when (lowY - (sy + sh) > maxOffset) $ bump failed
    [] -> bump failed

runSelectPickLowTest :: Context -> IORef Int -> IO ()
runSelectPickLowTest ctx failed = do
  let
    inp0 = withInput 320 200
    ui = select "Quality" ["Low", "Medium", "High"] 1
  (resp, idx0) <- warmup2 ctx inp0 ui
  when (idx0 /= 1) $ bump failed
  let
    Rect sx sy sw sh = respRect resp
    btn = V2 (sx + sw / 2) (sy + sh / 2)
    openPress =
      inp0
        { inputMousePos = btn
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx openPress ui
  let
    openRelease =
      openPress
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx openRelease ui
  overlaysOpen <- collectOverlayTextSpans ctx openRelease
  let
    lowYs = [rectY r | (r, txt, _, _, _) <- overlaysOpen, "Low" `T.isInfixOf` txt]
  case lowYs of
    (lowY : _) -> do
      let
        pickPos = V2 (sx + sw / 2) (lowY + 0.5)
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
      let
        pickRelease =
          pickPress
            { inputMousePressed = False
            , inputMouseDown = False
            , inputMouseReleased = True
            }
      ((_, idx1), _, _, _) <- runFrame ctx pickRelease ui
      when (idx1 /= 0) $ bump failed
      spans <- collectTextSpans ctx
      let
        hasLow =
          spansHas (T.pack "Quality: Low") spans
      failWhen failed (not hasLow)
    _ -> bump failed

runSelectKeyboardTest :: Context -> IORef Int -> IO ()
runSelectKeyboardTest ctx failed = do
  let
    inp0 = withInput 320 200
    ui = column defaultLayout (select "Quality" ["Low", "Medium", "High"] 1)
  (resp, idx0) <- warmup2 ctx inp0 ui
  when (idx0 /= 1) $ putStrLn "select-keyboard: idx0 /= 1" >> bump failed
  let
    Rect sx sy sw sh = respRect resp
    btn = V2 (sx + sw / 2) (sy + sh / 2)
    openPress =
      inp0
        { inputMousePos = btn
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseReleased = False
        }
  _ <- runFrame ctx openPress ui
  let
    openRelease =
      openPress
        { inputMousePressed = False
        , inputMouseDown = False
        , inputMouseReleased = True
        }
  _ <- runFrame ctx openRelease ui
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyDown]}) ui
  ((_, idx1), _, _, _) <- runFrame ctx openRelease ui
  when (idx1 /= 2) $ putStrLn ("select-keyboard: idx1 /= 2, idx1=" ++ show idx1) >> bump failed
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyUp]}) ui
  ((_, idx2), _, _, _) <- runFrame ctx openRelease ui
  when (idx2 /= 1) $ putStrLn ("select-keyboard: idx2 /= 1, idx2=" ++ show idx2) >> bump failed
  _ <- runFrame ctx (openRelease {inputKeys = inputKeysFromList [KeyEscape], inputMouseReleased = False}) ui
  let idleAfterOpen = openRelease {inputMouseReleased = False}
  _ <- runFrame ctx idleAfterOpen ui
  overlays <- collectOverlayTextSpans ctx idleAfterOpen
  let
    dropdownOpen =
      any
        (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"])
        overlays
  when dropdownOpen $ bump failed
  -- Focused with menu closed: arrows change value without opening the list.
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyTab]}) ui
  focus <- getFocusId ctx
  when (focus == WidgetId 0) $ bump failed
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyRight]}) ui
  ((_, idx3), _, _, _) <- runFrame ctx inp0 ui
  when (idx3 /= 2) $ bump failed
  closedOverlays <- collectOverlayTextSpans ctx inp0
  let
    closedMenuOpen =
      any
        (\(_, txt, _, _, _) -> txt `elem` ["Low", "Medium", "High"])
        closedOverlays
  when closedMenuOpen $ bump failed
  _ <- runFrame ctx (inp0 {inputKeys = inputKeysFromList [KeyLeft]}) ui
  ((_, idx4), _, _, _) <- runFrame ctx inp0 ui
  when (idx4 /= 1) $ bump failed



