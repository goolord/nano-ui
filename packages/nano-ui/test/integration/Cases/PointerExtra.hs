-- | The middle, side and other mouse buttons: a click of any button but the
-- left is reported apart from a click, and every button held or clicked
-- belongs to the widget it went down on.
module Cases.PointerExtra (tests) where

import Spec
import Control.Concurrent (threadDelay)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (findNodeM, getNodeRect, getText)
import NanoUI.Runner (shouldRedrawFrame)

tests :: [Spec]
tests =
  [ spec "pointer-middle-click" runMiddleClickTest
  , spec "pointer-middle-ownership" runMiddleOwnershipTest
  , spec "pointer-middle-covered" runMiddleCoveredTest
  , spec "pointer-middle-tab-close" runMiddleTabCloseTest
  , spec "pointer-middle-dismiss" runMiddleDismissTest
  , spec "pointer-side-buttons" runSideButtonsTest
  , spec "pointer-middle-disabled" runMiddleDisabledTest
  , spec "pointer-middle-tooltip" runMiddleTooltipTest
  , spec "pointer-button-sets" runButtonSetsTest
  , spec "pointer-held-ownership" runHeldOwnershipTest
  , spec "pointer-mouse-listeners" runMouseListenersTest
  , spec "pointer-mouse-area" runMouseAreaTest
  ]

win :: Input
win = withInputOff 400 300

-- | Run each frame and return the view's results.
frames :: Context -> NanoUI a -> [Input] -> IO [a]
frames ctx ui = mapM (\inp -> evalUi ctx inp ui)

-- | Run a middle press frame and a release frame at @pos@.
middleAt :: Context -> NanoUI a -> V2 -> IO [a]
middleAt ctx ui pos = let (press, release) = clickPairWith MouseMiddle win pos in frames ctx ui [press, release]

-- | Whether a fresh loop, idle since input @i@, draws a frame for @i@ again.
redrawsAgain :: Input -> IO Bool
redrawsAgain i = newContext >>= \idle -> shouldRedrawFrame idle i i False False False

-- | Whether a response saw no middle press or click.
noMiddle :: Response -> Bool
noMiddle r = not (respHeldWith MouseMiddle r || respClickedWith MouseMiddle r)

-- | A middle press and release on a button is a middle click, not a click.
runMiddleClickTest :: Context -> IORef Int -> IO ()
runMiddleClickTest ctx failed = do
  btn <- warmup2 ctx win (button' "Middle")
  let (press, release) = clickPairWith MouseMiddle win (centerOf btn)
      flags r = (respHeldWith MouseMiddle r, respClickedWith MouseMiddle r, respClicked r || respPressed r)
  rs <- frames ctx (button' "Middle") [press, release, clearEphemeral release]
  assertEq failed [(True, False, False), (False, True, False), (False, False, False)] (map flags rs)
  -- A held middle button keeps frames coming, and a repeated press gets a frame.
  assert failed (inputPointerHeld press && not (inputPointerHeld release))
  assert failed =<< redrawsAgain press
  assertEq failed [False, False] =<< middleAt ctx (button "Middle") (centerOf btn)

-- | A middle click belongs to the widget it went down on, not the one it ends on.
runMiddleOwnershipTest :: Context -> IORef Int -> IO ()
runMiddleOwnershipTest ctx failed = do
  let ui = row ((,) <$> button' "Alpha" <*> button' "Beta")
      clicked (a, b) = (respClickedWith MouseMiddle a, respClickedWith MouseMiddle b)
  (a, b) <- warmup2 ctx win ui
  [_, across] <- frames ctx ui [fst (clickPairWith MouseMiddle win (centerOf a)), snd (clickPairWith MouseMiddle win (centerOf b))]
  assertEq failed (False, False) (clicked across)
  [_, onB] <- middleAt ctx ui (centerOf b)
  assertEq failed (False, True) (clicked onB)

-- | A middle click beside a modal's panel reaches nothing under the modal.
runMiddleCoveredTest :: Context -> IORef Int -> IO ()
runMiddleCoveredTest ctx failed = do
  let ui open = buttonWith' (fixedWH 380 280) "Under" <* modal open "Cover" (label "on top")
  Rect x y _ _ <- respRect <$> warmup2 ctx win (ui False)
  _ <- warmup2 ctx win (ui True)
  rs <- middleAt ctx (ui True) (V2 (x + 4) (y + 4))
  assert failed (all (\r -> noMiddle r && not (respHovered r)) rs)

-- | A middle click on a closable tab's header or close button asks to close it,
-- without selecting it; a tab without a close button stays.
runMiddleTabCloseTest :: Context -> IORef Int -> IO ()
runMiddleTabCloseTest ctx failed = do
  let ui = tabBar' (0 :: Int) [closableTab 0 "Alpha" (), closableTab 1 "Beta" (), tab 2 "Gamma" ()]
      na = ctxNodeArena ctx
  _ <- warmup2 ctx win ui
  spans <- collectTextSpans ctx
  closeX <- traverse (getNodeRect na) =<< findNodeM na (fmap ("\215" `T.isInfixOf`) . getText na)
  forM_ [(spanRectOf "Beta" spans, Just 1), (closeX, Just 0), (spanRectOf "Gamma" spans, Nothing)] $ \(at, closes) ->
    assertJust failed at $ \r -> do
      [_, bar] <- middleAt ctx ui (spanCenter r)
      assertEq failed (closes, 0) (tabClosed bar, tabActive bar)

-- | A middle press outside a popup dismisses it; one inside does not.
runMiddleDismissTest :: Context -> IORef Int -> IO ()
runMiddleDismissTest ctx failed = do
  let ui = fst <$> popup True (defaultPopupConfig (AnchorPoint (V2 40 40))) (label "Popup body")
  shown <- warmup2 ctx win ui
  dismissed <- forM [centerOf shown, V2 380 280] $ \p -> respClicked <$> evalUi ctx (fst (clickPairWith MouseMiddle win p)) ui
  assertEq failed [False, True] dismissed

-- | The side buttons are held and released like the others, and a covered
-- layer sees none of it.
runSideButtonsTest :: Context -> IORef Int -> IO ()
runSideButtonsTest ctx failed = do
  let side b = applyMouseButton b True win {inputMousePos = V2 20 20}
      back = side MouseBack
      backUp = applyMouseButton MouseBack False (clearEphemeral back)
      sides i = (buttonPressed MouseBack i, buttonPressed MouseForward i)
  assertEq failed [(True, False), (False, True), (False, False), (False, False)] $
    map sides [back, side MouseForward, backUp, clearEphemeral back]
  assert failed (buttonHeld MouseBack (clearEphemeral back) && inputPointerHeld back)
  assert failed (buttonReleased MouseBack backUp && not (buttonHeld MouseBack backUp))
  assert failed =<< redrawsAgain back
  assert failed =<< redrawsAgain backUp
  assertEq failed (True, False) =<< evalUi ctx back (sides <$> askInput)
  let covered = sides <$> askInput <* modal True "Cover" (label "on top")
  _ <- warmup2 ctx win covered
  assertEq failed (False, False) =<< evalUi ctx back covered

-- | A disabled button takes no middle press or click.
runMiddleDisabledTest :: Context -> IORef Int -> IO ()
runMiddleDisabledTest ctx failed = do
  let ui = disabledWhen True (button' "Off")
  rs <- middleAt ctx ui . centerOf =<< warmup2 ctx win ui
  assert failed (all noMiddle rs)

-- | A middle press shuts a tooltip, with no wait while held and a new one on
-- release; a side button's press shuts it and waits again.
runMiddleTooltipTest :: Context -> IORef Int -> IO ()
runMiddleTooltipTest ctx failed = do
  let ui = button' "Help" >>= \b -> b <$ tooltipConfigured defaultTooltipConfig {tooltipDelay = 0.05, tooltipGrace = 0} b "Mouse tip"
      tip yes i = warmup ctx i ui >> (assertEq failed yes . hasText "Mouse tip" =<< collectOverlayTextSpans ctx i)
      waking yes = assert failed . (if yes then (> 0) else (== 0)) =<< getWakeAt ctx
      waitOut = threadDelay 80000
  btn <- warmup2 ctx win ui
  let hover = win {inputMousePos = centerOf btn}
      (press, release) = clickPairWith MouseMiddle hover (centerOf btn)
      open = warmup ctx hover ui >> waitOut >> tip True hover
  open
  tip False press
  waitOut
  tip False press {inputButtonsPressed = noButtons}
  waking False
  tip False release
  waking True
  waitOut >> tip True hover
  forM_ [MouseBack, MouseForward] $ \b -> tip False (applyMouseButton b True hover) >> open

-- | Sets of buttons: every button, the extra ones by number, goes down and
-- up through one set, and a press and release come in the set of the frame
-- they happen in.
runButtonSetsTest :: Context -> IORef Int -> IO ()
runButtonSetsTest _ failed = do
  let every = [MouseLeft, MouseRight, MouseMiddle, MouseBack, MouseForward, MouseOther 6, MouseOther 32]
      down = foldr (`applyMouseButton` True) win every
  assertEq failed (buttonsFromList every) (inputButtonsHeld down)
  assertEq failed (buttonsFromList every) (inputButtonsPressed down)
  assert failed (all (`buttonHeld` down) every && all (`buttonPressed` down) every)
  -- The list comes back in number order, the named buttons by their names.
  assertEq failed [MouseLeft, MouseMiddle, MouseRight, MouseBack, MouseForward, MouseOther 6, MouseOther 32] (buttonsToList (inputButtonsHeld down))
  assertEq failed [MouseLeft, MouseMiddle, MouseRight, MouseBack, MouseForward, MouseOther 6] (map mouseButtonNumber [1 .. 6])
  -- A button past 32 is not tracked.
  assert failed (buttonsNull (inputButtonsHeld (applyMouseButton (MouseOther 33) True win)))
  let up = applyMouseButton (MouseOther 6) False (clearEphemeral down)
  assert failed (buttonReleased (MouseOther 6) up && not (buttonHeld (MouseOther 6) up) && buttonHeld MouseLeft up)
  assert failed (anyButtonReleased up && not (anyButtonPressed up))
  assertEq failed (buttonsFromList [MouseLeft, MouseRight]) (buttonsFromList [MouseLeft] <> buttonsFromList [MouseRight])
  -- The next frame keeps what is held and drops the presses.
  assertEq failed (inputButtonsHeld down) (inputButtonsHeld (clearEphemeral down))
  assert failed (buttonsNull (inputButtonsPressed (clearEphemeral down)))

-- | A button held or clicked is the widget's only where it went down on the
-- widget: a right or middle drag from one button across another holds and
-- clicks neither, and an extra button is reported like the others.
runHeldOwnershipTest :: Context -> IORef Int -> IO ()
runHeldOwnershipTest ctx failed = do
  let ui = row ((,) <$> button' "Alpha" <*> button' "Beta")
      heldOn b (x, y) = (respHeldWith b x, respHeldWith b y)
  (a, b) <- warmup2 ctx win ui
  forM_ [MouseRight, MouseMiddle, MouseOther 7] $ \btn -> do
    let press = pressWith btn win (centerOf a)
        dragged = press {inputMousePos = centerOf b, inputButtonsPressed = noButtons}
    [onA, across, released] <- frames ctx ui [press, dragged, releaseWith btn dragged]
    assertEq failed (True, False) (heldOn btn onA)
    assertEq failed (False, False) (heldOn btn across)
    assert failed (not (respClickedWith btn (snd released) || respClickedWith btn (fst released)))
    assert failed (respHovered (snd across))
    -- A click of the button on one widget is that widget's alone.
    [_, clickedB] <- let (p, r) = clickPairWith btn win (centerOf b) in frames ctx ui [p, r]
    assertEq failed (False, True) (respClickedWith btn (fst clickedB), respClickedWith btn (snd clickedB))
    assert failed (not (respClicked (snd clickedB)))

-- | 'mousePressed', 'mouseReleased' and 'mouseHeld' hear a button anywhere on
-- the view's layer, and nothing behind a modal or inside 'disabledWhen'.
runMouseListenersTest :: Context -> IORef Int -> IO ()
runMouseListenersTest ctx failed = do
  let listen = (,,) <$> mousePressed MouseBack <*> mouseHeld MouseBack <*> mouseReleased MouseBack
      back = pressWith MouseBack win (V2 20 20)
      still = clearEphemeral back
      up = releaseWith MouseBack still
  _ <- warmup2 ctx win listen
  rs <- frames ctx listen [back, still, up]
  assertEq failed [(True, True, False), (False, True, False), (False, False, True)] rs
  let off = disabledWhen True listen
  assertEq failed [(False, False, False)] =<< frames ctx off [back]
  let covered = listen <* modal True "Cover" (label "on top")
  _ <- warmup2 ctx win covered
  assertEq failed [(False, False, False)] =<< frames ctx covered [back]

-- | A mouse area reports hover and every button over it and all inside it: a
-- middle click or a right press on its label is the area's, and so is a left
-- click beside its button, while a click on the button is the button's. What
-- the area reveals while hovered stays revealed with the pointer on it, even
-- where a pinned node elsewhere turns covering on.
runMouseAreaTest :: Context -> IORef Int -> IO ()
runMouseAreaTest ctx failed = do
  hoveredRef <- newIORef False
  let ui = columnWith tight $ do
        shown <- uiIO (readIORef hoveredRef)
        ((l, b), area) <- mouseArea (fixedWH 300 80) $ do
          l <- label' "Name"
          b <- if shown then Just <$> button' "Delete" else pure Nothing
          pure (l, b)
        uiIO (writeIORef hoveredRef (respHovered area))
        _ <- buttonWith' (pinAt 320 0 . fixedWH 40 20) "pinned"
        pure (l, b, area)
      at p = win {inputMousePos = p}
  (l0, _, area0) <- warmup2 ctx win ui
  [(_, _, areaMid)] <- frames ctx ui [snd (clickPairWith MouseMiddle win (centerOf l0))]
  assert failed (respClickedWith MouseMiddle areaMid && not (respClicked areaMid))
  [(_, _, areaRight)] <- frames ctx ui [fst (clickPairWith MouseRight win (centerOf l0))]
  assert failed (respHeldWith MouseRight areaRight && respHovered areaRight)
  let beside = V2 250 60
  (_, _, areaLeft) <- runClick ctx (at beside) ui beside
  assert failed (respClicked areaLeft && respClickedWith MouseLeft areaLeft)
  -- Hovering reveals the button, which stays while the pointer is on it.
  (_, _, _) <- warmup2 ctx (at (centerOf l0)) ui
  (_, b1, _) <- evalUi ctx (at (centerOf l0)) ui
  assertJust failed b1 $ \b -> do
    rs <- frames ctx ui (replicate 4 (at (centerOf b)))
    assert failed (all (\(_, b', a) -> respHovered a && maybe False respHovered b') rs)
    (_, bClicked, aClicked) <- runClick ctx (at (centerOf b)) ui (centerOf b)
    assert failed (maybe False respClicked bClicked && not (respClicked aClicked))
  assert failed (rectContains (respRect area0) beside)
