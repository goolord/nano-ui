-- | The middle and side mouse buttons: a middle click is reported apart from
-- a click and belongs to the widget it went down on.
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
  ]

win :: Input
win = withInputOff 400 300

-- | Run each frame and return the view's results.
frames :: Context -> NanoUI a -> [Input] -> IO [a]
frames ctx ui = mapM (\inp -> evalUi ctx inp ui)

-- | Run a middle press frame and a release frame at @pos@.
middleAt :: Context -> NanoUI a -> V2 -> IO [a]
middleAt ctx ui pos = let (press, release) = middleClickPair win pos in frames ctx ui [press, release]

-- | Whether a fresh loop, idle since input @i@, draws a frame for @i@ again.
redrawsAgain :: Input -> IO Bool
redrawsAgain i = newContext >>= \idle -> shouldRedrawFrame idle i i False False False

-- | Whether a response saw no middle press or click.
noMiddle :: Response -> Bool
noMiddle r = not (respMiddlePressed r || respMiddleClicked r)

-- | A middle press and release on a button is a middle click, not a click.
runMiddleClickTest :: Context -> IORef Int -> IO ()
runMiddleClickTest ctx failed = do
  btn <- warmup2 ctx win (button' "Middle")
  let (press, release) = middleClickPair win (centerOf btn)
      flags r = (respMiddlePressed r, respMiddleClicked r, respClicked r || respPressed r)
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
      clicked (a, b) = (respMiddleClicked a, respMiddleClicked b)
  (a, b) <- warmup2 ctx win ui
  [_, across] <- frames ctx ui [fst (middleClickPair win (centerOf a)), snd (middleClickPair win (centerOf b))]
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
  dismissed <- forM [centerOf shown, V2 380 280] $ \p -> respClicked <$> evalUi ctx (fst (middleClickPair win p)) ui
  assertEq failed [False, True] dismissed

-- | The side buttons report their press for one frame, unseen by a covered layer.
runSideButtonsTest :: Context -> IORef Int -> IO ()
runSideButtonsTest ctx failed = do
  let side b = applyMouseButton b True win {inputMousePos = V2 20 20}
      back = side MouseBack
      sides i = (inputMouseBackPressed i, inputMouseForwardPressed i)
  assertEq failed [(True, False), (False, True), (True, False), (False, False)] $
    map sides [back, side MouseForward, applyMouseButton MouseBack False back, clearEphemeral back]
  assert failed =<< redrawsAgain back
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
      (press, release) = middleClickPair hover (centerOf btn)
      open = warmup ctx hover ui >> waitOut >> tip True hover
  open
  tip False press
  waitOut
  tip False press {inputMouseMiddlePressed = False}
  waking False
  tip False release
  waking True
  waitOut >> tip True hover
  forM_ [MouseBack, MouseForward] $ \b -> tip False (applyMouseButton b True hover) >> open
