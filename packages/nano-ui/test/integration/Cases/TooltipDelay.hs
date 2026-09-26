module Cases.TooltipDelay (tests) where

import Control.Concurrent (threadDelay)
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import Spec

tests :: [Spec]
tests =
  [ spec "tooltip-delay-wait" runTooltipDelayWaitTest
  , spec "tooltip-delay-open" runTooltipDelayOpenTest
  , spec "tooltip-delay-reset" runTooltipDelayResetTest
  , spec "tooltip-grace" runTooltipGraceTest
  , spec "tooltip-grace-after-rest" runTooltipGraceAfterRestTest
  , spec "tooltip-target-not-widget" runTooltipTargetNotWidgetTest
  , spec "tooltip-follow-cursor" runTooltipFollowCursorTest
  , spec "tooltip-follow-covered" runTooltipFollowCoveredTest
  ]

inp0 :: Input
inp0 = withInput 400 300

-- | A button with a tooltip, and the id of the next button (which changes if
-- the tooltip shifts sibling ids).
tipView :: TooltipConfig -> NanoUI (Response, WidgetId)
tipView cfg = column $ do
  target <- button' "Tip Target"
  tooltipConfigured cfg target "Tip text"
  (target,) . respId <$> button' "After"

-- | Buttons Alpha, Bravo and Charlie with tooltips "Alpha tip", etc.
tipRow :: [TooltipConfig] -> NanoUI [Response]
tipRow cfgs = row . forM (zip ["Alpha", "Bravo", "Charlie"] cfgs) $ \(name, cfg) -> do
  r <- button' name
  r <$ tooltipConfigured cfg r (name <> " tip")

-- | Run a frame and return the text of each tooltip drawn.
tipsAfter :: Context -> NanoUI a -> Input -> IO [T.Text]
tipsAfter ctx ui i = do
  _ <- runFrame ctx i ui
  spans <- collectOverlayTextSpans ctx i
  pure (filter (`hasText` spans) ["Tip text", "Alpha tip", "Bravo tip", "Charlie tip", "Label tip", "Box tip"])

-- | Rects of all floating panels, i.e. the open tooltips.
tipRects :: Context -> IO [Rect]
tipRects ctx = IM.elems <$> floatingPanelRects ctx

-- | Hover the 'tipView' target with delay @d@ (the long frame time finishes
-- the hover animation). The tooltip stays closed and only schedules a wake at
-- the delay, within @slack@. Returns the view, the hover input, the wake time,
-- and a check that continued hovering settles: not dirty, no frame, sibling
-- id unchanged.
waitFor :: Context -> IORef Int -> Double -> Double -> IO (NanoUI (Response, WidgetId), Input, Double, IO ())
waitFor ctx failed d slack = do
  let ui = tipView defaultTooltipConfig {tooltipDelay = d}
  (target, after0) <- warmup2 ctx inp0 ui
  let hover = inp0 {inputMousePos = centerOf target, inputDeltaTime = 1}
  before <- getMonotonicTime
  assertEq failed [] =<< tipsAfter ctx ui hover
  wakeAt <- getWakeAt ctx
  assert failed (wakeAt >= before + d && wakeAt < before + d + slack)
  let settles = do
        _ <- runFrame ctx hover ui
        ((_, after1), _, _, dirty) <- runFrame ctx hover ui
        assertEq failed (False, after0) (dirty, after1)
        assert failed . not =<< needsRedraw ctx hover hover
  pure (ui, hover, wakeAt, settles)

-- | Moving over the target does not push the wake back.
runTooltipDelayWaitTest :: Context -> IORef Int -> IO ()
runTooltipDelayWaitTest ctx failed = do
  (ui, hover, wakeAt, settles) <- waitFor ctx failed 10 1
  settles
  assertEq failed wakeAt =<< getWakeAt ctx
  let V2 cx cy = inputMousePos hover
  assertEq failed [] =<< tipsAfter ctx ui hover {inputMousePos = V2 (cx + 6) (cy + 2)}
  assertEq failed wakeAt =<< getWakeAt ctx

-- | The first frame after the delay opens and repaints the tooltip; after
-- that no wake is requested.
runTooltipDelayOpenTest :: Context -> IORef Int -> IO ()
runTooltipDelayOpenTest ctx failed = do
  (ui, hover, _, settles) <- waitFor ctx failed 0.05 0.95
  threadDelay 100000
  assertEq failed ["Tip text"] =<< tipsAfter ctx ui hover
  dmg <- takeDamage ctx
  up <- tipRects ctx
  assert failed (not (null up) && all (damageCovers dmg) up)
  assertEq failed 0 =<< getWakeAt ctx
  settles
  assert failed . hasText "Tip text" =<< collectOverlayTextSpans ctx hover

-- | Leaving the target closes and repaints the tooltip; returning waits
-- again. A press closes it with no wake while held, and so does a wheel turn.
runTooltipDelayResetTest :: Context -> IORef Int -> IO ()
runTooltipDelayResetTest ctx failed = do
  let ui = tipView defaultTooltipConfig {tooltipDelay = 0.05, tooltipGrace = 0}
      tip yes i = assertEq failed ["Tip text" | yes] =<< tipsAfter ctx ui i
      waking yes = assert failed . (if yes then (> 0) else (== 0)) =<< getWakeAt ctx
      settle = threadDelay 100000
  (target, _) <- warmup2 ctx inp0 ui
  let hover = inp0 {inputMousePos = centerOf target}
      hold = holdAt hover (centerOf target)
      opens = settle >> tip True hover
  tip False hover >> opens
  up <- tipRects ctx
  tip False inp0 {inputMousePos = V2 390 290}
  dmg <- takeDamage ctx
  assert failed (not (null up) && all (damageCovers dmg) up)
  waking False
  tip False hover >> opens
  tip False (pressAt hover (centerOf target))
  tip False hold
  waking False
  settle >> tip False hold
  tip False (releaseAt hold)
  waking True
  opens
  tip False hover {inputScroll = V2 0 1}
  waking True
  opens

-- | A tooltip opens immediately within its grace period after another one
-- closes; without a grace period, or after a press, it waits.
runTooltipGraceTest :: Context -> IORef Int -> IO ()
runTooltipGraceTest ctx failed = do
  let slow = defaultTooltipConfig {tooltipDelay = 10, tooltipGrace = 5}
      ui = tipRow [defaultTooltipConfig {tooltipDelay = 0}, slow, slow {tooltipGrace = 0}]
  [a, b, c] <- warmup2 ctx inp0 ui
  let at r = inp0 {inputMousePos = centerOf r}
      hold = holdAt (at a) (centerOf b)
  forM_ [(at a, ["Alpha tip"]), (at c, []), (at a, ["Alpha tip"]), (at b, ["Bravo tip"]), (at a, ["Alpha tip"]), (pressAt (at a) (centerOf a), []), (hold, []), (releaseAt hold, [])] $ \(i, tips) ->
    assertEq failed tips =<< tipsAfter ctx ui i

-- | The grace period starts when a tooltip closes, whether the next tooltip
-- is declared before or after it. Once it expires, the next one waits again.
runTooltipGraceAfterRestTest :: Context -> IORef Int -> IO ()
runTooltipGraceAfterRestTest ctx failed = do
  let quick = defaultTooltipConfig {tooltipDelay = 0.05, tooltipGrace = 0.3}
      ui = tipRow [quick {tooltipDelay = 10}, quick, quick {tooltipDelay = 10}]
      tips expect i = assertEq failed expect =<< tipsAfter ctx ui i
      -- Longer than the grace period, with no frames run.
      rest = threadDelay 500000
  [a, b, c] <- warmup2 ctx inp0 ui
  let at r = inp0 {inputMousePos = centerOf r}
  tips [] (at b)
  threadDelay 100000
  tips ["Bravo tip"] (at b)
  rest >> tips ["Charlie tip"] (at c)
  rest >> tips ["Bravo tip"] (at b)
  rest >> tips [] inp0 {inputMousePos = V2 390 290}
  rest >> tips [] (at a)
  assert failed . (> 0) =<< getWakeAt ctx

-- | Entering or leaving a label or container with a tooltip needs a frame,
-- even though the hover probe finds no widget there; moving within it does
-- not.
runTooltipTargetNotWidgetTest :: Context -> IORef Int -> IO ()
runTooltipTargetNotWidgetTest ctx failed = do
  let ui = column $ do
        l <- labelWith' (fixedW 200) "Label target"
        tooltipConfigured defaultTooltipConfig {tooltipDelay = 0.05} l "Label tip"
        l <$ withTooltip (columnWith (fixedW 200 . fixedH 60) (label "Container target")) (label "Box tip")
      tips expect i = assertEq failed expect =<< tipsAfter ctx ui i
      redraws yes from to = assertEq failed yes =<< needsRedraw ctx from to
  l <- warmup2 ctx inp0 ui
  boxText <- spanRect "Container target" <$> collectTextSpans ctx
  let V2 lx ly = centerOf l
      onLabel = inp0 {inputMousePos = V2 lx ly}
  redraws True inp0 onLabel
  tips [] onLabel
  redraws False onLabel onLabel {inputMousePos = V2 (lx + 5) ly}
  threadDelay 100000
  tips ["Label tip"] onLabel
  redraws True onLabel inp0
  tips [] inp0
  assertJust failed boxText $ \r -> do
    let onBox = inp0 {inputMousePos = spanCenter r}
    redraws True inp0 onBox
    _ <- runFrame ctx onBox ui
    threadDelay 600000
    tips ["Box tip"] onBox
    redraws True onBox inp0

-- | A large button with an immediate tooltip placed at the pointer.
followView :: NanoUI Response
followView = column $ do
  target <- buttonWith' (fixedW 360 . fixedH 240) "Canvas"
  target <$ tooltipConfigured defaultTooltipConfig {tooltipDelay = 0, tooltipPlacement = PlacementAtCursor} target "Follow tip"

-- | A pointer-placed tooltip sits below the pointer and follows it (moves
-- need frames), repainting old and new rects. Near the bottom it flips above
-- the pointer to stay in the window.
runTooltipFollowCursorTest :: Context -> IORef Int -> IO ()
runTooltipFollowCursorTest ctx failed = do
  let win = withInput 400 260
      at p = win {inputMousePos = p, inputDeltaTime = 1}
      tipAt p = runFrame ctx (at p) followView >> tipRects ctx
      below (V2 px py) (Rect x y _ _) = assertEq failed (px, py + 20) (x, y)
  Rect tx ty tw th <- respRect <$> warmup2 ctx win followView
  let p1 = V2 (tx + 40) (ty + 40)
      p2 = V2 (tx + 120) (ty + 90)
  [r1] <- tipAt p1 >> tipAt p1
  below p1 r1
  assertEq failed [False, True] =<< mapM (needsRedraw ctx (at p1) . at) [p1, p2]
  [r2] <- tipAt p2
  below p2 r2
  dmg <- takeDamage ctx
  assert failed (damageCovers dmg r1 && damageCovers dmg r2)
  [Rect x3 y3 w3 h3] <- tipAt (V2 (tx + tw - 2) (ty + th - 2))
  assert failed (x3 >= 0 && x3 + w3 <= 400 && y3 >= 0 && y3 + h3 <= ty + th - 2)

-- | A pointer on the following tooltip still counts as over the target: the
-- tooltip stays open and moves out from under it.
runTooltipFollowCoveredTest :: Context -> IORef Int -> IO ()
runTooltipFollowCoveredTest ctx failed = do
  let tipAt p = runFrame ctx inp0 {inputMousePos = p} followView >> tipRects ctx
  Rect tx ty _ _ <- respRect <$> warmup2 ctx inp0 followView
  [r1] <- tipAt (V2 (tx + 40) (ty + 40))
  let p2@(V2 _ y2) = spanCenter r1
  [Rect _ y _ _] <- tipAt p2
  assertEq failed (y2 + 20) y
