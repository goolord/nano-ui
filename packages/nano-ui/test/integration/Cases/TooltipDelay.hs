{-# OPTIONS_GHC -O0 #-}
module Cases.TooltipDelay (tests) where

import Control.Concurrent (threadDelay)
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import GHC.Clock (getMonotonicTime)
import NanoUI.Testing.Sim
import Spec

tests :: [Spec]
tests =
  [ spec "tooltip-delay" runTooltipDelayTest
  , simSpec "tooltip-grace" 400 300 (tipRow [cfg 0 0.3, cfg 10 5, cfg 10 0]) graceOpens
  , simSpec "tooltip-grace-after-rest" 400 300 (tipRow [cfg 10 0.1, cfg 0.05 0.1, cfg 10 0.1]) graceAfterRest
  , simSpec "tooltip-target-not-widget" 400 300 notWidgetUi tooltipTargetNotWidget
  , simSpec "tooltip-follow-cursor" 400 260 followUi tooltipFollowCursor
  ]

cfg :: Double -> Double -> TooltipConfig
cfg d g = defaultTooltipConfig {tooltipDelay = d, tooltipGrace = g}

-- | The text of each tooltip the last frame drew.
tips :: Sim a [T.Text]
tips = (\ss -> filter (`hasText` ss) names) <$> spans
  where
    names = ["Tip text", "Alpha tip", "Bravo tip", "Charlie tip", "Label tip", "Box tip"]

-- | The tooltips drawn by the frames of @act@.
tipsAfter :: Sim a b -> Sim a [T.Text]
tipsAfter act = act >> tips

sleep :: Double -> Sim a ()
sleep s = liftIO (threadDelay (round (s * 1e6)))

away :: Target
away = At (V2 390 290)

-- | Rects of all floating panels, i.e. the open tooltips.
tipRects :: Sim a [Rect]
tipRects = onContext (fmap IM.elems . floatingPanelRects)

-- | Whether a pointer moving to @p@ from where it is would need a frame.
redrawsFor :: V2 -> Sim a Bool
redrawsFor p = wouldRedraw (\i -> i {inputMousePos = p})

-- | A button with a tooltip declared while the ref holds its delay, and the
-- next button's id (which changes if the tooltip shifts sibling ids).
tipView :: IORef (Maybe Double) -> NanoUI (Response, WidgetId)
tipView ref = column $ do
  delay <- liftIO (readIORef ref)
  target <- button' "Tip Target"
  forM_ delay $ \d -> tooltipConfigured (cfg d 0) target "Tip text"
  (target,) . respId <$> button' "After"

-- | Hovering waits for the delay with one wake and no frames; the first frame
-- after opens and repaints the tooltip. Leaving, any button press or a wheel
-- turn closes and repaints it, with no wake while a button is held. Omitting
-- the tooltip for a frame resets its delay.
runTooltipDelayTest :: Context -> IORef Int -> IO ()
runTooltipDelayTest ctx failed = do
  ref <- newIORef (Just 10)
  runSim ctx failed (withInput 400 300) (tipView ref) $ do
    (target, after0) <- result
    let waking yes = check . (if yes then (> 0) else (== 0)) =<< wakeTime
        none act = checkEq [] =<< tipsAfter act
        declare = liftIO . writeIORef ref
        -- Past the 0.05 s delay.
        wait = sleep 0.06
        opens = wait >> (checkEq ["Tip text"] =<< tipsAfter frame)
        settles = do
          (_, after1) <- frame >> frame
          again <- wantsFrame
          checkEq (False, after0) (again, after1)
          check . not =<< redrawsFor . inputMousePos =<< getInput
        V2 cx cy = centerOf target
    -- The long frame time finishes the hover animation.
    modifyInput (\i -> i {inputDeltaTime = 1})
    before <- liftIO getMonotonicTime
    none (moveTo (On target))
    w <- wakeTime
    check (w >= before + 10 && w < before + 11)
    settles
    none (moveTo (At (V2 (cx + 6) (cy + 2))))
    checkEq w =<< wakeTime
    declare (Just 0.05)
    none (moveTo away >> moveTo (On target))
    _ <- frameDamage
    opens
    dmg <- frameDamage
    up <- tipRects
    check (not (null up) && all (damageCovers dmg) up)
    checkEq 0 =<< wakeTime
    settles
    checkEq ["Tip text"] =<< tips
    none (moveTo away)
    check . (\d -> all (damageCovers d) up) =<< frameDamage
    waking False
    forM_ [MouseLeft, MouseMiddle, MouseBack, MouseForward] $ \b -> do
      none (moveTo (On target))
      opens
      none (pressButton b (On target))
      none frame
      waking False
      wait >> none frame
      none (releaseButton b)
      waking True
    opens
    none (frameWith (\i -> i {inputScroll = V2 0 1}))
    waking True
    opens
    -- Omitted for a frame while pending, then declared again on the target.
    none (moveTo away >> moveTo (On target))
    declare Nothing >> none frame
    declare (Just 0.05) >> wait >> none frame
    opens

-- | Buttons Alpha, Bravo and Charlie with tooltips "Alpha tip", etc.
tipRow :: [TooltipConfig] -> NanoUI [Response]
tipRow cfgs = row . forM (zip ["Alpha", "Bravo", "Charlie"] cfgs) $ \(name, c) -> do
  r <- button' name
  r <$ tooltipConfigured c r (name <> " tip")

-- | A tooltip opens immediately within its grace period after another one
-- closes; without a grace period, or after a press, it waits.
graceOpens :: Sim [Response] ()
graceOpens = do
  [a, b, c] <- map On <$> result
  let steps = [moveTo a, moveTo c, moveTo a, moveTo b, moveTo a, press a, moveTo b, release]
      shown = [["Alpha tip"], [], ["Alpha tip"], ["Bravo tip"], ["Alpha tip"], [], [], []]
  checkEq shown =<< mapM tipsAfter steps

-- | The grace period starts when a tooltip closes, whether the next tooltip
-- is declared before or after it. Once it expires, the next one waits again.
graceAfterRest :: Sim [Response] ()
graceAfterRest = do
  [a, b, c] <- map On <$> result
  -- Longer than the grace period, with no frames run.
  let rest = sleep 0.15
      steps =
        [ (moveTo b, [])
        , (sleep 0.06 >> frame, ["Bravo tip"])
        , (rest >> moveTo c, ["Charlie tip"])
        , (rest >> moveTo b, ["Bravo tip"])
        , (rest >> moveTo away, [])
        , (rest >> moveTo a, [])
        ]
  forM_ steps $ \(act, want) -> checkEq want =<< tipsAfter act
  check . (> 0) =<< wakeTime

notWidgetUi :: NanoUI Response
notWidgetUi = column $ do
  l <- labelWith' (fixedW 200) "Label target"
  tooltipConfigured (cfg 0.05 0.3) l "Label tip"
  -- A container, as 'withTooltip' wraps its target in, with a short delay.
  (_, holder) <- columnWith' (fixedW 200 . fixedH 60) (label "Container target")
  l <$ tooltipWidgetConfigured (cfg 0.05 0.3) holder (label "Box tip")

-- | Entering or leaving a label or container with a tooltip needs a frame,
-- though the hover probe finds no widget there; moving within it does not.
tooltipTargetNotWidget :: Sim Response ()
tooltipTargetNotWidget = do
  V2 lx ly <- centerOf <$> result
  onBox <- spanCenter <$> rectOf "Container target"
  _ <- moveTo away
  check =<< redrawsFor (V2 lx ly)
  checkEq [] =<< tipsAfter (moveTo (At (V2 lx ly)))
  check . not =<< redrawsFor (V2 (lx + 5) ly)
  checkEq ["Label tip"] =<< tipsAfter (sleep 0.06 >> frame)
  check =<< redrawsFor (V2 390 290)
  checkEq [] =<< tipsAfter (moveTo away)
  check =<< redrawsFor onBox
  checkEq ["Box tip"] =<< tipsAfter (moveTo (At onBox) >> sleep 0.06 >> frame)
  check =<< redrawsFor (V2 390 290)

-- | A large button with an immediate tooltip placed at the pointer.
followUi :: NanoUI Response
followUi = column $ do
  target <- buttonWith' (fixedW 360 . fixedH 240) "Canvas"
  target <$ tooltipConfigured (cfg 0 0.3) {tooltipPlacement = PlacementAtCursor} target "Follow tip"

-- | A pointer-placed tooltip sits below the pointer and follows it (moves
-- need frames), repainting old and new rects. A pointer on the tooltip still
-- counts as over the target, and the tooltip moves out from under it. Near
-- the bottom it flips above the pointer to stay in the window.
tooltipFollowCursor :: Sim Response ()
tooltipFollowCursor = do
  modifyInput (\i -> i {inputDeltaTime = 1})
  Rect tx ty tw th <- respRect <$> result
  let tipAt p = moveTo (At p) >> tipRects
      below (V2 px py) (Rect x y _ _) = checkEq (px, py + 20) (x, y)
      p1 = V2 (tx + 40) (ty + 40)
      p2 = V2 (tx + 120) (ty + 90)
  [r1] <- tipAt p1 >> tipAt p1
  below p1 r1
  checkEq [False, True] =<< mapM redrawsFor [p1, p2]
  [r2] <- tipAt p2
  below p2 r2
  dmg <- frameDamage
  check (damageCovers dmg r1 && damageCovers dmg r2)
  [r3] <- tipAt (spanCenter r2)
  below (spanCenter r2) r3
  [Rect x4 y4 w4 h4] <- tipAt (V2 (tx + tw - 2) (ty + th - 2))
  check (x4 >= 0 && x4 + w4 <= 400 && y4 >= 0 && y4 + h4 <= ty + th - 2)
