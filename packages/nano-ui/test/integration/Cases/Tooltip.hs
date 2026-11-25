{-# OPTIONS_GHC -O0 #-}
module Cases.Tooltip (tests) where

import Spec
import NanoUI.Internal.Context (getFocusables)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ simSpec "tooltip-targets" 400 300 targetsUi tooltipTargets
  , spec "tooltip-passthrough" runTooltipPassThroughTest
  ]

now :: TooltipConfig
now = defaultTooltipConfig {tooltipDelay = 0}

targetsUi :: NanoUI (Response, Response, Response)
targetsUi = columnWith tight $ do
  off <- disabledWhen True (buttonWith' (fixedWH 200 40) "Off")
  tooltipConfigured now off "Why it is off"
  _ <- buttonWith' (pinAt 150 0 . fixedWH 40 40) "cover"
  near <- buttonWith' (fixedWH 100 30) "Near"
  tooltipConfigured now {tooltipGap = 4} near "Near tip"
  far <- buttonWith' (fixedWH 100 30) "Far"
  tooltipWidgetConfigured now {tooltipGap = 20} far (label "Far tip")
  pure (off, near, far)

-- | A disabled widget shows its tooltip where the pointer is on it uncovered,
-- though it reports no hover. 'tooltipGap' sets the space below the target,
-- and a widget tooltip evaluates its body only while shown.
tooltipTargets :: Sim (Response, Response, Response) ()
tooltipTargets = do
  (_, near, far) <- result
  let offTip p = do
        (off, _, _) <- moveTo (At p) >> settle
        shown <- labelShown "Why it is off"
        farShown <- labelShown "Far tip"
        pure (respHovered off, farShown, shown)
      gapUnder r tip = do
        _ <- moveTo (On r) >> settle
        farShown <- labelShown "Far tip"
        (\t -> (farShown, rectY t - rectY (respRect r) - rectH (respRect r))) <$> rectOf tip
  checkEq [(False, False, True), (False, False, False), (False, False, False)]
    =<< mapM offTip [V2 60 20, V2 170 20, V2 300 150]
  (farNear, g4) <- gapUnder near "Near tip"
  (farFar, g20) <- gapUnder far "Far tip"
  checkEq (False, True, 16) (farNear, farFar, g20 - g4)

-- | A rich tooltip passes pointer events through to widgets underneath and
-- keeps controls inside its body inert (outside the keyboard focus order).
runTooltipPassThroughTest :: Context -> IORef Int -> IO ()
runTooltipPassThroughTest ctx failed = do
  tipClicks <- newIORef (0 :: Int)
  underClicks <- newIORef (0 :: Int)
  let ui = columnWith (tight . gap 4) $ do
        top <- buttonWith' (fixedWH 140 30) "Top"
        under <- buttonWith' (fixedWH 140 30) "Underneath"
        when (respClicked under) (liftIO (modifyIORef' underClicks (+ 1)))
        tooltipWidgetConfigured now {tooltipGap = 4} top $ do
          tipBtn <- buttonWith' (fixedWH 140 30) "TipBtn"
          when (respClicked tipBtn) (liftIO (modifyIORef' tipClicks (+ 1)))
        pure (top, under)
  runSim ctx failed (withInput 400 200) ui $ do
    (top, under) <- result
    -- Hover "Top" so the tooltip opens directly over "Underneath".
    _ <- moveTo (On top) >> settle
    check =<< labelShown "TipBtn"
    -- Focusables must include only "Top" and "Underneath", never "TipBtn".
    focs <- onContext getFocusables
    checkEq [respId top, respId under] focs
    -- Clicking over "Underneath" (covered visually by the tooltip) reaches "Underneath".
    _ <- click (On under) >> settle
    checkEq (0, 1) =<< liftIO ((,) <$> readIORef tipClicks <*> readIORef underClicks)
