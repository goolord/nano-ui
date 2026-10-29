{-# OPTIONS_GHC -O0 #-}
module Cases.Tooltip (tests) where

import Spec
import Data.Maybe (isJust)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ simSpec "tooltip-scroll-pos" 200 200 scrolledUi tooltipScrollPos
  , simSpec "tooltip-targets" 400 300 targetsUi tooltipTargets
  ]

now :: TooltipConfig
now = defaultTooltipConfig {tooltipDelay = 0}

scrolledUi :: NanoUI (WidgetId, Response)
scrolledUi = scrollArea (fillW . fixedH 80) $ column $ do
  replicateM_ 40 (label "pad line")
  btn <- button' "Tip Target"
  tooltipConfigured now btn "Scrolled tip text"
  replicateM_ 12 (label "tail line")
  pure btn

-- | A tooltip in a scrolled area appears under its target where it is
-- drawn, not at its unscrolled layout position.
tooltipScrollPos :: Sim (WidgetId, Response) ()
tooltipScrollPos = do
  ctx <- simContext
  (sid, _) <- result
  Just (off, btn) <- liftIO (wheelIntoView ctx (withInput 200 200) scrolledUi snd sid 16)
  checkGt off 0
  _ <- moveTo (On btn) >> settle
  tipY <- rectY <$> rectOf "Scrolled tip text"
  let bottom = rectY (respRect btn) + rectH (respRect btn)
  check (abs (tipY - bottom) <= 16)
  check (abs (tipY - bottom) < abs (tipY - (bottom + off)))

targetsUi :: NanoUI (Response, Response, Response, Maybe ())
targetsUi = columnWith tight $ do
  off <- disabledWhen True (buttonWith' (fixedWH 200 40) "Off")
  tooltipConfigured now off "Why it is off"
  _ <- buttonWith' (pinAt 150 0 . fixedWH 40 40) "cover"
  near <- buttonWith' (fixedWH 100 30) "Near"
  tooltipConfigured now {tooltipGap = 4} near "Near tip"
  far <- buttonWith' (fixedWH 100 30) "Far"
  body <- tooltipWidgetConfigured now {tooltipGap = 20} far (label "Far tip")
  pure (off, near, far, body)

-- | A disabled widget shows its tooltip where the pointer is on it uncovered,
-- though it reports no hover. 'tooltipGap' sets the space below the target,
-- and a widget tooltip evaluates its body only while shown.
tooltipTargets :: Sim (Response, Response, Response, Maybe ()) ()
tooltipTargets = do
  (_, near, far, _) <- result
  let offTip p = do
        (off, _, _, body) <- moveTo (At p) >> settle
        shown <- isJust <$> findSpan "Why it is off"
        pure (respHovered off, body, shown)
      gapUnder r tip = do
        (_, _, _, body) <- moveTo (On r) >> settle
        (\t -> (body, rectY t - rectY (respRect r) - rectH (respRect r))) <$> rectOf tip
  checkEq [(False, Nothing, True), (False, Nothing, False), (False, Nothing, False)]
    =<< mapM offTip [V2 60 20, V2 170 20, V2 300 150]
  (body4, g4) <- gapUnder near "Near tip"
  (body20, g20) <- gapUnder far "Far tip"
  checkEq (Nothing, Just (), 16) (body4, body20, g20 - g4)
