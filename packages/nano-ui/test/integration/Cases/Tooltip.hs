module Cases.Tooltip (tests) where

import Control.Concurrent (threadDelay)
import Spec
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "tooltip-hover" runTooltipHoverTest
  , spec "tooltip-scroll-pos" runTooltipScrollPosTest
  , spec "tooltip-disabled-target" runTooltipDisabledTargetTest
  , spec "tooltip-gap" runTooltipGapTest
  ]

-- | Hold the pointer still past the default tooltip delay, then return the
-- next frame's result.
restOn :: Context -> Input -> NanoUI a -> IO a
restOn ctx inp ui = do
  _ <- runFrame ctx inp ui
  threadDelay (round (tooltipDelay defaultTooltipConfig * 1e6) + 50000)
  evalUi ctx inp ui

-- Resting on a target shows its text tooltip. A widget tooltip evaluates its
-- body only while shown.
runTooltipHoverTest :: Context -> IORef Int -> IO ()
runTooltipHoverTest ctx failed = do
  let inp0 = withInput 640 480
      ui = rowWith fillW $ do
        help <- button' "Help Target"
        tooltip help "Helpful advice here"
        _ <- spacer (Grow 1) Fit
        rich <- button' "Rich Info"
        body <- tooltipWidget rich $ do
          row $ do
            void (label "[Icon]")
            label "Rich tooltip body text"
        pure (help, rich, body)

  -- Unhovered: no tooltip overlay, and the widget body is not evaluated
  (help, rich, body0) <- warmup2 ctx inp0 ui
  spans0 <- collectOverlayTextSpans ctx inp0
  assert failed (not (hasText "Helpful advice" spans0))
  assert failed (case body0 of Nothing -> True; _ -> False)

  -- Hovered: no tooltip until the pointer rests
  let hoverHelp = inp0 {inputMousePos = centerOf help}
  _ <- runFrame ctx hoverHelp ui
  assert failed . not . hasText "Helpful advice" =<< collectOverlayTextSpans ctx hoverHelp
  _ <- restOn ctx hoverHelp ui
  spans1 <- collectOverlayTextSpans ctx hoverHelp
  assert failed (hasText "Helpful advice" spans1)

  let hoverRich = inp0 {inputMousePos = centerOf rich}
  (_, _, body1) <- restOn ctx hoverRich ui
  assert failed (case body1 of Just _ -> True; Nothing -> False)

runTooltipScrollPosTest :: Context -> IORef Int -> IO ()
runTooltipScrollPosTest ctx failed = do
  let inp0 = withInput 200 200
      ui =
        scrollArea (fillW . fixedH 80) $
          column $ do
            mapM_ (\_ -> void (label "pad line")) [(1 :: Int) .. 40]
            btn <- button' "Tip Target"
            tooltip btn "Scrolled tip text"
            mapM_ (\_ -> void (label "tail line")) [(1 :: Int) .. 12]
            pure btn
  (sid, _) <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx sid) $ \scrollRect@(Rect _ sy _ sh) -> do
    let hover = inp0 {inputMousePos = spanCenter scrollRect}
        wheel = hover {inputScroll = V2 0 1}
        inView btn =
          let y = rectY (respRect btn)
              h = rectH (respRect btn)
           in y >= sy + 4 && y + h + 16 <= sy + sh
        pump = do
          before <- getScrollOffset ctx sid
          _ <- runFrame ctx wheel ui
          after <- getScrollOffset ctx sid
          ((_, btn), _, _, _) <- runFrame ctx hover ui
          if inView btn || after <= before then pure (after, btn) else pump
    (off, btn1) <- pump
    assert failed (off > 0)
    let hoverInp = inp0 {inputMousePos = centerOf btn1}
        visualBottom = rectY (respRect btn1) + rectH (respRect btn1)
        layoutBottom = visualBottom + off
    _ <- restOn ctx hoverInp ui
    spans <- collectOverlayTextSpans ctx hoverInp
    let ys =
          [ rectY r
          | (r, txt, _, _, _) <- spans
          , "Scrolled tip" `T.isInfixOf` txt
          ]
    case ys of
      [] -> assert failed False
      (tipY : _) -> do
        assert failed (abs (tipY - visualBottom) <= 16)
        assert failed (abs (tipY - visualBottom) < abs (tipY - layoutBottom))

-- | A disabled widget shows its tooltip where the pointer is on it and
-- nothing covers it, even though it reports no hover.
runTooltipDisabledTargetTest :: Context -> IORef Int -> IO ()
runTooltipDisabledTargetTest ctx failed = do
  let inp0 = withInputOff 400 200
      cfg = defaultTooltipConfig {tooltipDelay = 0}
      ui = columnWith tight $ do
        off <- disabledWhen True (buttonWith' (fixedWH 200 40) "Off")
        tooltipConfigured cfg off "Why it is off"
        _ <- buttonWith' (pinAt 150 0 . fixedWH 40 40) "cover"
        pure off
      tipAt p = do
        let inp = inp0 {inputMousePos = p}
        warmup ctx inp ui
        r <- evalUi ctx inp ui
        (,) (respHovered r) . hasText "Why it is off" <$> collectOverlayTextSpans ctx inp
  _ <- warmup2 ctx inp0 ui
  tipAt (V2 60 20) >>= assertEq failed (False, True)
  tipAt (V2 170 20) >>= assertEq failed (False, False)
  tipAt (V2 300 150) >>= assertEq failed (False, False)

-- | 'tooltipGap' sets the space between the tooltip and its target.
runTooltipGapTest :: Context -> IORef Int -> IO ()
runTooltipGapTest ctx failed = do
  let inp0 = withInputOff 400 300
      ui gap' = columnWith tight $ do
        b <- buttonWith' (fixedWH 100 30) "Target"
        tooltipConfigured defaultTooltipConfig {tooltipDelay = 0, tooltipGap = gap'} b "Gap tip"
        pure b
      tipTop gap' = do
        b <- warmup2 ctx inp0 (ui gap')
        let inp = inp0 {inputMousePos = centerOf b}
        warmup ctx inp (ui gap')
        warmup ctx inp (ui gap')
        spans <- collectOverlayTextSpans ctx inp
        pure (rectY <$> spanRect "Gap tip" spans)
  near <- tipTop 4
  far <- tipTop 20
  assertEq failed (Just 16) ((-) <$> far <*> near)
