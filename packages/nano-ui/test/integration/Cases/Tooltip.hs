module Cases.Tooltip
  ( runTooltipHoverTest
  , runTooltipIdStableTest
  , runTooltipScrollPosTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, evalUi, withInput)
import NanoUI.Testing.Harness
  ( centerOf
  , hasText
  , spanCenter
  , warmup2
  )

-- Hovering shows a text tooltip, and a widget tooltip only evaluates its body
-- while hovered.
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

  -- Hovered: tooltip overlay present
  let hoverHelp = inp0 {inputMousePos = centerOf help}
  _ <- runFrame ctx hoverHelp ui
  _ <- runFrame ctx hoverHelp ui
  spans1 <- collectOverlayTextSpans ctx hoverHelp
  assert failed (hasText "Helpful advice" spans1)

  let hoverRich = inp0 {inputMousePos = centerOf rich}
  _ <- runFrame ctx hoverRich ui
  ((_, _, body1), _, _, _) <- runFrame ctx hoverRich ui
  assert failed (case body1 of Just _ -> True; Nothing -> False)

runTooltipIdStableTest :: Context -> IORef Int -> IO ()
runTooltipIdStableTest ctx failed = do
  let inp0 = withInput 640 480
      ui = column $ do
        a <- button' "Help Target"
        tooltip a "tip"
        b <- button' "After"
        pure (a, b)
  (a0, b0) <- evalUi ctx inp0 ui
  let hoverInp = inp0 {inputMousePos = centerOf a0}
  _ <- runFrame ctx hoverInp ui
  ((_, b1), _, _, _) <- runFrame ctx hoverInp ui
  assert failed (respId b0 == respId b1)

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
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Nothing -> assert failed False
    Just scrollRect@(Rect _ sy _ sh) -> do
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
      _ <- runFrame ctx hoverInp ui
      _ <- runFrame ctx hoverInp ui
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
