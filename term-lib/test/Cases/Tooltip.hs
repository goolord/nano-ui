module Cases.Tooltip
  ( runTooltipHoverTest
  , runTooltipWidgetTest
  , runTooltipSpansTest
  , runTooltipIdStableTest
  , runTooltipScrollPosTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, eval2Ui, evalUi, withInput)
import NanoUI.Testing.Harness (centerOf, warmup2)

runTooltipHoverTest :: Context -> IORef Int -> IO ()
runTooltipHoverTest ctx failed = do
  let inp0 = withInput 640 480
      ui = column defaultLayout $ do
        btn <- button "Help Target"
        tooltip "Helpful advice here" btn

  -- Unhovered: no tooltip overlay
  _ <- evalUi ctx inp0 ui
  spans0 <- collectOverlayTextSpans ctx inp0
  assert failed (not (any (\(_, txt, _, _, _) -> "Helpful advice" `T.isInfixOf` txt) spans0))

  -- Hovered: tooltip overlay present
  btnWarm <- eval2Ui ctx inp0 (button "Help Target")
  let hoverInp = inp0 {inputMousePos = centerOf btnWarm}
  _ <- runFrame ctx hoverInp ui
  _ <- runFrame ctx hoverInp ui
  spans1 <- collectOverlayTextSpans ctx hoverInp
  assert failed (any (\(_, txt, _, _, _) -> "Helpful advice" `T.isInfixOf` txt) spans1)

runTooltipWidgetTest :: Context -> IORef Int -> IO ()
runTooltipWidgetTest ctx failed = do
  let inp0 = withInput 640 480
      ui = column defaultLayout $ do
        btn <- button "Rich Info"
        tooltipWidget btn $ do
          row defaultLayout $ do
            void (label "[Icon]")
            label "Rich tooltip body text"

  -- When unhovered, child is not evaluated / rendered
  mBody0 <- evalUi ctx inp0 ui
  assert failed (case mBody0 of Nothing -> True; _ -> False)

  -- When hovered, child is evaluated / rendered
  btnWarm <- eval2Ui ctx inp0 (button "Rich Info")
  let hoverInp = inp0 {inputMousePos = centerOf btnWarm}
  _ <- runFrame ctx hoverInp ui
  (mBody1, _, _, _) <- runFrame ctx hoverInp ui
  assert failed (case mBody1 of Just _ -> True; Nothing -> False)

runTooltipSpansTest :: Context -> IORef Int -> IO ()
runTooltipSpansTest ctx failed = do
  let inp0 = withInput 640 480
      ui = withTooltip (button "Action Button") (label "Detailed description")

  btnWarm <- eval2Ui ctx inp0 (button "Action Button")
  let hoverInp = inp0 {inputMousePos = centerOf btnWarm}
  _ <- runFrame ctx hoverInp ui
  _ <- runFrame ctx hoverInp ui
  spans <- collectOverlayTextSpans ctx hoverInp
  assert failed (any (\(_, txt, _, _, _) -> "Detailed description" `T.isInfixOf` txt) spans)

runTooltipIdStableTest :: Context -> IORef Int -> IO ()
runTooltipIdStableTest ctx failed = do
  let inp0 = withInput 640 480
      ui = column defaultLayout $ do
        a <- button "Help Target"
        tooltip "tip" a
        b <- button "After"
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
        scrollArea (defaultLayout {layoutWidth = Grow 1, layoutHeight = Fixed 80}) $
          column defaultLayout $ do
            mapM_ (\_ -> void (label "pad line")) [(1 :: Int) .. 40]
            btn <- button "Tip Target"
            tooltip "Scrolled tip text" btn
            mapM_ (\_ -> void (label "tail line")) [(1 :: Int) .. 12]
            pure btn
  (sid, _) <- warmup2 ctx inp0 ui
  mScroll <- getPrevRect ctx sid
  case mScroll of
    Nothing -> assert failed False
    Just (Rect sx sy sw sh) -> do
      let hover = inp0 {inputMousePos = V2 (sx + sw / 2) (sy + sh / 2)}
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
