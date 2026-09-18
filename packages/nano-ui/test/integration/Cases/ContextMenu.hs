module Cases.ContextMenu
  ( runContextMenuOpenTest
  , runContextMenuScrollPosTest
  , runContextMenuDisabledRowTest
  ) where

import Control.Monad (void)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, evalUi, withInput)
import NanoUI.Testing.Harness (centerOf, clickPair, rightClickPair, spanCenter, warmup2)

menuUi :: NanoUI (Response, Maybe (Response, Response))
menuUi = column $ do
  btn <- button' "Target Button"
  mInside <- contextMenu btn $ do
    cut <- menuItem' "Cut"
    copy <- menuItem' "Copy"
    pure (cut, copy)
  pure (btn, mInside)

openMenu :: Context -> IORef Int -> Input -> IO (Response, Maybe (Response, Response))
openMenu ctx failed inp0 = do
  (btnWarm, _) <- warmup2 ctx inp0 menuUi
  let (inpRightDown, inpRightUp) = rightClickPair inp0 (centerOf btnWarm)
  ((btnDown, _), _, _, _) <- runFrame ctx inpRightDown menuUi
  assert failed (not (respRightClicked btnDown))
  ((btnUp, mInside), _, _, _) <- runFrame ctx inpRightUp menuUi
  pure (btnUp, mInside)

runContextMenuOpenTest :: Context -> IORef Int -> IO ()
runContextMenuOpenTest ctx failed = do
  let inp0 = withInput 640 480
  (btn0, mInside0) <- evalUi ctx inp0 menuUi
  assert failed (not (respRightClicked btn0))
  assert failed (case mInside0 of Nothing -> True; _ -> False)

  (btnClicked, mInsideOpen) <- openMenu ctx failed inp0
  assert failed (respRightClicked btnClicked)
  assert failed (case mInsideOpen of Just _ -> True; Nothing -> False)
  -- A left click outside dismisses the menu.
  let (pressOut, releaseOut) = clickPair inp0 (V2 500 400)
  _ <- runFrame ctx pressOut menuUi
  ((_, mAfterClick), _, _, _) <- runFrame ctx releaseOut menuUi
  assert failed (case mAfterClick of Nothing -> True; Just _ -> False)
  -- So does a right press outside.
  _ <- openMenu ctx failed inp0
  let inpRightOut = fst (rightClickPair inp0 (V2 500 400))
  _ <- runFrame ctx inpRightOut menuUi
  ((_, mAfterRight), _, _, _) <- runFrame ctx inp0 menuUi
  assert failed (case mAfterRight of Nothing -> True; Just _ -> False)

runContextMenuScrollPosTest :: Context -> IORef Int -> IO ()
runContextMenuScrollPosTest ctx failed = do
  let inp0 = withInput 200 200
      ui =
        scrollArea (fillW . fixedH 80) $
          column $ do
            mapM_ (\_ -> void (label "pad line")) [(1 :: Int) .. 40]
            btn <- button' "Menu Target"
            cut <- contextMenu btn (menuItem "Scroll Cut")
            mapM_ (\_ -> void (label "tail line")) [(1 :: Int) .. 12]
            pure (btn, cut)
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
             in y >= sy + 4 && y + h + 8 <= sy + sh
          pump = do
            before <- getScrollOffset ctx sid
            _ <- runFrame ctx wheel ui
            after <- getScrollOffset ctx sid
            ((_, (btn, _)), _, _, _) <- runFrame ctx hover ui
            if inView btn || after <= before then pure (after, btn) else pump
      (off, btn1) <- pump
      assert failed (off > 0)
      let clickPos = centerOf btn1
          layoutY = rectY (respRect btn1) + off
          (inpRightDown, inpRightUp) = rightClickPair inp0 clickPos
      _ <- runFrame ctx inpRightDown ui
      _ <- runFrame ctx inpRightUp ui
      spans <- collectOverlayTextSpans ctx inpRightUp
      let hits =
            [ r
            | (r, txt, _, _, _) <- spans
            , "Scroll Cut" `T.isInfixOf` txt
            ]
      case hits of
        [] -> assert failed False
        (r : _) -> do
          let menuY = rectY r
              pick = V2 (rectX r + rectW r / 2) (rectY r + rectH r / 2)
          assert failed (abs (menuY - v2Y clickPos) <= 16)
          assert failed (abs (menuY - v2Y clickPos) < abs (menuY - layoutY))
          let (press, release) = clickPair inp0 pick
          _ <- runFrame ctx press ui
          ((_, (_, picked)), _, _, _) <- runFrame ctx release ui
          assert failed (picked == Just True)
          _ <- runFrame ctx inp0 ui
          spansAfter <- collectOverlayTextSpans ctx inp0
          assert failed (not (any (\(_, txt, _, _, _) -> "Scroll Cut" `T.isInfixOf` txt) spansAfter))

-- | A disabled row lines up with the enabled rows around it: its label starts
-- at the same x and it takes the same row height.
runContextMenuDisabledRowTest :: Context -> IORef Int -> IO ()
runContextMenuDisabledRowTest ctx failed = do
  let inp0 = withInput 640 480
      ui = column $ do
        btn <- button' "Target Button"
        _ <- contextMenu btn $ do
          _ <- menuItem "Row Cut"
          menuItemDisabled "Row Paste"
          menuItem "Row Undo"
        pure (btn, ())
  (btnWarm, _) <- warmup2 ctx inp0 ui
  let (inpRightDown, inpRightUp) = rightClickPair inp0 (centerOf btnWarm)
  _ <- runFrame ctx inpRightDown ui
  _ <- runFrame ctx inpRightUp ui
  _ <- runFrame ctx inp0 ui
  spans <- collectOverlayTextSpans ctx inp0
  let find t = [r | (r, txt, _, _, _) <- spans, txt == t]
  case (find "Row Cut", find "Row Paste", find "Row Undo") of
    ([cut], [paste], [undo]) -> do
      assert failed (abs (rectX paste - rectX cut) < 0.5)
      assert failed (abs ((rectY paste - rectY cut) - (rectY undo - rectY paste)) < 0.5)
    _ -> assert failed False
