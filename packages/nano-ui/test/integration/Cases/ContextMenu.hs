module Cases.ContextMenu (tests) where

import Spec
import Data.Text qualified as T

tests :: [Spec]
tests =
  [ spec "context-menu-open" runContextMenuOpenTest
  , spec "context-menu-scroll-pos" runContextMenuScrollPosTest
  , spec "context-menu-disabled-row" runContextMenuDisabledRowTest
  , spec "menu-button-popup-toggle" runMenuButtonPopupToggleTest
  , spec "popup-point-flip" runPopupPointFlipTest
  ]

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
  ((btnDown, _), _, _) <- runFrame ctx inpRightDown menuUi
  assert failed (not (respRightClicked btnDown))
  ((btnUp, mInside), _, _) <- runFrame ctx inpRightUp menuUi
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
  (_, mAfterClick) <- runClick ctx inp0 menuUi (V2 500 400)
  assert failed (case mAfterClick of Nothing -> True; Just _ -> False)
  -- So does a right press outside.
  _ <- openMenu ctx failed inp0
  let inpRightOut = fst (rightClickPair inp0 (V2 500 400))
  _ <- runFrame ctx inpRightOut menuUi
  ((_, mAfterRight), _, _) <- runFrame ctx inp0 menuUi
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
  assertJustM failed (getPrevRect ctx sid) $ \scrollRect@(Rect _ sy _ sh) -> do
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
          ((_, (btn, _)), _, _) <- runFrame ctx hover ui
          if inView btn || after <= before then pure (after, btn) else pump
    (off, btn1) <- pump
    assert failed (off > 0)
    let menuPos = centerOf btn1
        layoutY = rectY (respRect btn1) + off
        (inpRightDown, inpRightUp) = rightClickPair inp0 menuPos
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
            pick = spanCenter r
        assert failed (abs (menuY - v2Y menuPos) <= 16)
        assert failed (abs (menuY - v2Y menuPos) < abs (menuY - layoutY))
        (_, (_, picked)) <- runClick ctx inp0 ui pick
        assert failed (picked == Just True)
        _ <- runFrame ctx inp0 ui
        spansAfter <- collectOverlayTextSpans ctx inp0
        assert failed (not (hasText "Scroll Cut" spansAfter))

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

-- | A menu title toggling its drop-down on 'respClicked'. Returns the title
-- and whether the drop-down was open going into the frame.
dropDownUi :: NanoUI (Response, Bool)
dropDownUi = column $ do
  openVar <- useVar False
  open <- readVar openVar
  btn <- menuButton' "File" open
  when (respClicked btn) (writeVar openVar (not open))
  let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
  (popR, _) <- popup' (constVar open) cfg (menuItem "New")
  when (respClicked popR) (writeVar openVar False)
  pure (btn, open)

-- | Clicking the title of an open drop-down closes it: the press on the title
-- is not a press outside the popup, which would close it for the release to
-- open it again. A click elsewhere still closes it.
runMenuButtonPopupToggleTest :: Context -> IORef Int -> IO ()
runMenuButtonPopupToggleTest ctx failed = do
  let inp0 = withInput 400 300
      clickAt p = runClick ctx inp0 dropDownUi p >> (snd <$> warmup2 ctx inp0 dropDownUi)
  (btn, open0) <- warmup2 ctx inp0 dropDownUi
  assertEq failed False open0
  opened <- clickAt (centerOf btn)
  assertEq failed True opened
  closed <- clickAt (centerOf btn)
  assertEq failed False closed
  reopened <- clickAt (centerOf btn)
  assertEq failed True reopened
  outside <- clickAt (V2 380 280)
  assertEq failed False outside

-- | A popup placed before or after a point flips to the other side when it
-- would run off the window there and fits on the other side, and it always
-- stays within the window margin.
runPopupPointFlipTest :: Context -> IORef Int -> IO ()
runPopupPointFlipTest _ failed = do
  -- A 100 by 80 popup in an 800 wide window, with a margin of 14.
  let place winH px py = computePopupPosition 800 winH 14 100 80 (AnchorPoint (V2 px py))
  -- Above a point near the top: no room there, so below it.
  assertEq failed (snd (place 600 400 30 PlacementAbove 4)) 34
  -- Left of a point near the left edge: no room there, so right of it.
  assertEq failed (fst (place 600 30 300 PlacementLeft 4)) 34
  -- Below a point near the bottom: above it, the offset clear of it.
  assertEq failed (snd (place 600 400 590 PlacementBelow 20)) 490
  -- Neither side has room for the popup and its offset: it keeps within
  -- the margin rather than flipping into it.
  assertEq failed (snd (place 200 400 110 PlacementBelow 20)) (200 - 80 - 14)
