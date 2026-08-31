module Cases.Window
  ( runFitHeaderNoShrinkTest
  , runHeaderTopPadTest
  , runOverlayClickThroughTest
  , runOverlayPanelLiveTest
  , runSeparatorSpanTest
  , runWindowCloseDamageTest
  , runWindowDragDamageTest
  , runWindowDragTest
  , runWindowOverlayTest
  , runWindowResizeHaloHitTest
  , runWindowResizeTest
  , runWindowScrollGutterTest
  , runWindowScrollWheelTest
  ) where

import Control.Monad (replicateM, void, when)
import Data.IORef (IORef)
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (bump, failWhen, withInput)
import NanoUI.Testing.Harness
  ( assertWheelTitlePinned
  , clickPair
  , dragWindowEdge
  , runDragFrom
  , spanLabelYs
  , warmup2
  , windowTitleGrab
  , withInputOff
  )
runWindowScrollGutterTest :: Context -> IORef Int -> IO ()
runWindowScrollGutterTest ctx failed = do
  let
    inp0 = withInput 640 360
    long = T.pack (replicate 48 'M')
    ui = do
      (win, mwide) <-
        window True "GutterWin" $ do
          wide <- labelEx (fillW defaultLayout) "WWWW"
          kv "Key" long
          mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
          pure wide
      pure (win, mwide)
  (win, mwide) <- warmup2 ctx inp0 ui
  let
    Rect wx _ ww _ = respRect win
    contentRight = wx + ww - padR windowPad
  spans <- collectOverlayTextSpans ctx inp0
  let
    titleYs = [rectY r | (r, txt, _, _, _) <- spans, "GutterWin" `T.isInfixOf` txt]
  when (null titleYs) $ bump failed
  case mwide of
    Nothing -> bump failed
    Just wide -> do
      let
        Rect cx _ cw _ = respRect wide
      when (cx + cw < contentRight - 0.5) $ bump failed
      when (cx + cw > contentRight + 0.01) $ bump failed

runWindowCloseDamageTest :: Context -> IORef Int -> IO ()
runWindowCloseDamageTest _ failed = do
  ctx <- newContext
  let
    ui open = void (window open "Debug" (label "Body"))
    inp0 = withInput 640 400
  _ <- runFrame ctx inp0 (ui True)
  _ <- runFrame ctx inp0 (ui True)
  _ <- runFrame ctx inp0 (ui False)
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed
  let
    idle = inp0 {inputDeltaTime = 1}
  need <- needsRedraw ctx inp0 idle
  failWhen failed (not need)

runWindowDragDamageTest :: Context -> IORef Int -> IO ()
runWindowDragDamageTest _ failed = do
  ctx <- newContext
  let
    inp0 = withInput 640 400
    ui = do
      (win, _) <- window True "Debug" (label "Body")
      pure win
  win0 <- warmup2 ctx inp0 ui
  let
    Rect x0 y0 _ _ = respRect win0
    dest = V2 (x0 + 24 - 50) (y0 + 22 + 30)
  runDragFrom ctx inp0 ui (windowTitleGrab (respRect win0)) dest
  dmg <- takeDamage ctx
  when (dmg /= DamageFull) $ bump failed

runOverlayPanelLiveTest :: Context -> IORef Int -> IO ()
runOverlayPanelLiveTest _ failed = do
  let
    inp = withInputOff 320 240
    checkStatic ui = do
      ctx <- newContext
      _ <- runFrame ctx inp ui
      _ <- runFrame ctx inp ui
      need <- needsRedraw ctx inp inp
      when need $ bump failed
      _ <- runFrame ctx inp ui
      dmg <- takeDamage ctx
      when (not (damageIsEmpty dmg)) $ bump failed
    checkWindowLive ui = do
      ctx <- newContext
      _ <- runFrame ctx inp ui
      _ <- runFrame ctx inp ui
      need <- needsRedraw ctx inp inp
      failWhen failed (not need)
      _ <- runFrame ctx inp ui
      dmg <- takeDamage ctx
      when (damageIsEmpty dmg) $ bump failed
    checkDirtyWake ui = do
      ctx <- newContext
      _ <- runFrame ctx inp ui
      markDirty ctx
      need <- needsRedraw ctx inp inp
      failWhen failed (not need)
      _ <- runFrame ctx inp ui
      dmg <- takeDamage ctx
      when (dmg /= DamageFull) $ bump failed
  checkWindowLive (void (window True "Debug" (label "fps 0")))
  checkStatic (void (modal True "About" (label "body")))
  checkDirtyWake (void (modal True "About" (label "body")))

runHeaderTopPadTest :: Context -> IORef Int -> IO ()
runHeaderTopPadTest ctx failed = do
  let
    inp = withInput 800 600
    ui =
      column (padAll 12 . gap 8 . grow $ defaultLayout)
        $ panel (padXY 16 12 . fillW $ defaultLayout)
        $ label "nano-ui SDL3 demo"
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  let
    Rect _ y _ _ = respRect resp
  when (y < 24) $ bump failed

runFitHeaderNoShrinkTest :: Context -> IORef Int -> IO ()
runFitHeaderNoShrinkTest ctx failed = do
  let
    header = panel (padXY 16 12 . fillW $ defaultLayout) (label "nano-ui SDL3 demo")
    only =
      column (padAll 12 . grow $ defaultLayout) header
    withBody = do
      r <-
        column (padAll 12 . gap 8 . grow $ defaultLayout) $ do
          h <- header
          scroll (tight (grow defaultLayout))
            $ column (fillW defaultLayout)
            $ mapM_ (label_ . T.pack . show) [1 .. 40 :: Int]
          pure h
      pure r
    tall = withInput 400 800
    short = withInput 400 200
  _ <- runFrame ctx tall only
  (r0, _, _, _) <- runFrame ctx tall only
  _ <- runFrame ctx short withBody
  (r1, _, _, _) <- runFrame ctx short withBody
  when (rectH (respRect r1) + 0.5 < rectH (respRect r0)) $ bump failed

runWindowOverlayTest :: Context -> IORef Int -> IO ()
runWindowOverlayTest ctx failed = do
  let
    inp0 = withInput 640 400
    ui = do
      outside <- button "Outside"
      (win, mBody) <-
        window True "Debug" $ do
          label "Body"
      pure (outside, win, mBody)
    closedUi = do
      _ <- button "Outside"
      (win, mBody) <- window False "Debug" (label "Body")
      pure (win, mBody)
  do
    ((win, mBody), _, _, _) <- runFrame ctx inp0 closedUi
    when (respClicked win) $ bump failed
    case mBody of
      Nothing -> pure ()
      Just _ -> bump failed
    closedSpans <- collectOverlayTextSpans ctx inp0
    when (any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) closedSpans) $
      bump failed
  (outside0, win0, mBody0) <- warmup2 ctx inp0 ui
  overlays <- collectOverlayTextSpans ctx inp0
  let
    hasTitle = any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) overlays
    hasBody = any (\(_, txt, _, _, _) -> "Body" `T.isInfixOf` txt) overlays
    hasCloseGlyph = any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays
  when (not (hasTitle && hasBody)) $ bump failed
  when hasCloseGlyph $ bump failed
  let
    Rect wx wy ww wh = respRect win0
  when (ww < 100 || wh < 20) $ bump failed
  case mBody0 of
    Nothing -> bump failed
    Just _ -> pure ()
  let
    clickOut =
      inp0
        { inputMousePos =
            V2 (rectX (respRect outside0) + 8) (rectY (respRect outside0) + 8)
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx clickOut ui
  let
    releaseOut =
      clickOut
        { inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = True
        }
  ((outsideHit, _, _), _, _, _) <- runFrame ctx releaseOut ui
  when (not (respClicked outsideHit)) $ bump failed
  let
    mid = V2 (wx + ww / 2) (wy + wh * 0.7)
    clickWin =
      inp0
        { inputMousePos = mid
        , inputMouseDown = True
        , inputMousePressed = True
        }
  ((outsideMid, _, _), _, _, _) <- runFrame ctx clickWin ui
  when (respClicked outsideMid) $ bump failed
  let
    esc = inp0 {inputKeys = inputKeysFromList [KeyEscape]}
  ((_, winEsc, _), _, _, _) <- runFrame ctx esc ui
  when (respClicked winEsc) $ bump failed
  let
    closeAt = V2 (wx + ww - padR windowPad - 14) (wy + padT windowPad + 14)
    clickClose =
      inp0
        { inputMousePos = closeAt
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx clickClose ui
  let
    releaseClose =
      clickClose
        { inputMouseDown = False
        , inputMousePressed = False
        , inputMouseReleased = True
        }
  ((_, winClose, _), _, _, _) <- runFrame ctx releaseClose ui
  when (not (respClicked winClose)) $ bump failed

runOverlayClickThroughTest :: Context -> IORef Int -> IO ()
runOverlayClickThroughTest _ failed = do
  ctx <- newContext
  let
    inp0 = withInput 300 220
    windowUi = do
      outsides <- column defaultLayout (replicateM 10 (button "Outside"))
      (win, mInside) <-
        window True "Cover" $ do
          button "Inside"
      pure (outsides, win, mInside)
    modalUi = do
      outsides <- column defaultLayout (replicateM 10 (button "Outside"))
      (dlg, mInside) <-
        modal True "Cover" $ do
          button "Inside"
      pure (outsides, dlg, mInside)
    stackedUi = do
      (lo, mLo) <- window True "Low" (button "LowBtn")
      (hi, mHi) <- window True "High" (button "HighBtn")
      pure (lo, mLo, hi, mHi)
    childSafePoint cover childRects =
      let
        Rect x y w h = cover
        titleSkip = 40
        cands =
          [ V2 (x + 6) (y + h * 0.72)
          , V2 (x + w - 6) (y + h * 0.72)
          , V2 (x + w / 2) (y + h - 6)
          , V2 (x + 6) (y + h - 6)
          , V2 (x + w - 6) (y + titleSkip + 6)
          ]
        inCover p = rectContains cover p
        missesKids p = not (any (`rectContains` p) childRects)
       in
        case filter (\p -> inCover p && missesKids p) cands of
          (p : _) -> Just p
          [] -> Nothing
    clickNone clicked ui pos = do
      let
        (press, release) = clickPair inp0 pos
      _ <- runFrame ctx press ui
      runFrame ctx release ui
        >>= \(hit, _, _, _) -> when (clicked hit) (bump failed)
    runCovered ui = do
      _ <- runFrame ctx inp0 ui
      _ <- runFrame ctx inp0 ui
      ((_, cover0, mInside0), _, _, _) <- runFrame ctx inp0 ui
      let
        coverRect = respRect cover0
      when (rectW coverRect <= 0 || rectH coverRect <= 0) $ bump failed
      case mInside0 of
        Nothing -> bump failed
        Just inside0 -> do
          let
            kids = [respRect inside0]
          case childSafePoint coverRect kids of
            Nothing -> bump failed
            Just pos -> do
              let
                (press, release) = clickPair inp0 pos
              _ <- runFrame ctx press ui
              ((outsidesHit, _, _), _, _, _) <- runFrame ctx release ui
              when (any respClicked outsidesHit) $ bump failed
          let
            ir = respRect inside0
            ip = V2 (rectX ir + rectW ir / 2) (rectY ir + rectH ir / 2)
          when (rectW ir <= 0 || rectH ir <= 0) $ bump failed
          let
            (ipress, irelease) = clickPair inp0 ip
          _ <- runFrame ctx ipress ui
          ((_, _, mInsideHit), _, _, _) <- runFrame ctx irelease ui
          case mInsideHit of
            Just r -> when (not (respClicked r)) $ bump failed
            Nothing -> bump failed
    runStacked = do
      _ <- runFrame ctx inp0 stackedUi
      _ <- runFrame ctx inp0 stackedUi
      ((_, mLo0, hi0, mHi0), _, _, _) <- runFrame ctx inp0 stackedUi
      case (mLo0, mHi0) of
        (Just loBtn, Just hiBtn) -> do
          let
            cover = respRect hi0
            kids = [respRect loBtn, respRect hiBtn]
          when (rectW cover <= 0 || rectH cover <= 0) $ bump failed
          case childSafePoint cover kids of
            Nothing -> bump failed
            Just pos ->
              clickNone
                (\(_, loHit, _, _) -> maybe False respClicked loHit)
                stackedUi
                pos
          let
            hp =
              V2
                (rectX (respRect hiBtn) + rectW (respRect hiBtn) / 2)
                (rectY (respRect hiBtn) + rectH (respRect hiBtn) / 2)
            (hpress, hrelease) = clickPair inp0 hp
          _ <- runFrame ctx hpress stackedUi
          ((_, _, _, mHiHit), _, _, _) <- runFrame ctx hrelease stackedUi
          case mHiHit of
            Just r -> when (not (respClicked r)) $ bump failed
            Nothing -> bump failed
        _ -> bump failed
  runCovered windowUi
  runCovered modalUi
  runStacked

runWindowDragTest :: Context -> IORef Int -> IO ()
runWindowDragTest ctx failed = do
  let
    inp0 = withInput 640 400
    ui = do
      (win, _) <- window True "Debug" (label "Body")
      pure win
  win0 <- warmup2 ctx inp0 ui
  let
    r0 = respRect win0
    x0 = rectX r0
    y0 = rectY r0
    dest = V2 (x0 + 24 - 50) (y0 + 22 + 30)
  runDragFrom ctx inp0 ui (windowTitleGrab r0) dest
  (win1, _, _, _) <- runFrame ctx (inp0 {inputMousePos = dest}) ui
  let
    Rect x1 y1 _ _ = respRect win1
  when (x1 >= x0 - 10) $ bump failed
  when (y1 <= y0 + 10) $ bump failed

runWindowScrollWheelTest :: Context -> IORef Int -> IO ()
runWindowScrollWheelTest ctx failed = do
  let
    inp0 = withInput 320 220
    line1 = T.pack "line 1"
    title = T.pack "Scroll"
    ui = do
      (win, _) <-
        window True "Scroll"
          $ column defaultLayout
          $ mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
      pure win
  win <- warmup2 ctx inp0 ui
  let
    Rect wx _ ww wh = respRect win
  failWhen failed (ww <= 0 || wh <= 0)
  spans0 <- collectOverlayTextSpans ctx inp0
  case spanLabelYs line1 spans0 of
    [] -> bump failed
    b0 : _ ->
      assertWheelTitlePinned failed ctx inp0 ui title line1 (V2 (wx + ww / 2) (b0 + 2)) Nothing

runWindowResizeTest :: Context -> IORef Int -> IO ()
runWindowResizeTest ctx failed = do
  let
    inp0 = withInput 640 400
    ui = do
      (win, _) <- window True "Resize" (label "Body")
      pure win
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  mrect0 <- getPrevRect ctx (respId win0)
  case mrect0 of
    Nothing -> bump failed
    Just (Rect x0 y0 w0 h0) -> do
      when (w0 <= 0 || h0 <= 0) $ bump failed
      let
        hoverAt p = inp0 {inputMousePos = p}
        expectCursor p kind = do
          k <- uiCursorKind ctx (hoverAt p)
          when (k /= kind) $ bump failed
      expectCursor (V2 (x0 + w0 + 4) (y0 + h0 + 4)) UiCursorNwseResize
      expectCursor (V2 (x0 - 4) (y0 - 4)) UiCursorNwseResize
      expectCursor (V2 (x0 + w0 + 4) (y0 - 4)) UiCursorNeswResize
      expectCursor (V2 (x0 - 4) (y0 + h0 + 4)) UiCursorNeswResize
      expectCursor (V2 (x0 + w0 / 2) (y0 - 4)) UiCursorNsResize
      expectCursor (V2 (x0 + w0 / 2) (y0 + h0 + 4)) UiCursorNsResize
      expectCursor (V2 (x0 - 4) (y0 + h0 / 2)) UiCursorEwResize
      expectCursor (V2 (x0 + w0 + 4) (y0 + h0 / 2)) UiCursorEwResize
      expectCursor (V2 (x0 + w0 - 5) (y0 + h0 / 2)) UiCursorEwResize
      insideKind <-
        uiCursorKind ctx (hoverAt (V2 (x0 + w0 - padR windowPad - 4) (y0 + h0 / 2)))
      when (insideKind == UiCursorEwResize) $ bump failed
      mSe <-
        dragWindowEdge
          ctx
          inp0
          ui
          (V2 (x0 + w0 + 4) (y0 + h0 + 4))
          (V2 (x0 + w0 + 40) (y0 + h0 + 30))
      case mSe of
        Nothing -> bump failed
        Just (Rect x1 y1 w1 h1) -> do
          when (w1 <= w0 + 20) $ bump failed
          when (h1 <= h0 + 15) $ bump failed
          mW <-
            dragWindowEdge
              ctx
              inp0
              ui
              (V2 (x1 - 4) (y1 + h1 / 2))
              (V2 (x1 - 36) (y1 + h1 / 2))
          case mW of
            Nothing -> bump failed
            Just (Rect xw yw ww hw) -> do
              when (ww <= w1 + 15) $ bump failed
              when (xw >= x1 - 10) $ bump failed
              mN <-
                dragWindowEdge
                  ctx
                  inp0
                  ui
                  (V2 (xw + ww / 2) (yw - 4))
                  (V2 (xw + ww / 2) (yw - 20))
              case mN of
                Nothing -> bump failed
                Just (Rect xn yn wn hn) -> do
                  when (hn <= hw + 8) $ bump failed
                  when (yn >= yw - 5) $ bump failed
                  let
                    minTitleH = padT windowPad + 28 + padB windowPad
                  mShort <-
                    dragWindowEdge
                      ctx
                      inp0
                      ui
                      (V2 (xn + wn / 2) (yn + hn + 4))
                      (V2 (xn + wn / 2) (yn + 4))
                  case mShort of
                    Nothing -> bump failed
                    Just (Rect _ _ _ hMin) ->
                      when (hMin + 0.01 < minTitleH) $ bump failed

runWindowResizeHaloHitTest :: Context -> IORef Int -> IO ()
runWindowResizeHaloHitTest ctx failed = do
  let
    inp0 = withInput 640 400
    ui = do
      btn <- button "Hit"
      (win, _) <- window True "Resize" (label "Body")
      pure (btn, win)
  (btn0, win0) <- warmup2 ctx inp0 ui
  let
    Rect bx by bw bh = respRect btn0
    Rect x0 y0 _ _ = respRect win0
    grab = V2 (x0 + 24) (y0 + 22)
    destX = bx + bw + 4
    press =
      inp0
        { inputMousePos = grab
        , inputMouseDown = True
        , inputMousePressed = True
        }
  _ <- runFrame ctx press ui
  let
    moved =
      press
        { inputMousePos = V2 (destX + 24) (y0 + 22)
        , inputMousePressed = False
        }
  _ <- runFrame ctx moved ui
  ((_, win1), _, _, _) <-
    runFrame ctx (inp0 {inputMousePos = V2 destX (y0 + 22)}) ui
  let
    Rect x1 y1 _ h1 = respRect win1
    hit = V2 (bx + bw - 2) (by + bh - 2)
    inHalo =
      let
        s = 12
       in
        (fst2 hit < x1 && fst2 hit >= x1 - s)
          && snd2 hit >= y1 - s
          && snd2 hit <= y1 + h1 + s
    isResize k =
      k == UiCursorEwResize
        || k == UiCursorNsResize
        || k == UiCursorNwseResize
        || k == UiCursorNeswResize
  kind <- uiCursorKind ctx (inp0 {inputMousePos = hit})
  when (abs (x1 - destX) > 8) $ bump failed
  failWhen failed (not inHalo)
  when (isResize kind) $ bump failed
 where
  fst2 (V2 x _) = x
  snd2 (V2 _ y) = y

runSeparatorSpanTest :: Context -> IORef Int -> IO ()
runSeparatorSpanTest ctx failed = do
  let
    inp = withInput 200 120
    ui =
      column (fillW defaultLayout) $ do
        label_ "A"
        resp <- separator
        label_ "B"
        pure resp
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  let
    Rect _ _ w h = respRect resp
  when (w < 100) $ bump failed
  when (h > 2) $ bump failed


