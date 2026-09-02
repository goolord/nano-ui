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
  , runWindowTitleCenterTest
  , runWindowResizeHaloHitTest
  , runWindowResizeTest
  , runWindowScrollGutterTest
  , runWindowScrollWheelTest
  , runPageWindowScrollTest
  , runSiblingWindowScrollTest
  , runWindowScrollOnlyDamageTest
  , runScrolledDebugToggleTest
  ) where

import Control.Monad (replicateM, void, when)
import Data.IORef (IORef)
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import NanoUI
import NanoUI.Testing
import NanoUI.Testing.Assert (assert, assertEq, assertGt, assertLt, withInput)
import NanoUI.Testing.Harness
  ( assertWheelTitlePinned
  , runClickPair
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
  let inp0 = withInput 640 360
      long = T.pack (replicate 48 'M')
      ui = window True "GutterWin" $ do
        wide <- labelEx (fillW defaultLayout) "WWWW"
        kv "Key" long
        mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
        pure wide
  (win, mwide) <- warmup2 ctx inp0 ui
  let Rect wx _ ww _ = respRect win
      contentRight = wx + ww - padR windowPad
  spans <- collectOverlayTextSpans ctx inp0
  let titleYs = [rectY r | (r, txt, _, _, _) <- spans, "GutterWin" `T.isInfixOf` txt]
  assert failed (not (null titleYs))
  case mwide of
    Nothing -> assert failed False
    Just wide -> do
      let Rect cx _ cw _ = respRect wide
      assert failed (cx + cw >= contentRight - 0.5 && cx + cw <= contentRight + 0.01)

runWindowCloseDamageTest :: Context -> IORef Int -> IO ()
runWindowCloseDamageTest _ failed = do
  ctx <- newContext
  let ui open = void (window open "Debug" (label "Body"))
      inp0 = withInput 640 400
  _ <- warmup2 ctx inp0 (ui True)
  _ <- runFrame ctx inp0 (ui False)
  dmg <- takeDamage ctx
  assertEq failed dmg DamageFull
  need <- needsRedraw ctx inp0 (inp0 {inputDeltaTime = 1})
  assert failed need

runWindowDragDamageTest :: Context -> IORef Int -> IO ()
runWindowDragDamageTest _ failed = do
  ctx <- newContext
  let ui = fmap fst (window True "Debug" (label "Body"))
      inp0 = withInput 640 400
  win0 <- warmup2 ctx inp0 ui
  let Rect x0 y0 _ _ = respRect win0
      dest = V2 (x0 + 24 - 50) (y0 + 22 + 30)
  runDragFrom ctx inp0 ui (windowTitleGrab (respRect win0)) dest
  dmg <- takeDamage ctx
  assertEq failed dmg DamageFull

runOverlayPanelLiveTest :: Context -> IORef Int -> IO ()
runOverlayPanelLiveTest _ failed = do
  let inp = withInputOff 320 240
      checkStatic ui = do
        ctx <- newContext
        _ <- warmup2 ctx inp ui
        need <- needsRedraw ctx inp inp
        assert failed (not need)
        _ <- runFrame ctx inp ui
        dmg <- takeDamage ctx
        assert failed (damageIsEmpty dmg)
      checkDirtyWake ui = do
        ctx <- newContext
        _ <- runFrame ctx inp ui
        markDirty ctx
        need <- needsRedraw ctx inp inp
        assert failed need
        _ <- runFrame ctx inp ui
        dmg <- takeDamage ctx
        assertEq failed dmg DamageFull
  checkStatic (void (window True "Debug" (label "fps 0")))
  checkStatic (void (modal True "About" (label "body")))
  checkDirtyWake (void (modal True "About" (label "body")))

runHeaderTopPadTest :: Context -> IORef Int -> IO ()
runHeaderTopPadTest ctx failed = do
  let inp = withInput 800 600
      ui = column (padAll 12 . gap 8 . grow $ defaultLayout) $
             panel (padXY 16 12 . fillW $ defaultLayout) (label "nano-ui SDL3 demo")
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  assert failed (rectY (respRect resp) >= 24)

runFitHeaderNoShrinkTest :: Context -> IORef Int -> IO ()
runFitHeaderNoShrinkTest ctx failed = do
  let header = panel (padXY 16 12 . fillW $ defaultLayout) (label "nano-ui SDL3 demo")
      only = column (padAll 12 . grow $ defaultLayout) header
      withBody = column (padAll 12 . gap 8 . grow $ defaultLayout) $ do
        h <- header
        scroll (tight (grow defaultLayout)) $
          column (fillW defaultLayout) (mapM_ (label_ . T.pack . show) [1 .. 40 :: Int])
        pure h
      tall = withInput 400 800
      short = withInput 400 200
  _ <- runFrame ctx tall only
  (r0, _, _, _) <- runFrame ctx tall only
  _ <- runFrame ctx short withBody
  (r1, _, _, _) <- runFrame ctx short withBody
  assert failed (rectH (respRect r1) + 0.5 >= rectH (respRect r0))

runWindowOverlayTest :: Context -> IORef Int -> IO ()
runWindowOverlayTest ctx failed = do
  let inp0 = withInput 640 400
      ui = do
        outside <- button "Outside"
        (win, mBody) <- window True "Debug" (label "Body")
        pure (outside, win, mBody)
      closedUi = do
        _ <- button "Outside"
        (win, mBody) <- window False "Debug" (label "Body")
        pure (win, mBody)
  do
    ((win, mBody), _, _, _) <- runFrame ctx inp0 closedUi
    assert failed (not (respClicked win))
    assert failed (case mBody of Nothing -> True; _ -> False)
    closedSpans <- collectOverlayTextSpans ctx inp0
    assert failed (not (any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) closedSpans))
  (outside0, win0, mBody0) <- warmup2 ctx inp0 ui
  panels <- floatingPanelRects ctx
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (any (\(_, txt, _, _, _) -> "Debug" `T.isInfixOf` txt) overlays)
  assert failed (any (\(_, txt, _, _, _) -> "Body" `T.isInfixOf` txt) overlays)
  assert failed (not (any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays))
  let Rect wx wy ww wh = respRect win0
  assert failed (ww >= 100 && wh >= 20)
  assert failed (case mBody0 of Just _ -> True; _ -> False)
  let (pressOut, releaseOut) = clickPair inp0 (V2 (rectX (respRect outside0) + 8) (rectY (respRect outside0) + 8))
  _ <- runFrame ctx pressOut ui
  ((outsideHit, _, _), _, _, _) <- runFrame ctx releaseOut ui
  assert failed (respClicked outsideHit)
  let (clickWin, _) = clickPair inp0 (V2 (wx + ww / 2) (wy + wh * 0.7))
  ((outsideMid, _, _), _, _, _) <- runFrame ctx clickWin ui
  assert failed (not (respClicked outsideMid))
  let esc = inp0 {inputKeys = inputKeysFromList [KeyEscape]}
  ((_, winEsc, _), _, _, _) <- runFrame ctx esc ui
  assert failed (not (respClicked winEsc))
  let Rect px py pw _ =
        case map snd (IM.toList panels) of
          (r : _) -> r
          _ -> respRect win0
      closeAt = V2 (px + pw - padR windowPad - 12.5) (py + padT windowPad + 19.5)
      (clickClose, releaseClose) = clickPair inp0 closeAt
  _ <- runFrame ctx clickClose ui
  ((_, winClose, _), _, _, _) <- runFrame ctx releaseClose ui
  assert failed (respClicked winClose)

runWindowTitleCenterTest :: Context -> IORef Int -> IO ()
runWindowTitleCenterTest ctx failed = do
  let inp0 = withInput 640 400
      ui = fmap fst (window True "Debug" (label "Body"))
      titleBarChromeH = 39
  _ <- warmup2 ctx inp0 ui
  panels <- floatingPanelRects ctx
  (_, overlays) <- collectRasterSpans ctx inp0
  let wy =
        case map snd (IM.toList panels) of
          (Rect _ wy' _ _ : _) -> wy'
          _ -> 0
      barMid = wy + padT windowPad + titleBarChromeH / 2
  case [r | (r, txt, _, _, _) <- overlays, "Debug" `T.isInfixOf` txt] of
    (Rect _ ty _ th : _) -> do
      let fm = ctxFontMetrics ctx
          capMid =
            case fmGlyph fm 'H' of
              Nothing -> th / 2
              Just gq -> gqY gq + gqH gq / 2
          textInkMid = ty + capMid
      assert failed (abs (textInkMid - barMid) <= 4)
    _ -> assert failed False

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
    clickNone clicked u pos = do
      let (press, release) = clickPair inp0 pos
      _ <- runFrame ctx press u
      runFrame ctx release u >>= \(hit, _, _, _) -> assert failed (not (clicked hit))
    runCovered u = do
      _ <- warmup2 ctx inp0 u
      ((_, cover0, mInside0), _, _, _) <- runFrame ctx inp0 u
      let coverRect = respRect cover0
      assert failed (rectW coverRect > 0 && rectH coverRect > 0)
      case mInside0 of
        Nothing -> assert failed False
        Just inside0 -> do
          let kids = [respRect inside0]
          case childSafePoint coverRect kids of
            Nothing -> assert failed False
            Just pos -> do
              let (press, release) = clickPair inp0 pos
              _ <- runFrame ctx press u
              ((outsidesHit, _, _), _, _, _) <- runFrame ctx release u
              assert failed (not (any respClicked outsidesHit))
          let ir = respRect inside0
              ip = V2 (rectX ir + rectW ir / 2) (rectY ir + rectH ir / 2)
          assert failed (rectW ir > 0 && rectH ir > 0)
          let (ipress, irelease) = clickPair inp0 ip
          _ <- runFrame ctx ipress u
          ((_, _, mInsideHit), _, _, _) <- runFrame ctx irelease u
          assert failed (maybe False respClicked mInsideHit)
    runStacked = do
      _ <- warmup2 ctx inp0 stackedUi
      ((_, mLo0, hi0, mHi0), _, _, _) <- runFrame ctx inp0 stackedUi
      case (mLo0, mHi0) of
        (Just loBtn, Just hiBtn) -> do
          let cover = respRect hi0
              kids = [respRect loBtn, respRect hiBtn]
          assert failed (rectW cover > 0 && rectH cover > 0)
          case childSafePoint cover kids of
            Nothing -> assert failed False
            Just pos -> clickNone (\(_, loHit, _, _) -> maybe False respClicked loHit) stackedUi pos
          let hp = V2 (rectX (respRect hiBtn) + rectW (respRect hiBtn) / 2) (rectY (respRect hiBtn) + rectH (respRect hiBtn) / 2)
              (hpress, hrelease) = clickPair inp0 hp
          _ <- runFrame ctx hpress stackedUi
          ((_, _, _, mHiHit), _, _, _) <- runFrame ctx hrelease stackedUi
          assert failed (maybe False respClicked mHiHit)
        _ -> assert failed False
  runCovered windowUi
  runCovered modalUi
  runStacked

runWindowDragTest :: Context -> IORef Int -> IO ()
runWindowDragTest ctx failed = do
  let inp0 = withInput 640 400
      ui = fmap fst (window True "Debug" (label "Body"))
  win0 <- warmup2 ctx inp0 ui
  let r0 = respRect win0
      x0 = rectX r0
      y0 = rectY r0
      dest = V2 (x0 + 24 - 50) (y0 + 22 + 30)
  runDragFrom ctx inp0 ui (windowTitleGrab r0) dest
  (win1, _, _, _) <- runFrame ctx (inp0 {inputMousePos = dest}) ui
  let Rect x1 y1 _ _ = respRect win1
  assert failed (x1 < x0 - 10)
  assert failed (y1 > y0 + 10)

runPageWindowScrollTest :: Context -> IORef Int -> IO ()
runPageWindowScrollTest ctx failed = do
  let inp0 = withInput 320 220
      line1 = T.pack "line 1"
      title = T.pack "Debug"
      ui = do
        (_, win) <- scrollArea (tight (grow defaultLayout)) $ do
          void (button "OK")
          w <- fmap fst $
            window True "Debug" $
              column defaultLayout $
                mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 30]
          pure w
        pure win
  win <- warmup2 ctx inp0 ui
  let Rect wx _ ww _ = respRect win
  spans0 <- collectOverlayTextSpans ctx inp0
  case spanLabelYs line1 spans0 of
    [] -> assert failed False
    b0 : _ -> do
      let wheelAt = V2 (wx + ww / 2) (b0 + 2)
      assertWheelTitlePinned failed ctx inp0 ui title line1 wheelAt Nothing

runSiblingWindowScrollTest :: Context -> IORef Int -> IO ()
runSiblingWindowScrollTest ctx failed = do
  let inp0 = withInput 640 400
      line1 = T.pack "line 1"
      title = T.pack "Debug"
      ui = do
        scroll (tight (grow defaultLayout)) $ void (label "page")
        fmap fst $
          window True "Debug" $
            column defaultLayout $
              mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 30]
  win <- warmup2 ctx inp0 ui
  let Rect wx _ ww _ = respRect win
  spans0 <- collectOverlayTextSpans ctx inp0
  case spanLabelYs line1 spans0 of
    [] -> assert failed False
    b0 : _ -> do
      let wheelAt = V2 (wx + ww / 2) (b0 + 2)
      assertWheelTitlePinned failed ctx inp0 ui title line1 wheelAt Nothing

runWindowScrollOnlyDamageTest :: Context -> IORef Int -> IO ()
runWindowScrollOnlyDamageTest ctx failed = do
  let inp0 = withInput 640 400
      ui =
        fmap fst $
          window True "Debug" $
            column defaultLayout $
              mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 30]
  win <- warmup2 ctx inp0 ui
  let Rect wx wy ww wh = respRect win
      wheel =
        inp0
          { inputMousePos = V2 (wx + ww / 2) (wy + wh / 2)
          , inputScroll = V2 0 1
          }
  _ <- runFrame ctx wheel ui
  dmg <- takeDamage ctx
  let winPanel = Rect wx wy ww wh
  case dmg of
    DamageClip r ->
      assert failed (maybe False (\i -> rectW i > 0 && rectH i > 0) (rectIntersect r winPanel))
    _ -> assert failed False

runScrolledDebugToggleTest :: Context -> IORef Int -> IO ()
runScrolledDebugToggleTest ctx failed = do
  let inp0 = withInput 640 400
      title = T.pack "Debug"
      ui = do
        (readOpen, setOpen) <- useFlag False
        open <- readOpen
        (_, dbgBtn) <- scrollArea (tight (grow defaultLayout)) $ do
          b <- button "Debug"
          onClick b (setOpen (not open))
          pure b
        when open $ void (window True "Debug" (label "fps"))
        pure dbgBtn
  dbgBtn <- warmup2 ctx inp0 ui
  let Rect bx by bw bh = respRect dbgBtn
      pos = V2 (bx + bw / 2) (by + bh / 2)
  _ <- runClickPair ctx inp0 ui pos
  spans <- collectOverlayTextSpans ctx inp0
  let titles = [t | (_, t, _, _, _) <- spans, title `T.isInfixOf` t]
  assert failed (not (null titles))
  _ <- runFrame ctx inp0 ui
  spansAfter <- collectOverlayTextSpans ctx inp0
  let titlesAfter = [t | (_, t, _, _, _) <- spansAfter, title `T.isInfixOf` t]
  assert failed (not (null titlesAfter))

runWindowScrollWheelTest :: Context -> IORef Int -> IO ()
runWindowScrollWheelTest ctx failed = do
  let inp0 = withInput 320 220
      line1 = T.pack "line 1"
      title = T.pack "Scroll"
      ui = fmap fst $ window True "Scroll" $
             column defaultLayout (mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24])
  win <- warmup2 ctx inp0 ui
  let Rect wx _ ww wh = respRect win
  assert failed (ww > 0 && wh > 0)
  spans0 <- collectOverlayTextSpans ctx inp0
  case spanLabelYs line1 spans0 of
    [] -> assert failed False
    b0 : _ ->
      assertWheelTitlePinned failed ctx inp0 ui title line1 (V2 (wx + ww / 2) (b0 + 2)) Nothing

runWindowResizeTest :: Context -> IORef Int -> IO ()
runWindowResizeTest ctx failed = do
  let inp0 = withInput 640 400
      ui = fmap fst (window True "Resize" (label "Body"))
  _ <- runFrame ctx inp0 ui
  (win0, _, _, _) <- runFrame ctx inp0 ui
  mrect0 <- getPrevRect ctx (respId win0)
  case mrect0 of
    Nothing -> assert failed False
    Just (Rect x0 y0 w0 h0) -> do
      assert failed (w0 > 0 && h0 > 0)
      let hoverAt p = inp0 {inputMousePos = p}
          expectCursor p kind = do
            k <- uiCursorKind ctx (hoverAt p)
            assertEq failed k kind
      expectCursor (V2 (x0 + w0 + 4) (y0 + h0 + 4)) UiCursorNwseResize
      expectCursor (V2 (x0 - 4) (y0 - 4)) UiCursorNwseResize
      expectCursor (V2 (x0 + w0 + 4) (y0 - 4)) UiCursorNeswResize
      expectCursor (V2 (x0 - 4) (y0 + h0 + 4)) UiCursorNeswResize
      expectCursor (V2 (x0 + w0 / 2) (y0 - 4)) UiCursorNsResize
      expectCursor (V2 (x0 + w0 / 2) (y0 + h0 + 4)) UiCursorNsResize
      expectCursor (V2 (x0 - 4) (y0 + h0 / 2)) UiCursorEwResize
      expectCursor (V2 (x0 + w0 + 4) (y0 + h0 / 2)) UiCursorEwResize
      expectCursor (V2 (x0 + w0 - 5) (y0 + h0 / 2)) UiCursorEwResize
      insideKind <- uiCursorKind ctx (hoverAt (V2 (x0 + w0 - padR windowPad - 4) (y0 + h0 / 2)))
      assert failed (insideKind /= UiCursorEwResize)
      mSe <- dragWindowEdge ctx inp0 ui (V2 (x0 + w0 + 4) (y0 + h0 + 4)) (V2 (x0 + w0 + 40) (y0 + h0 + 30))
      case mSe of
        Nothing -> assert failed False
        Just (Rect x1 y1 w1 h1) -> do
          assertGt failed w1 (w0 + 20)
          assertGt failed h1 (h0 + 15)
          mW <- dragWindowEdge ctx inp0 ui (V2 (x1 - 4) (y1 + h1 / 2)) (V2 (x1 - 36) (y1 + h1 / 2))
          case mW of
            Nothing -> assert failed False
            Just (Rect xw yw ww hw) -> do
              assertGt failed ww (w1 + 15)
              assertLt failed xw (x1 - 10)
              mN <- dragWindowEdge ctx inp0 ui (V2 (xw + ww / 2) (yw - 4)) (V2 (xw + ww / 2) (yw - 20))
              case mN of
                Nothing -> assert failed False
                Just (Rect xn yn wn hn) -> do
                  assertGt failed hn (hw + 8)
                  assertLt failed yn (yw - 5)
                  let minTitleH = 39 + padT windowPad + padB windowPad
                  mShort <- dragWindowEdge ctx inp0 ui (V2 (xn + wn / 2) (yn + hn + 4)) (V2 (xn + wn / 2) (yn + 4))
                  case mShort of
                    Nothing -> assert failed False
                    Just (Rect _ _ _ hMin) -> assert failed (hMin + 0.01 >= minTitleH)

runWindowResizeHaloHitTest :: Context -> IORef Int -> IO ()
runWindowResizeHaloHitTest ctx failed = do
  let inp0 = withInput 640 400
      ui = do
        btn <- button "Hit"
        (win, _) <- window True "Resize" (label "Body")
        pure (btn, win)
  (btn0, win0) <- warmup2 ctx inp0 ui
  let Rect bx by bw bh = respRect btn0
      Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 24) (y0 + 22)
      destX = bx + bw + 4
      press = inp0 {inputMousePos = grab, inputMouseDown = True, inputMousePressed = True}
  _ <- runFrame ctx press ui
  let moved = press {inputMousePos = V2 (destX + 24) (y0 + 22), inputMousePressed = False}
  _ <- runFrame ctx moved ui
  ((_, win1), _, _, _) <- runFrame ctx (inp0 {inputMousePos = V2 destX (y0 + 22)}) ui
  let Rect x1 y1 _ h1 = respRect win1
      hit = V2 (bx + bw - 2) (by + bh - 2)
      inHalo = let s = 12
                in (v2X hit < x1 && v2X hit >= x1 - s)
                    && v2Y hit >= y1 - s
                    && v2Y hit <= y1 + h1 + s
      isResize k = k == UiCursorEwResize || k == UiCursorNsResize || k == UiCursorNwseResize || k == UiCursorNeswResize
  kind <- uiCursorKind ctx (inp0 {inputMousePos = hit})
  assert failed (abs (x1 - destX) <= 8)
  assert failed inHalo
  assert failed (not (isResize kind))

runSeparatorSpanTest :: Context -> IORef Int -> IO ()
runSeparatorSpanTest ctx failed = do
  let inp = withInput 200 120
      ui = column (fillW defaultLayout) $ do
        label_ "A"
        resp <- separator
        label_ "B"
        pure resp
  _ <- runFrame ctx inp ui
  (resp, _, _, _) <- runFrame ctx inp ui
  let Rect _ _ w h = respRect resp
  assert failed (w >= 100)
  assert failed (h <= 2)


