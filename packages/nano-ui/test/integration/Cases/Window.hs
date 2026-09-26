module Cases.Window (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))

tests :: [Spec]
tests =
  [ spec "fit-header-no-shrink" runFitHeaderNoShrinkTest
  , spec "separator-span" runSeparatorSpanTest
  , pixelSpec "window-scroll-gutter" runWindowScrollGutterTest
  , spec "window-overlay" runWindowOverlayTest
  , spec "overlay-sibling-state" runOverlaySiblingStateTest
  , spec "overlay-click-through" runOverlayClickThroughTest
  , spec "overlay-panel-live" runOverlayPanelLiveTest
  , spec "window-drag" runWindowDragTest
  , spec "window-layout-reuse" runWindowLayoutReuseTest
  , spec "window-close-damage" runWindowCloseDamageTest
  , spec "page-window-scroll" runPageWindowScrollTest
  , spec "window-scroll-only-damage" runWindowScrollOnlyDamageTest
  , spec "window-content-churn" runWindowContentChurnTest
  , spec "scrolled-debug-toggle" runScrolledDebugToggleTest
  , spec "window-resize" runWindowResizeTest
  , spec "window-resize-halo-hit" runWindowResizeHaloHitTest
  , spec "heading-mono-truncate" runHeadingMonoTruncateTest
  , spec "window-fit-scroll-gutter" runWindowFitScrollGutterTest
  ]

runWindowScrollGutterTest :: Context -> IORef Int -> IO ()
runWindowScrollGutterTest ctx failed = do
  let inp0 = withInput 640 360
      long = T.pack (replicate 48 'M')
      ui = window True "GutterWin" $ do
        wide <- labelWith' fillW "WWWW"
        kv "Key" long
        mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 24]
        pure wide
  (win, mwide) <- warmup2 ctx inp0 ui
  let Rect wx _ ww _ = respRect win
      -- The body's content keeps the window's side padding before the bar.
      contentRight = wx + ww - padR windowPad - scrollBarGutter ScrollBarWindow 0
  spans <- collectOverlayTextSpans ctx inp0
  let titleYs = [rectY r | (r, txt, _, _, _) <- spans, "GutterWin" `T.isInfixOf` txt]
  assert failed (not (null titleYs))
  assertJust failed mwide $ \wide -> do
    let Rect cx _ cw _ = respRect wide
    assert failed (cx + cw >= contentRight - 0.5 && cx + cw <= contentRight + 0.01)

runWindowCloseDamageTest :: Context -> IORef Int -> IO ()
runWindowCloseDamageTest ctx failed = do
  let ui open = void (window open "Debug" (label "Body"))
      inp0 = withInput 640 400
  _ <- warmup2 ctx inp0 (ui True)
  _ <- runFrame ctx inp0 (ui False)
  assertEq failed DamageFull =<< takeDamage ctx
  assert failed =<< needsRedraw ctx inp0 (inp0 {inputDeltaTime = 1})

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
        assert failed =<< needsRedraw ctx inp inp
        _ <- runFrame ctx inp ui
        assertEq failed DamageFull =<< takeDamage ctx
  checkStatic (void (window True "Debug" (label "fps 0")))
  checkStatic (void (modal True "About" (label "body")))
  checkDirtyWake (void (modal True "About" (label "body")))

-- Opening a modal or window must not shift the ids, and so the stored state,
-- of the widgets after it.
runOverlaySiblingStateTest :: Context -> IORef Int -> IO ()
runOverlaySiblingStateTest _ failed = do
  let inp = withInput 640 400
      ui overlay open edit = do
        _ <- overlay open "About" (label "body")
        (txt, setTxt) <- useText "start"
        when edit (setTxt "edited")
        textInput txt
  forM_ [modal, window] $ \overlay -> do
    ctx <- newContext
    _ <- runFrame ctx inp (ui overlay False True)
    assertEq failed "edited" =<< warmup2 ctx inp (ui overlay False False)
    assertEq failed "edited" =<< warmup2 ctx inp (ui overlay True False)

runFitHeaderNoShrinkTest :: Context -> IORef Int -> IO ()
runFitHeaderNoShrinkTest ctx failed = do
  let header = panelWith (padXY 16 12 . fillW) (label' "nano-ui SDL3 demo")
      only = columnWith (padAll 12 . grow) header
      withBody = columnWith (padAll 12 . gap 8 . grow) $ do
        h <- header
        scrollWith (tight . grow) $
          columnWith fillW (mapM_ (label . T.pack . show) [1 .. 40 :: Int])
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
        outside <- button' "Outside"
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
    assert failed (not (hasText "Debug" closedSpans))
  (outside0, win0, mBody0) <- warmup2 ctx inp0 ui
  panels <- floatingPanelRects ctx
  overlays <- collectOverlayTextSpans ctx inp0
  assert failed (hasText "Debug" overlays && hasText "Body" overlays)
  assert failed (not (any (\(_, txt, _, _, _) -> T.strip txt == "X") overlays))
  let Rect wx wy ww wh = respRect win0
  assert failed (ww >= 100 && wh >= 20)
  assert failed (case mBody0 of Just _ -> True; _ -> False)
  (outsideHit, _, _) <- runClick ctx inp0 ui (V2 (rectX (respRect outside0) + 8) (rectY (respRect outside0) + 8))
  assert failed (respClicked outsideHit)
  (outsideMid, _, _) <- evalUi ctx (pressAt inp0 (V2 (wx + ww / 2) (wy + wh * 0.7))) ui
  assert failed (not (respClicked outsideMid))
  let esc = keyInp KeyEscape inp0
  ((_, winEsc, _), _, _, _) <- runFrame ctx esc ui
  assert failed (not (respClicked winEsc))
  let Rect px py pw _ =
        case map snd (IM.toList panels) of
          (r : _) -> r
          _ -> respRect win0
      closeAt = V2 (px + pw - padR windowPad - 12.5) (py + padT windowPad + 19.5)
  (_, winClose, _) <- runClick ctx inp0 ui closeAt
  assert failed (respClicked winClose)

runOverlayClickThroughTest :: Context -> IORef Int -> IO ()
runOverlayClickThroughTest ctx failed = do
  let
    inp0 = withInput 300 220
    windowUi = do
      outsides <- column (replicateM 10 (button' "Outside"))
      (win, mInside) <-
        window True "Cover" $ do
          button' "Inside"
      pure (outsides, win, mInside)
    modalUi = do
      outsides <- column (replicateM 10 (button' "Outside"))
      (dlg, mInside) <-
        modal True "Cover" $ do
          button' "Inside"
      pure (outsides, dlg, mInside)
    stackedUi = do
      (lo, mLo) <- window True "Low" (button' "LowBtn")
      (hi, mHi) <- window True "High" (button' "HighBtn")
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
    clickNone clicked u pos = runClick ctx inp0 u pos >>= \hit -> assert failed (not (clicked hit))
    runCovered u = do
      _ <- warmup2 ctx inp0 u
      ((_, cover0, mInside0), _, _, _) <- runFrame ctx inp0 u
      let coverRect = respRect cover0
      assert failed (rectW coverRect > 0 && rectH coverRect > 0)
      assertJust failed mInside0 $ \inside0 -> do
        let kids = [respRect inside0]
        assertJust failed (childSafePoint coverRect kids) $ \pos -> do
          (outsidesHit, _, _) <- runClick ctx inp0 u pos
          assert failed (not (any respClicked outsidesHit))
        let ir = respRect inside0
        assert failed (rectW ir > 0 && rectH ir > 0)
        (_, _, mInsideHit) <- runClick ctx inp0 u (spanCenter ir)
        assert failed (maybe False respClicked mInsideHit)
    runStacked = do
      _ <- warmup2 ctx inp0 stackedUi
      ((_, mLo0, hi0, mHi0), _, _, _) <- runFrame ctx inp0 stackedUi
      assertJust failed ((,) <$> mLo0 <*> mHi0) $ \(loBtn, hiBtn) -> do
          let cover = respRect hi0
              kids = [respRect loBtn, respRect hiBtn]
          assert failed (rectW cover > 0 && rectH cover > 0)
          assertJust failed (childSafePoint cover kids) $ \pos -> clickNone (\(_, loHit, _, _) -> maybe False respClicked loHit) stackedUi pos
          (_, _, _, mHiHit) <- runClick ctx inp0 stackedUi (centerOf hiBtn)
          assert failed (maybe False respClicked mHiHit)
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
  assertEq failed DamageFull =<< takeDamage ctx
  (win1, _, _, _) <- runFrame ctx (inp0 {inputMousePos = dest}) ui
  let Rect x1 y1 _ _ = respRect win1
  assert failed (x1 < x0 - 10)
  assert failed (y1 > y0 + 10)

-- | With a window open the solve is reused and only the floating panels are
-- placed again, still or mid-drag. Each such frame lays out every node where
-- the same frame solved from scratch does.
runWindowLayoutReuseTest :: Context -> IORef Int -> IO ()
runWindowLayoutReuseTest ctx failed = do
  let inp0 = withInput 640 400
      ui = do
        column $ forM_ [1 .. 20 :: Int] $ \i -> void (button (T.pack ("row " <> show i)))
        fmap fst (window True "Tools" (column (label "Body" >> void (button "ok"))))
      rects = arenaRects ctx
      -- The frame as it ran, then the same frame with nothing to reuse.
      sameAsFresh frameInp = do
        (win, _, _, _) <- runFrame ctx frameInp ui
        reused <- rects
        writeIORef (ctxLayoutCache ctx) Nothing
        _ <- runFrame ctx frameInp ui
        assertEq failed reused =<< rects
        pure (respRect win)
  win0 <- warmup2 ctx inp0 ui
  let r0 = respRect win0
      V2 gx gy = windowTitleGrab r0
  _ <- sameAsFresh inp0
  _ <- runFrame ctx (pressAt inp0 (V2 gx gy)) ui
  forM_ [1 .. 4 :: Int] $ \k -> do
    let step = inp0 {inputMousePos = V2 (gx - 20 * fromIntegral k) (gy + 10 * fromIntegral k), inputButtonsHeld = buttonsFromList [MouseLeft]}
    void (sameAsFresh step)
  Rect x1 y1 _ _ <- sameAsFresh (applyMouseButton MouseLeft False inp0 {inputMousePos = V2 (gx - 80) (gy + 40)})
  assert failed (x1 < rectX r0 - 40 && y1 > rectY r0 + 20)

-- Wheeling over a window's body scrolls the window, not the page, whether the
-- window is declared inside a page scroll area or beside one.
runPageWindowScrollTest :: Context -> IORef Int -> IO ()
runPageWindowScrollTest _ failed = do
  let line1 = T.pack "line 1"
      title = T.pack "Debug"
      debugWindow =
        fmap fst $
          window True "Debug" $
            column $
              mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 30]
      nested = do
        (_, win) <- scrollArea (tight . grow) $ do
          void (button "OK")
          debugWindow
        pure win
      sibling = do
        scrollWith (tight . grow) $ void (label "page")
        debugWindow
  forM_ [(withInput 320 220, nested), (withInput 640 400, sibling)] $ \(inp0, ui) -> do
    ctx <- newContext
    win <- warmup2 ctx inp0 ui
    let Rect wx _ ww _ = respRect win
    spans0 <- collectOverlayTextSpans ctx inp0
    case spanYOf line1 spans0 of
      [] -> assert failed False
      b0 : _ -> do
        let wheelAt = V2 (wx + ww / 2) (b0 + 2)
        assertWheelTitlePinned failed ctx inp0 ui title line1 wheelAt

runWindowScrollOnlyDamageTest :: Context -> IORef Int -> IO ()
runWindowScrollOnlyDamageTest ctx failed = do
  let inp0 = withInput 640 400
      ui =
        fmap fst $
          window True "Debug" $
            column $
              mapM_ (\i -> label (T.pack ("line " <> show (i :: Int)))) [1 .. 30]
  win <- warmup2 ctx inp0 ui
  let wheel =
        inp0
          { inputMousePos = centerOf win
          , inputScroll = V2 0 1
          }
  _ <- runFrame ctx wheel ui
  dmg <- takeDamage ctx
  case dmg of
    DamageClip r ->
      assert failed (maybe False (\i -> rectW i > 0 && rectH i > 0) (rectIntersect r (respRect win)))
    _ -> assert failed False

runWindowContentChurnTest :: Context -> IORef Int -> IO ()
runWindowContentChurnTest ctx failed = do
  let inp0 = withInput 640 400
      ui k = do
        _ <- button "Outside"
        fst <$> window True "Debug" (columnWith (tight . gap 4 . minW 300 . fillW) $ do
          void $ label (T.pack (replicate (1 + (k `mod` 9)) 'M'))
          void $ label "static row"
          )
  _ <- warmup2 ctx inp0 (ui 0)
  allClip <- forM [1 .. 30] $ \k -> do
    _ <- runFrame ctx inp0 (ui k)
    dmg <- takeDamage ctx
    case dmg of
      DamageClip _ -> pure True
      _ -> pure False
  assert failed (and allClip)

runScrolledDebugToggleTest :: Context -> IORef Int -> IO ()
runScrolledDebugToggleTest ctx failed = do
  let inp0 = withInput 640 400
      title = T.pack "Debug"
      ui = do
        (open, setOpen) <- useFlag False
        (_, dbgBtn) <- scrollArea (tight . grow) $ do
          b <- button' "Debug"
          when (respClicked b) (setOpen (not open))
          pure b
        when open $ void (window True "Debug" (label "fps"))
        pure dbgBtn
  dbgBtn <- warmup2 ctx inp0 ui
  _ <- runClick ctx inp0 ui (centerOf dbgBtn)
  assert failed . hasText title =<< collectOverlayTextSpans ctx inp0
  _ <- runFrame ctx inp0 ui
  assert failed . hasText title =<< collectOverlayTextSpans ctx inp0

runWindowResizeTest :: Context -> IORef Int -> IO ()
runWindowResizeTest ctx failed = do
  let inp0 = withInput 640 400
      ui = fmap fst (window True "Resize" (label "Body"))
  win0 <- warmup2 ctx inp0 ui
  assertJustM failed (getPrevRect ctx (respId win0)) $ \(Rect x0 y0 w0 h0) -> do
    assert failed (w0 > 0 && h0 > 0)
    let hoverAt p = inp0 {inputMousePos = p}
        expectCursor p kind = do
          assertEq failed kind =<< uiCursorKind ctx (hoverAt p)
    expectCursor (V2 (x0 + w0 + 4) (y0 + h0 + 4)) UiCursorNwseResize
    expectCursor (V2 (x0 - 4) (y0 - 4)) UiCursorNwseResize
    expectCursor (V2 (x0 + w0 + 4) (y0 - 4)) UiCursorNeswResize
    expectCursor (V2 (x0 - 4) (y0 + h0 + 4)) UiCursorNeswResize
    expectCursor (V2 (x0 + w0 / 2) (y0 - 4)) UiCursorNsResize
    expectCursor (V2 (x0 + w0 / 2) (y0 + h0 + 4)) UiCursorNsResize
    expectCursor (V2 (x0 - 4) (y0 + h0 / 2)) UiCursorEwResize
    expectCursor (V2 (x0 + w0 + 4) (y0 + h0 / 2)) UiCursorEwResize
    expectCursor (V2 (x0 + w0 - 5) (y0 + h0 / 2)) UiCursorEwResize
    -- Every margin resizes from inside the window too, the top one in a
    -- strip above the title bar, and corners take both sides.
    expectCursor (V2 (x0 + 5) (y0 + h0 / 2)) UiCursorEwResize
    expectCursor (V2 (x0 + w0 / 2) (y0 + h0 - 5)) UiCursorNsResize
    expectCursor (V2 (x0 + w0 / 2) (y0 + 3)) UiCursorNsResize
    expectCursor (V2 (x0 + 5) (y0 + 5)) UiCursorNwseResize
    expectCursor (V2 (x0 + 5) (y0 + h0 - 5)) UiCursorNeswResize
    expectCursor (V2 (x0 + w0 - 5) (y0 + h0 - 5)) UiCursorNwseResize
    expectCursor (V2 (x0 - 4) (y0 + 8)) UiCursorNwseResize
    titleKind <- uiCursorKind ctx (hoverAt (V2 (x0 + w0 / 2) (y0 + 20)))
    assert failed (titleKind /= UiCursorNsResize)
    insideKind <- uiCursorKind ctx (hoverAt (V2 (x0 + w0 - padR windowPad - 4) (y0 + h0 / 2)))
    assert failed (insideKind /= UiCursorEwResize)
    assertJustM failed (dragWindowEdge ctx inp0 ui (V2 (x0 + w0 + 4) (y0 + h0 + 4)) (V2 (x0 + w0 + 40) (y0 + h0 + 30))) $ \(Rect x1 y1 w1 h1) -> do
      assertGt failed w1 (w0 + 20)
      assertGt failed h1 (h0 + 15)
      assertJustM failed (dragWindowEdge ctx inp0 ui (V2 (x1 - 4) (y1 + h1 / 2)) (V2 (x1 - 36) (y1 + h1 / 2))) $ \(Rect xw yw ww hw) -> do
        assertGt failed ww (w1 + 15)
        assertLt failed xw (x1 - 10)
        assertJustM failed (dragWindowEdge ctx inp0 ui (V2 (xw + ww / 2) (yw - 4)) (V2 (xw + ww / 2) (yw - 20))) $ \(Rect xn yn wn hn) -> do
          assertGt failed hn (hw + 8)
          assertLt failed yn (yw - 5)
          let minTitleH = 39 + padT windowPad + padB windowPad
          assertJustM failed (dragWindowEdge ctx inp0 ui (V2 (xn + wn / 2) (yn + hn + 4)) (V2 (xn + wn / 2) (yn + 4))) $ \(Rect xs ys ws hMin) -> do
            assert failed (hMin + 0.01 >= minTitleH)
            -- A press in the left margin, inside the window, resizes.
            let midY = ys + hMin / 2
            assertJustM failed (dragWindowEdge ctx inp0 ui (V2 (xs + 5) midY) (V2 (xs - 25) midY)) $ \(Rect xi _ wi _) -> do
              assertGt failed wi (ws + 20)
              assertLt failed xi (xs - 20)

runWindowResizeHaloHitTest :: Context -> IORef Int -> IO ()
runWindowResizeHaloHitTest ctx failed = do
  let inp0 = withInput 640 400
      ui = do
        btn <- button' "Hit"
        (win, _) <- window True "Resize" (label "Body")
        pure (btn, win)
  (btn0, win0) <- warmup2 ctx inp0 ui
  let Rect bx by bw bh = respRect btn0
      Rect x0 y0 _ _ = respRect win0
      grab = V2 (x0 + 24) (y0 + 22)
      destX = bx + bw + 4
      press = pressAt inp0 grab
  _ <- runFrame ctx press ui
  let moved = press {inputMousePos = V2 (destX + 24) (y0 + 22), inputButtonsPressed = noButtons}
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
      ui = columnWith fillW $ do
        label "A"
        sid <- currentId
        separator
        label "B"
        pure sid
  sid <- warmup2 ctx inp ui
  assertJustM failed (getPrevRect ctx sid) $ \(Rect _ _ w h) -> do
    assert failed (w >= 100)
    assert failed (h <= 2)

runHeadingMonoTruncateTest :: Context -> IORef Int -> IO ()
runHeadingMonoTruncateTest ctx failed = do
  let inp = withInput 1600 600
      longPath = T.pack "C:\\Users\\zach\\AppData\\Local\\Microsoft\\Windows\\Fonts\\JetBrainsMono-Regular.ttf"
      ui = fst <$> window True "Debug" (do
        heading "Draw"
        _ <- label "NormalLabel"
        kvMono "font" longPath)
      fontSpans spans = [(r, t) | (r, t, _, _, _) <- spans, "JetBrainsMono" `T.isInfixOf` t || "..." `T.isInfixOf` t]
  win <- warmup2 ctx inp ui
  let Rect wx wy ww wh = respRect win
      contentRight = wx + ww - padR windowPad
  spans <- collectOverlayTextSpans ctx inp
  case fontSpans spans of
    [(Rect fx _ fw _, t)] -> do
      assert failed ("..." `T.isSuffixOf` t)
      assert failed (abs (fx + fw - contentRight) < 2.0)
    _ -> assert failed False
  assertJustM failed (dragWindowEdge ctx inp ui (V2 (wx - 4) (wy + wh / 2)) (V2 (wx - 1100) (wy + wh / 2))) $ \(Rect wxWide _ wwWide _) -> do
    assertGt failed wwWide (ww + 800)
    spansWide <- collectOverlayTextSpans ctx inp
    case fontSpans spansWide of
      [(Rect fx2 _ fw2 _, t2)] -> do
        assert failed (t2 == longPath)
        assert failed (not ("..." `T.isSuffixOf` t2))
        let contentRightWide = wxWide + wwWide - padR windowPad
        assert failed (abs (fx2 + fw2 - contentRightWide) < 2.0)
      _ -> assert failed False

-- | A window that fits a minimum-width body and scrolls leaves room for its
-- scrollbar, so right-aligned values end before the bar instead of under it.
runWindowFitScrollGutterTest :: Context -> IORef Int -> IO ()
runWindowFitScrollGutterTest ctx failed = do
  let inp = withInput 1200 300
      ui = fst <$> window True "Debug" (columnWith (tight . gap 4 . minW 300 . fillW) $
        mapM_ (\i -> kvMono (T.pack ("row " <> show (i :: Int))) (T.pack ("value" <> show i))) [1 .. 30])
  win <- warmup2 ctx inp ui
  let Rect wx _ ww _ = respRect win
      contentRight = wx + ww - padR windowPad - scrollBarGutter ScrollBarWindow 0
  spans <- collectOverlayTextSpans ctx inp
  let values = [r | (r, t, _, _, _) <- spans, "value" `T.isPrefixOf` t]
  assert failed (not (null values))
  forM_ values $ \(Rect vx _ vw _) -> assert failed (vx + vw <= contentRight + 0.5)
