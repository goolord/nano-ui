module Cases.Tabs (tests) where

import Spec
import Data.Hashable (Hashable (..))
import Data.Maybe (isJust, listToMaybe)
import Data.Text qualified as T
import Data.Sequence qualified as Seq
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena (arenaCount, findNodeM, getNodeFontSize, getNodeRect, getParent, getText, getWidgetId)

tests :: [Spec]
tests =
  [ spec "panel-body-swap-damage" runPanelBodySwapDamageTest
  , spec "tabs-laziness" runTabsLazinessTest
  , spec "tabs-enum" runEnumTabsTest
  , spec "tabs-closable" runTabsClosableTest
  , spec "tabs-close-margin" runTabsCloseMarginTest
  , spec "tabs-disabled" runTabsDisabledTest
  , spec "tabs-scroll" runTabsScrollTest
  , spec "tabs-scroll-widen" runTabsScrollWidenTest
  , spec "tabs-scroll-close-button" runTabsScrollCloseButtonTest
  , spec "tabs-range-damage" runTabsRangeDamageTest
  , spec "tabs-state-persistence" runTabsStatePersistenceTest
  , spec "tabs-bodies-apart" runTabsBodiesApartTest
  , spec "tabs-keyed-reorder-state" runTabsKeyedReorderTest
  , spec "tabs-damage" runTabsDamageTest
  , spec "tab-response-forwarding" runTabResponseForwardingTest
  , spec "tabs-contained-body-damage" runTabsContainedBodyDamageTest
  , spec "tabs-drag-lifecycle" runTabDragLifecycleTest
  , spec "drag-escape-ownership" runDragEscapeOwnershipTest
  , spec "tabs-drag-controls" runTabDragControlsTest
  , spec "tabs-drag-reorder" runTabDragReorderTest
  , spec "tabs-vertical-drop-bounds" runVerticalDropBoundsTest
  , spec "tabs-clipped-drop-bounds" runClippedDropBoundsTest
  , spec "tabs-scroll-follow-var" runTabsFollowVarTest
  , spec "tabs-missing-key" runTabsMissingKeyTest
  , spec "tabs-wheel-in-page" runTabsWheelInPageTest
  , spec "tabs-wheel-fraction" runTabsWheelFractionTest
  , spec "tabs-arrow-damage" runTabsArrowDamageTest
  , spec "tabs-badge-size" runTabsBadgeSizeTest
  ]

-- | Headers that overflow a 240px strip.
overflowLabels :: [T.Text]
overflowLabels = ["Controls", "Graphics", "Typography", "Diagnostics", "LongestTabName"]

-- A tab selected through the Var, not by a click, is brought into view,
-- and so is one appended and selected in the same frame.
runTabsFollowVarTest :: Context -> IORef Int -> IO ()
runTabsFollowVarTest ctx failed = do
  active <- newVar (0 :: Int)
  let inp = withInput 240 120
      ui labels = tabBar' active [tab i l () | (i, l) <- zip [0 ..] labels]
      shows' needle = hasText needle <$> collectTextSpans ctx
  replicateM_ 3 (runFrame ctx inp (ui overflowLabels))
  assert failed . not =<< shows' "LongestTabName"
  writeVarIO active 4
  replicateM_ 2 (runFrame ctx inp (ui overflowLabels))
  assert failed =<< shows' "LongestTabName"
  writeVarIO active 0
  replicateM_ 2 (runFrame ctx inp (ui overflowLabels))
  assert failed =<< shows' "Controls"
  writeVarIO active 5
  replicateM_ 3 (runFrame ctx inp (ui (overflowLabels ++ ["Appended"])))
  assert failed =<< shows' "Appended"

-- An active key no tab has shows the first tab, its header and body alike,
-- and reports it.
runTabsMissingKeyTest :: Context -> IORef Int -> IO ()
runTabsMissingKeyTest ctx failed = do
  active <- newVar (3 :: Int)
  let ui = tabs' active [tab 1 "One" (label "Body one"), tab 2 "Two" (label "Body two")]
  r <- warmup2 ctx (withInputOff 300 100) ui
  assertEq failed 1 (tabActive r)
  assertEq failed 1 =<< readVarIO active
  assertEq failed [1, 0] =<< buttonValues ctx
  assert failed . hasText "Body one" =<< collectTextSpans ctx

-- The vertical wheel over an overflowing strip pages the strip alone, not
-- the page scroller round it too.
runTabsWheelInPageTest :: Context -> IORef Int -> IO ()
runTabsWheelInPageTest ctx failed = do
  let inp = withInput 240 120
      ui = scrollArea (tight . grow) $ do
        _ <- tabBar' (constVar (0 :: Int)) [tab i l () | (i, l) <- zip [0 ..] overflowLabels]
        replicateM_ 20 (label "line")
  (pageWid, _) <- warmup2 ctx inp ui
  _ <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  assertJust failed (spanRect "Controls" spans) $ \r -> do
    _ <- runFrame ctx inp {inputMousePos = spanCenter r, inputScroll = V2 0 20} ui
    _ <- runFrame ctx inp ui
    assertEq failed 0 =<< getScrollOffset ctx pageWid
    assert failed . not . hasText "Controls" =<< collectTextSpans ctx

-- A trackpad's fraction of a notch over an overflowing strip pages it by that
-- fraction: rounded to whole notches, a slow swipe would move nothing.
runTabsWheelFractionTest :: Context -> IORef Int -> IO ()
runTabsWheelFractionTest ctx failed = do
  let inp = withInput 240 120
      ui = tabBar' (constVar (0 :: Int)) [tab i l () | (i, l) <- zip [0 ..] overflowLabels]
      controls = spanRect "Controls" <$> collectTextSpans ctx
  replicateM_ 3 (runFrame ctx inp ui)
  assertJustM failed controls $ \r -> do
    _ <- runFrame ctx inp {inputMousePos = spanCenter r, inputScroll = V2 0 0.3} ui
    _ <- runFrame ctx inp ui
    assertJustM failed controls $ \r' -> assertLt failed (rectX r') (rectX r)

-- Paging the strip repaints the arrow that turns usable, whose muted look is
-- a colour no rect or text change covers.
runTabsArrowDamageTest :: Context -> IORef Int -> IO ()
runTabsArrowDamageTest ctx failed = do
  let inp = withInput 240 120
      ui = tabBar' (constVar (0 :: Int)) [tab i l () | (i, l) <- zip [0 ..] overflowLabels]
      arrow ch = (\spans -> listToMaybe [r | (r, t, _, _, _) <- spans, T.any (== ch) t]) <$> collectTextSpans ctx
  replicateM_ 3 (runFrame ctx inp ui)
  assertJustM failed (arrow '\8249') $ \left ->
    assertJustM failed (arrow '\8250') $ \right -> do
      let (press, release) = clickPair inp (spanCenter right)
      _ <- runFrame ctx press ui
      _ <- runFrame ctx release ui
      _ <- runFrame ctx release {inputButtonsReleased = noButtons} ui
      dmg <- takeDamage ctx
      assert failed (damageCovers dmg left)

-- A badge is smaller than its header's text, whatever the base font size.
runTabsBadgeSizeTest :: Context -> IORef Int -> IO ()
runTabsBadgeSizeTest ctx0 failed = do
  let ctx = withFontSize ctx0 10
      na = ctxNodeArena ctx
  _ <- warmup2 ctx (withInputOff 300 100) (tabBar' (constVar (1 :: Int)) [(tab 1 "Inbox" ()) {tabBadge = Just "3"}])
  badge <- findNodeM na (fmap (== "3") . getText na)
  assertJust failed badge $ \i -> assertEq failed (10 * 0.78) =<< getNodeFontSize na i

data DummyTab = TabA | TabB | TabC
  deriving (Eq, Show, Enum, Bounded)

instance Hashable DummyTab where
  hashWithSalt salt = hashWithSalt salt . fromEnum

-- Header focus and body-local state follow the key, not the list position.
runTabsKeyedReorderTest :: Context -> IORef Int -> IO ()
runTabsKeyedReorderTest ctx failed = do
  seen <- newIORef Nothing
  let inp = withInputOff 500 250
      body key = do
        nVar <- useVar (0 :: Int)
        n <- readVar nVar
        (field, _) <- withValue (T.pack (show key)) textInput'
        liftIO (writeIORef seen (Just (n, nVar, respId field)))
      ui active order = tabs' (constVar active) [tab key (T.pack (show key)) (body key) | key <- order]
      readBody = atomicModifyIORef' seen (\s -> (Nothing, s)) >>= maybe (fail "tab body did not run") pure
  original <- warmup2 ctx inp (ui TabB [TabA, TabB, TabC])
  (_, nVar, fieldId) <- readBody
  runNanoUI ctx inp (writeVar nVar 7)
  writeIORef (ctxFocusId ctx) fieldId
  forM_ [[TabC, TabA, TabB], [TabB, TabC], [TabC, TabB]] $ \order -> do
    moved <- warmup2 ctx inp (ui TabB order)
    (n, _, fieldId') <- readBody
    assertEq failed (7, fieldId) (n, fieldId')
    assertEq failed fieldId =<< getFocusId ctx
    forM_ order $ \key ->
      assertEq failed (respId <$> lookup key (tabHeaders original)) (respId <$> lookup key (tabHeaders moved))
  _ <- warmup2 ctx inp (ui TabC [TabC, TabB])
  (other, _, _) <- readBody
  assertEq failed 0 other
  _ <- warmup2 ctx inp (ui TabB [TabC, TabB])
  (kept, _, _) <- readBody
  assertEq failed 7 kept

runVerticalDropBoundsTest :: Context -> IORef Int -> IO ()
runVerticalDropBoundsTest ctx failed = do
  let ui = panelWith (padAll 40 . fillW . fillH) $
        tabBarConfigured' (TabsConfig TabUnderline TabLeft (pure ())) (constVar TabA)
          [tab TabA "Alpha" (), tab TabB "Beta" ()]
  r <- warmup2 ctx (withInput 400 200) ui
  let bounds = tabStripRect r
  assert failed (rectX bounds >= 40 && rectY bounds >= 40)
  assertEq failed Nothing
    (insertionIndex DragAxisY bounds (map (respRect . snd) (tabHeaders r)) (V2 10 10))

runClippedDropBoundsTest :: Context -> IORef Int -> IO ()
runClippedDropBoundsTest ctx failed = do
  result <- newIORef (Rect 0 0 0 0)
  let ui orientation = paneGrid defaultPaneGridConfig
        { pgLayout = fixedWH 200 60
        , pgViewPane = \_ _ -> do
            r <- panelWith (fixedW 600) $
              tabBarConfigured' (TabsConfig TabUnderline orientation (pure ())) (constVar TabA)
                [tab TabA "Alpha" (), tab TabB "Beta" (), tab TabC "Gamma" ()]
            liftIO (writeIORef result (tabStripRect r))
            pure (PaneView "Tabs" False)
        }
  forM_ [TabTop, TabLeft] $ \orientation -> do
    writeIORef result (Rect 0 0 0 0)
    void (warmup2 ctx (withInput 400 200) (ui orientation))
    bounds <- readIORef result
    assert failed (rectW bounds > 0 && rectW bounds <= 200)
    assert failed (rectH bounds > 0 && rectH bounds <= 60)

runTabDragLifecycleTest :: Context -> IORef Int -> IO ()
runTabDragLifecycleTest ctx failed = do
  dragHandle <- newDrag
  let inp = withInput 400 200
      ui order = do
        r <- tabBar' (constVar TabA) [tab k (T.pack (show k)) () | k <- order]
        d <- useDrag dragHandle (tabHeaders r)
        pure (r, d)
      keys = [TabA, TabB, TabC]
  (r, _) <- warmup2 ctx inp (ui keys)
  first <- maybe (fail "missing tab header") (pure . snd) (listToMaybe (tabHeaders r))
  let start = spanCenter (respRect first)
      V2 x y = start
      press = pressAt inp start
      moved = holdAt press (V2 (x + 20) y)
  (_, armed) <- evalUi ctx press (ui keys)
  assertEq failed Nothing armed
  (_, small) <- evalUi ctx (holdAt press (V2 (x + 3) y)) (ui keys)
  assertEq failed Nothing small
  (_, begun) <- evalUi ctx moved (ui keys)
  assertEq failed (Just (TabA, DragStarted)) ((\d -> (dragPayload d, dragPhase d)) <$> begun)
  (_, reordered) <- evalUi ctx moved (ui [TabC, TabB, TabA])
  assertEq failed (Just (TabA, Dragging)) ((\d -> (dragPayload d, dragPhase d)) <$> reordered)
  (_, released) <- evalUi ctx (releaseAt moved) (ui keys)
  assertEq failed (Just DragReleased) (dragPhase <$> released)
  (_, after) <- evalUi ctx inp (ui keys)
  assertEq failed Nothing after
  forM_ [0 .. 2 :: Int] $ \reason -> do
    _ <- evalUi ctx press (ui keys)
    _ <- evalUi ctx moved (ui keys)
    let cancelInput = case reason of
          0 -> keyInp KeyEscape moved
          1 -> inp
          _ -> moved
        remaining = if reason == 2 then [TabB, TabC] else keys
    (_, cancelled) <- evalUi ctx cancelInput (ui remaining)
    assertEq failed (Just DragCancelled) (dragPhase <$> cancelled)
    (_, ended) <- evalUi ctx (releaseAt moved) (ui keys)
    assertEq failed Nothing ended
    void (warmup2 ctx inp (ui keys))

runDragEscapeOwnershipTest :: Context -> IORef Int -> IO ()
runDragEscapeOwnershipTest ctx failed = do
  handle <- newDrag
  let inp = withInputOff 400 200
      ui takeFirst = modalPanel' (constVar True) $ do
        when takeFirst (void takeEscape)
        source <- button' "Drag"
        gesture <- useDrag handle [((), source)]
        later <- takeEscape
        pure (source, gesture, later)
  (_, body) <- warmup2 ctx inp (ui False)
  assertJust failed body $ \(source, _, _) -> do
    let p@(V2 x y) = centerOf source
        down = pressAt inp p
        moved = holdAt down (V2 (x + 15) y)
        escape = keyInp KeyEscape moved
    _ <- evalUi ctx down (ui False)
    _ <- evalUi ctx moved (ui False)
    (_, claimed) <- evalUi ctx escape (ui True)
    assertJust failed claimed $ \(_, gesture, later) -> do
      assertEq failed (Just Dragging) (dragPhase <$> gesture)
      assertEq failed False later
    (dialog, cancelled) <- evalUi ctx escape (ui False)
    assertJust failed cancelled $ \(_, gesture, later) -> do
      assertEq failed (Just DragCancelled) (dragPhase <$> gesture)
      assertEq failed False later
    assertEq failed False (respClicked dialog)

runTabDragControlsTest :: Context -> IORef Int -> IO ()
runTabDragControlsTest ctx failed = do
  dragHandle <- newDrag
  let inp = withInput 400 200
      ui = do
        r <- tabBar' (constVar TabA)
          [closableTab TabA "Alpha" (), (tab TabB "Disabled" ()) {tabDisabled = True}]
        useDrag dragHandle (tabHeaders r)
  _ <- warmup2 ctx inp ui
  spans <- collectTextSpans ctx
  close <- findCloseButtonRect ctx
  forM_ [close, spanRect "Disabled" spans] $ \target ->
    assertJust failed target $ \r -> do
      let pos@(V2 x y) = spanCenter r
          press = pressAt inp pos
          moved = holdAt press (V2 (x + 20) y)
      _ <- evalUi ctx press ui
      result <- evalUi ctx moved ui
      assertEq failed Nothing result
      void (evalUi ctx (releaseAt moved) ui)

runTabDragReorderTest :: Context -> IORef Int -> IO ()
runTabDragReorderTest ctx failed = do
  dragHandle <- newDrag
  orderRef <- newIORef [TabA, TabB, TabC]
  let inp = withInput 400 200
      ui = do
        order <- liftIO (readIORef orderRef)
        r <- tabBar' (constVar TabA) [tab k (T.pack (show k)) () | k <- order]
        gesture <- useDrag dragHandle (tabHeaders r)
        forM_ gesture $ \d -> when (dragPhase d == DragReleased) $ do
          let rest = filter ((/= dragPayload d) . fst) (tabHeaders r)
          forM_ (insertionIndex DragAxisX (tabStripRect r) (map (respRect . snd) rest) (dragAt d)) $ \i -> do
            let (before, after) = splitAt i (map fst rest)
            liftIO (writeIORef orderRef (before ++ dragPayload d : after))
        pure r
  r <- warmup2 ctx inp ui
  first <- maybe (fail "missing first tab") pure (lookup TabA (tabHeaders r))
  third <- maybe (fail "missing third tab") pure (lookup TabC (tabHeaders r))
  let press = pressAt inp (spanCenter (respRect first))
      Rect x y w h = respRect third
      moved = holdAt press (V2 (x + w - 1) (y + h / 2))
  _ <- evalUi ctx press ui
  _ <- evalUi ctx moved ui
  assertEq failed [TabA, TabB, TabC] =<< readIORef orderRef
  _ <- evalUi ctx (releaseAt moved) ui
  assertEq failed [TabB, TabC, TabA] =<< readIORef orderRef
  settled <- evalUi ctx inp ui
  assertEq failed [TabB, TabC, TabA] (map fst (tabHeaders settled))

-- Content replacement inside a floating window must repaint every pixel of
-- the new body: a clip that skipped any incoming row would leave the pane
-- rendering stale pixels from the previous body ("ghosting"). The stable
-- content slot and the incoming key rects (diffNew) must together cover all
-- five rows.
runPanelBodySwapDamageTest :: Context -> IORef Int -> IO ()
runPanelBodySwapDamageTest ctx failed = do
  let inp0 = withInput 400 300
      uiA = window' (constVar True) "TabWin" (columnWith (fixedH 300) (label "WIDE BODY ROW ONE"))
      uiB = window' (constVar True) "TabWin" (columnWith (fixedH 300) (column (replicateM 5 (label "row line") >> pure ())))
  _ <- warmup2 ctx inp0 uiA
  _ <- runFrame ctx inp0 uiA
  _ <- runFrame ctx inp0 uiB
  dmg <- takeDamage ctx
  assert failed (not (damageIsEmpty dmg))
  spans <- collectOverlayTextSpans ctx inp0
  let rows = [r | (r, t, _, _, _) <- spans, "row line" `T.isInfixOf` t]
  assert failed (length rows == 5)
  forM_ rows $ \r -> assert failed (damageCovers dmg r)

runTabsLazinessTest :: Context -> IORef Int -> IO ()
runTabsLazinessTest ctx failed = do
  evalCountA <- newIORef (0 :: Int)
  evalCountB <- newIORef (0 :: Int)
  evalCountC <- newIORef (0 :: Int)
  let inp = withInput 200 100
      ui = tabs' (constVar TabB) $ Seq.fromList
        [ tab TabA "A" (liftIO (modifyIORef' evalCountA (+ 1)) >> label "Body A")
        , tab TabB "B" (liftIO (modifyIORef' evalCountB (+ 1)) >> label "Body B")
        , tab TabC "C" (liftIO (modifyIORef' evalCountC (+ 1)) >> label "Body C")
        ]
  _ <- runFrame ctx inp ui
  assertEq failed [0, 1, 0] =<< mapM readIORef [evalCountA, evalCountB, evalCountC]

-- Composite responses expose every flag of their widget response.
runTabResponseForwardingTest :: Context -> IORef Int -> IO ()
runTabResponseForwardingTest _ failed = do
  let inner = mempty {rawRespSubmitted = True, rawRespHeld = buttonsFromList [MouseRight], rawRespChanged = True}
      tabResp = TabResponse inner Nothing TabA [] (Rect 0 0 0 0)
      tableResp = TableResponse inner (SortCol 0 SortAsc) [] mempty
  assert failed (respSubmitted tabResp && respRightPressed tabResp && respChanged tabResp)
  assert failed (respSubmitted tableResp && respRightPressed tableResp && respChanged tableResp)

runTabsClosableTest :: Context -> IORef Int -> IO ()
runTabsClosableTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs' (constVar curTab)
        [ closableTab TabA "Alpha" (label "Body A")
        , closableTab TabB "Beta" (label "Body B")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  assertJustM failed (findCloseButtonRect ctx) $ \r -> do
    tResp <- runClick ctx inp0 (ui TabA) (spanCenter r)
    assertEq failed (tabClosed tResp) (Just TabA)
    assertEq failed (tabActive tResp) TabA

-- | A click on a header just left of its close button selects the tab: the
-- close button does not take the press there and then lose the release.
runTabsCloseMarginTest :: Context -> IORef Int -> IO ()
runTabsCloseMarginTest ctx failed = do
  let inp0 = withInput 300 100
      ui = tabs' (constVar TabA) [tab TabA "Alpha" (label "Body A"), closableTab TabB "Beta" (label "Body B")]
  _ <- warmup2 ctx inp0 ui
  assertJustM failed (findCloseButtonRect ctx) $ \(Rect x y _ h) -> do
    tResp <- runClick ctx inp0 ui (V2 (x - 3) (y + h / 2))
    assertEq failed (tabClosed tResp) Nothing
    assertEq failed (tabActive tResp) TabB

-- A strip that overflowed drops its arrows once the window grows wide enough
-- for every header in the bar, though the scroller between the arrows would
-- still be too narrow for them.
runTabsScrollWidenTest :: Context -> IORef Int -> IO ()
runTabsScrollWidenTest wide failed = do
  let labels = ["Controls", "Graphics", "Typography", "Diagnostics", "LongestTabName"]
      mkTabs = tabBar' (constVar (0 :: Int)) [tab i l () | (i, l) <- zip [0 ..] labels]
      nodeRect ctx l = do
        let na = ctxNodeArena ctx
        found <- findNodeM na (fmap (== l) . getText na)
        traverse (getNodeRect na) found
      hasArrows ctx = do
        spans <- collectTextSpans ctx
        pure (T.any (`elem` ['\8250', '\8249']) (T.concat [t | (_, t, _, _, _) <- spans]))
  _ <- warmup2 wide (withInput 900 120) mkTabs
  mFirst <- nodeRect wide "Controls"
  mLast <- nodeRect wide "LongestTabName"
  case (mFirst, mLast) of
    (Just (Rect x0 _ _ _), Just (Rect lx _ lw _)) -> do
      -- The headers' extent plus the window's padding on both sides, and
      -- less spare room than the two arrows take.
      let fitW = lx + lw + x0 + 10
      ctx <- newContext
      _ <- warmup2 ctx (withInput 240 120) mkTabs
      _ <- runFrame ctx (withInput 240 120) mkTabs
      assert failed =<< hasArrows ctx
      replicateM_ 3 (runFrame ctx (withInput fitW 120) mkTabs)
      spans <- collectTextSpans ctx
      forM_ labels $ \l -> assert failed (hasText l spans)
      assert failed . not =<< hasArrows ctx
    _ -> assert failed False

-- A strip whose last close button runs past the bar overflows, though every
-- header's main button fits: the range counts each header's close button.
runTabsScrollCloseButtonTest :: Context -> IORef Int -> IO ()
runTabsScrollCloseButtonTest wide failed = do
  let labels = ["Controls", "Graphics", "Typography"]
      mkTabs = tabBar' (constVar (0 :: Int)) [closableTab i l () | (i, l) <- zip [0 ..] labels]
      nodeRects ctx p = do
        let na = ctxNodeArena ctx
        n <- arenaCount na
        fmap concat . forM [0 .. n - 1] $ \i -> do
          t <- getText na i
          if p t then pure <$> getNodeRect na i else pure []
      hasArrows ctx = do
        spans <- collectTextSpans ctx
        pure (T.any (`elem` ['\8250', '\8249']) (T.concat [t | (_, t, _, _, _) <- spans]))
  _ <- warmup2 wide (withInput 900 120) mkTabs
  firsts <- nodeRects wide (== "Controls")
  closes <- nodeRects wide (== "\215")
  case firsts of
    Rect x0 _ _ _ : _ | length closes == length labels -> do
      -- The last close button's right edge, less a little, plus the
      -- window's right padding: only that button runs past the bar.
      let clipW = maximum [rectX r + rectW r | r <- closes] - 4 + x0
      ctx <- newContext
      replicateM_ 3 (runFrame ctx (withInput clipW 120) mkTabs)
      assert failed =<< hasArrows ctx
    _ -> assert failed False

-- The frame a strip first measures an overflow publishes its range, a float.
-- That is layout, not a scroll: the resize that made the strip overflow
-- still repaints the whole window.
runTabsRangeDamageTest :: Context -> IORef Int -> IO ()
runTabsRangeDamageTest ctx failed = do
  let labels = ["Controls", "Graphics", "Typography", "Diagnostics", "LongestTabName"]
      ui = column $ do
        _ <- tabBar' (constVar (0 :: Int)) [tab i l () | (i, l) <- zip [0 ..] labels]
        label "Below"
  _ <- warmup2 ctx (withInput 900 120) ui
  _ <- runFrame ctx (withInput 900 120) ui
  _ <- runFrame ctx (withInput 240 120) ui
  dmg <- takeDamage ctx
  assert failed (dmg == DamageFull)

findCloseButtonRect :: Context -> IO (Maybe Rect)
findCloseButtonRect ctx = do
  let na = ctxNodeArena ctx
  found <- findNodeM na (fmap ("\215" `T.isInfixOf`) . getText na)
  traverse (getNodeRect na) found

-- The public disabled flag covers both the header and its close control,
-- including retained keyboard focus when an enabled tab becomes disabled.
runTabsDisabledTest :: Context -> IORef Int -> IO ()
runTabsDisabledTest _ failed = forM_ [TabTop, TabLeft] $ \orientation -> do
  ctx <- newContext
  let inp = withInputOff 400 240
      ui disabled = tabsConfigured' defaultTabsConfig {tabsOrientation = orientation} (constVar TabA)
        [ (closableTab TabB "Disabled" (label "Body B")) {tabDisabled = disabled}
        , tab TabA "Enabled" (label "Body A")
        ]
      check response = do
        assertEq failed (tabActive response) TabA
        assertEq failed (tabClosed response) Nothing
        assert failed (not (respClicked response) && not (respChanged response))
  _ <- warmup2 ctx inp (ui False)
  let arena = ctxNodeArena ctx
  n <- arenaCount arena
  headers <- mapM (\i -> (,) <$> getText arena i <*> getWidgetId arena i) [0 .. n - 1]
  _ <- warmup2 ctx inp (ui True)
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "Disabled" spans) $ \r -> check =<< runClick ctx inp (ui True) (spanCenter r)
  assertJustM failed (findCloseButtonRect ctx) $ \r -> check =<< runClick ctx inp (ui True) (spanCenter r)
  forM_ [wid | (txt, wid) <- headers, txt == "Disabled" || txt == "\215"] $ \wid -> do
    writeIORef (ctxFocusId ctx) wid
    (result, _, _) <- runFrame ctx (keyInp KeyEnter inp) (ui True)
    check result
  -- Re-enabling the same header preserves its identity and restores activation.
  _ <- warmup2 ctx inp (ui False)
  spansEnabled <- collectTextSpans ctx
  assertJust failed (spanRectOf "Disabled" spansEnabled) $ \r -> do
    response <- runClick ctx inp (ui False) (spanCenter r)
    assertEq failed (tabActive response) TabB

-- | Bodies of different tabs keep separate state, even at the same position.
runTabsBodiesApartTest :: Context -> IORef Int -> IO ()
runTabsBodiesApartTest ctx failed = do
  let inp0 = withInput 300 100
      body name = do
        flagVar <- useVar False
        flag <- readVar flagVar
        whenM (button ("Toggle" <> name)) (writeVar flagVar (not flag))
        label (name <> if flag then "On" else "Off")
      ui curTab = tabs' (constVar curTab) [tab TabA "A" (body "A"), tab TabB "B" (body "B")]
  _ <- warmup2 ctx inp0 (ui TabA)
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "ToggleA" spans) $ \r -> do
    _ <- runClick ctx inp0 (ui TabA) (spanCenter r)
    _ <- warmup2 ctx inp0 (ui TabA)
    spansA <- collectTextSpans ctx
    assert failed (hasText "AOn" spansA)
    _ <- warmup2 ctx inp0 (ui TabB)
    spansB <- collectTextSpans ctx
    assert failed (hasText "BOff" spansB)

-- | enumTabs has a tab per value, labelled with show, and a click stores
-- the value itself in the Var; boundedTabBar does the same with labels of
-- its own and no bodies.
runEnumTabsTest :: Context -> IORef Int -> IO ()
runEnumTabsTest ctx failed = do
  let inp0 = withInput 400 120
  page <- newVar TabA
  let ui = enumTabs page (label . ("Body of " <>) . T.pack . show)
  _ <- warmup2 ctx inp0 ui
  spans <- collectTextSpans ctx
  assert failed (hasText "Body of TabA" spans)
  assertJust failed (spanRectOf "TabC" spans) $ \r -> do
    _ <- runClick ctx inp0 ui (spanCenter r)
    assertEq failed TabC =<< readVarIO page
    _ <- warmup2 ctx inp0 ui
    assert failed . hasText "Body of TabC" =<< collectTextSpans ctx
  pick <- newVar TabA
  let bar = boundedTabBar (T.toLower . T.pack . show) pick
  _ <- warmup2 ctx inp0 bar
  barSpans <- collectTextSpans ctx
  assertJust failed (spanRectOf "tabb" barSpans) $ \r -> do
    _ <- runClick ctx inp0 bar (spanCenter r)
    assertEq failed TabB =<< readVarIO pick

runTabsStatePersistenceTest :: Context -> IORef Int -> IO ()
runTabsStatePersistenceTest ctx failed = do
  let inp0 = withInput 300 100
      ui curTab = tabs' (constVar curTab)
        [ tab TabA "A" $
            withKey ("tab-a" :: T.Text) $
              withKey ("flag" :: T.Text) $ do
                flagVar <- useVar False
                flag <- readVar flagVar
                whenM (button "ToggleA") (writeVar flagVar (not flag))
                label (if flag then "FlagIsOn" else "FlagIsOff")
        , tab TabB "B" (label "OtherTab")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  spans0 <- collectTextSpans ctx
  assertJust failed (spanRect "ToggleA" spans0) $ \r -> do
    _ <- runClick ctx inp0 (ui TabA) (spanCenter r)
    _ <- runFrame ctx inp0 (ui TabA)
    spans1 <- collectTextSpans ctx
    assertSpansHas failed "FlagIsOn" spans1

    _ <- runFrame ctx inp0 (ui TabB)
    spans2 <- collectTextSpans ctx
    assertSpansHas failed "OtherTab" spans2
    assert failed (not (hasText "FlagIsOn" spans2))

    _ <- runFrame ctx inp0 (ui TabA)
    spans3 <- collectTextSpans ctx
    assertSpansHas failed "FlagIsOn" spans3

runTabsDamageTest :: Context -> IORef Int -> IO ()
runTabsDamageTest ctx failed = do
  let inp0 = withInputOff 300 100
      ui curTab = tabs' (constVar curTab)
        [ tab TabA "Alpha" (label "Body A with some text")
        , tab TabB "Beta" (label "Body B different widgets")
        ]
  _ <- runFrame ctx inp0 (ui TabA)
  _ <- runFrame ctx inp0 (ui TabA)
  dIdle <- takeDamage ctx
  assert failed (dIdle /= DamageFull)
  spansIdle <- collectTextSpans ctx
  assertSpansHas failed "Body A" spansIdle

  assertJust failed (spanRect "Beta" spansIdle) $ \beta -> do
    resp <- runClick ctx inp0 (ui TabA) (spanCenter beta)
    assert failed (respChanged resp && tabActive resp == TabB)
    spansSwitch <- collectTextSpans ctx
    assertSpansHas failed "Body B" spansSwitch
    assert failed (not (hasText "Body A" spansSwitch))
    let bodyB = [r | (r, txt, _, _, _) <- spansSwitch, "Body B" `T.isInfixOf` txt]
    dSwitch <- takeDamage ctx
    assert failed (not (null bodyB) && all (damageCovers dSwitch) bodyB)

    -- The switch frame already drew the new body, so the frame that passes
    -- the new tab back has nothing to repaint.
    _ <- runFrame ctx inp0 (ui TabB)
    dTabB <- takeDamage ctx
    assert failed (dTabB /= DamageFull)

    _ <- runFrame ctx inp0 (ui TabB)
    dSettled <- takeDamage ctx
    assert failed (dSettled /= DamageFull)

-- Too-wide tab strips scroll instead of overflowing. The framework scroll
-- container owns the clip, the offset and the damage; the strip adds
-- chevron buttons on the left and right and pages the same offset with
-- them. Acceptance: scrolling left and right works (buttons, up/down wheel
-- notches mapped onto the horizontal offset) as well as left+right
-- (horizontal wheel, applied by the framework scroller itself); the tab
-- headers look exactly as they did before the strip could scroll (no
-- scroller well behind them); and no scrollbar appears.
runTabsScrollTest :: Context -> IORef Int -> IO ()
runTabsScrollTest wide failed = do
  let labels = ["Controls", "Graphics", "Typography", "Diagnostics", "LongestTabName"]
      mkTabs cur = tabBar' (constVar cur) [tab (i :: Int) l () | (i, l) <- zip [0 ..] labels]
      arrowRect ctx ch = do
        spans <- collectTextSpans ctx
        pure [r | (r, t, _, _, _) <- spans, T.any (== ch) t]
      chevrons = ['\8250', '\8249']

  -- Wide bar: everything fits and no arrows are drawn.
  let wideInp = withInput 900 120
  _ <- run2Frames wide wideInp (mkTabs 0)
  wideSpans <- collectTextSpans wide
  forM_ labels $ \l -> assert failed (hasText l wideSpans)
  assert failed (not (T.any (`elem` chevrons) (T.concat [t | (_, t, _, _, _) <- wideSpans])))

  ctx <- newContext
  let inp = withInput 240 120
      -- Whether the first header shows once @act@ and a settling frame ran.
      controlsAfter :: IO a -> IO Bool
      controlsAfter act = act >> runFrame ctx inp (mkTabs 0) >> hasText "Controls" <$> collectTextSpans ctx
  _ <- warmup2 ctx inp (mkTabs 0)
  -- The strip only pulls in its scroller once it has measured an overflow,
  -- after the first frame's layout, so the arrow buttons show from the second
  -- frame on.
  _ <- runFrame ctx inp (mkTabs 0)
  spans0 <- collectTextSpans ctx
  assert failed (hasText "Controls" spans0)
  assert failed (not (hasText "LongestTabName" spans0))

  -- No scroller well: while the strip is scrollable it must not paint the
  -- input background, the input border, or any scrollbar track or thumb
  -- behind the headers. Only the tab buttons themselves (and the arrows)
  -- may paint in the bar. The checked region is the full window width and
  -- the bar's height (header 28 + 4 slack + 2 slop), derived from the input
  -- so a resize of the test window cannot silently shrink coverage.
  theme <- readIORef (ctxTheme ctx)
  (_, dd, _) <- run2Frames ctx inp (mkTabs 0)
  quads <- drawQuads dd
  let Size winW _ = inputWindowSize inp
      bar = Rect 0 0 winW 34
      inputSurface = themeInput theme
      forbidden =
        [ styleBg inputSurface
        , styleBorder inputSurface
        , scrollBarTrackColor inputSurface theme
        , scrollBarThumbColor inputSurface theme
        ]
      inBar = [(r, c) | (r, c) <- quads, isJust (rectIntersect r bar), c `elem` forbidden]
  assert failed (null inBar)

  -- Left and right buttons page the strip; the right arrow is pinned to the
  -- bar's far edge rather than trailing the last visible tab.
  mRight <- arrowRect ctx '\8250'
  assertJust failed (listToMaybe mRight) $ \r -> do
    assert failed (rectX r + rectW r > 200)
    assert failed . not =<< controlsAfter (runClick ctx inp (mkTabs 0) (spanCenter r))
    mLeft <- arrowRect ctx '\8249'
    assertJust failed (listToMaybe mLeft) $ \left ->
      assert failed =<< controlsAfter (runClick ctx inp (mkTabs 0) (spanCenter left))

  -- Wheel up/down over the bar pages the window too (the strip maps the
  -- notches onto the horizontal offset).
  spans3 <- collectTextSpans ctx
  assertJust failed (spanRect "Controls" spans3) $ \c -> do
    let wheelDown = inp {inputMousePos = spanCenter c, inputScroll = V2 0 20}
    assert failed . not =<< controlsAfter (runFrame ctx wheelDown (mkTabs 0))

    -- Left+right wheel over the bar scrolls the same offset through the
    -- framework scroller. The vertical wheel pinned the offset at max, so
    -- wheeling right stays put (clamped at the end); wheeling left runs
    -- back to the start and re-shows the first tab, and further left
    -- notches clamp at zero instead of running past it. The deltas are
    -- coupled to the framework wheel step (scrollWheelStep, 60px per notch
    -- by default): V2 0 20 saturates at max, and +/-100 notches
    -- crosses the whole range regardless of the exact step.
    let wheelX d = inp {inputMousePos = spanCenter c, inputScroll = V2 d 0}
    assert failed . not =<< controlsAfter (runFrame ctx (wheelX 10) (mkTabs 0))
    assert failed =<< controlsAfter (runFrame ctx (wheelX (-100)) (mkTabs 0))
    assert failed =<< controlsAfter (runFrame ctx (wheelX (-100)) (mkTabs 0))

  -- Tab-list changes recompute the reachable range: shrinking back under
  -- the width drops the scroller and both arrows; growing past it again
  -- re-engages them and re-clips the tail. (One header fits the 240-wide
  -- window; two would still overflow the ~180px viewport between arrows.)
  let shortLabels = ["Controls"]
      mkShort cur = tabBar' (constVar cur) [tab (i :: Int) l () | (i, l) <- zip [0 ..] shortLabels]
  _ <- runFrame ctx inp (mkShort 0)
  _ <- warmup2 ctx inp (mkShort 0)
  spansS <- collectTextSpans ctx
  forM_ shortLabels $ \l -> assert failed (hasText l spansS)
  mRightS <- arrowRect ctx '\8250'
  mLeftS <- arrowRect ctx '\8249'
  assert failed (null mRightS && null mLeftS)
  _ <- runFrame ctx inp (mkTabs 0)
  _ <- warmup2 ctx inp (mkTabs 0)
  spansG <- collectTextSpans ctx
  mRightG <- arrowRect ctx '\8250'
  assert failed (not (null mRightG))
  assert failed (not (hasText "LongestTabName" spansG))

-- A contained body paints its own surface and border: when its text wraps
-- to fewer lines, the frame repaints where its bottom border was.
runTabsContainedBodyDamageTest :: Context -> IORef Int -> IO ()
runTabsContainedBodyDamageTest ctx failed = do
  let inp0 = withInputOff 300 300
      ui txt = tabsConfigured' defaultTabsConfig {tabsStyle = TabContained} (constVar TabA)
        [ tab TabA "Alpha" (labelWith fillW txt)
        , tab TabB "Beta" (label "Body B")
        ]
      long = T.unwords (replicate 30 "body")
      na = ctxNodeArena ctx
      -- The body is the column round the tab's label.
      bodyRect = do
        found <- findNodeM na (fmap (`elem` [long, "body"]) . getText na)
        traverse (getNodeRect na <=< getParent na) found
  _ <- warmup2 ctx inp0 (ui long)
  _ <- runFrame ctx inp0 (ui long)
  mTall <- bodyRect
  _ <- runFrame ctx inp0 (ui "body")
  dShrink <- takeDamage ctx
  mShort <- bodyRect
  assertJust failed ((,) <$> mTall <*> mShort) $ \(tall, short) -> do
    assert failed (rectH short < rectH tall)
    assert failed (damageCovers dShrink tall)
