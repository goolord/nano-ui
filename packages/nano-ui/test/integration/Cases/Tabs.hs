module Cases.Tabs (tests) where

import Spec
import Data.Hashable (Hashable (..))
import Data.Maybe (isJust)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Layout.Arena
  ( NodeArena
  , NodeIdx
  , arenaCount
  , findNodeM
  , getNodeFontSize
  , getNodeRect
  , getParent
  , getText
  , getWidgetId
  )
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "tabs-scroll" runTabsScrollTest
  , spec "tabs-scroll-close-button" runTabsScrollCloseButtonTest
  , spec "tabs-strip-in-page" runTabsStripInPageTest
  , spec "tabs-bodies" runTabsBodiesTest
  , spec "tabs-keyed-reorder-state" runTabsKeyedReorderTest
  , spec "tabs-missing-key" runTabsMissingKeyTest
  , spec "tabs-enum" runEnumTabsTest
  , spec "tabs-close" runTabsCloseTest
  , spec "tabs-disabled" runTabsDisabledTest
  , spec "tabs-contained-body-damage" runTabsContainedBodyDamageTest
  , spec "panel-body-swap-damage" runPanelBodySwapDamageTest
  , spec "tabs-badge-size" runTabsBadgeSizeTest
  , spec "tabs-drag-lifecycle" runTabDragLifecycleTest
  , spec "drag-escape-ownership" runDragEscapeOwnershipTest
  , spec "tabs-drag-controls" runTabDragControlsTest
  , spec "tabs-drag-reorder" runTabDragReorderTest
  , spec "tabs-drop-bounds" runTabsDropBoundsTest
  ]

data DummyTab = TabA | TabB | TabC
  deriving (Eq, Show, Enum, Bounded)

instance Hashable DummyTab where
  hashWithSalt salt = hashWithSalt salt . fromEnum

-- | Headers that overflow a 240px strip.
overflowLabels :: [T.Text]
overflowLabels = ["Controls", "Graphics", "Typography", "Diagnostics", "LongestTabName"]

-- | A strip of @labels@ keyed by @keys@, all plain tabs.
strip :: Var Int -> [Int] -> [T.Text] -> NanoUI (TabResponse Int)
strip active keys labels = tabBar' active [tab k l () | (k, l) <- zip keys labels]

-- | What @get@ reads off each node whose text is @txt@, in arena order.
nodesTexted :: T.Text -> (NodeArena -> NodeIdx -> IO b) -> Context -> IO [b]
nodesTexted txt get ctx = do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  filterM (fmap (== txt) . getText na) [0 .. n - 1] >>= mapM (get na)

-- | The close buttons' rects, in order.
closeRects :: Context -> IO [Rect]
closeRects = nodesTexted "\215" getNodeRect

-- | Whether some span of the last frame shows @txt@.
showing :: T.Text -> Sim a Bool
showing txt = any (\(_, t, _, _, _) -> txt `T.isInfixOf` t) <$> spans

-- | Whether the strip's paging arrows show.
hasArrows :: Sim a Bool
hasArrows = any (\(_, t, _, _, _) -> T.any (`elem` ['\8249', '\8250']) t) <$> spans

-- | The first span holding @ch@.
arrowRect :: Char -> Sim a Rect
arrowRect ch = do
  found : _ <- (\ss -> [r | (r, t, _, _, _) <- ss, T.any (== ch) t]) <$> spans
  pure found

at :: Float -> Float -> Target
at x y = At (V2 x y)

-- | Too-wide strips scroll: the arrows page them, as do the vertical wheel
-- (mapped onto the strip) and the sideways one, clamped at both ends, and a
-- trackpad's fraction of a notch moves them by that fraction. The headers
-- look as before the strip could scroll, with no scroller well or bar behind
-- them. The arrows go when the headers fit again, though the scroller between
-- them would still be too narrow, and a tab selected through the Var comes
-- into view, as does one appended and selected in the same frame.
runTabsScrollTest :: Context -> IORef Int -> IO ()
runTabsScrollTest ctx failed = do
  labelsRef <- newIORef overflowLabels
  active <- newVar 0
  let ui = column $ do
        _ <- strip active [0 ..] =<< liftIO (readIORef labelsRef)
        label "Below"
      setLabels = liftIO . writeIORef labelsRef
  runSim ctx failed (withInput 900 120) ui $ do
    mapM_ (check <=< showing) overflowLabels
    check . not =<< hasArrows
    [Rect x0 _ _ _] <- liftIO (nodesTexted "Controls" getNodeRect ctx)
    [Rect lx _ lw _] <- liftIO (nodesTexted "LongestTabName" getNodeRect ctx)
    -- The frame a strip first measures an overflow publishes its range. That
    -- is layout, not a scroll: the resize still repaints the whole window.
    modifyInput (\i -> i {inputWindowSize = Size 240 120})
    _ <- frame
    checkEq DamageFull =<< liftIO (takeDamage ctx)
    _ <- settle
    check =<< showing "Controls"
    check . not =<< showing "LongestTabName"
    theme <- liftIO (readIORef (ctxTheme ctx))
    inp <- getInput
    (_, dd, _) <- liftIO (run2Frames ctx inp ui)
    quads <- liftIO (drawQuads dd)
    let well = themeInput theme
        forbidden = [styleBg well, styleBorder well, scrollBarTrackColor well theme, scrollBarThumbColor well theme]
    check (null [c | (r, c) <- quads, isJust (rectIntersect r (Rect 0 0 240 34)), c `elem` forbidden])
    -- The right arrow is pinned to the bar's far edge. Paging repaints the
    -- left one, whose muted look no rect or text change covers.
    left <- arrowRect '\8249'
    right <- arrowRect '\8250'
    checkGt (rectX right + rectW right) 200
    _ <- click (Within right) >> frame
    check . not =<< showing "Controls"
    check . (`damageCovers` left) =<< liftIO (takeDamage ctx)
    _ <- click (Within left) >> frame
    check =<< showing "Controls"
    controls <- rectOf "Controls"
    _ <- wheel (Within controls) (V2 0 0.3) >> frame
    scrolled <- rectOf "Controls"
    checkLt (rectX scrolled) (rectX controls)
    forM_ [(V2 0 20, False), (V2 10 0, False), (V2 (-100) 0, True), (V2 (-100) 0, True)] $ \(d, shown) -> do
      _ <- wheel (Within controls) d >> frame
      checkEq shown =<< showing "Controls"
    -- One header fits the window, two would not fit between the arrows.
    setLabels ["Controls"] >> void settle
    check =<< showing "Controls"
    check . not =<< hasArrows
    setLabels overflowLabels >> void settle
    check =<< hasArrows
    check . not =<< showing "LongestTabName"
    -- The headers' extent and the window's padding on both sides: less
    -- spare room than the two arrows take.
    _ <- resize (lx + lw + x0 + 10) 120
    mapM_ (check <=< showing) overflowLabels
    check . not =<< hasArrows
    _ <- resize 240 120
    forM_ [(4, "LongestTabName"), (0, "Controls")] $ \(k, l) -> do
      liftIO (writeVarIO active k)
      check =<< (settle >> showing l)
    setLabels (overflowLabels ++ ["Appended"]) >> liftIO (writeVarIO active 5)
    check =<< (settle >> showing "Appended")

-- | A strip whose last close button runs past the bar overflows, though every
-- header's main button fits: the range counts each header's close button.
runTabsScrollCloseButtonTest :: Context -> IORef Int -> IO ()
runTabsScrollCloseButtonTest ctx failed = do
  let ui = tabBar' (constVar (0 :: Int)) [closableTab i l () | (i, l) <- zip [0 ..] ["Controls", "Graphics", "Typography"]]
  runSim ctx failed (withInput 900 120) ui $ do
    [Rect x0 _ _ _] <- liftIO (nodesTexted "Controls" getNodeRect ctx)
    closes <- liftIO (closeRects ctx)
    checkEq 3 (length closes)
    -- The last close button's right edge, less a little, and the window's
    -- right padding: only that button runs past the bar.
    _ <- resize (maximum [rectX r + rectW r | r <- closes] - 4 + x0) 120
    check =<< hasArrows

-- | A strip brings its active tab into view whatever its key, 0 too (the
-- last of headers keyed newest first). The vertical wheel over it pages the
-- strip alone, not the page scroller round it.
runTabsStripInPageTest :: Context -> IORef Int -> IO ()
runTabsStripInPageTest ctx failed = do
  let ui = scrollArea (tight . grow) $ do
        _ <- strip (constVar 0) [4, 3 .. 0] overflowLabels
        replicateM_ 20 (label "line")
  runSim ctx failed (withInput 240 120) ui $ do
    (page, _) <- result
    longest <- rectOf "LongestTabName"
    forM_ [(V2 0 (-20), True), (V2 0 20, False)] $ \(d, first) -> do
      _ <- wheel (Within longest) d >> frame
      checkEq first =<< showing "Controls"
      checkEq 0 =<< liftIO (getScrollOffset ctx page)

-- | Only the active tab's body runs. Bodies keep separate state, even at the
-- same position, and keep it while hidden. A header click switches tabs in
-- its own frame, repainting the new body, and the frames after repaint
-- nothing whole.
runTabsBodiesTest :: Context -> IORef Int -> IO ()
runTabsBodiesTest ctx failed = do
  ran <- newIORef []
  active <- newVar TabA
  let body name = do
        liftIO (modifyIORef' ran (name :))
        flagVar <- useVar False
        flag <- readVar flagVar
        whenM (button ("Toggle" <> name)) (writeVar flagVar (not flag))
        label (name <> if flag then "On" else "Off")
      ui = tabs' active [tab TabA "A" (body "A"), tab TabB "B" (body "B"), tab TabC "C" (body "C")]
      damage = liftIO (takeDamage ctx)
  runSim ctx failed (withInputOff 300 100) ui $ do
    check . all (== "A") =<< liftIO (readIORef ran)
    check . (/= DamageFull) =<< damage
    _ <- click "ToggleA" >> settle
    expect "AOn"
    switched <- click "B"
    check (respChanged switched && tabActive switched == TabB)
    expect "BOff"
    expectNot "AOn"
    bodyB <- rectOf "BOff"
    check . (`damageCovers` bodyB) =<< damage
    replicateM_ 2 (frame >> (check . (/= DamageFull) =<< damage))
    _ <- click "A" >> settle
    expect "AOn"

data Seen = Seen Int (Var Int) WidgetId

-- | Header focus and body-local state follow the key, not the list position.
runTabsKeyedReorderTest :: Context -> IORef Int -> IO ()
runTabsKeyedReorderTest ctx failed = do
  seen <- newIORef Nothing
  let inp = withInputOff 500 250
      body key = do
        nVar <- useVar (0 :: Int)
        n <- readVar nVar
        (field, _) <- withValue (T.pack (show key)) textInput'
        liftIO (writeIORef seen (Just (Seen n nVar (respId field))))
      ui active order = tabs' (constVar active) [tab key (T.pack (show key)) (body key) | key <- order]
      readBody = atomicModifyIORef' seen (Nothing,) >>= maybe (fail "tab body did not run") pure
  original <- warmup2 ctx inp (ui TabB [TabA, TabB, TabC])
  Seen _ nVar fieldId <- readBody
  runNanoUI ctx inp (writeVar nVar 7)
  writeIORef (ctxFocusId ctx) fieldId
  forM_ [[TabC, TabA, TabB], [TabB, TabC], [TabC, TabB]] $ \order -> do
    moved <- warmup2 ctx inp (ui TabB order)
    Seen n _ fieldId' <- readBody
    assertEq failed (7, fieldId) (n, fieldId')
    assertEq failed fieldId =<< getFocusId ctx
    forM_ order $ \key ->
      assertEq failed (respId <$> lookup key (tabHeaders original)) (respId <$> lookup key (tabHeaders moved))
  forM_ [(TabC, 0), (TabB, 7)] $ \(key, want) -> do
    _ <- warmup2 ctx inp (ui key [TabC, TabB])
    Seen n _ _ <- readBody
    assertEq failed want n

-- | An active key no tab has shows the first tab, its header and body alike,
-- and reports it.
runTabsMissingKeyTest :: Context -> IORef Int -> IO ()
runTabsMissingKeyTest ctx failed = do
  active <- newVar (3 :: Int)
  r <- warmup2 ctx (withInputOff 300 100) (tabs' active [tab 1 "One" (label "Body one"), tab 2 "Two" (label "Body two")])
  assertEq failed 1 (tabActive r)
  assertEq failed 1 =<< readVarIO active
  assertEq failed [1, 0] =<< buttonValues ctx
  assert failed . hasText "Body one" =<< collectTextSpans ctx

-- | enumTabs has a tab per value, labelled with show, and a click stores
-- the value itself in the Var; boundedTabBar does the same with labels of
-- its own and no bodies.
runEnumTabsTest :: Context -> IORef Int -> IO ()
runEnumTabsTest ctx failed = do
  page <- newVar TabA
  pick <- newVar TabA
  let ui = column $ do
        enumTabs page (label . ("Body of " <>) . T.pack . show)
        boundedTabBar (T.toLower . T.pack . show) pick
  runSim ctx failed (withInput 400 200) ui $ do
    expect "Body of TabA"
    _ <- click "TabC" >> settle
    checkEq TabC =<< liftIO (readVarIO page)
    expect "Body of TabC"
    _ <- click "tabb"
    checkEq TabB =<< liftIO (readVarIO pick)

-- | A click on a close button reports its tab closed and keeps the active
-- one. A click on a header just left of its close button selects the tab:
-- the close button does not take the press there and then lose the release.
runTabsCloseTest :: Context -> IORef Int -> IO ()
runTabsCloseTest ctx failed = do
  let ui = tabs' (constVar TabA) [closableTab TabA "Alpha" (label "Body A"), closableTab TabB "Beta" (label "Body B")]
  runSim ctx failed (withInput 300 100) ui $ do
    [closeA, Rect bx by _ bh] <- liftIO (closeRects ctx)
    closed <- click (Within closeA)
    checkEq (Just TabA, TabA) (tabClosed closed, tabActive closed)
    beside <- click (at (bx - 3) (by + bh / 2))
    checkEq (Nothing, TabB) (tabClosed beside, tabActive beside)

-- | The public disabled flag covers both the header and its close control,
-- including retained keyboard focus when an enabled tab becomes disabled, and
-- re-enabling the header restores activation.
runTabsDisabledTest :: Context -> IORef Int -> IO ()
runTabsDisabledTest _ failed = forM_ [TabTop, TabLeft] $ \orientation -> do
  ctx <- newContext
  disabledRef <- newIORef False
  let ui = do
        disabled <- liftIO (readIORef disabledRef)
        tabsConfigured' defaultTabsConfig {tabsOrientation = orientation} (constVar TabA)
          [ (closableTab TabB "Disabled" (label "Body B")) {tabDisabled = disabled}
          , tab TabA "Enabled" (label "Body A")
          ]
      inert r = checkEq (TabA, Nothing, False, False) (tabActive r, tabClosed r, respClicked r, respChanged r)
      disable b = liftIO (writeIORef disabledRef b) >> void settle
  runSim ctx failed (withInputOff 400 240) ui $ do
    wids <- liftIO ((++) <$> nodesTexted "Disabled" getWidgetId ctx <*> closeIds ctx)
    checkEq 2 (length wids)
    disable True
    inert =<< click "Disabled"
    [close] <- liftIO (closeRects ctx)
    inert =<< click (Within close)
    forM_ wids $ \wid -> do
      liftIO (writeIORef (ctxFocusId ctx) wid)
      inert =<< pressKey KeyEnter
    disable False
    checkEq TabB . tabActive =<< click "Disabled"
  where
    closeIds = nodesTexted "\215" getWidgetId

-- | A contained body paints its own surface and border: when its text wraps
-- to fewer lines, the frame repaints where its bottom border was.
runTabsContainedBodyDamageTest :: Context -> IORef Int -> IO ()
runTabsContainedBodyDamageTest ctx failed = do
  let inp0 = withInputOff 300 300
      ui txt = tabsConfigured' defaultTabsConfig {tabsStyle = TabContained} (constVar TabA)
        [tab TabA "Alpha" (labelWith fillW txt), tab TabB "Beta" (label "Body B")]
      long = T.unwords (replicate 30 "body")
      na = ctxNodeArena ctx
      -- The body is the column round the tab's label.
      bodyRect = findNodeM na (fmap (`elem` [long, "body"]) . getText na) >>= traverse (getNodeRect na <=< getParent na)
  _ <- run2Frames ctx inp0 (ui long) >> runFrame ctx inp0 (ui long)
  mTall <- bodyRect
  _ <- runFrame ctx inp0 (ui "body")
  dShrink <- takeDamage ctx
  mShort <- bodyRect
  assertJust failed ((,) <$> mTall <*> mShort) $ \(tall, short) -> do
    assert failed (rectH short < rectH tall)
    assert failed (damageCovers dShrink tall)

-- | Content replaced inside a floating window repaints every row of the new
-- body: a clip that skipped one would leave stale pixels of the old body.
runPanelBodySwapDamageTest :: Context -> IORef Int -> IO ()
runPanelBodySwapDamageTest ctx failed = do
  let inp0 = withInput 400 300
      win body = window' (constVar True) "TabWin" (columnWith (fixedH 300) body)
      uiA = win (label "WIDE BODY ROW ONE")
  _ <- run2Frames ctx inp0 uiA >> runFrame ctx inp0 uiA
  _ <- runFrame ctx inp0 (win (column (replicateM_ 5 (label "row line"))))
  dmg <- takeDamage ctx
  rows <- (\ss -> [r | (r, t, _, _, _) <- ss, "row line" `T.isInfixOf` t]) <$> collectOverlayTextSpans ctx inp0
  assertEq failed 5 (length rows)
  assert failed (not (damageIsEmpty dmg) && all (damageCovers dmg) rows)

-- | A badge is smaller than its header's text, whatever the base font size.
runTabsBadgeSizeTest :: Context -> IORef Int -> IO ()
runTabsBadgeSizeTest ctx0 failed = do
  let ctx = withFontSize ctx0 10
      na = ctxNodeArena ctx
  _ <- warmup2 ctx (withInputOff 300 100) (tabBar' (constVar (1 :: Int)) [(tab 1 "Inbox" ()) {tabBadge = Just "3"}])
  badge <- findNodeM na (fmap (== "3") . getText na)
  assertJust failed badge $ \i -> assertEq failed (10 * 0.78) =<< getNodeFontSize na i

-- | A header drag starts past the threshold, keeps its payload while the
-- headers reorder, and is released or cancelled: by Escape, by a release the
-- window never saw, or by its source going away.
runTabDragLifecycleTest :: Context -> IORef Int -> IO ()
runTabDragLifecycleTest ctx failed = do
  dragHandle <- newDrag
  let inp = withInput 400 200
      ui order = do
        r <- tabBar' (constVar TabA) [tab k (T.pack (show k)) () | k <- order]
        (,) (tabHeaders r) <$> useDrag dragHandle (tabHeaders r)
      keys = [TabA, TabB, TabC]
      phase i order = fmap (\d -> (dragPayload d, dragPhase d)) . snd <$> evalUi ctx i (ui order)
  (_, first) : _ <- fst <$> warmup2 ctx inp (ui keys)
  let V2 x y = centerOf first
      down = pressAt inp (V2 x y)
      moved = holdAt down (V2 (x + 20) y)
  assertEq failed Nothing =<< phase down keys
  assertEq failed Nothing =<< phase (holdAt down (V2 (x + 3) y)) keys
  assertEq failed (Just (TabA, DragStarted)) =<< phase moved keys
  assertEq failed (Just (TabA, Dragging)) =<< phase moved [TabC, TabB, TabA]
  assertEq failed (Just (TabA, DragReleased)) =<< phase (releaseAt moved) keys
  assertEq failed Nothing =<< phase inp keys
  forM_ [(keyInp KeyEscape moved, keys), (inp, keys), (moved, [TabB, TabC])] $ \(cancel, remaining) -> do
    mapM_ (`phase` keys) [down, moved]
    assertEq failed (Just DragCancelled) . fmap snd =<< phase cancel remaining
    assertEq failed Nothing =<< phase (releaseAt moved) keys
    void (warmup2 ctx inp (ui keys))

-- | Escape taken before the drag sees it leaves the drag going; untaken, it
-- cancels the drag and nothing after it sees the key, the dialog included.
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
        afterEscape takeFirst = fmap (\(_, gesture, later) -> (dragPhase <$> gesture, later)) . snd <$> evalUi ctx escape (ui takeFirst)
    mapM_ (\i -> evalUi ctx i (ui False)) [down, moved]
    assertEq failed (Just (Just Dragging, False)) =<< afterEscape True
    (dialog, cancelled) <- evalUi ctx escape (ui False)
    assertEq failed (Just (Just DragCancelled, False)) ((\(_, g, l) -> (dragPhase <$> g, l)) <$> cancelled)
    assertEq failed False (respClicked dialog)

-- | A close button and a disabled header do not start a drag.
runTabDragControlsTest :: Context -> IORef Int -> IO ()
runTabDragControlsTest ctx failed = do
  dragHandle <- newDrag
  let ui = do
        r <- tabBar' (constVar TabA) [closableTab TabA "Alpha" (), (tab TabB "Disabled" ()) {tabDisabled = True}]
        useDrag dragHandle (tabHeaders r)
  runSim ctx failed (withInput 400 200) ui $ do
    [close] <- liftIO (closeRects ctx)
    disabled <- rectOf "Disabled"
    forM_ [close, disabled] $ \r -> do
      let V2 x y = spanCenter r
      _ <- press (Within r)
      checkEq Nothing =<< moveTo (at (x + 20) y)
      release

-- | A header dropped on the far edge of the last one moves there, on the
-- release and not before.
runTabDragReorderTest :: Context -> IORef Int -> IO ()
runTabDragReorderTest ctx failed = do
  dragHandle <- newDrag
  orderRef <- newIORef [TabA, TabB, TabC]
  let ui = do
        order <- liftIO (readIORef orderRef)
        r <- tabBar' (constVar TabA) [tab k (T.pack (show k)) () | k <- order]
        gesture <- useDrag dragHandle (tabHeaders r)
        forM_ gesture $ \d -> when (dragPhase d == DragReleased) $ do
          let rest = filter ((/= dragPayload d) . fst) (tabHeaders r)
          forM_ (insertionIndex DragAxisX (tabStripRect r) (map (respRect . snd) rest) (dragAt d)) $ \i -> do
            let (before, after) = splitAt i (map fst rest)
            liftIO (writeIORef orderRef (before ++ dragPayload d : after))
        pure r
      currentOrder = liftIO (readIORef orderRef)
  runSim ctx failed (withInput 400 200) ui $ do
    Just third <- lookup TabC . tabHeaders <$> result
    let Rect x y w h = respRect third
    _ <- press "TabA"
    _ <- moveTo (at (x + w - 1) (y + h / 2))
    checkEq [TabA, TabB, TabC] =<< currentOrder
    _ <- release
    checkEq [TabB, TabC, TabA] =<< currentOrder
    checkEq [TabB, TabC, TabA] . map fst . tabHeaders =<< frame

-- | A strip's drop bounds are where it is drawn: inside its padded panel, and
-- cut to the pane it is clipped to, in either orientation.
runTabsDropBoundsTest :: Context -> IORef Int -> IO ()
runTabsDropBoundsTest ctx failed = do
  let inp = withInput 400 200
      bar orientation = tabBarConfigured' (TabsConfig TabUnderline orientation (pure ())) (constVar TabA)
  r <- warmup2 ctx inp (panelWith (padAll 40 . fillW . fillH) (bar TabLeft [tab TabA "Alpha" (), tab TabB "Beta" ()]))
  let bounds = tabStripRect r
  assert failed (rectX bounds >= 40 && rectY bounds >= 40)
  assertEq failed Nothing (insertionIndex DragAxisY bounds (map (respRect . snd) (tabHeaders r)) (V2 10 10))
  stripRect <- newIORef (Rect 0 0 0 0)
  forM_ [TabTop, TabLeft] $ \orientation -> do
    let pane _ _ = do
          s <- panelWith (fixedW 600) (bar orientation [tab TabA "Alpha" (), tab TabB "Beta" (), tab TabC "Gamma" ()])
          liftIO (writeIORef stripRect (tabStripRect s))
          pure (PaneView "Tabs" False)
    c <- newContext
    _ <- warmup2 c inp (paneGrid defaultPaneGridConfig {pgLayout = fixedWH 200 60, pgViewPane = pane})
    Rect _ _ w h <- readIORef stripRect
    assert failed (w > 0 && w <= 200 && h > 0 && h <= 60)
