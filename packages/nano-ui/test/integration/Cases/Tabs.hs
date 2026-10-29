{-# OPTIONS_GHC -O0 #-}
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
  , simSpec "tabs-scroll-close-button" 900 120 closeButtonsUi tabsScrollCloseButton
  , simSpec "tabs-strip-in-page" 240 120 stripInPageUi tabsStripInPage
  , spec "tabs-bodies" runTabsBodiesTest
  , spec "tabs-keyed-reorder-state" runTabsKeyedReorderTest
  , spec "tabs-missing-key" runTabsMissingKeyTest
  , simSpec "tabs-close" 300 100 closingUi tabsClose
  , spec "tabs-disabled" runTabsDisabledTest
  , spec "tabs-contained-body-damage" runTabsContainedBodyDamageTest
  , spec "panel-body-swap-damage" runPanelBodySwapDamageTest
  , spec "tabs-badge-size" runTabsBadgeSizeTest
  , spec "tabs-drag-lifecycle" runTabDragLifecycleTest
  , spec "drag-escape-ownership" runDragEscapeOwnershipTest
  , spec "tabs-drag-reorder" runTabDragReorderTest
  , spec "tabs-drop-bounds" runTabsDropBoundsTest
  ]

data DummyTab = TabA | TabB | TabC
  deriving (Eq, Show, Enum)

instance Hashable DummyTab where
  hashWithSalt salt = hashWithSalt salt . fromEnum

-- | Headers that overflow a 240px strip.
overflowLabels :: [T.Text]
overflowLabels = ["Controls", "Graphics", "Typography", "Diagnostics", "LongestTabName"]

-- | A strip of @labels@ keyed by @keys@, all plain tabs.
strip :: Var Int -> [Int] -> [T.Text] -> NanoUI (TabResponse Int)
strip active keys labels = tabBar' active [tab k l () | (k, l) <- zip keys labels]

-- | What @get@ reads off each node whose text is @txt@, in arena order.
nodesTexted :: T.Text -> (NodeArena -> NodeIdx -> IO b) -> Sim a [b]
nodesTexted txt get = onContext $ \ctx -> do
  let na = ctxNodeArena ctx
  n <- arenaCount na
  filterM (fmap (== txt) . getText na) [0 .. n - 1] >>= mapM (get na)

-- | The close buttons' rects, in order.
closeRects :: Sim a [Rect]
closeRects = nodesTexted "\215" getNodeRect

-- | Whether the strip's paging arrows show.
hasArrows :: Sim a Bool
hasArrows = any (\(_, t, _, _, _) -> T.any (`elem` ['\8249', '\8250']) t) <$> spans

-- | The first span holding @ch@.
arrowRect :: Char -> Sim a Rect
arrowRect ch = do
  found : _ <- (\ss -> [r | (r, t, _, _, _) <- ss, T.any (== ch) t]) <$> spans
  pure found

-- | A too-wide strip scrolls, paged by its arrows, either wheel (clamped) and
-- a fraction of a notch, with no well or bar behind the headers. The arrows
-- go once the headers fit the bar, and a tab selected through the Var, or
-- appended and selected at once, comes into view.
runTabsScrollTest :: Context -> IORef Int -> IO ()
runTabsScrollTest ctx failed = do
  labelsRef <- newIORef overflowLabels
  active <- newVar 0
  let ui = column $ do
        _ <- strip active [0 ..] =<< liftIO (readIORef labelsRef)
        label "Below"
      setLabels = liftIO . writeIORef labelsRef
  runSim ctx failed (withInput 900 120) ui $ do
    mapM_ (check <=< textShown) overflowLabels
    check . not =<< hasArrows
    [Rect x0 _ _ _] <- nodesTexted "Controls" getNodeRect
    [Rect lx _ lw _] <- nodesTexted "LongestTabName" getNodeRect
    -- The frame a strip first measures an overflow publishes its range. That
    -- is layout, not a scroll: the resize still repaints the whole window.
    modifyInput (\i -> i {inputWindowSize = Size 240 120})
    _ <- frame
    checkEq DamageFull =<< frameDamage
    _ <- settle
    check =<< textShown "Controls"
    check . not =<< textShown "LongestTabName"
    theme <- liftIO (readIORef (ctxTheme ctx))
    inp <- getInput
    (_, dd, _) <- liftIO (run2Frames ctx inp ui)
    quads <- liftIO (drawQuads dd)
    let well = themeInput theme
        forbidden =
          styleBg well : styleBorder well : map (\f -> f well theme) [scrollBarTrackColor, scrollBarThumbColor]
    check (null [c | (r, c) <- quads, isJust (rectIntersect r (Rect 0 0 240 34)), c `elem` forbidden])
    -- The right arrow is pinned to the bar's far edge. Paging repaints the
    -- left one, whose muted look no rect or text change covers.
    left <- arrowRect '\8249'
    right <- arrowRect '\8250'
    checkGt (rectX right + rectW right) 200
    _ <- click (Within right) >> frame
    check . not =<< textShown "Controls"
    check . (`damageCovers` left) =<< frameDamage
    _ <- click (Within left) >> frame
    check =<< textShown "Controls"
    controls <- rectOf "Controls"
    _ <- wheel (Within controls) (V2 0 0.3) >> frame
    scrolled <- rectOf "Controls"
    checkLt (rectX scrolled) (rectX controls)
    forM_ [(V2 0 20, False), (V2 10 0, False), (V2 (-100) 0, True), (V2 (-100) 0, True)] $ \(d, shown) -> do
      _ <- wheel (Within controls) d >> frame
      checkEq shown =<< textShown "Controls"
    -- One header fits the window, two would not fit between the arrows.
    setLabels ["Controls"] >> void settle
    check =<< textShown "Controls"
    check . not =<< hasArrows
    setLabels overflowLabels >> void settle
    check =<< hasArrows
    check . not =<< textShown "LongestTabName"
    -- The headers' extent and the window's padding on both sides: less
    -- spare room than the two arrows take.
    _ <- resize (lx + lw + x0 + 10) 120
    mapM_ (check <=< textShown) overflowLabels
    check . not =<< hasArrows
    _ <- resize 240 120
    forM_ [(4, "LongestTabName"), (0, "Controls")] $ \(k, l) -> do
      writeV active k
      check =<< (settle >> textShown l)
    setLabels (overflowLabels ++ ["Appended"]) >> writeV active 5
    check =<< (settle >> textShown "Appended")

-- | A strip whose last close button runs past the bar overflows, though every
-- header's main button fits: the range counts each header's close button.
tabsScrollCloseButton :: Sim (TabResponse Int) ()
tabsScrollCloseButton = do
  [Rect x0 _ _ _] <- nodesTexted "Controls" getNodeRect
  closes <- closeRects
  checkEq 3 (length closes)
  -- The last close button's right edge, less a little, and the window's
  -- right padding: only that button runs past the bar.
  _ <- resize (maximum [rectX r + rectW r | r <- closes] - 4 + x0) 120
  check =<< hasArrows

closeButtonsUi :: NanoUI (TabResponse Int)
closeButtonsUi =
  tabBar' (constVar 0) (zipWith (\i l -> closableTab i l ()) [0 ..] ["Controls", "Graphics", "Typography"])

-- | A strip brings its active tab into view whatever its key, 0 too, and the
-- vertical wheel over it pages the strip alone, not the page round it.
tabsStripInPage :: Sim (WidgetId, ()) ()
tabsStripInPage = do
  (page, _) <- result
  longest <- rectOf "LongestTabName"
  forM_ [(V2 0 (-20), True), (V2 0 20, False)] $ \(d, first) -> do
    _ <- wheel (Within longest) d >> frame
    checkEq first =<< textShown "Controls"
    checkEq 0 =<< scrollOffsetOf page

stripInPageUi :: NanoUI (WidgetId, ())
stripInPageUi = scrollArea (tight . grow) $ do
  _ <- strip (constVar 0) [4, 3 .. 0] overflowLabels
  replicateM_ 20 (label "line")

-- | Only the active body runs. Bodies keep separate state, even at the same
-- position, while hidden too. A header click switches tabs and repaints the
-- new body in its own frame, leaving the frames after it nothing whole.
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
  runSim ctx failed (withInputOff 300 100) ui $ do
    check . all (== "A") =<< liftIO (readIORef ran)
    check . (/= DamageFull) =<< frameDamage
    _ <- click "ToggleA" >> settle
    expect "AOn"
    switched <- click "B"
    check (respChanged switched && tabActive switched == TabB)
    expect "BOff"
    expectNot "AOn"
    bodyB <- rectOf "BOff"
    check . (`damageCovers` bodyB) =<< frameDamage
    replicateM_ 2 (frame >> (check . (/= DamageFull) =<< frameDamage))
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
  setFocus ctx fieldId
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
  let ui = tabs' active [tab k l (label ("Body " <> l)) | (k, l) <- [(1, "one"), (2, "two")]]
  r <- warmup2 ctx (withInputOff 300 100) ui
  assertEq failed 1 (tabActive r)
  assertEq failed 1 =<< readVarIO active
  assertEq failed [1, 0] =<< buttonValues ctx
  assert failed . hasText "Body one" =<< collectTextSpans ctx

-- | A close button reports its tab closed and keeps the active one. A click
-- just left of one selects its tab: the close button does not take the press.
tabsClose :: Sim (TabResponse DummyTab) ()
tabsClose = do
  [closeA, Rect bx by _ bh] <- closeRects
  closed <- click (Within closeA)
  checkEq (Just TabA, TabA) (tabClosed closed, tabActive closed)
  beside <- click (at (bx - 3) (by + bh / 2))
  checkEq (Nothing, TabB) (tabClosed beside, tabActive beside)

closingUi :: NanoUI (TabResponse DummyTab)
closingUi = tabs' (constVar TabA) [closableTab k (T.pack (show k)) (label "Body") | k <- [TabA, TabB]]

-- | A disabled header ignores clicks on it and its close control, and Enter
-- with focus kept from before; re-enabled, it activates again.
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
    wids <- (++) <$> nodesTexted "Disabled" getWidgetId <*> nodesTexted "\215" getWidgetId
    checkEq 2 (length wids)
    disable True
    inert =<< click "Disabled"
    [close] <- closeRects
    inert =<< click (Within close)
    forM_ wids $ \wid -> do
      focusOn wid
      inert =<< pressKey KeyEnter
    disable False
    checkEq TabB . tabActive =<< click "Disabled"

-- | A contained body that shrinks repaints where its own bottom border was.
runTabsContainedBodyDamageTest :: Context -> IORef Int -> IO ()
runTabsContainedBodyDamageTest ctx failed = do
  let inp0 = withInputOff 300 300
      ui txt = tabsConfigured' defaultTabsConfig {tabsStyle = TabContained} (constVar TabA)
        [tab TabA "Alpha" (labelWith fillW txt), tab TabB "Beta" (label "Body B")]
      long = T.unwords (replicate 30 "body")
      na = ctxNodeArena ctx
      -- The body is the column round the tab's label.
      bodyText = findNodeM na (fmap (`elem` [long, "body"]) . getText na)
      bodyRect = traverse (getNodeRect na <=< getParent na) =<< bodyText
  _ <- run2Frames ctx inp0 (ui long) >> runFrame ctx inp0 (ui long)
  mTall <- bodyRect
  _ <- runFrame ctx inp0 (ui "body")
  dShrink <- takeDamage ctx
  mShort <- bodyRect
  assertJust failed ((,) <$> mTall <*> mShort) $ \(tall, short) -> do
    assert failed (rectH short < rectH tall)
    assert failed (damageCovers dShrink tall)

-- | A body swapped inside a floating window repaints every row of the new
-- one, or stale pixels of the old body show through.
runPanelBodySwapDamageTest :: Context -> IORef Int -> IO ()
runPanelBodySwapDamageTest ctx failed = do
  let inp0 = withInput 400 300
      win body = window' (constVar True) "TabWin" (columnWith (fixedH 300) body)
      uiA = win (label "WIDE BODY ROW ONE")
  _ <- run2Frames ctx inp0 uiA >> runFrame ctx inp0 uiA
  _ <- runFrame ctx inp0 (win (column (replicateM_ 5 (label "row line"))))
  dmg <- takeDamage ctx
  ss <- collectOverlayTextSpans ctx inp0
  let rows = [r | (r, t, _, _, _) <- ss, "row line" `T.isInfixOf` t]
  assertEq failed 5 (length rows)
  assert failed (not (damageIsEmpty dmg) && all (damageCovers dmg) rows)

-- | A badge is smaller than its header's text, whatever the base font size.
runTabsBadgeSizeTest :: Context -> IORef Int -> IO ()
runTabsBadgeSizeTest ctx0 failed = do
  let ctx = withFontSize ctx0 10
      na = ctxNodeArena ctx
  let inbox = (tab (1 :: Int) "Inbox" ()) {tabBadge = Just "3"}
  _ <- warmup2 ctx (withInputOff 300 100) (tabBar' (constVar 1) [inbox])
  badge <- findNodeM na (fmap (== "3") . getText na)
  assertJust failed badge $ \i -> assertEq failed (10 * 0.78) =<< getNodeFontSize na i

-- | A header drag starts past the threshold, keeps its payload while the
-- headers reorder, and ends released or cancelled: by Escape, by a release
-- the window never saw, or by its source going away.
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
-- cancels the drag, and nothing after it sees the key, the dialog included.
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
        seen (_, gesture, later) = (dragPhase <$> gesture, later)
    mapM_ (\i -> evalUi ctx i (ui False)) [down, moved]
    assertEq failed (Just (Just Dragging, False)) . fmap seen . snd =<< evalUi ctx escape (ui True)
    (dialog, cancelled) <- evalUi ctx escape (ui False)
    assertEq failed (Just (Just DragCancelled, False)) (seen <$> cancelled)
    assertEq failed False (respClicked dialog)

-- | A close button and a disabled header start no drag. A header dropped on
-- the far edge of the last one moves there, on the release and not before.
runTabDragReorderTest :: Context -> IORef Int -> IO ()
runTabDragReorderTest ctx failed = do
  dragHandle <- newDrag
  orderRef <- newIORef [TabA, TabB, TabC]
  let header k = (if k == TabA then closableTab else tab) k (T.pack (show k)) ()
      ui = do
        order <- liftIO (readIORef orderRef)
        r <- tabBar' (constVar TabA) [(header k) {tabDisabled = k == TabC} | k <- order]
        gesture <- useDrag dragHandle (tabHeaders r)
        forM_ gesture $ \d -> when (dragPhase d == DragReleased) $ do
          let rest = filter ((/= dragPayload d) . fst) (tabHeaders r)
          forM_ (insertionIndex DragAxisX (tabStripRect r) (map (respRect . snd) rest) (dragAt d)) $ \i -> do
            let (before, after) = splitAt i (map fst rest)
            liftIO (writeIORef orderRef (before ++ dragPayload d : after))
        pure (r, gesture)
      currentOrder = liftIO (readIORef orderRef)
  runSim ctx failed (withInput 400 200) ui $ do
    [close] <- closeRects
    disabled <- rectOf "TabC"
    forM_ [close, disabled] $ \r -> do
      let V2 x y = spanCenter r
      _ <- press (Within r)
      checkEq Nothing . snd =<< moveTo (at (x + 20) y)
      release
    Just third <- lookup TabC . tabHeaders . fst <$> result
    let Rect x y w h = respRect third
    _ <- press "TabA"
    _ <- moveTo (at (x + w - 1) (y + h / 2))
    checkEq [TabA, TabB, TabC] =<< currentOrder
    _ <- release
    checkEq [TabB, TabC, TabA] =<< currentOrder
    checkEq [TabB, TabC, TabA] . map fst . tabHeaders . fst =<< frame

-- | A strip's drop bounds are where it is drawn: inside its padded panel, and
-- cut to the pane it is clipped to, in either orientation.
runTabsDropBoundsTest :: Context -> IORef Int -> IO ()
runTabsDropBoundsTest ctx failed = do
  let inp = withInput 400 200
      bar orientation n = tabBarConfigured' (TabsConfig TabUnderline orientation (pure ())) (constVar TabA) $
        take n [tab TabA "Alpha" (), tab TabB "Beta" (), tab TabC "Gamma" ()]
  r <- warmup2 ctx inp (panelWith (padAll 40 . fillW . fillH) (bar TabLeft 2))
  let bounds = tabStripRect r
  assert failed (rectX bounds >= 40 && rectY bounds >= 40)
  assertEq failed Nothing (insertionIndex DragAxisY bounds (map (respRect . snd) (tabHeaders r)) (V2 10 10))
  stripRect <- newIORef (Rect 0 0 0 0)
  forM_ [TabTop, TabLeft] $ \orientation -> do
    let pane _ _ = do
          s <- panelWith (fixedW 600) (bar orientation 3)
          liftIO (writeIORef stripRect (tabStripRect s))
          pure (PaneView "Tabs" False)
    c <- newContext
    _ <- warmup2 c inp (paneGrid defaultPaneGridConfig {pgLayout = fixedWH 200 60, pgViewPane = pane})
    Rect _ _ w h <- readIORef stripRect
    assert failed (w > 0 && w <= 200 && h > 0 && h <= 60)
