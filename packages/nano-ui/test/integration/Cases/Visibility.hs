module Cases.Visibility (tests) where

import Spec
import GHC.Stack (HasCallStack)
import NanoUI.Emit (emit)

tests :: [Spec]
tests =
  [ spec "visibility-initial" runInitialTest
  , spec "visibility-scroll" runScrollTest
  , spec "visibility-nested-scroll" runNestedScrollTest
  , spec "visibility-clipped" runClippedTest
  , spec "visibility-anticipate" runAnticipateTest
  , spec "visibility-removed" runRemovedTest
  , spec "visibility-tabs" runTabsTest
  , spec "visibility-two-passes" runTwoPassesTest
  , spec "visibility-use-visibility" runUseVisibilityTest
  , spec "visibility-ids" runIdsTest
  , spec "visibility-floating" runFloatingTest
  , spec "visibility-resize" runResizeTest
  , spec "visibility-layout-shift" runLayoutShiftTest
  , spec "visibility-disjoint-viewport-hit" runDisjointViewportHitTest
  , spec "visibility-pinned" runPinnedTest
  ]

inp :: Input
inp = withInputOff 300 200

-- | Run a frame: the view's result, and whether it asked for a follow-up frame.
step :: Context -> Input -> NanoUI a -> IO (a, Bool)
step ctx i ui = (\(a, _, _, follow) -> (a, follow)) <$> runFrame ctx i ui

settle :: Context -> Input -> NanoUI a -> Int -> IO ()
settle ctx i ui n = replicateM_ n (step ctx i ui)

-- | What a sensor reports: whether it is visible, and its event.
type State = (Bool, Maybe VisibilityEvent)

off, on, cameIn, wentOut :: State
off = (False, Nothing)
on = (True, Nothing)
cameIn = (True, Just BecameVisible)
wentOut = (False, Just BecameHidden)

hidden :: Visibility
hidden = Visibility False Nothing (Rect 0 0 0 0)

-- | Check what sensors report; a hidden one has an empty rect.
expectVis :: HasCallStack => IORef Int -> [State] -> [Visibility] -> IO ()
expectVis failed want vs = do
  assertEq failed want [(visVisible v, visEvent v) | v <- vs]
  forM_ vs $ \v -> unless (visVisible v) (assertEq failed (Rect 0 0 0 0) (visRect v))

-- | Run a frame per expected report of sensor @sel@ and follow-up request; return the results.
frames :: HasCallStack => IORef Int -> Context -> Input -> NanoUI a -> (a -> Visibility) -> [(State, Bool)] -> IO [a]
frames failed ctx i ui sel = mapM $ \(want, wantFollow) -> do
  (a, follow) <- step ctx i ui
  expectVis failed [want] [sel a]
  a <$ assertEq failed wantFollow follow

grey :: Color
grey = colorRGBA 90 90 90 255

-- | A 40-pixel row.
bar :: NanoUI ()
bar = box (fixedH 40 . fillW) grey

-- | A scroller @h@ pixels tall over a column without gaps.
scrollCol :: Float -> NanoUI a -> NanoUI (WidgetId, a)
scrollCol h = scrollArea (fixedH h . fillW) . columnWith (tight . gap 0 . fillW)

-- | Twenty rows in a 100-pixel scroller; row 10, 400 pixels down, is a sensor.
rows :: NanoUI (WidgetId, Visibility)
rows = scrollCol 100 (replicateM_ 10 bar *> (fst <$> sensorWith fillW bar) <* replicateM_ 9 bar)

-- A sensor on screen reports it once, on the follow-up frame the first one asks for.
runInitialTest :: Context -> IORef Int -> IO ()
runInitialTest ctx failed = do
  [_, v, _] <- frames failed ctx inp (column (fst <$> sensor (label "watched"))) id [(off, True), (cameIn, False), (on, False)]
  assert failed (rectW (visRect v) > 0 && rectH (visRect v) > 0)

-- Scrolling a row in and out reports each change once; 'visRect' is its part in the viewport.
runScrollTest :: Context -> IORef Int -> IO ()
runScrollTest ctx failed = do
  let go = frames failed ctx inp rows snd
  (sid, _) : _ <- go ((off, True) : replicate 3 (off, False))
  -- Row 10 now starts 70 pixels down the 100-pixel viewport.
  setScrollOffset ctx sid 330
  [_, (_, v2)] <- go [(off, True), (cameIn, False)]
  -- The view may have changed anything on seeing it: that frame repaints whole.
  takeDamage ctx >>= assertEq failed DamageFull
  assert failed (rectH (visRect v2) > 0 && rectH (visRect v2) < 40)
  assertJustM failed (getPrevRect ctx sid) $ \viewport ->
    assertEq failed (rectIntersect viewport (visRect v2)) (Just (visRect v2))
  -- Moved within view, the rect follows on the next frame, which is not asked for.
  setScrollOffset ctx sid 320
  [_, (_, v4)] <- go [(on, False), (on, False)]
  assertEq failed (rectY (visRect v4)) (rectY (visRect v2) + 10)
  takeDamage ctx >>= assert failed . (/= DamageFull)
  setScrollOffset ctx sid 0
  void $ go [(on, True), (wentOut, False), (off, False)]
  setScrollOffset ctx sid 380
  [_, (_, v)] <- go [(off, True), (cameIn, False)]
  assertEq failed 40 (rectH (visRect v))

-- A row in an inner scroller shows only when inside both viewports, even disjoint ones.
runNestedScrollTest :: Context -> IORef Int -> IO ()
runNestedScrollTest ctx failed = do
  let ui = scrollCol 100 $ do
        box (fixedH 150 . fillW) grey
        inner <- scrollCol 30 (fst <$> sensorWith fillW (box (fixedH 20 . fillW) grey) <* box (fixedH 200 . fillW) grey)
        inner <$ box (fixedH 300 . fillW) grey
      go = void . frames failed ctx inp ui (snd . snd)
  (outer, (inner, _)) <- evalUi ctx inp ui
  -- Scrolled, the row is in the outer viewport but above the inner; unscrolled, below the outer.
  forM_ [100, 0] $ \y -> setScrollOffset ctx inner y >> settle ctx inp ui 3 >> go [(off, False)]
  setScrollOffset ctx outer 150 >> go [(off, True), (cameIn, False)]
  setScrollOffset ctx inner 100 >> go [(on, True), (wentOut, False)]

-- A panel clips its overflow, and paint skips an empty container whose content overflows it.
runClippedTest :: Context -> IORef Int -> IO ()
runClippedTest ctx failed = do
  let ui = column $ do
        inPanel <- panelWith (fixedH 30 . fillW . tight . gap 0) $
          box (fixedH 40 . fillW) grey >> fst <$> sensorWith (fixedH 10 . fillW) (pure ())
        inEmpty <- columnWith (fixedH 0 . fillW . tight) (fst <$> sensorWith fillW (box (fixedH 20 . fillW) grey))
        shown <- fst <$> sensorWith fillW (box (fixedH 20 . fillW) grey)
        pure [inPanel, inEmpty, shown]
  settle ctx inp ui 3
  (vs, follow) <- step ctx inp ui
  expectVis failed [off, off, on] vs >> assert failed (not follow)

-- An anticipate margin shows a row before it scrolls in; a zero-height marker shows in view.
runAnticipateTest :: Context -> IORef Int -> IO ()
runAnticipateTest ctx failed = do
  let ui = scrollCol 100 $ do
        marker <- fst <$> sensorWith (fixedH 0 . fillW) (pure ())
        replicateM_ 3 bar
        -- Row 3 starts about 25 pixels below the viewport.
        sensed <- rowWith (tight . gap 0 . fillW) . forM [40, 0] $ \margin ->
          fst <$> sensorConfigured defaultSensorConfig {sensorAnticipate = margin, sensorLayout = fillW} bar
        (marker : sensed) <$ replicateM_ 10 bar
  (sid, _) <- evalUi ctx inp ui
  (_, vs@[_, near, _]) <- evalUi ctx inp ui
  expectVis failed [cameIn, cameIn, off] vs
  assertEq failed (visRect near) (Rect 0 0 0 0)
  setScrollOffset ctx sid 30 >> settle ctx inp ui 1
  expectVis failed [wentOut, on, cameIn] . snd =<< evalUi ctx inp ui

-- A sensor not built is forgotten: no frames while gone, a new report when back, stable ids.
runRemovedTest :: Context -> IORef Int -> IO ()
runRemovedTest ctx failed = do
  shownRef <- newIORef True
  let ui = column $ do
        shown <- uiIO (readIORef shownRef)
        vis <- scope (if shown then fst <$> sensor (label "watched") else pure hidden)
        (,) vis . respId <$> button' "After"
      go = frames failed ctx inp ui fst
  a <- go [(off, True), (cameIn, False)]
  writeIORef shownRef False
  b <- go [(off, False), (off, False)]
  writeIORef shownRef True
  c <- go [(off, True), (cameIn, False)]
  let ids = map snd (a ++ b ++ c)
  assert failed (and (zipWith (==) ids (drop 1 ids)))

-- A sensor in a hidden tab's body is forgotten while another tab shows.
runTabsTest :: Context -> IORef Int -> IO ()
runTabsTest ctx failed = do
  activeRef <- newIORef (0 :: Int)
  eventsRef <- newIORef []
  let watched = sensor (label "in tab A") >>= \(vis, _) -> forM_ (visEvent vis) (uiIO . modifyIORef' eventsRef . (:))
      ui = do
        active <- uiIO (readIORef activeRef)
        void (tabs active [tab 0 "A" watched, tab 1 "B" (label "in tab B")])
  settle ctx inp ui 3
  writeIORef activeRef 1 >> settle ctx inp ui 3
  (_, follow) <- step ctx inp ui
  assert failed (not follow)
  writeIORef activeRef 0 >> settle ctx inp ui 3
  readIORef eventsRef >>= assertEq failed [BecameVisible, BecameVisible]

-- A hook write on the event reruns the view, which sees no event; a message is sent once.
runTwoPassesTest :: Context -> IORef Int -> IO ()
runTwoPassesTest ctx failed = do
  seenRef <- newIORef (0 :: Int)
  let view _ = column $ do
        (loaded, setLoaded) <- useFlag False
        (vis, _) <- sensor (label (if loaded then "loaded" else "loading"))
        when (becameVisible vis) (uiIO (modifyIORef' seenRef (+ 1)) >> emit (1 :: Int) >> setLoaded True)
        pure (loaded, (visVisible vis, visEvent vis))
      frame model = (\(r, model', _, _, _) -> (r, model')) <$> runFrameReduce (+) ctx inp model view
  (second, model2) <- frame (0 :: Int) >>= frame . snd
  model5 <- snd <$> (frame model2 >>= frame . snd >>= frame . snd)
  seen <- readIORef seenRef
  assertEq failed ((True, on), 1, 1) (second, seen, model5)

-- 'useVisibility' watches a widget built before it, takes one id, and reports it gone once.
runUseVisibilityTest :: Context -> IORef Int -> IO ()
runUseVisibilityTest ctx failed = do
  shownRef <- newIORef True
  targetRef <- newIORef (WidgetId 0)
  let ui = scrollCol 100 $ do
        replicateM_ 5 bar
        shown <- uiIO (readIORef shownRef)
        scope . when shown $ uiIO . writeIORef targetRef . respId =<< button' "Target"
        target <- uiIO (readIORef targetRef)
        vis <- useVisibility 0 target
        after <- button' "After"
        (vis, target, respId after) <$ replicateM_ 5 bar
      go = frames failed ctx inp ui (\(_, (v, _, _)) -> v)
  [(sid, (_, target0, after0))] <- go [(off, True)]
  setScrollOffset ctx sid 180
  [_, (_, (v, target, after))] <- go [(off, True), (cameIn, False)]
  assertJustM failed (getPrevRect ctx target) $ \r ->
    assertEq failed (rectIntersect r (visRect v)) (Just (visRect v))
  assertEq failed (target, after) (target0, after0)
  writeIORef shownRef False
  void $ go [(on, True), (wentOut, False)]

-- A sensor takes the one id a column takes, so swapping one for the other moves no sibling.
runIdsTest :: Context -> IORef Int -> IO ()
runIdsTest ctx failed = do
  let after enclose n = column $ do
        _ <- enclose (replicateM_ n (label "item"))
        respId <$> button' "After"
  plain <- evalUi ctx inp (after column 2)
  forM_ [0, 2, 5] $ \n -> evalUi ctx inp (after (fmap snd . sensor) n) >>= assertEq failed plain

-- A sensor in a window declared in a scroller's scrolled-out content is on screen.
runFloatingTest :: Context -> IORef Int -> IO ()
runFloatingTest ctx failed = do
  let wide = withInput 400 300
      ui = scrollArea (fixedH 100 . fixedW 60) . columnWith (tight . gap 0 . fillW) $ do
        replicateM_ 10 bar
        (_, inWindow) <- window True "Panel" (fst <$> sensor (label "in window"))
        (,) inWindow . fst <$> sensor bar
  settle ctx wide ui 2
  ((_, (inWindow, below)), follow) <- step ctx wide ui
  assertEq failed (visVisible <$> inWindow, below, follow) (Just True, hidden, False)

-- A resize hides a sensor on the follow-up frame it asks for; growing back shows it.
runResizeTest :: Context -> IORef Int -> IO ()
runResizeTest ctx failed = do
  let ui = column (box (fixedH 150 . fillW) grey >> fst <$> sensorWith fillW bar)
      go h = void . frames failed ctx (withInput 300 h) ui id
  settle ctx (withInput 300 300) ui 3
  go 300 [(on, False)]
  go 100 [(on, True), (wentOut, False), (off, False)]
  go 300 [(off, True), (cameIn, False)]

-- Content growing above two sensors hides both on one follow-up frame.
runLayoutShiftTest :: Context -> IORef Int -> IO ()
runLayoutShiftTest ctx failed = do
  countRef <- newIORef (0 :: Int)
  let ui = scrollCol 100 $ do
        n <- uiIO (readIORef countRef)
        scope (replicateM_ n bar)
        replicateM 2 (fst <$> sensorWith fillW bar)
  settle ctx inp ui 3
  ((_, before), follow1) <- step ctx inp ui
  writeIORef countRef 3
  (_, follow2) <- step ctx inp ui
  ((_, after), follow3) <- step ctx inp ui
  expectVis failed [on, on, wentOut, wentOut] (before ++ after)
  assertEq failed [False, True, False] [follow1, follow2, follow3]

-- A button outside an inner viewport but in the outer's rect is hidden and inert; in both, live.
runDisjointViewportHitTest :: Context -> IORef Int -> IO ()
runDisjointViewportHitTest ctx failed = do
  let filler h = spacer (Fixed 10) (Fixed h)
      ui = scrollArea (fixedH 100 . fillW) . column $ do
        filler 150
        inner <- scrollArea (fixedH 30 . fillW) . column $ sensor (button' "Deep") <* filler 200
        inner <$ filler 100
      look i = snd . snd <$> evalUi ctx i ui
      -- The sensor's report, and whether the button hovers, presses and clicks.
      probe = do
        (vis, deep) <- look inp
        let over = inp {inputMousePos = centerOf deep}
            (press, release) = clickPair over (centerOf deep)
        void (look over)
        flags <- forM [(over, respHovered), (press, respPressed), (release, respClicked)] $ \(i, f) -> f . snd <$> look i
        pure (vis, respRect deep, flags)
  (outer, (inner, _)) <- evalUi ctx inp ui
  -- Scrolled by 100, the inner scroller puts the button at about y 50.
  setScrollOffset ctx inner 100 >> settle ctx inp ui 3
  (vis1, Rect _ by _ bh, flags1) <- probe
  assert failed (by >= 0 && by + bh < 100)
  assertEq failed (hidden, [False, False, False]) (vis1, flags1)
  -- The outer scroller brings the inner one up, which shows its top.
  setScrollOffset ctx outer 150 >> setScrollOffset ctx inner 0 >> settle ctx inp ui 3
  (vis2, _, flags2) <- probe
  assertEq failed (True, [True, True, True]) (visVisible vis2, flags2)

-- A sensor pinned into view from a sizeless or scrolled-out container is drawn and visible.
runPinnedTest :: Context -> IORef Int -> IO ()
runPinnedTest ctx failed = do
  let (red, blue) = (colorRGBA 200 40 40 255, colorRGBA 40 40 200 255)
      ui = columnWith (tight . gap 0 . fillW) $ do
        (inStack, _) <- stackWith tight (sensorWith (pinAt 10 10) (box (fixedWH 20 20) red))
        (sid, (hanging, _)) <- scrollCol 100 $
          columnWith (tight . fixedH 10 . fillW) (sensorWith (pinAt 0 150) (box (fixedWH 20 20) blue))
            <* box (fixedH 400 . fillW) grey
        pure (sid, [inStack, hanging])
  (sid, _) <- evalUi ctx inp ui
  -- The strip is above the viewport, the box it pins 50 pixels down it.
  setScrollOffset ctx sid 100 >> settle ctx inp ui 3
  ((_, vs), _, draw, follow) <- runFrame ctx inp ui
  colours <- map snd <$> drawQuads draw
  assertEq failed ([True, True], False) (map (`elem` colours) [red, blue], follow)
  expectVis failed [on, on] vs
  assertEq failed [(20, 20), (20, 20)] [(w, h) | Rect _ _ w h <- map visRect vs]
