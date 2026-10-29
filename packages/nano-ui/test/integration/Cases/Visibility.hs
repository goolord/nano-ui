{-# OPTIONS_GHC -O0 #-}
module Cases.Visibility (tests) where

import Spec
import Control.Concurrent (threadDelay)
import GHC.Stack (HasCallStack)

tests :: [Spec]
tests =
  [ spec "visibility-initial" runInitialTest
  , spec "visibility-scroll" runScrollTest
  , spec "visibility-nested-scroll" runNestedScrollTest
  , spec "visibility-clipped" runClippedTest
  , spec "visibility-anticipate" runAnticipateTest
  , spec "visibility-removed" runRemovedTest
  , spec "visibility-tabs" runTabsTest
  , spec "visibility-use-visibility" runUseVisibilityTest
  , spec "visibility-layout-change" runLayoutChangeTest
  , spec "visibility-delay" runDelayTest
  , spec "visibility-shared-id" runSharedIdTest
  ]

inp :: Input
inp = withInputOff 300 200

-- | Run a frame; return the view's result and whether it asked for a follow-up frame.
step :: Context -> Input -> NanoUI a -> IO (a, Bool)
step ctx i ui = (\(a, _, follow) -> (a, follow)) <$> runFrame ctx i ui

settle :: Context -> Input -> NanoUI a -> Int -> IO ()
settle ctx i ui n = replicateM_ n (step ctx i ui)

-- | A sensor's visibility and event.
type State = (Bool, Maybe VisibilityEvent)

off, on, cameIn, wentOut :: State
off = (False, Nothing)
on = (True, Nothing)
cameIn = (True, Just BecameVisible)
wentOut = (False, Just BecameHidden)

hidden :: Visibility
hidden = Visibility False Nothing (Rect 0 0 0 0) (Rect 0 0 0 0)

-- | A report minus 'visBounds', which a hidden widget still has.
reported :: Visibility -> (Bool, Maybe VisibilityEvent, Rect)
reported v = (visVisible v, visEvent v, visRect v)

-- | Check what sensors report; a hidden one has an empty rect.
expectVis :: HasCallStack => IORef Int -> [State] -> [Visibility] -> IO ()
expectVis failed want vs = do
  assertEq failed want [(visVisible v, visEvent v) | v <- vs]
  forM_ vs $ \v -> unless (visVisible v) (assertEq failed (Rect 0 0 0 0) (visRect v))

-- | Run one frame per expected (report, follow-up) pair, checking sensor @sel@.
frames ::
  HasCallStack => IORef Int -> Context -> Input -> NanoUI a -> (a -> Visibility) -> [(State, Bool)] -> IO [a]
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

-- | Twenty rows in a 100-pixel scroller; row 10 (400 pixels down) is a sensor.
rows :: NanoUI (WidgetId, Visibility)
rows = scrollCol 100 (replicateM_ 10 bar *> (fst <$> sensorWith fillW bar) <* replicateM_ 9 bar)

-- An on-screen sensor reports once, on the follow-up frame the first requests.
-- A sensor takes as many ids as a column, so swapping them keeps sibling ids.
runInitialTest :: Context -> IORef Int -> IO ()
runInitialTest ctx failed = do
  let watched = column (fst <$> sensor (label "watched"))
  [_, v, _] <- frames failed ctx inp watched id [(off, True), (cameIn, False), (on, False)]
  assert failed (rectW (visRect v) > 0 && rectH (visRect v) > 0)
  let after enclose n = column (enclose (replicateM_ n (label "item")) >> respId <$> button' "After")
  plain <- evalUi ctx inp (after column 2)
  forM_ [0, 2, 5] $ \n -> evalUi ctx inp (after (fmap snd . sensor) n) >>= assertEq failed plain

-- Scrolling a row in and out reports each change once; 'visRect' is its
-- visible part, and 'visBounds' the whole row, which a hidden row keeps.
runScrollTest :: Context -> IORef Int -> IO ()
runScrollTest ctx failed = do
  let go = frames failed ctx inp rows snd
  -- Nothing on screen changes and nothing read the layout, so the first
  -- frame asks for no second.
  (sid, _) : _ <- go (replicate 4 (off, False))
  -- Row 10 now starts 70 pixels down the 100-pixel viewport.
  setScrollOffset ctx sid 330
  [_, (_, v2)] <- go [(off, True), (cameIn, False)]
  -- The view may react to the event arbitrarily, so that frame repaints fully.
  takeDamage ctx >>= assertEq failed DamageFull
  assert failed (rectH (visRect v2) > 0 && rectH (visRect v2) < 40)
  assertEq failed (40, Just (visRect v2)) (rectH (visBounds v2), rectIntersect (visBounds v2) (visRect v2))
  assertJustM failed (getPrevRect ctx sid) $ \viewport ->
    assertEq failed (rectIntersect viewport (visRect v2)) (Just (visRect v2))
  -- Moving within view updates the rect next frame without requesting one.
  setScrollOffset ctx sid 320
  [_, (_, v4)] <- go [(on, False), (on, False)]
  assertEq failed (rectY (visRect v4)) (rectY (visRect v2) + 10)
  takeDamage ctx >>= assert failed . (/= DamageFull)
  setScrollOffset ctx sid 0
  [_, (_, gone), _] <- go [(on, True), (wentOut, False), (off, False)]
  assertEq failed 40 (rectH (visBounds gone))
  setScrollOffset ctx sid 380
  [_, (_, v)] <- go [(off, True), (cameIn, False)]
  assertEq failed 40 (rectH (visRect v))

-- A button in an inner scroller is visible and live only inside both viewports, even
-- disjoint ones; outside the inner but inside the outer it is hidden and inert.
runNestedScrollTest :: Context -> IORef Int -> IO ()
runNestedScrollTest ctx failed = do
  let filler h = spacer (Fixed 10) (Fixed h)
      ui = scrollCol 100 $ do
        filler 150
        inner <- scrollArea (fixedH 30 . fillW) . column $ sensor (button' "Deep") <* filler 200
        inner <$ filler 300
      go = void . frames failed ctx inp ui (fst . snd . snd)
      look i = snd . snd <$> evalUi ctx i ui
      -- The sensor report, and whether the button hovers, presses and clicks.
      probe = do
        (vis, deep) <- look inp
        let over = inp {inputMousePos = centerOf deep}
            (press, release) = clickPair over (centerOf deep)
        void (look over)
        flags <- forM [(over, respHovered), (press, respPressed), (release, respClicked)] $ \(i, f) ->
          f . snd <$> look i
        pure (vis, respRect deep, flags)
  (outer, (inner, _)) <- evalUi ctx inp ui
  -- Inner scrolled by 100: the button is above the inner viewport, at about y 50.
  setScrollOffset ctx inner 100 >> settle ctx inp ui 3 >> go [(off, False)]
  (vis1, Rect _ by _ bh, flags1) <- probe
  assert failed (by >= 0 && by + bh < 100)
  assertEq failed (reported hidden, [False, False, False]) (reported vis1, flags1)
  -- Unscrolled, it is below the outer viewport.
  setScrollOffset ctx inner 0 >> settle ctx inp ui 3 >> go [(off, False)]
  setScrollOffset ctx outer 150 >> go [(off, True), (cameIn, False)]
  (vis2, _, flags2) <- probe
  assertEq failed (True, [True, True, True]) (visVisible vis2, flags2)
  setScrollOffset ctx inner 100 >> go [(on, True), (wentOut, False)]

-- Sensors clipped by a panel, or in a zero-height container, are hidden. One in a
-- window declared in scrolled-out content is visible, and so is one pinned out of a
-- sizeless or scrolled-out container, which is drawn.
runClippedTest :: Context -> IORef Int -> IO ()
runClippedTest ctx failed = do
  let box20 = box (fixedH 20 . fillW) grey
      ui = column $ do
        inPanel <- panelWith (fixedH 30 . fillW . tight . gap 0) $
          box (fixedH 40 . fillW) grey >> fst <$> sensorWith (fixedH 10 . fillW) (pure ())
        inEmpty <- columnWith (fixedH 0 . fillW . tight) (fst <$> sensorWith fillW box20)
        shown <- fst <$> sensorWith fillW box20
        pure [inPanel, inEmpty, shown]
  settle ctx inp ui 3
  (vs, follow) <- step ctx inp ui
  expectVis failed [off, off, on] vs >> assert failed (not follow)
  let floating = scrollArea (fixedH 100 . fixedW 60) . columnWith (tight . gap 0 . fillW) $ do
        replicateM_ 10 bar
        (_, inWindow) <- window' (constVar True) "Panel" (fst <$> sensor (label "in window"))
        (,) inWindow . fst <$> sensor bar
  c <- newContext
  settle c (withInput 400 300) floating 2
  ((_, (inWindow, below)), follow') <- step c (withInput 400 300) floating
  assertEq failed (visVisible <$> inWindow, reported below, follow') (Just True, reported hidden, False)
  let (red, blue) = (colorRGBA 200 40 40 255, colorRGBA 40 40 200 255)
      pinned = columnWith (tight . gap 0 . fillW) $ do
        (inStack, _) <- layersWith tight (sensorWith (pinAt 10 10) (box (fixedWH 20 20) red))
        (sid, (hanging, _)) <- scrollCol 100 $
          columnWith (tight . fixedH 10 . fillW) (sensorWith (pinAt 0 150) (box (fixedWH 20 20) blue))
            <* box (fixedH 400 . fillW) grey
        pure (sid, [inStack, hanging])
  p <- newContext
  (sid, _) <- evalUi p inp pinned
  -- The strip is above the viewport; its pinned box is 50 pixels into it.
  setScrollOffset p sid 100 >> settle p inp pinned 3
  ((_, pvs), draw, pfollow) <- runFrame p inp pinned
  colours <- map snd <$> drawQuads draw
  assertEq failed ([True, True], False) (map (`elem` colours) [red, blue], pfollow)
  expectVis failed [on, on] pvs
  assertEq failed [(20, 20), (20, 20)] [(w, h) | Rect _ _ w h <- map visRect pvs]

-- An anticipate margin reports a row visible before it scrolls in; a zero-height
-- marker in view is visible.
runAnticipateTest :: Context -> IORef Int -> IO ()
runAnticipateTest ctx failed = do
  let ui = scrollCol 100 $ do
        marker <- fst <$> sensorWith (fixedH 0 . fillW) (pure ())
        replicateM_ 3 bar
        -- Row 3 starts about 25 pixels below the viewport.
        sensed <- rowWith (tight . gap 0 . fillW) . forM [40, 0] $ \margin ->
          fst <$> sensorConfigured defaultSensorConfig {sensorAnticipate = margin, sensorLayout = fillW} bar
        (marker : sensed) <$ replicateM_ 10 bar
  (sid, vs@[_, near, _]) <- warmup2 ctx inp ui
  expectVis failed [cameIn, cameIn, off] vs
  assertEq failed (visRect near) (Rect 0 0 0 0)
  setScrollOffset ctx sid 30 >> settle ctx inp ui 1
  expectVis failed [wentOut, on, cameIn] . snd =<< evalUi ctx inp ui

-- An unbuilt sensor is forgotten: no frames while gone, a fresh report on
-- return, and stable ids after it; also beside sensors sharing an id, and
-- when swapped for another sensor in one frame.
runRemovedTest :: Context -> IORef Int -> IO ()
runRemovedTest _ failed = do
  shownRef <- newIORef True
  let shown = liftIO (readIORef shownRef)
      watched = fst <$> sensor (label "watched")
      maybeWatched = shown >>= \s -> scope (if s then watched else pure hidden)
      ui = column ((,) <$> maybeWatched <*> (respId <$> button' "After"))
      twins = column (replicateM_ 2 (withKey (0 :: Int) (sensor (label "twin"))) >> maybeWatched)
      swapped = column (shown >>= \s -> withKey s watched)
      reports = [(off, True), (cameIn, False)]
      hideAndShow :: NanoUI a -> (a -> Visibility) -> [(State, Bool)] -> IO [a]
      hideAndShow view sel whileHidden = do
        ctx <- newContext
        writeIORef shownRef True
        a <- frames failed ctx inp view sel reports
        b <- writeIORef shownRef False >> frames failed ctx inp view sel whileHidden
        c <- writeIORef shownRef True >> frames failed ctx inp view sel reports
        pure (a ++ b ++ c)
  ids <- map snd <$> hideAndShow ui fst [(off, False), (off, False)]
  assert failed (and (zipWith (==) ids (drop 1 ids)))
  void (hideAndShow twins id [(off, False)])
  void (hideAndShow swapped id reports)

-- Watches sharing an id are one sensor: the last watch is measured, and the
-- first read takes the event.
runSharedIdTest :: Context -> IORef Int -> IO ()
runSharedIdTest ctx failed = do
  let ui = scrollCol 100 $ do
        shown <- respId <$> button' "Shown"
        replicateM_ 5 bar
        below <- respId <$> button' "Below"
        forM [below, shown] (withKey (0 :: Int) . useVisibility defaultSensorConfig)
  ((_, first), follow) <- step ctx inp ui
  expectVis failed [off, off] first >> assert failed follow
  ((_, second), _) <- step ctx inp ui
  expectVis failed [cameIn, on] second

-- A sensor in a hidden tab's body is forgotten while another tab shows.
runTabsTest :: Context -> IORef Int -> IO ()
runTabsTest ctx failed = do
  activeRef <- newIORef (0 :: Int)
  eventsRef <- newIORef []
  let watched = sensor (label "in tab A") >>= \(vis, _) ->
        forM_ (visEvent vis) (liftIO . modifyIORef' eventsRef . (:))
      ui = do
        active <- liftIO (readIORef activeRef)
        void (tabs' (constVar active) [tab 0 "A" watched, tab 1 "B" (label "in tab B")])
  settle ctx inp ui 3
  writeIORef activeRef 1 >> settle ctx inp ui 3
  (_, follow) <- step ctx inp ui
  assert failed (not follow)
  writeIORef activeRef 0 >> settle ctx inp ui 3
  readIORef eventsRef >>= assertEq failed [BecameVisible, BecameVisible]

-- 'useVisibility' watches an earlier widget, takes one id, and reports its removal once.
runUseVisibilityTest :: Context -> IORef Int -> IO ()
runUseVisibilityTest ctx failed = do
  shownRef <- newIORef True
  targetRef <- newIORef (WidgetId 0)
  let ui = scrollCol 100 $ do
        replicateM_ 5 bar
        shown <- liftIO (readIORef shownRef)
        scope . when shown $ liftIO . writeIORef targetRef . respId =<< button' "Target"
        target <- liftIO (readIORef targetRef)
        vis <- useVisibility defaultSensorConfig target
        after <- button' "After"
        (vis, target, respId after) <$ replicateM_ 5 bar
      go = frames failed ctx inp ui (\(_, (v, _, _)) -> v)
  [(sid, (_, target0, after0))] <- go [(off, False)]
  setScrollOffset ctx sid 180
  [_, (_, (v, target, after))] <- go [(off, True), (cameIn, False)]
  assertJustM failed (getPrevRect ctx target) $ \r ->
    assertEq failed (rectIntersect r (visRect v)) (Just (visRect v))
  assertEq failed (target, after) (target0, after0)
  writeIORef shownRef False
  void $ go [(on, True), (wentOut, False)]

-- Shrinking the window hides a sensor on the follow-up frame and growing it back
-- shows it; content growing above two sensors hides both on one follow-up frame.
runLayoutChangeTest :: Context -> IORef Int -> IO ()
runLayoutChangeTest ctx failed = do
  let ui = column (box (fixedH 150 . fillW) grey >> fst <$> sensorWith fillW bar)
      go h = void . frames failed ctx (withInput 300 h) ui id
  settle ctx (withInput 300 300) ui 3
  go 300 [(on, False)]
  go 100 [(on, True), (wentOut, False), (off, False)]
  go 300 [(off, True), (cameIn, False)]
  countRef <- newIORef (0 :: Int)
  shifting <- newContext
  let growing = scrollCol 100 $ do
        n <- liftIO (readIORef countRef)
        scope (replicateM_ n bar)
        replicateM 2 (fst <$> sensorWith fillW bar)
  settle shifting inp growing 3
  ((_, before), follow1) <- step shifting inp growing
  writeIORef countRef 3
  (_, follow2) <- step shifting inp growing
  ((_, after), follow3) <- step shifting inp growing
  expectVis failed [on, on, wentOut, wentOut] (before ++ after)
  assertEq failed [False, True, False] [follow1, follow2, follow3]

-- With a delay, a sensor reports visible only after staying in view that long,
-- woken rather than by follow-up frames; hiding is immediate.
runDelayTest :: Context -> IORef Int -> IO ()
runDelayTest ctx failed = do
  let cfg = defaultSensorConfig {sensorDelay = 0.03}
      ui = scrollCol 100 (replicateM_ 10 bar *> (fst <$> sensorConfigured cfg bar) <* replicateM_ 9 bar)
      report = do
        ((_, v), _) <- step ctx inp ui
        pure (visVisible v, visEvent v)
  (sid, _) <- evalUi ctx inp ui
  settle ctx inp ui 2
  -- Row 10 is in view, but not yet for long enough.
  setScrollOffset ctx sid 380
  settle ctx inp ui 2
  report >>= assertEq failed off
  assert failed . (> 0) =<< getWakeAt ctx
  threadDelay 50000
  -- The woken frame measures it visible; the next frame reports it.
  _ <- step ctx inp ui
  report >>= assertEq failed cameIn
  setScrollOffset ctx sid 0
  _ <- step ctx inp ui
  report >>= assertEq failed wentOut
  assertEq failed 0 =<< getWakeAt ctx
