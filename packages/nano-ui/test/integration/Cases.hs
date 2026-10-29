module Cases (tests) where

import Spec
import Control.Concurrent (threadDelay)
import GHC.Clock (getMonotonicTime)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Data.Word (Word64)
import Effectful (Eff, IOE, runEff)
import Effectful.State.Static.Local (State, evalState, get, modify)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Monad (localInput)
import NanoUI.Effectful (Ui, embedNanoUI, runFrameEff, withRunInNanoUI)
import NanoUI.Internal.Layout.Arena
  ( arenaArrays
  , TagCol (..)
  , TreeCol (..)
  , writeTagEnum
  , writeTree
  )
import NanoUI.Internal.Widgets.SplitPane
  ( PaneDrop (..)
  , DropPreview (..)
  , dropPreviewTreeSized
  , dropTargetForPane
  , layoutNode
  , reflowFixed
  , topLevelDropTarget
  )
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "embed-state" runEmbedStateTest
  , spec "host-slot" runHostSlotTest
  , spec "layout-flow" runLayoutFlowTest
  , spec "flex-sizing" runFlexSizingTest
  , pixelSpec "label-align-end" runLabelAlignEndTest
  , spec "empty-frame" runEmptyFrameTest
  , pixelSpec "basic-widgets" runBasicWidgetsTest
  , spec "search-input-clear" runSearchInputClearTest
  , spec "search-input-debounce" runSearchInputDebounceTest
  , spec "pane-grid-mixed-drag" runPaneGridMixedDragTest
  , spec "pane-grid-clipped-control" runPaneGridClippedControlTest
  , spec "pane-grid-drop-preview" runPaneGridDropPreviewTest
  , spec "pane-grid-pinned-pane" runPaneGridPinnedPaneTest
  ]

-- | An Effectful view embeds NanoUI, and a body run in a NanoUI scope keeps
-- the effect's state and sees that scope's input.
runEmbedStateTest :: Context -> IORef Int -> IO ()
runEmbedStateTest ctx failed = do
  let ui :: Eff '[Ui, State Int, IOE] (Int, Float)
      ui = do
        _ <- embedNanoUI (column (pure ()))
        modify (+ (1 :: Int))
        w <- withRunInNanoUI $ \run -> column $ do
          run (modify (+ (1 :: Int)))
          inp <- askInput
          localInput inp {inputWindowSize = Size 123 45} (run (embedNanoUI windowWidth))
        (,w) <$> get
  ((n, w), _, _) <- runFrameEff (runEff . evalState (0 :: Int)) ctx (withInput 80 80) ui
  assertEq failed (2, 123) (n, w)

-- | Hosts hand the view what the app set, each of its own type, and a compact
-- host hands back the whole value.
runHostSlotTest :: Context -> IORef Int -> IO ()
runHostSlotTest ctx failed = do
  stringHost <- newHost
  intHost <- newHost
  otherHost <- newHost
  compactOwner <- newHost
  let ask :: Host a -> IO (Maybe a)
      ask h = evalUi ctx (withInput 80 80) (column (pure ()) >> askHost h)
  assertEq failed Nothing =<< ask stringHost
  setHost stringHost ("ok" :: String)
  setHost intHost (1 :: Int)
  setHost otherHost ("independent" :: String)
  assertEq failed (Just "ok") =<< ask stringHost
  assertEq failed (Just 1) =<< ask intHost
  assertEq failed (Just "independent") =<< ask otherHost
  _ <- compactHost compactOwner ([0 .. 9999] :: [Int])
  got <- evalUi ctx (withInput 80 80) (column (pure ()) >> askCompact compactOwner)
  assertEq failed (Just (10000, 9999)) ((\xs -> (length xs, last xs)) <$> got)


-- | Grow, percent, shrink and aspect sizing, 12 px a character. Locking the
-- largest grow child must lock the middle one on a later sweep (one sweep
-- gives it 55), and percent children give a gap's overflow back.
runFlexSizingTest :: Context -> IORef Int -> IO ()
runFlexSizingTest ctx failed = forM_ cases $ \(w, h, ui, size, want) -> do
  ids <- warmup2 ctx (withInput w h) ui
  got <- mapM (fmap (maybe [] size) . getPrevRect ctx) ids
  let near a b = abs (a - b) <= 0.5
  unless (map length got == map length want && and (zipWith near (concat got) (concat want))) $
    assertEq failed want got
  where
    sized l txt = respId <$> labelWith' (l . tight) txt
    filling l = sized (fillW . l)
    shrink l = l {layoutWidth = Shrink 1}
    rowOf w = rowWith (fixedW w . tight . gap 0) . sequence
    widths = pure . rectW
    sizes r = [rectW r, rectH r]
    -- Spacers keep a non-zero width: prev-rect tracking skips zero-area rects.
    growSpacer = currentId <* spacer (Fixed 10) (Grow 1)
    halves = rowWith (fixedW 206 . tight . gap 6) (sequence [sized (percent 50) "A", sized (percent 50) "B"])
    cases :: [(Float, Float, NanoUI [WidgetId], Rect -> [Float], [[Float]])]
    cases =
      [ (210, 40, rowOf 210 [filling id "A", filling id "AAAAA"], widths, [[105], [105]])
      , (200, 40, rowOf 200 [filling id "A", filling id (T.replicate 15 "A")], widths, [[20], [180]])
      , (240, 40, rowOf 240 (map (`filling` "A") [minW 12, minW 60, minW 130]), widths, [[50], [60], [130]])
      , (60, 200, columnWith (fixedH 200 . tight . gap 0) (sequence [growSpacer, growSpacer]), pure . rectH, [[100], [100]])
      , (200, 80, rowOf 200 [sized (percent 25) "A", sized (percent 75) "B"], widths, [[50], [150]])
      , (300, 80, halves, \r -> [rectX r, rectW r], [[0, 100], [106, 100]])
      , ( 400
        , 200
        , rowOf 100 [sized shrink "abcdefghijkl", spacer (Grow 1) (Fixed 5) >> respId <$> buttonWith' tight "X"]
        , \r -> [rectX r + rectW r]
        , [[64], [100]]
        )
      , (400, 200, rowOf 100 [sized shrink "abcdefghijkl" <* labelWith (shrink . tight) "ab"], widths, [[100]])
      , (320, 240, columnWith (fixedW 160 . tight) (sequence [sized (fixedAspectW 160 2) "X"]), sizes, [[160, 80]])
      ]

-- | An end-aligned label's text ends at the label's right edge.
runLabelAlignEndTest :: Context -> IORef Int -> IO ()
runLabelAlignEndTest ctx failed = do
  let tw = fmAdvance (ctxFontMetrics ctx) ' ' * 2
      ui = rowWith (fixedW (tw + 4) . tight . gap 0) (labelWith' (fillW . alignEnd . tight) "ab")
  lab <- warmup2 ctx emptyInput {inputWindowSize = Size (tw + 12) 8} ui
  assertJustM failed (spanRect "ab" <$> collectTextSpans ctx) $ \(Rect x _ w _) -> do
    let Rect bx _ bw _ = respRect lab
    assert failed (abs (x + w - (bx + bw)) <= 0.6 && abs (w - tw) <= 0.6)
  checkLabelAlignEndInk failed

-- | Nesting past the level array's first capacity lays out, a responsive row
-- stacks only below its breakpoint, and a multi-line kv value pushes the next
-- row down.
runLayoutFlowTest :: Context -> IORef Int -> IO ()
runLayoutFlowTest ctx failed = do
  let nest :: Int -> NanoUI ()
      nest 0 = label "deep"
      nest k = column (nest (k - 1))
      stacking = responsiveRowCol 720 (tight . gap 8 . fillW) $ do
        columnWith (tight . gap 8 . fillW) (card (label "LeftTop") >> card (label "LeftBot"))
        card (label "Right")
      kvs = column . card $ kv "Notes" "Line 1\nLine 2\nLine 3\nLine 4\nLine 5" >> kv "Tree" "0"
      above a b = join ((\ra rb -> checkGt (rectY rb) (rectY ra + 1)) <$> rectOf a <*> rectOf b)
  runSim ctx failed (withInput 300 300) (nest 320) (expect "deep")
  responsiveCtx <- newPixelContext
  runSim responsiveCtx failed (withInput 520 800) stacking $ do
    above "LeftBot" "Right"
    _ <- resize 1200 800
    right <- rectOf "Right"
    leftTop <- rectOf "LeftTop"
    checkGt (rectX right) (rectX leftTop + 1)
  kvCtx <- newPixelContext
  runSim kvCtx failed (withInput 320 400) kvs (above "Line 5" "Tree")

-- | A frame with no widgets runs input, redraw and cursor queries on the
-- empty arena, not on node 0, which starts out as its own child: reading it
-- recursed without end.
runEmptyFrameTest :: Context -> IORef Int -> IO ()
runEmptyFrameTest ctx failed = do
  arrays <- arenaArrays (ctxNodeArena ctx)
  writeTagEnum arrays 0 TagNodeType NodeContainer
  writeTree arrays 0 TreeFirstChild 0
  writeTree arrays 0 TreeNextSibling (-1)
  let inp0 = (withInput 320 200) {inputMousePos = V2 40 40}
  _ <- runFrame ctx inp0 (pure ())
  _ <- runFrame ctx (applyMouseButton MouseLeft True inp0) {inputScroll = V2 0 1} (pure ())
  _ <- needsRedraw ctx inp0 (inp0 {inputMousePos = V2 60 60})
  _ <- uiCursorKind ctx inp0
  wid <- warmup2 ctx inp0 (row (currentId <* image (fixedWH 40 24) (ImageId 0)))
  assertJustM failed (getPrevRect ctx wid) $ \(Rect _ _ w h) ->
    assert failed (abs (w - 40) <= 0.5 && abs (h - 24) <= 0.5)

-- | Pointer cursor over a button and a (pressed) checkbox only. A checkbox
-- shows its Var's value before any click stores state, and each click at
-- once. A slider in a filling column spans it to its maximum.
runBasicWidgetsTest :: Context -> IORef Int -> IO ()
runBasicWidgetsTest ctx failed = do
  checked <- newVar True
  level <- newVar 0
  let ui = columnWith fillW ((,,) <$> button' "Click" <*> checkbox' "Opt" checked <*> slider' 0 100 level)
      wantsPointer = liftIO . pointerCursorWanted ctx =<< getInput
      showsChecked cb on = do
        checkEq on =<< liftIO (readVarIO checked)
        checkEq (Just (if on then 1 else 0)) =<< liftIO (widgetValue ctx (respId cb))
  runSim ctx failed (withInput 400 200) ui $ do
    (btn, cb, sl) <- result
    showsChecked cb True
    check =<< (moveTo (On btn) >> wantsPointer)
    check . not =<< (moveTo (At (V2 (-1) (-1))) >> wantsPointer)
    check =<< (moveTo (On cb) >> wantsPointer)
    check =<< (press (On cb) >> wantsPointer)
    _ <- release
    showsChecked cb False
    _ <- frame
    showsChecked cb False
    let Rect cx cy _ _ = respRect cb
    _ <- click (At (V2 (cx + 1) (cy + 0.5)))
    showsChecked cb True
    let Rect sx sy sw sh = respRect sl
        Rect tx ty tw th = sliderTrackBounds sx sy sw sh
    checkGt sw 300
    _ <- press (At (V2 (tx + tw - 2) (ty + th / 2)))
    liftIO (readVarIO level) >>= \v -> checkGt v 90

-- | The embedded clear button empties the field, keeps focus, and reports the
-- change at once rather than after the debounce.
runSearchInputClearTest :: Context -> IORef Int -> IO ()
runSearchInputClearTest ctx failed = do
  query <- newVar "hello world"
  runSim ctx failed (withInput 320 100) (column (searchInput' "Search…" query)) $ do
    Rect bx by bw bh <- respRect <$> result
    pressTab 1
    -- The clear button is where the field shows the pointer cursor.
    let cy = by + bh / 2
        findClear x
          | x < bx = pure Nothing
          | otherwise = do
              _ <- moveTo (At (V2 x cy))
              kind <- liftIO . uiCursorKind ctx =<< getInput
              if kind == UiCursorPointer then pure (Just x) else findClear (x - 2)
    findClear (bx + bw - 6) >>= \m -> checkJust m $ \x -> do
      _ <- press (At (V2 x cy))
      cleared <- release
      checkEq "" =<< liftIO (readVarIO query)
      check (respChanged cleared)
      checkEq (respId cleared) =<< liftIO (getFocusId ctx)
      check . not . respChanged =<< frame

-- | A search field commits caller-set text after one pause (with no edit time
-- it once never did, waking every period), and typing on a wake 40 ms after
-- the last edit.
runSearchInputDebounceTest :: Context -> IORef Int -> IO ()
runSearchInputDebounceTest ctx failed = do
  query <- newVar ""
  let ui = column (held query (searchInputConfigured' defaultSearchInputConfig {sicDebounceMs = 40}))
      wakeAt = liftIO (getWakeAt ctx)
      showing want changed (resp, txt) = checkEq (want, changed) (txt, respChanged resp)
      afterPause us = liftIO (threadDelay us) >> frame
  runSim ctx failed (withInput 320 100) ui $ do
    pressTab 1
    liftIO (writeVarIO query "recent")
    showing "recent" False =<< frame
    wakeAt >>= \t -> checkGt t 0
    showing "recent" True =<< afterPause 80000
    checkEq 0 =<< wakeAt
    liftIO (writeVarIO query "")
    _ <- afterPause 80000
    showing "a" False =<< typeText "a"
    before <- liftIO getMonotonicTime
    showing "ab" False =<< typeText "b"
    after <- liftIO getMonotonicTime
    -- The edit is stamped during its frame: the wake is 40 ms past a moment in it.
    wakeAt >>= \t -> check (t >= before + 0.04 && t <= after + 0.042)
    showing "ab" True =<< afterPause 80000
    checkEq 0 =<< wakeAt
    showing "ab" False =<< afterPause 50000

-- | Where 'seedMixedGrid' has got to.
data MixedSeed = SeedStart | SeedRight Word64 | SeedDone

-- | From every pane's view, grows a grid into one pane left, two right.
seedMixedGrid :: IORef MixedSeed -> Word64 -> PaneGridCtx -> NanoUI ()
seedMixedGrid ref pid pctx =
  liftIO (readIORef ref) >>= \case
    SeedStart -> pgcSplit pctx AxisV >>= liftIO . writeIORef ref . SeedRight
    SeedRight nb | pid == nb -> pgcSplit pctx AxisH >> liftIO (writeIORef ref SeedDone)
    _ -> pure ()

-- | A filling grid with 40px minimum panes and 4px dividers.
testGridConfig :: PaneGridConfig
testGridConfig = defaultPaneGridConfig {pgLayout = fillW . fillH, pgMinSize = 40, pgSpacing = 4}

recordPane :: IORef (IM.IntMap v) -> Word64 -> v -> NanoUI ()
recordPane ref pid v = liftIO (modifyIORef' ref (IM.insert (fromIntegral pid) v))

-- | Drag the root divider of a 600 by 400 grid from its middle to x = 120.
dragDividerTo120 :: Context -> NanoUI a -> IO ()
dragDividerTo120 ctx ui = runDragFrom ctx inp ui (V2 300 200) (V2 120 200) >> void (runFrame ctx (releaseAt hold) ui)
  where
    inp = withInput 600 400
    hold = holdAt inp (V2 120 200)

-- | Drop previews simulate the post-drop layout (lifting a pane re-flows a
-- mixed-axis grid) and size-preserving drops keep the pane's extent. Live, a
-- drop lands with every pane's state, outside the grid cancels, and a drag
-- from a close button lifts nothing.
runPaneGridMixedDragTest :: Context -> IORef Int -> IO ()
runPaneGridMixedDragTest ctx failed = do
  let minSize = 40
      gutter = 4
      base = Rect 0 0 600 400
      lay t = fst (layoutNode minSize gutter t base)
      tree0 = Split 100 AxisV 0.5 (Pane 1) (Split 101 AxisH 0.5 (Pane 2) (Pane 3))
      r2 = lay tree0 M.! 2
      r3 = lay tree0 M.! 3
      preview dt = dpRect <$> dropPreviewTreeSized Nothing minSize gutter tree0 1 0 base dt
      within (Rect x y w h) fx fy = V2 (x + w * fx) (y + h * fy)
  assertEq failed (M.fromList [(1, Rect 0 0 298 400), (2, Rect 302 0 298 198), (3, Rect 302 202 298 198)]) (lay tree0)
  -- With pane 1 lifted the right branch fills the grid, so an edge drop on
  -- pane 3 lands across its foot, not in half its old rect (302 301 298 99).
  forM_
    [ (dropTargetForPane r3 (within r3 0.5 0.9) 3, DropSplit 3 AxisH False, Rect 0 303 600 97)
    , (dropTargetForPane r2 (within r2 0.9 0.5) 2, DropSplit 2 AxisV False, r2)
    , (dropTargetForPane r2 (spanCenter r2) 2, DropSwap 2, r2)
    , (DropTop AxisV True, DropTop AxisV True, Rect 0 0 298 400)
    ]
    $ \(dt, want, rect) -> assertEq failed (want, Just rect) (dt, preview dt)
  assertEq failed (Just (DropTop AxisV True)) (topLevelDropTarget 20 base (V2 5 200))
  assertEq failed Nothing (topLevelDropTarget 20 base (V2 300 200))
  let sizedTree = Split 100 AxisV 0.25 (Pane 1) (Split 101 AxisH 0.3 (Pane 2) (Pane 3))
      s1 = lay sizedTree M.! 1
      s2 = lay sizedTree M.! 2
      sized src p = dropPreviewTreeSized (Just src) minSize gutter sizedTree p 102 base
  forM_ [True, False] $ \onA -> do
    let both ax = [DropTop ax onA, DropSplit 3 ax onA]
    forM_ (both AxisV) $ \dt -> assertJust failed (sized s1 1 dt) $ \dp ->
      assertEq failed (Just (dpRect dp)) (M.lookup 1 (lay (dpTree dp)))
    -- Extents carry across axes, within the destination's minimum.
    forM_
      ( [(s1, 1, dt, rectW, rectW s1) | dt <- both AxisV]
          <> [(s1, 1, dt, rectH, rectW s1) | dt <- both AxisH]
          <> [(s2, 2, DropTop AxisH onA, rectH, rectH s2)]
          <> [(s2, 2, dt, rectW, rectH s2) | dt <- both AxisV]
          <> [(Rect 0 0 500 400, 1, DropTop AxisH onA, rectH, 312)]
      )
      $ \(src, p, dt, size, want) ->
        assertJust failed (sized src p dt) $ \dp -> assert failed (abs (size (dpRect dp) - want) < 0.01)
  assertEq failed (Just s2) (dpRect <$> sized s1 1 (DropSwap 2))
  rects <- newIORef IM.empty
  closeRects <- newIORef IM.empty
  stateUpdates <- newIORef IM.empty
  paneStates <- newIORef IM.empty
  seed <- newIORef SeedStart
  let inp0 = withInput 600 400
      ui =
        paneGrid
          testGridConfig
            { pgViewPane = \pid pctx -> do
                recordPane rects pid (pgcRect pctx)
                valueVar <- useVar (0 :: Int)
                value <- readVar valueVar
                marker <- nextId
                pending <- liftIO (IM.lookup (fromIntegral pid) <$> readIORef stateUpdates)
                forM_ pending $ \n -> do
                  writeVar valueVar n
                  liftIO (modifyIORef' stateUpdates (IM.delete (fromIntegral pid)))
                recordPane paneStates pid (marker, value)
                seedMixedGrid seed pid pctx
                close <- button' "x"
                recordPane closeRects pid (respRect close)
                when (respClicked close) (pgcClose pctx)
                pure (PaneView "P" True)
            }
      rectsAt i = writeIORef rects IM.empty >> runFrame ctx i ui >> readIORef rects
      key = fromIntegral :: Word64 -> Int
  _ <- warmup2 ctx inp0 ui
  _ <- runFrame ctx inp0 ui
  [pa, pb, pc] <- pgrPanes <$> warmup2 ctx inp0 ui
  let expected = IM.fromList [(key p, key p + 100) | p <- [pa, pb, pc]]
  writeIORef stateUpdates expected
  _ <- warmup2 ctx inp0 ui
  initialStates <- readIORef paneStates
  assertEq failed expected (IM.map snd initialStates)
  rs <- readIORef rects
  Just ra <- pure (IM.lookup (key pa) rs)
  Just rc <- pure (IM.lookup (key pc) rs)
  let ps = IM.elems rs
      gx = minimum (map rectX ps)
      gy = minimum (map rectY ps)
      gw = maximum (map (\r -> rectX r + rectW r) ps) - gx
      gh = maximum (map (\r -> rectY r + rectH r) ps) - gy
      -- In the header, 2px from the divider, whose leeway must not steal it.
      grab = V2 (rectX ra + rectW ra - 2) (rectY ra + 12)
      -- In pane 3's bottom zone, clear of the 20px top-level band, at the
      -- grid's middle: with pane 1 lifted, pane 3 spans the grid.
      dest = V2 (gx + gw / 2) (rectY rc + rectH rc - 30)
      grabbed = pressAt inp0 grab
      hold = holdAt inp0 dest
      dropped = releaseAt hold
  -- The order checked below is the same after a top-level band drop.
  assertEq failed Nothing (topLevelDropTarget 20 (Rect gx gy gw gh) dest)
  _ <- runFrame ctx grabbed ui
  writeIORef paneStates IM.empty
  during <- rectsAt hold
  assertEq failed (IM.delete (key pa) initialStates) =<< readIORef paneStates
  assertEq failed (IM.keys (IM.delete (key pa) rs)) (IM.keys during)
  assert failed (all ((== gw) . rectW) during)
  -- Back over the grab point the pane stays lifted and the drag no click.
  assert failed . not . IM.member (key pa) =<< rectsAt hold {inputMousePos = grab}
  (cancelled, _, _) <- runFrame ctx dropped {inputMousePos = V2 (-20) (-20)} ui
  assertEq failed [pa, pb, pc] (pgrPanes cancelled)
  writeIORef rects IM.empty
  _ <- warmup2 ctx inp0 ui
  assertEq failed rs =<< readIORef rects
  assertEq failed initialStates =<< readIORef paneStates
  mapM_ (\i -> runFrame ctx i ui) [grabbed, hold]
  (landed, _, _) <- runFrame ctx dropped ui
  assertEq failed [pb, pc, pa] (pgrPanes landed)
  _ <- warmup2 ctx inp0 ui
  assertEq failed initialStates =<< readIORef paneStates
  Just closePos <- fmap spanCenter . IM.lookup (key pa) <$> readIORef closeRects
  _ <- runFrame ctx (pressAt inp0 closePos) ui
  assertEq failed 3 . IM.size =<< rectsAt (holdAt inp0 (V2 (v2X closePos + 60) (v2Y closePos + 40)))
  (closed, _, _) <- runFrame ctx (releaseAt (holdAt inp0 closePos)) ui
  assertEq failed [pb, pc] (pgrPanes closed)

-- | Hovering a target lays the grid out as the drop will leave it, the
-- dragged pane's slot empty: over uneven splits a highlight on the pre-drop
-- panes lines up with nothing.
runPaneGridDropPreviewTest :: Context -> IORef Int -> IO ()
runPaneGridDropPreviewTest ctx failed = do
  -- Per pane: 'pgcRect', and the solved rect of a full-width strip inside it.
  seen <- newIORef IM.empty
  seed <- newIORef SeedStart
  let inp0 = withInput 600 400
      ui =
        paneGrid
          testGridConfig
            { pgViewPane = \pid pctx -> do
                strip <- labelWith' (fixedH 20 . fillW . tight) "H"
                recordPane seen pid (pgcRect pctx, respRect strip)
                seedMixedGrid seed pid pctx
                pure (PaneView "P" True)
            }
      -- The layout once @inp@ has settled (the strip's rect lags a frame).
      layoutAt inp = do
        _ <- warmup2 ctx inp ui
        writeIORef seen IM.empty
        _ <- runFrame ctx inp ui
        readIORef seen
      -- Lift pane @p@, park it outside the grid, hover at @dest@ of that
      -- lifted layout, and release: the layouts lifted, hovering and after.
      dragPane p dest = do
        Just (Rect px py _ _, _) <- IM.lookup p <$> layoutAt inp0
        _ <- runFrame ctx (pressAt inp0 (V2 (px + 10) (py + 10))) ui
        lifted <- layoutAt (holdAt inp0 (V2 (-50) (-50)))
        during <- layoutAt (holdAt inp0 (dest lifted))
        _ <- runFrame ctx (releaseAt (holdAt inp0 (dest lifted))) ui
        (lifted, during,) <$> layoutAt inp0
      at q (fx, fy) lifted = case IM.lookup q lifted of
        Just (Rect x y w h, _) -> V2 (x + w * fx) (y + h * fy)
        Nothing -> V2 (-50) (-50)
      -- Whole pixels: a dragged ratio leaves float dust (112.00001).
      wholePx (Rect x y w h) = Rect (r x) (r y) (r w) (r h) where r v = fromIntegral (round v :: Int)
      -- Nothing moves on release, and the dragged pane takes the empty slot.
      previewed p (lifted, during, after) = do
        assert failed (not (IM.member p lifted) && not (IM.member p during) && IM.member p after)
        assertEq failed during (IM.delete p after)
  replicateM_ 2 (layoutAt inp0)
  -- Pane 1 left, 3 and 5 right, uneven.
  dragDividerTo120 ctx ui
  uneven <- layoutAt inp0
  assertEq failed (IM.fromList [(1, Rect 0 0 112 400), (3, Rect 128 0 472 192), (5, Rect 128 208 472 192)]) $
    IM.map (wholePx . fst) uneven
  -- The model rects are the solver's: the strip spans its pane.
  let across r = (rectX (wholePx r), rectW (wholePx r))
  assert failed (all (\(m, s) -> across m == across s) uneven)
  -- A centre drop swaps: lifted, pane 3 spans the top row, and the hover
  -- already shows it in pane 1's narrow column.
  swap@(liftedS, duringS, _) <- dragPane 1 (at 3 (0.5, 0.5))
  assertEq failed (Just (Rect 0 0 600 192)) (wholePx . fst <$> IM.lookup 3 liftedS)
  assertEq failed (Just (Rect 0 0 112 400)) (wholePx . fst <$> IM.lookup 3 duringS)
  previewed 1 swap
  -- Edge drops show the target already halved.
  previewed 1 =<< dragPane 1 (at 5 (0.1, 0.5))
  previewed 5 =<< dragPane 5 (at 3 (0.5, 0.9))
  -- The grid's outer band squeezes the whole grid into the other half.
  top@(_, duringT, _) <- dragPane 3 (const (V2 590 200))
  assert failed (all (\(Rect x _ w _, _) -> x + w <= 300) duringT)
  previewed 3 top
  -- A gutter still targets the nearer pane, so the preview holds across it.
  gut@(liftedG, duringG, _) <- dragPane 1 $ \lifted ->
    let panes = map fst (IM.elems lifted)
        gutterPoints =
          [ pt
          | Rect x y w h <- panes
          , pt@(V2 px py) <- [V2 (x + w + 2) (y + h / 2), V2 (x + w / 2) (y + h + 2)]
          , px > 20 && px < 580 && py > 20 && py < 380
          , not (any (`rectContains` pt) panes)
          ]
     in case gutterPoints of
          pt : _ -> pt
          [] -> V2 (-50) (-50)
  assert failed (duringG /= liftedG)
  previewed 1 gut
  -- Outside the grid the release cancels and the uneven layout comes back
  -- (a top-level drop would re-split it 50/50).
  start <- layoutAt inp0
  (_, duringC, afterC) <- dragPane 1 (const (V2 (-20) 200))
  assert failed (not (IM.member 1 duringC))
  assertEq failed start afterC

-- | A pinned pane keeps its width from the first frame of a resize; unpinned,
-- both sides scale.
runPaneGridPinnedPaneTest :: Context -> IORef Int -> IO ()
runPaneGridPinnedPaneTest ctx failed = do
  rects <- newIORef IM.empty
  split <- newIORef False
  let ui pinned =
        paneGrid
          testGridConfig
            { pgFixedPanes = if pinned then (== 1) else const False
            , pgViewPane = \pid pctx -> do
                recordPane rects pid (pgcRect pctx)
                -- One split on the first frame: pane 1 left, pane 3 right.
                done <- liftIO (readIORef split)
                unless done (pgcSplit pctx AxisV >> liftIO (writeIORef split True))
                pure (PaneView "P" False)
            }
      -- @n@ frames at the width, then one more to report what they solved
      -- ('pgcRect' lags a frame, so @n = 1@ reads the first frame at a size).
      settleAt pinned w n = do
        replicateM_ n (runFrame ctx (withInput w 400) (ui pinned))
        writeIORef rects IM.empty
        _ <- runFrame ctx (withInput w 400) (ui pinned)
        readIORef rects
      inp0 = withInput 600 400
  replicateM_ 2 (warmup2 ctx inp0 (ui True))
  -- The 16px gutter leaves 584 to share.
  dragDividerTo120 ctx (ui True)
  forM_
    [ (True, 600, 2, [(1, 112), (3, 472)])
    , (True, 900, 1, [(1, 112)])
    , (True, 900, 3, [(1, 112), (3, 772)])
    , (True, 600, 3, [(1, 112), (3, 472)])
    , -- Unpinned it scales; pinned again, it keeps the width it was left at.
      (False, 900, 3, [(1, 169.5)])
    , (True, 600, 3, [(1, 169.5)])
    , -- With no room for the pin and the neighbour's minimum, the pin gives way.
      (True, 100, 3, [(1, 44), (3, 40)])
    ]
    $ \(pinned, w, n, want) -> do
      ms <- settleAt pinned w n
      forM_ want $ \(p, x) -> case IM.lookup p ms of
        Just r | abs (rectW r - x) <= 1 -> pure ()
        got -> assertEq failed (pinned, w, p, Just x) (pinned, w, p, rectW <$> got)
  -- The pure re-ratio on trees the widget case cannot reach: two pins at
  -- once, a pin under a split on the other axis, and a right-hand pin held
  -- through a drag growing the grid by a fraction a frame without walking.
  let lay t r = fst (layoutNode 40 16 t r)
      reflow fixed old new = reflowFixed (`elem` fixed) 40 16 old new
      area = Rect 0 0
      ends = Split 10 AxisV 0.25 (Pane 1) (Split 11 AxisV 0.5 (Pane 2) (Pane 3))
      stacked = Split 20 AxisH 0.5 (Split 21 AxisV 0.5 (Pane 1) (Pane 2)) (Pane 3)
      stacked' = reflow [1] (area 600 400) (area 900 600) stacked
      rightPin = Split 30 AxisV 0.5 (Pane 1) (Pane 2)
      at k = area (600 + k * 2 / 3) 400
  forM_
    [ (ends, area 600 400, rectW, [(1, 146), (3, 211)])
    , (reflow [1, 3] (area 600 400) (area 900 400) ends, area 900 400, rectW, [(1, 146), (3, 211), (2, 511)])
    , (stacked, area 600 400, rectW, [(1, 292)])
    , (stacked', area 900 600, rectW, [(1, 292), (2, 592)])
    , (stacked', area 900 600, rectH, [(1, 292), (3, 292)])
    , (rightPin, at 0, rectW, [(2, 292)])
    , (foldl (\t k -> reflow [2] (at k) (at (k + 1)) t) rightPin [0 .. 29 :: Float], at 30, rectW, [(2, 292)])
    ]
    $ \(t, r, size, want) -> forM_ want $ \(p, x) -> case M.lookup p (lay t r) of
      Just g | abs (size g - x) <= 0.01 -> pure ()
      got -> assertEq failed (r, p, Just x) (r, p, size <$> got)

-- | A button scrolled above its viewport can overlap the header, but its
-- hidden rect must not claim the header's drag press.
runPaneGridClippedControlTest :: Context -> IORef Int -> IO ()
runPaneGridClippedControlTest ctx failed = do
  rendered <- newIORef False
  geometry <- newIORef Nothing
  let inp0 = withInput 300 240
      ui =
        paneGrid
          defaultPaneGridConfig
            { pgLayout = fillW . fillH
            , pgViewPane = \_ _ -> do
                liftIO (writeIORef rendered True)
                header <- labelWith' (fixedH 40 . fillW . tight) "Header"
                (sid, target) <- scrollArea (fixedH 120 . fillW . tight) . columnWith tight $
                  button' "Scrolled control" <* replicateM_ 20 (label "Scroll content")
                liftIO (writeIORef geometry (Just (respId header, sid, respId target)))
                pure (PaneView "Panel" True)
            }
  _ <- warmup2 ctx inp0 ui
  Just (headerId, sid, targetId) <- readIORef geometry
  Just hr <- getPrevRect ctx headerId
  Just br <- getPrevRect ctx targetId
  -- Scroll the button's centre to the header's.
  setScrollOffset ctx sid (rectY br + rectH br / 2 - (rectY hr + rectH hr / 2))
  _ <- warmup2 ctx inp0 ui
  Just grab <- fmap spanCenter <$> getPrevRect ctx targetId
  assert failed (rectContains hr grab)
  _ <- runFrame ctx (pressAt inp0 grab) ui
  writeIORef rendered False
  _ <- runFrame ctx (holdAt inp0 (V2 (v2X grab + 30) (v2Y grab))) ui
  assert failed . not =<< readIORef rendered
