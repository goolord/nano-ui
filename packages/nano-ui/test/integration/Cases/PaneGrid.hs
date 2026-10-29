{-# OPTIONS_GHC -O0 #-}
-- | The pane grid: the split tree it reports and adopts, when an
-- arrangement settles, padding, pane sizes, drag handles and dividers.
module Cases.PaneGrid (tests) where

import Control.Exception (IOException, try)
import Data.Either (isLeft)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as M
import Data.Maybe (isJust, listToMaybe)
import Data.Word (Word64)
import NanoUI.Internal.Widgets.SplitPane
  ( DropPreview (..)
  , PaneDrop (..)
  , dropPreviewTreeSized
  , dropTargetForPane
  , layoutNode
  , reflowFixed
  , topLevelDropTarget
  )
import NanoUI.Testing.Sim
import Spec

tests :: [Spec]
tests =
  [ spec "pane-grid-layout" runLayoutTest
  , spec "pane-grid-divider" runDividerTest
  , spec "pane-grid-controlled" runControlledTest
  , spec "pane-grid-pane-drag" runPaneDragTest
  , spec "pane-grid-drop" runDropTest
  , spec "pane-grid-tab-drop" runTabDropTest
  , spec "pane-grid-clip" runClipTest
  , simSpec "pane-grid-escape" 600 400 ((,) <$> paneGrid halvesGrid <*> takeEscape) escapeRestores
  , spec "pane-grid-empty" runEmptyTest
  , spec "pane-grid-validation" runValidationTest
  , spec "pane-grid-mixed-drag" runMixedDragTest
  , spec "pane-grid-drop-preview" runDropPreviewTest
  , spec "pane-grid-pinned-pane" runPinnedPaneTest
  , spec "pane-grid-clipped-control" runClippedControlTest
  ]

-- | Two panes side by side in a 600 by 400 grid: a 16px gutter from 292 to
-- 308, down the middle.
halves :: GridNode
halves = Split 30 AxisV 0.5 (Pane 10) (Pane 20)

-- | 'halves' filling the window.
halvesGrid :: PaneGridConfig
halvesGrid = defaultPaneGridConfig {pgLayout = fillW . fillH, pgTree = InitialTree (Just halves)}

-- | The left pane at its 40px minimum, its ratio below it; the gutter runs
-- from 40 to 56.
narrowLeft :: GridNode
narrowLeft = Split 30 AxisV 0 (Pane 10) (Pane 20)

-- | A fresh context running @script@ against @ui@ in a 600 by 400 window.
inGrid :: IORef Int -> NanoUI a -> Sim a r -> IO r
inGrid failed ui script = newContext >>= \ctx -> runSim ctx failed (withInput 600 400) ui script

-- | A pane view that, once @flag@ is set, runs @act@ in pane @target@.
actOnce :: IORef Bool -> Word64 -> (PaneGridCtx -> NanoUI ()) -> Word64 -> PaneGridCtx -> NanoUI PaneView
actOnce flag target act pid pctx = do
  wanted <- liftIO (readIORef flag)
  when (wanted && pid == target) (liftIO (writeIORef flag False) >> act pctx)
  pure (PaneView "P" False)

-- | A tree the caller holds fixed.
controlled :: Maybe GridNode -> PaneTree
controlled = ControlledTree . constVar

splitH :: PaneGridCtx -> NanoUI ()
splitH pctx = void (pgcSplit pctx AxisH)

-- | One pane left, two stacked right.
mixed :: GridNode
mixed = Split 2 AxisV 0.5 (Pane 1) (Split 4 AxisH 0.5 (Pane 3) (Pane 5))

-- | Record @v@ as pane @pid@'s.
note :: IORef (IM.IntMap v) -> Word64 -> v -> NanoUI ()
note ref pid v = liftIO (modifyIORef' ref (IM.insert (fromIntegral pid) v))

-- | What the panes record in @ref@ while @act@ runs.
recording :: IORef (IM.IntMap v) -> Sim a b -> Sim a (IM.IntMap v)
recording ref act = liftIO (writeIORef ref IM.empty) >> act >> liftIO (readIORef ref)

-- | The ratio of the root split.
rootRatio :: Maybe GridNode -> Maybe Float
rootRatio = \case
  Just (Split _ _ r _ _) -> Just r
  _ -> Nothing

-- | Press at @from@, nudge past the drag threshold, hold at @to@ for two
-- frames and release there; the frame after.
dragPane :: V2 -> V2 -> Sim a a
dragPane from@(V2 x y) to = do
  _ <- press (At from)
  mapM_ moveTo [At (V2 (x + 20) y), At to, At to]
  _ <- release
  frame

-- | A grid, the rect each pane's body was laid out at, and the rect each
-- pane was told it has ('pgcRect').
probeGrid ::
  (PaneGridConfig -> PaneGridConfig)
  -> (Word64 -> NanoUI ())
  -> IO (NanoUI PaneGridResponse, IORef (IM.IntMap Rect), IORef (IM.IntMap Rect))
probeGrid f body = do
  laid <- newIORef IM.empty
  told <- newIORef IM.empty
  let cfg =
        f
          halvesGrid
            { pgSpacing = 4
            , pgLeeway = 6
            , pgViewPane = \pid pctx -> do
                note told pid (pgcRect pctx)
                (_, area) <- mouseArea (fillW . fillH) (body pid)
                note laid pid (respRect area)
                pure (PaneView "P" False)
            }
  pure (paneGrid cfg, laid, told)

-- | 'pgLayout' padding insets the panes, content wider than a slot keeps both
-- slots, 'pgcRect' is where a pane is laid out, and 'pgDividerColor' fills
-- the gutter.
runLayoutTest :: Context -> IORef Int -> IO ()
runLayoutTest _ failed = do
  let wide pid = when (pid == 10) . void . panelWith (fillW . fillH) . rowWith fillW $ do
        labelWith fillW "A title that runs on"
        box (minW 900 . minH 900 . fillW) (colorRGBA 255 0 0 255)
      split axis c = c {pgTree = InitialTree (Just (Split 30 axis 0.5 (Pane 10) (Pane 20)))}
  -- Padded: 560 inside the padding, less a 16px gutter, halved.
      padded c = c {pgLayout = fillW . fillH . padAll 20}
  forM_
    [ (padded, const (pure ()), Rect 20 20 272 360, Rect 308 20 272 360)
    , (split AxisV, wide, Rect 0 0 292 400, Rect 308 0 292 400)
    , (split AxisH, wide, Rect 0 0 600 192, Rect 0 208 600 192)
    ]
    $ \(f, body, first, second) -> do
      ctx <- newContext
      (ui, laid, told) <- probeGrid f body
      replicateM_ 4 (runFrame ctx (withInput 600 400) ui)
      l <- readIORef laid
      assertEq failed (Just first, Just second) (IM.lookup 10 l, IM.lookup 20 l)
      assertEq failed l =<< readIORef told
  let c = colorRGBA 10 20 30 255
  ctx <- newContext
  (_, dd) <- warmupDraw ctx (withInput 600 400) (paneGrid halvesGrid {pgDividerColor = Just c})
  assert failed . elem (Rect 292 0 16 400, c) =<< drawQuads dd

-- | A divider drag sets 'pgrChanged' while it moves and 'pgrCommitted' on its
-- release (a split sets both, a still press neither), repainting both panes.
-- A divider at a minimum its ratio lies below follows from the first pixel,
-- and a drag released while the grid was not built is over.
runDividerTest :: Context -> IORef Int -> IO ()
runDividerTest _ failed = do
  splitNow <- newIORef False
  -- The flags of every pass of a frame: a grid that changes runs the view
  -- again, and an app acts on the pass that saw the change.
  seen <- newIORef (False, False)
  let labelled pid pctx = do
        _ <- actOnce splitNow 10 splitH pid pctx
        PaneView "P" False <$ label (if pid == 10 then "left" else "right")
      ui = do
        resp <- paneGrid halvesGrid {pgViewPane = labelled}
        liftIO (modifyIORef' seen (\(c, k) -> (c || pgrChanged resp, k || pgrCommitted resp)))
      flags act = liftIO (writeIORef seen (False, False)) >> act >> liftIO (readIORef seen)
  inGrid failed ui $ do
    moving <- mapM flags [press (at 300 200), moveTo (at 360 200)]
    -- The right pane moved from x 308 to 368, and the left one grew.
    check . (`damageCovers` Rect 0 0 600 400) =<< frameDamage
    right <- findSpan "right"
    checkJust right $ \r -> checkGt (rectX r) 350
    rest <- mapM flags [moveTo (at 360 200), release, frame, click (at 300 200), frame]
    splitting <- mapM flags [liftIO (writeIORef splitNow True) >> frame, frame]
    let none = (False, False)
    checkEq [none, (True, False), none, (False, True), none, none, none] (moving ++ rest)
    checkEq [(True, True), none] splitting
  -- The left pane ends 70px wide: its 40px minimum and the 30 moved.
  let narrow = paneGrid halvesGrid {pgTree = InitialTree (Just narrowLeft)}
  inGrid failed narrow $ do
    r <- drag (at 48 200) (at 78 200) 1
    checkJust (rootRatio (pgrTree r)) $ \ratio -> check (abs (ratio - 70 / 584) < 1.0e-4)
  -- A press elsewhere, still held when the grid is back, leaves the divider.
  shownRef <- newIORef True
  let showing = liftIO . writeIORef shownRef
      sometimes = do
        shown <- liftIO (readIORef shownRef)
        if shown then pgrTree <$> paneGrid halvesGrid else pure Nothing
  inGrid failed sometimes $ do
    _ <- press (at 300 200)
    dragged <- moveTo (at 350 200)
    check (dragged /= Just halves)
    showing False
    _ <- release
    _ <- press (at 100 100)
    showing True
    -- The first frame back lays the grid out; the next has its rect to drag in.
    replicateM_ 2 (checkEq dragged =<< moveTo (at 150 100))
    checkEq dragged =<< release

-- | A controlled tree passed back each frame keeps the grid, mid-drag too;
-- another replaces it; one not passed back is rejected, and a new
-- 'InitialTree' seed changes nothing. A tree in a Var resets from the view,
-- and written over a maximized pane shows it unmaximized, taking drops.
runControlledTest :: Context -> IORef Int -> IO ()
runControlledTest _ failed = do
  given <- newIORef (Just halves)
  let passedBack = do
        t <- liftIO (readIORef given)
        resp <- paneGrid halvesGrid {pgTree = ControlledTree (constVar t)}
        liftIO (writeIORef given (pgrTree resp))
        pure resp
      other = Split 5 AxisH 0.25 (Pane 1) (Pane 2)
      dragged = Just (0.5 + 100 / 584)
  inGrid failed passedBack $ do
    checkEq dragged . rootRatio . pgrTree =<< drag (at 300 200) (at 400 200) 2
    liftIO (writeIORef given (Just other))
    r <- frame
    checkEq (Just other, [1, 2]) (pgrTree r, pgrPanes r)
  edit <- newIORef False
  proposed <- newIORef Nothing
  let rejecting = do
        r <- paneGrid halvesGrid {pgTree = controlled (Just halves), pgViewPane = actOnce edit 10 splitH}
        when (pgrChanged r) (liftIO (writeIORef proposed (pgrTree r)))
        pure r
  ctx <- newContext
  runSim ctx failed (withInputOff 600 400) rejecting $ do
    liftIO (writeIORef edit True)
    _ <- frame
    change <- liftIO (readIORef proposed)
    check (change /= Nothing && change /= Just halves)
    checkEq (Just halves) . pgrTree =<< frame
  kept <- evalUi ctx (withInputOff 600 400) (paneGrid halvesGrid {pgTree = InitialTree (Just (Pane 90))})
  assertEq failed (Just halves) (pgrTree kept)
  resetNow <- newIORef False
  maximize <- newIORef False
  arrangement <- newVar (Just halves)
  let inVarCfg = halvesGrid {pgTree = ControlledTree arrangement, pgViewPane = actOnce maximize 20 pgcMaximize}
      inVar = do
        resp <- paneGrid inVarCfg
        wanted <- liftIO (readIORef resetNow)
        when wanted (liftIO (writeIORef resetNow False) >> writeVar arrangement (Just halves))
        pure resp
      raise ref = liftIO (writeIORef ref True) >> settle
  inGrid failed inVar $ do
    _ <- drag (at 300 200) (at 400 200) 1
    checkEq dragged . rootRatio . pgrTree =<< frame
    checkEq (Just 0.5) . rootRatio . pgrTree =<< raise resetNow
    checkEq 20 . pgrMaximizedPane =<< raise maximize
    writeV arrangement (Just (Split 5 AxisV 0.5 (Pane 1) (Pane 2)))
    _ <- moveTo (at 5 200)
    checkEq (Just (OutsideGrid PaneLeft)) . fmap pgdLocation . pgrDropTarget =<< settle
    writeV arrangement (Just halves)
    checkEq 0 . pgrMaximizedPane =<< settle

-- | A press on a 'paneDragHandle' drags its pane (onto the other's middle to
-- swap them) under a repainting indicator. Below the handle, on its button,
-- past its pane or under a scrim, a press drags nothing.
runPaneDragTest :: Context -> IORef Int -> IO ()
runPaneDragTest _ failed = do
  clicks <- newIORef (0 :: Int)
  -- The handle is the top 40px of each pane; its button is at its right end.
  let handled _ pctx = do
        paneDragHandle pctx (fillW . fixedH 40) $ do
          flex
          whenM (buttonWith (fixedWH 40 30) "x") (liftIO (modifyIORef' clicks (+ 1)))
        pure (PaneView "P" False)
  inGrid failed (paneGrid halvesGrid {pgViewPane = handled}) $ do
    panes <- mapM (\from -> pgrPanes <$> dragPane from (V2 454 200)) [V2 60 20, V2 60 200, V2 270 20]
    checkEq (replicate 3 [20, 10]) panes
    checkEq 0 =<< liftIO (readIORef clicks)
    checkEq UiCursorGrab =<< cursorAt (at 60 20)
  let overrun pid pctx = do
        when (pid == 10) $ paneDragHandle pctx (fixedWH 200 30) (pure ())
        pure (PaneView "P" False)
  inGrid failed (paneGrid halvesGrid {pgTree = InitialTree (Just narrowLeft), pgViewPane = overrun}) $
    checkEq (Just narrowLeft) . pgrTree =<< dragPane (V2 100 15) (V2 300 200)
  -- The indicator sits 12px right of and below the pointer, and repaints
  -- though the drop zone stays the same.
  let anywhere = paneGrid halvesGrid {pgViewPane = \_ _ -> pure (PaneView "P" True)}
  inGrid failed anywhere $ do
    _ <- press (at 100 200)
    mapM_ (\x -> moveTo (at x 200)) [300, 305, 310]
    check . (`damageCovers` Rect 317 212 117 28) =<< frameDamage
  let covered = columnWith (fillW . fillH) $ do
        r <- anywhere
        panelWith (pinAt 0 0 . pointer PointerBlock . fixedWH 600 400) (pure ())
        pure r
  inGrid failed covered $ forM_ [V2 300 200, V2 150 200] $ \from ->
    checkEq (Just halves) . pgrTree =<< dragPane from (V2 450 200)

-- | The pointer at the edge or beside a divider previews an external drop
-- (no extra pane rendered), elsewhere none. A drop commits once, focused, a
-- replaced grid's target is stale, and an overlay covers the drop zone.
runDropTest :: Context -> IORef Int -> IO ()
runDropTest _ failed = do
  rendered <- newIORef []
  let noting pid _ = PaneView "Pane" False <$ liftIO (modifyIORef' rendered (pid :))
      recorder = paneGrid halvesGrid {pgViewPane = noting}
      hover x = moveTo (at x 200)
  inGrid failed recorder $ do
    liftIO (writeIORef rendered [])
    preview <- hover 5
    checkEq (Just halves, False) (pgrTree preview, pgrChanged preview)
    checkEq [20, 10] =<< liftIO (readIORef rendered)
    checkJust (pgrDropTarget preview) $ \target -> do
      checkEq (OutsideGrid PaneLeft, 10) (pgdLocation target, pgdPane target)
      check (rectW (pgdRect target) > 0 && rectH (pgdRect target) == 400)
    checkEq (Just (BesidePane 10 PaneRight)) . fmap pgdLocation . pgrDropTarget =<< hover 280
    forM_ [-1, 150, 300] $ \x -> do
      r <- hover x
      checkEq (Just halves, Nothing) (pgrTree r, pgrDropTarget r)
  ctx <- newContext
  let edge = (withInput 600 400) {inputMousePos = V2 5 200}
      view = paneGrid halvesGrid
      committing = do
        r <- view
        commits <- forM (pgrDropTarget r) $ \target -> (,) <$> commitPaneDrop target <*> commitPaneDrop target
        pure (r, commits)
  _ <- warmup2 ctx edge view
  ((before, commits), _, dirty) <- runFrame ctx edge committing
  assert failed dirty
  assertEq failed (Just halves) (pgrTree before)
  after <- evalUi ctx edge view
  assertJust failed commits $ \(committed, repeated) -> do
    assertEq failed Nothing repeated
    assertJust failed committed $ \(pid, arrangement) ->
      assertEq failed (Just arrangement, pid, 3) (pgrTree after, pgrFocusedPane after, pgrPaneCount after)
  stale <- maybe (fail "missing target") pure (pgrDropTarget after)
  let replaced = paneGrid halvesGrid {pgTree = controlled (Just (Pane 90))}
  assertEq failed Nothing =<< evalUi ctx edge (replaced >> commitPaneDrop stale)
  covered <- newContext
  let overlaid = columnWith (fillW . fillH) $ do
        r <- paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH}
        void (buttonWith' (pinAt 0 150 . fixedWH 80 100) "Overlay")
        pure r
  r <- warmup covered edge overlaid >> warmup2 covered edge overlaid
  assertEq failed (1, Nothing) (pgrPaneCount r, pgrDropTarget r)

-- | A tab dragged onto the grid's edge previews without an uncommitted pane,
-- and its release, consumed in the frame rendering its source, commits one.
runTabDropTest :: Context -> IORef Int -> IO ()
runTabDropTest ctx failed = do
  dragHandle <- newDrag
  headers <- newIORef []
  let tabsPane pid _ = do
        r <- tabBar' (constVar (1 :: Int)) [tab 1 "Alpha" (), tab 2 "Beta" ()]
        liftIO (modifyIORef' headers (++ [((pid, k), h) | (k, h) <- tabHeaders r]))
        pure (PaneView "Editor" False)
      ui = do
        liftIO (writeIORef headers [])
        response <- paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH, pgViewPane = tabsPane}
        gesture <- useDrag dragHandle =<< liftIO (readIORef headers)
        committed <- case (gesture, pgrDropTarget response) of
          (Just d, Just target) | dragPhase d == DragReleased -> commitPaneDrop target
          _ -> pure Nothing
        pure (response, committed)
  runSim ctx failed (withInput 600 400) ui $ do
    _ <- press "Alpha"
    checkEq 1 . pgrPaneCount . fst =<< moveTo (at 5 200)
    check . isJust . snd =<< release
    checkEq 2 . pgrPaneCount . fst =<< frame

-- | Content is cut to its pane: a button or a scroller's bar past it takes
-- no click or grab there. A bar dragged in a pane that drags from anywhere
-- scrolls and leaves the pane.
runClipTest :: Context -> IORef Int -> IO ()
runClipTest ctx failed = do
  clicks <- newIORef (0 :: Int)
  ids <- newIORef IM.empty
  let tall = box (fixedWH 100 2000) (colorRGBA 200 0 0 255)
      body 10 = do
        (wid, _) <- scrollArea (fixedWH 450 300) tall
        hidden <- rowWith (tight . gap 0) $ do
          box (fixedWH 400 30) (colorRGBA 200 0 0 255)
          buttonWith' (fixedWH 60 30) "Hidden"
        when (respClicked hidden) (liftIO (modifyIORef' clicks (+ 1)))
        note ids 0 (respId hidden)
        pure wid
      body _ = fst <$> scrollArea (tight . grow) tall
      pane pid _ = body pid >>= note ids pid >> pure (PaneView "P" True)
      ui = paneGrid halvesGrid {pgViewPane = pane}
  runSim ctx failed (withInput 600 400) ui $ do
    known <- liftIO (readIORef ids)
    [hidden, Rect wx wy ww _, Rect px py pw _] <- mapM (prevRect . (known IM.!)) [0, 10, 20]
    -- Laid out past the pane, which ends at 292.
    check (rectX hidden >= 292)
    _ <- click (Within hidden)
    checkEq 0 =<< liftIO (readIORef clicks)
    checkGt (wx + ww) 320
    kinds <- mapM (\d -> cursorAt (at (wx + ww - d) (wy + 10))) [1 .. 16]
    check (UiCursorGrab `notElem` kinds)
    -- The other pane's thumb, at the top of the bar along its right edge.
    thumbs <- filterM (\d -> (== UiCursorGrab) <$> cursorAt (at (px + pw - d) (py + 10))) [1 .. 16]
    checkJust (listToMaybe thumbs) $ \d -> do
      _ <- press (at (px + pw - d) (py + 10))
      replicateM_ 2 (moveTo (at (px + pw - d) (py + 60)))
      checkEq (Just halves) . pgrTree =<< release
      flip checkGt 0 =<< scrollOffsetOf (known IM.! 20)

-- | A focused grid takes Escape only to restore a maximized pane; otherwise
-- the key goes on to a handler after it.
escapeRestores :: Sim (PaneGridResponse, Bool) ()
escapeRestores = do
  pressTab 1
  check . snd =<< pressKey KeyEscape
  _ <- pressKey (KeyChar 'm')
  check . (/= 0) . pgrMaximizedPane . fst =<< settle
  check . not . snd =<< pressKey KeyEscape
  checkEq 0 . pgrMaximizedPane . fst =<< settle

-- | A grid with no tree has no panes, a pane closed in it leaves none, and a
-- controlled tree brings one back or clears it.
runEmptyTest :: Context -> IORef Int -> IO ()
runEmptyTest ctx failed = do
  close <- newIORef False
  let inp = withInputOff 600 400
      withTree t = paneGrid halvesGrid {pgTree = t, pgViewPane = actOnce close 1 pgcClose}
      one = controlled (Just (Pane 1))
      none = InitialTree Nothing
  empty <- warmup2 ctx inp (withTree none)
  assertEq failed (Nothing, 0, []) (pgrTree empty, pgrPaneCount empty, pgrPanes empty)
  _ <- warmup2 ctx inp (withTree one)
  writeIORef close True
  _ <- evalUi ctx inp (withTree none)
  closed <- warmup2 ctx inp (withTree none)
  assertEq failed (Nothing, 0) (pgrTree closed, pgrPaneCount closed)
  assertEq failed [1] . pgrPanes =<< evalUi ctx inp (withTree one)
  assertEq failed [] . pgrPanes =<< evalUi ctx inp (withTree (controlled Nothing))

runValidationTest :: Context -> IORef Int -> IO ()
runValidationTest ctx failed = do
  assertEq failed (Right ()) (validateGridTree halves)
  let invalid =
        [ Pane 0
        , Pane (2 ^ (63 :: Int))
        , Split 1 AxisV 0.5 (Pane 1) (Pane 2)
        , Split 3 AxisV 0.5 (Pane 1) (Pane 1)
        , Split 3 AxisV (0 / 0) (Pane 1) (Pane 2)
        , Split 3 AxisV 1.1 (Pane 1) (Pane 2)
        ]
  forM_ invalid $ \arrangement -> do
    assert failed (isLeft (validateGridTree arrangement))
    outcome <- try @IOException $ runFrame ctx (withInput 600 400) $
      paneGrid defaultPaneGridConfig {pgTree = controlled (Just arrangement)}
    assert failed (isLeft outcome)

-- | Drop previews simulate the post-drop layout (lifting a pane re-flows a
-- mixed-axis grid) and size-preserving drops keep the pane's extent. Live, a
-- drop lands with every pane's state, outside the grid cancels, and a drag
-- from a close button lifts nothing.
runMixedDragTest :: Context -> IORef Int -> IO ()
runMixedDragTest ctx failed = do
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
  let ui =
        paneGrid
          halvesGrid
            { pgTree = InitialTree (Just mixed)
            , pgViewPane = \pid pctx -> do
                note rects pid (pgcRect pctx)
                valueVar <- useVar (0 :: Int)
                value <- readVar valueVar
                marker <- nextId
                pending <- liftIO (IM.lookup (fromIntegral pid) <$> readIORef stateUpdates)
                forM_ pending $ \n -> do
                  writeVar valueVar n
                  liftIO (modifyIORef' stateUpdates (IM.delete (fromIntegral pid)))
                note paneStates pid (marker, value)
                close <- button' "x"
                note closeRects pid (respRect close)
                when (respClicked close) (pgcClose pctx)
                pure (PaneView "P" True)
            }
      key = fromIntegral :: Word64 -> Int
      states = liftIO (readIORef paneStates)
  runSim ctx failed (withInput 600 400) ui $ do
    [pa, pb, pc] <- pgrPanes <$> result
    let expected = IM.fromList [(key p, key p + 100) | p <- [pa, pb, pc]]
    liftIO (writeIORef stateUpdates expected)
    rs <- recording rects settle
    initialStates <- states
    checkEq expected (IM.map snd initialStates)
    Just ra <- pure (IM.lookup (key pa) rs)
    Just rc <- pure (IM.lookup (key pc) rs)
    let ps = IM.elems rs
        gx = minimum (map rectX ps)
        gy = minimum (map rectY ps)
        gw = maximum (map (\r -> rectX r + rectW r) ps) - gx
        gh = maximum (map (\r -> rectY r + rectH r) ps) - gy
        -- In the header, 2px from the divider, whose leeway must not steal it.
        grab = At (V2 (rectX ra + rectW ra - 2) (rectY ra + 12))
        -- In pane 3's bottom zone, clear of the 20px top-level band, at the
        -- grid's middle: with pane 1 lifted, pane 3 spans the grid.
        dest = V2 (gx + gw / 2) (rectY rc + rectH rc - 30)
    -- The order checked below is the same after a top-level band drop.
    checkEq Nothing (topLevelDropTarget 20 (Rect gx gy gw gh) dest)
    _ <- press grab
    liftIO (writeIORef paneStates IM.empty)
    during <- recording rects (moveTo (At dest))
    checkEq (IM.delete (key pa) initialStates) =<< states
    checkEq (IM.keys (IM.delete (key pa) rs)) (IM.keys during)
    check (all ((== gw) . rectW) during)
    -- Back over the grab point the pane stays lifted and the drag no click.
    check . not . IM.member (key pa) =<< recording rects (moveTo grab)
    checkEq [pa, pb, pc] . pgrPanes =<< (moveTo (at (-20) (-20)) >> release)
    checkEq initialStates =<< (settle >> states)
    checkEq [pb, pc, pa] . pgrPanes =<< (press grab >> moveTo (At dest) >> release)
    checkEq initialStates =<< (settle >> states)
    Just closePos <- fmap spanCenter . IM.lookup (key pa) <$> liftIO (readIORef closeRects)
    _ <- press (At closePos)
    checkEq 3 . IM.size =<< recording rects (moveTo (At (v2Add closePos (V2 60 40))))
    checkEq [pb, pc] . pgrPanes =<< (moveTo (At closePos) >> release)

-- | Hovering a target lays the grid out as the drop will leave it, the
-- dragged pane's slot empty: over uneven splits a highlight on the pre-drop
-- panes lines up with nothing.
runDropPreviewTest :: Context -> IORef Int -> IO ()
runDropPreviewTest ctx failed = do
  -- Per pane: 'pgcRect', and the solved rect of a full-width strip inside it.
  seen <- newIORef IM.empty
  let ui =
        paneGrid
          halvesGrid
            { pgTree = InitialTree (Just mixed)
            , pgViewPane = \pid pctx -> do
                strip <- labelWith' (fixedH 20 . fillW . tight) "H"
                note seen pid (pgcRect pctx, respRect strip)
                pure (PaneView "P" True)
            }
      -- The layout once the input has settled (the strip's rect lags a frame).
      layout = settle >> recording seen frame
      -- Lift pane @p@, park it outside the grid, hover at @dest@ of that
      -- lifted layout, and release: the layouts lifted, hovering and after.
      lift p dest = do
        Just (Rect px py _ _, _) <- IM.lookup p <$> layout
        _ <- press (at (px + 10) (py + 10))
        lifted <- moveTo (at (-50) (-50)) >> layout
        during <- moveTo (At (dest lifted)) >> layout
        (lifted, during,) <$> (release >> layout)
      spot q (fx, fy) lifted = case IM.lookup q lifted of
        Just (Rect x y w h, _) -> V2 (x + w * fx) (y + h * fy)
        Nothing -> V2 (-50) (-50)
      -- Whole pixels: a dragged ratio leaves float dust (112.00001).
      wholePx (Rect x y w h) = Rect (r x) (r y) (r w) (r h) where r v = fromIntegral (round v :: Int)
      -- Nothing moves on release, and the dragged pane takes the empty slot.
      previewed p (lifted, during, after) = do
        check (not (IM.member p lifted) && not (IM.member p during) && IM.member p after)
        checkEq during (IM.delete p after)
  runSim ctx failed (withInput 600 400) ui $ do
    -- Pane 1 left, 3 and 5 right, uneven.
    _ <- drag (at 300 200) (at 120 200) 1
    uneven <- layout
    checkEq (IM.fromList [(1, Rect 0 0 112 400), (3, Rect 128 0 472 192), (5, Rect 128 208 472 192)]) $
      IM.map (wholePx . fst) uneven
    -- The model rects are the solver's: the strip spans its pane.
    let across r = (rectX (wholePx r), rectW (wholePx r))
    check (all (\(m, s) -> across m == across s) uneven)
    -- A centre drop swaps: lifted, pane 3 spans the top row, and the hover
    -- already shows it in pane 1's narrow column.
    swap@(liftedS, duringS, _) <- lift 1 (spot 3 (0.5, 0.5))
    checkEq (Just (Rect 0 0 600 192)) (wholePx . fst <$> IM.lookup 3 liftedS)
    checkEq (Just (Rect 0 0 112 400)) (wholePx . fst <$> IM.lookup 3 duringS)
    previewed 1 swap
    -- Edge drops show the target already halved.
    previewed 1 =<< lift 1 (spot 5 (0.1, 0.5))
    previewed 5 =<< lift 5 (spot 3 (0.5, 0.9))
    -- The grid's outer band squeezes the whole grid into the other half.
    top@(_, duringT, _) <- lift 3 (const (V2 590 200))
    check (all (\(Rect x _ w _, _) -> x + w <= 300) duringT)
    previewed 3 top
    -- A gutter still targets the nearer pane, so the preview holds across it.
    gut@(liftedG, duringG, _) <- lift 1 $ \lifted ->
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
    check (duringG /= liftedG)
    previewed 1 gut
    -- Outside the grid the release cancels and the uneven layout comes back
    -- (a top-level drop would re-split it 50/50).
    start <- layout
    (_, duringC, afterC) <- lift 1 (const (V2 (-20) 200))
    check (not (IM.member 1 duringC))
    checkEq start afterC

-- | A pinned pane keeps its width from the first frame of a resize; unpinned,
-- both sides scale.
runPinnedPaneTest :: Context -> IORef Int -> IO ()
runPinnedPaneTest ctx failed = do
  rects <- newIORef IM.empty
  pin <- newIORef True
  let ui = do
        pinned <- liftIO (readIORef pin)
        paneGrid
          halvesGrid
            { pgFixedPanes = if pinned then (== 10) else const False
            , pgViewPane = \pid pctx -> PaneView "P" False <$ note rects pid (pgcRect pctx)
            }
  runSim ctx failed (withInput 600 400) ui $ do
    -- The 16px gutter leaves 584 to share.
    _ <- drag (at 300 200) (at 120 200) 1
    forM_
      [ (True, 600, 2, [(10, 112), (20, 472)])
      , (True, 900, 1, [(10, 112)])
      , (True, 900, 3, [(10, 112), (20, 772)])
      , (True, 600, 3, [(10, 112), (20, 472)])
      , -- Unpinned it scales; pinned again, it keeps the width it was left at.
        (False, 900, 3, [(10, 169.5)])
      , (True, 600, 3, [(10, 169.5)])
      , -- With no room for the pin and the neighbour's minimum, the pin gives way.
        (True, 100, 3, [(10, 44), (20, 40)])
      ]
      $ \(pinned, w, n, want) -> do
        -- @n@ frames at the width, then one more to report what they solved
        -- ('pgcRect' lags a frame, so @n = 1@ reads the first frame at a size).
        liftIO (writeIORef pin pinned)
        modifyInput (\i -> i {inputWindowSize = Size w 400})
        ms <- replicateM_ n frame >> recording rects frame
        forM_ want $ \(p, x) -> case IM.lookup p ms of
          Just r | abs (rectW r - x) <= 1 -> pure ()
          got -> checkEq (pinned, w, p, Just x) (pinned, w, p, rectW <$> got)
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
      widthAt k = area (600 + k * 2 / 3) 400
  forM_
    [ (ends, area 600 400, rectW, [(1, 146), (3, 211)])
    , (reflow [1, 3] (area 600 400) (area 900 400) ends, area 900 400, rectW, [(1, 146), (3, 211), (2, 511)])
    , (stacked, area 600 400, rectW, [(1, 292)])
    , (stacked', area 900 600, rectW, [(1, 292), (2, 592)])
    , (stacked', area 900 600, rectH, [(1, 292), (3, 292)])
    , (rightPin, widthAt 0, rectW, [(2, 292)])
    , (foldl (\t k -> reflow [2] (widthAt k) (widthAt (k + 1)) t) rightPin [0 .. 29 :: Float], widthAt 30, rectW, [(2, 292)])
    ]
    $ \(t, r, size, want) -> forM_ want $ \(p, x) -> case M.lookup p (lay t r) of
      Just g | abs (size g - x) <= 0.01 -> pure ()
      got -> assertEq failed (r, p, Just x) (r, p, size <$> got)

-- | A button scrolled above its viewport can overlap the header, but its
-- hidden rect must not claim the header's drag press.
runClippedControlTest :: Context -> IORef Int -> IO ()
runClippedControlTest ctx failed = do
  rendered <- newIORef False
  geometry <- newIORef Nothing
  let ui =
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
  runSim ctx failed (withInput 300 240) ui $ do
    Just (headerId, sid, targetId) <- liftIO (readIORef geometry)
    hr <- prevRect headerId
    br <- prevRect targetId
    -- Scroll the button's centre to the header's.
    onContext (\c -> setScrollOffset c sid (rectY br + rectH br / 2 - (rectY hr + rectH hr / 2)))
    grab <- settle >> targetPoint (OnWidget targetId)
    check (rectContains hr grab)
    _ <- press (At grab)
    liftIO (writeIORef rendered False)
    _ <- moveTo (At (v2Add grab (V2 30 0)))
    check . not =<< liftIO (readIORef rendered)
