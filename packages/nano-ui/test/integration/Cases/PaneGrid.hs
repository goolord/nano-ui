-- | The pane grid: the split tree it reports and adopts, when an
-- arrangement settles, padding, pane sizes, drag handles and dividers.
module Cases.PaneGrid (tests) where

import Data.IntMap.Strict qualified as IM
import Data.Maybe (listToMaybe)
import Data.Word (Word64)
import Spec

tests :: [Spec]
tests =
  [ spec "pane-grid-padding" runPaddingTest
  , spec "pane-grid-overflow" runOverflowTest
  , spec "pane-grid-tree" runTreeTest
  , spec "pane-grid-committed" runCommittedTest
  , spec "pane-grid-drag-handle" runDragHandleTest
  , spec "pane-grid-divider-color" runDividerColorTest
  , spec "pane-grid-reset" runResetTest
  , spec "pane-grid-clip" runClipTest
  , spec "pane-grid-drop-destination" runDropDestinationTest
  , spec "pane-grid-drop-commit" runDropCommitTest
  , spec "pane-grid-tab-drop" runTabDropTest
  , spec "pane-grid-covered-drop" runCoveredDropTest
  ]

-- | Two panes side by side in a 600 by 400 grid: a 16px gutter from 292 to
-- 308, down the middle.
halves :: GridNode
halves = Split 30 AxisV 0.5 (Pane 10) (Pane 20)

runCoveredDropTest :: Context -> IORef Int -> IO ()
runCoveredDropTest ctx failed = do
  let at = V2 5 200
      inp = (withInput 600 400) {inputMousePos = at}
      ui = columnWith (fillW . fillH) $ do
        r <- paneGrid defaultPaneGridConfig
          { pgLayout = fillW . fillH
          }
        void (buttonWith' (pinAt 0 150 . fixedWH 80 100) "Overlay")
        pure r
  _ <- warmup2 ctx inp ui
  r <- evalUi ctx inp ui
  assertEq failed (1, Nothing) (pgrPaneCount r, pgrDropTarget r)

runDropDestinationTest :: Context -> IORef Int -> IO ()
runDropDestinationTest ctx failed = do
  rendered <- newIORef []
  let inp = withInput 600 400
      ui = paneGrid defaultPaneGridConfig
        { pgLayout = fillW . fillH
        , pgTree = Just halves
        , pgViewPane = \pid _ -> do
            liftIO (modifyIORef' rendered (pid :))
            pure (PaneView "Pane" False)
        }
  _ <- warmup2 ctx inp ui
  writeIORef rendered []
  preview <- evalUi ctx (inp {inputMousePos = V2 5 200}) ui
  assertEq failed (Just halves, False) (pgrTree preview, pgrChanged preview)
  assertJust failed (pgrDropTarget preview) $ \target -> do
    assertEq failed (OutsideGrid PaneLeft) (pgdLocation target)
    assertEq failed 10 (pgdPane target)
    assert failed (rectW (pgdRect target) > 0 && rectH (pgdRect target) == 400)
  assertEq failed [20, 10] =<< readIORef rendered
  beside <- evalUi ctx (inp {inputMousePos = V2 280 200}) ui
  assertEq failed (Just (BesidePane 10 PaneRight)) (pgdLocation <$> pgrDropTarget beside)
  forM_ [V2 (-1) 200, V2 150 200, V2 300 200] $ \pos -> do
    r <- evalUi ctx (inp {inputMousePos = pos}) ui
    assertEq failed (Just halves, Nothing) (pgrTree r, pgrDropTarget r)

runDropCommitTest :: Context -> IORef Int -> IO ()
runDropCommitTest ctx failed = do
  let inp = (withInput 600 400) {inputMousePos = V2 5 200}
      view arrangement = paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH, pgTree = Just arrangement}
      ui = do
        r <- view halves
        case pgrDropTarget r of
          Nothing -> pure (r, Nothing, Nothing)
          Just target -> do
            committed <- commitPaneDrop target
            repeated <- commitPaneDrop target
            pure (r, committed, repeated)
  _ <- warmup2 ctx inp (view halves)
  ((before, committed, repeated), _, _, dirty) <- runFrame ctx inp ui
  assert failed dirty
  assertEq failed (Just halves) (pgrTree before)
  assertEq failed Nothing repeated
  after <- evalUi ctx inp (view halves)
  assertJust failed committed $ \(pid, arrangement) -> do
    assertEq failed (Just arrangement) (pgrTree after)
    assertEq failed pid (pgrFocusedPane after)
    assertEq failed 3 (pgrPaneCount after)
  stale <- maybe (fail "missing target") pure (pgrDropTarget after)
  rejected <- evalUi ctx inp $ do
    _ <- view (Pane 90)
    commitPaneDrop stale
  assertEq failed Nothing rejected

-- The source is rendered inside the grid, yet the release is consumed in
-- that same frame. Previewing must never render an uncommitted pane id.
runTabDropTest :: Context -> IORef Int -> IO ()
runTabDropTest ctx failed = do
  headers <- newIORef []
  let inp = withInput 600 400
      ui = do
        liftIO (writeIORef headers [])
        response <- paneGrid defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgViewPane = \pid _ -> do
              r <- tabBar' (1 :: Int) [tab 1 "Alpha" (), tab 2 "Beta" ()]
              liftIO (modifyIORef' headers (++ [((pid, k), response) | (k, response) <- tabHeaders r]))
              pure (PaneView "Editor" False)
          }
        sources <- liftIO (readIORef headers)
        gesture <- useDrag sources
        committed <- case (gesture, pgrDropTarget response) of
          (Just d, Just target) | dragPhase d == DragReleased -> commitPaneDrop target
          _ -> pure Nothing
        pure (response, committed)
  _ <- warmup2 ctx inp ui
  hs <- readIORef headers
  first <- maybe (fail "missing tab header") (pure . snd) (listToMaybe hs)
  let press = pressAt inp (spanCenter (respRect first))
      moved = holdAt press (V2 5 200)
  _ <- evalUi ctx press ui
  (preview, _) <- evalUi ctx moved ui
  assertEq failed 1 (pgrPaneCount preview)
  (_, committed) <- evalUi ctx (releaseAt moved) ui
  assertJust failed committed (\_ -> pure ())
  (after, _) <- evalUi ctx inp ui
  assertEq failed 2 (pgrPaneCount after)

-- | The ratio of the root split.
rootRatio :: Maybe GridNode -> Maybe Float
rootRatio = \case
  Just (Split _ _ r _ _) -> Just r
  _ -> Nothing

-- | A grid, the rect each pane's body was laid out at, and the rect each
-- pane was told it has ('pgcRect').
probeGrid ::
  (PaneGridConfig -> PaneGridConfig)
  -> (Word64 -> NanoUI ())
  -> IO (NanoUI PaneGridResponse, IORef (IM.IntMap Rect), IORef (IM.IntMap Rect))
probeGrid f body = do
  laid <- newIORef IM.empty
  told <- newIORef IM.empty
  let
    cfg =
      f
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgSpacing = 4
          , pgLeeway = 6
          , pgTree = Just halves
          , pgViewPane = \pid pctx -> do
              liftIO (modifyIORef' told (IM.insert (fromIntegral pid) (pgcRect pctx)))
              (_, area) <- mouseArea (fillW . fillH) (body pid)
              liftIO (modifyIORef' laid (IM.insert (fromIntegral pid) (respRect area)))
              pure (PaneView "P" False)
          }
  pure (paneGrid cfg, laid, told)

-- | Padding given through 'pgLayout' insets the panes, and the rects the
-- grid hands them ('pgcRect') are where they were laid out.
runPaddingTest :: Context -> IORef Int -> IO ()
runPaddingTest ctx failed = do
  (ui, laid, told) <-
    probeGrid (\c -> c {pgLayout = fillW . fillH . padAll 20}) (\_ -> pure ())
  replicateM_ 4 (runFrame ctx (withInput 600 400) ui)
  l <- readIORef laid
  t <- readIORef told
  -- 560 inside the padding, less a 16px gutter, halved.
  assertEq failed (IM.lookup 10 l) (Just (Rect 20 20 272 360))
  assertEq failed (IM.lookup 20 l) (Just (Rect 308 20 272 360))
  assertEq failed (IM.lookup 10 t) (IM.lookup 10 l)
  assertEq failed (IM.lookup 20 t) (IM.lookup 20 l)

-- | A pane whose content is wider than its slot keeps the slot, and so does
-- its neighbour.
runOverflowTest :: Context -> IORef Int -> IO ()
runOverflowTest ctx failed = do
  forM_ [AxisV, AxisH] $ \axis -> do
    (ui, laid, told) <-
      probeGrid (\c -> c {pgTree = Just (Split 30 axis 0.5 (Pane 10) (Pane 20))}) $ \pid ->
        when (pid == 10) . void . panelWith (fillW . fillH) . rowWith fillW $ do
          labelWith fillW "A title that runs on"
          box (minW 900 . minH 900 . fillW) (colorRGBA 255 0 0 255)
    replicateM_ 4 (runFrame ctx (withInput 600 400) (withKey (show axis) ui))
    l <- readIORef laid
    t <- readIORef told
    let
      (first, second) = case axis of
        AxisV -> (Rect 0 0 292 400, Rect 308 0 292 400)
        AxisH -> (Rect 0 0 600 192, Rect 0 208 600 192)
    assertEq failed (IM.lookup 10 t, IM.lookup 20 t) (Just first, Just second)
    assertEq failed (IM.lookup 10 l, IM.lookup 20 l) (Just first, Just second)

-- | 'pgrTree' is the grid's tree and reads back from 'show'. A caller that
-- passes it back keeps the grid as it is, even mid-drag, and a different
-- tree replaces it.
runTreeTest :: Context -> IORef Int -> IO ()
runTreeTest ctx failed = do
  given <- newIORef (Just halves)
  let
    inp = withInput 600 400
    ui =
      paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH, pgTree = Just halves}
    controlled = do
      t <- liftIO (readIORef given)
      resp <- paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH, pgTree = t}
      liftIO (writeIORef given (pgrTree resp))
      pure resp
  resp0 <- warmup2 ctx inp ui
  assertEq failed (pgrTree resp0) (Just halves)
  assertEq failed (read . show <$> pgrTree resp0) (Just halves)
  -- Drag the divider 100px right, passing the tree back every frame.
  _ <- warmup2 ctx inp (withKey (1 :: Int) controlled)
  let
    press = pressAt inp (V2 300 200)
  forM_ [press, holdAt press (V2 350 200), holdAt press (V2 400 200)] $ \i ->
    void (runFrame ctx i (withKey (1 :: Int) controlled))
  resp1 <-
    evalUi
      ctx
      (releaseAt (holdAt press (V2 400 200)))
      (withKey (1 :: Int) controlled)
  assertEq failed (rootRatio (pgrTree resp1)) (Just (0.5 + 100 / 584))
  -- A new tree replaces the grid's.
  let
    other = Split 5 AxisH 0.25 (Pane 1) (Pane 2)
  writeIORef given (Just other)
  resp2 <- evalUi ctx inp (withKey (1 :: Int) controlled)
  assertEq failed (pgrTree resp2) (Just other)
  assertEq failed (pgrPanes resp2) [1, 2]

-- | 'pgrCommitted' is set once, on the frame a divider is let go after
-- moving, and after a split; not while the divider moves, nor for a press
-- that moves nothing.
runCommittedTest :: Context -> IORef Int -> IO ()
runCommittedTest ctx failed = do
  splitNow <- newIORef False
  -- The flags of every pass of a frame: a grid that changes runs the view
  -- again, and an app acts on the pass that saw the change.
  seen <- newIORef (False, False)
  let
    inp = withInput 600 400
    ui = do
      resp <-
        paneGrid
          defaultPaneGridConfig
            { pgLayout = fillW . fillH
            , pgTree = Just halves
            , pgViewPane = \pid pctx -> do
                wanted <- liftIO (readIORef splitNow)
                when (wanted && pid == 10) $ do
                  liftIO (writeIORef splitNow False)
                  void (pgcSplit pctx AxisH)
                pure (PaneView "P" False)
            }
      liftIO
        (modifyIORef' seen (\(c, k) -> (c || pgrChanged resp, k || pgrCommitted resp)))
    frame i = do
      writeIORef seen (False, False)
      _ <- runFrame ctx i ui
      readIORef seen
  _ <- warmup2 ctx inp ui
  let
    press = pressAt inp (V2 300 200)
    moved = holdAt press (V2 360 200)
  assertEq failed (False, False) =<< frame press
  assertEq failed (True, False) =<< frame moved
  assertEq failed (False, False) =<< frame moved
  assertEq failed (False, True) =<< frame (releaseAt moved)
  assertEq failed (False, False) =<< frame inp
  -- A press and release in place.
  assertEq failed (False, False) =<< frame (pressAt inp (V2 300 200))
  assertEq failed (False, False) =<< frame (releaseAt (pressAt inp (V2 300 200)))
  assertEq failed (False, False) =<< frame inp
  writeIORef splitNow True
  assertEq failed (True, True) =<< frame inp
  assertEq failed (False, False) =<< frame inp

-- | A press on a pane's 'paneDragHandle' drags the pane, onto the middle of
-- the other one to swap them; a press on a button inside the handle does not.
runDragHandleTest :: Context -> IORef Int -> IO ()
runDragHandleTest ctx failed = do
  clicks <- newIORef (0 :: Int)
  let
    inp = withInput 600 400
    ui =
      paneGrid
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgTree = Just halves
          , pgViewPane = \_ pctx -> do
              paneDragHandle pctx (fillW . fixedH 40) $ do
                flex
                whenM (buttonWith (fixedWH 40 30) "x") (liftIO (modifyIORef' clicks (+ 1)))
              pure (PaneView "P" False)
          }
    panesAfter frames = do
      mapM_ (\i -> runFrame ctx i ui) (init frames)
      pgrPanes <$> evalUi ctx (last frames) ui
    dragFrom from to =
      let
        press = pressAt inp from
        hold = holdAt press to
       in
        [ press
        , holdAt press (V2 (v2X from + 20) (v2Y from))
        , hold
        , hold
        , releaseAt hold
        , inp
        ]
  _ <- warmup2 ctx inp ui
  -- The handle is the top 40px of each pane; its button is at its right end.
  assertEq failed [20, 10] =<< panesAfter (dragFrom (V2 60 20) (V2 454 200))
  -- Below the handle is not a handle, and the button is not either.
  assertEq failed [20, 10] =<< panesAfter (dragFrom (V2 60 200) (V2 454 200))
  assertEq failed [20, 10] =<< panesAfter (dragFrom (V2 270 20) (V2 454 200))
  assertEq failed 0 =<< readIORef clicks
  -- Grab cursor over the handle.
  assertEq failed UiCursorGrab =<< cursorOver ctx inp ui (V2 60 20)

-- | 'pgDividerColor' fills the gutter.
runDividerColorTest :: Context -> IORef Int -> IO ()
runDividerColorTest ctx failed = do
  let
    inp = withInput 600 400
    c = colorRGBA 10 20 30 255
    ui =
      paneGrid
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgTree = Just halves
          , pgDividerColor = Just c
          }
  (_, dd) <- warmupDraw ctx inp ui
  quads <- drawQuads dd
  assert failed ((Rect 292 0 16 400, c) `elem` quads)

-- | The reset the 'pgTree' documentation shows: the tree kept in state and
-- passed back each frame, and set to the starting layout by a button.
runResetTest :: Context -> IORef Int -> IO ()
runResetTest ctx failed = do
  resetNow <- newIORef False
  let
    inp = withInput 600 400
    ui = do
      (arrangement, setArrangement) <- useState (Just halves)
      resp <-
        paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH, pgTree = arrangement}
      setArrangement (pgrTree resp)
      wanted <- liftIO (readIORef resetNow)
      when wanted $ do
        liftIO (writeIORef resetNow False)
        setArrangement (Just halves)
      pure resp
    ratioAfter i = rootRatio . pgrTree <$> evalUi ctx i ui
  _ <- warmup2 ctx inp ui
  let
    press = pressAt inp (V2 300 200)
    moved = holdAt press (V2 400 200)
  mapM_ (\i -> runFrame ctx i ui) [press, moved, releaseAt moved]
  assertEq failed (Just (0.5 + 100 / 584)) =<< ratioAfter inp
  writeIORef resetNow True
  _ <- runFrame ctx inp ui
  assertEq failed (Just 0.5) =<< ratioAfter inp

-- | Content wider than its pane is clipped to it: a button pushed past the
-- pane's right edge takes no click there.
runClipTest :: Context -> IORef Int -> IO ()
runClipTest ctx failed = do
  clicks <- newIORef (0 :: Int)
  hidden <- newIORef Nothing
  let
    inp = withInput 600 400
    ui =
      paneGrid
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgTree = Just halves
          , pgViewPane = \pid _ -> do
              when (pid == 10) . rowWith (tight . gap 0) $ do
                box (fixedWH 400 30) (colorRGBA 200 0 0 255)
                resp <- buttonWith' (fixedWH 60 30) "Hidden"
                liftIO (writeIORef hidden (Just (respRect resp)))
                when (respClicked resp) (liftIO (modifyIORef' clicks (+ 1)))
              pure (PaneView "P" False)
          }
  _ <- warmup2 ctx inp ui
  mr <- readIORef hidden
  assertJust failed mr $ \r -> do
    -- Laid out past the pane, which ends at 292.
    assert failed (rectX r >= 292)
    _ <- runClick ctx inp ui (spanCenter r)
    assertEq failed 0 =<< readIORef clicks
