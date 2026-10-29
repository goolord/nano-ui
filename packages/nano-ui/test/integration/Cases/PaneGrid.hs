-- | The pane grid: the split tree it reports and adopts, when an
-- arrangement settles, padding, pane sizes, drag handles and dividers.
module Cases.PaneGrid (tests) where

import Control.Exception (IOException, try)
import Data.Either (isLeft)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (isJust, listToMaybe)
import Data.Word (Word64)
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
  , spec "pane-grid-escape" runEscapeTest
  , spec "pane-grid-empty" runEmptyTest
  , spec "pane-grid-validation" runValidationTest
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

at :: Float -> Float -> Target
at x y = At (V2 x y)

-- | The cursor with the pointer moved onto the target.
cursorAt :: Target -> Sim a UiCursorKind
cursorAt t = do
  _ <- moveTo t
  ctx <- simContext
  liftIO . uiCursorKind ctx =<< getInput

-- | A pane view that, once @flag@ is set, runs @act@ in pane @target@.
actOnce :: IORef Bool -> Word64 -> (PaneGridCtx -> NanoUI ()) -> Word64 -> PaneGridCtx -> NanoUI PaneView
actOnce flag target act pid pctx = do
  wanted <- liftIO (readIORef flag)
  when (wanted && pid == target) (liftIO (writeIORef flag False) >> act pctx)
  pure (PaneView "P" False)

splitH :: PaneGridCtx -> NanoUI ()
splitH pctx = void (pgcSplit pctx AxisH)

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
  let note ref pid r = liftIO (modifyIORef' ref (IM.insert (fromIntegral pid) r))
      cfg =
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

-- | Padding given through 'pgLayout' insets the panes, a pane whose content
-- is wider than its slot keeps the slot (and so does its neighbour), and the
-- rects the grid hands the panes ('pgcRect') are where they were laid out.
-- 'pgDividerColor' fills the gutter.
runLayoutTest :: Context -> IORef Int -> IO ()
runLayoutTest _ failed = do
  let wide pid = when (pid == 10) . void . panelWith (fillW . fillH) . rowWith fillW $ do
        labelWith fillW "A title that runs on"
        box (minW 900 . minH 900 . fillW) (colorRGBA 255 0 0 255)
      split axis c = c {pgTree = InitialTree (Just (Split 30 axis 0.5 (Pane 10) (Pane 20)))}
  -- Padded: 560 inside the padding, less a 16px gutter, halved.
  forM_
    [ (\c -> c {pgLayout = fillW . fillH . padAll 20}, const (pure ()), Rect 20 20 272 360, Rect 308 20 272 360)
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

-- | Dragging the divider sets 'pgrChanged' while it moves and 'pgrCommitted'
-- once, on its release, as a split sets both; a press that moves nothing
-- sets neither. Both panes it moves repaint, at their new place. A divider
-- drawn at a minimum its ratio lies below follows the pointer from its first
-- pixel, and a drag released while the grid was not built is over.
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
    ctx <- simContext
    moving <- mapM flags [press (at 300 200), moveTo (at 360 200)]
    -- The right pane moved from x 308 to 368, and the left one grew.
    dmg <- liftIO (takeDamage ctx)
    check (damageCovers dmg (Rect 0 0 600 400))
    right <- findSpan "right"
    checkJust right $ \r -> checkGt (rectX r) 350
    rest <- mapM flags [moveTo (at 360 200), release, frame, click (at 300 200), frame]
    splitting <- mapM flags [liftIO (writeIORef splitNow True) >> frame, frame]
    let none = (False, False)
    checkEq [none, (True, False), none, (False, True), none, none, none, (True, True), none] (moving ++ rest ++ splitting)
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

-- | A tree passed back every frame keeps the grid as it is, even mid-drag,
-- and a different one replaces it. One the caller does not pass back is
-- rejected, and a changed 'InitialTree' seed does not overwrite the grid.
-- A tree kept in a Var the grid edits resets from the view, and written over
-- a maximized pane shows unmaximized (taking external drops) and stays so
-- when the old tree comes back.
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
        r <- paneGrid halvesGrid {pgTree = ControlledTree (constVar (Just halves)), pgViewPane = actOnce edit 10 splitH}
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
  let inVar = do
        resp <- paneGrid halvesGrid {pgTree = ControlledTree arrangement, pgViewPane = actOnce maximize 20 pgcMaximize}
        wanted <- liftIO (readIORef resetNow)
        when wanted (liftIO (writeIORef resetNow False) >> writeVar arrangement (Just halves))
        pure resp
      raise ref = liftIO (writeIORef ref True) >> settle
  inGrid failed inVar $ do
    _ <- drag (at 300 200) (at 400 200) 1
    checkEq dragged . rootRatio . pgrTree =<< frame
    checkEq (Just 0.5) . rootRatio . pgrTree =<< raise resetNow
    checkEq 20 . pgrMaximizedPane =<< raise maximize
    liftIO (writeVarIO arrangement (Just (Split 5 AxisV 0.5 (Pane 1) (Pane 2))))
    _ <- moveTo (at 5 200)
    checkEq (Just (OutsideGrid PaneLeft)) . fmap pgdLocation . pgrDropTarget =<< settle
    liftIO (writeVarIO arrangement (Just halves))
    checkEq 0 . pgrMaximizedPane =<< settle

-- | A press on a pane's 'paneDragHandle' drags the pane, onto the middle of
-- the other one to swap them, and the indicator following the pointer
-- repaints as it moves. A press below the handle, on a button in it, past
-- its pane's edge or under a scrim drags nothing.
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
    ctx <- simContext
    dmg <- liftIO (takeDamage ctx)
    check (damageCovers dmg (Rect 317 212 117 28))
  let covered = columnWith (fillW . fillH) $ do
        r <- anywhere
        panelWith (pinAt 0 0 . pointer PointerBlock . fixedWH 600 400) (pure ())
        pure r
  inGrid failed covered $ forM_ [V2 300 200, V2 150 200] $ \from ->
    checkEq (Just halves) . pgrTree =<< dragPane from (V2 450 200)

-- | The pointer at the grid's edge or beside a divider previews an external
-- drop, rendering no extra pane, and elsewhere none. A drop is committed
-- once and lands on the next frame with the dropped pane focused; a target
-- from a grid since replaced is stale. An overlay over the edge takes the
-- pointer from the drop zone.
runDropTest :: Context -> IORef Int -> IO ()
runDropTest _ failed = do
  rendered <- newIORef []
  let recording = paneGrid halvesGrid {pgViewPane = \pid _ -> PaneView "Pane" False <$ liftIO (modifyIORef' rendered (pid :))}
      hover x = moveTo (at x 200)
  inGrid failed recording $ do
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
  let replaced = paneGrid halvesGrid {pgTree = ControlledTree (constVar (Just (Pane 90)))}
  assertEq failed Nothing =<< evalUi ctx edge (replaced >> commitPaneDrop stale)
  covered <- newContext
  let overlaid = columnWith (fillW . fillH) $ do
        r <- paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH}
        void (buttonWith' (pinAt 0 150 . fixedWH 80 100) "Overlay")
        pure r
  r <- warmup covered edge overlaid >> warmup2 covered edge overlaid
  assertEq failed (1, Nothing) (pgrPaneCount r, pgrDropTarget r)

-- | A tab dragged out of a pane onto the grid's edge previews without
-- rendering an uncommitted pane, and its release, consumed in the frame that
-- renders its source, commits a new pane.
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

-- | Content wider than its pane is cut to it, bars and all: a button pushed
-- past the pane takes no click there, and the bar of a scroller running on
-- into the next pane shows no grab cursor there. Dragging the bar of a
-- scroller in a pane that drags from anywhere scrolls it and leaves the pane.
runClipTest :: Context -> IORef Int -> IO ()
runClipTest ctx failed = do
  clicks <- newIORef (0 :: Int)
  ids <- newIORef IM.empty
  let note k wid = liftIO (modifyIORef' ids (IM.insert k wid))
      tall = box (fixedWH 100 2000) (colorRGBA 200 0 0 255)
      body 10 = do
        (wid, _) <- scrollArea (fixedWH 450 300) tall
        hidden <- rowWith (tight . gap 0) $ do
          box (fixedWH 400 30) (colorRGBA 200 0 0 255)
          buttonWith' (fixedWH 60 30) "Hidden"
        when (respClicked hidden) (liftIO (modifyIORef' clicks (+ 1)))
        note 0 (respId hidden)
        pure wid
      body _ = fst <$> scrollArea (tight . grow) tall
      ui = paneGrid halvesGrid {pgViewPane = \pid _ -> body pid >>= note (fromIntegral pid) >> pure (PaneView "P" True)}
  runSim ctx failed (withInput 600 400) ui $ do
    known <- liftIO (readIORef ids)
    [Just hidden, Just (Rect wx wy ww _), Just (Rect px py pw _)] <- liftIO (mapM (getPrevRect ctx . (known IM.!)) [0, 10, 20])
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
      scrolled <- liftIO (getScrollOffset ctx (known IM.! 20))
      checkGt scrolled 0

-- | A focused grid takes Escape only to restore a maximized pane; otherwise
-- the key goes on to a handler after it.
runEscapeTest :: Context -> IORef Int -> IO ()
runEscapeTest ctx failed =
  runSim ctx failed (withInput 600 400) ((,) <$> paneGrid halvesGrid <*> takeEscape) $ do
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
      one = ControlledTree (constVar (Just (Pane 1)))
      none = InitialTree Nothing
  empty <- warmup2 ctx inp (withTree none)
  assertEq failed (Nothing, 0, []) (pgrTree empty, pgrPaneCount empty, pgrPanes empty)
  _ <- warmup2 ctx inp (withTree one)
  writeIORef close True
  _ <- evalUi ctx inp (withTree none)
  closed <- warmup2 ctx inp (withTree none)
  assertEq failed (Nothing, 0) (pgrTree closed, pgrPaneCount closed)
  assertEq failed [1] . pgrPanes =<< evalUi ctx inp (withTree one)
  assertEq failed [] . pgrPanes =<< evalUi ctx inp (withTree (ControlledTree (constVar Nothing)))

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
      paneGrid defaultPaneGridConfig {pgTree = ControlledTree (constVar (Just arrangement))}
    assert failed (isLeft outcome)
