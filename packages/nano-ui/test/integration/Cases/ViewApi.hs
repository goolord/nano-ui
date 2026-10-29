{-# OPTIONS_GHC -O0 #-}
-- | The view API a widget that works out its own input leans on: where it
-- was laid out, the keyboard, pointer frames, content keys, scrolling, and
-- pane grids.
module Cases.ViewApi (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Monad (focusedWidget, releaseFocus)
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "view-queries" runViewQueriesTest
  , spec "hold-focus" runHoldFocusTest
  , spec "pointer-track" runPointerTrackTest
  , spec "content-key-of" runContentKeyOfTest
  , spec "layout-variants" runLayoutVariantsTest
  , spec "pane-grid-initial" runPaneGridInitialTest
  , spec "scroll-ui" runScrollUiTest
  , simSpec "take-escape" 400 300 escapeUi takeEscapeOnce
  ]

-- | A custom widget of a fixed size, focusable or not.
sizedWidget :: Float -> Float -> Bool -> CustomWidgetSpec ()
sizedWidget w h focusable =
  defaultCustomWidgetSpec {widgetLayout = fixedWH w h defaultLayout, widgetFocusable = focusable}

-- | 'lastRect' is nothing before a widget's first frame and its laid-out rect
-- after. 'uiFontSize' is the metrics' line height until overridden.
runViewQueriesTest :: Context -> IORef Int -> IO ()
runViewQueriesTest ctx failed = do
  let inp = withInput 400 300
      ui = do
        wid <- nextId
        before <- lastRect wid
        (resp, ()) <- customWidgetWithId wid (sizedWidget 120 40 False)
        pure (before, respRect resp)
  assertEq failed Nothing . fst =<< evalUi ctx inp ui
  (second, laidOut) <- warmup2 ctx inp ui
  assertEq failed (Just laidOut) second
  assertEq failed (120, 40) (rectW laidOut, rectH laidOut)
  let sized c = evalUi c inp uiFontSize
      big = withFontMetrics ctx (monospaceMetrics 20)
  assertEq failed (fmLineHeight (ctxFontMetrics ctx)) =<< sized ctx
  assertEq failed 20 =<< sized big
  assertEq failed 15 =<< sized (withFontSize big 15)

data FocusMode = Holding | Free | Releasing
  deriving (Eq)

-- | 'holdFocus' keeps the keyboard, ringless, through a Tab (no other widget
-- gets it even for a frame); let go, Tab moves on. 'releaseFocus' frees only
-- the widget named. A holder behind an open modal leaves the modal's focus.
runHoldFocusTest :: Context -> IORef Int -> IO ()
runHoldFocusTest ctx failed = do
  modeRef <- newIORef Holding
  let inp = withInput 400 300
      ui = do
        wid <- nextId
        mode <- liftIO (readIORef modeRef)
        when (mode == Holding) (holdFocus wid)
        when (mode == Releasing) (releaseFocus wid)
        _ <- customWidgetWithId wid (sizedWidget 100 30 True)
        other <- nextId
        when (mode == Holding) (releaseFocus other)
        _ <- customWidgetWithId other (sizedWidget 100 30 True)
        (wid,) <$> focusedWidget
      noRing = assertEq failed False =<< readIORef (ctxFocusVisible ctx)
  (wid, held') <- warmup2 ctx inp ui
  assertEq failed wid held'
  noRing
  _ <- runFrame ctx (tabInp inp) ui
  assertEq failed wid =<< readIORef (ctxFocusId ctx)
  noRing
  writeIORef modeRef Free
  _ <- runFrame ctx (tabInp inp) ui
  moved <- readIORef (ctxFocusId ctx)
  assert failed (moved /= wid && moved /= WidgetId 0)
  writeIORef modeRef Holding
  assertEq failed wid . snd =<< warmup2 ctx inp ui
  writeIORef modeRef Releasing
  assertEq failed (WidgetId 0) . snd =<< warmup2 ctx inp ui
  behind <- newContext
  let page = do
        p <- nextId
        holdFocus p
        _ <- customWidgetWithId p (sizedWidget 100 30 False)
        p <$ modal' (constVar True) "Dialog" (customWidget (sizedWidget 100 30 True))
  _ <- warmup2 behind inp page
  _ <- runFrame behind (tabInp inp) page
  inModal <- readIORef (ctxFocusId behind)
  p <- warmup2 behind inp page
  assert failed (inModal /= WidgetId 0 && inModal /= p)
  assertEq failed inModal =<< readIORef (ctxFocusId behind)

-- | A pointer that moves within one widget runs no frame, unless that widget
-- tracks the pointer; one that moves onto another widget always does.
runPointerTrackTest :: Context -> IORef Int -> IO ()
runPointerTrackTest ctx failed = do
  let ui = rowWith (tight . gap 0) $ do
        _ <- customWidget (sizedWidget 100 100 False) {widgetTrackPointer = True}
        void (customWidget (sizedWidget 100 100 False))
      pt x y = (withInput 400 300) {inputMousePos = V2 x y}
      -- Hover fades run out first, so what is left is the move's own ask.
      settleAt from n = do
        void (runFrame ctx from {inputDeltaTime = 0.1} ui)
        busy <- needsRedraw ctx from from
        when (busy && n > (0 :: Int)) (settleAt from (n - 1))
      needsAfter from to = warmup2 ctx from ui >> settleAt from 50 >> needsRedraw ctx from to
  assert failed =<< needsAfter (pt 20 50) (pt 30 50)
  assertEq failed False =<< needsAfter (pt 120 50) (pt 130 50)
  assert failed =<< needsAfter (pt 120 50) (pt 20 50)

-- | 'contentKeyOf' tells apart what 'contentKey' would round together, keeps
-- the order of its parts, and never gives the "no key" 0.
runContentKeyOfTest :: Context -> IORef Int -> IO ()
runContentKeyOfTest _ failed = do
  let big = 2 ^ (40 :: Int) :: Int
      ints :: [Int] -> Int
      ints xs = contentKeyOf (map keyPart xs)
      texts :: [Text] -> Int
      texts xs = contentKeyOf (map keyPart xs)
  assert failed (ints [big] /= ints [big + 1])
  assert failed (ints [1, 2] /= ints [2, 1])
  assertEq failed (ints [1, 2]) (ints [1, 2])
  assert failed (texts ["ab", "c"] /= texts ["a", "bc"])
  assert failed (contentKeyOf [keyPart (Nothing :: Maybe Int)] /= contentKeyOf [keyPart (Just (0 :: Int))])
  assert failed (contentKeyOf [keyPart (0.1 :: Double)] /= contentKeyOf [keyPart (0.1 + 1e-12 :: Double)])
  assert failed (contentKeyOf [] /= 0)

-- | The @With@ variants apply their layout: a centred checkbox, a row that
-- reports its rect, a sized modal whose filling body gets the inside, and a
-- bare modal panel, which Escape dismisses.
runLayoutVariantsTest :: Context -> IORef Int -> IO ()
runLayoutVariantsTest _ failed = do
  let sim :: Float -> Float -> NanoUI a -> Sim a () -> IO ()
      sim w h ui script = newContext >>= \c -> runSim c failed (withInput w h) ui script
      filler = respRect . fst <$> customWidget defaultCustomWidgetSpec {widgetLayout = fillW (fillH defaultLayout)}
      centred = fst <$> withValue False (checkboxWith' alignMid "Match case")
      ab = (,) <$> buttonWith' (fixedWH 50 20) "A" <*> buttonWith' (fixedWH 50 20) "B"
  sim 400 300 (rowWith (tight . gap 0 . fixedH 60 . fillW) centred) $ do
    Rect _ y _ h <- respRect <$> result
    checkLt h 60
    check (abs (y + h / 2 - 30) <= 1)
  sim 400 300 (rowWith' (fillW . tight . gap 0) ab) $ do
    ((a, b), bar) <- result
    checkEq 400 (rectW (respRect bar))
    checkEq (rectX (respRect a) + 50) (rectX (respRect b))
  sim 800 600 (modalWith' (fixedWH 500 400) (constVar True) "Sized" (columnWith (fillW . fillH . tight) filler)) $ do
    result >>= \(_, inside) -> checkJust inside $ \(Rect x _ w h) -> do
      -- Inside the panel's padding and title bar, and most of it.
      check (w > 440 && w <= 500 && h > 300 && h <= 400)
      check (abs (x + w / 2 - 400) <= 12)
  sim 800 600 (modalPanelWith' (fixedWH 500 400) (constVar True) filler) $ do
    result >>= \(_, inside) -> checkJust inside $ \(Rect x y w h) -> do
      checkEq (500, 400) (w, h)
      check (abs (x + w / 2 - 400) <= 1 && abs (y + h / 2 - 300) <= 1)
    check . respClicked . fst =<< pressKey KeyEscape

-- | A grid given 'pgTree' lays its split out from the first frame, numbers
-- later panes above it, and pins at the width the ratio first gave. The seed
-- takes once: closing every pane leaves an empty grid. An unfocusable grid
-- is no Tab stop.
runPaneGridInitialTest :: Context -> IORef Int -> IO ()
runPaneGridInitialTest ctx failed = do
  rects <- newIORef IM.empty
  -- What each pane's body was laid out at the first time it was.
  firstLaid <- newIORef IM.empty
  splitIt <- newIORef False
  let ui =
        paneGrid
          defaultPaneGridConfig
            { pgLayout = fillW . fillH
            , pgMinSize = 40
            , pgSpacing = 4
            , pgFixedPanes = (== 10)
            , pgTree = InitialTree (Just (Split 30 AxisV 0.25 (Pane 10) (Pane 20)))
            , pgViewPane = \pid pctx -> do
                liftIO (modifyIORef' rects (IM.insert (fromIntegral pid) (pgcRect pctx)))
                body <- nextId
                laid <- lastRect body
                liftIO (forM_ laid (modifyIORef' firstLaid . IM.insertWith (\_ old -> old) (fromIntegral pid)))
                _ <- customWidgetWithId body defaultCustomWidgetSpec {widgetLayout = fillW (fillH defaultLayout)}
                wantSplit <- liftIO (readIORef splitIt)
                when (wantSplit && pid == 20) $ do
                  liftIO (writeIORef splitIt False)
                  void (pgcSplit pctx AxisH)
                pure (PaneView "P" False)
            }
      widthsAfter w = do
        writeIORef rects IM.empty
        replicateM_ 3 (runFrame ctx (withInput w 400) ui)
        IM.map (round . rectW) <$> readIORef rects
  -- A 16px gutter (4 drawn, 6 of leeway each side) leaves 584 of 600 to
  -- share: a quarter is 146. The very first frame, before the grid has a
  -- rect of its own, lays the split out at its ratio too.
  assertEq failed (IM.fromList [(10, 146), (20, 438 :: Int)]) =<< widthsAfter 600
  assertEq failed (Just (146 :: Int)) . fmap (round . rectW) . IM.lookup 10 =<< readIORef firstLaid
  -- Pinned from the first size it had, not from before it had one.
  assertEq failed (IM.fromList [(10, 146), (20, 738)]) =<< widthsAfter 900
  writeIORef splitIt True
  panes <- pgrPanes <$> evalUi ctx (withInput 900 400) ui
  assertEq failed 3 (length panes)
  assert failed (all (\p -> p `elem` [10, 20] || p > 30) panes)
  once <- newContext
  closing <- newIORef False
  let inp = withInput 600 400
      seeded = fmap pgrPanes . paneGrid $
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgTree = InitialTree (Just (Split 30 AxisV 0.5 (Pane 10) (Pane 20)))
          , pgViewPane = \_ pctx -> do
              close <- liftIO (readIORef closing)
              when close (pgcClose pctx)
              pure (PaneView "P" False)
          }
  assertEq failed [10, 20] =<< warmup2 once inp seeded
  writeIORef closing True
  replicateM_ 3 (runFrame once inp seeded)
  writeIORef closing False
  assertEq failed [[], []] =<< replicateM 2 (evalUi once inp seeded)
  tabbed <- newContext
  let grids focusable =
        paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH, pgFocusable = focusable} >> focusedWidget
  _ <- warmup2 tabbed inp (grids False)
  _ <- runFrame tabbed (tabInp inp) (grids False)
  assertEq failed (WidgetId 0) =<< evalUi tabbed inp (grids False)
  _ <- runFrame tabbed (tabInp inp) (grids True)
  assert failed . (/= WidgetId 0) =<< evalUi tabbed inp (grids True)

-- | The scroll commands run from a view: a row scrolls into view, an offset
-- past the content is held to it, and a page goes back. A view reading
-- 'getScrollMetrics' gets a frame when a resize changes them, and no more.
runScrollUiTest :: Context -> IORef Int -> IO ()
runScrollUiTest ctx failed = do
  command <- newIORef (Nothing :: Maybe (WidgetId -> NanoUI ()))
  let inp = withInput 400 300
      ui = do
        sid <- currentId
        cmd <- liftIO (readIORef command)
        liftIO (writeIORef command Nothing)
        mapM_ ($ sid) cmd
        (sid', ()) <- scrollArea (fixedWH 200 200 . tight . gap 0) $
          void (customWidget (sizedWidget 180 1000 False))
        m <- getScrollMetricsUi sid'
        pure (sid == sid', m)
      run cmd = writeIORef command (Just cmd) >> replicateM_ 3 (runFrame ctx inp ui)
      offsetNow = fmap (v2Y . scrollOffset) . snd <$> evalUi ctx inp ui
  (same, m0) <- warmup2 ctx inp ui
  assert failed same
  assertEq failed (Just 0) (v2Y . scrollOffset <$> m0)
  -- A row at 500..520 comes into view at the viewport's foot.
  run (\sid -> scrollRectIntoViewUi sid (Rect 0 500 1 20) ScrollNearest ScrollInstant)
  assertJustM failed offsetNow $ \y -> assert failed (y >= 320 && y <= 500)
  run (\sid -> setScrollOffsetUi sid (V2 0 5000))
  assertJustM failed (snd <$> evalUi ctx inp ui) $ \m ->
    assertEq failed (v2Y (scrollRange m)) (v2Y (scrollOffset m))
  before <- offsetNow
  run (\sid -> scrollPagesUi sid (V2 0 (-0.5)) ScrollInstant)
  after <- offsetNow
  assertJust failed ((,) <$> before <*> after) $ \(b, a) -> assertLt failed a b
  reader <- newContext
  let rows = do
        sid <- currentId
        c <- askContext
        m <- liftIO (getScrollMetrics c sid)
        _ <- scrollArea (fillW . fillH) (void (customWidget (sizedWidget 180 1000 False)))
        pure (fmap (rectH . scrollViewport) m)
      dirtyAfter h = (\(_, _, d) -> d) <$> runFrame reader (withInput 400 h) rows
  replicateM_ 3 (runFrame reader inp rows)
  assertEq failed [False, True, False] =<< mapM dirtyAfter [300, 200, 200]

escapeUi :: NanoUI (Response, Bool, Bool)
escapeUi = do
  (resp, _) <- withValue ("query" :: Text) textInput'
  (,,) resp <$> takeEscape <*> takeEscape

-- | 'takeEscape' is the view's when nothing else took it, is taken once, and
-- is not the view's while a text field's menu is open.
takeEscapeOnce :: Sim (Response, Bool, Bool) ()
takeEscapeOnce = do
  (field, none, _) <- result
  check (not none)
  (_, first, again) <- pressKey KeyEscape
  checkEq (True, False) (first, again)
  _ <- rightClick (On field) >> frame
  (_, forMenu, _) <- pressKey KeyEscape
  check (not forMenu)
