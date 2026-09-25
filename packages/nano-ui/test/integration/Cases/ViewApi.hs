-- | The parts of the view API a widget that works out its own input leans
-- on: where it was laid out, the keyboard, the clipboard, frames for a
-- pointer moving over it, its content key, and a pane grid it lays out from
-- a split of its own.
module Cases.ViewApi (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Text (Text)
import Effectful (liftIO)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Monad (focusedWidget, releaseFocus)

tests :: [Spec]
tests =
  [ spec "last-rect" runLastRectTest
  , spec "hold-focus" runHoldFocusTest
  , spec "hold-focus-modal" runHoldFocusModalTest
  , spec "hold-focus-tab" runHoldFocusTabTest
  , spec "clipboard" runClipboardTest
  , spec "pointer-track" runPointerTrackTest
  , spec "content-key-of" runContentKeyOfTest
  , spec "checkbox-with" runCheckboxWithTest
  , spec "pane-grid-initial" runPaneGridInitialTest
  , spec "pane-grid-initial-once" runPaneGridInitialOnceTest
  , spec "pane-grid-unfocusable" runPaneGridUnfocusableTest
  , spec "scroll-ui" runScrollUiTest
  , spec "take-escape" runTakeEscapeTest
  , spec "modal-with" runModalWithTest
  ]

-- | 'lastRect' is nothing before a widget's first frame and the rect it was
-- laid out in after it.
runLastRectTest :: Context -> IORef Int -> IO ()
runLastRectTest ctx failed = do
  let inp = withInput 400 300
      ui = do
        wid <- nextId
        before <- lastRect wid
        (resp, ()) <- customWidgetWithId wid defaultCustomWidgetSpec {widgetLayout = fixedWH 120 40 defaultLayout}
        pure (before, respRect resp)
  ((first, _), _, _, _) <- runFrame ctx inp ui
  assertEq failed first Nothing
  (second, laidOut) <- warmup2 ctx inp ui
  assertEq failed second (Just laidOut)
  assertEq failed (rectW laidOut, rectH laidOut) (120, 40)

-- | 'holdFocus' gives a widget the keyboard without the ring Tab draws, and
-- 'releaseFocus' takes it off again, and off only the widget named.
runHoldFocusTest :: Context -> IORef Int -> IO ()
runHoldFocusTest ctx failed = do
  holding <- newIORef True
  let inp = withInput 400 300
      ui = do
        wid <- nextId
        hold <- liftIO (readIORef holding)
        if hold then holdFocus wid else releaseFocus wid
        _ <- customWidgetWithId wid defaultCustomWidgetSpec {widgetLayout = fixedWH 100 30 defaultLayout, widgetFocusable = True}
        other <- nextId
        releaseFocus other
        (,) wid <$> focusedWidget
  (wid, focused) <- warmup2 ctx inp ui
  assertEq failed focused wid
  assertEq failed False =<< readIORef (ctxFocusVisible ctx)
  writeIORef holding False
  (_, released) <- warmup2 ctx inp ui
  assertEq failed released (WidgetId 0)

-- | A widget behind an open modal that holds focus leaves the keyboard with
-- the widget in the modal that has it.
runHoldFocusModalTest :: Context -> IORef Int -> IO ()
runHoldFocusModalTest ctx failed = do
  let inp = withInput 400 300
      ui = do
        page <- nextId
        holdFocus page
        _ <- customWidgetWithId page defaultCustomWidgetSpec {widgetLayout = fixedWH 100 30 defaultLayout}
        _ <-
          modal True "Dialog" $
            customWidget defaultCustomWidgetSpec {widgetLayout = fixedWH 100 30 defaultLayout, widgetFocusable = True}
        pure page
  _ <- warmup2 ctx inp ui
  -- Tab puts the keyboard on a widget in the modal.
  _ <- runFrame ctx (tabInp inp) ui
  inModal <- readIORef (ctxFocusId ctx)
  (page, _, _, _) <- runFrame ctx inp ui
  _ <- runFrame ctx inp ui
  focus <- readIORef (ctxFocusId ctx)
  assert failed (inModal /= WidgetId 0 && inModal /= page)
  assertEq failed focus inModal

-- | Tab pressed while a widget holds the keyboard is the widget's: focus
-- stays on it with no ring, and no other widget has it even for a frame.
-- Let go, the same Tab moves focus to the next widget.
runHoldFocusTabTest :: Context -> IORef Int -> IO ()
runHoldFocusTabTest ctx failed = do
  holding <- newIORef True
  let inp = withInput 400 300
      focusable = defaultCustomWidgetSpec {widgetLayout = fixedWH 100 30 defaultLayout, widgetFocusable = True}
      ui = do
        wid <- nextId
        hold <- liftIO (readIORef holding)
        when hold (holdFocus wid)
        _ <- customWidgetWithId wid focusable
        _ <- customWidget focusable
        pure wid
  wid <- warmup2 ctx inp ui
  _ <- runFrame ctx (tabInp inp) ui
  kept <- readIORef (ctxFocusId ctx)
  ring <- readIORef (ctxFocusVisible ctx)
  assertEq failed kept wid
  assertEq failed ring False
  writeIORef holding False
  _ <- runFrame ctx (tabInp inp) ui
  moved <- readIORef (ctxFocusId ctx)
  assert failed (moved /= wid && moved /= WidgetId 0)

-- | 'setClipboard' and 'getClipboard' go through the context's clipboard.
runClipboardTest :: Context -> IORef Int -> IO ()
runClipboardTest ctx0 failed = do
  board <- newIORef (Nothing :: Maybe Text)
  let ctx = withClipboard ctx0 (readIORef board) (\t -> writeIORef board (Just t) >> pure True)
      inp = withInput 400 300
  ((empty, _), _, _, _) <- runFrame ctx inp ((,) <$> getClipboard <*> setClipboard "copied")
  assertEq failed empty Nothing
  assertEq failed (Just "copied") =<< readIORef board
  ((pasted, _), _, _, _) <- runFrame ctx inp ((,) <$> getClipboard <*> pure ())
  assertEq failed pasted (Just "copied")

-- | A pointer that moves within one widget runs no frame, unless that widget
-- tracks the pointer; one that moves onto another widget always does.
runPointerTrackTest :: Context -> IORef Int -> IO ()
runPointerTrackTest ctx failed = do
  let inp = withInput 400 300
      ui = rowWith (tight . gap 0) $ do
        _ <- customWidget defaultCustomWidgetSpec {widgetLayout = fixedWH 100 100 defaultLayout, widgetTrackPointer = True}
        _ <- customWidget defaultCustomWidgetSpec {widgetLayout = fixedWH 100 100 defaultLayout}
        pure ()
      at x y = inp {inputMousePos = V2 x y}
      -- Hover fades run out first, so what is left is the move's own ask.
      settle from n = do
        void (runFrame ctx from {inputDeltaTime = 0.1} ui)
        busy <- needsRedraw ctx from from
        when (busy && n > (0 :: Int)) (settle from (n - 1))
      needsAfter from to = do
        void (warmup2 ctx from ui)
        settle from 50
        needsRedraw ctx from to
  assert failed =<< needsAfter (at 20 50) (at 30 50)
  assertEq failed False =<< needsAfter (at 120 50) (at 130 50)
  assert failed =<< needsAfter (at 120 50) (at 20 50)

-- | 'contentKeyOf' tells apart what 'contentKey' would round together, keeps
-- the order of its parts, and never gives the "no key" 0.
runContentKeyOfTest :: Context -> IORef Int -> IO ()
runContentKeyOfTest _ failed = do
  let big = 2 ^ (40 :: Int) :: Int
      key :: [Int] -> Int
      key xs = contentKeyOf (map keyPart xs)
  assert failed (key [big] /= key [big + 1])
  assert failed (key [1, 2] /= key [2, 1])
  assertEq failed (key [1, 2]) (key [1, 2])
  assert failed (contentKeyOf [keyPart ("ab" :: Text), keyPart ("c" :: Text)] /= contentKeyOf [keyPart ("a" :: Text), keyPart ("bc" :: Text)])
  assert failed (contentKeyOf [keyPart (Nothing :: Maybe Int)] /= contentKeyOf [keyPart (Just (0 :: Int))])
  assert failed (contentKeyOf [keyPart (0.1 :: Double)] /= contentKeyOf [keyPart (0.1 + 1e-12 :: Double)])
  assert failed (contentKeyOf [] /= 0)

-- | 'checkboxWith' takes a layout: centred in a row taller than itself.
runCheckboxWithTest :: Context -> IORef Int -> IO ()
runCheckboxWithTest ctx failed = do
  let inp = withInput 400 300
      ui = rowWith (tight . gap 0 . fixedH 60 . fillW) $ fst <$> checkboxWith' alignMid "Match case" False
  resp <- warmup2 ctx inp ui
  let r = respRect resp
      mid = rectY r + rectH r / 2
  assert failed (rectH r < 60)
  assert failed (abs (mid - 30) <= 1)

-- | A grid given 'pgInitial' lays its split out from the first frame, asks
-- for the panes it names, gives a pane it makes later an id above all of
-- them, and holds a pinned pane at the width the ratio first gave it.
runPaneGridInitialTest :: Context -> IORef Int -> IO ()
runPaneGridInitialTest ctx failed = do
  rects <- newIORef IM.empty
  -- What each pane's body was laid out at the first time it was.
  firstLaid <- newIORef IM.empty
  splitIt <- newIORef False
  let cfg =
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgMinSize = 40
          , pgSpacing = 4
          , pgFixedPanes = (== 10)
          , pgInitial = Just (Split 30 AxisV 0.25 (Pane 10) (Pane 20))
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
              pure (PaneView "P" False Nothing)
          }
      ui = paneGrid cfg
      frames inp n = replicateM_ n (runFrame ctx inp ui)
      widthOf p want = do
        ms <- readIORef rects
        assertJust failed (IM.lookup p ms) $ \r ->
          if abs (rectW r - want) <= 1 then pure () else assertEq failed (p, rectW r) (p, want)
  -- A 16px gutter (4 drawn, 6 of leeway each side) leaves 584 of 600 to
  -- share: a quarter is 146.
  frames (withInput 600 400) 3
  panes0 <- IM.keys <$> readIORef rects
  assertEq failed panes0 [10, 20]
  widthOf 10 146
  widthOf 20 438
  -- The very first frame, before the grid has a rect of its own, lays the
  -- split out at its ratio too.
  firstWidths <- IM.map (round . rectW) <$> readIORef firstLaid
  assertEq failed (IM.lookup 10 firstWidths) (Just (146 :: Int))
  -- Pinned from the first size it had, not from before it had one.
  writeIORef rects IM.empty
  frames (withInput 900 400) 3
  widthOf 10 146
  widthOf 20 738
  -- A pane made later takes an id above every id in the initial tree.
  writeIORef splitIt True
  writeIORef rects IM.empty
  (PaneGridResponse {pgrPanes = panes}, _, _, _) <- runFrame ctx (withInput 900 400) ui
  assertEq failed (length panes) 3
  assert failed (all (\p -> p `elem` [10, 20] || p > 30) panes)

-- | 'pgInitial' is where a grid starts, not where it goes back to: once its
-- last pane is closed it starts again from one fresh pane, whose id no closed
-- pane had.
runPaneGridInitialOnceTest :: Context -> IORef Int -> IO ()
runPaneGridInitialOnceTest ctx failed = do
  closing <- newIORef False
  let cfg =
        defaultPaneGridConfig
          { pgLayout = fillW . fillH
          , pgInitial = Just (Split 30 AxisV 0.5 (Pane 10) (Pane 20))
          , pgViewPane = \_ pctx -> do
              close <- liftIO (readIORef closing)
              when close (pgcClose pctx)
              pure (PaneView "P" False Nothing)
          }
      inp = withInput 600 400
      ui = paneGrid cfg
  PaneGridResponse {pgrPanes = start} <- warmup2 ctx inp ui
  assertEq failed start [10, 20]
  writeIORef closing True
  replicateM_ 3 (runFrame ctx inp ui)
  writeIORef closing False
  (PaneGridResponse {pgrPanes = after}, _, _, _) <- runFrame ctx inp ui
  (PaneGridResponse {pgrPanes = again}, _, _, _) <- runFrame ctx inp ui
  assertEq failed (length again) 1
  assert failed (all (> 30) again)
  assert failed (after == [] || after == again)

-- | A grid that is not focusable is no Tab stop.
runPaneGridUnfocusableTest :: Context -> IORef Int -> IO ()
runPaneGridUnfocusableTest ctx failed = do
  let inp = withInput 400 300
      ui focusable = do
        resp <- paneGrid defaultPaneGridConfig {pgLayout = fillW . fillH, pgFocusable = focusable}
        focus <- focusedWidget
        pure (resp, focus)
  _ <- warmup2 ctx inp (ui False)
  _ <- runFrame ctx (tabInp inp) (ui False)
  assertEq failed (WidgetId 0) . snd =<< evalUi ctx inp (ui False)
  _ <- runFrame ctx (tabInp inp) (ui True)
  ((_, focused), _, _, _) <- runFrame ctx inp (ui True)
  assert failed (focused /= WidgetId 0)

-- | The scroll commands run from a view: a list scrolls a row into view, a
-- raw offset past the content is held to what the content turns out to be,
-- and the metrics say where the scroller is.
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
          void (customWidget defaultCustomWidgetSpec {widgetLayout = fixedWH 180 1000 defaultLayout})
        m <- getScrollMetricsUi sid'
        pure (sid == sid', m)
      settle = replicateM_ 3 (runFrame ctx inp ui)
      offsetNow = do
        ((_, m), _, _, _) <- runFrame ctx inp ui
        pure (fmap (v2Y . scrollOffset) m)
  (same, m0) <- warmup2 ctx inp ui
  assert failed same
  assertEq failed (fmap (v2Y . scrollOffset) m0) (Just 0)
  -- A row at 500..520 comes into view at the viewport's foot.
  writeIORef command (Just (\sid -> scrollRectIntoViewUi sid (Rect 0 500 1 20) ScrollNearest ScrollInstant))
  settle
  assertJustM failed offsetNow $ \y -> assert failed (y >= 320 && y <= 500)
  -- Past the end: held to the range.
  writeIORef command (Just (\sid -> setScrollOffsetUi sid (V2 0 5000)))
  settle
  assertJustM failed offsetNow $ \y -> assert failed (y <= 1000 - 150)
  -- Half a page back up.
  before <- offsetNow
  writeIORef command (Just (\sid -> scrollPagesUi sid (V2 0 (-0.5)) ScrollInstant))
  settle
  after <- offsetNow
  assertJust failed ((,) <$> before <*> after) $ \(b, a) -> assert failed (a < b)

-- | 'takeEscape' is the view's when nothing else took it, is taken once, and
-- is not the view's while a text field's menu is open.
runTakeEscapeTest :: Context -> IORef Int -> IO ()
runTakeEscapeTest ctx failed = do
  let inp = withInput 400 300
      esc = inp {inputKeys = inputKeysFromList [KeyEscape]}
      ui = do
        (resp, _) <- textInput' ("query" :: Text)
        first <- takeEscape
        again <- takeEscape
        pure (resp, first, again)
  (resp, none, _) <- warmup2 ctx inp ui
  assertEq failed none False
  ((_, first, again), _, _, _) <- runFrame ctx esc ui
  assertEq failed (first, again) (True, False)
  -- Open the field's right-click menu: that Escape closes the menu instead.
  let (press, release) = rightClickPair inp (centerOf resp)
  mapM_ (\i -> runFrame ctx i ui) [press, release, inp]
  ((_, forMenu, _), _, _, _) <- runFrame ctx esc ui
  assertEq failed forMenu False

-- | 'modalWith' sizes the panel, and a body that fills it gets the inside.
runModalWithTest :: Context -> IORef Int -> IO ()
runModalWithTest ctx failed = do
  let inp = withInput 800 600
      ui = modalWith (fixedWH 500 400) True "Sized" $ columnWith (fillW . fillH . tight) $ do
        (resp, ()) <- customWidget defaultCustomWidgetSpec {widgetLayout = (fillW . fillH) defaultLayout}
        pure (respRect resp)
  _ <- warmup2 ctx inp ui
  (_, inside) <- warmup2 ctx inp ui
  assertJust failed inside $ \r -> do
    -- Inside the panel's padding and title bar, and most of it.
    assert failed (rectW r > 440 && rectW r <= 500)
    assert failed (rectH r > 300 && rectH r <= 400)
    -- Centred in the window.
    assert failed (abs (rectX r + rectW r / 2 - 400) <= 12)
