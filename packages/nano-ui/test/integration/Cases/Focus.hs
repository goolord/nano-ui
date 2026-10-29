module Cases.Focus (tests) where

import Spec
import Cases.SimExtra (focusId, focusOn, tap)
import Data.Maybe (isJust)
import Data.Text qualified as T
import NanoUI.Internal.Context (InteractionState (..), getFocusVisible, getsInteraction, intKey)
import NanoUI.Internal.Store (anySelectOpen, fieldSelection, fieldText, findSlot)
import NanoUI.Monad (focusedWidget, releaseFocus)
import NanoUI.Shortcut
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "canvas-focus-click" runCanvasFocusClickTest
  , spec "focus-request-kinds" runFocusRequestKindsTest
  , spec "focus-request-tab-order" runFocusRequestTabOrderTest
  , spec "focus-request-refused" runFocusRequestRefusedTest
  , spec "focus-request-modal" runFocusRequestModalTest
  , spec "focus-request-blurs-previous" runFocusRequestBlursPreviousTest
  , spec "focus-request-damage" runFocusRequestDamageTest
  , spec "focus-request-first-pass" runFocusRequestFirstPassTest
  , spec "focus-request-idle" runFocusRequestIdleTest
  , spec "focus-request-order" runFocusRequestOrderTest
  , spec "focus-tab-closes-menus" runFocusTabClosesMenusTest
  , spec "focus-tab-claimed" runFocusTabClaimedTest
  , spec "focus-reveals" runFocusRevealsTest
  , spec "focus-next-previous" runFocusNextPreviousTest
  , spec "focus-clear-release" runFocusClearReleaseTest
  , spec "focus-text-command" runFocusTextCommandTest
  ]

inp :: Input
inp = withInputOff 400 400

-- | Run the script on @mk req@, where @req@ requests focus for the ids the
-- script's @ask f ids@ queues; @ask@ runs a frame on input @f@ and returns
-- whether it asked for another and the focus after it.
asking ::
  Context
  -> IORef Int
  -> (NanoUI () -> NanoUI a)
  -> (((Input -> Input) -> [WidgetId] -> Sim a (Bool, WidgetId)) -> Sim a ())
  -> IO ()
asking ctx failed mk script = do
  q <- newIORef []
  let ask f ids = liftIO (writeIORef q ids) >> frameWith f >> ((,) <$> wantsFrame <*> focusId)
  runSim ctx failed inp (mk (mapM_ requestFocus =<< liftIO (atomicModifyIORef' q ([],)))) (script ask)

ring :: Sim a Bool
ring = simContext >>= liftIO . getFocusVisible

-- | A field's selection, as (anchor, caret).
selectionOf :: Context -> WidgetId -> Sim a (Int, Int)
selectionOf ctx wid = liftIO ((\st -> fieldSelection st k (findSlot fieldText "" k st)) <$> getStore ctx)
  where
    k = intKey wid

-- | Every Tab-stop control kind can be focused by its response id.
runFocusRequestKindsTest :: Context -> IORef Int -> IO ()
runFocusRequestKindsTest _ failed =
  forM_ kinds $ \(name, widget) -> do
    ctx <- newContext
    asking ctx failed (\req -> column (respId <$> widget <* req)) $ \ask -> do
      wid <- result
      before <- focusId
      checkEq (name, False, wid) . (name, before == wid,) . snd =<< ask id [wid]
  where
    kinds :: [(String, NanoUI Response)]
    kinds =
      [ ("button", button' "Go")
      , ("checkbox", checkbox' "Check" (constVar False))
      , ("toggle", toggleSwitchWith' id (constVar False))
      , ("slider", fst <$> withValue 50 (slider' 0 100))
      , ("knob", fst <$> withValue 50 (knobWith' id 36 0 100))
      , ("select", fst <$> withValue 0 (select' ["One", "Two"]))
      , ("text input", fst <$> withValue "text" textInput')
      , ("search input", fst <$> withValue "text" (searchInput' "Search"))
      , ("numeric input", fst <$> withValue 5 numericInput')
      , ("combo box", fst <$> withValue "a" (comboBox' "Choose" ["a", "b"]))
      , ("text area", fst <$> withValue "text" textArea')
      , ("color picker", colorPicker' (constVar (colorRGBA 200 40 40 255)))
      ]

-- | Shift+Tab with nothing focused goes to the last Tab stop, and Tab and
-- Shift+Tab continue from the widget focused from code.
runFocusRequestTabOrderTest :: Context -> IORef Int -> IO ()
runFocusRequestTabOrderTest ctx failed =
  asking ctx failed (\req -> column (mapM (fmap respId . button') ["A", "B", "C"] <* req)) $ \ask -> do
    [a, b, c] <- result
    checkEq c =<< (tap (shift <> key KeyTab) >> focusId)
    forM_ [(key KeyTab, c), (shift <> key KeyTab, a)] $ \(chord', next) -> do
      checkEq b . snd =<< ask id [b]
      checkEq next =<< (tap chord' >> focusId)

-- | Requesting focus for a disabled field, a label, or an unknown id is a
-- no-op and asks for no frame. Requesting @WidgetId 0@ unfocuses the field,
-- and the loop can sleep.
runFocusRequestRefusedTest :: Context -> IORef Int -> IO ()
runFocusRequestRefusedTest ctx failed = do
  textRef <- newVar ""
  let disabled = disabledWhen True (withValue "" textInput')
      widgets = [fst <$> held textRef textInput', fst <$> disabled, label' "Label"]
  asking ctx failed (\req -> column (mapM (fmap respId) widgets <* req)) $ \ask -> do
    [field, off, lbl] <- result
    checkEq field =<< (ask id [field] >> typeText "a" >> focusId)
    forM_ [off, lbl, WidgetId 987654321] $ \w -> checkEq (False, field) =<< ask id [w]
    check =<< liftIO (textFieldActive ctx)
    checkEq (True, WidgetId 0) =<< ask id [WidgetId 0]
    check . not =<< liftIO (textFieldActive ctx)
    _ <- typeText "b" >> settle
    checkEq "a" =<< liftIO (readVarIO textRef)
    check . not =<< (getInput >>= \i -> liftIO (needsRedraw ctx i i))
    checkEq 0 =<< liftIO (getWakeAt ctx)

-- | With a modal open, widgets inside it can be focused from code and widgets
-- behind it cannot, even requested every frame, until the modal closes.
runFocusRequestModalTest :: Context -> IORef Int -> IO ()
runFocusRequestModalTest ctx failed = do
  open <- newIORef True
  let ui req = column $ do
        (page, _) <- withValue "page" textInput'
        shown <- liftIO (readIORef open)
        let dialog = modal' (constVar True) "Dialog" (fst <$> withValue "inside" textInput')
        inner <- if shown then snd <$> dialog else pure Nothing
        (respId page, respId <$> inner) <$ req
  asking ctx failed ui $ \ask -> do
    (page, inner) <- result
    checkJust inner $ \r -> do
      checkEq r =<< (ask id [r] >> frame >> focusId)
      checkEq [False, False, False] . map (== page) . map snd =<< replicateM 3 (ask id [page])
    liftIO (writeIORef open False)
    checkEq page . snd =<< (ask id [page] >> ask id [page])

-- | Losing focus to a request collapses a field's selection, and a search
-- field commits its debounced query, as with a click elsewhere.
runFocusRequestBlursPreviousTest :: Context -> IORef Int -> IO ()
runFocusRequestBlursPreviousTest ctx failed = do
  queryRef <- newVar ""
  let slow = defaultSearchInputConfig {sicDebounceMs = 600000}
      search = fst <$> held queryRef (searchInputConfigured' slow)
      other' = respId . fst <$> withValue "other" textInput'
  asking ctx failed (\req -> column ((,) <$> search <*> other' <* req)) $ \ask -> do
    (field, other) <- result
    _ <- ask id [respId field]
    check . not . respChanged . fst =<< typeText "abc"
    checkEq "abc" =<< liftIO (readVarIO queryRef)
    _ <- tap (cmdOrCtrl <> key 'a')
    checkEq (0, 3) =<< selectionOf ctx (respId field)
    _ <- ask id [other]
    checkEq (3, 3) =<< selectionOf ctx (respId field)
    check . respChanged . fst =<< frame
    checkEq other =<< focusId

-- | The request's frame repaints the old and new focus as clipped damage.
runFocusRequestDamageTest :: Context -> IORef Int -> IO ()
runFocusRequestDamageTest ctx failed = do
  let buttons = sequence [buttonWith' (fixedW 80) "Top" <* flex, buttonWith' (fixedW 80) "Bottom"]
  asking ctx failed (\req -> columnWith (fillW . fillH) (buttons <* req)) $ \ask -> do
    [a, b] <- result
    _ <- ask id [respId a] >> settle
    liftIO (writeIORef (ctxPaintFull ctx) False)
    checkEq (respId b) . snd =<< ask id [respId b]
    dmg <- liftIO (takeDamage ctx)
    pieces <- liftIO (takeDamagePieces ctx)
    check (all (clipCovers dmg . respRect) [a, b])
    check (null pieces || all (\r -> any (`covers` respRect r) pieces) [a, b])

-- | A request from a view pass that a hook write reruns still takes effect.
runFocusRequestFirstPassTest :: Context -> IORef Int -> IO ()
runFocusRequestFirstPassTest ctx failed = do
  r <- evalUi ctx inp . column $ do
    askedVar <- useVar False
    asked <- readVar askedVar
    r <- respId . fst <$> withValue "" textInput'
    r <$ unless asked (requestFocus r >> writeVar askedVar True)
  assertEq failed r =<< getFocusId ctx

-- | Requesting the already focused widget every frame costs no frames. A
-- click on it hides the ring, and later requests do not bring it back.
runFocusRequestIdleTest :: Context -> IORef Int -> IO ()
runFocusRequestIdleTest ctx failed = do
  let ui = column (fst <$> withValue "" textInput' >>= \r -> r <$ requestFocus (respId r))
  (r0, _, first) <- runFrame ctx inp ui
  r <- warmup2 ctx inp ui
  (_, _, dirty) <- runFrame ctx inp ui
  assertEq failed (True, False) (first, dirty)
  assertEq failed (respId r0) =<< getFocusId ctx
  assert failed . not =<< needsRedraw ctx inp inp
  _ <- runClick ctx inp ui (centerOf r)
  _ <- warmup2 ctx inp {inputMousePos = centerOf r} ui
  assert failed . not =<< getFocusVisible ctx
  assertEq failed (respId r0) =<< getFocusId ctx

-- | A request can name a later widget, the frame's last request wins, and a
-- Tab in that frame continues from it. The field focused shows the ring and
-- takes typing from the next frame, not the requesting one.
runFocusRequestOrderTest :: Context -> IORef Int -> IO ()
runFocusRequestOrderTest ctx failed = do
  textRef <- newVar "hello"
  let widgets = (:) <$> (respId . fst <$> held textRef textInput') <*> mapM (fmap respId . button') ["A", "B"]
  asking ctx failed (\req -> column (req >> widgets)) $ \ask -> do
    [field, a, b] <- result
    checkEq (WidgetId 0) =<< focusId
    checkEq (True, field) =<< ask (\i -> i {inputChars = "y"}) [field]
    check =<< ring
    _ <- typeText "!"
    checkEq "hello!" =<< liftIO (readVarIO textRef)
    let asks = [ask id [a, WidgetId 0], ask id [WidgetId 0, b, a], ask tabInp [field]]
    checkEq [WidgetId 0, a, a] . map snd =<< sequence asks

-- | Tab or a focus request away from an open select or a field's edit menu
-- closes it, and so does a right press on the field, which focuses it and
-- opens its edit menu. The arrows then move the caret of the field and leave
-- the select's value alone.
runFocusTabClosesMenusTest :: Context -> IORef Int -> IO ()
runFocusTabClosesMenusTest ctx failed = do
  index <- newVar (0 :: Int)
  -- The field above the select, where the dropdown does not cover it.
  let text' = fst <$> withValue "text" textInput'
      widgets = (,) <$> text' <*> (fst <$> held index (select' ["One", "Two", "Three"]))
      selectOpen = liftIO (anySelectOpen <$> getStore ctx)
      menuOpen = liftIO (isJust <$> getsInteraction ctx isTextInputMenu)
  asking ctx failed (\req -> column (widgets <* req)) $ \ask -> do
    (field, sel) <- result
    let leaves = [void (pressKey KeyTab), void (rightClick (On field)), void (ask id [respId field])]
    forM_ leaves $ \leave -> do
      check =<< (click (On sel) >> selectOpen)
      leave
      checkEq (respId field) =<< focusId
      check . not =<< selectOpen
      _ <- pressKey KeyRight >> frame
      checkEq 0 =<< liftIO (readVarIO index)
    forM_ [void (pressKey KeyTab), void (ask id [respId sel])] $ \leave -> do
      check =<< (rightClick (On field) >> menuOpen)
      checkEq (respId sel) =<< (leave >> focusId)
      check . not =<< menuOpen

-- | Tab, Shift+Tab and a focus request scroll the widget they focus into
-- its scroller's view, from below the fold, back up when Tab wraps, and
-- from above. Around nested scrollers, the outer one shows the widget where
-- the inner one moved it.
runFocusRevealsTest :: Context -> IORef Int -> IO ()
runFocusRevealsTest ctx failed = do
  target <- newIORef Nothing
  let labels = map T.singleton ['A' .. 'J']
      buttons = scrollArea (fixedH 100 . fillW) (column (mapM (fmap respId . button') labels))
      ui = (mapM_ requestFocus =<< liftIO (readIORef target)) >> buttons
      showsIn c sid = do
        rect <- getPrevRect c =<< getFocusId c
        metrics <- getScrollMetrics c sid
        pure $ case (rect, scrollViewport <$> metrics) of
          (Just (Rect _ y _ h), Just (Rect _ vy _ vh)) -> y >= vy - 0.5 && y + h <= vy + vh + 0.5
          _ -> False
      shown = writeIORef target Nothing >> evalUi ctx inp ui >>= showsIn ctx . fst
  nested <- newContext
  let filler = columnWith (fixedH 150 . fillW) (pure ())
      outerUi = scrollArea (fixedH 120 . fillW) (column (filler *> buttons <* filler <* filler))
  _ <- warmup2 nested inp outerUi
  replicateM_ 10 (runFrame nested (tabInp inp) outerUi)
  (outer, (inner, _)) <- evalUi nested inp outerUi
  assert failed =<< ((&&) <$> showsIn nested outer <*> showsIn nested inner)
  (_, ids) <- warmup2 ctx inp ui
  forM_ [1 .. 11 :: Int] $ \i -> runFrame ctx (tabInp inp) ui >> (assertEq failed (i, True) . (i,) =<< shown)
  _ <- runFrame ctx (chordInp (shift <> key KeyTab) inp) ui
  assert failed =<< shown
  writeIORef target (Just (ids !! 4))
  _ <- runFrame ctx inp ui
  assertEq failed (ids !! 4) =<< getFocusId ctx
  assert failed =<< shown

-- | A focused custom widget that claims Tab ('KeysAll', or 'KeysOnly'
-- naming the chord) reads it and keeps focus; one claiming other keys lets
-- Tab move focus on.
runFocusTabClaimedTest :: Context -> IORef Int -> IO ()
runFocusTabClaimedTest _ failed = do
  let claims = [(KeysAll, True), (KeysOnly [key KeyTab], True), (KeysOnly [shift <> key KeyTab], False)]
  forM_ (claims ++ [(KeysNavigate, False)]) $
    \(claim, keeps) -> do
      ctx <- newContext
      let ui = column $ do
            let custom = defaultCustomWidgetSpec {widgetFocusable = True, widgetKeys = claim}
            (r, _) <- customWidget custom {widgetLayout = fixedWH 100 30 defaultLayout}
            tabbed <- focusedKeyPressed (respId r) KeyTab
            _ <- button' "Next"
            pure (respId r, tabbed)
      runSim ctx failed inp ui $ do
        (w, _) <- pressTab 1 >> frame
        checkEq w =<< focusId
        check . snd =<< pressKey KeyTab
        checkEq (claim, keeps) . (claim,) . (== w) =<< focusId

-- | 'focusNext' and 'focusPrevious' act like Tab and Shift+Tab at the end of
-- the frame, wrapping at both ends; 'isFocused' reports the focused widget.
-- 'requestFocus' with 'currentId' targets the next declared widget.
runFocusNextPreviousTest :: Context -> IORef Int -> IO ()
runFocusNextPreviousTest ctx failed = do
  move <- newIORef (pure ())
  let ui = column $ do
        join (liftIO (readIORef move))
        ids <- mapM (fmap respId . button') ["A", "B", "C"]
        (,) ids <$> mapM isFocused ids
      step m = liftIO (writeIORef move m) >> frame >> liftIO (writeIORef move (pure ())) >> snd <$> frame
  runSim ctx failed inp ui $ do
    ([a, _, c], _) <- result
    focused <- mapM step [focusNext, focusNext, focusPrevious, focusPrevious]
    checkEq [[True, False, False], [False, True, False], [True, False, False], [False, False, True]] focused
    checkEq c =<< focusId
    check =<< ring
    checkEq [True, False, False] =<< step (requestFocus =<< currentId)
    checkEq a =<< focusId

-- | 'clearFocus' unfocuses at the end of the frame and collapses the field's
-- selection; 'releaseFocus' unfocuses immediately and keeps the selection.
runFocusClearReleaseTest :: Context -> IORef Int -> IO ()
runFocusClearReleaseTest ctx failed = do
  act <- newIORef (const (pure ()))
  let ui = column $ do
        (r, _) <- withValue ("hello" :: T.Text) textInput'
        liftIO (readIORef act) >>= ($ respId r)
        (,) (respId r) <$> focusedWidget
      setAct = liftIO . writeIORef act
  runSim ctx failed inp ui $ do
    (field, _) <- pressTab 1 >> frame
    _ <- tap (cmdOrCtrl <> key 'a')
    checkEq (0, 5) =<< selectionOf ctx field
    setAct (const clearFocus)
    checkEq (field, field) =<< frame
    checkEq (WidgetId 0) =<< focusId
    checkEq (5, 5) =<< selectionOf ctx field
    setAct (const (pure ()))
    _ <- pressKey KeyTab >> tap (cmdOrCtrl <> key 'a')
    setAct releaseFocus
    checkEq (field, WidgetId 0) =<< frame
    checkEq (0, 5) =<< selectionOf ctx field

-- | A text command run from code focuses the field like 'requestFocus': at
-- the end of the frame, with the ring, and never when disabled. What Select
-- All selects stays selected for typing to replace.
runFocusTextCommandTest :: Context -> IORef Int -> IO ()
runFocusTextCommandTest ctx failed = do
  offRef <- newIORef False
  run <- newIORef False
  ref <- newVar "hello"
  let ui = column $ do
        off <- liftIO (readIORef offRef)
        (r, _) <- disabledWhen off (held ref textInput')
        whenM (liftIO (readIORef run)) (runTextCommand (respId r) SelectAll)
        (,) (respId r) <$> focusedWidget
  runSim ctx failed inp ui $ do
    (field, _) <- result
    liftIO (writeIORef run True)
    checkEq (field, WidgetId 0) =<< frame
    checkEq field =<< focusId
    check =<< ring
    liftIO (writeIORef run False)
    checkEq "Z" =<< (typeText "Z" >> liftIO (readVarIO ref))
    focusOn (WidgetId 0)
    liftIO (writeIORef offRef True >> writeIORef run True)
    checkEq (WidgetId 0) =<< (frame >> focusId)

-- | Pressing a focusable canvas focuses it without showing the Tab ring, so
-- 'focusedKeyPressed' matches for it; pressing a plain canvas clears focus,
-- so it no longer matches.
runCanvasFocusClickTest :: Context -> IORef Int -> IO ()
runCanvasFocusClickTest ctx failed = do
  heard <- newIORef False
  let canvasOf focusable =
        canvasConfigured defaultCanvasConfig {canvasLayout = size, canvasFocusable = focusable} (\_ -> pure ())
      size = fixedWH 100 40 defaultLayout
      ui = column $ do
        live <- canvasOf True
        still <- canvasOf False
        whenM (focusedKeyPressed (respId live) KeyRight) (liftIO (writeIORef heard True))
        (live, still,) <$> focusedWidget
      focusedAfter t = (\(_, _, f) -> f) <$> (click (On t) >> frame)
      heardRight = liftIO (writeIORef heard False) >> pressKey KeyRight >> liftIO (readIORef heard)
  runSim ctx failed inp ui $ do
    (live, still, _) <- result
    checkEq (respId live) =<< focusedAfter live
    check . not =<< ring
    check =<< heardRight
    checkEq (WidgetId 0) =<< focusedAfter still
    check . not =<< heardRight
