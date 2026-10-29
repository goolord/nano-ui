{-# OPTIONS_GHC -O0 #-}
module Cases.Focus (tests) where

import Spec
import Data.Text qualified as T
import NanoUI.Internal.Context (getFocusVisible, intKey)
import NanoUI.Internal.Store (fieldSelection, fieldText, findSlot)
import NanoUI.Monad (focusedWidget, releaseFocus)
import NanoUI.Shortcut
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ simSpec "canvas-focus-click" 400 400 canvasFocusUi canvasFocusClick
  , spec "focus-request-kinds" runFocusRequestKindsTest
  , varSimSpec "focus-request-refused" 400 400 "" refusedUi focusRequestRefused
  , varSimSpec "focus-request-modal" 400 400 True modalUi focusRequestModal
  , varSimSpec "focus-request-blurs-previous" 400 400 "" blursUi focusRequestBlursPrevious
  , spec "focus-request-damage" runFocusRequestDamageTest
  , spec "focus-request-first-pass" runFocusRequestFirstPassTest
  , spec "focus-request-idle" runFocusRequestIdleTest
  , spec "focus-request-order" runFocusRequestOrderTest
  , varSimSpec "focus-tab-closes-menus" 400 400 0 closesMenusUi focusTabClosesMenus
  , spec "focus-tab-claimed" runFocusTabClaimedTest
  , spec "focus-reveals" runFocusRevealsTest
  , spec "focus-next-previous" runFocusNextPreviousTest
  , simSpec "focus-clear-release" 400 400 clearReleaseUi focusClearRelease
  , varSimSpec "focus-text-command" 400 400 False textCommandUi focusTextCommand
  ]

inp :: Input
inp = withInputOff 400 400

-- | One frame that requests focus for @ids@ after the view: whether it asked
-- for another frame, and the focus after it.
askFocus :: [WidgetId] -> Sim a (Bool, WidgetId)
askFocus ids = afterView (mapM_ requestFocus ids) >> ((,) <$> wantsFrame <*> focusedId)

-- | A field's selection, as (anchor, caret).
selectionOf :: WidgetId -> Sim a (Int, Int)
selectionOf wid = onContext (fmap (\st -> fieldSelection st k (findSlot fieldText "" k st)) . getStore)
  where
    k = intKey wid

-- | Every control can be focused by its response id. A radio group's
-- response carries an option's id, not the group's Tab stop.
runFocusRequestKindsTest :: Context -> IORef Int -> IO ()
runFocusRequestKindsTest _ failed =
  forM_ (filter ((/= "radio") . fst) everyControl) $ \(name, widget) -> do
    ctx <- newContext
    runSim ctx failed inp (column (respId . fst <$> widget)) $ do
      wid <- result
      before <- focusedId
      checkEq (name, False, wid) . (name, before == wid,) . snd =<< askFocus [wid]

-- | Requesting focus for a disabled field, a label, or an unknown id is a
-- no-op and asks for no frame. Requesting @WidgetId 0@ unfocuses the field,
-- and the loop can sleep.
refusedUi :: Var T.Text -> NanoUI [WidgetId]
refusedUi ref = column (mapM (fmap respId) [fst <$> held ref textInput', fst <$> disabled, label' "Label"])
  where
    disabled = disabledWhen True (withValue "" textInput')

focusRequestRefused :: Var T.Text -> Sim [WidgetId] ()
focusRequestRefused ref = do
  [field, off, lbl] <- result
  checkEq field =<< (askFocus [field] >> typeText "a" >> focusedId)
  forM_ [off, lbl, WidgetId 987654321] $ \w -> checkEq (False, field) =<< askFocus [w]
  check =<< onContext textFieldActive
  checkEq (True, WidgetId 0) =<< askFocus [WidgetId 0]
  check . not =<< onContext textFieldActive
  _ <- typeText "b" >> settle
  checkEq "a" =<< readV ref
  check . not =<< wouldRedraw id
  checkEq 0 =<< wakeTime

-- | With a modal open, widgets inside it can be focused from code and widgets
-- behind it cannot, even requested every frame, until the modal closes.
modalUi :: Var Bool -> NanoUI (WidgetId, Maybe WidgetId)
modalUi open = column $ do
  (page, _) <- withValue "page" textInput'
  shown <- readVar open
  let dialog = modal' (constVar True) "Dialog" (fst <$> withValue "inside" textInput')
  inner <- if shown then snd <$> dialog else pure Nothing
  pure (respId page, respId <$> inner)

focusRequestModal :: Var Bool -> Sim (WidgetId, Maybe WidgetId) ()
focusRequestModal open = do
  (page, inner) <- result
  checkJust inner $ \r -> do
    checkEq r =<< (askFocus [r] >> frame >> focusedId)
    checkEq [False, False, False] . map ((== page) . snd) =<< replicateM 3 (askFocus [page])
  writeV open False
  checkEq page . snd =<< (askFocus [page] >> askFocus [page])

-- | Losing focus to a request collapses a field's selection, and a search
-- field commits its debounced query, as with a click elsewhere.
blursUi :: Var T.Text -> NanoUI (Response, WidgetId)
blursUi ref = column ((,) <$> search <*> (respId . fst <$> withValue "other" textInput'))
  where
    slow = defaultSearchInputConfig {sicDebounceMs = 600000}
    search = fst <$> held ref (searchInputConfigured' slow)

focusRequestBlursPrevious :: Var T.Text -> Sim (Response, WidgetId) ()
focusRequestBlursPrevious ref = do
  (field, other) <- result
  _ <- askFocus [respId field]
  check . not . respChanged . fst =<< typeText "abc"
  checkEq "abc" =<< readV ref
  _ <- chord (cmdOrCtrl <> key 'a')
  checkEq (0, 3) =<< selectionOf (respId field)
  _ <- askFocus [other]
  checkEq (3, 3) =<< selectionOf (respId field)
  check . respChanged . fst =<< frame
  checkEq other =<< focusedId

-- | The request's frame repaints the old and new focus as clipped damage.
runFocusRequestDamageTest :: Context -> IORef Int -> IO ()
runFocusRequestDamageTest ctx failed = do
  let buttons = sequence [buttonWith' (fixedW 80) "Top" <* flex, buttonWith' (fixedW 80) "Bottom"]
  runSim ctx failed inp (columnWith (fillW . fillH) buttons) $ do
    [a, b] <- result
    _ <- askFocus [respId a] >> settle
    liftIO (writeIORef (ctxPaintFull ctx) False)
    checkEq (respId b) . snd =<< askFocus [respId b]
    dmg <- frameDamage
    pieces <- onContext takeDamagePieces
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
  q <- newIORef []
  -- The requests run before the widgets they name.
  let requests = mapM_ requestFocus =<< liftIO (atomicModifyIORef' q ([],))
      widgets = (:) <$> (respId . fst <$> held textRef textInput') <*> mapM (fmap respId . button') ["A", "B"]
      ask f ids = liftIO (writeIORef q ids) >> frameWith f >> ((,) <$> wantsFrame <*> focusedId)
  runSim ctx failed inp (column (requests >> widgets)) $ do
    [field, a, b] <- result
    checkEq (WidgetId 0) =<< focusedId
    checkEq (True, field) =<< ask (\i -> i {inputChars = "y"}) [field]
    check =<< focusRing
    _ <- typeText "!"
    checkEq "hello!" =<< readV textRef
    let asks = [ask id [a, WidgetId 0], ask id [WidgetId 0, b, a], ask tabInp [field]]
    checkEq [WidgetId 0, a, a] . map snd =<< sequence asks

-- | Tab or a focus request away from an open select or a field's edit menu
-- closes it, and so does a right press on the field, which focuses it and
-- opens its edit menu. The arrows then move the caret of the field and leave
-- the select's value alone.
closesMenusUi :: Var Int -> NanoUI (Response, Response)
closesMenusUi index = column ((,) <$> field <*> (fst <$> held index (select' ["One", "Two", "Three"])))
  where
    -- Above the select, where the dropdown does not cover it.
    field = fst <$> withValue "text" textInput'

focusTabClosesMenus :: Var Int -> Sim (Response, Response) ()
focusTabClosesMenus index = do
  (field, sel) <- result
  -- "Three" shows only in the open dropdown, "Paste" only in the edit menu.
  let leaves = [void (pressKey KeyTab), void (rightClick (On field)), void (askFocus [respId field])]
  forM_ leaves $ \leave -> do
    check =<< (click (On sel) >> labelShown "Three")
    leave
    checkEq (respId field) =<< focusedId
    check . not =<< labelShown "Three"
    _ <- pressKey KeyRight >> frame
    checkEq 0 =<< readV index
  forM_ [void (pressKey KeyTab), void (askFocus [respId sel])] $ \leave -> do
    check =<< (rightClick (On field) >> labelShown "Paste")
    checkEq (respId sel) =<< (leave >> focusedId)
    check . not =<< labelShown "Paste"

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
        checkEq w =<< focusedId
        check . snd =<< pressKey KeyTab
        checkEq (claim, keeps) . (claim,) . (== w) =<< focusedId

-- | 'focusNext' and 'focusPrevious' act like Tab and Shift+Tab at the end of
-- the frame, wrapping at both ends; 'isFocused' reports the focused widget.
-- 'requestFocus' with 'currentId' targets the next declared widget. Shift+Tab
-- with nothing focused goes to the last Tab stop, and Tab and Shift+Tab
-- continue from the widget focused from code.
runFocusNextPreviousTest :: Context -> IORef Int -> IO ()
runFocusNextPreviousTest ctx failed = do
  -- The move runs before the widgets, for 'currentId'.
  move <- newIORef (pure ())
  let ui = column $ do
        join (liftIO (readIORef move))
        ids <- mapM (fmap respId . button') ["A", "B", "C"]
        (,) ids <$> mapM isFocused ids
      step m = liftIO (writeIORef move m) >> frame >> liftIO (writeIORef move (pure ())) >> snd <$> frame
  runSim ctx failed inp ui $ do
    ([a, b, c], _) <- result
    checkEq c =<< (chord (shift <> key KeyTab) >> focusedId)
    focused <- mapM step [focusNext, focusPrevious, focusPrevious]
    checkEq [[True, False, False], [False, False, True], [False, True, False]] focused
    check =<< focusRing
    checkEq [True, False, False] =<< step (requestFocus =<< currentId)
    checkEq a =<< focusedId
    forM_ [(key KeyTab, c), (shift <> key KeyTab, a)] $ \(next, after) -> do
      checkEq [False, True, False] =<< step (requestFocus b)
      checkEq after =<< (chord next >> focusedId)

clearReleaseUi :: NanoUI WidgetId
clearReleaseUi = column (respId . fst <$> withValue "hello" textInput')

-- | 'clearFocus' unfocuses at the end of the frame and collapses the field's
-- selection; 'releaseFocus' unfocuses immediately and keeps the selection.
focusClearRelease :: Sim WidgetId ()
focusClearRelease = do
  field <- pressTab 1 >> frame
  _ <- chord (cmdOrCtrl <> key 'a')
  checkEq (0, 5) =<< selectionOf field
  checkEq field =<< afterView (clearFocus >> focusedWidget)
  checkEq (WidgetId 0) =<< focusedId
  checkEq (5, 5) =<< selectionOf field
  _ <- pressKey KeyTab >> chord (cmdOrCtrl <> key 'a')
  checkEq (WidgetId 0) =<< afterView (releaseFocus field >> focusedWidget)
  checkEq (0, 5) =<< selectionOf field

-- | A text command run from code focuses the field like 'requestFocus': at
-- the end of the frame, with the ring, and never when disabled. What Select
-- All selects stays selected for typing to replace.
textCommandUi :: Var Bool -> NanoUI (Response, T.Text)
textCommandUi off = column (readVar off >>= \o -> disabledWhen o (withValue "hello" textInput'))

focusTextCommand :: Var Bool -> Sim (Response, T.Text) ()
focusTextCommand off = do
  field <- respId . fst <$> result
  checkEq (WidgetId 0) =<< afterView (runTextCommand field SelectAll >> focusedWidget)
  checkEq field =<< focusedId
  check =<< focusRing
  checkEq "Z" . snd =<< typeText "Z"
  focusOn (WidgetId 0)
  writeV off True
  checkEq (WidgetId 0) =<< (afterView (runTextCommand field SelectAll) >> frame >> focusedId)

-- | Pressing a focusable canvas focuses it without showing the Tab ring, so
-- 'focusedKeyPressed' matches for it; pressing a plain canvas clears focus,
-- so it no longer matches.
canvasFocusUi :: NanoUI (Response, Response, WidgetId, Bool)
canvasFocusUi = column $ do
  live <- canvasOf True
  still <- canvasOf False
  (live, still,,) <$> focusedWidget <*> focusedKeyPressed (respId live) KeyRight
  where
    canvasOf focusable = canvasConfigured (config focusable) (\_ -> pure ())
    config focusable = defaultCanvasConfig {canvasLayout = size, canvasFocusable = focusable}
    size = fixedWH 100 40 defaultLayout

canvasFocusClick :: Sim (Response, Response, WidgetId, Bool) ()
canvasFocusClick = do
  (live, still, _, _) <- result
  let focusedAfter t = (\(_, _, f, _) -> f) <$> (click (On t) >> frame)
      heardRight = (\(_, _, _, h) -> h) <$> pressKey KeyRight
  checkEq (respId live) =<< focusedAfter live
  check . not =<< focusRing
  check =<< heardRight
  checkEq (WidgetId 0) =<< focusedAfter still
  check . not =<< heardRight
