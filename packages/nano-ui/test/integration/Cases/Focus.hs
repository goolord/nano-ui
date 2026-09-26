module Cases.Focus (tests) where

import Spec
import Data.Maybe (isJust)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..), InteractionState (..), getFocusVisible, getsInteraction, intKey)
import NanoUI.Monad (focusedWidget, releaseFocus)
import NanoUI.Internal.Store (anySelectOpen, fieldSelection, fieldText, findSlot)
import NanoUI.Shortcut

tests :: [Spec]
tests =
  [ spec "focus-request-types" runFocusRequestTypesTest
  , spec "focus-request-kinds" runFocusRequestKindsTest
  , spec "focus-request-tab-order" runFocusRequestTabOrderTest
  , spec "focus-request-none" runFocusRequestNoneTest
  , spec "focus-request-refused" runFocusRequestRefusedTest
  , spec "focus-request-modal" runFocusRequestModalTest
  , spec "focus-request-blurs-previous" runFocusRequestBlursPreviousTest
  , spec "focus-request-damage" runFocusRequestDamageTest
  , spec "focus-request-first-pass" runFocusRequestFirstPassTest
  , spec "focus-request-idle" runFocusRequestIdleTest
  , spec "focus-request-order" runFocusRequestOrderTest
  , spec "focus-request-closes-menus" runFocusRequestClosesMenusTest
  , spec "focus-next-previous" runFocusNextPreviousTest
  , spec "focus-clear-release" runFocusClearReleaseTest
  , spec "focus-text-command" runFocusTextCommandTest
  ]

inp :: Input
inp = withInputOff 400 400

-- | The view @mk req@, where @req@ requests focus for queued ids, and an
-- action that queues ids, runs a frame, and returns the dirty flag and focus.
asking :: Context -> (NanoUI () -> NanoUI a) -> IO (NanoUI a, [WidgetId] -> Input -> IO (Bool, WidgetId))
asking ctx mk = do
  q <- newIORef []
  let ui = mk (mapM_ requestFocus =<< uiIO (atomicModifyIORef' q ([],)))
  pure (ui, \ids i -> writeIORef q ids >> runFrame ctx i ui >>= \(_, _, _, dirty) -> (dirty,) <$> getFocusId ctx)

-- | A field focused from code shows the ring and accepts typing from the
-- next frame, not the requesting one.
runFocusRequestTypesTest :: Context -> IORef Int -> IO ()
runFocusRequestTypesTest ctx failed = do
  refs <- replicateM 2 (newIORef "")
  (ui, ask) <- asking ctx $ \req -> column (mapM (fmap (respId . fst) . (`held` textInput')) refs <* req)
  [_, b] <- warmup2 ctx inp ui
  assertEq failed (WidgetId 0) =<< getFocusId ctx
  assertEq failed (True, b) =<< ask [b] inp {inputChars = "y"}
  assert failed =<< getFocusVisible ctx
  _ <- runFrame ctx inp {inputChars = "x"} ui
  assertEq failed ["", "x"] =<< mapM readIORef refs

-- | Every Tab-stop control kind can be focused by its response id.
runFocusRequestKindsTest :: Context -> IORef Int -> IO ()
runFocusRequestKindsTest _ failed =
  forM_ kinds $ \(name, widget) -> do
    ctx <- newContext
    (ui, ask) <- asking ctx $ \req -> column (respId <$> widget <* req)
    wid <- warmup2 ctx inp ui
    before <- getFocusId ctx
    assertEq failed (name, False, wid) . (name, before == wid,) . snd =<< ask [wid] inp
  where
    kinds :: [(String, NanoUI Response)]
    kinds =
      [ ("button", button' "Go"), ("checkbox", fst <$> checkbox' "Check" False), ("toggle", fst <$> toggleSwitchWith' id False)
      , ("slider", fst <$> slider' 0 100 50), ("knob", fst <$> knobWith' id 36 0 100 50), ("select", fst <$> select' ["One", "Two"] 0)
      , ("text input", fst <$> textInput' "text"), ("search input", fst <$> searchInput' "Search" "text"), ("numeric input", fst <$> numericInput' 5)
      , ("combo box", fst <$> comboBox' "Choose" ["a", "b"] "a"), ("text area", fst <$> textArea' "text")
      , ("color picker", fst <$> colorPicker' (colorRGBA 200 40 40 255))
      ]

-- | Tab and Shift+Tab continue from the widget focused from code.
runFocusRequestTabOrderTest :: Context -> IORef Int -> IO ()
runFocusRequestTabOrderTest ctx failed = do
  (ui, ask) <- asking ctx $ \req -> column (mapM (fmap respId . button') ["A", "B", "C"] <* req)
  [a, b, c] <- warmup2 ctx inp ui
  forM_ [(key KeyTab, c), (shift <> key KeyTab, a)] $ \(chord, next) -> do
    assertEq failed b . snd =<< ask [b] inp
    _ <- runFrame ctx (chordInp chord inp) ui
    assertEq failed next =<< getFocusId ctx

-- | Requesting @WidgetId 0@ unfocuses the field, and the loop can sleep.
runFocusRequestNoneTest :: Context -> IORef Int -> IO ()
runFocusRequestNoneTest ctx failed = do
  textRef <- newIORef ""
  (ui, ask) <- asking ctx $ \req -> column (fst <$> held textRef textInput' <* req)
  r <- warmup2 ctx inp ui
  _ <- ask [respId r] inp
  _ <- runFrame ctx inp {inputChars = "a"} ui
  assert failed =<< textFieldActive ctx
  assertEq failed (True, WidgetId 0) =<< ask [WidgetId 0] inp
  assert failed . not =<< textFieldActive ctx
  _ <- runFrame ctx inp {inputChars = "b"} ui
  assertEq failed "a" =<< readIORef textRef
  _ <- warmup2 ctx inp ui
  assert failed . not =<< needsRedraw ctx inp inp
  assertEq failed 0 =<< getWakeAt ctx

-- | A disabled field, a label and an unknown id refuse focus without
-- requesting a frame.
runFocusRequestRefusedTest :: Context -> IORef Int -> IO ()
runFocusRequestRefusedTest ctx failed = do
  (ui, ask) <- asking ctx $ \req -> column $ do
    ids <- sequence [respId . fst <$> textInput' "", respId . fst <$> disabledWhen True (textInput' ""), respId <$> label' "Label"]
    ids <$ req
  [field, off, lbl] <- warmup2 ctx inp ui
  _ <- ask [field] inp >> warmup ctx inp ui
  assertEq failed field =<< getFocusId ctx
  forM_ [off, lbl, WidgetId 987654321] $ \w -> assertEq failed (False, field) =<< ask [w] inp

-- | With a modal open, widgets inside it can be focused and widgets behind it
-- cannot.
runFocusRequestModalTest :: Context -> IORef Int -> IO ()
runFocusRequestModalTest ctx failed = do
  (ui, ask) <- asking ctx $ \req -> column $ do
    (page, _) <- textInput' "page"
    (_, inner) <- modal True "Dialog" (fst <$> textInput' "inside")
    (respId page, respId <$> inner) <$ req
  (page, inner) <- warmup2 ctx inp ui
  assertJust failed inner $ \r -> do
    _ <- ask [r] inp >> warmup ctx inp ui
    assertEq failed r =<< getFocusId ctx
    assertEq failed (False, r) =<< ask [page] inp

-- | Losing focus to a request collapses a field's selection, and a search
-- field commits its debounced query, as with a click elsewhere.
runFocusRequestBlursPreviousTest :: Context -> IORef Int -> IO ()
runFocusRequestBlursPreviousTest ctx failed = do
  queryRef <- newIORef ""
  let cfg = defaultSearchInputConfig {sicDebounceMs = 600000}
  (ui, ask) <- asking ctx $ \req ->
    column ((,) <$> (fst <$> held queryRef (searchInputConfigured' cfg)) <*> (respId . fst <$> textInput' "other") <* req)
  (search, other) <- warmup2 ctx inp ui
  let sid = intKey (respId search)
      selection = (\st -> fieldSelection st sid (findSlot fieldText "" sid st)) <$> getStore ctx
  _ <- ask [respId search] inp
  (typed, _) <- evalUi ctx inp {inputChars = "abc"} ui
  assertEq failed "abc" =<< readIORef queryRef
  assert failed (not (respChanged typed))
  _ <- runFrame ctx (chordInp (ctrl <> key 'a') inp) ui
  assertEq failed (0, 3) =<< selection
  _ <- ask [other] inp
  assertEq failed (3, 3) =<< selection
  (blurred, _) <- evalUi ctx inp ui
  assert failed (respChanged blurred)
  assertEq failed other =<< getFocusId ctx

-- | The request's frame repaints the old and new focus as clipped damage.
runFocusRequestDamageTest :: Context -> IORef Int -> IO ()
runFocusRequestDamageTest ctx failed = do
  (ui, ask) <- asking ctx $ \req ->
    columnWith (fillW . fillH) (sequence [buttonWith' (fixedW 80) "Top" <* flex, buttonWith' (fixedW 80) "Bottom"] <* req)
  [a, b] <- warmup2 ctx inp ui
  _ <- ask [respId a] inp >> warmup2 ctx inp ui
  writeIORef (ctxPaintFull ctx) False
  assertEq failed (respId b) . snd =<< ask [respId b] inp
  dmg <- takeDamage ctx
  pieces <- takeDamagePieces ctx
  assert failed (all (clipCovers dmg . respRect) [a, b])
  assert failed (null pieces || all (\r -> any (`covers` respRect r) pieces) [a, b])

-- | A request from a view pass that a hook write reruns still takes effect.
runFocusRequestFirstPassTest :: Context -> IORef Int -> IO ()
runFocusRequestFirstPassTest ctx failed = do
  r <- evalUi ctx inp . column $ do
    (asked, setAsked) <- useFlag False
    r <- respId . fst <$> textInput' ""
    r <$ unless asked (requestFocus r >> setAsked True)
  assertEq failed r =<< getFocusId ctx

-- | Requesting the already focused widget every frame costs no frames. A
-- click on it hides the ring, and later requests do not bring it back.
runFocusRequestIdleTest :: Context -> IORef Int -> IO ()
runFocusRequestIdleTest ctx failed = do
  let ui = column (fst <$> textInput' "" >>= \r -> r <$ requestFocus (respId r))
  (r0, _, _, first) <- runFrame ctx inp ui
  r <- warmup2 ctx inp ui
  (_, _, _, dirty) <- runFrame ctx inp ui
  assertEq failed (True, False) (first, dirty)
  assertEq failed (respId r0) =<< getFocusId ctx
  assert failed . not =<< needsRedraw ctx inp inp
  _ <- runClick ctx inp ui (centerOf r)
  _ <- warmup2 ctx inp {inputMousePos = centerOf r} ui
  assert failed . not =<< getFocusVisible ctx
  assertEq failed (respId r0) =<< getFocusId ctx

-- | A request can name a later widget, the frame's last request wins, typing
-- lands after the frame ends, and a Tab in that frame continues from it.
runFocusRequestOrderTest :: Context -> IORef Int -> IO ()
runFocusRequestOrderTest ctx failed = do
  textRef <- newIORef "hello"
  (ui, ask) <- asking ctx $ \req ->
    column (req >> sequence [respId . fst <$> held textRef textInput', respId <$> button' "A", respId <$> button' "B"])
  [field, a, b] <- warmup2 ctx inp ui
  let focusAfter i ids = snd <$> ask ids i
  assertEq failed field =<< focusAfter inp [field]
  _ <- runFrame ctx inp {inputChars = "!"} ui
  assertEq failed "hello!" =<< readIORef textRef
  assertEq failed (WidgetId 0) =<< focusAfter inp [a, WidgetId 0]
  assertEq failed a =<< focusAfter inp [WidgetId 0, b, a]
  assertEq failed a =<< focusAfter (tabInp inp) [field]

-- | Focusing from code closes an open dropdown and the focused field's
-- context menu.
runFocusRequestClosesMenusTest :: Context -> IORef Int -> IO ()
runFocusRequestClosesMenusTest ctx failed = do
  (ui, ask) <- asking ctx $ \req ->
    column ((,,) <$> (fst <$> select' ["One", "Two", "Three"] 0) <*> (fst <$> textInput' "text") <*> (respId <$> button' "B") <* req)
  (sel, field, b) <- warmup2 ctx inp ui
  let closes open = do
        _ <- warmup2 ctx inp ui
        assert failed =<< open
        assertEq failed b . snd =<< ask [b] inp
        assert failed . not =<< open
  _ <- runClick ctx inp ui (centerOf sel)
  closes (anySelectOpen <$> getStore ctx)
  let (rightPress, rightRelease) = rightClickPair inp (centerOf field)
  mapM_ (\i -> warmup ctx i ui) [rightPress, rightRelease]
  assertEq failed (respId field) =<< getFocusId ctx
  closes (isJust <$> getsInteraction ctx isTextInputMenu)

-- | 'focusNext' and 'focusPrevious' act like Tab and Shift+Tab at the end of
-- the frame, wrapping at both ends; 'isFocused' reports the focused widget.
-- 'requestFocus' with 'currentId' targets the next declared widget.
runFocusNextPreviousTest :: Context -> IORef Int -> IO ()
runFocusNextPreviousTest ctx failed = do
  move <- newIORef (pure ())
  let ui = column $ do
        join (uiIO (readIORef move))
        ids <- mapM (fmap respId . button') ["A", "B", "C"]
        (,) ids <$> mapM isFocused ids
      step m = writeIORef move m >> runFrame ctx inp ui >> writeIORef move (pure ()) >> snd <$> evalUi ctx inp ui
  ([a, _, c], _) <- warmup2 ctx inp ui
  assertEq failed [True, False, False] =<< step focusNext
  assertEq failed [False, True, False] =<< step focusNext
  assertEq failed [True, False, False] =<< step focusPrevious
  assertEq failed [False, False, True] =<< step focusPrevious
  assertEq failed c =<< getFocusId ctx
  assert failed =<< getFocusVisible ctx
  assertEq failed [True, False, False] =<< step (requestFocus =<< currentId)
  assertEq failed a =<< getFocusId ctx

-- | 'clearFocus' unfocuses at the end of the frame and collapses the field's
-- selection; 'releaseFocus' unfocuses immediately and keeps the selection.
runFocusClearReleaseTest :: Context -> IORef Int -> IO ()
runFocusClearReleaseTest ctx failed = do
  act <- newIORef (const (pure ()))
  let ui = column $ do
        (r, _) <- textInput' ("hello" :: T.Text)
        f <- uiIO (readIORef act)
        f (respId r)
        after <- focusedWidget
        pure (respId r, after)
      selection wid = (\st -> fieldSelection st (intKey wid) (findSlot fieldText "" (intKey wid) st)) <$> getStore ctx
      selectAll = chordInp (cmdOrCtrl <> key 'a') inp
  warmupFocused ctx inp ui
  (field, _) <- evalUi ctx inp ui
  _ <- runFrame ctx selectAll ui
  assertEq failed (0, 5) =<< selection field
  writeIORef act (const clearFocus)
  assertEq failed (field, field) =<< evalUi ctx inp ui
  assertEq failed (WidgetId 0) =<< getFocusId ctx
  assertEq failed (5, 5) =<< selection field
  writeIORef act (const (pure ()))
  _ <- runFrame ctx (tabInp inp) ui
  _ <- runFrame ctx selectAll ui
  writeIORef act releaseFocus
  assertEq failed (field, WidgetId 0) =<< evalUi ctx inp ui
  assertEq failed (0, 5) =<< selection field

-- | A text command run from code focuses the field like 'requestFocus': at
-- the end of the frame, with the ring, and never when disabled.
runFocusTextCommandTest :: Context -> IORef Int -> IO ()
runFocusTextCommandTest ctx failed = do
  offRef <- newIORef False
  run <- newIORef False
  let ui = column $ do
        off <- uiIO (readIORef offRef)
        (r, _) <- disabledWhen off (textInput' ("hello" :: T.Text))
        whenM (uiIO (readIORef run)) (runTextCommand (respId r) SelectAll)
        (,) (respId r) <$> focusedWidget
  (field, _) <- warmup2 ctx inp ui
  writeIORef run True
  assertEq failed (field, WidgetId 0) =<< evalUi ctx inp ui
  assertEq failed field =<< getFocusId ctx
  assert failed =<< getFocusVisible ctx
  writeIORef (ctxFocusId ctx) (WidgetId 0)
  writeIORef offRef True
  writeIORef run True
  _ <- warmup ctx inp ui
  assertEq failed (WidgetId 0) =<< getFocusId ctx
