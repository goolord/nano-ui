module Cases.Focus (tests) where

import Spec
import Data.Maybe (isJust)
import NanoUI.Internal.Context (InteractionState (..), getFocusVisible, getsInteraction, intKey)
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
  ]

inp :: Input
inp = withInputOff 400 400

-- | The view @mk req@, where @req@ requests focus for the queued ids, and an
-- action that queues ids and runs a frame, returning its dirty flag and focus.
asking :: Context -> (NanoUI () -> NanoUI a) -> IO (NanoUI a, [WidgetId] -> Input -> IO (Bool, WidgetId))
asking ctx mk = do
  q <- newIORef []
  let ui = mk (mapM_ requestFocus =<< uiIO (atomicModifyIORef' q ([],)))
  pure (ui, \ids i -> writeIORef q ids >> runFrame ctx i ui >>= \(_, _, _, dirty) -> (dirty,) <$> getFocusId ctx)

-- | A field focused from code shows the ring, and types from the next frame
-- on (which the request asks for), not in the frame that asks.
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

-- | Every kind of control Tab stops at takes the keyboard by its response's id.
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

-- | Tab goes on from the widget focused from code, both ways.
runFocusRequestTabOrderTest :: Context -> IORef Int -> IO ()
runFocusRequestTabOrderTest ctx failed = do
  (ui, ask) <- asking ctx $ \req -> column (mapM (fmap respId . button') ["A", "B", "C"] <* req)
  [a, b, c] <- warmup2 ctx inp ui
  forM_ [(key KeyTab, c), (shift <> key KeyTab, a)] $ \(chord, next) -> do
    assertEq failed b . snd =<< ask [b] inp
    _ <- runFrame ctx (chordInp chord inp) ui
    assertEq failed next =<< getFocusId ctx

-- | Asking for @WidgetId 0@ takes the keyboard off the field, and the loop can sleep.
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

-- | A disabled field, a label and an unknown id refuse the keyboard, asking for no frame.
runFocusRequestRefusedTest :: Context -> IORef Int -> IO ()
runFocusRequestRefusedTest ctx failed = do
  (ui, ask) <- asking ctx $ \req -> column $ do
    ids <- sequence [respId . fst <$> textInput' "", respId . fst <$> disabledWhen True (textInput' ""), respId <$> label' "Label"]
    ids <$ req
  [field, off, lbl] <- warmup2 ctx inp ui
  _ <- ask [field] inp >> warmup ctx inp ui
  assertEq failed field =<< getFocusId ctx
  forM_ [off, lbl, WidgetId 987654321] $ \w -> assertEq failed (False, field) =<< ask [w] inp

-- | While a modal is up, a widget in it takes the keyboard and one behind it refuses it.
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

-- | The field that loses the keyboard to a request collapses its selection, and
-- a search field commits the query it was holding back, as on a click elsewhere.
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

-- | The request's frame repaints the old and the new focus, in a clip.
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

-- | A request in a view pass that a hook write runs again is carried out.
runFocusRequestFirstPassTest :: Context -> IORef Int -> IO ()
runFocusRequestFirstPassTest ctx failed = do
  r <- evalUi ctx inp . column $ do
    (asked, setAsked) <- useFlag False
    r <- respId . fst <$> textInput' ""
    r <$ unless asked (requestFocus r >> setAsked True)
  assertEq failed r =<< getFocusId ctx

-- | Asking every frame for the focused widget costs no frames, and a click on
-- it hides the ring for good.
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

-- | A request can name a widget declared after it, the last request of a frame
-- wins, the field types at its end, and a Tab in the frame goes on from it.
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

-- | Focus from code closes an open dropdown and the focused field's context menu.
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
