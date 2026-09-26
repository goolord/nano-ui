module Cases.Shortcut (tests) where

import Spec
import Data.Either (isRight)
import Data.Foldable (toList)
import Data.Text qualified as T
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Input (isHardQuitInput)
import NanoUI.Shortcut
import System.Info (os)

tests :: [Spec]
tests =
  [ spec "shortcut-parse-label" runShortcutParseTest
  , spec "shortcut-exact-modifiers" runShortcutExactModifiersTest
  , spec "shortcut-once-per-press" runShortcutOncePerPressTest
  , spec "shortcut-modal" runShortcutModalTest
  , spec "shortcut-focused-field" runShortcutFocusedFieldTest
  , spec "shortcut-focused-control" runShortcutFocusedControlTest
  , spec "shortcut-escape-tab" runShortcutEscapeTabTest
  , spec "shortcut-menu-item" runShortcutMenuItemTest
  , spec "key-release-held" runKeyReleaseTest
  , spec "key-listeners-focus" runKeyListenersFocusTest
  , spec "key-repeats" runKeyRepeatsTest
  , spec "key-repeats-widgets" runKeyRepeatsWidgetsTest
  , spec "key-pressable" runKeyPressableTest
  , spec "key-hard-quit" runKeyHardQuitTest
  , spec "shortcut-focused-text-area" runShortcutFocusedTextAreaTest
  , spec "shortcut-focus-from-code" runShortcutFocusFromCodeTest
  ]

inp0 :: Input
inp0 = withInput 400 300

ctrlHeld :: Modifiers
ctrlHeld = noModifiers {modCtrl = True}

-- | Build the view @mk note@ and a frame runner returning what @note@
-- recorded that frame, newest first.
noting :: Context -> ((T.Text -> NanoUI ()) -> NanoUI a) -> IO (NanoUI a, Input -> IO [T.Text])
noting ctx mk = do
  fired <- newIORef []
  let ui = mk (\c -> uiIO (modifyIORef' fired (c :)))
  pure (ui, \i -> writeIORef fired [] >> runFrame ctx i ui >> readIORef fired)

binds :: (T.Text -> NanoUI ()) -> [Shortcut] -> NanoUI ()
binds note = mapM_ (\c -> whenM (shortcut c) (note (shortcutLabel c)))

-- | Press each chord in its own frame and expect only it to fire, or nothing.
fires :: IORef Int -> (Input -> IO [T.Text]) -> Bool -> [Shortcut] -> IO ()
fires failed press yes = mapM_ (\c -> let l = shortcutLabel c in assertEq failed (l, [l | yes]) . (l,) =<< press (chordInp c inp0))

-- | Parsed chords equal the '<>'-built ones for every key and modifier, and
-- labels match what a menu shows.
runShortcutParseTest :: Context -> IORef Int -> IO ()
runShortcutParseTest _ failed = do
  forM_
    [ ("C-s", ctrl <> key 's'), ("C-S-P", ctrl <> shift <> key 'P'), ("A-<Enter>", alt <> key KeyEnter)
    , ("<F5>", key (KeyF 5)), ("<F24>", key (KeyF 24)), ("<PageDown>", key KeyPageDown)
    , ("s-k", super <> key 'k'), ("M-s", cmdOrCtrl <> key 's')
    , ("C-+", ctrl <> key '+'), ("C-S-+", ctrl <> shift <> key '+'), ("C--", ctrl <> key '-'), ("-", key '-')
    , ("<", key '<'), ("C-<", ctrl <> key '<'), ("<Escape>", key KeyEscape), ("C-=", ctrl <> key '=')
    ]
    $ \(txt, sc) -> assertEq failed (txt, Right sc) (txt, parseShortcut txt)
  -- The last key wins and modifiers accumulate; a chord without a key never fires.
  assertEq failed (shift <> ctrl <> key 'b') (ctrl <> key 'a' <> shift <> key 'b')
  assertEq failed "Ctrl+Shift" (shortcutLabel (ctrl <> shift))
  assertEq failed [] (filter (isRight . parseShortcut) ["", "C-", "C-S-", "X-p", "<F25>", "<F0>", "C-<Bogus>", "ab", "<Enter"])
  -- Round-trip every key under every modifier combination.
  let keys = [KeyBackspace, KeyDelete, KeyEnter, KeyEscape, KeyTab, KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd, KeyPageUp, KeyPageDown, KeyInsert, KeySpace, KeyPrintScreen, KeyPause, KeyCapsLock, KeyNumLock, KeyScrollLock, KeyMenu] ++ map KeyF [1 .. 24] ++ map KeyChar "a1=+-<>,."
      chordText (Shortcut k m) = T.concat ([p | (True, p) <- zip [modCtrl m, modShift m, modAlt m, modSuper m] ["C-", "S-", "A-", "s-"]] ++ foldMap (pure . keyText) k)
      keyText = \case
        KeyChar ch -> T.singleton ch
        k -> "<" <> keyLabel k <> ">"
  assertEq failed [] [sc | k <- keys, [sh, ct, a, u] <- replicateM 4 [False, True], let sc = Shortcut (Just k) (Modifiers sh ct a u), parseShortcut (chordText sc) /= Right sc]
  forM_ [("S-C-p", "Ctrl+Shift+P"), ("C-+", "Ctrl++"), ("<Escape>", "Escape"), ("<PageDown>", "PageDown"), ("<F11>", "F11")] $ \(txt, lbl) ->
    assertEq failed (Right lbl) (shortcutLabel <$> parseShortcut txt)

-- | A chord needs exactly its modifiers; typed text and releases do not fire it.
runShortcutExactModifiersTest :: Context -> IORef Int -> IO ()
runShortcutExactModifiersTest ctx failed = do
  (ui, press) <- noting ctx $ \note -> column (binds note [ctrl <> key 's', ctrl <> shift <> key 's', key 's'] >> label "view")
  warmup ctx inp0 ui
  fires failed press True [ctrl <> key 's', ctrl <> shift <> key 's', key 's']
  fires failed press False [ctrl <> alt <> key 's', super <> key 's', ctrl <> key 't']
  forM_ [inp0, inp0 {inputChars = "s", inputModifiers = ctrlHeld}, keyUpInp (KeyChar 's') inp0 {inputModifiers = ctrlHeld}] $ \i ->
    assertEq failed [] =<< press i
  assertEq failed [True, False] [shortcutIn (ctrl <> key 's') (chordInp k inp0) | k <- [ctrl <> key 's', ctrl <> shift <> key 's']]

-- | A press fires once: not again in the rerun pass after a hook write, not
-- for a second binding, and not for repeats within the frame.
runShortcutOncePerPressTest :: Context -> IORef Int -> IO ()
runShortcutOncePerPressTest ctx failed = do
  (ui, press) <- noting ctx $ \note -> column $ do
    note "pass"
    (n, setN) <- useInt 0
    whenM (shortcut (ctrl <> key 'k')) (note "fired" >> setN (n + 1))
    whenM (shortcut (ctrl <> key 'k')) (note "again")
    label (T.pack (show n))
  warmup ctx inp0 ui
  let ctrlK = chordInp (ctrl <> key 'k') inp0
  forM_ [ctrlK, ctrlK {inputKeys = inputKeysFromList [KeyChar 'k', KeyChar 'k']}] $ \i ->
    assertEq failed ["pass", "fired", "pass"] =<< press i
  assertEq failed ["pass"] =<< press inp0
  assertSpansHas failed "2" =<< collectTextSpans ctx

-- | With a modal open only its own shortcuts fire, and raw key listeners hear nothing.
runShortcutModalTest :: Context -> IORef Int -> IO ()
runShortcutModalTest ctx failed = do
  openRef <- newIORef True
  (ui, press) <- noting ctx $ \note -> column $ do
    binds note [ctrl <> key 's']
    open <- uiIO (readIORef openRef)
    _ <- modal open "Dialog" (binds note [ctrl <> key 'd'] >> label "Body")
    binds note [ctrl <> key 'e']
    whenM (keyPressed (KeyChar 'e')) (note "raw")
  warmup2 ctx inp0 ui
  fires failed press False [ctrl <> key 's', ctrl <> key 'e']
  fires failed press True [ctrl <> key 'd']
  writeIORef openRef False
  warmup2 ctx inp0 ui
  fires failed press True [ctrl <> key 's']
  assertEq failed ["raw", "Ctrl+E"] =<< press (chordInp (ctrl <> key 'e') inp0)
  fires failed press False [ctrl <> key 'd']

-- | A focused text field keeps its typing and editing keys, even when a
-- shortcut was bound first; other chords still fire.
runShortcutFocusedFieldTest :: Context -> IORef Int -> IO ()
runShortcutFocusedFieldTest ctx failed = do
  textRef <- newIORef "hello"
  (ui, press) <- noting ctx $ \note -> column $ do
    binds note [ctrl <> key 'a', ctrl <> key 's', key 'j', shift <> key 'j', alt <> key 'j', key KeyEnter, ctrl <> key KeyLeft, key (KeyF 2), ctrl <> key KeySpace]
    held textRef textInput'
  warmupFocused ctx inp0 ui
  fires failed press False [ctrl <> key 'a']
  _ <- runFrame ctx (keyInp KeyBackspace inp0) ui
  assertEq failed "" =<< readIORef textRef
  fires failed press False [key 'j', shift <> key 'j', key KeyEnter, ctrl <> key KeyLeft]
  -- Alt with a letter types on macOS (Option).
  fires failed press True ([ctrl <> key 's', key (KeyF 2), ctrl <> key KeySpace] ++ [alt <> key 'j' | os /= "darwin"])
  writeIORef (ctxFocusId ctx) (WidgetId 0)
  fires failed press True [ctrl <> key 'a', key 'j', key KeyEnter]

-- | A focused button keeps Enter and Space (plain or with Shift), a slider
-- also the arrows. Ctrl+Enter, Alt+Left and similar go to shortcuts instead.
-- A custom widget keeps the keys it claims, here all of them.
runShortcutFocusedControlTest :: Context -> IORef Int -> IO ()
runShortcutFocusedControlTest ctx failed = do
  let chords = [key KeyEnter, shift <> key KeyEnter, ctrl <> key KeyEnter, key KeyRight, shift <> key KeyRight, alt <> key KeyLeft, key 's']
  (ui, press) <- noting ctx $ \note ->
    column (binds note chords >> whenM (button "Go") (note "button"))
  warmupFocused ctx inp0 ui
  forM_ [key KeyEnter, shift <> key KeyEnter] $ \c -> assertEq failed ["button"] =<< press (chordInp c inp0)
  fires failed press True [ctrl <> key KeyEnter, key KeyRight, shift <> key KeyRight, alt <> key KeyLeft, key 's']
  s <- newContext
  (sliderUi, pressS) <- noting s $ \note -> column (binds note chords >> void (slider 0 10 5))
  warmupFocused s inp0 sliderUi
  fires failed pressS False [key KeyEnter, key KeyRight, shift <> key KeyRight]
  fires failed pressS True [ctrl <> key KeyEnter, alt <> key KeyLeft, key 's']
  sel <- newContext
  selRef <- newIORef 0
  (selUi, pressSel) <- noting sel $ \note -> column (binds note [ctrl <> key KeyDown] >> void (held selRef (select' ["a", "b", "c"])))
  warmupFocused sel inp0 selUi
  let chosenAfter c = pressSel (chordInp c inp0) >> pressSel inp0 >> readIORef selRef
  assertEq failed 0 =<< chosenAfter (ctrl <> key KeyDown)
  assertEq failed 1 =<< chosenAfter (key KeyDown)
  c <- newContext
  (customUi, pressC) <- noting c $ \note ->
    column (binds note chords >> void (customWidget defaultCustomWidgetSpec {widgetFocusable = True, widgetKeys = KeysAll}))
  warmupFocused c inp0 customUi
  fires failed pressC False chords

-- | An Escape shortcut consumes Escape so the app does not quit, and a Tab
-- shortcut does not move focus.
runShortcutEscapeTabTest :: Context -> IORef Int -> IO ()
runShortcutEscapeTabTest ctx failed = do
  (ui, press) <- noting ctx $ \note -> column $ do
    binds note [key KeyEscape]
    whenM takeEscape (note "take")
    binds note [ctrl <> key KeyTab]
    respId <$> button' "a" <* button' "b"
  a <- warmup2 ctx inp0 ui
  fires failed press True [key KeyEscape]
  assert failed =<< overlayConsumesQuit ctx (keyInp KeyEscape inp0)
  _ <- runFrame ctx (tabInp inp0) ui
  assertEq failed a =<< getFocusId ctx
  fires failed press True [ctrl <> key KeyTab]
  assertEq failed a =<< getFocusId ctx

-- | A menu row binds its chord only while the menu is open and shows its
-- label. A keyless chord shows its modifiers and binds nothing.
runShortcutMenuItemTest :: Context -> IORef Int -> IO ()
runShortcutMenuItemTest ctx failed = do
  (ui, press) <- noting ctx $ \note -> column $ do
    target <- button' "Target"
    _ <- contextMenu target $ do
      whenM (menuItemShortcut "Save" (ctrl <> key 's')) (note "Ctrl+S")
      void (menuItemShortcut "Help" (ctrl <> shift))
    pure target
  target <- warmup2 ctx inp0 ui
  fires failed press False [ctrl <> key 's']
  let (down, up) = rightClickPair inp0 (centerOf target)
  mapM_ press [down, up]
  shown <- (++) <$> collectTextSpans ctx <*> collectOverlayTextSpans ctx inp0
  assert failed (hasText "Save  Ctrl+S" shown && hasText "Help  Ctrl+Shift" shown)
  fires failed press False [key 's', ctrl <> key 'd']
  fires failed press True [ctrl <> key 's']

-- | Releases and held keys reach the view; 'clearEphemeral' clears releases
-- like presses.
runKeyReleaseTest :: Context -> IORef Int -> IO ()
runKeyReleaseTest ctx failed = do
  let keys = sequence [keyPressed KeySpace, keyReleased KeySpace, keyHeld KeySpace]
      ui = column keys
      down = applyKey KeySpace True inp0
      holding = clearEphemeral down
      up = applyKey KeySpace False holding
  warmup ctx inp0 ui
  forM_ [(down, [True, False, True]), (holding, [False, False, True]), (up, [False, True, False]), (clearEphemeral up, [False, False, False])] $ \(i, seen) ->
    assertEq failed seen =<< evalUi ctx i ui
  -- A repeated key appears once in the held set.
  assertEq failed ([KeySpace], [], [KeySpace]) (toList (inputKeysHeld holding), toList (inputKeysReleased (clearEphemeral up)), toList (inputKeysHeld (applyKey KeySpace True down)))
  assert failed (inputInteracted holding up)
  assertEq failed [False, False, False] =<< evalUi ctx down (disabledWhen True keys)

-- | Key listeners hear only keys the focused widget does not claim: a text
-- field keeps Delete, F2 reaches the view. The raw input still has both.
runKeyListenersFocusTest :: Context -> IORef Int -> IO ()
runKeyListenersFocusTest ctx failed = do
  textRef <- newIORef "hello"
  let listen = mapM (\k -> (,) <$> keyPressed k <*> keyHeld k) [KeyDelete, KeyF 2]
      ui = column (listen <* held textRef textInput')
      pressBoth = applyKey (KeyF 2) True (applyKey KeyDelete True inp0)
  warmupFocused ctx inp0 ui
  assertEq failed [(False, False), (True, True)] =<< evalUi ctx pressBoth ui
  assert failed (pressedIn KeyDelete pressBoth && heldIn KeyDelete pressBoth)
  writeIORef (ctxFocusId ctx) (WidgetId 0)
  _ <- runFrame ctx inp0 ui
  assertEq failed [(True, True), (True, True)] =<< evalUi ctx pressBoth ui

-- | Auto-repeats are presses but not new ones: 'shortcut' and 'keyPressed'
-- fire on them, 'shortcutOnce' and 'keyPressedOnce' do not. A 'shortcutOnce'
-- that does not fire leaves the repeat to a later 'shortcut'.
runKeyRepeatsTest :: Context -> IORef Int -> IO ()
runKeyRepeatsTest ctx failed = do
  (ui, press) <- noting ctx $ \note -> column $ do
    whenM (shortcut (ctrl <> key 'k')) (note "k")
    whenM (shortcutOnce (ctrl <> key 'j')) (note "j once")
    whenM (shortcut (ctrl <> key 'j')) (note "j")
    whenM (keyPressed (KeyChar 'k')) (note "pressed")
    whenM (keyPressedOnce (KeyChar 'k')) (note "pressed once")
  warmup ctx inp0 ui
  let down c = chordInp (ctrl <> key c) inp0
      repeated c = (applyKey (KeyChar c) True (clearEphemeral (down c)))
  assertEq failed ["pressed once", "pressed", "k"] =<< press (down 'k')
  assertEq failed ["pressed", "k"] =<< press (repeated 'k')
  assertEq failed ["j once"] =<< press (down 'j')
  assertEq failed ["j"] =<< press (repeated 'j')
  -- Backends pass every repeat; the held set lists the key once.
  let r = repeated 'k'
  assertEq failed ([KeyChar 'k'], [], [KeyChar 'k']) (toList (inputKeys r), toList (inputKeysNew r), toList (inputKeysHeld r))

-- | Held Enter repeats line breaks in a text area but clicks a button and
-- submits a text field only once.
runKeyRepeatsWidgetsTest :: Context -> IORef Int -> IO ()
runKeyRepeatsWidgetsTest ctx failed = do
  areaRef <- newIORef ""
  let area = column (held areaRef textArea')
  warmupFocused ctx inp0 area
  mapM_ (\i -> runFrame ctx i area) [keyInp KeyEnter inp0, keyRepeatInp KeyEnter inp0, keyRepeatInp KeyEnter inp0]
  assertEq failed "\n\n\n" =<< readIORef areaRef
  c <- newContext
  let buttons = column (button' "Go")
  warmupFocused c inp0 buttons
  assertEq failed [True, False] . map respClicked =<< mapM (\i -> evalUi c i buttons) [keyInp KeyEnter inp0, keyRepeatInp KeyEnter inp0]
  f <- newContext
  fieldRef <- newIORef "x"
  let field = column (held fieldRef textInput')
  warmupFocused f inp0 field
  assertEq failed [True, False] . map (respSubmitted . fst) =<< mapM (\i -> evalUi f i field) [keyInp KeyEnter inp0, keyRepeatInp KeyEnter inp0]

-- | Keys and mouse buttons share one query API; losing keyboard focus
-- releases held keys and modifiers.
runKeyPressableTest :: Context -> IORef Int -> IO ()
runKeyPressableTest _ failed = do
  let down = applyMouseButton MouseMiddle True (applyKey KeyEnter True inp0)
      up = applyMouseButton MouseMiddle False (applyKey KeyEnter False (clearEphemeral down))
      queries i = [pressedIn KeyEnter i, pressedOnceIn KeyEnter i, releasedIn KeyEnter i, heldIn KeyEnter i, pressedIn MouseMiddle i, pressedOnceIn MouseMiddle i, releasedIn MouseMiddle i, heldIn MouseMiddle i]
  assertEq failed [True, True, False, True, True, True, False, True] (queries down)
  assertEq failed [False, False, False, True, False, False, False, True] (queries (clearEphemeral down))
  assertEq failed [False, False, True, False, False, False, True, False] (queries up)
  let blurred = releaseAllKeys (clearEphemeral (chordInp (ctrl <> key 's') inp0))
  assertEq failed ([], [KeyChar 's'], noModifiers) (toList (inputKeysHeld blurred), toList (inputKeysReleased blurred), inputModifiers blurred)

-- | Ctrl+C is a hard quit when it arrives as a key, not just as text.
runKeyHardQuitTest :: Context -> IORef Int -> IO ()
runKeyHardQuitTest _ failed =
  assertEq failed [True, False, False] [isHardQuitInput (chordInp c inp0) | c <- [ctrl <> key 'c', key 'c', ctrl <> shift <> key 'x']]

-- | A focused text area keeps Enter and Shift+Enter; Ctrl+Enter and Alt+Enter go to shortcuts.
runShortcutFocusedTextAreaTest :: Context -> IORef Int -> IO ()
runShortcutFocusedTextAreaTest ctx failed = do
  textRef <- newIORef "hi"
  (ui, press) <- noting ctx $ \note ->
    column (binds note [key KeyEnter, shift <> key KeyEnter, ctrl <> key KeyEnter, alt <> key KeyEnter] >> held textRef textArea')
  warmupFocused ctx inp0 ui
  fires failed press False [key KeyEnter, shift <> key KeyEnter]
  fires failed press True [ctrl <> key KeyEnter, alt <> key KeyEnter]
  assertEq failed 2 . T.count "\n" =<< readIORef textRef

-- | Ctrl+F focuses a search box from code. The next frame's typing goes there
-- and the chord types nothing into the previous field.
runShortcutFocusFromCodeTest :: Context -> IORef Int -> IO ()
runShortcutFocusFromCodeTest ctx failed = do
  refs <- mapM newIORef ["notes", ""]
  let ui = column $ do
        ids <- mapM (fmap (respId . fst) . (`held` textInput')) refs
        forM_ (drop 1 ids) (whenM (shortcut (ctrl <> key 'f')) . requestFocus)
        pure ids
  warmupFocused ctx inp0 ui
  [notes, search] <- evalUi ctx inp0 ui
  assertEq failed notes =<< getFocusId ctx
  _ <- runFrame ctx (chordInp (ctrl <> key 'f') inp0) ui
  assertEq failed search =<< getFocusId ctx
  _ <- runFrame ctx inp0 {inputChars = "abc"} ui
  assertEq failed ["notes", "abc"] =<< mapM readIORef refs
