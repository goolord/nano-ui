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
  , spec "key-hard-quit" runKeyHardQuitTest
  , spec "shortcut-focused-text-area" runShortcutFocusedTextAreaTest
  , spec "shortcut-focus-from-code" runShortcutFocusFromCodeTest
  ]

inp0 :: Input
inp0 = withInput 400 300

ctrlHeld :: Modifiers
ctrlHeld = noModifiers {modCtrl = True}

-- | The view @mk note@, and an action running a frame that returns the names
-- @note@ recorded in it, newest first.
noting :: Context -> ((T.Text -> NanoUI ()) -> NanoUI a) -> IO (NanoUI a, Input -> IO [T.Text])
noting ctx mk = do
  fired <- newIORef []
  let ui = mk (\c -> uiIO (modifyIORef' fired (c :)))
  pure (ui, \i -> writeIORef fired [] >> runFrame ctx i ui >> readIORef fired)

-- | Bind each chord to recording its label.
binds :: (T.Text -> NanoUI ()) -> [Shortcut] -> NanoUI ()
binds note = mapM_ (\c -> whenM (shortcut c) (note (shortcutLabel c)))

-- | Press each chord in a frame of its own: it fires just itself, or nothing.
fires :: IORef Int -> (Input -> IO [T.Text]) -> Bool -> [Shortcut] -> IO ()
fires failed press yes = mapM_ (\c -> let l = shortcutLabel c in assertEq failed (l, [l | yes]) . (l,) =<< press (chordInp c inp0))

-- | Chords read from text are the chords put together with '<>', every key
-- and modifier among them, and the labels a menu shows for them.
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
  -- The key is the last one given; modifiers add up, and none is a chord no key presses.
  assertEq failed (shift <> ctrl <> key 'b') (ctrl <> key 'a' <> shift <> key 'b')
  assertEq failed "Ctrl+Shift" (shortcutLabel (ctrl <> shift))
  assertEq failed [] (filter (isRight . parseShortcut) ["", "C-", "C-S-", "X-p", "<F25>", "<F0>", "C-<Bogus>", "ab", "<Enter"])
  -- Every key, under every set of modifiers, reads back from its chord.
  let keys = [KeyBackspace, KeyDelete, KeyEnter, KeyEscape, KeyTab, KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd, KeyPageUp, KeyPageDown, KeyInsert, KeySpace, KeyPrintScreen, KeyPause, KeyCapsLock, KeyNumLock, KeyScrollLock, KeyMenu] ++ map KeyF [1 .. 24] ++ map KeyChar "a1=+-<>,."
      chordText (Shortcut k m) = T.concat ([p | (True, p) <- zip [modCtrl m, modShift m, modAlt m, modSuper m] ["C-", "S-", "A-", "s-"]] ++ foldMap (pure . keyText) k)
      keyText = \case
        KeyChar ch -> T.singleton ch
        k -> "<" <> keyLabel k <> ">"
  assertEq failed [] [sc | k <- keys, [sh, ct, a, u] <- replicateM 4 [False, True], let sc = Shortcut (Just k) (Modifiers sh ct a u), parseShortcut (chordText sc) /= Right sc]
  forM_ [("S-C-p", "Ctrl+Shift+P"), ("C-+", "Ctrl++"), ("<Escape>", "Escape"), ("<PageDown>", "PageDown"), ("<F11>", "F11")] $ \(txt, lbl) ->
    assertEq failed (Right lbl) (shortcutLabel <$> parseShortcut txt)

-- | A chord fires only with exactly its modifiers; typed text or a release is no press.
runShortcutExactModifiersTest :: Context -> IORef Int -> IO ()
runShortcutExactModifiersTest ctx failed = do
  (ui, press) <- noting ctx $ \note -> column (binds note [ctrl <> key 's', ctrl <> shift <> key 's', key 's'] >> label "view")
  warmup ctx inp0 ui
  fires failed press True [ctrl <> key 's', ctrl <> shift <> key 's', key 's']
  fires failed press False [ctrl <> alt <> key 's', super <> key 's', ctrl <> key 't']
  forM_ [inp0, inp0 {inputChars = "s", inputModifiers = ctrlHeld}, keyUpInp (KeyChar 's') inp0 {inputModifiers = ctrlHeld}] $ \i ->
    assertEq failed [] =<< press i
  assertEq failed [True, False] [shortcutIn (ctrl <> key 's') (chordInp k inp0) | k <- [ctrl <> key 's', ctrl <> shift <> key 's']]

-- | A press fires once, not in the pass its hook write reruns nor for a later
-- binding of the chord, even when the key auto-repeated within the frame.
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

-- | Behind an open modal only the shortcuts inside it fire; a raw key listener hears nothing.
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

-- | A focused text field keeps what it types and edits with, even from an
-- earlier shortcut; the other chords still fire.
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

-- | A focused control keeps Enter and the arrows, whatever the modifiers.
runShortcutFocusedControlTest :: Context -> IORef Int -> IO ()
runShortcutFocusedControlTest ctx failed = do
  (ui, press) <- noting ctx $ \note ->
    column (binds note [key KeyEnter, ctrl <> key KeyEnter, key KeyRight, key 's'] >> whenM (button "Go") (note "button"))
  warmupFocused ctx inp0 ui
  forM_ [key KeyEnter, ctrl <> key KeyEnter] $ \c -> assertEq failed ["button"] =<< press (chordInp c inp0)
  fires failed press False [key KeyRight]
  fires failed press True [key 's']

-- | An Escape shortcut takes Escape, so the app does not quit on it, and a Tab
-- shortcut keeps focus where it is.
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

-- | A menu row binds its chord only while its menu is open, which shows the
-- chord's label, and a chord with no key shows its modifiers and binds nothing.
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

-- | Key releases and held keys reach the view, and 'clearEphemeral' treats a
-- release as it treats a press.
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
  -- An auto-repeat lists a held key once.
  assertEq failed ([KeySpace], [], [KeySpace]) (toList (inputKeysHeld holding), toList (inputKeysReleased (clearEphemeral up)), toList (inputKeysHeld (applyKey KeySpace True down)))
  assert failed (inputInteracted holding up)
  assertEq failed [False, False, False] =<< evalUi ctx down (disabledWhen True keys)

-- | Ctrl+C quits from its key as it did from its typed letter.
runKeyHardQuitTest :: Context -> IORef Int -> IO ()
runKeyHardQuitTest _ failed =
  assertEq failed [True, False, False] [isHardQuitInput (chordInp c inp0) | c <- [ctrl <> key 'c', key 'c', ctrl <> shift <> key 'x']]

-- | A focused text area keeps Enter and leaves Ctrl+Enter and Alt+Enter to a shortcut.
runShortcutFocusedTextAreaTest :: Context -> IORef Int -> IO ()
runShortcutFocusedTextAreaTest ctx failed = do
  textRef <- newIORef "hi"
  (ui, press) <- noting ctx $ \note ->
    column (binds note [key KeyEnter, shift <> key KeyEnter, ctrl <> key KeyEnter, alt <> key KeyEnter] >> held textRef textArea')
  warmupFocused ctx inp0 ui
  fires failed press False [key KeyEnter, shift <> key KeyEnter]
  fires failed press True [ctrl <> key KeyEnter, alt <> key KeyEnter]
  assertEq failed 2 . T.count "\n" =<< readIORef textRef

-- | Ctrl+F moves the keyboard from code to a search box, which takes the next
-- frame's typing; the chord types nothing into the field it left.
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
