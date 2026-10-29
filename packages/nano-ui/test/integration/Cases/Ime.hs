-- | IME composition: shown at the focused field's caret without changing
-- the value, its keys edit nothing, its commit inserts text, and the frame
-- reports where the candidate window goes.
module Cases.Ime (tests) where

import Spec
import Cases.SimExtra (above)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import NanoUI.Internal.Context (intKey, setDrawSquareGeometry)
import NanoUI.Internal.Store (Slot (..), WidgetStore (..), slotKey)
import NanoUI.Shortcut
import NanoUI.Testing.Sim

tests :: [Spec]
tests =
  [ spec "ime-input" runImeInputTest
  , pixelSpec "ime-preedit-inline" runImePreeditInlineTest
  , pixelSpec "ime-commit" runImeCommitTest
  , pixelSpec "ime-keys" runImeKeysTest
  , pixelSpec "ime-text-area" runImeTextAreaTest
  , pixelSpec "ime-area-paint" runImeAreaPaintTest
  , pixelSpec "ime-scroll" runImeScrollTest
  , pixelSpec "ime-bidi" runImeBidiTest
  , pixelSpec "ime-owner" runImeOwnerTest
  , pixelSpec "ime-damage" runImeDamageTest
  , pixelSpec "ime-repeated-pass" runImeRepeatedPassTest
  , pixelSpec "ime-shortcuts" runImeShortcutsTest
  , pixelSpec "ime-custom-widget" runImeCustomWidgetTest
  , pixelSpec "ime-purpose" runImePurposeTest
  ]

inp :: Input
inp = withInput 400 300

-- | Input composing @txt@ with the IME caret @cursor@ characters in.
composing :: T.Text -> Int -> Input -> Input
composing txt cursor = applyComposition txt cursor 0

-- | Input ending the composition and committing @txt@.
commit :: T.Text -> Input -> Input
commit txt i = (applyComposition "" 0 0 i) {inputChars = txt}

-- | A frame composing (the composition lasts until changed or ended), and
-- one committing.
compose :: T.Text -> Int -> Sim a a
compose txt cursor = frameWith (composing txt cursor)

commitText :: T.Text -> Sim a a
commitText txt = frameWith (commit txt)

-- | Whether the frame reported a change, and the value.
changed :: (Response, T.Text) -> (Bool, T.Text)
changed (r, v) = (respChanged r, v)

-- | Every @yes@ text is a frame span, and no span contains any @no@ text.
shown :: HasCallStack => [T.Text] -> [T.Text] -> Sim a ()
shown yes no = do
  ts <- map (\(_, t, _, _, _) -> t) <$> spans
  check (all (`elem` ts) yes && not (or [n `T.isInfixOf` t | n <- no, t <- ts]))

type Field = (Response, T.Text)

imeArea :: Sim a (Maybe TextInputArea)
imeArea = simContext >>= liftIO . textInputArea

-- | Run the script on @widget@ holding @v@ in a column, focused by Tab, its
-- caret @back@ characters from the end.
field :: Context -> IORef Int -> T.Text -> Int -> (Var T.Text -> NanoUI Response) -> Sim Field () -> IO ()
field ctx failed v back widget script = do
  ref <- newVar v
  runSim ctx failed inp (column (held ref widget)) $
    pressTab 1 >> replicateM_ back (pressKey KeyLeft) >> script

-- | Composition persists until changed or ended; bad positions clamp.
runImeInputTest :: Context -> IORef Int -> IO ()
runImeInputTest _ failed = do
  let comp s l = inputComposition (applyComposition "かな" s l inp)
      next = clearEphemeral (composing "かな" 1 inp) {inputChars = "x"}
      clamped = [Composition "かな" 1 1, Composition "かな" 2 0, Composition "かな" 1 1]
  assertEq failed [comp 1 1, comp (-1) (-1), comp 1 9] (Just <$> clamped)
  assertEq failed (inputComposition (commit "" (composing "かな" 1 inp))) Nothing
  assertEq failed (inputComposition next, inputChars next) (Just (Composition "かな" 1 0), "")
  assert failed (inputInteracted inp (composing "か" 1 inp))
  assert failed (inputInteracted (composing "か" 1 inp) (composing "かな" 2 inp))

-- | Composition replaces any selection at the caret in plain, search and
-- (masked) password fields; the value and change flag are unchanged.
runImePreeditInlineTest :: Context -> IORef Int -> IO ()
runImePreeditInlineTest ctx failed = do
  field ctx failed "abcd" 2 textInput' $ do
    forM_ [("xyz", "abxyzcd"), ("xyzw", "abxyzwcd")] $ \(c, line) -> do
      checkEq (False, "abcd") . changed =<< compose c (T.length c)
      shown [line] ["abcd"]
    _ <- commitText "" >> chord (shift <> key KeyRight)
    checkEq "abcd" . snd =<< compose "xyz" 3
    shown ["abxyzd"] []
  let password = textInputConfigured' defaultTextInputConfig {ticPassword = True}
  forM_ [(searchInput' "Find", "abxy", []), (password, "****", ["xy"])] $ \(widget, line, hidden) -> do
    c <- newPixelContext
    field c failed "ab" 0 widget (compose "xy" 2 >> shown [line] hidden)

-- | A commit inserts at the caret and ends the composition before any new
-- one in the same input. An empty field shows the composition instead of its
-- placeholder.
runImeCommitTest :: Context -> IORef Int -> IO ()
runImeCommitTest ctx failed = do
  field ctx failed "abcd" 2 textInput' $ do
    _ <- compose "かんじ" 3
    checkEq (True, "ab漢字cd") . changed =<< commitText "漢字"
    shown ["ab漢字cd"] ["かんじ"]
    checkEq "ab漢字xcd" . snd =<< frameWith (\i -> (composing "y" 1 i) {inputChars = "x"})
    shown ["ab漢字xycd"] []
  c <- newPixelContext
  field c failed "" 0 (textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Name"}) $ do
    _ <- compose "かな" 1
    shown ["かな"] ["Name"]
    imeArea >>= (`checkJust` \(TextInputArea (Rect _ _ aw _) cursor _) -> checkEq (32, 16) (aw, cursor))
    checkEq "仮名" . snd =<< commitText "仮名"

-- | A view rerun after a hook write shows the composition and commits it
-- once.
runImeRepeatedPassTest :: Context -> IORef Int -> IO ()
runImeRepeatedPassTest ctx failed =
  field ctx failed "ab" 0 (\v -> textInput' v <* rerun) $ do
    compose "xy" 2 >> shown ["abxy"] []
    checkEq "abz" . snd =<< commitText "z"
  where
    rerun = useVar (0 :: Int) >>= \n -> readVar n >>= writeVar n . (+ 1)

-- | While composing, keys belong to the IME: none edits, submits or moves
-- focus, and Escape does not quit. An empty composition counts as none.
runImeKeysTest :: Context -> IORef Int -> IO ()
runImeKeysTest ctx failed =
  field ctx failed "abcd" 2 (\v -> textInput' v <* button "After") $ do
    let escapeKept f = do
          i <- keyInp KeyEscape . f <$> getInput
          frameWith (const i) >> liftIO (overlayConsumesQuit ctx i)
    _ <- compose "xyz" 3
    let keys = [KeyBackspace, KeyDelete, KeyLeft, KeyRight, KeyHome, KeyEnd, KeyEnter, KeyEscape, KeyTab]
    forM_ keys $ \k -> do
      (resp, value) <- pressKey k
      check (value == "abcd" && not (respChanged resp || respSubmitted resp))
      checkEq (respId resp) =<< focusedId
    shown ["abxyzcd"] []
    check =<< escapeKept id
    -- The key that ends a composition with nothing committed, an Escape that
    -- cancels it or a Backspace on its last character, is the IME's too.
    check =<< escapeKept (commit "")
    _ <- compose "xyz" 3
    checkEq "abcd" . snd =<< frameWith (keyInp KeyBackspace . commit "")
    check . not =<< escapeKept id
    let emptyComposition i = i {inputComposition = Just (Composition "" 0 0)}
    checkEq "acd" . snd =<< frameWith (keyInp KeyBackspace . emptyComposition)

-- | A text area shows the composition on the caret's row; over a multi-row
-- selection it replaces the first row's part. Enter does nothing, and the
-- commit replaces the selection.
runImeTextAreaTest :: Context -> IORef Int -> IO ()
runImeTextAreaTest ctx failed =
  field ctx failed "one\ntwo\nthree" 0 textArea' $ do
    _ <- pressKey KeyDown >> pressKey KeyEnd
    forM_ [compose "xy" 2, pressKey KeyEnter] $ \step -> checkEq (False, "one\ntwo\nthree") . changed =<< step
    shown ["twoxy", "one"] []
    checkEq (True, "one\ntwo続\nthree") . changed =<< commitText "続"
    shown ["two続"] []
    _ <- pressKey KeyUp >> pressKey KeyHome >> pressKey KeyRight
    _ <- chord (shift <> key KeyDown) >> chord (shift <> key KeyRight)
    checkEq (False, "one\ntwo続\nthree") . changed =<< compose "XY" 2
    shown ["oXY", "two続", "three"] ["one"]
    checkEq (True, "oZo続\nthree") . changed =<< commitText "Z"

-- | 'textInputArea', composition painting, and click placement over a
-- composition, in a field and a text area. No area once focus is gone.
runImeAreaPaintTest :: Context -> IORef Int -> IO ()
runImeAreaPaintTest ctx failed = do
  setDrawSquareGeometry ctx True
  t <- getTheme ctx
  ref <- newVar "abcd"
  areaRef <- newVar "one\ntwo"
  let cell = 16
      ui = column (held ref textInput' <* held areaRef textArea')
      near a b = abs (a - b) <= 1
      frames = mapM_ (\i -> runFrame ctx i ui)
      paint step = do
        (_, draw, _) <- runFrame ctx step ui
        (,) <$> drawQuads draw <*> collectTextSpans ctx
      carets quads cx = [r | (r@(Rect x _ w h), _) <- quads, near x cx, w <= 1.5, h >= cell / 2]
      -- On the row showing @line@: a composition @len@ cells long starting
      -- at cell @at@, IME caret @c@ cells in.
      check' line pt len c step = do
        (quads, spans') <- paint step
        assertJust failed (spanRectOf line spans') $ \(Rect sx sy _ sh) -> do
          let x0 = sx + pt * cell
              underlines (Rect x y w h, _) =
                near x x0 && near w (len * cell) && h <= 2 && y > sy + sh / 2 && y < sy + sh + 2
          assertJustM failed (textInputArea ctx) $ \(TextInputArea (Rect ax ay aw ah) cursor _) ->
            assertEq failed (ax, ay, aw, ah, cursor) (x0, sy, len * cell, cell, c * cell)
          assert failed (any underlines quads)
          -- The IME caret is drawn, not the field's caret at its start.
          assert failed (not (null (carets quads (x0 + c * cell))) && null (carets quads x0))
  warmupFocused ctx inp ui
  frames (replicate 2 (keyInp KeyLeft inp))
  spans0 <- collectTextSpans ctx
  assertJust failed (spanRectOf "abcd" spans0) $ \(Rect sx sy _ sh) -> do
    assertJustM failed (textInputArea ctx) $ \(TextInputArea r cursor _) ->
      assertEq failed (r, cursor) (Rect (sx + 2 * cell) sy 1 cell, 0)
    check' "abxyzcd" 2 3 1 (composing "xyz" 1 inp)
    -- A selection inside the composition is highlighted, with no caret.
    (quads, _) <- paint (applyComposition "xyz" 0 2 inp)
    let highlight (Rect x _ w _, col) = near x (sx + 2 * cell) && near w (2 * cell) && col == themeSelection t
    assert failed (any highlight quads)
    assert failed (null (carets quads (sx + 2 * cell)) && null (carets quads (sx + 4 * cell)))
    -- A click between the shown "c" and "d" lands after the value's "c".
    let between = V2 (sx + 6 * cell + 1) (sy + sh / 2)
    assertEq failed "abcd" . snd =<< runClick ctx (applyComposition "xyz" 0 2 inp) ui between
    assertSpansHas failed "abcxyzd" =<< collectTextSpans ctx
  -- The text area's caret starts at the document's start.
  frames [commit "" inp, tabInp inp, keyInp KeyDown inp, keyInp KeyRight inp]
  check' "txywo" 1 2 2 (composing "xy" 2 inp)
  _ <- runClick ctx (commit "" inp) ui (V2 390 290)
  assertEq failed Nothing =<< textInputArea ctx

-- | A long composition scrolls a narrow field to keep the IME caret in
-- view, and scrolls back when the caret returns to the start.
runImeScrollTest :: Context -> IORef Int -> IO ()
runImeScrollTest ctx failed = do
  let narrow = textInputConfigured' defaultTextInputConfig {ticLayout = fixedW 120 defaultLayout}
  field ctx failed "abc" 0 narrow $ do
    (resp, _) <- compose "0123456789" 10
    let Rect fx _ fw _ = respRect resp
        caretIn ok = imeArea >>= (`checkJust` \(TextInputArea r cursor _) -> check (ok (rectX r + cursor)))
    store <- liftIO (getStore ctx)
    above 0 (IM.findWithDefault 0 (slotKey SlotTextInputScroll (intKey (respId resp))) (storeFloat store))
    caretIn (\x -> x >= fx && x <= fx + fw)
    _ <- compose "0123456789" 0
    caretIn (>= fx)

-- | The area follows the shaped line: a Hebrew composition after Latin text
-- runs right to left, with caret 0 at its right edge.
runImeBidiTest :: Context -> IORef Int -> IO ()
runImeBidiTest base failed = do
  let line = "ab\x05D0\x05D1"
      shape t = if t == line then Just (ShapedText 40 40 (primArrayFromList [0, 10, 40, 30, 20])) else Nothing
      ctx = withFontMetrics base (monospaceMetrics 10) {fmShape = shape}
  field ctx failed "ab" 0 textInput' $ do
    Rect sx _ _ _ <- compose "\x05D0\x05D1" 0 >> rectOf line
    forM_ [(0, 20), (2, 0)] $ \(c, cursor) -> do
      area <- compose "\x05D0\x05D1" c >> imeArea
      checkJust area $ \(TextInputArea (Rect ax _ aw _) cur _) -> checkEq (sx + 20, 20, cursor) (ax, aw, cur)

-- | A composition shows only in the field focused when it last changed.
-- After focus moves it belongs to no field, and keys work again until it
-- changes.
runImeOwnerTest :: Context -> IORef Int -> IO ()
runImeOwnerTest ctx failed = do
  aRef <- newVar "aa"
  bRef <- newVar "bb"
  let comp n = compose (T.take n "xyz") n
  runSim ctx failed inp (column ((,) <$> held aRef textInput' <*> held bRef textInput')) $ do
    -- With no focus it is shown nowhere, and Tab still moves focus.
    _ <- comp 1
    shown [] ["x"]
    ((a, _), (b, _)) <- pressKey KeyTab >> frame
    checkEq (respId a) =<< focusedId
    -- Unchanged, it stays out of the field Tab reached.
    shown [] ["x"]
    _ <- comp 2
    shown ["aaxy"] []
    _ <- click (Within (respRect b))
    checkEq (respId b) =<< focusedId
    shown ["aa", "bb"] ["xy"]
    checkEq "b" . snd . snd =<< (pressKey KeyEnd >> pressKey KeyBackspace)
    _ <- comp 3
    shown ["bxyz"] []
    checkEq "aa" =<< liftIO (readVarIO aRef)

-- | Starting, changing or ending a composition repaints its field on an
-- idle frame.
runImeDamageTest :: Context -> IORef Int -> IO ()
runImeDamageTest ctx failed = do
  ref <- newVar "abcd"
  let damage = liftIO (takeDamage ctx)
  let ui = column (label "Above" *> held ref textInput' <* label "Below")
  runSim ctx failed inp {inputDeltaTime = 1} ui $ do
    (resp, _) <- pressTab 1 >> settle
    check . damageIsEmpty =<< damage
    check =<< (getInput >>= \i -> liftIO (needsRedraw ctx i (composing "x" 1 i)))
    forM_ [composing "x" 1, composing "xy" 2, commit ""] $ \step -> do
      check . (`clipCovers` respRect resp) =<< (frameWith step >> damage)
      check . damageIsEmpty =<< (frameWith step >> damage)

-- | IME keys fire no shortcuts. While composing, presses, releases and held
-- keys are dropped. On the commit frame, passed-on keys reach the field but
-- fire nothing, and the commit is inserted even with Ctrl held.
runImeShortcutsTest :: Context -> IORef Int -> IO ()
runImeShortcutsTest ctx failed = do
  fired <- newIORef ([] :: [T.Text])
  seen <- newIORef ([] :: [T.Text])
  let note r c = liftIO (modifyIORef' r (c :))
      widget v = do
        r <- textInput' v
        forM_ [ctrl <> key 's', key KeyEscape, ctrl <> key KeyEnter, ctrl <> key 'd'] $ \c ->
          whenM (shortcut c) (note fired (shortcutLabel c))
        forM_ [("pressed", keyPressed), ("released", keyReleased), ("held", keyHeld)] $ \(n, on) ->
          whenM (on (KeyChar 's')) (note seen n)
        pure r
      -- A frame on input @f@, with no key or modifier held after it.
      step f = do
        liftIO (writeIORef fired [] >> writeIORef seen [])
        (_, value) <- frameWith f
        modifyInput (\i -> i {inputKeysHeld = mempty, inputModifiers = noModifiers})
        liftIO ((,,) value <$> readIORef fired <*> readIORef seen)
      valueFired f = (\(v, fs, _) -> (v, fs)) <$> step f
      composingKeys =
        map chordInp [ctrl <> key 's', ctrl <> key 'd', key KeyEscape, ctrl <> key KeyEnter]
          ++ [keyUpInp (KeyChar 's'), \i -> i {inputKeysHeld = inputKeysFromList [KeyChar 's']}]
  field ctx failed "ab" 0 widget $ do
    _ <- compose "かな" 2
    forM_ composingKeys $ \f -> checkEq ("ab", [], []) =<< step f
    checkEq ("ab漢字", [], []) =<< step (chordInp (ctrl <> key 's') . commit "漢字")
    _ <- compose "かな" 2
    checkEq ("ab漢字かな", []) =<< valueFired (chordInp (ctrl <> key 'd') . commit "かな")
    -- A key passed on with the commit still reaches the field.
    _ <- compose "かな" 2
    checkEq ("ab漢字かなx", []) =<< valueFired (keyInp KeyLeft . commit "x")
    checkEq ("ab漢字かなyx", []) =<< valueFired (\i -> i {inputChars = "y"})
    forM_ [ctrl <> key 's', ctrl <> key 'd'] $ \c ->
      checkEq ("ab漢字かなyx", [shortcutLabel c]) =<< valueFired (chordInp c)

-- | A custom widget takes text with 'useInputMethod' while focused: it gets
-- the composition to draw, the IME gets its keys, and the IME area is its
-- caret. Unfocused, it requests and receives nothing.
runImeCustomWidgetTest :: Context -> IORef Int -> IO ()
runImeCustomWidgetTest ctx failed = do
  seen <- newIORef ([] :: [T.Text])
  let caret = Rect 40 20 2 16
      size = fixedWH 100 40 defaultLayout
      spec' = defaultCustomWidgetSpec {widgetFocusable = True, widgetKeys = KeysAll, widgetLayout = size}
      ui = column $ do
        wid <- nextId
        preedit <- useInputMethod wid InputNormal caret
        (_, ()) <- customWidgetWithId wid spec'
        whenM (shortcut (key KeyEscape)) (liftIO (modifyIORef' seen ("escape" :)))
        pure preedit
  runSim ctx failed inp ui $ do
    checkEq Nothing =<< compose "か" 1
    checkEq Nothing =<< imeArea
    checkEq Nothing =<< (commitText "" >> pressKey KeyTab >> frame)
    checkEq (Just (TextInputArea caret 0 InputNormal)) =<< imeArea
    checkEq (Just (Composition "か" 1 0)) =<< compose "か" 1
    _ <- pressKey KeyEscape
    checkEq [] =<< liftIO (readIORef seen)
    checkEq Nothing =<< commitText ""

-- | Fields report their input purpose (secure for passwords, numeric for
-- numbers), already on the frame Tab focuses them. Selectable labels and
-- buttons take no text.
runImePurposeTest :: Context -> IORef Int -> IO ()
runImePurposeTest ctx failed = do
  let ui = column $ do
        _ <- button "Before"
        _ <- withValue "secret" (textInputConfigured defaultTextInputConfig {ticPassword = True})
        _ <- withValue 3 numericInput
        _ <- withValue "notes" textArea
        selectableText "label"
      purpose = fmap textInputAreaPurpose <$> imeArea
  runSim ctx failed inp ui $ do
    let purposes = [Nothing, Just InputSecure, Just InputNumeric, Just InputNormal, Nothing]
    checkEq purposes =<< replicateM 5 (pressKey KeyTab >> purpose)
    checkEq Nothing =<< (frame >> purpose)
