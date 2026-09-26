-- | IME composition: shown at the focused field's caret without changing
-- the value, its keys edit nothing, its commit inserts text, and the frame
-- reports where the candidate window goes.
module Cases.Ime (tests) where

import Spec
import Data.IntMap.Strict qualified as IM
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import NanoUI.Internal.Store (Slot (..), WidgetStore (..), slotKey)
import NanoUI.Internal.Context (intKey, setDrawSquareGeometry)
import NanoUI.Shortcut

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

shifted :: Key -> Input
shifted k = (keyInp k inp) {inputModifiers = noModifiers {modShift = True}}

frames :: Context -> NanoUI a -> [Input] -> IO ()
frames ctx ui = mapM_ (\i -> runFrame ctx i ui)

-- | Every @yes@ text is a frame span, and no span contains any @no@ text.
shown :: HasCallStack => Context -> IORef Int -> [T.Text] -> [T.Text] -> IO ()
shown ctx failed yes no = do
  ts <- map (\(_, t, _, _, _) -> t) <$> collectTextSpans ctx
  assert failed (all (`elem` ts) yes && not (or [n `T.isInfixOf` t | n <- no, t <- ts]))

-- | @widget@ holding @v@ in a column, focused by Tab, caret @back@
-- characters from the end.
field :: Context -> T.Text -> Int -> (T.Text -> NanoUI (Response, T.Text)) -> IO (IORef T.Text, NanoUI (Response, T.Text))
field ctx v back widget = do
  ref <- newIORef v
  let ui = column (held ref widget)
  warmupFocused ctx inp ui
  frames ctx ui (replicate back (keyInp KeyLeft inp))
  pure (ref, ui)

-- | Composition persists until changed or ended; bad positions clamp.
runImeInputTest :: Context -> IORef Int -> IO ()
runImeInputTest _ failed = do
  let comp s l = inputComposition (applyComposition "かな" s l inp)
      next = clearEphemeral (composing "かな" 1 inp) {inputChars = "x"}
  assertEq failed [comp 1 1, comp (-1) (-1), comp 1 9] (Just <$> [Composition "かな" 1 1, Composition "かな" 2 0, Composition "かな" 1 1])
  assertEq failed (inputComposition (commit "" (composing "かな" 1 inp))) Nothing
  assertEq failed (inputComposition next, inputChars next) (Just (Composition "かな" 1 0), "")
  assert failed (inputInteracted inp (composing "か" 1 inp) && inputInteracted (composing "か" 1 inp) (composing "かな" 2 inp))

-- | Composition replaces any selection at the caret in plain, search and
-- (masked) password fields; the value and change flag are unchanged.
runImePreeditInlineTest :: Context -> IORef Int -> IO ()
runImePreeditInlineTest ctx failed = do
  (ref, ui) <- field ctx "abcd" 2 textInput'
  forM_ [("xyz", "abxyzcd"), ("xyzw", "abxyzwcd")] $ \(c, line) -> do
    (resp, value) <- evalUi ctx (composing c (T.length c) inp) ui
    assert failed (value == "abcd" && not (respChanged resp))
    shown ctx failed [line] ["abcd"]
  frames ctx ui [commit "" inp, shifted KeyRight, composing "xyz" 3 inp]
  shown ctx failed ["abxyzd"] []
  assertEq failed "abcd" =<< readIORef ref
  forM_ [(searchInput' "Find", "abxy", []), (textInputConfigured' defaultTextInputConfig {ticPassword = True}, "****", ["xy"])] $
    \(widget, line, hidden) -> do
      c <- newPixelContext
      (_, u) <- field c "ab" 0 widget
      _ <- runFrame c (composing "xy" 2 inp) u
      shown c failed [line] hidden

-- | A commit inserts at the caret and ends the composition before any new
-- one in the same input. An empty field shows the composition instead of
-- its placeholder.
runImeCommitTest :: Context -> IORef Int -> IO ()
runImeCommitTest ctx failed = do
  (_, ui) <- field ctx "abcd" 2 textInput'
  _ <- runFrame ctx (composing "かんじ" 3 inp) ui
  (resp, value) <- evalUi ctx (commit "漢字" inp) ui
  assert failed (value == "ab漢字cd" && respChanged resp)
  shown ctx failed ["ab漢字cd"] ["かんじ"]
  assertEq failed "ab漢字xcd" . snd =<< evalUi ctx (composing "y" 1 inp) {inputChars = "x"} ui
  shown ctx failed ["ab漢字xycd"] []
  c <- newPixelContext
  (_, u) <- field c "" 0 (textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Name"})
  _ <- runFrame c (composing "かな" 1 inp) u
  shown c failed ["かな"] ["Name"]
  assertJustM failed (textInputArea c) $ \(TextInputArea (Rect _ _ aw _) cursor _) -> assertEq failed (aw, cursor) (32, 16)
  assertEq failed "仮名" . snd =<< evalUi c (commit "仮名" inp) u

-- | While composing, keys belong to the IME: none edits, submits or moves
-- focus, and Escape does not quit. An empty composition counts as none.
runImeKeysTest :: Context -> IORef Int -> IO ()
runImeKeysTest ctx failed = do
  (_, ui) <- field ctx "abcd" 2 (\v -> textInput' v <* button "After")
  let comp = composing "xyz" 3 inp
      escape i = runFrame ctx (keyInp KeyEscape i) ui >> overlayConsumesQuit ctx (keyInp KeyEscape i)
  _ <- runFrame ctx comp ui
  forM_ [KeyBackspace, KeyDelete, KeyLeft, KeyRight, KeyHome, KeyEnd, KeyEnter, KeyEscape, KeyTab] $ \k -> do
    (resp, value) <- evalUi ctx (keyInp k comp) ui
    assert failed (value == "abcd" && not (respChanged resp || respSubmitted resp))
    assertEq failed (respId resp) =<< getFocusId ctx
  shown ctx failed ["abxyzcd"] []
  assert failed =<< escape comp
  _ <- runFrame ctx (commit "" inp) ui
  assert failed . not =<< escape inp
  assertEq failed "acd" . snd =<< evalUi ctx (keyInp KeyBackspace inp {inputComposition = Just (Composition "" 0 0)}) ui

-- | A text area shows the composition on the caret's row; over a multi-row
-- selection it replaces the first row's part. Enter does nothing, and the
-- commit replaces the selection.
runImeTextAreaTest :: Context -> IORef Int -> IO ()
runImeTextAreaTest ctx failed = do
  (_, ui) <- field ctx "one\ntwo\nthree" 0 textArea'
  frames ctx ui [keyInp KeyDown inp, keyInp KeyEnd inp]
  forM_ [composing "xy" 2 inp, keyInp KeyEnter (composing "xy" 2 inp)] $ \i -> do
    (resp, value) <- evalUi ctx i ui
    assert failed (value == "one\ntwo\nthree" && not (respChanged resp))
  shown ctx failed ["twoxy", "one"] []
  (resp, value) <- evalUi ctx (commit "続" inp) ui
  assert failed (value == "one\ntwo続\nthree" && respChanged resp)
  shown ctx failed ["two続"] []
  frames ctx ui [keyInp KeyUp inp, keyInp KeyHome inp, keyInp KeyRight inp, shifted KeyDown, shifted KeyRight]
  (resp2, value2) <- evalUi ctx (composing "XY" 2 inp) ui
  assert failed (value2 == value && not (respChanged resp2))
  shown ctx failed ["oXY", "two続", "three"] ["one"]
  (resp3, value3) <- evalUi ctx (commit "Z" inp) ui
  assert failed (value3 == "oZo続\nthree" && respChanged resp3)

-- | 'textInputArea', composition painting, and click placement over a
-- composition, in a field and a text area. No area once focus is gone.
runImeAreaPaintTest :: Context -> IORef Int -> IO ()
runImeAreaPaintTest ctx failed = do
  setDrawSquareGeometry ctx True
  theme <- getTheme ctx
  ref <- newIORef "abcd"
  areaRef <- newIORef "one\ntwo"
  let cell = 16
      ui = column (held ref textInput' <* held areaRef textArea')
      near a b = abs (a - b) <= 1
      paint step = do
        writeIORef (ctxPaintFull ctx) True
        (_, _, draw, _) <- runFrame ctx step ui
        (,) <$> drawQuads draw <*> collectTextSpans ctx
      carets quads cx = [r | (r@(Rect x _ w h), _) <- quads, near x cx, w <= 1.5, h >= cell / 2]
      -- On the row showing @line@: a composition @len@ cells long starting
      -- at cell @at@, IME caret @c@ cells in.
      check line at len c step = do
        (quads, spans) <- paint step
        assertJust failed (spanRectOf line spans) $ \(Rect sx sy _ sh) -> do
          let x0 = sx + at * cell
          assertJustM failed (textInputArea ctx) $ \(TextInputArea (Rect ax ay aw ah) cursor _) ->
            assertEq failed (ax, ay, aw, ah, cursor) (x0, sy, len * cell, cell, c * cell)
          assert failed (or [near x x0 && near w (len * cell) && h <= 2 && y > sy + sh / 2 && y < sy + sh + 2 | (Rect x y w h, _) <- quads])
          -- The IME caret is drawn, not the field's caret at its start.
          assert failed (not (null (carets quads (x0 + c * cell))) && null (carets quads x0))
  warmupFocused ctx inp ui
  frames ctx ui (replicate 2 (keyInp KeyLeft inp))
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf "abcd" spans) $ \(Rect sx sy _ sh) -> do
    assertJustM failed (textInputArea ctx) $ \(TextInputArea r cursor _) -> assertEq failed (r, cursor) (Rect (sx + 2 * cell) sy 1 cell, 0)
    check "abxyzcd" 2 3 1 (composing "xyz" 1 inp)
    -- A selection inside the composition is highlighted, with no caret.
    (quads, _) <- paint (applyComposition "xyz" 0 2 inp)
    assert failed (any (\(Rect x _ w _, col) -> near x (sx + 2 * cell) && near w (2 * cell) && col == themeSelection theme) quads)
    assert failed (null (carets quads (sx + 2 * cell)) && null (carets quads (sx + 4 * cell)))
    -- A click between the shown "c" and "d" lands after the value's "c".
    assertEq failed "abcd" . snd =<< runClick ctx (applyComposition "xyz" 0 2 inp) ui (V2 (sx + 6 * cell + 1) (sy + sh / 2))
    shown ctx failed ["abcxyzd"] []
  -- The text area's caret starts at the document's start.
  frames ctx ui [commit "" inp, tabInp inp, keyInp KeyDown inp, keyInp KeyRight inp]
  check "txywo" 1 2 2 (composing "xy" 2 inp)
  _ <- runClick ctx (commit "" inp) ui (V2 390 290)
  assertEq failed Nothing =<< textInputArea ctx

-- | A long composition scrolls a narrow field to keep the IME caret in
-- view, and scrolls back when the caret returns to the start.
runImeScrollTest :: Context -> IORef Int -> IO ()
runImeScrollTest ctx failed = do
  (_, ui) <- field ctx "abc" 0 (textInputConfigured' defaultTextInputConfig {ticLayout = fixedW 120 defaultLayout})
  (resp, _) <- evalUi ctx (composing "0123456789" 10 inp) ui
  store <- getStore ctx
  let Rect fx _ fw _ = respRect resp
      caretIn ok = assertJustM failed (textInputArea ctx) $ \(TextInputArea (Rect ax _ _ _) cursor _) -> assert failed (ok (ax + cursor))
  assertGt failed (IM.findWithDefault 0 (slotKey SlotTextInputScroll (intKey (respId resp))) (storeFloat store)) 0
  caretIn (\x -> x >= fx && x <= fx + fw)
  _ <- runFrame ctx (composing "0123456789" 0 inp) ui
  caretIn (>= fx)

-- | The area follows the shaped line: a Hebrew composition after Latin text
-- runs right to left, with caret 0 at its right edge.
runImeBidiTest :: Context -> IORef Int -> IO ()
runImeBidiTest base failed = do
  let line = "ab\x05D0\x05D1"
      shape t = if t == line then Just (ShapedText 40 40 (primArrayFromList [0, 10, 40, 30, 20])) else Nothing
      ctx = withFontMetrics base (monospaceMetrics 10) {fmShape = shape}
  (_, ui) <- field ctx "ab" 0 textInput'
  _ <- runFrame ctx (composing "\x05D0\x05D1" 0 inp) ui
  spans <- collectTextSpans ctx
  assertJust failed (spanRectOf line spans) $ \(Rect sx _ _ _) ->
    forM_ [(0, 20), (2, 0)] $ \(c, cursor) -> do
      _ <- runFrame ctx (composing "\x05D0\x05D1" c inp) ui
      assertJustM failed (textInputArea ctx) $ \(TextInputArea (Rect ax _ aw _) cur _) ->
        assertEq failed (ax, aw, cur) (sx + 20, 20, cursor)

-- | A composition shows only in the field focused when it last changed.
-- After focus moves it belongs to no field, and keys work again until it
-- changes.
runImeOwnerTest :: Context -> IORef Int -> IO ()
runImeOwnerTest ctx failed = do
  aRef <- newIORef "aa"
  bRef <- newIORef "bb"
  let ui = column ((,) <$> held aRef textInput' <*> held bRef textInput')
      comp n = composing (T.take n "xyz") n inp
  -- With no focus it is shown nowhere, and Tab still moves focus.
  warmup ctx inp ui
  _ <- runFrame ctx (comp 1) ui
  shown ctx failed [] ["x"]
  _ <- runFrame ctx (tabInp (comp 1)) ui
  ((a, _), (b, _)) <- evalUi ctx (comp 1) ui
  assertEq failed (respId a) =<< getFocusId ctx
  -- Unchanged, it stays out of the field Tab reached.
  shown ctx failed [] ["x"]
  _ <- runFrame ctx (comp 2) ui
  shown ctx failed ["aaxy"] []
  _ <- runClick ctx (comp 2) ui (spanCenter (respRect b))
  assertEq failed (respId b) =<< getFocusId ctx
  shown ctx failed ["aa", "bb"] ["xy"]
  _ <- runFrame ctx (keyInp KeyEnd (comp 2)) ui
  assertEq failed "b" . snd . snd =<< evalUi ctx (keyInp KeyBackspace (comp 2)) ui
  _ <- runFrame ctx (comp 3) ui
  shown ctx failed ["bxyz"] []
  assertEq failed "aa" =<< readIORef aRef

-- | Starting, changing or ending a composition repaints its field on an
-- idle frame.
runImeDamageTest :: Context -> IORef Int -> IO ()
runImeDamageTest ctx failed = do
  ref <- newIORef "abcd"
  let i = inp {inputDeltaTime = 1}
      ui = column (label "Above" *> held ref textInput' <* label "Below")
  warmupFocused ctx i ui
  (resp, _) <- warmup2 ctx i ui
  assert failed . damageIsEmpty =<< takeDamage ctx
  assert failed =<< needsRedraw ctx i (composing "x" 1 i)
  forM_ [composing "x" 1 i, composing "xy" 2 i, commit "" i] $ \step -> do
    _ <- runFrame ctx step ui
    assert failed . (`clipCovers` respRect resp) =<< takeDamage ctx
    _ <- runFrame ctx step ui
    assert failed . damageIsEmpty =<< takeDamage ctx

-- | A view rerun after a hook write shows the composition and commits it
-- once.
runImeRepeatedPassTest :: Context -> IORef Int -> IO ()
runImeRepeatedPassTest ctx failed = do
  ref <- newIORef "ab"
  let ui = column $ do
        (n, setN) <- useInt 0
        held ref textInput' <* setN (n + 1)
  warmupFocused ctx inp ui
  _ <- runFrame ctx (composing "xy" 2 inp) ui
  shown ctx failed ["abxy"] []
  _ <- runFrame ctx (commit "z" inp) ui
  assertEq failed "abz" =<< readIORef ref

-- | IME keys fire no shortcuts. While composing, presses, releases and held
-- keys are dropped. On the commit frame, passed-on keys reach the field but
-- fire nothing, and the commit is inserted even with Ctrl held.
runImeShortcutsTest :: Context -> IORef Int -> IO ()
runImeShortcutsTest ctx failed = do
  fired <- newIORef ([] :: [T.Text])
  seen <- newIORef ([] :: [T.Text])
  let note r c = uiIO (modifyIORef' r (c :))
  (_, ui) <- field ctx "ab" 0 $ \v -> do
    r <- textInput' v
    forM_ [ctrl <> key 's', key KeyEscape, ctrl <> key KeyEnter, ctrl <> key 'd'] $ \c -> whenM (shortcut c) (note fired (shortcutLabel c))
    forM_ [("pressed", keyPressed), ("released", keyReleased), ("held", keyHeld)] $ \(n, on) ->
      whenM (on (KeyChar 's')) (note seen n)
    pure r
  let frame i = do
        writeIORef fired [] >> writeIORef seen []
        (_, value) <- evalUi ctx i ui
        (,,) value <$> readIORef fired <*> readIORef seen
      valueFired i = (\(v, f, _) -> (v, f)) <$> frame i
      comp = composing "かな" 2 inp
  _ <- frame comp
  let heldS = comp {inputKeysHeld = inputKeysFromList [KeyChar 's']}
  forM_ (map (`chordInp` comp) [ctrl <> key 's', ctrl <> key 'd', key KeyEscape, ctrl <> key KeyEnter] ++ [keyUpInp (KeyChar 's') comp, heldS]) $ \i ->
    assertEq failed ("ab", [], []) =<< frame i
  assertEq failed ("ab漢字", [], []) =<< frame (chordInp (ctrl <> key 's') (commit "漢字" inp))
  _ <- frame comp
  assertEq failed ("ab漢字かな", []) =<< valueFired (chordInp (ctrl <> key 'd') (commit "かな" inp))
  _ <- frame comp
  -- A key passed on with the commit still reaches the field.
  assertEq failed ("ab漢字かなx", []) =<< valueFired (keyInp KeyLeft (commit "x" inp))
  assertEq failed ("ab漢字かなyx", []) =<< valueFired inp {inputChars = "y"}
  forM_ [ctrl <> key 's', ctrl <> key 'd'] $ \c -> assertEq failed ("ab漢字かなyx", [shortcutLabel c]) =<< valueFired (chordInp c inp)

-- | A custom widget takes text with 'useInputMethod' while focused: it gets
-- the composition to draw, the IME gets its keys, and the IME area is its
-- caret. Unfocused, it requests and receives nothing.
runImeCustomWidgetTest :: Context -> IORef Int -> IO ()
runImeCustomWidgetTest ctx failed = do
  seen <- newIORef ([] :: [T.Text])
  let caret = Rect 40 20 2 16
      ui = column $ do
        wid <- nextId
        preedit <- useInputMethod wid InputNormal caret
        (_, ()) <- customWidgetWithId wid defaultCustomWidgetSpec {widgetFocusable = True, widgetKeys = KeysAll, widgetLayout = fixedWH 100 40 defaultLayout}
        whenM (shortcut (key KeyEscape)) (uiIO (modifyIORef' seen ("escape" :)))
        pure preedit
  warmup ctx inp ui
  assertEq failed Nothing =<< evalUi ctx (composing "か" 1 inp) ui
  assertEq failed Nothing =<< textInputArea ctx
  _ <- runFrame ctx (tabInp inp) ui
  assertEq failed Nothing =<< evalUi ctx inp ui
  assertEq failed (Just (TextInputArea caret 0 InputNormal)) =<< textInputArea ctx
  assertEq failed (Just (Composition "か" 1 0)) =<< evalUi ctx (composing "か" 1 inp) ui
  _ <- runFrame ctx (keyInp KeyEscape (composing "か" 1 inp)) ui
  assertEq failed [] =<< readIORef seen
  assertEq failed Nothing =<< evalUi ctx (commit "" inp) ui

-- | Fields report their input purpose (secure for passwords, numeric for
-- numbers), already on the frame Tab focuses them. Selectable labels and
-- buttons take no text.
runImePurposeTest :: Context -> IORef Int -> IO ()
runImePurposeTest ctx failed = do
  let ui = column $ do
        _ <- button "Before"
        _ <- textInputConfigured defaultTextInputConfig {ticPassword = True} "secret"
        _ <- numericInput 3
        _ <- textArea "notes"
        selectableText "label"
  warmup2 ctx inp ui
  let purposeAfter i = runFrame ctx i ui >> fmap textInputAreaPurpose <$> textInputArea ctx
  assertEq failed [Nothing, Just InputSecure, Just InputNumeric, Just InputNormal, Nothing]
    =<< mapM purposeAfter (replicate 5 (tabInp inp))
  assertEq failed Nothing =<< purposeAfter inp
