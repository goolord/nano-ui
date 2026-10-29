module Cases.TextInput (tests) where

import Spec
import Cases.SimExtra
import Data.IntMap.Strict qualified as IM
import Data.Text qualified as T
import GHC.Stack (HasCallStack)
import NanoUI.Internal.Context (Context (..), intKey)
import NanoUI.Internal.Frame.TextArea
  ( TextAreaBars (..)
  , TextAreaHit (..)
  , resolveTextAreaFont
  , textAreaBarLane
  , textAreaBarLayouts
  , textAreaBars
  , textAreaContentMetrics
  , textAreaHitForWidget
  , textAreaLineHeight
  )
import NanoUI.Internal.Input (onMac)
import NanoUI.Internal.Store (Slot (..), WidgetStore (..), slotKey)
import NanoUI.Internal.Widgets.TextArea (TextAreaState, loadTextAreaState, tasBuffer, tasSelectionAnchor)
import NanoUI.Shortcut
import NanoUI.Testing.Sim
import NanoUI.Widgets.TextBuffer (fromText, getCursor, lineAt, toLines, toText)

tests :: [Spec]
tests =
  [ spec "text-edits" runEditsTest
  , pixelSpec "text-input-press-focus" runFieldPressFocusTest
  , spec "text-input-search-clear" runSearchClearTest
  , spec "text-input-menu-clipboard" runMenuClipboardTest
  , spec "text-idle" runTextIdleTest
  , spec "text-input-scroll" runFieldScrollTest
  , spec "text-undo" runTextUndoTest
  , spec "text-area-document" runTextAreaDocumentTest
  , spec "text-area-width-tracking" runTextAreaWidthTrackingTest
  , pixelSpec "text-area-scrolling" runTextAreaScrollingTest
  , pixelSpec "text-area-zoom-scroll" runTextAreaZoomScrollTest
  , pixelSpec "text-area-remount-scroll" runTextAreaRemountScrollTest
  , pixelSpec "text-area-menu" runTextAreaMenuTest
  , pixelSpec "text-area-bar-range" runTextAreaBarRangeTest
  , pixelSpec "text-area-caret-in-view" runCaretInViewTest
  , spec "text-area-font" runTextAreaFontTest
  ]

type Field = (Response, T.Text)

-- | Each case puts its text in a fresh field, whose clipboard holds "clip",
-- and runs its steps, which check the value and clipboard as they go.
editCases :: [(Var T.Text -> NanoUI Response, T.Text, [Sim Field Field])]
editCases =
  [ -- Shift+Left selects back from the caret, typing replaces the selection,
    -- Cut leaves none, and Select All types nothing though the backend also
    -- sends its letter as text.
    ( textInput'
    , "hello"
    , [ tabKey, shiftLeft, shiftLeft, typeText "X", is "helX"
      , shiftLeft, command 'x', clipIs "X", typeText "z", is "helz"
      , chordTyping (cmdOrCtrl <> key 'a') "a", command 'c', clipIs "helz", is "helz"
      , pressKey KeyBackspace, is "", command 'v', is "helz"
      ]
    )
  , -- An event filtered to nothing keeps the selection, and navigation
    -- follows text inserted in the same frame.
    ( textInput'
    , "aOLDz"
    , [ tabKey, pressKey KeyLeft, shiftLeft, shiftLeft, shiftLeft, typeText "\n\t", is "aOLDz"
      , frameWith (\i -> (keyInp KeyLeft i) {inputChars = "é\n世界\t"}), is "aé世界z"
      , typeText "X", is "aé世X界z"
      ]
    )
  , -- Word-wise keys, as in the text area.
    ( textInput'
    , "hello world"
    , [ tabKey, chord (ctrl <> key KeyBackspace), is "hello ", chord (ctrl <> key KeyDelete), is "hello "
      , chord (ctrl <> key KeyLeft), typeText "X", is "Xhello "
      , chord (ctrl <> key KeyDelete), is "X "
      ]
    )
  , -- A value holding a line break keeps its caret as an offset into the
    -- whole value, and Select All selects every character.
    ( textInput'
    , "ab\ncd"
    , [tabKey, pressKey KeyLeft, typeText "X", is "ab\ncXd", command 'a', typeText "Y", is "Y"]
    )
  , -- A Windows line end stays two characters: a key that edits nothing
    -- reports no change.
    ( textInput'
    , "ab\r\ncd"
    , [tabKey, changes False (pressKey KeyLeft), is "ab\r\ncd", typeText "X", is "ab\r\ncXd"]
    )
  , -- A character typed with a command modifier, Alt (Command with Option on
    -- macOS), is the shortcut's, also a frame after the press. AltGr
    -- (Ctrl+Alt) and Option on macOS type, as does X11's AltGr reported as Alt
    -- alone when the character is not its key's.
    ( textInput'
    , "ab"
    , [ tabKey, chordTyping (commandMods <> key 'f') "f", is "ab", chordTyping commandMods "f", is "ab"
      , chordTyping ((if onMac then alt else ctrl <> alt) <> key 'f') "@", is "ab@"
      , chordTyping (alt <> key 'q') "@", is "ab@@"
      ]
    )
  , -- A drag across the text selects it, a double click a word and a triple
    -- click the line.
    (textInput', "hello", [dragAcross "hello", typeText "z", is "z"])
  , (textInput', "hello world", [clicksAtStart 2 "hello world", pressKey KeyBackspace, is " world"])
  , (textInput', "hello", [clicksAtStart 3 "hello", pressKey KeyBackspace, is ""])
  , -- A press lands on the glyph boundary under it in a run of f's.
    (textInput', "ffffff", [clickCells 3 "ffffff", typeText "x", is "fffxfff"])
  , -- Tab focus stores no caret, which reads as the text's end: a press
    -- elsewhere collapses onto it, so Tab back and typing appends.
    (textInput', "hello", [tabKey, click (At (V2 390 190)), tabKey, typeText "X", is "helloX"])
  , -- A password field shows a mask and copies nothing.
    ( textInputConfigured' defaultTextInputConfig {ticPassword = True}
    , "hunter2"
    , [ expectNot "hunter2" >> expect "*******" >> tabKey
      , command 'a', command 'c', clipIs "clip", is "hunter2"
      ]
    )
  , -- The edit menu offers Paste unfocused, and its Paste and Cut edit through
    -- the clipboard, surviving the caller passing back the frame before's value.
    ( textInput'
    , "hello"
    , [ rightClick "hello", settle, click "Paste", is "helloclip"
      , rightClick "helloclip", settle, click "Cut", is "", clipIs "helloclip"
      , frame, expectNot "Cut" >> result
      ]
    )
  , -- A text area reports an edit as 'respChanged', not a caret move or a
    -- selection, and Cut leaves no selection.
    ( textArea'
    , "hello"
    , [ tabKey, changes False (pressKey KeyLeft), changes False (command 'a'), command 'x'
      , clipIs "hello", is "", changes True (typeText "z"), is "z"
      ]
    )
  ]
  where
    tabKey = pressKey KeyTab
    shiftLeft = chord (shift <> key KeyLeft)
    commandMods = if onMac then super <> alt else alt

runEditsTest :: Context -> IORef Int -> IO ()
runEditsTest _ failed = forM_ editCases $ \(widget, initial, steps) -> do
  clip <- newIORef (Just "clip")
  ctx <- (\c -> withClipboard c (readIORef clip) (\t -> writeIORef clip (Just t) >> pure True)) <$> newContext
  ref <- newVar initial
  runSim ctx failed (withInput 400 200) (column (held ref widget)) (sequence_ steps)

-- | The field's value is @want@.
is :: HasCallStack => T.Text -> Sim Field Field
is want = result >>= \r -> r <$ checkEq want (snd r)

-- | The clipboard holds @want@.
clipIs :: HasCallStack => T.Text -> Sim a a
clipIs want = do
  ctx <- simContext
  checkEq (Just want) =<< liftIO (ctxClipboardGet ctx)
  result

-- | Run the step and check whether it reported a change.
changes :: HasCallStack => Bool -> Sim Field Field -> Sim Field Field
changes want step = step >>= \r -> r <$ checkEq want (respChanged (fst r))

-- | Ctrl+@c@, Command+@c@ on macOS.
command :: Char -> Sim a a
command c = chord (cmdOrCtrl <> key c)

-- | 'chord', typing @txt@ on the same frame.
chordTyping :: Shortcut -> T.Text -> Sim a a
chordTyping s txt = do
  mods <- inputModifiers <$> getInput
  a <- frameWith (\i -> (chordInp s i) {inputChars = txt})
  a <$ modifyInput (\i -> i {inputModifiers = mods})

-- | Drag from the first to the last character of the visible text.
dragAcross :: T.Text -> Sim a a
dragAcross txt = do
  Rect x y w h <- rectOf txt
  drag (At (V2 (x + 1) (y + h / 2))) (At (V2 (x + w - 1) (y + h / 2))) 1

-- | @n@ quick clicks at the start of the visible text.
clicksAtStart :: Int -> T.Text -> Sim a a
clicksAtStart n txt = do
  Rect x y _ h <- rectOf txt
  forM_ [1 .. n] $ \k ->
    frameWith (\i -> (pressAt i (V2 (x + 1) (y + h / 2))) {inputMouseClicks = k}) >> release
  result

-- | Click the visible text @n@ characters in, measured as a run.
clickCells :: Int -> T.Text -> Sim a a
clickCells n txt = do
  fm <- ctxFontMetrics <$> simContext
  Rect x y _ h <- rectOf txt
  click (At (V2 (x + lineWidth fm (T.take n txt)) (y + h / 2)))

-- | A text area under a separate label.
labeledArea :: T.Text -> T.Text -> NanoUI (Response, T.Text)
labeledArea lbl initial = label lbl >> withValue initial textArea'

-- | Forty numbered lines, taller than a text area.
fortyLines :: T.Text
fortyLines = T.unlines ["Line " <> T.pack (show (i :: Int)) | i <- [1 .. 40]]

-- | Run the commands queued in @q@ on the widget, after it.
withCommands :: IORef [TextCommand] -> NanoUI (Response, b) -> NanoUI (Response, b)
withCommands q widget = do
  r <- widget
  mapM_ (runTextCommand (respId (fst r))) =<< liftIO (atomicModifyIORef' q ([],))
  pure r

textAreaState :: Context -> WidgetId -> Sim a TextAreaState
textAreaState ctx wid = liftIO ((`loadTextAreaState` intKey wid) <$> getStore ctx)

-- | A press near the top of a field focuses it, and it stays focused.
runFieldPressFocusTest :: Context -> IORef Int -> IO ()
runFieldPressFocusTest ctx failed = do
  let field = withValue "" (textInputConfigured' defaultTextInputConfig {ticPlaceholder = "Enter Name"})
  runSim ctx failed (withInput 320 120) (column (label "Name" >> field)) $ do
    wid <- respId . fst <$> result
    Rect x y w _ <- rectOf "Enter Name"
    checkEq wid =<< (press (At (V2 (x + w / 2) (y + 0.5))) >> focusId)
    checkEq (replicate 5 wid) =<< (release >> replicateM 5 (frame >> focusId))

-- | Emptying a search field from the keyboard reports the change on that
-- frame, as its clear button does, rather than after the debounce. The clear
-- button empties it and repaints a clip, not the window.
runSearchClearTest :: Context -> IORef Int -> IO ()
runSearchClearTest ctx failed = do
  ref <- newVar "abc"
  runSim ctx failed (withInput 320 120) (column (fst <$> held ref (searchInput' "Search"))) $ do
    resp <- click . On =<< result
    let Rect x y w h = respRect resp
        fm = ctxFontMetrics ctx
        iconW = max 12 (min 15 (fmLineHeight fm * 0.8))
    check . respChanged =<< (command 'a' >> pressKey KeyBackspace)
    checkEq "" =<< liftIO (readVarIO ref)
    _ <- typeText "abc" >> settle
    _ <- click (At (V2 (x + w - fst (widgetContentInset fm) - iconW / 2) (y + h / 2)))
    check . (/= DamageFull) =<< liftIO (takeDamage ctx)
    checkEq "" =<< liftIO (readVarIO ref)

-- | An open edit menu paints and hovers its Paste row without reading the
-- clipboard, a round trip to its owner on X11. A press on a row that cannot
-- run (nothing to undo) closes the menu with no full repaint.
runMenuClipboardTest :: Context -> IORef Int -> IO ()
runMenuClipboardTest base failed = do
  clipReads <- newIORef (0 :: Int)
  let ctx = withClipboard base (Nothing <$ modifyIORef' clipReads (+ 1)) (\_ -> pure True)
  runSim ctx failed (withInput 320 160) (column (withValue "hello" textInput')) $ do
    _ <- rightClick "hello" >> settle
    Rect px py pw ph <- rectOf "Paste"
    mapM_ (\x -> moveTo (At (V2 x (py + ph / 2)))) [px + 2, px + pw / 2]
    checkEq 0 =<< liftIO (readIORef clipReads)
    _ <- click "Undo"
    check . (/= DamageFull) =<< liftIO (takeDamage ctx)
    expectNot "Undo"

-- | Text widgets let the loop sleep: a focused field asks for no frame or
-- wake (its caret does not blink) until input or a redraw request arrives. A
-- selection drag held still past the field's edge keeps waking, so the text
-- scrolls under it, until the button comes up. A button held still in a text
-- area after a click selects nothing new and asks for nothing.
runTextIdleTest :: Context -> IORef Int -> IO ()
runTextIdleTest ctx failed = do
  let ui = column ((,) <$> withValue "some text to select" textInput' <*> withValue "some\ntext" textArea')
      wake = liftIO (getWakeAt ctx)
      needs f = getInput >>= \i -> liftIO (needsRedraw ctx i (f i))
  runSim ctx failed (withInput 320 220) ui $ do
    ((field, _), (area, _)) <- result
    let Rect fx fy fw _ = respRect field
        grab = At (V2 (fx + 4) (fy + 4))
        grabArea = At (V2 (rectX (respRect area) + 20) (rectY (respRect area) + 10))
    _ <- click grab
    modifyInput (\i -> i {inputDeltaTime = 1})
    _ <- settle
    check . not =<< needs id
    checkEq 0 =<< wake
    check =<< needs (\i -> i {inputChars = "x"})
    check =<< needs (\i -> i {inputWindowRedraw = True})
    _ <- press grab >> frame
    checkEq 0 =<< wake
    _ <- moveTo (At (V2 (fx + fw + 40) (fy + 4)))
    above 0 =<< wake
    _ <- release >> frame
    checkEq 0 =<< wake
    _ <- drag grabArea grabArea 1 >> frame >> frame
    _ <- targetPoint grabArea >>= \p -> frameWith (`holdAt` p)
    check . not =<< wantsFrame
    checkEq 0 =<< wake

-- | End scrolls a long field to show its caret, and Home scrolls back. A
-- field in its own font lays out by that font: its text, caret and hit tests
-- start at its content inset (30 px in for 24 px cells, so a press just past
-- the start of the "c" puts the caret before it, not a cell later), and 17
-- cells end its caret 408 px in, past the 300 px field, where the 12 px base
-- font would not scroll.
runFieldScrollTest :: Context -> IORef Int -> IO ()
runFieldScrollTest _ failed = do
  pixel <- newPixelContext
  sized <- withMonospaceFonts 12 24 <$> newContext
  let big = defaultTextInputConfig {ticLayout = fontSize 24 (fixedW 300 (ticLayout defaultTextInputConfig))}
      scrollsBack :: Context -> WidgetId -> Float -> Sim a ()
      scrollsBack ctx wid past = do
        let slot = slotKey SlotTextInputScroll (intKey wid)
            scrollX = liftIO (IM.findWithDefault 0 slot . storeFloat <$> getStore ctx)
        above past =<< (pressKey KeyEnd >> scrollX)
        checkEq 0 =<< (pressKey KeyHome >> scrollX)
      long = "VeryLongTextEnteredIntoTheFieldThatExceedsTheWidth"
  runSim pixel failed (withInput 200 200) (column (fst <$> withValue long textInput')) $ do
    wid <- respId <$> (click . On =<< result)
    scrollsBack pixel wid 0
  ref <- newVar "abcdef"
  runSim sized failed (withInputOff 400 200) (column (held ref (textInputConfigured' big))) $ do
    (resp, _) <- result
    Rect penX penY _ penH <- rectOf "abcdef"
    checkEq (rectX (respRect resp) + 24 * 1.25) penX
    _ <- click (At (V2 (penX + 2 * 24 + 4) (penY + penH / 2)))
    checkEq "abXcdef" . snd =<< typeText "X"
    _ <- pressKey KeyEnd >> typeText "ghijklmnop"
    scrollsBack sized (respId resp) (408 - 300)

-- | Ctrl+Z and Ctrl+Shift+Z or Ctrl+Y (Command on macOS) undo and redo typing;
-- commands run from outside the frame edit the field and pulse 'respChanged'
-- once; replacing the value the field is passed clears its history.
runTextUndoTest :: Context -> IORef Int -> IO ()
runTextUndoTest ctx failed = do
  ref <- newVar ""
  q <- newIORef []
  let ui = column (withCommands q (held ref textInput') >>= \(r, _) -> (,) r <$> textCanUndo (respId r))
      value = liftIO (readVarIO ref)
      afterChord c = chord (cmdOrCtrl <> c) >> value
  runSim ctx failed (withInput 320 120) ui $ do
    pressTab 1
    mapM_ (typeText . T.singleton) ("red fox" :: String)
    check . snd =<< frame
    checkEq ["red ", "", "red ", "red fox"] =<< mapM afterChord [key 'z', key 'z', shift <> key 'z', key 'y']
    liftIO (writeIORef q [InsertText "!"])
    checkEq [True, False] . drop 1 =<< replicateM 3 (respChanged . fst <$> frame)
    checkEq "red fox!" =<< value
    liftIO (writeIORef q [Undo])
    checkEq "red fox" =<< (frame >> frame >> value)
    liftIO (writeVarIO ref "something else")
    check . not . snd =<< (frame >> frame)

-- | A text area over a 'TextDocument' hands back the document it was given
-- while nothing edits it (a caret move included) and a new one on edits, with
-- undo, and replacing the document clears its history. A caller that builds
-- an equal document every frame lets the loop sleep.
runTextAreaDocumentTest :: Context -> IORef Int -> IO ()
runTextAreaDocumentTest ctx failed = do
  let original = T.intercalate "\n" ["line " <> T.pack (show i) | i <- [0 .. 999 :: Int]]
  ref <- newVar (textDocument original)
  let ui = column (held ref textAreaDocument' >>= \(r, doc) -> (,) (r, doc) <$> textCanUndo (respId r))
      -- Whether the step reported a change and handed back the same document.
      edit step = do
        before <- liftIO (readVarIO ref)
        ((r, after), undoable) <- step
        pure ((respChanged r, sameDocument before after, undoable), after)
      quiet step = checkEq (False, True) . (\((c, s, _), _) -> (c, s)) =<< edit step
  runSim ctx failed (withInput 320 220) ui $ do
    pressTab 1 >> quiet frame >> quiet (pressKey KeyDown)
    (flags, typed) <- edit (typeText "x")
    checkEq ((True, False, True), "xline 1", 1000) (flags, documentLine 1 typed, documentLineCount typed)
    check . (\((_, s, _), _) -> s) =<< edit frame
    _ <- chord (cmdOrCtrl <> key 'z')
    checkEq original . documentText =<< liftIO (readVarIO ref)
    liftIO (writeVarIO ref (textDocument "fresh"))
    ((_, _, undoable), fresh) <- frame >> edit frame
    checkEq (textDocument "fresh", False) (fresh, undoable)
  copyCtx <- newContext
  source <- newIORef original
  -- Split inside the frame, so each frame passes a new copy.
  let copies = column (textAreaDocument . constVar =<< liftIO (textDocument <$> readIORef source))
      idle = (withInput 320 220) {inputDeltaTime = 1}
  _ <- warmup2 copyCtx idle copies >> warmup2 copyCtx idle copies
  assert failed . not =<< needsRedraw copyCtx idle idle

-- | The content width a text area keeps up to date line by line matches a
-- fresh measurement after edits that widen, move and shorten its widest line.
runTextAreaWidthTrackingTest :: Context -> IORef Int -> IO ()
runTextAreaWidthTrackingTest ctx failed = do
  let shorts = replicate 200 "short line"
  ref <- newVar (T.intercalate "\n" (shorts ++ ["the widest line of them all"] ++ shorts))
  runSim ctx failed (withInput 400 300) (column (held ref (textAreaWith' grow))) $ do
    let widthsMatch = do
          (resp, text) <- frame
          liftIO (textAreaHitForWidget ctx (respId resp)) >>= (`checkJust` \hit -> do
            (tracked, _) <- liftIO (textAreaContentMetrics ctx (tahNodeIdx hit))
            fm <- liftIO (resolveTextAreaFont ctx (tahNodeIdx hit))
            widths <- liftIO (mapM (lineWidthIO fm) (T.splitOn "\n" text))
            checkEq (maximum widths) tracked)
    pressTab 1 >> widthsMatch
    -- Widen a short line past the widest, then shorten it again.
    replicateM_ 10 (pressKey KeyDown) >> replicateM_ 40 (typeText "x") >> widthsMatch
    replicateM_ 40 (pressKey KeyBackspace) >> widthsMatch
    -- Delete the widest line itself, then undo.
    replicateM_ 190 (pressKey KeyDown) >> chord (shift <> key KeyEnd) >> pressKey KeyBackspace >> widthsMatch
    chord (cmdOrCtrl <> key 'z') >> widthsMatch

-- | The pointer shows the text cursor over a text area's text, not its label,
-- and grab cursors on its thumbs, whose bars keep out of each other's corner.
-- Dragging a thumb or turning the wheel scrolls the area, clamped at the
-- start, without moving its caret, selecting or touching its text, and an idle
-- frame does not snap back. Typing brings the caret back into view, and a
-- click in scrolled text lands on the column under the pointer. Text that
-- fits does not scroll.
runTextAreaScrollingTest :: Context -> IORef Int -> IO ()
runTextAreaScrollingTest ctx failed = do
  let rows = [T.pack (show i) <> " - " <> T.replicate 10 "abcdefghij" | i <- [1 .. 40 :: Int]]
      txt = T.unlines rows
      fm = ctxFontMetrics ctx
  runSim ctx failed (withInput 320 220) (column (labeledArea "Notes" txt)) $ do
    (resp, _) <- result
    let field = respRect resp
        offsets = liftIO (getScrollOffset2D ctx (respId resp))
        wheelBy d = wheel (Within field) d >> offsets
        bars = textAreaBars fm field (maximum (map (lineWidth fm) rows)) (40 * textAreaLineHeight fm)
        caret = getCursor . tasBuffer <$> textAreaState ctx (respId resp)
    checkEq UiCursorDefault =<< cursorAt "Notes"
    checkEq UiCursorText =<< cursorAt (At (V2 (rectX field + 20) (rectY field + 20)))
    checkJust (uncurry (liftA2 (,)) (textAreaBarLayouts field bars 0 0)) $ \(v, h) -> do
      check (rectY (sbTrack v) + rectH (sbTrack v) <= rectY field + rectH field - textAreaBarLane + 1)
      check (rectX (sbTrack h) + rectW (sbTrack h) <= rectX field + rectW field - textAreaBarLane + 1)
      forM_ [(v, v2Y, V2 0 30), (h, v2X, V2 30 0)] $ \(bar, along, V2 dx dy) -> do
        let V2 cx cy = spanCenter (sbThumb bar)
        checkEq UiCursorGrab =<< cursorAt (At (V2 cx cy))
        checkEq UiCursorGrabbing =<< (press (At (V2 cx cy)) >> pointerCursor)
        before <- along <$> offsets
        above before . along =<< (moveTo (At (V2 (cx + dx) (cy + dy))) >> offsets)
        void release
    pressTab 1
    checkEq (V2 0 0) =<< wheelBy (V2 (-10) (-10))
    V2 x0 y0 <- wheelBy (V2 2 3)
    check (x0 > 0 && y0 > 0)
    checkEq (V2 0 0) =<< wheelBy (V2 (-10) (-10))
    V2 x1 y1 <- wheelBy (V2 0 1)
    check (x1 == 0 && y1 > 0)
    checkEq (V2 0 y1) =<< (frame >> offsets)
    above y1 . v2Y =<< wheelBy (V2 0 3)
    st <- textAreaState ctx (respId resp)
    checkEq (txt, Cursor 0 0) (toText (tasBuffer st), getCursor (tasBuffer st))
    checkEq (Cursor 0 0) (tasSelectionAnchor st)
    checkEq (V2 0 0, Cursor 0 1) =<< (typeText "!" >> (,) <$> offsets <*> caret)
    V2 x2 y2 <- wheelBy (V2 1 0)
    check (x2 > 0 && y2 == 0)
    V2 x3 _ <- wheelBy (V2 3 0)
    checkGt x3 x2
    let (ix, iy) = widgetContentInset fm
    Cursor _ col <- click (At (V2 (rectX field + ix + 30) (rectY field + iy + 5))) >> caret
    check (abs (fromIntegral col - (x3 + 30) / fmAdvance fm '0') <= 1)
    checkEq 0 . v2X =<< wheelBy (V2 (-10) 0)
  short <- newPixelContext
  runSim short failed (withInput 320 220) (column (labeledArea "Short" "Line 1\nLine 2")) $ do
    (resp, _) <- result
    checkEq 0 =<< (wheel (On resp) (V2 0 1) >> liftIO (getScrollOffset short (respId resp)))

-- | A text area in its own font size scrolls as far as that font's content
-- needs, short of each bar's lane.
runTextAreaZoomScrollTest :: Context -> IORef Int -> IO ()
runTextAreaZoomScrollTest ctx failed =
  runSim ctx failed (withInput 320 220) (column (withValue fortyLines (textAreaWith' (fontSize 32)))) $ do
    (resp, _) <- result
    liftIO (textAreaHitForWidget ctx (respId resp)) >>= (`checkJust` \hit -> do
      fm <- liftIO (resolveTextAreaFont ctx (tahNodeIdx hit))
      let field = tahFieldRect hit
          contentH = fromIntegral (max 1 (length (toLines (fromText fortyLines)))) * tahLineH hit
          contentW = maximum (0 : [lineWidth fm l | l <- T.lines fortyLines])
          (ix, iy) = widgetContentInset fm
          innerW = rectW field - 2 * ix
          innerH = rectH field - 2 * iy
          hasH = contentW > (if contentH > innerH then max 0 (innerW - textAreaBarLane) else innerW)
          availH = if hasH then max 0 (innerH - textAreaBarLane) else innerH
      off <- wheel (Within field) (V2 0 100) >> liftIO (getScrollOffset ctx (respId resp))
      check (abs (off - max 0 (contentH - availH)) < 0.5))

-- | An editor mounted under a fresh key scrolls on wheel hover without
-- focus, and the scroll damages it so it repaints.
runTextAreaRemountScrollTest :: Context -> IORef Int -> IO ()
runTextAreaRemountScrollTest ctx failed = do
  keyRef <- newIORef (1 :: Int)
  let ui = column (liftIO (readIORef keyRef) >>= \k -> withKey k (withValue fortyLines (textAreaWith' grow)))
  runSim ctx failed (withInput 320 220) ui $ do
    liftIO (writeIORef keyRef 2)
    (resp, _) <- settle
    -- Settle hover animations in 1 s frames, or a hot-widget change on the
    -- wheel frame would damage the area and mask a missing scroll repaint.
    modifyInput (\i -> i {inputDeltaTime = 1})
    _ <- moveTo (On resp) >> replicateM_ 3 frame
    check . damageIsEmpty =<< liftIO (takeDamage ctx)
    _ <- frameWith (\i -> i {inputScroll = V2 0 1})
    above 0 =<< liftIO (getScrollOffset ctx (respId resp))
    check . not . damageIsEmpty =<< liftIO (takeDamage ctx)

-- | The edit menu edits a text area with no key or text on the frame. Select
-- All keeps the whole document selected once the button comes up (the
-- gesture moves no caret and starts no drag), and Cut pulses 'respChanged'
-- once, on its release frame. A command that only selects pulses nothing.
runTextAreaMenuTest :: Context -> IORef Int -> IO ()
runTextAreaMenuTest ctx failed = do
  let original = T.intercalate "\n" (["line " <> T.pack (show i) | i <- [0 .. 199 :: Int]] ++ [lastLine])
      lastLine = "the last line"
  ref <- newVar original
  q <- newIORef []
  runSim ctx failed (withInput 400 300) (column (withCommands q (held ref (textAreaWith' grow)))) $ do
    (resp, _) <- result
    let Rect x y w h = respRect resp
        mid = At (V2 (x + w / 3) (y + h / 3))
        pick entry = rightClick mid >> settle >> click entry
        changedValue (r, v) = (respChanged r, v)
    _ <- click mid
    liftIO (writeIORef q [SelectAll])
    checkEq (False, original) . changedValue =<< (frame >> frame)
    st <- pick "Select All" >> textAreaState ctx (respId resp)
    checkEq (Cursor 0 0, Cursor 200 (T.length lastLine)) (tasSelectionAnchor st, getCursor (tasBuffer st))
    checkEq (True, "") . changedValue =<< pick "Cut"
    checkEq (False, "") . changedValue =<< frame

-- | A text area's bar spans the text's own range, not its insets: scrolled
-- to the end, the thumb meets the end of its track, with the other bar shown
-- or not.
runTextAreaBarRangeTest :: Context -> IORef Int -> IO ()
runTextAreaBarRangeTest ctx failed =
  forM_ [0, 1000] $ \contentW -> do
    let fm = ctxFontMetrics ctx
        field = Rect 0 0 200 100
        bars = textAreaBars fm field contentW (10 * textAreaLineHeight fm)
        V2 _ rangeY = tabRange bars
    assertJust failed (fst (textAreaBarLayouts field bars 0 rangeY)) $ \layout -> do
      let Rect _ ty _ th = sbTrack layout
          Rect _ my _ mh = sbThumb layout
      assertEq failed rangeY (sbMaxOff layout)
      assert failed (abs (ty + th - (my + mh)) < 0.01)

-- | A text area keeps its caret in view: typing past its right edge scrolls
-- across, and Home back; the caret moved to the last row sits above the
-- horizontal bar's lane; and keys follow the rows as painted on the device
-- pixel grid, 12.8 px apart for a 13 px font at 125%, not 13.
runCaretInViewTest :: Context -> IORef Int -> IO ()
runCaretInViewTest base failed = do
  let area ctx ref widget script =
        runSim ctx failed (withInput 320 220) (column (fst <$> held ref widget)) $ do
          wid <- respId <$> result
          pressTab 1
          script (liftIO (caretInView ctx wid)) (liftIO (getScrollOffset2D ctx wid))
  follow <- newPixelContext
  typed <- newVar ""
  area follow typed (textAreaWith' (fixedW 200)) $ \inView offsets -> do
    check . and =<< replicateM 60 (typeText "x" >> inView)
    checkEq 60 . T.length =<< liftIO (readVarIO typed)
    above 0 . v2X =<< offsets
    checkEq 0 . v2X =<< (pressKey KeyHome >> offsets)
  wide <- newPixelContext
  wideRef <- newVar (T.replicate 30 "wide " <> "\n" <> fortyLines)
  area wide wideRef textArea' $ \inView _ -> command 'a' >> frame >> (check =<< inView)
  let fm = (monospaceMetrics 13) {fmSnapScale = 1.25}
      measure _ _ _ _ t = pure (13 * fromIntegral (T.length t), 13)
      snapped = withFontResolver base (\_ _ _ _ -> pure (fm, False)) measure
  blank <- newVar (T.replicate 59 "\n")
  area snapped blank (textAreaWith' (fontSize 13)) $ \inView _ ->
    check . and =<< mapM (\k -> pressKey k >> inView) (replicate 59 KeyDown ++ replicate 20 KeyUp)

-- | Whether text area @wid@'s caret, at its stored scroll, lies inside the
-- clip its rows are painted in: its content box, short of the lane of each
-- scrollbar that shows.
caretInView :: Context -> WidgetId -> IO Bool
caretInView ctx wid = do
  st <- (`loadTextAreaState` intKey wid) <$> getStore ctx
  V2 sx sy <- getScrollOffset2D ctx wid
  textAreaHitForWidget ctx wid >>= \case
    Nothing -> pure False
    Just (TextAreaHit idx field lineH) -> do
      fm <- resolveTextAreaFont ctx idx
      (contentW, contentH) <- textAreaContentMetrics ctx idx
      let
        bars = textAreaBars fm field contentW contentH
        (ix, iy) = widgetContentInset fm
        lane shown = if shown then textAreaBarLane else 0
        viewW = rectW field - 2 * ix - lane (tabVertical bars)
        viewH = rectH field - 2 * iy - lane (tabHorizontal bars)
        Cursor caretRow caretCol = getCursor (tasBuffer st)
        left = lineWidth fm (T.take caretCol (lineAt caretRow (tasBuffer st))) - sx
        top = fromIntegral caretRow * lineH - sy
      pure (left >= -0.5 && left + 1 <= viewW + 0.5 && top >= -0.5 && top + lineH <= viewH + 0.5)

-- | A text area sets its rows in the face its layout names, not only the
-- size, and in its font colour, which the caret takes too. A change of face
-- measures the lines again.
runTextAreaFontTest :: Context -> IORef Int -> IO ()
runTextAreaFontTest base failed = do
  let
    ctx = withMonoFontMetrics base (monospaceMetrics 20)
    red = colorRGBA 255 0 0 255
    inp0 = withInput 320 220
    ui f = column (fst <$> withValue "code" (textAreaWith' f))
    widest resp =
      textAreaHitForWidget ctx (respId resp) >>= traverse (fmap fst . textAreaContentMetrics ctx . tahNodeIdx)
  resp <- warmup2 ctx inp0 (ui (fontMono . fontColor red))
  (_, draw, _) <- runFrame ctx (tabInp inp0) (ui (fontMono . fontColor red))
  assertJustM failed (textAreaHitForWidget ctx (respId resp)) $ \hit -> do
    assertEq failed 20 (tahLineH hit)
    assertEq failed 20 . fmLineHeight =<< resolveTextAreaFont ctx (tahNodeIdx hit)
  assert failed . elem red . fillsIn (respRect resp) =<< drawQuads draw
  assertEq failed (Just 80) =<< widest resp
  assertEq failed (Just 48) =<< widest =<< warmup2 ctx inp0 (ui id)
