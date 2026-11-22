module Main (main) where

import Control.Monad (forM_)
import Data.Text qualified as T
import NanoUI.Internal.Frame.TextArea (textWordBounds)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), emptyInput, inputKeysFromList, primaryModifiers)
import NanoUI.Internal.Input (onMac)
import NanoUI.Widgets.TextArea as TA
import NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextDocument qualified as TD
import NanoUI.Internal.Store (ptrEq)
import NanoUI.Widgets.TextEditor as TE
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, property, conjoin, counterexample, elements, frequency, listOf, oneof, vectorOf, (.&&.), (===))

main :: IO ()
main = hspec (spec >> editModelSpec)

noMods, ctrlMods, altMods :: Modifiers
noMods = Modifiers False False False False
ctrlMods = Modifiers False True False False
altMods = Modifiers False False True False

-- | One frame's typed text and keys, with modifiers held.
frameInput :: Modifiers -> T.Text -> [Key] -> Input
frameInput mods chars keys =
  emptyInput {inputChars = chars, inputKeys = inputKeysFromList keys, inputModifiers = mods}

-- | Run one frame of input on a text area.
typeArea :: Modifiers -> T.Text -> [Key] -> TA.TextAreaState -> TA.TextAreaState
typeArea mods chars keys s =
  foldl' (flip TA.runTextAreaCommand) s (TE.inputTextCommands TE.multiLineMode (frameInput mods chars keys))

-- | A text area's text and caret.
areaText :: TA.TextAreaState -> (T.Text, TB.Cursor)
areaText s = (TB.toText (TA.tasBuffer s), TB.getCursor (TA.tasBuffer s))

-- | Run commands on a buffer.
edit :: [TE.TextCommand] -> TB.TextBuffer -> TB.TextBuffer
edit cmds buf = TE.editorBuffer (foldl' (flip (TE.runCommand TE.multiLineMode)) (TE.editorFromBuffer buf) cmds)

spec :: Spec
spec = do
  describe "NanoUI.Widgets.TextBuffer" $ do
    it "looks up rows safely after cursor movement and decodes tabs" $ do
      let
        b = TB.withCursor (TB.Cursor 1 1) (TB.fromText "α\tβ\n猫\n")
      map (`TB.lineAt` b) [-1, 0, 1, 2, 3, maxBound]
        `shouldBe` ["", "α\tβ", "猫", "", "", ""]

    it "splits and joins lines, reading Windows line ends and pasting classic Mac ones" $ do
      TB.toText (TB.fromText "a\n") `shouldBe` "a\n"
      TB.toLines (TB.fromText "a\n") `shouldBe` ["a", ""]
      mapM_
        (\t -> TB.joinLines (TB.splitLines t) `shouldBe` t)
        ["", "\n", "\n\n", "a", "a\n", "\nλ", "α\n\n猫🙂\n", "one\ntwo\nthree"]
      TB.toLines (TB.fromText "one\r\ntwo\r\n") `shouldBe` ["one", "two", ""]
      TB.toText (TB.fromText "one\r\ntwo") `shouldBe` "one\ntwo"
      TB.insertableText "a\rb\r\nc" `shouldBe` "a\nb\nc"

    it "leaves the expected text and caret after each edit" $ do
      let
        uni = TB.fromText "αβ\n猫犬\nend"
        backwards = TE.Select (TB.Cursor 1 1) (TB.Cursor 0 1)
        atEnd = TB.moveToEOL . TB.fromText
      TB.selectedText (TB.Cursor 1 1) (TB.Cursor 0 1) uni `shouldBe` "β\n猫"
      forM_
        [ ("replace a backwards selection", [backwards, TE.InsertText "🙂\nλ"], uni, "α🙂\nλ犬\nend", Just (TB.Cursor 1 1))
        , ("delete a backwards selection", [backwards, TE.Delete TE.CharLeft], uni, "α犬\nend", Just (TB.Cursor 0 1))
        , -- text-zipper would drop the tab.
          ("insert a tab", [TE.InsertText "\t"], TB.empty, "\t", Just (TB.Cursor 0 1))
        , ( "filter control characters"
          , [TE.InsertText "α\t\n猫\x01"]
          , TB.withCursor (TB.Cursor 0 1) (TB.fromText "ab")
          , "aα\t\n猫b"
          , Just (TB.Cursor 1 1)
          )
        , ("delete a word left with its trailing space", [TE.Delete TE.WordLeft], atEnd "foo ", "", Nothing)
        , ("delete the previous word", [TE.Delete TE.WordLeft], atEnd "foo bar", "foo ", Nothing)
        , ("join lines at a line start", [TE.Delete TE.WordLeft], TB.moveDown (TB.fromText "foo\nbar"), "bar", Nothing)
        , ("delete a word right", [TE.Delete TE.WordRight], TB.fromText "foo bar", " bar", Nothing)
        , ("delete to the line end", [TE.Delete TE.LineEnd], TB.moveRight (TB.fromText "hello"), "h", Nothing)
        , ("delete to the line start", [TE.Delete TE.LineStart], atEnd "hello", "", Nothing)
        , -- Whole flags, emoji sequences and combining marks; Backspace takes a
          -- combining mark off its letter, and a variation selector or keycap
          -- with its character.
          ("delete a skin-toned emoji", [TE.Delete TE.CharLeft], atEnd "a👍🏽", "a", Nothing)
        , ("delete a flag", [TE.Delete TE.CharRight], TB.fromText "🇺🇸🇫🇷", "🇫🇷", Nothing)
        , ("delete a vowel sign", [TE.Delete TE.CharLeft], atEnd "a\x0915\x093F", "a\x0915", Nothing)
        , ("delete a combining accent", [TE.Delete TE.CharLeft], atEnd "e\x0301", "e", Nothing)
        , ("delete a variation sequence", [TE.Delete TE.CharLeft], atEnd "a\x2764\xFE0F", "a", Nothing)
        , ("delete a keycap", [TE.Delete TE.CharLeft], atEnd "a#\xFE0F\x20E3", "a", Nothing)
        , ("delete an accented letter", [TE.Delete TE.CharRight], TB.fromText "e\x0301x", "x", Nothing)
        ]
        $ \(name, cmds, b, txt, cursor) -> do
          let b' = edit cmds b
          (name :: String, TB.toText b') `shouldBe` (name, txt)
          forM_ cursor $ \c -> (name, TB.getCursor b') `shouldBe` (name, c)

    it "keeps joiners, bidi marks and private-use characters that are typed or pasted" $ do
      let kept = "👩\x200D💻 \x200C \x200F \xE0A0"
      TB.insertableText kept `shouldBe` kept
      TE.inputTextCommands TE.multiLineMode (frameInput noMods "\x200C\x7F" [])
        `shouldBe` [TE.InsertText "\x200C"]

    it "moves by grapheme and keeps the preferred column through short lines and document ends" $ do
      let
        atEnd = TB.moveToEOL . TB.fromText
        top = TB.withCursor (TB.Cursor 0 3) (TB.fromText "abcd\nx\n猫猫猫猫")
        bottom = TB.moveDown (TB.moveDown top)
        short = TB.withCursor (TB.Cursor 0 4) (TB.fromText "12345\n12\n12345")
        cursors = map TB.getCursor
      cursors [TB.moveLeft (atEnd "a🇺🇸"), TB.moveLeft (atEnd "a👩\x200D💻"), TB.moveRight (TB.fromText "e\x0301x")]
        `shouldBe` [TB.Cursor 0 1, TB.Cursor 0 1, TB.Cursor 0 2]
      forM_ ["a🇺🇸🇫🇷x", "US🇺🇸🇫🇷🇺", "a😀\x200D👩\x200D👧b", "\x200Dx", "e\x0301\n👍🏽y"] $ \sample -> do
        let buf = TB.fromText sample
            endCur = TB.documentEnd buf
            traceBounds targetCur step s0 =
              let cur = TB.getCursor s0
               in cur : if cur == targetCur then [] else traceBounds targetCur step (step s0)
            fwd = traceBounds endCur TB.moveRight buf
            bwd = reverse (traceBounds (TB.Cursor 0 0) TB.moveLeft (TB.withCursor endCur buf))
        (sample, bwd) `shouldBe` (sample, fwd)
      cursors [TB.moveUp top, bottom, TB.moveDown bottom, TB.moveUp (TB.moveUp bottom), TB.moveDown TB.empty]
        `shouldBe` [TB.Cursor 0 3, TB.Cursor 2 3, TB.Cursor 2 3, TB.Cursor 0 3, TB.Cursor 0 0]
      cursors [TB.moveDown short, TB.moveDown (TB.moveDown short), TB.moveUp (TB.moveDown (TB.moveDown short))]
        `shouldBe` [TB.Cursor 1 2, TB.Cursor 2 4, TB.Cursor 1 2]
      -- An empty insertion keeps the preferred column too.
      let
        b = TB.moveDown (TB.withCursor (TB.Cursor 0 4) (TB.fromText "12345\nx\n12345"))
      TB.getCursor (TB.moveDown (edit [TE.InsertText ""] b)) `shouldBe` TB.Cursor 2 4
      map TB.documentEnd [TB.fromText "α\n猫🙂", TB.fromText "α\n", TB.empty]
        `shouldBe` [TB.Cursor 1 2, TB.Cursor 1 0, TB.Cursor 0 0]

  describe "NanoUI.Widgets.TextDocument and edits" $ do
    it "replaces a range, reads one back, and shares untouched lines" $ do
      let
        doc0 = TD.textDocument "one\ntwo\nthree\nfour"
        doc1 = TD.replaceDocumentRange (TB.Cursor 2 5) (TB.Cursor 1 1) "W\nX" doc0
      TD.documentText doc1 `shouldBe` "one\ntW\nX\nfour"
      TD.documentRange (TB.Cursor 0 1) (TB.Cursor 1 2) doc1 `shouldBe` "ne\ntW"
      -- Forced first, so the comparison sees values rather than thunks.
      let shared row = let !a = TD.documentLine row doc0; !b = TD.documentLine row doc1 in ptrEq a b
      map shared [0, 3] `shouldBe` [True, True]

    it "applies and inverts a recorded edit, on documents and buffers" $ do
      let
        doc0 = TD.textDocument "alpha\nbeta"
        e = TB.replaceEdit "B" (TB.Cursor 1 0) (TB.Cursor 1 1) (TD.documentBuffer doc0)
        doc1 = TD.editDocument e doc0
      TD.documentText doc1 `shouldBe` "alpha\nBeta"
      TD.editDocument (TB.invertEdit e) doc1 `shouldBe` doc0
      TD.bufferDocument (TD.documentBuffer doc1) `shouldBe` doc1
      let
        b0 = TB.fromText "αβ\n猫犬\nend"
        across = TB.replaceEdit "🙂\nλ\nμ" (TB.Cursor 0 1) (TB.Cursor 2 1) b0
        b1 = TB.applyEdit across b0
      TB.editRemoved across `shouldBe` "β\n猫犬\ne"
      (TB.toText b1, TB.getCursor b1) `shouldBe` ("α🙂\nλ\nμnd", TB.Cursor 2 1)
      TB.toText (TB.applyEdit (TB.invertEdit across) b1) `shouldBe` "αβ\n猫犬\nend"

    it "edits the middle of a long document locally" $ do
      let
        doc = T.intercalate "\n" [T.pack (show i) | i <- [1 .. 20000 :: Int]]
        insertAt b i = let at = TB.Cursor (5000 + i) 0 in TB.applyEdit (TB.replaceEdit "x\n" at at b) b
        edited = foldl' insertAt (TB.fromText doc) [1 .. 500 :: Int]
      (TB.getLineCount edited, TB.lineAt 5002 edited) `shouldBe` (20500, "x")

  describe "NanoUI.Widgets.TextEditor" $ do
    let
      run mode = foldl' (flip (TE.runCommand mode))
      line = TE.runCommand TE.singleLineMode
      typeText mode t ed = run mode ed [TE.InsertText (T.singleton c) | c <- T.unpack t]
      single = TE.editorFromBuffer TB.empty
      text = TB.toText . TE.editorBuffer
      depth = TE.historyDepth . TE.editorHistory

    it "joins a run of deletes into one step and restores the selection it replaced" $ do
      let
        deleted = run TE.singleLineMode (typeText TE.singleLineMode "abcdef" single) (replicate 3 backspace)
        backspace = TE.Delete TE.CharLeft
        replaced = run TE.singleLineMode deleted [TE.Select (TB.Cursor 0 1) (TB.Cursor 0 3), TE.InsertText "Z"]
        undone = line TE.Undo replaced
      map text [deleted, line TE.Undo deleted, replaced, undone] `shouldBe` ["abc", "abcdef", "aZ", "abc"]
      TE.editorSelection undone `shouldBe` (TB.Cursor 0 1, TB.Cursor 0 3)

    it "drops redo after a new edit and ignores no-op commands" $ do
      let
        undone = line TE.Undo (typeText TE.singleLineMode "ab" single)
        retyped = line (TE.InsertText "c") undone
      map (TE.canRedo . TE.editorHistory) [undone, retyped] `shouldBe` [True, False]
      depth (line (TE.Delete TE.CharLeft) single) `shouldBe` 0
      text (line (TE.ReplaceAll "c") retyped) `shouldBe` "c"
      depth (line (TE.ReplaceAll "c") retyped) `shouldBe` depth retyped

    it "collapses the selection when typing over it with the same text" $ do
      let
        selected = line (TE.Select (TB.Cursor 0 0) (TB.Cursor 0 1)) (typeText TE.singleLineMode "abc" single)
        retyped = typeText TE.singleLineMode "ab" selected
      (text retyped, TE.hasSelection retyped) `shouldBe` ("abbc", False)

    it "Left and Right without Shift land on a selection's start and end" $ do
      let
        typed = typeText TE.singleLineMode "hello" single
        selected = line TE.SelectAll typed
        backwards = line (TE.Select (TB.Cursor 0 4) (TB.Cursor 0 1)) typed
        moved m extend = TE.editorSelection . line (TE.Move m extend)
      moved TE.CharLeft False selected `shouldBe` (TB.Cursor 0 0, TB.Cursor 0 0)
      moved TE.CharRight False backwards `shouldBe` (TB.Cursor 0 4, TB.Cursor 0 4)
      moved TE.CharLeft True selected `shouldBe` (TB.Cursor 0 0, TB.Cursor 0 4)

    it "keeps line breaks out of single-line fields and bounds history depth" $ do
      text (line (TE.InsertText "a\nb") single) `shouldBe` "ab"
      text (TE.runCommand TE.multiLineMode (TE.InsertText "a\r\nb") single) `shouldBe` "a\nb"
      let
        edits = run TE.multiLineMode single (replicate 1000 (TE.InsertText "\n"))
      depth edits `shouldSatisfy` (<= 550)
      text (run TE.multiLineMode edits (replicate 2000 TE.Undo)) `shouldSatisfy` (\t -> T.length t >= 450)

    it "undoes a large paste in one step without copying the document per keystroke" $ do
      let
        doc = T.intercalate "\n" (replicate 50000 "some line of text")
        pasted = TE.runCommand TE.multiLineMode (TE.InsertText doc) single
        back = run TE.multiLineMode (typeText TE.multiLineMode "tail" pasted) [TE.Undo, TE.Undo]
      text back `shouldBe` ""
      text (run TE.multiLineMode back [TE.Redo, TE.Redo]) `shouldBe` doc <> "tail"

  describe "NanoUI.Widgets.TextArea" $ do
    let
      start = TA.initTextAreaState
      atEnd = typeArea noMods "" [KeyEnd] . start
      foo = atEnd "foo bar"
      (wordMods, otherMods) = if onMac then (altMods, ctrlMods) else (ctrlMods, altMods)
      lineMods = if onMac then Modifiers False False False True else Modifiers True True False False
      at = TB.Cursor 0
      mac a b = if onMac then a else b

    it "maps keys and typed text to edits by platform" $
      forM_
        [ -- The word modifier edits and moves by word, the other of Ctrl and Alt by character.
          ("word delete", typeArea wordMods "" [KeyDelete] (start "foo bar"), (" bar", at 0))
        , ("word right", typeArea wordMods "" [KeyRight] (start "foo bar"), ("foo bar", at 3))
        , ("word left", typeArea wordMods "" [KeyLeft] (atEnd "foo"), ("foo", at 0))
        , ("character delete", typeArea otherMods "" [KeyDelete] (start "foo bar"), ("oo bar", at 0))
        , ("character right", typeArea otherMods "" [KeyRight] (start "foo bar"), ("foo bar", at 1))
        , ("character left", typeArea otherMods "" [KeyLeft] (atEnd "foo"), ("foo", at 2))
        , -- Command, or elsewhere Ctrl+Shift, edits and moves to the line's ends.
          ("line delete", typeArea lineMods "" [KeyBackspace] (atEnd "foo bar\nbaz"), ("\nbaz", at 0))
        , ("Ctrl+Left", typeArea (mac lineMods ctrlMods) "" [KeyLeft] foo, ("foo bar", at (mac 0 4)))
        , -- Ctrl alone moves and deletes as in Emacs on macOS, and is no command elsewhere.
          ("Ctrl+A", typeArea ctrlMods "" [KeyChar 'a'] foo, ("foo bar", at (mac 0 7)))
        , ("Ctrl+B", typeArea ctrlMods "" [KeyChar 'b'] foo, ("foo bar", at (mac 6 7)))
        , ("Ctrl+H", typeArea ctrlMods "" [KeyChar 'h'] foo, (mac "foo ba" "foo bar", at (mac 6 7)))
        , ("Ctrl+U", typeArea ctrlMods "" [KeyChar 'u'] foo, (mac "" "foo bar", at (mac 0 7)))
        , -- Backspace at the end of a line read with Windows line ends deletes its last character.
          ("CRLF backspace", typeArea noMods "" [KeyBackspace] (atEnd "one\r\ntwo"), ("on\ntwo", at 2))
        ]
        $ \(name, s, want) -> (name :: String, areaText s) `shouldBe` (name, want)

    it "selects all with Ctrl+A or Ctrl+a (Command on macOS), and reads chords from keys" $ do
      forM_ [KeyChar 'a', KeyChar 'A'] $ \k -> do
        let s = typeArea primaryModifiers "" [k] foo
        (TA.tasSelectionAnchor s, TB.getCursor (TA.tasBuffer s)) `shouldBe` (at 0, at 7)
      -- Chords come from key events; characters typed with Ctrl held are ignored.
      TE.inputTextCommands TE.singleLineMode (frameInput primaryModifiers "z\x1a" [KeyChar 'z', KeyChar 'y'])
        `shouldBe` [TE.Undo, TE.Redo]

    it "typing and Enter replace a backwards multiline selection and collapse its anchor" $ do
      let
        selected = TA.setTextAreaSelection (TB.Cursor 1 1) (TB.Cursor 0 1) (TA.initTextAreaState "abc\ndef")
        typed = typeArea noMods "λ" [] selected
        entered = typeArea noMods "" [KeyEnter] selected
      (areaText typed, TA.tasSelectionAnchor typed) `shouldBe` (("aλef", TB.Cursor 0 2), TB.Cursor 0 2)
      (areaText entered, TA.tasSelectionAnchor entered) `shouldBe` (("a\nef", TB.Cursor 1 0), TB.Cursor 1 0)

    it "clamps a selection past the document's end and scrolls the caret into view" $ do
      let s = TA.setTextAreaSelection (TB.Cursor 9 9) (TB.Cursor 9 9) (TA.initTextAreaState "ab")
      (TA.tasSelectionAnchor s, TB.getCursor (TA.tasBuffer s)) `shouldBe` (TB.Cursor 0 2, TB.Cursor 0 2)
      let s0 = TA.setTextAreaViewport (80, 16) 16 (TA.initTextAreaState "a\nb")
      TA.tasScrollOffset (typeArea noMods "" [KeyDown] s0) `shouldBe` (0, 16)

  describe "text word selection" $
    it "groups Unicode words, whitespace and punctuation, keeps marks in words, and clamps" $ do
      map (textWordBounds "αβ_猫  🙂!?") [0 .. 8]
        `shouldBe` replicate 4 (0, 4) ++ replicate 2 (4, 6) ++ replicate 3 (6, 9)
      forM_
        [ ("नमस्ते जी", 0, (0, 6))
        , ("cafe\x0301 au lait", 4, (0, 5))
        , ("می\x200Cخواهم", 0, (0, 8))
        , ("", 10, (0, 0))
        , ("one two", -10, (0, 3))
        , ("one two", 100, (4, 7))
        , (T.replicate 10000 "猫", 5000, (0, 10000))
        ]
        $ \(t, i :: Int, want) -> (T.take 20 t, i, textWordBounds t i) `shouldBe` (T.take 20 t, i, want)

  describe "cluster movement" $
    it "steps left symmetrically with right across combining marks, ZWJ chains, flags, and long lines" $
      forM_
        [ "abc"
        , "cafe\x0301x"
        , "a\x200Db\x200Dc"
        , "a\x200D\n"
        , "\x1F1FA\x1F1F8\x1F1EC\x1F1E7"
        , T.replicate 200 "a" <> "e\x0301\x0302"
        ]
        $ \t -> do
          let walkRight b =
                let b' = TB.moveRight b
                 in if TB.getCursor b' == TB.getCursor b then [TB.getCursor b] else TB.getCursor b : walkRight b'
              walkLeft b =
                let b' = TB.moveLeft b
                 in if TB.getCursor b' == TB.getCursor b then [TB.getCursor b] else TB.getCursor b : walkLeft b'
              start0 = TB.fromText t
              fwd = walkRight start0
              atEndBuf = TB.withCursor (last fwd) start0
              bwd = reverse (walkLeft atEndBuf)
          bwd `shouldBe` fwd

-- | Pieces of text the random edits insert: ASCII, spaces, combining marks,
-- ZWJ emoji, a skin-toned emoji, flags, CJK and line breaks.
pieces :: [T.Text]
pieces =
  ["a", "xyz", " ", "  ", "e\x0301", "\x0915\x093F", "\x1F469\x200D\x1F4BB", "\x1F44D\x1F3FD", "\x1F1FA\x1F1F8", "\x732B\x72AC", "\n", "a\nb", "foo bar", "\n\n"]

-- | A text built from 'pieces'.
genText :: Gen T.Text
genText = T.concat <$> listOf (elements pieces)

-- | A position that may lie past the document's end, which commands clamp.
genCursor :: Gen TB.Cursor
genCursor = TB.Cursor <$> choose (-1, 6) <*> choose (-1, 12)

-- | One edit a user or an app makes on a multiline field.
newtype Step = Step TE.TextCommand
  deriving (Show)

instance Arbitrary Step where
  arbitrary =
    Step
      <$> frequency
        [ (6, TE.InsertText <$> elements pieces)
        , (4, TE.Delete <$> elements [TE.CharLeft, TE.CharRight, TE.WordLeft, TE.WordRight, TE.LineStart, TE.LineEnd])
        , (3, TE.Move <$> elements [minBound .. maxBound] <*> arbitrary)
        , (2, TE.Select <$> genCursor <*> genCursor)
        , (1, pure TE.SelectAll)
        , (1, TE.Replace <$> genCursor <*> genCursor <*> elements pieces)
        , (2, pure TE.Undo)
        , (1, pure TE.Redo)
        ]

-- | A starting document and the edits made on it.
data Session = Session T.Text [Step]
  deriving (Show)

instance Arbitrary Session where
  arbitrary = Session <$> genText <*> (choose (0, 60) >>= (`vectorOf` arbitrary))
  shrink (Session t steps) = [Session t s | s <- shrinkList steps]
    where
      shrinkList xs = [take i xs <> drop (i + 1) xs | i <- [0 .. length xs - 1]]

-- | The character offset of a position in a text whose lines are @lns@.
offsetOf :: [T.Text] -> TB.Cursor -> Int
offsetOf lns (TB.Cursor r c) = sum [T.length l + 1 | l <- take r lns] + c

-- | Whether the editor's caret and anchor lie inside its document.
insideDocument :: TE.Editor -> Bool
insideDocument ed =
  let buf = TE.editorBuffer ed
      ok (TB.Cursor r c) = r >= 0 && r < TB.getLineCount buf && c >= 0 && c <= T.length (TB.lineAt r buf)
      (a, cur) = TE.editorSelection ed
   in ok a && ok cur

-- | The editor's text, and its selection as ordered character offsets.
flat :: TE.Editor -> (T.Text, Int, Int)
flat ed =
  let buf = TE.editorBuffer ed
      lns = TB.toLines buf
      (a, c) = TE.editorSelection ed
      (lo, hi) = TB.selectionRange a c
   in (TB.toText buf, offsetOf lns lo, offsetOf lns hi)

-- | The caret's character offset.
caretOffset :: TE.Editor -> Int
caretOffset ed = let buf = TE.editorBuffer ed in offsetOf (TB.toLines buf) (TB.getCursor buf)

runMulti :: TE.TextCommand -> TE.Editor -> TE.Editor
runMulti = TE.runCommand TE.multiLineMode

-- | What one step must do, checked against the editor before and after it:
-- an insert replaces the selection with the text and leaves the caret after
-- it; a delete removes the selection, or else a run of text that ends (to
-- the left) or starts (to the right) at the caret; an undo that a redo
-- follows, or the reverse, gives the text back.
stepModel :: TE.TextCommand -> TE.Editor -> TE.Editor -> Property
stepModel cmd prev next =
  let (old, lo, hi) = flat prev
      new = TB.toText (TE.editorBuffer next)
      caret = caretOffset next
      selected = lo /= hi
      splice a b ins = T.take a old <> ins <> T.drop b old
      leftOf = T.take lo old
   in counterexample (show (cmd, old, lo, hi, new, caret)) $ case cmd of
        TE.InsertText t -> new === splice lo hi t .&&. caret === lo + T.length t
        TE.Delete _
          | selected -> new === splice lo hi "" .&&. caret === lo
        TE.Delete m
          | m `elem` [TE.CharLeft, TE.WordLeft, TE.LineStart] ->
              counterexample "left delete" $
                caret <= lo .&&. new === T.take caret old <> T.drop lo old
          | otherwise ->
              counterexample "right delete" $
                caret === lo .&&. T.isPrefixOf leftOf new .&&. T.isSuffixOf (T.drop lo new) old
        TE.Undo
          | TE.canUndo (TE.editorHistory prev) ->
              TB.toText (TE.editorBuffer (runMulti TE.Redo next)) === old
        TE.Redo
          | TE.canRedo (TE.editorHistory prev) ->
              TB.toText (TE.editorBuffer (runMulti TE.Undo next)) === old
        _ | cmd `elem` [TE.Undo, TE.Redo] -> new === old
        TE.Move _ _ -> new === old
        TE.Select _ _ -> new === old
        TE.SelectAll -> new === old
        _ -> property True

-- | Undo until there is nothing left to undo, and the texts on the way.
undoAll :: TE.Editor -> [T.Text]
undoAll ed
  | TE.canUndo (TE.editorHistory ed) = let ed' = runMulti TE.Undo ed in TB.toText (TE.editorBuffer ed') : undoAll ed'
  | otherwise = []

-- | Random edit sessions against a plain 'T.Text' model, the history, and
-- recorded edits.
editModelSpec :: Spec
editModelSpec = describe "random edits" $ modifyMaxSuccess (max 300) $ do
  prop "match a text model, keep the caret in the document, and undo to the start" $ \(Session t0 steps) ->
    let ed0 = TE.editorFromBuffer (TB.fromText t0)
        trace = scanl (\ed (Step cmd) -> runMulti cmd ed) ed0 steps
        final = last trace
        stepChecks =
          [ counterexample "caret outside the document" (insideDocument ed') .&&. stepModel cmd ed ed'
          | (Step cmd, ed, ed') <- zip3 steps trace (drop 1 trace)
          ]
        seen = map (TB.toText . TE.editorBuffer) trace
        undone = undoAll final
        n = length undone
        start = iterate (runMulti TE.Undo) final !! n
        -- As many redos as undos: steps undone before the session ended
        -- stay on the redo stack past the final text.
        redone = iterate (runMulti TE.Redo) start !! n
     in conjoin stepChecks
          .&&. counterexample "undo reached a text the session never had" (all (`elem` seen) undone)
          .&&. counterexample "undo all" (TB.toText (TE.editorBuffer start) === t0)
          .&&. counterexample "redo what was undone" (TB.toText (TE.editorBuffer redone) === TB.toText (TE.editorBuffer final))
          .&&. counterexample "caret after undo and redo" (insideDocument start .&&. insideDocument redone)

  prop "apply and invert a replace edit between any two positions" $
    \(Session t0 _) -> do
      a <- genCursor
      b <- genCursor
      ins <- oneof [elements pieces, genText]
      let buf = TB.fromText t0
          e = TB.replaceEdit ins a b buf
          applied = TB.applyEdit e buf
          lns = TB.toLines buf
          (lo, hi) = TB.selectionRange (TB.clampCursor buf a) (TB.clampCursor buf b)
          (olo, ohi) = (offsetOf lns lo, offsetOf lns hi)
      pure $
        TB.editRemoved e === T.take (ohi - olo) (T.drop olo t0)
          .&&. TB.toText applied === T.take olo t0 <> ins <> T.drop ohi t0
          .&&. TB.toText (TB.applyEdit (TB.invertEdit e) applied) === t0
