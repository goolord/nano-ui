module Main (main) where

import Data.Text qualified as T
import NanoUI.Frame.TextEdit (textWordBounds)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), emptyInput, inputKeysFromList)
import NanoUI.Widgets.TextArea as TA
import NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextEditor as TE
import Test.Hspec

main :: IO ()
main = hspec spec

noMods :: Modifiers
noMods = Modifiers False False False

ctrlMods :: Modifiers
ctrlMods = Modifiers False True False

-- | One frame's typed text and keys, with modifiers held.
frameInput :: Modifiers -> T.Text -> [Key] -> Input
frameInput mods chars keys =
  emptyInput {inputChars = chars, inputKeys = inputKeysFromList keys, inputModifiers = mods}

-- | Run one frame of input on a text area.
typeArea :: Modifiers -> T.Text -> [Key] -> TA.TextAreaState -> TA.TextAreaState
typeArea mods chars keys s =
  foldl' (flip TA.runTextAreaCommand) s (TE.inputTextCommands TE.multiLineMode (frameInput mods chars keys))

-- | Run commands on a buffer.
edit :: TE.EditorMode -> [TE.TextCommand] -> TB.TextBuffer -> TB.TextBuffer
edit mode cmds buf = TE.editorBuffer (foldl' (flip (TE.runCommand mode)) (TE.editorFromBuffer buf) cmds)

spec :: Spec
spec = do
  describe "NanoUI.Widgets.TextBuffer" $ do
    it "looks up rows safely after cursor movement and decodes tabs" $ do
      let
        b = TB.withCursor (TB.Cursor 1 1) (TB.fromText "α\tβ\n猫\n")
      map (`TB.lineAt` b) [-1, 0, 1, 2, 3, maxBound]
        `shouldBe` ["", "α\tβ", "猫", "", "", ""]

    it
      "replaces a backwards selection across Unicode lines and places the caret after the insertion" $ do
      let
        b = TB.fromText "αβ\n猫犬\nend"
        start = TB.Cursor 0 1
        end = TB.Cursor 1 1
        replaced = edit TE.multiLineMode [TE.Select end start, TE.InsertText "🙂\nλ"] b
        deleted = edit TE.multiLineMode [TE.Select end start, TE.Delete TE.CharLeft] b
      TB.selectedText end start b `shouldBe` "β\n猫"
      TB.toText replaced `shouldBe` "α🙂\nλ犬\nend"
      TB.getCursor replaced `shouldBe` TB.Cursor 1 1
      TB.toText deleted `shouldBe` "α犬\nend"
      TB.getCursor deleted `shouldBe` start

    it "roundtrips a trailing newline through fromText/toText" $ do
      TB.toText (TB.fromText "a\n") `shouldBe` "a\n"
      TB.toLines (TB.fromText "a\n") `shouldBe` ["a", ""]

    it "inserts a tab that text-zipper would otherwise drop" $ do
      let
        b = edit TE.multiLineMode [TE.InsertText "\t"] TB.empty
      TB.toText b `shouldBe` "\t"
      TB.getCursor b `shouldBe` TB.Cursor 0 1

    it "inserts Unicode, tabs, and newlines while filtering control characters" $ do
      let
        b = edit TE.multiLineMode [TE.InsertText "α\t\n猫\x01"] (TB.withCursor (TB.Cursor 0 1) (TB.fromText "ab"))
      TB.toLines b `shouldBe` ["aα\t", "猫b"]
      TB.getCursor b `shouldBe` TB.Cursor 1 1

    it "empty insertion preserves the preferred column on a short line" $ do
      let
        b = TB.moveDown (TB.withCursor (TB.Cursor 0 4) (TB.fromText "12345\nx\n12345"))
      TB.getCursor (TB.moveDown (edit TE.multiLineMode [TE.InsertText ""] b)) `shouldBe` TB.Cursor 2 4

    it
      "clamps vertical motion at document boundaries without losing the preferred column" $ do
      let
        top = TB.withCursor (TB.Cursor 0 3) (TB.fromText "abcd\nx\n猫猫猫猫")
        bottom = TB.moveDown (TB.moveDown top)
      TB.getCursor (TB.moveUp top) `shouldBe` TB.Cursor 0 3
      TB.getCursor bottom `shouldBe` TB.Cursor 2 3
      TB.getCursor (TB.moveDown bottom) `shouldBe` TB.Cursor 2 3
      TB.getCursor (TB.moveUp (TB.moveUp bottom)) `shouldBe` TB.Cursor 0 3
      TB.getCursor (TB.moveDown TB.empty) `shouldBe` TB.Cursor 0 0
      -- Moving through a shorter line snaps the column and restores it after.
      let
        short = TB.withCursor (TB.Cursor 0 4) (TB.fromText "12345\n12\n12345")
      TB.getCursor (TB.moveDown short) `shouldBe` TB.Cursor 1 2
      TB.getCursor (TB.moveDown (TB.moveDown short)) `shouldBe` TB.Cursor 2 4
      TB.getCursor (TB.moveUp (TB.moveDown (TB.moveDown short))) `shouldBe` TB.Cursor 1 2

    it "finds the document end independently of the current cursor" $ do
      TB.documentEnd (TB.fromText "α\n猫🙂") `shouldBe` TB.Cursor 1 2
      TB.documentEnd (TB.fromText "α\n") `shouldBe` TB.Cursor 1 0
      TB.documentEnd TB.empty `shouldBe` TB.Cursor 0 0

    it "deleting a word left eats trailing whitespace then the previous word" $ do
      let
        deleteWordLeft = TB.toText . edit TE.multiLineMode [TE.Delete TE.WordLeft] . TB.moveToEOL . TB.fromText
      deleteWordLeft "foo " `shouldBe` ""
      deleteWordLeft "foo bar" `shouldBe` "foo "

    it "deleting a word left joins lines at beginning of line" $ do
      let
        b = edit TE.multiLineMode [TE.Delete TE.WordLeft] (TB.moveToBOL (TB.moveDown (TB.fromText "foo\nbar")))
      TB.toText b `shouldBe` "bar"

    it "deleting a word right deletes the word after the cursor" $ do
      TB.toText (edit TE.multiLineMode [TE.Delete TE.WordRight] (TB.fromText "foo bar")) `shouldBe` " bar"

    it "deleting to the line end or start removes the rest of the line on either side" $ do
      TB.toText (edit TE.multiLineMode [TE.Delete TE.LineEnd] (TB.moveRight (TB.fromText "hello"))) `shouldBe` "h"
      TB.toText (edit TE.multiLineMode [TE.Delete TE.LineStart] (TB.moveToEOL (TB.fromText "hello"))) `shouldBe` ""

  describe "NanoUI.Widgets.TextBuffer edits" $ do
    it "applies an edit across lines and inverts it back" $ do
      let
        b0 = TB.fromText "αβ\n猫犬\nend"
        e = TB.replaceEdit "🙂\nλ\nμ" (TB.Cursor 0 1) (TB.Cursor 2 1) b0
        b1 = TB.applyEdit e b0
      TB.editRemoved e `shouldBe` "β\n猫犬\ne"
      TB.toText b1 `shouldBe` "α🙂\nλ\nμnd"
      TB.getCursor b1 `shouldBe` TB.Cursor 2 1
      TB.toText (TB.applyEdit (TB.invertEdit e) b1) `shouldBe` "αβ\n猫犬\nend"

    it "edits the middle of a long document locally" $ do
      let
        doc = T.intercalate "\n" [T.pack (show i) | i <- [1 .. 20000 :: Int]]
        b0 = TB.fromText doc
        insertAt b i = let at = TB.Cursor (5000 + i) 0 in TB.applyEdit (TB.replaceEdit "x\n" at at b) b
        edited = foldl' insertAt b0 [1 .. 500 :: Int]
      TB.getLineCount edited `shouldBe` 20500
      TB.lineAt 5002 edited `shouldBe` "x"

  describe "NanoUI.Widgets.TextEditor" $ do
    let
      run mode = foldl' (flip (TE.runCommand mode))
      typeText mode t ed = run mode ed [TE.InsertText (T.singleton c) | c <- T.unpack t]
      single = TE.editorFromBuffer TB.empty
      text = TB.toText . TE.editorBuffer

    it "undoes typing a word at a time and redoes it" $ do
      let
        typed = typeText TE.singleLineMode "hello world" single
        once = TE.runCommand TE.singleLineMode TE.Undo typed
        twice = TE.runCommand TE.singleLineMode TE.Undo once
      text typed `shouldBe` "hello world"
      text once `shouldBe` "hello "
      text twice `shouldBe` ""
      text (run TE.singleLineMode twice [TE.Redo, TE.Redo]) `shouldBe` "hello world"
      TB.getCursor (TE.editorBuffer once) `shouldBe` TB.Cursor 0 6

    it "joins a run of deletes into one step and restores the selection it replaced" $ do
      let
        typed = typeText TE.singleLineMode "abcdef" single
        deleted = run TE.singleLineMode typed (replicate 3 (TE.Delete TE.CharLeft))
        selected = TE.runCommand TE.singleLineMode (TE.Select (TB.Cursor 0 1) (TB.Cursor 0 3)) deleted
        replaced = TE.runCommand TE.singleLineMode (TE.InsertText "Z") selected
        undone = TE.runCommand TE.singleLineMode TE.Undo replaced
      text deleted `shouldBe` "abc"
      text (TE.runCommand TE.singleLineMode TE.Undo deleted) `shouldBe` "abcdef"
      text replaced `shouldBe` "aZ"
      text undone `shouldBe` "abc"
      TE.editorSelection undone `shouldBe` (TB.Cursor 0 1, TB.Cursor 0 3)

    it "drops redo after a new edit and ignores no-op commands" $ do
      let
        typed = typeText TE.singleLineMode "ab" single
        undone = TE.runCommand TE.singleLineMode TE.Undo typed
        retyped = TE.runCommand TE.singleLineMode (TE.InsertText "c") undone
      TE.canRedo (TE.editorHistory undone) `shouldBe` True
      TE.canRedo (TE.editorHistory retyped) `shouldBe` False
      TE.historyDepth (TE.editorHistory (TE.runCommand TE.singleLineMode (TE.Delete TE.CharLeft) single)) `shouldBe` 0
      text (TE.runCommand TE.singleLineMode (TE.ReplaceAll "c") retyped) `shouldBe` "c"
      TE.historyDepth (TE.editorHistory (TE.runCommand TE.singleLineMode (TE.ReplaceAll "c") retyped))
        `shouldBe` TE.historyDepth (TE.editorHistory retyped)

    it "keeps line breaks out of single-line fields and bounds history depth" $ do
      text (TE.runCommand TE.singleLineMode (TE.InsertText "a\nb") single) `shouldBe` "ab"
      text (TE.runCommand TE.multiLineMode (TE.InsertText "a\r\nb") single) `shouldBe` "a\nb"
      let
        edits = run TE.multiLineMode single (concat (replicate 1000 [TE.InsertText "\n"]))
      TE.historyDepth (TE.editorHistory edits) `shouldSatisfy` (<= 550)
      text (run TE.multiLineMode edits (replicate 2000 TE.Undo)) `shouldSatisfy` (\t -> T.length t >= 450)

    it "undoes a large paste in one step without copying the document per keystroke" $ do
      let
        doc = T.intercalate "\n" (replicate 50000 "some line of text")
        pasted = TE.runCommand TE.multiLineMode (TE.InsertText doc) single
        typed = typeText TE.multiLineMode "tail" pasted
        back = run TE.multiLineMode typed [TE.Undo, TE.Undo]
      text back `shouldBe` ""
      text (run TE.multiLineMode back [TE.Redo, TE.Redo]) `shouldBe` doc <> "tail"

  describe "NanoUI.Widgets.TextArea" $ do
    it "Ctrl+Z undoes and Ctrl+Shift+Z redoes" $ do
      let
        s0 = TA.initTextAreaState ""
        typed = foldl' (\s c -> typeArea noMods (T.singleton c) [] s) s0 ("one two" :: String)
        undone = typeArea ctrlMods "z" [] typed
        redone = typeArea (Modifiers True True False) "z" [] undone
      TB.toText (TA.buffer undone) `shouldBe` "one "
      TB.toText (TA.buffer redone) `shouldBe` "one two"

    it "Ctrl+Alt types characters (AltGr) while Ctrl alone runs shortcuts" $ do
      let
        s0 = typeArea noMods "" [KeyEnd] (TA.initTextAreaState "ab")
        altGr = Modifiers False True True
      TB.toText (TA.buffer (typeArea altGr "@€" [] s0)) `shouldBe` "ab@€"
      TE.inputTextCommands TE.singleLineMode (frameInput altGr "@" [])
        `shouldBe` [TE.InsertText "@"]
      TE.inputTextCommands TE.singleLineMode (frameInput ctrlMods "a" [KeyLeft])
        `shouldBe` [TE.SelectAll, TE.Move TE.WordLeft False]

    it
      "typing and Enter replace a backwards multiline selection and collapse its anchor" $ do
      let
        selected =
          TA.setTextAreaSelection (TB.Cursor 1 1) (TB.Cursor 0 1) $
            TA.initTextAreaState "abc\ndef"
        typed = typeArea noMods "λ" [] selected
        entered = typeArea noMods "" [KeyEnter] selected
      TB.toText (TA.buffer typed) `shouldBe` "aλef"
      TB.getCursor (TA.buffer typed) `shouldBe` TB.Cursor 0 2
      TA.selectionAnchor typed `shouldBe` TB.Cursor 0 2
      TB.toText (TA.buffer entered) `shouldBe` "a\nef"
      TB.getCursor (TA.buffer entered) `shouldBe` TB.Cursor 1 0
      TA.selectionAnchor entered `shouldBe` TB.Cursor 1 0

    it "Ctrl and Alt edit and move by word" $ do
      let
        s0 = TA.initTextAreaState "foo bar"
      mapM_
        ( \mods -> do
            let
              deleted = typeArea mods "" [KeyDelete] s0
              right = typeArea mods "" [KeyRight] s0
              left = typeArea mods "" [KeyLeft] right
            TB.toText (TA.buffer deleted) `shouldBe` " bar"
            TB.getCursor (TA.buffer right) `shouldBe` TB.Cursor 0 3
            TB.getCursor (TA.buffer left) `shouldBe` TB.Cursor 0 0
        )
        [ctrlMods, Modifiers False False True]

    it "scrolls the caret into a one-line viewport" $ do
      let
        s0 =
          TA.setTextAreaViewport (80, 16) 16 $
            TA.initTextAreaState "a\nb"
        s1 = typeArea noMods "" [KeyDown] s0
      TA.scrollOffset s1 `shouldBe` (0, 16)

    it "Ctrl+A and Ctrl+a both select all" $ do
      let
        s0 = TA.initTextAreaState "hello"
        atEnd = typeArea noMods "" [KeyEnd] s0
        fromLower = typeArea ctrlMods "a" [] atEnd
        fromUpper = typeArea ctrlMods "A" [] atEnd
      TB.getCursor (TA.buffer fromLower) `shouldBe` TB.Cursor 0 5
      TA.selectionAnchor fromLower `shouldBe` TB.Cursor 0 0
      TB.getCursor (TA.buffer fromUpper) `shouldBe` TB.Cursor 0 5
      TA.selectionAnchor fromUpper `shouldBe` TB.Cursor 0 0

  describe "text word selection" $ do
    it "groups Unicode words, whitespace and punctuation by character index" $ do
      let
        text = "αβ_猫  🙂!?"
      map (textWordBounds text) [0 .. 8]
        `shouldBe` replicate 4 (0, 4) ++ replicate 2 (4, 6) ++ replicate 3 (6, 9)
    it "clamps clicks outside the text and handles empty text" $ do
      textWordBounds "" 10 `shouldBe` (0, 0)
      textWordBounds "one two" (-10) `shouldBe` (0, 3)
      textWordBounds "one two" 100 `shouldBe` (4, 7)
      textWordBounds (T.replicate 10000 "猫") 5000 `shouldBe` (0, 10000)
