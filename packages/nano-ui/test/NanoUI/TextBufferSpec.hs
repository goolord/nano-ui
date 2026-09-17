{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import NanoUI.Frame.TextEdit (textWordBounds)
import NanoUI.Input (Key (..))
import NanoUI.Widgets.TextArea as TA
import NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextEditor as TE
import Test.Hspec

main :: IO ()
main = hspec spec

noMods :: TA.Modifiers
noMods = TA.Modifiers False False False

ctrlMods :: TA.Modifiers
ctrlMods = TA.Modifiers False True False

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
        replaced = TB.replaceRange "🙂\nλ" end start b
        deleted = TB.deleteRange end start b
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
        b = TB.insertChar '\t' TB.empty
      TB.toText b `shouldBe` "\t"
      TB.getCursor b `shouldBe` TB.Cursor 0 1

    it "inserts Unicode, tabs, and newlines while filtering control characters" $ do
      let
        b = TB.insertText "α\t\n猫\x01" (TB.withCursor (TB.Cursor 0 1) (TB.fromText "ab"))
      TB.toLines b `shouldBe` ["aα\t", "猫b"]
      TB.getCursor b `shouldBe` TB.Cursor 1 1

    it "empty insertion preserves the preferred column on a short line" $ do
      let
        b = TB.moveDown (TB.withCursor (TB.Cursor 0 4) (TB.fromText "12345\nx\n12345"))
      TB.getCursor (TB.moveDown (TB.insertText "" b)) `shouldBe` TB.Cursor 2 4

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

    it "deletePrevWord eats trailing whitespace then the previous word" $ do
      let
        b = TB.deletePrevWord (TB.moveToEOL (TB.fromText "foo "))
      TB.toText b `shouldBe` ""
      TB.toText (TB.deletePrevWord (TB.moveToEOL (TB.fromText "foo bar"))) `shouldBe` "foo "

    it "deletePrevWord joins lines at beginning of line" $ do
      let
        b =
          TB.deletePrevWord
            (TB.moveToBOL (TB.moveDown (TB.fromText "foo\nbar")))
      TB.toText b `shouldBe` "bar"

    it "deleteNextWord deletes the word after the cursor" $ do
      let
        b = TB.deleteNextWord (TB.fromText "foo bar")
      TB.toText b `shouldBe` " bar"

    it "killToEOL and killToBOL remove the rest of the line on either side" $ do
      TB.toText (TB.killToEOL (TB.moveRight (TB.fromText "hello"))) `shouldBe` "h"
      TB.toText (TB.killToBOL (TB.moveToEOL (TB.fromText "hello"))) `shouldBe` ""

  describe "NanoUI.Widgets.TextBuffer edits" $ do
    it "applies an edit across lines and inverts it back" $ do
      let
        b0 = TB.fromText "αβ\n猫犬\nend"
        e = TB.replaceEdit "🙂\nλ\nμ" (TB.Cursor 0 1) (TB.Cursor 2 1) b0
        b1 = TB.applyEdit e b0
      TB.editRemoved e `shouldBe` "β\n猫犬\ne"
      TB.toText b1 `shouldBe` "α🙂\nλ\nμnd"
      TB.getCursor b1 `shouldBe` TB.Cursor 2 1
      TB.editEnd e `shouldBe` TB.Cursor 2 1
      TB.toText (TB.applyEdit (TB.invertEdit e) b1) `shouldBe` "αβ\n猫犬\nend"

    it "edits the middle of a long document locally" $ do
      let
        doc = T.intercalate "\n" [T.pack (show i) | i <- [1 .. 20000 :: Int]]
        b0 = TB.fromText doc
        edited = foldl' (\b i -> TB.insertText "x\n" (TB.withCursor (TB.Cursor (5000 + i) 0) b)) b0 [1 .. 500 :: Int]
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
        typed = foldl' (\s c -> TA.handleTextAreaEvent (TA.TAChar c) noMods s) s0 ("one two" :: String)
        undone = TA.handleTextAreaEvent (TA.TAChar 'z') ctrlMods typed
        redone = TA.handleTextAreaEvent (TA.TAChar 'z') (TA.Modifiers True True False) undone
      TB.toText (TA.buffer undone) `shouldBe` "one "
      TB.toText (TA.buffer redone) `shouldBe` "one two"


    it
      "typing and Enter replace a backwards multiline selection and collapse its anchor" $ do
      let
        selected =
          TA.setTextAreaSelection (TB.Cursor 1 1) (TB.Cursor 0 1) $
            TA.initTextAreaState "abc\ndef"
        typed = TA.handleTextAreaEvent (TA.TAChar 'λ') noMods selected
        entered = TA.handleTextAreaEvent (TA.TAKey KeyEnter) noMods selected
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
              deleted = TA.handleTextAreaEvent (TA.TAKey KeyDelete) mods s0
              right = TA.handleTextAreaEvent (TA.TAKey KeyRight) mods s0
              left = TA.handleTextAreaEvent (TA.TAKey KeyLeft) mods right
            TB.toText (TA.buffer deleted) `shouldBe` " bar"
            TB.getCursor (TA.buffer right) `shouldBe` TB.Cursor 0 3
            TB.getCursor (TA.buffer left) `shouldBe` TB.Cursor 0 0
        )
        [ctrlMods, TA.Modifiers False False True]

    it "layout subtracts scrollOffset from caret and line Y" $ do
      let
        s0 =
          TA.setTextAreaViewport (80, 16) 16 $
            TA.initTextAreaState "a\nb"
        s1 = TA.handleTextAreaEvent (TA.TAKey KeyDown) noMods s0
        layout = TA.computeTextAreaLayout (fromIntegral . T.length) 16 s1
      TA.layoutCaretY layout `shouldBe` 0
      map TA.visualLineY (TA.layoutLines layout) `shouldBe` [-16, 0]

    it "Ctrl+A and Ctrl+a both select all" $ do
      let
        s0 = TA.initTextAreaState "hello"
        atEnd = TA.handleTextAreaEvent (TA.TAKey KeyEnd) noMods s0
        fromLower = TA.handleTextAreaEvent (TA.TAChar 'a') ctrlMods atEnd
        fromUpper = TA.handleTextAreaEvent (TA.TAChar 'A') ctrlMods atEnd
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
