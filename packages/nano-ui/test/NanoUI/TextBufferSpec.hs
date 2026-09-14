{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text qualified as T
import NanoUI.Frame.TextEdit (textWordBounds)
import NanoUI.Widgets.TextArea as TA
import NanoUI.Widgets.TextBuffer as TB
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

    it "initializes an empty buffer with a single line and (0,0) cursor" $ do
      let
        b = TB.empty
      TB.toText b `shouldBe` ""
      TB.getCursor b `shouldBe` TB.Cursor 0 0
      TB.getLineCount b `shouldBe` 1

    it "places the cursor at (0,0) for any fromText input" $ do
      TB.getCursor (TB.fromText "hello") `shouldBe` TB.Cursor 0 0
      TB.getCursor (TB.fromText "a\nb") `shouldBe` TB.Cursor 0 0

    it "deleteRange leaves the cursor at the start of the deleted span" $ do
      let
        gone = TB.deleteRange (TB.Cursor 0 0) (TB.Cursor 0 5) (TB.fromText "hello")
        mid = TB.deleteRange (TB.Cursor 0 1) (TB.Cursor 0 4) (TB.fromText "hello")
      TB.toText gone `shouldBe` ""
      TB.getCursor gone `shouldBe` TB.Cursor 0 0
      TB.toText mid `shouldBe` "ho"
      TB.getCursor mid `shouldBe` TB.Cursor 0 1

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

    it "handles basic character insertion" $ do
      let
        b = foldl (flip TB.insertChar) TB.empty (T.unpack "ab")
      TB.toText b `shouldBe` "ab"
      TB.getCursor b `shouldBe` TB.Cursor 0 2

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

    it "splits lines correctly on breakLine" $ do
      let
        b = TB.breakLine . TB.moveToEOL . TB.fromText $ "hello"
      TB.toLines b `shouldBe` ["hello", ""]
      TB.getCursor b `shouldBe` TB.Cursor 1 0

    it "preserves column position when navigating lines of varying lengths" $ do
      -- Line 0: "12345" (length 5)
      -- Line 1: "12"    (length 2)
      -- Line 2: "12345" (length 5)
      let
        b0 = TB.fromText "12345\n12\n12345"
      let
        bAtCol4 =
          TB.moveRight . TB.moveRight . TB.moveRight . TB.moveRight $ b0
      TB.getCursor bAtCol4 `shouldBe` TB.Cursor 0 4

      -- Move down into shorter line (snaps visually to column 2)
      let
        bDown1 = TB.moveDown bAtCol4
      TB.getCursor bDown1 `shouldBe` TB.Cursor 1 2

      -- Move down again into longer line (restores original column 4)
      let
        bDown2 = TB.moveDown bDown1
      TB.getCursor bDown2 `shouldBe` TB.Cursor 2 4

      -- Move up restores the preferred column too
      TB.getCursor (TB.moveUp bDown2) `shouldBe` TB.Cursor 1 2

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

    it "finds the document end independently of the current cursor" $ do
      TB.documentEnd (TB.fromText "α\n猫🙂") `shouldBe` TB.Cursor 1 2
      TB.documentEnd (TB.fromText "α\n") `shouldBe` TB.Cursor 1 0
      TB.documentEnd TB.empty `shouldBe` TB.Cursor 0 0

    it "deletes words backward properly" $ do
      let
        b = TB.deletePrevWord (TB.moveToEOL (TB.fromText "foo bar"))
      TB.toText b `shouldBe` "foo "

    it "deletePrevWord eats trailing whitespace then the previous word" $ do
      let
        b = TB.deletePrevWord (TB.moveToEOL (TB.fromText "foo "))
      TB.toText b `shouldBe` ""

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

    it "killToEOL removes the rest of the current line" $ do
      let
        b = TB.killToEOL (TB.moveRight (TB.fromText "hello"))
      TB.toText b `shouldBe` "h"

    it "killToBOL removes text before the cursor on the current line" $ do
      let
        b = TB.killToBOL (TB.moveToEOL (TB.fromText "hello"))
      TB.toText b `shouldBe` ""

  describe "NanoUI.Widgets.TextArea" $ do
    it
      "typing and Enter replace a backwards multiline selection and collapse its anchor" $ do
      let
        selected =
          TA.setTextAreaSelection (TB.Cursor 1 1) (TB.Cursor 0 1) $
            TA.initTextAreaState "abc\ndef"
        typed = TA.handleTextAreaEvent (TA.KeyChar 'λ') noMods selected
        entered = TA.handleTextAreaEvent TA.KeyEnter noMods selected
      TB.toText (TA.buffer typed) `shouldBe` "aλef"
      TB.getCursor (TA.buffer typed) `shouldBe` TB.Cursor 0 2
      TA.selectionAnchor typed `shouldBe` TB.Cursor 0 2
      TB.toText (TA.buffer entered) `shouldBe` "a\nef"
      TB.getCursor (TA.buffer entered) `shouldBe` TB.Cursor 1 0
      TA.selectionAnchor entered `shouldBe` TB.Cursor 1 0

    it "Ctrl+Left/Right move by word" $ do
      let
        s0 = TA.initTextAreaState "foo bar"
        sRight = TA.handleTextAreaEvent TA.KeyRight ctrlMods s0
        sLeft = TA.handleTextAreaEvent TA.KeyLeft ctrlMods sRight
      TB.getCursor (TA.buffer sRight) `shouldBe` TB.Cursor 0 3
      TB.getCursor (TA.buffer sLeft) `shouldBe` TB.Cursor 0 0

    it "PageDown/PageUp move by viewport page and follow the caret" $ do
      let
        s0 =
          TA.setTextAreaViewport (80, 32) 16 $
            TA.initTextAreaState "l0\nl1\nl2\nl3"
        sDown = TA.handleTextAreaEvent TA.KeyPageDown noMods s0
        sDown2 = TA.handleTextAreaEvent TA.KeyPageDown noMods sDown
        sUp = TA.handleTextAreaEvent TA.KeyPageUp noMods sDown2
      TB.getCursor (TA.buffer sDown) `shouldBe` TB.Cursor 2 0
      snd (TA.scrollOffset sDown) `shouldBe` 16
      TB.getCursor (TA.buffer sDown2) `shouldBe` TB.Cursor 3 0
      TB.getCursor (TA.buffer sUp) `shouldBe` TB.Cursor 1 0

    it "layout subtracts scrollOffset from caret and line Y" $ do
      let
        s0 =
          TA.setTextAreaViewport (80, 16) 16 $
            TA.initTextAreaState "a\nb"
        s1 = TA.handleTextAreaEvent TA.KeyDown noMods s0
        layout = TA.computeTextAreaLayout (fromIntegral . T.length) 16 s1
      TA.layoutCaretY layout `shouldBe` 0
      map TA.visualLineY (TA.layoutLines layout) `shouldBe` [-16, 0]

    it "Ctrl+A and Ctrl+a both select all" $ do
      let
        s0 = TA.initTextAreaState "hello"
        atEnd = TA.handleTextAreaEvent TA.KeyEnd noMods s0
        fromLower = TA.handleTextAreaEvent (TA.KeyChar 'a') ctrlMods atEnd
        fromUpper = TA.handleTextAreaEvent (TA.KeyChar 'A') ctrlMods atEnd
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
    it "handles a long Unicode word" $ do
      textWordBounds (T.replicate 10000 "猫") 5000 `shouldBe` (0, 10000)
