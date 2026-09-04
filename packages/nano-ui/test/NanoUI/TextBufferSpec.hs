{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import qualified Data.Text as T
import NanoUI.Widgets.TextArea as TA
import NanoUI.Widgets.TextBuffer as TB

main :: IO ()
main = hspec spec

noMods :: TA.Modifiers
noMods = TA.Modifiers False False False

ctrlMods :: TA.Modifiers
ctrlMods = TA.Modifiers False True False

spec :: Spec
spec = do
  describe "NanoUI.Widgets.TextBuffer" $ do
    it "initializes an empty buffer with a single line and (0,0) cursor" $ do
      let b = TB.empty
      TB.toText b `shouldBe` ""
      TB.getCursor b `shouldBe` TB.Cursor 0 0
      TB.getLineCount b `shouldBe` 1

    it "places the cursor at (0,0) for any fromText input" $ do
      TB.getCursor (TB.fromText "hello") `shouldBe` TB.Cursor 0 0
      TB.getCursor (TB.fromText "a\nb") `shouldBe` TB.Cursor 0 0

    it "deleteRange leaves the cursor at the start of the deleted span" $ do
      let gone = TB.deleteRange (TB.Cursor 0 0) (TB.Cursor 0 5) (TB.fromText "hello")
          mid = TB.deleteRange (TB.Cursor 0 1) (TB.Cursor 0 4) (TB.fromText "hello")
      TB.toText gone `shouldBe` ""
      TB.getCursor gone `shouldBe` TB.Cursor 0 0
      TB.toText mid `shouldBe` "ho"
      TB.getCursor mid `shouldBe` TB.Cursor 0 1

    it "roundtrips a trailing newline through fromText/toText" $ do
      TB.toText (TB.fromText "a\n") `shouldBe` "a\n"
      TB.toLines (TB.fromText "a\n") `shouldBe` ["a", ""]

    it "handles basic character insertion" $ do
      let b = foldl (flip TB.insertChar) TB.empty (T.unpack "ab")
      TB.toText b `shouldBe` "ab"
      TB.getCursor b `shouldBe` TB.Cursor 0 2

    it "inserts a tab that text-zipper would otherwise drop" $ do
      let b = TB.insertChar '\t' TB.empty
      TB.toText b `shouldBe` "\t"
      TB.getCursor b `shouldBe` TB.Cursor 0 1

    it "insertText handles newlines without unpacking a thunk chain" $ do
      let b = TB.insertText "a\nb" TB.empty
      TB.toLines b `shouldBe` ["a", "b"]
      TB.getCursor b `shouldBe` TB.Cursor 1 1

    it "splits lines correctly on breakLine" $ do
      let b = TB.breakLine . TB.moveToEOL . TB.fromText $ "hello"
      TB.toLines b `shouldBe` ["hello", ""]
      TB.getCursor b `shouldBe` TB.Cursor 1 0

    it "preserves column position when navigating lines of varying lengths" $ do
      -- Line 0: "12345" (length 5)
      -- Line 1: "12"    (length 2)
      -- Line 2: "12345" (length 5)
      let b0 = TB.fromText "12345\n12\n12345"
      let bAtCol4 =
            TB.moveRight . TB.moveRight . TB.moveRight . TB.moveRight $ b0
      TB.getCursor bAtCol4 `shouldBe` TB.Cursor 0 4

      -- Move down into shorter line (snaps visually to column 2)
      let bDown1 = TB.moveDown bAtCol4
      TB.getCursor bDown1 `shouldBe` TB.Cursor 1 2

      -- Move down again into longer line (restores original column 4)
      let bDown2 = TB.moveDown bDown1
      TB.getCursor bDown2 `shouldBe` TB.Cursor 2 4

      -- Move up restores the preferred column too
      TB.getCursor (TB.moveUp bDown2) `shouldBe` TB.Cursor 1 2

    it "deletes words backward properly" $ do
      let b = TB.deletePrevWord (TB.moveToEOL (TB.fromText "foo bar"))
      TB.toText b `shouldBe` "foo "

    it "deletePrevWord eats trailing whitespace then the previous word" $ do
      let b = TB.deletePrevWord (TB.moveToEOL (TB.fromText "foo "))
      TB.toText b `shouldBe` ""

    it "deletePrevWord joins lines at beginning of line" $ do
      let b =
            TB.deletePrevWord
              (TB.moveToBOL (TB.moveDown (TB.fromText "foo\nbar")))
      TB.toText b `shouldBe` "bar"

    it "deleteNextWord deletes the word after the cursor" $ do
      let b = TB.deleteNextWord (TB.fromText "foo bar")
      TB.toText b `shouldBe` " bar"

    it "killToEOL removes the rest of the current line" $ do
      let b = TB.killToEOL (TB.moveRight (TB.fromText "hello"))
      TB.toText b `shouldBe` "h"

    it "killToBOL removes text before the cursor on the current line" $ do
      let b = TB.killToBOL (TB.moveToEOL (TB.fromText "hello"))
      TB.toText b `shouldBe` ""

  describe "NanoUI.Widgets.TextArea" $ do
    it "Ctrl+Left/Right move by word" $ do
      let s0 = TA.initTextAreaState "foo bar"
          sRight = TA.handleTextAreaEvent TA.KeyRight ctrlMods s0
          sLeft = TA.handleTextAreaEvent TA.KeyLeft ctrlMods sRight
      TB.getCursor (TA.buffer sRight) `shouldBe` TB.Cursor 0 3
      TB.getCursor (TA.buffer sLeft) `shouldBe` TB.Cursor 0 0

    it "PageDown/PageUp move by viewport page and follow the caret" $ do
      let s0 =
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
      let s0 =
            TA.setTextAreaViewport (80, 16) 16 $
              TA.initTextAreaState "a\nb"
          s1 = TA.handleTextAreaEvent TA.KeyDown noMods s0
          layout = TA.computeTextAreaLayout (fromIntegral . T.length) 16 s1
      TA.layoutCaretY layout `shouldBe` 0
      map TA.visualLineY (TA.layoutLines layout) `shouldBe` [-16, 0]

    it "Ctrl+A and Ctrl+a both select all" $ do
      let s0 = TA.initTextAreaState "hello"
          atEnd = TA.handleTextAreaEvent TA.KeyEnd noMods s0
          fromLower = TA.handleTextAreaEvent (TA.KeyChar 'a') ctrlMods atEnd
          fromUpper = TA.handleTextAreaEvent (TA.KeyChar 'A') ctrlMods atEnd
      TB.getCursor (TA.buffer fromLower) `shouldBe` TB.Cursor 0 5
      TA.selectionAnchor fromLower `shouldBe` TB.Cursor 0 0
      TB.getCursor (TA.buffer fromUpper) `shouldBe` TB.Cursor 0 5
      TA.selectionAnchor fromUpper `shouldBe` TB.Cursor 0 0
