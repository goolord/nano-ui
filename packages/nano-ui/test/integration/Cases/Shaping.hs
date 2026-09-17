{-# LANGUAGE OverloadedStrings #-}

module Cases.Shaping
  ( runBidiRunsTest
  , runShapedCaretTest
  ) where

import Data.IORef (IORef)
import Data.Primitive.PrimArray (primArrayFromList)
import NanoUI
import NanoUI.Bidi (BidiRun (..), bidiRuns, needsBidi)
import NanoUI.Testing (Context, caretX, selectionSpans, textIndexAtX)
import NanoUI.Testing.Assert (assert, assertEq)

-- | Direction runs come out in visual order, with numbers and spaces
-- resolved against their neighbours.
runBidiRunsTest :: Context -> IORef Int -> IO ()
runBidiRunsTest _ failed = do
  assert failed (not (needsBidi "plain text 123"))
  assertEq failed [] (bidiRuns "")
  assertEq failed [BidiRun 0 5 False] (bidiRuns "hello")
  -- A left-to-right line: the space before Hebrew stays with the Latin.
  assertEq failed [BidiRun 0 4 False, BidiRun 4 7 True] (bidiRuns "abc \x05D0\x05D1\x05D2")
  -- A right-to-left line puts the trailing Latin word on the left.
  assertEq failed [BidiRun 4 7 False, BidiRun 0 4 True] (bidiRuns "\x05D0\x05D1\x05D2 abc")
  -- Numbers in Hebrew read left to right, left of the word before them.
  assertEq failed [BidiRun 5 8 False, BidiRun 0 5 True] (bidiRuns "\x05E9\x05DC\x05D5\x05DD 123")
  -- Latin in the middle of an Arabic line.
  assertEq
    failed
    [BidiRun 7 10 True, BidiRun 4 7 False, BidiRun 0 4 True]
    (bidiRuns "\x0645\x0631\x062D abc\x0628\x0627\x0644")

-- | Carets, hit testing and selection spans follow a host's shaped layout,
-- including right-to-left carets that decrease.
runShapedCaretTest :: Context -> IORef Int -> IO ()
runShapedCaretTest _ failed = do
  let rtl = "\x05D0\x05D1\x05D2"
      mixed = "ab\x05D0\x05D1"
      shape t
        | t == rtl = Just (ShapedText 30 30 (primArrayFromList [30, 20, 10, 0]))
        | t == mixed = Just (ShapedText 40 40 (primArrayFromList [0, 10, 40, 30, 20]))
        | otherwise = Nothing
      fm = (monospaceMetrics 16) {fmShape = shape}
  assertEq failed 30 (caretX fm rtl 0)
  assertEq failed 0 (caretX fm rtl 3)
  assertEq failed 0 (caretX fm rtl 99)
  assertEq failed 2 (textIndexAtX fm rtl 12)
  assertEq failed 0 (textIndexAtX fm rtl 100)
  assertEq failed [(10, 30)] (selectionSpans fm rtl 0 2)
  assertEq failed [] (selectionSpans fm rtl 2 2)
  -- Selecting the Hebrew of a mixed line covers its run, not the Latin.
  assertEq failed [(20, 40)] (selectionSpans fm mixed 2 4)
  assertEq failed [(0, 10)] (selectionSpans fm mixed 0 1)
  assertEq failed 3 (textIndexAtX fm mixed 29)
