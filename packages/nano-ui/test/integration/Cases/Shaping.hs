{-# OPTIONS_GHC -O0 #-}
module Cases.Shaping (tests) where

import Spec
import Data.Primitive.PrimArray (primArrayFromList)
import NanoUI.Bidi (BidiRun (..), bidiRuns, needsBidi)

tests :: [Spec]
tests =
  [ spec "bidi-runs" runBidiRunsTest
  , spec "shaped-carets" runShapedCaretTest
  ]

-- | Direction runs come out in visual order, with numbers and spaces
-- resolved against their neighbours.
runBidiRunsTest :: Context -> IORef Int -> IO ()
runBidiRunsTest _ failed = do
  assert failed (not (needsBidi "plain text 123"))
  forM_ cases $ \(txt, want) -> assertEq failed (txt, want) (txt, bidiRuns txt)
  where
    -- Numbers in Hebrew read left to right, left of the word before them,
    -- and a separator or terminator beside the digits stays with them.
    hebrewThen = [BidiRun 5 8 False, BidiRun 0 5 True]
    cases =
      [ ("", [])
      , ("hello", [BidiRun 0 5 False])
      , -- A left-to-right line: the space before Hebrew stays with the Latin.
        ("abc \x05D0\x05D1\x05D2", [BidiRun 0 4 False, BidiRun 4 7 True])
      , -- A right-to-left line puts the trailing Latin word on the left.
        ("\x05D0\x05D1\x05D2 abc", [BidiRun 4 7 False, BidiRun 0 4 True])
      , ("\x05E9\x05DC\x05D5\x05DD 123", hebrewThen)
      , ("\x05E9\x05DC\x05D5\x05DD 1.5", hebrewThen)
      , ("\x05E9\x05DC\x05D5\x05DD 50%", hebrewThen)
      , -- A byte order mark does not turn a line right to left.
        ("\xFEFF# Title.", [BidiRun 0 9 False])
      , ("\x0645\x0631\x062D abc\x0628\x0627\x0644", [BidiRun 7 10 True, BidiRun 4 7 False, BidiRun 0 4 True])
      ]

-- | Carets, hit testing and selection spans follow a host's shaped layout,
-- including right-to-left carets that decrease. A click lands between
-- user-perceived characters, on whichever side is nearer, never inside a
-- flag or between a letter and its accent. Without shaping, a font taken as
-- monospace places carets and finds them by whole advances alike.
runShapedCaretTest :: Context -> IORef Int -> IO ()
runShapedCaretTest _ failed = do
  let rtl = "\x05D0\x05D1\x05D2"
      mixed = "ab\x05D0\x05D1"
      flag = "\x1F1EC\x1F1E7"
      accented = "ae\x0301\&b"
      shape t
        | t == rtl = Just (ShapedText 30 30 (primArrayFromList [30, 20, 10, 0]))
        | t == mixed = Just (ShapedText 40 40 (primArrayFromList [0, 10, 40, 30, 20]))
        -- The flag is one glyph, which its two characters share.
        | t == flag = Just (ShapedText 16 16 (primArrayFromList [0, 8, 16]))
        | otherwise = Nothing
      fm = (monospaceMetrics 16) {fmShape = shape}
      -- ' ', 'x' and 'M' share an advance, so the font is taken as monospace.
      unshaped = (monospaceMetrics 10) {fmAdvance = \c -> if c == 'i' then 4 else 10}
  assertEq failed [30, 0, 0, 20] [caretX fm rtl 0, caretX fm rtl 3, caretX fm rtl 99, caretX unshaped "iiii" 2]
  assertEq
    failed
    [2, 0, 3, 2, 0, 1, 3, 4, 2]
    -- Unshaped, the accent takes an advance of its own, as 'lineWidth'
    -- measures it.
    [ textIndexAtX fm rtl 12, textIndexAtX fm rtl 100, textIndexAtX fm mixed 29, textIndexAtX fm flag 10
    , textIndexAtX fm flag 6, textIndexAtX unshaped accented 17, textIndexAtX unshaped accented 23
    , textIndexAtX unshaped accented 37, textIndexAtX unshaped "iiii" 20
    ]
  -- Selecting the Hebrew of a mixed line covers its run, not the Latin.
  assertEq failed [[(10, 30)], [], [(20, 40)], [(0, 10)]] $
    [selectionSpans fm rtl 0 2, selectionSpans fm rtl 2 2, selectionSpans fm mixed 2 4, selectionSpans fm mixed 0 1]
