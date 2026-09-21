-- | Character classes, word selection, and caret geometry shared by text fields.
-- Text positions count Unicode characters rather than UTF-8 bytes.
module NanoUI.Internal.Widgets.TextCommon
  ( -- * Character classes and word boundaries
    TextCharClass (..)
  , textCharClass
  , textWordBounds
    -- * Selection and caret helpers
  , textSelectionForClick
  , textSelectionForDrag
  , selectionCaretGeom
  ) where

import Data.Char (isAlphaNum, isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Internal.Types (clamp)

-- | Character classification for double-click word selection.
data TextCharClass = TextWord | TextSpace | TextOther
  deriving (Eq)

textCharClass :: Char -> TextCharClass
textCharClass c
  | isAlphaNum c || c == '_' = TextWord
  | isSpace c = TextSpace
  | otherwise = TextOther

-- | Find the word bounds around a character position in text.
textWordBounds :: Text -> Int -> (Int, Int)
textWordBounds text raw
  | T.null text = (0, 0)
  | otherwise =
      -- Split once: repeatedly indexing UTF-8 text makes long-word selection
      -- quadratic. The clamped index guarantees a non-empty suffix.
      let i = clamp 0 (T.length text - 1) raw
          (before, after) = T.splitAt i text
          sameClass = (== textCharClass (T.head after)) . textCharClass
       in ( i - T.length (T.takeWhileEnd sameClass before)
          , i + T.length (T.takeWhile sameClass after)
          )

-- | Calculate selection span for single/double/triple click.
textSelectionForClick :: Text -> Int -> Int -> (Int, Int)
textSelectionForClick value idx clicks
  | clicks >= 3 = (0, T.length value)
  | clicks == 2 = textWordBounds value idx
  | otherwise = (idx, idx)

-- | Calculate selection span when dragging mouse across text.
textSelectionForDrag :: Text -> Int -> Int -> Int -> (Int, Int)
textSelectionForDrag value anchor idx clicks
  | clicks >= 3 = (0, T.length value)
  | clicks == 2 =
      let (a0, a1) = textWordBounds value anchor
          (c0, c1) = textWordBounds value idx
       in (min a0 c0, max a1 c1)
  | otherwise = (anchor, idx)

-- | Shared caret geometry (caretX, caretY, caretH).
{-# INLINE selectionCaretGeom #-}
selectionCaretGeom :: Float -> Float -> Float -> Float -> (Float, Float, Float)
selectionCaretGeom originX originY pw lineH =
  (originX + pw, originY + 1, max 4 (lineH - 2))
