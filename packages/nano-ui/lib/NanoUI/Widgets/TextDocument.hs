-- | The text a text area edits, kept as its lines. A text area takes a
-- document and returns the document after the frame's edits: an edit
-- replaces the lines it touches and shares the rest, so a keystroke costs
-- the lines it changed, not the length of the document. Join the lines into
-- one @Text@ with 'documentText' only when the whole text is wanted, such as
-- when saving.
module NanoUI.Widgets.TextDocument
  ( TextDocument
  , textDocument
  , emptyDocument
  , documentText
  , documentLines
  , documentLine
  , documentLineCount
  , sameDocument
  ) where

import NanoUI.Internal.Widgets.TextDocument
