-- | The text a text area edits, kept as its lines. A text area takes a
-- document and returns the document after the frame's edits: an edit
-- replaces the lines it touches and shares the rest, so a keystroke costs
-- the lines it changed, not the length of the document. Join the lines into
-- one @Text@ with 'documentText' only when the whole text is wanted, such as
-- when saving.
--
-- Programs edit a document the same way: 'replaceDocumentRange' and
-- 'editDocument' change only the lines they touch, and 'documentBuffer' and
-- 'bufferDocument' move between a document and a
-- "NanoUI.Widgets.TextBuffer" in O(1) for searching or moving through it.
module NanoUI.Widgets.TextDocument
  ( TextDocument
  , textDocument
  , emptyDocument
  , documentText
  , documentLines
  , documentLine
  , documentLineCount
  , sameDocument

    -- * Editing
  , documentRange
  , replaceDocumentRange
  , editDocument
  , documentBuffer
  , bufferDocument
  ) where

import NanoUI.Internal.Widgets.TextDocument
