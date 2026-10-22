-- | The commands text fields run: the same ones for keys, context menus and
-- app code.
module NanoUI.Widgets.TextCommand
  ( TextCommand (..)
  , TextMotion (..)
  ) where

import Data.Text (Text)
import NanoUI.Widgets.TextBuffer (Cursor)

-- | Cursor movement unit and direction.
data TextMotion
  = CharLeft
  | CharRight
  | WordLeft
  | WordRight
  | LineStart
  | LineEnd
  | LineUp
  | LineDown
  | DocumentStart
  | DocumentEnd
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Editing, navigation, selection, history, and clipboard operations on a
-- text field. Mutating commands record an undo step and replace any active
-- selection.
data TextCommand
  = -- | Replace the selection with text (typing, a snippet).
    InsertText !Text
  | -- | Delete the selection, or from the cursor to where the motion lands:
    -- @Delete CharLeft@ is Backspace, @Delete WordRight@ Ctrl+Delete.
    Delete !TextMotion
  | -- | Move the cursor, extending the selection when the flag is set.
    -- Without it, 'CharLeft' and 'CharRight' over a selection go to its start
    -- and its end.
    Move !TextMotion !Bool
  | SelectAll
  | -- | Select from the first position (the anchor) to the second (the
    -- cursor), clamped into the document.
    Select !Cursor !Cursor
  | -- | Replace the text between two positions, leaving the cursor after it.
    Replace !Cursor !Cursor !Text
  | -- | Replace the whole document as one undoable edit.
    ReplaceAll !Text
  | Undo
  | Redo
  | Cut
  | Copy
  | Paste
  deriving (Eq, Show)

