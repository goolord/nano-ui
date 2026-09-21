-- | The editing core every text field shares: a document, a selection and an
-- undo history, changed only by 'TextCommand's. Keys and menu rows map onto
-- commands, and an app can run the same commands on a field.
module NanoUI.Widgets.TextEditor
  ( -- * Commands
    TextCommand (..)
  , TextMotion (..)
    -- * Editors
  , Editor (..)
  , EditorMode (..)
  , singleLineMode
  , multiLineMode
  , editorFromBuffer
  , editorSelection
  , hasSelection
  , runCommand
  , runCommandIO
    -- * Key bindings
  , inputTextCommands
  , keyCommand
    -- * History
  , EditHistory (..)
  , EditGroup (..)
  , StoredEdit (..)
  , EditKind (..)
  , emptyHistory
  , sealHistory
  , canUndo
  , canRedo
  ) where

import NanoUI.Internal.Widgets.TextEditor
