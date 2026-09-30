-- | Pure editor types, shared by editing operations and typed widget storage.
module NanoUI.Internal.TextEditor.Types
  ( EditorMode (..)
  , EditKind (..)
  , StoredEdit (..)
  , EditGroup (..)
  , EditHistory (..)
  )
where

import Data.Text.Short qualified as TS
import NanoUI.Widgets.TextBuffer (Cursor)

-- | How a field lets its document be changed.
data EditorMode = EditorMode
  { modeMultiLine :: !Bool
  , modeEditable :: !Bool
  -- ^ Off for selectable labels: only motion, selection and copy apply.
  , modeCopyable :: !Bool
  -- ^ Off for passwords: nothing reaches the clipboard.
  }
  deriving (Eq, Show)

-- | What started a group of edits, which decides what may join it.
data EditKind = EditTyping | EditDeleting | EditOther
  deriving (Eq, Show)

-- | Compact copied text avoids retaining slices of large documents in history.
data StoredEdit = StoredEdit !Cursor !TS.ShortText !TS.ShortText
  deriving (Eq, Show)

-- | Edits undone and redone as one step.
data EditGroup = EditGroup
  { groupKind :: !EditKind
  , groupEdits :: ![StoredEdit]
  -- ^ Newest first.
  , groupBefore :: !(Cursor, Cursor)
  -- ^ Anchor and cursor before the first edit.
  , groupAfter :: !(Cursor, Cursor)
  -- ^ Anchor and cursor after the last edit.
  }
  deriving (Eq, Show)

-- | Undo and redo groups, newest first, with consecutive-edit grouping state.
data EditHistory = EditHistory
  { historyUndo :: ![EditGroup]
  , historyRedo :: ![EditGroup]
  , historyDepth :: !Int
  -- ^ Length of 'historyUndo'.
  , historyOpen :: !Bool
  -- ^ Whether the next edit may join the newest group.
  }
  deriving (Eq, Show)
