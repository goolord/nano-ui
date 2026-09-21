-- | Implementation of "NanoUI.Widgets.TextEditor", plus the store encoding
-- of an 'EditorMode'.
module NanoUI.Internal.Widgets.TextEditor
  ( -- * Commands
    TextCommand (..)
  , TextMotion (..)
    -- * Editors
  , Editor (..)
  , EditorMode (..)
  , singleLineMode
  , multiLineMode
  , editorModeCode
  , editorModeFromCode
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

import Control.Monad (void, when)
import Data.Bits ((.&.), (.|.))
import Data.Char (isPrint, isSpace, toLower)
import Data.Text qualified as T
import Data.Text.Short qualified as TS
import NanoUI.Internal.Context (Context (..))
import NanoUI.Internal.Input (Input (..), Key (..), Modifiers (..))
import NanoUI.Widgets.TextBuffer (Cursor (..), TextBuffer, TextEdit (..))
import NanoUI.Widgets.TextCommand (TextCommand (..), TextMotion (..))
import NanoUI.Widgets.TextBuffer qualified as TB

-- | How a field lets its document be changed.
data EditorMode = EditorMode
  { modeMultiLine :: !Bool
  , modeEditable :: !Bool
  -- ^ Off for selectable labels: only motion, selection and copy apply.
  , modeCopyable :: !Bool
  -- ^ Off for passwords: nothing reaches the clipboard.
  }
  deriving (Eq, Show)

-- | Editable, copyable text without newline insertion.
singleLineMode :: EditorMode
singleLineMode = EditorMode {modeMultiLine = False, modeEditable = True, modeCopyable = True}

-- | Editable, copyable text with multiline commands enabled.
multiLineMode :: EditorMode
multiLineMode = singleLineMode {modeMultiLine = True}

-- | A mode as a store integer, so a command sent to a widget id between
-- frames knows what kind of field it edits.
editorModeCode :: EditorMode -> Int
editorModeCode m =
  8
    .|. (if modeMultiLine m then 1 else 0)
    .|. (if modeEditable m then 0 else 2)
    .|. (if modeCopyable m then 0 else 4)

-- | Decode a stored mode. 'Nothing' means the field-registration bit is absent.
editorModeFromCode :: Int -> Maybe EditorMode
editorModeFromCode code
  | code .&. 8 == 0 = Nothing
  | otherwise =
      Just
        EditorMode
          { modeMultiLine = code .&. 1 /= 0
          , modeEditable = code .&. 2 == 0
          , modeCopyable = code .&. 4 == 0
          }

-- | A document with its selection (the cursor is the buffer's, the anchor
-- the other end) and history.
data Editor = Editor
  { editorBuffer :: !TextBuffer
  , editorAnchor :: !Cursor
  , editorHistory :: !EditHistory
  }
  deriving (Show)

-- | Start an editor with no selection or history at the buffer's current cursor.
editorFromBuffer :: TextBuffer -> Editor
editorFromBuffer buf = Editor buf (TB.getCursor buf) emptyHistory

-- | @(anchor, cursor)@.
editorSelection :: Editor -> (Cursor, Cursor)
editorSelection ed = (editorAnchor ed, TB.getCursor (editorBuffer ed))

-- | Whether the selection anchor differs from the buffer cursor.
hasSelection :: Editor -> Bool
hasSelection ed = editorAnchor ed /= TB.getCursor (editorBuffer ed)

--------------------------------------------------------------------------------
-- History
--------------------------------------------------------------------------------

-- | What started a group of edits, which decides what may join it.
data EditKind = EditTyping | EditDeleting | EditOther
  deriving (Eq, Show)

-- | An edit as history keeps it. Undo steps live for the life of a field and
-- are rarely replayed, so their texts are compact copies: they cost two
-- words less than a 'T.Text', and never keep alive the larger text a slice
-- was cut from.
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

-- | Undo and redo groups, newest first, with grouping state for consecutive edits.
data EditHistory = EditHistory
  { historyUndo :: ![EditGroup]
  , historyRedo :: ![EditGroup]
  , historyDepth :: !Int
  -- ^ Length of 'historyUndo'.
  , historyOpen :: !Bool
  -- ^ Whether the next edit may join the newest group. Undo, redo, cursor
  -- moves and commands from outside the field close it.
  }
  deriving (Eq, Show)

-- | No undo/redo steps and no open edit group.
emptyHistory :: EditHistory
emptyHistory = EditHistory [] [] 0 False

-- | Start the next edit in a group of its own.
sealHistory :: EditHistory -> EditHistory
sealHistory h = h {historyOpen = False}

-- | Whether at least one edit group can be undone.
canUndo :: EditHistory -> Bool
canUndo = not . null . historyUndo

-- | Whether at least one undone group can be reapplied.
canRedo :: EditHistory -> Bool
canRedo = not . null . historyRedo

-- | Undo steps kept per field. Older steps are dropped in batches.
maxHistoryDepth :: Int
maxHistoryDepth = 500

-- | Record an edit. Typing joins the group before it while the selection is
-- where that group left it, until a new word starts; consecutive deletes
-- join the same way. Anything else starts a group. Recording clears redo.
record :: EditKind -> (Cursor, Cursor) -> TextEdit -> (Cursor, Cursor) -> EditHistory -> EditHistory
record kind before edit after (EditHistory undos _ depth open) =
  case undos of
    g : rest
      | joins g ->
          EditHistory (g {groupEdits = stored : groupEdits g, groupAfter = after} : rest) [] depth True
    _ ->
      let depth' = depth + 1
          group = EditGroup kind [stored] before after
       in if depth' > maxHistoryDepth + 50
            then EditHistory (take maxHistoryDepth (group : undos)) [] maxHistoryDepth True
            else EditHistory (group : undos) [] depth' True
  where
    stored = StoredEdit (editAt edit) (TS.fromText (editRemoved edit)) (TS.fromText (editInserted edit))
    joins g =
      open
        && kind /= EditOther
        && groupKind g == kind
        && groupAfter g == before
        && (kind /= EditTyping || not (startsWord g))
    -- A letter typed after a space starts a new undo step.
    startsWord g = case (groupEdits g, T.uncons (editInserted edit)) of
      (StoredEdit _ _ prevInserted : _, Just (c, _)) -> not (isSpace c) && maybe False (isSpace . snd) (TS.unsnoc prevInserted)
      _ -> False

--------------------------------------------------------------------------------
-- Commands
--------------------------------------------------------------------------------

-- | Run a command that needs no clipboard. 'Cut', 'Copy' and 'Paste' do
-- nothing here; 'runCommandIO' runs them.
runCommand :: EditorMode -> TextCommand -> Editor -> Editor
runCommand mode cmd ed@(Editor buf anchor hist) =
  case cmd of
    InsertText raw
      | modeEditable mode ->
          let txt = singleLine raw
              kind
                | T.length txt == 1 && txt /= "\n" = EditTyping
                | otherwise = EditOther
           in if T.null txt && not (hasSelection ed) then ed else replaceSelection kind txt
    Delete motion
      | modeEditable mode ->
          if hasSelection ed
            then replaceSelection EditDeleting T.empty
            else
              let target = motionTarget motion
               in if target == cursor
                    then ed
                    else edit EditDeleting (TB.replaceEdit T.empty cursor target buf)
    Move motion extend ->
      let moved = moveBuffer motion
       in Editor moved (if extend then anchor else TB.getCursor moved) (sealHistory hist)
    SelectAll ->
      let end = TB.documentEnd buf
       in Editor (TB.withCursor end buf) (Cursor 0 0) (sealHistory hist)
    Select a c ->
      Editor (TB.withCursor c buf) (TB.clampCursor buf a) (sealHistory hist)
    Replace a b txt
      | modeEditable mode -> edit EditOther (TB.replaceEdit (singleLine txt) a b buf)
    ReplaceAll txt
      | modeEditable mode ->
          edit EditOther (TB.replaceEdit (singleLine txt) (Cursor 0 0) (TB.documentEnd buf) buf)
    Undo -> case historyUndo hist of
      g : rest ->
        let buf' = foldl (\b e -> TB.applyEdit (TB.invertEdit (replayed e)) b) buf (groupEdits g)
            (a, c) = groupBefore g
         in Editor (TB.withCursor c buf') a hist {historyUndo = rest, historyRedo = g : historyRedo hist, historyDepth = historyDepth hist - 1, historyOpen = False}
      [] -> ed
    Redo -> case historyRedo hist of
      g : rest ->
        let buf' = foldr (TB.applyEdit . replayed) buf (groupEdits g)
            (a, c) = groupAfter g
         in Editor (TB.withCursor c buf') a hist {historyUndo = g : historyUndo hist, historyRedo = rest, historyDepth = historyDepth hist + 1, historyOpen = False}
      [] -> ed
    _ -> ed
  where
    cursor = TB.getCursor buf
    replayed (StoredEdit at removed inserted) = TextEdit at (TS.toText removed) (TS.toText inserted)
    singleLine = (if modeMultiLine mode then id else T.filter (/= '\n')) . TB.insertableText
    replaceSelection kind txt = edit kind (TB.replaceEdit txt anchor cursor buf)
    edit kind e
      | editRemoved e == editInserted e = ed
      | otherwise =
      let buf' = TB.applyEdit e buf
          end = TB.getCursor buf'
       in Editor buf' end (record kind (anchor, cursor) e (end, end) hist)
    motionTarget = \case
      -- Deleting to the end of a line from its end takes the line break, so
      -- Ctrl+K keeps making progress.
      LineEnd | T.length (TB.lineAt (cursorRow cursor) buf) == cursorCol cursor -> TB.getCursor (TB.moveRight buf)
      motion -> TB.getCursor (moveBuffer motion)
    moveBuffer = \case
      CharLeft -> TB.moveLeft buf
      CharRight -> TB.moveRight buf
      WordLeft -> TB.moveWordLeft buf
      WordRight -> TB.moveWordRight buf
      LineStart -> TB.moveToBOL buf
      LineEnd -> TB.moveToEOL buf
      LineUp -> if modeMultiLine mode then TB.moveUp buf else buf
      LineDown -> if modeMultiLine mode then TB.moveDown buf else buf
      DocumentStart -> TB.moveToTop buf
      DocumentEnd -> TB.moveToBottom buf

-- | 'runCommand', with the clipboard commands going through the context's
-- clipboard.
runCommandIO :: Context -> EditorMode -> TextCommand -> Editor -> IO Editor
runCommandIO ctx mode cmd ed =
  case cmd of
    Copy -> ed <$ copySelection
    Cut
      | modeEditable mode && modeCopyable mode -> do
          copySelection
          pure (runCommand mode (Delete CharRight) (if hasSelection ed then ed else runCommand mode SelectAll ed))
      | otherwise -> pure ed
    Paste
      | modeEditable mode -> do
          clip <- ctxClipboardGet ctx
          pure (maybe ed (\txt -> runCommand mode (InsertText txt) ed) clip)
      | otherwise -> pure ed
    _ -> pure (runCommand mode cmd ed)
  where
    -- Copy without a selection takes the whole field.
    copySelection = when (modeCopyable mode) $ do
      let (a, c) = editorSelection ed
          buf = editorBuffer ed
          txt = if a /= c then TB.selectedText a c buf else TB.toText buf
      when (not (T.null txt)) $ void (ctxClipboardSet ctx txt)

-- | The command a key runs. Ctrl or Alt turns character and deletion keys
-- into word motions, and Shift extends the selection.
keyCommand :: EditorMode -> Modifiers -> Key -> Maybe TextCommand
keyCommand mode mods key =
  case key of
    KeyBackspace -> Just (Delete (if word then WordLeft else CharLeft))
    KeyDelete -> Just (Delete (if word then WordRight else CharRight))
    KeyLeft -> move (if word then WordLeft else CharLeft)
    KeyRight -> move (if word then WordRight else CharRight)
    KeyHome -> move (if modCtrl mods && multi then DocumentStart else LineStart)
    KeyEnd -> move (if modCtrl mods && multi then DocumentEnd else LineEnd)
    KeyUp | multi && not word -> move LineUp
    KeyDown | multi && not word -> move LineDown
    KeyEnter | multi && not word -> Just (InsertText "\n")
    _ -> Nothing
  where
    multi = modeMultiLine mode
    word = modCtrl mods || modAlt mods
    move m = Just (Move m (modShift mods))

-- | This frame's typing and keys as commands, typed characters first. Ctrl
-- turns characters into shortcuts. Ctrl with Alt is AltGr on many layouts, so
-- its characters are typed like plain ones.
inputTextCommands :: EditorMode -> Input -> [TextCommand]
inputTextCommands mode inp = T.foldr char keys (inputChars inp)
  where
    mods = inputModifiers inp
    shortcut = modCtrl mods && not (modAlt mods)
    char c rest
      | shortcut = maybe rest (: rest) (ctrlCharCommand mode mods c)
      | isPrint c = InsertText (T.singleton c) : rest
      | otherwise = rest
    keys = foldr (\k rest -> maybe rest (: rest) (keyCommand mode mods k)) [] (inputKeys inp)

-- | The command a character typed with Ctrl runs. Letters may arrive as the
-- letter or as their control code.
ctrlCharCommand :: EditorMode -> Modifiers -> Char -> Maybe TextCommand
ctrlCharCommand mode mods c =
  case toLower c of
    'a' -> Just SelectAll
    'c' -> Just Copy
    'x' -> Just Cut
    'v' -> Just Paste
    'z' | modShift mods || c == 'Z' -> Just Redo
    'z' -> Just Undo
    'y' -> Just Redo
    'k' | multi -> Just (Delete LineEnd)
    'u' | multi -> Just (Delete LineStart)
    'e' | multi -> Just (Move LineEnd False)
    '\x01' -> Just SelectAll
    '\x03' -> Just Copy
    '\x18' -> Just Cut
    '\x16' -> Just Paste
    '\x1a' -> Just Undo
    '\x19' -> Just Redo
    '\v' | multi -> Just (Delete LineEnd)
    '\NAK' | multi -> Just (Delete LineStart)
    '\ENQ' | multi -> Just (Move LineEnd False)
    _ -> Nothing
  where
    multi = modeMultiLine mode
