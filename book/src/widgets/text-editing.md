# Text Editing

- **`textArea`** edits a `Var Text`. Use it for short text: notes, descriptions, commit messages.
- **`textAreaDocument`** edits a `Var TextDocument`, which shares unchanged lines between edits. Use it for files and code buffers.

## `textArea` for short text

```haskell
notes <- useVar ""
textArea notes
```

Every edit builds a new `Text`, which is cheap for a few kilobytes.

## `textAreaDocument` for documents

```haskell
editorView :: Var TextDocument -> NanoUI ()
editorView docVar = do
  resp <- textAreaDocumentWith' (grow . minW 320 . minH 200) docVar
  when (respChanged resp) markDirty
  writeVar editorId (respId resp)
```

Only the edited lines are copied; unchanged lines are shared. `documentText` materializes the whole text; call it when saving, not on every edit.

```haskell
save = liftIO (BS.writeFile path (TE.encodeUtf8 (documentText doc)))
```

`documentLineCount` is cheap enough for a status bar.

## One undo history

Keyboard edits, mouse selection, the right-click menu, and programmatic commands all dispatch `TextCommand`s, so they share one undo history:

```haskell
data TextCommand
  = InsertText Text
  | Delete TextMotion
  | Move TextMotion Bool
  | SelectAll
  | Select Cursor Cursor
  | Replace Cursor Cursor Text
  | ReplaceAll Text
  | Undo
  | Redo
  | Cut
  | Copy
  | Paste
```

Run a command against a field by its `WidgetId`:

```haskell
whenM (menuItem "Undo") (runTextCommand editor Undo)
whenM (menuItem "Insert date") (runTextCommand editor (InsertText today))
```

Query history state for menu enablement:

```haskell
editor <- readVar editorId
canUndo <- textCanUndo editor
canRedo <- textCanRedo editor
```

A complete Edit menu built this way is in [Build a Notepad](../tutorials/notepad.md).

> [!TIP]
> Directly overwriting the `Var Text` or `Var TextDocument` from outside resets the undo stack. To make a programmatic edit undoable, issue a `TextCommand` (`InsertText`, `ReplaceAll`, and so on) instead of writing the `Var`.

In the primed variants' `Response`, `respChanged` reports an edit, not a caret move, so it is the signal to mark a document dirty. `respId` names the editor for commands and focus.
