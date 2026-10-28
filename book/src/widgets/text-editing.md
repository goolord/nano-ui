# Text Editing

nano-ui offers two levels of multi-line editing:

1. **`textArea`** works on a `Var Text`. It is right for short notes, descriptions, and commit messages, where the whole string is the value.
2. **`textAreaDocument`** works on a `Var TextDocument`. It is built for large files, editors, and code buffers, with line-level immutable sharing and an undo history that survives edits.

## `textArea` for short text

```haskell
notes <- useVar ""
textArea notes
```

Every edit produces a new `Text`; for a few kilobytes that is a fine trade for the simple API.

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

`documentLineCount` is cheap and is the right way to show a line count in a status bar.

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

## Reading the response

The primed variants return a `Response`:

- `respChanged` reports an **edit**, not a caret move. Use it to mark a document dirty.
- `respSubmitted` reports Enter in a single-line field.
- `respId` identifies the editor for commands and focus.

For a single-line field, `textInput` and `textInput'` are the equivalents; `searchInput'` adds debounced change reporting and a clear button.

## See also

- Widget inventory: [Widget Gallery](index.md).
- Input widgets and their signatures: [Buttons, Menus & Inputs](controls.md).
- Working with the document in an application: [Build a Notepad](../tutorials/notepad.md).
- Theming and typography: [Styling & Theming](../styling/index.md).
