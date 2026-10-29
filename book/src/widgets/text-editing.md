# Text Editing

- `textArea` edits a `Var Text`. Use it for short text: notes, descriptions, commit messages.
- `textAreaDocument` edits a `Var TextDocument`, which shares unchanged lines between edits. Use it for files and code buffers.

## `textArea` for short text

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:text_area}}
```

Every edit builds a new `Text`, which is cheap for a few kilobytes.

## `textAreaDocument` for documents

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:document}}
```

Only the edited lines are copied; unchanged lines are shared. `documentText` materializes the whole text; call it when saving instead of on every edit.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:save}}
```

`documentLineCount` is cheap enough for a status bar.

## One undo history

Keyboard edits, mouse selection, the right-click menu, and programmatic commands all dispatch `TextCommand`s, so they share one undo history:

```haskell
{{#include ../../../packages/nano-ui/lib/NanoUI/Widgets/TextCommand.hs:text_command}}
```

Run a command against a field by its `WidgetId`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:commands}}
```

Query history state for menu enablement:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:history}}
```

A complete Edit menu built this way is in [Build a Notepad](../tutorials/notepad.md).

> [!TIP]
> Directly overwriting the `Var Text` or `Var TextDocument` from outside resets the undo stack. To make a programmatic edit undoable, issue a `TextCommand` (`InsertText`, `ReplaceAll`, and so on) instead of writing the `Var`.

In the primed variants' `Response`, `respChanged` is `True` after an edit and `False` after a caret move alone, so use it to mark a document dirty. `respId` names the editor for commands and focus.
