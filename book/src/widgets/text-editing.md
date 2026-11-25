# Text Editing

`nano-ui` provides two multi-line text editors depending on how much text you are editing:

- `textArea` edits a plain `Var Text`. Use it for short fields like notes, comments, or commit messages.
- `textAreaDocument` edits a `Var TextDocument`, which shares unchanged lines across edits. Use it for files, code buffers, and large documents.

## Short text with `textArea`

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:text_area}}
```

Each keystroke produces a new flat `Text`, which is fast and convenient for a few kilobytes of text.

## Large documents with `textAreaDocument`

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:document}}
```

`TextDocument` copies only the lines touched by an edit and shares everything else. Call `documentText` to flatten the document back into a single `Text` when saving or exporting, rather than on every edit:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:save}}
```

For status bars, `documentLineCount` runs without flattening the text.

## Unified undo history

Keyboard typing, mouse selection, right-click context menu actions, and programmatic commands all run through `TextCommand`, so everything shares a single undo/redo stack:

```haskell
{{#include ../../../packages/nano-ui/lib/NanoUI/Widgets/TextCommand.hs:text_command}}
```

Dispatch a command to any text field or editor by its `WidgetId`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:commands}}
```

Query undo/redo availability to enable or disable Edit menu entries:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:history}}
```

See [Tutorial 2: Build a Notepad](../tutorials/notepad.md) for a full Edit menu wired up this way.

> [!TIP]
> Overwriting a `Var Text` or `Var TextDocument` directly from outside the widget clears its undo stack. If you want a programmatic edit to be undoable, dispatch a `TextCommand` (`InsertText`, `ReplaceAll`, and so on) instead.

When using primed variants (`textArea'`, `textAreaDocument'`), `respChanged` is `True` after actual text edits and `False` after cursor movement or selection changes, making it safe to use for dirty-state tracking. Keep `respId` to target the editor with `runTextCommand` or `requestFocus`.

## Rich text and hyperlinks

For read-only paragraphs mixing bold, italic, monospaced code runs, highlights, tooltips, and clickable links, use `richText` (or `selectableRichText` when users should be able to drag-select and copy):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/TextEditing.hs:rich_text}}
```

Plain string literals produce `Inline` pieces via `OverloadedStrings`. Combine them with `strong`, `emphasis`, `inlineCode`, `inlineWith`, `restyle`, `inlineBackground`, `inlineTooltip`, and `hyperlink target label`. Both `richText` and `selectableRichText` wrap to their container width and return `Maybe Text` holding the `target` of any link clicked on that frame. For full CommonMark documents, use `nano-ui-markdown`.
