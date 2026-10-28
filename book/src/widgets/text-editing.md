# Text Editing

`nano-ui` offers two levels of multi-line text editing:
1. **`textArea`**: Works on a `Var Text` directly. Suitable for short notes, descriptions, and commit messages.
2. **`textAreaDocument`**: Uses a `Var TextDocument` piece tree. Optimized for large files, editors, and code buffers with line-level immutable sharing.

## Using `textAreaDocument` for Large Files

```haskell
editorView :: Var TextDocument -> NanoUI ()
editorView docVar = do
  resp <- textAreaDocument' docVar
  
  -- Programmatic undo/redo integration
  when (textCanUndo (respId resp)) $ do
    -- Right click menu or toolbar button can invoke TextCommand
    pure ()
```

### Text Commands & Shared Undo History

Edits performed via the keyboard, mouse selection, context menu, and programmatic calls to `runTextCommand` share a single undo history:

```haskell
-- Run an undo command on a text editor
runTextCommand editorWid TextUndo
```

> [!TIP]
> Directly overwriting the `Var Text` or `Var TextDocument` from outside resets the undo stack. To preserve undo across programmatic edits, issue a `TextCommand` instead.
