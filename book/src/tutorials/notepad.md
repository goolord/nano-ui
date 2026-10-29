# Tutorial 2: Build a Notepad

A menu bar, file dialogs, a text document with undo, a status bar, and a prompt before losing unsaved work.

The complete program is [`packages/nano-ui-demo/examples/Notepad.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/Notepad.hs). Run it with:

```sh
cabal run nano-ui-sdl-notepad
```

## 1. State that belongs to the session

A text document is not a type `useVar` can hold, and the editor's identity must outlive any single frame, so both live in a record allocated once:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:state}}
```

The menu state, path, dirty flag, zoom, and transient messages are simpler and can be `useVar`s inside the view. Allocate in `IO` what other code needs a handle to (a document, a task, a dialog), and use `useVar` for view-local state.

## 2. Close confirmation

By default the window closes as soon as the close button is pressed. To prompt about unsaved changes first, the notepad creates the window with `wsExitOnCloseRequest = False`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:main}}
```

With exit-on-close off, `winCloseRequested <$> askWindow` reports the request inside the view, where it can open a confirmation modal instead of quitting:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:close_request}}
```

`exitApp` quits immediately when the document is clean, and opens a modal when it is not. The full wiring is in section 7.

## 3. A menu bar from primitives

There is no `menuBar` widget. A menu bar is a row of `menuButton'`s, each paired with a `popup'` anchored to the button's rectangle, so a menu's contents are ordinary `NanoUI ()`.

`openMenu` holds the label of the open menu (empty when all are closed):

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:menu_bar}}
```

`popup'` needs a `Var Bool` for whether it is open, but the state is derived from `openMenu` ("is this label the open one?"). `constVar isOpen` supplies a read-only `Var`; writes to it are no-ops. When the popup reports a click, the code writes the real state (`openMenu`) instead.

> [!IMPORTANT]
> Call overlays unconditionally. `popup'` is called for every title on every frame, even when closed; it draws nothing while closed. Wrapping it in `when isOpen` would change the widget ids of later siblings each time a menu opens.

## 4. Commands, menus, and shortcuts

Each menu row is a `menuItem` / `menuItemShortcut`. The row reports activation, and the action runs in the same frame:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:item_helper}}

{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:file_menu}}
```

(`newDocument`, `openDocument`, `saveDocument`, and `exitApp` are ordinary `NanoUI ()` actions defined in the full source; `openDocument` starts a non-blocking `askOpenFileDialog`.)

`menuItemShortcut` shows the chord label and binds it **while the menu is open**. To make the command work with the menu closed, bind the same chord with `shortcut` outside the menu:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:shortcut_bindings}}
```

Only the first binding of a chord in a frame fires, so the menu row and the view binding never both run it. The menu row is declared first, so while the menu is open its row wins.

- `menuItemDisabled "Undo"` draws a dimmed row that cannot be clicked. The Edit menu draws a real item or a disabled one depending on `textCanUndo` / `textCanRedo`.
- An action that opens a file dialog writes the new `FileDialogId` into a `Var`; dialogs are non-blocking, so the view is not interrupted.

## 5. Opening and saving files

The SDL backend's file dialogs are asynchronous. `useFileDialog` polls a `Var (Maybe FileDialogId)`, consumes the finished result once, and restores focus afterwards:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:file_dialogs}}
```

The Open command starts one and keeps its id:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:open_cmd}}
```

`writeDocument` is ordinary Haskell around `liftIO`, because file I/O is an event-driven side effect:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:write_document}}
```

Only `documentText` converts the document to a `Text`; editing works on the document directly and never rebuilds the whole string per keystroke.

## 6. The editor

`textAreaDocument` edits a `Var TextDocument` with line-sharing and an undo history. The primed variant supplies the `Response` the application needs:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:editor}}
```

- `withKey gen` changes the editor's key when the document is replaced, so a new document gets a fresh caret, selection, and undo stack. Editing does not change `gen`.
- `respChanged` is `True` after an edit and `False` after a caret move alone. Marking dirty on caret movement would make a read-only inspection look like a modification.
- `respId` is saved for menu commands, which call `runTextCommand editor Undo` and the other `TextCommand`s.

`fontSizeScale zoomVal` zooms the text without touching the document. The zoom commands are ordinary `modifyVar` calls clamped to a sane range.

## 7. Dirty state, title, and the close prompt

The title is derived every frame from the path and the dirty flag, and set declaratively:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:window_title}}

-- ...after the layout, which returns the name as docName:
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:set_title}}
```

`setWindowTitleUi` does nothing when the title has not changed, so it is safe to call unconditionally. The imperative window actions (`resizeWindowUi`, `maximizeWindowUi`) are different: they run on every call and belong behind events.

The close request and the modal fit together like this:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:exit_app}}

-- ...inside the view, called unconditionally:
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:exit_modal}}
```

There is one modal at a time: opening the exit prompt closes the About modal first. Overlay state is a `Var Bool`, and the modal call itself is unconditional.

## 8. Undo, redo, and the rest of the editing commands

The document keeps one undo history shared by typing, context-menu actions, and programmatic commands. The Edit menu reads the history state and runs commands by widget id:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:edit_menu}}
```

`TextCommand` covers `InsertText`, `Delete`, `Move`, `Select`, `Replace`, `ReplaceAll`, `Undo`, `Redo`, `Cut`, `Copy`, `Paste`, and `SelectAll`. Using commands rather than writing the `Var` keeps undo intact; assigning a freshly computed `TextDocument` to `doc` clears the history.

> [!TIP]
> A focused text editor claims its editing chords (Ctrl+Z, Ctrl+X, and so on), and `shortcut` skips claimed keys. The menu items still work because they run explicit commands, and the top-level bindings fire when focus is elsewhere.

## 9. The status bar

The status bar is a row of muted labels. Line count comes from the document without materializing its text; word and character counts are computed on demand from `documentText`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:status_bar}}
```

Reading `doc` here records a dependency: every edit repaints the status bar. That is intentional, because the line count changes. If the label's text length changes often, pin its width with `fixedW` so the rest of the bar does not shift (see [Recurring Bugs](../troubleshooting/recurring-bugs.md)).

## 10. Try it

- Add a recent-files list persisted between runs.
- Replace the About modal with a floating `window` that can be moved and resized.
- Add word wrap using the editor's layout options.
- Build a custom title bar with `captionButtons` and `dragSpans` (see [Desktop Windows](../advanced/window-management.md)).

Next: [Tutorial 3: Live Data & Background Work](live-data.md) moves slow work off the frame and streams results back.
