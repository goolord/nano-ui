# Tutorial 2: Build a Notepad

In this tutorial, you will build a plain-text editor with a menu bar, keyboard shortcuts, asynchronous file dialogs, undo/redo history, a status bar, and a warning prompt when closing with unsaved edits.

The complete program is in [`packages/nano-ui-demo/examples/Notepad.hs`](https://github.com/goolord/nano-ui/blob/main/packages/nano-ui-demo/examples/Notepad.hs). Run it with:

```sh
cabal run nano-ui-sdl-notepad
```

## 1. Session-level state

`TextDocument` is not one of the primitive types `useVar` supports, and our menu commands need a stable reference to the editor's widget ID. Both belong in a state record allocated once during setup:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:state}}
```

Simpler view-local values (which menu is open, the current file path, the dirty flag, zoom level, and status messages) can live in `useVar` calls inside the view. As a rule of thumb, allocate shared or handle-based state (documents, tasks, dialogs) in `IO` before starting the app, and keep local UI flags in `useVar`.

## 2. Close confirmation

Normally, clicking the window's close button exits immediately. To catch unsaved changes first, disable automatic exit with `wsExitOnCloseRequest = False`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:main}}
```

With that option disabled, `winCloseRequested <$> askWindow` becomes `True` on the frame the user tries to close the window. Instead of quitting immediately, the view calls `exitApp`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:close_request}}
```

When the document has no unsaved edits, `exitApp` closes the window right away; otherwise it opens a confirmation modal. We will wire up the modal in section 7.

## 3. Building a menu bar from primitives

`nano-ui` does not have a dedicated `menuBar` widget. Instead, you compose a row of `menuButton'` widgets, each paired with a `popup'` anchored to its button rectangle. Because a popup's body is plain `NanoUI ()`, menus can contain any combination of items, separators, or custom rows.

`openMenu` stores the title of whichever menu is currently open (or an empty string when all menus are closed):

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:menu_bar}}
```

`popup'` expects a `Var Bool` for its open state, while our source of truth is `openMenu`. Passing `constVar isOpen` supplies a read-only `Var` (writes to it are ignored), and when the popup reports a click or dismissal via its response, we update `openMenu` directly.

> [!IMPORTANT]
> Always call overlays unconditionally. Notice that `popup'` runs for every menu on every frame, even when closed; while closed it draws nothing. Putting `popup'` inside `when isOpen` would shift the widget IDs of following siblings whenever a menu opens or closes.

## 4. Commands, menus, and shortcuts

Menu rows use `menuItem` and `menuItemShortcut`. Each returns `True` on the frame it is activated, so the command runs right away:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:item_helper}}

{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:file_menu}}
```

(`newDocument`, `openDocument`, `saveDocument`, and `exitApp` are plain `NanoUI ()` helpers defined in the full source; `openDocument` launches a non-blocking dialog with `askOpenFileDialog`.)

`menuItemShortcut` displays the shortcut hint on the row and listens for that chord **only while the menu is open**. To make shortcuts work when the menu is closed too, register the same chords with `shortcut` outside the menu:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:shortcut_bindings}}
```

Only the first matching binding in a frame fires, so the menu item and the top-level shortcut will never trigger twice. Because the menu is declared earlier in the view, its item handles the chord whenever it is open.

- `menuItemDisabled "Undo"` draws a dimmed, non-interactive row. The Edit menu switches between a normal item and a disabled one based on `textCanUndo` and `textCanRedo`.
- Opening a file dialog stores the returned `FileDialogId` in a `Var`. Dialogs run asynchronously, so the UI keeps responding while the OS dialog is open.

## 5. Opening and saving files

In `nano-ui-sdl`, file dialogs never block the render loop. `useFileDialog` polls a `Var (Maybe FileDialogId)`, passes the selected paths to your callback once the dialog closes, and restores focus to the main window:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:file_dialogs}}
```

Clicking Open starts a dialog and saves its handle:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:open_cmd}}
```

Saving is ordinary `IO` wrapped in `liftIO`, triggered only by an event:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:write_document}}
```

Only call `documentText` when you actually need a flat `Text` value (such as when saving). During normal typing, `TextDocument` updates individual lines without rebuilding the full string on every keystroke.

## 6. The editor

`textAreaDocument` edits a `Var TextDocument` with structural line sharing and built-in undo history. Its primed variant (`textAreaDocument'`) returns the `Response` we need for dirty tracking and menu commands:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:editor}}
```

- `withKey gen` resets the editor's internal state whenever a new file is loaded or created by bumping `gen`, giving the new document a clean caret, selection, and undo stack. Normal edits leave `gen` unchanged.
- `respChanged` is `True` only when text is modified, not when the user merely moves the cursor or selects text. That prevents inspecting a file from marking it dirty.
- Saving `respId` lets our Edit menu target the editor with `runTextCommand editor Undo` and other `TextCommand` actions.

`fontSizeScale zoomVal` adjusts display size without altering the document, so the zoom actions are simple clamped `modifyVar` updates.

## 7. Dirty state, window title, and the close prompt

The window title is computed from the current file name and dirty flag on every frame, then passed to `setWindowTitle`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:window_title}}

-- ...after the layout, which returns the name as docName:
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:set_title}}
```

`setWindowTitle` caches the last title and skips the OS call when nothing changed, so calling it unconditionally on every frame is safe and free. Imperative window commands like `resizeWindow` or `maximizeWindow` run on every call and must always sit behind an event check.

Here is how `exitApp` and the confirmation modal connect:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:exit_app}}

-- ...inside the view, called unconditionally:
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:exit_modal}}
```

Opening the exit prompt closes the About modal first so only one modal appears at a time. Like popups, `modal` is called unconditionally every frame and controlled by its `Var Bool`.

## 8. Undo, redo, and editing commands

Typing, right-click menu actions, and programmatic commands all flow through `TextCommand`, so they share a single undo stack. The Edit menu queries `textCanUndo` and `textCanRedo`, then dispatches commands using the editor's widget ID:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:edit_menu}}
```

`TextCommand` includes `InsertText`, `Delete`, `Move`, `Select`, `Replace`, `ReplaceAll`, `Undo`, `Redo`, `Cut`, `Copy`, `Paste`, and `SelectAll`. Dispatching commands preserves undo history; overwriting the `Var TextDocument` directly resets it.

> [!TIP]
> While a text editor has focus, it claims its own editing shortcuts (Ctrl+Z, Ctrl+X, Ctrl+C, and so on), and `shortcut` ignores claimed keys. Menu rows still work because they dispatch commands directly, while the top-level shortcuts handle cases where focus is outside the editor.

## 9. The status bar

The status bar is a row of muted labels. Reading `documentLineCount` does not flatten the document into a single `Text`; word and character counts are computed from `documentText`:

```haskell
{{#include ../../../packages/nano-ui-demo/examples/Notepad.hs:status_bar}}
```

Reading `doc` here causes the status bar to repaint whenever the document changes, which is what we want so the counters stay current. If a counter's width fluctuates as digits are added, pin it with `fixedW` so adjacent status labels do not shift sideways (see [Recurring Bugs](../troubleshooting/recurring-bugs.md)).

## 10. Things to try next

- Persist a list of recently opened files across launches.
- Turn the About dialog into a draggable, resizable `floatingWindow`.
- Toggle word wrap through the editor's configuration.
- Replace the OS title bar with a custom one using `windowCaption` and `windowFrame` (see [Desktop Windows](../advanced/window-management.md)).

Next: [Tutorial 3: Live Data & Background Work](live-data.md) moves slow I/O onto worker threads and plots streamed values in real time.
