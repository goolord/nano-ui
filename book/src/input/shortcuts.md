# Keyboard Shortcuts

Keyboard commands are built from typed chords and read during the frame, the same way buttons are. Nothing is registered with the window; a shortcut is a function call that returns `True` on the frame its chord is pressed.

```haskell
import NanoUI.Shortcut

saveChord :: Shortcut
saveChord = cmdOrCtrl <> key 's'

saveCommand :: NanoUI ()
saveCommand = whenM (shortcut saveChord) saveDocument
```

`<>` unions the modifiers and keeps the right-hand key. `key` accepts a `Key` or a `Char`; letters are case-insensitive, and space maps to `KeySpace`:

```haskell
ctrl <> key 's'                    -- Ctrl+S
cmdOrCtrl <> shift <> key 'p'      -- Cmd+Shift+P on macOS, Ctrl+Shift+P elsewhere
alt <> key KeyEnter                -- Alt+Enter
key (KeyF 5)                       -- F5
```

Use `cmdOrCtrl` for application commands so they follow the platform's primary modifier. `ctrl`, `shift`, `alt`, and `super` are exact: `ctrl <> key 's'` does **not** match Ctrl+Shift+S.

## `shortcut` versus `shortcutOnce`

- **`shortcut`** fires on the frame the chord goes down, including auto-repeat. Holding Ctrl+Z repeats undo, which is usually what you want.
- **`shortcutOnce`** ignores auto-repeat. Use it for toggles and one-shot commands:

```haskell
whenM (shortcutOnce (cmdOrCtrl <> key 'b')) toggleBold
whenM (shortcut (cmdOrCtrl <> key 'q')) quitUi
```

Only the **first** matching `shortcut` call in a frame fires. It consumes that key's presses for the frame, so two bindings of the same chord never both run. Declaration order is priority order. A view that runs twice in one frame still matches the press once.

## What suppresses a shortcut

Shortcuts and key listeners are filtered automatically. A chord does not fire:

- behind an open modal;
- inside `disabledWhen True`;
- when the focused widget claims the key — a text field keeps typing, cursor, and editing chords such as Ctrl+A and Ctrl+Z; buttons keep Enter and Space; sliders, selects, and lists keep arrows, Home, End, Page Up, and Page Down;
- for Escape when `takeEscape` fails, or when an open overlay takes it first.

Focused widgets claim only their own unmodified or Shift-modified keys, so modified chords such as `Ctrl+Enter`, `Alt+Left`, or `Ctrl+S` still reach the application while a field is focused. This means the view almost never needs to ask what has focus.

The matching low-level functions are filtered the same way:

```haskell
whenM (keyPressed KeyEscape) clearSelection
whenM (keyPressedOnce KeyTab) nextPanel
```

`keyPressed` counts auto-repeats; `keyPressedOnce` does not. `keyReleased` and `keyHeld` complete the set. For raw, unfiltered input — a custom widget implementing its own editing — use `pressedIn` on the `Input` instead.

## Menus and shortcuts

`menuItemShortcut` shows a chord beside a menu row and binds it **while the menu is open**. To make a command work with the menu closed, bind the same chord outside the menu:

```haskell
-- In the File popup:
whenM (menuItemShortcut "Save" saveChord) saveDocument

-- Outside the popup, for when the menu is closed:
whenM (shortcut saveChord) saveDocument
```

The menu row is declared first, so while the menu is open its binding wins; the outside binding is ignored for that press. Do not duplicate the action — define it once and connect both triggers to it.

For a complete menu bar with these bindings, see [Build a Notepad](../tutorials/notepad.md).

## Escape has its own rules

Text fields do not claim Escape. That is deliberate: Escape should still close a popup, clear a selection, or leave a mode while the caret is in a field. It also means every Escape handler needs its own condition, independent of the "a field is editing" guard used for Ctrl+A or Delete.

`takeEscape` claims a press for the current handler, so a later handler in the same frame does not also act on it:

```haskell
-- Inside an editing row:
when isEditing (whenM takeEscape (writeVar editing False))

-- Later, at the application level:
whenM takeEscape clearFocus
```

The first call that succeeds wins. An open popup, dropdown, or text-edit menu reserves Escape before the view sees it.

## User-configurable chords

`parseShortcut` reads a chord from text, for a settings file or a keymap editor:

```haskell
case parseShortcut "C-S-s" of
  Right chord -> ...
  Left err    -> showError err
```

The syntax is modifier prefixes followed by a key: `C-` Ctrl, `S-` Shift, `A-` Alt/Option, `s-` Super, `M-` Command on macOS and Ctrl elsewhere. A key is one character as typed without Shift, or a named key in angle brackets: `C-s`, `M-S-p`, `A-<Enter>`, `<F5>`, `C--`, `C-S-=`. Name a symbol by its unshifted key, so Ctrl++ on a US layout is `C-S-=`.

`shortcutLabel` renders a chord for display in menus and tooltips:

```haskell
label (shortcutLabel saveChord)   -- "Ctrl+S", or "Cmd+S" on macOS
```

## Reading focus-dependent state

A shortcut's guard often wants to know what the user is doing — "clear the selection, unless a field has focus". The focus query belongs at the **end** of the view:

```haskell
appView = do
  -- ... draw the widgets, including the text fields ...

  -- Now focus state is known for this frame.
  searching <- isFocused searchFieldId
  when (not searching) $ do
    whenM (shortcut (key KeyDelete)) deleteSelected
    whenM (shortcut (key KeyEscape)) clearSelection
```

The node arena is reset before the view runs, so an `isFocused` call at the top of the view always answers `False`. This is the most common keyboard bug in nano-ui; it is covered in [Recurring Bugs & Fixes](../troubleshooting/recurring-bugs.md).

## Quitting and bare keys

The window's quit predicate is ordinary input filtering:

```haskell
runSdlApp defaultSdlOptions
  { sdlAppShouldQuit = pressedOnceIn KeyEscape }
  view
```

Ctrl+C closes the window like a close request *unless* something in the view uses it — a shortcut, a focused text field copying, or an open text-edit menu. A raw Ctrl+C that nothing handles is treated as a quit, so the application does not need to bind it.

For arrow-key navigation inside a custom widget, use `useKeyNav` or `focusedKeyPressed`; for editable text, use the built-in text widgets, which implement the standard editing commands and IME handling. The next chapter covers focus and IME.
