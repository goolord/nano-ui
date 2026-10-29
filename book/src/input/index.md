# Keyboard, Focus & IME

Keyboard input is read during the frame, not registered with the window. A focused widget claims the keys it uses; shortcuts and key listeners are filtered automatically.

## Shortcuts

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:import}}
```

`<>` unions modifiers and keeps the right-hand key. `key` accepts a `Key` or a `Char`; letters are case-insensitive, and space maps to `KeySpace`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:chords}}
```

Use `cmdOrCtrl` for application commands. `ctrl`, `shift`, `alt`, and `super` are exact: `ctrl <> key 's'` does **not** match Ctrl+Shift+S.

- **`shortcut`** fires on the frame the chord goes down, including auto-repeat (holding Ctrl+Z repeats undo).
- **`shortcutOnce`** ignores auto-repeat. Use it for toggles and one-shot commands.

Only the **first** matching `shortcut` call in a frame fires. It consumes that key's presses for the frame, so two bindings of the same chord never both run. Declaration order is priority order.

## What suppresses a shortcut

A chord does not fire:

- behind an open modal;
- inside `disabledWhen True`;
- when the focused widget claims the key: a text field keeps typing, cursor, and editing chords such as Ctrl+A and Ctrl+Z; buttons keep Enter and Space; sliders, selects, and lists keep arrows, Home, End, Page Up, and Page Down;
- for Escape when `takeEscape` fails, or when an open overlay takes it first.

Focused widgets claim only their own unmodified or Shift-modified keys, so modified chords such as `Ctrl+Enter`, `Alt+Left`, or `Ctrl+S` still reach the application while a field is focused. The matching low-level functions are filtered the same way: `keyPressed`, `keyPressedOnce`, `keyReleased`, and `keyHeld`. Use raw `pressedIn` only when implementing a custom widget.

## Menus and shortcuts

`menuItemShortcut` shows a chord beside a menu row and binds it **while the menu is open**. Bind the same chord outside the menu for when it is closed:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:menu_and_shortcut}}
```

The open menu's row is declared first, so it wins.

## Escape

Text fields do not claim Escape, so Escape can still close a popup, clear a selection, or leave a mode while the caret is in a field. That also means every Escape handler needs its own condition. Do not share the “a field is editing” guard used for Ctrl+A or Delete.

`takeEscape` claims a press for the current handler:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:take_escape}}
```

The first call that succeeds wins. An open popup, dropdown, or text-edit menu reserves Escape before the view sees it.

## User-configurable chords

`parseShortcut` reads a chord from text, for a settings file or keymap editor:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:parse_shortcut}}
```

The syntax is modifier prefixes followed by a key: `C-` Ctrl, `S-` Shift, `A-` Alt/Option, `s-` Super, `M-` Command on macOS and Ctrl elsewhere. A key is one character as typed without Shift, or a named key in angle brackets: `C-s`, `M-S-p`, `A-<Enter>`, `<F5>`, `C--`, `C-S-=`. Name a symbol by its unshifted key, so Ctrl++ on a US layout is `C-S-=`.

`shortcutLabel` renders a chord for menus and tooltips.

## Focus

- **Click** a text field, select, slider, or focusable custom widget to focus it. Clicking elsewhere unfocuses the current field.
- **Tab** moves to the next focusable widget; **Shift+Tab** to the previous.
- **`requestFocus (respId resp)`** focuses a widget. **`focusNext` / `focusPrevious`** behave like Tab. **`clearFocus`** drops focus at the end of the frame.
- **`holdFocus wid`** claims focus every frame for a view that handles input itself; it also consumes Tab so an editor can indent with it.

Focus requests are applied **at the end of the frame**, against that frame's layout. A focus move is visible on the next frame:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:request_focus}}
```

A request can name a widget declared later in the same pass (for example with `currentId` just before it), but not one that is not declared on this pass at all. A widget that appears after a state change needs a delayed request:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:delayed_focus}}
```

> [!WARNING]
> Query focus at the **end** of the view. The node arena is reset before the view runs, so `isFocused` at the top always answers `False`. Keyboard shortcuts that depend on focus belong after the widgets have run.

A focus request is ignored for a disabled widget, a widget behind an open modal, one not declared this pass, or a group response that is not itself a Tab stop. A widget in an inactive tab is not declared, so it cannot keep focus; request focus again when the tab is shown.

`disabledWhen` dims a subtree and blocks interaction without changing layout or state. The focus ring is themed by `themeFocusRing`.

## IME

Input method editors compose text before committing it. Built-in text fields handle the whole cycle, including caret rendering and suppressing shortcuts mid-composition.

A custom widget that reads committed characters from `inputChars` must opt in every frame it accepts text:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:input_method}}
```

`useInputMethod :: WidgetId -> InputPurpose -> Rect -> NanoUI (Maybe Composition)` places the candidate window at the caret and returns the in-progress composition to draw. Pass the caret rectangle, not the whole widget. It checks focus itself, so a separate `isFocused` guard is unnecessary.

Raw input fields: `inputChars` (committed text), `inputKeys` (pressed, including repeats), `inputKeysNew` (initial presses), `inputKeysHeld`, `inputKeysReleased`, and `inputComposition`. The terminal demo in `packages/nano-ui-demo/examples/Terminal.hs` is a complete custom text widget.

## Testing

`NanoUI.Testing.Sim` drives a view headlessly the way a user would. `runSim` settles the view, then the script clicks text, types, and presses keys, each action running the frames a real event would:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:sim}}
```

A string literal aims at the visible text with that label. `At p`, `Within rect`, `On response` and `OnWidget wid` aim elsewhere. `check` and `checkEq` count failures on `failed`.

For lower-level tests, `runFrame ctx input view` runs one frame. Focus changes apply on the next frame, and geometry lags one frame, so warm up with `warmup2` before targeting a widget by `respRect`. `keyInp` / `chordInp` from `NanoUI.Testing.Harness` build key input.
