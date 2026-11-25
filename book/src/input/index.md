# Keyboard, Focus & IME

Views check keyboard input inline during each frame rather than registering key listeners. Whenever a widget holds focus, it claims the keys it handles, and higher-level shortcuts and key queries automatically ignore claimed keys.

## Shortcuts

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:import}}
```

Combine modifiers and a key with `<>`. `key` accepts either a `Key` constructor or a `Char` (letters are case-insensitive, and `' '` maps to `KeySpace`):

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:chords}}
```

Prefer `cmdOrCtrl` for cross-platform application commands (Command on macOS, Ctrl elsewhere). All four modifier combinators (`ctrl`, `shift`, `alt`, and `super`) match strictly: `ctrl <> key 's'` will **not** trigger on Ctrl+Shift+S.

- `shortcut` fires on the initial press and on OS key repeats (useful for holding Ctrl+Z to repeat undo).
- `shortcutOnce` fires only on the initial press and ignores repeats (useful for toggles and dialogs).

Only the **first** matching `shortcut` call in a frame fires. Once it matches, that key press is consumed for the rest of the frame, so duplicate bindings never both run. Priority follows declaration order in your view.

## When shortcuts are suppressed

A shortcut will not fire:

- behind an open modal;
- inside a `disabledWhen True` subtree;
- when the focused widget claims the key (text fields claim typing, cursor movement, and editing chords like Ctrl+A/Ctrl+Z; buttons claim Enter and Space; sliders, selects, and lists claim arrows, Home, End, Page Up, and Page Down);
- on Escape when an open overlay already dismissed itself or an earlier `takeEscape` claimed it.

Because focused widgets only claim unmodified and Shift-modified navigation keys (plus their own editing chords), application shortcuts like `Ctrl+Enter`, `Alt+Left`, or `Ctrl+S` still work while typing in a text field. The view-level key queries (`keyPressed`, `keyPressedOnce`, `keyReleased`, `keyHeld`) apply the exact same filtering. Only drop down to raw `pressedIn` when writing a custom widget.

## Menus and shortcuts

`menuItemShortcut` prints the chord next to the menu item and activates it **only while the menu is open**. To make the shortcut available when the menu is closed too, declare a matching `shortcut` outside the menu:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:menu_and_shortcut}}
```

Because the menu row runs before the fallback binding, it handles the chord first whenever the menu is open.

## Handling Escape

Text fields deliberately leave Escape unclaimed so Escape can close popups, clear filters, or exit a mode even while the cursor is inside a field. For the same reason, do not gate Escape behind an "a field is focused" guard that you use for Ctrl+A or Delete.

Use `takeEscape` to claim Escape for the current handler:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:take_escape}}
```

The first call that runs takes the press. Open popups, dropdowns, and text context menus claim Escape before your view's handlers see it.

## User-configurable shortcuts

If your app reads keybindings from a config file or lets users customize shortcuts, parse chords from text with `parseShortcut`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:parse_shortcut}}
```

The format uses modifier prefixes followed by a key: `C-` (Ctrl), `S-` (Shift), `A-` (Alt/Option), `s-` (Super), and `M-` (Command on macOS, Ctrl elsewhere). Keys are written as an unshifted character or a named key in angle brackets: `C-s`, `M-S-p`, `A-<Enter>`, `<F5>`, `C--`, `C-S-=`. Always write symbols using their unshifted key (so Ctrl++ on a US keyboard is `C-S-=`).

Use `shortcutLabel` to format any parsed or constructed chord for menus and tooltips.

## Focus

- Clicking a text field, select, slider, or focusable custom widget focuses it; clicking empty background removes focus.
- Tab advances to the next focusable widget, and Shift+Tab moves backward.
- `requestFocus (respId resp)` moves focus programmatically, `focusNext` and `focusPrevious` step through the Tab order, and `clearFocus` clears focus at the end of the frame.
- `holdFocus wid` keeps focus locked onto a custom view (such as a code editor or terminal) and intercepts Tab so the widget can handle indentation itself.

Focus requests take effect **at the end of the frame** against that frame's solved layout, so the new focus state becomes visible on the next frame:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:request_focus}}
```

You can call `requestFocus` for a widget declared later in the same pass (for instance, by reading `currentId` right before calling it), but not for a widget that does not appear at all on the current pass. When a state change reveals a new field, save a `focusPending` flag and request focus on the pass that actually renders the field:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:delayed_focus}}
```

> [!WARNING]
> Always check focus at the **end** of your view. The node arena resets before the view runs, so calling `isFocused` at the top always returns `False`. Place focus-dependent keyboard shortcuts after the widgets have been declared.

Focus requests are ignored if the target widget is disabled, covered by an open modal, missing from the current pass, or not a Tab stop. Because inactive tab bodies do not run, widgets inside them lose focus when you switch away; call `requestFocus` again when switching back if needed.

`disabledWhen` dims a subtree and blocks focus and input without disturbing layout. Customize the focus outline color through `themeFocusRing`.

## Input Method Editors (IME)

Built-in text inputs and text areas support IME composition directly, rendering pre-edit text at the caret and suppressing shortcuts until composition commits.

If you build a custom widget that reads typed characters from `inputChars`, opt into text input on every frame it can accept text:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:input_method}}
```

`useInputMethod :: WidgetId -> InputPurpose -> Rect -> NanoUI (Maybe Composition)` tells the OS where to place the candidate popup and returns any active composition string for you to render. Pass the *caret* rectangle rather than the entire widget rectangle. `useInputMethod` checks focus internally, so you do not need a separate `isFocused` guard.

For low-level custom widgets, `Input` exposes `inputChars` (committed text), `inputKeys` (pressed keys including repeats), `inputKeysNew` (initial presses only), `inputKeysHeld`, `inputKeysReleased`, and `inputComposition`. See `packages/nano-ui-demo/examples/Terminal.hs` for a full custom widget using IME.

## Testing input headlessly

`NanoUI.Testing.Sim` scripts a view without opening a window. `runSim` settles the layout, then runs high-level user actions like clicking labels, typing strings, and pressing shortcuts across the exact frames real input would trigger:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Input.hs:sim}}
```

String targets match visible text labels. For unlabeled widgets or specific points, target with `At p`, `Within rect`, `On response`, or `OnWidget wid`. `check` and `checkEq` increment the `failed` counter on mismatches.

For lower-level frame tests, call `runFrame ctx input view` directly. Because layout geometry lags by one frame and focus updates apply at frame end, run `warmup2` before clicking a widget by its `respRect`, and build keyboard inputs with `keyInp` and `chordInp` from `NanoUI.Testing.Harness`.
