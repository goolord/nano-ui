# Keyboard Navigation & Focus

Keyboard interaction in `nano-ui` covers shortcut chords, focus cycling, programmatic focus control, and IME composition.

## Keyboard Shortcuts

Define keyboard chords using `NanoUI.Shortcut`:

```haskell
-- Fires once per keystroke (ignores key repeat)
whenM (shortcutOnce (cmdOrCtrl <> key 's')) $ do
  saveDocument

-- Complex multi-modifier chords
whenM (shortcut (cmdOrCtrl <> shift <> key 'p')) $ do
  openCommandPalette
```

### Automatic Shortcut Suppression
Shortcuts and key queries are automatically suppressed when:
- An overlay/modal is open (unless the chord targets the modal).
- The widget is inside a `disabledWhen True` subtree.
- A focused widget claims the key (e.g. typing text in a field claims letters, arrows on a slider claim directions). Modified chords like `Ctrl+Enter` or `Alt+Left` still pass through.

---

## Focus Lifecycle

- **Click to Focus**: Clicking any focusable widget (text input, slider, select) focuses it.
- **Tab Cycling**: Pressing `Tab` moves focus to the next widget; `Shift+Tab` moves to the previous widget.
- **Programmatic Focus**:
  ```haskell
  resp <- textInput' myVar
  when shouldFocus $ requestFocus (respId resp)
  ```
- **Timing**: Focus takes effect at the **end of the frame**.

> [!WARNING]
> Because the node arena is reset before the view runs, checking focus at the *top* of a view always returns `False`. Query focus state at the **end** of the view, after the widgets have executed.

---

## IME (Input Method Editor) Composition

Built-in text fields handle IME composition automatically. For custom widgets that consume raw committed characters from `inputChars`, call `useInputMethod` on every focused frame to position the OS candidate window correctly:

```haskell
when (isFocused wid) $ do
  useInputMethod (respRect resp)
```
