# Interaction States

Widgets transition through four distinct interactive states based on pointer coordinates, clicks, and disabled status:

1. **Default**: Rest state. Uses `styleBg`, `styleFg`, and `styleBorder`.
2. **Hover**: Cursor is positioned over the widget's bounding geometry. Uses `styleHoverBg`.
3. **Pressed (Active)**: Primary mouse button is held down within the widget bounds. Uses `styleActiveBg`.
4. **Disabled**: Interaction is suppressed by `disabledWhen True`. Rendering is automatically dimmed, and events are blocked.

---

## State Transition Laboratory

The interactive specimen below demonstrates how the four states render across controls in both dark and light modes:

<nano-ui-state-lab></nano-ui-state-lab>

---

## Disabling Subtrees with `disabledWhen`

`disabledWhen` preserves layout dimensions and state while dimming visual output and intercepting events:

```haskell
disabledWhen isSaving $ do
  row $ do
    button "Cancel"
    button "Confirm"
```

Shortcuts and mouse events inside a disabled subtree are suppressed automatically.
