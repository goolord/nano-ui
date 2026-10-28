# Overlays, Windows & Modals

Overlays in `nano-ui` include modal dialogs, draggable floating windows, popups, and hover tooltips.

## The Cardinal Rule: Call Overlays Unconditionally

`modal`, `window`, and `popup` take a `Var Bool` representing their open/closed state:

```haskell
modal  :: Var Bool -> Text -> NanoUI () -> NanoUI ()
window :: Var Bool -> Text -> NanoUI () -> NanoUI ()
popup  :: Var Bool -> PopupConfig -> NanoUI () -> NanoUI ()
```

> [!CAUTION]
> **Always call overlays unconditionally every frame.**
> Wrapping a modal call in `when isShown (modal ...)` shifts widget IDs for all subsequent siblings when the modal opens or closes! Overlays draw nothing when their `Var Bool` is `False`.

```haskell
-- CORRECT: Called unconditionally
showConfirm <- useVar False

whenM (button "Delete File...") (writeVar showConfirm True)

modal showConfirm "Confirm Deletion" $ do
  label "Are you sure you want to permanently delete this file?"
  row $ do
    whenM (button "Cancel") (writeVar showConfirm False)
    whenM (buttonWith destructive "Delete") $ do
      deleteFile
      writeVar showConfirm False
```

### Auto-Dismiss Behavior
By default, clicking outside an overlay, pressing the `Escape` key, or clicking the close button writes `False` to the `Var Bool`.
For dialogs that require explicit confirmation or derived open states, pass `constVar True` and use `modal'` with `respClicked`.

---

## Tooltips

Tooltips are stateless and anchor to any widget `Response`:

```haskell
resp <- button' "Refresh"
tooltip resp "Poll the remote server for updates"
```
Customize placement and delay with `tooltipConfigured`.
