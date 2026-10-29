# Overlays & Windows

Modals, floating windows, and popups are ordinary `NanoUI` actions that take a `Var Bool` for their open state.

## Call overlays unconditionally

```haskell
modal  :: Var Bool -> Text -> NanoUI () -> NanoUI ()
window :: Var Bool -> Text -> NanoUI () -> NanoUI ()
popup  :: Var Bool -> PopupConfig -> NanoUI () -> NanoUI ()
```

> [!CAUTION]
> Always call an overlay on every frame. Wrapping it in `when isShown (modal ...)` shifts widget ids for all subsequent siblings when it opens or closes, which moves focus, carets, and per-row state. Overlays draw nothing while their `Var Bool` is `False`.

```haskell
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

Escape, a click outside, and the close button write `False` to the `Var`, so the dialog closes in the same frame. A modal blocks interaction with the content behind it and is drawn above it.

## Modals

`modal` renders a titled panel with a close button, centered in the window and limited to the window's bounds. The body is not scrolled automatically; wrap it in `scrollWith` for long content, and give the panel a size with `modalWith` when the body needs to fill it:

```haskell
modalWith (fixedWH 600 400) open "Settings" $
  columnWith (fillW . fillH) settingsBody
```

`modal'` returns `(Response, Maybe a)` and reports a close request as a click. When the open flag is derived from other state, pass `constVar` and write the real flag from the response:

```haskell
open <- useVar False
(resp, _) <- modal' (constVar open) "Important" $ do
  label "This dialog is shown while `open` is True."
  whenM (button "OK") (writeVar open False)
when (respClicked resp) (writeVar open False)
```

For a modal without a title bar or close button, use `modalPanel` / `modalPanel'`; the panel surface and backdrop remain, and Escape or an outside click still writes `False`.

## Floating windows

`window` is a draggable, resizable in-app window with a scrolling body. Other windows and the page remain interactive outside its bounds:

```haskell
inspectorOpen <- useVar False
window inspectorOpen "Inspector" $ do
  inspectorBody
```

`window'` returns the body result and a response. Use `window` for panels that coexist with the main content, `modal` when the user must answer first.

## Popups

`popup` is a floating panel placed by a `PopupConfig`:

```haskell
filterOpen <- useVar False
filterR <- button' "Filter"
when (respClicked filterR) (modifyVar filterOpen not)

popup filterOpen (defaultPopupConfig (AnchorRect (respRect filterR))) $ do
  checkbox "Fruit" showFruit
  checkbox "Vegetables" showVeg
```

A press on the anchor is not "outside", so the button that toggles the popup does not immediately reopen it.

Popup placement:

- `AnchorRect (respRect r)` anchors to a widget; `AnchorPoint pos` anchors to a coordinate.
- `cfgPlacement` chooses `PlacementAuto`, `PlacementBelow`, `PlacementAbove`, `PlacementAtCursor`, and the other placements.
- `cfgOffset` is the gap from the anchor.
- `cfgDismissable = False` disables Escape/outside-click closing; act on the response from `popup'` instead.

An anchored popup positions against the previous frame's `respRect`, so warm a frame before relying on it in tests. For long popup bodies, cap the height with `popupWith (maxH ...)` and scroll the body.

## Tooltips

Tooltips are stateless and attach to any widget with a `Response`:

```haskell
save <- button' "Save"
tooltip save "Write the file to disk"

more <- button' "More"
tooltipConfigured defaultTooltipConfig
  {tooltipPlacement = PlacementAtCursor} more "Follows the pointer"
```

The default delay is half a second; `TooltipConfig` sets the delay and placement. `withTooltip` attaches a rich tooltip widget instead of text.

## Rules

- Modals stack. Escape closes the innermost first; side-by-side modals close in declaration order.
- Keep the open flag at the level that owns the decision, not in a child that may not be drawn.
- Read focus after the overlay call.
- A `scrollArea`, `panel`, or `card` inside a dialog body draws a box in a box; strip its chrome (see [Double borders](../troubleshooting/recurring-bugs.md#2-double-borders--box-in-a-box)).

[Build a Notepad](../tutorials/notepad.md) wires a modal to the window's close request and builds a menu bar from `popup'`.
