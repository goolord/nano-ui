# Overlays & Windows

Modals, floating in-app windows, and popups are regular `NanoUI` actions controlled by a `Var Bool` that tracks whether they are open.

## Always call overlays unconditionally

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Signatures.hs:overlays}}
```

> [!CAUTION]
> Call every overlay on every frame, even when it is closed. Wrapping `modal`, `window`, or `popup` inside `when isShown` shifts the widget IDs of every sibling after it whenever it opens or closes, breaking focus, carets, and per-row state. Closed overlays draw nothing.

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Overlays.hs:modal}}
```

A modal draws above the page and blocks pointer and keyboard input behind it. Pressing Escape, clicking outside the dialog, or clicking its close button sets the `Var Bool` to `False`, closing the modal in the same frame.

## Modals

`modal` draws a centered panel with a title bar and close button, clamped to fit inside the viewport. Its body does not scroll automatically: wrap long content in `scrollWith`, and give the modal an explicit size with `modalWith` when the body needs to fill it:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Overlays.hs:modal_with}}
```

`modal'` returns `(Response, Maybe a)` and reports dismissal attempts through `respClicked`. When your open state is derived from another value (rather than stored directly in a `Var Bool`), pass `constVar` and update your real state from the response:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Overlays.hs:modal_derived}}
```

If you want a modal backdrop and panel without a title bar or close button, use `modalPanel` or `modalPanel'`. Pressing Escape or clicking outside still closes it.

## Floating windows

`window` creates a draggable, resizable floating panel with a scrollable body. Unlike a modal, the main UI and any other open floating windows remain interactive around it:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Overlays.hs:window}}
```

`window'` returns both the body's result and a `Response`. Use `window` for inspectors, palettes, and tool panels that sit alongside the main view; use `modal` when the user needs to finish an interaction before returning to the page.

## Popups

`popup` draws a floating panel positioned by a `PopupConfig`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Overlays.hs:popup}}
```

Clicks on the anchor rectangle count as clicks inside the popup, so clicking the trigger button to dismiss a popup will not immediately pop it back open on the same press.

Key placement options:

- `AnchorRect (respRect r)` anchors the popup to a widget; `AnchorPoint pos` anchors it to a specific coordinate.
- `cfgPlacement` chooses where the popup opens (`PlacementAuto`, `PlacementBelow`, `PlacementAbove`, `PlacementAtCursor`, and others).
- `cfgOffset` sets the pixel gap between the anchor and the popup.
- `cfgDismissable = False` prevents automatic closing on Escape or outside clicks so you can handle dismissal explicitly via `popup'`.

Because anchored popups read `respRect` (which comes from the previous frame), warm up one frame in headless tests before clicking to open them. For tall popup menus, cap the height with `popupWith (maxH ...)` and place a scroller inside.

## Tooltips

Tooltips require no state `Var` and attach directly to any widget's `Response`:

```haskell
{{#include ../../../packages/nano-ui-demo/book/Book/Overlays.hs:tooltips}}
```

By default, tooltips appear after hovering for 0.5 seconds. Pass a `TooltipConfig` to `tooltipConfigured` to change the delay or placement, or use `withTooltip` to render a custom widget subtree inside the tooltip instead of plain text.

## Keep in mind

- **Modals stack cleanly.** Escape dismisses the innermost modal first; sibling modals close in declaration order.
- **Keep the open `Var` alive outside conditional branches.** Allocate or read it in a scope that runs even while the overlay is hidden.
- **Check focus after overlay calls** so focus queries see widgets declared inside open overlays.
- **Avoid nested borders.** Putting a `scrollArea`, `panel`, or `card` inside a modal or popup draws a box inside a box; strip the inner chrome (see [Double borders](../troubleshooting/recurring-bugs.md#2-double-borders--box-in-a-box)).

For a complete example connecting a menu bar of `popup'` panels and a close-confirmation `modal`, see [Tutorial 2: Build a Notepad](../tutorials/notepad.md).
