# Widget Gallery

This is the single place to start when you need a widget. Every widget is an ordinary function in `NanoUI`; the family pages below cover the API, and the styling pages cover how it looks.

## Find a widget by task

| You need… | Read |
| --- | --- |
| Buttons, menus, checkboxes, toggles, radios, selects, combo boxes, sliders, numeric fields, text fields, color pickers | [Buttons, Menus & Inputs](controls.md) |
| Multi-line text, documents, undo history, IME-aware editing | [Text Editing](text-editing.md) |
| Tabs, sortable tables, trees | [Tabs, Tables & Trees](data-views.md) |
| Modals, floating windows, popups, tooltips, context menus | [Overlays & Windows](overlays.md) |
| Drag-and-drop, tab reordering, dockable pane grids | [Drag & Drop](drag-and-drop.md) |
| Knobs, switches, progress bars, spinners, sparklines, canvas drawing | [Custom Drawing & Controls](custom-drawing.md) |
| Colors, themes, interaction states, fonts | [Styling & Theming](../styling/index.md) |
| Live examples of every styling modifier | [Interactive Styling Lab](../styling/lab.md) |
| Non-obvious styling behavior | [Styling Gotchas](../styling/gotchas.md) |

Containers (`row`, `column`, `grid`, `panel`, `card`, `layers`, ...) and sizing are covered once in [Layout & Scrolling](../layout/index.md).

## How widget names work

The same four forms appear across the library:

| Form | Meaning |
| --- | --- |
| `button "Save"` | Base form. Returns `Bool` (activated) or `()` |
| `button'` | Primed form. Returns a `Response` |
| `buttonWith (fixedW 120)` | Takes a `Layout -> Layout` modifier first |
| `buttonConfigured cfg` | Takes a config record; every config has a `default...Config` |

A `Response` carries identity and interaction state:

| Field / helper | Meaning |
| --- | --- |
| `respId` | The widget's `WidgetId` (focus, menus, scroll targets) |
| `respRect` | The widget's rectangle from the **previous** frame |
| `respHovered` | Pointer is over the widget |
| `respPressed` | Held down, not yet released |
| `respClicked` | Activated: left click, Enter, or Space |
| `respChanged` | The value was edited |
| `respSubmitted` | Committed, for example Enter in a text field |
| `respHeldWith` / `respClickedWith` | Which `MouseButton` is held / clicked |
| `respRightPressed` / `respRightClicked` | Secondary-button variants |

Anything with a `Response` can be passed to `tooltip`, `contextMenu`, and the focus helpers.

## The one input rule

Input widgets take the `Var` they display and mutate it directly; there is no callback and no on-change handler. To react to an edit, use the primed form's `Response`. The full set of controls and their examples is in [Buttons, Menus & Inputs](controls.md).

Containers and modifiers are not repeated here: see [Layout & Scrolling](../layout/index.md) for `row`, `column`, `grid`, padding, alignment, and scrolling.

## Reading order

If you are new to the toolkit, the shortest route through this section is:

1. [Buttons, Menus & Inputs](controls.md) — the controls most applications need.
2. [Text Editing](text-editing.md) — the moment a field becomes a document.
3. [Tabs, Tables & Trees](data-views.md) — showing collections.
4. [Overlays & Windows](overlays.md) — dialogs and menus that float.
5. [Drag & Drop](drag-and-drop.md) and [Custom Drawing & Controls](custom-drawing.md) when the built-in widgets are not enough.
6. [Styling & Theming](../styling/index.md), then the [lab](../styling/lab.md) and [gotchas](../styling/gotchas.md).

Every family page has a "See also" block back to this guide and across to the styling pages.
