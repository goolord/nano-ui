# Widget Gallery

Every widget is an ordinary function in `NanoUI` that appends a node to the frame and reports what happened to it. This page is the index; each family has a chapter with signatures and examples.

## How widget names work

The same four names appear across the library:

| Form | Meaning |
| --- | --- |
| `button "Save"` | Base form. Returns `Bool` (activated) or `()` |
| `button'` | Primed form. Also — or instead — returns a `Response` |
| `buttonWith (fixedW 120)` | Takes a `Layout -> Layout` modifier first |
| `buttonConfigured cfg` | Takes a config record; every config has a `default...Config` |

`Response` is the shared record for widget identity and interaction:

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

Anything with a `Response` can be a `HasResponse` argument to `tooltip`, `contextMenu`, and focus helpers.

## The families

| Family | What is there | Chapter |
| --- | --- | --- |
| Containers | `row`, `column`, `grid`, `panel`, `card`, `callout`, `toolbar`, `center`, `responsive`, `scroll`, `scroll2D`, `layers`, `mouseArea`, `separator`, `spacer` | [Containers](../layout/containers.md) |
| Text | `label`, `heading`, `muted`, `mono`, `danger`, `kv`, `selectableText` | [Typography](../styling/typography.md) |
| Buttons and menus | `button`, `iconButton`, `buttonContent`, `menuButton`, `menuItem`, `menuSeparator`, `contextMenu` | [Buttons & Menus](buttons-menus.md) |
| Inputs | `checkbox`, `toggleButton`, `radio` / `enumRadio`, `select` / `enumSelect` / `boundedSelect`, `slider`, `textInput`, `searchInput`, `numericInput`, `comboBox`, `textArea`, `textAreaDocument`, `colorPicker` | [Input Controls](inputs.md) |
| Tabs | `tabs`, `tabBar`, `closableTab`, enum and bounded variants | [Workbenches](../styling/widget-surfaces.md) |
| Trees and tables | `tree`, `table`, `simpleTable` (Colonnade-based, sortable) | [Tables & Trees](tables-trees.md) |
| Overlays | `modal`, `window`, `popup`, `tooltip`, `contextMenu` | [Overlays & Windows](overlays.md) |
| Panes | `paneGrid` for dockable, splittable layouts | [Drag & Drop](drag-and-drop.md) |
| Drag and drop | `useDrag`, `useDrop`, `dropZone`, `useReorder`, `insertionIndex` | [Drag & Drop](drag-and-drop.md) |
| Rich text | `NanoUI.Widgets.RichText` spans and paragraphs | `NanoUI.Widgets.RichText` |
| Images and SVG | `image`, `imageConfigured`, `svgIcon`, `parseSvg`, image handles | `NanoUI` API |
| Custom controls | switches, knobs, progress bars, spinners, sparklines | [Custom Controls](custom-controls.md) |
| Canvas drawing | `customWidget`, `canvas`, paths, strokes, gradients | [Canvas Drawing](canvas-custom.md) |

The authoritative list is the export list in `packages/nano-ui/lib/NanoUI.hs`; if this book and that file disagree, the file wins. The [interactive workbenches](../styling/widget-surfaces.md) render many of these families live.

## Input widgets and `Var`

Input widgets take the `Var` they display and mutate it directly. There is no callback and no on-change handler:

```haskell
query <- useVar ""
textInput query

volume <- newVar 40
slider 0 100 volume

enabled <- useVar True
checkbox "Enabled" enabled
```

To react to an edit, use the primed form's `Response`:

```haskell
resp <- textInput' query
when (respSubmitted resp) (search =<< readVar query)
```

`searchInput'` debounces `respChanged` until typing pauses; `textInput'` reports every change. That difference matters for expensive work:

```haskell
resp <- searchInput' query
when (respChanged resp) refreshResults     -- once per pause, not per keystroke
```

## Layout is an argument

`row`, `column`, and friends take a layout; `rowWith` and `columnWith` take a `Layout -> Layout` modifier first. Modifiers compose with `(.)`, and the leftmost wins:

```haskell
columnWith (gap 8 . padAll 12 . fillW) $ do
  label "Settings"
  buttonWith (fixedW 120) "Apply"
```

Sizing comes from content by default. A label whose text changes length will move its siblings unless its width is pinned; see [Recurring Bugs & Fixes](../troubleshooting/recurring-bugs.md) for the fixes that came from real applications.

## Where to go next

- Build one: [Build a Todo List](../tutorials/todo-list.md) uses lists, keys, text input, filtering, and per-row state.
- Arrange them: [Layout](../layout/containers.md) and [Layout Modifiers](../layout/modifiers.md).
- Style them: [Theming & Surfaces](../styling/overview.md) and the [workbenches](../styling/widget-surfaces.md).
- Draw outside the box: [Custom Controls](custom-controls.md) and [Canvas Drawing](canvas-custom.md).
