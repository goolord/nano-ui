# Containers

`nano-ui` provides a rich set of layout containers that arrange child nodes into rows, columns, grids, layered stacks, and responsive panes.

## Container Inventory

| Container | Description |
| --- | --- |
| `row` / `rowWith` | Lays out children horizontally in a flexible flow. |
| `column` / `columnWith` | Lays out children vertically from top to bottom. |
| `grid` / `gridWith` | Arranges children into rows and columns with `gridCols n`. |
| `panel` / `panelWith` | Renders a styled surface container with background, border, and padding. |
| `card` / `cardWith` | Elevated panel with distinct background and rounded corners. |
| `callout` / `calloutWith` | Informational or alert panel with semantic tone styling. |
| `layers` | Stacks children on top of each other in call order (last on top), aligning within the shared bounding box. |
| `center` | Centers child content both horizontally and vertically. |
| `responsive` | Adapts layout dynamically based on available width. |
| `mouseArea` | Wraps a subtree to report hover, clicks, and drag events across the entire region. |
| `separator` | Horizontal or vertical divider line styled by `themeSeparator`. |
| `spacer` | Flexible space absorber that expands to fill available room. |

---

## Examples

### Horizontal Toolbar with Alignments

```haskell
rowWith (gap 8 . padAll 8 . fillW . alignMid) $ do
  button "Back"
  button "Forward"
  spacer  -- Pushes search input to the right
  textInput searchVar
```

### Layered Scrim / Modal Backdrop

```haskell
layers $ do
  mainContent
  
  -- Dimming scrim positioned across the entire parent
  panelWith (pinAt 0 0 . grow . pointer PointerBlock) $ do
    modalDialog
```
