# Layout Modifiers

Layout modifiers are functions of type `Layout -> Layout` composed with standard function composition `(.)`.

> [!IMPORTANT]
> **Precedence Rule: Leftmost Wins**
> When two modifiers configure the same property, the **leftmost modifier wins**:
> ```haskell
> -- fixedW 200 takes effect; fixedW 100 is ignored
> buttonWith (fixedW 200 . fixedW 100) "Submit"
> ```

## Modifier Reference

### Sizing and Expansion
- **`fillW` / `fillH`**: Expands to take up all available space in the parent container along the specified axis.
- **`grow`**: Shorthand for `fillW . fillH`.
- **`fixedW w` / `fixedH h` / `fixedWH w h`**: Sets explicit logical pixel dimensions.
- **`minW w` / `maxW w` / `minH h` / `maxH h`**: Constrains minimum and maximum dimensions.
- **`percent p`**: Sets size as a percentage (0 to 100) of the parent box.
- **`aspect r`**: Constrains the aspect ratio (width / height).

### Padding and Spacing
- **`padAll p`**: Insets all four sides by `p` logical pixels.
- **`padXY x y`**: Insets horizontally by `x` and vertically by `y`.
- **`padLeft` / `padRight` / `padTop` / `padBottom`**: Insets individual sides.
- **`padLRTB l r t b`**: Explicit padding for left, right, top, and bottom.
- **`gap g`**: Spacing between consecutive children in rows, columns, or grids.
- **`tight`**: Removes all internal padding while preserving inter-item gap.

### Alignment and Wrapping
- **`alignMid`**: Centers children perpendicular to the flow axis.
- **`alignStart` / `alignEnd`**: Aligns children to the start or end of the container.
- **`alignCenter`**: Centers along both axes.
- **`alignBaseline`**: Aligns text baselines across adjacent widgets.
- **`wrap`**: Wraps overflowing children onto subsequent rows/columns (customize with `lineGap` and `lineAlign`).

### Absolute Positioning and Overlays
- **`pinAt x y`**: Positions a child absolutely at `(x, y)` logical coordinates within its parent container.
- **`pointer PointerBlock`**: Consumes pointer events (preventing clicks from passing through).
- **`pointer PointerPass`**: Transparent to pointer clicks (allows interaction with underlying elements).
