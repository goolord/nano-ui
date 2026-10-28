# Custom Drawing & Controls

When the built-in widgets do not fit, draw the control yourself with the 2D canvas API.

The widgets in `NanoUI.Widgets.Custom` (`knob`, `toggleSwitch`, `circularProgress`, `progressBar`, `spinner`, `sparkline`) are designed to serve as **reference implementations** for custom widgets users may want to implement themselves. This API is meant to **accompany the canvas API**, demonstrating how 2D vector drawing, layout sizing, gesture hooks (`useDrag2DOn`, `useWheelDeltaOn`), keyboard navigation, and responsive state binding (`bindVar`) integrate into cohesive, interactive controls.

## Reference custom controls

```haskell
import NanoUI.Widgets.Custom
```

| Control | Signature | Default size | Notes |
| --- | --- | --- | --- |
| `knob` | `Float -> Float -> Var Float -> NanoUI ()` | 36×36 | Min, max, value; vertical drag, wheel, arrows |
| `toggleSwitch` | `Var Bool -> NanoUI ()` | 44×24 | Click, Space, or Enter toggles |
| `circularProgress` | `Float -> NanoUI ()` | 32×32 | Fraction in `[0, 1]` |
| `progressBar` | `Float -> NanoUI ()` | fills width, 12 high | Fraction in `[0, 1]` |
| `spinner` | `NanoUI ()` | 18×18 | Indeterminate; requests animation frames while declared |
| `sparkline` | `[Float] -> NanoUI ()` | 80×24 | Scales to the values' range |

Each has a primed form returning `Response` (`knob'`, `toggleSwitch'`, ...) and a `...With` form that takes a layout modifier and size parameters:

```haskell
knobWith alignMid 48 0 100 volume
progressBarWith fillW 6 progress
sparklineWith (fixedWH 120 32) 120 32 samples
```

A settings row:

```haskell
settingsRow :: Var Bool -> Var Float -> NanoUI ()
settingsRow enabled volume = rowWith (gap 12 . alignMid) $ do
  toggleSwitch enabled
  labelWith (alignMid . fixedW 64) "Enabled"
  knob 0 100 volume
  v <- readVar volume
  labelWith (alignMid . fontMono . fixedW 48) (T.pack (show (round v :: Int)))
```

The `fixedW` on the changing number keeps the controls from shifting. Read the value *below* the knob so a drag resolves in one pass.

`spinner` calls `keepAnimating` internally while it is drawn. Do not add a global `keepAnimating`; that would keep the loop running when the spinner is not visible.

### Implementing custom controls

You can inspect the source code of `NanoUI.Widgets.Custom` as a blueprint for implementing your own controls. A typical interactive control follows this pattern:

1. **Allocate widget identity**: `(wid, ctx) <- freshWidget` obtains a unique `WidgetId`.
2. **Define geometry & drawing**: Specify layout constraints and vector draw ops via `CustomWidgetSpec` and `runCanvasFor`.
3. **Capture gestures & input**: Use gesture hooks (`drag <- useDrag2DOn resp`, `(_, scrollY) <- useWheelDeltaOn resp`, `nav <- useKeyNav wid`) to compute value deltas.
4. **Synchronize state**: Apply `bindVar` or `finishInput` to bind the pure calculation to the caller's mutable `Var`.

## Canvas drawing

`customWidget` (a `CustomWidgetSpec` with layout, draw, cursor, and content key) or `canvas` / `canvasConfigured` draws with the 2D canvas API:

```haskell
import NanoUI.Path qualified as P

badge :: NanoUI ()
badge = canvas (fixedWH 120 40) $ \_rect -> do
  drawRoundedRect (Rect 0 0 120 40) 8 (colorRGBA 37 99 235 255)
  drawStrokePath (P.circle (V2 20 20) 8) 2 colorWhite
  drawText (V2 68 20) AlignCenter AlignMiddle "Ready" colorWhite
```

Outside a canvas, use `customWidget` with a `CustomWidgetSpec`:

```haskell
(resp, ()) <- customWidget defaultCustomWidgetSpec
  { widgetLayout = fixedWH 48 48
  , widgetContent = contentKey [angle, volume]
  , widgetDraw = \cdc (Rect x y w h) -> runCanvasFor cdc $ do
      drawCircle (V2 (x + w / 2) (y + h / 2)) 16 (themeAccent (cdcTheme cdc))
  , widgetCursor = Just (\_ _ _ -> UiCursorPointer)
  }
```

`runCanvasFor` adapts tessellation to the display scale inside `widgetDraw`. `drawContext` / `cdcTheme` supply the active theme, font, hover, and press state.

### Paths, strokes, and text

- `P.moveTo`, `P.lineTo`, `P.cubicTo`, `P.arc`, `P.circle` build paths.
- `drawPath` / `drawPathWith` fill them; `P.NonZero` or `P.EvenOdd` selects the fill rule, `P.Solid` or `P.Linear` the paint.
- `drawStrokePathWith` takes a `P.Stroke` record (width, caps, joins, dash).
- `withTransform` (translate/rotate/scale) and `withClip` scope drawing.
- `drawText pos alignX alignY txt color`; measure with `lineWidth` and `uiFontMetrics`.
- `drawRoundedRect` and `drawStrokeRoundedRect` draw a bordered box; the stroke is inset inside the bounds, unlike a centred path stroke.

### Content keys and caching

Without `widgetContent`, a custom widget's draw ops are rebuilt and diffed every frame. With a key, unchanged frames skip rebuild and repaint, but the key must cover **everything the drawing reads**:

```haskell
(resp, ()) <- customWidget defaultCustomWidgetSpec
  { widgetLayout = fixedWH 48 48
  , widgetContent = contentKey [angle, volume, fromIntegral (round scrollY)]
  , widgetDraw = \cdc rect -> runCanvasFor cdc (drawGauge rect)
  }
```

A key that misses an input shows stale pixels. See [Styling Gotchas](../styling/gotchas.md).

A drawing whose layout rectangle never changes (a drag ghost pinned under the pointer with `pinAt`) gets no layout change when it moves. Give it an explicit version that includes what moves it:

```haskell
-- dragGhostOps :: Rect -> SmallArray DrawOp
drawingVersioned (contentKey [mx, my]) (fixedWH 32 32) dragGhostOps
```

Otherwise it keeps the ops of the frame it first appeared in.

## SVG & Vector Graphics

`nano-ui` includes vector SVG support at both the widget level and the canvas drawing level:

- **SVG icons**: Parse in-memory SVG strings (`parseSvg`) or load SVG files (`loadSvg`), then display them with `svgIcon` or in buttons.
- **Canvas SVG textures**: Rasterize SVGs to RGBA buffers with `rasterizeSvg` from `NanoUI.Svg` and draw them onto a canvas using `drawImage`.
- **Canvas to SVG export**: Export `DrawOp` sequences directly to SVG vector files using `nano-ui-svg` (`drawOpsToSvg`).

### 1. SVG Icons in Views

Monochrome SVG icons use `currentColor` and automatically inherit the active text color. This means they can be tinted with `fontColor` or semantic styles:

```haskell
import qualified Data.ByteString.Char8 as BS8
import NanoUI

-- Parse an inline SVG icon (e.g. 24x24 viewBox)
starIcon :: Svg
starIcon = case parseSvg "<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'><polygon points='12,2 15,9 22,9.5 16.5,14.5 18.5,21.5 12,17.5 5.5,21.5 7.5,14.5 2,9.5 9,9'/></svg>" of
  Right svg -> svg
  Left err  -> error ("Invalid SVG: " ++ err)

-- Render SVG icons with various sizes and theme colors
iconBar :: Svg -> NanoUI ()
iconBar icon = rowWith (gap 12 . alignMid) $ do
  theme <- uiTheme

  -- 1. Standard icon sizing (24x24 logical pixels)
  svgIcon 24 icon

  -- 2. Scoped styling and tinting via fontColor
  svgIconWith (fixedWH 32 32 . fontColor (themeAccent theme)) icon

  -- 3. In an icon button
  whenM (iconButton icon "Star") $
    putStrLn "Starred!"

  -- 4. Custom button content
  whenM (buttonContent $ rowWith (gap 6 . alignMid) $ do
            svgIcon 16 icon
            label "Favorite") $
    putStrLn "Favorited!"
```

Multi-color SVGs preserve their embedded fill and stroke colors. For flexible image-style scaling, `svgIconConfigured` accepts `ImageConfig` with CSS-like fitting (`FitContain`, `FitCover`, `FitFill`).

### 2. Drawing SVG inside Canvas

Within `CanvasM`, you can draw rasterized textures with `drawImage` / `drawImageUV` / `drawImageWith`. To draw an `Svg` onto a canvas, rasterize it to RGBA bytes using `rasterizeSvg` (or `rasterizeSvgIn`) from `NanoUI.Svg`:

```haskell
import NanoUI
import NanoUI.Svg (rasterizeSvg)
import NanoUI.Widgets.Custom (CanvasM, canvas, drawImage, drawRoundedRect, drawStrokeRoundedRect, drawText)

-- | A custom badge widget that draws an SVG icon alongside custom vector geometry.
customBadge :: ImageHandle () -> Svg -> Text -> NanoUI ()
customBadge imgHandle svg title = do
  theme <- uiTheme
  let iconPx = 32
      -- Rasterize the SVG to RGBA at target pixel dimensions with a tint color
      rgba = rasterizeSvg iconPx iconPx (themeAccent theme) svg

  -- Lease the rasterized texture in the backend atlas
  mIid <- useImageRgba imgHandle () iconPx iconPx rgba

  canvas (fixedWH 140 44) $ \(Rect x y w h) -> do
    -- Draw container geometry
    drawRoundedRect (Rect x y w h) 8 (styleBg (themeButton theme))
    drawStrokeRoundedRect (Rect x y w h) 8 1 (styleBorder (themeButton theme))

    -- Draw the rasterized SVG icon if registered
    case mIid of
      Just iid -> drawImage (Rect (x + 8) (y + 6) 32 32) iid
      Nothing  -> pure ()

    -- Draw accompanying text
    drawText (V2 (x + 48) (y + 22)) AlignStart AlignMiddle title (styleFg (themeButton theme))
```

### 3. Exporting Canvas Drawings to SVG (`nano-ui-svg`)

When you need vector screenshots, reports, or test fixtures, `nano-ui-svg` can export any `DrawOp` array generated by `runCanvas` or `runCanvasFor` directly to an SVG string:

```haskell
import qualified Data.Text.IO as TIO
import NanoUI
import NanoUI.Draw.Svg (defaultSvgConfig, drawOpsToSvg, renderDrawOpsSvg, SvgConfig (..))
import NanoUI.Path qualified as P
import NanoUI.Widgets.Custom

-- | Draw a vector gauge and export it to an SVG file.
exportGaugeSvg :: FilePath -> Float -> IO ()
exportGaugeSvg filePath value = do
  let bounds = Rect 0 0 160 50
      ops = runCanvas bounds $ do
        -- Outer background
        drawRoundedRect bounds 10 (colorRGBA 30 41 59 255)
        drawStrokeRoundedRect bounds 10 1.5 (colorRGBA 56 189 248 255)

        -- Progress bar path
        let barW = 140 * clamp 0 1 value
        drawRoundedRect (Rect 10 32 barW 8) 4 (colorRGBA 56 189 248 255)

        -- Indicator circle & text
        drawStrokePath (P.circle (V2 24 18) 8) 2 colorWhite
        drawText (V2 42 18) AlignStart AlignMiddle "CPU Load" colorWhite

  -- Export directly as a standalone SVG document:
  TIO.writeFile filePath (drawOpsToSvg bounds ops)
```

To embed the output inside an existing HTML/SVG document, pass `svgStandalone = False`:

```haskell
-- Emits an inner <g> fragment instead of a full <svg> document
let fragment = renderDrawOpsSvg defaultSvgConfig { svgStandalone = False } bounds ops
```

For whole-window or view-level SVG captures, see `NanoUI.Draw.FrameSvg` in `nano-ui-svg`.

## See also

- Widget inventory: [Widget Gallery](index.md).
- Drawing styles, colors, and themes: [Styling & Theming](../styling/index.md).
- SVG export of canvas drawings: [Ecosystem Packages](../packages/index.md).

