# Custom Drawing & Controls

When the built-in widgets do not fit, draw the control yourself. `NanoUI.Widgets.Custom` already provides the common canvas-drawn controls, and the canvas API handles everything else.

## Ready-made custom controls

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

## Canvas drawing

`customWidget` (a `CustomWidgetSpec` with layout, draw, cursor, and content key) or `canvas` / `canvasConfigured` draws with the 2D canvas API:

```haskell
import NanoUI.Path qualified as P

badge :: NanoUI ()
badge = canvas (fixedWH 120 40) $ \_dc -> do
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

Without `widgetContent`, a custom widget's draw ops are rebuilt and diffed every frame. With a key, unchanged frames skip rebuild and repaint — but the key must cover **everything the drawing reads**:

```haskell
(resp, ()) <- customWidget defaultCustomWidgetSpec
  { widgetLayout = fixedWH 48 48
  , widgetContent = contentKey [angle, volume, fromIntegral (round scrollY)]
  , widgetDraw = \cdc rect -> runCanvasFor cdc (drawGauge rect)
  }
```

A key that misses an input shows stale pixels. This is the most common custom-drawing bug; see [Styling Gotchas](../styling/gotchas.md).

A drawing whose layout rectangle never changes — a drag ghost pinned under the pointer with `pinAt` — gets no layout change when it moves. Give it an explicit version that includes what moves it:

```haskell
-- dragGhostOps :: Rect -> SmallArray DrawOp
drawingVersioned (contentKey [mx, my]) (fixedWH 32 32) dragGhostOps
```

Otherwise it keeps the ops of the frame it first appeared in.

## See also

- Widget inventory: [Widget Gallery](index.md).
- Drawing styles, colors, and themes: [Styling & Theming](../styling/index.md).
- SVG export of canvas drawings: [Ecosystem Packages](../packages/index.md).
