# Custom Controls

Some controls have no platform widget behind them: a rotary knob, a toggle switch, a progress ring. They are drawn with the canvas primitives in `NanoUI.Widgets.Custom`, behave like built-in widgets (focus, hover, press, theme), and return the same `Response` from their primed forms.

```haskell
import NanoUI.Widgets.Custom
```

| Control | Signature | Default size | Notes |
| --- | --- | --- | --- |
| `knob` | `Float -> Float -> Var Float -> NanoUI ()` | 36×36 | Min, max, value. Vertical drag, wheel, arrow keys. |
| `toggleSwitch` | `Var Bool -> NanoUI ()` | 44×24 | Click, Space, or Enter toggles. |
| `circularProgress` | `Float -> NanoUI ()` | 32×32 | Fraction in `[0, 1]`; rings on the accent color. |
| `progressBar` | `Float -> NanoUI ()` | fills width, 12 high | Fraction in `[0, 1]`; minimum content width 120. |
| `spinner` | `NanoUI ()` | 18×18 | Indeterminate; requests animation frames while declared. |
| `sparkline` | `[Float] -> NanoUI ()` | 80×24 | Scales to the values' range. |

Every control follows the naming conventions:

- **Primed** (`knob'`, `toggleSwitch'`, `circularProgress'`, `progressBar'`, `spinner'`, `sparkline'`) returns `Response`.
- **`...With`** takes a `Layout -> Layout` modifier and the control's size parameters first: `knobWith (alignMid) 48 0 100 volume`, `progressBarWith fillW 6 progress`, `sparklineWith (fixedWH 120 32) 120 32 samples`.

## A settings row

```haskell
settingsRow :: Var Bool -> Var Float -> NanoUI ()
settingsRow enabled volume = rowWith (gap 12 . alignMid) $ do
  toggleSwitch enabled
  labelWith (alignMid . fixedW 64) "Enabled"
  knob 0 100 volume
  v <- readVar volume
  labelWith (alignMid . fontMono . fixedW 48) (T.pack (show (round v :: Int)))
```

The `fixedW` on the changing label keeps the controls from shifting as the number's width changes. The knob reads its `Var` directly; the label reads it *below* the knob, so a drag resolves in the same pass.

## Progress and busy states

`progressBar` and `circularProgress` take a fraction, not a percentage:

```haskell
progressSection :: Float -> Bool -> NanoUI ()
progressSection frac running = columnWith (gap 8) $ do
  rowWith (gap 12 . alignMid) $ do
    -- The spinner comes and goes; the label keeps its id either way.
    scope (when running spinner)
    label (if running then "Working..." else "Idle")
  progressBar frac
  muted (T.pack (show (round (frac * 100) :: Int)) <> "%")
  circularProgressWith (fixedWH 40 40) 40 frac
```

`spinner` calls `keepAnimating` internally for as long as it is drawn. Do not add a global `keepAnimating`; that would keep the frame loop running when the spinner is not visible.

## Sparklines in a table

`sparkline` is a pure drawing of a list of values, which makes it useful in dashboards and table cells:

```haskell
cpuCell :: [Float] -> NanoUI ()
cpuCell samples = rowWith (gap 8 . alignMid) $ do
  sparklineWith (fixedWH 80 24) 80 24 samples
  let latest = if null samples then 0 else last samples
  labelWith (fontMono . fixedW 40) (T.pack (show (round latest :: Int)) <> "%")
```

The sparkline scales each list to its own min and max, so a flat series draws a flat line rather than a zero baseline. It does not smooth or resample; feed it the samples you want drawn.

## Customizing further

These controls are reference implementations of `customWidget`: each has a `CustomWidgetSpec` with a layout, a draw function, a cursor, and a content key. When the keyed content changes, only that widget repaints.

To build your own control, read [Canvas Drawing](canvas-custom.md). The important rules carry over:

- **Include everything the drawing reads in the content key** — values, colors, text, and animation state. A key that misses an input shows stale pixels.
- **Pin moving overlays with a version** when their layout rectangle does not change (a drag ghost pinned at the cursor).
- **Use the theme** (`themeAccent`, `themeButton`, `themeMuted`) rather than literal colors, so the control follows light/dark and per-subtree styling.

The [workbenches](../styling/widget-surfaces.md) render these custom controls with every styling modifier applied, which is the fastest way to see what `buttonStyle` or `accentColor` does to them.
