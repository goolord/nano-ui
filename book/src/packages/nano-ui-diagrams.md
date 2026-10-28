# Charts & Diagrams (`nano-ui-diagrams`)

`nano-ui-diagrams` brings two things into a nano-ui layout:

- **`NanoUI.Plot`** — line, bar, scatter, area, and step charts with axes, grid, legend, hover readout, and decimation.
- **`NanoUI.Diagrams`** — the diagrams-lib backend and widget, for arbitrary vector drawings.

```haskell
import NanoUI
import NanoUI.Plot
```

## A first chart

```haskell
data App = App {appPlots :: PlotCache}

newApp :: IO App
newApp = App <$> newPlotCache

cpuChart :: App -> Seq (Double, Double) -> NanoUI ()
cpuChart app samples =
  void $ plot (appPlots app) (fillW . fixedH 220) $
    withGrid GridBoth
      . withXAxis "time (s)"
      . withYAxis "CPU %"
      . withLegend LegendNone
      $ chart [line "cpu" samples]
```

`plot` takes the cache, a layout modifier, and the `Chart`, and returns a `PlotResponse`. The cache is what makes it cheap: the `Chart` value can be rebuilt every frame, but the diagram behind it is rebuilt only when the chart or theme actually changes.

`lineChart`, `barChart`, `scatterChart`, and `areaChart` are the one-series shortcuts:

```haskell
resp <- lineChart cache (fillW . fixedH 200) samples
```

## Series

```haskell
line    "cpu"   points     -- (x, y) pairs, connected in input order
scatter "load"  points
area    "mem"   points     -- filled to a baseline
step    "state" points     -- horizontal/vertical steps
bar     "disk"  [(label, value)]
```

Style modifiers compose on a series (`&` is from `Diagrams.Prelude`):

```haskell
import Diagrams.Prelude ((&))

chart
  [ line "cpu" cpuPoints
      & withColor (colorHex "#38bdf8")
      & withStrokeWidth 2
      & withMarker MarkCircle
  , area "memory" memPoints
  ]
```

Charts have a `Domain` per axis; `emptyChart` defaults to the data's extent. `withTitle`, `withXAxis`, `withYAxis`, `withGrid`, `withLegend`, and `withDecimate` decorate the chart. `addSeries` appends a series while preserving order.

Decimation is on by default for line, area, and step series; it thins the drawing and hit testing without changing the stored data. Scatter series and marked lines are thinned only past 2,000 points.

## Hover and live readouts

`plot` returns `PlotResponse {plotResponse, plotHover}`. Hover is `Nothing` outside the data box:

```haskell
resp <- plot cache (fillW . fixedH 260) theChart
case plotHover resp of
  Nothing -> muted "Hover the chart to inspect a point"
  Just h  -> kv "value"
               (T.pack (show (round (hoverDataY h) :: Int)) <> " at sample "
                  <> T.pack (show (round (hoverDataX h) :: Int)))
```

For a crosshair cursor over the chart, wrap the call:

```haskell
withCursorShape UiCursorCrosshair (plot cache layout theChart)
```

## Theme awareness

Charts read the active theme for the frame, grid, axes, labels, and the series palette. A chart inside `styled` or `themed` picks up that scope automatically. There is no separate chart theme to configure; `themeAccent`, `themeMuted`, and the palette fields do the work.

## Cache lifetime

A `PlotCache` belongs to one component and one session:

```haskell
data App = App
  { appPlots :: PlotCache
  , appSeries :: Var [Point]
  }

newApp :: IO App
newApp = App <$> newPlotCache <*> newVar []
```

Do not allocate a cache inside the view; it would discard the comparison state every frame. The cache keeps an entry per plot widget id and does not evict entries, so avoid creating plots whose ids change constantly (for example, one per item of an unbounded list). For a scrolling list of charts, reuse a fixed window of plot ids instead.

## Diagrams-lib content

`diagram` places any `QDiagram` in the layout:

```haskell
import Diagrams.Prelude
import NanoUI.Diagrams

myDiagram :: NanoUI Response
myDiagram = diagram (fixedWH 320 200) $ circle 80 # fc blue
```

`diagramWithEnvelope` sets an explicit envelope; `diagramWithKeyAndEnvelope` adds a content version for cached drawings. The backend is exposed as `NanoUI.Diagrams.Backend` if you need to emit `DrawOp`s yourself.

## Performance checklist

- Allocate the `PlotCache` once, in `IO`.
- Build the `Chart` value per frame only from data you already have; do not parse or regroup in the view.
- Keep the point count sane; decimation helps drawing, not the cost of building the list.
- For live data, keep only the visible window of samples (the `LiveChart` example keeps 200).
- Do not create a new plot id per item in a growing list.

The live chart tutorial shows the stream-to-chart pipeline end to end: [Live Data & Background Work](../tutorials/live-data.md).
