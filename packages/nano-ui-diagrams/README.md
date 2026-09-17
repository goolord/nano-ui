# nano-ui-diagrams

Charts and [diagrams](https://diagrams.github.io/) drawings for
[nano-ui](https://github.com/goolord/nano-ui).

`diagram` places a diagrams-lib `Diagram` in a nano-ui layout. `NanoUI.Plot`
builds line, bar, scatter, area, and step charts with axes, legends, and hover
lookup, and thins out long series before drawing them.

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Text qualified as T
import NanoUI
import NanoUI.Plot

temperatures :: Chart
temperatures =
  withTitle "Temperature" . withLegend LegendBottom $
    chart
      [ line "Indoor" [(0, 20), (1, 21.5), (2, 22), (3, 21)]
      , line "Outdoor" [(0, 8), (1, 11), (2, 14), (3, 12)]
      ]

view :: NanoUI ()
view = column $ do
  resp <- plot (minH 240 . fillW) temperatures
  case plotHover resp of
    Just h -> label (T.pack (show (hoverDataX h, hoverDataY h)))
    Nothing -> pure ()
```

The Plots tab of `nano-ui-sdl-demo` in `nano-ui-demo` shows more charts and a
diagram.
