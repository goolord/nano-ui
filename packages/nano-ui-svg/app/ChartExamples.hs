-- | Chart and diagram examples for the book. Each anchored block is
-- included in a book page (@packages/index.md@, @tutorials/live-data.md@,
-- @widgets/custom-drawing.md@), and the glossary generator renders the same
-- definitions to @book/src/images/charts/@, so the code on the page is the
-- code that drew the picture beside it.
--
-- Everything outside the anchors is annotation: the captioned cells each
-- example is drawn into, and the pointer marking where a hover was taken.
module ChartExamples (chartExamples) where

import Control.Monad (void)
import Data.Bits (shiftR)
import Data.Foldable (for_)
import Data.List (unfoldr)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Diagrams.Prelude (Diagram, arrowBetween', fc, fontSizeL, frame, headLength, lc, local, lw, lwO, none, p2, roundedRect, text, translate, with, (#), (&), (.~))
import Diagrams.Prelude qualified as D
import NanoUI
import NanoUI.Diagrams
import NanoUI.Path qualified as P

-- ANCHOR: cpu_chart
cpuChart :: PlotCache -> [(Double, Double)] -> NanoUI ()
cpuChart cache samples =
  void $ plot cache (fillW . fixedH 220) $
    withGrid GridBoth
      . withXAxis "time (s)"
      . withYAxis "CPU %"
      . withLegend LegendNone
      $ chart [line "cpu" samples]
-- ANCHOR_END: cpu_chart

-- | A minute of CPU load: two slow swells and some faster wobble.
cpuSamples :: [(Double, Double)]
cpuSamples = [(t, 38 + 16 * sin (t / 7) + 7 * sin (t * 0.9 + 1) + 4 * sin (t * 2.3)) | t <- [0 .. 60]]

-- ANCHOR: data
wave :: [(Double, Double)]
wave = [(x, 3 + sin x + 0.5 * sin (2.3 * x)) | x <- [0, 0.3 .. 2 * pi]]

counts :: [(Double, Double)]
counts = zip [0 ..] [2, 4, 3, 6, 5, 7, 4, 5]

quarters :: [(Text, Double)]
quarters = [("Q1", 3), ("Q2", 5), ("Q3", 4), ("Q4", 7)]
-- ANCHOR_END: data

-- ANCHOR: series
seriesKinds :: [(Text, Series)]
seriesKinds =
  [ ("line", line "line" wave)
  , ("scatter", scatter "scatter" wave)
  , ("area", area "area" wave)
  , ("step", step "step" counts)
  , ("bar", bar "bar" quarters)
  ]
-- ANCHOR_END: series

-- ANCHOR: styles
seriesStyles :: [(Text, Series)]
seriesStyles =
  [ ("withColor", withColor (colorRGB 20 184 166) (line "s" wave))
  , ("withStrokeWidth 4", withStrokeWidth 4 (line "s" wave))
  , ("withMarker MarkCircle", withMarker MarkCircle (line "s" counts))
  , ("withBaseline 3", withBaseline 3 (area "s" wave))
  ]
-- ANCHOR_END: styles

-- ANCHOR: markers
markers :: [(Text, Series)]
markers =
  [ (name, withStrokeWidth 6 (withMarker shape (scatter name counts)))
  | (name, shape) <-
      [ ("MarkCircle", MarkCircle)
      , ("MarkSquare", MarkSquare)
      , ("MarkDiamond", MarkDiamond)
      , ("MarkTriangle", MarkTriangle)
      , ("MarkCross", MarkCross)
      ]
  ]
-- ANCHOR_END: markers

-- ANCHOR: grids
grids :: [(Text, Chart)]
grids =
  [ (name, withGrid mode . withLegend LegendNone $ chart [line "s" wave])
  | (name, mode) <-
      [("GridBoth", GridBoth), ("GridHorizontal", GridHorizontal), ("GridVertical", GridVertical), ("GridNone", GridNone)]
  ]
-- ANCHOR_END: grids

-- ANCHOR: legends
latency :: Chart
latency =
  withTitle "Latency" $
    chart [line "p50" [(x, 20 + 4 * sin x) | x <- [0, 0.5 .. 6]], line "p99" [(x, 42 + 9 * sin (x + 1)) | x <- [0, 0.5 .. 6]]]

legends :: [(Text, Chart)]
legends =
  [ (name, withLegend pos latency)
  | (name, pos) <-
      [ ("LegendRight", LegendRight)
      , ("LegendTop", LegendTop)
      , ("LegendNone", LegendNone)
      , ("LegendBottom", LegendBottom)
      , ("LegendInside", LegendInside)
      ]
  ]
-- ANCHOR_END: legends

-- ANCHOR: hover
requests :: [(Double, Double)]
requests = zip [0 ..] [120, 180, 150, 260, 210, 300, 240, 280]

hoverReadout :: PlotCache -> NanoUI ()
hoverReadout cache = columnWith (gap 8) $ do
  resp <- plot cache (fixedWH 240 200) (withLegend LegendNone (chart [line "requests" requests]))
  case plotHover resp of
    Nothing -> muted "Hover a point"
    Just h -> kv "value" (T.pack (show (round (hoverDataY h) :: Int)))
-- ANCHOR_END: hover

-- ANCHOR: diagram
-- | Three labelled boxes joined by arrows, coloured from the theme.
stages :: Theme -> Diagram B
stages t = (mconcat (zipWith stage [0 ..] ["view", "layout", "paint"]) <> arrows) # frame 0.25
  where
    ink = colourOf (styleFg (themePanel t))
    stage i name =
      (text name # fontSizeL 0.3 # fc ink <> roundedRect 1.6 0.8 0.15 # fc (colourOf (themeAccent t)) # lw none)
        # translate (D.r2 (2.4 * i, 0))
    arrows =
      mconcat
        [ arrowBetween' (with & headLength .~ local 0.2) (p2 (2.4 * i + 0.9, 0)) (p2 (2.4 * i + 1.5, 0)) # lc ink # lwO 1.5
        | i <- [0, 1]
        ]

pipelineDiagram :: NanoUI ()
pipelineDiagram = void $ diagram (fixedWH 360 72) . stages =<< uiTheme
-- ANCHOR_END: diagram

-- ANCHOR: sparkline
sparklines :: [Float] -> NanoUI ()
sparklines samples = rowWith (gap 16 . alignMid) $ do
  sparkline samples
  sparklineWith (fixedWH 120 32) 120 32 samples
-- ANCHOR_END: sparkline

sparkSamples :: [Float]
sparkSamples = [4 + 2 * sin (x / 2) + sin (x * 1.7) | x <- [0 .. 30]]

-- | The live chart tutorial's chart after 200 samples: the same random walk
-- and chart as @walkChart@ and @advance@ in @nano-ui-demo/examples/LiveChart.hs@.
liveChart :: PlotCache -> NanoUI ()
liveChart cache =
  void $
    plot cache (fillW . minH 300) $
      withGrid GridBoth . withLegend LegendNone . withXAxis "sample" . withYAxis "value" $
        chart [line "walk" (zip [0 ..] (take 200 walk))]
  where
    walk = unfoldr stepWalk (42, 0)
    stepWalk (seed, level) =
      let seed' = seed * 6364136223846793005 + 1442695040888963407 :: Word64
          level' = level + fromIntegral (seed' `shiftR` 11) / 2 ^ (53 :: Int) - 0.5 :: Double
       in Just (level', (seed', level'))

-- ---------------------------------------------------------------------------
-- Annotation
-- ---------------------------------------------------------------------------

-- | Charts of one height, each captioned with its name, wrapped onto lines.
-- A chart with a fixed height takes the width its drawing's proportions
-- give it.
cells :: Float -> PlotCache -> [(Text, Chart)] -> NanoUI ()
cells h cache examples =
  rowWith (wrap . gap 16 . lineGap 16 . padAll 12 . fillW) $
    for_ examples $ \(caption, c) -> columnWith (tight . gap 6) $ do
      void (plot cache (fixedH h) c)
      labelWith (alignCenter . fontMono . fontSize 12 . fontMuted) caption

-- | Cells of one series each, without a legend.
seriesCells :: Float -> PlotCache -> [(Text, Series)] -> NanoUI ()
seriesCells h cache examples =
  cells h cache [(caption, withLegend LegendNone (chart [s])) | (caption, s) <- examples]

-- | Where the hover example's pointer rests, in window coordinates.
hoverPoint :: V2
hoverPoint = V2 222 70

-- | A pointer arrow with its tip on 'hoverPoint', drawn over the view.
pointerOver :: NanoUI () -> NanoUI ()
pointerOver ui = layers $ do
  ui
  let V2 px py = hoverPoint
  void $ canvas (pinAt px py . fixedWH 14 20 . pointer PointerPass) $ \(Rect x y _ _) -> do
    let tip = P.polygon [V2 x y, V2 x (y + 16), V2 (x + 4) (y + 12), V2 (x + 7) (y + 19), V2 (x + 9) (y + 18), V2 (x + 6) (y + 11), V2 (x + 11) (y + 11)]
    drawPath tip colorWhite
    drawStrokePath tip 1 colorBlack

-- | Every example as (file name, widest view width, pointer, view). The
-- generator narrows a view to what it draws and crops its height.
chartExamples :: [(Text, Float, Maybe V2, PlotCache -> NanoUI ())]
chartExamples =
  [ ("cpu-chart", 320, Nothing, \cache -> columnWith (padAll 12 . fillW) (cpuChart cache cpuSamples))
  , ("series", 760, Nothing, \cache -> seriesCells 180 cache seriesKinds)
  , ("styles", 760, Nothing, \cache -> seriesCells 120 cache seriesStyles)
  , ("markers", 760, Nothing, \cache -> seriesCells 110 cache markers)
  , ("grids", 760, Nothing, \cache -> cells 120 cache grids)
  , ("legends", 760, Nothing, \cache -> cells 170 cache legends)
  , ("hover", 760, Just hoverPoint, \cache -> pointerOver (columnWith (padAll 12) (hoverReadout cache)))
  , ("diagram", 760, Nothing, \_ -> columnWith (padAll 12) pipelineDiagram)
  , ("sparkline", 760, Nothing, \_ -> columnWith (padAll 12) (sparklines sparkSamples))
  , ("live-chart", 480, Nothing, \cache -> columnWith (padAll 12 . fillW) (liveChart cache))
  ]
