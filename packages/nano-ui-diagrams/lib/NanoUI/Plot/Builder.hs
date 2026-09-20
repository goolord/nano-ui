-- | Functions that build a 'NanoUI.Plot.Types.Chart' from series and set its
-- titles, legend, grid and decimation.
module NanoUI.Plot.Builder
  ( chart
  , withTitle
  , withXAxis
  , withYAxis
  , withLegend
  , withGrid
  , withDecimate
  , addSeries
  ) where

import Data.Text (Text)
import NanoUI.Plot.Types
  ( Chart (..)
  , GridMode
  , LegendPos
  , Series
  , emptyChart
  )

-- | A chart with the supplied series and 'emptyChart' defaults.
chart :: [Series] -> Chart
chart series = emptyChart {chartSeries = series}

-- | Set the title above the chart.
withTitle :: Text -> Chart -> Chart
withTitle t c = c {chartTitle = Just t}

-- | Set the horizontal axis title; does not change its data domain.
withXAxis :: Text -> Chart -> Chart
withXAxis t c = c {chartXTitle = Just t}

-- | Set the vertical axis title; does not change its data domain.
withYAxis :: Text -> Chart -> Chart
withYAxis t c = c {chartYTitle = Just t}

-- | Choose the legend position or hide it.
withLegend :: LegendPos -> Chart -> Chart
withLegend p c = c {chartLegend = p}

-- | Choose which grid directions to draw.
withGrid :: GridMode -> Chart -> Chart
withGrid g c = c {chartGrid = g}

-- | Enable or disable thinning numeric series before drawing and hit testing.
withDecimate :: Bool -> Chart -> Chart
withDecimate b c = c {chartDecimate = b}

-- | Append a series, preserving the existing series and legend order.
addSeries :: Series -> Chart -> Chart
addSeries s c = c {chartSeries = chartSeries c ++ [s]}
