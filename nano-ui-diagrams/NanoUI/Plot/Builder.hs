{-# LANGUAGE OverloadedStrings #-}

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

chart :: [Series] -> Chart
chart series = emptyChart {chartSeries = series}

withTitle :: Text -> Chart -> Chart
withTitle t c = c {chartTitle = Just t}

withXAxis :: Text -> Chart -> Chart
withXAxis t c = c {chartXTitle = Just t}

withYAxis :: Text -> Chart -> Chart
withYAxis t c = c {chartYTitle = Just t}

withLegend :: LegendPos -> Chart -> Chart
withLegend p c = c {chartLegend = p}

withGrid :: GridMode -> Chart -> Chart
withGrid g c = c {chartGrid = g}

withDecimate :: Bool -> Chart -> Chart
withDecimate b c = c {chartDecimate = b}

addSeries :: Series -> Chart -> Chart
addSeries s c = c {chartSeries = chartSeries c ++ [s]}
