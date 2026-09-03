{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Widget
  ( plot
  , lineChart
  , barChart
  , scatterChart
  , areaChart
  ) where

import Data.Text (Text)
import Effectful (Eff, type (:>))
import NanoUI
  ( Layout
  , Responding (..)
  , Ui
  , uiFontMetrics
  , uiMousePos
  , uiTheme
  )
import NanoUI.Diagrams.Widget (diagram, uiPlotStyle)
import NanoUI.Plot.Chrome (chartDiagram)
import NanoUI.Plot.Hit (hitTestChart)
import NanoUI.Plot.Series (area, bar, line, scatter)
import NanoUI.Plot.Types
  ( Chart (..)
  , GridMode (..)
  , LegendPos (..)
  , PlotResponse (..)
  )

plot :: Ui :> es => Layout -> Chart -> Eff es PlotResponse
plot layout chart = do
  fm <- uiFontMetrics
  theme <- uiTheme
  ps <- uiPlotStyle
  resp <- diagram layout (chartDiagram fm theme ps chart)
  mouse <- uiMousePos
  let hover = hitTestChart fm theme ps chart (respRect resp) mouse
  pure PlotResponse {plotResponse = resp, plotHover = hover}

lineChart :: Ui :> es => Layout -> [(Double, Double)] -> Eff es PlotResponse
lineChart layout pts =
  plot
    layout
    Chart
      { chartTitle = Nothing
      , chartXTitle = Nothing
      , chartYTitle = Nothing
      , chartSeries = [line "series" pts]
      , chartLegend = LegendNone
      , chartGrid = GridBoth
      , chartDecimate = True
      }

barChart :: Ui :> es => Layout -> [(Text, Double)] -> Eff es PlotResponse
barChart layout pts =
  plot
    layout
    Chart
      { chartTitle = Nothing
      , chartXTitle = Nothing
      , chartYTitle = Nothing
      , chartSeries = [bar "series" pts]
      , chartLegend = LegendNone
      , chartGrid = GridBoth
      , chartDecimate = False
      }

scatterChart :: Ui :> es => Layout -> [(Double, Double)] -> Eff es PlotResponse
scatterChart layout pts =
  plot
    layout
    Chart
      { chartTitle = Nothing
      , chartXTitle = Nothing
      , chartYTitle = Nothing
      , chartSeries = [scatter "series" pts]
      , chartLegend = LegendNone
      , chartGrid = GridBoth
      , chartDecimate = False
      }

areaChart :: Ui :> es => Layout -> [(Double, Double)] -> Eff es PlotResponse
areaChart layout pts =
  plot
    layout
    Chart
      { chartTitle = Nothing
      , chartXTitle = Nothing
      , chartYTitle = Nothing
      , chartSeries = [area "series" pts]
      , chartLegend = LegendNone
      , chartGrid = GridBoth
      , chartDecimate = True
      }
