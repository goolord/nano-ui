{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Widget
  ( plot
  , lineChart
  , barChart
  , scatterChart
  , areaChart
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import Diagrams.Prelude (Diagram)
import Effectful (Eff, type (:>))
import NanoUI
  ( FontMetrics
  , Layout
  , Responding (..)
  , Theme
  , Ui
  , WidgetId
  , uiFontMetrics
  , uiMousePos
  , uiTheme
  )
import NanoUI.Context (intKey)
import NanoUI.Monad (nextId, uiIO)
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (PlotStyle, diagram, uiPlotStyle)
import NanoUI.Plot.Chrome (chartDiagram)
import NanoUI.Plot.Hit (hitTestChart)
import NanoUI.Plot.Series (area, bar, line, scatter)
import NanoUI.Plot.Types
  ( Chart (..)
  , GridMode (..)
  , LegendPos (..)
  , PlotResponse (..)
  )
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE chartCacheRef #-}
chartCacheRef :: IORef (IM.IntMap (Chart, Theme, Diagram B))
chartCacheRef = unsafePerformIO (newIORef IM.empty)

cachedChartDiagram :: WidgetId -> FontMetrics -> Theme -> PlotStyle -> Chart -> IO (Diagram B)
cachedChartDiagram wid fm theme ps chart = do
  let k = intKey wid
  cache <- readIORef chartCacheRef
  case IM.lookup k cache of
    Just (c, t, d) | c == chart && t == theme -> pure d
    _ -> do
      let !d = chartDiagram fm theme ps chart
      writeIORef chartCacheRef (IM.insert k (chart, theme, d) cache)
      pure d

plot :: Ui :> es => Layout -> Chart -> Eff es PlotResponse
plot layout chart = do
  wid <- nextId
  fm <- uiFontMetrics
  theme <- uiTheme
  ps <- uiPlotStyle
  d <- uiIO (cachedChartDiagram wid fm theme ps chart)
  resp <- diagram layout d
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
