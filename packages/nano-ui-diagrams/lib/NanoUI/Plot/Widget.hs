{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Widget
  ( plot
  , lineChart
  , barChart
  , scatterChart
  , areaChart
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Diagrams.Prelude (Diagram, V2 (..), extentX, extentY, size)
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
import NanoUI.Diagrams.Widget (PlotStyle, diagramWithKeyAndEnvelope, uiPlotStyle)
import NanoUI.Plot.Chrome (chartDiagram)
import NanoUI.Plot.Hit (hitTestChartCached)
import NanoUI.Plot.Series (area, bar, line, scatter)
import NanoUI.Plot.Types
  ( Chart (..)
  , GridMode (..)
  , LegendPos (..)
  , PlotResponse (..)
  )
import System.IO.Unsafe (unsafePerformIO)

data CachedChart = CachedChart
  { ccChart :: !Chart
  , ccTheme :: !Theme
  , ccVersion :: {-# UNPACK #-} !Int
  , ccDiagram :: !(Diagram B)
  , ccWidth :: {-# UNPACK #-} !Double
  , ccHeight :: {-# UNPACK #-} !Double
  , ccExtX :: !(Double, Double)
  , ccExtY :: !(Double, Double)
  }

{-# NOINLINE chartCacheRef #-}
chartCacheRef :: IORef (IM.IntMap CachedChart)
chartCacheRef = unsafePerformIO (newIORef IM.empty)

-- Monotonic version source for the draw-op / fit caches. Bumped only when a
-- chart diagram is rebuilt, so cached ops survive hover animations and other
-- per-frame state that does not change chart content.
{-# NOINLINE chartVersionRef #-}
chartVersionRef :: IORef Int
chartVersionRef = unsafePerformIO (newIORef 0)

cachedChartDiagram :: WidgetId -> FontMetrics -> Theme -> PlotStyle -> Chart -> IO CachedChart
cachedChartDiagram wid fm theme ps chart = do
  let k = intKey wid
  cache <- readIORef chartCacheRef
  case IM.lookup k cache of
    Just cc | ccChart cc == chart && ccTheme cc == theme -> pure cc
    _ -> do
      let !d = chartDiagram fm theme ps chart
          !(V2 dw dh) = size d
          extX = fromMaybe (0, dw) (extentX d)
          extY = fromMaybe (0, dh) (extentY d)
      v <- atomicModifyIORef' chartVersionRef (\n -> (n + 1, n + 1))
      let !cc = CachedChart chart theme v d dw dh extX extY
      writeIORef chartCacheRef (IM.insert k cc cache)
      pure cc

plot :: Ui :> es => Layout -> Chart -> Eff es PlotResponse
plot layout chart = do
  wid <- nextId
  fm <- uiFontMetrics
  theme <- uiTheme
  ps <- uiPlotStyle
  cc <- uiIO (cachedChartDiagram wid fm theme ps chart)
  resp <- diagramWithKeyAndEnvelope (ccVersion cc) (ccWidth cc) (ccHeight cc) layout (ccDiagram cc)
  mouse <- uiMousePos
  let hover = hitTestChartCached (ccDiagram cc) (ccWidth cc) (ccHeight cc) (ccExtX cc) (ccExtY cc) chart (respRect resp) mouse
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
