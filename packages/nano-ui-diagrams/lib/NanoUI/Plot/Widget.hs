-- | Chart widgets: 'plot' draws a chart and reports the hovered point, and
-- 'lineChart', 'barChart', 'scatterChart' and 'areaChart' draw one series.
module NanoUI.Plot.Widget
  ( plot
  , lineChart
  , barChart
  , scatterChart
  , areaChart
  ) where

import Data.IORef (readIORef)
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Data.Vector.Unboxed qualified as U
import Diagrams.Prelude (Diagram, V2 (..), extentX, extentY, size)
import Effectful (Eff, type (:>))
import NanoUI
  ( FontMetrics
  , Layout
  , Theme
  , Ui
  , WidgetId
  , uiFontMetrics
  , uiMousePos
  , uiTheme
  , prepareFontMetricsMany
  , respRect
  )
import NanoUI.Context (Context (..), getStore, intKey, setStore)
import NanoUI.Monad (askContext, nextId, uiIO)
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (PlotStyle, diagramWithKeyAndEnvelope, uiPlotStyle)
import NanoUI.Plot.Builder qualified as Builder
import NanoUI.Plot.Chrome (chartDiagram, seriesDomains, seriesPoints)
import NanoUI.Plot.Scale (formatTick, niceTicks)
import NanoUI.Plot.Hit (hitTestChartCached)
import NanoUI.Plot.Series (area, bar, line, scatter)
import NanoUI.Plot.Types
  ( Chart (..)
  , Domain
  , Series (..)
  , LegendPos (..)
  , PlotResponse (..)
  )
import NanoUI.Store (insertDyn, lookupDyn)

data CachedChart = CachedChart
  { ccChart :: !Chart
  , ccTheme :: !Theme
  , ccFont :: {-# UNPACK #-} !Int
  , ccStyle :: !PlotStyle
  , ccVersion :: {-# UNPACK #-} !Int
  , ccDiagram :: !(Diagram B)
  , ccWidth :: {-# UNPACK #-} !Double
  , ccHeight :: {-# UNPACK #-} !Double
  , ccExtX :: !(Double, Double)
  , ccExtY :: !(Double, Double)
  , ccDomains :: !(Domain, Domain)
  , ccPoints :: ![U.Vector (Double, Double)]
  }

-- Keep the cache in the owning context's widget store. Versions only need
-- to distinguish successive contents of this widget's draw-op cache.
cachedChartDiagram :: Context -> WidgetId -> FontMetrics -> Theme -> PlotStyle -> Chart -> IO CachedChart
cachedChartDiagram ctx wid fm theme ps chart = do
  let k = intKey wid
  font <- readIORef (ctxMetricGen ctx)
  store <- getStore ctx
  let previous = lookupDyn k store
  case previous of
    Just cc | ccChart cc == chart && ccTheme cc == theme && ccFont cc == font && ccStyle cc == ps -> pure cc
    _ -> do
      let domains@(xDom, yDom) = seriesDomains chart
          points = map (seriesPoints chart) (chartSeries chart)
          labels = catMaybes [chartTitle chart, chartXTitle chart, chartYTitle chart]
            ++ map seriesName (chartSeries chart)
            ++ map formatTick (niceTicks 6 xDom ++ niceTicks 6 yDom)
      prepared <- prepareFontMetricsMany fm labels
      let !d = chartDiagram prepared theme ps domains points chart
          !(V2 dw dh) = size d
          extX = fromMaybe (0, dw) (extentX d)
          extY = fromMaybe (0, dh) (extentY d)
      let !v = maybe 1 ((+ 1) . ccVersion) previous
          !cc = CachedChart chart theme font ps v d dw dh extX extY domains points
      setStore ctx (insertDyn k cc store)
      pure cc

-- | Draw a chart sized by the layout modifier. The response reports the
-- nearest data point under the pointer.
plot :: Ui :> es => (Layout -> Layout) -> Chart -> Eff es PlotResponse
plot f chart = do
  wid <- nextId
  ctx <- askContext
  fm <- uiFontMetrics
  theme <- uiTheme
  ps <- uiPlotStyle
  cc <- uiIO (cachedChartDiagram ctx wid fm theme ps chart)
  resp <- diagramWithKeyAndEnvelope (ccVersion cc) (ccWidth cc) (ccHeight cc) f (ccDiagram cc)
  mouse <- uiMousePos
  let hover = hitTestChartCached (ccWidth cc) (ccHeight cc) (ccExtX cc) (ccExtY cc) (ccDomains cc) (ccPoints cc) (respRect resp) mouse
  pure PlotResponse {plotResponse = resp, plotHover = hover}

-- | One line series with a grid and no legend.
lineChart :: Ui :> es => (Layout -> Layout) -> [(Double, Double)] -> Eff es PlotResponse
lineChart f pts = plot f (singleSeries True (line "series" pts))

-- | One category-bar series with a grid, no legend, and no decimation.
barChart :: Ui :> es => (Layout -> Layout) -> [(Text, Double)] -> Eff es PlotResponse
barChart f pts = plot f (singleSeries False (bar "series" pts))

-- | One scatter series with a grid, no legend, and no decimation.
scatterChart :: Ui :> es => (Layout -> Layout) -> [(Double, Double)] -> Eff es PlotResponse
scatterChart f pts = plot f (singleSeries False (scatter "series" pts))

-- | One area series with a zero baseline, grid, no legend, and decimation.
areaChart :: Ui :> es => (Layout -> Layout) -> [(Double, Double)] -> Eff es PlotResponse
areaChart f pts = plot f (singleSeries True (area "series" pts))

-- | A gridded chart of one series without a legend, optionally decimated.
singleSeries :: Bool -> Series -> Chart
singleSeries decimate s =
  Builder.withDecimate decimate (Builder.withLegend LegendNone (Builder.chart [s]))
