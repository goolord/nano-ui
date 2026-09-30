-- | Chart widgets: 'plot' draws a chart and reports the hovered point, and
-- 'lineChart', 'barChart', 'scatterChart' and 'areaChart' draw one series.
module NanoUI.Plot.Widget
  ( PlotCache
  , newPlotCache
  , plot
  , lineChart
  , barChart
  , scatterChart
  , areaChart
  ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
import Data.Vector.Unboxed qualified as U
import Diagrams.Prelude (Diagram, V2 (..), extentX, extentY, size)
import NanoUI
  ( NanoUI
  , Layout
  , Theme
  , WidgetId
  , uiMousePos
  , uiTheme
  , respRect
  )
import NanoUI.Backend (FontMetrics, prepareFontMetricsMany, uiFontMetrics)
import NanoUI.Internal.Context (Context (..), intKey, markDirtyCovered)
import NanoUI.Internal.Monad (freshWidget)
import NanoUI.Monad (liftIO)
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (diagramWithKeyAndEnvelope, themePlotStyle)
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
import NanoUI.Internal.Store (eqByPtr)

-- | Typed chart cache owned by a component in one session. Allocate once
-- during setup; multiple plots in that component are distinguished by widget id.
newtype PlotCache = PlotCache (IORef (IM.IntMap CachedChart))

newPlotCache :: IO PlotCache
newPlotCache = PlotCache <$> newIORef IM.empty

data CachedChart = CachedChart
  { ccChart :: !Chart
  , ccTheme :: !Theme
  , ccFont :: {-# UNPACK #-} !Int
  , ccVersion :: {-# UNPACK #-} !Int
  , ccDiagram :: !(Diagram B)
  , ccWidth :: {-# UNPACK #-} !Double
  , ccHeight :: {-# UNPACK #-} !Double
  , ccExtX :: !(Double, Double)
  , ccExtY :: !(Double, Double)
  , ccDomains :: !(Domain, Domain)
  , ccPoints :: ![U.Vector (Double, Double)]
  }

-- Keep the cache in the component's typed owner. Versions only need
-- to distinguish successive contents of this widget's draw-op cache. The
-- plot style is derived from the theme, so the theme check covers it. A
-- chart kept across frames matches by pointer, before its value is compared.
cachedChartDiagram :: PlotCache -> Context -> WidgetId -> FontMetrics -> Theme -> Chart -> IO CachedChart
cachedChartDiagram (PlotCache ref) ctx wid fm theme chart = do
  let k = intKey wid
  font <- readIORef (ctxMetricGen ctx)
  cache <- readIORef ref
  let previous = IM.lookup k cache
  case previous of
    Just cc | ccTheme cc == theme && ccFont cc == font && eqByPtr (ccChart cc) chart -> pure cc
    _ -> do
      let ps = themePlotStyle theme
          domains@(xDom, yDom) = seriesDomains chart
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
          !cc = CachedChart chart theme font v d dw dh extX extY domains points
      modifyIORef' ref (IM.insert k cc)
      -- The diagram's content version damages its actual drawing node.
      markDirtyCovered ctx
      pure cc

-- | Draw a chart sized by the layout modifier. The response reports the
-- nearest data point under the pointer.
plot :: PlotCache -> (Layout -> Layout) -> Chart -> NanoUI PlotResponse
plot cache f chart = do
  (wid, ctx) <- freshWidget
  fm <- uiFontMetrics
  theme <- uiTheme
  cc <- liftIO (cachedChartDiagram cache ctx wid fm theme chart)
  resp <- diagramWithKeyAndEnvelope (ccVersion cc) (ccWidth cc) (ccHeight cc) f (ccDiagram cc)
  mouse <- uiMousePos
  let hover = hitTestChartCached (ccWidth cc) (ccHeight cc) (ccExtX cc) (ccExtY cc) (ccDomains cc) (ccPoints cc) (respRect resp) mouse
  pure PlotResponse {plotResponse = resp, plotHover = hover}

-- | One line series with a grid and no legend.
lineChart :: PlotCache -> (Layout -> Layout) -> [(Double, Double)] -> NanoUI PlotResponse
lineChart cache f pts = plot cache f (singleSeries True (line "series" pts))

-- | One category-bar series with a grid, no legend, and no decimation.
barChart :: PlotCache -> (Layout -> Layout) -> [(Text, Double)] -> NanoUI PlotResponse
barChart cache f pts = plot cache f (singleSeries False (bar "series" pts))

-- | One scatter series with a grid, no legend, and no decimation.
scatterChart :: PlotCache -> (Layout -> Layout) -> [(Double, Double)] -> NanoUI PlotResponse
scatterChart cache f pts = plot cache f (singleSeries False (scatter "series" pts))

-- | One area series with a zero baseline, grid, no legend, and decimation.
areaChart :: PlotCache -> (Layout -> Layout) -> [(Double, Double)] -> NanoUI PlotResponse
areaChart cache f pts = plot cache f (singleSeries True (area "series" pts))

-- | A gridded chart of one series without a legend, optionally decimated.
singleSeries :: Bool -> Series -> Chart
singleSeries decimate s =
  Builder.withDecimate decimate (Builder.withLegend LegendNone (Builder.chart [s]))
