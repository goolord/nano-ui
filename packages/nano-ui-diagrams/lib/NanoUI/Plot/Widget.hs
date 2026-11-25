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

import Data.IORef (readIORef)
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
import NanoUI.Backend
  ( FontMetrics
  , IntGenCache
  , intGenCacheInsert
  , intGenCacheLookup
  , intGenCacheStartFrame
  , intGenCacheTouch
  , newIntGenCache
  , prepareFontMetricsMany
  , uiFontMetrics
  )
import NanoUI.Internal.Context (Context (..), frameNumber, intKey, markDirty)
import NanoUI.Internal.Monad (freshWidget)
import NanoUI.Monad (liftIO)
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (diagramWithKeyAndEnvelope, themePlotStyle)
import NanoUI.Plot.Builder qualified as Builder
import NanoUI.Plot.Chrome (chartDiagramWithTicks, chartTicks, seriesDomains, seriesPoints)
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
-- Backed by a bounded two-generation cache, so dynamic plot ids do not retain
-- stale diagrams indefinitely.
newtype PlotCache = PlotCache (IntGenCache CachedChart)

-- | An empty cache. Allocate it once, during setup, and pass it to the
-- component's plots: a cache made each frame rebuilds every chart each frame.
newPlotCache :: IO PlotCache
newPlotCache = PlotCache <$> newIntGenCache 16

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
cachedChartDiagram (PlotCache cache) ctx wid fm theme chart = do
  let k = intKey wid
  frame <- frameNumber ctx
  intGenCacheStartFrame cache frame
  font <- readIORef (ctxMetricGen ctx)
  previous <- intGenCacheLookup cache k
  case previous of
    Just cc | ccTheme cc == theme && ccFont cc == font && eqByPtr (ccChart cc) chart -> do
      intGenCacheTouch cache k cc
      pure cc
    _ -> do
      let ps = themePlotStyle theme
          domains = seriesDomains chart
          ticks@(xTicks, yTicks) = chartTicks domains chart
          points = map (seriesPoints chart) (chartSeries chart)
          labels = catMaybes [chartTitle chart, chartXTitle chart, chartYTitle chart]
            ++ map seriesName (chartSeries chart)
            ++ map snd xTicks
            ++ map snd yTicks
      prepared <- prepareFontMetricsMany fm labels
      let !d = chartDiagramWithTicks prepared theme ps domains ticks points chart
          !(V2 dw dh) = size d
          extX = fromMaybe (0, dw) (extentX d)
          extY = fromMaybe (0, dh) (extentY d)
      let !v = maybe 1 ((+ 1) . ccVersion) previous
          !cc = CachedChart chart theme font v d dw dh extX extY domains points
      intGenCacheInsert cache k cc
      -- The diagram's content version damages its actual drawing node.
      markDirty ctx
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
