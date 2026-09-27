{-# LANGUAGE MagicHash #-}

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
import GHC.Exts (isTrue#, reallyUnsafePtrEquality#)
import NanoUI
  ( Layout
  , Theme
  , Ui
  , WidgetId
  , uiMousePos
  , uiTheme
  , respRect
  )
import NanoUI.Backend (FontMetrics, prepareFontMetricsMany, uiFontMetrics)
import NanoUI.Internal.Context (Context (..), getStore, intKey, setStore)
import NanoUI.Internal.Monad (freshWidget)
import NanoUI.Monad (uiIO)
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
import NanoUI.Internal.Store (insertDyn, lookupDyn)

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

-- Keep the cache in the owning context's widget store. Versions only need
-- to distinguish successive contents of this widget's draw-op cache. The
-- plot style is derived from the theme, so the theme check covers it.
cachedChartDiagram :: Context -> WidgetId -> FontMetrics -> Theme -> Chart -> IO CachedChart
cachedChartDiagram ctx wid fm theme chart = do
  let k = intKey wid
  font <- readIORef (ctxMetricGen ctx)
  store <- getStore ctx
  let previous = lookupDyn k store
  case previous of
    Just cc | ccTheme cc == theme && ccFont cc == font && sameChart (ccChart cc) chart -> pure cc
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
      setStore ctx (insertDyn k cc store)
      pure cc

-- | Whether the cached chart stands for @chart@: the same value, as when a
-- chart is kept across frames, or an equal one.
sameChart :: Chart -> Chart -> Bool
sameChart !a !b = isTrue# (reallyUnsafePtrEquality# a b) || a == b

-- | Draw a chart sized by the layout modifier. The response reports the
-- nearest data point under the pointer.
plot :: Ui :> es => (Layout -> Layout) -> Chart -> Eff es PlotResponse
plot f chart = do
  (wid, ctx) <- freshWidget
  fm <- uiFontMetrics
  theme <- uiTheme
  cc <- uiIO (cachedChartDiagram ctx wid fm theme chart)
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
