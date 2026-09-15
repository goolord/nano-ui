{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Widget
  ( plot
  , lineChart
  , barChart
  , scatterChart
  , areaChart
  ) where

import Data.Dynamic (fromDynamic, toDyn)
import Data.IORef (readIORef)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, catMaybes)
import Data.Text (Text)
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
import NanoUI.Context (Context (..), WidgetStore (..), getStore, intKey, setStore)
import NanoUI.Monad (askContext, nextId, uiIO)
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (PlotStyle, diagramWithKeyAndEnvelope, uiPlotStyle)
import NanoUI.Plot.Builder qualified as Builder
import NanoUI.Plot.Chrome (chartDiagram, seriesDomains)
import NanoUI.Plot.Scale (formatTick, niceTicks)
import NanoUI.Plot.Hit (hitTestChartCached)
import NanoUI.Plot.Series (area, bar, line, scatter)
import NanoUI.Plot.Types
  ( Chart (..)
  , Series (..)
  , LegendPos (..)
  , PlotResponse (..)
  )

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
  }

-- Keep the cache in the owning context's widget store. Versions only need
-- to distinguish successive contents of this widget's draw-op cache.
cachedChartDiagram :: Context -> WidgetId -> FontMetrics -> Theme -> PlotStyle -> Chart -> IO CachedChart
cachedChartDiagram ctx wid fm theme ps chart = do
  let k = intKey wid
  font <- readIORef (ctxMetricGen ctx)
  store <- getStore ctx
  let previous = IM.lookup k (storeDyn store) >>= fromDynamic
  case previous of
    Just cc | ccChart cc == chart && ccTheme cc == theme && ccFont cc == font && ccStyle cc == ps -> pure cc
    _ -> do
      let (xDom, yDom) = seriesDomains chart
          labels = catMaybes [chartTitle chart, chartXTitle chart, chartYTitle chart]
            ++ map seriesName (chartSeries chart)
            ++ map formatTick (niceTicks 6 xDom ++ niceTicks 6 yDom)
      prepared <- prepareFontMetricsMany fm labels
      let !d = chartDiagram prepared theme ps chart
          !(V2 dw dh) = size d
          extX = fromMaybe (0, dw) (extentX d)
          extY = fromMaybe (0, dh) (extentY d)
      let !v = maybe 1 ((+ 1) . ccVersion) previous
          !cc = CachedChart chart theme font ps v d dw dh extX extY
      setStore ctx (store {storeDyn = IM.insert k (toDyn cc) (storeDyn store)})
      pure cc

plot :: Ui :> es => Layout -> Chart -> Eff es PlotResponse
plot layout chart = do
  wid <- nextId
  ctx <- askContext
  fm <- uiFontMetrics
  theme <- uiTheme
  ps <- uiPlotStyle
  cc <- uiIO (cachedChartDiagram ctx wid fm theme ps chart)
  resp <- diagramWithKeyAndEnvelope (ccVersion cc) (ccWidth cc) (ccHeight cc) layout (ccDiagram cc)
  mouse <- uiMousePos
  let hover = hitTestChartCached (ccDiagram cc) (ccWidth cc) (ccHeight cc) (ccExtX cc) (ccExtY cc) chart (respRect resp) mouse
  pure PlotResponse {plotResponse = resp, plotHover = hover}

lineChart :: Ui :> es => Layout -> [(Double, Double)] -> Eff es PlotResponse
lineChart layout pts = plot layout (singleSeries True (line "series" pts))

barChart :: Ui :> es => Layout -> [(Text, Double)] -> Eff es PlotResponse
barChart layout pts = plot layout (singleSeries False (bar "series" pts))

scatterChart :: Ui :> es => Layout -> [(Double, Double)] -> Eff es PlotResponse
scatterChart layout pts = plot layout (singleSeries False (scatter "series" pts))

areaChart :: Ui :> es => Layout -> [(Double, Double)] -> Eff es PlotResponse
areaChart layout pts = plot layout (singleSeries True (area "series" pts))

-- | A gridded chart of one series without a legend, optionally decimated.
singleSeries :: Bool -> Series -> Chart
singleSeries decimate s =
  Builder.withDecimate decimate (Builder.withLegend LegendNone (Builder.chart [s]))
