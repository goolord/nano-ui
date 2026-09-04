{-# LANGUAGE DuplicateRecordFields #-}

module NanoUI.Plot.Hit
  ( hitTestChart
  , diagramPointAt
  , nearestPlotHover
  ) where

import Data.Maybe (fromMaybe)
import Diagrams.Core (QDiagram)
import Diagrams.Prelude qualified as Dia
import NanoUI (FontMetrics, Rect (..), Theme (..), V2, rectContains, v2X, v2Y)
import NanoUI.Diagrams.Backend (NanoUIBackend, uniformHeight)
import NanoUI.Diagrams.Widget (PlotStyle (..))
import NanoUI.Plot.Chrome (chartDiagram, chartXDomain, chartYDomain, seriesPoints)
import NanoUI.Plot.Scale (plotToDomain)
import NanoUI.Plot.Types (Chart (..), PlotHover (..), Range (..))
import qualified Data.Vector as V

diagramBorder :: Float
diagramBorder = 1

hitTestChart ::
  FontMetrics ->
  Theme ->
  PlotStyle ->
  Chart ->
  Rect ->
  V2 ->
  Maybe PlotHover
hitTestChart fm theme ps chart widgetRect mouse =
  let inner =
        Rect
          (rectX widgetRect + diagramBorder)
          (rectY widgetRect + diagramBorder)
          (max 0 (rectW widgetRect - 2 * diagramBorder))
          (max 0 (rectH widgetRect - 2 * diagramBorder))
   in if not (rectContains inner mouse)
        then Nothing
        else
          let lx = v2X mouse - rectX inner
              ly = v2Y mouse - rectY inner
              d = chartDiagram fm theme ps chart
              w = realToFrac (rectW inner) :: Double
              h = realToFrac (rectH inner)
           in case diagramPointAt w h d lx ly of
                Nothing -> Nothing
                Just (gx, gy) -> nearestPlotHover chart gx gy

diagramPointAt ::
  Double ->
  Double ->
  QDiagram NanoUIBackend Dia.V2 Double Dia.Any ->
  Float ->
  Float ->
  Maybe (Double, Double)
diagramPointAt w h d px py
  | w <= 0 || h <= 0 = Nothing
  | otherwise =
      let Dia.V2 dw dh = Dia.size d
          outH = uniformHeight w h d
          outW = if dh <= 1e-9 then w else outH * dw / dh
          offX = (w - outW) / 2
          offY = (h - outH) / 2
          lx = realToFrac px
          ly = realToFrac py
       in if lx < offX || ly < offY || lx > offX + outW || ly > offY + outH
            then Nothing
            else
              let (x0, x1) = fromMaybe (0, dw) (Dia.extentX d)
                  (y0, y1) = fromMaybe (0, dh) (Dia.extentY d)
                  gx = x0 + (lx - offX) / outW * (x1 - x0)
                  gy = y1 - (ly - offY) / outH * (y1 - y0)
               in Just (gx, gy)

nearestPlotHover :: Chart -> Double -> Double -> Maybe PlotHover
nearestPlotHover chart gx gy =
  case chart of
    Chart {chartSeries = []} -> Nothing
    _ | gx < 0 || gx > 1 || gy < 0 || gy > 1 -> Nothing
    Chart {chartSeries = series} ->
      let xDom = chartXDomain chart
          yDom = chartYDomain chart
          dataX = plotToDomain xDom (Range 0 1) gx
          dataY = plotToDomain yDom (Range 0 1) gy
          candidates =
            [ ( si
              , ptIdx
              , x
              , y
              , (x - dataX) * (x - dataX) + (y - dataY) * (y - dataY)
              )
            | (si, s) <- zip [0 ..] series
            , (ptIdx, (x, y)) <- zip [0 ..] (V.toList (seriesPoints chart s))
            ]
       in case candidates of
            [] -> Nothing
            _ ->
              let (si, ptIdx, x, y, _) = minimumBy (\(_, _, _, _, d0) (_, _, _, _, d1) -> compare d0 d1) candidates
               in Just
                    PlotHover
                      { hoverDataX = x
                      , hoverDataY = y
                      , hoverSeriesIdx = si
                      , hoverPointIdx = ptIdx
                      }

minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ [] = error "minimumBy: empty list"
minimumBy _ [x] = x
minimumBy cmp (x : xs) =
  let y = minimumBy cmp xs
   in if cmp x y == GT then y else x
