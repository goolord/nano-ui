{-# LANGUAGE DuplicateRecordFields #-}

module NanoUI.Plot.Hit
  ( hitTestChartCached
  , diagramPointAtWithExtents
  , nearestPlotHover
  ) where

import Diagrams.Core (QDiagram)
import Diagrams.Prelude qualified as Dia
import NanoUI (Rect (..), V2, rectContains, v2X, v2Y)
import NanoUI.Diagrams.Backend (NanoUIBackend, uniformHeight)
import NanoUI.Plot.Chrome (seriesDomains, seriesPoints)
import NanoUI.Plot.Scale (plotToDomain)
import NanoUI.Plot.Types (Chart (..), PlotHover (..))
import qualified Data.Vector.Unboxed as U

diagramBorder :: Float
diagramBorder = 1

hitTestChartCached ::
  QDiagram NanoUIBackend Dia.V2 Double Dia.Any ->
  Double ->
  Double ->
  (Double, Double) ->
  (Double, Double) ->
  Chart ->
  Rect ->
  V2 ->
  Maybe PlotHover
hitTestChartCached d dw dh extX extY chart widgetRect mouse =
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
              w = realToFrac (rectW inner) :: Double
              h = realToFrac (rectH inner)
           in case diagramPointAtWithExtents dw dh extX extY w h d lx ly of
                Nothing -> Nothing
                Just (gx, gy) -> nearestPlotHover chart gx gy

diagramPointAtWithExtents ::
  Double ->
  Double ->
  (Double, Double) ->
  (Double, Double) ->
  Double ->
  Double ->
  QDiagram NanoUIBackend Dia.V2 Double Dia.Any ->
  Float ->
  Float ->
  Maybe (Double, Double)
diagramPointAtWithExtents dw dh (x0, x1) (y0, y1) w h d px py
  | w <= 0 || h <= 0 = Nothing
  | otherwise =
      let outH = uniformHeight w h d
          outW = if dh <= 1e-9 then w else outH * dw / dh
          offX = (w - outW) / 2
          offY = (h - outH) / 2
          lx = realToFrac px
          ly = realToFrac py
       in if lx < offX || ly < offY || lx > offX + outW || ly > offY + outH
            then Nothing
            else
              let gx = x0 + (lx - offX) / outW * (x1 - x0)
                  gy = y1 - (ly - offY) / outH * (y1 - y0)
               in Just (gx, gy)

nearestPlotHover :: Chart -> Double -> Double -> Maybe PlotHover
nearestPlotHover chart gx gy
  | gx < 0 || gx > 1 || gy < 0 || gy > 1 = Nothing
  | otherwise = snd <$> scanSeries 0 Nothing (chartSeries chart)
 where
  (xDom, yDom) = seriesDomains chart
  dataX = plotToDomain xDom gx
  dataY = plotToDomain yDom gy
  -- The best hover carries its squared distance, so each point computes only
  -- its own and a point that loses allocates nothing.
  scanSeries !_ !best [] = best
  scanSeries !si !best (s : rest) =
    let pick current !ptIdx (!x, !y) =
          let !dx = x - dataX
              !dy = y - dataY
              !d = dx * dx + dy * dy
           in case current of
                Just (bestD, _) | bestD <= d -> current
                _ -> Just (d, PlotHover x y si ptIdx)
        !best' = U.ifoldl' pick best (seriesPoints chart s)
     in scanSeries (si + 1) best' rest
