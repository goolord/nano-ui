-- | Finding the data point nearest the pointer on a drawn chart.
module NanoUI.Plot.Hit
  ( hitTestChartCached
  , nearestPlotHover
  ) where

import NanoUI (Rect (..), V2, rectContains, v2X, v2Y)
import NanoUI.Diagrams.Backend (letterbox)
import NanoUI.Diagrams.Widget (frameInner)
import NanoUI.Plot.Scale (plotToDomain)
import NanoUI.Plot.Types (Domain, PlotHover (..))
import qualified Data.Vector.Unboxed as U

-- | The hover target under the pointer, for a diagram of envelope @dw@ by
-- @dh@ with extents @(x0, x1)@ and @(y0, y1)@ drawn framed in @widgetRect@.
hitTestChartCached ::
  Double ->
  Double ->
  (Double, Double) ->
  (Double, Double) ->
  (Domain, Domain) ->
  [U.Vector (Double, Double)] ->
  Rect ->
  V2 ->
  Maybe PlotHover
hitTestChartCached dw dh (x0, x1) (y0, y1) domains points widgetRect mouse
  | not (rectContains inner mouse) || w <= 0 || h <= 0 = Nothing
  | lx < offX || ly < offY || lx > offX + outW || ly > offY + outH = Nothing
  | otherwise =
      nearestPlotHover
        domains
        points
        (x0 + (lx - offX) / outW * (x1 - x0))
        (y1 - (ly - offY) / outH * (y1 - y0))
 where
  inner = frameInner widgetRect
  w = realToFrac (rectW inner)
  h = realToFrac (rectH inner)
  (outW, outH, offX, offY) = letterbox dw dh w h
  lx = realToFrac (v2X mouse - rectX inner)
  ly = realToFrac (v2Y mouse - rectY inner)

-- | The nearest drawn point to a position in the unit plot box. Distance is
-- measured in data coordinates, not screen pixels, with no distance cutoff.
-- Ties keep the earlier series/point. Returns 'Nothing' outside the unit box
-- or when every series is empty.
nearestPlotHover :: (Domain, Domain) -> [U.Vector (Double, Double)] -> Double -> Double -> Maybe PlotHover
nearestPlotHover (xDom, yDom) points gx gy
  | gx < 0 || gx > 1 || gy < 0 || gy > 1 = Nothing
  | otherwise = snd <$> scanSeries 0 Nothing points
 where
  dataX = plotToDomain xDom gx
  dataY = plotToDomain yDom gy
  -- The best hover carries its squared distance, so each point computes only
  -- its own and a point that loses allocates nothing.
  scanSeries !_ !best [] = best
  scanSeries !si !best (pts : rest) =
    let pick current !ptIdx (!x, !y) =
          let !dx = x - dataX
              !dy = y - dataY
              !d = dx * dx + dy * dy
           in case current of
                Just (bestD, _) | bestD <= d -> current
                _ -> Just (d, PlotHover x y si ptIdx)
        !best' = U.ifoldl' pick best pts
     in scanSeries (si + 1) best' rest
