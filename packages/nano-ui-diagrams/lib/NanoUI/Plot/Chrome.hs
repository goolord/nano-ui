{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Chrome
  ( chartDiagram
  , chartMargins
  , Margins (..)
  , chartXDomain
  , chartYDomain
  , seriesDomains
  , seriesPoints
  ) where

import Data.Colour (Colour)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Diagrams.Prelude
  ( Diagram
  , P2
  , alignedText
  , alignBL
  , circle
  , closeTrail
  , fc
  , fontSizeL
  , fromVertices
  , lc
  , lw
  , lwO
  , moveTo
  , none
  , p2
  , phantom
  , rect
  , strokeTrail
  , translate
  , (^&)
  , ( # )
  )
import NanoUI
  ( Color
  , FontMetrics (..)
  , Rect (..)
  , Theme (..)
  , drawTextBox
  , fmLineHeight
  , lerpColor
  , themeSeries
  )
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (PlotStyle (..), colourOf)
import NanoUI.Plot.Decimate (lttb)
import NanoUI.Plot.Scale
  ( domainExtent
  , domainExtentBy
  , domainToPlot
  , formatTick
  , mergeDomains
  , niceTicks
  , padDomain
  )
import NanoUI.Plot.Types
  ( Chart (..)
  , Domain (..)
  , GridMode (..)
  , LegendPos (..)
  , MarkShape (..)
  , Series (..)
  , SeriesData (..)
  , SeriesKind (..)
  )

-- lwO is output pixels. Do not scale into the 0..1 plot box.
plotStroke :: Float -> Double
plotStroke pt = realToFrac (max 1 pt)

-- Marker radius in plot-box units. Sized for ~160-200px charts.
plotMarkerRadius :: Float -> Double
plotMarkerRadius sz = realToFrac sz * 0.006

-- Host glyphs do not scale. Convert px using the intended data-box height.
-- Do not iterate against the full letterbox: that grows pads, shrinks the
-- data box, then grows pads again.
plotGapRef :: FontMetrics -> Double
plotGapRef fm = max 120 (realToFrac (fmLineHeight fm) * 7.5)

data Margins = Margins
  { marginLeft :: !Double
  , marginRight :: !Double
  , marginBottom :: !Double
  , marginTop :: !Double
  }
  deriving (Eq, Show)

data ChartChrome = ChartChrome
  { ccMargins :: !Margins
  , ccTickPad :: !Double
  , ccXTickPad :: !Double
  , ccPx :: Float -> Double
  , ccYTitleX :: !Double
  , ccXTitleY :: !Double
  }

chartMargins :: FontMetrics -> Chart -> Margins
chartMargins fm chart = ccMargins (chartChrome fm (chartYDomain chart) chart)

-- | Chrome for a chart whose y domain the caller has already computed.
chartChrome :: FontMetrics -> Domain -> Chart -> ChartChrome
chartChrome fm yDom chart =
  let yLabels = map formatTick (niceTicks 6 yDom)
      maxYW = maximum (0 : map (textWidth fm) yLabels)
      lh = fmLineHeight fm
      yTitleW =
        case chartYTitle chart of
          Nothing -> 0
          Just t -> textWidth fm t
      legendW =
        case chartLegend chart of
          LegendNone -> 0
          _ ->
            maximum
              ( 0
                  : map (textWidth fm . seriesName) (chartSeries chart)
              )
      s = plotGapRef fm
      px u = realToFrac u / s
      tickPad = px 6
      xTickPad = px 2
      titleGap = px 10
      -- Glyphs grow in plot units when the data box is shorter than
      -- plotGapRef. Pad tick size so titles stay just outside the ticks.
      tickW = px maxYW * 1.35
      tickH = px lh * 1.35
      yTitleX = -tickPad - tickW - titleGap
      xTitleY = -xTickPad - tickH - titleGap
      leftTick = tickPad + tickW + px 4
      botTick = xTickPad + tickH + px 4
      topM =
        if chartTitle chart /= Nothing
          then px lh + px 8
          else px 4
      rightM =
        case chartLegend chart of
          LegendRight -> px legendW + 0.22
          _ -> px 4
      leftTitle =
        if chartYTitle chart /= Nothing then titleGap + px yTitleW else 0
      botTitle =
        if chartXTitle chart /= Nothing then titleGap + tickH else 0
      botLegend =
        case chartLegend chart of
          LegendBottom -> px lh + px 6
          _ -> 0
      leftM = leftTick + leftTitle
      botM = botTick + botTitle + botLegend
   in ChartChrome
        { ccMargins =
            Margins
              { marginLeft = leftM
              , marginRight = rightM
              , marginBottom = botM
              , marginTop = topM
              }
        , ccTickPad = tickPad
        , ccXTickPad = xTickPad
        , ccPx = px
        , ccYTitleX = yTitleX
        , ccXTitleY = xTitleY
        }

textWidth :: FontMetrics -> T.Text -> Float
textWidth fm s = rectW (drawTextBox fm 0 0 0 (-1) s)

seriesDomains :: Chart -> (Domain, Domain)
seriesDomains chart =
  case map seriesExtent (chartSeries chart) of
    [] -> (Domain 0 1, Domain 0 1)
    d : ds -> foldl' mergePair d ds
  where
    mergePair (dx, dy) (xd, yd) = (mergeDomains dx xd, mergeDomains dy yd)

seriesExtent :: Series -> (Domain, Domain)
seriesExtent s =
  case seriesData s of
    PointsXY pts ->
      let (xs, ys) = U.unzip pts
       in (padDomain 0.05 (domainExtent xs), padDomain 0.05 (domainExtent ys))
    CategoryY pts ->
      let n = V.length pts
       in (Domain (-0.5) (fromIntegral n - 0.5), padDomain 0.05 (domainExtentBy snd pts))

chartXDomain :: Chart -> Domain
chartYDomain :: Chart -> Domain
chartXDomain = fst . seriesDomains
chartYDomain = snd . seriesDomains

chartDiagram :: FontMetrics -> Theme -> PlotStyle -> Chart -> Diagram B
chartDiagram fm theme ps chart =
  let chrome = chartChrome fm yDom chart
      margins = ccMargins chrome
      leftM = marginLeft margins
      rightM = marginRight margins
      botM = marginBottom margins
      topM = marginTop margins
      (xDom, yDom) = seriesDomains chart
      xTicks = niceTicks 6 xDom
      yTicks = niceTicks 6 yDom
      tickPad = ccTickPad chrome
      xTickPad = ccXTickPad chrome
      toX = domainToPlot xDom
      toY = domainToPlot yDom
      horizontalGrid = mconcat [fromVertices [p2 (0, toY y), p2 (1, toY y)] | y <- yTicks]
      verticalGrid = mconcat [fromVertices [p2 (toX x, 0), p2 (toX x, 1)] | x <- xTicks]
      grid =
        case chartGrid chart of
          GridNone -> mempty
          GridHorizontal -> horizontalGrid
          GridVertical -> verticalGrid
          GridBoth -> horizontalGrid <> verticalGrid
      axes =
        fromVertices [p2 (0, 0), p2 (1, 0)]
          <> fromVertices [p2 (0, 0), p2 (0, 1)]
          <> mconcat [fromVertices [p2 (toX x, 0), p2 (toX x, 0.03)] | x <- xTicks]
          <> mconcat [fromVertices [p2 (0, toY y), p2 (0.03, toY y)] | y <- yTicks]
      xLabs =
        mconcat
          [ plotLbl ps 0.5 1 (T.unpack (formatTick x)) # moveTo (p2 (toX x, -xTickPad))
          | x <- xTicks
          ]
      yLabs =
        mconcat
          [ plotLbl ps 1 0.5 (T.unpack (formatTick y)) # moveTo (p2 (-tickPad, toY y))
          | y <- yTicks
          ]
      title =
        case chartTitle chart of
          Nothing -> mempty
          Just t -> plotLbl ps 0.5 0 (T.unpack t) # moveTo (p2 (0.5, 1.03))
      xt =
        case chartXTitle chart of
          Nothing -> mempty
          Just t ->
            plotLbl ps 0.5 1 (T.unpack t)
              # moveTo (p2 (0.5, ccXTitleY chrome))
      yt =
        case chartYTitle chart of
          Nothing -> mempty
          Just t ->
            plotLbl ps 1 0.5 (T.unpack t)
              # moveTo (p2 (ccYTitleX chrome, 0.5))
      coloredSeries =
        [ (fromMaybe fallback (seriesColor s), s)
        | (fallback, s) <- zip (cycle (themeSeries theme)) (chartSeries chart)
        ]
      seriesDia =
        mconcat
          [ renderSeries ps color xDom yDom chart s
          | (color, s) <- coloredSeries
          ]
      legend = renderLegend fm ps coloredSeries chart chrome
      marginBox :: Diagram B
      marginBox =
        rect (1 + leftM + rightM) (1 + botM + topM)
          # alignBL
          # moveTo (p2 (-leftM, -botM))
      gridDia = grid # lc (plotGrid ps) # lwO (plotStroke 1)
      axesDia = axes # lc (plotMuted ps) # lwO (plotStroke 1)
      -- Diagrams composes front-to-back: keep the grid behind the data.
   in xLabs <> yLabs <> title <> xt <> yt <> legend <> seriesDia <> axesDia <> gridDia <> phantom marginBox

plotLbl :: PlotStyle -> Double -> Double -> String -> Diagram B
plotLbl ps ax ay s =
  alignedText ax ay s # fontSizeL 0.085 # fc (plotMuted ps) # lc (plotMuted ps) # lw none

renderSeries :: PlotStyle -> Color -> Domain -> Domain -> Chart -> Series -> Diagram B
renderSeries ps c xDom yDom chart s =
  let ink = colourOf c
      fillCol = lerpColor c (plotFrameBg ps) 0.18
      fill = colourOf fillCol
      pts = seriesPoints chart s
      toP (x, y) = p2 (domainToPlot xDom x, domainToPlot yDom y)
   in case seriesKind s of
        LineSeries w _ ->
          fromVertices (U.foldr (\p acc -> toP p : acc) [] pts) # lc ink # lwO (plotStroke w)
        ScatterSeries w mk ->
          U.foldl' (\acc p -> acc <> markShape mk w ink (toP p)) mempty pts
        BarSeries frac ->
          renderBars ink frac pts
        AreaSeries baseline ->
          areaPath baseline xDom yDom pts # fc fill # lw none
        StepSeries w ->
          fromVertices (stepPoints pts xDom yDom) # lc ink # lwO (plotStroke w)

seriesPoints :: Chart -> Series -> U.Vector (Double, Double)
seriesPoints chart s =
  case seriesData s of
    PointsXY pts ->
      let k = decimateK (U.length pts)
       in if chartDecimate chart && U.length pts > k then lttb k pts else pts
    CategoryY rows ->
       U.generate (V.length rows) (\i -> (fromIntegral i, snd (rows V.! i)))

decimateK :: Int -> Int
decimateK n = min n (max 64 (min 2000 (n `div` 2)))

renderBars :: Colour Double -> Float -> U.Vector (Double, Double) -> Diagram B
renderBars fill frac pts
  | U.null pts = mempty
  | otherwise =
      let !len  = U.length pts
          !n    = fromIntegral len :: Double
          !w    = realToFrac frac / n
          !invN = 1.0 / n
          !xOff = 0.5 * invN

          -- Single-pass strict fold for maxY (avoids allocating a list or intermediate vector)
          !maxY = U.foldl' (\ !acc (_, y) -> max acc (abs y)) 1e-9 pts
          !invMaxY = 1.0 / maxY

          drawBar (x, y) =
            let !absY = abs y
                !h    = absY * invMaxY
                !posX = x * invN + xOff
                !posY = signum y * h * 0.5
             in rect w h
                  # fc fill
                  # lw none
                  # translate (posX ^& posY)
       in U.foldl' (\acc p -> acc <> drawBar p) mempty pts

areaPath :: Double -> Domain -> Domain -> U.Vector (Double, Double) -> Diagram B
areaPath baseline xDom yDom pts
  | U.null pts = mempty
  | otherwise =
      let !baseY = domainToPlot yDom baseline
          toTop (!x, !y) = p2 (domainToPlot xDom x, domainToPlot yDom y)
          toBase (!x, !_) = p2 (domainToPlot xDom x, baseY)

          -- Forward traversal builds `top` in order
          top = U.foldr (\p acc -> toTop p : acc) [] pts
          -- Left fold naturally yields reverse order without allocating an intermediate reversed vector
          base = U.foldl' (\acc p -> toBase p : acc) [] pts
       in closedPoly (top ++ base)

closedPoly :: [P2 Double] -> Diagram B
closedPoly pts = fromVertices pts # closeTrail # strokeTrail

stepPoints :: U.Vector (Double, Double) -> Domain -> Domain -> [P2 Double]
stepPoints pts xDom yDom =
  let toP (x, y) = p2 (domainToPlot xDom x, domainToPlot yDom y)
   in U.foldr
        (\((x0, y0), (x1, _)) acc -> toP (x0, y0) : toP (x1, y0) : acc)
        []
        (U.zip pts (U.drop 1 pts))

markShape :: MarkShape -> Float -> Colour Double -> P2 Double -> Diagram B
markShape MarkCircle w c p =
  circle (plotMarkerRadius w) # fc c # lw none # moveTo p
markShape MarkSquare w c p =
  let s = plotMarkerRadius w * 2
   in rect s s # fc c # lw none # moveTo p
markShape MarkDiamond w c p =
  let r = plotMarkerRadius w * 1.4
   in closedPoly [p2 (0, r), p2 (r, 0), p2 (0, -r), p2 (-r, 0)]
        # fc c
        # lw none
        # moveTo p
markShape MarkTriangle w c p =
  let r = plotMarkerRadius w * 1.6
   in closedPoly [p2 (0, r), p2 (-r, -r * 0.6), p2 (r, -r * 0.6)]
        # fc c
        # lw none
        # moveTo p
markShape MarkCross w c p =
  let r = plotMarkerRadius w * 1.4
      sw = plotStroke w
   in ( (fromVertices [p2 (-r, -r), p2 (r, r)] # lc c # lwO sw)
          <> (fromVertices [p2 (-r, r), p2 (r, -r)] # lc c # lwO sw)
      )
        # moveTo p

renderLegend :: FontMetrics -> PlotStyle -> [(Color, Series)] -> Chart -> ChartChrome -> Diagram B
renderLegend _ _ _ Chart {chartLegend = LegendNone} _ = mempty
renderLegend fm ps coloredSeries chart chrome =
  let px = ccPx chrome
      row = px (fmLineHeight fm + 8)
      col =
        let names = map seriesName (chartSeries chart)
            w = maximum (0 : map (textWidth fm) names)
         in px w + 0.22
      botLegendY =
        case chartXTitle chart of
          Nothing -> -(ccXTickPad chrome) - px (fmLineHeight fm) - px 6
          Just _ -> ccXTitleY chrome - px (fmLineHeight fm) - px 6
      position i = case chartLegend chart of
        LegendRight -> (1.04, 0.98 - i * row)
        LegendBottom -> (i * col, botLegendY)
        LegendTop -> (i * col, 1.12)
        LegendInside -> (0.02, 0.98 - i * row)
        LegendNone -> (0, 0)
   in mconcat
        [ legendEntry ps color (T.unpack (seriesName s)) # moveTo (p2 (position i))
        | (i, (color, s)) <- zip [0 ..] coloredSeries
        ]

legendEntry :: PlotStyle -> Color -> String -> Diagram B
legendEntry ps col name =
  (fromVertices [p2 (0, 0), p2 (0.12, 0)] # lc (colourOf col) # lwO (plotStroke 1.5))
    <> (plotLbl ps 0 0.5 name # moveTo (p2 (0.16, 0)))
