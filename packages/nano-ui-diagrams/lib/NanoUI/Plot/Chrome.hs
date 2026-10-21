-- | Chart drawing: axes, ticks, grid, title and legend around the series, as
-- a diagram.
module NanoUI.Plot.Chrome
  ( chartDiagram
  , chartMargins
  , Margins (..)
  , seriesDomains
  , seriesPoints
  ) where

import Data.Colour (Colour)
import Data.Foldable (toList)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
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
  , mapLoc
  , moveTo
  , none
  , p2
  , phantom
  , rect
  , strokeLocTrail
  , translate
  , (^&)
  , ( # )
  )
import NanoUI
  ( Color
  , Rect (..)
  , Theme (..)
  , lerpColor
  , themeSeries
  )
import NanoUI.Backend (FontMetrics (..), drawTextBox, fmLineHeight)
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (PlotStyle (..), colourOf)
import NanoUI.Plot.Decimate (lttb)
import NanoUI.Plot.Scale
  ( domainBoundsBy
  , domainToPlot
  , mergeDomains
  , niceTickLabels
  , padDomain
  , settleDomain
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

-- | Space around the unit data box, in plot coordinates rather than pixels.
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
  , ccLegendW :: !Float
  , ccTopLegend :: !Double
  }

-- | Space needed for titles, ticks, and legend with the supplied font metrics.
-- Prepare those metrics for the chart's text before measuring.
chartMargins :: FontMetrics -> Chart -> Margins
chartMargins fm chart = ccMargins (chartChrome fm (snd (seriesDomains chart)) chart)

-- | Chrome for a chart whose y domain the caller has already computed.
chartChrome :: FontMetrics -> Domain -> Chart -> ChartChrome
chartChrome fm yDom chart =
  let yLabels = map snd (niceTickLabels 6 yDom)
      maxYW = maximum (0 : map (textWidth fm) yLabels)
      lh = fmLineHeight fm
      yTitleW = maybe 0 (textWidth fm) (chartYTitle chart)
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
      -- A top legend takes a row of its own, under the title.
      topLegend =
        case chartLegend chart of
          LegendTop -> px lh + px 6
          _ -> 0
      topM =
        topLegend
          + if chartTitle chart /= Nothing
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
        , ccLegendW = legendW
        , ccTopLegend = topLegend
        }

textWidth :: FontMetrics -> T.Text -> Float
textWidth fm s = rectW (drawTextBox fm 0 0 0 (-1) s)

-- | Combined x/y extents of the series' finite values, with 5% numeric
-- padding. Categories use zero-based positions with half a slot at each
-- end, and their bars' y extent includes zero. An axis with no values spans
-- 0-1 before the padding, and one with a single value half a unit each side
-- of it.
seriesDomains :: Chart -> (Domain, Domain)
seriesDomains chart = (xDom, fit yBounds)
  where
    -- Series merge their bounds before the fallbacks and padding, so an
    -- empty or constant one does not widen the others' span.
    fit = padDomain 0.05 . settleDomain
    (xBounds, yBounds, slots) = foldl' add (Domain (1 / 0) (-1 / 0), Domain (1 / 0) (-1 / 0), 0) (chartSeries chart)
    add (!bx, !by, !n) s = case seriesData s of
      PointsXY pts -> (mergeDomains bx (domainBoundsBy fst pts), mergeDomains by (domainBoundsBy snd pts), n)
      CategoryY _ values ->
        let k = sizeofPrimArray values
         in (bx, mergeDomains by (mergeDomains (Domain 0 0) (domainBoundsBy (indexPrimArray values) (U.enumFromN 0 k))), max n k)
    slotDom = Domain (-0.5) (fromIntegral slots - 0.5)
    xDom
      | slots == 0 = fit xBounds
      | Domain lo hi <- xBounds, lo > hi = slotDom
      | otherwise = mergeDomains slotDom (fit xBounds)

-- | Draw a chart from its 'seriesDomains' and each series' 'seriesPoints'.
chartDiagram :: FontMetrics -> Theme -> PlotStyle -> (Domain, Domain) -> [U.Vector (Double, Double)] -> Chart -> Diagram B
chartDiagram fm theme ps (xDom, yDom) points chart =
  let chrome = chartChrome fm yDom chart
      m = ccMargins chrome
      -- A category series names its slots; other x axes are numbered.
      xTicks = case [labels | Series {seriesData = CategoryY labels _} <- chartSeries chart] of
        labels : _ -> zip [0 ..] (toList labels)
        [] -> niceTickLabels 6 xDom
      yTicks = niceTickLabels 6 yDom
      tickPad = ccTickPad chrome
      xTickPad = ccXTickPad chrome
      toX = domainToPlot xDom
      toY = domainToPlot yDom
      horizontalGrid = mconcat [fromVertices [p2 (0, toY y), p2 (1, toY y)] | (y, _) <- yTicks]
      verticalGrid = mconcat [fromVertices [p2 (toX x, 0), p2 (toX x, 1)] | (x, _) <- xTicks]
      grid =
        case chartGrid chart of
          GridNone -> mempty
          GridHorizontal -> horizontalGrid
          GridVertical -> verticalGrid
          GridBoth -> horizontalGrid <> verticalGrid
      axes =
        fromVertices [p2 (0, 0), p2 (1, 0)]
          <> fromVertices [p2 (0, 0), p2 (0, 1)]
          <> mconcat [fromVertices [p2 (toX x, 0), p2 (toX x, 0.03)] | (x, _) <- xTicks]
          <> mconcat [fromVertices [p2 (0, toY y), p2 (0.03, toY y)] | (y, _) <- yTicks]
      xLabs =
        mconcat
          [ plotLbl ps 0.5 1 (T.unpack t) # moveTo (p2 (toX x, -xTickPad))
          | (x, t) <- xTicks
          ]
      yLabs =
        mconcat
          [ plotLbl ps 1 0.5 (T.unpack t) # moveTo (p2 (-tickPad, toY y))
          | (y, t) <- yTicks
          ]
      title = foldMap (\t -> plotLbl ps 0.5 0 (T.unpack t) # moveTo (p2 (0.5, 1.03 + ccTopLegend chrome))) (chartTitle chart)
      xt = foldMap (\t -> plotLbl ps 0.5 1 (T.unpack t) # moveTo (p2 (0.5, ccXTitleY chrome))) (chartXTitle chart)
      yt = foldMap (\t -> plotLbl ps 1 0.5 (T.unpack t) # moveTo (p2 (ccYTitleX chrome, 0.5))) (chartYTitle chart)
      coloredSeries =
        [ (fromMaybe fallback (seriesColor s), s)
        | (fallback, s) <- zip (cycle (themeSeries theme)) (chartSeries chart)
        ]
      seriesDia =
        mconcat
          [ renderSeries ps color xDom yDom s pts
          | ((color, s), pts) <- zip coloredSeries points
          ]
      legend = renderLegend fm ps coloredSeries chart chrome
      marginBox :: Diagram B
      marginBox =
        rect (1 + marginLeft m + marginRight m) (1 + marginBottom m + marginTop m)
          # alignBL
          # moveTo (p2 (-marginLeft m, -marginBottom m))
      gridDia = grid # lc (plotGrid ps) # lwO (plotStroke 1)
      axesDia = axes # lc (plotMuted ps) # lwO (plotStroke 1)
      -- Diagrams composes front-to-back: keep the grid behind the data.
   in xLabs <> yLabs <> title <> xt <> yt <> legend <> seriesDia <> axesDia <> gridDia <> phantom marginBox

plotLbl :: PlotStyle -> Double -> Double -> String -> Diagram B
plotLbl ps ax ay s =
  alignedText ax ay s # fontSizeL 0.085 # fc (plotMuted ps) # lc (plotMuted ps) # lw none

renderSeries :: PlotStyle -> Color -> Domain -> Domain -> Series -> U.Vector (Double, Double) -> Diagram B
renderSeries ps c xDom yDom s pts =
  let ink = colourOf c
      toP (x, y) = p2 (domainToPlot xDom x, domainToPlot yDom y)
   in case seriesKind s of
        -- Markers are twice the line's width, a default scatter marker's
        -- size on a default line.
        LineSeries w mark ->
          (fromVertices (U.foldr (\p acc -> toP p : acc) [] pts) # lc ink # lwO (plotStroke w))
            <> foldMap (\mk -> U.foldl' (\acc p -> acc <> markShape mk (2 * w) ink (toP p)) mempty pts) mark
        ScatterSeries w mk ->
          U.foldl' (\acc p -> acc <> markShape mk w ink (toP p)) mempty pts
        BarSeries frac ->
          renderBars ink frac xDom yDom pts
        AreaSeries baseline ->
          mconcat [closedPoly (map toP ring) | ring <- areaRings baseline pts]
            # fc (colourOf (lerpColor c (plotFrameBg ps) 0.18))
            # lw none
        StepSeries w ->
          let steps = U.foldr (\((x0, y0), (x1, _)) acc -> toP (x0, y0) : toP (x1, y0) : acc) [toP (U.last pts) | not (U.null pts)] (U.zip pts (U.drop 1 pts))
           in fromVertices steps # lc ink # lwO (plotStroke w)

-- | An area series' fill polygons: for each run of points on one side of
-- the baseline, the points and then the baseline's two ends under them. The
-- baseline is straight, so those two are enough; a point under every
-- sample only doubled the polygon, its triangulation and its outline. A run
-- ends where the data crosses the baseline, at the crossing, so no polygon
-- crosses itself.
areaRings :: Double -> U.Vector (Double, Double) -> [[(Double, Double)]]
areaRings baseline = map closeRun . runs . U.toList
  where
    closeRun run = case run of
      (x0, _) : _ -> run ++ [(fst (last run), baseline), (x0, baseline)]
      [] -> []
    side (_, y) = compare y baseline
    runs (p : ps) = go [p] (side p) p ps
    runs [] = []
    -- The run so far, reversed; its side (EQ while all its points are on
    -- the baseline); and its last point.
    go run _ _ [] = [reverse run]
    go run s prev@(px, py) (q@(qx, qy) : qs)
      | sq == EQ || s == EQ || sq == s = go (q : run) (if s == EQ then sq else s) q qs
      | side prev == EQ = reverse run : go [q, prev] sq q qs
      | otherwise =
          let crossing = (px + (qx - px) * (baseline - py) / (qy - py), baseline)
           in reverse (crossing : run) : go [q, crossing] sq q qs
      where
        sq = side q

-- | Points used for drawing and hit tests, without those that are not
-- finite. Line, area and step data may be decimated; categories become
-- zero-based x positions paired with their values.
seriesPoints :: Chart -> Series -> U.Vector (Double, Double)
seriesPoints chart s =
  case seriesData s of
    PointsXY raw ->
      let pts = finitePoints raw
          k = decimateK (U.length pts)
          -- Markers are not joined, so a dropped one is simply missing.
          thins = case seriesKind s of
            ScatterSeries {} -> False
            _ -> True
       in if chartDecimate chart && thins && U.length pts > k then lttb k pts else pts
    CategoryY _ values ->
       finitePoints (U.generate (sizeofPrimArray values) (\i -> (fromIntegral i, indexPrimArray values i)))
  where
    -- Finite data, the usual case, is not copied.
    finitePoints pts = if U.all ok pts then pts else U.filter ok pts
    ok (x, y) = not (isNaN x || isInfinite x || isNaN y || isInfinite y)

decimateK :: Int -> Int
decimateK n = min n (max 64 (min 2000 (n `div` 2)))

-- | Bars a fraction of their category slot wide, from zero to each value on
-- the chart's axes.
renderBars :: Colour Double -> Float -> Domain -> Domain -> U.Vector (Double, Double) -> Diagram B
renderBars fill frac xDom yDom = U.foldl' (\acc p -> acc <> drawBar p) mempty
  where
    half = realToFrac frac / 2
    base = domainToPlot yDom 0
    drawBar (x, y) =
      let x0 = domainToPlot xDom (x - half)
          x1 = domainToPlot xDom (x + half)
          top = domainToPlot yDom y
       in rect (x1 - x0) (abs (top - base)) # fc fill # lw none # translate (((x0 + x1) / 2) ^& ((base + top) / 2))

-- | A closed polygon through the points. The trail keeps its location: a
-- bare trail would be drawn from the origin rather than its first point.
closedPoly :: [P2 Double] -> Diagram B
closedPoly pts = strokeLocTrail (mapLoc closeTrail (fromVertices pts))

markShape :: MarkShape -> Float -> Colour Double -> P2 Double -> Diagram B
markShape shape w c p = moveTo p $ case shape of
  MarkCircle -> filled (circle (plotMarkerRadius w))
  MarkSquare -> let s = plotMarkerRadius w * 2 in filled (rect s s)
  MarkDiamond ->
    let r = plotMarkerRadius w * 1.4
     in filled (closedPoly [p2 (0, r), p2 (r, 0), p2 (0, -r), p2 (-r, 0)])
  MarkTriangle ->
    let r = plotMarkerRadius w * 1.6
     in filled (closedPoly [p2 (0, r), p2 (-r, -r * 0.6), p2 (r, -r * 0.6)])
  MarkCross ->
    let r = plotMarkerRadius w * 1.4
        stroke' a b = fromVertices [a, b] # lc c # lwO (plotStroke w)
     in stroke' (p2 (-r, -r)) (p2 (r, r)) <> stroke' (p2 (-r, r)) (p2 (r, -r))
  where
    filled d = d # fc c # lw none

renderLegend :: FontMetrics -> PlotStyle -> [(Color, Series)] -> Chart -> ChartChrome -> Diagram B
renderLegend _ _ _ Chart {chartLegend = LegendNone} _ = mempty
renderLegend fm ps coloredSeries chart chrome =
  let px = ccPx chrome
      row = px (fmLineHeight fm + 8)
      col = px (ccLegendW chrome) + 0.22
      botLegendY =
        case chartXTitle chart of
          Nothing -> -(ccXTickPad chrome) - px (fmLineHeight fm) - px 6
          Just _ -> ccXTitleY chrome - px (fmLineHeight fm) - px 6
      position i = case chartLegend chart of
        LegendRight -> (1.04, 0.98 - i * row)
        LegendBottom -> (i * col, botLegendY)
        LegendTop -> (i * col, 1 + px 4 + px (fmLineHeight fm) / 2)
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
