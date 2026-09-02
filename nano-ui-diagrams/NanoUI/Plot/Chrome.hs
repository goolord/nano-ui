{-# LANGUAGE OverloadedStrings #-}

module NanoUI.Plot.Chrome
  ( chartDiagram
  , chartMargins
  , chartXDomain
  , chartYDomain
  , seriesDomains
  , seriesPoints
  ) where

import Data.Colour (Colour)
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Diagrams.Prelude
  ( Diagram
  , P2
  , alignedText
  , alignBL
  , circle
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
  , translate
  , (^&)
  , ( # )
  )
import NanoUI
  ( Color
  , FontMetrics (..)
  , Rect (..)
  , Theme (..)
  , defaultTheme
  , drawTextBox
  , fmLineHeight
  , lerpColor
  , themeMuted
  , themeSeries
  )
import NanoUI.Diagrams.Backend (B)
import NanoUI.Diagrams.Widget (PlotStyle (..), colourOf)
import NanoUI.Plot.Decimate (lttb)
import NanoUI.Plot.Scale
  ( domainExtent
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
  , Range (..)
  , Series (..)
  , SeriesData (..)
  , SeriesKind (..)
  )

data Margins = Margins
  { marginLeft :: !Double
  , marginRight :: !Double
  , marginBottom :: !Double
  , marginTop :: !Double
  }
  deriving (Eq, Show)

chartMargins :: FontMetrics -> PlotStyle -> Chart -> Margins
chartMargins fm _ chart =
  let yDom = chartYDomain chart
      yLabels = map formatTick (niceTicks 6 yDom)
      maxYW = maximum (map (textWidth fm . T.unpack) yLabels ++ [0 :: Float])
      lh = fmLineHeight fm
      leftM = realToFrac maxYW / 200 + 0.08
      botM = realToFrac lh / 120 + 0.12
      topM = if chartTitle chart /= Nothing then 0.14 else 0.06
      rightM =
        case chartLegend chart of
          LegendRight -> 0.28
          _ -> 0.04
      botExtra = if chartXTitle chart /= Nothing then 0.08 else 0
      leftExtra = if chartYTitle chart /= Nothing then 0.06 else 0
   in Margins
        { marginLeft = leftM + leftExtra
        , marginRight = rightM
        , marginBottom = botM + botExtra
        , marginTop = topM
        }

textWidth :: FontMetrics -> String -> Float
textWidth fm s = rectW (drawTextBox fm 0 0 0 (-1) (T.pack s))

seriesDomains :: Chart -> (Domain, Domain)
seriesDomains chart = foldr addSeries (Domain 0 1, Domain 0 1) (chartSeries chart)
  where
    addSeries s (dx, dy) =
      let (xd, yd) = seriesExtent s
       in (mergeDomains dx xd, mergeDomains dy yd)

seriesExtent :: Series -> (Domain, Domain)
seriesExtent s =
  case seriesData s of
    PointsXY pts ->
      let xs = V.toList (V.map fst pts)
          ys = V.toList (V.map snd pts)
       in (padDomain 0.05 (domainExtent xs), padDomain 0.05 (domainExtent ys))
    CategoryY pts ->
      let ys = V.toList (V.map snd pts)
          n = V.length pts
       in (Domain (-0.5) (fromIntegral n - 0.5), padDomain 0.05 (domainExtent ys))

chartXDomain :: Chart -> Domain
chartYDomain :: Chart -> Domain
chartXDomain = fst . seriesDomains
chartYDomain = snd . seriesDomains

chartDiagram :: FontMetrics -> Theme -> PlotStyle -> Chart -> Diagram B
chartDiagram fm theme ps chart =
  let margins = chartMargins fm ps chart
      leftM = marginLeft margins
      rightM = marginRight margins
      botM = marginBottom margins
      topM = marginTop margins
      xDom = chartXDomain chart
      yDom = chartYDomain chart
      plotRange = Range 0 1
      xTicks = niceTicks 6 xDom
      yTicks = niceTicks 6 yDom
      toX v = domainToPlot xDom plotRange v
      toY v = domainToPlot yDom plotRange v
      grid =
        case chartGrid chart of
          GridNone -> mempty
          GridHorizontal ->
            mconcat [fromVertices [p2 (0, toY y), p2 (1, toY y)] | y <- yTicks]
          GridVertical ->
            mconcat [fromVertices [p2 (toX x, 0), p2 (toX x, 1)] | x <- xTicks]
          GridBoth ->
            mconcat
              [ fromVertices [p2 (0, toY y), p2 (1, toY y)] | y <- yTicks
              ]
              <> mconcat
                [ fromVertices [p2 (toX x, 0), p2 (toX x, 1)] | x <- xTicks
                ]
      axes =
        fromVertices [p2 (0, 0), p2 (1, 0)]
          <> fromVertices [p2 (0, 0), p2 (0, 1)]
          <> mconcat [fromVertices [p2 (toX x, 0), p2 (toX x, 0.03)] | x <- xTicks]
          <> mconcat [fromVertices [p2 (0, toY y), p2 (0.03, toY y)] | y <- yTicks]
      xLabs =
        mconcat
          [ plotLbl ps 0.5 1 (T.unpack (formatTick x)) # moveTo (p2 (toX x, -0.04))
          | x <- xTicks
          ]
      yLabs =
        mconcat
          [ plotLbl ps 1 0.5 (T.unpack (formatTick y)) # moveTo (p2 (-0.04, toY y))
          | y <- yTicks
          ]
      title =
        case chartTitle chart of
          Nothing -> mempty
          Just t -> plotLbl ps 0.5 1 (T.unpack t) # fontSizeL 0.1 # moveTo (p2 (0.5, 1.08))
      xt =
        case chartXTitle chart of
          Nothing -> mempty
          Just t -> plotLbl ps 0.5 1 (T.unpack t) # fontSizeL 0.1 # moveTo (p2 (0.5, -0.14))
      yt =
        case chartYTitle chart of
          Nothing -> mempty
          Just t -> plotLbl ps 0 0.5 (T.unpack t) # fontSizeL 0.1 # moveTo (p2 (-0.18, 0.5))
      colors = themeSeries theme
      seriesDia =
        mconcat
          [ renderSeries ps (colors !! (i `mod` length colors)) xDom yDom chart s
          | (i, s) <- zip [0 ..] (chartSeries chart)
          ]
      legend = renderLegend ps colors chart
      marginBox :: Diagram B
      marginBox =
        rect (1 + leftM + rightM) (1 + botM + topM)
          # alignBL
          # moveTo (p2 (-leftM, -botM))
   in (grid <> axes <> seriesDia <> xLabs <> yLabs <> title <> xt <> yt <> legend)
        # lc (plotGrid ps)
        # lwO 1
        <> phantom marginBox

plotLbl :: PlotStyle -> Double -> Double -> String -> Diagram B
plotLbl ps ax ay s =
  alignedText ax ay s # fontSizeL 0.085 # fc (plotMuted ps) # lc (plotMuted ps) # lw none

renderSeries :: PlotStyle -> Color -> Domain -> Domain -> Chart -> Series -> Diagram B
renderSeries _ col xDom yDom chart s =
  let c = fromMaybe col (seriesColor s)
      ink = colourOf c
      fillCol = lerpColor c (themeMuted defaultTheme) 0.22
      fill = colourOf fillCol
      pts = seriesPoints chart s
      toP (x, y) = p2 (domainToPlot xDom (Range 0 1) x, domainToPlot yDom (Range 0 1) y)
   in case seriesKind s of
        LineSeries w _ ->
          fromVertices (map toP pts) # lc ink # lwO (realToFrac w)
        ScatterSeries w mk ->
          mconcat [markShape mk w ink (toP p) | p <- pts]
        BarSeries frac ->
          renderBars fill frac pts
        AreaSeries baseline ->
          areaPath baseline xDom yDom pts # fc fill # lw none
        StepSeries w ->
          fromVertices (stepPoints pts xDom yDom) # lc ink # lwO (realToFrac w)

seriesPoints :: Chart -> Series -> [(Double, Double)]
seriesPoints chart s =
  case seriesData s of
    PointsXY v ->
      let pts = V.toList v
          k = decimateK (length pts)
       in if chartDecimate chart && length pts > k then V.toList (lttb k v) else pts
    CategoryY v ->
      let rows = V.toList v
       in zip [0 .. fromIntegral (length rows - 1) :: Double] (map snd rows)

decimateK :: Int -> Int
decimateK n = min n (max 64 (min 2000 (n `div` 2)))

renderBars :: Colour Double -> Float -> [(Double, Double)] -> Diagram B
renderBars fill frac pts =
  let n = fromIntegral (length pts) :: Double
      w = realToFrac frac / n
      maxY = maximum (1e-9 : map (abs . snd) pts)
   in mconcat
        [ rect w (abs y / maxY)
            # fc fill
            # lw none
            # translate ((x / n + 0.5 / n) ^& (signum y * abs y / maxY / 2))
        | (x, y) <- pts
        ]

areaPath :: Double -> Domain -> Domain -> [(Double, Double)] -> Diagram B
areaPath baseline xDom yDom pts =
  let baseY = domainToPlot yDom (Range 0 1) baseline
      top = map (\(x, y) -> p2 (domainToPlot xDom (Range 0 1) x, domainToPlot yDom (Range 0 1) y)) pts
      base =
        [ p2 (domainToPlot xDom (Range 0 1) x, baseY)
        | (x, _) <- reverse pts
        ]
   in fromVertices (top ++ base)

stepPoints :: [(Double, Double)] -> Domain -> Domain -> [P2 Double]
stepPoints pts xDom yDom =
  let toP (x, y) = p2 (domainToPlot xDom (Range 0 1) x, domainToPlot yDom (Range 0 1) y)
   in case pts of
        [] -> []
        _ ->
          concat
            [ [toP (x0, y0), toP (x1, y0)]
            | ((x0, y0), (x1, _)) <- zip pts (drop 1 pts)
            ]

markShape :: MarkShape -> Float -> Colour Double -> P2 Double -> Diagram B
markShape MarkCircle w c p = circle (realToFrac w / 200) # fc c # lw none # moveTo p
markShape MarkSquare w c p =
  rect (realToFrac w / 100) (realToFrac w / 100) # fc c # lw none # moveTo p
markShape MarkDiamond _ c p =
  fromVertices [p2 (0, 0.04), p2 (0.04, 0), p2 (0, -0.04), p2 (-0.04, 0)]
    # fc c
    # lw none
    # moveTo p
markShape MarkTriangle _ c p =
  fromVertices [p2 (0, 0.05), p2 (-0.04, -0.03), p2 (0.04, -0.03)]
    # fc c
    # lw none
    # moveTo p
markShape MarkCross w c p =
  (fromVertices [p2 (-0.03, -0.03), p2 (0.03, 0.03)] # lc c # lwO (realToFrac w / 2))
    <> (fromVertices [p2 (-0.03, 0.03), p2 (0.03, -0.03)] # lc c # lwO (realToFrac w / 2))
    # moveTo p

renderLegend :: PlotStyle -> [Color] -> Chart -> Diagram B
renderLegend ps colors chart =
  case chartLegend chart of
    LegendNone -> mempty
    LegendRight ->
      mconcat
        [ legendEntry ps (colors !! (i `mod` length colors)) (T.unpack (seriesName s))
          # moveTo (p2 (1.06, 1 - fromIntegral i * 0.12))
        | (i, s) <- zip [0 ..] (chartSeries chart)
        ]
    LegendBottom ->
      mconcat
        [ legendEntry ps (colors !! (i `mod` length colors)) (T.unpack (seriesName s))
          # moveTo (p2 (fromIntegral i * 0.28, -0.22))
        | (i, s) <- zip [0 ..] (chartSeries chart)
        ]
    LegendTop ->
      mconcat
        [ legendEntry ps (colors !! (i `mod` length colors)) (T.unpack (seriesName s))
          # moveTo (p2 (fromIntegral i * 0.28, 1.14))
        | (i, s) <- zip [0 ..] (chartSeries chart)
        ]
    LegendInside ->
      mconcat
        [ legendEntry ps (colors !! (i `mod` length colors)) (T.unpack (seriesName s))
          # moveTo (p2 (0.02, 1 - fromIntegral i * 0.1))
        | (i, s) <- zip [0 ..] (chartSeries chart)
        ]

legendEntry :: PlotStyle -> Color -> String -> Diagram B
legendEntry ps col name =
  (fromVertices [p2 (0, 0), p2 (0.12, 0)] # lc (colourOf col) # lwO 2)
    <> (plotLbl ps 0 0.5 name # moveTo (p2 (0.16, 0)))
