{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (unless)
import Data.Colour.Names (coral, steelblue)
import Data.List (tails)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Diagrams.Prelude
  ( Diagram
  , circle
  , fc
  , lw
  , lwO
  , none
  , ( # )
  )
import NanoUI
import NanoUI.Diagrams
  ( B
  , PlotStyle (..)
  , colourOf
  , defaultPlotStyle
  , diagram
  , diagramOps
  , fitLayout
  )
import NanoUI.Diagrams.Tessellation (fillPolygon, triangulatePolygon)
import NanoUI.Plot.Chrome (chartDiagram, seriesDomains)
import NanoUI.Plot.Decimate (lttb)
import NanoUI.Plot.Hit (nearestPlotHover)
import NanoUI.Plot.Scale (formatTick, mergeDomains, niceTicks)
import NanoUI.Plot.Series (bar, line, scatter)
import NanoUI.Plot.Types (Chart (..), Domain (..), GridMode (..), LegendPos (..), PlotHover (..))
import NanoUI.Testing (Context, DrawData (..), drawCmdNull, newPixelContext, runFrame)

main :: IO ()
main = do
  ctx <- newPixelContext
  let inp = emptyInput {inputWindowSize = Size 240 120}
      fm = monospaceMetrics 16
  testRendering ctx inp fm
  testConcaveTriangulation
  testRectFastPath
  testNiceTicks
  testMultiSeriesDomains
  testLttb
  testLabelFit fm
  testChartChrome fm
  testPlotHover fm
  putStrLn "nano-ui-diagrams: ok"

testRendering :: Context -> Input -> FontMetrics -> IO ()
testRendering ctx inp fm = do
  let ok d = drawIndexCount d > 0 && not (drawCmdNull d)
  (_, _, filled, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (circle 1 # fc coral # lw none)
  (_, _, stroked, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (linePlotDiag fm [(x, sin x) | x <- [0, 0.2 .. 6.2]])
  (_, _, bars, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (barPlotDiag fm [(1, 2), (2, 5), (3, 3)])
  (_, _, scatterPts, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (scatterPlotDiag fm [(1, 4), (2, 1), (5, 3)])
  unless (ok filled && ok stroked && ok bars && ok scatterPts) $
    fail "diagrams produced no draw commands"
  (_, _, filledAgain, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (circle 1 # fc coral # lw none)
  unless (ok filledAgain) $
    fail "cached diagram produced no draw commands"

linePlotDiag :: FontMetrics -> [(Double, Double)] -> Diagram B
linePlotDiag fm pts =
  chartDiagram fm defaultTheme defaultPlotStyle lineChart
    # lwO 2
    # fc steelblue
  where
    lineChart =
      Chart
        { chartTitle = Nothing
        , chartXTitle = Nothing
        , chartYTitle = Nothing
        , chartSeries = [line "s" pts]
        , chartLegend = LegendNone
        , chartGrid = GridNone
        , chartDecimate = False
        }

barPlotDiag :: FontMetrics -> [(Double, Double)] -> Diagram B
barPlotDiag fm pts =
  chartDiagram fm defaultTheme defaultPlotStyle barChart
    # fc steelblue
    # lw none
  where
    barChart =
      Chart
        { chartTitle = Nothing
        , chartXTitle = Nothing
        , chartYTitle = Nothing
        , chartSeries = [bar "s" (zip (map (T.pack . show . fst) pts) (map snd pts))]
        , chartLegend = LegendNone
        , chartGrid = GridNone
        , chartDecimate = False
        }

scatterPlotDiag :: FontMetrics -> [(Double, Double)] -> Diagram B
scatterPlotDiag fm pts =
  chartDiagram fm defaultTheme defaultPlotStyle scatterChart
    # fc coral
    # lw none
  where
    scatterChart =
      Chart
        { chartTitle = Nothing
        , chartXTitle = Nothing
        , chartYTitle = Nothing
        , chartSeries = [scatter "s" pts]
        , chartLegend = LegendNone
        , chartGrid = GridNone
        , chartDecimate = False
        }

testConcaveTriangulation :: IO ()
testConcaveTriangulation = do
  let star =
        [ (0, 0.5)
        , (0.12, 0.12)
        , (0.5, 0.12)
        , (0.18, -0.08)
        , (0.32, -0.45)
        , (0, -0.18)
        , (-0.32, -0.45)
        , (-0.18, -0.08)
        , (-0.5, 0.12)
        , (-0.12, 0.12)
        ]
      tris = triangulatePolygon star
  unless (length tris >= 8) $
    fail "ear clipping did not triangulate star"
  let areas = [triArea a b c | (a, b, c) <- tris]
  unless (all (> 0) areas) $
    fail "ear clipping produced degenerate triangles"

triArea :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
triArea (x0, y0) (x1, y1) (x2, y2) =
  abs ((x0 - x2) * (y1 - y0) - (x0 - x1) * (y2 - y0)) * 0.5

testRectFastPath :: IO ()
testRectFastPath = do
  let ops = fillPolygon (themeRed defaultTheme) [(0, 0), (10, 0), (10, 5), (0, 5)]
  unless (length ops == 1) $
    fail "axis-aligned rectangle did not use FillRect fast path"
  case ops of
    [FillRect _ _] -> pure ()
    _ -> fail "rectangle fast path emitted wrong op"

testNiceTicks :: IO ()
testNiceTicks = do
  let t0 = niceTicks 6 (Domain 0 100)
      t1 = niceTicks 6 (Domain (-5) 5)
      t2 = niceTicks 6 (Domain 123.4 567.8)
      t3 = niceTicks 6 (Domain 4 4)
  unless (length t0 >= 3 && maybe False (<= 0) (listToMaybe t0) && maybe False (>= 100) (listToMaybe (reverse t0))) $
    fail "nice ticks failed for [0,100]"
  unless (any (== 0) t1) $
    fail "nice ticks failed for [-5,5]"
  unless (length t2 >= 3) $
    fail "nice ticks failed for wide float range"
  unless (length t3 >= 1) $
    fail "nice ticks failed for single-value range"
  unless (formatTick 6 == "6") $
    fail "formatTick integer"

testMultiSeriesDomains :: IO ()
testMultiSeriesDomains = do
  let s1 = line "a" [(0, 0), (1, 1)]
      s2 = line "b" [(0, 10), (1, 20)]
      c =
        Chart
          { chartTitle = Nothing
          , chartXTitle = Nothing
          , chartYTitle = Nothing
          , chartSeries = [s1, s2]
          , chartLegend = LegendRight
          , chartGrid = GridBoth
          , chartDecimate = False
          }
      (Domain xLo xHi, Domain yLo yHi) = seriesDomains c
  unless (yLo <= 0 && yHi >= 20 && xLo <= 0 && xHi >= 1) $
    fail "multi-series domains do not share bounds"
  unless (mergeDomains (Domain 0 1) (Domain 0 10) == Domain 0 10) $
    fail "mergeDomains broken"

testLttb :: IO ()
testLttb = do
  let pts = V.fromList [(fromIntegral i, sin (fromIntegral i / 10)) | i <- [0 .. 9999 :: Int]]
      out = lttb 500 pts
  unless (V.length out == 500) $
    fail "LTTB did not downsample to target count"
  let ys = V.toList (V.map snd out)
  unless (minimum ys < -0.5 && maximum ys > 0.5) $
    fail "LTTB lost waveform extrema"

testLabelFit :: FontMetrics -> IO ()
testLabelFit fm = do
  let dump = chartDiagram fm defaultTheme defaultPlotStyle barChartSample
      fitted = fitLayout fm (fixedH 180 defaultLayout) dump
      ops =
        case (layoutWidth fitted, layoutHeight fitted) of
          (Fixed bw, Fixed bh) -> diagramOps (realToFrac bw) (realToFrac bh) dump
          _ -> V.empty
      texts = [(x, y, ax, ay, t) | DrawText x y ax ay t _ <- V.toList ops]
      xs = [x | (x, _, _, _, _) <- texts]
      boxes = [drawTextBox fm x y ax ay t | (x, y, ax, ay, t) <- texts]
      overlap a b =
        let Rect x1 y1 w1 h1 = a
            Rect x2 y2 w2 h2 = b
         in x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1
  unless (length xs >= 3 && maximum xs - minimum xs > 20) $
    fail "axis labels did not spread along x"
  unless (not (or [overlap a b | (a : rest) <- tails boxes, b <- rest])) $
    fail "axis label boxes overlap"

barChartSample :: Chart
barChartSample =
  Chart
    { chartTitle = Nothing
    , chartXTitle = Just "day"
    , chartYTitle = Just "count"
    , chartSeries = [bar "count" [("Mon", 2), ("Tue", 5), ("Wed", 4), ("Thu", 7), ("Fri", 3)]]
    , chartLegend = LegendRight
    , chartGrid = GridBoth
    , chartDecimate = False
    }

testChartChrome :: FontMetrics -> IO ()
testChartChrome fm = do
  let chart = barChartSample
      ops = diagramOps 400 280 (chartDiagram fm defaultTheme defaultPlotStyle chart)
      chartTexts = [t | DrawText _ _ _ _ t _ <- V.toList ops]
  unless (length chartTexts >= 6) $
    fail "chart chrome dropped tick or title text"
  unless (plotInk defaultPlotStyle == colourOf (themeRed defaultTheme)) $
    fail "defaultPlotStyle ink is not themeRed"
  unless (length (themeSeries defaultTheme) == 6) $
    fail "themeSeries dropped a series colour"

testPlotHover :: FontMetrics -> IO ()
testPlotHover _fm = do
  let c =
        Chart
          { chartTitle = Nothing
          , chartXTitle = Nothing
          , chartYTitle = Nothing
          , chartSeries = [line "a" [(0, 0), (1, 1), (2, 4)]]
          , chartLegend = LegendNone
          , chartGrid = GridNone
          , chartDecimate = False
          }
  case nearestPlotHover c 0.5 0.5 of
    Nothing -> fail "nearestPlotHover missed center point"
    Just h ->
      unless (hoverSeriesIdx h == 0 && hoverPointIdx h == 1 && hoverDataX h == 1 && hoverDataY h == 1) $
        fail "nearestPlotHover picked wrong point"
