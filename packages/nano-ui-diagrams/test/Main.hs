module Main (main) where

import Control.Monad (forM_, unless)
import Data.Colour.Names (coral, steelblue)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.List (tails)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Data.Vector qualified as V
import Diagrams.Prelude
  ( Diagram
  , circle
  , fc
  , lw
  , lwO
  , none
  , (#)
  )
import NanoUI
import NanoUI.Context (Context (..), DrawingCacheState (..), withFontMetrics)
import NanoUI.Context.Types (DrawOpCacheEntry (..))
import NanoUI.Diagrams
  ( B
  , defaultPlotStyle
  , diagram
  , diagramOps
  , fitLayout
  )
import NanoUI.Diagrams.Backend (diagramTextOps)
import NanoUI.Diagrams.Tessellation
  ( strokePolyline
  , triangulatePolygon
  )
import NanoUI.Plot.Chrome
  ( Margins (..)
  , chartDiagram
  , chartMargins
  , seriesDomains
  )
import NanoUI.Plot.Decimate (lttb, minMaxDecimate)
import NanoUI.Plot.Hit (nearestPlotHover)
import NanoUI.Plot.Scale (formatTick, mergeDomains, niceTicks)
import NanoUI.Plot.Series
  ( area
  , bar
  , line
  , scatter
  , withColor
  , withMarker
  )
import NanoUI.Plot.Types
  ( Chart (..)
  , Domain (..)
  , GridMode (..)
  , LegendPos (..)
  , MarkShape (..)
  , PlotHover (..)
  , Series (..)
  )
import NanoUI.Plot.Widget qualified as Plot
import NanoUI.Testing (DrawData (..), drawCmdNull, newPixelContext, runFrame)
import Test.Hspec (describe, hspec, it)

main :: IO ()
main = hspec $ do
  let
    fm = monospaceMetrics 16
  describe "rendering" $ do
    it "draws and redraws a filled diagram" $ do
      ctx <- newPixelContext
      testRendering ctx (emptyInput {inputWindowSize = Size 240 120})
    it "renders chart labels apart from geometry" (testTextOnlyRendering fm)
    it "reuses cached chart content within one context" testChartCache
  describe "tessellation" $ do
    it "triangulates indexed polygons with full coverage" testIndexedTriangulation
    it "covers polyline strokes end to end" testStrokeCoversMidpoint
  describe "scales and domains" $ do
    it "picks and formats nice ticks" testNiceTicks
    it "shares bounds across series" testMultiSeriesDomains
  describe "decimation" $ do
    it "keeps LTTB extrema and endpoints" testLttb
    it "keeps min/max extrema" testMinMaxDecimate
  describe "chart chrome" $ do
    it "keeps labels, titles and legends apart" (testLabelFit fm)
    it "colors legend entries like their series" (testLegendColors fm)
    it "picks the nearest hover point" testPlotHover
    it "fills closed series and markers" (testClosedSeriesFills fm)
    it "caps the height of growing plots" (testGrowPlotHeight fm)

-- | A chart of the given series with no legend, grid or decimation.
bareChart :: [Series] -> Chart
bareChart ss =
  Chart
    { chartTitle = Nothing
    , chartXTitle = Nothing
    , chartYTitle = Nothing
    , chartSeries = ss
    , chartLegend = LegendNone
    , chartGrid = GridNone
    , chartDecimate = False
    }

chartDia :: FontMetrics -> Chart -> Diagram B
chartDia fm = chartDiagram fm defaultTheme defaultPlotStyle

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

testTextOnlyRendering :: FontMetrics -> IO ()
testTextOnlyRendering fm = do
  let
    d = linePlotDiag fm [(0, 0), (1, 2), (2, 1)]
    isText DrawText {} = True
    isText _ = False
  forM_ [(400, 240), (120, 400), (0, 100), (100, -1)] $ \(w, h) -> do
    let
      full = diagramOps w h d
      labels = diagramTextOps w h d
    unless (labels == V.filter isText full) $
      fail "text-only rendering differs from the full render's text"
    unless
      (w <= 0 || h <= 0 || (not (V.null labels) && V.length labels < V.length full)) $
      fail "text-only rendering did not separate chart labels from geometry"

testChartCache :: IO ()
testChartCache = do
  base <- newPixelContext
  other <- newPixelContext
  let
    inp = emptyInput {inputWindowSize = Size 400 240}
    fm = monospaceMetrics 16
    larger = monospaceMetrics 24
    ctx = withFontMetrics base fm
    render c = do
      _ <-
        runFrame c inp $ Plot.lineChart (fixedWH 360 200 defaultLayout) [(0, 0), (1, 1)]
      cache <- readIORef (ctxDrawingCache c)
      pure (map doeContent (toList (dcsDrawOpCache cache)))
  first <- render ctx
  again <- render ctx
  unless (not (null first) && first == again) $
    fail "chart cache did not reuse unchanged content"
  changed <- render (withFontMetrics base larger)
  unless (changed /= first) $
    fail "chart cache ignored changed font metrics"
  independent <- render (withFontMetrics other larger)
  unless (independent == first) $
    fail "chart cache version leaked across contexts"

testRendering :: Context -> Input -> IO ()
testRendering ctx inp = do
  let
    ok d = drawIndexCount d > 0 && not (drawCmdNull d)
  (_, _, filled, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (circle 1 # fc coral # lw none)
  unless (ok filled) $
    fail "diagram produced no draw commands"
  (_, _, filledAgain, _) <-
    runFrame ctx inp $
      diagram (fixedWH 200 80 defaultLayout) (circle 1 # fc coral # lw none)
  unless (ok filledAgain) $
    fail "cached diagram produced no draw commands"

linePlotDiag :: FontMetrics -> [(Double, Double)] -> Diagram B
linePlotDiag fm pts =
  chartDia fm (bareChart [line "s" pts])
    # lwO 2
    # fc steelblue

triArea :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float
triArea (x0, y0) (x1, y1) (x2, y2) =
  abs ((x0 - x2) * (y1 - y0) - (x0 - x1) * (y2 - y0)) * 0.5

testIndexedTriangulation :: IO ()
testIndexedTriangulation = do
  forM_ [[], [(0, 0)], [(0, 0), (1, 1)], [(0, 0), (1, 1), (0, 0)]] $ \pts ->
    unless (null (triangulatePolygon pts)) $
      fail "undersized polygon emitted triangles"
  -- Alternating radii exercise repeated ear removal and wraparound indices.
  forM_ [3, 16, 127, 256 :: Int] $ \n -> do
    let
      points =
        [ let
            angle = 2 * pi * fromIntegral i / fromIntegral n
            radius = if even i then 10 else 6
           in
            (radius * cos angle, radius * sin angle)
        | i <- [0 .. n - 1]
        ]
      polygonArea =
        abs
          (sum [x * y' - x' * y | ((x, y), (x', y')) <- zip points (drop 1 (cycle points))])
          / 2
    forM_ [points, reverse points, points ++ take 1 points] $ \pts -> do
      let
        triangles = triangulatePolygon pts
        areaSum = sum [triArea a b c | (a, b, c) <- triangles]
      unless (length triangles == n - 2 && abs (areaSum - polygonArea) < 0.01) $
        fail
          ( "indexed triangulation changed polygon coverage: "
              ++ show (n, length triangles, areaSum, polygonArea)
          )

testStrokeCoversMidpoint :: IO ()
testStrokeCoversMidpoint = do
  let
    col = themeRed defaultTheme
    ops = strokePolyline col 2 False [(0, 0), (20, 0), (20, 20)]
    tris =
      [ ((x0, y0), (x1, y1), (x2, y2))
      | FillTriangle x0 y0 x1 y1 x2 y2 _ <- ops
      ]
    covered p = any (inTri p) tris
  unless (covered (10, 0) && covered (1, 0) && covered (20, 10)) $
    fail "stroke polyline left a gap along the segment"

inTri ::
  (Float, Float) -> ((Float, Float), (Float, Float), (Float, Float)) -> Bool
inTri p (a, b, c) =
  let
    s = triArea a b c
    s' = triArea p b c + triArea a p c + triArea a b p
   in
    s > 1e-6 && abs (s' - s) <= 1e-3

testNiceTicks :: IO ()
testNiceTicks = do
  let
    t0 = niceTicks 6 (Domain 0 100)
    t1 = niceTicks 6 (Domain (-5) 5)
  unless
    ( maybe False (<= 0) (listToMaybe t0)
        && maybe False (>= 100) (listToMaybe (reverse t0))
    ) $
    fail "nice ticks failed for [0,100]"
  unless (any (== 0) t1) $
    fail "nice ticks failed for [-5,5]"
  unless (formatTick 6 == "6") $
    fail "formatTick integer"
  unless (formatTick 0.2 == "0.2" && formatTick 0.4 == "0.4") $
    fail "formatTick fractional"
  unless (formatTick (0.2 + 0.2 + 0.2) == "0.6") $
    fail "formatTick binary residue"
  unless (formatTick 0.0008 == "0.0008") $
    fail "formatTick small decimal"
  unless (formatTick 1e308 == "1.000e308" && formatTick (-1e308) == "-1.000e308") $
    fail "formatTick overflowed while snapping a finite value"
  unless
    ( niceTicks 6 (Domain (-1e308) 1e308) == []
        && niceTicks 6 (Domain 1e308 1e308) == [1e308]
        && niceTicks 0 (Domain 0 100) == []
    )
    $ fail "niceTicks failed on overflowing, singleton, or empty-budget domains"

testMultiSeriesDomains :: IO ()
testMultiSeriesDomains = do
  let
    s1 = line "a" (V.fromList [(0, 0), (1, 1)])
    s2 = line "b" [(0, 10), (1, 20)]
    (Domain xLo xHi, Domain yLo yHi) = seriesDomains (bareChart [s1, s2])
  unless (yLo <= 0 && yHi >= 20 && xLo <= 0 && xHi >= 1) $
    fail "multi-series domains do not share bounds"
  unless (mergeDomains (Domain 0 1) (Domain 0 10) == Domain 0 10) $
    fail "mergeDomains broken"
  let
    (Domain fitXLo _, Domain fitYLo _) = seriesDomains (bareChart [scatter "s" [(4, 3), (9, 8)]])
  unless (fitXLo > 2 && fitYLo > 1) $
    fail "seriesDomains seeded with 0..1"

testLttb :: IO ()
testLttb = do
  let
    pts =
      V.fromList
        [(fromIntegral i, sin (fromIntegral i / 10)) | i <- [0 .. 9999 :: Int]]
    out = lttb 500 pts
  unless (V.length out == 500) $
    fail "LTTB did not downsample to target count"
  let
    ys = V.toList (V.map snd out)
  unless (minimum ys < -0.5 && maximum ys > 0.5) $
    fail "LTTB lost waveform extrema"
  let
    spike = V.fromList [(x, if x == 1 then 10 else 0) | x <- [0 .. 9]]
  unless (lttb 3 spike == V.fromList [(0, 0), (1, 10), (9, 0)]) $
    fail "LTTB skipped the first bucket's spike"
  forM_ [0 .. 60] $ \n -> forM_ [-1 .. n + 1] $ \k -> do
    let
      input = V.generate n (\i -> (fromIntegral i, sin (fromIntegral i)))
      sampled = lttb k input
    unless (V.length sampled == min n (max 0 k) && orderedPoints sampled) $
      fail "LTTB violated its point budget or input order"
    unless (V.null sampled || V.head sampled == V.head input) $
      fail "LTTB lost the first endpoint"
    unless (V.length sampled < 2 || V.last sampled == V.last input) $
      fail "LTTB lost the last endpoint"

testMinMaxDecimate :: IO ()
testMinMaxDecimate = do
  unless
    (minMaxDecimate 1 (V.fromList [(0, 2), (1, 2), (2, 2)]) == V.singleton (0, 2)) $
    fail "min/max decimation changed equal-extrema tie handling"
  let
    descending = V.fromList [(x, 9 - x) | x <- [0 .. 8]]
  unless
    (minMaxDecimate 2 descending == V.fromList [(0, 9), (4, 5), (5, 4), (8, 1)]) $
    fail "min/max decimation lost extrema or reversed their order"
  forM_ [0 .. 60] $ \n -> forM_ [-1 .. n + 1] $ \k -> do
    let
      input = V.generate n (\i -> (fromIntegral i, sin (fromIntegral i)))
      sampled = minMaxDecimate k input
    unless (V.length sampled <= max 0 (2 * k) && orderedPoints sampled) $
      fail "min/max decimation violated its bucket budget or input order"
    unless (V.all (`V.elem` input) sampled) $
      fail "min/max decimation invented a point"
    unless
      ( V.null sampled
          || ( V.minimum (V.map snd input) == V.minimum (V.map snd sampled)
                 && V.maximum (V.map snd input) == V.maximum (V.map snd sampled)
             )
      )
      $ fail "min/max decimation lost a global extremum"

orderedPoints :: V.Vector (Double, Double) -> Bool
orderedPoints points = V.and (V.zipWith (\a b -> fst a < fst b) points (V.drop 1 points))

testLabelFit :: FontMetrics -> IO ()
testLabelFit fm = do
  let
    dump = chartDia fm barChartSample
    fitted = fitLayout fm (fixedH 180 defaultLayout) dump
    ops =
      case (layoutWidth fitted, layoutHeight fitted) of
        (Fixed bw, Fixed bh) -> diagramOps (realToFrac bw) (realToFrac bh) dump
        _ -> V.empty
    texts = [(x, y, ax, ay, t) | DrawText x y ax ay t _ <- V.toList ops]
    xs = [x | (x, _, _, _, _) <- texts]
    boxes = [drawTextBox fm x y ax ay t | (x, y, ax, ay, t) <- texts]
  unless (length xs >= 3 && maximum xs - minimum xs > 20) $
    fail "axis labels did not spread along x"
  unless (not (or [rectsOverlap a b | (a : rest) <- tails boxes, b <- rest])) $
    fail "axis label boxes overlap"
  let
    sleepChart =
      (bareChart [scatter "focus" [(4, 3), (9, 8)], line "trend" [(4, 3), (9, 8)]])
        { chartLegend = LegendRight
        , chartYTitle = Just "focus"
        , chartXTitle = Just "hours slept"
        }
    legendDump = chartDia fm sleepChart
    legendOps = diagramOps 400 240 legendDump
    tightOps = diagramOps 220 150 legendDump
    barTightOps = diagramOps 220 150 dump
    botChart = sleepChart {chartLegend = LegendBottom}
    botOps = diagramOps 400 240 (chartDia fm botChart)
    tickText t =
      T.all (\c -> c == '-' || c == '.' || c >= '0' && c <= '9') t && not (T.null t)
    overlapTitleTick chart w h drawOps =
      let
        ts =
          [(drawTextBox fm x y ax ay t, t) | DrawText x y ax ay t _ <- V.toList drawOps]
        titles =
          [ b
          | (b@(Rect bx by _ _), t) <- ts
          , (chartXTitle chart == Just t && by < h * 0.45)
              || (chartYTitle chart == Just t && bx < w * 0.4)
          ]
        ticks = [b | (b, t) <- ts, tickText t]
       in
        or [rectsOverlap a b | a <- titles, b <- ticks]
    overlapLegendTick chart w h drawOps =
      let
        names = map seriesName (chartSeries chart)
        ts =
          [(drawTextBox fm x y ax ay t, t) | DrawText x y ax ay t _ <- V.toList drawOps]
        legends =
          [ b
          | (b@(Rect bx by _ _), t) <- ts
          , t `elem` names
          , case chartLegend chart of
              LegendRight -> bx > w * 0.55
              LegendBottom -> by < h * 0.45
              _ -> False
          ]
        ticks = [b | (b, t) <- ts, tickText t]
       in
        or [rectsOverlap a b | a <- legends, b <- ticks]
  unless (not (overlapTitleTick sleepChart 400 240 legendOps)) $
    fail "axis titles overlap ticks"
  unless (not (overlapLegendTick sleepChart 400 240 legendOps)) $
    fail "legend overlaps ticks"
  unless (not (overlapTitleTick sleepChart 220 150 tightOps)) $
    fail "axis titles overlap ticks on a small plot"
  unless (not (overlapTitleTick barChartSample 220 150 barTightOps)) $
    fail "bar axis titles overlap ticks on a small plot"
  unless (not (overlapTitleTick botChart 400 240 botOps)) $
    fail "axis titles overlap ticks with bottom legend"
  unless (not (overlapLegendTick botChart 400 240 botOps)) $
    fail "bottom legend overlaps ticks"
  let
    shortTitles =
      (bareChart [line "sin(x)" [(0, 0), (1, 1)]])
        { chartLegend = LegendRight
        , chartYTitle = Just "y"
        , chartXTitle = Just "x"
        }
    shortM = chartMargins fm shortTitles
  unless (marginLeft shortM < 0.85 && marginBottom shortM < 0.65) $
    fail "short axis titles left a huge gutter"

barChartSample :: Chart
barChartSample =
  (bareChart [bar "count" [("Mon", 2), ("Tue", 5), ("Wed", 4), ("Thu", 7), ("Fri", 3)]])
    { chartXTitle = Just "day"
    , chartYTitle = Just "count"
    , chartLegend = LegendRight
    , chartGrid = GridBoth
    }

testPlotHover :: IO ()
testPlotHover = do
  forM_ [bareChart [], bareChart [line "empty" []]] $ \chart ->
    unless (nearestPlotHover chart 0.5 0.5 == Nothing) $
      fail "empty chart produced a hover target"
  let
    tied = bareChart [line "first" [(0, 0), (0, 0)], line "second" [(0, 0)]]
  unless
    ( fmap (\h -> (hoverSeriesIdx h, hoverPointIdx h)) (nearestPlotHover tied 0.5 0.5)
        == Just (0, 0)
    ) $
    fail "equidistant hover targets did not prefer the first point"
  case nearestPlotHover (bareChart [line "a" [(0, 0), (1, 1), (2, 4)]]) 0.5 0.5 of
    Nothing -> fail "nearestPlotHover missed center point"
    Just h ->
      unless
        ( hoverSeriesIdx h == 0
            && hoverPointIdx h == 1
            && hoverDataX h == 1
            && hoverDataY h == 1
        ) $
        fail "nearestPlotHover picked wrong point"

-- Empty series isolate the legend strokes from data geometry. Every placement
-- must retain the labels and use the same color overrides as the series.
testLegendColors :: FontMetrics -> IO ()
testLegendColors fm = do
  let
    custom = colorRGBA 17 211 83 255
    chart = bareChart [withColor custom (line "custom" []), line "default" []]
    fallback = themeSeries defaultTheme !! 1
  forM_ [LegendNone, LegendRight, LegendBottom, LegendTop, LegendInside] $ \position -> do
    let
      ops = V.toList (diagramOps 400 280 (chartDia fm chart {chartLegend = position}))
      labels =
        [text | DrawText _ _ _ _ text _ <- ops, text == "custom" || text == "default"]
      colors = [color | FillTriangle _ _ _ _ _ _ color <- ops]
    if position == LegendNone
      then
        unless (null labels && custom `notElem` colors) $
          fail "hidden legend rendered entries"
      else do
        unless (length labels == 2 && "custom" `elem` labels && "default" `elem` labels) $
          fail "legend lost or duplicated a series label"
        unless (custom `elem` colors && fallback `elem` colors) $
          fail "legend colors differ from series colors"

fillTriCount :: V.Vector DrawOp -> Int
fillTriCount ops = length [() | FillTriangle {} <- V.toList ops]

testClosedSeriesFills :: FontMetrics -> IO ()
testClosedSeriesFills fm = do
  let
    seriesOps s = diagramOps 200 120 (chartDia fm (bareChart [s]))
    areaOps = seriesOps (area "a" [(0, 1), (1, 2), (2, 0)])
    diamondOps = seriesOps (withMarker MarkDiamond (scatter "d" [(1, 1), (2, 3)]))
    triOps = seriesOps (withMarker MarkTriangle (scatter "t" [(1, 1)]))
    crossOps = seriesOps (withMarker MarkCross (scatter "x" [(8, 8)]))
    ink = fromMaybe (themeRed defaultTheme) (listToMaybe (themeSeries defaultTheme))
    inkXs =
      [ x
      | FillTriangle x0 _ x1 _ x2 _ c <- V.toList crossOps
      , c == ink
      , x <- [x0, x1, x2]
      ]
  unless (fillTriCount areaOps >= 2) $
    fail "area series produced no fill triangles"
  unless (fillTriCount diamondOps >= 2) $
    fail "diamond marker produced no fill"
  unless (fillTriCount triOps >= 1) $
    fail "triangle marker produced no fill"
  unless (not (null inkXs) && maximum inkXs - minimum inkXs < 40) $
    fail "MarkCross arm left at origin"

testGrowPlotHeight :: FontMetrics -> IO ()
testGrowPlotHeight fm = do
  let
    fitted = fitLayout fm (fillW defaultLayout) (chartDia fm barChartSample)
  unless (layoutMinH fitted <= 260 && layoutMaxH fitted <= 260) $
    fail "plot grow height ballooned"
