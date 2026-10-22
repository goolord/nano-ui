module Main (main) where

import Control.Monad (forM_, unless)
import Data.Colour (withOpacity)
import Data.Colour.Names (coral, red, steelblue)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.List (nub, sortOn, tails)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Primitive.PrimArray (indexPrimArray, primArrayToList, sizeofPrimArray)
import Data.Text qualified as T
import Data.Primitive.SmallArray (SmallArray, emptySmallArray)
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Diagrams.Prelude qualified as D
import Diagrams.Prelude
  ( Diagram
  , circle
  , fc
  , lw
  , lwO
  , none
  , rect
  , rotateBy
  , (#)
  )
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Context (Context (..), DrawingCacheState (..))
import NanoUI.Internal.Context.Types (DrawOpCacheEntry (..))
import NanoUI.Diagrams
  ( B
  , defaultPlotStyle
  , diagram
  , diagramOps
  , fitLayout
  )
import NanoUI.Diagrams.Backend (diagramTextOps)
import NanoUI.Internal.Path (FillRule (..), Paint (..), Shade (..), fillPathOps, strokePathOps)
import NanoUI.Path qualified as P
import NanoUI.Plot.Chrome
  ( Margins (..)
  , chartDiagram
  , chartMargins
  , seriesDomains
  , seriesPoints
  )
import NanoUI.Plot.Decimate (lttb, minMaxDecimate)
import NanoUI.Plot.Builder qualified as Builder
import NanoUI.Plot.Hit (nearestPlotHover)
import NanoUI.Plot.Scale (domainToPlot, formatTick, mergeDomains, niceTickLabels, niceTicks)
import NanoUI.Plot.Series
  ( area
  , bar
  , line
  , scatter
  , step
  , withBaseline
  , withColor
  , withMarker
  , withStrokeWidth
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
import Test.Hspec (describe, hspec, it)

-- | Fail with @msg@ unless the condition holds.
check :: String -> Bool -> IO ()
check msg ok = unless ok (fail msg)

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
    it "keeps a polyline's points, dropping repeats and the closing point" testStrokePolylinePoints
    it "fills a level rectangle with a rect op" testRectFill
    it "keeps a translucent colour's own alpha and the opacity" testTranslucentFill
    it "cuts a loop inside another out by the fill rule" testFillRule
    it "strokes with the style's caps, joins and dashes" testStrokeStyle
  describe "scales and domains" $ do
    it "picks and formats nice ticks" testNiceTicks
    it "labels ticks with the digits their step needs" (testTickLabels fm)
    it "shares bounds across series" testMultiSeriesDomains
    it "reaches an area's baseline" testAreaBaselineDomain
    it "leaves out values that are not finite" (testNonFinite fm)
    it "settles on a chart holding NaN" testNaNChartIdle
  describe "decimation" $ do
    it "keeps LTTB extrema and endpoints" testLttb
    it "keeps min/max extrema" testMinMaxDecimate
    it "thins connected series, and markers only past 2000" testDecimateKinds
  describe "chart chrome" $ do
    it "keeps labels, titles and legends apart" (testLabelFit fm)
    it "colors legend entries like their series" (testLegendColors fm)
    it "picks the nearest hover point" testPlotHover
    it "fills closed series and markers" (testClosedSeriesFills fm)
    it "draws a step series to its last point" (testStepLastPoint fm)
    it "stands bars on zero under their category names" (testBars fm)
    it "marks a line's points with its marker" (testLineMarkers fm)
    it "fills an area a ring per side of its baseline" (testAreaRings fm)
    it "places filled series where their data is" (testFillPlacement fm)
    it "caps the height of growing plots" (testGrowPlotHeight fm)
    it "sizes a diagram without width by the caller's box" (testDegenerateFit fm)

-- | A chart of the given series with no legend, grid or decimation.
bareChart :: [Series] -> Chart
bareChart ss = (Builder.chart ss) {chartLegend = LegendNone, chartGrid = GridNone, chartDecimate = False}

chartDia :: FontMetrics -> Chart -> Diagram B
chartDia fm c = chartDiagram fm defaultTheme defaultPlotStyle (seriesDomains c) (map (seriesPoints c) (chartSeries c)) c

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
    check "text-only rendering differs from the full render's text" (toList labels == filter isText (toList full))
    check "text-only rendering did not separate chart labels from geometry" $
      w <= 0 || h <= 0 || (not (null labels) && length labels < length full)

testChartCache :: IO ()
testChartCache = do
  base <- newPixelContext
  other <- newPixelContext
  owner <- Plot.newPlotCache
  otherOwner <- Plot.newPlotCache
  let
    inp = emptyInput {inputWindowSize = Size 400 240}
    fm = monospaceMetrics 16
    larger = monospaceMetrics 24
    ctx = withFontMetrics base fm
    render cacheOwner c = do
      _ <-
        runFrame c inp $ Plot.lineChart cacheOwner (fixedWH 360 200) [(0, 0), (1, 1)]
      cache <- readIORef (ctxDrawingCache c)
      pure (map doeContent (toList (dcsDrawOpCache cache)))
  first <- render owner ctx
  again <- render owner ctx
  check "chart cache did not reuse unchanged content" (not (null first) && first == again)
  changed <- render owner (withFontMetrics base larger)
  check "chart cache ignored changed font metrics" (changed /= first)
  independent <- render otherOwner (withFontMetrics other larger)
  check "chart cache version leaked across contexts" (independent == first)

testRendering :: Context -> Input -> IO ()
testRendering ctx inp = do
  -- The second frame draws from the cache.
  forM_ ["diagram", "cached diagram"] $ \what -> do
    (_, dd, _) <- runFrame ctx inp (diagram (fixedWH 200 80) (circle 1 # fc coral # lw none))
    check (what <> " produced no draw commands") (drawIndexCount dd > 0 && not (drawCmdNull dd))

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
  let col = themeRed defaultTheme
      toV (x, y) = NanoUI.V2 x y
      triangles pts =
        [ (corner k, corner (k + 1), corner (k + 2))
        | FillPolygon vs _ tris _ <- fillPathOps 0.5 mempty NonZero (P.polygon (map toV pts)) (Solid col)
        , let corner k = let i = indexPrimArray tris k in (indexPrimArray vs (2 * i), indexPrimArray vs (2 * i + 1))
        , k <- [0, 3 .. sizeofPrimArray tris - 3]
        ]
  forM_ [[], [(0, 0)], [(0, 0), (1, 1)], [(0, 0), (1, 1), (0, 0)]] $ \pts ->
    check "undersized polygon emitted triangles" (null (triangles pts))
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
        tris = triangles pts
        areaSum = sum [triArea a b c | (a, b, c) <- tris]
      unless (length tris == n - 2 && abs (areaSum - polygonArea) < 0.01) $
        fail
          ( "indexed triangulation changed polygon coverage: "
              ++ show (n, length tris, areaSum, polygonArea)
          )

testStrokePolylinePoints :: IO ()
testStrokePolylinePoints = do
  let
    col = themeRed defaultTheme
    stroke w path = strokePathOps 0.5 mempty (P.stroke w) path (Solid col)
  -- One anti-aliased op for the whole line, its repeated point dropped.
  check "stroke polyline changed its points" $ case stroke 2 (P.polyline [NanoUI.V2 0 0, NanoUI.V2 20 0, NanoUI.V2 20 0, NanoUI.V2 20 20]) of
    [StrokePolyline pts 2 False _ _ _ (Flat c)] -> c == col && primArrayToList pts == [0, 0, 20, 0, 20, 20]
    _ -> False
  check "closed stroke polyline repeated its first point" $ case stroke 1 (P.polygon [NanoUI.V2 0 0, NanoUI.V2 10 0, NanoUI.V2 10 10, NanoUI.V2 0 0]) of
    [StrokePolyline pts 1 True _ _ _ _] -> sizeofPrimArray pts == 6
    _ -> False

testRectFill :: IO ()
testRectFill = do
  let fills d = [op | op <- toList (diagramOps 100 100 (d # fc coral # lw none)), isFill op]
      isFill = \case
        FillRect {} -> True
        FillPolygon {} -> True
        _ -> False
  check "a level rectangle is not one rect op" $ case fills (rect 4 2) of
    [FillRect {}] -> True
    _ -> False
  check "a turned rectangle is not one polygon" $ case fills (rect 4 2 # rotateBy (1 / 8)) of
    [FillPolygon {}] -> True
    _ -> False

-- | Colours are straight alpha: half-opaque red keeps its full red channel,
-- whether the alpha is the colour's or the diagram's opacity.
testTranslucentFill :: IO ()
testTranslucentFill = do
  let fillsOf d = mapMaybe inkColor (toList (diagramOps 100 100 (d # lw none)))
      halfRed = colorRGBA 255 0 0 128
  check "a translucent fill was darkened" (fillsOf (circle 1 # D.fcA (red `withOpacity` 0.5)) == [halfRed])
  check "opacity did not reach the fill" (fillsOf (circle 1 # fc red # D.opacity 0.5) == [halfRed])

testFillRule :: IO ()
testFillRule = do
  let ringsOf d = [sizeofPrimArray rings - 1 | FillPolygon _ rings _ _ <- toList (diagramOps 100 100 (d # fc coral # lw none))]
      annulus :: D.Path D.V2 Double
      annulus = D.circle 2 <> D.circle 1
  -- A path's loops fill together. The inner circle is a hole under even-odd,
  -- or under winding when reversed; with the same direction it is filled.
  check "even-odd annulus lost its hole" (ringsOf (D.strokeP annulus # D.fillRule D.EvenOdd) == [2])
  check "winding annulus lost its hole" (ringsOf (D.strokeP (D.circle 2 <> D.reversePath (D.circle 1))) == [2])
  check "winding annulus cut a hole" (ringsOf (D.strokeP annulus) == [1])

testStrokeStyle :: IO ()
testStrokeStyle = do
  let strokes d = [(cap, join, sizeofPrimArray pts) | StrokePolyline pts _ _ cap join _ _ <- toList (diagramOps 100 100 (d # D.lc steelblue))]
      zigzag = D.fromVertices [D.p2 (0, 0), D.p2 (1, 0), D.p2 (1, 1)] :: Diagram B
  check "a line's cap and join were not the style's" $
    map (\(c, j, _) -> (c, j)) (strokes (zigzag # D.lineCap D.LineCapRound # D.lineJoin D.LineJoinRound)) == [(P.RoundCap, P.RoundJoin)]
  check "a line's default cap and join changed" $
    map (\(c, j, _) -> (c, j)) (strokes zigzag) == [(P.ButtCap, P.MiterJoin)]
  check "a dashed line was not cut into dashes" (length (strokes (zigzag # D.dashingO [4, 4] 0)) > 4)

testNiceTicks :: IO ()
testNiceTicks = do
  let
    t0 = niceTicks 6 (Domain 0 100)
    t1 = niceTicks 6 (Domain (-5) 5)
  check "nice ticks failed for [0,100]" $
    maybe False (<= 0) (listToMaybe t0)
      && maybe False (>= 100) (listToMaybe (reverse t0))
  check "nice ticks failed for [-5,5]" (any (== 0) t1)
  check "formatTick integer" (formatTick 6 == "6")
  check "formatTick fractional" (formatTick 0.2 == "0.2" && formatTick 0.4 == "0.4")
  check "formatTick binary residue" (formatTick (0.2 + 0.2 + 0.2) == "0.6")
  check "formatTick small decimal" (formatTick 0.0008 == "0.0008")
  check "formatTick overflowed while snapping a finite value" $
    formatTick 1e308 == "1.000e308" && formatTick (-1e308) == "-1.000e308"
  check "niceTicks failed on overflowing, singleton, or empty-budget domains" $
    niceTicks 6 (Domain (-1e308) 1e308) == []
      && niceTicks 6 (Domain 1e308 1e308) == [1e308]
      && niceTicks 0 (Domain 0 100) == []

-- | Neighbouring tick labels differ however fine the step or large the
-- values' common offset, as for Unix time over one day.
testTickLabels :: FontMetrics -> IO ()
testTickLabels fm = do
  let
    yLabels pts = [t | DrawText _ _ 1 _ t _ <- toList (diagramOps 400 240 (chartDia fm (bareChart [line "s" pts])))]
    distinct ts = length ts >= 3 && length (nub ts) == length ts
  check "fine ticks lost their digits" (distinct (yLabels [(0, 1), (1, 1.0000004)]))
  check "a large offset hid the ticks' step" (distinct (yLabels [(0, 1700000000), (1, 1700086400)]))
  check "formatTick snapped a small value before choosing its form" (formatTick 1.5e-10 == "1.500e-10")
  let labels d = map snd (niceTickLabels 6 d)
  check "tick labels carry noise or spare digits" $
    labels (Domain 0 1) == ["0", "0.2", "0.4", "0.6", "0.8", "1"]
      && labels (Domain 1e-8 5e-8) == ["1e-8", "2e-8", "3e-8", "4e-8", "5e-8"]
      && labels (Domain 1700000000 1700086400) == ["1.7e9", "1.70002e9", "1.70004e9", "1.70006e9", "1.70008e9"]

testMultiSeriesDomains :: IO ()
testMultiSeriesDomains = do
  let
    s1 = line "a" (V.fromList [(0, 0), (1, 1)])
    s2 = line "b" [(0, 10), (1, 20)]
    (Domain xLo xHi, Domain yLo yHi) = seriesDomains (bareChart [s1, s2])
  check "multi-series domains do not share bounds" (yLo <= 0 && yHi >= 20 && xLo <= 0 && xHi >= 1)
  check "mergeDomains broken" (mergeDomains (Domain 0 1) (Domain 0 10) == Domain 0 10)
  let
    (Domain fitXLo _, Domain fitYLo _) = seriesDomains (bareChart [scatter "s" [(4, 3), (9, 8)]])
  check "seriesDomains seeded with 0..1" (fitXLo > 2 && fitYLo > 1)
  -- A flat target line or a series still loading leaves the others' span.
  let
    (_, Domain flatLo flatHi) = seriesDomains (bareChart [line "p" [(0, 0.10), (1, 0.15)], line "target" [(0, 0.12), (1, 0.12)]])
    (Domain loadingXLo _, Domain loadingYLo _) = seriesDomains (bareChart [line "a" [(1000, 1000), (2000, 2000)], line "b" []])
  check "a constant series widened the shared domain" (flatLo > 0.09 && flatHi < 0.16)
  check "an empty series pulled the shared domain to zero" (loadingXLo > 900 && loadingYLo > 900)

-- | An area's fill reaches its baseline, so its y domain does too, and the
-- fill stays in the plot. An empty area or a baseline that is not finite
-- leaves the domain alone.
testAreaBaselineDomain :: IO ()
testAreaBaselineDomain = do
  let
    yDom ss = snd (seriesDomains (bareChart ss))
    Domain lo hi = yDom [area "a" [(0, 50), (1, 55), (2, 60)]]
    Domain _ aboveHi = yDom [withBaseline 80 (area "a" [(0, 50), (1, 60)])]
    Domain loadingLo _ = yDom [line "a" [(0, 1000), (1, 2000)], area "b" []]
    Domain nanLo nanHi = yDom [withBaseline (0 / 0) (area "a" [(0, 50), (1, 60)])]
  check "an area's baseline is outside its y domain" (lo <= 0 && hi >= 60 && aboveHi >= 80)
  check "an empty area pulled the shared domain to its baseline" (loadingLo > 900)
  check "a NaN baseline reached the y domain" (nanLo > 40 && nanHi < 70)

-- | Values that are not finite are left out of the domains, the drawing and
-- hover, instead of corrupting them.
testNonFinite :: FontMetrics -> IO ()
testNonFinite fm = do
  let
    nan = 0 / 0
    finite x = not (isNaN x || isInfinite x)
    (_, Domain afterNaNLo _) = seriesDomains (bareChart [line "s" [(0, -100), (1, nan), (2, 5), (3, 6)]])
    (Domain xLo xHi, Domain yLo yHi) = seriesDomains (bareChart [line "s" [(0, 1), (1, 1 / 0), (2, 3)]])
    holes = bareChart [line "s" [(0, 0), (1, nan), (2, 10)]]
    hovered = nearestPlotHover (seriesDomains holes) (map (seriesPoints holes) (chartSeries holes)) 0.05 0.05
    strokes = [primArrayToList p | StrokePolyline p _ _ _ _ _ _ <- toList (diagramOps 200 120 (chartDia fm holes))]
  check "a NaN dropped the domain's earlier minimum" (afterNaNLo <= -100)
  check "an infinite value made the domain infinite" (all finite [xLo, xHi, yLo, yHi] && yHi >= 3)
  check "hover picked a point past a NaN" (fmap hoverDataX hovered == Just 0)
  check "a NaN point reached the drawing" (all (all finite) strokes)

-- | A chart holding NaN equals a rebuilt copy of itself, so a plot built
-- from it every frame settles instead of rebuilding and asking for another
-- frame forever.
testNaNChartIdle :: IO ()
testNaNChartIdle = do
  base <- newPixelContext
  owner <- Plot.newPlotCache
  let
    nan = 0 / 0
    nanF = 0 / 0 :: Float
    held = bareChart [line "l" [(0, nan)], bar "b" [("x", nan)]]
    -- A NaN in a series' own parameters, not its data.
    params = bareChart [withBaseline nan (area "a" [(0, 1)]), withStrokeWidth nanF (line "l" [(0, 1)])]
    ctx = withFontMetrics base (monospaceMetrics 16)
    -- The points depend on the frame, so each frame builds a new chart.
    frame k = runFrame ctx (emptyInput {inputWindowSize = Size 400 240}) $ do
      _ <- Plot.lineChart owner (fixedWH 360 100) [(0, 0), (1, nan * fromIntegral (k :: Int)), (2, 1)]
      Plot.plot owner (fixedWH 360 100) (bareChart [withBaseline (nan * fromIntegral k) (area "a" [(0, 0), (1, 1)])])
  check "a chart holding NaN differs from itself" (held == held)
  check "a chart with a NaN baseline or width differs from itself" (params == params)
  again <- mapM frame [1 .. 4]
  check "a plot of a chart holding NaN asks for frames forever" (not (or [more | (_, _, more) <- drop 2 again]))

testLttb :: IO ()
testLttb = do
  let
    pts =
      V.fromList
        [(fromIntegral i, sin (fromIntegral i / 10)) | i <- [0 .. 9999 :: Int]]
    out = lttb 500 pts
  check "LTTB did not downsample to target count" (V.length out == 500)
  let
    ys = toList (V.map snd out)
  check "LTTB lost waveform extrema" (minimum ys < -0.5 && maximum ys > 0.5)
  let
    spike = V.fromList [(x, if x == 1 then 10 else 0) | x <- [0 .. 9]]
  check "LTTB skipped the first bucket's spike" (lttb 3 spike == V.fromList [(0, 0), (1, 10), (9, 0)])
  forM_ [0 .. 60] $ \n -> forM_ [-1 .. n + 1] $ \k -> do
    let
      input = V.generate n (\i -> (fromIntegral i, sin (fromIntegral i)))
      sampled = lttb k input
    check "LTTB violated its point budget or input order" (V.length sampled == min n (max 0 k) && orderedPoints sampled)
    check "LTTB lost the first endpoint" (V.null sampled || V.head sampled == V.head input)
    check "LTTB lost the last endpoint" (V.length sampled < 2 || V.last sampled == V.last input)

testMinMaxDecimate :: IO ()
testMinMaxDecimate = do
  check "min/max decimation changed equal-extrema tie handling" $
    minMaxDecimate 1 (V.fromList [(0, 2), (1, 2), (2, 2)]) == V.singleton (0, 2)
  let
    descending = V.fromList [(x, 9 - x) | x <- [0 .. 8]]
  check "min/max decimation lost extrema or reversed their order" $
    minMaxDecimate 2 descending == V.fromList [(0, 9), (4, 5), (5, 4), (8, 1)]
  forM_ [0 .. 60] $ \n -> forM_ [-1 .. n + 1] $ \k -> do
    let
      input = V.generate n (\i -> (fromIntegral i, sin (fromIntegral i)))
      sampled = minMaxDecimate k input
    check "min/max decimation violated its bucket budget or input order" $
      V.length sampled <= max 0 (2 * k) && orderedPoints sampled
    check "min/max decimation invented a point" (V.all (`V.elem` input) sampled)
    check "min/max decimation lost a global extremum" $
      V.null sampled
        || ( V.minimum (V.map snd input) == V.minimum (V.map snd sampled)
               && V.maximum (V.map snd input) == V.maximum (V.map snd sampled)
           )

-- | Default decimation thins a long line, but every marker of a scatter or a
-- marked line stays up to 2000: unconnected markers that are dropped are
-- simply missing.
testDecimateKinds :: IO ()
testDecimateKinds = do
  let
    pts n = [(fromIntegral i, sin (fromIntegral i)) | i <- [0 .. n - 1 :: Int]]
    drawn s = U.length (seriesPoints (Builder.chart [s]) s)
  check "decimation did not thin a long line" (drawn (line "l" (pts 100)) < 100)
  check "decimation thinned a scatter series" (drawn (scatter "s" (pts 100)) == 100)
  check "decimation thinned a marked line" (drawn (withMarker MarkCircle (line "l" (pts 100))) == 100)
  check "decimation kept every point of a huge scatter" (drawn (scatter "s" (pts 5000)) == 2000)

orderedPoints :: V.Vector (Double, Double) -> Bool
orderedPoints points = V.and (V.zipWith (\a b -> fst a < fst b) points (V.drop 1 points))

testLabelFit :: FontMetrics -> IO ()
testLabelFit fm = do
  let
    dump = chartDia fm barChartSample
    -- Tall enough for the fit, at most twice the size, to part five
    -- three-letter days in glyphs as wide as they are high.
    fitted = fitLayout fm (fixedH 200 defaultLayout) dump
    ops =
      case (layoutWidth fitted, layoutHeight fitted) of
        (Fixed bw, Fixed bh) -> diagramOps (realToFrac bw) (realToFrac bh) dump
        _ -> emptySmallArray
    texts = [(x, y, ax, ay, t) | DrawText x y ax ay t _ <- toList ops]
    xs = [x | (x, _, _, _, _) <- texts]
    boxes = [drawTextBox fm x y ax ay t | (x, y, ax, ay, t) <- texts]
  check "axis labels did not spread along x" (length xs >= 3 && maximum xs - minimum xs > 20)
  check "axis label boxes overlap" (not (or [rectsOverlap a b | (a : rest) <- tails boxes, b <- rest]))
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
    -- Whether a label selected by @picks@ overlaps a tick label.
    overlapsTicks picks drawOps =
      let
        ts = [(drawTextBox fm x y ax ay t, t) | DrawText x y ax ay t _ <- toList drawOps]
       in
        or [rectsOverlap a b | (a, t) <- ts, picks a t, (b, t') <- ts, tickText t']
    overlapTitleTick c w h =
      overlapsTicks $ \(Rect bx by _ _) t ->
        (chartXTitle c == Just t && by > h * 0.55) || (chartYTitle c == Just t && bx < w * 0.4)
    overlapLegendTick c w h =
      overlapsTicks $ \(Rect bx by _ _) t ->
        t `elem` map seriesName (chartSeries c)
          && case chartLegend c of
            LegendRight -> bx > w * 0.55
            LegendBottom -> by > h * 0.55
            _ -> False
  check "axis titles overlap ticks" (not (overlapTitleTick sleepChart 400 240 legendOps))
  check "legend overlaps ticks" (not (overlapLegendTick sleepChart 400 240 legendOps))
  check "axis titles overlap ticks on a small plot" (not (overlapTitleTick sleepChart 220 150 tightOps))
  check "bar axis titles overlap ticks on a small plot" (not (overlapTitleTick barChartSample 220 150 barTightOps))
  check "axis titles overlap ticks with bottom legend" (not (overlapTitleTick botChart 400 240 botOps))
  check "bottom legend overlaps ticks" (not (overlapLegendTick botChart 400 240 botOps))
  -- A top legend has a row of its own, under the title and inside the
  -- drawing, also where the drawing is as tall as its box.
  let
    topChart = (bareChart [line "in" [(0, 20), (1, 22)], line "out" [(0, 8), (1, 14)]]) {chartLegend = LegendTop}
    topBoxes c w h = [(t, drawTextBox fm x y ax ay t) | DrawText x y ax ay t _ <- toList (diagramOps w h (chartDia fm c))]
    titled = topChart {chartTitle = Just "Temperature"}
    entries c w h = [b | (t, b) <- topBoxes c w h, t == "in" || t == "out"]
  check "top legend overlaps the title" $
    not (or [rectsOverlap a b | (t, a) <- topBoxes titled 400 240, t == "Temperature", b <- entries titled 400 240])
  check "top legend spills above the drawing" $
    all (\(Rect _ y _ _) -> y >= 0) (entries topChart 400 150 ++ entries titled 400 150)
  let
    shortTitles =
      (bareChart [line "sin(x)" [(0, 0), (1, 1)]])
        { chartLegend = LegendRight
        , chartYTitle = Just "y"
        , chartXTitle = Just "x"
        }
    shortM = chartMargins fm shortTitles
  check "short axis titles left a huge gutter" (marginLeft shortM < 0.85 && marginBottom shortM < 0.65)

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
  let hoverAt c = nearestPlotHover (seriesDomains c) (map (seriesPoints c) (chartSeries c)) 0.5 0.5
  forM_ [bareChart [], bareChart [line "empty" []]] $ \chart ->
    check "empty chart produced a hover target" (hoverAt chart == Nothing)
  let
    tied = bareChart [line "first" [(0, 0), (0, 0)], line "second" [(0, 0)]]
  check "equidistant hover targets did not prefer the first point" $
    fmap (\h -> (hoverSeriesIdx h, hoverPointIdx h)) (hoverAt tied)
      == Just (0, 0)
  case hoverAt (bareChart [line "a" [(0, 0), (1, 1), (2, 4)]]) of
    Nothing -> fail "nearestPlotHover missed center point"
    Just h ->
      check "nearestPlotHover picked wrong point" $
        hoverSeriesIdx h == 0
          && hoverPointIdx h == 1
          && hoverDataX h == 1
          && hoverDataY h == 1
  -- Just above the first point, the second is far off on screen but nearer
  -- in data units, where y spans a thousand times x.
  let
    spread = bareChart [scatter "s" [(0.1, 500), (0.9, 505), (0, 0), (1, 1000)]]
    (xDom, yDom) = seriesDomains spread
    hovered = nearestPlotHover (xDom, yDom) (map (seriesPoints spread) (chartSeries spread)) (domainToPlot xDom 0.1) (domainToPlot yDom 510)
  check "hover picked a point far away on screen" (fmap hoverPointIdx hovered == Just 0)

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
      ops = toList (diagramOps 400 280 (chartDia fm chart {chartLegend = position}))
      labels =
        [text | DrawText _ _ _ _ text _ <- ops, text == "custom" || text == "default"]
      colors = mapMaybe inkColor ops
    if position == LegendNone
      then
        check "hidden legend rendered entries" (null labels && custom `notElem` colors)
      else do
        check "legend lost or duplicated a series label" (length labels == 2 && "custom" `elem` labels && "default" `elem` labels)
        check "legend colors differ from series colors" (custom `elem` colors && fallback `elem` colors)

-- | The colour of a polygon fill or a polyline stroke.
inkColor :: DrawOp -> Maybe Color
inkColor (FillPolygon _ _ _ (Flat c)) = Just c
inkColor (StrokePolyline _ _ _ _ _ _ (Flat c)) = Just c
inkColor _ = Nothing

-- | Triangles across the polygon fills.
fillTriCount :: SmallArray DrawOp -> Int
fillTriCount ops = sum [sizeofPrimArray tris `div` 3 | FillPolygon _ _ tris _ <- toList ops]

-- | Every x of the polygon fills and polyline strokes in @c@.
inkXsOf :: Color -> SmallArray DrawOp -> [Float]
inkXsOf c ops =
  [ x
  | op <- toList ops
  , inkColor op == Just c
  , pts <- case op of
      FillPolygon p _ _ _ -> [p]
      StrokePolyline p _ _ _ _ _ _ -> [p]
      _ -> []
  , (i, x) <- zip [0 :: Int ..] (primArrayToList pts)
  , even i
  ]

testClosedSeriesFills :: FontMetrics -> IO ()
testClosedSeriesFills fm = do
  let
    seriesOps s = diagramOps 200 120 (chartDia fm (bareChart [s]))
    areaOps = seriesOps (area "a" [(0, 1), (1, 2), (2, 0)])
    diamondOps = seriesOps (withMarker MarkDiamond (scatter "d" [(1, 1), (2, 3)]))
    triOps = seriesOps (withMarker MarkTriangle (scatter "t" [(1, 1)]))
    crossOps = seriesOps (withMarker MarkCross (scatter "x" [(8, 8)]))
    inkXs = inkXsOf seriesInk crossOps
  check "area series produced no fill triangles" (fillTriCount areaOps >= 2)
  check "diamond marker produced no fill" (fillTriCount diamondOps >= 2)
  check "triangle marker produced no fill" (fillTriCount triOps >= 1)
  check "MarkCross arm left at origin" (not (null inkXs) && maximum inkXs - minimum inkXs < 40)

-- | The first theme series colour.
seriesInk :: Color
seriesInk = fromMaybe (themeRed defaultTheme) (listToMaybe (themeSeries defaultTheme))

-- | A step series ends with the riser to its last point, the highest here.
testStepLastPoint :: FontMetrics -> IO ()
testStepLastPoint fm = do
  let
    ops = diagramOps 200 120 (chartDia fm (bareChart [step "s" [(0, 0), (1, 1), (2, 5)]]))
    ys = [[indexPrimArray p k | k <- [1, 3 .. sizeofPrimArray p - 1]] | op@(StrokePolyline p _ _ _ _ _ _) <- toList ops, inkColor op == Just seriesInk]
  check "step series stopped short of its last point" $ case ys of
    [path] -> length path == 5 && last path == minimum path
    _ -> False

-- | Bars rise from the y axis' zero to their values on it, and the x axis
-- names their categories.
testBars :: FontMetrics -> IO ()
testBars fm = do
  let
    ops = toList (diagramOps 400 240 (chartDia fm (bareChart [bar "count" [("Mon", 2), ("Tue", 5), ("Wed", 4), ("Thu", 7), ("Fri", 3)]])))
    -- A y tick label is right-aligned, level with its tick.
    tickY t = listToMaybe [y | DrawText _ y 1 _ t' _ <- ops, t' == t]
    bars = map snd (sortOn fst [(rectX r, (rectY r, rectY r + rectH r)) | FillRect r c <- ops, c == seriesInk])
    near a b = abs (a - b) < 0.5
  check "bar categories are not on the x axis" (all (`elem` [t | DrawText _ _ _ _ t _ <- ops]) ["Mon", "Tue", "Wed", "Thu", "Fri"])
  check "bars disagree with the y axis" $ case (tickY "0", tickY "2", tickY "4", bars) of
    (Just y0, Just y2, Just y4, [(top2, _), _, (top4, _), _, _]) -> all (near y0 . snd) bars && near top2 y2 && near top4 y4
    _ -> False

testLineMarkers :: FontMetrics -> IO ()
testLineMarkers fm = do
  let
    ops = diagramOps 200 120 (chartDia fm (bareChart [withMarker MarkDiamond (line "l" [(0, 0), (1, 1), (2, 0)])]))
    diamonds = [() | op@(FillPolygon p _ _ _) <- toList ops, inkColor op == Just seriesInk, sizeofPrimArray p == 8]
  check "line markers were not drawn" (length diamonds == 3)

-- | An area series fills a ring per run of samples on one side of its
-- baseline, split where the data crosses it: the samples and the baseline's
-- two ends, triangulated into as many triangles as the ring has sides less
-- two, covering it exactly.
testAreaRings :: FontMetrics -> IO ()
testAreaRings fm = do
  let
    rings samples = [(p, t) | FillPolygon p _ t _ <- toList (diagramOps 200 120 (chartDia fm (bareChart [area "a" samples])))]
    exact (p, t) =
      let ring = sizeofPrimArray p `div` 2
          at k = (indexPrimArray p (2 * k), indexPrimArray p (2 * k + 1))
          shoelace = abs (sum [x0 * y1 - x1 * y0 | k <- [0 .. ring - 1], let (x0, y0) = at k; (x1, y1) = at ((k + 1) `mod` ring)]) / 2
          covered = sum [triArea (at (indexPrimArray t k)) (at (indexPrimArray t (k + 1))) (at (indexPrimArray t (k + 2))) | k <- [0, 3 .. sizeofPrimArray t - 3]]
       in sizeofPrimArray t `div` 3 == ring - 2 && abs (covered - shoelace) <= 1e-3 * shoelace
    above = [(x, 1 + sin x) | x <- [0, 0.5 .. 10]]
    -- sin crosses its baseline at pi, 2 pi and 3 pi.
    crossing = [(x, sin x) | x <- [0, 0.5 .. 10]]
  case rings above of
    [r@(p, _)] -> do
      check "area ring has more than its samples and the baseline's ends" (sizeofPrimArray p `div` 2 <= length above + 2)
      check "area triangles do not cover their ring" (exact r)
    polys -> fail ("expected one area polygon, got " <> show (length polys))
  check "crossing area is not a ring per side" (length (rings crossing) == 4)
  check "crossing area triangles do not cover their rings" (all exact (rings crossing))

-- | Filled series shapes sit where their data is: an area starts at the
-- line through the same samples, and a diamond marker is centred on its
-- point.
testFillPlacement :: FontMetrics -> IO ()
testFillPlacement fm = do
  let
    samples = [(x, 1 + sin x) | x <- [0, 0.5 .. 10]]
    ops = toList (diagramOps 200 120 (chartDia fm (bareChart [area "a" samples, line "l" samples, withMarker MarkDiamond (scatter "d" samples)])))
    pointsOf p = [(indexPrimArray p (2 * k), indexPrimArray p (2 * k + 1)) | k <- [0 .. sizeofPrimArray p `div` 2 - 1]]
    near (x0, y0) (x1, y1) = abs (x1 - x0) < 1e-3 && abs (y1 - y0) < 1e-3
    centre ps = (sum (map fst ps) / fromIntegral (length ps), sum (map snd ps) / fromIntegral (length ps))
    linePts = [pointsOf p | StrokePolyline p _ _ _ _ _ _ <- ops, sizeofPrimArray p == 2 * length samples]
    areaPts = [pointsOf p | FillPolygon p _ _ _ <- ops, sizeofPrimArray p > 8]
    diamonds = [centre (pointsOf p) | FillPolygon p _ _ _ <- ops, sizeofPrimArray p == 8]
  case (linePts, areaPts) of
    ([l@(start : _)], [area0 : _]) -> do
      check "area does not start at its first sample" (near start area0)
      check "diamond markers are not centred on their points" (length diamonds == length l && all (\d -> any (near d) l) diamonds)
    _ -> fail ("expected one line and one area, got " <> show (length linePts, length areaPts))

testGrowPlotHeight :: FontMetrics -> IO ()
testGrowPlotHeight fm = do
  let
    fittedWith f = fitLayout fm (f defaultLayout) (chartDia fm barChartSample)
    fitted = fittedWith fillW
    fixed400 = fittedWith (fillW . fixedH 400)
    -- Too short for the labels, which would otherwise enlarge it.
    fixed100 = fittedWith (fillW . fixedH 100)
  check "plot grow height ballooned" (layoutMinH fitted <= 260 && layoutMaxH fitted <= 260)
  -- The caller's own height holds: a taller minimum, a fixed height, or growing.
  check "a growing plot capped a taller minimum" (layoutMinH (fittedWith (fillW . minH 300)) >= 300)
  check "a growing plot lost its fixed height" (layoutMinH fixed400 == 400 && layoutMaxH fixed400 == 400)
  check "a growing plot lost a fixed height under the cap" (layoutMinH fixed100 == 100 && layoutMaxH fixed100 == 100)
  check "a growing plot stopped growing in height" $ case layoutHeight (fittedWith grow) of
    Grow _ -> True
    _ -> False

-- | A diagram with no width, such as a vertical rule, takes the caller's
-- box: a fixed width does not make it infinitely tall, and a fixed box keeps
-- its width.
testDegenerateFit :: FontMetrics -> IO ()
testDegenerateFit fm = do
  let
    rule = D.vrule 50 :: Diagram B
    tall = fitLayout fm (fixedW 200 defaultLayout) rule
    boxed = fitLayout fm (fixedWH 200 100 defaultLayout) rule
  check "a fixed width made a rule infinitely tall" (layoutMaxH tall < 1e6)
  check "a fixed box lost its width for a rule" (layoutMinW boxed == 200 && layoutMaxW boxed == 200)
