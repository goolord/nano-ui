module Main (main) where

import Control.Monad (forM_, replicateM_, unless)
import Data.Colour (withOpacity)
import Data.Colour.Names (coral, red, steelblue)
import Data.Foldable (toList)
import Data.IORef (readIORef)
import Data.List (nub, sortOn, tails)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayToList, sizeofPrimArray)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Diagrams.Prelude qualified as D
import Diagrams.Prelude (Diagram, (#))
import NanoUI
import NanoUI.Backend
import NanoUI.Internal.Context (Context (..), DrawingCacheState (..))
import NanoUI.Internal.Context.Types (DrawOpCacheEntry (..))
import NanoUI.Diagrams
import NanoUI.Internal.Path (Shade (..))
import NanoUI.Path qualified as P
import NanoUI.Plot.Widget qualified as Plot
import Test.Hspec (describe, hspec, it)

-- | Fail with @msg@ unless the condition holds.
check :: String -> Bool -> IO ()
check msg ok = unless ok (fail msg)

main :: IO ()
main = hspec $ do
  describe "rendering" $ do
    it "draws a diagram, then from its cache, with labels apart from geometry" testRendering
    it "reuses cached chart content within one context" testChartCache
    it "settles on a chart holding NaN" testNaNChartIdle
    it "turns diagram fills and strokes into ops" testDiagramOps
  describe "scales and domains" $ do
    it "picks nice ticks and labels them with the digits their step needs" testTicks
    it "shares finite bounds across series and baselines" testDomains
  describe "decimation" $
    it "keeps extrema, endpoints and order, and markers up to 2000" testDecimation
  describe "chart chrome" $ do
    it "keeps labels, titles and legends apart" testLabelFit
    it "colors legend entries like their series" testLegendColors
    it "picks the nearest hover point" testPlotHover
    it "draws series shapes where their data is" testSeriesShapes
    it "fills an area a ring per side of its baseline" testAreaRings
    it "sizes plots and diagrams by the caller's box" testFit

fm :: FontMetrics
fm = monospaceMetrics 16

-- | A chart of the given series with no legend, grid or decimation.
bareChart :: [Series] -> Chart
bareChart ss = (chart ss) {chartLegend = LegendNone, chartGrid = GridNone, chartDecimate = False}

chartDia :: Chart -> Diagram B
chartDia c = chartDiagram fm defaultTheme defaultPlotStyle (seriesDomains c) (map (seriesPoints c) (chartSeries c)) c

-- | The ops of a chart drawn @w@ by @h@.
chartOps :: Float -> Float -> Chart -> [DrawOp]
chartOps w h = toList . diagramOps (realToFrac w) (realToFrac h) . chartDia

-- | The ops of a 100 by 100 diagram.
diaOps :: Diagram B -> [DrawOp]
diaOps = toList . diagramOps 100 100

rectsOverlap :: Rect -> Rect -> Bool
rectsOverlap (Rect x1 y1 w1 h1) (Rect x2 y2 w2 h2) =
  x1 < x2 + w2 && x2 < x1 + w1 && y1 < y2 + h2 && y2 < y1 + h1

-- | A flat array of x, y pairs as points.
pointsOf :: PrimArray Float -> [(Float, Float)]
pointsOf p = [(indexPrimArray p (2 * k), indexPrimArray p (2 * k + 1)) | k <- [0 .. sizeofPrimArray p `div` 2 - 1]]

-- | The colour of a polygon fill or a polyline stroke.
inkColor :: DrawOp -> Maybe Color
inkColor (FillPolygon _ _ _ (Flat c)) = Just c
inkColor (StrokePolyline _ _ _ _ _ _ (Flat c)) = Just c
inkColor _ = Nothing

-- | The first theme series colour.
seriesInk :: Color
seriesInk = fromMaybe (themeRed defaultTheme) (listToMaybe (themeSeries defaultTheme))

testRendering :: IO ()
testRendering = do
  ctx <- newPixelContext
  -- The second frame draws from the cache.
  replicateM_ 2 $ do
    (_, dd, _) <-
      runFrame ctx (emptyInput {inputWindowSize = Size 240 120}) $
        diagram (fixedWH 200 80) (D.circle 1 # D.fc coral # D.lw D.none)
    check "diagram produced no draw commands" (drawIndexCount dd > 0 && not (drawCmdNull dd))
  let
    d = chartDia (bareChart [line "s" [(0, 0), (1, 2), (2, 1)]]) # D.lwO 2 # D.fc steelblue
    isText = \case
      DrawText {} -> True
      _ -> False
  forM_ [(400, 240), (120, 400), (0, 100), (100, -1)] $ \(w, h) -> do
    let
      full = toList (diagramOps w h d)
      labels = toList (diagramTextOps w h d)
    check "text-only rendering differs from the full render's text" (labels == filter isText full)
    check "text-only rendering did not separate chart labels from geometry" $
      w <= 0 || h <= 0 || (not (null labels) && length labels < length full)

testChartCache :: IO ()
testChartCache = do
  base <- newPixelContext
  other <- newPixelContext
  owner <- Plot.newPlotCache
  otherOwner <- Plot.newPlotCache
  let
    ctx = withFontMetrics base fm
    larger = monospaceMetrics 24
    render cacheOwner c = do
      _ <-
        runFrame c (emptyInput {inputWindowSize = Size 400 240}) $
          Plot.lineChart cacheOwner (fixedWH 360 200) [(0, 0), (1, 1)]
      map doeContent . toList . dcsDrawOpCache <$> readIORef (ctxDrawingCache c)
  first <- render owner ctx
  again <- render owner ctx
  check "chart cache did not reuse unchanged content" (not (null first) && first == again)
  changed <- render owner (withFontMetrics base larger)
  check "chart cache ignored changed font metrics" (changed /= first)
  independent <- render otherOwner (withFontMetrics other larger)
  check "chart cache version leaked across contexts" (independent == first)

-- | A chart holding NaN equals a rebuilt copy of itself, so a plot built
-- from it every frame settles instead of asking for frames forever.
testNaNChartIdle :: IO ()
testNaNChartIdle = do
  base <- newPixelContext
  owner <- Plot.newPlotCache
  let
    nan = 0 / 0
    held = bareChart [line "l" [(0, nan)], bar "b" [("x", nan)]]
    -- A NaN in a series' own parameters, not its data.
    params = bareChart [withBaseline nan (area "a" [(0, 1)]), withStrokeWidth (0 / 0) (line "l" [(0, 1)])]
    ctx = withFontMetrics base fm
    -- The points depend on the frame, so each frame builds a new chart.
    frame k = runFrame ctx (emptyInput {inputWindowSize = Size 400 240}) $ do
      _ <- Plot.lineChart owner (fixedWH 360 100) [(0, 0), (1, nan * fromIntegral (k :: Int)), (2, 1)]
      Plot.plot owner (fixedWH 360 100) (bareChart [withBaseline (nan * fromIntegral k) (area "a" [(0, 0), (1, 1)])])
  check "a chart holding NaN differs from itself" (held == held && params == params)
  again <- mapM frame [1 .. 4]
  check "a plot of a chart holding NaN asks for frames forever" (not (or [more | (_, _, more) <- drop 2 again]))

testDiagramOps :: IO ()
testDiagramOps = do
  let
    fills d = [op | op <- diaOps (d # D.fc coral # D.lw D.none), isFill op]
    isFill = \case
      FillRect {} -> True
      FillPolygon {} -> True
      _ -> False
  check "a level rectangle is not one rect op" $ case fills (D.rect 4 2) of
    [FillRect {}] -> True
    _ -> False
  check "a turned rectangle is not one polygon" $ case fills (D.rect 4 2 # D.rotateBy (1 / 8)) of
    [FillPolygon {}] -> True
    _ -> False
  -- Colours are straight alpha, whether the alpha is the colour's or the opacity's.
  let
    fillsOf d = mapMaybe inkColor (diaOps (d # D.lw D.none))
    halfRed = colorRGBA 255 0 0 128
  check "a translucent fill was darkened" (fillsOf (D.circle 1 # D.fcA (red `withOpacity` 0.5)) == [halfRed])
  check "opacity did not reach the fill" (fillsOf (D.circle 1 # D.fc red # D.opacity 0.5) == [halfRed])
  -- A path's loops fill together. The inner circle is a hole under even-odd,
  -- or under winding when reversed; with the same direction it is filled.
  let
    ringsOf d = [sizeofPrimArray rings - 1 | FillPolygon _ rings _ _ <- diaOps (d # D.fc coral # D.lw D.none)]
    annulus :: D.Path D.V2 Double
    annulus = D.circle 2 <> D.circle 1
  check "even-odd annulus lost its hole" (ringsOf (D.strokeP annulus # D.fillRule D.EvenOdd) == [2])
  check "winding annulus lost its hole" (ringsOf (D.strokeP (D.circle 2 <> D.reversePath (D.circle 1))) == [2])
  check "winding annulus cut a hole" (ringsOf (D.strokeP annulus) == [1])
  let
    strokes d = [(cap, join) | StrokePolyline _ _ _ cap join _ _ <- diaOps (d # D.lc steelblue)]
    zigzag = D.fromVertices [D.p2 (0, 0), D.p2 (1, 0), D.p2 (1, 1)] :: Diagram B
  check "a line's cap and join were not the style's" $
    strokes (zigzag # D.lineCap D.LineCapRound # D.lineJoin D.LineJoinRound) == [(P.RoundCap, P.RoundJoin)]
  check "a line's default cap and join changed" (strokes zigzag == [(P.ButtCap, P.MiterJoin)])
  check "a dashed line was not cut into dashes" (length (strokes (zigzag # D.dashingO [4, 4] 0)) > 4)

testTicks :: IO ()
testTicks = do
  let
    t0 = niceTicks 6 (Domain 0 100)
    labels d = map snd (niceTickLabels 6 d)
  check "nice ticks failed for [0,100]" (fmap (<= 0) (listToMaybe t0) == Just True && fmap (>= 100) (listToMaybe (reverse t0)) == Just True)
  check "nice ticks failed for [-5,5]" (0 `elem` niceTicks 6 (Domain (-5) 5))
  check "niceTicks failed on overflowing, singleton, or empty-budget domains" $
    niceTicks 6 (Domain (-1e308) 1e308) == []
      && niceTicks 6 (Domain 1e308 1e308) == [1e308]
      && niceTicks 0 (Domain 0 100) == []
  -- Values snap only after choosing their form, and never overflow.
  forM_
    [ (6, "6"), (0.2, "0.2"), (0.2 + 0.2 + 0.2, "0.6"), (0.0008, "0.0008"), (1.5e-10, "1.500e-10")
    , (1e308, "1.000e308"), (-1e308, "-1.000e308")
    ]
    $ \(v, want) -> check ("formatTick " <> show v) (formatTick v == want)
  check "tick labels carry noise or spare digits" $
    labels (Domain 0 1) == ["0", "0.2", "0.4", "0.6", "0.8", "1"]
      && labels (Domain 1e-8 5e-8) == ["1e-8", "2e-8", "3e-8", "4e-8", "5e-8"]
      && labels (Domain 1700000000 1700086400) == ["1.7e9", "1.70002e9", "1.70004e9", "1.70006e9", "1.70008e9"]
  -- Neighbouring y labels differ however fine the step or large the values'
  -- common offset, as for Unix time over one day.
  let
    yLabels pts = [t | DrawText _ _ 1 _ t _ <- chartOps 400 240 (bareChart [line "s" pts])]
    distinct ts = length ts >= 3 && length (nub ts) == length ts
  check "fine ticks lost their digits" (distinct (yLabels [(0, 1), (1, 1.0000004)]))
  check "a large offset hid the ticks' step" (distinct (yLabels [(0, 1700000000), (1, 1700086400)]))

-- | Series share bounds; an area's reaches its baseline. Empty series, flat
-- lines and values that are not finite leave the others' span alone.
testDomains :: IO ()
testDomains = do
  let
    nan = 0 / 0
    finite x = not (isNaN x || isInfinite x)
    domains msg ok ss = check msg (ok (seriesDomains (bareChart ss)))
    yDomain msg ok = domains msg (ok . snd)
  domains "multi-series domains do not share bounds" (\(Domain xl xh, Domain yl yh) -> yl <= 0 && yh >= 20 && xl <= 0 && xh >= 1) $
    [line "a" (V.fromList [(0, 0), (1, 1)]), line "b" [(0, 10), (1, 20)]]
  domains "seriesDomains seeded with 0..1" (\(Domain xl _, Domain yl _) -> xl > 2 && yl > 1) [scatter "s" [(4, 3), (9, 8)]]
  yDomain "a constant series widened the shared domain" (\(Domain lo hi) -> lo > 0.09 && hi < 0.16) $
    [line "p" [(0, 0.10), (1, 0.15)], line "target" [(0, 0.12), (1, 0.12)]]
  domains "an empty series pulled the shared domain to zero" (\(Domain xl _, Domain yl _) -> xl > 900 && yl > 900) $
    [line "a" [(1000, 1000), (2000, 2000)], line "b" []]
  yDomain "an area's baseline is outside its y domain" (\(Domain lo hi) -> lo <= 0 && hi >= 60) $
    [area "a" [(0, 50), (1, 55), (2, 60)]]
  yDomain "a baseline above an area is outside its y domain" (\(Domain _ hi) -> hi >= 80) $
    [withBaseline 80 (area "a" [(0, 50), (1, 60)])]
  yDomain "an empty area pulled the shared domain to its baseline" (\(Domain lo _) -> lo > 900) $
    [line "a" [(0, 1000), (1, 2000)], area "b" []]
  yDomain "a NaN baseline reached the y domain" (\(Domain lo hi) -> lo > 40 && hi < 70) $
    [withBaseline nan (area "a" [(0, 50), (1, 60)])]
  yDomain "a NaN dropped the domain's earlier minimum" (\(Domain lo _) -> lo <= -100) $
    [line "s" [(0, -100), (1, nan), (2, 5), (3, 6)]]
  domains "an infinite value made the domain infinite" (\(Domain a b, Domain c d) -> all finite [a, b, c, d] && d >= 3) $
    [line "s" [(0, 1), (1, 1 / 0), (2, 3)]]
  check "mergeDomains broken" (mergeDomains (Domain 0 1) (Domain 0 10) == Domain 0 10)
  -- Hover and drawing skip a NaN too.
  let
    holes = bareChart [line "s" [(0, 0), (1, nan), (2, 10)]]
    hovered = nearestPlotHover (seriesDomains holes) (map (seriesPoints holes) (chartSeries holes)) 0.05 0.05
  check "hover picked a point past a NaN" (fmap hoverDataX hovered == Just 0)
  check "a NaN point reached the drawing" $
    all (all finite . primArrayToList) [p | StrokePolyline p _ _ _ _ _ _ <- chartOps 200 120 holes]

testDecimation :: IO ()
testDecimation = do
  let
    wave = V.fromList [(fromIntegral i, sin (fromIntegral i / 10)) | i <- [0 .. 9999 :: Int]]
    ys = toList (V.map snd (lttb 500 wave))
    ordered ps = V.and (V.zipWith (\a b -> fst a < fst b) ps (V.drop 1 ps))
  check "LTTB lost its target count or waveform extrema" (length ys == 500 && minimum ys < -0.5 && maximum ys > 0.5)
  check "LTTB skipped the first bucket's spike" $
    lttb 3 (V.fromList [(x, if x == 1 then 10 else 0) | x <- [0 .. 9]]) == V.fromList [(0, 0), (1, 10), (9, 0)]
  check "min/max decimation changed equal-extrema tie handling" $
    minMaxDecimate 1 (V.fromList [(0, 2), (1, 2), (2, 2)]) == V.singleton (0, 2)
  check "min/max decimation lost extrema or reversed their order" $
    minMaxDecimate 2 (V.fromList [(x, 9 - x) | x <- [0 .. 8]]) == V.fromList [(0, 9), (4, 5), (5, 4), (8, 1)]
  forM_ [0 .. 60] $ \n -> forM_ [-1 .. n + 1] $ \k -> do
    let
      input = V.generate n (\i -> (fromIntegral i, sin (fromIntegral i)))
      sampled = lttb k input
      kept = minMaxDecimate k input
      extremes ps = (V.minimum (V.map snd ps), V.maximum (V.map snd ps))
    check "LTTB violated its point budget or input order" (V.length sampled == min n (max 0 k) && ordered sampled)
    check "LTTB lost an endpoint" $
      (V.null sampled || V.head sampled == V.head input) && (V.length sampled < 2 || V.last sampled == V.last input)
    check "min/max decimation violated its budget or input order" (V.length kept <= max 0 (2 * k) && ordered kept)
    check "min/max decimation invented a point" (V.all (`V.elem` input) kept)
    check "min/max decimation lost a global extremum" (V.null kept || extremes input == extremes kept)
  -- Default decimation thins a long line, but keeps every marker of a
  -- scatter or a marked line up to 2000: a dropped marker is simply missing.
  let
    pts n = [(fromIntegral i, sin (fromIntegral i)) | i <- [0 .. n - 1 :: Int]]
    drawn s = U.length (seriesPoints (chart [s]) s)
  check "decimation did not thin a long line" (drawn (line "l" (pts 100)) < 100)
  check "decimation thinned a scatter series" (drawn (scatter "s" (pts 100)) == 100)
  check "decimation thinned a marked line" (drawn (withMarker MarkCircle (line "l" (pts 100))) == 100)
  check "decimation kept every point of a huge scatter" (drawn (scatter "s" (pts 5000)) == 2000)

barChartSample :: Chart
barChartSample =
  (bareChart [bar "count" [("Mon", 2), ("Tue", 5), ("Wed", 4), ("Thu", 7), ("Fri", 3)]])
    { chartXTitle = Just "day"
    , chartYTitle = Just "count"
    , chartLegend = LegendRight
    , chartGrid = GridBoth
    }

-- | Each label drawn, with its box.
textBoxes :: [DrawOp] -> [(T.Text, Rect)]
textBoxes ops = [(t, drawTextBox fm x y ax ay t) | DrawText x y ax ay t _ <- ops]

testLabelFit :: IO ()
testLabelFit = do
  let
    -- Tall enough for the fit, at most twice the size, to part five
    -- three-letter days in glyphs as wide as they are high.
    fitted = fitLayout fm (fixedH 200 defaultLayout) (chartDia barChartSample)
    boxes = case (layoutWidth fitted, layoutHeight fitted) of
      (Fixed bw, Fixed bh) -> map snd (textBoxes (chartOps bw bh barChartSample))
      _ -> []
    xs = [x | Rect x _ _ _ <- boxes]
  check "axis labels did not spread along x" (length xs >= 3 && maximum xs - minimum xs > 20)
  check "axis label boxes overlap" (not (or [rectsOverlap a b | (a : rest) <- tails boxes, b <- rest]))
  let
    sleepChart =
      (bareChart [scatter "focus" [(4, 3), (9, 8)], line "trend" [(4, 3), (9, 8)]])
        { chartLegend = LegendRight
        , chartYTitle = Just "focus"
        , chartXTitle = Just "hours slept"
        }
    botChart = sleepChart {chartLegend = LegendBottom}
    tickText t = not (T.null t) && T.all (\c -> c == '-' || c == '.' || c >= '0' && c <= '9') t
    -- Whether a label selected by @picks@ overlaps a tick label.
    overlapsTicks c w h picks =
      let ts = textBoxes (chartOps w h c)
       in or [rectsOverlap a b | (t, a) <- ts, picks a t, (t', b) <- ts, tickText t']
    titleOverlaps c w h = overlapsTicks c w h $ \(Rect bx by _ _) t ->
      (chartXTitle c == Just t && by > h * 0.55) || (chartYTitle c == Just t && bx < w * 0.4)
    legendOverlaps c w h = overlapsTicks c w h $ \(Rect bx by _ _) t ->
      t `elem` map seriesName (chartSeries c) && case chartLegend c of
        LegendRight -> bx > w * 0.55
        LegendBottom -> by > h * 0.55
        _ -> False
  forM_ [(sleepChart, 400, 240), (sleepChart, 220, 150), (barChartSample, 220, 150), (botChart, 400, 240)] $
    \(c, w, h) -> check ("axis titles overlap ticks at " <> show (w, h)) (not (titleOverlaps c w h))
  forM_ [sleepChart, botChart] $ \c -> check "legend overlaps ticks" (not (legendOverlaps c 400 240))
  -- A top legend has a row of its own, under the title and inside the
  -- drawing, also where the drawing is as tall as its box.
  let
    topChart = (bareChart [line "in" [(0, 20), (1, 22)], line "out" [(0, 8), (1, 14)]]) {chartLegend = LegendTop}
    titled = topChart {chartTitle = Just "Temperature"}
    named ns c h = [b | (t, b) <- textBoxes (chartOps 400 h c), t `elem` ns]
    entries = named ["in", "out"]
  check "top legend overlaps the title" $
    not (or [rectsOverlap a b | a <- named ["Temperature"] titled 240, b <- entries titled 240])
  check "top legend spills above the drawing" $
    all (\(Rect _ y _ _) -> y >= 0) (entries topChart 150 ++ entries titled 150)
  let
    shortTitles = (bareChart [line "sin(x)" [(0, 0), (1, 1)]]) {chartYTitle = Just "y", chartXTitle = Just "x"}
    shortM = chartMargins fm shortTitles {chartLegend = LegendRight}
  check "short axis titles left a huge gutter" (marginLeft shortM < 0.85 && marginBottom shortM < 0.65)

-- | Empty series isolate the legend strokes from data geometry. Every
-- placement keeps the labels and the series' colour overrides.
testLegendColors :: IO ()
testLegendColors = do
  let
    custom = colorRGBA 17 211 83 255
    fallback = themeSeries defaultTheme !! 1
    legendChart = bareChart [withColor custom (line "custom" []), line "default" []]
  forM_ [LegendNone, LegendRight, LegendBottom, LegendTop, LegendInside] $ \position -> do
    let
      ops = chartOps 400 280 legendChart {chartLegend = position}
      labels = [t | DrawText _ _ _ _ t _ <- ops, t `elem` ["custom", "default"]]
      colors = mapMaybe inkColor ops
    if position == LegendNone
      then check "hidden legend rendered entries" (null labels && custom `notElem` colors)
      else do
        check "legend lost or duplicated a series label" (length labels == 2 && nub labels == labels)
        check "legend colors differ from series colors" (custom `elem` colors && fallback `elem` colors)

testPlotHover :: IO ()
testPlotHover = do
  let
    hoverAt c = nearestPlotHover (seriesDomains c) (map (seriesPoints c) (chartSeries c))
    centre c = hoverAt c 0.5 0.5
    picked = fmap (\h -> (hoverSeriesIdx h, hoverPointIdx h, hoverDataX h, hoverDataY h)) . centre . bareChart
  forM_ [bareChart [], bareChart [line "empty" []]] $ \c ->
    check "empty chart produced a hover target" (centre c == Nothing)
  check "equidistant hover targets did not prefer the first point" $
    picked [line "first" [(0, 0), (0, 0)], line "second" [(0, 0)]] == Just (0, 0, 0, 0)
  check "nearestPlotHover picked wrong point" (picked [line "a" [(0, 0), (1, 1), (2, 4)]] == Just (0, 1, 1, 1))
  -- Just above the first point, the second is far off on screen but nearer
  -- in data units, where y spans a thousand times x.
  let
    spread = bareChart [scatter "s" [(0.1, 500), (0.9, 505), (0, 0), (1, 1000)]]
    (xDom, yDom) = seriesDomains spread
  check "hover picked a point far away on screen" $
    fmap hoverPointIdx (hoverAt spread (domainToPlot xDom 0.1) (domainToPlot yDom 510)) == Just 0

-- | Series shapes sit where their data is: an area starts at the line
-- through the same samples, markers centre on their points, a step ends on
-- its last point, and bars stand on zero under their category names.
testSeriesShapes :: IO ()
testSeriesShapes = do
  let
    samples = [(x, 1 + sin x) | x <- [0, 0.5 .. 10]]
    ops = chartOps 200 120 (bareChart [area "a" samples, line "l" samples, withMarker MarkDiamond (scatter "d" samples)])
    near (x0, y0) (x1, y1) = abs (x1 - x0) < 1e-3 && abs (y1 - y0) < 1e-3
    centre ps = (sum (map fst ps) / fromIntegral (length ps), sum (map snd ps) / fromIntegral (length ps))
    linePts = [pointsOf p | StrokePolyline p _ _ _ _ _ _ <- ops, sizeofPrimArray p == 2 * length samples]
    areaPts = [pointsOf p | FillPolygon p _ _ _ <- ops, sizeofPrimArray p > 8]
    diamonds = [centre (pointsOf p) | FillPolygon p _ _ _ <- ops, sizeofPrimArray p == 8]
  case (linePts, areaPts) of
    ([l@(start : _)], [area0 : _]) -> do
      check "area does not start at its first sample" (near start area0)
      check "diamond markers are not centred on their points" $
        length diamonds == length l && all (\d -> any (near d) l) diamonds
    _ -> fail ("expected one line and one area, got " <> show (length linePts, length areaPts))
  let
    -- The points of each fill and each stroke in the series colour.
    fills s = [pointsOf p | op@(FillPolygon p _ _ _) <- chartOps 200 120 (bareChart [s]), inkColor op == Just seriesInk]
    strokes s = [pointsOf p | op@(StrokePolyline p _ _ _ _ _ _) <- chartOps 200 120 (bareChart [s]), inkColor op == Just seriesInk]
    cross = withMarker MarkCross (scatter "x" [(8, 8)])
    crossXs = map fst (concat (fills cross ++ strokes cross))
    lineMarkers = filter ((== 4) . length) (fills (withMarker MarkDiamond (line "l" [(0, 0), (1, 1), (2, 0)])))
    triangles = [t | FillPolygon _ _ t _ <- chartOps 200 120 (bareChart [withMarker MarkTriangle (scatter "t" [(1, 1)])])]
  check "triangle marker produced no fill" (sum (map sizeofPrimArray triangles) >= 3)
  check "MarkCross arm left at origin" (not (null crossXs) && maximum crossXs - minimum crossXs < 40)
  check "line markers were not drawn" (length lineMarkers == 3)
  -- A step series ends with the riser to its last point, the highest here.
  check "step series stopped short of its last point" $ case map (map snd) (strokes (step "s" [(0, 0), (1, 1), (2, 5)])) of
    [path] -> length path == 5 && last path == minimum path
    _ -> False
  let
    days = ["Mon", "Tue", "Wed", "Thu", "Fri"]
    barOps = chartOps 400 240 (bareChart [bar "count" (zip days [2, 5, 4, 7, 3])])
    -- A y tick label is right-aligned, level with its tick.
    tickY t = listToMaybe [y | DrawText _ y 1 _ t' _ <- barOps, t' == t]
    bars = map snd (sortOn fst [(rectX r, (rectY r, rectY r + rectH r)) | FillRect r c <- barOps, c == seriesInk])
    close a b = abs (a - b) < 0.5
  check "bar categories are not on the x axis" (all (`elem` [t | DrawText _ _ _ _ t _ <- barOps]) days)
  check "bars disagree with the y axis" $ case (tickY "0", tickY "2", tickY "4", bars) of
    (Just y0, Just y2, Just y4, [(top2, _), _, (top4, _), _, _]) ->
      all (close y0 . snd) bars && close top2 y2 && close top4 y4
    _ -> False

-- | An area series fills a ring per run of samples on one side of its
-- baseline, split where the data crosses it: the samples and the baseline's
-- two ends, triangulated into as many triangles as the ring has sides less
-- two, covering it exactly.
testAreaRings :: IO ()
testAreaRings = do
  let
    rings samples = [(p, t) | FillPolygon p _ t _ <- chartOps 200 120 (bareChart [area "a" samples])]
    triArea ((x0, y0), (x1, y1), (x2, y2)) = abs ((x0 - x2) * (y1 - y0) - (x0 - x1) * (y2 - y0)) * 0.5
    exact (p, t) =
      let
        at k = pointsOf p !! indexPrimArray t k
        tris = [(at k, at (k + 1), at (k + 2)) | k <- [0, 3 .. sizeofPrimArray t - 3]]
        ps = pointsOf p
        want = abs (sum [x * y' - x' * y | ((x, y), (x', y')) <- zip ps (drop 1 (cycle ps))]) / 2
       in
        length tris == length ps - 2 && abs (sum (map triArea tris) - want) <= 1e-3 * want
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

-- | A growing plot's height is capped, but the caller's own height holds. A
-- diagram with no width, such as a vertical rule, takes the caller's box.
testFit :: IO ()
testFit = do
  let
    fittedWith f = fitLayout fm (f defaultLayout) (chartDia barChartSample)
    heights f = let l = fittedWith f in (layoutMinH l, layoutMaxH l)
    (grownMin, grownMax) = heights fillW
  check "plot grow height ballooned" (grownMin <= 260 && grownMax <= 260)
  check "a growing plot capped a taller minimum" (fst (heights (fillW . minH 300)) >= 300)
  check "a growing plot lost its fixed height" (heights (fillW . fixedH 400) == (400, 400))
  -- Too short for the labels, which would otherwise enlarge it.
  check "a growing plot lost a fixed height under the cap" (heights (fillW . fixedH 100) == (100, 100))
  check "a growing plot stopped growing in height" $ case layoutHeight (fittedWith grow) of
    Grow _ -> True
    _ -> False
  let
    rule = D.vrule 50 :: Diagram B
    boxed = fitLayout fm (fixedWH 200 100 defaultLayout) rule
  check "a fixed width made a rule infinitely tall" (layoutMaxH (fitLayout fm (fixedW 200 defaultLayout) rule) < 1e6)
  check "a fixed box lost its width for a rule" (layoutMinW boxed == 200 && layoutMaxW boxed == 200)
