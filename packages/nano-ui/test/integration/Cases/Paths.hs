module Cases.Paths (tests) where

import Spec
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayToList, sizeofPrimArray)
import GHC.Stack (HasCallStack)
import NanoUI.Internal.Context (Context (..), DrawingCacheState (..))
import NanoUI.Internal.Context.Types (CustomDrawOpCacheEntry (..))
import NanoUI.Path qualified as P

tests :: [Spec]
tests =
  [ spec "path-arc-flattening" runArcFlatteningTest
  , spec "path-bezier-flattening" runBezierFlatteningTest
  , spec "path-arc-endpoints" runArcEndpointsTest
  , spec "path-strokes" runStrokeTest
  , spec "path-fill-triangulation" runFillTriangulationTest
  , spec "path-transforms" runTransformTest
  , spec "path-transform-ops" runTransformOpsTest
  , spec "path-degenerate" runDegenerateTest
  , spec "path-canvas-frames" runCanvasFramesTest
  ]

red, black :: Color
red = colorRGBA 220 40 40 255
black = colorRGBA 0 0 0 255

-- | A canvas block's ops, flattened for a display of @scale@ device pixels to the logical one.
opsAt :: Float -> CanvasM () -> [DrawOp]
opsAt scale = toList . runCanvasFor (CustomDrawContext False False False False False defaultTheme (monospaceMetrics 16) {fmSnapScale = scale})

pairs :: PrimArray Float -> [V2]
pairs pts = [V2 (indexPrimArray pts (2 * i)) (indexPrimArray pts (2 * i + 1)) | i <- [0 .. sizeofPrimArray pts `div` 2 - 1]]

-- | Each stroke's points and whether it closes.
strokes :: [DrawOp] -> [([V2], Bool)]
strokes ops = [(pairs pts, closed) | StrokePolyline pts _ closed _ <- ops]

-- | Each filled polygon's outline and triangles.
fills :: [DrawOp] -> [([V2], [Int])]
fills ops = [(pairs pts, primArrayToList tris) | FillPolygon pts tris _ <- ops]

-- | A path's strokes at width 1, and its fills, on a display of scale 1.
stroked :: P.Path -> [([V2], Bool)]
stroked path = strokes (opsAt 1 (drawStrokePath path 1 black))

filled :: P.Path -> [([V2], [Int])]
filled path = fills (opsAt 1 (drawPath path red))

-- | Check the only element, or count a failure.
single :: HasCallStack => IORef Int -> [a] -> (a -> IO ()) -> IO ()
single failed xs k = case xs of [x] -> k x; _ -> assertEq failed 1 (length xs)

-- | Require points pairwise within @eps@, showing both lists otherwise.
assertNear :: HasCallStack => IORef Int -> Float -> [V2] -> [V2] -> IO ()
assertNear failed eps want got = unless (length want == length got && and (zipWith (near eps) want got)) (assertEq failed want got)

dist :: V2 -> V2 -> Float
dist (V2 x0 y0) (V2 x1 y1) = sqrt ((x1 - x0) * (x1 - x0) + (y1 - y0) * (y1 - y0))

near :: Float -> V2 -> V2 -> Bool
near eps a b = dist a b <= eps

-- | A line's first and last points.
ends :: [V2] -> [V2]
ends pts = take 1 pts ++ drop (length pts - 1) pts

-- | How far a point is from an open polyline.
polyDist :: [V2] -> V2 -> Float
polyDist ps p@(V2 px py) = minimum (zipWith seg ps (drop 1 ps))
  where
    seg (V2 ax ay) (V2 bx by) =
      let (dx, dy) = (bx - ax, by - ay)
          t = max 0 (min 1 (((px - ax) * dx + (py - ay) * dy) / max 1e-12 (dx * dx + dy * dy)))
       in dist p (V2 (ax + t * dx) (ay + t * dy))

-- | The shoelace area, positive for a ring clockwise on screen.
polyArea :: [V2] -> Float
polyArea ps = sum [x0 * y1 - x1 * y0 | (V2 x0 y0, V2 x1 y1) <- zip ps (drop 1 ps ++ take 1 ps)] / 2

-- | The area a triangulation covers, and whether its indices are in range.
covered :: [V2] -> [Int] -> (Float, Bool)
covered ps tris = (sum (map area (triples tris)), all (\i -> i >= 0 && i < length ps) tris && length tris `mod` 3 == 0)
  where
    triples (a : b : c : rest) = (ps !! a, ps !! b, ps !! c) : triples rest
    triples _ = []
    area (V2 x0 y0, V2 x1 y1, V2 x2 y2) = abs ((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)) / 2

-- | A point of the Bezier curve with these control points.
bezier :: [V2] -> Float -> V2
bezier [p] _ = p
bezier ps t = bezier (zipWith (\(V2 x0 y0) (V2 x1 y1) -> V2 (x0 + t * (x1 - x0)) (y0 + t * (y1 - y0))) ps (drop 1 ps)) t

-- | Whether a point is on the axis-aligned ellipse about @c@ with radii @rx@ and @ry@.
onEllipse :: V2 -> Float -> Float -> V2 -> Bool
onEllipse (V2 cx cy) rx ry (V2 x y) = abs (((x - cx) / rx) ^ (2 :: Int) + ((y - cy) / ry) ^ (2 :: Int) - 1) < 1e-3

-- | A circle's chords stay within a quarter device pixel of it at any scale, no finer.
runArcFlatteningTest :: Context -> IORef Int -> IO ()
runArcFlatteningTest _ failed = do
  let c = V2 100 100
      ring scale = strokes (opsAt scale (drawStrokePath (P.circle c 50) 1 black))
      count scale = sum [length pts | (pts, _) <- ring scale]
  forM_ [1, 2, 3] $ \scale -> single failed (ring scale) $ \(pts, closed) -> do
    let tol = 0.25 / scale
        sags = [50 - dist c (V2 ((ax + bx) / 2) ((ay + by) / 2)) | (V2 ax ay, V2 bx by) <- zip pts (drop 1 pts ++ take 1 pts)]
    assert failed (closed && all (\p -> abs (dist c p - 50) < 1e-3) pts && all (<= tol + 1e-3) sags)
    assertGt failed (maximum sags) (tol / 2)
  assertGt failed (count 2) (count 1)
  assertGt failed (count 3) (count 2)
  -- 'runCanvas' does not know the display, and flattens as for scale 2.
  assert failed (opsAt 2 (drawPath (P.circle c 50) red) == toList (runCanvas (drawPath (P.circle c 50) red)))

-- | A Bezier curve's polyline shares its ends and stays within the tolerance of it.
runBezierFlatteningTest :: Context -> IORef Int -> IO ()
runBezierFlatteningTest _ failed = do
  let (p0, c1, c2, q, p3) = (V2 10 10, V2 400 (-100), V2 (-200) 300, V2 150 (-200), V2 300 300)
      samples = [fromIntegral k / 2000 | k <- [0 .. 2000 :: Int]]
  forM_ [(s, c) | s <- [1, 2], c <- [(P.cubicTo c1 c2 p3, [p0, c1, c2, p3]), (P.quadTo q p3, [p0, q, p3])]] $ \(scale, (seg, ctrl)) ->
    single failed (strokes (opsAt scale (drawStrokePath (P.moveTo p0 <> seg) 1 black))) $ \(pts, closed) -> do
      assertEq failed (False, [p0, p3]) (closed, ends pts)
      assertLt failed (maximum (map (polyDist pts . bezier ctrl) samples)) (0.25 / scale + 1e-3)
      -- Every point the flattening kept is on the curve.
      assert failed (all (\p -> minimum (map (dist p . bezier ctrl) samples) < 1) pts)
  -- A straight "curve" is one chord.
  single failed (stroked (P.moveTo p0 <> P.cubicTo (V2 20 10) (V2 30 10) (V2 40 10))) $ \(pts, _) -> assertEq failed [p0, V2 40 10] pts

-- | Arcs end where their angles say; SVG arcs on their end point, sided and sized by their flags.
runArcEndpointsTest :: Context -> IORef Int -> IO ()
runArcEndpointsTest _ failed = do
  let c = V2 50 50
      onCircle a = V2 (50 + 30 * cos a) (50 + 30 * sin a)
      ys pts = [y | V2 _ y <- pts]
      svgArc radius large clockwise = stroked (P.moveTo (V2 0 0) <> P.arcTo (V2 radius radius) 0 large clockwise (V2 20 0))
      onSmallCircle = all (\p -> abs (dist (V2 10 0) p - 10) < 1e-3)
  single failed (stroked (P.arc c 30 0.3 1.2)) $ \(pts, closed) -> do
    assert failed (not closed)
    assertNear failed 1e-3 [onCircle 0.3, onCircle 1.5] (ends pts)
  -- A negative sweep turns the other way.
  single failed (stroked (P.arc c 30 0.3 (-1.2))) $ \(pts, _) -> do
    assertNear failed 1e-3 [onCircle (-0.9)] (drop 1 (ends pts))
    assert failed (all (<= 50 + 30 * sin 0.3 + 1e-3) (ys pts))
  -- An arc after a point runs a line to its start: a pie slice.
  single failed (filled (P.moveTo c <> P.arc c 30 0 (pi / 2) <> P.close)) $ \(pts, _) -> do
    assertEq failed [c] (take 1 pts)
    assertLt failed (abs (abs (polyArea pts) - 30 * 30 * pi / 4)) 30
  -- A clockwise SVG arc from (0, 0) to (20, 0) goes over the top.
  single failed (svgArc 10 False True) $ \(pts, _) -> do
    assertEq failed [V2 0 0, V2 20 0] (ends pts)
    assert failed (onSmallCircle pts)
    assertLt failed (minimum (ys pts)) (-9.7)
  single failed (svgArc 10 False False) $ \(pts, _) -> assertGt failed (maximum (ys pts)) 9.7
  -- Radii too small to reach grow until they do.
  single failed (svgArc 2 False True) $ assert failed . onSmallCircle . fst
  -- The large arc of a circle of radius 20 through both ends bulges further.
  single failed (svgArc 20 True True) $ \(pts, _) -> assertLt failed (minimum (ys pts)) (-37)
  single failed (svgArc 20 False True) $ \(pts, _) -> assertGt failed (minimum (ys pts)) (-2.7)
  -- A zero radius is a straight line.
  single failed (svgArc 0 False True) $ assertEq failed [V2 0 0, V2 20 0] . fst
  -- An ellipse's arc turns with its rotation.
  single failed (stroked (P.ellipticalArc c (V2 40 20) (pi / 2) 0 pi)) $ \(pts, _) -> do
    assertNear failed 1e-3 [V2 50 90, V2 50 10] (ends pts)
    assert failed (all (onEllipse c 20 40) pts)
    assertLt failed (abs (minimum [x | V2 x _ <- pts] - 30)) 0.3

-- | Open and closed strokes, subpaths, and the ends of an open one.
runStrokeTest :: Context -> IORef Int -> IO ()
runStrokeTest _ failed = do
  let (a, b, c) = (V2 0 0, V2 10 0, V2 10 10)
      plain ops = [(primArrayToList pts, w, closed) | StrokePolyline pts w closed _ <- ops]
      line w path = plain (opsAt 1 (drawStrokePath path w black))
      capped cap path = opsAt 1 (drawStrokePathCapped cap path 4 black)
      rounds = capped P.RoundCap (P.polyline [a, b])
  assertEq failed [([0, 0, 10, 0, 10, 10], 3, False)] (line 3 (P.polyline [a, b, c]))
  -- Closing drops a repeat of the first point.
  assertEq failed [([0, 0, 10, 0, 10, 10], 2, True)] (line 2 (P.moveTo a <> P.lineTo b <> P.lineTo c <> P.lineTo a <> P.close))
  -- Returning to the start without closing leaves the line open.
  assertEq failed [([0, 0, 10, 0, 10, 10, 0, 0], 2, False)] (line 2 (P.moveTo a <> P.lineTo b <> P.lineTo c <> P.lineTo a))
  -- Each subpath is its own polyline; a close returns to the subpath's start.
  assertEq failed [([0, 0, 10, 0, 10, 10], 1, True), ([0, 0, 0, 20], 1, False)] (line 1 (P.polygon [a, b, c] <> P.lineTo (V2 0 20)))
  assertEq failed 2 (length (opsAt 1 (drawStrokePath (P.polyline [a, b] <> P.polyline [c, V2 20 20]) 1 black)))
  -- Caps; a closed subpath has no ends to cap.
  assertEq failed [([-2, 0, 12, 0], 4, False)] (plain (capped P.SquareCap (P.polyline [a, b])))
  assertEq failed ([([0, 0, 10, 0], 4, False)], [(0, 0, 2), (10, 0, 2)]) (plain rounds, [(x, y, r) | FillCircle x y r _ <- rounds])
  assertEq failed 1 (length (capped P.RoundCap (P.polygon [a, b, c])))
  -- A fill closes an open subpath.
  assertEq failed [([a, b, c], [0, 1, 2])] (filled (P.polyline [a, b, c]))

-- | Every subpath fills as one polygon whose triangles cover exactly its area, in either winding.
runFillTriangulationTest :: Context -> IORef Int -> IO ()
runFillTriangulationTest _ failed = do
  let c = V2 60 60
      -- @k@ corners round the centre, corner @i@ at distance @rad i@.
      ring k rad = [V2 (60 + rad i * cos a) (60 + rad i * sin a) | i <- [0 .. k - 1 :: Int], let a = 2 * pi * fromIntegral i / fromIntegral k]
      star = ring 10 (\i -> if even i then 50 else 20)
      ell = [V2 0 0, V2 60 0, V2 60 20, V2 20 20, V2 20 60, V2 0 60]
      comb = [V2 0 0, V2 100 0, V2 100 40, V2 80 40, V2 80 10, V2 60 10, V2 60 40, V2 40 40, V2 40 10, V2 20 10, V2 20 40, V2 0 40]
      -- A repeatable number in [0, 1) for each seed.
      rnd :: Int -> Float
      rnd k = let v = sin (fromIntegral k * 12.9898) * 43758.5453 in v - fromIntegral (floor v :: Int)
      shapes =
        [ ("circle", P.circle c 50), ("ellipse", P.ellipse c (V2 50 20)), ("rounded rect", P.roundedRect (Rect 0 0 100 60) 12)
        , ("hexagon", P.polygon (ring 6 (const 40))), ("star", P.polygon star), ("star anticlockwise", P.polygon (reverse star))
        , ("L", P.polygon ell), ("comb", P.polygon comb)
        , ("band", P.arc c 50 0 (1.5 * pi) <> P.arc c 25 (1.5 * pi) (-1.5 * pi) <> P.close)
        , ("blob", P.moveTo (V2 0 0) <> P.cubicTo (V2 60 (-30)) (V2 100 30) (V2 100 60) <> P.quadTo (V2 50 100) (V2 0 60) <> P.close)
          -- A pie chart's only slice: out to the rim and back along the same radius.
        , ("whole pie", P.moveTo c <> P.arc c 50 (-pi / 2) (2 * pi) <> P.close)
        , ("pie over half", P.moveTo c <> P.arc c 50 0.3 (1.6 * pi) <> P.close)
        ]
          -- Simple polygons of up to 64 corners at random distances round the centre.
          <> [("random star " <> show seed, P.polygon (ring (5 + seed `mod` 60) (\i -> 5 + 50 * rnd (seed * 1000 + i)))) | seed <- [1 .. 60 :: Int]]
  forM_ shapes $ \(name, shape) ->
    assertEq failed (name, [True]) . (,) name $
      [ inRange && length tris == 3 * (length pts - 2) && abs (area - want) <= 1e-3 * want
      | (pts, tris) <- filled shape
      , let (area, inRange) = covered pts tris
            want = abs (polyArea pts)
      ]
  -- The polygons are the shapes: exact for straight sides, within the tolerance for curves.
  let areaOf shape = sum [abs (polyArea pts) | (pts, _) <- filled shape]
  assertLt failed (abs (areaOf (P.polygon ell) - 2000)) 1e-3
  assertLt failed (abs (areaOf (P.polygon comb) - 2800)) 1e-3
  assertLt failed (abs (areaOf (P.circle c 50) - pi * 50 * 50)) (2 * pi * 50 * 0.25)
  assertLt failed (abs (areaOf (P.roundedRect (Rect 0 0 100 60) 12) - (6000 - (4 - pi) * 144))) (2 * pi * 12 * 0.25)
  -- Each subpath fills on its own; one inside another is drawn over it.
  assertEq failed [2, 2] [length (filled (P.rect r0 <> P.rect r1)) | (r0, r1) <- [(Rect 0 0 10 10, Rect 20 0 10 10), (Rect 0 0 100 100, Rect 20 20 10 10)]]

-- | Transforms compose inside out, apply before flattening, and scale stroke widths.
runTransformTest :: Context -> IORef Int -> IO ()
runTransformTest _ failed = do
  let tri = [V2 1 0, V2 2 0, V2 2 1]
      triangle = drawPath (P.polygon tri) red
      outline block = concat [pts | (pts, _) <- fills (opsAt 1 block)]
      at = map . P.transformPoint
      -- A quarter turn takes (x, y) to (-y, x), then the scale.
      rotatedThenScaled = [V2 (-2 * y) (3 * x) | V2 x y <- tri]
      points block = sum [length pts | (pts, _) <- strokes (opsAt 1 block)]
      circle r = drawStrokePath (P.circle (V2 0 0) r) 1 black
      widths t w = [w' | StrokePolyline _ w' _ _ <- opsAt 1 (withTransform t (drawStrokePath (P.polyline [V2 0 0, V2 1 1]) w black))]
  forM_
    [ (rotatedThenScaled, outline (withTransform (P.scale 2 3) (withTransform (P.rotate (pi / 2)) triangle)))
    , (rotatedThenScaled, outline (withTransform (P.scale 2 3 <> P.rotate (pi / 2)) triangle))
    , ([V2 (-3 * y) (2 * x) | V2 x y <- tri], outline (withTransform (P.rotate (pi / 2) <> P.scale 2 3) triangle))
    , (rotatedThenScaled, at (P.scale 2 3 <> P.rotate (pi / 2)) tri)
    , ([V2 9 9], at (P.rotateAround (V2 9 9) 1.1) [V2 9 9]), ([V2 13 4], at (P.translate 3 4 <> P.affine 1 0 0 1 10 0) [V2 0 0])
    , (tri, outline (withTransform mempty triangle))
    , -- A transform ends with its block.
      (at (P.translate 5 0) tri <> tri, outline (withTransform (P.translate 5 0) triangle >> triangle))
    ]
    $ uncurry (assertNear failed 1e-4)
  -- Flattening sees the transformed size.
  assertGt failed (points (withTransform (P.scale 10 10) (circle 10))) (points (circle 10))
  assertEq failed (points (circle 100)) (points (withTransform (P.scale 10 10) (circle 10)))
  assertEq failed (points (circle 100)) (points (withTransform (P.scale 10 1) (circle 10)))
  -- A stroke's width scales with the transform.
  assertEq failed ([6], [4]) (widths (P.scale 3 3) 2, widths (P.scale 8 2) 1)

-- | The canvas's other ops follow a transform as far as their shapes allow.
runTransformOpsTest :: Context -> IORef Int -> IO ()
runTransformOpsTest _ failed = do
  let (blue, green, white) = (colorRGBA 0 0 255 255, colorRGBA 0 255 0 255, colorRGBA 255 255 255 255)
      under t block = opsAt 1 (withTransform t block)
      r0 = Rect 10 20 10 20
      -- The only op, if it is a turned image: its centre, size, angle and UVs.
      turned block = [([V2 (x + w / 2) (y + h / 2), V2 w h], angle, (u0, v0, u1, v1)) | [DrawImageRotated (Rect x y w h) angle _ u0 v0 u1 v1 _] <- [block]]
  -- A flip keeps a rect's size positive, and turns gradients and images over.
  assertEq failed [] . map fst . filter (not . snd) . zip [0 :: Int ..] $
    [ [FillRect (Rect 15 25 10 20) red] == under (P.translate 5 5) (drawRect r0 red)
    , [FillRect (Rect (-20) 20 10 20) red] == under (P.scale (-1) 1) (drawRect r0 red)
    , [FillRoundedRect (Rect 20 40 20 40) 8 red] == under (P.scale 2 2) (drawRoundedRect r0 4 red)
    , [DrawText 17 5 0.5 0.5 "hi" red] == under (P.translate 5 5 <> P.scale 3 3) (drawText (V2 4 0) AlignCenter AlignMiddle "hi" red)
    , [FillQuadGradient (Rect (-10) 0 10 10) blue red red blue] == under (P.scale (-1) 1) (drawLinearGradientH (Rect 0 0 10 10) red blue)
    , [FillQuadGradient (Rect 0 (-10) 10 10) blue blue red red] == under (P.scale 1 (-1)) (drawLinearGradientV (Rect 0 0 10 10) red blue)
    , [DrawImageRect (Rect (-10) 0 10 10) 7 1 0 0 1 red] == under (P.scale (-1) 1) (drawImage (Rect 0 0 10 10) (ImageId 7) red)
    ]
  -- A quarter turn keeps rects, and turns a gradient's corners with it.
  assertNear failed 1e-4 [V2 (-20) 0, V2 20 10] $
    concat [[V2 x y, V2 w h] | [FillRect (Rect x y w h) c] <- [under (P.rotate (pi / 2)) (drawRect (Rect 0 0 10 20) red)], c == red]
  assertEq failed [[white, red, green, blue]] $
    [[tl, tr, br, bl] | [FillQuadGradient _ tl tr br bl] <- [under (P.rotate (pi / 2)) (drawQuadGradient (Rect 0 0 10 10) red green blue white)]]
  assertNear failed 1e-4 [V2 2 0, V2 2 20, V2 4 0] $
    concat [[V2 x0 y0, V2 x1 y1, V2 w 0] | [StrokeLineAA x0 y0 x1 y1 w c] <- [under (P.translate 2 0 <> P.rotate (pi / 2) <> P.scale 4 4) (drawStrokeAA (V2 0 0) (V2 5 0) 1 red)], c == red]
  -- A turned rect is a polygon of the same area.
  single failed (fills (under (P.rotateAround (V2 15 30) (pi / 5)) (drawRect r0 red))) $ \(pts, tris) -> do
    assertEq failed 4 (length pts)
    assertLt failed (abs (fst (covered pts tris) - 200)) 1e-2
    assertNear failed 1e-3 [V2 15 30] [V2 (sum [x | V2 x _ <- pts] / 4) (sum [y | V2 _ y <- pts] / 4)]
  -- A circle stays one under a turn and a uniform scale; a scale on one axis makes an ellipse.
  assertNear failed 1e-4 [V2 20 0, V2 10 0] $
    concat [[V2 x y, V2 r 0] | [FillCircle x y r _] <- [under (P.rotate (pi / 2) <> P.scale 2 2) (drawCircle (V2 0 (-10)) 5 red)]]
  single failed (fills (under (P.scale 2 1) (drawCircle (V2 10 10) 5 red))) $ assert failed . all (onEllipse (V2 20 10) 10 5) . fst
  single failed (strokes (under (P.scale 1 2) (drawStrokeCircle (V2 0 0) 10 2 red))) $ \(pts, closed) ->
    assert failed (closed && all (onEllipse (V2 0 0) 9 18) pts)
  -- A turning transform turns an image, a turned one further, and a flip turns it over.
  forM_
    [ (under (P.translate 30 0 <> P.rotate (pi / 2)) (drawImage (Rect 0 0 20 10) (ImageId 7) red), [V2 25 10, V2 20 10], pi / 2, (0, 0, 1, 1))
    , (under (P.rotate 0.3 <> P.scale 2 2) (drawImageRotated (Rect 0 0 10 10) 0.2 (ImageId 7) red), [V2 (10 * cos 0.3 - 10 * sin 0.3) (10 * sin 0.3 + 10 * cos 0.3), V2 20 20], 0.5, (0, 0, 1, 1))
    , (under (P.scale (-1) 1) (drawImageRotated (Rect 0 0 10 10) 0.2 (ImageId 7) red), [V2 (-5) 5, V2 10 10], pi - 0.2, (0, 1, 1, 0))
    ]
    $ \(block, want, wantAngle, uvs) -> single failed (turned block) $ \(got, angle, uv) -> do
      assertNear failed 1e-3 want got
      assert failed (abs (angle - wantAngle) < 1e-5 && uv == uvs)

-- | Degenerate paths draw nothing or only their sound parts, never NaN, infinite or unbounded.
runDegenerateTest :: Context -> IORef Int -> IO ()
runDegenerateTest _ failed = do
  let nan = 0 / 0 :: Float
      inf = 1 / 0 :: Float
      p = V2 5 5
      numbersIn op = case op of
        FillPolygon pts _ _ -> primArrayToList pts
        StrokePolyline pts w _ _ -> w : primArrayToList pts
        FillRect (Rect x y w h) _ -> [x, y, w, h]
        FillCircle x y r _ -> [x, y, r]
        _ -> []
      sound ops = all (\v -> not (isNaN v || isInfinite v)) (concatMap numbersIn ops)
      both path = opsAt 1 (drawPath path red >> drawStrokePath path 2 black)
      lines' = map fst . stroked
      segment w = opsAt 1 (drawStrokePath (P.polyline [V2 0 0, V2 10 0]) w black)
  -- Zero lengths and radii, collinear points, NaNs, bad widths and NaN transforms draw nothing.
  assertEq failed [] . map fst . filter (not . null . snd) . zip [0 :: Int ..] $
    [ both (P.moveTo p <> P.lineTo p <> P.lineTo p), both (P.moveTo p <> P.lineTo p <> P.close), both (P.circle p 0)
    , both (P.arc p 0 0 pi), both (P.moveTo p), both mempty, both (P.polygon [])
    , opsAt 1 (drawPath (P.polygon [V2 0 0, V2 5 0, V2 10 0]) red)
    , both (P.circle p nan), both (P.arc p 10 0 nan), segment nan, segment (-1)
    , opsAt 1 (withTransform (P.scale nan 1) (drawPath (P.circle p 5) red >> drawRect (Rect 0 0 5 5) red >> drawCircle p 3 red))
    , opsAt 1 (withTransform (P.rotate inf) (drawPath (P.circle p 5) red))
    ]
  -- Repeated points drop out of a line, and a segment with a NaN or infinity is skipped.
  assertEq failed [[V2 0 0, V2 10 0, V2 10 10]] (lines' (P.polyline [V2 0 0, V2 0 0, V2 10 0, V2 10 0, V2 10 10]))
  assertEq failed [[V2 0 0, V2 10 0]] (lines' (P.polyline [V2 0 0, V2 nan 3, V2 inf 0, V2 10 0]))
  assertEq failed [[V2 0 0, V2 10 0]] (lines' (P.moveTo (V2 0 0) <> P.cubicTo (V2 nan 0) (V2 1 1) (V2 2 2) <> P.arcTo (V2 inf 1) 0 False False (V2 3 3) <> P.lineTo (V2 10 0)))
  -- Huge shapes stay bounded, and a sweep of many turns draws one.
  forM_ [P.circle p 1e7, P.moveTo (V2 0 0) <> P.cubicTo (V2 1e9 0) (V2 (-1e9) 1e9) (V2 1e9 1e9)] $ \path ->
    let n = sum (map length (lines' path)) in assert failed (n > 4 && n <= 1025)
  assert failed (opsAt 1 (drawPath (P.moveTo (V2 30 0) <> P.arc (V2 0 0) 30 0 (2 * pi)) red) == opsAt 1 (drawPath (P.moveTo (V2 30 0) <> P.arc (V2 0 0) 30 0 1e9) red))
  assert failed (opsAt 1 (drawPath (P.circle (V2 0 0) (-30)) red) == opsAt 1 (drawPath (P.circle (V2 0 0) 30) red))
  -- An arc on a radius far larger than its chord is a line to its end, the subpath kept.
  forM_ [1e7, 1e9, 1e12, 1e20] $ \radius ->
    assertEq failed [[V2 0 0, V2 20 0, V2 20 20]] (lines' (P.moveTo (V2 0 0) <> P.arcTo (V2 radius radius) 0 False True (V2 20 0) <> P.lineTo (V2 20 20)))
  assertEq failed [2] (map length (lines' (P.arc (V2 0 1e8) 1e8 (-pi / 2) 1e-6)))
  -- A whole turn ends exactly where it started: no sliver of an edge where a ring closes.
  forM_ [(V2 0 0, 3000), (V2 123.4 (-56.7), 777)] $ \(centre, radius) ->
    single failed (strokes (opsAt 2 (drawStrokePath (P.circle centre radius) 1 black))) $ \(pts, closed) -> do
      assert failed closed
      assertGt failed (minimum (zipWith dist pts (drop 1 pts ++ take 1 pts))) 1
  -- A draw context with no sensible display scale flattens as for scale 1.
  forM_ [0, -2, nan, inf] $ \scale -> assert failed (opsAt 1 (drawPath (P.circle p 40) red) == opsAt scale (drawPath (P.circle p 40) red))
  -- A star drawn as a pentagram crosses itself; it fills in part, soundly.
  let crossing = opsAt 1 (drawPath (P.polygon [V2 (50 * cos a) (50 * sin a) | i <- [0 .. 4 :: Int], let a = 4 * pi * fromIntegral i / 5]) red)
  assert failed (sound crossing && all (\(pts, tris) -> snd (covered pts tris)) (fills crossing))
  assert failed (sound (both (P.polyline [V2 0 0, V2 nan nan, V2 1e30 1e30, V2 (-1e30) 0])))

-- | A canvas repaints only when what it draws changes, and flattens for its display.
runCanvasFramesTest :: Context -> IORef Int -> IO ()
runCanvasFramesTest ctx failed = do
  let inp = withInputOff 400 300
      ui turn = column $ do
        label "Other"
        drawn <- canvas (fixedWH 120 120) $ \(Rect x y w h) ->
          withTransform (P.rotateAround (V2 (x + w / 2) (y + h / 2)) turn) $ do
            drawPath (P.roundedRect (Rect (x + 20) (y + 20) 80 80) 10) red
            drawStrokePath (P.circle (V2 (x + 60) (y + 60)) 30) 2 black
        (keyed, ()) <- customWidget defaultCustomWidgetSpec
          { widgetLayout = fixedWH 60 60 defaultLayout
          , widgetContent = 1
          , widgetDraw = \cdc (Rect x y _ _) -> runCanvasFor cdc (drawPath (P.circle (V2 (x + 30) (y + 30)) 25) red)
          }
        pure (drawn, keyed)
      -- Each cached canvas's filled points.
      pointsIn c = do
        dc <- readIORef (ctxDrawingCache c)
        pure [sum [sizeofPrimArray pts `div` 2 | FillPolygon pts _ _ <- toList (cdeOps e)] | e <- IM.elems (dcsCustomDrawOpCache dc)]
  (drawn, keyed) <- warmup2 ctx inp (ui 0)
  _ <- runFrame ctx inp (ui 0)
  takeDamage ctx >>= \same -> assert failed (not (any (damageCovers same . respRect) [drawn, keyed]))
  _ <- runFrame ctx inp (ui 0.4)
  takeDamage ctx >>= assert failed . (`clipCovers` respRect drawn)
  -- Both flatten finer on a denser display, the keyed one too though its key is unchanged.
  before <- pointsIn ctx
  let dense = withFontMetrics ctx (ctxFontMetrics ctx) {fmSnapScale = 3}
  _ <- warmup2 dense inp (ui 0.4)
  after <- pointsIn dense
  assertEq failed (2, 2, True) (length before, length after, and (zipWith (<) before after))
