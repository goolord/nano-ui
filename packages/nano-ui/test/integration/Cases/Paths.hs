-- The deprecated 'runCanvas' is still tested.
{-# OPTIONS_GHC -Wno-deprecations #-}

module Cases.Paths (tests) where

import Spec
import Data.Foldable (toList)
import Data.IntMap.Strict qualified as IM
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, primArrayToList, sizeofPrimArray)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Storable (peekByteOff)
import NanoUI.Internal.Context (setDrawSnapScale)
import GHC.Stack (HasCallStack)
import NanoUI.Internal.Context (Context (..), DrawingCacheState (..))
import NanoUI.Internal.Path (fillPathOps)
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
  , spec "path-fill-holes" runFillHolesTest
  , spec "path-stroke-dashes" runDashTest
  , spec "path-stroke-joins" runJoinGeometryTest
  , spec "path-gradients" runGradientTest
  , spec "path-clip" runClipTest
  , spec "path-shapes-and-inverse" runShapesAndInverseTest
  , spec "canvas-configured" runCanvasConfiguredTest
  ]

red, black :: Color
red = colorRGBA 220 40 40 255
black = colorRGBA 0 0 0 255

-- | A canvas block's ops, flattened at @scale@ device pixels per logical pixel.
opsAt :: Float -> CanvasM () -> [DrawOp]
opsAt scale = toList . runCanvasFor (CustomDrawContext False False False False False defaultTheme (monospaceMetrics 16) {fmSnapScale = scale})

pairs :: PrimArray Float -> [V2]
pairs pts = [V2 (indexPrimArray pts (2 * i)) (indexPrimArray pts (2 * i + 1)) | i <- [0 .. sizeofPrimArray pts `div` 2 - 1]]

-- | Each stroke's points and whether it closes.
strokes :: [DrawOp] -> [([V2], Bool)]
strokes ops = [(pairs pts, closed) | StrokePolyline pts _ closed _ _ _ _ <- ops]

-- | Each filled polygon's outline and triangles.
fills :: [DrawOp] -> [([V2], [Int])]
fills ops = [(pairs pts, primArrayToList tris) | FillPolygon pts _ tris _ <- ops]

-- | A path's strokes (width 1) and fills, at scale 1.
stroked :: P.Path -> [([V2], Bool)]
stroked path = strokes (opsAt 1 (drawStrokePath path 1 black))

filled :: P.Path -> [([V2], [Int])]
filled path = fills (opsAt 1 (drawPath path red))

-- | Check the single element; fail if there is not exactly one.
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

-- | Distance from a point to an open polyline.
polyDist :: [V2] -> V2 -> Float
polyDist ps p@(V2 px py) = minimum (zipWith seg ps (drop 1 ps))
  where
    seg (V2 ax ay) (V2 bx by) =
      let (dx, dy) = (bx - ax, by - ay)
          t = max 0 (min 1 (((px - ax) * dx + (py - ay) * dy) / max 1e-12 (dx * dx + dy * dy)))
       in dist p (V2 (ax + t * dx) (ay + t * dy))

-- | Signed shoelace area, positive for a ring that is clockwise on screen.
polyArea :: [V2] -> Float
polyArea ps = sum [x0 * y1 - x1 * y0 | (V2 x0 y0, V2 x1 y1) <- zip ps (drop 1 ps ++ take 1 ps)] / 2

-- | Total triangle area, and whether the index list is well formed.
covered :: [V2] -> [Int] -> (Float, Bool)
covered ps tris = (sum (map area (triples tris)), all (\i -> i >= 0 && i < length ps) tris && length tris `mod` 3 == 0)
  where
    triples (a : b : c : rest) = (ps !! a, ps !! b, ps !! c) : triples rest
    triples _ = []
    area (V2 x0 y0, V2 x1 y1, V2 x2 y2) = abs ((x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)) / 2

-- | Evaluate the Bezier curve with these control points at @t@.
bezier :: [V2] -> Float -> V2
bezier [p] _ = p
bezier ps t = bezier (zipWith (\(V2 x0 y0) (V2 x1 y1) -> V2 (x0 + t * (x1 - x0)) (y0 + t * (y1 - y0))) ps (drop 1 ps)) t

-- | Whether a point lies on the axis-aligned ellipse centred at @c@ with radii @rx@ and @ry@.
onEllipse :: V2 -> Float -> Float -> V2 -> Bool
onEllipse (V2 cx cy) rx ry (V2 x y) = abs (((x - cx) / rx) ^ (2 :: Int) + ((y - cy) / ry) ^ (2 :: Int) - 1) < 1e-3

-- | Circle chords stay within a quarter device pixel of the circle at every
-- scale, and are not much finer than that.
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
  -- 'runCanvas' has no display scale and flattens as if at scale 2.
  assert failed (opsAt 2 (drawPath (P.circle c 50) red) == toList (runCanvas (drawPath (P.circle c 50) red)))

-- | A flattened Bezier keeps its end points and stays within tolerance of the curve.
runBezierFlatteningTest :: Context -> IORef Int -> IO ()
runBezierFlatteningTest _ failed = do
  let (p0, c1, c2, q, p3) = (V2 10 10, V2 400 (-100), V2 (-200) 300, V2 150 (-200), V2 300 300)
      samples = [fromIntegral k / 2000 | k <- [0 .. 2000 :: Int]]
  forM_ [(s, c) | s <- [1, 2], c <- [(P.cubicTo c1 c2 p3, [p0, c1, c2, p3]), (P.quadTo q p3, [p0, q, p3])]] $ \(scale, (seg, ctrl)) ->
    single failed (strokes (opsAt scale (drawStrokePath (P.moveTo p0 <> seg) 1 black))) $ \(pts, closed) -> do
      assertEq failed (False, [p0, p3]) (closed, ends pts)
      assertLt failed (maximum (map (polyDist pts . bezier ctrl) samples)) (0.25 / scale + 1e-3)
      -- Every emitted point lies on the curve.
      assert failed (all (\p -> minimum (map (dist p . bezier ctrl) samples) < 1) pts)
  -- A straight "curve" is one chord.
  single failed (stroked (P.moveTo p0 <> P.cubicTo (V2 20 10) (V2 30 10) (V2 40 10))) $ \(pts, _) -> assertEq failed [p0, V2 40 10] pts

-- | Arcs end at their end angle. SVG arcs end at their end point, with the
-- side and size picked by the flags.
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
  -- An arc after a moveTo draws a line to the arc's start (a pie slice).
  single failed (filled (P.moveTo c <> P.arc c 30 0 (pi / 2) <> P.close)) $ \(pts, _) -> do
    assertEq failed [c] (take 1 pts)
    assertLt failed (abs (abs (polyArea pts) - 30 * 30 * pi / 4)) 30
  -- A clockwise SVG arc from (0, 0) to (20, 0) goes over the top.
  single failed (svgArc 10 False True) $ \(pts, _) -> do
    assertEq failed [V2 0 0, V2 20 0] (ends pts)
    assert failed (onSmallCircle pts)
    assertLt failed (minimum (ys pts)) (-9.7)
  single failed (svgArc 10 False False) $ \(pts, _) -> assertGt failed (maximum (ys pts)) 9.7
  -- Radii too small to span the end points are scaled up until they do.
  single failed (svgArc 2 False True) $ assert failed . onSmallCircle . fst
  -- With radius 20, the large arc bulges further than the small one.
  single failed (svgArc 20 True True) $ \(pts, _) -> assertLt failed (minimum (ys pts)) (-37)
  single failed (svgArc 20 False True) $ \(pts, _) -> assertGt failed (minimum (ys pts)) (-2.7)
  -- A zero radius is a straight line.
  single failed (svgArc 0 False True) $ assertEq failed [V2 0 0, V2 20 0] . fst
  -- An elliptical arc follows the ellipse's rotation.
  single failed (stroked (P.ellipticalArc c (V2 40 20) (pi / 2) 0 pi)) $ \(pts, _) -> do
    assertNear failed 1e-3 [V2 50 90, V2 50 10] (ends pts)
    assert failed (all (onEllipse c 20 40) pts)
    assertLt failed (abs (minimum [x | V2 x _ <- pts] - 30)) 0.3

-- | Open and closed strokes, subpaths, and line caps.
runStrokeTest :: Context -> IORef Int -> IO ()
runStrokeTest _ failed = do
  let (a, b, c) = (V2 0 0, V2 10 0, V2 10 10)
      plain ops = [(primArrayToList pts, w, closed) | StrokePolyline pts w closed _ _ _ _ <- ops]
      line w path = plain (opsAt 1 (drawStrokePath path w black))
      capped cap path = opsAt 1 (drawStrokePathWith (P.stroke 4) {P.strokeCap = cap} path (P.Solid black))
      caps ops = [cap | StrokePolyline _ _ _ cap _ _ _ <- ops]
  assertEq failed [([0, 0, 10, 0, 10, 10], 3, False)] (line 3 (P.polyline [a, b, c]))
  -- Closing drops a last point that repeats the first.
  assertEq failed [([0, 0, 10, 0, 10, 10], 2, True)] (line 2 (P.moveTo a <> P.lineTo b <> P.lineTo c <> P.lineTo a <> P.close))
  -- Returning to the start without closing leaves the line open.
  assertEq failed [([0, 0, 10, 0, 10, 10, 0, 0], 2, False)] (line 2 (P.moveTo a <> P.lineTo b <> P.lineTo c <> P.lineTo a))
  -- Each subpath is a separate polyline. A close returns to the subpath's start.
  assertEq failed [([0, 0, 10, 0, 10, 10], 1, True), ([0, 0, 0, 20], 1, False)] (line 1 (P.polygon [a, b, c] <> P.lineTo (V2 0 20)))
  assertEq failed 2 (length (opsAt 1 (drawStrokePath (P.polyline [a, b] <> P.polyline [c, V2 20 20]) 1 black)))
  -- Caps are carried on the polyline op. A closed subpath has none, and a
  -- closed two-point subpath becomes an open line with butt caps.
  forM_ [P.ButtCap, P.SquareCap, P.RoundCap] $ \cap -> do
    let ops = capped cap (P.polyline [a, b])
    assertEq failed ([([0, 0, 10, 0], 4, False)], [cap]) (plain ops, caps ops)
  assertEq failed [([0, 0, 10, 0, 10, 10], 4, True)] (plain (capped P.RoundCap (P.polygon [a, b, c])))
  assertEq failed [P.ButtCap] (caps (capped P.RoundCap (P.moveTo a <> P.lineTo b <> P.close)))
  -- A fill closes an open subpath.
  assertEq failed [([a, b, c], [0, 1, 2])] (filled (P.polyline [a, b, c]))

-- | Each subpath fills as one polygon whose triangles cover exactly its area, in either winding.
runFillTriangulationTest :: Context -> IORef Int -> IO ()
runFillTriangulationTest _ failed = do
  let c = V2 60 60
      -- @k@ corners around the centre; corner @i@ is at distance @rad i@.
      ring k rad = [V2 (60 + rad i * cos a) (60 + rad i * sin a) | i <- [0 .. k - 1 :: Int], let a = 2 * pi * fromIntegral i / fromIntegral k]
      star = ring 10 (\i -> if even i then 50 else 20)
      ell = [V2 0 0, V2 60 0, V2 60 20, V2 20 20, V2 20 60, V2 0 60]
      comb = [V2 0 0, V2 100 0, V2 100 40, V2 80 40, V2 80 10, V2 60 10, V2 60 40, V2 40 40, V2 40 10, V2 20 10, V2 20 40, V2 0 40]
      -- Deterministic pseudo-random number in [0, 1).
      rnd :: Int -> Float
      rnd k = let v = sin (fromIntegral k * 12.9898) * 43758.5453 in v - fromIntegral (floor v :: Int)
      shapes =
        [ ("circle", P.circle c 50), ("ellipse", P.ellipse c (V2 50 20)), ("rounded rect", P.roundedRect (Rect 0 0 100 60) 12)
        , ("hexagon", P.polygon (ring 6 (const 40))), ("star", P.polygon star), ("star anticlockwise", P.polygon (reverse star))
        , ("L", P.polygon ell), ("comb", P.polygon comb)
        , ("band", P.arc c 50 0 (1.5 * pi) <> P.arc c 25 (1.5 * pi) (-1.5 * pi) <> P.close)
        , ("blob", P.moveTo (V2 0 0) <> P.cubicTo (V2 60 (-30)) (V2 100 30) (V2 100 60) <> P.quadTo (V2 50 100) (V2 0 60) <> P.close)
          -- A one-slice pie chart: out to the rim and back along the same radius.
        , ("whole pie", P.moveTo c <> P.arc c 50 (-pi / 2) (2 * pi) <> P.close)
        , ("pie over half", P.moveTo c <> P.arc c 50 0.3 (1.6 * pi) <> P.close)
        ]
          -- Random simple star polygons with 5 to 64 corners.
          <> [("random star " <> show seed, P.polygon (ring (5 + seed `mod` 60) (\i -> 5 + 50 * rnd (seed * 1000 + i)))) | seed <- [1 .. 60 :: Int]]
  forM_ shapes $ \(name, shape) ->
    assertEq failed (name, [True]) . (,) name $
      [ inRange && length tris == 3 * (length pts - 2) && abs (area - want) <= 1e-3 * want
      | (pts, tris) <- filled shape
      , let (area, inRange) = covered pts tris
            want = abs (polyArea pts)
      ]
  -- Filled area matches the shape: exactly for straight sides, within tolerance for curves.
  let areaOf shape = sum [abs (polyArea pts) | (pts, _) <- filled shape]
  assertLt failed (abs (areaOf (P.polygon ell) - 2000)) 1e-3
  assertLt failed (abs (areaOf (P.polygon comb) - 2800)) 1e-3
  assertLt failed (abs (areaOf (P.circle c 50) - pi * 50 * 50)) (2 * pi * 50 * 0.25)
  assertLt failed (abs (areaOf (P.roundedRect (Rect 0 0 100 60) 12) - (6000 - (4 - pi) * 144))) (2 * pi * 12 * 0.25)
  -- Disjoint subpaths fill as separate polygons. Under the non-zero rule, a
  -- subpath inside another with the same winding merges into it.
  assertEq failed [2, 1] [length (filled (P.rect r0 <> P.rect r1)) | (r0, r1) <- [(Rect 0 0 10 10, Rect 20 0 10 10), (Rect 0 0 100 100, Rect 20 20 10 10)]]

-- | Transforms compose inside out, apply before flattening, and scale stroke widths.
runTransformTest :: Context -> IORef Int -> IO ()
runTransformTest _ failed = do
  let tri = [V2 1 0, V2 2 0, V2 2 1]
      triangle = drawPath (P.polygon tri) red
      outline block = concat [pts | (pts, _) <- fills (opsAt 1 block)]
      at = map . P.transformPoint
      -- Rotate a quarter turn, (x, y) to (-y, x), then scale.
      rotatedThenScaled = [V2 (-2 * y) (3 * x) | V2 x y <- tri]
      points block = sum [length pts | (pts, _) <- strokes (opsAt 1 block)]
      circle r = drawStrokePath (P.circle (V2 0 0) r) 1 black
      widths t w = [w' | StrokePolyline _ w' _ _ _ _ _ <- opsAt 1 (withTransform t (drawStrokePath (P.polyline [V2 0 0, V2 1 1]) w black))]
  forM_
    [ (rotatedThenScaled, outline (withTransform (P.scale 2 3) (withTransform (P.rotate (pi / 2)) triangle)))
    , (rotatedThenScaled, outline (withTransform (P.scale 2 3 <> P.rotate (pi / 2)) triangle))
    , ([V2 (-3 * y) (2 * x) | V2 x y <- tri], outline (withTransform (P.rotate (pi / 2) <> P.scale 2 3) triangle))
    , (rotatedThenScaled, at (P.scale 2 3 <> P.rotate (pi / 2)) tri)
    , ([V2 9 9], at (P.rotateAround (V2 9 9) 1.1) [V2 9 9]), ([V2 13 4], at (P.translate 3 4 <> P.affine 1 0 0 1 10 0) [V2 0 0])
    , (tri, outline (withTransform mempty triangle))
    , -- A transform applies only inside its block.
      (at (P.translate 5 0) tri <> tri, outline (withTransform (P.translate 5 0) triangle >> triangle))
    ]
    $ uncurry (assertNear failed 1e-4)
  -- Flattening uses the transformed size.
  assertGt failed (points (withTransform (P.scale 10 10) (circle 10))) (points (circle 10))
  assertEq failed (points (circle 100)) (points (withTransform (P.scale 10 10) (circle 10)))
  assertEq failed (points (circle 100)) (points (withTransform (P.scale 10 1) (circle 10)))
  -- A stroke's width scales with the transform.
  assertEq failed ([6], [4]) (widths (P.scale 3 3) 2, widths (P.scale 8 2) 1)

-- | Non-path canvas ops follow a transform as far as their shape allows.
runTransformOpsTest :: Context -> IORef Int -> IO ()
runTransformOpsTest _ failed = do
  let (blue, green, white) = (colorRGBA 0 0 255 255, colorRGBA 0 255 0 255, colorRGBA 255 255 255 255)
      under t block = opsAt 1 (withTransform t block)
      r0 = Rect 10 20 10 20
      -- If the block is a single rotated image: its centre, size, angle and UVs.
      turned block = [([V2 (x + w / 2) (y + h / 2), V2 w h], angle, (u0, v0, u1, v1)) | [DrawImage (Rect x y w h) angle _ u0 v0 u1 v1 _] <- [block], angle /= 0]
      turnedImage r angle = drawImageWith (imageDraw r (ImageId 7)) {imageAngle = angle, imageTint = red}
  -- A flip keeps rect sizes positive and mirrors gradients and images.
  assertEq failed [] . map fst . filter (not . snd) . zip [0 :: Int ..] $
    [ [FillRect (Rect 15 25 10 20) red] == under (P.translate 5 5) (drawRect r0 red)
    , [FillRect (Rect (-20) 20 10 20) red] == under (P.scale (-1) 1) (drawRect r0 red)
    , [FillRoundedRect (Rect 20 40 20 40) 8 red] == under (P.scale 2 2) (drawRoundedRect r0 4 red)
    , [DrawText 9 5 0.5 0.5 "hi" red] == under (P.translate 5 5) (drawText (V2 4 0) AlignCenter AlignMiddle "hi" red)
      -- A rotation leaves text as plain 'DrawText'; a scale scales the text.
    , case under (P.rotate 0.3) (drawText (V2 4 0) AlignCenter AlignMiddle "hi" red) of [DrawText {}] -> True; _ -> False
    , [DrawTextAligned 17 5 0.5 0.5 3 defaultTextFont "hi" red] == under (P.translate 5 5 <> P.scale 3 3) (drawText (V2 4 0) AlignCenter AlignMiddle "hi" red)
    , [DrawTextAligned 17 5 0 1 6 defaultTextFont {textFontSize = 12} "hi" red] == under (P.translate 5 5 <> P.scale 3 3) (withTransform (P.scale 2 2) (drawTextWith defaultTextFont {textFontSize = 12} (V2 2 0) AlignStart AlignTop "hi" red))
    , [FillQuadGradient (Rect (-10) 0 10 10) blue red red blue] == under (P.scale (-1) 1) (drawLinearGradientH (Rect 0 0 10 10) red blue)
    , [FillQuadGradient (Rect 0 (-10) 10 10) blue blue red red] == under (P.scale 1 (-1)) (drawLinearGradientV (Rect 0 0 10 10) red blue)
    , [DrawImageRect (Rect (-10) 0 10 10) 7 1 0 0 1 red] == under (P.scale (-1) 1) (drawImage (Rect 0 0 10 10) (ImageId 7) red)
    ]
  -- A quarter turn keeps rects as rects and rotates a gradient's corner colours.
  assertNear failed 1e-4 [V2 (-20) 0, V2 20 10] $
    concat [[V2 x y, V2 w h] | [FillRect (Rect x y w h) c] <- [under (P.rotate (pi / 2)) (drawRect (Rect 0 0 10 20) red)], c == red]
  assertEq failed [[white, red, green, blue]] $
    [[tl, tr, br, bl] | [FillQuadGradient _ tl tr br bl] <- [under (P.rotate (pi / 2)) (drawQuadGradient (Rect 0 0 10 10) red green blue white)]]
  assertNear failed 1e-4 [V2 2 0, V2 2 20, V2 4 0] $
    concat [[V2 x0 y0, V2 x1 y1, V2 w 0] | [StrokeLineAA x0 y0 x1 y1 w c] <- [under (P.translate 2 0 <> P.rotate (pi / 2) <> P.scale 4 4) (drawStrokeAA (V2 0 0) (V2 5 0) 1 red)], c == red]
  -- A rect at any other angle becomes a polygon of the same area.
  single failed (fills (under (P.rotateAround (V2 15 30) (pi / 5)) (drawRect r0 red))) $ \(pts, tris) -> do
    assertEq failed 4 (length pts)
    assertLt failed (abs (fst (covered pts tris) - 200)) 1e-2
    assertNear failed 1e-3 [V2 15 30] [V2 (sum [x | V2 x _ <- pts] / 4) (sum [y | V2 _ y <- pts] / 4)]
  -- A circle stays a circle under rotation and uniform scale; a non-uniform scale makes an ellipse.
  assertNear failed 1e-4 [V2 20 0, V2 10 0] $
    concat [[V2 x y, V2 r 0] | [FillCircle x y r _] <- [under (P.rotate (pi / 2) <> P.scale 2 2) (drawCircle (V2 0 (-10)) 5 red)]]
  single failed (fills (under (P.scale 2 1) (drawCircle (V2 10 10) 5 red))) $ assert failed . all (onEllipse (V2 20 10) 10 5) . fst
  single failed (strokes (under (P.scale 1 2) (drawStrokeCircle (V2 0 0) 10 2 red))) $ \(pts, closed) ->
    assert failed (closed && all (onEllipse (V2 0 0) 9 18) pts)
  -- A rotation rotates an image, adding to its own angle; a flip mirrors it.
  forM_
    [ (under (P.translate 30 0 <> P.rotate (pi / 2)) (drawImage (Rect 0 0 20 10) (ImageId 7) red), [V2 25 10, V2 20 10], pi / 2, (0, 0, 1, 1))
    , (under (P.rotate 0.3 <> P.scale 2 2) (turnedImage (Rect 0 0 10 10) 0.2), [V2 (10 * cos 0.3 - 10 * sin 0.3) (10 * sin 0.3 + 10 * cos 0.3), V2 20 20], 0.5, (0, 0, 1, 1))
    , (under (P.scale (-1) 1) (turnedImage (Rect 0 0 10 10) 0.2), [V2 (-5) 5, V2 10 10], pi - 0.2, (0, 1, 1, 0))
    ]
    $ \(block, want, wantAngle, uvs) -> single failed (turned block) $ \(got, angle, uv) -> do
      assertNear failed 1e-3 want got
      assert failed (abs (angle - wantAngle) < 1e-5 && uv == uvs)

-- | Degenerate paths draw nothing, or only their valid parts. Output never
-- contains NaN or infinity, and point counts stay bounded.
runDegenerateTest :: Context -> IORef Int -> IO ()
runDegenerateTest _ failed = do
  let nan = 0 / 0 :: Float
      inf = 1 / 0 :: Float
      p = V2 5 5
      numbersIn op = case op of
        FillPolygon pts _ _ _ -> primArrayToList pts
        StrokePolyline pts w _ _ _ _ _ -> w : primArrayToList pts
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
  -- Repeated points are dropped, and segments with NaN or infinity are skipped.
  assertEq failed [[V2 0 0, V2 10 0, V2 10 10]] (lines' (P.polyline [V2 0 0, V2 0 0, V2 10 0, V2 10 0, V2 10 10]))
  assertEq failed [[V2 0 0, V2 10 0]] (lines' (P.polyline [V2 0 0, V2 nan 3, V2 inf 0, V2 10 0]))
  assertEq failed [[V2 0 0, V2 10 0]] (lines' (P.moveTo (V2 0 0) <> P.cubicTo (V2 nan 0) (V2 1 1) (V2 2 2) <> P.arcTo (V2 inf 1) 0 False False (V2 3 3) <> P.lineTo (V2 10 0)))
  -- Huge shapes get a bounded point count; a sweep of many turns draws one turn.
  forM_ [P.circle p 1e7, P.moveTo (V2 0 0) <> P.cubicTo (V2 1e9 0) (V2 (-1e9) 1e9) (V2 1e9 1e9)] $ \path ->
    let n = sum (map length (lines' path)) in assert failed (n > 4 && n <= 1025)
  assert failed (opsAt 1 (drawPath (P.moveTo (V2 30 0) <> P.arc (V2 0 0) 30 0 (2 * pi)) red) == opsAt 1 (drawPath (P.moveTo (V2 30 0) <> P.arc (V2 0 0) 30 0 1e9) red))
  assert failed (opsAt 1 (drawPath (P.circle (V2 0 0) (-30)) red) == opsAt 1 (drawPath (P.circle (V2 0 0) 30) red))
  -- An arc whose radius dwarfs its chord becomes a line, and the subpath continues.
  forM_ [1e7, 1e9, 1e12, 1e20] $ \radius ->
    assertEq failed [[V2 0 0, V2 20 0, V2 20 20]] (lines' (P.moveTo (V2 0 0) <> P.arcTo (V2 radius radius) 0 False True (V2 20 0) <> P.lineTo (V2 20 20)))
  assertEq failed [2] (map length (lines' (P.arc (V2 0 1e8) 1e8 (-pi / 2) 1e-6)))
  -- A full circle closes exactly at its start, with no sliver edge at the seam.
  forM_ [(V2 0 0, 3000), (V2 123.4 (-56.7), 777)] $ \(centre, radius) ->
    single failed (strokes (opsAt 2 (drawStrokePath (P.circle centre radius) 1 black))) $ \(pts, closed) -> do
      assert failed closed
      assertGt failed (minimum (zipWith dist pts (drop 1 pts ++ take 1 pts))) 1
  -- An invalid display scale flattens as scale 1.
  forM_ [0, -2, nan, inf] $ \scale -> assert failed (opsAt 1 (drawPath (P.circle p 40) red) == opsAt scale (drawPath (P.circle p 40) red))
  -- A self-intersecting pentagram fills partially, with valid output.
  let crossing = opsAt 1 (drawPath (P.polygon [V2 (50 * cos a) (50 * sin a) | i <- [0 .. 4 :: Int], let a = 4 * pi * fromIntegral i / 5]) red)
  assert failed (sound crossing && all (\(pts, tris) -> snd (covered pts tris)) (fills crossing))
  assert failed (sound (both (P.polyline [V2 0 0, V2 nan nan, V2 1e30 1e30, V2 (-1e30) 0])))

-- | A canvas repaints only when its drawing changes, and flattens for the display scale.
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
      -- Filled point count of each cached canvas.
      pointsIn c = do
        dc <- readIORef (ctxDrawingCache c)
        pure [sum [sizeofPrimArray pts `div` 2 | FillPolygon pts _ _ _ <- toList (cdeOps e)] | e <- IM.elems (dcsCustomDrawOpCache dc)]
  (drawn, keyed) <- warmup2 ctx inp (ui 0)
  _ <- runFrame ctx inp (ui 0)
  takeDamage ctx >>= \same -> assert failed (not (any (damageCovers same . respRect) [drawn, keyed]))
  _ <- runFrame ctx inp (ui 0.4)
  takeDamage ctx >>= assert failed . (`clipCovers` respRect drawn)
  -- Both flatten finer at a higher display scale, including the keyed one whose key is unchanged.
  before <- pointsIn ctx
  let dense = withFontMetrics ctx (ctxFontMetrics ctx) {fmSnapScale = 3}
  _ <- warmup2 dense inp (ui 0.4)
  after <- pointsIn dense
  assertEq failed (2, 2, True) (length before, length after, and (zipWith (<) before after))

-- | Points, ring starts and triangles of each filled polygon under a fill rule.
polygons :: P.FillRule -> P.Path -> [([V2], [Int], [Int])]
polygons rule path = [(pairs pts, primArrayToList rings, primArrayToList tris) | FillPolygon pts rings tris _ <- opsAt 1 (drawPathWith rule path (P.Solid red))]

-- | Area covered by a polygon's triangles, or 'Nothing' if an index is out of range.
coverage :: ([V2], [Int], [Int]) -> Maybe Float
coverage (pts, _, tris) = let (a, ok) = covered pts tris in if ok then Just a else Nothing

-- | A square outline, clockwise on screen ('square') or anticlockwise ('squareBack').
square, squareBack :: Float -> Float -> Float -> P.Path
square x y s = P.polygon [V2 x y, V2 (x + s) y, V2 (x + s) (y + s), V2 x (y + s)]
squareBack x y s = P.polygon [V2 x y, V2 x (y + s), V2 (x + s) (y + s), V2 (x + s) y]

-- | Whether any triangle covers a point.
coversPoint :: ([V2], [Int], [Int]) -> V2 -> Bool
coversPoint (pts, _, tris) p = any inside (triples tris)
  where
    triples (a : b : c : rest) = (pts !! a, pts !! b, pts !! c) : triples rest
    triples _ = []
    side (V2 x0 y0) (V2 x1 y1) (V2 x y) = (x1 - x0) * (y - y0) - (y1 - y0) * (x - x0)
    inside (a, b, c) =
      let d1 = side a b p
          d2 = side b c p
          d3 = side c a p
       in (d1 > 0 && d2 > 0 && d3 > 0) || (d1 < 0 && d2 < 0 && d3 < 0)

-- | A nested subpath is a hole when the fill rule leaves it unfilled, and
-- the triangles cover exactly the filled area.
runFillHolesTest :: Context -> IORef Int -> IO ()
runFillHolesTest _ failed = do
  let c = V2 60 60
      -- A circle wound the other way.
      circleBack r = P.moveTo (V2 (60 + r) 60) <> P.arc c r 0 (-2 * pi) <> P.close
      areaNear want got = abs (got - want) <= 1e-3 * want
      check :: String -> P.FillRule -> P.Path -> [Float] -> IO ()
      check name rule path wantAreas =
        let got = map coverage (polygons rule path)
         in unless (length got == length wantAreas && and (zipWith (\w a -> maybe False (areaNear w) a) wantAreas got)) $
              assertEq failed (name, map Just wantAreas) (name, got)
  -- A square inside a square: even-odd cuts a hole; non-zero cuts one only
  -- when the inner square winds the other way.
  check "even-odd hole" P.EvenOdd (square 0 0 100 <> square 20 20 60) [6400]
  check "non-zero hole" P.NonZero (square 0 0 100 <> squareBack 20 20 60) [6400]
  check "non-zero same way" P.NonZero (square 0 0 100 <> square 20 20 60) [10000]
  check "two holes" P.EvenOdd (square 0 0 100 <> square 10 10 30 <> squareBack 60 10 30) [8200]
  -- An island inside a hole is a separate polygon.
  check "island" P.EvenOdd (square 0 0 100 <> square 20 20 60 <> square 40 40 20) [6400, 400]
  check "island non-zero" P.NonZero (square 0 0 100 <> squareBack 20 20 60 <> square 40 40 20) [6400, 400]
  -- Crossing subpaths fill separately.
  check "crossing" P.EvenOdd (square 0 0 60 <> square 40 40 60) [3600, 3600]
  -- Concentric circles: an annulus, like a glyph with a curved counter.
  let ringArea = sum (map (maybe 0 id . coverage) (polygons P.EvenOdd (P.circle c 50 <> P.circle c 30)))
  assertLt failed (abs (ringArea - pi * (50 * 50 - 30 * 30))) (2 * pi * 80 * 0.25)
  assertEq failed [3] [length rings | (_, rings, _) <- polygons P.NonZero (P.circle c 50 <> circleBack 30)]
  single failed (polygons P.EvenOdd (P.circle c 50 <> P.circle c 30)) $ \poly@(_, rings, _) -> do
    assertEq failed 3 (length rings)
    assert failed (not (coversPoint poly c) && coversPoint poly (V2 60 20))
  -- Many holes in a row, each a random convex polygon.
  let rnd :: Int -> Float
      rnd k = let v = sin (fromIntegral k * 12.9898) * 43758.5453 in v - fromIntegral (floor v :: Int)
      blob i = [V2 (25 + 50 * fromIntegral i + 12 * cos a) (50 + 12 * sin a) | k <- [0 .. 7 :: Int], let a = 2 * pi * (fromIntegral k + 0.4 * rnd (i * 10 + k)) / 8]
      blobs = map blob [0 .. 7 :: Int]
      shoelace ps = abs (sum [x0 * y1 - x1 * y0 | (V2 x0 y0, V2 x1 y1) <- zip ps (drop 1 ps ++ take 1 ps)]) / 2
  single failed (polygons P.EvenOdd (square 0 0 400 <> foldMap P.polygon blobs)) $ \poly ->
    assertLt failed (abs (maybe 0 id (coverage poly) - (160000 - sum (map shoelace blobs)))) 1
  -- A single subpath is one polygon with one ring.
  assertEq failed [[0, 4]] [rings | (_, rings, _) <- polygons P.EvenOdd (square 0 0 10)]
  -- 'fillPathOps', which backends use, matches the canvas output.
  assert failed (opsAt 1 (drawPathWith P.EvenOdd (square 0 0 10) (P.Solid red)) == fillPathOps 0.25 mempty P.EvenOdd (square 0 0 10) (P.Solid red))

-- | Dashes split a stroke by the pattern, measured in transformed lengths.
runDashTest :: Context -> IORef Int -> IO ()
runDashTest _ failed = do
  let dashed pattern off path = [(pairs pts, cap) | StrokePolyline pts _ closed cap _ _ _ <- opsAt 1 (drawStrokePathWith (P.stroke 2) {P.strokeDash = pattern, P.strokeDashOffset = off, P.strokeCap = P.RoundCap} path (P.Solid black)), not closed]
      line = P.polyline [V2 0 0, V2 100 0]
      spans = map (\(ps, _) -> [x | V2 x _ <- ends ps])
  assertEq failed [[0, 10], [15, 25], [30, 40], [45, 55], [60, 70], [75, 85], [90, 100]] (spans (dashed [10, 5] 0 line))
  -- An offset starts partway into the pattern; an odd-length pattern is repeated twice.
  assertEq failed [[0, 5], [10, 20], [25, 35], [40, 50], [55, 65], [70, 80], [85, 95]] (spans (dashed [10, 5] 5 line))
  assertEq failed [[0, 10], [20, 30], [40, 50], [60, 70], [80, 90]] (spans (dashed [10] 0 line))
  -- Each dash is capped.
  assert failed (all ((== P.RoundCap) . snd) (dashed [10, 5] 0 line))
  -- A dash turns a corner with its line.
  assertEq failed [[V2 0 0, V2 10 0, V2 10 5]] (map fst (dashed [15, 100] 0 (P.polyline [V2 0 0, V2 10 0, V2 10 10])))
  -- Dashes on a closed subpath run around it.
  assertEq failed [[V2 0 0, V2 5 0], [V2 10 0, V2 10 5], [V2 10 10, V2 5 10], [V2 0 10, V2 0 5]] (map fst (dashed [5, 5] 0 (square 0 0 10)))
  -- Zero-length dashes become very short dots.
  let dots = dashed [0, 10] 0 line
  assertEq failed 11 (length dots)
  assert failed (all (\(ps, _) -> case ps of [V2 x0 _, V2 x1 _] -> x1 - x0 > 0 && x1 - x0 < 0.01; _ -> False) dots)
  -- An invalid or too fine pattern draws a solid line.
  forM_ [[-1, 2], [0, 0], [0.001]] $ \pattern -> assertEq failed [[V2 0 0, V2 100 0]] (map fst (dashed pattern 0 line))
  -- A transform scales the pattern with the line.
  assertEq failed [[0, 20], [30, 50], [60, 80], [90, 100]] (map (\ps -> [x | V2 x _ <- ends ps]) [pairs pts | StrokePolyline pts _ _ _ _ _ _ <- opsAt 1 (withTransform (P.scale 2 2) (drawStrokePathWith (P.stroke 1) {P.strokeDash = [10, 5]} (P.polyline [V2 0 0, V2 50 0]) (P.Solid black)))])

-- | Vertex positions (relative to the canvas corner) and alphas from one
-- frame that draws a canvas at scale 1.
canvasVertices :: Context -> (Rect -> CanvasM ()) -> IO [(V2, Float)]
canvasVertices ctx draw = do
  setDrawSnapScale ctx 1
  (resp, _, dd, _) <- runFrame ctx (withInput 300 300) (canvas (fixedWH 200 200) draw)
  setDrawSnapScale ctx 0
  let Rect ox oy _ _ = respRect resp
  withForeignPtr (drawVertices dd) $ \vp ->
    forM [0 .. drawVertexCount dd - 1] $ \i -> do
      let at o = peekByteOff vp (i * vertexSize + o) :: IO Float
      (\x y a -> (V2 (x - ox) (y - oy), a)) <$> at 0 <*> at 4 <*> at 20

-- | How far stroke vertices reach outward past a corner (miter tip, round
-- arc or bevel edge), and past a line end for each cap.
runJoinGeometryTest :: Context -> IORef Int -> IO ()
runJoinGeometryTest ctx failed = do
  let corner = V2 100 100
      -- A right-angle corner whose outside faces the top right.
      path (Rect x y _ _) = P.polyline [V2 (x + 40) (y + 100), V2 (x + 100) (y + 100), V2 (x + 100) (y + 160)]
      outward = V2 (sqrt 0.5) (negate (sqrt 0.5))
      reach st = do
        vs <- canvasVertices ctx (\r -> drawStrokePathWith st (path r) (P.Solid black))
        pure (maximum [v2Dot (v2Sub p corner) outward | (p, _) <- vs, v2Dist p corner < 30])
      hw = 5
      edge = hw + 0.5
  miter <- reach (P.stroke 10)
  bevel <- reach (P.stroke 10) {P.strokeJoin = P.BevelJoin}
  roundJ <- reach (P.stroke 10) {P.strokeJoin = P.RoundJoin}
  -- A right angle's miter is 1.41 times the width, so a limit of 1.3 bevels it.
  limited <- reach (P.stroke 10) {P.strokeMiterLimit = 1.3}
  assertLt failed (abs (miter - edge * sqrt 2)) 0.01
  assert failed (roundJ > edge * 0.95 && roundJ < edge + 0.01)
  assertLt failed (abs (bevel - edge * sqrt 0.5)) 0.01
  assertLt failed (abs (limited - bevel)) 0.01
  -- Each cap extends past the line end by its expected amount.
  let capReach cap = do
        vs <- canvasVertices ctx (\(Rect x y _ _) -> drawStrokePathWith (P.stroke 10) {P.strokeCap = cap} (P.polyline [V2 (x + 40) (y + 100), V2 (x + 100) (y + 100)]) (P.Solid black))
        pure (maximum [px - 100 | (V2 px py, _) <- vs, abs (py - 100) < 20, px > 90, px < 130])
  butt <- capReach P.ButtCap
  sq <- capReach P.SquareCap
  roundC <- capReach P.RoundCap
  assertEq failed [True, True, True] [abs (butt - 0) < 0.01, abs (sq - edge + 0.5) < 0.01, abs (roundC - edge) < 0.01]
  where
    v2Dot (V2 a b) (V2 c d) = a * c + b * d
    v2Dist a b = let V2 dx dy = v2Sub a b in sqrt (dx * dx + dy * dy)

-- | A linear gradient colours points by position, splits geometry at its
-- stops, and follows transforms.
runGradientTest :: Context -> IORef Int -> IO ()
runGradientTest _ failed = do
  let (blue, green) = (colorRGBA 0 0 255 255, colorRGBA 0 255 0 255)
      shaded ops = [(pairs pts, primArrayToList tris, map Color (primArrayToList cs)) | FillPolygon pts _ tris (Shaded cs) <- ops]
      gradient p0 p1 stops path = shaded (opsAt 1 (drawPathWith P.NonZero path (P.Linear p0 p1 stops)))
      bar = P.rect (Rect 0 0 100 10)
      channel f c = fromIntegral (f c) :: Float
  -- Two stops spanning the shape: corner colours only, no extra vertices.
  single failed (gradient (V2 0 0) (V2 100 0) [(0, red), (1, blue)] bar) $ \(pts, _, cols) -> do
    assertEq failed 4 (length pts)
    assertEq failed [red, blue, blue, red] cols
  -- A gradient over part of the shape splits it at every stop, and no
  -- triangle crosses a stop.
  single failed (gradient (V2 25 0) (V2 75 0) [(0, red), (0.5, green), (1, blue)] bar) $ \(pts, tris, cols) -> do
    let xs = [x | V2 x _ <- pts]
        triples (a : b : c : rest) = [a, b, c] : triples rest
        triples _ = []
        straddles v t = let gs = [xs !! i | i <- t] in minimum gs < v - 1e-3 && maximum gs > v + 1e-3
    assert failed (all (\v -> any (\x -> abs (x - v) < 1e-3) xs) [25, 50, 75])
    assert failed (not (or [straddles v t | v <- [25, 50, 75], t <- triples tris]))
    assertLt failed (abs (fst (covered pts tris) - 1000)) 1e-2
    assert failed (and [c == (if x <= 25 then red else if x >= 75 then blue else c) | (V2 x _, c) <- zip pts cols])
    assert failed (and [c == green | (V2 x _, c) <- zip pts cols, abs (x - 50) < 1e-3])
  -- Rotated with its shape, each point keeps its unrotated colour.
  let turn = P.rotateAround (V2 50 5) 0.7
  single failed (shaded (opsAt 1 (withTransform turn (drawPathWith P.NonZero bar (P.Linear (V2 0 0) (V2 100 0) [(0, red), (1, blue)]))))) $ \(pts, _, cols) ->
    forM_ (zip pts cols) $ \(p, c) -> case P.invert turn of
      Just back ->
        let V2 x _ = P.transformPoint back p
         in assertLt failed (abs (channel colorB c - (channel colorB red + (255 - channel colorB red) * x / 100))) 1.5
      Nothing -> assert failed False
  -- A zero-length gradient, or one with a single stop, is a flat fill.
  assert failed (null (gradient (V2 5 5) (V2 5 5) [(0, red), (1, blue)] bar))
  assertEq failed [1] [1 :: Int | FillPolygon _ _ _ (Flat c) <- opsAt 1 (drawPathWith P.NonZero bar (P.Linear (V2 5 5) (V2 5 5) [(0, red), (1, blue)])), c == blue]
  -- A stroke takes the colours along its centre line.
  assertEq failed [[red, blue]] [map Color (primArrayToList cs) | StrokePolyline _ _ _ _ _ _ (Shaded cs) <- opsAt 1 (drawStrokePathWith (P.stroke 4) (P.polyline [V2 0 0, V2 100 0]) (P.Linear (V2 0 0) (V2 100 0) [(0, red), (1, blue)]))]
  -- A four-corner gradient at a non-quarter angle becomes a shaded polygon
  -- with its corner colours.
  single failed [(pts, cs) | FillPolygon pts _ _ (Shaded cs) <- opsAt 1 (withTransform (P.rotate 0.5) (drawQuadGradient (Rect 0 0 10 10) red green blue black))] $ \(pts, cs) -> do
    assertNear failed 1e-4 (map (P.transformPoint (P.rotate 0.5)) [V2 0 0, V2 10 0, V2 10 10, V2 0 10]) (pairs pts)
    assertEq failed [red, green, blue, black] (map Color (primArrayToList cs))

-- | A clip emits push and pop ops around its content, and the frame applies it.
runClipTest :: Context -> IORef Int -> IO ()
runClipTest ctx failed = do
  let clipOps block = [op | op <- opsAt 1 block, isClip op]
      isClip = \case PushClip _ -> True; PopClip -> True; _ -> False
  assert failed (clipOps (withClip (Rect 1 2 3 4) (drawRect (Rect 0 0 10 10) red)) == [PushClip (Rect 1 2 3 4), PopClip])
  assert failed (clipOps (withTransform (P.translate 5 5) (withClip (Rect 1 2 3 4) (pure ()))) == [PushClip (Rect 6 7 3 4), PopClip])
  -- Under rotation, the clip is the bounding box of the rotated rect.
  case clipOps (withTransform (P.rotate (pi / 4)) (withClip (Rect 0 0 10 10) (pure ()))) of
    [PushClip (Rect x y w h), PopClip] -> assertNear failed 1e-3 [V2 (-10 * sqrt 0.5) 0, V2 (20 * sqrt 0.5) (20 * sqrt 0.5)] [V2 x y, V2 w h]
    _ -> assert failed False
  -- In a frame, the content is clipped and later draws are not.
  let ui = canvas (fixedWH 100 100) $ \(Rect x y _ _) -> do
        withClip (Rect (x + 10) (y + 10) 20 20) (drawRect (Rect x y 100 100) red)
        drawRect (Rect x y 50 50) black
  (resp, _, dd, _) <- runFrame ctx (withInput 300 300) ui
  let Rect x y _ _ = respRect resp
      clips = [(cmdClipX c - x, cmdClipY c - y, cmdClipW c, cmdClipH c) | c <- drawCmdElems dd, cmdIndexCount c > 0]
  assert failed ((10, 10, 20, 20) `elem` clips)
  assert failed (any (\(_, _, w, _) -> w >= 100) clips)

-- | Per-corner rounded rects, and transforms undone.
runShapesAndInverseTest :: Context -> IORef Int -> IO ()
runShapesAndInverseTest _ failed = do
  let areaOf path = sum [abs (polyArea pts) | (pts, _) <- filled path]
      quarterCut r = (4 - pi) / 4 * r * r
      -- Area that flattening a quarter circle of radius @r@ may lose.
      flat r = pi / 2 * r * 0.25
  assertLt failed (abs (areaOf (P.roundedRectCorners (Rect 0 0 100 60) 0 20 0 10) - (6000 - quarterCut 20 - quarterCut 10))) (flat 20 + flat 10)
  -- Oversized radii scale down together until they fit, as in CSS.
  assertLt failed (abs (areaOf (P.roundedRectCorners (Rect 0 0 100 60) 60 60 0 0) - (6000 - 2 * quarterCut 50))) (2 * flat 50)
  assert failed (filled (P.roundedRectCorners (Rect 0 0 10 10) 0 0 (-3) 0) == filled (P.rect (Rect 0 0 10 10)))
  assert failed (filled (P.roundedRect (Rect 0 0 40 20) 6) == filled (P.roundedRectCorners (Rect 0 0 40 20) 6 6 6 6))
  let t = P.translate 3 (-2) <> P.rotate 0.4 <> P.scale 2 (-0.5) <> P.affine 1 0.3 0 1 0 0
      pts = [V2 1 2, V2 (-40) 7, V2 0 0]
  case P.invert t of
    Just back -> assertNear failed 1e-3 pts (map (P.transformPoint back . P.transformPoint t) pts)
    Nothing -> assert failed False
  assertEq failed [True, True] [P.invert (P.scale 0 1) == Nothing, P.invert (P.scale (0 / 0) 1) == Nothing]

-- | A configured canvas caches its drawing by key, the drawing sees hover
-- state, and the canvas sets its cursor.
runCanvasConfiguredTest :: Context -> IORef Int -> IO ()
runCanvasConfiguredTest ctx failed = do
  let cfg key = defaultCanvasConfig {canvasLayout = fixedWH 60 40 defaultLayout, canvasContent = key, canvasCursor = Just (\_ _ _ -> UiCursorPointer)}
      ui key = canvasConfigured (cfg key) $ \r -> do
        cdc <- drawContext
        drawRect r (if cdcHovered cdc then red else black)
        pure ()
      hoverAt resp = let Rect x y w h = respRect resp in (withInput 300 300) {inputMousePos = V2 (x + w / 2) (y + h / 2)}
  resp <- warmup2 ctx (withInput 300 300) (ui 1)
  _ <- run2Frames ctx (hoverAt resp) (ui 1)
  dc <- readIORef (ctxDrawingCache ctx)
  assertEq failed [red] [c | e <- IM.elems (dcsCustomDrawOpCache dc), FillRect _ c <- toList (cdeOps e)]
  kind <- uiCursorKind ctx (hoverAt resp)
  assertEq failed UiCursorPointer kind
