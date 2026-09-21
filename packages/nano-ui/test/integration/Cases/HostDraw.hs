module Cases.HostDraw (tests) where

import Spec
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI.Internal.Context (setDrawExternalText, setDrawSnapScale, setDrawSquareGeometry)

tests :: [Spec]
tests =
  [ spec "draw-square-geometry" runSquareGeometryTest
  , spec "draw-external-text" runExternalTextTest
  , spec "draw-concentric-circles" runConcentricCirclesTest
  ]

-- | Alpha of every vertex of every indexed triangle.
triangleAlphas :: DrawData -> IO [(Float, Float, Float)]
triangleAlphas dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip ->
      forM [0, 3 .. drawIndexCount dd - 3] $ \i -> do
        a <- alphaAt vp ip i
        b <- alphaAt vp ip (i + 1)
        c <- alphaAt vp ip (i + 2)
        pure (a, b, c)
  where
    alphaAt :: Ptr Word8 -> Ptr Word8 -> Int -> IO Float
    alphaAt vp ip i = do
      vi <- peekByteOff ip (i * indexSize) :: IO Word32
      peekByteOff vp (fromIntegral vi * vertexSize + 20)

controls :: NanoUI ()
controls = column $ do
  void (button "ok")
  void (checkbox "check" True)
  void (slider 0 1 0.5)
  void (button' "menu")

-- | Rounded fills and AA strokes carry transparent fringe vertices next to
-- opaque ones. Square geometry emits only flat primitives, so every triangle
-- has a uniform alpha.
runSquareGeometryTest :: Context -> IORef Int -> IO ()
runSquareGeometryTest ctx failed = do
  let inp = withInput 300 200
      uniform (a, b, c) = a == b && b == c
  (_, _, dRound, _) <- runFrame ctx inp controls
  roundTris <- triangleAlphas dRound
  assert failed (not (all uniform roundTris))
  setDrawSquareGeometry ctx True
  (_, _, dSquare, _) <- runFrame ctx inp controls
  squareTris <- triangleAlphas dSquare
  assert failed (not (null squareTris))
  assert failed (all uniform squareTris)
  setDrawSquareGeometry ctx False

-- | External text keeps text spans but pushes no text quads, so the buffer
-- does not grow with the label length.
runExternalTextTest :: Context -> IORef Int -> IO ()
runExternalTextTest ctx failed = do
  let inp = withInput 400 100
      ui txt = column (void (label txt))
  setDrawExternalText ctx True
  (_, _, dShort, _) <- runFrame ctx inp (ui "ab")
  (_, _, dLong, _) <- runFrame ctx inp (ui "abcdefghijklmnop")
  spans <- collectTextSpans ctx
  assertEq failed (drawVertexCount dLong) (drawVertexCount dShort)
  assert failed (any (\(_, t, _, _, _) -> t == "abcdefghijklmnop") spans)
  setDrawExternalText ctx False

-- | Circles sharing a centre stay concentric at a fractional centre, filled
-- or stroked, whatever their radii. Pixel snapping must preserve their shared centre.
runConcentricCirclesTest :: Context -> IORef Int -> IO ()
runConcentricCirclesTest ctx failed = do
  let inp = withInput 200 100
      ui = void $ customWidget defaultCustomWidgetSpec
        { widgetLayout = fixedWH 120 60 defaultLayout
        , widgetDraw = \_ r -> runCanvas $ do
            let fill = V2 (rectX r + 20.3) (rectY r + 20.3)
                ring = V2 (rectX r + 60.7) (rectY r + 20.2)
            drawCircle fill 6 (colorRGBA 255 0 0 255)
            drawCircle fill 4.5 (colorRGBA 0 255 0 255)
            drawStrokeCircle ring 6 1.5 (colorRGBA 0 0 255 255)
            drawCircle ring 2.5 (colorRGBA 255 255 0 255)
        }
  setDrawSnapScale ctx 1
  (_, _, dd, _) <- runFrame ctx inp ui
  setDrawSnapScale ctx 0
  verts <- vertexColours dd
  let centreOf rgb = case [(x, y) | (x, y, c) <- verts, c == rgb] of
        [] -> Nothing
        ps ->
          let xs = map fst ps
              ys = map snd ps
           in Just ((minimum xs + maximum xs) / 2, (minimum ys + maximum ys) / 2)
      concentric a b = case (centreOf a, centreOf b) of
        (Just (ax, ay), Just (bx, by)) -> abs (ax - bx) < 1e-3 && abs (ay - by) < 1e-3
        _ -> False
  assert failed (concentric (1, 0, 0) (0, 1, 0))
  assert failed (concentric (0, 0, 1) (1, 1, 0))

-- | Position and colour, without alpha, of every vertex.
vertexColours :: DrawData -> IO [(Float, Float, (Float, Float, Float))]
vertexColours dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    forM [0 .. drawVertexCount dd - 1] $ \i -> do
      let at o = peekByteOff vp (i * vertexSize + o) :: IO Float
      (,,) <$> at 0 <*> at 4 <*> ((,,) <$> at 8 <*> at 12 <*> at 16)
