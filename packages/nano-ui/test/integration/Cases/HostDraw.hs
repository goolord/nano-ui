module Cases.HostDraw (tests) where

import Spec
import Data.List (nub, sort)
import Data.Primitive.PrimArray (primArrayFromList)
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
  , spec "draw-glyph-pages" runGlyphPagesTest
  , spec "draw-text-input-font" runTextInputFontTest
  ]

-- | A text field set in a larger font draws its value's glyphs in that font:
-- they cover what a label in the same font covers, and not what the base
-- font's glyph covers.
runTextInputFontTest :: Context -> IORef Int -> IO ()
runTextInputFontTest base failed = do
  let inp = withInput 400 200
      -- Each font shapes any text to one glyph of its own size.
      shapedFont size gw gh =
        let fm = (monospaceMetrics size) {fmBackend = Just (FontBackend (\_ -> pure fm) (\_ -> pure (Just glyph)))}
            glyph = ShapedGlyphs (primArrayFromList [0, 0, gw, gh, 0.25, 0.5, 0.5, 0.75])
         in fm
      small = shapedFont 12 6 10
      large = shapedFont 32 16 26
      ctx = withFontResolver (withFontMetrics base small) (\_ _ _ _ -> pure (large, False)) (\_ _ _ _ _ -> pure (32, 32))
      big = fontSize 32
      field = void (textInputConfigured defaultTextInputConfig {ticLayout = big (ticLayout defaultTextInputConfig)} "Hg")
      extent ui = do
        _ <- runFrame ctx inp (column ui)
        (_, _, dd, _) <- runFrame ctx inp (column ui)
        ps <- glyphVertices dd
        let xs = map fst ps
            ys = map snd ps
        pure (if null ps then (0, 0) else (maximum xs - minimum xs, maximum ys - minimum ys))
  (fieldW, fieldH) <- extent field
  (labelW, labelH) <- extent (labelWith big "Hg")
  assertEq failed (16, 26) (labelW, labelH)
  assertEq failed (labelW, labelH) (fieldW, fieldH)

-- | Position of every vertex of the glyph quads the fonts of
-- 'runTextInputFontTest' draw: atlas vertices at that glyph's UV corners.
glyphVertices :: DrawData -> IO [(Float, Float)]
glyphVertices dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip ->
      fmap concat . forM glyphCmds $ \cmd ->
        fmap concat . forM [cmdIndexOffset cmd .. cmdIndexOffset cmd + cmdIndexCount cmd - 1] $ \i -> do
          vi <- peekByteOff ip (fromIntegral i * indexSize) :: IO Word32
          let at o = peekByteOff vp (fromIntegral vi * vertexSize + o) :: IO Float
          (x, y, u, v) <- (,,,) <$> at 0 <*> at 4 <*> at 24 <*> at 28
          pure [(x, y) | u `elem` [0.25, 0.5], v `elem` [0.5, 0.75]]
  where
    glyphCmds = [c | c <- drawCmdElems dd, cmdTextureId c == glyphAtlasTextureId, cmdIndexCount c > 0]

-- | A shaped run with glyphs on two atlas pages draws each page's glyphs
-- under that page's texture, with the page taken out of their u.
runGlyphPagesTest :: Context -> IORef Int -> IO ()
runGlyphPagesTest ctx failed = do
  let glyph x page = [x, 0, 6, 10, page + 0.25, 0.5, page + 0.5, 0.75]
      glyphs = ShapedGlyphs (primArrayFromList (concat [glyph 0 0, glyph 8 1, glyph 16 1]))
      fm = (monospaceMetrics 12) {fmBackend = Just (FontBackend (\_ -> pure fm) (\_ -> pure (Just glyphs)))}
  (_, _, dd, _) <- runFrame (withFontMetrics ctx fm) (withInput 300 200) (label "abc")
  let onPage page = [c | c <- drawCmdElems dd, cmdTextureId c == glyphPageTextureId page, cmdIndexCount c > 0]
      vertexUs cmd =
        withForeignPtr (drawVertices dd) $ \vp ->
          withForeignPtr (drawIndices dd) $ \ip ->
            forM [cmdIndexOffset cmd .. cmdIndexOffset cmd + cmdIndexCount cmd - 1] $ \i -> do
              vi <- peekByteOff ip (fromIntegral i * indexSize) :: IO Word32
              peekByteOff vp (fromIntegral vi * vertexSize + 24) :: IO Float
  case onPage 1 of
    [cmd] -> do
      assertEq failed 12 (cmdIndexCount cmd)
      assertEq failed [0.25, 0.5] . nub . sort =<< vertexUs cmd
    other -> assertEq failed 1 (length other)
  -- The first glyph stays on page 0, drawn before the others.
  firstUs <- concat <$> mapM vertexUs (onPage 0)
  assert failed (0.25 `elem` firstUs && 0.5 `elem` firstUs)

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
        , widgetDraw = \cdc r -> runCanvasFor cdc $ do
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
