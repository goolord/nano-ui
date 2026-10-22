module Cases.HostDraw (tests) where

import Spec
import Data.List (nub, sort)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI.Internal.Context (setDrawExternalText, setDrawSnapScale, setDrawSquareGeometry)

tests :: [Spec]
tests =
  [ spec "draw-square-geometry" runSquareGeometryTest
  , spec "draw-thick-rounded-stroke" runThickRoundedStrokeTest
  , spec "draw-thick-rounded-stroke-alpha" runThickRoundedStrokeAlphaTest
  , spec "draw-external-text" runExternalTextTest
  , spec "draw-external-drawing-text" runExternalDrawingTextTest
  , spec "draw-empty-clip" runEmptyClipTest
  , spec "draw-concentric-circles" runConcentricCirclesTest
  , spec "draw-polygon-bad-index" runPolygonBadIndexTest
  , spec "draw-glyph-pages" runGlyphPagesTest
  , spec "draw-glyph-page-range" runGlyphPageRangeTest
  , spec "draw-long-shaped-line" runLongShapedLineTest
  , spec "draw-text-input-font" runTextInputFontTest
  , spec "draw-glyph-kerning" runGlyphKerningTest
  , spec "draw-missing-glyph-space" runMissingGlyphSpaceTest
  ]

-- | Glyphs drawn by character move a kerned pair's second glyph: "AV"
-- kerned 2 closer draws the V 2 nearer the A, and the A after it where it
-- would be anyway. Aligned to the end, the last V's ink meets the edge.
runGlyphKerningTest :: Context -> IORef Int -> IO ()
runGlyphKerningTest base failed = do
  let uOf c = if c == 'V' then 0.6 else 0.25
      fm =
        (monospaceMetrics 10)
          { fmAdvance = \c -> if c == 'M' then 12 else 10
          , fmKerning = \a b -> if a == 'A' && b == 'V' then -2 else 0
          , fmGlyph = \c -> Just (GlyphQuad 0 0 6 10 (uOf c) 0 (uOf c + 0.25) 1)
          }
      ctx = withFontMetrics base fm
      ui = rowWith (fixedW 100 . tight . gap 0) (labelWith' (fillW . alignEnd . tight) "AVAV")
  (lab, dd) <- warmupDraw ctx (withInput 300 100) ui
  xus <- withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip ->
      fmap concat . forM [c | c <- drawCmdElems dd, cmdTextureId c == glyphAtlasTextureId] $ \cmd ->
        forM [cmdIndexOffset cmd .. cmdIndexOffset cmd + cmdIndexCount cmd - 1] $ \i -> do
          vi <- peekByteOff ip (fromIntegral i * indexSize) :: IO Word32
          let at o = peekByteOff vp (fromIntegral vi * vertexSize + o) :: IO Float
          (,) <$> at 0 <*> at 24
  let aXs = [x | (x, u) <- xus, u < 0.55]
      vXs = [x | (x, u) <- xus, u > 0.55]
      Rect bx _ bw _ = respRect lab
  if null aXs || null vXs
    then assert failed False
    else do
      assertEq failed (8, 18) (minimum vXs - minimum aXs, maximum aXs - 6 - minimum aXs)
      assert failed (abs (maximum vXs - (bx + bw)) <= 0.6)

-- | A character with no glyph draws an advance box, but whitespace of any
-- kind draws nothing.
runMissingGlyphSpaceTest :: Context -> IORef Int -> IO ()
runMissingGlyphSpaceTest ctx failed = do
  (_, dd, _) <- runFrame ctx (withInput 300 100) (label "a\x00A0\&b\x3000\&c d")
  assertEq failed (4 * 6) (sum [cmdIndexCount c | c <- drawCmdElems dd, cmdTextureId c == glyphAtlasTextureId])

-- | A text field set in a larger font draws its value's glyphs in that font:
-- they cover what a label in the same font covers, and not what the base
-- font's glyph covers. The field's caret, selection and clicks go by that
-- font too.
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
      field = void ((snd <$> withValue "Hg" (textInputConfigured defaultTextInputConfig {ticLayout = big (ticLayout defaultTextInputConfig)})))
      extent ui = do
        (_, dd) <- warmupDraw ctx inp (column ui)
        ps <- glyphVertices dd
        let xs = map fst ps
            ys = map snd ps
        pure (if null ps then (0, 0) else (maximum xs - minimum xs, maximum ys - minimum ys))
  (fieldW, fieldH) <- extent field
  (labelW, labelH) <- extent (labelWith big "Hg")
  assertEq failed (16, 26) (labelW, labelH)
  assertEq failed (labelW, labelH) (fieldW, fieldH)
  -- Search and numeric fields too, which paint without a caption.
  assertEq failed (labelW, labelH) =<< extent (void (withValue "Hg" (searchInputConfigured defaultSearchInputConfig {sicLayout = big (sicLayout defaultSearchInputConfig)})))
  assertEq failed (labelW, labelH) =<< extent (void (withValue 7 (numericInputConfigured defaultNumericInputConfig {nicLayout = big (nicLayout defaultNumericInputConfig)})))

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
  (_, dd, _) <- runFrame (withFontMetrics ctx fm) (withInput 300 200) (label "abc")
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

-- | A shaped glyph whose u lies outside every atlas page is skipped: the
-- run binds no texture past the pages and ends, even for a u just below 0.
runGlyphPageRangeTest :: Context -> IORef Int -> IO ()
runGlyphPageRangeTest ctx failed = do
  let glyph x u = [x, 0, 6, 10, u, 0.5, u + 0.25, 0.75]
      glyphs = ShapedGlyphs (primArrayFromList (concat [glyph 0 0.25, glyph 8 (-0.25), glyph 16 4.25, glyph 24 (-1)]))
      fm = (monospaceMetrics 12) {fmBackend = Just (FontBackend (\_ -> pure fm) (\_ -> pure (Just glyphs)))}
  (_, dd, _) <- runFrame (withFontMetrics ctx fm) (withInput 300 200) (label "abcd")
  assertEq failed [glyphAtlasTextureId] (nub [cmdTextureId c | c <- drawCmdElems dd, cmdIndexCount c > 0])

-- | A shaped line far longer than its clip is wide reserves room only for
-- the glyphs that can show, so it does not grow the draw buffers.
runLongShapedLineTest :: Context -> IORef Int -> IO ()
runLongShapedLineTest ctx failed = do
  let glyphs = ShapedGlyphs (primArrayFromList (concat [[fromIntegral k * 6, 0, 6, 10, 0.25, 0.5, 0.5, 0.75] | k <- [0 .. 9999 :: Int]]))
      fm = (monospaceMetrics 12) {fmBackend = Just (FontBackend (\_ -> pure fm) (\_ -> pure (Just glyphs)))}
      inp = withInput 300 200
      ui = void (scrollArea (fixedWH 120 40) (label "abc"))
  (_, before, _) <- runFrame ctx inp ui
  (_, after, _) <- runFrame (withFontMetrics ctx fm) inp ui
  assert failed (drawVertices before == drawVertices after)

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
  void ((snd <$> withValue True (checkbox "check")))
  void ((snd <$> withValue 0.5 (slider 0 1)))
  void (button' "menu")

-- | Rounded fills and AA strokes carry transparent fringe vertices next to
-- opaque ones. Square geometry emits only flat primitives, so every triangle
-- has a uniform alpha.
runSquareGeometryTest :: Context -> IORef Int -> IO ()
runSquareGeometryTest ctx failed = do
  let inp = withInput 300 200
      uniform (a, b, c) = a == b && b == c
  (_, dRound, _) <- runFrame ctx inp controls
  roundTris <- triangleAlphas dRound
  assert failed (not (all uniform roundTris))
  setDrawSquareGeometry ctx True
  (_, dSquare, _) <- runFrame ctx inp controls
  squareTris <- triangleAlphas dSquare
  assert failed (not (null squareTris))
  assert failed (all uniform squareTris)

-- | A rounded border wider than twice its radius keeps its ink inside its
-- box: the corner arcs end on the corner radius.
runThickRoundedStrokeTest :: Context -> IORef Int -> IO ()
runThickRoundedStrokeTest ctx failed = do
  let blue = colorRGBA 0 0 255 255
      ops (Rect x y _ _) = [StrokeRoundedRect (Rect (x + 10) (y + 10) 40 40) 1 8 blue]
  (_, dd, _) <- runFrame ctx (withInput 300 200) (void (drawing (fixedWH 60 60) (smallArrayFromList . ops)))
  inked <- withForeignPtr (drawVertices dd) $ \vp ->
    fmap concat . forM [0 .. drawVertexCount dd - 1] $ \i -> do
      let at o = peekByteOff vp (i * vertexSize + o) :: IO Float
      (x, y, rgb, a) <- (,,,) <$> at 0 <*> at 4 <*> ((,,) <$> at 8 <*> at 12 <*> at 16) <*> at 20
      pure [(x, y) | rgb == (0, 0, 1), a > 0]
  let extent f = maximum (map f inked) - minimum (map f inked)
  assert failed (not (null inked))
  assert failed (extent fst <= 40 && extent snd <= 40)

-- | A translucent rounded border wider than its radius covers each point of
-- its band once: at the inner corners, beside the arcs and along the sides.
runThickRoundedStrokeAlphaTest :: Context -> IORef Int -> IO ()
runThickRoundedStrokeAlphaTest ctx failed = do
  let ink = colorRGBA 0 0 255 128
      ops (Rect x y _ _) = [StrokeRoundedRect (Rect (x + 10) (y + 10) 40 40) 1 8 ink]
  (resp, dd, _) <- runFrame ctx (withInput 300 200) (drawing (fixedWH 60 60) (smallArrayFromList . ops))
  tris <- triangles dd
  let Rect rx ry _ _ = respRect resp
      alphaAt (px, py) = sum [a | ((x0, y0, a0), (x1, y1, a1), (x2, y2, a2)) <- tris, Just (l0, l1, l2) <- [barycentric (x0, y0) (x1, y1) (x2, y2)], let a = l0 * a0 + l1 * a1 + l2 * a2]
        where
          barycentric (x0, y0) (x1, y1) (x2, y2) =
            let d = (y1 - y2) * (x0 - x2) + (x2 - x1) * (y0 - y2)
                l0 = ((y1 - y2) * (px - x2) + (x2 - x1) * (py - y2)) / d
                l1 = ((y2 - y0) * (px - x2) + (x0 - x2) * (py - y2)) / d
                l2 = 1 - l0 - l1
             in if d /= 0 && l0 >= 0 && l1 >= 0 && l2 >= 0 then Just (l0, l1, l2) else Nothing
      -- Off the triangles' shared edges, which both would count.
      points = [(4.3, 4.6), (0.8, 4.3), (4.3, 0.8), (35.7, 4.6), (4.3, 35.4), (35.7, 35.4), (39.2, 35.7), (4.3, 20.7), (20.7, 4.3), (35.7, 20.3), (20.3, 35.7)]
  forM_ points $ \(dx, dy) ->
    assert failed (abs (alphaAt (rx + 10 + dx, ry + 10 + dy) - 128 / 255) < 0.01)

-- | Every indexed triangle's vertices: position and alpha.
triangles :: DrawData -> IO [((Float, Float, Float), (Float, Float, Float), (Float, Float, Float))]
triangles dd =
  withForeignPtr (drawVertices dd) $ \vp ->
    withForeignPtr (drawIndices dd) $ \ip ->
      forM [0, 3 .. drawIndexCount dd - 3] $ \i ->
        (,,) <$> vertexAt vp ip i <*> vertexAt vp ip (i + 1) <*> vertexAt vp ip (i + 2)
  where
    vertexAt :: Ptr Word8 -> Ptr Word8 -> Int -> IO (Float, Float, Float)
    vertexAt vp ip i = do
      vi <- peekByteOff ip (i * indexSize) :: IO Word32
      let at o = peekByteOff vp (fromIntegral vi * vertexSize + o) :: IO Float
      (,,) <$> at 0 <*> at 4 <*> at 20

-- | External text keeps text spans but pushes no text quads, so the buffer
-- does not grow with the label length.
runExternalTextTest :: Context -> IORef Int -> IO ()
runExternalTextTest ctx failed = do
  let inp = withInput 400 100
      ui txt = column (void (label txt))
  setDrawExternalText ctx True
  (_, dShort, _) <- runFrame ctx inp (ui "ab")
  (_, dLong, _) <- runFrame ctx inp (ui "abcdefghijklmnop")
  spans <- collectTextSpans ctx
  assertEq failed (drawVertexCount dLong) (drawVertexCount dShort)
  assert failed (any (\(_, t, _, _, _) -> t == "abcdefghijklmnop") spans)

-- | External text leaves out drawing text too, plain, styled or aligned, so
-- a host that draws only spans shows no box for each character.
runExternalDrawingTextTest :: Context -> IORef Int -> IO ()
runExternalDrawingTextTest ctx failed = do
  let inp = withInput 400 100
      ink = colorRGBA 255 255 255 255
      heavy = defaultTextFont {textFontWeight = WeightBold}
      ops txt (Rect x y _ _) =
        [ DrawText x y 0 1 txt ink
        , DrawTextStyled x (y + 20) heavy txt ink
        , DrawTextAligned x (y + 40) 0 1 2 defaultTextFont txt ink
        ]
      ui txt = void (drawing (fixedWH 300 80) (smallArrayFromList . ops txt))
  setDrawExternalText ctx True
  (_, dShort, _) <- runFrame ctx inp (ui "ab")
  (_, dLong, _) <- runFrame ctx inp (ui "abcdefghijklmnop")
  assertEq failed (drawVertexCount dShort) (drawVertexCount dLong)

-- | Geometry clipped to nothing gets no command, which a host would still
-- draw into the pixel its empty clip rounds to.
runEmptyClipTest :: Context -> IORef Int -> IO ()
runEmptyClipTest ctx failed = do
  let ops r = [FillRect r (colorRGBA 255 255 255 255), PushClip (Rect 0 0 10 10), PushClip (Rect 20 20 10 10), FillRect r (colorRGBA 255 0 0 255), PopClip, PopClip]
  (_, dd, _) <- runFrame ctx (withInput 300 200) (void (drawing (fixedWH 60 60) (smallArrayFromList . ops)))
  assert failed (not (drawCmdNull dd))
  assertEq failed [] [c | c <- drawCmdElems dd, cmdClipW c <= 0 || cmdClipH c <= 0]

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
  (_, dd, _) <- runFrame ctx inp ui
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

-- | A polygon whose triangles index past its points draws nothing, rather
-- than triangles into other geometry or past the frame's vertices.
runPolygonBadIndexTest :: Context -> IORef Int -> IO ()
runPolygonBadIndexTest ctx failed = do
  let tri = primArrayFromList [0, 0, 40, 0, 0, 40]
      ops = [FillPolygon tri (primArrayFromList [0, 3]) (primArrayFromList [0, 1, 100]) (Flat (colorRGBA 255 0 0 255))]
  (_, dd, _) <- runFrame ctx (withInput 300 200) (void (drawing (fixedWH 60 60) (const (smallArrayFromList ops))))
  indices <- withForeignPtr (drawIndices dd) $ \ip ->
    forM [0 .. drawIndexCount dd - 1] $ \i -> peekByteOff ip (i * indexSize) :: IO Word32
  assert failed (all ((< drawVertexCount dd) . fromIntegral) indices)
