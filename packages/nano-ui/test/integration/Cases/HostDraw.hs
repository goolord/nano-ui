module Cases.HostDraw (tests) where

import Spec
import Data.List (nub, sort)
import Data.Primitive.PrimArray (primArrayFromList)
import Data.Primitive.SmallArray (smallArrayFromList)
import Data.Text qualified as T
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI.Internal.Context (setDrawExternalText, setDrawSnapScale, setDrawSquareGeometry)
import NanoUI.Internal.Store (ptrEq)

tests :: [Spec]
tests =
  [ spec "draw-square-geometry" runSquareGeometryTest
  , spec "draw-thick-rounded-stroke" runThickRoundedStrokeTest
  , spec "draw-geometry-guards" runGeometryGuardsTest
  , spec "draw-glyph-pages" runGlyphPagesTest
  , spec "draw-glyph-kerning" runGlyphKerningTest
  , spec "draw-text-input-font" runTextInputFontTest
  , spec "draw-field-font-style" runFieldFontStyleTest
  , spec "draw-external-text" runExternalTextTest
  , spec "draw-external-text-frames" runExternalTextFramesTest
  ]

-- | A vertex: position, colour, alpha and UV.
data Vertex = Vertex {vX, vY :: Float, vRgb :: (Float, Float, Float), vA, vU, vV :: Float}

vertexAt :: Ptr Word8 -> Int -> IO Vertex
vertexAt vp i = Vertex <$> at 0 <*> at 4 <*> ((,,) <$> at 8 <*> at 12 <*> at 16) <*> at 20 <*> at 24 <*> at 28
  where
    at o = peekByteOff vp (i * vertexSize + o) :: IO Float

-- | Every vertex, in buffer order.
vertices :: DrawData -> IO [Vertex]
vertices dd = withForeignPtr (drawVertices dd) $ \vp -> mapM (vertexAt vp) [0 .. drawVertexCount dd - 1]

-- | The vertex each of these index-buffer positions names.
indexed :: DrawData -> [Int] -> IO [Vertex]
indexed dd is =
  withForeignPtr (drawVertices dd) $ \vp -> withForeignPtr (drawIndices dd) $ \ip ->
    forM is $ \i -> (peekByteOff ip (i * indexSize) :: IO Word32) >>= vertexAt vp . fromIntegral

-- | The vertices the commands drawn from texture @tex@ index.
fromTexture :: Int -> DrawData -> IO [Vertex]
fromTexture tex dd = indexed dd (concat [range c | c <- drawCmdElems dd, cmdTextureId c == tex])
  where
    range c = [fromIntegral (cmdIndexOffset c) .. fromIntegral (cmdIndexOffset c + cmdIndexCount c) - 1]

-- | Every indexed triangle.
triangles :: DrawData -> IO [(Vertex, Vertex, Vertex)]
triangles dd = triples <$> indexed dd [0 .. drawIndexCount dd - 1]
  where
    triples (a : b : c : rest) = (a, b, c) : triples rest
    triples _ = []

-- | A font that shapes any text to these glyphs: x, y, w, h and UV corners.
shapedFont :: Float -> [[Float]] -> FontMetrics
shapedFont size glyphs = fm
  where
    fm = (monospaceMetrics size) {fmBackend = Just (FontBackend (\_ -> pure fm) (\_ -> pure (Just (ShapedGlyphs (primArrayFromList (concat glyphs))))))}

drawn :: Context -> NanoUI a -> IO DrawData
drawn ctx ui = (\(_, dd, _) -> dd) <$> runFrame ctx (withInput 300 200) ui

-- | Rounded fills and AA strokes carry transparent fringe vertices next to
-- opaque ones. Square geometry emits only flat primitives, so every triangle
-- has a uniform alpha.
runSquareGeometryTest :: Context -> IORef Int -> IO ()
runSquareGeometryTest ctx failed = do
  let uniform (a, b, c) = vA a == vA b && vA b == vA c
      controls = column $ do
        void (button "ok")
        void (withValue True (checkbox "check"))
        void (withValue 0.5 (slider 0 1))
        void (button' "menu")
  assert failed . not . all uniform =<< triangles =<< drawn ctx controls
  setDrawSquareGeometry ctx True
  squareTris <- triangles =<< drawn ctx controls
  assert failed (not (null squareTris) && all uniform squareTris)

-- | A rounded border wider than twice its radius keeps its ink inside its
-- box: the corner arcs end on the corner radius. A translucent one covers
-- each point of its band once: at the inner corners, beside the arcs and
-- along the sides.
runThickRoundedStrokeTest :: Context -> IORef Int -> IO ()
runThickRoundedStrokeTest ctx failed = do
  let border ink = drawing (fixedWH 60 60) (\(Rect x y _ _) -> smallArrayFromList [StrokeRoundedRect (Rect (x + 10) (y + 10) 40 40) 1 8 ink])
  inked <- filter (\v -> vRgb v == (0, 0, 1) && vA v > 0) <$> (vertices =<< drawn ctx (border (colorRGBA 0 0 255 255)))
  let extent f = maximum (map f inked) - minimum (map f inked)
  assert failed (not (null inked) && extent vX <= 40 && extent vY <= 40)
  (resp, dd, _) <- runFrame ctx (withInput 300 200) (border (colorRGBA 0 0 255 128))
  tris <- triangles dd
  let Rect rx ry _ _ = respRect resp
      alphaAt px py = sum [l0 * vA a + l1 * vA b + l2 * vA c | (a, b, c) <- tris, Just (l0, l1, l2) <- [barycentric a b c]]
        where
          barycentric (Vertex x0 y0 _ _ _ _) (Vertex x1 y1 _ _ _ _) (Vertex x2 y2 _ _ _ _) =
            let d = (y1 - y2) * (x0 - x2) + (x2 - x1) * (y0 - y2)
                l0 = ((y1 - y2) * (px - x2) + (x2 - x1) * (py - y2)) / d
                l1 = ((y2 - y0) * (px - x2) + (x0 - x2) * (py - y2)) / d
                l2 = 1 - l0 - l1
             in if d /= 0 && l0 >= 0 && l1 >= 0 && l2 >= 0 then Just (l0, l1, l2) else Nothing
      -- Off the triangles' shared edges, which both would count.
      points =
        [ (4.3, 4.6), (0.8, 4.3), (4.3, 0.8), (35.7, 4.6), (4.3, 35.4), (35.7, 35.4), (39.2, 35.7)
        , (4.3, 20.7), (20.7, 4.3), (35.7, 20.3), (20.3, 35.7)
        ]
  forM_ points $ \(dx, dy) -> assert failed (abs (alphaAt (rx + 10 + dx) (ry + 10 + dy) - 128 / 255) < 0.01)

-- | Geometry clipped to nothing gets no command, which a host would still
-- draw into the pixel its empty clip rounds to. A polygon whose triangles
-- index past its points draws nothing, rather than triangles into other
-- geometry or past the frame's vertices. Circles sharing a fractional
-- centre stay concentric, filled or stroked, whatever their radii, under
-- pixel snapping.
runGeometryGuardsTest :: Context -> IORef Int -> IO ()
runGeometryGuardsTest ctx failed = do
  let opsIn ops = drawn ctx (void (drawing (fixedWH 60 60) (smallArrayFromList . ops)))
      white = colorRGBA 255 255 255 255
  clipped <- opsIn $ \r -> [FillRect r white, PushClip (Rect 0 0 10 10), PushClip (Rect 20 20 10 10), FillRect r red, PopClip, PopClip]
  assert failed (not (drawCmdNull clipped))
  assertEq failed [] [c | c <- drawCmdElems clipped, cmdClipW c <= 0 || cmdClipH c <= 0]
  bad <- opsIn . const $ [FillPolygon (primArrayFromList [0, 0, 40, 0, 0, 40]) (primArrayFromList [0, 3]) (primArrayFromList [0, 1, 100]) (Flat red)]
  indices <- withForeignPtr (drawIndices bad) $ \ip -> forM [0 .. drawIndexCount bad - 1] $ \i -> peekByteOff ip (i * indexSize) :: IO Word32
  assert failed (all ((< drawVertexCount bad) . fromIntegral) indices)
  let circles = void $ customWidget defaultCustomWidgetSpec
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
  verts <- vertices =<< drawn ctx circles
  let centreOf rgb = case [(vX v, vY v) | v <- verts, vRgb v == rgb] of
        [] -> Nothing
        ps -> let (xs, ys) = unzip ps in Just ((minimum xs + maximum xs) / 2, (minimum ys + maximum ys) / 2)
      concentric a b = case (centreOf a, centreOf b) of
        (Just (ax, ay), Just (bx, by)) -> abs (ax - bx) < 1e-3 && abs (ay - by) < 1e-3
        _ -> False
  assert failed (concentric (1, 0, 0) (0, 1, 0) && concentric (0, 0, 1) (1, 1, 0))
  where
    red = colorRGBA 255 0 0 255

-- | A shaped run with glyphs on two atlas pages draws each page's glyphs
-- under that page's texture, with the page taken out of their u, and the
-- first glyph on page 0 before the others. A glyph whose u lies outside
-- every page is skipped: the run binds no texture past the pages, even for
-- a u just below 0. A shaped line far longer than its clip reserves room
-- only for the glyphs that can show, so it does not grow the draw buffers.
runGlyphPagesTest :: Context -> IORef Int -> IO ()
runGlyphPagesTest ctx failed = do
  let glyph x u = [x, 0, 6, 10, u, 0.5, u + 0.25, 0.75]
  dd <- drawn (withFontMetrics ctx (shapedFont 12 [glyph 0 0.25, glyph 8 1.25, glyph 16 1.25])) (label "abc")
  case [c | c <- drawCmdElems dd, cmdTextureId c == glyphPageTextureId 1, cmdIndexCount c > 0] of
    [cmd] -> assertEq failed 12 (cmdIndexCount cmd)
    other -> assertEq failed 1 (length other)
  assertEq failed [0.25, 0.5] . nub . sort . map vU =<< fromTexture (glyphPageTextureId 1) dd
  firstUs <- map vU <$> fromTexture (glyphPageTextureId 0) dd
  assert failed (0.25 `elem` firstUs && 0.5 `elem` firstUs)
  outside <- drawn (withFontMetrics ctx (shapedFont 12 [glyph 0 0.25, glyph 8 (-0.25), glyph 16 4.25, glyph 24 (-1)])) (label "abcd")
  assertEq failed [glyphAtlasTextureId] (nub [cmdTextureId c | c <- drawCmdElems outside, cmdIndexCount c > 0])
  let long = shapedFont 12 [glyph (fromIntegral k * 6) 0.25 | k <- [0 .. 9999 :: Int]]
      scrolled = void (scrollArea (fixedWH 120 40) (label "abc"))
  before <- drawn ctx scrolled
  after <- drawn (withFontMetrics ctx long) scrolled
  assert failed (drawVertices before == drawVertices after)

-- | Glyphs drawn by character move a kerned pair's second glyph: "AV"
-- kerned 2 closer draws the V 2 nearer the A, and the A after it where it
-- would be anyway. Aligned to the end, the last V's ink meets the edge. A
-- character with no glyph draws an advance box, but whitespace of any kind
-- draws nothing.
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
  (lab, dd) <- warmupDraw ctx (withInput 300 100) (rowWith (fixedW 100 . tight . gap 0) (labelWith' (fillW . alignEnd . tight) "AVAV"))
  xus <- map (\v -> (vX v, vU v)) <$> fromTexture glyphAtlasTextureId dd
  let aXs = [x | (x, u) <- xus, u < 0.55]
      vXs = [x | (x, u) <- xus, u > 0.55]
      Rect bx _ bw _ = respRect lab
  if null aXs || null vXs
    then assert failed False
    else do
      assertEq failed (8, 18) (minimum vXs - minimum aXs, maximum aXs - 6 - minimum aXs)
      assert failed (abs (maximum vXs - (bx + bw)) <= 0.6)
  spaced <- drawn base (label "a\x00A0\&b\x3000\&c d")
  assertEq failed (4 * 6) (sum [cmdIndexCount c | c <- drawCmdElems spaced, cmdTextureId c == glyphAtlasTextureId])

-- | A text field set in a larger font draws its value's glyphs in that font:
-- they cover what a label in the same font covers, and not what the base
-- font's glyph covers. Search and numeric fields too, which paint without a
-- caption.
runTextInputFontTest :: Context -> IORef Int -> IO ()
runTextInputFontTest base failed = do
  -- Each font shapes any text to one glyph of its own size.
  let small = shapedFont 12 [[0, 0, 6, 10, 0.25, 0.5, 0.5, 0.75]]
      large = shapedFont 32 [[0, 0, 16, 26, 0.25, 0.5, 0.5, 0.75]]
      ctx = withFontResolver (withFontMetrics base small) (\_ _ _ _ -> pure (large, False)) (\_ _ _ _ _ -> pure (32, 32))
      big = fontSize 32
      extent ui = do
        (_, dd) <- warmupDraw ctx (withInput 400 200) (column ui)
        -- The glyph's own quad: atlas vertices at its UV corners.
        ps <- filter (\v -> vU v `elem` [0.25, 0.5] && vV v `elem` [0.5, 0.75]) <$> fromTexture glyphAtlasTextureId dd
        let size f = if null ps then 0 else maximum (map f ps) - minimum (map f ps)
        pure (size vX, size vY)
  extent (labelWith big "Hg") >>= assertEq failed (16, 26)
  mapM extent
    [ void (withValue "Hg" (textInputConfigured defaultTextInputConfig {ticLayout = big (ticLayout defaultTextInputConfig)}))
    , void (withValue "Hg" (searchInputConfigured defaultSearchInputConfig {sicLayout = big (sicLayout defaultSearchInputConfig)}))
    , void (withValue 7 (numericInputConfigured defaultNumericInputConfig {nicLayout = big (nicLayout defaultNumericInputConfig)}))
    ]
    >>= assertEq failed (replicate 3 (16, 26))

-- | Text, search and numeric fields, selectable text and text areas draw the
-- layout's weight, decoration and tone as a label does, though the font
-- gives no face for them: bold draws each of the three glyphs twice (the
-- headless font's advance boxes), an underline adds a rule in the text's
-- colour, and a muted tone takes the theme's muted colour.
runFieldFontStyleTest :: Context -> IORef Int -> IO ()
runFieldFontStyleTest ctx failed = do
  mutedFg <- themeMuted <$> getTheme ctx
  let red = colorRGBA 255 0 0 255
      frameOf ui = snd <$> warmupDraw ctx (withInput 400 300) (column ui)
      quadsIn col f ui = length . filter ((== col) . snd) <$> (drawQuads =<< frameOf (ui f))
      atlasQuads f ui = (\dd -> sum [cmdIndexCount c `div` 6 | c <- drawCmdElems dd, cmdTextureId c == glyphAtlasTextureId]) <$> frameOf (ui f)
      kinds :: [(Layout -> Layout) -> NanoUI ()]
      kinds =
        [ \f -> void (withValue "abc" (textInputConfigured defaultTextInputConfig {ticLayout = f (ticLayout defaultTextInputConfig)}))
        , \f -> selectableTextWith f "abc"
        , \f -> void (withValue "abc" (textAreaWith' f))
        , \f -> void (withValue "abc" (searchInputConfigured defaultSearchInputConfig {sicLayout = f (sicLayout defaultSearchInputConfig)}))
        , \f -> void (withValue 123 (numericInputConfigured defaultNumericInputConfig {nicLayout = f (nicLayout defaultNumericInputConfig)}))
        ]
  forM_ kinds $ \ui -> do
    assertEq failed 3 =<< ((-) <$> atlasQuads fontBold ui <*> atlasQuads id ui)
    assertEq failed 1 =<< ((-) <$> quadsIn red (fontUnderline . fontColor red) ui <*> quadsIn red (fontColor red) ui)
    assertEq failed 3 =<< ((-) <$> quadsIn mutedFg fontMuted ui <*> quadsIn mutedFg id ui)

-- | A drawing with @txt@ in each kind of text op: plain from its top-left,
-- bold, at twice the size, and centred under a clip that cuts it.
drawingText :: T.Text -> NanoUI Response
drawingText txt = drawing (fixedWH 300 80) $ \(Rect x y _ _) ->
  smallArrayFromList
    [ DrawText x y 0 1 txt drawingInk
    , DrawTextStyled x (y + 20) defaultTextFont {textFontWeight = WeightBold} txt drawingInk
    , DrawTextAligned x (y + 40) 0 1 2 defaultTextFont txt drawingInk
    , PushClip (Rect (x + 140) (y + 60) 20 20)
    , DrawText (x + 150) (y + 70) 0.5 0.5 txt drawingInk
    , PopClip
    ]

drawingInk :: Color
drawingInk = colorRGBA 200 40 40 255

-- | The spans 'collectRasterSpans' adds for drawing text: those past the
-- node spans, base then overlay.
drawnSpans :: Context -> Input -> IO ([(Rect, T.Text, Color, Color, Rect)], [(Rect, T.Text, Color, Color, Rect)])
drawnSpans ctx inp = do
  (base, overlay) <- collectRasterSpans ctx inp
  nodeBase <- collectTextSpans ctx
  nodeOverlay <- collectOverlayTextSpans ctx inp
  pure (drop (length nodeBase) base, drop (length nodeOverlay) overlay)

textsOf :: [(Rect, T.Text, Color, Color, Rect)] -> [T.Text]
textsOf ss = [t | (_, t, _, _, _) <- ss]

-- | A context drawing text externally, as a host that draws spans does.
externalContext :: IO Context
externalContext = newContext >>= \c -> c <$ setDrawExternalText c True

-- | External text keeps text spans but pushes no text quads, so the buffer
-- does not grow with the text, for labels and for drawing text, plain,
-- styled or aligned. Drawing text is recorded as spans after the node spans
-- of the base list, in op order: its line box, the ink on a transparent
-- background, and the drawing's clip, cut by a 'PushClip'. In a floating
-- window it is overlay text, clipped inside the window. Without external
-- text (the SDL host) drawing text draws quads, and 'collectRasterSpans'
-- has the node spans alone.
runExternalTextTest :: Context -> IORef Int -> IO ()
runExternalTextTest internal failed = do
  let inp = withInput 400 100
      txt = "abcdefghijklmnop"
      -- The vertex counts for a short text and a long one.
      vertexCounts :: Context -> (T.Text -> NanoUI x) -> IO (Int, Int)
      vertexCounts c ui = (,) <$> count c (ui "ab") <*> count c (ui txt)
      count :: Context -> NanoUI x -> IO Int
      count c ui = drawVertexCount . snd <$> warmupDraw c inp ui
  ctx <- externalContext
  vertexCounts ctx (column . label) >>= uncurry (assertEq failed)
  assert failed . hasText txt =<< collectTextSpans ctx
  vertexCounts ctx drawingText >>= uncurry (assertEq failed)
  resp <- warmup2 ctx inp (drawingText txt)
  (base, overlay) <- collectRasterSpans ctx inp
  (spans', spansOver) <- drawnSpans ctx inp
  let r@(Rect x y _ _) = respRect resp
      Rect cx cy _ _ = drawTextBox (ctxFontMetrics ctx) (x + 150) (y + 70) 0.5 0.5 txt
  assertEq failed [txt, txt, txt, txt] (textsOf spans')
  assertEq failed [(x, y), (x, y + 20), (x, y + 40), (cx, cy)] [(bx, by) | (Rect bx by _ _, _, _, _, _) <- spans']
  assertEq failed [r, r, r, Rect (x + 140) (y + 60) 20 20] [clip | (_, _, _, _, clip) <- spans']
  assert failed (all (\(_, _, fg, bg, _) -> fg == drawingInk && bg == Color 0) spans')
  -- Twice the size: the aligned line is twice as tall as the plain one.
  assertEq failed [12, 12, 24, 12] [h | (Rect _ _ _ h, _, _, _, _) <- spans']
  assertEq failed spans' (drop (length base - 4) base)
  assert failed (null spansOver && not (hasText txt overlay))
  floating <- externalContext
  let winInp = withInput 600 300
  _ <- warmup2 floating winInp (window' (constVar True) "W" (drawingText "floating"))
  (winBase, winOverlay) <- drawnSpans floating winInp
  assertEq failed ([], replicate 4 "floating") (winBase, textsOf winOverlay)
  nodesOfType NodeWindow floating >>= \case
    (_, win) : _ -> assert failed (all (\(_, _, _, _, clip) -> covers win clip) winOverlay)
    [] -> assert failed False
  vertexCounts internal drawingText >>= \(short, long) -> assertGt failed long short
  assertEq failed ([], []) =<< drawnSpans internal inp

-- | A full frame reused from the last paint keeps that paint's drawing
-- spans, however often they are collected, and so does a frame painted with
-- reuse off. A drawing far taller than its scroller records only the lines
-- within the glyph slack of the viewport, all of those in view among them,
-- each clipped to the viewport. A clip frame records no span for a drawing
-- it does not repaint, and the spans of one it does, inside the damage.
runExternalTextFramesTest :: Context -> IORef Int -> IO ()
runExternalTextFramesTest _ failed = do
  let inp = withInputOff 400 300
  ctx <- externalContext
  let frame' = (\(_, dd, _) -> dd) <$> runFrame ctx inp (drawingText "reused")
      reused = assertEq failed (replicate 4 "reused") . textsOf . fst =<< drawnSpans ctx inp
  replicateM_ 2 frame'
  d1 <- frame'
  reused
  d2 <- frame'
  -- The case returns the field itself, as Cases.Cache does for 'ptrEq'.
  assert failed (ptrEq d1 d2)
  reused
  setDrawReuse ctx False
  _ <- frame'
  reused
  culled <- externalContext
  let line i = T.pack ("line " ++ show i)
      lines' = [0 .. 149 :: Int]
      tall = scrollArea (fixedWH 300 200) . drawing (fixedWH 280 2000) $ \(Rect x y _ _) ->
        smallArrayFromList [DrawText x (y + 13 * fromIntegral i) 0 1 (line i) drawingInk | i <- lines']
  (sid, _) <- warmup2 culled inp tall
  setScrollOffset culled sid 900
  (_, resp) <- warmup2 culled inp tall
  (spans', _) <- drawnSpans culled inp
  assertJustM failed (getScrollMetrics culled sid) $ \m -> do
    let view@(Rect _ vy _ vh) = scrollViewport m
        dr@(Rect _ dy _ _) = respRect resp
        lh = fmLineHeight (ctxFontMetrics culled)
        -- 'NanoUI.Internal.Draw.glyphSlackLines' line heights.
        slack = 4 * lh
        inView i = let top = dy + 13 * fromIntegral i in top + lh > vy && top < vy + vh
    assert failed (length spans' > 10 && length spans' < 40)
    assert failed (all (\(Rect _ top _ _, _, _, _, _) -> top + lh + slack >= vy && top - slack <= vy + vh) spans')
    assert failed (all ((`elem` textsOf spans') . line) (filter inView lines'))
    assert failed (all (\(_, _, _, _, clip) -> Just clip == rectIntersect view dr) spans')
  damaged <- externalContext
  let versioned version txt = column $ do
        _ <- drawingVersioned version (fixedWH 200 40) $ \(Rect x y _ _) -> smallArrayFromList [DrawText x y 0 1 txt drawingInk]
        box (fixedWH 10 80) (colorRGBA 0 0 0 0)
        button' "far"
  b <- warmup2 damaged inp (versioned 1 "before")
  let hover = inp {inputMousePos = centerOf b}
  writeIORef (ctxPaintFull damaged) False
  _ <- runFrame damaged hover (versioned 1 "before")
  takeDamage damaged >>= assert failed . (/= DamageFull)
  assertEq failed ([], []) =<< drawnSpans damaged hover
  _ <- runFrame damaged hover (versioned 2 "after")
  dmg <- takeDamage damaged
  (after, _) <- drawnSpans damaged hover
  assertEq failed ["after"] (textsOf after)
  case dmg of
    DamageClip d -> assert failed (all (\(_, _, _, _, clip) -> covers (rectInflate 1 d) clip) after)
    DamageFull -> assert failed False
