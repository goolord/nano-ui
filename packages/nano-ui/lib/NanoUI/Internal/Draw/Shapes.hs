{-# LANGUAGE StrictData #-}

-- | Solid geometry emitters: rects, gradients, images, rounded fills and
-- borders, coverage-AA strokes, lines and triangles.
module NanoUI.Internal.Draw.Shapes
  ( pushRect
  , pushQuadGradient
  , pushImage
  , pushImageRotated
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushCircle
  , pushCircleStroke
  , pushLine
  , pushStrokeAA
  , pushStroke
  , pushFilledTriangle
  , pushPolygonAA
  , pushPolylineAA
  , points3
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, newPrimArray, primArrayFromListN, runPrimArray, sizeofPrimArray, writePrimArray)
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import NanoUI.Internal.Draw.Arena
import NanoUI.Internal.Draw.Types (DrawArena (..), LineCap (..), LineJoin (..), Shade (..), glyphAtlasTextureId, indexSize, vertexSize)
import NanoUI.Internal.Path (miterOffset, shoelace)
import NanoUI.Internal.SIMD
import NanoUI.Internal.Types (Color (..), Rect (..), clamp, forUpTo_, onGrid)

{-# INLINE pushRect #-}
pushRect :: DrawArena -> Rect -> Color -> IO ()
pushRect da rect col = do
  r <- snapRectOrigin da rect
  setTexture da glyphAtlasTextureId
  pushQuad da r whitePixel whitePixel whitePixel whitePixel col

-- Quad with a color per corner. GPU interpolates across the two triangles.
-- Corners: top-left, top-right, bottom-right, bottom-left.
pushQuadGradient :: DrawArena -> Rect -> Color -> Color -> Color -> Color -> IO ()
pushQuadGradient da rect@(Rect _ _ w h) tl tr br bl
  | w <= 0 || h <= 0 = pure ()
  | otherwise = do
      Rect px py _ _ <- snapRectOrigin da rect
      setTexture da glyphAtlasTextureId
      let !c0 = unpackColorF tl
          !c1 = unpackColorF tr
          !c2 = unpackColorF br
          !c3 = unpackColorF bl
      withVerts da 4 6 $ \vp ip vOff iOff baseIdxWord ->
        pokeQuadGradientSIMD vp vOff ip iOff px py w h whitePixel whitePixel c0 c1 c2 c3 baseIdxWord

{-# INLINE pushImage #-}
pushImage :: DrawArena -> Rect -> Int -> Float -> Float -> Float -> Float -> Color -> IO ()
pushImage da rect tex u0 v0 u1 v1 col
  | tex <= 0 = pushRect da rect col
  | otherwise = do
      r <- snapRectOrigin da rect
      setTexture da tex
      pushQuad da r u0 v0 u1 v1 col

-- | 'pushImage' turned by @angle@ radians, clockwise on screen, about the
-- rect's centre. Its corners fall between device pixels, so nothing snaps.
pushImageRotated :: DrawArena -> Rect -> Float -> Int -> Float -> Float -> Float -> Float -> Color -> IO ()
pushImageRotated da (Rect x y w h) angle tex0 u0 v0 u1 v1 col = do
  -- A texture-less quad takes the white pixel, as 'pushRect' does.
  let (!tex, !tu0, !tv0, !tu1, !tv1)
        | tex0 <= 0 = (glyphAtlasTextureId, whitePixel, whitePixel, whitePixel, whitePixel)
        | otherwise = (tex0, u0, v0, u1, v1)
      !(r, g, b, a) = unpackColorF col
      !c = cos angle
      !s = sin angle
      !cx = x + w / 2
      !cy = y + h / 2
      !hw = w / 2
      !hh = h / 2
      -- The corner at (dx, dy) from the centre, turned.
      cornerX dx dy = cx + dx * c - dy * s
      cornerY dx dy = cy + dx * s + dy * c
  setTexture da tex
  withVerts da 4 6 $ \vp ip vOff iOff baseIdxWord ->
    pokeQuadCornersSIMD
      vp
      vOff
      ip
      iOff
      (cornerX (-hw) (-hh))
      (cornerY (-hw) (-hh))
      (cornerX hw (-hh))
      (cornerY hw (-hh))
      (cornerX hw hh)
      (cornerY hw hh)
      (cornerX (-hw) hh)
      (cornerY (-hw) hh)
      tu0
      tv0
      tu1
      tv1
      r
      g
      b
      a
      baseIdxWord

-- 4 segments per 90° arc, so 'cornerCosSin' has 5 points per quadrant.
cornerSegments :: Int
cornerSegments = 4

-- | How far a stroked arc fades out either side of its solid part, in
-- pixels. A quad's alpha is interpolated across it and read at the middle of
-- each pixel, so a straight side, snapped to the grid, is read at the line's
-- own alpha and comes out at full strength. An arc is not on the grid and
-- never can be: its curve passes between the pixels, and each one is read
-- some way down the fade. A whole pixel of fade on each side, with no solid
-- part between them, leaves the brightest pixel of a hairline arc at about
-- three quarters of the colour the straight sides it joins are drawn in, so
-- a rounded corner reads lighter than its own edges. Half a pixel, with the
-- rest of the width left solid, spends the same ink over a narrower band and
-- gives the pixels nearest the curve the whole of it.
arcFeather :: Float
arcFeather = 0.5

-- | Precomputed unit-circle cos/sin of point @seg@ of rounded-rect corner @q@
-- (top left, then clockwise): each corner's arc is the one before it turned a
-- quarter.
{-# INLINE cornerCosSin #-}
cornerCosSin :: Int -> Int -> (Float, Float)
cornerCosSin q seg =
  case q of
    0 -> (-c, -s)
    1 -> (s, -c)
    2 -> (c, s)
    _ -> (-s, c)
  where
    (c, s) = case seg of
      0 -> (1.0, 0.0)
      1 -> (0.9238795325, 0.3826834324)
      2 -> (0.7071067812, 0.7071067812)
      3 -> (0.3826834324, 0.9238795325)
      _ -> (0.0, 1.0)

-- | Poke one coverage-AA strip into a reservation at vertex offset @vi@ and
-- index offset @ii@ (both relative to @base@/@baseIdx@). Callers guarantee
-- @(x0,y0) /= (x1,y1)@. Shared by straight strokes and the fused
-- rounded-stroke paths, so a whole border shares one arena reservation.
{-# INLINE pokeStripAt #-}
pokeStripAt ::
  Ptr Word8 ->
  Ptr Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  IO ()
pokeStripAt vp ip base baseIdx vi ii x0 y0 x1 y1 bw r g b a = do
  let !dx = x1 - x0
      !dy = y1 - y0
      !len = sqrt (dx * dx + dy * dy)
      !nx = (-dy) / len
      !ny = dx / len
      !half = bw * 0.5
      !core = max 0 (half - 0.5)
      !outer = half + 0.5
      pokeEnd !ev !ex !ey =
        pokeBandVerts vp ((base + vi + ev) * vertexSize) True r g b a $
          concentricOffsetsSIMD ex ey nx ny (-outer) (-core) core outer
  pokeEnd 0 x0 y0
  pokeEnd 4 x1 y1
  let !va = fromIntegral (base + vi) :: Word32
  pokeBandIndices ip ((baseIdx + ii) * indexSize) True va (va + 4)

-- | Poke one cross-section of a coverage-AA band at byte offset @vBase@: four
-- points clear, solid, solid, clear, or with @hasCore@ off three, the two
-- solid ones sharing the second point.
{-# INLINE pokeBandVerts #-}
pokeBandVerts ::
  Ptr Word8 ->
  Int ->
  Bool ->
  Float ->
  Float ->
  Float ->
  Float ->
  ((Float, Float), (Float, Float), (Float, Float), (Float, Float)) ->
  IO ()
pokeBandVerts vp vBase hasCore r g b a ((p0x, p0y), (p1x, p1y), (p2x, p2y), (p3x, p3y)) = do
  pokeVertexSIMD vp vBase p0x p0y r g b 0 whitePixel whitePixel
  pokeVertexSIMD vp (vBase + 32) p1x p1y r g b a whitePixel whitePixel
  when hasCore $
    pokeVertexSIMD vp (vBase + 64) p2x p2y r g b a whitePixel whitePixel
  pokeVertexSIMD vp (vBase + if hasCore then 96 else 64) p3x p3y r g b 0 whitePixel whitePixel

-- | Index the quads between two 'pokeBandVerts' cross-sections starting at
-- vertices @va@ and @vb@, at byte offset @iOff@: three quads, or two without
-- @hasCore@.
{-# INLINE pokeBandIndices #-}
pokeBandIndices :: Ptr Word8 -> Int -> Bool -> Word32 -> Word32 -> IO ()
pokeBandIndices ip iOff hasCore va vb = do
  pokeQuadIndices ip iOff va (va + 1) (vb + 1) vb
  pokeQuadIndices ip (iOff + 24) (va + 1) (va + 2) (vb + 2) (vb + 1)
  when hasCore $
    pokeQuadIndices ip (iOff + 48) (va + 2) (va + 3) (vb + 3) (vb + 2)

{-# INLINE pushRoundedRect #-}
pushRoundedRect :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRect da rect radius col =
  snapRectOrigin da rect >>= \r -> pushRoundedRectRaw da r radius col

-- | Unsnapped variant used when the rect is already anchored to the snapped
-- device pixel grid, e.g. a mark that must stay concentric with a border that
-- has already snapped its own origin. Re-snapping here would round the
-- off-origin inset (delta = (box - mark)/2) away, and since absolute snapping
-- rides on the fractional part of the widget position the mark would drift
-- off-center by up to a pixel as the widget scrolls.
-- Keep the fused emitter out of its many paint callers: inlining it duplicates
-- the corner loops and increases instruction-cache pressure substantially.
{-# NOINLINE pushRoundedRectRaw #-}
pushRoundedRectRaw :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRectRaw da (Rect x y w h) radius col
  | w <= 0 || h <= 0 = pure ()
  | radius <= 0.5 = pushRect da (Rect x y w h) col
  | otherwise = do
      square <- readIORef (daSquareGeometry da)
      let !rad = min radius (min (w * 0.5) (h * 0.5))
      if square || rad <= 0.5
        then pushRect da (Rect x y w h) col
        else do
          setTexture da glyphAtlasTextureId
          let !segs = cornerSegments
              !ring = segs + 1
              !midW = max 0 (w - 2 * rad)
              !midH = max 0 (h - 2 * rad)
              !hasCenter = midW > 0 && midH > 0
              !hasTB = midW > 0
              !hasLR = midH > 0
              !quadCount =
                (if hasCenter then 1 else 0)
                  + (if hasTB then 2 else 0)
                  + (if hasLR then 2 else 0)
              !cornerV = 1 + 2 * ring
              !cornerI = segs * 9
              !needV = quadCount * 4 + 4 * cornerV
              !needI = quadCount * 6 + 4 * cornerI
          withVertsRaw da needV needI $ \vp ip base baseIdx -> do
            let !(cr, cg, cb, ca) = unpackColorF col
                !u = whitePixel
                pokeQuadAt !vi !ii !qx !qy !qw !qh =
                  pokeQuadSIMD
                    vp
                    ((base + vi) * vertexSize)
                    ip
                    ((baseIdx + ii) * indexSize)
                    qx
                    qy
                    qw
                    qh
                    u
                    u
                    u
                    u
                    cr
                    cg
                    cb
                    ca
                    (fromIntegral (base + vi))
                pokeCorner !vi !ii !ccx !ccy !q = do
                  let !vBase = (base + vi) * vertexSize
                      !centerIdx = fromIntegral (base + vi) :: Word32
                      !inRad = max 0 (rad - 1.0)
                  pokeVertexSIMD vp vBase ccx ccy cr cg cb ca u u
                  forUpTo_ (segs + 1) $ \i -> do
                    let !(ct, st) = cornerCosSin q i
                        !rimI = base + vi + 1 + i
                        !outI = base + vi + 1 + ring + i
                    pokeVertexSIMD vp (rimI * vertexSize) (ccx + inRad * ct) (ccy + inRad * st) cr cg cb ca u u
                    pokeVertexSIMD vp (outI * vertexSize) (ccx + rad * ct) (ccy + rad * st) cr cg cb 0 u u
                    when (i > 0) $ do
                      let !k = i - 1
                          !rim0 = fromIntegral (base + vi + i) :: Word32
                          !rim1 = fromIntegral (base + vi + 1 + i) :: Word32
                          !out0 = fromIntegral (base + vi + 1 + ring + k) :: Word32
                          !out1 = fromIntegral (base + vi + 1 + ring + i) :: Word32
                          !fillOff = (baseIdx + ii + k * 3) * indexSize
                          !fringeOff = (baseIdx + ii + segs * 3 + k * 6) * indexSize
                      pokeByteOff ip fillOff centerIdx
                      pokeByteOff ip (fillOff + 4) rim0
                      pokeByteOff ip (fillOff + 8) rim1
                      pokeQuadIndices ip fringeOff rim0 out0 out1 rim1
                !vi1 = if hasCenter then 4 else 0
                !ii1 = if hasCenter then 6 else 0
                !vi2 = vi1 + (if hasTB then 8 else 0)
                !ii2 = ii1 + (if hasTB then 12 else 0)
                !vi3 = vi2 + (if hasLR then 8 else 0)
                !ii3 = ii2 + (if hasLR then 12 else 0)
            when hasCenter $ pokeQuadAt 0 0 (x + rad) (y + rad) midW midH
            when hasTB $ do
              pokeQuadAt vi1 ii1 (x + rad) y midW rad
              pokeQuadAt (vi1 + 4) (ii1 + 6) (x + rad) (y + h - rad) midW rad
            when hasLR $ do
              pokeQuadAt vi2 ii2 x (y + rad) rad midH
              pokeQuadAt (vi2 + 4) (ii2 + 6) (x + w - rad) (y + rad) rad midH
            pokeCorner vi3 ii3 (x + rad) (y + rad) 0
            pokeCorner (vi3 + cornerV) (ii3 + cornerI) (x + w - rad) (y + rad) 1
            pokeCorner (vi3 + 2 * cornerV) (ii3 + 2 * cornerI) (x + w - rad) (y + h - rad) 2
            pokeCorner (vi3 + 3 * cornerV) (ii3 + 3 * cornerI) (x + rad) (y + h - rad) 3

-- | A filled circle. The centre snaps to the device pixel grid, not the
-- bounding box's origin: snapping the origin rounds @cx - radius@, so two
-- circles sharing a centre but not a radius would land up to a pixel apart.
{-# INLINE pushCircle #-}
pushCircle :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
pushCircle da cx cy radius col = do
  s <- readIORef (daSnapScale da)
  pushRoundedRectRaw da (circleBox (onGrid s cx) (onGrid s cy) radius) radius col

-- | A circle's outline, its centre snapped as 'pushCircle' snaps it.
{-# INLINE pushCircleStroke #-}
pushCircleStroke :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
pushCircleStroke da cx cy radius bw col = do
  s <- readIORef (daSnapScale da)
  pushRoundedStrokeRaw da (circleBox (onGrid s cx) (onGrid s cy) radius) radius bw col

circleBox :: Float -> Float -> Float -> Rect
circleBox cx cy radius = Rect (cx - radius) (cy - radius) (2 * radius) (2 * radius)

{-# INLINE pushRoundedStroke #-}
pushRoundedStroke :: DrawArena -> Rect -> Float -> Float -> Color -> IO ()
pushRoundedStroke da rect radius bw col =
  snapRectOrigin da rect >>= \r -> pushRoundedStrokeRaw da r radius bw col

-- | 'pushRoundedStroke' without snapping the origin, for a rect already
-- anchored to the grid; see 'pushRoundedRectRaw'.
{-# NOINLINE pushRoundedStrokeRaw #-}
pushRoundedStrokeRaw :: DrawArena -> Rect -> Float -> Float -> Color -> IO ()
pushRoundedStrokeRaw da (Rect px py w h) radius bw col
  | w <= 0 || h <= 0 || bw <= 0 = pure ()
  | otherwise = do
      setTexture da glyphAtlasTextureId
      square <- readIORef (daSquareGeometry da)
      let !rad = min (max 0 radius) (min (w * 0.5) (h * 0.5))
          !ibw = min bw (min (w * 0.5) (h * 0.5))
      if square
        then pushSquareStroke da px py w h ibw col
        else do
          let !n = cornerSegments
              -- Square corners run the sides' centre lines into each other;
              -- rounded ones end the sides where the arcs start.
              !corners = rad > 0.5
              !topY = py + ibw / 2
              !leftX = px + ibw / 2
              !x0 = if corners then px + rad else leftX
              !y0 = if corners then py + rad else topY
              !midW = max 0 (if corners then w - 2 * rad else w - ibw)
              !midH = max 0 (if corners then h - 2 * rad else h - ibw)
              !botY = if corners then py + h - ibw / 2 else topY + midH
              !rightX = if corners then px + w - ibw / 2 else leftX + midW
              !cr = max 0.25 (rad - ibw / 2)
              !doTB = midW >= 0.001
              !doLR = midH >= 0.001
              !stripCount = (if doTB then 2 else 0) + (if doLR then 2 else 0)
              -- A stroke no wider than the feather has coincident inner/outer
              -- core rings. Share that ring and omit its zero-area triangles
              -- instead of submitting a fourth vertex and a third quad for
              -- every arc segment. A one-pixel hairline has a solid core now
              -- and takes the four-vertex path.
              !core = max 0 ((ibw - arcFeather) * 0.5)
              !hasCore = core > 0
              !arcStride = if hasCore then 4 else 3
              !arcIndices = if hasCore then 18 else 12
              !arcV = (n + 1) * arcStride
              !arcI = n * arcIndices
              !needV = stripCount * 8 + (if corners then 4 * arcV else 0)
              !needI = stripCount * 18 + (if corners then 4 * arcI else 0)
          withVertsRaw da needV needI $ \vp ip base baseIdx -> do
            let !(r, g, b, a) = unpackColorF col
                pokeArc !vi !ii !ccx !ccy !q = do
                  let !inner = max 0 (cr - core)
                      !outerR = cr + core
                      !innerAA = max 0 (inner - arcFeather)
                      !outerAA = outerR + arcFeather
                  forUpTo_ (n + 1) $ \i -> do
                    let !(ct, st) = cornerCosSin q i
                    pokeBandVerts vp ((base + vi + i * arcStride) * vertexSize) hasCore r g b a $
                      concentricOffsetsSIMD ccx ccy ct st innerAA inner outerR outerAA
                  forUpTo_ n $ \i -> do
                    let !va = fromIntegral (base + vi + i * arcStride) :: Word32
                    pokeBandIndices ip ((baseIdx + ii + i * arcIndices) * indexSize) hasCore va (va + fromIntegral arcStride)
                !viLR = if doTB then 16 else 0
                !iiLR = if doTB then 36 else 0
                !viC = stripCount * 8
                !iiC = stripCount * 18
            when doTB $ do
              pokeStripAt vp ip base baseIdx 0 0 x0 topY (x0 + midW) topY ibw r g b a
              pokeStripAt vp ip base baseIdx 8 18 x0 botY (x0 + midW) botY ibw r g b a
            when doLR $ do
              pokeStripAt vp ip base baseIdx viLR iiLR leftX y0 leftX (y0 + midH) ibw r g b a
              pokeStripAt vp ip base baseIdx (viLR + 8) (iiLR + 18) rightX y0 rightX (y0 + midH) ibw r g b a
            when corners $ do
              pokeArc viC iiC (px + rad) (py + rad) 0
              pokeArc (viC + arcV) (iiC + arcI) (px + w - rad) (py + rad) 1
              pokeArc (viC + 2 * arcV) (iiC + 2 * arcI) (px + w - rad) (py + h - rad) 2
              pokeArc (viC + 3 * arcV) (iiC + 3 * arcI) (px + rad) (py + h - rad) 3

-- | Border of four flat rects inside @(x, y, w, h)@, @t@ thick. The origin is
-- already snapped by the caller; the texture is already selected.
pushSquareStroke :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushSquareStroke da x y w h t col = do
  let edge qx qy qw qh =
        when (qw > 0 && qh > 0) $
          pushQuad da (Rect qx qy qw qh) whitePixel whitePixel whitePixel whitePixel col
      !innerH = h - 2 * t
  edge x y w t
  edge x (y + h - t) w t
  edge x (y + t) t innerH
  edge (x + w - t) (y + t) t innerH

-- | A line @thickness@ wide with round caps: a coverage-AA strip with a
-- round cap on each end. An axis-aligned line is a plain rect spanning its
-- caps.
{-# INLINE pushLine #-}
pushLine :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushLine da x1 y1 x2 y2 thickness col = do
  square <- readIORef (daSquareGeometry da)
  let !r = thickness / 2
      cap cx cy = pushCircle da cx cy r col
  if square
    then pushStroke da x1 y1 x2 y2 thickness col
    else
      if x1 == x2 || y1 == y2
        then pushRect da (Rect (min x1 x2 - r) (min y1 y2 - r) (abs (x2 - x1) + thickness) (abs (y2 - y1) + thickness)) col
        else when (thickness > 0) $ do
          pushStrokeAA da x1 y1 x2 y2 thickness col
          cap x1 y1
          cap x2 y2

-- Coverage-AA strip for a straight segment. Same weight as pushCornerArcStroke,
-- without round caps that blob at rounded-rect corners.
{-# INLINE pushStrokeAA #-}
pushStrokeAA :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushStrokeAA da x0 y0 x1 y1 bw col
  | bw <= 0 = pure ()
  | otherwise = do
      s <- readIORef (daSnapScale da)
      pushStrokeAARaw da (onGrid s x0) (onGrid s y0) (onGrid s x1) (onGrid s y1) bw col

-- | Unsnapped variant: the caller already snapped the endpoints.
pushStrokeAARaw :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushStrokeAARaw da x0 y0 x1 y1 bw col = do
  square <- readIORef (daSquareGeometry da)
  if square
    then pushStroke da x0 y0 x1 y1 bw col
    else case strokeAxes x0 y0 x1 y1 of
      Nothing -> pure ()
      Just _ -> do
        setTexture da glyphAtlasTextureId
        let !(r, g, b, a) = unpackColorF col
        withVertsRaw da 8 18 $ \vp ip base baseIdx ->
          pokeStripAt vp ip base baseIdx 0 0 x0 y0 x1 y1 bw r g b a

strokeAxes :: Float -> Float -> Float -> Float -> Maybe (Float, Float, Float)
strokeAxes x0 y0 x1 y1 =
  let dx = x1 - x0
      dy = y1 - y0
      len = sqrt (dx * dx + dy * dy)
   in if len < 0.001 then Nothing else Just (dx, dy, len)

-- One quad per segment. Plots and diagrams use this; pushLine adds round caps.
pushStroke :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushStroke da x1 y1 x2 y2 thickness col
  | thickness <= 0 = pure ()
  | otherwise = do
      s <- readIORef (daSnapScale da)
      let !px1 = onGrid s x1
          !py1 = onGrid s y1
          !px2 = onGrid s x2
          !py2 = onGrid s y2
      case strokeAxes px1 py1 px2 py2 of
        Nothing -> pure ()
        Just (dx, dy, len) -> do
          setTexture da glyphAtlasTextureId
          let !invLen = (thickness * 0.5) / len
              !hx = (-dy) * invLen
              !hy = dx * invLen
          withVerts da 4 6 $ \vp ip vOff iOff baseIdxWord -> do
            let !(r, g, b, a) = unpackColorF col
                !u = whitePixel
            pokeQuadCornersSIMD vp vOff ip iOff (px1 + hx) (py1 + hy) (px2 + hx) (py2 + hy) (px2 - hx) (py2 - hy) (px1 - hx) (py1 - hy) u u u u r g b a baseIdxWord

-- | A lone filled triangle with anti-aliased edges; see 'pushPolygonAA'. It
-- moves to put the middle of its width, not its first corner, on the grid,
-- so an arrow symmetric about an upright axis stays symmetric whatever its
-- width, and its first corner's row, which an arrow's flat base passes
-- through, stays sharp.
pushFilledTriangle :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushFilledTriangle da x0 y0 x1 y1 x2 y2 col =
  polygonAAFrom
    da
    ((min x0 (min x1 x2) + max x0 (max x1 x2)) * 0.5)
    y0
    (points3 x0 y0 x1 y1 x2 y2)
    triangleRing
    triangleIndices
    (Flat col)

-- | Three points as the flat coordinate array 'pushPolygonAA' and
-- 'pushPolylineAA' take, written straight into the array: a list literal
-- through 'primArrayFromListN' is not fused and boxes every coordinate.
{-# INLINE points3 #-}
points3 :: Float -> Float -> Float -> Float -> Float -> Float -> PrimArray Float
points3 x0 y0 x1 y1 x2 y2 = runPrimArray $ do
  a <- newPrimArray 6
  writePrimArray a 0 x0
  writePrimArray a 1 y0
  writePrimArray a 2 x1
  writePrimArray a 3 y1
  writePrimArray a 4 x2
  writePrimArray a 5 y2
  pure a

triangleIndices :: PrimArray Int
triangleIndices = primArrayFromListN 3 [0, 1, 2]

-- | The ring starts of a lone triangle: one ring of three points.
triangleRing :: PrimArray Int
triangleRing = primArrayFromListN 2 [0, 3]

-- | Half the width of an anti-aliased edge's fade, in logical pixels: half a
-- device pixel, so an edge on the grid is solid on one side of it and clear
-- on the other.
{-# INLINE edgeFeather #-}
edgeFeather :: Float -> Float
edgeFeather s = if s > 0 then 0.5 / s else 0.5

-- | How far a miter may reach, as a multiple of the offset squared: a
-- join sharper than 120 degrees is cut back to twice the offset instead of
-- shooting off into a spike. A polygon's fringe, and the inside of a
-- stroke's corner, are cut back so.
miterLimit :: Float
miterLimit = 4

-- | The offset at a vertex between two edges with unit normals @a@ and @b@,
-- scaled so moving the vertex by @d@ along it moves both edges by @d@. A zero
-- normal (a repeated point) defers to the other.
{-# INLINE miterOf #-}
miterOf :: Float -> Float -> Float -> Float -> (Float, Float)
miterOf ax ay bx by
  | ax == 0 && ay == 0 = (bx, by)
  | bx == 0 && by == 0 = (ax, ay)
  | d2 < 1.0e-6 = (ax, ay)
  | otherwise = let !k = min miterLimit (1 / d2) in (mx * k, my * k)
  where
    !mx = (ax + bx) * 0.5
    !my = (ay + by) * 0.5
    !d2 = mx * mx + my * my

-- | Unit normal of the segment from @(x0, y0)@ to @(x1, y1)@, a quarter turn
-- from its direction, or zero for a zero-length segment.
{-# INLINE segNormal #-}
segNormal :: Float -> Float -> Float -> Float -> (Float, Float)
segNormal x0 y0 x1 y1 =
  let !dx = x1 - x0
      !dy = y1 - y0
      !len = sqrt (dx * dx + dy * dy)
   in if len < 1.0e-6 then (0, 0) else (-dy / len, dx / len)

-- | Whether a shade has a colour for each of @n@ points, or one for all.
shadeCovers :: Shade -> Int -> Bool
shadeCovers sh n = case sh of
  Flat _ -> True
  Shaded cs -> sizeofPrimArray cs >= n

-- | Fill a polygon, holes and all, with anti-aliased edges. @pts@ holds its
-- rings' points as x/y pairs, in either winding and without repeating a
-- ring's first point, one ring after another, and then any points inside
-- it; @rings@ where each ring starts and the last ends; and @tris@ index
-- triples into the points that cover it. The first ring is the outline and
-- the rest are holes, wound the other way. The polygon moves as a whole to
-- put its first point on the device grid: snapping each point on its own
-- would bend a small shape, an arrow's two sides landing a pixel apart.
-- Each ring's edges then fade out across one device pixel centred on them,
-- so an edge on the grid comes out sharp and a slanted one smooth; the
-- points inside have no fade.
pushPolygonAA :: DrawArena -> PrimArray Float -> PrimArray Int -> PrimArray Int -> Shade -> IO ()
pushPolygonAA da pts rings tris
  | sizeofPrimArray pts < 2 = const (pure ())
  | otherwise = polygonAAFrom da (indexPrimArray pts 0) (indexPrimArray pts 1) pts rings tris

-- | 'pushPolygonAA', moved to put @(rx, ry)@ rather than the first point on
-- the grid.
{-# NOINLINE polygonAAFrom #-}
polygonAAFrom :: DrawArena -> Float -> Float -> PrimArray Float -> PrimArray Int -> PrimArray Int -> Shade -> IO ()
polygonAAFrom da rx ry pts rings tris shade
  | n < 3 || nt < 3 || not ringsValid || area == 0 || not (shadeCovers shade n) = pure ()
  | otherwise = do
      s <- readIORef (daSnapScale da)
      square <- readIORef (daSquareGeometry da)
      setTexture da glyphAtlasTextureId
      let !ox = onGrid s rx - rx
          !oy = onGrid s ry - ry
          !f = if square then 0 else edgeFeather s
          -- 'segNormal' points into a ring of positive area; the holes,
          -- wound the other way, then have theirs pointing out of the fill.
          !out = if area > 0 then -1 else 1
          {-# INLINE normalAt #-}
          normalAt i j =
            let (nx, ny) = segNormal (px i) (py i) (px j) (py j)
             in (nx * out, ny * out)
          -- Square geometry has no fade, so no fringe either.
          !fringe = if square then 0 else nOut
          !flat = case shade of
            Flat c -> unpackColorF c
            Shaded _ -> (0, 0, 0, 0)
          {-# INLINE rgbaAt #-}
          rgbaAt i = case shade of
            Flat _ -> flat
            Shaded cs -> unpackColorF (Color (indexPrimArray cs i))
      withVertsRaw da (n + fringe) (nt + 6 * fringe) $ \vp ip base baseIdx -> do
        forRings $ \from to -> forUpTo_ (to - from) $ \k -> do
          let !i = from + k
              !prev = if k == 0 then to - 1 else i - 1
              !next = if i + 1 >= to then from else i + 1
              (ax, ay) = normalAt prev i
              (bx, by) = normalAt i next
              (mx, my) = miterOf ax ay bx by
              !vx = px i + ox
              !vy = py i + oy
              !(r, g, b, a) = rgbaAt i
          pokeVertexSIMD vp ((base + i) * vertexSize) (vx - f * mx) (vy - f * my) r g b a whitePixel whitePixel
          when (fringe > 0) $
            pokeVertexSIMD vp ((base + n + i) * vertexSize) (vx + f * mx) (vy + f * my) r g b 0 whitePixel whitePixel
        forUpTo_ (n - nOut) $ \k -> do
          let !i = nOut + k
              !(r, g, b, a) = rgbaAt i
          pokeVertexSIMD vp ((base + i) * vertexSize) (px i + ox) (py i + oy) r g b a whitePixel whitePixel
        forUpTo_ nt $ \k ->
          pokeByteOff ip ((baseIdx + k) * indexSize) (fromIntegral (base + indexPrimArray tris k) :: Word32)
        when (fringe > 0) $
          forRings $ \from to -> forUpTo_ (to - from) $ \k -> do
            let !i = from + k
                !j = if i + 1 >= to then from else i + 1
                !inI = fromIntegral (base + i) :: Word32
                !inJ = fromIntegral (base + j) :: Word32
                !m = fromIntegral n :: Word32
            pokeQuadIndices ip ((baseIdx + nt + 6 * i) * indexSize) inI inJ (inJ + m) (inI + m)
  where
    !n = sizeofPrimArray pts `div` 2
    !nt = sizeofPrimArray tris - sizeofPrimArray tris `mod` 3
    !nr = sizeofPrimArray rings - 1
    ringAt r = indexPrimArray rings r
    -- Where the rings end and the points inside begin.
    !nOut = if nr >= 1 then ringAt nr else 0
    ringsValid = nr >= 1 && ringAt 0 == 0 && nOut <= n && and [ringAt r < ringAt (r + 1) | r <- [0 .. nr - 1]]
    forRings body = forUpTo_ nr $ \r -> body (ringAt r) (ringAt (r + 1))
    px i = indexPrimArray pts (2 * i)
    py i = indexPrimArray pts (2 * i + 1)
    -- The outline's area, whose sign says which way the rings wind.
    !area = shoelace (if nr >= 1 then ringAt 1 else 0) (\i -> (px i, py i))

-- | Stroke a polyline @w@ wide with anti-aliased sides. @pts@ holds x/y
-- pairs; @closed@ joins the last point back to the first, which should not
-- be repeated. An open line's ends are capped as @cap@ says, and its
-- corners joined as @join@ does, a miter longer than @limit@ times the width
-- beveled. The inside of a corner sharper than 60 degrees is cut back, as
-- 'miterLimit' says. A corner turned so little that no join would show is
-- mitered whatever the join, so a flattened curve's many small turns cost
-- no more than a straight line's.
--
-- The line moves as a whole to put its first point's edges on the device
-- grid, as 'pushPolygonAA' moves a polygon, so a level or upright line a
-- whole number of pixels wide is sharp and every segment keeps its angle.
-- A line thinner than its fade keeps its ink by drawing fainter. Each point
-- has a cross-section of four vertices, faded, solid, solid, faded, and a
-- round or beveled join has two, one square to each of its segments, the
-- outside of its corner filled between them.
{-# NOINLINE pushPolylineAA #-}
pushPolylineAA :: DrawArena -> PrimArray Float -> Float -> Bool -> LineCap -> LineJoin -> Float -> Shade -> IO ()
pushPolylineAA da pts w closed cap join limit shade
  | n < 2 || not (w > 0) || not (shadeCovers shade n) = pure ()
  | otherwise = do
      s <- readIORef (daSnapScale da)
      square <- readIORef (daSquareGeometry da)
      setTexture da glyphAtlasTextureId
      let !hw = w * 0.5
          !ox = onGrid s (px 0 - hw) + hw - px 0
          !oy = onGrid s (py 0 - hw) + hw - py 0
          !f = if square then 0 else edgeFeather s
          !core = max 0 (hw - f)
          !outer = hw + f
          !thin = if core > 0 then 1 else min 1 (w / outer)
          !flat = case shade of
            Flat c -> let !(r, g, b, a) = unpackColorF c in (r, g, b, a * thin)
            Shaded _ -> (0, 0, 0, 0)
          {-# INLINE colourAt #-}
          colourAt i = case shade of
            Flat _ -> flat
            Shaded cs -> let !(r, g, b, a) = unpackColorF (Color (indexPrimArray cs i)) in (r, g, b, a * thin)
          !segs = if closed then n else n - 1
          -- The most chords a round join or cap is cut into: a half turn's,
          -- each within a quarter device pixel of the line's edge.
          !halfTurn = max 2 (arcChords s outer pi)
          !roundJoins = join == RoundJoin
          !roundCaps = cap == RoundCap && not closed
          !maxV = 8 * n + (if roundJoins then 2 * halfTurn * n else 0) + (if roundCaps then 4 * halfTurn else 0)
          !maxI = 18 * segs + 9 * n * (if roundJoins then halfTurn else 1) + (if roundCaps then 18 * halfTurn else 0)
          -- A corner no join would show on: a miter reaching less than a
          -- tenth of a device pixel past the line's side.
          !straightD2 = let q = hw / (hw + 0.1 / max 1 s) in q * q
          {-# INLINE normalAt #-}
          normalAt i =
            let !j = if i + 1 >= n then 0 else i + 1
             in segNormal (px i) (py i) (px j) (py j)
      withVertsReserve da maxV maxI $ \vp ip base baseIdx commit -> do
        let vidx k = fromIntegral (base + k) :: Word32
            vert k x y r g b a = pokeVertexSIMD vp ((base + k) * vertexSize) (x + ox) (y + oy) r g b a whitePixel whitePixel
            -- A cross-section at @(x, y)@: its negative side along
            -- @(nx, ny)@, its positive side along @(qx, qy)@.
            section v x y nx ny qx qy (r, g, b, a) = do
              vert v (x - nx * outer) (y - ny * outer) r g b 0
              vert (v + 1) (x - nx * core) (y - ny * core) r g b a
              vert (v + 2) (x + qx * core) (y + qy * core) r g b a
              vert (v + 3) (x + qx * outer) (y + qy * outer) r g b 0
            -- A section square to both sides, the common case, as the
            -- concentric offsets write it.
            {-# INLINE evenSection #-}
            evenSection v x y mx my (r, g, b, a) =
              pokeBandVerts vp ((base + v) * vertexSize) True r g b a $
                concentricOffsetsSIMD (x + ox) (y + oy) mx my (-outer) (-core) core outer
            {-# INLINE band #-}
            band k va vb = do
              pokeBandIndices ip ((baseIdx + k) * indexSize) True (vidx va) (vidx vb)
              pure (k + 18)
            -- The edge round @(x, y)@ from angle @a0@ through @sweep@ in
            -- @chords@ chords: its solid core fanned from vertex @centre@,
            -- faded out to the edge, from the core and edge vertices @c0@
            -- and @o0@ to @c1@ and @o1@, with the ones between from @v@.
            fan v k centre c0 o0 c1 o1 x y a0 sweep chords (r, g, b, a) = do
              forUpTo_ (chords - 1) $ \j0 -> do
                let !t = a0 + sweep * fromIntegral (j0 + 1) / fromIntegral chords
                    !ct = cos t
                    !st = sin t
                vert (v + 2 * j0) (x + ct * core) (y + st * core) r g b a
                vert (v + 2 * j0 + 1) (x + ct * outer) (y + st * outer) r g b 0
              let coreAt j
                    | j == 0 = c0
                    | j == chords = c1
                    | otherwise = v + 2 * (j - 1)
                  edgeAt j
                    | j == 0 = o0
                    | j == chords = o1
                    | otherwise = v + 2 * (j - 1) + 1
              forUpTo_ chords $ \j -> do
                let !o = (baseIdx + k + 9 * j) * indexSize
                pokeByteOff ip o (vidx centre)
                pokeByteOff ip (o + 4) (vidx (coreAt j))
                pokeByteOff ip (o + 8) (vidx (coreAt (j + 1)))
                pokeQuadIndices ip (o + 12) (vidx (coreAt j)) (vidx (edgeAt j)) (vidx (edgeAt (j + 1))) (vidx (coreAt (j + 1)))
              pure (v + 2 * (chords - 1), k + 9 * chords)
            -- An open end: a section square to the end segment, whose
            -- normal is @(nx, ny)@, at the end point or, for a square cap,
            -- half the width past it; a round cap fans round beyond it.
            -- @dir@ is 1 at the last point and -1 at the first.
            capAt i v k nx ny dir = do
              let !col = colourAt i
                  !reach = if cap == SquareCap then hw * dir else 0
                  !x = px i + ny * reach
                  !y = py i - nx * reach
              evenSection v x y nx ny col
              if cap == RoundCap
                then do
                  (v', k') <- fan (v + 4) k (v + 2) (v + 2) (v + 3) (v + 1) v x y (atan2 ny nx) (negate dir * pi) halfTurn col
                  pure (v, v, v', k')
                else pure (v, v, v + 4, k)
            -- A corner between segments whose normals are @a@ and @b@.
            joinAt i v k ax0 ay0 bx0 by0 = do
              let (!ax, !ay) = if ax0 == 0 && ay0 == 0 then (bx0, by0) else (ax0, ay0)
                  (!bx, !by) = if bx0 == 0 && by0 == 0 then (ax, ay) else (bx0, by0)
                  !mx = (ax + bx) * 0.5
                  !my = (ay + by) * 0.5
                  !d2 = mx * mx + my * my
                  !col = colourAt i
                  !x = px i
                  !y = py i
                  -- The inside of the corner, cut back if very sharp.
                  (!ix, !iy) = miterOf ax ay bx by
                  miter = if join == MiterJoin then miterOffset limit ax ay bx by else Nothing
              if d2 >= straightD2 || (d2 >= 0.25 && isJust miter)
                then do
                  evenSection v x y ix iy col
                  pure (v, v, v + 4, k)
                else do
                  -- The outside of the corner is the side the next segment
                  -- turns away from.
                  let !turnsPositive = by * ax - bx * ay > 0
                      !outSign = if turnsPositive then -1 else 1
                  case miter of
                    Just (fx, fy) -> do
                      -- A sharp miter: the outside runs to the full point.
                      if turnsPositive then section v x y fx fy ix iy col else section v x y ix iy fx fy col
                      pure (v, v, v + 4, k)
                    Nothing -> do
                      let (!centre, !cIn, !oIn, !cOut, !oOut)
                            | turnsPositive = (v + 2, v + 1, v, v + 5, v + 4)
                            | otherwise = (v + 1, v + 2, v + 3, v + 6, v + 7)
                          !ux = outSign * ax
                          !uy = outSign * ay
                          !sweep = atan2 (ux * outSign * by - uy * outSign * bx) (ux * outSign * bx + uy * outSign * by)
                          !chords = if join == RoundJoin then arcChords s outer (abs sweep) else 1
                      if turnsPositive
                        then section v x y ax ay ix iy col >> section (v + 4) x y bx by ix iy col
                        else section v x y ix iy ax ay col >> section (v + 4) x y ix iy bx by col
                      (v', k') <- fan (v + 8) k centre cIn oIn cOut oOut x y (atan2 uy ux) sweep chords col
                      pure (v, v + 4, v', k')
            pointAt i v k
              | not closed && i == 0 = let (nx, ny) = normalAt 0 in capAt i v k nx ny (-1)
              | not closed && i == n - 1 = let (nx, ny) = normalAt (n - 2) in capAt i v k nx ny 1
              | otherwise =
                  let (ax, ay) = normalAt (if i == 0 then n - 1 else i - 1)
                      (bx, by) = normalAt i
                   in joinAt i v k ax ay bx by
            loop !i !v !k !prevOut !firstIn
              | i >= n = do
                  k' <- if closed then band k prevOut firstIn else pure k
                  commit v k'
              -- The common corner, one a miter joins without cutting back:
              -- one section, with no tuple to return.
              | closed || (i > 0 && i < n - 1)
              , (ax, ay) <- normalAt (if i == 0 then n - 1 else i - 1)
              , (bx, by) <- normalAt i
              , evenJoin ax ay bx by = do
                  let (mx, my) = miterOf ax ay bx by
                  evenSection v (px i) (py i) mx my (colourAt i)
                  k' <- if i > 0 then band k prevOut v else pure k
                  loop (i + 1) (v + 4) k' v (if i == 0 then v else firstIn)
              | otherwise = do
                  (inV, outV, v', k') <- pointAt i v k
                  k'' <- if i > 0 then band k' prevOut inV else pure k'
                  loop (i + 1) v' k'' outV (if i == 0 then inV else firstIn)
            -- A corner a single section square to its miter draws: one
            -- turned so little no join would show, or a miter within its
            -- limit that needs no cutting back inside.
            evenJoin ax ay bx by
              | (ax == 0 && ay == 0) || (bx == 0 && by == 0) = True
              | otherwise =
                  let !mx = (ax + bx) * 0.5
                      !my = (ay + by) * 0.5
                      !d2 = mx * mx + my * my
                   in d2 >= straightD2 || (join == MiterJoin && d2 >= 0.25 && d2 * limit * limit >= 1)
        loop 0 0 0 0 0
  where
    !n = sizeofPrimArray pts `div` 2
    px i = indexPrimArray pts (2 * i)
    py i = indexPrimArray pts (2 * i + 1)

-- | Chords for @sweep@ radians of a circle @r@ logical pixels across on a
-- display of @s@ device pixels to the logical one, each within a quarter
-- device pixel of it: at least 1, at most 32.
arcChords :: Float -> Float -> Float -> Int
arcChords s r sweep
  | not (rd > 0.25) = 1
  | otherwise = clamp 1 32 (ceiling (abs sweep / (2 * acos (1 - 0.25 / rd))))
  where
    rd = r * max 1 s
