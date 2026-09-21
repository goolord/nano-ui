{-# LANGUAGE StrictData #-}

-- | Solid geometry emitters: rects, gradients, images, rounded fills and
-- borders, coverage-AA strokes, lines and triangles.
module NanoUI.Internal.Draw.Shapes
  ( pushRect
  , pushQuadGradient
  , pushImage
  , pushRoundedRect
  , pushRoundedRectRaw
  , pushRoundedStroke
  , pushCircle
  , pushCircleStroke
  , pushLine
  , pushStrokeAA
  , pushStroke
  , pushFilledTriangle
  ) where

import Control.Monad (when)
import Data.IORef (readIORef)
import Data.Word (Word32, Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (pokeByteOff)
import NanoUI.Internal.Draw.Arena
import NanoUI.Internal.Draw.Types (DrawArena (..), glyphAtlasTextureId, indexSize, vertexSize)
import NanoUI.Internal.SIMD
  ( concentricOffsetsSIMD
  , pokeQuadGradientSIMD
  , pokeQuadSIMD
  , pokeVertexSIMD
  )
import NanoUI.Internal.Types (Color (..), Rect (..), onGrid)

{-# INLINE pushRect #-}
pushRect :: DrawArena -> Rect -> Color -> IO ()
pushRect da rect col = do
  r <- snapRectOrigin da rect
  setTexture da glyphAtlasTextureId
  pushQuad da r whitePixelU whitePixelV whitePixelU whitePixelV col

-- Quad with a color per corner. GPU interpolates across the two triangles.
-- Corners: top-left, top-right, bottom-right, bottom-left.
pushQuadGradient :: DrawArena -> Rect -> Color -> Color -> Color -> Color -> IO ()
pushQuadGradient da (Rect x y w h) tl tr br bl
  | w <= 0 || h <= 0 = pure ()
  | otherwise = do
      s <- readIORef (daSnapScale da)
      setTexture da glyphAtlasTextureId
      let !px = onGrid s x
          !py = onGrid s y
          !c0 = unpackColorF tl
          !c1 = unpackColorF tr
          !c2 = unpackColorF br
          !c3 = unpackColorF bl
      withVerts da 4 6 $ \vp ip vOff iOff baseIdxWord ->
        pokeQuadGradientSIMD vp vOff ip iOff px py w h whitePixelU whitePixelV c0 c1 c2 c3 baseIdxWord

{-# INLINE pushImage #-}
pushImage :: DrawArena -> Rect -> Int -> Float -> Float -> Float -> Float -> Color -> IO ()
pushImage da rect tex u0 v0 u1 v1 col
  | tex <= 0 = pushRect da rect col
  | otherwise = do
      r <- snapRectOrigin da rect
      setTexture da tex
      pushQuad da r u0 v0 u1 v1 col

-- 4 segments per 90° arc. Lookup table in cornerCosSin has 5 points per quadrant.
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

-- Precomputed unit-circle cos/sin for rounded-rect corners (4 segments per 90° arc).
{-# INLINE cornerCosSin #-}
cornerCosSin :: Int -> Int -> (Float, Float)
cornerCosSin q seg =
  case q * 5 + seg of
    0 -> (-1.0, 0.0)
    1 -> (-0.9238795325, -0.3826834324)
    2 -> (-0.7071067812, -0.7071067812)
    3 -> (-0.3826834324, -0.9238795325)
    4 -> (0.0, -1.0)
    5 -> (0.0, -1.0)
    6 -> (0.3826834324, -0.9238795325)
    7 -> (0.7071067812, -0.7071067812)
    8 -> (0.9238795325, -0.3826834324)
    9 -> (1.0, 0.0)
    10 -> (1.0, 0.0)
    11 -> (0.9238795325, 0.3826834324)
    12 -> (0.7071067812, 0.7071067812)
    13 -> (0.3826834324, 0.9238795325)
    14 -> (0.0, 1.0)
    15 -> (0.0, 1.0)
    16 -> (-0.3826834324, 0.9238795325)
    17 -> (-0.7071067812, 0.7071067812)
    18 -> (-0.9238795325, 0.3826834324)
    19 -> (-1.0, 0.0)
    _ -> (0.0, 0.0)

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
      pokeEnd !ev !ex !ey = do
        let ((p0x, p0y), (p1x, p1y), (p2x, p2y), (p3x, p3y)) =
              concentricOffsetsSIMD ex ey nx ny (-outer) (-core) core outer
            !vBase = (base + vi + ev) * vertexSize
        pokeVertexSIMD vp vBase p0x p0y r g b 0 whitePixelU whitePixelV
        pokeVertexSIMD vp (vBase + 32) p1x p1y r g b a whitePixelU whitePixelV
        pokeVertexSIMD vp (vBase + 64) p2x p2y r g b a whitePixelU whitePixelV
        pokeVertexSIMD vp (vBase + 96) p3x p3y r g b 0 whitePixelU whitePixelV
  pokeEnd 0 x0 y0
  pokeEnd 4 x1 y1
  let !va = fromIntegral (base + vi) :: Word32
      !vb = va + 4
  pokeQuadIndices ip ((baseIdx + ii) * indexSize) va (va + 1) (vb + 1) vb
  pokeQuadIndices ip ((baseIdx + ii + 6) * indexSize) (va + 1) (va + 2) (vb + 2) (vb + 1)
  pokeQuadIndices ip ((baseIdx + ii + 12) * indexSize) (va + 2) (va + 3) (vb + 3) (vb + 2)

{-# INLINE pushRoundedRect #-}
pushRoundedRect :: DrawArena -> Rect -> Float -> Color -> IO ()
pushRoundedRect da (Rect x y w h) radius col = do
  s <- readIORef (daSnapScale da)
  pushRoundedRectRaw da (Rect (onGrid s x) (onGrid s y) w h) radius col

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
                !u = whitePixelU
                !v = whitePixelV
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
                    v
                    u
                    v
                    cr
                    cg
                    cb
                    ca
                    (fromIntegral (base + vi))
                pokeCorner !vi !ii !ccx !ccy !q = do
                  let !vBase = (base + vi) * vertexSize
                      !centerIdx = fromIntegral (base + vi) :: Word32
                      !inRad = max 0 (rad - 1.0)
                  pokeVertexSIMD vp vBase ccx ccy cr cg cb ca u v
                  loopIO 0 segs $ \i -> do
                    let !(ct, st) = cornerCosSin q i
                        !rimI = base + vi + 1 + i
                        !outI = base + vi + 1 + ring + i
                    pokeVertexSIMD vp (rimI * vertexSize) (ccx + inRad * ct) (ccy + inRad * st) cr cg cb ca u v
                    pokeVertexSIMD vp (outI * vertexSize) (ccx + rad * ct) (ccy + rad * st) cr cg cb 0 u v
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
pushRoundedStroke da (Rect x y w h) radius bw col = do
  s <- readIORef (daSnapScale da)
  pushRoundedStrokeRaw da (Rect (onGrid s x) (onGrid s y) w h) radius bw col

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
        else if rad <= 0.5
        then do
          let !t = ibw
              !ox = px + t / 2
              !oy = py + t / 2
              !ow = max 0 (w - t)
              !oh = max 0 (h - t)
              !doTB = ow >= 0.001
              !doLR = oh >= 0.001
              !stripCount = (if doTB then 2 else 0) + (if doLR then 2 else 0)
          withVertsRaw da (stripCount * 8) (stripCount * 18) $ \vp ip base baseIdx -> do
            let !(r, g, b, a) = unpackColorF col
                !viLR = if doTB then 16 else 0
                !iiLR = if doTB then 36 else 0
            when doTB $ do
              pokeStripAt vp ip base baseIdx 0 0 ox oy (ox + ow) oy t r g b a
              pokeStripAt vp ip base baseIdx 8 18 ox (oy + oh) (ox + ow) (oy + oh) t r g b a
            when doLR $ do
              pokeStripAt vp ip base baseIdx viLR iiLR ox oy ox (oy + oh) t r g b a
              pokeStripAt vp ip base baseIdx (viLR + 8) (iiLR + 18) (ox + ow) oy (ox + ow) (oy + oh) t r g b a
        else do
          let !n = cornerSegments
          let !midW = max 0 (w - 2 * rad)
              !midH = max 0 (h - 2 * rad)
              !topY = py + ibw / 2
              !botY = py + h - ibw / 2
              !leftX = px + ibw / 2
              !rightX = px + w - ibw / 2
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
              !needV = stripCount * 8 + 4 * arcV
              !needI = stripCount * 18 + 4 * arcI
          withVertsRaw da needV needI $ \vp ip base baseIdx -> do
            let !(r, g, b, a) = unpackColorF col
                pokeArc !vi !ii !ccx !ccy !q = do
                  let !inner = max 0 (cr - core)
                      !outerR = cr + core
                      !innerAA = max 0 (inner - arcFeather)
                      !outerAA = outerR + arcFeather
                  loopIO 0 n $ \i -> do
                    let !(ct, st) = cornerCosSin q i
                        !v0 = base + vi + i * arcStride
                        !vBase = v0 * vertexSize
                        ((p0x, p0y), (p1x, p1y), (p2x, p2y), (p3x, p3y)) =
                          concentricOffsetsSIMD ccx ccy ct st innerAA inner outerR outerAA
                    pokeVertexSIMD vp vBase p0x p0y r g b 0 whitePixelU whitePixelV
                    pokeVertexSIMD vp (vBase + 32) p1x p1y r g b a whitePixelU whitePixelV
                    when hasCore $
                      pokeVertexSIMD vp (vBase + 64) p2x p2y r g b a whitePixelU whitePixelV
                    pokeVertexSIMD vp (vBase + (arcStride - 1) * vertexSize) p3x p3y r g b 0 whitePixelU whitePixelV
                  loopIO 0 (n - 1) $ \i -> do
                    let !va = fromIntegral (base + vi + i * arcStride) :: Word32
                        !vb = va + fromIntegral arcStride
                        !iOff = (baseIdx + ii + i * arcIndices) * indexSize
                    pokeQuadIndices ip iOff va (va + 1) (vb + 1) vb
                    pokeQuadIndices ip (iOff + 24) (va + 1) (va + 2) (vb + 2) (vb + 1)
                    when hasCore $
                      pokeQuadIndices ip (iOff + 48) (va + 2) (va + 3) (vb + 3) (vb + 2)
                !viLR = if doTB then 16 else 0
                !iiLR = if doTB then 36 else 0
                !viC = stripCount * 8
                !iiC = stripCount * 18
            when doTB $ do
              pokeStripAt vp ip base baseIdx 0 0 (px + rad) topY (px + rad + midW) topY ibw r g b a
              pokeStripAt vp ip base baseIdx 8 18 (px + rad) botY (px + rad + midW) botY ibw r g b a
            when doLR $ do
              pokeStripAt vp ip base baseIdx viLR iiLR leftX (py + rad) leftX (py + rad + midH) ibw r g b a
              pokeStripAt vp ip base baseIdx (viLR + 8) (iiLR + 18) rightX (py + rad) rightX (py + rad + midH) ibw r g b a
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
          pushQuad da (Rect qx qy qw qh) whitePixelU whitePixelV whitePixelU whitePixelV col
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
                poke off px py = pokeVertexSIMD vp off px py r g b a whitePixelU whitePixelV
            poke vOff (px1 + hx) (py1 + hy)
            poke (vOff + 32) (px2 + hx) (py2 + hy)
            poke (vOff + 64) (px2 - hx) (py2 - hy)
            poke (vOff + 96) (px1 - hx) (py1 - hy)
            pokeQuadIndices ip iOff baseIdxWord (baseIdxWord + 1) (baseIdxWord + 2) (baseIdxWord + 3)

pushFilledTriangle :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
pushFilledTriangle da x0 y0 x1 y1 x2 y2 col = do
  s <- readIORef (daSnapScale da)
  setTexture da glyphAtlasTextureId
  let !(r, g, b, a) = unpackColorF col
  withVerts da 3 3 $ \vp ip vOff iOff baseIdxWord -> do
    pokeVertexSIMD vp vOff (onGrid s x0) (onGrid s y0) r g b a whitePixelU whitePixelV
    pokeVertexSIMD vp (vOff + 32) (onGrid s x1) (onGrid s y1) r g b a whitePixelU whitePixelV
    pokeVertexSIMD vp (vOff + 64) (onGrid s x2) (onGrid s y2) r g b a whitePixelU whitePixelV
    pokeByteOff ip iOff baseIdxWord
    pokeByteOff ip (iOff + 4) (baseIdxWord + 1)
    pokeByteOff ip (iOff + 8) (baseIdxWord + 2)
