module NanoUI.Rgfw.Render
  ( renderArena
  ) where

import Control.Monad (when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, peekElemOff, pokeElemOff)
import NanoUI (Color (..), Rect (..), roundHalfUp)
import NanoUI.Rgfw.Internal.Context (TextSpan, paintInLayerOrder)
import NanoUI.Rgfw.Internal.Font.Cozette (CozetteFont)
import NanoUI.Rgfw.Internal.Gl (physClip, toPhysRect)
import NanoUI.Rgfw.Surface
  ( RgfwSurface (..)
  , drawTextScaled
  , fillRect
  , packColor
  , popClip
  , pushClip
  )
import NanoUI.Testing
  ( DrawCmd (..)
  , DrawData (..)
  , backdropDimTextureId
  , forDrawCmdsInLayer_
  , indexSize
  , vertexSize
  )

-- | One vertex colour, straight out of the shared draw buffer.
type RGBA = (Float, Float, Float, Float)

-- | Rasterize a frame into pixels: fill every primitive of the core's
-- 'DrawData' and stamp the core's collected text spans with the embedded
-- Cozette bitmap font, in 'paintInLayerOrder'. Widget drawing (windows,
-- popups, buttons, fields, scroll chrome, ...) comes from the core draw path,
-- so RGFW gets theme parity and core drawing behavior for free.
--
-- The frame must come from a context built by
-- 'NanoUI.Rgfw.Internal.Context.newRgfwContext': square geometry means the buffer holds
-- only flat quads and triangles (no rounded fans or transparent AA fringes),
-- and external text means it holds no text quads, so every primitive is filled
-- as-is and glyphs come solely from the span lists.
renderArena ::
  RgfwSurface ->
  CozetteFont ->
  Float -> -- logical (layout) -> physical (pixel) scale
  DrawData ->
  [TextSpan] -> -- base spans
  [TextSpan] -> -- overlay spans
  IO ()
renderArena surf font !scale drawData baseSpans overlaySpans =
  paintInLayerOrder
    (\layer -> forDrawCmdsInLayer_ layer drawData (applyCmd surf scale drawData))
    (mapM_ (stampSpan surf font scale) baseSpans)
    (mapM_ (stampSpan surf font scale) overlaySpans)

applyCmd :: RgfwSurface -> Float -> DrawData -> DrawCmd -> IO ()
applyCmd surf !scale dd cmd
  | count < 3 = pure ()
  | otherwise =
      withForeignPtr (drawVertices dd) $ \vp ->
        withForeignPtr (drawIndices dd) $ \ip ->
          case physClip scale (sWidth surf) (sHeight surf) (Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)) of
            Nothing -> pure ()
            Just clip -> walkPrims surf scale dd vp ip isDim clip start (start + count)
  where
    start = fromIntegral (cmdIndexOffset cmd)
    count = fromIntegral (cmdIndexCount cmd)
    isDim = cmdTextureId cmd == backdropDimTextureId

-- | Walk a cmd's index range. The draw buffer holds two primitive shapes:
-- quads (6 indices, @a b c a c d@, vertices @a..d@ contiguous) and filled
-- triangles (3 fresh indices). A group whose 4th and 5th indices repeat the
-- 1st and 3rd is a quad; anything else is a triangle.
walkPrims ::
  RgfwSurface ->
  Float ->
  DrawData ->
  Ptr Word8 ->
  Ptr Word8 ->
  Bool ->
  (Int, Int, Int, Int) ->
  Int ->
  Int ->
  IO ()
walkPrims surf scale dd vp ip isDim clip !i !end
  | i + 3 > end = pure ()
  | otherwise = do
      ia <- indexAt dd ip i
      ib <- indexAt dd ip (i + 1)
      ic <- indexAt dd ip (i + 2)
      isQuad <-
        if i + 6 <= end
          then do
            ia' <- indexAt dd ip (i + 3)
            ic' <- indexAt dd ip (i + 4)
            pure (ia' == ia && ic' == ic)
          else pure False
      if isQuad
        then do
          idv <- indexAt dd ip (i + 5)
          stampQuad surf scale dd vp isDim clip ia ib ic idv
          walkPrims surf scale dd vp ip isDim clip (i + 6) end
        else do
          stampTriangle surf scale dd vp clip ia ib ic
          walkPrims surf scale dd vp ip isDim clip (i + 3) end

-- | Fill one quad. Axis-aligned quads are filled as pixel rects; rotated ones
-- (diagonal strokes such as check marks and close crosses) are scan converted.
-- 4-corner gradient quads have no gradient support on the software surface and
-- take the average of their corner colours, which is exact for the
-- equal-corner quads the core emits for plain rects. Translucent fills (drop
-- shadows, modal backdrop dims) alpha-blend instead of overwriting;
-- 'backdropDimTextureId' cmds blend uniformly (mix taken from vertex alpha).
stampQuad ::
  RgfwSurface ->
  Float ->
  DrawData ->
  Ptr Word8 ->
  Bool ->
  (Int, Int, Int, Int) ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO ()
stampQuad surf scale dd vp isDim clip ia ib ic idv = do
  mvs <- mapM (vertexAt dd vp) [ia, ib, ic, idv]
  case mvs of
    [Just (x0, y0, c0), Just (x1, y1, c1), Just (x2, y2, c2), Just (x3, y3, c3)] -> do
      let !col = avgRGBA [c0, c1, c2, c3]
          xs = [x0, x1, x2, x3]
          ys = [y0, y1, y2, y3]
          !minX = minimum xs
          !maxX = maximum xs
          !minY = minimum ys
          !maxY = maximum ys
          onEdge lo hi v = abs (v - lo) < 1.0e-3 || abs (v - hi) < 1.0e-3
          axisAligned = all (onEdge minX maxX) xs && all (onEdge minY maxY) ys
      when (alpha8 col > 0) $
        if axisAligned
          then do
            let (!px, !py, !pw, !ph) = toPhysRect scale minX minY (maxX - minX) (maxY - minY)
            case clipRect clip px py pw ph of
              Nothing -> pure ()
              Just (fx, fy, fw, fh) ->
                if isDim || alpha8 col < 255
                  then blendRectPx surf fx fy fw fh col
                  else fillRect surf fx fy fw fh (surfaceWord col)
          else
            fillConvexPx surf clip [physPt scale x0 y0, physPt scale x1 y1, physPt scale x2 y2, physPt scale x3 y3] col
    _ -> pure ()

-- | Fill a triangle (sort arrows, select chevrons) with its average corner
-- colour.
stampTriangle ::
  RgfwSurface ->
  Float ->
  DrawData ->
  Ptr Word8 ->
  (Int, Int, Int, Int) ->
  Int ->
  Int ->
  Int ->
  IO ()
stampTriangle surf scale dd vp clip ia ib ic = do
  mvs <- mapM (vertexAt dd vp) [ia, ib, ic]
  case mvs of
    [Just (x0, y0, c0), Just (x1, y1, c1), Just (x2, y2, c2)] -> do
      let !col = avgRGBA [c0, c1, c2]
      when (alpha8 col > 0) $
        fillConvexPx surf clip [physPt scale x0 y0, physPt scale x1 y1, physPt scale x2 y2] col
    _ -> pure ()

-- | Flat convex polygon fill on pixel centers, clipped to the cmd clip. Only
-- triangles and rotated quads come through here; rects take the span fill.
fillConvexPx ::
  RgfwSurface ->
  (Int, Int, Int, Int) ->
  [(Int, Int)] ->
  RGBA ->
  IO ()
fillConvexPx _ _ [] _ = pure ()
fillConvexPx surf (cx0, cy0, cx1, cy1) pts@(p0 : _) col
  | area == 0 = pure ()
  | otherwise = goRows yLo
  where
    xLo = max cx0 (minimum (map fst pts))
    xHi = min (cx1 - 1) (maximum (map fst pts))
    yLo = max cy0 (minimum (map snd pts))
    yHi = min (cy1 - 1) (maximum (map snd pts))
    edges = zip pts (drop 1 pts ++ [p0])
    area = sum [f ax * f by - f bx * f ay | ((ax, ay), (bx, by)) <- edges]
    !s = if area < 0 then -1 else 1 :: Float
    -- The nested forM_ ranges leave a shared x-coordinate list in optimized
    -- Core. Traverse numeric bounds directly instead, preserving row order.
    goRows !py = when (py <= yHi) $ do
      goCols py xLo
      when (py < yHi) $ goRows (py + 1)
    goCols !py !px = when (px <= xHi) $ do
      let !pxc = fromIntegral px + (0.5 :: Float)
          !pyc = fromIntegral py + (0.5 :: Float)
          inside ((ax, ay), (bx, by)) =
            ((f bx - f ax) * (pyc - f ay) - (f by - f ay) * (pxc - f ax)) * s >= 0
      when (all inside edges) $
        pokePixel surf px py col
      when (px < xHi) $ goCols py (px + 1)
    f :: Int -> Float
    f = fromIntegral

-- | Stamp one core text span with the Cozette bitmap font. The span carries
-- its own clip rect (logical pixels), pushed onto the surface clip stack for
-- the blit. The span's background colour is not painted: every real
-- background is already a quad in the DrawData, and span rects cover
-- the text run rather than the widget, so painting it would overdraw.
stampSpan :: RgfwSurface -> CozetteFont -> Float -> TextSpan -> IO ()
stampSpan surf font !scale (Rect rx ry _ _, txt, fg, _, clip)
  | T.null txt = pure ()
  | otherwise = case physClip scale (sWidth surf) (sHeight surf) clip of
      Nothing -> pure ()
      Just (cx0, cy0, cx1, cy1) -> do
        pushClip surf cx0 cy0 (cx1 - cx0) (cy1 - cy0)
        drawTextScaled surf font scale rx ry txt (packColor fg)
        popClip surf

-- | Flat alpha-blended fill. x/y/w/h must already be clipped to the surface.
blendRectPx :: RgfwSurface -> Int -> Int -> Int -> Int -> RGBA -> IO ()
blendRectPx surf !x !y !w !h col
  | w <= 0 || h <= 0 = pure ()
  | otherwise = loopY y
  where
    buf = sBuffer surf
    stride = sWidth surf
    loopY !py
      | py >= y + h = pure ()
      | otherwise = loopX py x >> loopY (py + 1)
    loopX !py !px
      | px >= x + w = pure ()
      | otherwise = do
          let !base = py * stride + px
          !dst <- peekElemOff buf base
          pokeElemOff buf base (blendPixelWord dst col)
          loopX py (px + 1)

pokePixel :: RgfwSurface -> Int -> Int -> RGBA -> IO ()
pokePixel surf px py col
  | alpha8 col >= 255 =
      pokeElemOff (sBuffer surf) (py * sWidth surf + px) (surfaceWord col)
  | otherwise = do
      let !base = py * sWidth surf + px
      !dst <- peekElemOff (sBuffer surf) base
      pokeElemOff (sBuffer surf) base (blendPixelWord dst col)

-- | Alpha-blend an RGBA colour over a packed BGRA surface pixel.
blendPixelWord :: Word32 -> RGBA -> Word32
blendPixelWord !dst (r, g, b, a) =
  let !t = max 0 (min 1 a)
      !inv = 1 - t
      !dr = fromIntegral ((dst `shiftR` 16) .&. 0xFF) :: Float
      !dg = fromIntegral ((dst `shiftR` 8) .&. 0xFF) :: Float
      !db = fromIntegral (dst .&. 0xFF) :: Float
      !orC = r * t + dr * inv
      !ogC = g * t + dg * inv
      !obC = b * t + db * inv
   in packColor (Color (rgbaWord (orC, ogC, obC, 1)))

indexAt :: DrawData -> Ptr Word8 -> Int -> IO Int
indexAt dd ip i
  | i < 0 || i >= drawIndexCount dd = pure (-1)
  | otherwise = do
      !w <- peekByteOff ip (i * indexSize) :: IO Word32
      pure (fromIntegral w)

vertexAt :: DrawData -> Ptr Word8 -> Int -> IO (Maybe (Float, Float, RGBA))
vertexAt dd vp vi
  | vi < 0 || vi >= drawVertexCount dd = pure Nothing
  | otherwise = do
      x <- peekFloatAt vp (vi * vertexSize)
      y <- peekFloatAt vp (vi * vertexSize + 4)
      r <- peekFloatAt vp (vi * vertexSize + 8)
      g <- peekFloatAt vp (vi * vertexSize + 12)
      b <- peekFloatAt vp (vi * vertexSize + 16)
      a <- peekFloatAt vp (vi * vertexSize + 20)
      pure (Just (x, y, (r, g, b, a)))

peekFloatAt :: Ptr Word8 -> Int -> IO Float
peekFloatAt p off = peekByteOff p off

clipRect :: (Int, Int, Int, Int) -> Int -> Int -> Int -> Int -> Maybe (Int, Int, Int, Int)
clipRect (cx0, cy0, cx1, cy1) x y w h =
  let !x0 = max cx0 x
      !y0 = max cy0 y
      !x1 = min cx1 (x + w)
      !y1 = min cy1 (y + h)
   in if x0 >= x1 || y0 >= y1 then Nothing else Just (x0, y0, x1 - x0, y1 - y0)

physPt :: Float -> Float -> Float -> (Int, Int)
physPt !s !x !y = (roundHalfUp (x * s), roundHalfUp (y * s))

rgbaA :: RGBA -> Float
rgbaA (_, _, _, a) = a

-- Vertex alphas are 0..1 floats; the fill gates use the packed 0..255 scale.
alpha8 :: RGBA -> Int
alpha8 c = max 0 (min 255 (round (rgbaA c * 255) :: Int))

avgRGBA :: [RGBA] -> RGBA
avgRGBA cs =
  let !n = fromIntegral (length cs) :: Float
      mean g = sum (map g cs) / n
   in ( mean (\(r, _, _, _) -> r)
      , mean (\(_, g, _, _) -> g)
      , mean (\(_, _, b, _) -> b)
      , mean (\(_, _, _, a) -> a)
      )

clampByte :: Float -> Word32
clampByte v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int)))

rgbaWord :: RGBA -> Word32
rgbaWord (r, g, b, a) =
  (clampByte r `shiftL` 24) .|. (clampByte g `shiftL` 16) .|. (clampByte b `shiftL` 8) .|. clampByte a

surfaceWord :: RGBA -> Word32
surfaceWord = packColor . Color . rgbaWord
