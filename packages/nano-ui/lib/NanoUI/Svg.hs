-- | An anti-aliased rasterizer for the SVG icons that @nano-svg@ parses.
--
-- @nano-svg@ reads the document into shapes with absolute segments, a
-- transform and a resolved style; this module flattens those to contours,
-- strokes them and scan-converts the result into an RGBA image. See
-- "Graphics.NanoSvg" for what the parser supports; gradients, patterns,
-- text, masks, clipping and filters are not among them.
module NanoUI.Svg
  ( Svg
  , svgDocument
  , svgSize
  , svgKey
  , svgMonochrome
  , parseSvg
  , rasterizeSvg
  , rasterizeSvgIn
  ) where

import Control.Monad (forM_, when)
import Control.Monad.Primitive (PrimMonad, PrimState)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray, indexPrimArray, newPrimArray, readPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray)
import Data.Word (Word16, Word8)
import Foreign.Marshal.Utils (fillBytes)
import Foreign.Storable (pokeByteOff)
import Graphics.NanoSvg
  ( Box (..)
  , Document (..)
  , FillRule (..)
  , LineCap (..)
  , LineJoin (..)
  , Matrix (..)
  , Paint (..)
  , Point (..)
  , RGBA (..)
  , Segment (..)
  , Shape (..)
  , Style (..)
  , black
  )
import Graphics.NanoSvg qualified as NanoSvg
import NanoUI.Internal.Path (Rings (..), buildRings, cleanRings, ringCount)
import NanoUI.Internal.Path qualified as P
import NanoUI.Internal.Types (Color (..), Rect (..), bucketOffsets, clamp, colorA, colorB, colorG, colorR, newZeroedPrimArray)

-- | A parsed SVG document. Only 'parseSvg' makes one, so its raster cache
-- key ('svgKey') always describes its source: an edited @nano-svg@
-- 'Document' cannot reuse another drawing's rasters.
newtype Svg = Svg Document
  deriving newtype (Eq, Show)

-- | Parse an SVG document's bytes.
parseSvg :: ByteString -> Either String Svg
parseSvg = fmap Svg . NanoSvg.parseSvg

-- | The parsed shapes, read-only, as @nano-svg@ returns them.
svgDocument :: Svg -> Document
svgDocument (Svg doc) = doc

-- | The document's own width and height, from its @width@ and @height@ or
-- else its @viewBox@.
svgSize :: Svg -> (Float, Float)
svgSize = documentSize . svgDocument

-- | A hash of the source, for caching rasters.
svgKey :: Svg -> Int
svgKey = documentKey . svgDocument

-- | Every paint is @currentColor@ or unspecified, so the drawing is one
-- colour and can be tinted.
svgMonochrome :: Svg -> Bool
svgMonochrome = documentMonochrome . svgDocument

--------------------------------------------------------------------------------
-- Flattening
--------------------------------------------------------------------------------

-- | Convert a @nano-svg@ matrix into a path 'P.Transform'.
toTransform :: Matrix -> P.Transform
toTransform (Matrix a b c d e f) = P.Transform a b c d e f

-- | Flatten segments through a transform into contours in device pixels,
-- each tagged 1 when closed. Curves are split until they are within a
-- quarter pixel of their chords. Arcs use steps of at most pi/16, and at
-- least four steps.
flatten :: P.Transform -> SmallArray Segment -> Rings
flatten tr segs =
  P.flattenPath 0.25 (\_ sweep -> max 4 (ceiling (abs sweep / (pi / 16)))) tr (P.Path (map segment (toList segs)))
  where
    segment = \case
      MoveTo (Point x y) -> P.SegMove x y
      LineTo (Point x y) -> P.SegLine x y
      CubicTo (Point x1 y1) (Point x2 y2) (Point x y) -> P.SegCubic x1 y1 x2 y2 x y
      QuadTo (Point qx qy) (Point x y) -> P.SegQuad qx qy x y
      ArcTo rx ry rot large sweep (Point x y) -> P.SegArcTo rx ry (rot * pi / 180) large sweep x y
      ClosePath -> P.SegClose

--------------------------------------------------------------------------------
-- Stroking
--------------------------------------------------------------------------------

-- | Polygons covering a stroke of width @w@ along the contours, each wound
-- clockwise on screen so a non-zero fill of all of them is their union.
strokePolygons :: Float -> LineCap -> LineJoin -> Float -> Rings -> Rings
strokePolygons w cap join miterLimit contours = buildRings (strokeWalk w cap join miterLimit (cleanRings False contours))

-- | The outline walk behind 'strokePolygons'. Contours must have no
-- repeated points.
{-# INLINE strokeWalk #-}
strokeWalk :: Float -> LineCap -> LineJoin -> Float -> [(PrimArray Float, Bool)] -> (Float -> Float -> ST s ()) -> (Int -> ST s ()) -> ST s ()
strokeWalk w cap join miterLimit contours point end =
  let hw = w / 2
      emitP (Point x y) = point x y
      -- Twice the signed area of a polygon's corners, positive when they
      -- wind clockwise on screen, as 'P.shoelace' counts.
      turn (Point x0 y0) (Point x1 y1) = x0 * y1 - x1 * y0
      -- A triangle or quad in the order given, or reversed when that winds
      -- anticlockwise on screen.
      triangle a b c = do
        if turn a b + turn b c + turn c a < 0
          then emitP c >> emitP b >> emitP a
          else emitP a >> emitP b >> emitP c
        end 0
      quad a b c d
        | turn a b + turn b c + turn c d + turn d a < 0 = emit4 d c b a
        | otherwise = emit4 a b c d
      emit4 a b c d = emitP a >> emitP b >> emitP c >> emitP d >> end 0
      normal (Point x0 y0) (Point x1 y1) =
        let dx = x1 - x0
            dy = y1 - y0
            len = max 1e-6 (sqrt (dx * dx + dy * dy))
         in (negate dy / len * hw, dx / len * hw)
      segmentQuad a@(Point ax ay) b@(Point bx by) = do
        let (nx, ny) = normal a b
        -- The quad winds clockwise as built, whatever its direction.
        emit4 (Point (ax - nx) (ay - ny)) (Point (bx - nx) (by - ny)) (Point (bx + nx) (by + ny)) (Point (ax + nx) (ay + ny))
      corner prev v@(Point vx vy) next = do
        let (n1x, n1y) = normal prev v
            (n2x, n2y) = normal v next
            bevel = do
              triangle v (Point (vx + n1x) (vy + n1y)) (Point (vx + n2x) (vy + n2y))
              triangle v (Point (vx - n1x) (vy - n1y)) (Point (vx - n2x) (vy - n2y))
        case join of
          JoinRound -> disc v
          JoinBevel -> bevel
          JoinMiter -> case P.miterOffset miterLimit (n1x / hw) (n1y / hw) (n2x / hw) (n2y / hw) of
            Nothing -> bevel
            Just (mx, my) -> do
              quad v (Point (vx + n1x) (vy + n1y)) (Point (vx + mx * hw) (vy + my * hw)) (Point (vx + n2x) (vy + n2y))
              quad v (Point (vx - n1x) (vy - n1y)) (Point (vx - mx * hw) (vy - my * hw)) (Point (vx - n2x) (vy - n2y))
      endCap inner e@(Point ex ey) = case cap of
        CapButt -> pure ()
        CapRound -> disc e
        CapSquare -> do
          -- The outward direction, hw long: the normal turned back.
          let (nx, ny) = normal inner e
              ux = ny
              uy = negate nx
          -- Wound clockwise as built, like a segment's quad.
          emit4 (Point (ex - nx) (ey - ny)) (Point (ex - nx + ux) (ey - ny + uy)) (Point (ex + nx + ux) (ey + ny + uy)) (Point (ex + nx) (ey + ny))
      disc (Point cx cy) = do
        let n = clamp 8 48 (ceiling (hw * 2.5) :: Int)
        forM_ [0 .. n - 1] $ \i ->
          let t = 2 * pi * fromIntegral i / fromIntegral n in point (cx + hw * cos t) (cy + hw * sin t)
        end 0
      square (Point cx cy) =
        emit4 (Point (cx - hw) (cy - hw)) (Point (cx + hw) (cy - hw)) (Point (cx + hw) (cy + hw)) (Point (cx - hw) (cy + hw))
      contour (pts, closed) = do
        let n = sizeofPrimArray pts `div` 2
            pt i = Point (indexPrimArray pts (2 * i)) (indexPrimArray pts (2 * i + 1))
            cornerAt i = corner (pt ((i - 1 + n) `mod` n)) (pt i) (pt ((i + 1) `mod` n))
        case n of
          0 -> pure ()
          1 -> case cap of
            CapButt -> pure ()
            CapRound -> disc (pt 0)
            CapSquare -> square (pt 0)
          _ -> do
            forM_ [0 .. n - 2] $ \i -> segmentQuad (pt i) (pt (i + 1))
            if closed
              then segmentQuad (pt (n - 1)) (pt 0) >> forM_ [0 .. n - 1] cornerAt
              else do
                forM_ [1 .. n - 2] cornerAt
                endCap (pt 1) (pt 0)
                endCap (pt (n - 2)) (pt (n - 1))
   in mapM_ contour contours

--------------------------------------------------------------------------------
-- Rasterizing
--------------------------------------------------------------------------------

-- | Render the document into a @width@ by @height@ RGBA image (rows top to
-- bottom), scaled to fit and centred as SVG's default @xMidYMid meet@ does.
-- @current@ is what @currentColor@ paints with, and an unspecified fill too
-- when the document is one colour ('svgMonochrome'); otherwise an
-- unspecified fill is black, as in SVG.
rasterizeSvg :: Int -> Int -> Color -> Svg -> ByteString
rasterizeSvg width height current (Svg svg) =
  rasterizeView width height (meet (documentViewBox svg) (fromIntegral width) (fromIntegral height)) current svg

-- | 'rasterizeSvg' with the document's own viewport ('svgSize') drawn over
-- @Rect x y w h@, in raster pixels, instead of fitted to the raster. A rect of
-- another shape stretches the document, and whatever falls outside the raster
-- is cut off. The viewBox keeps @xMidYMid meet@ within the viewport.
--
-- > rasterizeSvgIn 64 64 (Rect (-16) 0 96 64) white doc  -- a 3:2 icon covering a square
rasterizeSvgIn :: Int -> Int -> Rect -> Color -> Svg -> ByteString
rasterizeSvgIn width height (Rect x y w h) current (Svg svg) =
  rasterizeView width height (P.Transform (w / dw) 0 0 (h / dh) x y <> meet (documentViewBox svg) dw dh) current svg
  where
    (dw, dh) = documentSize svg

-- | The viewBox fitted into a @w@ by @h@ viewport and centred, as SVG's
-- default @xMidYMid meet@ does.
meet :: Box -> Float -> Float -> P.Transform
meet (Box vx vy vw vh) w h = P.Transform s 0 0 s ((w - vw * s) / 2 - vx * s) ((h - vh * s) / 2 - vy * s)
  where
    s = min (w / vw) (h / vh)

-- | Render with @view@ mapping viewBox units to raster pixels.
rasterizeView :: Int -> Int -> P.Transform -> Color -> Document -> ByteString
rasterizeView width height view current svg
  | width <= 0 || height <= 0 = BS.empty
  -- A degenerate viewBox or rect makes a view of infinities or NaNs.
  | not (P.transformFinite view) = BS.replicate (width * height * 4) 0
  | otherwise =
      case collectPaints of
        [] -> BS.replicate (width * height * 4) 0
        [(rule, rings, col, alpha)] ->
          -- Single-paint SVG (most UI icons): write straight to the output
          -- buffer row by row with no full-frame accumulator or coverage array.
          BSI.unsafeCreate (width * height * 4) $ \out -> do
            fillBytes out 0 (width * height * 4)
            cov <- newZeroedPrimArray width
            let !rByte = colorR col
                !gByte = colorG col
                !bByte = colorB col
                !aScale = alpha * fromIntegral (colorA col) / 255
                writeRow !row = go 0
                  where
                    !rowBase = row * width * 4
                    go !x
                      | x >= width = pure ()
                      | otherwise = do
                          c <- readPrimArray cov x
                          when (c > 0) $ do
                            writePrimArray cov x 0
                            let !sa = min 1 c * aScale
                                !aByte = toByte sa
                            when (aByte > 0) $ do
                              let !off = rowBase + x * 4
                              pokeByteOff out off rByte
                              pokeByteOff out (off + 1) gByte
                              pokeByteOff out (off + 2) bByte
                              pokeByteOff out (off + 3) aByte
                          go (x + 1)
            coverPolygons width height cov rule rings writeRow
        paints ->
          let !image = rasterizeMulti paints
           in BSI.unsafeCreate (width * height * 4) $ \out ->
                let !n = width * height
                    go !i
                      | i >= n = pure ()
                      | otherwise = do
                          let !off = i * 4
                              !al16 = indexPrimArray image (off + 3)
                          if al16 == 0
                            then do
                              pokeByteOff out off (0 :: Word8)
                              pokeByteOff out (off + 1) (0 :: Word8)
                              pokeByteOff out (off + 2) (0 :: Word8)
                              pokeByteOff out (off + 3) (0 :: Word8)
                            else if al16 == 65535
                              then do
                                pokeByteOff out off (toByte16 (indexPrimArray image off))
                                pokeByteOff out (off + 1) (toByte16 (indexPrimArray image (off + 1)))
                                pokeByteOff out (off + 2) (toByte16 (indexPrimArray image (off + 2)))
                                pokeByteOff out (off + 3) (255 :: Word8)
                              else do
                                let !invA = 255.0 / fromIntegral al16 :: Float
                                pokeByteOff out off (clampByte (fromIntegral (indexPrimArray image off) * invA))
                                pokeByteOff out (off + 1) (clampByte (fromIntegral (indexPrimArray image (off + 1)) * invA))
                                pokeByteOff out (off + 2) (clampByte (fromIntegral (indexPrimArray image (off + 2)) * invA))
                                pokeByteOff out (off + 3) (toByte16 al16)
                          go (i + 1)
                 in go 0
  where
    {-# INLINE toByte #-}
    toByte :: Float -> Word8
    toByte x
      | x <= 0 = 0
      | x >= 1 = 255
      | otherwise = fromIntegral (round (x * 255) :: Int)

    {-# INLINE toByte16 #-}
    toByte16 :: Word16 -> Word8
    toByte16 w = fromIntegral ((fromIntegral w * (255 :: Int) + 32767) `quot` 65535)

    {-# INLINE clampByte #-}
    clampByte :: Float -> Word8
    clampByte x
      | x <= 0 = 0
      | x >= 255 = 255
      | otherwise = fromIntegral (round x :: Int)

    paintColor p = case p of
      PaintNone -> Nothing
      PaintCurrent -> Just current
      PaintColor col -> Just (Color (rgbaToWord32 col))

    collectPaints = concatMap shapePaints (toList (documentShapes svg))
      where
        shapePaints (Shape segs m style) =
          let full = view <> toTransform m
              contours = flatten full segs
              scaleOf = P.averageStretch full
              opacity = styleOpacity style
              fill = fromMaybe (if documentMonochrome svg then PaintCurrent else PaintColor black) (styleFill style)
              fillItem = case paintColor fill of
                Just col -> [(styleFillRule style, contours, col, opacity * styleFillOpacity style)]
                Nothing -> []
              strokeItem = case paintColor (fromMaybe PaintNone (styleStroke style)) of
                Just col
                  | styleStrokeWidth style > 0 ->
                      let wanted = styleStrokeWidth style * scaleOf
                          w = max 1 wanted
                          polys = strokePolygons w (styleCap style) (styleJoin style) (styleMiterLimit style) contours
                       in [(NonZero, polys, col, opacity * styleStrokeOpacity style * min 1 (wanted / w))]
                _ -> []
           in fillItem ++ strokeItem

    -- Multi-paint SVG: 16-bit premultiplied RGBA accumulator (half the size
    -- of Float with 256x 8-bit precision) plus a single scanline coverage row.
    rasterizeMulti :: [(FillRule, Rings, Color, Float)] -> PrimArray Word16
    rasterizeMulti paints = runST $ do
      acc <- newZeroedPrimArray (width * height * 4)
      cov <- newZeroedPrimArray width
      forM_ paints $ \(rule, rings, col, alpha) ->
        coverPolygons width height cov rule rings (compositeRow16 width acc cov col alpha)
      unsafeFreezePrimArray acc

-- | Scan-convert @rings@ row by row into the single-row buffer @cov@ (of
-- length @width@, initially zero), invoking @onRow r@ after each covered row
-- @r@. @onRow@ must zero every non-zero entry it reads from @cov@.
{-# SPECIALIZE coverPolygons :: Int -> Int -> MutablePrimArray s Float -> FillRule -> Rings -> (Int -> ST s ()) -> ST s () #-}
{-# SPECIALIZE coverPolygons :: Int -> Int -> MutablePrimArray (PrimState IO) Float -> FillRule -> Rings -> (Int -> IO ()) -> IO () #-}
coverPolygons :: PrimMonad m => Int -> Int -> MutablePrimArray (PrimState m) Float -> FillRule -> Rings -> (Int -> m ()) -> m ()
coverPolygons width height cov rule rings@(Rings pts starts _) onRow = do
  let {-# INLINE forEdges #-}
      forEdges visit =
        forM_ [0 .. ringCount rings - 1] $ \r -> do
          let from = indexPrimArray starts r
              to = indexPrimArray starts (r + 1)
          when (to - from >= 3) $
            forM_ [from .. to - 1] $ \k -> do
              let k' = if k + 1 == to then from else k + 1
                  ax = indexPrimArray pts (2 * k)
                  ay = indexPrimArray pts (2 * k + 1)
                  bx = indexPrimArray pts (2 * k')
                  by = indexPrimArray pts (2 * k' + 1)
                  up = ay < by
                  x0 = if up then ax else bx
                  y0 = if up then ay else by
                  x1 = if up then bx else ax
                  y1 = if up then by else ay
              -- The row is tested before 'floor': an edge from far above (a
              -- huge or infinite coordinate) floors to garbage, not a row.
              when (ay /= by && y1 > 0 && y0 < fromIntegral height) $
                visit (if y0 <= 0 then 0 else floor y0) y0 y1 x0 ((x1 - x0) / (y1 - y0)) (if up then 1 else -1)
  (rowStart, cursor) <- bucketOffsets height (\emit -> forEdges (\row _ _ _ _ _ -> emit row))
  totalEdges <- readPrimArray rowStart height
  when (totalEdges > 0) $ do
    edges <- newPrimArray (totalEdges * 4)
    windings <- newPrimArray totalEdges
    forEdges $ \row y0 y1 x0 slope dir -> do
      e <- readPrimArray cursor row
      writePrimArray cursor row (e + 1)
      writePrimArray edges (e * 4) y0
      writePrimArray edges (e * 4 + 1) y1
      writePrimArray edges (e * 4 + 2) x0
      writePrimArray edges (e * 4 + 3) slope
      writePrimArray windings e (dir :: Int)
    let firstRow !r
          | r >= height = pure height
          | otherwise = readPrimArray rowStart (r + 1) >>= \e -> if e > 0 then pure r else firstRow (r + 1)
    top <- firstRow 0
    active <- newPrimArray totalEdges
    crossX <- newPrimArray totalEdges
    crossDir <- newPrimArray totalEdges
    let samples = 5 :: Int
        weight = 1 / fromIntegral samples :: Float
        inside :: Int -> Bool
        inside = P.fillsWinding $ case rule of
          NonZero -> P.NonZero
          EvenOdd -> P.EvenOdd
        add !i !v = readPrimArray cov i >>= \c -> writePrimArray cov i (c + v)
        spanCover !xa0 !xb0 = do
          let !xa = clamp 0 (fromIntegral width) xa0
              !xb = clamp 0 (fromIntegral width) xb0
          when (xb > xa) $ do
            let !ia = floor xa :: Int
                !ib = min (width - 1) (floor xb)
            if ia == ib
              then add ia ((xb - xa) * weight)
              else do
                add ia ((fromIntegral (ia + 1) - xa) * weight)
                let fillSpan !i
                      | i >= ib = pure ()
                      | otherwise = add i weight >> fillSpan (i + 1)
                fillSpan (ia + 1)
                when (ib < width) $ add ib ((xb - fromIntegral ib) * weight)
        admit !e !stop !n
          | e >= stop = pure n
          | otherwise = writePrimArray active n e >> admit (e + 1) stop (n + 1)
        insertCrossing !j !x !d
          | j > 0 = do
              xj <- readPrimArray crossX (j - 1)
              if xj > x
                then do
                  writePrimArray crossX j xj
                  readPrimArray crossDir (j - 1) >>= writePrimArray crossDir j
                  insertCrossing (j - 1) x d
                else writePrimArray crossX j x >> writePrimArray crossDir j (d :: Int)
          | otherwise = writePrimArray crossX 0 x >> writePrimArray crossDir 0 d
        walk !crossings !k !w
          | k + 1 >= crossings = pure ()
          | otherwise = do
              d <- readPrimArray crossDir k
              let !w' = w + d
              when (inside w') $ do
                xa <- readPrimArray crossX k
                xb <- readPrimArray crossX (k + 1)
                spanCover xa xb
              walk crossings (k + 1) w'
        sample !r !si !n !a !kept !crossings
          | a < n = do
              e <- readPrimArray active a
              let !sy = fromIntegral r + (fromIntegral si + 0.5) * weight
              y1 <- readPrimArray edges (e * 4 + 1)
              if y1 <= sy
                then sample r si n (a + 1) kept crossings
                else do
                  writePrimArray active kept e
                  y0 <- readPrimArray edges (e * 4)
                  if sy < y0
                    then sample r si n (a + 1) (kept + 1) crossings
                    else do
                      x0 <- readPrimArray edges (e * 4 + 2)
                      slope <- readPrimArray edges (e * 4 + 3)
                      d <- readPrimArray windings e
                      insertCrossing crossings (x0 + (sy - y0) * slope) d
                      sample r si n (a + 1) (kept + 1) (crossings + 1)
          | otherwise = do
              walk crossings 0 0
              if si + 1 < samples
                then sample r (si + 1) kept 0 0 0
                else onRow r >> rows (r + 1) kept
        rows !r !n
          | r >= height = pure ()
          | otherwise = do
              from <- readPrimArray rowStart r
              to <- readPrimArray rowStart (r + 1)
              if n == 0 && from >= totalEdges
                then pure ()
                else admit from to n >>= \n' -> sample r 0 n' 0 0 0
    rows top 0

-- | Composite one row @r@ from the single-row coverage buffer @cov@ into the
-- 16-bit premultiplied RGBA accumulator @acc@, zeroing @cov@ as it goes.
compositeRow16 :: Int -> MutablePrimArray s Word16 -> MutablePrimArray s Float -> Color -> Float -> Int -> ST s ()
compositeRow16 width acc cov col alpha !row = go 0
  where
    !rowBase = row * width * 4
    !sr = fromIntegral (colorR col) * (65535.0 / 255.0) :: Float
    !sg = fromIntegral (colorG col) * (65535.0 / 255.0) :: Float
    !sb = fromIntegral (colorB col) * (65535.0 / 255.0) :: Float
    !aScale = alpha * fromIntegral (colorA col) / 255.0 :: Float
    {-# INLINE toW16 #-}
    toW16 :: Float -> Word16
    toW16 x
      | x <= 0 = 0
      | x >= 65535 = 65535
      | otherwise = fromIntegral (round x :: Int)
    go !x
      | x >= width = pure ()
      | otherwise = do
          c <- readPrimArray cov x
          when (c > 0) $ do
            writePrimArray cov x 0
            let !sa = min 1 c * aScale
                !invSa = 1 - sa
                !off = rowBase + x * 4
            dr <- readPrimArray acc off
            dg <- readPrimArray acc (off + 1)
            db <- readPrimArray acc (off + 2)
            da <- readPrimArray acc (off + 3)
            writePrimArray acc off (toW16 (sr * sa + fromIntegral dr * invSa))
            writePrimArray acc (off + 1) (toW16 (sg * sa + fromIntegral dg * invSa))
            writePrimArray acc (off + 2) (toW16 (sb * sa + fromIntegral db * invSa))
            writePrimArray acc (off + 3) (toW16 (65535.0 * sa + fromIntegral da * invSa))
          go (x + 1)
