-- | An anti-aliased rasterizer for the SVG icons that @nano-svg@ parses.
--
-- @nano-svg@ reads the document into shapes with absolute segments, a
-- transform and a resolved style; this module flattens those to contours,
-- strokes them and scan-converts the result into an RGBA image. See
-- "Graphics.NanoSvg" for what the parser supports; gradients, patterns,
-- text, masks, clipping and filters are not among them.
module NanoUI.Svg
  ( Svg
  , svgSize
  , svgKey
  , svgMonochrome
  , parseSvg
  , rasterizeSvg
  ) where

import Control.Monad (forM_, unless, when)
import Control.Monad.ST (ST, runST)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray, copyMutablePrimArray, indexPrimArray, newPrimArray, readPrimArray, setPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray)
import Data.Word (Word8)
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
  , averageScale
  , black
  , multiply
  , parseSvg
  )
import NanoUI.Internal.Path (Rings (..), buildRings, cleanRings, ringCount)
import NanoUI.Internal.Path qualified as P
import NanoUI.Internal.Types (Color (..), clamp, colorA, colorB, colorG, colorR)

-- | A parsed SVG document, as @nano-svg@ returns it.
type Svg = Document

-- | The document's own width and height, from its @width@ and @height@ or
-- else its @viewBox@.
svgSize :: Svg -> (Float, Float)
svgSize = documentSize

-- | A hash of the source, for caching rasters.
svgKey :: Svg -> Int
svgKey = documentKey

-- | Every paint is @currentColor@ or unspecified, so the drawing is one
-- colour and can be tinted.
svgMonochrome :: Svg -> Bool
svgMonochrome = documentMonochrome

--------------------------------------------------------------------------------
-- Flattening
--------------------------------------------------------------------------------

-- | Flatten segments through a transform into contours in device pixels,
-- each tagged 1 when closed. Curves are split until they are within a
-- quarter pixel of their chords. Arcs use steps of at most pi/16, and at
-- least four steps.
flatten :: Matrix -> SmallArray Segment -> Rings
flatten (Matrix a b c d e f) segs =
  P.flattenPath 0.25 arcSteps (P.Transform a b c d e f) (P.Path (map segment (toList segs)))
  where
    arcSteps _ sweep = max 4 (ceiling (abs sweep / (pi / 16)))
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
-- counter-clockwise so a non-zero fill of all of them is their union.
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
      -- wind counter-clockwise.
      turn (Point x0 y0) (Point x1 y1) = x0 * y1 - x1 * y0
      -- A triangle or quad in the order given, or reversed when that winds
      -- clockwise.
      triangle a b c = do
        if turn a b + turn b c + turn c a < 0
          then emitP c >> emitP b >> emitP a
          else emitP a >> emitP b >> emitP c
        end 0
      quad a b c d
        | turn a b + turn b c + turn c d + turn d a < 0 = emit4 d c b a
        | otherwise = emit4 a b c d
      -- A quad in the order given.
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
            when closed $ segmentQuad (pt (n - 1)) (pt 0)
            if closed
              then forM_ [0 .. n - 1] cornerAt
              else forM_ [1 .. n - 2] cornerAt
            unless closed $ do
              endCap (pt 1) (pt 0)
              endCap (pt (n - 2)) (pt (n - 1))
   in mapM_ contour contours

--------------------------------------------------------------------------------
-- Rasterizing
--------------------------------------------------------------------------------

-- | Render the document into a @width@ by @height@ RGBA image (rows top to
-- bottom), scaled to fit and centred as SVG's default @xMidYMid meet@ does.
-- @current@ is what @currentColor@, and an unspecified fill, paint with.
rasterizeSvg :: Int -> Int -> Color -> Svg -> ByteString
rasterizeSvg width height current svg
  | width <= 0 || height <= 0 = BS.empty
  | otherwise = BSI.unsafeCreate (width * height * 4) $ \out ->
      forM_ [0 .. width * height - 1] $ \i -> do
        let al = indexPrimArray image (i * 4 + 3)
            byte x = fromIntegral (clamp 0 255 (round (x * 255) :: Int)) :: Word8
            unpremul k = pokeByteOff out (i * 4 + k) (if al <= 0 then 0 else byte (indexPrimArray image (i * 4 + k) / al))
        unpremul 0
        unpremul 1
        unpremul 2
        pokeByteOff out (i * 4 + 3) (byte al)
  where
    image = runST $ do
      -- Premultiplied RGBA in [0, 1].
      acc <- newPrimArray (width * height * 4)
      setPrimArray acc 0 (width * height * 4) (0 :: Float)
      cov <- newPrimArray (width * height)
      let Box vx vy vw vh = documentViewBox svg
          s = min (fromIntegral width / vw) (fromIntegral height / vh)
          tx = (fromIntegral width - vw * s) / 2 - vx * s
          ty = (fromIntegral height - vh * s) / 2 - vy * s
          view = Matrix s 0 0 s tx ty
      forM_ (documentShapes svg) $ \(Shape segs m style) -> do
        let full = view `multiply` m
            contours = flatten full segs
            scaleOf = averageScale full
            opacity = styleOpacity style
            paintColor p = case p of
              PaintNone -> Nothing
              PaintCurrent -> Just current
              PaintColor col -> Just (Color (rgbaToWord32 col))
            -- An unspecified fill paints black, or the current colour in a
            -- monochrome document, so an icon without paints tints.
            fill = fromMaybe (if documentMonochrome svg then PaintCurrent else PaintColor black) (styleFill style)
        forM_ (paintColor fill) $ \col -> do
          coverPolygons width height cov (styleFillRule style) contours
          composite width height acc cov col (opacity * styleFillOpacity style)
        forM_ (paintColor (fromMaybe PaintNone (styleStroke style))) $ \col ->
          when (styleStrokeWidth style > 0) $ do
            let wanted = styleStrokeWidth style * scaleOf
                w = max 1 wanted
                polys = strokePolygons w (styleCap style) (styleJoin style) (styleMiterLimit style) contours
            coverPolygons width height cov NonZero polys
            -- A hairline thinner than a pixel keeps its weight as opacity.
            composite width height acc cov col (opacity * styleStrokeOpacity style * min 1 (wanted / w))
      unsafeFreezePrimArray acc

-- | Coverage of the rings with at least three points in @cov@ (cleared
-- first): five sample rows a pixel, each span's coverage split exactly
-- across the pixels it crosses.
--
-- Edges are counted per pixel row they start in and then written in row
-- order, so the sweep down the rows admits each row's edges as a block,
-- drops an edge past its bottom, and insertion sorts a row's few crossings
-- in a scratch array.
coverPolygons :: Int -> Int -> MutablePrimArray s Float -> FillRule -> Rings -> ST s ()
coverPolygons width height cov rule rings@(Rings pts starts _) = do
  setPrimArray cov 0 (width * height) 0
  let capacity = sizeofPrimArray pts `div` 2
      -- Each edge that can cover a row: its first row, top, bottom, x at the
      -- top, slope and winding.
      {-# INLINE forEdges #-}
      forEdges :: (Int -> Float -> Float -> Float -> Float -> Int -> ST s ()) -> ST s ()
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
              when (ay /= by && y1 > 0 && y0 < fromIntegral height) $
                visit (max 0 (floor y0)) y0 y1 x0 ((x1 - x0) / (y1 - y0)) (if up then 1 else -1)
  -- Where each row's edges begin: counts, then running totals.
  rowStart <- newPrimArray (height + 1)
  setPrimArray rowStart 0 (height + 1) (0 :: Int)
  forEdges $ \row _ _ _ _ _ -> readPrimArray rowStart (row + 1) >>= writePrimArray rowStart (row + 1) . (+ 1)
  forM_ [1 .. height] $ \row -> do
    before <- readPrimArray rowStart (row - 1)
    readPrimArray rowStart row >>= writePrimArray rowStart row . (+ before)
  -- Four numbers an edge, in row order: top, bottom, x at the top, slope;
  -- windings apart.
  edges <- newPrimArray (capacity * 4)
  windings <- newPrimArray capacity
  cursor <- newPrimArray height
  copyMutablePrimArray cursor 0 rowStart 0 height
  forEdges $ \row y0 y1 x0 slope dir -> do
    e <- readPrimArray cursor row
    writePrimArray cursor row (e + 1)
    writePrimArray edges (e * 4) y0
    writePrimArray edges (e * 4 + 1) y1
    writePrimArray edges (e * 4 + 2) x0
    writePrimArray edges (e * 4 + 3) slope
    writePrimArray windings e (dir :: Int)
  totalEdges <- readPrimArray rowStart height
  active <- newPrimArray capacity
  crossX <- newPrimArray capacity
  crossDir <- newPrimArray capacity
  let samples = 5 :: Int
      weight = 1 / fromIntegral samples :: Float
      inside :: Int -> Bool
      inside = P.fillsWinding $ case rule of
        NonZero -> P.NonZero
        EvenOdd -> P.EvenOdd
      add i v = readPrimArray cov i >>= \c -> writePrimArray cov i (c + v)
      spanCover base xa0 xb0 = do
        let xa = clamp 0 (fromIntegral width) xa0
            xb = clamp 0 (fromIntegral width) xb0
        when (xb > xa) $ do
          let ia = floor xa :: Int
              ib = min (width - 1) (floor xb)
          if ia == ib
            then add (base + ia) ((xb - xa) * weight)
            else do
              add (base + ia) ((fromIntegral (ia + 1) - xa) * weight)
              forM_ [ia + 1 .. ib - 1] $ \i -> add (base + i) weight
              when (ib < width) $ add (base + ib) ((xb - fromIntegral ib) * weight)
      -- The row's edges join the active list.
      admit !e !stop !n
        | e >= stop = pure n
        | otherwise = writePrimArray active n e >> admit (e + 1) stop (n + 1)
      -- A crossing goes where it sorts among the @j@ before it.
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
      walk !base !crossings !k !w
        | k + 1 >= crossings = pure ()
        | otherwise = do
            d <- readPrimArray crossDir k
            let w' = w + d
            when (inside w') $ do
              xa <- readPrimArray crossX k
              xb <- readPrimArray crossX (k + 1)
              spanCover base xa xb
            walk base crossings (k + 1) w'
      -- One sample row: drop edges above it, collect the crossings of the
      -- rest, then cover the spans inside the fill.
      sample !r !si !n !a !kept !crossings
        | a < n = do
            e <- readPrimArray active a
            let sy = fromIntegral r + (fromIntegral si + 0.5) * weight
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
            walk (r * width) crossings 0 0
            if si + 1 < samples
              then sample r (si + 1) kept 0 0 0
              else rows (r + 1) kept
      rows !r !n
        | r >= height = pure ()
        | otherwise = do
            from <- readPrimArray rowStart r
            to <- readPrimArray rowStart (r + 1)
            -- Past the last edge's bottom nothing is left to cover.
            if n == 0 && from >= totalEdges
              then pure ()
              else admit from to n >>= \n' -> sample r 0 n' 0 0 0
  rows 0 0

-- | Draw @col@ at @alpha@ through the coverage over the accumulated image.
composite :: Int -> Int -> MutablePrimArray s Float -> MutablePrimArray s Float -> Color -> Float -> ST s ()
composite width height acc cov col alpha =
  forM_ [0 .. width * height - 1] $ \i -> do
    c <- readPrimArray cov i
    when (c > 0) $ do
      let sa = min 1 c * alpha * fromIntegral (colorA col) / 255
          blend k src = do
            dst <- readPrimArray acc (i * 4 + k)
            writePrimArray acc (i * 4 + k) (src * sa + dst * (1 - sa))
      blend 0 (fromIntegral (colorR col) / 255)
      blend 1 (fromIntegral (colorG col) / 255)
      blend 2 (fromIntegral (colorB col) / 255)
      dstA <- readPrimArray acc (i * 4 + 3)
      writePrimArray acc (i * 4 + 3) (sa + dstA * (1 - sa))
