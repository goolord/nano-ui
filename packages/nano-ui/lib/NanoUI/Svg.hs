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
import Data.Primitive.PrimArray (MutablePrimArray, PrimArray, copyMutablePrimArray, indexPrimArray, newPrimArray, readPrimArray, setPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
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
  , transformPoint
  )
import NanoUI.Internal.Types (Color, clamp, colorA, colorB, colorFromWord32, colorG, colorR)

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
-- Rings
--------------------------------------------------------------------------------

-- | Point lists in flat arrays: ring @i@ is points @starts[i]@ up to
-- @starts[i + 1]@, stored x then y, with a number the builder tagged it with.
data Rings = Rings !(PrimArray Float) !(PrimArray Int) !(PrimArray Int)

ringCount :: Rings -> Int
ringCount (Rings _ starts _) = sizeofPrimArray starts - 1

-- | Rings from a walk that calls @point x y@ for each point and @end tag@
-- after each ring's last. The walk runs twice, to size the arrays and then
-- to fill them, so it must visit the same points both times.
{-# INLINE buildRings #-}
buildRings :: (forall s. (Float -> Float -> ST s ()) -> (Int -> ST s ()) -> ST s ()) -> Rings
buildRings walk = runST $ do
  counts <- newPrimArray 2
  setPrimArray counts 0 2 (0 :: Int)
  let bump k = readPrimArray counts k >>= writePrimArray counts k . (+ 1)
  walk (\_ _ -> bump 0) (\_ -> bump 1)
  nPoints <- readPrimArray counts 0
  nRings <- readPrimArray counts 1
  points <- newPrimArray (2 * nPoints)
  starts <- newPrimArray (nRings + 1)
  tags <- newPrimArray nRings
  writePrimArray starts 0 0
  setPrimArray counts 0 2 0
  walk
    ( \x y -> do
        k <- readPrimArray counts 0
        writePrimArray points (2 * k) x
        writePrimArray points (2 * k + 1) y
        writePrimArray counts 0 (k + 1)
    )
    ( \tag -> do
        r <- readPrimArray counts 1
        readPrimArray counts 0 >>= writePrimArray starts (r + 1)
        writePrimArray tags r tag
        writePrimArray counts 1 (r + 1)
    )
  Rings <$> unsafeFreezePrimArray points <*> unsafeFreezePrimArray starts <*> unsafeFreezePrimArray tags

--------------------------------------------------------------------------------
-- Flattening
--------------------------------------------------------------------------------

-- | Flatten segments through a transform into contours in device pixels,
-- each tagged 1 when closed. Curves are split until they are within a
-- quarter pixel of their chords.
flatten :: Matrix -> SmallArray Segment -> Rings
flatten m segs = buildRings (flattenWalk m segs)

{-# INLINE flattenWalk #-}
flattenWalk :: Matrix -> SmallArray Segment -> (Float -> Float -> ST s ()) -> (Int -> ST s ()) -> ST s ()
flattenWalk m segs point end =
  let count = sizeofSmallArray segs
      emit p = let Point x y = transformPoint m p in point x y
      finish n closed = when (n > 0) (end (if closed then 1 else 0))
      -- A segment with no subpath open starts one at the current point.
      begin n started cur = if started || n > 0 then pure n else emit cur >> pure (1 :: Int)
      go !i !n !started cur start
        | i >= count = finish n False
        | otherwise = case indexSmallArray segs i of
            MoveTo p -> finish n False >> emit p >> go (i + 1) 1 True p p
            LineTo p -> do
              n1 <- begin n started cur
              emit p
              go (i + 1) (n1 + 1) True p start
            CubicTo c1 c2 p -> do
              n1 <- begin n started cur
              k <- cubicPoints point (transformPoint m cur) (transformPoint m c1) (transformPoint m c2) (transformPoint m p)
              go (i + 1) (n1 + k) True p start
            QuadTo c1 p -> do
              n1 <- begin n started cur
              let Point x0 y0 = cur
                  Point x1 y1 = c1
                  Point x2 y2 = p
                  q1 = Point (x0 + 2 / 3 * (x1 - x0)) (y0 + 2 / 3 * (y1 - y0))
                  q2 = Point (x2 + 2 / 3 * (x1 - x2)) (y2 + 2 / 3 * (y1 - y2))
              k <- cubicPoints point (transformPoint m cur) (transformPoint m q1) (transformPoint m q2) (transformPoint m p)
              go (i + 1) (n1 + k) True p start
            ArcTo rx ry rot large sweep p -> do
              n1 <- begin n started cur
              k <- arcPoints emit cur rx ry rot large sweep p
              go (i + 1) (n1 + k) True p start
            ClosePath -> finish n True >> go (i + 1) 0 False start start
   in go 0 0 False (Point 0 0) (Point 0 0)

-- | The points after the start of a cubic, subdividing by flatness, and how
-- many there were.
{-# INLINE cubicPoints #-}
cubicPoints :: (Float -> Float -> ST s ()) -> Point -> Point -> Point -> Point -> ST s Int
cubicPoints point = go (0 :: Int)
  where
    go depth a b c d
      | depth >= 12 || flat a b c d = let Point x y = d in point x y >> pure 1
      | otherwise = do
          let ab = mid a b
              bc = mid b c
              cd = mid c d
              abc = mid ab bc
              bcd = mid bc cd
              abcd = mid abc bcd
          k1 <- go (depth + 1) a ab abc abcd
          k2 <- go (depth + 1) abcd bcd cd d
          pure (k1 + k2)
    mid (Point x0 y0) (Point x1 y1) = Point ((x0 + x1) / 2) ((y0 + y1) / 2)
    flat (Point x0 y0) (Point x1 y1) (Point x2 y2) (Point x3 y3) =
      let ux = 3 * x1 - 2 * x0 - x3
          uy = 3 * y1 - 2 * y0 - y3
          vx = 3 * x2 - 2 * x3 - x0
          vy = 3 * y2 - 2 * y3 - y0
       in max (ux * ux) (vx * vx) + max (uy * uy) (vy * vy) <= 16 * 0.25 * 0.25

-- | The points after the start of an SVG arc, by the endpoint-to-centre
-- conversion in the SVG specification, in user space, and how many there
-- were.
{-# INLINE arcPoints #-}
arcPoints :: (Point -> ST s ()) -> Point -> Float -> Float -> Float -> Bool -> Bool -> Point -> ST s Int
arcPoints emit (Point x1 y1) rx0 ry0 rotDeg large sweep (Point x2 y2)
  | rx0 == 0 || ry0 == 0 || (x1 == x2 && y1 == y2) = emit (Point x2 y2) >> pure 1
  | otherwise = do
      forM_ [1 .. steps - 1] $ \i -> emit (pointAt i)
      emit (Point x2 y2)
      pure steps
  where
    phi = rotDeg * pi / 180
    cosP = cos phi
    sinP = sin phi
    dx = (x1 - x2) / 2
    dy = (y1 - y2) / 2
    x1' = cosP * dx + sinP * dy
    y1' = negate sinP * dx + cosP * dy
    lambda = (x1' * x1') / (rx0 * rx0) + (y1' * y1') / (ry0 * ry0)
    scale = if lambda > 1 then sqrt lambda else 1
    rx = abs rx0 * scale
    ry = abs ry0 * scale
    num = rx * rx * ry * ry - rx * rx * y1' * y1' - ry * ry * x1' * x1'
    den = rx * rx * y1' * y1' + ry * ry * x1' * x1'
    coef = (if large == sweep then -1 else 1) * sqrt (max 0 (num / den))
    cx' = coef * rx * y1' / ry
    cy' = coef * negate (ry * x1' / rx)
    cx = cosP * cx' - sinP * cy' + (x1 + x2) / 2
    cy = sinP * cx' + cosP * cy' + (y1 + y2) / 2
    angle ux uy vx vy = atan2 (ux * vy - uy * vx) (ux * vx + uy * vy)
    theta1 = angle 1 0 ((x1' - cx') / rx) ((y1' - cy') / ry)
    dtheta0 = angle ((x1' - cx') / rx) ((y1' - cy') / ry) ((negate x1' - cx') / rx) ((negate y1' - cy') / ry)
    dtheta
      | not sweep && dtheta0 > 0 = dtheta0 - 2 * pi
      | sweep && dtheta0 < 0 = dtheta0 + 2 * pi
      | otherwise = dtheta0
    steps = max 4 (ceiling (abs dtheta / (pi / 16)) :: Int)
    pointAt i =
      let t = theta1 + dtheta * fromIntegral i / fromIntegral steps
          ex = rx * cos t
          ey = ry * sin t
       in Point (cosP * ex - sinP * ey + cx) (sinP * ex + cosP * ey + cy)

--------------------------------------------------------------------------------
-- Stroking
--------------------------------------------------------------------------------

-- | Polygons covering a stroke of width @w@ along the contours, each wound
-- counter-clockwise so a non-zero fill of all of them is their union.
strokePolygons :: Float -> LineCap -> LineJoin -> Float -> Rings -> Rings
strokePolygons w cap join miterLimit contours = buildRings (strokeWalk w cap join miterLimit contours)

{-# INLINE strokeWalk #-}
strokeWalk :: Float -> LineCap -> LineJoin -> Float -> Rings -> (Float -> Float -> ST s ()) -> (Int -> ST s ()) -> ST s ()
strokeWalk w cap join miterLimit contours@(Rings cpts cstarts ctags) point end =
  let hw = w / 2
      at k = Point (indexPrimArray cpts (2 * k)) (indexPrimArray cpts (2 * k + 1))
      close (Point x0 y0) (Point x1 y1) = abs (x0 - x1) < 1e-4 && abs (y0 - y1) < 1e-4
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
          JoinMiter -> do
            let mx = n1x + n2x
                my = n1y + n2y
                mlen2 = mx * mx + my * my
                -- The miter point sits along the bisector at hw / cos(half angle).
                scale = if mlen2 < 1e-9 then 0 else 2 * hw * hw / mlen2
                ratio = if mlen2 < 1e-9 then 1 / 0 else sqrt (scale * scale * mlen2) / hw
            if ratio > miterLimit
              then bevel
              else do
                quad v (Point (vx + n1x) (vy + n1y)) (Point (vx + mx * scale) (vy + my * scale)) (Point (vx + n2x) (vy + n2y))
                quad v (Point (vx - n1x) (vy - n1y)) (Point (vx - mx * scale) (vy - my * scale)) (Point (vx - n2x) (vy - n2y))
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
      contour r = do
        let from = indexPrimArray cstarts r
            to = indexPrimArray cstarts (r + 1)
            closed = indexPrimArray ctags r /= 0
        -- The contour's points, dropping any that repeat the one before.
        kept <- newPrimArray (to - from)
        let dedupe !k !n
              | k >= to = pure n
              | n > 0 = do
                  prevK <- readPrimArray kept (n - 1)
                  if close (at prevK) (at k)
                    then dedupe (k + 1) n
                    else writePrimArray kept n k >> dedupe (k + 1) (n + 1)
              | otherwise = writePrimArray kept 0 k >> dedupe (k + 1) 1
        n0 <- dedupe from 0
        firstK <- readPrimArray kept 0
        lastK <- readPrimArray kept (max 0 (n0 - 1))
        -- A closed contour that returns to its start ends on that point.
        let n = if closed && n0 >= 2 && close (at firstK) (at lastK) then n0 - 1 else n0
            pt i = at <$> readPrimArray kept i
        case n of
          0 -> pure ()
          1 -> do
            p <- pt 0
            case cap of
              CapButt -> pure ()
              CapRound -> disc p
              CapSquare -> square p
          _ -> do
            forM_ [0 .. n - 2] $ \i -> do
              a <- pt i
              b <- pt (i + 1)
              segmentQuad a b
            when closed $ do
              a <- pt (n - 1)
              b <- pt 0
              segmentQuad a b
            let cornerAt i = do
                  prev <- pt ((i - 1 + n) `mod` n)
                  v <- pt i
                  next <- pt ((i + 1) `mod` n)
                  corner prev v next
            if closed
              then forM_ [0 .. n - 1] cornerAt
              else forM_ [1 .. n - 2] cornerAt
            unless closed $ do
              first <- pt 0
              second <- pt 1
              beforeLast <- pt (n - 2)
              final <- pt (n - 1)
              endCap second first
              endCap beforeLast final
   in forM_ [0 .. ringCount contours - 1] contour

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
              PaintColor col -> Just (colorFromWord32 (rgbaToWord32 col))
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
      inside w = case rule of
        NonZero -> w /= 0
        EvenOdd -> odd w
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
