-- | The reference software rasteriser the tests check RGFW frames with: a
-- BGRA pixel buffer, every primitive of the core's 'DrawData' filled flat,
-- and the collected text spans stamped with the Cozette bitmap font, in
-- 'paintInLayerOrder'.
--
-- The frame must come from 'NanoUI.Rgfw.Internal.Context.newRgfwContext':
-- square geometry leaves only flat quads and triangles in the buffer, and
-- external text leaves no text quads, so glyphs come from the spans alone.
module NanoUI.Rgfw.Render
  ( RgfwSurface (..)
  , withSurface
  , packColor
  , renderArena
  ) where

import Control.Exception (bracket)
import Control.Monad (forM_, when)
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Text qualified as T
import Data.Word (Word32)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff, peekElemOff, pokeElemOff)
import NanoUI (Color (..), Rect (..), roundHalfUp)
import NanoUI.Rgfw.Internal.Context (TextSpan, paintInLayerOrder)
import NanoUI.Rgfw.Internal.Font.Cozette (CozetteFont, foldPenPositions, renderGlyphScaledToBuffer)
import NanoUI.Rgfw.Internal.Gl (physClip, toPhysRect)
import NanoUI.Testing (DrawCmd (..), DrawData (..), forDrawCmdsInLayer_, indexSize, vertexSize)

data RgfwSurface = RgfwSurface
  { sWidth :: !Int
  , sHeight :: !Int
  , sBuffer :: !(Ptr Word32)
  }

type RGBA = (Float, Float, Float, Float)

-- | A @w@ by @h@ surface cleared to @bg@, freed after @k@.
withSurface :: Int -> Int -> Word32 -> (RgfwSurface -> IO a) -> IO a
withSurface w h bg k = bracket (mallocBytes (w * h * 4)) free $ \buf -> do
  forM_ [0 .. w * h - 1] $ \i -> pokeElemOff buf i bg
  k (RgfwSurface w h buf)

-- | A colour as a BGRA surface word (ARGB from the high byte down).
packColor :: Color -> Word32
packColor (Color w) = (w `shiftL` 24) .|. (w `shiftR` 8)

-- | Rasterise a frame at @scale@ physical pixels per layout unit, with its
-- base and overlay text spans.
renderArena :: RgfwSurface -> CozetteFont -> Float -> DrawData -> [TextSpan] -> [TextSpan] -> IO ()
renderArena surf font scale dd base overlay =
  paintInLayerOrder (\layer -> forDrawCmdsInLayer_ layer dd (fillCmd surf scale dd)) (stamp base) (stamp overlay)
  where
    stamp = mapM_ $ \(Rect x y _ _, txt, fg, _, clip) ->
      case physClip scale (sWidth surf) (sHeight surf) clip of
        Just (cx0, cy0, cx1, cy1) | not (T.null txt) ->
          let glyph () px py =
                renderGlyphScaledToBuffer (sBuffer surf) (sWidth surf) cx0 cy0 cx1 cy1 scale px py (packColor fg) font
           in foldPenPositions font scale x y () glyph txt
        _ -> pure ()

-- | Fill a command's primitives: quads (six indices @a b c a c d@) and
-- triangles (three), each in the average of its corner colours.
fillCmd :: RgfwSurface -> Float -> DrawData -> DrawCmd -> IO ()
fillCmd surf scale dd cmd = case physClip scale (sWidth surf) (sHeight surf) cmdClip of
  Nothing -> pure ()
  Just clip -> withForeignPtr (drawVertices dd) $ \vp -> withForeignPtr (drawIndices dd) $ \ip -> do
    let start = fromIntegral (cmdIndexOffset cmd)
        index i = fromIntegral <$> (peekByteOff ip (i * indexSize) :: IO Word32)
        vertex i = do
          [x, y, r, g, b, a] <- mapM (\o -> peekByteOff vp (i * vertexSize + o)) [0, 4 .. 20]
          pure ((x, y), (r, g, b, a))
        walk (a : b : c : a' : c' : d : rest) | a' == a && c' == c = mapM vertex [a, b, c, d] >>= quad >> walk rest
        walk (a : b : c : rest) = mapM vertex [a, b, c] >>= convex >> walk rest
        walk _ = pure ()
        quad vs
          | axisAligned = case toPhysRect scale x0 y0 (x1 - x0) (y1 - y0) of
              (px, py, pw, ph) ->
                let (fx0, fy0, cx1, cy1) = clip
                 in forM_ [max fy0 py .. min cy1 (py + ph) - 1] $ \row ->
                      forM_ [max fx0 px .. min cx1 (px + pw) - 1] $ \col -> poke col row (average vs)
          | otherwise = convex vs
          where
            (xs, ys) = unzip (map fst vs)
            (x0, x1, y0, y1) = (minimum xs, maximum xs, minimum ys, maximum ys)
            near lo hi v = abs (v - lo) < 1.0e-3 || abs (v - hi) < 1.0e-3
            axisAligned = all (near x0 x1) xs && all (near y0 y1) ys
        convex vs = fillConvex clip [(phys x, phys y) | ((x, y), _) <- vs] (average vs)
        phys v = roundHalfUp (v * scale)
        poke col row c = when (alpha8 c > 0) (blendInto surf col row c)
        fillConvex (cx0, cy0, cx1, cy1) pts c = when (area /= 0) $
          forM_ [max cy0 (minimum (map snd pts)) .. min (cy1 - 1) (maximum (map snd pts))] $ \row ->
            forM_ [max cx0 (minimum (map fst pts)) .. min (cx1 - 1) (maximum (map fst pts))] $ \col ->
              when (all (inside col row) edges) (poke col row c)
          where
            edges = zip pts (drop 1 pts ++ take 1 pts)
            area = sum [f ax * f by - f bx * f ay | ((ax, ay), (bx, by)) <- edges]
            inside col row ((ax, ay), (bx, by)) =
              let (pxc, pyc) = (f col + 0.5, f row + 0.5)
               in ((f bx - f ax) * (pyc - f ay) - (f by - f ay) * (pxc - f ax)) * signum area >= 0
    mapM index [start .. start + fromIntegral (cmdIndexCount cmd) - 1] >>= walk
  where
    cmdClip = Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)
    f :: Int -> Float
    f = fromIntegral

average :: [((Float, Float), RGBA)] -> RGBA
average vs = (mean 0, mean 1, mean 2, mean 3)
  where
    mean i = sum [[r, g, b, a] !! i | (_, (r, g, b, a)) <- vs] / fromIntegral (length vs)

alpha8 :: RGBA -> Int
alpha8 (_, _, _, a) = max 0 (min 255 (round (a * 255)))

-- | Write an opaque colour, or blend a translucent one over the pixel.
blendInto :: RgfwSurface -> Int -> Int -> RGBA -> IO ()
blendInto surf col row c@(r, g, b, a) = do
  let i = row * sWidth surf + col
      t = if alpha8 c >= 255 then 1 else max 0 (min 1 a)
      byte v = fromIntegral (max 0 (min 255 (round (v * 255) :: Int))) :: Word32
  dst <- peekElemOff (sBuffer surf) i
  let channel s v = byte (v * t + fromIntegral ((dst `shiftR` s) .&. 0xFF) / 255 * (1 - t))
  pokeElemOff (sBuffer surf) i $
    0xFF000000 .|. (channel 16 r `shiftL` 16) .|. (channel 8 g `shiftL` 8) .|. channel 0 b
