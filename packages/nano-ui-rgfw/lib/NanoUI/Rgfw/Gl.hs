-- | OpenGL presentation for the RGFW host.
--
-- Geometry goes to the GPU straight from the core's shared 'DrawData'
-- buffers, one scissored draw per command. Text comes from the collected
-- spans: every glyph is a quad sampled from an atlas that the Cozette
-- software blitter bakes at the current scale, so glyph pixels match what the
-- blitter stamps.
--
-- The frame must come from a context built by
-- 'NanoUI.Rgfw.Context.newRgfwContext' (external text: the buffer holds no
-- text quads). Draw order is 'NanoUI.Rgfw.Context.paintInLayerOrder'.
module NanoUI.Rgfw.Gl
  ( GlRenderer
  , newGlRenderer
  , freeGlRenderer
  , renderArenaGl
  , GlyphAtlas (..)
  , glyphAtlasFor
  , atlasCell
  , bakeGlyphAtlas
  , writeSpanQuads
  , toPhysRect
  , physClip
  ) where

import Control.Exception (bracket)
import Control.Monad (foldM, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import qualified Data.Text.Foreign as TF
import Data.Word (Word32, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (callocBytes, free, reallocBytes)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (pokeByteOff)
import NanoUI (Color (..), Rect (..), roundHalfUp)
import NanoUI.Rgfw.Context (TextSpan, paintInLayerOrder)
import NanoUI.Rgfw.Font.Cozette
  ( CozetteFont (..)
  , cozetteGlyphFootprint
  , foldPenPositions
  , renderGlyphScaledToBuffer
  )
import NanoUI.Testing
  ( DrawCmd (..)
  , DrawData (..)
  , forDrawCmdsInLayer_
  , vertexSize
  )

-- | Opaque C renderer state (cbits/nano_ui_gl.c).
data NanoUiGl

foreign import ccall unsafe "nano_ui_gl_create"
  c_create :: IO (Ptr NanoUiGl)

foreign import ccall unsafe "nano_ui_gl_destroy"
  c_destroy :: Ptr NanoUiGl -> IO ()

foreign import ccall unsafe "nano_ui_gl_upload_atlas"
  c_uploadAtlas :: Ptr NanoUiGl -> Ptr Word32 -> Int32 -> Int32 -> IO Int32

foreign import ccall unsafe "nano_ui_gl_begin"
  c_begin :: Ptr NanoUiGl -> Int32 -> Int32 -> Float -> Float -> Float -> Float -> IO ()

foreign import ccall unsafe "nano_ui_gl_upload_geometry"
  c_uploadGeometry :: Ptr NanoUiGl -> Ptr Word8 -> Int32 -> Ptr Word8 -> Int32 -> IO ()

foreign import ccall unsafe "nano_ui_gl_upload_text"
  c_uploadText :: Ptr NanoUiGl -> Ptr Word8 -> Int32 -> IO ()

foreign import ccall unsafe "nano_ui_gl_draw_geometry"
  c_drawGeometry :: Ptr NanoUiGl -> Int32 -> Int32 -> Int32 -> Int32 -> Word32 -> Word32 -> IO ()

foreign import ccall unsafe "nano_ui_gl_draw_text"
  c_drawText :: Ptr NanoUiGl -> Int32 -> Int32 -> IO ()

-- | GPU resources and glyph scratch storage owned by one OpenGL context.
-- Release with 'freeGlRenderer' while that context is still current.
data GlRenderer = GlRenderer
  { glHandle :: !(Ptr NanoUiGl)
  , glAtlas  :: !(IORef (Maybe GlyphAtlas))
  , glText   :: !(IORef (Ptr Word8, Int)) -- ^ glyph vertex scratch, capacity in vertices
  }

-- | Build the renderer on the calling thread's current OpenGL context.
newGlRenderer :: IO GlRenderer
newGlRenderer = do
  h <- c_create
  when (h == nullPtr) $
    fail "nano-ui-rgfw: OpenGL renderer setup failed (needs an OpenGL 3.2 core context)"
  GlRenderer h <$> newIORef Nothing <*> newIORef (nullPtr, 0)

-- | Release the GPU objects (the context must still be current) and the
-- glyph vertex scratch.
freeGlRenderer :: GlRenderer -> IO ()
freeGlRenderer r = do
  c_destroy (glHandle r)
  (p, _) <- readIORef (glText r)
  free p

-- | Draw a frame into the current context's default framebuffer. The caller
-- swaps buffers.
renderArenaGl ::
  GlRenderer ->
  CozetteFont ->
  Float -> -- logical (layout) -> physical (pixel) scale
  Int -> -- framebuffer width (physical pixels)
  Int -> -- framebuffer height
  Color -> -- clear colour
  DrawData ->
  [TextSpan] -> -- base spans
  [TextSpan] -> -- overlay spans
  IO ()
renderArenaGl r font !scale !fbW !fbH bg drawData baseSpans overlaySpans = do
  atlas <- ensureAtlas r font scale
  buf <- ensureTextCapacity r ((spanChars baseSpans + spanChars overlaySpans) * 6)
  nBase <- foldM (writeSpanQuads atlas font fbW fbH buf) 0 baseSpans
  nAll <- foldM (writeSpanQuads atlas font fbW fbH buf) nBase overlaySpans
  let !h = glHandle r
      (!bgR, !bgG, !bgB, _) = colorFloats bg
  c_begin h (fromIntegral fbW) (fromIntegral fbH) scale bgR bgG bgB
  withForeignPtr (drawVertices drawData) $ \vp ->
    withForeignPtr (drawIndices drawData) $ \ip ->
      c_uploadGeometry h vp (fromIntegral (drawVertexCount drawData)) ip (fromIntegral (drawIndexCount drawData))
  c_uploadText h buf (fromIntegral nAll)
  paintInLayerOrder
    (\layer -> forDrawCmdsInLayer_ layer drawData (drawCmd h scale fbW fbH))
    (c_drawText h 0 (fromIntegral nBase))
    (c_drawText h (fromIntegral nBase) (fromIntegral (nAll - nBase)))

drawCmd :: Ptr NanoUiGl -> Float -> Int -> Int -> DrawCmd -> IO ()
drawCmd h !scale !fbW !fbH cmd
  | cmdIndexCount cmd < 3 = pure ()
  | otherwise =
      case physClip scale fbW fbH (Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)) of
        Nothing -> pure ()
        Just (x0, y0, x1, y1) ->
          c_drawGeometry h (fromIntegral x0) (fromIntegral y0) (fromIntegral x1) (fromIntegral y1)
            (cmdIndexOffset cmd) (cmdIndexCount cmd)

-- | Scale logical x/y/width/height to physical pixels, snapping both edges
-- with half-up rounding. Returns x/y/width/height with non-negative extents.
{-# INLINE toPhysRect #-}
toPhysRect :: Float -> Float -> Float -> Float -> Float -> (Int, Int, Int, Int)
toPhysRect !scale !rx !ry !rw !rh =
  let !x0 = roundHalfUp (rx * scale)
      !y0 = roundHalfUp (ry * scale)
      !x1 = roundHalfUp ((rx + rw) * scale)
      !y1 = roundHalfUp ((ry + rh) * scale)
   in (x0, y0, max 0 (x1 - x0), max 0 (y1 - y0))

-- | A logical clip rect scaled to physical pixels and intersected with a
-- w x h target, as @(x0, y0, x1, y1)@ with exclusive ends; 'Nothing' if empty.
{-# INLINE physClip #-}
physClip :: Float -> Int -> Int -> Rect -> Maybe (Int, Int, Int, Int)
physClip !scale !w !h (Rect x y rw rh) =
  let (!px, !py, !pw, !ph) = toPhysRect scale x y rw rh
      !x0 = max 0 px
      !y0 = max 0 py
      !x1 = min w (px + pw)
      !y1 = min h (py + ph)
   in if x0 >= x1 || y0 >= y1 then Nothing else Just (x0, y0, x1, y1)

-- | Cell grid of one glyph bake. Every glyph owns a 'gaCellW' x 'gaCellH'
-- cell: the footprint 'renderGlyphScaledToBuffer' stamps at 'gaScale'.
data GlyphAtlas = GlyphAtlas
  { gaScale  :: !Float
  , gaCellW  :: !Int
  , gaCellH  :: !Int
  , gaCols   :: !Int
  , gaWidth  :: !Int
  , gaHeight :: !Int
  }
  deriving (Eq, Show)

-- | A roughly square atlas holding every glyph of the font at a scale.
glyphAtlasFor :: CozetteFont -> Float -> GlyphAtlas
glyphAtlasFor font !scale =
  let (!cw, !ch) = cozetteGlyphFootprint scale
      !n = max 1 (cfNumGlyphs font)
      !cols = max 1 (ceiling (sqrt (fromIntegral (n * ch) / fromIntegral cw :: Double)))
      !rows = (n + cols - 1) `quot` cols
   in GlyphAtlas scale cw ch cols (cols * cw) (rows * ch)

-- | Top-left texel of a glyph's cell.
atlasCell :: GlyphAtlas -> Int -> (Int, Int)
atlasCell ga gid =
  let (!row, !col) = gid `quotRem` gaCols ga
   in (col * gaCellW ga, row * gaCellH ga)

-- | Stamp every glyph into its cell with the software blitter, opaque white
-- over transparent black, and hand the pixels to the continuation: each
-- pixel's low byte is its coverage. The pixels are freed afterwards.
bakeGlyphAtlas :: CozetteFont -> GlyphAtlas -> (Ptr Word32 -> IO a) -> IO a
bakeGlyphAtlas font ga k =
  bracket (callocBytes (gaWidth ga * gaHeight ga * 4)) free $ \px -> do
    let go !gid = when (gid < cfNumGlyphs font) $ do
          let (!cx, !cy) = atlasCell ga gid
          renderGlyphScaledToBuffer px (gaWidth ga) cx cy (cx + gaCellW ga) (cy + gaCellH ga)
            (gaScale ga) cx cy 0xFFFFFFFF font (fromIntegral gid)
          go (gid + 1)
    go 0
    k px

ensureAtlas :: GlRenderer -> CozetteFont -> Float -> IO GlyphAtlas
ensureAtlas r font !scale = do
  cur <- readIORef (glAtlas r)
  case cur of
    Just ga | gaScale ga == scale -> pure ga
    _ -> do
      let !ga = glyphAtlasFor font scale
      ok <- bakeGlyphAtlas font ga $ \px ->
        c_uploadAtlas (glHandle r) px (fromIntegral (gaWidth ga)) (fromIntegral (gaHeight ga))
      when (ok == 0) $ fail "nano-ui-rgfw: glyph atlas upload failed"
      writeIORef (glAtlas r) (Just ga)
      pure ga

ensureTextCapacity :: GlRenderer -> Int -> IO (Ptr Word8)
ensureTextCapacity r !needVerts = do
  (p, cap) <- readIORef (glText r)
  if needVerts <= cap
    then pure p
    else do
      let !cap' = maximum [needVerts, cap * 2, 4096]
      p' <- reallocBytes p (cap' * vertexSize)
      writeIORef (glText r) (p', cap')
      pure p'

-- | Upper bound on a span list's glyph count: the UTF-8 byte length, which is
-- O(1) per span.
spanChars :: [TextSpan] -> Int
spanChars = foldl' (\acc (_, t, _, _, _) -> acc + TF.lengthWord8 t) 0

-- | Append a span's glyph quads to a vertex buffer, 6 vertices per glyph in
-- the core's vertex layout: physical-pixel position, span colour, atlas UV.
-- Pen positions are 'foldPenPositions' at the atlas scale. Quads are clipped
-- to the span clip and the framebuffer with UVs cut to match, so text draws
-- need no scissor. The buffer must have room for 6 vertices per character;
-- returns the new vertex count.
writeSpanQuads :: GlyphAtlas -> CozetteFont -> Int -> Int -> Ptr Word8 -> Int -> TextSpan -> IO Int
writeSpanQuads ga font !fbW !fbH buf !n0 (Rect rx ry _ _, txt, fg, _, clip) =
  case physClip scale fbW fbH clip of
    Nothing -> pure n0
    Just (cx0, cy0, cx1, cy1) ->
      let quad !n !penX !penY !glyph =
            let !gid = if fromIntegral glyph < cfNumGlyphs font then fromIntegral glyph else 0
                (!ax, !ay) = atlasCell ga gid
                !x0 = max cx0 penX
                !y0 = max cy0 penY
                !x1 = min cx1 (penX + gaCellW ga)
                !y1 = min cy1 (penY + gaCellH ga)
                u x = fromIntegral (ax + x - penX) / aw
                v y = fromIntegral (ay + y - penY) / ah
             in if x0 >= x1 || y0 >= y1
                  then pure n
                  else do
                    vertex n x0 y0 (u x0) (v y0)
                    vertex (n + 1) x1 y0 (u x1) (v y0)
                    vertex (n + 2) x1 y1 (u x1) (v y1)
                    vertex (n + 3) x0 y0 (u x0) (v y0)
                    vertex (n + 4) x1 y1 (u x1) (v y1)
                    vertex (n + 5) x0 y1 (u x0) (v y1)
                    pure (n + 6)
       in foldPenPositions font scale rx ry n0 quad txt
  where
    !scale = gaScale ga
    !aw = fromIntegral (gaWidth ga) :: Float
    !ah = fromIntegral (gaHeight ga) :: Float
    (!fr, !fgG, !fb, !fa) = colorFloats fg
    vertex :: Int -> Int -> Int -> Float -> Float -> IO ()
    vertex !i !x !y !u !v = do
      let !off = i * vertexSize
      pokeByteOff buf off (fromIntegral x :: Float)
      pokeByteOff buf (off + 4) (fromIntegral y :: Float)
      pokeByteOff buf (off + 8) fr
      pokeByteOff buf (off + 12) fgG
      pokeByteOff buf (off + 16) fb
      pokeByteOff buf (off + 20) fa
      pokeByteOff buf (off + 24) u
      pokeByteOff buf (off + 28) v

colorFloats :: Color -> (Float, Float, Float, Float)
colorFloats (Color w) = (chan 24, chan 16, chan 8, chan 0)
  where
    chan s = fromIntegral ((w `shiftR` s) .&. 0xFF) / 255
