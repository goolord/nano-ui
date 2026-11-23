-- | Submit nano-ui draw batches to SDL with texture binding and damage clipping.
module NanoUI.Sdl.Internal.Render
  ( RenderBatch
  , newRenderBatch
  , destroyRenderBatch
  , flushRenderBatch
  , discardRenderBatch
  , renderDrawDataPass
  , glyphPageTexture
  , snapDamage
  )
where

import Control.Exception (onException)
import Control.Monad (unless, void, when)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
import NanoUI (Rect (..), colorA, colorB, colorG, colorR)
import NanoUI.Backend (cmdEffectiveClip, snapDamage)
import NanoUI.Sdl.Internal.Image (ImageAtlas, lookupImage)
import NanoUI.Testing
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render
  ( renderClearSafe
  , setRenderClipRect
  , setRenderDrawColorSafe
  )

data ClipState
  = ClipNone
  | ClipKey
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
      {-# UNPACK #-} !Int
  deriving Eq

-- | A layout-unit clip as the device-pixel rect SDL clips to. The pass
-- draws at render scale 1 ('nano_ui_batch_begin' scales the positions), so
-- the clip is the same whole pixels the damage was snapped to
-- ('snapDamage'), whatever the scale ('clipPixelEdges'). SDL clips to at
-- least one pixel each way.
{-# INLINE toClipKey #-}
toClipKey :: Float -> Rect -> ClipState
toClipKey scale r =
  let (x0, y0, x1, y1) = clipPixelEdges scale r
   in ClipKey x0 y0 (max 1 (x1 - x0)) (max 1 (y1 - y0))

applyClipState ::
  RenderBatch -> IORef ClipState -> Ptr SDL_Renderer -> ClipState -> IO ()
applyClipState batch ref ren next = do
  prev <- readIORef ref
  when (prev /= next) $ do
    flushRenderBatch batch
    writeIORef ref next
    void $ case next of
      ClipNone -> setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
      ClipKey px py pw ph -> do
        poke (rbClipRect batch) (SDL_Rect (fromIntegral px) (fromIntegral py) (fromIntegral pw) (fromIntegral ph))
        setRenderClipRect ren (PtrConst.unsafeFromPtr (rbClipRect batch))

-- | Draw every command in layer order, clipped to its own rect and to
-- the damage. A full repaint clears the target to 'drawClear' first.
--
-- The renderer must be at render scale 1: positions are multiplied by the
-- scale here, and clips are whole device pixels. The pass submits
-- everything it queued before it returns, while the draw data's buffers
-- are held; a pass that throws drops what it had queued instead.
renderDrawDataPass ::
  RenderBatch
  -> Ptr SDL_Renderer
  -> Float
  -- ^ Device pixels per layout unit.
  -> DrawData
  -> ImageAtlas
  -> Ptr ()
  -- ^ The glyph atlas, whose pages text draws from ('glyphPageTexture').
  -> Damage
  -> IO ()
renderDrawDataPass batch ren scale drawData images glyphs damage =
  unless (damageIsEmpty damage) $ do
    batchBegin (rbBatch batch) (realToFrac scale)
    clipRef <- newIORef ClipNone
    void $ setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
    mDamage <- case damage of
      DamageFull -> do
        let clear = drawClear drawData
        void $ setRenderDrawColorSafe ren (colorR clear) (colorG clear) (colorB clear) (colorA clear)
        void $ renderClearSafe ren
        pure Nothing
      -- The draw list starts a clip frame with its own page backdrop, so a
      -- clip needs no clear here.
      DamageClip r -> Just r <$ applyClipState batch clipRef ren (toClipKey scale r)
    let drawCmd vp ip cmd = do
          let !count = fromIntegral (cmdIndexCount cmd)
              -- The C side rejects triangles outside the damage: whether
              -- there is one, and its rect.
              (hasDamage, dx, dy, dw, dh) = case mDamage of
                Nothing -> (0, 0, 0, 0, 0)
                Just (Rect x y w h) -> (1, realToFrac x, realToFrac y, realToFrac w, realToFrac h)
          when (count >= 3) $ for_ (cmdEffectiveClip mDamage cmd) $ \mClip -> do
            applyClipState batch clipRef ren (maybe ClipNone (toClipKey scale) mClip)
            let !texId = cmdTextureId cmd
            tex <- if textureGlyphPage texId >= 0 then glyphPageTexture glyphs (textureGlyphPage texId) else lookupImage images texId
            batchDrawRange (rbBatch batch) vp (fromIntegral (drawVertexCount drawData)) ip
              (fromIntegral (cmdIndexOffset cmd)) count tex hasDamage dx dy dw dh
    -- The batch keeps the vertex and index pointers until it flushes, so
    -- the last flush is inside the buffers' scope.
    ( withForeignPtr (drawVertices drawData) $ \vp ->
        withForeignPtr (drawIndices drawData) $ \ip -> do
          U.forM_ (drawCommands drawData) (drawCmd vp ip)
          flushRenderBatch batch
          applyClipState batch clipRef ren ClipNone
      )
      `onException` discardRenderBatch batch

-- | The texture of a glyph atlas page: null for a page never opened, one past
-- the last, or a null atlas.
glyphPageTexture :: Ptr () -> Int -> IO (Ptr SDL_Texture)
glyphPageTexture atlas = textAtlasTexture atlas . fromIntegral

-- | Session-owned coalescing state and reusable clip storage. Commands whose
-- logical clips differ can still merge after damage clipping and pixel rounding.
data RenderBatch = RenderBatch {rbBatch :: !(Ptr ()), rbClipRect :: !(Ptr SDL_Rect)}

-- | Create a persistent render batch. Reusing one batch across frames avoids
-- a C calloc/free pair per presented frame; flush after each render pass.
newRenderBatch :: Ptr SDL_Renderer -> IO RenderBatch
newRenderBatch ren = do
  p <- batchCreate ren
  if p == nullPtr
    then fail "nano_ui_batch_create failed"
    else (RenderBatch p <$> malloc) `onException` batchDestroy p

destroyRenderBatch :: RenderBatch -> IO ()
destroyRenderBatch batch = batchDestroy (rbBatch batch) >> free (rbClipRect batch)

-- | Draw what the batch has queued.
flushRenderBatch :: RenderBatch -> IO ()
flushRenderBatch = batchFlush . rbBatch

-- | Drop what the batch has queued without drawing it.
discardRenderBatch :: RenderBatch -> IO ()
discardRenderBatch = batchDiscard . rbBatch

foreign import ccall unsafe "nano_ui_text_atlas_texture"
  textAtlasTexture :: Ptr () -> CInt -> IO (Ptr SDL_Texture)

foreign import ccall unsafe "nano_ui_batch_create"
  batchCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_batch_destroy"
  batchDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_flush"
  batchFlush :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_discard"
  batchDiscard :: Ptr () -> IO ()

-- | Start a pass at a scale, in device pixels per layout unit.
foreign import ccall unsafe "nano_ui_batch_begin"
  batchBegin :: Ptr () -> CFloat -> IO ()

-- | Queue a command: the batch, the vertices and their count, the indices,
-- the command's first index and index count, its texture, then whether to
-- reject triangles outside the damage and the damage's x, y, width and height.
foreign import ccall unsafe "nano_ui_batch_draw_range"
  batchDrawRange ::
    Ptr () -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> CInt -> Ptr SDL_Texture
    -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
