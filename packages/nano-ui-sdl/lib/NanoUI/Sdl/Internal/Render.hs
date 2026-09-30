-- | Submit nano-ui draw batches to SDL with texture binding and damage clipping.
module NanoUI.Sdl.Internal.Render
  ( RenderBatch
  , newRenderBatch
  , destroyRenderBatch
  , flushRenderBatch
  , renderDrawDataPass
  , glyphPageTexture
  , snapDamage
  )
where

import Control.Exception (onException)
import Control.Monad (unless, void, when)
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Vector.Unboxed qualified as U
import Data.Word (Word8)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
import NanoUI (Color, Rect (..), colorA, colorB, colorG, colorR, rectIntersect)
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

-- | The pixel edges of a rect at a scale, rounded outwards: left, top, right
-- and bottom.
{-# INLINE pixelEdges #-}
pixelEdges :: Float -> Rect -> (Int, Int, Int, Int)
pixelEdges s (Rect x y w h) =
  (floor (x * s), floor (y * s), ceiling ((x + w) * s), ceiling ((y + h) * s))

{-# INLINE snapDamage #-}
snapDamage :: Float -> Damage -> Damage
snapDamage _ DamageFull = DamageFull
snapDamage scale (DamageClip r) =
  let (x0, y0, x1, y1) = pixelEdges scale r
      at v = fromIntegral v / scale
   in DamageClip (Rect (at x0) (at y0) (at x1 - at x0) (at y1 - at y0))

{-# INLINE toClipKey #-}
toClipKey :: Rect -> ClipState
toClipKey r = let (x0, y0, x1, y1) = pixelEdges 1 r in ClipKey x0 y0 (max 1 (x1 - x0)) (max 1 (y1 - y0))

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
-- the damage. A full repaint clears the target to the colour first.
renderDrawDataPass ::
  RenderBatch
  -> Ptr SDL_Renderer
  -> Color
  -> DrawData
  -> ImageAtlas
  -> Ptr ()
  -- ^ The glyph atlas, whose pages text draws from ('glyphPageTexture').
  -> Damage
  -> IO ()
renderDrawDataPass batch ren bg drawData images glyphs damage =
  unless (damageIsEmpty damage) $ do
    clipRef <- newIORef ClipNone
    void $ setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
    mDamage <- case damage of
      DamageFull -> do
        void $ setRenderDrawColorSafe ren (colorR bg) (colorG bg) (colorB bg) (colorA bg)
        Nothing <$ renderClearSafe ren
      -- The draw list starts a clip frame with its own window backdrop, so
      -- a clip needs no clear here.
      DamageClip r -> Just r <$ applyClipState batch clipRef ren (toClipKey r)
    let drawCmd vp ip cmd = do
          let !count = fromIntegral (cmdIndexCount cmd)
              !cmdRect = Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)
              !cmdOpen = cmdClipW cmd >= 1e8 || cmdClipH cmd >= 1e8
              live = case (mDamage, cmdOpen) of
                (Nothing, _) -> Just cmdRect
                (Just dmg, True) -> Just dmg
                (Just dmg, False) -> rectIntersect dmg cmdRect
              -- The C side rejects triangles outside the damage: whether
              -- there is one, and its rect.
              (hasDamage, dx, dy, dw, dh) = case mDamage of
                Nothing -> (0, 0, 0, 0, 0)
                Just (Rect x y w h) -> (1, realToFrac x, realToFrac y, realToFrac w, realToFrac h)
          when (count >= 3) $ for_ live $ \clip -> do
            applyClipState batch clipRef ren (if cmdOpen && isNothing mDamage then ClipNone else toClipKey clip)
            let !texId = cmdTextureId cmd
            tex <- if textureGlyphPage texId >= 0 then glyphPageTexture glyphs (textureGlyphPage texId) else lookupImage images texId
            batchDrawRange (rbBatch batch) vp (fromIntegral (drawVertexCount drawData)) ip
              (fromIntegral (cmdIndexOffset cmd)) count tex hasDamage dx dy dw dh
    withForeignPtr (drawVertices drawData) $ \vp ->
      withForeignPtr (drawIndices drawData) $ \ip ->
        U.forM_ (drawCommands drawData) (drawCmd vp ip)
    applyClipState batch clipRef ren ClipNone

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

flushRenderBatch :: RenderBatch -> IO ()
flushRenderBatch = batchFlush . rbBatch

foreign import ccall unsafe "nano_ui_text_atlas_texture"
  textAtlasTexture :: Ptr () -> CInt -> IO (Ptr SDL_Texture)

foreign import ccall unsafe "nano_ui_batch_create"
  batchCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_batch_destroy"
  batchDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_flush"
  batchFlush :: Ptr () -> IO ()

-- | Queue a command: the batch, the vertices and their count, the indices,
-- the command's first index and index count, its texture, then whether to
-- reject triangles outside the damage and the damage's x, y, width and height.
foreign import ccall unsafe "nano_ui_batch_draw_range"
  batchDrawRange ::
    Ptr () -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> CInt -> Ptr SDL_Texture
    -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()
