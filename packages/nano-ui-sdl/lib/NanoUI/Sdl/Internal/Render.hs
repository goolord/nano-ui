{-# OPTIONS_HADDOCK not-home #-}

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
import Data.ByteString.Unsafe qualified as BSU
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Primitive.ByteArray (ByteArray, copyByteArrayToAddr, sizeofByteArray)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64, Word8)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, malloc)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke)
import NanoUI (Rect (..), colorA, colorB, colorG, colorR)
import NanoUI.Sdl.Internal.Image (ImageAtlas, lookupImage)
import NanoUI.Backend
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
-- draws at render scale 1 (@nano_ui_batch_begin@ scales the positions), so
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
    let calls = drawShaderCalls drawData
        uBytes = drawUniformBytes drawData
        spans = drawEffectSpans drawData
        drawCmd vp ip curDmg ox oy cmd = do
          let !count = fromIntegral (cmdIndexCount cmd)
              -- The C side rejects triangles outside the damage: whether
              -- there is one, and its rect.
              (hasDamage, dx, dy, dw, dh) = case curDmg of
                Nothing -> (0, 0, 0, 0, 0)
                Just (Rect x y w h) -> (1, realToFrac x, realToFrac y, realToFrac w, realToFrac h)
              !shIdx = fromIntegral (cmdShader cmd) :: Int
          when (count >= 3) $ for_ (cmdEffectiveClip curDmg cmd) $ \mClip -> do
            let shiftedClip =
                  if ox == 0 && oy == 0
                    then mClip
                    else fmap (shiftRect (-ox) (-oy)) mClip
            applyClipState batch clipRef ren (maybe ClipNone (toClipKey scale) shiftedClip)
            let !texId = cmdTextureId cmd
            tex <- if textureGlyphPage texId >= 0 then glyphPageTexture glyphs (textureGlyphPage texId) else lookupImage images texId
            if shIdx > 0 && shIdx <= sizeofSmallArray calls
              then
                drawShaderCmd
                  (rbBatch batch) vp (fromIntegral (drawVertexCount drawData)) ip
                  (fromIntegral (cmdIndexOffset cmd)) count tex hasDamage dx dy dw dh
                  (indexSmallArray calls (shIdx - 1)) uBytes
              else
                batchDrawRange (rbBatch batch) vp (fromIntegral (drawVertexCount drawData)) ip
                  (fromIntegral (cmdIndexOffset cmd)) count tex hasDamage dx dy dw dh
    -- The batch keeps the vertex and index pointers until it flushes, so
    -- the last flush is inside the buffers' scope.
    ( withForeignPtr (drawVertices drawData) $ \vp ->
        withForeignPtr (drawIndices drawData) $ \ip -> do
          if sizeofSmallArray spans == 0
            then U.forM_ (drawCommands drawData) (drawCmd vp ip mDamage 0 0)
            else do
              rootTarget <- batchCurrentTarget (rbBatch batch)
              let cmds = drawCommands drawData
                  drv =
                    EffectDriver
                      { edScale = scale
                      , edFlipY = False
                      , edPreparePasses = \passes ->
                          pure $
                            if all (\i -> let p = scProgram (indexSmallArray passes i) in isJust (spSpirv p) || isJust (spMsl p)) [0 .. sizeofSmallArray passes - 1]
                              then Just passes
                              else Nothing
                      , edAcquireTarget = acquireTarget (rbBatch batch)
                      , edReleaseTarget = batchReleaseTarget (rbBatch batch) . fromIntegral
                      , edBindTarget = \slot clr ox oy -> do
                          ok <- batchBindTarget (rbBatch batch) (fromIntegral slot) rootTarget (if clr then 1 else 0)
                          if ok == 0
                            then pure False
                            else do
                              writeIORef clipRef ClipNone
                              unless clr (void $ setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr))
                              batchSetOrigin (rbBatch batch) (realToFrac ox) (realToFrac oy)
                              pure True
                      , edFlush = flushRenderBatch batch
                      , edEmitPlain = \_ _ _ ox oy curDmg from to ->
                          let loop !i = when (i < to) (drawCmd vp ip curDmg ox oy (U.unsafeIndex cmds i) >> loop (i + 1))
                           in loop from
                      , edAfterSpanContent = \_ -> pure ()
                      , edCompositePass = \_ sc pd _ _ -> do
                          unless (epdIntermediate pd) $
                            applyClipState batch clipRef ren (maybe ClipNone (toClipKey scale) (epdClip pd))
                          compositeEffectPass (rbBatch batch) sc uBytes pd
                      }
              execEffectSpans drv spans 0 (-1) (0, 0) 0 0 mDamage 0 (U.length cmds)
          flushRenderBatch batch
          applyClipState batch clipRef ren ClipNone
      )
      `onException` discardRenderBatch batch

{-# INLINE shiftRect #-}
shiftRect :: Float -> Float -> Rect -> Rect
shiftRect dx dy (Rect x y w h) = Rect (x + dx) (y + dy) w h

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

-- | Free a render batch and its clip rectangle storage.
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

{-# NOINLINE drawShaderCmd #-}
drawShaderCmd ::
  Ptr () ->
  Ptr Word8 -> CInt ->
  Ptr Word8 -> CInt -> CInt ->
  Ptr SDL_Texture ->
  CInt -> CFloat -> CFloat -> CFloat -> CFloat ->
  ShaderCall ->
  ByteArray ->
  IO ()
drawShaderCmd batchPtr vp vc ip idxStart idxCount tex hasDmg dx dy dw dh sc uniformBytes = do
  let prog = scProgram sc
      !uOff = scUniformOffset sc
      !uLen = min (scUniformLength sc) (max 0 (sizeofByteArray uniformBytes - uOff))
      !needsTex = if spNeedsTex prog then 1 else 0 :: CInt
      withOptBS Nothing k = k nullPtr 0
      withOptBS (Just bs) k = BSU.unsafeUseAsCStringLen bs $ \(p, l) -> k (castPtr p) (fromIntegral l)
      withOptText Nothing k = k nullPtr 0
      withOptText (Just t) k = withOptBS (Just (TE.encodeUtf8 t)) k
  withOptBS (spSpirv prog) $ \spvPtr spvLen ->
    withOptText (spMsl prog) $ \mslPtr mslLen -> do
      ok <-
        if uLen > 0
          then allocaBytes uLen $ \uPtr -> do
            copyByteArrayToAddr uPtr uniformBytes uOff uLen
            batchDrawShader batchPtr vp vc ip idxStart idxCount tex hasDmg dx dy dw dh
              (scHash sc) spvPtr spvLen mslPtr mslLen needsTex uPtr (fromIntegral uLen)
          else
            batchDrawShader batchPtr vp vc ip idxStart idxCount tex hasDmg dx dy dw dh
              (scHash sc) spvPtr spvLen mslPtr mslLen needsTex nullPtr 0
      when (ok == 0) $
        batchDrawRange batchPtr vp vc ip idxStart idxCount nullPtr hasDmg dx dy dw dh

-- | Queue a command: the batch, the vertices and their count, the indices,
-- the command's first index and index count, its texture, then whether to
-- reject triangles outside the damage and the damage's x, y, width and height.
foreign import ccall unsafe "nano_ui_batch_draw_range"
  batchDrawRange ::
    Ptr () -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> CInt -> Ptr SDL_Texture
    -> CInt -> CFloat -> CFloat -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "nano_ui_batch_draw_shader"
  batchDrawShader ::
    Ptr () -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> CInt -> Ptr SDL_Texture
    -> CInt -> CFloat -> CFloat -> CFloat -> CFloat
    -> Word64
    -> Ptr Word8 -> CInt
    -> Ptr Word8 -> CInt
    -> CInt
    -> Ptr Word8 -> CInt
    -> IO CInt

foreign import ccall unsafe "nano_ui_batch_set_origin"
  batchSetOrigin :: Ptr () -> CFloat -> CFloat -> IO ()

foreign import ccall unsafe "nano_ui_batch_current_target"
  batchCurrentTarget :: Ptr () -> IO (Ptr SDL_Texture)

foreign import ccall unsafe "nano_ui_batch_acquire_target"
  batchAcquireTarget :: Ptr () -> CInt -> CInt -> Ptr CInt -> Ptr CInt -> IO CInt

foreign import ccall unsafe "nano_ui_batch_release_target"
  batchReleaseTarget :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "nano_ui_batch_bind_target"
  batchBindTarget :: Ptr () -> CInt -> Ptr SDL_Texture -> CInt -> IO CInt

foreign import ccall unsafe "nano_ui_batch_composite_effect"
  batchCompositeEffect ::
    Ptr () -> CInt
    -> CFloat -> CFloat -> CFloat -> CFloat
    -> CFloat -> CFloat -> CFloat -> CFloat
    -> CInt
    -> Word64
    -> Ptr Word8 -> CInt
    -> Ptr Word8 -> CInt
    -> Ptr Word8 -> CInt
    -> IO CInt

acquireTarget :: Ptr () -> Int -> Int -> IO (Int, Int, Int)
acquireTarget batchPtr minW minH =
  alloca $ \pW ->
    alloca $ \pH -> do
      slot <- batchAcquireTarget batchPtr (fromIntegral minW) (fromIntegral minH) pW pH
      if slot < 0
        then pure (-1, 0, 0)
        else do
          cw <- peek pW
          ch <- peek pH
          pure (fromIntegral slot, fromIntegral cw, fromIntegral ch)

{-# NOINLINE compositeEffectPass #-}
compositeEffectPass :: Ptr () -> ShaderCall -> ByteArray -> EffectPassDraw -> IO ()
compositeEffectPass batchPtr sc uniformBytes pd = do
  let prog = scProgram sc
      withOptBS Nothing k = k nullPtr 0
      withOptBS (Just bs) k = BSU.unsafeUseAsCStringLen bs $ \(p, l) -> k (castPtr p) (fromIntegral l)
      withOptText Nothing k = k nullPtr 0
      withOptText (Just t) k = withOptBS (Just (TE.encodeUtf8 t)) k
      blendCopy = if epdIntermediate pd then 1 else 0 :: CInt
  withOptBS (spSpirv prog) $ \spvPtr spvLen ->
    withOptText (spMsl prog) $ \mslPtr mslLen ->
      withEffectUniforms sc uniformBytes pd $ \uPtr uLen ->
        void $
          batchCompositeEffect
            batchPtr (fromIntegral (epdSrcSlot pd))
            (realToFrac (epdDstX pd)) (realToFrac (epdDstY pd)) (realToFrac (epdDstW pd)) (realToFrac (epdDstH pd))
            (realToFrac (epdU0 pd)) (realToFrac (epdV0 pd)) (realToFrac (epdU1 pd)) (realToFrac (epdV1 pd))
            blendCopy (scHash sc)
            spvPtr spvLen mslPtr mslLen
            uPtr (fromIntegral uLen)
