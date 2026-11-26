{-# OPTIONS_HADDOCK not-home #-}

-- | Submit nano-ui draw batches to SDL with texture binding and damage clipping.
module NanoUI.Sdl.Internal.Render
  ( RenderBatch
  , newRenderBatch
  , destroyRenderBatch
  , flushRenderBatch
  , discardRenderBatch
  , renderDrawDataPass
  , presentGpuRetain
  , glyphPageTexture
  , snapDamage
  )
where

import Control.Exception (onException)
import Control.Monad (unless, void, when)
import Data.ByteString.Unsafe qualified as BSU
import Data.Maybe (isJust)
import Data.Primitive.ByteArray (ByteArray, copyByteArrayToAddr, sizeofByteArray)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray)
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64, Word8)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import NanoUI (Color, Rect (..), colorA, colorB, colorG, colorR)
import NanoUI.Sdl.Internal.Display (outPair)
import NanoUI.Sdl.Internal.Image (ImageAtlas, lookupImage)
import NanoUI.Backend
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Render
  ( renderClearSafe
  , setRenderDrawColorSafe
  )

{-# INLINE setBatchClip #-}
setBatchClip :: Ptr () -> Float -> Maybe Rect -> IO ()
setBatchClip batchPtr _ Nothing = batchSetClip batchPtr 0 0 0 0 0
setBatchClip batchPtr scale (Just r) =
  let !(x0, y0, x1, y1) = clipPixelEdges scale r
   in batchSetClip batchPtr 1 (fromIntegral x0) (fromIntegral y0) (fromIntegral (max 1 (x1 - x0))) (fromIntegral (max 1 (y1 - y0)))

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
    let !batchPtr = rbBatch batch
        !vertCount = fromIntegral (drawVertexCount drawData)
    batchBegin batchPtr (realToFrac scale)
    mDamage <- case damage of
      DamageFull -> do
        let clear = drawClear drawData
        void $ setRenderDrawColorSafe ren (colorR clear) (colorG clear) (colorB clear) (colorA clear)
        void $ renderClearSafe ren
        pure Nothing
      -- The draw list starts a clip frame with its own page backdrop, so a
      -- clip needs no clear here.
      DamageClip r -> Just r <$ setBatchClip batchPtr scale (Just r)
    let calls = drawShaderCalls drawData
        uBytes = drawUniformBytes drawData
        spans = drawEffectSpans drawData
        drawCmd vp ip curDmg !ox !oy cmd = do
          let !count = fromIntegral (cmdIndexCount cmd)
              -- The C side rejects triangles outside the damage: whether
              -- there is one, and its rect.
              (hasDamage, dx, dy, dw, dh) = case curDmg of
                Nothing -> (0, 0, 0, 0, 0)
                Just (Rect x y w h) -> (1, realToFrac x, realToFrac y, realToFrac w, realToFrac h)
              !shIdx = fromIntegral (cmdShader cmd) :: Int
          when (count >= 3) $ case cmdEffectiveClip curDmg cmd of
            Nothing -> pure ()
            Just mClip -> do
              setBatchClip batchPtr scale (shiftClip (-ox) (-oy) mClip)
              let !texId = cmdTextureId cmd
                  !page = textureGlyphPage texId
              tex <- if page >= 0 then glyphPageTexture glyphs page else lookupImage images texId
              if shIdx > 0 && shIdx <= sizeofSmallArray calls
                then
                  drawShaderCmd
                    batchPtr vp vertCount ip
                    (fromIntegral (cmdIndexOffset cmd)) count tex hasDamage dx dy dw dh
                    (indexSmallArray calls (shIdx - 1)) uBytes
                else
                  batchDrawRange batchPtr vp vertCount ip
                    (fromIntegral (cmdIndexOffset cmd)) count tex hasDamage dx dy dw dh
    -- The batch keeps the vertex and index pointers until it flushes, so
    -- the last flush is inside the buffers' scope.
    ( withForeignPtr (drawVertices drawData) $ \vp ->
        withForeignPtr (drawIndices drawData) $ \ip -> do
          if sizeofSmallArray spans == 0
            then U.forM_ (drawCommands drawData) (drawCmd vp ip mDamage 0 0)
            else do
              rootTarget <- batchCurrentTarget batchPtr
              let cmds = drawCommands drawData
                  drv =
                    EffectDriver
                      { edScale = scale
                      , edFlipY = False
                      , edPreparePasses = \passes ->
                          pure $
                            if all (\sc -> let p = scProgram sc in isJust (spSpirv p) || isJust (spMsl p) || isJust (spGlsl150 p)) passes
                              then Just passes
                              else Nothing
                      , edAcquireTarget = acquireTarget batchPtr
                      , edReleaseTarget = batchReleaseTarget batchPtr . fromIntegral
                      , edBindTarget = \slot clr ox oy -> do
                          ok <- batchBindTarget batchPtr (fromIntegral slot) rootTarget (if clr then 1 else 0)
                          if ok == 0
                            then pure False
                            else do
                              batchSetOrigin batchPtr (realToFrac ox) (realToFrac oy)
                              pure True
                      , edFlush = flushRenderBatch batch
                      , edEmitPlain = \_ _ _ ox oy curDmg from to ->
                          let loop !i = when (i < to) (drawCmd vp ip curDmg ox oy (U.unsafeIndex cmds i) >> loop (i + 1))
                           in loop from
                      , edAfterSpanContent = \_ -> pure ()
                      , edCompositePass = \_ sc pd _ _ -> do
                          unless (epdIntermediate pd) $
                            setBatchClip batchPtr scale (epdClip pd)
                          compositeEffectPass batchPtr sc uBytes pd
                      }
              execEffectSpans drv spans 0 (-1) (0, 0) 0 0 mDamage 0 (U.length cmds)
          flushRenderBatch batch
          batchSetClip batchPtr 0 0 0 0 0
      )
      `onException` discardRenderBatch batch

-- | The texture of a glyph atlas page: null for a page never opened, one past
-- the last, or a null atlas.
glyphPageTexture :: Ptr () -> Int -> IO (Ptr SDL_Texture)
glyphPageTexture atlas = textAtlasTexture atlas . fromIntegral

-- | Session-owned coalescing state and clip deduplication. Commands whose
-- logical clips differ can still merge after damage clipping and pixel rounding.
newtype RenderBatch = RenderBatch {rbBatch :: Ptr ()}

-- | Create a persistent render batch. Reusing one batch across frames avoids
-- a C calloc/free pair per presented frame; flush after each render pass.
newRenderBatch :: Ptr SDL_Renderer -> IO RenderBatch
newRenderBatch ren = do
  p <- batchCreate ren
  if p == nullPtr
    then fail "nano_ui_batch_create failed"
    else pure (RenderBatch p)

-- | Free a render batch.
destroyRenderBatch :: RenderBatch -> IO ()
destroyRenderBatch = batchDestroy . rbBatch

-- | Draw what the batch has queued.
flushRenderBatch :: RenderBatch -> IO ()
flushRenderBatch = batchFlush . rbBatch

-- | Present an opaque retained texture directly to the SDL 3.4 GPU swapchain
-- at 1:1 pixels while live-resizing away from SDL 3.4's internal GPU
-- backbuffer size. Returns 'False' when the standard copy-and-present path
-- should run instead (steady frames, SDL 3.5+, or non-GPU renderers).
presentGpuRetain :: RenderBatch -> Ptr SDL_Window -> Ptr SDL_Texture -> Int -> Int -> Bool -> Color -> IO Bool
presentGpuRetain batch win tex rw rh inResize clear =
  (/= 0)
    <$> batchPresentGpuRetain
      (rbBatch batch)
      win
      tex
      (fromIntegral rw)
      (fromIntegral rh)
      (if inResize then 1 else 0)
      (colorR clear)
      (colorG clear)
      (colorB clear)
      (colorA clear)

-- | Drop what the batch has queued without drawing it.
discardRenderBatch :: RenderBatch -> IO ()
discardRenderBatch = batchDiscard . rbBatch

foreign import ccall safe "nano_ui_batch_present_gpu_retain"
  batchPresentGpuRetain ::
    Ptr () -> Ptr SDL_Window -> Ptr SDL_Texture -> CInt -> CInt -> CInt -> Word8 -> Word8 -> Word8 -> Word8 -> IO CInt

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

foreign import ccall unsafe "nano_ui_batch_set_clip"
  batchSetClip :: Ptr () -> CInt -> CInt -> CInt -> CInt -> CInt -> IO ()

withShaderCode :: ShaderProgram -> (Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO a) -> IO a
withShaderCode prog k =
  withOptBS (spSpirv prog) $ \spvPtr spvLen ->
    withOptBS (TE.encodeUtf8 <$> spMsl prog) $ \mslPtr mslLen ->
      withOptBS (TE.encodeUtf8 <$> spGlsl150 prog) (k spvPtr spvLen mslPtr mslLen)
  where
    withOptBS Nothing f = f nullPtr 0
    withOptBS (Just bs) f = BSU.unsafeUseAsCStringLen bs $ \(p, l) -> f (castPtr p) (fromIntegral l)

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
  withShaderCode prog $ \spvPtr spvLen mslPtr mslLen glslPtr glslLen -> do
    let call uPtr len =
          batchDrawShader batchPtr vp vc ip idxStart idxCount tex hasDmg dx dy dw dh
            (scHash sc) spvPtr spvLen mslPtr mslLen glslPtr glslLen needsTex uPtr len
    ok <-
      if uLen > 0
        then allocaBytes uLen $ \uPtr -> copyByteArrayToAddr uPtr uniformBytes uOff uLen >> call uPtr (fromIntegral uLen)
        else call nullPtr 0
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
    -> Ptr Word8 -> CInt
    -> IO CInt

acquireTarget :: Ptr () -> Int -> Int -> IO (Int, Int, Int)
acquireTarget batchPtr minW minH = do
  (slot, cw, ch) <- outPair (batchAcquireTarget batchPtr (fromIntegral minW) (fromIntegral minH))
  pure (if slot < 0 then (-1, 0, 0) else (fromIntegral slot, fromIntegral cw, fromIntegral ch))

{-# NOINLINE compositeEffectPass #-}
compositeEffectPass :: Ptr () -> ShaderCall -> ByteArray -> EffectPassDraw -> IO ()
compositeEffectPass batchPtr sc uniformBytes pd =
  withShaderCode (scProgram sc) $ \spvPtr spvLen mslPtr mslLen glslPtr glslLen ->
    withEffectUniforms sc uniformBytes pd $ \uPtr uLen ->
      void $
        batchCompositeEffect
          batchPtr (fromIntegral (epdSrcSlot pd))
          (realToFrac (epdDstX pd)) (realToFrac (epdDstY pd)) (realToFrac (epdDstW pd)) (realToFrac (epdDstH pd))
          (realToFrac (epdU0 pd)) (realToFrac (epdV0 pd)) (realToFrac (epdU1 pd)) (realToFrac (epdV1 pd))
          (if epdIntermediate pd then 1 else 0) (scHash sc)
          spvPtr spvLen mslPtr mslLen glslPtr glslLen
          uPtr (fromIntegral uLen)
