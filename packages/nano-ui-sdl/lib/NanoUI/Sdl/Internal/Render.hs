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

import Control.Exception (finally, onException)
import Control.Monad (unless, void, when)
import Data.ByteString.Unsafe qualified as BSU
import Data.Foldable (for_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortOn)
import Data.Primitive.ByteArray (ByteArray, copyByteArrayToAddr, sizeofByteArray)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray)
import Data.Text.Encoding qualified as TE
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64, Word8)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes, free, malloc)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek, poke, pokeByteOff)
import NanoUI (Rect (..), V2 (..), colorA, colorB, colorG, colorR, rectIntersect)
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
              execEffectRange
                batch ren clipRef scale rootTarget spans cmds uBytes
                (drawCmd vp ip) 0 (-1) 0 0 mDamage 0 (U.length cmds)
          flushRenderBatch batch
          applyClipState batch clipRef ren ClipNone
      )
      `onException` discardRenderBatch batch

{-# INLINE shiftRect #-}
shiftRect :: Float -> Float -> Rect -> Rect
shiftRect dx dy (Rect x y w h) = Rect (x + dx) (y + dy) w h

spanEffectiveClip :: Maybe Rect -> Rect -> Maybe (Maybe Rect)
spanEffectiveClip mDamage clip@(Rect _ _ cw ch) =
  let !open = cw >= 1e8 || ch >= 1e8
   in case (mDamage, open) of
        (Nothing, True) -> Just Nothing
        (Nothing, False) -> Just (Just clip)
        (Just dmg, True) -> Just (Just dmg)
        (Just dmg, False) -> Just <$> rectIntersect dmg clip

-- | Recursively execute commands in @[rangeStart, rangeEnd)@ belonging to
-- target @parentId@ (@0@ = root target, @> 0@ = an enclosing 'EffectSpan').
-- Child spans redirect into 64px-bucketed pooled offscreen textures, run
-- multi-pass ping-pong shader chains, and composite back onto the parent.
{-# NOINLINE execEffectRange #-}
execEffectRange ::
  RenderBatch ->
  Ptr SDL_Renderer ->
  IORef ClipState ->
  Float ->
  Ptr SDL_Texture ->
  SmallArray EffectSpan ->
  U.Vector DrawCmd ->
  ByteArray ->
  (Maybe Rect -> Float -> Float -> DrawCmd -> IO ()) ->
  Int ->
  Int ->
  Float ->
  Float ->
  Maybe Rect ->
  Int ->
  Int ->
  IO ()
execEffectRange batch ren clipRef scale rootTarget spans cmds uBytes emitCmd parentId parentSlot parentOx parentOy parentDmg rangeStart rangeEnd =
  go rangeStart directChildren
  where
    directChildren =
      sortOn (\sp -> (esCmdStart sp, esId sp))
        [ sp
        | i <- [0 .. sizeofSmallArray spans - 1]
        , let sp = indexSmallArray spans i
        , esParent sp == parentId
        , esCmdStart sp >= rangeStart
        , esCmdEnd sp <= rangeEnd
        ]

    emitPlain from to = loop from
      where
        loop !i
          | i >= to = pure ()
          | otherwise = emitCmd parentDmg parentOx parentOy (U.unsafeIndex cmds i) >> loop (i + 1)

    go !cur [] = emitPlain cur rangeEnd
    go !cur (sp : rest) = do
      let !s = max cur (esCmdStart sp)
          !e = min rangeEnd (esCmdEnd sp)
      when (s > cur) (emitPlain cur s)
      when (e > s) $ runSpan sp s e
      go (max cur e) rest

    runSpan sp s e = do
      let Rect bx by bw bh = esBounds sp
          V2 mx my = esMargin sp
          expanded@(Rect ex ey ew eh) = Rect (bx - mx) (by - my) (bw + 2 * mx) (bh + 2 * my)
          passes = esPasses sp
          !nPasses = sizeofSmallArray passes
          inDamage = case parentDmg of
            Nothing -> True
            Just dmg -> case rectIntersect dmg expanded of
              Just _ -> True
              Nothing -> False
      when inDamage $
        if nPasses <= 0 || ew <= 0 || eh <= 0
          then
            execEffectRange
              batch ren clipRef scale rootTarget spans cmds uBytes
              emitCmd (esId sp) parentSlot parentOx parentOy parentDmg s e
          else do
            let !px0 = floor (ex * scale) :: Int
                !py0 = floor (ey * scale) :: Int
                !px1 = ceiling ((ex + ew) * scale) :: Int
                !py1 = ceiling ((ey + eh) * scale) :: Int
                !pw = max 1 (px1 - px0)
                !ph = max 1 (py1 - py0)
                !ox = fromIntegral px0 / scale
                !oy = fromIntegral py0 / scale
            (slot0, cap0W, cap0H) <- acquireTarget (rbBatch batch) pw ph
            if slot0 < 0
              then
                -- Graceful fallback if offscreen target allocation fails.
                execEffectRange
                  batch ren clipRef scale rootTarget spans cmds uBytes
                  emitCmd (esId sp) parentSlot parentOx parentOy parentDmg s e
              else do
                let restoreParent = do
                      flushRenderBatch batch
                      void $ batchBindTarget (rbBatch batch) (fromIntegral parentSlot) rootTarget 0
                      batchSetOrigin (rbBatch batch) (realToFrac parentOx) (realToFrac parentOy)
                      writeIORef clipRef ClipNone
                      void $ setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
                    runOffscreen = do
                      flushRenderBatch batch
                      okBind <- batchBindTarget (rbBatch batch) (fromIntegral slot0) nullPtr 1
                      when (okBind /= 0) $ do
                        writeIORef clipRef ClipNone
                        batchSetOrigin (rbBatch batch) (realToFrac ox) (realToFrac oy)
                        execEffectRange
                          batch ren clipRef scale rootTarget spans cmds uBytes
                          emitCmd (esId sp) slot0 ox oy Nothing s e
                        flushRenderBatch batch
                        let !nodePx = bx * scale - fromIntegral px0
                            !nodePy = by * scale - fromIntegral py0
                            !nodePw = max 1.0e-4 (bw * scale)
                            !nodePh = max 1.0e-4 (bh * scale)
                            !destPx = fromIntegral px0 - parentOx * scale
                            !destPy = fromIntegral py0 - parentOy * scale
                            runPasses !srcSlot !srcCapW !srcCapH !dstSlot !dstCapW !dstCapH !passIdx
                              | passIdx >= nPasses = pure ()
                              | otherwise = do
                                  let !u1 = fromIntegral pw / fromIntegral srcCapW
                                      !v1 = fromIntegral ph / fromIntegral srcCapH
                                      !nodeU0 = nodePx / fromIntegral srcCapW
                                      !nodeV0 = nodePy / fromIntegral srcCapH
                                      !nodeUSpan = nodePw / fromIntegral srcCapW
                                      !nodeVSpan = nodePh / fromIntegral srcCapH
                                      sc = indexSmallArray passes passIdx
                                  if passIdx < nPasses - 1 && dstSlot >= 0
                                    then do
                                      okDst <- batchBindTarget (rbBatch batch) (fromIntegral dstSlot) nullPtr 1
                                      when (okDst /= 0) $ do
                                        writeIORef clipRef ClipNone
                                        compositeEffectPass
                                          (rbBatch batch) srcSlot
                                          0 0 (fromIntegral pw) (fromIntegral ph)
                                          0 0 u1 v1 1 sc uBytes
                                          nodeU0 nodeV0 nodeUSpan nodeVSpan
                                          (fromIntegral srcCapW) (fromIntegral srcCapH)
                                        runPasses dstSlot dstCapW dstCapH srcSlot srcCapW srcCapH (passIdx + 1)
                                    else do
                                      okPar <- batchBindTarget (rbBatch batch) (fromIntegral parentSlot) rootTarget 0
                                      when (okPar /= 0) $ do
                                        writeIORef clipRef ClipNone
                                        void $ setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
                                        for_ (spanEffectiveClip parentDmg (esClip sp)) $ \mClip -> do
                                          let shiftedClip =
                                                if parentOx == 0 && parentOy == 0
                                                  then mClip
                                                  else fmap (shiftRect (-parentOx) (-parentOy)) mClip
                                          applyClipState batch clipRef ren (maybe ClipNone (toClipKey scale) shiftedClip)
                                          compositeEffectPass
                                            (rbBatch batch) srcSlot
                                            destPx destPy (fromIntegral pw) (fromIntegral ph)
                                            0 0 u1 v1 0 sc uBytes
                                            nodeU0 nodeV0 nodeUSpan nodeVSpan
                                            (fromIntegral srcCapW) (fromIntegral srcCapH)
                        if nPasses > 1
                          then do
                            (slot1, cap1W, cap1H) <- acquireTarget (rbBatch batch) pw ph
                            runPasses slot0 cap0W cap0H slot1 cap1W cap1H 0
                              `finally` when (slot1 >= 0) (batchReleaseTarget (rbBatch batch) (fromIntegral slot1))
                          else
                            runPasses slot0 cap0W cap0H (-1) 0 0 0
                runOffscreen
                  `finally` (restoreParent >> batchReleaseTarget (rbBatch batch) (fromIntegral slot0))

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
    withOptText (spMsl prog) $ \mslPtr mslLen ->
      if uLen > 0
        then allocaBytes uLen $ \uPtr -> do
          copyByteArrayToAddr uPtr uniformBytes uOff uLen
          void $
            batchDrawShader batchPtr vp vc ip idxStart idxCount tex hasDmg dx dy dw dh
              (scHash sc) spvPtr spvLen mslPtr mslLen needsTex uPtr (fromIntegral uLen)
        else
          void $
            batchDrawShader batchPtr vp vc ip idxStart idxCount tex hasDmg dx dy dw dh
              (scHash sc) spvPtr spvLen mslPtr mslLen needsTex nullPtr 0

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
compositeEffectPass ::
  Ptr () ->
  Int ->
  Float -> Float -> Float -> Float ->
  Float -> Float -> Float -> Float ->
  CInt ->
  ShaderCall ->
  ByteArray ->
  Float -> Float -> Float -> Float ->
  Float -> Float ->
  IO ()
compositeEffectPass
  batchPtr srcSlot
  px py pw ph
  u0 v0 u1 v1
  blendCopy sc uniformBytes
  nodeU0 nodeV0 nodeUSpan nodeVSpan
  targetW targetH = do
    let prog = scProgram sc
        !uOff = scUniformOffset sc
        !uLen = max sysUniformBytes (min (scUniformLength sc) (max 0 (sizeofByteArray uniformBytes - uOff)))
        !copyLen = min uLen (max 0 (sizeofByteArray uniformBytes - uOff))
        withOptBS Nothing k = k nullPtr 0
        withOptBS (Just bs) k = BSU.unsafeUseAsCStringLen bs $ \(p, l) -> k (castPtr p) (fromIntegral l)
        withOptText Nothing k = k nullPtr 0
        withOptText (Just t) k = withOptBS (Just (TE.encodeUtf8 t)) k
    withOptBS (spSpirv prog) $ \spvPtr spvLen ->
      withOptText (spMsl prog) $ \mslPtr mslLen ->
        allocaBytes uLen $ \uPtr -> do
          when (copyLen > 0) $ copyByteArrayToAddr uPtr uniformBytes uOff copyLen
          pokeByteOff uPtr 16 (CFloat nodeU0)
          pokeByteOff uPtr 20 (CFloat nodeV0)
          pokeByteOff uPtr 24 (CFloat nodeUSpan)
          pokeByteOff uPtr 28 (CFloat nodeVSpan)
          pokeByteOff uPtr 40 (CFloat targetW)
          pokeByteOff uPtr 44 (CFloat targetH)
          void $
            batchCompositeEffect
              batchPtr (fromIntegral srcSlot)
              (realToFrac px) (realToFrac py) (realToFrac pw) (realToFrac ph)
              (realToFrac u0) (realToFrac v0) (realToFrac u1) (realToFrac v1)
              blendCopy (scHash sc)
              spvPtr spvLen mslPtr mslLen
              uPtr (fromIntegral uLen)
