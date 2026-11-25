-- | A frame's draw data as WebGL draw commands: the texture uploads it
-- needs, then each command clipped to its rect and the damage, as
-- nano-ui-sdl's @NanoUI.Sdl.Internal.Render@ submits them to SDL.
module NanoUI.SdlWeb.Internal.Render
  ( Renderer
  , newRenderer
  , freeRenderer
  , forgetTextures
  , syncTextures
  , renderDrawData
  , DrawCommand (..)
  , drawCommands'
  , snapDamage
  ) where

import Control.Monad (forM_, unless, when)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import qualified Data.Map.Strict as Map
import Data.Primitive.ByteArray (ByteArray, copyByteArrayToAddr, sizeofByteArray)
import Data.Primitive.SmallArray (indexSmallArray, sizeofSmallArray, smallArrayFromListN)
import qualified Data.Text.Encoding as TE
import qualified Data.Vector.Unboxed as U
import Data.Word (Word64, Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (allocaBytes, free, mallocBytes, reallocBytes)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import NanoUI (Rect (..), colorA, colorB, colorG, colorR)
import NanoUI.Backend
  ( AtlasUpload (..)
  , Context
  , Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , EffectDriver (..)
  , EffectPassDraw (..)
  , ShaderCall (..)
  , ShaderProgram (..)
  , atlasChanges
  , atlasTextureId
  , clipPixelEdges
  , cmdEffectiveClip
  , execEffectSpans
  , snapDamage
  , textureGlyphPage
  , withEffectUniforms
  )
import NanoUI.SdlWeb.Internal.Font (GlyphAtlas, glyphAtlasSize, glyphPageUploads)
import NanoUI.SdlWeb.Internal.Host (WebHost (..), commandRecordSize, imageAtlasSlot)

-- | The session's draw state: the command buffer, reused across frames, the
-- image atlas generation on the GPU, and the registered shader cache.
data Renderer = Renderer
  { rCommands :: !(IORef (Ptr Word8, Int))
  -- ^ The buffer and its capacity in commands.
  , rImageGeneration :: !(IORef Int)
  , rShaders :: !(IORef (Map.Map Word64 Int, Int))
  -- ^ Registered shader slots by program hash (0 marks a failed compile),
  -- paired with the next 1-based slot ID.
  }

newRenderer :: IO Renderer
newRenderer = Renderer <$> newIORef (nullPtr, 0) <*> newIORef 0 <*> newIORef (Map.empty, 1)

freeRenderer :: Renderer -> IO ()
freeRenderer r = do
  (buf, _) <- readIORef (rCommands r)
  free buf
  writeIORef (rCommands r) (nullPtr, 0)

-- | Upload the image atlas in full with the next 'syncTextures' and drop
-- cached shader slots after the GPU lost its WebGL context. The glyph pages
-- are marked on the atlas ('NanoUI.SdlWeb.Internal.Font.markGlyphPagesDirty').
forgetTextures :: Renderer -> IO ()
forgetTextures r = do
  writeIORef (rImageGeneration r) 0
  writeIORef (rShaders r) (Map.empty, 1)

-- | Upload what changed in the glyph pages and the image atlas since the
-- last call, before a frame that may draw from them.
syncTextures :: WebHost -> Renderer -> GlyphAtlas -> Context -> IO ()
syncTextures host r glyphs ctx = do
  let page = round glyphAtlasSize
  uploads <- glyphPageUploads glyphs
  forM_ uploads $ \(slot, pixels, (x, y, w, h)) ->
    whUploadTexture host slot page page pixels page x y w h
  gen <- readIORef (rImageGeneration r)
  atlasChanges ctx gen >>= \case
    Nothing -> pure ()
    Just (w, h, pixels, gen', upload) -> do
      let rects = case upload of
            AtlasWhole -> [(0, 0, w, h)]
            -- A texture never made, or lost, takes the whole atlas.
            AtlasRegions rs -> if gen == 0 then [(0, 0, w, h)] else rs
      withForeignPtr pixels $ \p ->
        forM_ rects $ \(x, y, rw, rh) ->
          whUploadTexture host imageAtlasSlot w h p w x y rw rh
      writeIORef (rImageGeneration r) gen'

-- | One WebGL draw: texture slot (-1 for none), index range, optional scissor
-- in device pixels from the top left, and shader call index (0 for standard
-- geometry; 1-based index into 'drawShaderCalls' otherwise).
data DrawCommand = DrawCommand
  { dcSlot :: !Int
  , dcFirst :: !Int
  , dcCount :: !Int
  , dcScissor :: !(Maybe (Int, Int, Int, Int))
  , dcShaderCall :: !Int
  }
  deriving (Eq, Show)

-- | The draws for a frame's commands over its damage, at a scale, into a
-- drawing buffer of a pixel size. Each command is clipped to its own rect
-- and to the damage, in the device pixels 'clipPixelEdges' gives, as
-- nano-ui-sdl clips; one with nothing left is dropped. A command that
-- continues the last one's indices with the same texture, shader and scissor
-- joins it, which turns most of a frame into a few draws.
drawCommands' :: Float -> (Int, Int) -> Damage -> DrawData -> [DrawCommand]
drawCommands' scale (pw, ph) damage dd =
  drawCommandsSlice scale (pw, ph) 0 0 mDamage (drawCommands dd) 0 (U.length (drawCommands dd))
  where
    mDamage = case damage of
      DamageFull -> Nothing
      DamageClip r -> Just r

drawCommandsSlice ::
  Float ->
  (Int, Int) ->
  Float ->
  Float ->
  Maybe Rect ->
  U.Vector DrawCmd ->
  Int ->
  Int ->
  [DrawCommand]
drawCommandsSlice scale (pw, ph) ox oy mDamage rawCmds lo hi = reverse (go lo [])
  where
    go !i acc
      | i >= hi = acc
      | otherwise = go (i + 1) (step acc (U.unsafeIndex rawCmds i))
    step acc cmd
      | count < 3 = acc
      | otherwise = case cmdEffectiveClip mDamage cmd of
          Nothing -> acc
          Just mClip ->
            let shifted = if ox == 0 && oy == 0 then mClip else fmap (\(Rect x y w h) -> Rect (x - ox) (y - oy) w h) mClip
                scissor = fmap toPixels shifted
             in case scissor of
                  Just (_, _, sw, sh) | sw <= 0 || sh <= 0 -> acc
                  _ ->
                    push
                      (DrawCommand (slotOf (cmdTextureId cmd)) (fromIntegral (cmdIndexOffset cmd)) count scissor (fromIntegral (cmdShader cmd)))
                      acc
      where
        count = fromIntegral (cmdIndexCount cmd)
    push c (prev : rest)
      | dcShaderCall prev == 0
          && dcShaderCall c == 0
          && dcSlot prev == dcSlot c
          && dcScissor prev == dcScissor c
          && dcFirst prev + dcCount prev == dcFirst c =
          prev {dcCount = dcCount prev + dcCount c} : rest
    push c acc = c : acc
    toPixels = toPixelScissor scale (pw, ph)
    slotOf tex
      | page >= 0 = page
      | tex == atlasTextureId = imageAtlasSlot
      | otherwise = -1
      where
        page = textureGlyphPage tex

-- | Convert a logical clip rectangle into a clamped device-pixel scissor
-- @(x, y, width, height)@ within a @(pw, ph)@ target.
toPixelScissor :: Float -> (Int, Int) -> Rect -> (Int, Int, Int, Int)
toPixelScissor scale (pw, ph) r =
  let (x0, y0, x1, y1) = clipPixelEdges scale r
      cx0 = max 0 x0
      cy0 = max 0 y0
   in (cx0, cy0, min pw x1 - cx0, min ph y1 - cy0)

-- | Look up or lazily register a GLSL ES 300 shader program on the host,
-- returning its 1-based slot (or 0 if unavailable or compilation failed).
ensureShaderSlot :: WebHost -> Renderer -> ShaderCall -> IO Int
ensureShaderSlot host r sc = do
  (cache, nextSlot) <- readIORef (rShaders r)
  case Map.lookup (scHash sc) cache of
    Just slot -> pure slot
    Nothing -> case spGlslEs300 (scProgram sc) of
      Nothing -> do
        writeIORef (rShaders r) (Map.insert (scHash sc) 0 cache, nextSlot)
        pure 0
      Just src -> do
        let bytes = TE.encodeUtf8 src
        ok <- unsafeUseAsCStringLen bytes $ \(p, len) ->
          whRegisterShader host nextSlot (castPtr p) len
        let slot = if ok then nextSlot else 0
            next' = if ok then nextSlot + 1 else nextSlot
        writeIORef (rShaders r) (Map.insert (scHash sc) slot cache, next')
        pure slot

-- | Draw a frame over its damage. A full frame clears to the draw data's
-- colour first; a partial one paints over the frame the drawing buffer
-- kept, as the draw list starts a clip frame with its own backdrop.
renderDrawData :: WebHost -> Renderer -> Float -> (Int, Int) -> Damage -> DrawData -> IO ()
renderDrawData host r scale (pw, ph) damage dd = do
  let full = case damage of DamageFull -> True; DamageClip _ -> False
      mDamage = case damage of DamageFull -> Nothing; DamageClip rect -> Just rect
      clear = drawClear dd
      unit v = realToFrac v :: Double
      calls = drawShaderCalls dd
      uBytes = drawUniformBytes dd
      uSize = sizeofByteArray uBytes
      spans = drawEffectSpans dd
      uploadUniforms =
        when (sizeofSmallArray calls > 0 && uSize > 0) $
          allocaBytes uSize $ \uPtr -> do
            copyByteArrayToAddr uPtr uBytes 0 uSize
            whUploadUniforms host uPtr uSize
  whBeginFrame host full (unit (colorR clear) / 255) (unit (colorG clear) / 255) (unit (colorB clear) / 255) (unit (colorA clear) / 255) (realToFrac scale) pw ph
  if sizeofSmallArray spans == 0
    then do
      let cmds = drawCommands' scale (pw, ph) damage dd
      unless (null cmds) $ do
        uploadUniforms
        withForeignPtr (drawVertices dd) $ \vp ->
          withForeignPtr (drawIndices dd) $ \ip ->
            submitCommands host r dd vp ip cmds
    else do
      uploadUniforms
      let rawCmds = drawCommands dd
      withForeignPtr (drawVertices dd) $ \vp ->
        withForeignPtr (drawIndices dd) $ \ip -> do
          let drv =
                EffectDriver
                  { edScale = scale
                  , edFlipY = True
                  , edPreparePasses = \passes -> do
                      let !n = sizeofSmallArray passes
                      slots <- mapM (ensureShaderSlot host r . indexSmallArray passes) [0 .. n - 1]
                      pure (if any (<= 0) slots then Nothing else Just (smallArrayFromListN n slots))
                  , edAcquireTarget = acquireWebTarget host
                  , edReleaseTarget = whReleaseTarget host
                  , edBindTarget = \slot clr ox oy ->
                      True <$ whBindTarget host slot clr (realToFrac ox) (realToFrac oy)
                  , edFlush = pure ()
                  , edEmitPlain = \_ cw ch ox oy curDmg from to ->
                      submitCommands host r dd vp ip (drawCommandsSlice scale (cw, ch) ox oy curDmg rawCmds from to)
                  , edAfterSpanContent = \_ -> pure ()
                  , edCompositePass = \shSlot sc pd dstW dstH -> do
                      let mScissor = fmap (toPixelScissor scale (dstW, dstH)) (epdClip pd)
                          nonEmpty = case mScissor of
                            Just (_, _, sw, sh) -> sw > 0 && sh > 0
                            Nothing -> True
                      when nonEmpty $
                        compositeWebPass host shSlot mScissor sc uBytes pd
                  }
          execEffectSpans drv spans 0 (-1) (pw, ph) 0 0 mDamage 0 (U.length rawCmds)

submitCommands :: WebHost -> Renderer -> DrawData -> Ptr Word8 -> Ptr Word8 -> [DrawCommand] -> IO ()
submitCommands host r dd vp ip cmds =
  unless (null cmds) $ do
    let n = length cmds
        calls = drawShaderCalls dd
    buf <- commandBuffer r n
    let ints = castPtr buf :: Ptr Int32
        resolveShader callIdx
          | callIdx <= 0 || callIdx > sizeofSmallArray calls = pure (0, 0, 0)
          | otherwise = do
              let sc = indexSmallArray calls (callIdx - 1)
              shSlot <- ensureShaderSlot host r sc
              pure (shSlot, scUniformOffset sc, scUniformLength sc)
    forM_ (zip [0 ..] cmds) $ \(i, DrawCommand slot first count scissor callIdx) -> do
      (shSlot, uOff, uLen) <- resolveShader callIdx
      let put k v = pokeElemOff ints (i * (commandRecordSize `div` 4) + k) (fromIntegral v)
          (on, sx, sy, sw, sh) = maybe (0, 0, 0, 0, 0) (\(x, y, w, h) -> (1 :: Int, x, y, w, h)) scissor
      put 0 slot
      put 1 first
      put 2 count
      put 3 on
      put 4 sx
      put 5 sy
      put 6 sw
      put 7 sh
      put 8 shSlot
      put 9 uOff
      put 10 uLen
      put 11 (0 :: Int)
    whDraw host vp (drawVertexCount dd) ip (drawIndexCount dd) buf n

acquireWebTarget :: WebHost -> Int -> Int -> IO (Int, Int, Int)
acquireWebTarget host minW minH =
  allocaArray 2 $ \capsPtr -> do
    slot <- whAcquireTarget host minW minH capsPtr
    if slot < 0
      then pure (-1, 0, 0)
      else do
        cw <- fromIntegral <$> peekElemOff capsPtr 0
        ch <- fromIntegral <$> peekElemOff capsPtr 1
        pure (slot, cw, ch)

{-# NOINLINE compositeWebPass #-}
compositeWebPass ::
  WebHost ->
  Int ->
  Maybe (Int, Int, Int, Int) ->
  ShaderCall ->
  ByteArray ->
  EffectPassDraw ->
  IO ()
compositeWebPass host shSlot mScissor sc uniformBytes pd = do
  let (hasSc, sx, sy, sw, sh) = case mScissor of
        Nothing -> (0 :: Int32, 0, 0, 0, 0)
        Just (x, y, w, h) -> (1, fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h)
  allocaArray 8 $ \rvPtr ->
    allocaArray 5 $ \scPtr ->
      withEffectUniforms sc uniformBytes pd $ \uPtr uLen -> do
        pokeArray rvPtr [epdDstX pd, epdDstY pd, epdDstW pd, epdDstH pd, epdU0 pd, epdV0 pd, epdU1 pd, epdV1 pd]
        pokeArray scPtr [hasSc, sx, sy, sw, sh]
        whCompositeEffect host (epdSrcSlot pd) shSlot (epdIntermediate pd) rvPtr scPtr uPtr uLen

-- | The command buffer, grown to hold at least @n@ commands.
commandBuffer :: Renderer -> Int -> IO (Ptr Word8)
commandBuffer r n = do
  (buf, cap) <- readIORef (rCommands r)
  if n <= cap && buf /= nullPtr
    then pure buf
    else do
      let cap' = max 64 (max n (cap * 2))
      buf' <- if buf == nullPtr then mallocBytes (cap' * commandRecordSize) else reallocBytes buf (cap' * commandRecordSize)
      when (buf' == nullPtr) $ fail "nano-ui-sdl-web: out of memory for draw commands"
      buf' <$ writeIORef (rCommands r) (buf', cap')
