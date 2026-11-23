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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import qualified Data.Vector.Unboxed as U
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free, mallocBytes, reallocBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (pokeElemOff)
import NanoUI (colorA, colorB, colorG, colorR)
import NanoUI.Backend
  ( AtlasUpload (..)
  , Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , atlasTextureId
  , cmdEffectiveClip
  , pixelEdges
  , snapDamage
  , textureGlyphPage
  )
import NanoUI.Backend (Context, atlasChanges)
import NanoUI.SdlWeb.Internal.Font (GlyphAtlas, glyphAtlasSize, glyphPageUploads)
import NanoUI.SdlWeb.Internal.Host (WebHost (..), commandRecordSize, imageAtlasSlot)

-- | The session's draw state: the command buffer, reused across frames, and
-- the image atlas generation on the GPU.
data Renderer = Renderer
  { rCommands :: !(IORef (Ptr Word8, Int))
  -- ^ The buffer and its capacity in commands.
  , rImageGeneration :: !(IORef Int)
  }

newRenderer :: IO Renderer
newRenderer = Renderer <$> newIORef (nullPtr, 0) <*> newIORef 0

freeRenderer :: Renderer -> IO ()
freeRenderer r = do
  (buf, _) <- readIORef (rCommands r)
  free buf
  writeIORef (rCommands r) (nullPtr, 0)

-- | Upload the image atlas in full with the next 'syncTextures', after the
-- GPU lost its textures. The glyph pages are marked on the atlas
-- ('NanoUI.SdlWeb.Internal.Font.markGlyphPagesDirty').
forgetTextures :: Renderer -> IO ()
forgetTextures r = writeIORef (rImageGeneration r) 0

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

-- | One WebGL draw: texture slot (-1 for none), index range, and the
-- scissor in device pixels from the top left, if any.
data DrawCommand = DrawCommand
  { dcSlot :: !Int
  , dcFirst :: !Int
  , dcCount :: !Int
  , dcScissor :: !(Maybe (Int, Int, Int, Int))
  }
  deriving (Eq, Show)

-- | The draws for a frame's commands over its damage, at a scale, into a
-- drawing buffer of a pixel size. Each command is clipped to its own rect
-- and to the damage; one with nothing left is dropped. A command that
-- continues the last one's indices with the same texture and scissor joins
-- it, which turns most of a frame into a few draws.
drawCommands' :: Float -> (Int, Int) -> Damage -> DrawData -> [DrawCommand]
drawCommands' scale (pw, ph) damage dd = reverse (U.foldl' step [] (drawCommands dd))
  where
    mDamage = case damage of
      DamageFull -> Nothing
      DamageClip r -> Just r
    step acc cmd
      | count < 3 = acc
      | otherwise = case cmdEffectiveClip mDamage cmd of
          Nothing -> acc
          Just mClip ->
            let scissor = fmap toPixels mClip
             in case scissor of
                  Just (_, _, sw, sh) | sw <= 0 || sh <= 0 -> acc
                  _ -> push (DrawCommand (slotOf (cmdTextureId cmd)) (fromIntegral (cmdIndexOffset cmd)) count scissor) acc
      where
        count = fromIntegral (cmdIndexCount cmd)
    push c (prev : rest)
      | dcSlot prev == dcSlot c && dcScissor prev == dcScissor c && dcFirst prev + dcCount prev == dcFirst c =
          prev {dcCount = dcCount prev + dcCount c} : rest
    push c acc = c : acc
    toPixels r =
      let (x0, y0, x1, y1) = pixelEdges scale r
          cx0 = max 0 x0
          cy0 = max 0 y0
       in (cx0, cy0, min pw x1 - cx0, min ph y1 - cy0)
    slotOf tex
      | page >= 0 = page
      | tex == atlasTextureId = imageAtlasSlot
      | otherwise = -1
      where
        page = textureGlyphPage tex

-- | Draw a frame over its damage. A full frame clears to the draw data's
-- colour first; a partial one paints over the frame the drawing buffer
-- kept, as the draw list starts a clip frame with its own backdrop.
renderDrawData :: WebHost -> Renderer -> Float -> (Int, Int) -> Damage -> DrawData -> IO ()
renderDrawData host r scale (pw, ph) damage dd = do
  let cmds = drawCommands' scale (pw, ph) damage dd
      n = length cmds
      full = case damage of DamageFull -> True; DamageClip _ -> False
      clear = drawClear dd
      unit v = realToFrac v :: Double
  whBeginFrame host full (unit (colorR clear) / 255) (unit (colorG clear) / 255) (unit (colorB clear) / 255) (unit (colorA clear) / 255) (realToFrac scale) pw ph
  unless (n == 0) $ do
    buf <- commandBuffer r n
    let ints = castPtr buf :: Ptr Int32
    forM_ (zip [0 ..] cmds) $ \(i, DrawCommand slot first count scissor) -> do
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
    withForeignPtr (drawVertices dd) $ \vp ->
      withForeignPtr (drawIndices dd) $ \ip ->
        whDraw host vp (drawVertexCount dd) ip (drawIndexCount dd) buf n

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
