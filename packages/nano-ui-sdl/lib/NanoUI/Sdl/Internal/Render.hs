-- | Submit nano-ui draw batches to SDL with texture binding and damage clipping.
module NanoUI.Sdl.Internal.Render
  ( RenderBatch
  , newRenderBatch
  , destroyRenderBatch
  , flushRenderBatch
  , renderDrawDataPass
  , snapDamage
  )
where

import Control.Exception (onException)
import Control.Monad (void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
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
  ( Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , damageIsEmpty
  , textureGlyphPage
  )
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

{-# INLINE snapDamage #-}
snapDamage :: Float -> Damage -> Damage
snapDamage _ DamageFull = DamageFull
snapDamage scale (DamageClip (Rect x y w h)) =
  let
    px = fromIntegral (floor (x * scale) :: Int) / scale
    py = fromIntegral (floor (y * scale) :: Int) / scale
    pw = fromIntegral (ceiling ((x + w) * scale) :: Int) / scale - px
    ph = fromIntegral (ceiling ((y + h) * scale) :: Int) / scale - py
   in
    DamageClip (Rect px py pw ph)

{-# INLINE toClipKey #-}
toClipKey :: Rect -> ClipState
toClipKey (Rect x y w h) =
  let
    px = floor x :: Int
    py = floor y :: Int
    x1 = ceiling (x + w) :: Int
    y1 = ceiling (y + h) :: Int
   in
    ClipKey px py (max 1 (x1 - px)) (max 1 (y1 - py))

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
        let
          rect = rbClipRect batch
        poke
          rect
          (SDL_Rect (fromIntegral px) (fromIntegral py) (fromIntegral pw) (fromIntegral ph))
        setRenderClipRect ren (PtrConst.unsafeFromPtr rect)

-- | Draw every command in layer order, clipped to its own rect and to
-- the damage. A full repaint clears the target to the colour first.
renderDrawDataPass ::
  RenderBatch
  -> Ptr SDL_Renderer
  -> Color
  -> DrawData
  -> ImageAtlas
  -> (Int -> Ptr SDL_Texture)
  -- ^ The glyph atlas's texture for each of its pages.
  -> Damage
  -> IO ()
renderDrawDataPass batch ren bg drawData images glyphTex damage =
  when (not (damageIsEmpty damage)) $ do
    clipRef <- newIORef ClipNone
    void $ setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
    clip <- case damage of
      DamageFull -> do
        void $ setRenderDrawColorSafe ren (colorR bg) (colorG bg) (colorB bg) (colorA bg)
        Nothing <$ renderClearSafe ren
      -- The draw list starts a clip frame with its own window backdrop, so
      -- a clip needs no clear here.
      DamageClip r -> Just r <$ applyClipState batch clipRef ren (toClipKey r)
    let
      vc = drawVertexCount drawData
      cmds = drawCommands drawData
    withForeignPtr (drawVertices drawData) $ \vp ->
      withForeignPtr (drawIndices drawData) $ \ip ->
        let
          goCmd !i
            | i >= U.length cmds = pure ()
            | otherwise = do
                drawCmd batch ren vp vc ip images glyphTex clip clipRef (U.unsafeIndex cmds i)
                goCmd (i + 1)
         in
          goCmd 0
    applyClipState batch clipRef ren ClipNone

{-# INLINE drawCmd #-}
drawCmd ::
  RenderBatch
  -> Ptr SDL_Renderer
  -> Ptr Word8
  -> Int
  -> Ptr Word8
  -> ImageAtlas
  -> (Int -> Ptr SDL_Texture)
  -> Maybe Rect
  -> IORef ClipState
  -> DrawCmd
  -> IO ()
drawCmd batch ren vp vc ip images glyphTex mDamage clipRef cmd = do
  let
    !count = fromIntegral (cmdIndexCount cmd)
    !cmdRect = Rect (cmdClipX cmd) (cmdClipY cmd) (cmdClipW cmd) (cmdClipH cmd)
    !cmdOpen = cmdClipW cmd >= 1e8 || cmdClipH cmd >= 1e8
    live = case (mDamage, cmdOpen) of
      (Nothing, _) -> Just cmdRect
      (Just dmg, True) -> Just dmg
      (Just dmg, False) -> rectIntersect dmg cmdRect
  when (count >= 3) $
    case live of
      Nothing -> pure ()
      Just clip -> do
        if cmdOpen && mDamage == Nothing
          then applyClipState batch clipRef ren ClipNone
          else applyClipState batch clipRef ren (toClipKey clip)
        let
          !start = fromIntegral (cmdIndexOffset cmd)
          !texId = cmdTextureId cmd
        tex <-
          if textureGlyphPage texId >= 0
            then pure (glyphTex (textureGlyphPage texId))
            else fromMaybe nullPtr <$> lookupImage images texId
        let
          (hasDamage, dx, dy, dw, dh) = case mDamage of
            Nothing -> (0, 0, 0, 0, 0)
            Just (Rect x y w h) -> (1, realToFrac x, realToFrac y, realToFrac w, realToFrac h)
        batchDrawRange
          (rbBatch batch)
          vp
          (fromIntegral vc)
          ip
          start
          count
          tex
          hasDamage
          dx
          dy
          dw
          dh

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

foreign import ccall unsafe "nano_ui_batch_create"
  batchCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_batch_destroy"
  batchDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_flush"
  batchFlush :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_draw_range"
  batchDrawRange ::
    Ptr ()
    -> Ptr Word8
    -> CInt
    -> Ptr Word8
    -> CInt
    -> CInt
    -> Ptr SDL_Texture
    -> CInt
    -> CFloat
    -> CFloat
    -> CFloat
    -> CFloat
    -> IO ()
