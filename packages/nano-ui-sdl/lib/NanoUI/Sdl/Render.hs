module NanoUI.Sdl.Render
  ( RenderBatch
  , newRenderBatch
  , destroyRenderBatch
  , flushRenderBatch
  , renderDrawDataPass
  , snapDamage
  ) where

import NanoUI.Sdl.Image (ImageAtlas, lookupImage)

import Control.Monad (void, when)
import Data.Bits (shiftR, (.&.))
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.PrimArray (indexPrimArray, sizeofPrimArray)
import Data.Word (Word8)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (poke)
import NanoUI (Color (..), Rect (..), rectIntersect)
import NanoUI.Testing
  ( Damage (..)
  , DrawCmd (..)
  , DrawData (..)
  , LayerSlice (..)
  , damageIsEmpty
  , glyphAtlasTextureId
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
  | ClipKey {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
  deriving (Eq)

{-# INLINE snapDamage #-}
snapDamage :: Float -> Damage -> Damage
snapDamage _ DamageFull = DamageFull
snapDamage scale (DamageClip (Rect x y w h)) =
  let px = fromIntegral (floor (x * scale) :: Int) / scale
      py = fromIntegral (floor (y * scale) :: Int) / scale
      pw = fromIntegral (ceiling ((x + w) * scale) :: Int) / scale - px
      ph = fromIntegral (ceiling ((y + h) * scale) :: Int) / scale - py
   in DamageClip (Rect px py pw ph)

{-# INLINE toClipKey #-}
toClipKey :: Rect -> ClipState
toClipKey (Rect x y w h) =
  let px = floor x :: Int
      py = floor y :: Int
      x1 = ceiling (x + w) :: Int
      y1 = ceiling (y + h) :: Int
   in ClipKey px py (max 1 (x1 - px)) (max 1 (y1 - py))

applyClipState :: RenderBatch -> IORef ClipState -> Ptr SDL_Renderer -> ClipState -> IO ()
applyClipState batch ref ren next = do
  prev <- readIORef ref
  when (prev /= next) $ do
    flushRenderBatch batch
    writeIORef ref next
    void $ case next of
      ClipNone -> setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
      ClipKey px py pw ph -> do
        let rect = rbClipRect batch
        poke rect (SDL_Rect (fromIntegral px) (fromIntegral py) (fromIntegral pw) (fromIntegral ph))
        setRenderClipRect ren (PtrConst.unsafeFromPtr rect)

-- | Draw every command in layer-slice order, clipped to its own rect and to
-- the damage. A full repaint with a clear colour clears the target first.
renderDrawDataPass :: RenderBatch -> Ptr SDL_Renderer -> Maybe Color -> DrawData -> ImageAtlas -> Ptr SDL_Texture -> Damage -> IO ()
renderDrawDataPass batch ren mClear drawData images glyphTex damage =
  when (not (damageIsEmpty damage)) $ do
    clipRef <- newIORef ClipNone
    void $ setRenderClipRect ren (PtrConst.unsafeFromPtr nullPtr)
    case (mClear, damage) of
      (Just clearColor, DamageFull) -> do
        let (cr, cg, cb, ca) = unpackColor clearColor
        void $ setRenderDrawColorSafe ren cr cg cb ca
        void $ renderClearSafe ren
      (Just _clearColor, DamageClip r) ->
        -- The draw list starts a clip frame with its own window backdrop,
        -- so the clip needs no clear here.
        applyClipState batch clipRef ren (toClipKey r)
      (Nothing, DamageClip r) -> applyClipState batch clipRef ren (toClipKey r)
      (Nothing, DamageFull) -> pure ()
    let clip = case damage of
          DamageFull -> Nothing
          DamageClip r -> Just r
        vc = drawVertexCount drawData
        cmds = drawCommands drawData
        slices = drawLayerSlices drawData
    withForeignPtr (drawVertices drawData) $ \vp ->
      withForeignPtr (drawIndices drawData) $ \ip ->
        let goLy !li
              | li >= sizeofPrimArray slices = pure ()
              | otherwise = do
                  let LayerSlice off cnt = indexPrimArray slices li
                      goCmd !j
                        | j >= cnt = pure ()
                        | otherwise = do
                            drawCmd batch ren vp vc ip images glyphTex clip clipRef (indexPrimArray cmds (off + j))
                            goCmd (j + 1)
                  goCmd 0
                  goLy (li + 1)
         in goLy 0
    applyClipState batch clipRef ren ClipNone

{-# INLINE drawCmd #-}
drawCmd ::
  RenderBatch ->
  Ptr SDL_Renderer ->
  Ptr Word8 ->
  Int ->
  Ptr Word8 ->
  ImageAtlas ->
  Ptr SDL_Texture ->
  Maybe Rect ->
  IORef ClipState ->
  DrawCmd ->
  IO ()
drawCmd batch ren vp vc ip images glyphTex mDamage clipRef cmd = do
  let !count = fromIntegral (cmdIndexCount cmd)
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
        let !start = fromIntegral (cmdIndexOffset cmd)
            !texId = cmdTextureId cmd
        tex <-
          if texId == glyphAtlasTextureId
            then pure glyphTex
            else if texId > 0
              then maybe nullPtr id <$> lookupImage images texId
              else pure nullPtr
        batchDrawRange batch vp vc ip start count tex mDamage

{-# INLINE unpackColor #-}
unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral (w .&. 0xFF)
  )

-- | The C batch and a clip rect it passes to SDL, both owned for the session.
data RenderBatch = RenderBatch
  { rbBatch :: !(Ptr ())
  , rbClipRect :: !(Ptr SDL_Rect)
  }

-- | Create a persistent render batch. Reusing one batch across frames avoids
-- a C calloc/free pair per presented frame; flush after each render pass.
newRenderBatch :: Ptr SDL_Renderer -> IO RenderBatch
newRenderBatch ren = do
  p <- batchCreate ren
  if p == nullPtr
    then fail "nano_ui_batch_create failed"
    else RenderBatch p <$> malloc

destroyRenderBatch :: RenderBatch -> IO ()
destroyRenderBatch batch = do
  batchDestroy (rbBatch batch)
  free (rbClipRect batch)

flushRenderBatch :: RenderBatch -> IO ()
flushRenderBatch batch = batchFlush (rbBatch batch)

batchDrawRange ::
  RenderBatch ->
  Ptr Word8 ->
  Int ->
  Ptr Word8 ->
  Int ->
  Int ->
  Ptr SDL_Texture ->
  Maybe Rect ->
  IO ()
batchDrawRange batch verts vc indices start n tex mDmg =
  batchDrawRangeC
    (rbBatch batch)
    verts
    (ci vc)
    indices
    (ci start)
    (ci n)
    tex
    hasDmg
    (cf dx)
    (cf dy)
    (cf dw)
    (cf dh)
  where
    ci = fromIntegral
    (hasDmg, dx, dy, dw, dh) = case mDmg of
      Nothing -> (0, 0, 0, 0, 0)
      Just (Rect x y w h) -> (1, x, y, w, h)

cf :: Float -> CFloat
cf = realToFrac

foreign import ccall unsafe "nano_ui_batch_create"
  batchCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_batch_destroy"
  batchDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_flush"
  batchFlush :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_draw_range"
  batchDrawRangeC ::
    Ptr () ->
    Ptr Word8 ->
    CInt ->
    Ptr Word8 ->
    CInt ->
    CInt ->
    Ptr SDL_Texture ->
    CInt ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO ()
