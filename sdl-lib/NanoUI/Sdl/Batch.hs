module NanoUI.Sdl.Batch
  ( RenderBatch
  , withRenderBatch
  , batchFillSolid
  , batchTextureDst
  , batchTextureSized
  , flushRenderBatch
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Word (Word8)
import Foreign.C.Types (CFloat (..))
import Foreign.Ptr (Ptr, nullPtr)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)

newtype RenderBatch = RenderBatch (Ptr ())

withRenderBatch :: Ptr SDL_Renderer -> (RenderBatch -> IO a) -> IO a
withRenderBatch ren act =
  bracket
    (batchCreate ren >>= \p -> if p == nullPtr then fail "nano_ui_batch_create failed" else pure (RenderBatch p))
    (\(RenderBatch p) -> batchDestroy p)
    $ \rb -> do
      result <- act rb
      batchFlush (batchPtr rb)
      pure result
  where
    batchPtr (RenderBatch p) = p

batchFillSolid :: RenderBatch -> Word8 -> Word8 -> Word8 -> Word8 -> Float -> Float -> Float -> Float -> IO ()
batchFillSolid (RenderBatch p) r g b a x y w h =
  batchFillSolidC p r g b a (cf x) (cf y) (cf w) (cf h)

batchTextureDst ::
  RenderBatch ->
  Ptr () ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Word8 ->
  Word8 ->
  Word8 ->
  Word8 ->
  IO ()
batchTextureDst (RenderBatch p) tex atW atH dx dy dw dh u0 v0 u1 v1 r g b a =
  void $
    batchTextureDstC
      p
      tex
      (cf atW)
      (cf atH)
      (cf dx)
      (cf dy)
      (cf dw)
      (cf dh)
      (cf u0)
      (cf v0)
      (cf u1)
      (cf v1)
      r
      g
      b
      a

batchTextureSized :: RenderBatch -> Ptr () -> Float -> Float -> Float -> Float -> IO ()
batchTextureSized (RenderBatch p) tex x y w h =
  void $ batchTextureSizedC p tex (cf x) (cf y) (cf w) (cf h)

flushRenderBatch :: RenderBatch -> IO ()
flushRenderBatch (RenderBatch p) = batchFlush p

cf :: Float -> CFloat
cf = realToFrac

foreign import ccall unsafe "nano_ui_batch_create"
  batchCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_batch_destroy"
  batchDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_flush"
  batchFlush :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_batch_fill_solid"
  batchFillSolidC ::
    Ptr () ->
    Word8 ->
    Word8 ->
    Word8 ->
    Word8 ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO ()

foreign import ccall unsafe "nano_ui_batch_texture_dst"
  batchTextureDstC ::
    Ptr () ->
    Ptr () ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    Word8 ->
    Word8 ->
    Word8 ->
    Word8 ->
    IO Bool

foreign import ccall unsafe "nano_ui_batch_texture_sized"
  batchTextureSizedC :: Ptr () -> Ptr () -> CFloat -> CFloat -> CFloat -> CFloat -> IO Bool
