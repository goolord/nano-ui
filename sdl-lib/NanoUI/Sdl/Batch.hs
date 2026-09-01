module NanoUI.Sdl.Batch
  ( RenderBatch
  , withRenderBatch
  , batchDrawRange
  , flushRenderBatch
  ) where

import Control.Exception (bracket)
import Data.Word (Word8)
import Foreign.C.Types (CFloat (..), CInt (..))
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI (Rect (..))
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

flushRenderBatch :: RenderBatch -> IO ()
flushRenderBatch (RenderBatch p) = batchFlush p

batchDrawRange ::
  RenderBatch ->
  Ptr Word8 ->
  Int ->
  Ptr Word8 ->
  Int ->
  Int ->
  Int ->
  Int ->
  Ptr () ->
  Float ->
  Float ->
  Float ->
  Maybe Rect ->
  IO ()
batchDrawRange (RenderBatch p) verts vc indices ic start n texId tex tw th scale mDmg =
  batchDrawRangeC
    p
    verts
    (ci vc)
    indices
    (ci ic)
    (ci start)
    (ci n)
    (ci texId)
    tex
    (cf tw)
    (cf th)
    (cf scale)
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
    CInt ->
    CInt ->
    Ptr () ->
    CFloat ->
    CFloat ->
    CFloat ->
    CInt ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO ()
