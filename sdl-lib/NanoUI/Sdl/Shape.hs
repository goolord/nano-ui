module NanoUI.Sdl.Shape
  ( fillSolidRect
  , fillRoundedRect
  ) where

import Control.Monad (void, when)
import Data.Word (Word8)
import Foreign.C.Types (CFloat (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import SDL3.Sys.Bindgen.Rect (SDL_FRect (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Render (renderFillRectSafe, setRenderDrawColorSafe)

fillSolidRect :: Ptr SDL_Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> Float -> Float -> Float -> Float -> IO ()
fillSolidRect ren r g b a x y w h =
  when (w > 0 && h > 0) $ do
    void $ setRenderDrawColorSafe ren r g b a
    allocaRect $ \rect -> do
      poke rect (SDL_FRect {x = cf (snap x), y = cf (snap y), w = cf (snap w), h = cf (snap h)})
      void $ renderFillRectSafe ren (PtrConst.unsafeFromPtr rect)

fillRoundedRect :: Ptr SDL_Renderer -> Word8 -> Word8 -> Word8 -> Word8 -> Float -> Float -> Float -> Float -> Float -> IO ()
fillRoundedRect ren r g b a x y w h radius =
  when (w > 0 && h > 0) $
    void $
      fillRoundedRectC
        ren
        r
        g
        b
        a
        (cf (snap x))
        (cf (snap y))
        (cf (snap w))
        (cf (snap h))
        (cf (max 0 radius))

snap :: Float -> Float
snap x = fromIntegral (round x :: Int)

foreign import ccall safe "nano_ui_fill_rounded_rect"
  fillRoundedRectC ::
    Ptr SDL_Renderer ->
    Word8 ->
    Word8 ->
    Word8 ->
    Word8 ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    CFloat ->
    IO Bool

allocaRect :: (Ptr SDL_FRect -> IO a) -> IO a
allocaRect = alloca

cf :: Float -> CFloat
cf = realToFrac
