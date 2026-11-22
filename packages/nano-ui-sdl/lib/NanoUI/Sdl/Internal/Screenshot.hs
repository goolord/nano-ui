-- | Reading an SDL session's frame back: to a BMP file ('saveScreenshot'),
-- as 'NanoUI.Screenshot' pixels ('captureScreenshot'), or from the
-- backbuffer before a present ('captureFrame').
module NanoUI.Sdl.Internal.Screenshot
  ( saveScreenshot
  , captureScreenshot
  , captureFrame
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString.Internal qualified as BSI
import Data.IORef (readIORef)
import qualified GHC.Foreign as GHC
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (RgbaPixels, Screenshot (..), rgbaPixels)
import NanoUI.Sdl.Internal.Env (Retain (..), SdlEnv (..))
import NanoUI.Sdl.Internal.Font (sdlPathEncoding)
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Render (SDL_Texture)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Surface (SDL_Surface)
import SDL3.Sys.Bindgen.Surface qualified as Surface
import SDL3.Sys.Render (renderReadPixels, setRenderTarget)
import SDL3.Sys.Surface (convertPixels, destroySurface, saveBMP)

-- | Write the last presented frame to a BMP file, on the display thread. A
-- retained session reads its retained texture, since SDL leaves the window
-- backbuffer undefined after a present.
--
-- A direct-to-window session ('sdlAppContinuous') has no retained frame and
-- a readable one only between drawing and presenting, so from a view use
-- 'NanoUI.requestScreenshot', which reads it at that point.
saveScreenshot :: SdlEnv -> FilePath -> IO Bool
saveScreenshot env path =
  withSurface (readFrame env . retainTexture =<< readIORef (sdlRetain env)) False $ \surface ->
    sdlPathEncoding >>= \enc -> GHC.withCString enc path (saveBMP surface . PtrConst.unsafeFromPtr)

-- | The last presented frame, as 'NanoUI.requestScreenshot' returns it:
-- window pixels (the backbuffer's size, about the logical size times the
-- pixel density and UI zoom) with the drawn alpha, which is the theme's
-- window colour's alpha where nothing covers it.
-- 'Nothing' when SDL cannot read the frame back. Call on the display thread.
--
-- A direct-to-window session ('sdlAppContinuous') has a readable frame only
-- between drawing and presenting, so from a view use
-- 'NanoUI.requestScreenshot', which reads it at that point.
captureScreenshot :: SdlEnv -> IO (Maybe Screenshot)
captureScreenshot env = do
  scale <- readIORef (sdlScaleRef env)
  fmap (`Screenshot` scale) <$> (captureFrame env . retainTexture =<< readIORef (sdlRetain env))

-- | The frame's pixels from the retained texture, or from the window
-- backbuffer when the target is null (valid only before presenting).
captureFrame :: SdlEnv -> Ptr SDL_Texture -> IO (Maybe RgbaPixels)
captureFrame env target = withSurface (readFrame env target) Nothing $ \surface -> do
  Surface.SDL_Surface _ format sw sh pitch pixels _ _ <- peek surface
  let w = fromIntegral sw
      h = fromIntegral sh
  -- Converted straight into the result, from whatever format was read back;
  -- a failed conversion leaves too few bytes for 'rgbaPixels'.
  bytes <- BSI.createUptoN (w * h * 4) $ \dst -> do
    ok <- convertPixels (fromIntegral w) (fromIntegral h) format (PtrConst.unsafeFromPtr pixels) (fromIntegral pitch) Pixels.SDL_PIXELFORMAT_RGBA32 (castPtr dst) (fromIntegral w * 4)
    pure (if ok then w * h * 4 else 0)
  pure (rgbaPixels w h bytes)

-- | Run an action on a new surface, and free it after; @none@ when there is
-- no surface.
withSurface :: IO (Ptr SDL_Surface) -> a -> (Ptr SDL_Surface -> IO a) -> IO a
withSurface make none act = bracket make destroySurface $ \surface ->
  if surface == nullPtr then pure none else act surface

-- | Read the used area of the retained texture, or the backbuffer for a null
-- target, into a new surface. Null on failure.
readFrame :: SdlEnv -> Ptr SDL_Texture -> IO (Ptr SDL_Surface)
readFrame env tex = do
  r <- readIORef (sdlRetain env)
  let ren = sdlRenderer env
  if tex == nullPtr
    then renderReadPixels ren (PtrConst.unsafeFromPtr nullPtr)
    else do
      void $ setRenderTarget ren tex
      s <- with (SDL_Rect 0 0 (fromIntegral (retainW r)) (fromIntegral (retainH r))) $ \rp ->
        renderReadPixels ren (PtrConst.unsafeFromPtr rp)
      void $ setRenderTarget ren nullPtr
      pure s
