-- | Making the retained frame's texture the render target, around a fault in
-- SDL's OpenGL ES 2 renderer.
--
-- That renderer changes target without making its GL context current
-- (@GLES2_SetRenderTarget@, SDL 3.2 through 3.4). Destroying a renderer
-- leaves no GL context current, so after a further window closes, the next
-- target change of a remaining window binds its framebuffer with no context,
-- fails, and leaves SDL treating the texture as the target all the same. A
-- target change first runs the commands already queued, and running them
-- makes the context current, so a command queued beforehand avoids the
-- fault.
module NanoUI.Sdl.Internal.Target
  ( setRetainTarget
  ) where

import Control.Monad (void, when)
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI.Sdl.Internal.Env (SdlEnv (..))
import SDL3.Sys.Bindgen.Render (SDL_Texture)
import SDL3.Sys.Bindgen.Video (SDL_GLContext (..))
import SDL3.Sys.Render (getRenderTarget, renderClear, setRenderTarget)
import SDL3.Sys.Video (glGetCurrentContext)

-- | Make @tex@ the render target of the session's renderer. On the OpenGL ES
-- 2 renderer with no GL context current, while the window is the target, a
-- clear of the window is queued first; every retained frame covers the
-- window with its copy, and a screenshot is read after the present, when
-- the window's backbuffer holds nothing to keep.
setRetainTarget :: SdlEnv -> Ptr SDL_Texture -> IO Bool
setRetainTarget env tex = do
  let ren = sdlRenderer env
  when (sdlRendererName env == "opengles2" && tex /= nullPtr) $ do
    SDL_GLContext current <- glGetCurrentContext
    onWindow <- (== nullPtr) <$> getRenderTarget ren
    when (current == nullPtr && onWindow) $ void (renderClear ren)
  setRenderTarget ren tex
