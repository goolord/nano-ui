module NanoUI.Sdl.Display
  ( defaultUiScale
  , defaultFontSize
  , initSdlHints
  , queryWindowDisplayScale
  , queryWindowLogicalSize
  , queryMouseWindowPos
  , setRenderScale
  , windowToLogicalCoords
  , installResizeWatch
  ) where

import Control.Monad (unless)
import Foreign.C.Types (CFloat (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Storable (peek)
import NanoUI (Size (..), V2 (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Video (SDL_Window)

defaultUiScale :: Float
defaultUiScale = 1

defaultFontSize :: Float
defaultFontSize = 16

initSdlHints :: IO ()
initSdlHints = sdlInitHintsC

queryWindowDisplayScale :: Ptr SDL_Window -> IO Float
queryWindowDisplayScale win = do
  s <- windowDisplayScaleC win
  pure (if s > 0 then realToFrac s else defaultUiScale)

queryWindowLogicalSize :: Ptr SDL_Window -> Float -> IO Size
queryWindowLogicalSize win scale =
  alloca $ \wp ->
    alloca $ \hp -> do
      ok <- windowLogicalSizeC win (realToFrac scale) wp hp
      if ok
        then do
          w <- peek wp
          h <- peek hp
          pure (Size (realToFrac w) (realToFrac h))
        else pure (Size 0 0)

queryMouseWindowPos :: IO (Maybe V2)
queryMouseWindowPos =
  alloca $ \xp ->
    alloca $ \yp -> do
      ok <- mouseWindowPosC xp yp
      if ok
        then do
          x <- peek xp
          y <- peek yp
          pure (Just (V2 (realToFrac x) (realToFrac y)))
        else pure Nothing

-- Renderer stays at 1:1 pixels; layout uses logical coordinates.
setRenderScale :: Ptr SDL_Renderer -> Float -> IO Bool
setRenderScale ren scale = setRenderScaleC ren (realToFrac scale)

windowToLogicalCoords :: Float -> V2 -> V2
windowToLogicalCoords scale (V2 wx wy) =
  let s = if scale > 0 then scale else defaultUiScale
   in V2 (wx / s) (wy / s)

-- Windows runs a modal loop while the user drags the border, so the app
-- event wait does not run. SDL still delivers resize events to this watch.
installResizeWatch :: IO () -> IO (IO ())
installResizeWatch act = do
  fp <- mkResizeCb act
  ok <- installResizeWatchC fp
  unless ok $ fail "SDL_AddEventWatch failed"
  pure $ do
    removeResizeWatchC
    freeHaskellFunPtr fp

foreign import ccall safe "nano_ui_sdl_init_hints"
  sdlInitHintsC :: IO ()

foreign import ccall safe "nano_ui_window_display_scale"
  windowDisplayScaleC :: Ptr SDL_Window -> IO CFloat

foreign import ccall safe "nano_ui_window_logical_size"
  windowLogicalSizeC ::
    Ptr SDL_Window ->
    CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    IO Bool

foreign import ccall safe "nano_ui_mouse_window_pos"
  mouseWindowPosC :: Ptr CFloat -> Ptr CFloat -> IO Bool

foreign import ccall safe "nano_ui_set_render_scale"
  setRenderScaleC :: Ptr SDL_Renderer -> CFloat -> IO Bool

foreign import ccall "wrapper"
  mkResizeCb :: IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "nano_ui_install_resize_watch"
  installResizeWatchC :: FunPtr (IO ()) -> IO Bool

foreign import ccall safe "nano_ui_remove_resize_watch"
  removeResizeWatchC :: IO ()
