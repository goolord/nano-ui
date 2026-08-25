module NanoUI.Sdl.Display
  ( defaultUiScale
  , defaultFontSize
  , queryWindowDisplayScale
  , queryWindowLogicalSize
  , queryMouseWindowPos
  , setRenderScale
  , windowToRenderCoords
  ) where

import Control.Monad (unless)
import Foreign.C.Types (CFloat (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import NanoUI (Size (..), V2 (..))
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Video (SDL_Window)

defaultUiScale :: Float
defaultUiScale = 1

defaultFontSize :: Float
defaultFontSize = 16

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

setRenderScale :: Ptr SDL_Renderer -> Float -> IO Bool
setRenderScale ren scale = setRenderScaleC ren (realToFrac scale)

windowToRenderCoords :: Ptr SDL_Renderer -> V2 -> IO V2
windowToRenderCoords ren (V2 wx wy) =
  alloca $ \xp ->
    alloca $ \yp -> do
      ok <- renderCoordsFromWindowC ren (realToFrac wx) (realToFrac wy) xp yp
      unless ok $ fail "SDL_RenderCoordinatesFromWindow failed"
      rx <- peek xp
      ry <- peek yp
      pure (V2 (realToFrac rx) (realToFrac ry))

foreign import ccall safe "nano_ui_render_coords_from_window"
  renderCoordsFromWindowC ::
    Ptr SDL_Renderer ->
    CFloat ->
    CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    IO Bool

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
