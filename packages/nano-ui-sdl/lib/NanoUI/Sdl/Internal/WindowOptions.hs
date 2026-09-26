-- | What a view may do to the window -- its title, icon, size limits,
-- opacity, mode, position and size, maximizing and minimizing -- as the
-- context's 'WindowHost', and the window's state as views read it
-- ('queryWindowState'). "NanoUI.Sdl.Internal.Window" installs the host as a
-- window opens, which applies the settings it did not open with.
module NanoUI.Sdl.Internal.WindowOptions
  ( windowHostFor
  , queryWindowState
  ) where

import Control.Monad (void)
import Data.Bits (zeroBits, (.&.))
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text.Foreign qualified as TextForeign
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import NanoUI (RgbaPixels, Size (..), WindowMode (..), rgbaBytes, rgbaHeight, rgbaWidth)
import NanoUI.Backend (WindowHost (..), WindowState (..), defaultWindowState)
import NanoUI.Sdl.Internal.Display (outPair)
import NanoUI.Sdl.Internal.Frame (nativeFrameOutset)
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Surface (createSurfaceFrom, destroySurface)
import SDL3.Sys.Video qualified as SDL

-- Everything that changes the window goes through the safe bindings, as in
-- "NanoUI.Sdl.Internal.Chrome": on Windows each of these runs the window's
-- procedure before it returns, which can reach the Haskell hit test.

-- | What views may do to the window, at the zoom it is at when they ask:
-- the window coordinates a layout unit is worth.
windowHostFor :: Ptr SDL_Window -> IO Float -> WindowHost
windowHostFor win zoom =
  WindowHost
    { hostSetTitle = \t -> TextForeign.withCString t (void . SDL.setWindowTitleSafe win . PtrConst.unsafeFromPtr)
    , hostSetIcon = setIcon win
    , hostSetMinSize = sizeLimit SDL.setWindowMinimumSizeSafe
    , hostSetMaxSize = sizeLimit SDL.setWindowMaximumSizeSafe
    , hostSetOpacity = void . SDL.setWindowOpacitySafe win
    , hostSetMode = \case
        Windowed -> void (SDL.setWindowFullscreenSafe win False) >> void (SDL.showWindowSafe win)
        Fullscreen -> void (SDL.showWindowSafe win) >> void (SDL.setWindowFullscreenSafe win True)
        Hidden -> void (SDL.hideWindowSafe win)
    , hostMove = \x y -> void (SDL.setWindowPositionSafe win (fromIntegral x) (fromIntegral y))
    , -- SDL_WINDOWPOS_CENTERED: on the display the window is on.
      hostCenter = void (SDL.setWindowPositionSafe win 0x2FFF0000 0x2FFF0000)
    , hostResize = \s -> viewSize s >>= \(w, h) -> void (SDL.setWindowSizeSafe win w h)
    , hostMinimize = void (SDL.minimizeWindowSafe win)
    , hostMaximize = void (SDL.maximizeWindowSafe win)
    , hostRestore = void (SDL.restoreWindowSafe win)
    }
  where
    -- A size in window coordinates at the zoom, with the desktop's frame
    -- on a 'NanoUI.Sdl.Internal.Frame.DecorationsFrame' window added, as the
    -- window opened with it, so the size is the view's. An axis of zero
    -- stays zero, which is no limit.
    viewSize (Size w h) = do
      z <- zoom
      (across, down) <- nativeFrameOutset win
      let axis v outset = if v <= 0 then 0 else fromIntegral (round (v * z) + outset) :: Int32
      pure (axis w across, axis h down)
    -- SDL resizes a window already past the limit.
    sizeLimit :: (Ptr SDL_Window -> Int32 -> Int32 -> IO Bool) -> Maybe Size -> IO ()
    sizeLimit set limit = viewSize (fromMaybe (Size 0 0) limit) >>= \(w, h) -> void (set win w h)

-- | Give the window an icon. SDL keeps a copy; a video driver without icons
-- leaves it be.
setIcon :: Ptr SDL_Window -> RgbaPixels -> IO ()
setIcon win px =
  BSU.unsafeUseAsCString (rgbaBytes px) $ \p -> do
    surface <- createSurfaceFrom (fromIntegral (rgbaWidth px)) (fromIntegral (rgbaHeight px)) Pixels.SDL_PIXELFORMAT_RGBA32 (castPtr p) (fromIntegral (rgbaWidth px * 4))
    if surface == nullPtr
      then pure ()
      else void (SDL.setWindowIconSafe win surface) >> destroySurface surface

-- | The window's state for views, at a scale: its position as the desktop
-- last said (Wayland does not say), and its focus and mode from its flags.
-- Two reads of what SDL keeps, cheap enough for every frame.
queryWindowState :: Ptr SDL_Window -> Float -> IO WindowState
queryWindowState win scale = do
  flags <- SDL.getWindowFlags win
  (ok, x, y) <- outPair (SDL.getWindowPosition win)
  let has bit = flags .&. bit /= zeroBits
  pure
    defaultWindowState
      { winScale = scale
      , winPosition = if ok then Just (fromIntegral x, fromIntegral y) else Nothing
      , winFocused = has SDL.SDL_WINDOW_INPUT_FOCUS
      , winMaximized = has SDL.SDL_WINDOW_MAXIMIZED
      , winMinimized = has SDL.SDL_WINDOW_MINIMIZED
      , winFullscreen = has SDL.SDL_WINDOW_FULLSCREEN
      }
