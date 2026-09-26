-- | The context's 'WindowHost' for an SDL window (title, icon, size limits,
-- opacity, mode, position, size, maximize and minimize), and the window state
-- views read ('queryWindowState'). "NanoUI.Sdl.Internal.Window" installs the
-- host when a window opens, applying the settings it did not open with.
module NanoUI.Sdl.Internal.WindowOptions
  ( windowHostFor
  , queryWindowState
  ) where

import Control.Monad (unless, void)
import Data.Bits (zeroBits, (.&.))
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Data.Maybe (fromMaybe)
import Data.Text.Foreign qualified as TextForeign
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import NanoUI (RgbaPixels, Size (..), WindowMode (..), rgbaBytes, rgbaHeight, rgbaWidth)
import NanoUI.Backend (WindowHost (..), WindowState (..), defaultWindowState)
import NanoUI.Sdl.Internal.Display (outPair, windowPosCentered)
import NanoUI.Sdl.Internal.Frame (nativeFrameOutset)
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Surface (createSurfaceFrom, destroySurface)
import SDL3.Sys.Video qualified as SDL

-- Window changes use the safe bindings, as in "NanoUI.Sdl.Internal.Chrome":
-- on Windows each call runs the window procedure before returning, which can
-- call back into the Haskell hit test.

-- | The window host for @win@. @zoom@ gives the window coordinates per layout
-- unit, read at each call.
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
    , hostCenter = void (SDL.setWindowPositionSafe win windowPosCentered windowPosCentered)
    , hostResize = \s -> viewSize s >>= \(w, h) -> void (SDL.setWindowSizeSafe win w h)
    , hostMinimize = void (SDL.minimizeWindowSafe win)
    , hostMaximize = void (SDL.maximizeWindowSafe win)
    , hostRestore = void (SDL.restoreWindowSafe win)
    }
  where
    -- A view size in window coordinates at the current zoom, plus the
    -- desktop frame on a 'NanoUI.Sdl.Internal.Frame.DecorationsFrame' window
    -- (as at open). A zero axis stays zero, meaning no limit.
    viewSize (Size w h) = do
      z <- zoom
      (across, down) <- nativeFrameOutset win
      let axis v outset = if v <= 0 then 0 else fromIntegral (round (v * z) + outset) :: Int32
      pure (axis w across, axis h down)
    -- SDL resizes a window that is already past the limit.
    sizeLimit :: (Ptr SDL_Window -> Int32 -> Int32 -> IO Bool) -> Maybe Size -> IO ()
    sizeLimit set limit = viewSize (fromMaybe (Size 0 0) limit) >>= \(w, h) -> void (set win w h)

-- | Set the window icon. SDL keeps a copy; video drivers without icons
-- ignore it.
setIcon :: Ptr SDL_Window -> RgbaPixels -> IO ()
setIcon win px =
  BSU.unsafeUseAsCString (rgbaBytes px) $ \p -> do
    surface <- createSurfaceFrom (fromIntegral (rgbaWidth px)) (fromIntegral (rgbaHeight px)) Pixels.SDL_PIXELFORMAT_RGBA32 (castPtr p) (fromIntegral (rgbaWidth px * 4))
    unless (surface == nullPtr) $
      void (SDL.setWindowIconSafe win surface) >> destroySurface surface

-- | The window state for views: the position the desktop last reported
-- (Wayland never does), and focus and mode from the window flags. Two reads
-- of SDL's cached state, cheap enough for every frame.
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
