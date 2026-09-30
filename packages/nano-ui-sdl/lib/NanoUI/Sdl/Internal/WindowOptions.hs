-- | The context's 'WindowHost' for an SDL window (title, icon, size limits,
-- opacity, mode, position, size, maximize and minimize), and the window state
-- views read ('queryWindowState'). "NanoUI.Sdl.Internal.Window" installs the
-- host when a window opens, applying the settings it did not open with.
module NanoUI.Sdl.Internal.WindowOptions
  ( windowHostFor
  , queryWindowState
  ) where

import Control.Monad (unless, void)
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (isNothing)
import Data.Text.Foreign qualified as TextForeign
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import NanoUI (RgbaPixels, Size (..), WindowMode (..), rgbaBytes, rgbaHeight, rgbaWidth)
import NanoUI.Backend (WindowCapabilities (..), WindowHost (..), WindowState (..), allWindowCapabilities, defaultWindowState, sizeLimitAt)
import NanoUI.Sdl.Internal.Display (outPair, queryWindowPosition, sendWaylandSizeLimits, windowPosCentered)
import NanoUI.Sdl.Internal.Frame (hasFlag, nativeFrameOutset)
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Surface (createSurfaceFrom, destroySurface)
import SDL3.Sys.Video qualified as SDL

-- Window changes use the safe bindings, as in "NanoUI.Sdl.Internal.Chrome":
-- on Windows each call runs the window procedure before returning, which can
-- call back into the Haskell hit test.

-- | The window host for @win@. @zoom@ gives the window coordinates per layout
-- unit, read at each call. @limits@ holds the size limits of a Wayland
-- toplevel that nano-ui sends itself ('NanoUI.Sdl.Internal.Window.sdlSizeLimits').
windowHostFor :: Ptr SDL_Window -> IO Float -> Maybe (IORef (Int32, Int32, Int32, Int32)) -> WindowHost
windowHostFor win zoom limits =
  WindowHost
    { -- A Wayland toplevel cannot place itself.
      hostCapabilities = allWindowCapabilities {wcMove = isNothing limits}
    , hostSetTitle = \t -> TextForeign.withCString t (void . SDL.setWindowTitleSafe win . PtrConst.unsafeFromPtr)
    , hostSetIcon = setIcon win
    , hostSetMinSize = sizeLimit SDL.setWindowMinimumSizeSafe (\(w, h) (_, _, xw, xh) -> (w, h, raiseMax w xw, raiseMax h xh))
    , hostSetMaxSize = sizeLimit SDL.setWindowMaximumSizeSafe (\(w, h) (nw, nh, _, _) -> (lowerMin nw w, lowerMin nh h, w, h))
    , hostSetOpacity = void . SDL.setWindowOpacitySafe win
    , hostSetMode = \case
        Windowed -> void (SDL.setWindowFullscreenSafe win False) >> void (SDL.showWindowSafe win)
        Fullscreen -> void (SDL.showWindowSafe win) >> void (SDL.setWindowFullscreenSafe win True)
        Hidden -> void (SDL.hideWindowSafe win)
    , hostMove = \x y -> void (SDL.setWindowPositionSafe win (fromIntegral x) (fromIntegral y))
    , hostCenter = void (SDL.setWindowPositionSafe win windowPosCentered windowPosCentered)
    , hostResize = \s -> viewSize (Just s) >>= \(w, h) -> void (SDL.setWindowSizeSafe win w h)
    , hostMinimize = void (SDL.minimizeWindowSafe win)
    , hostMaximize = void (SDL.maximizeWindowSafe win)
    , hostRestore = void (SDL.restoreWindowSafe win)
    }
  where
    -- On a Wayland toplevel the new limit wins over the other one on an axis
    -- where the minimum would pass the maximum: the compositor disconnects a
    -- client that sends such a pair. A zero is no limit.
    raiseMax lo hi = if hi > 0 && lo > hi then lo else hi
    lowerMin lo hi = if hi > 0 && lo > hi then hi else lo
    -- A view size in window coordinates at the current zoom, plus the
    -- desktop frame on a 'NanoUI.Sdl.Internal.Frame.DecorationsFrame' window
    -- (as at open). A zero axis stays zero, meaning no limit ('sizeLimitAt').
    viewSize size = do
      z <- zoom
      outset <- nativeFrameOutset win
      let (w, h) = sizeLimitAt z outset size
      pure (fromIntegral w :: Int32, fromIntegral h)
    -- SDL resizes a window that is already past the limit. A Wayland
    -- toplevel's limits go to the compositor ('sendWaylandSizeLimits') and
    -- not to SDL, which would clamp configures to them, so it is resized here.
    sizeLimit ::
      (Ptr SDL_Window -> Int32 -> Int32 -> IO Bool) ->
      ((Int32, Int32) -> (Int32, Int32, Int32, Int32) -> (Int32, Int32, Int32, Int32)) ->
      Maybe Size ->
      IO ()
    sizeLimit set place limit = do
      (w, h) <- viewSize limit
      case limits of
        Nothing -> void (set win w h)
        Just ref -> do
          new@(nw, nh, xw, xh) <- place (w, h) <$> readIORef ref
          writeIORef ref new
          sendWaylandSizeLimits win nw nh xw xh
          (_, cw, ch) <- outPair (SDL.getWindowSize win)
          let fit v lo hi = (if hi > 0 then min hi else id) (max lo (fromIntegral v))
              (fw, fh) = (fit cw nw xw, fit ch nh xh)
          unless (fw == fromIntegral cw && fh == fromIntegral ch) $
            void (SDL.setWindowSizeSafe win fw fh)

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
  (ok, x, y) <- outPair (queryWindowPosition win)
  let has bit = hasFlag bit flags
  pure
    defaultWindowState
      { winScale = scale
      , winPosition = if ok /= 0 then Just (fromIntegral x, fromIntegral y) else Nothing
      , winFocused = has SDL.SDL_WINDOW_INPUT_FOCUS
      , winMaximized = has SDL.SDL_WINDOW_MAXIMIZED
      , winMinimized = has SDL.SDL_WINDOW_MINIMIZED
      , winFullscreen = has SDL.SDL_WINDOW_FULLSCREEN
      }
