-- | What a window opens with and a view can change later: the icon, limits
-- on its size, its position and its opacity. The functions here work on the
-- native window alone. "NanoUI.Sdl.Internal.Window" applies them as a
-- window opens and hands them to views as the context's 'WindowHost';
-- "NanoUI.Sdl.Internal.Chrome" has them for a session.
module NanoUI.Sdl.Internal.WindowOptions
  ( applyWindowIcon
  , applyWindowMinSize
  , applyWindowMaxSize
  , applyWindowPosition
  , queryWindowPosition
  , applyWindowOpacity
  , windowHostFor
  ) where

import Control.Monad (void)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Int (Int32)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import NanoUI (RgbaImage (..), Size (..), WindowPosition (..))
import NanoUI.Backend (WindowHost (..))
import NanoUI.Sdl.Internal.Display (outPair)
import NanoUI.Sdl.Internal.Frame (nativeFrameOutset)
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Surface (createSurfaceFrom, destroySurface)
import SDL3.Sys.Video qualified as SDL

-- Everything that changes the window goes through the safe bindings, as in
-- "NanoUI.Sdl.Internal.Chrome": on Windows each of these runs the window's
-- procedure before it returns.

-- | Give the window an icon. SDL keeps a copy. 'False' for an image whose
-- pixels are too few for its size, or a video driver without icons.
applyWindowIcon :: Ptr SDL_Window -> RgbaImage -> IO Bool
applyWindowIcon win (RgbaImage _ w h pixels)
  | w <= 0 || h <= 0 || BS.length pixels < w * h * 4 = pure False
  | otherwise =
      BSU.unsafeUseAsCString pixels $ \p -> do
        surface <- createSurfaceFrom (fromIntegral w) (fromIntegral h) Pixels.SDL_PIXELFORMAT_RGBA32 (castPtr p) (fromIntegral (w * 4))
        if surface == nullPtr
          then pure False
          else do
            ok <- SDL.setWindowIconSafe win surface
            destroySurface surface
            pure ok

-- | Keep the window at least a size, in layout units at a zoom, or take the
-- limit off. An axis of zero has no limit.
applyWindowMinSize :: Ptr SDL_Window -> Float -> Maybe Size -> IO ()
applyWindowMinSize = applySizeLimit SDL.setWindowMinimumSizeSafe

-- | Keep the window at most a size, in layout units at a zoom, or take the
-- limit off. An axis of zero has no limit.
applyWindowMaxSize :: Ptr SDL_Window -> Float -> Maybe Size -> IO ()
applyWindowMaxSize = applySizeLimit SDL.setWindowMaximumSizeSafe

-- | The limit in window coordinates: the zoom's, as the size the window
-- opens at is, and with the desktop's frame on a 'DecorationsFrame' window
-- added, as 'NanoUI.Sdl.Internal.Chrome.setWindowSize' adds it, so the
-- limit is on the view. SDL resizes a window already past the limit.
applySizeLimit :: (Ptr SDL_Window -> Int32 -> Int32 -> IO Bool) -> Ptr SDL_Window -> Float -> Maybe Size -> IO ()
applySizeLimit set win zoom limit = do
  (across, down) <- nativeFrameOutset win
  let axis v outset = if v <= 0 then 0 else fromIntegral (round (v * zoom) + outset)
      (w, h) = case limit of
        Nothing -> (0, 0)
        Just (Size lw lh) -> (axis lw across, axis lh down)
  void (set win w h)

-- | Move the window. 'WindowPositionDefault' leaves it where it is.
applyWindowPosition :: Ptr SDL_Window -> WindowPosition -> IO ()
applyWindowPosition win = \case
  WindowPositionDefault -> pure ()
  -- SDL_WINDOWPOS_CENTERED: on the display the window is on.
  WindowPositionCentered -> void (SDL.setWindowPositionSafe win 0x2FFF0000 0x2FFF0000)
  WindowPositionAt x y -> void (SDL.setWindowPositionSafe win (fromIntegral x) (fromIntegral y))

-- | Where the window's top-left corner is, in the desktop's coordinates, as
-- the desktop last said; 'Nothing' when SDL cannot say. Wayland does not
-- tell a window where it is, so there this is not where it is.
queryWindowPosition :: Ptr SDL_Window -> IO (Maybe (Int, Int))
queryWindowPosition win = do
  (ok, x, y) <- outPair (SDL.getWindowPosition win)
  pure (if ok then Just (fromIntegral x, fromIntegral y) else Nothing)

-- | Fade the whole window, from 0 (invisible) to 1 (opaque).
applyWindowOpacity :: Ptr SDL_Window -> Float -> IO ()
applyWindowOpacity win o = void (SDL.setWindowOpacitySafe win (max 0 (min 1 o)))

-- | What views may do to the window, at the zoom the window is at when they
-- ask.
windowHostFor :: Ptr SDL_Window -> IO Float -> WindowHost
windowHostFor win zoom =
  WindowHost
    { hostSetIcon = void . applyWindowIcon win
    , hostSetMinSize = \s -> zoom >>= \z -> applyWindowMinSize win z s
    , hostSetMaxSize = \s -> zoom >>= \z -> applyWindowMaxSize win z s
    , hostSetPosition = applyWindowPosition win
    , hostSetOpacity = applyWindowOpacity win
    }
