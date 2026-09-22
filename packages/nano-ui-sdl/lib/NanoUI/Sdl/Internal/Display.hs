{-# LANGUAGE OverloadedRecordDot #-}

-- | SDL window dimensions, display scale, refresh timing, and event-loop wake support.
module NanoUI.Sdl.Internal.Display
  ( queryWindowPixelDensity
  , queryWindowRefreshHz
  , queryWindowLogicalSize
  , queryMouseWindowPos
  , outPair
  , zoomWindow
  , installResizeWatch
  , refreshEventType
  , initRefreshEvent
  , pushRefreshEvent
  , takeRefreshEvent
  ) where

import Control.Monad (unless, void)
import Data.IORef (IORef, newIORef)
import GHC.IORef (atomicSwapIORef)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Marshal.Alloc (alloca, callocBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Storable (Storable, peek, poke, sizeOf)
import Data.Word (Word32)
import NanoUI (Size (..), V2 (..))
import SDL3.Sys.Bindgen.Events (SDL_Event)
import SDL3.Sys.Bindgen.Stdinc (Uint32 (..))
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Events (pushEvent, registerEvents)
import SDL3.Sys.Mouse (getMouseState)
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Video (getDisplayForWindow, getDisplayUsableBounds, getWindowPixelDensity, getWindowSize, setWindowPosition, setWindowSize)
import System.IO.Unsafe (unsafePerformIO)

-- | Backbuffer pixels per window coordinate: the factor the retained
-- framebuffer, glyph rasterization and snapping need. This is not
-- 'SDL_GetWindowDisplayScale', which also folds in the desktop's content
-- scale. On Windows window coordinates are already pixels, so at 125%
-- scaling the display scale is 1.25 while the density is 1; sizing the
-- framebuffer by the display scale rendered 1.56x the window's pixels and
-- squeezed them back down on every present.
queryWindowPixelDensity :: Ptr SDL_Window -> IO Float
queryWindowPixelDensity win = (\s -> if s > 0 then s else 1) <$> getWindowPixelDensity win

-- | Vertical refresh rate of the window's current display mode, in Hz
-- (0 when unavailable).
queryWindowRefreshHz :: Ptr SDL_Window -> IO Int
queryWindowRefreshHz win = max 0 . fromIntegral <$> windowRefreshRateC win

-- | Window size in window (logical) coordinates; 0x0 when SDL cannot say.
-- SDL_GetWindowSize already returns the window-coordinate size, not pixels.
-- Dividing by the display scale would shrink the logical size on DPI-scaled
-- displays, making the retained framebuffer too small.
queryWindowLogicalSize :: Ptr SDL_Window -> IO Size
queryWindowLogicalSize win = do
  (ok, w, h) <- outPair (getWindowSize win)
  pure (if ok then Size (fromIntegral w) (fromIntegral h) else Size 0 0)

-- | Pointer position relative to the window with mouse focus, in window
-- coordinates. Uses 'SDL_GetMouseState' rather than the global pointer +
-- window position: the latter is unreliable on Wayland (window position is not
-- exposed) and breaks hover/wheel targeting.
queryMouseWindowPos :: IO V2
queryMouseWindowPos = do
  (_, x, y) <- outPair getMouseState
  pure (V2 (realToFrac x) (realToFrac y))

-- | Call a native function that answers through two out pointers, and return
-- its result with both answers. The answers are not meaningful when the call
-- reports a failure.
outPair :: (Storable a, Storable b) => (Ptr a -> Ptr b -> IO r) -> IO (r, a, b)
outPair f = alloca $ \pa -> alloca $ \pb -> do
  r <- f pa pb
  (r,,) <$> peek pa <*> peek pb

-- Windows runs a modal loop while the user drags the border, so the app
-- event watch does not run. SDL still delivers resize events to this watch.
installResizeWatch :: IO () -> IO (IO ())
installResizeWatch act = do
  fp <- mkResizeCb act
  ok <- (/= 0) <$> installResizeWatchC fp
  unless ok $ fail "SDL_AddEventWatch failed"
  pure $ do
    removeResizeWatchC
    freeHaskellFunPtr fp

-- | The event 'pushRefreshEvent' sends, filled in once by 'initRefreshEvent'.
-- The core wakes the loop on every 'markDirty', so a push must not allocate.
{-# NOINLINE refreshEvent #-}
refreshEvent :: Ptr SDL_Event
refreshEvent = unsafePerformIO (callocBytes (sizeOf (undefined :: SDL_Event)))

-- | The user event type that wakes the event loop, registered once per
-- process by 'initRefreshEvent'; 0 until then.
refreshEventType :: IO Word32
refreshEventType = (\(Uint32 ty) -> ty) <$> peek refreshEvent.type'

initRefreshEvent :: IO Bool
initRefreshEvent = do
  -- A wake queued as the last session closed went down with SDL's queue.
  -- Left pending, it would stop this session from ever queuing one.
  takeRefreshEvent
  registered <- refreshEventType
  if registered /= 0
    then pure True
    else do
      ty <- registerEvents 1
      poke refreshEvent.type' (Uint32 ty)
      pure (ty /= 0)

-- | Whether a refresh event is queued that the loop has not taken yet.
{-# NOINLINE refreshPending #-}
refreshPending :: IORef Bool
refreshPending = unsafePerformIO (newIORef False)

-- | Wake the event loop from any thread. One queued event wakes it as well as
-- many, so a wake while one is pending costs an atomic swap and no SDL call:
-- the core wakes on every 'NanoUI.Testing.markDirty', most of them made by
-- the loop's own thread in the middle of a frame.
--
-- The swap is a memory barrier, so whatever the caller wrote before waking is
-- visible by the time the loop takes the event: 'takeRefreshEvent' runs
-- before the frame that reads it.
pushRefreshEvent :: IO ()
pushRefreshEvent = do
  ty <- refreshEventType
  unless (ty == 0) $ do
    pending <- atomicSwapIORef refreshPending True
    unless pending $ do
      ok <- pushEvent refreshEvent
      unless ok $ void (atomicSwapIORef refreshPending False)

-- | The loop took the queued refresh event, so the next wake queues another.
takeRefreshEvent :: IO ()
takeRefreshEvent = void (atomicSwapIORef refreshPending False)

foreign import ccall unsafe "nano_ui_window_refresh_rate"
  windowRefreshRateC :: Ptr SDL_Window -> IO CInt

foreign import ccall "wrapper"
  mkResizeCb :: IO () -> IO (FunPtr (IO ()))

foreign import ccall safe "nano_ui_install_resize_watch"
  installResizeWatchC :: FunPtr (IO ()) -> IO CBool

foreign import ccall safe "nano_ui_remove_resize_watch"
  removeResizeWatchC :: IO ()

-- | Grow a window just opened at a logical size by the UI zoom, as far as
-- the usable area of its display allows, and centre it again.
zoomWindow :: Ptr SDL_Window -> Size -> Float -> IO ()
zoomWindow win (Size w h) zoom = do
  display <- getDisplayForWindow win
  -- Left empty, and so not a limit, when SDL cannot say.
  usable <- with (SDL_Rect 0 0 0 0) $ \rp -> getDisplayUsableBounds display rp >> peek rp
  let fit want avail = if avail > 0 then min want (fromIntegral avail) else want
      zw = fit (w * zoom) usable.w
      zh = fit (h * zoom) usable.h
      centred = 0x2FFF0000 -- SDL_WINDOWPOS_CENTERED
  void $ setWindowSize win (round zw) (round zh)
  void $ setWindowPosition win centred centred
