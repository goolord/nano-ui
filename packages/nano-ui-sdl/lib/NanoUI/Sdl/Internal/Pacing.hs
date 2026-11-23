-- | How an SDL session paces animation: whether its presents wait for
-- vertical sync ('notePresent', 'presentPaces'), the refresh period of the
-- window's display, and the event wait between the frames of an animation
-- whose presents do not wait ('sdlPacingMs').
module NanoUI.Sdl.Internal.Pacing
  ( notePresent
  , restartPresentProbe
  , presentPaces
  , refreshDisplayRate
  , refreshPeriodOf
  , sdlPacingMs
  , sdlLivePacingMs
  ) where

import Control.Monad (when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Foreign.Ptr (Ptr)
import NanoUI.Runner (pacingMsFor)
import NanoUI.Sdl.Internal.Display (queryWindowRefreshHz)
import NanoUI.Sdl.Internal.Env (PresentProbe (..), SdlEnv (..))
import SDL3.Sys.Bindgen.Video (SDL_Window)

-- | Presents in a row, each less than half a refresh after the last, that
-- show vertical sync is not honoured. A driver may queue two or three
-- frames before a present blocks, so a few fast presents prove nothing.
vsyncProbeFrames :: Int
vsyncProbeFrames = 10

-- | Note a present that returned at time @t@ (a session with 'sdlVsync'
-- only), and give up on vertical sync once 'vsyncProbeFrames' presents in a
-- row returned sooner than half a refresh apart.
notePresent :: SdlEnv -> Double -> IO ()
notePresent env t = when (sdlVsync env) $
  readIORef (sdlPresentProbe env) >>= \case
    VsyncIgnored -> pure ()
    PresentProbe lastT n -> do
      period <- readIORef (sdlRefreshPeriodRef env)
      let n' = if t - lastT < period / 2 then n + 1 else 0
      writeIORef (sdlPresentProbe env) $! if n' >= vsyncProbeFrames then VsyncIgnored else PresentProbe t n'

-- | Start counting presents afresh, after presents that ran with vertical
-- sync off on purpose (the resize watch's). A finding that vertical sync is
-- not honoured stays.
restartPresentProbe :: SdlEnv -> IO ()
restartPresentProbe env = modifyIORef' (sdlPresentProbe env) $ \case
  VsyncIgnored -> VsyncIgnored
  PresentProbe _ _ -> PresentProbe 0 0

-- | Whether the last frame's present waited for the display: it presented,
-- 'sdlVsync' asked for vertical sync, and the presents so far have not
-- shown that the driver ignores it.
presentPaces :: SdlEnv -> IO Bool
presentPaces env
  | sdlVsync env = (&&) <$> readIORef (sdlLastPresented env) <*> ((/= VsyncIgnored) <$> readIORef (sdlPresentProbe env))
  | otherwise = pure False

-- | Read the window's refresh rate again, after it moved to another display
-- or its display changed mode, and start the vertical sync probe afresh:
-- another display's driver may honour it.
refreshDisplayRate :: SdlEnv -> IO ()
refreshDisplayRate env = do
  writeIORef (sdlRefreshPeriodRef env) =<< refreshPeriodOf (sdlWindow env)
  writeIORef (sdlPresentProbe env) (PresentProbe 0 0)

-- | The refresh period of a window's display, in seconds; 60 Hz's when SDL
-- cannot say.
refreshPeriodOf :: Ptr SDL_Window -> IO Double
refreshPeriodOf win = (\hz -> if hz > 0 then 1 / realToFrac hz else 1 / 60) <$> queryWindowRefreshHz win

-- | 'sdlPacingMs' for the window's current display ('sdlRefreshPeriodRef'),
-- whether or not presents wait for vertical sync: the wait for a frame that
-- presents nothing, or whose present does not wait.
sdlLivePacingMs :: SdlEnv -> IO Int
sdlLivePacingMs env = pacingMsFor <$> readIORef (sdlRefreshPeriodRef env)

-- | The event wait while a window animates. With vsync on, presents throttle
-- the loop. With vsync off a live animation would spin at max speed, so wait
-- 'pacingMsFor' the frame period: SDL_WaitEventTimeout overruns by about a
-- millisecond, and the slack keeps the wait from ending past the boundary.
sdlPacingMs :: SdlEnv -> Int
sdlPacingMs env = if sdlVsync env then 16 else pacingMsFor (sdlRefreshPeriod env)
