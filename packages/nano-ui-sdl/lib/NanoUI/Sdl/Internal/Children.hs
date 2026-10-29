-- | A session's further windows ('NanoUI.nativeWindow') under SDL: how to
-- open, draw and close one ("NanoUI.Runner.Windows" does the rest), and the
-- routing of SDL's single event queue to the windows its events are for.
module NanoUI.Sdl.Internal.Children
  ( SdlWindows (..)
  , newSdlWindows
  , routeEvents
  , afterMainPass
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (writeIORef)
import Data.List (find)
import Data.Word (Word32)
import NanoUI (whenM)
import NanoUI.Backend (Context, Input)
import NanoUI.Runner.Windows (WindowOps (..), Windows, newWindows, openWindows)
import NanoUI.Runner.Windows qualified as Windows
import NanoUI.Sdl.Internal.Cursor (syncPointerCursor)
import NanoUI.Sdl.Internal.Display (pointerInWindow)
import NanoUI.Sdl.Internal.Input
import NanoUI.Sdl.Internal.Runner (resetRenderTextures, sdlDrawFrame)
import NanoUI.Sdl.Internal.Window (SdlEnv (..), SdlOptions, openChildWindow, syncDisplay, windowZoom)
import SDL3.Sys.Bindgen.Stdinc (Uint32 (..))
import SDL3.Sys.Bindgen.Video (SDL_WindowID (..))
import SDL3.Sys.Video (getWindowID)

-- | A further SDL window: its environment, SDL id, and the action that
-- closes it.
data SdlChild = SdlChild
  { scEnv :: !SdlEnv
  , scId :: !Word32
  , scClose :: IO ()
  }

data SdlWindows = SdlWindows
  { swFirst :: !SdlEnv
  , swFirstId :: !Word32
  , swWindows :: !(Windows SdlChild SdlEvent)
  }

newSdlWindows :: SdlOptions -> SdlEnv -> IO SdlWindows
newSdlWindows opts first = SdlWindows first <$> windowId first <*> newWindows ops
  where
    ops =
      WindowOps
        { -- Every further window takes its fonts and scale from the first.
          woOpen = \_ parentCtx settings -> do
            (ctx, env, close) <- openChildWindow opts first parentCtx settings
            (\wid -> (SdlChild env wid close, ctx)) <$> windowId env
        , woClose = scClose
        , woPoll = \_ -> pure []
        , woApplyEvent = applyEvent
        , woIsButtonEdge = isButtonEdge
        , woIsClose = (== EvCloseRequested)
        , woSync = \c ctx inp -> syncDisplay ctx (scEnv c) inp
        , woDraw = \c ctx view inp force -> sdlDrawFrame ctx view (scEnv c) inp force
        , woPointerIn = fmap (/= 0) . pointerInWindow . sdlWindow . scEnv
        , woCursor = syncPointerCursor . sdlCursors . scEnv
        , -- Further windows present without vsync: pace them by the display.
          woPacingMs = \c -> max 1 (floor (sdlRefreshPeriod (scEnv c) * 1000))
        }

windowId :: SdlEnv -> IO Word32
windowId env = (\(SDL_WindowID (Uint32 w)) -> w) <$> getWindowID (sdlWindow env)

-- | Keep the first window's events and queue the others' on their windows,
-- in layout units. With none open, every event is the first window's, as in
-- a session of one; a close request there is followed by SDL's quit. With
-- some open, the first window's close request ends the session (SDL quits
-- only after the last window), events for every window go to each, and
-- those for a window just closed go nowhere.
routeEvents :: SdlWindows -> [(Word32, SdlEvent)] -> IO [SdlEvent]
routeEvents w evs = do
  kids <- openWindows (swWindows w)
  if null kids
    then pure [ev | (_, ev) <- evs, ev /= EvCloseRequested]
    else concat <$> mapM (route kids) evs
  where
    route kids (wid, ev)
      | wid == 0 = do
          unless (ev == EvQuit || ev == EvRefresh) $ forM_ kids (\(c, queue) -> toChild c queue ev)
          pure [ev]
      | wid == swFirstId w = pure [if ev == EvCloseRequested then EvQuit else ev]
      | otherwise = [] <$ forM_ (find ((== wid) . scId . fst) kids) (\(c, queue) -> toChild c queue ev)
    toChild c queue ev = do
      let env = scEnv c
      zoom <- windowZoom env
      let scaled = scaleEventPositions zoom [ev]
      when (hasPointerEvent scaled) (writeIORef (sdlPointerSeen env) True)
      when (ev == EvRenderDeviceReset) (resetRenderTextures env)
      mapM_ queue scaled

-- | After a pass of the first window's loop: the further windows' work
-- ('Windows.afterMainPass'), and the cursor, from the first window only
-- while the pointer is over it when others are open.
afterMainPass :: SdlWindows -> Context -> Input -> IO ()
afterMainPass w ctx inp = do
  others <- Windows.afterMainPass (swWindows w) ctx
  let first = swFirst w
      sync = syncPointerCursor (sdlCursors first) ctx inp
  if others then whenM ((/= 0) <$> pointerInWindow (sdlWindow first)) sync else sync
