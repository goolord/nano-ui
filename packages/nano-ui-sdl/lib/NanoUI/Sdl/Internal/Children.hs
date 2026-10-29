-- | A session's further windows ('NanoUI.nativeWindow') under SDL: how to
-- open, draw and close one ("NanoUI.Runner.Windows" does the rest), and the
-- routing of SDL's single event queue to the windows its events are for.
module NanoUI.Sdl.Internal.Children
  ( SdlWindows (..)
  , newSdlWindows
  , decodeForWindow
  , routeEvents
  , syncFirstCursor
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (writeIORef)
import Data.Word (Word32)
import NanoUI (whenM)
import NanoUI.Backend (Context, Input)
import NanoUI.Runner.Windows (WindowOps (..), Windows, hasWindows, newWindows, openWindows)
import NanoUI.Sdl.Internal.Cursor (syncPointerCursor)
import NanoUI.Sdl.Internal.Display (pointerInWindow)
import NanoUI.Sdl.Internal.Input
import NanoUI.Sdl.Internal.Runner (resetRenderTextures, sdlDrawFrame)
import NanoUI.Sdl.Internal.Window (SdlEnv (..), SdlOptions, openChildWindow, sdlPacingMs, syncDisplay, windowZoom)
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
          woOpen = \parentCtx settings -> do
            (ctx, env, close) <- openChildWindow opts first parentCtx settings
            (\wid -> (SdlChild env wid close, ctx)) <$> windowId env
        , woClose = scClose
        , woApplyEvent = applyEvent
        , woIsButtonEdge = isButtonEdge
        , woIsClose = (== EvCloseRequested)
        , woSync = \c ctx inp -> syncDisplay ctx (scEnv c) inp
        , woDraw = \c ctx view inp force -> sdlDrawFrame ctx view (scEnv c) inp force
        , woCursor = \c ctx inp -> syncCursorIn (scEnv c) ctx inp
        , woPacingMs = sdlPacingMs . scEnv
        }

windowId :: SdlEnv -> IO Word32
windowId env = (\(SDL_WindowID (Uint32 w)) -> w) <$> getWindowID (sdlWindow env)

-- | A batch of a window's events in layout units, as each had them, after
-- noting what they mean for the window: a pointer seen, or a render device
-- reset, which the frame it asks for answers by making the textures again.
decodeForWindow :: SdlEnv -> [SdlEvent] -> IO [SdlEvent]
decodeForWindow env evs = do
  zoom <- windowZoom env
  when (hasPointerEvent evs) (writeIORef (sdlPointerSeen env) True)
  when (EvRenderDeviceReset `elem` evs) (resetRenderTextures env)
  pure (scaleEventPositions zoom evs)

-- | Keep the first window's events and queue the others' on their windows.
-- With none open, every event is the first window's, as in a session of
-- one; a close request there is followed by SDL's quit. With some open, the
-- first window's close request ends the session (SDL quits only after the
-- last window), events for every window go to each, and those for a window
-- just closed go nowhere.
routeEvents :: SdlWindows -> [(Word32, SdlEvent)] -> IO [SdlEvent]
routeEvents w evs = do
  kids <- openWindows (swWindows w)
  if null kids
    then pure [ev | (_, ev) <- evs, ev /= EvCloseRequested]
    else do
      forM_ kids $ \(c, queue) -> do
        let mine = [ev | (wid, ev) <- evs, wid == scId c || (wid == 0 && ev /= EvQuit && ev /= EvRefresh)]
        unless (null mine) $ decodeForWindow (scEnv c) mine >>= mapM_ queue
      pure [if ev == EvCloseRequested then EvQuit else ev | (wid, ev) <- evs, wid == 0 || wid == swFirstId w]

-- | Sync the cursor from the first window: from the window the pointer is
-- over only, while others are open.
syncFirstCursor :: SdlWindows -> Context -> Input -> IO ()
syncFirstCursor w ctx inp = do
  others <- hasWindows (swWindows w)
  if others then syncCursorIn (swFirst w) ctx inp else syncPointerCursor (sdlCursors (swFirst w)) ctx inp

syncCursorIn :: SdlEnv -> Context -> Input -> IO ()
syncCursorIn env ctx inp = whenM ((/= 0) <$> pointerInWindow (sdlWindow env)) $ syncPointerCursor (sdlCursors env) ctx inp
