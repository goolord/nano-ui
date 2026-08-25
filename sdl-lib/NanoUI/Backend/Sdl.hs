-- | SDL3 backend: consumes geometry ('DrawData') from the core frame loop and
-- maps SDL events into 'Input'. Idle frames skip 'runFrame' until input,
-- 'markDirty', or an active animation demands a redraw (damage tracking; see design doc).
module NanoUI.Backend.Sdl
  ( runSdlApp
  , runSdlAppWithQuit
  ) where

import Control.Monad (void)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Context (..)
  , Input (..)
  , UI
  , anyAnimating
  , collectTextSpans
  , ctxTheme
  , emptyInput
  , isDirty
  , needsRedraw
  , runFrame
  , themeWindow
  )
import NanoUI.Sdl.Event (SdlEvent (..))
import NanoUI.Sdl.Font (renderTextSpans)
import NanoUI.Sdl.Input
  ( applyEvent
  , clearEphemeral
  , isHardQuit
  , isHardQuitInput
  , pollEvents
  , splitFrame
  , waitEventTimeout
  )
import NanoUI.Sdl.Render (renderDrawData)
import NanoUI.Sdl.Window (SdlEnv (..), defaultWindowSize, syncDisplay, withSdl)
import SDL3.Sys.Bindgen.Blendmode (sDL_BLENDMODE_BLEND)
import SDL3.Sys.Render (renderPresentSafe, setRenderDrawBlendModeSafe)

animateTimeout :: Int
animateTimeout = 16

idleTimeout :: Int
idleTimeout = 250

runSdlApp :: Context -> UI () -> IO ()
runSdlApp ctx ui = runSdlAppWithQuit ctx (const False) ui

runSdlAppWithQuit :: Context -> (Input -> Bool) -> UI () -> IO ()
runSdlAppWithQuit ctx shouldQuit ui =
  withSdl ctx "nano-ui" defaultWindowSize $ \ctx' env -> do
    void $ setRenderDrawBlendModeSafe (sdlRenderer env) (fromIntegral sDL_BLENDMODE_BLEND)
    now <- getMonotonicTime
    (_, inp0) <- syncDisplay ctx' env emptyInput
    prev <- newIORef inp0
    pendingRedraw <- newIORef False
    wasAnimating <- newIORef False
    (_, synced0) <- draw ctx' ui env inp0
    writeIORef pendingRedraw False
    writeIORef prev synced0
    loop ctx' ui env prev pendingRedraw wasAnimating shouldQuit synced0 [] now

loop ::
  Context ->
  UI () ->
  SdlEnv ->
  IORef Input ->
  IORef Bool ->
  IORef Bool ->
  (Input -> Bool) ->
  Input ->
  [SdlEvent] ->
  Double ->
  IO ()
loop ctx ui env prev pendingRedraw wasAnimating shouldQuit inp queued lastT = do
  pending <-
    if null queued
      then do
        polled <- pollEvents
        if not (null polled)
          then pure polled
          else do
            animating <- anyAnimating ctx
            if animating
              then waitEventTimeout animateTimeout
              else waitEventTimeout idleTimeout
      else pure queued
  let (group, rest) = splitFrame pending
  if any isHardQuit group || any (== EvQuit) group
    then pure ()
    else do
      now <- getMonotonicTime
      let dt = realToFrac (now - lastT)
          inp' =
            foldl'
              applyEvent
              (clearEphemeral inp {inputDeltaTime = dt})
              group
      (_, inpSynced) <- syncDisplay ctx env inp'
      if shouldQuit inpSynced || isHardQuitInput inpSynced
        then pure ()
        else do
          prevInp <- readIORef prev
          pendingDirty <- readIORef pendingRedraw
          wasAnim <- readIORef wasAnimating
          need <- needsRedraw ctx prevInp inpSynced
          dirtyNow <- isDirty ctx
          anim <- anyAnimating ctx
          let forceFinal = wasAnim && not anim
          writeIORef wasAnimating anim
          if need || anim || forceFinal || pendingDirty || dirtyNow || not (null group)
            then do
              (_, synced) <- draw ctx ui env inpSynced
              writeIORef pendingRedraw False
              writeIORef prev synced
              loop ctx ui env prev pendingRedraw wasAnimating shouldQuit synced rest now
            else
              if null rest
                then loop ctx ui env prev pendingRedraw wasAnimating shouldQuit inpSynced [] now
                else loop ctx ui env prev pendingRedraw wasAnimating shouldQuit inpSynced rest now

draw :: Context -> UI () -> SdlEnv -> Input -> IO (Bool, Input)
draw ctx ui env inp = do
  (_, _, drawData, dirtyAfterUi) <- runFrame ctx inp ui
  spans <- collectTextSpans ctx
  font <- readIORef (sdlFontRef env)
  let clear = themeWindow (ctxTheme ctx)
  renderDrawData (sdlRenderer env) clear drawData
  renderTextSpans (sdlRenderer env) font (sdlTextCache env) spans
  void $ renderPresentSafe (sdlRenderer env)
  pure (dirtyAfterUi, inp)
