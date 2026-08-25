-- | Terminal backend built directly on the platform console APIs.
--
-- Replaces the vty backend. vty cannot report pointer motion: its SGR parser
-- rejects the any-motion button code, and its input loop discards the whole
-- pending buffer on a rejected sequence, which also swallowed clicks that
-- arrived in the same read. Owning the input path makes hover a normal event
-- and keeps clicks intact.
module NanoUI.Backend.Term
  ( runTermApp
  , runTermAppWithQuit
  ) where

import Control.Exception (finally)
import Control.Monad (when)
import Data.ByteString.Builder (string7)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Clock (getMonotonicTime)
import NanoUI
  ( Context
  , Input (..)
  , Modifiers (..)
  , Size (..)
  , UI
  , V2 (..)
  , anyAnimating
  , collectTextSpans
  , emptyInput
  , runFrame
  )
import NanoUI.Term.Ansi (frameBytes, setup, teardown)
import NanoUI.Term.Cells (cellsSize, rasterize)
import NanoUI.Term.Driver (Driver (..), withDriver)
import NanoUI.Term.Event (MouseAction (..), TermEvent (..))

-- | Poll interval while animations are running, in milliseconds.
animateTimeout :: Int
animateTimeout = 16

-- | Idle wait. Long enough to cost nothing, short enough that a missed wakeup
-- is not noticeable.
idleTimeout :: Int
idleTimeout = 250

runTermApp :: Context -> UI () -> IO ()
runTermApp ctx ui = runTermAppWithQuit ctx (const False) ui

runTermAppWithQuit :: Context -> (Input -> Bool) -> UI () -> IO ()
runTermAppWithQuit ctx shouldQuit ui =
  withDriver $ \drv -> do
    (w0, h0) <- drvSize drv
    drvWrite drv setup
    drvFlush drv
    prev <- newIORef Nothing
    now <- getMonotonicTime
    let inp0 =
          emptyInput
            { inputWindowSize = Size (fromIntegral w0) (fromIntegral h0)
            }
    -- Paint before waiting on input, so startup is not a blank screen for as
    -- long as the idle timeout.
    ( do
        draw drv prev inp0
        loop drv prev inp0 [] now
      )
      `finally` ( do
                    drvWrite drv teardown
                    drvFlush drv
                )
  where
    loop drv prev inp queued lastT = do
      pending <-
        if null queued
          then do
            animating <- anyAnimating ctx
            drvRead drv (if animating then animateTimeout else idleTimeout)
          else pure []
      let (group, rest) = splitFrame (queued ++ pending)
      if any isHardQuit group
        then pure ()
        else do
          now <- getMonotonicTime
          let inp' =
                foldl'
                  applyEvent
                  (clearEphemeral inp {inputDeltaTime = realToFrac (now - lastT)})
                  group
          if shouldQuit inp' || isHardQuitInput inp'
            then pure ()
            else do
              draw drv prev inp'
              loop drv prev inp' rest now

    draw drv prev inp = do
      (_, _, drawData, _) <- runFrame ctx inp ui
      spans <- collectTextSpans ctx
      let Size w h = inputWindowSize inp
      cells <- rasterize (round w) (round h) drawData spans
      before <- readIORef prev
      when (before /= Just cells) $ do
        -- A size change leaves stale cells outside the new grid.
        when (fmap cellsSize before /= Just (cellsSize cells)) $
          drvWrite drv (string7 "\ESC[2J")
        drvWrite drv (frameBytes before cells)
        drvFlush drv
        writeIORef prev (Just cells)

-- | Mouse button transitions have to be visible to exactly one frame, so a
-- batch is cut after the first one. Without this a fast click whose press and
-- release land in the same read would never register.
splitFrame :: [TermEvent] -> ([TermEvent], [TermEvent])
splitFrame events =
  case break isButtonEdge events of
    (before, edge : rest) -> (before ++ [edge], rest)
    (before, []) -> (before, [])

isButtonEdge :: TermEvent -> Bool
isButtonEdge ev =
  case ev of
    EvMouse (MousePress _) _ _ _ -> True
    EvMouse (MouseRelease _) _ _ _ -> True
    _ -> False

-- Signal generation is disabled in raw mode, so Ctrl-C is handled here.
-- POSIX reports the control byte as the letter it was typed with; the
-- Windows console reports the control character itself.
isHardQuit :: TermEvent -> Bool
isHardQuit ev =
  case ev of
    EvChar c mods -> modCtrl mods && (c == 'c' || c == '\ETX')
    _ -> False

isHardQuitInput :: Input -> Bool
isHardQuitInput inp =
  any (\c -> modCtrl (inputModifiers inp) && (c == 'c' || c == '\ETX')) (inputChars inp)

clearEphemeral :: Input -> Input
clearEphemeral inp =
  inp
    { inputKeys = []
    , inputChars = []
    , inputMousePressed = False
    , inputMouseReleased = False
    , inputScroll = V2 0 0
    }

applyEvent :: Input -> TermEvent -> Input
applyEvent inp ev =
  case ev of
    EvResize w h -> inp {inputWindowSize = Size (fromIntegral w) (fromIntegral h)}
    EvKey k mods -> inp {inputKeys = inputKeys inp ++ [k], inputModifiers = mods}
    EvChar c mods -> inp {inputChars = inputChars inp ++ [c], inputModifiers = mods}
    EvMouse action col row mods ->
      let positioned =
            inp
              { inputMousePos = V2 (fromIntegral col) (fromIntegral row)
              , inputModifiers = mods
              }
       in case action of
            MousePress _ ->
              positioned {inputMouseDown = True, inputMousePressed = True}
            MouseRelease _ ->
              positioned {inputMouseDown = False, inputMouseReleased = True}
            MouseDrag _ -> positioned {inputMouseDown = True}
            MouseMove -> positioned
            MouseScrollUp -> positioned {inputScroll = V2 0 1}
            MouseScrollDown -> positioned {inputScroll = V2 0 (-1)}
