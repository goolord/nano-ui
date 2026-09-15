-- | Universal session loop helpers: timing, click tracking, drawing locks, redraw predicates, and lifecycle checks.
module NanoUI.Runner
  ( -- * Drawing Lock
    DrawingLock (..)
  , newDrawingLock
  , tryWithDrawingLock
    -- * Redraw Decision
  , shouldRedrawFrame
    -- * Universal Session Runner
  , SessionDriver (..)
  , runSessionLoop
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, mask)
import Control.Monad (when)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import GHC.Clock (getMonotonicTime)
import NanoUI.Context
  ( Context
  , anyAnimating
  , isDirty
  , overlayConsumesQuit
  , textInputEditActive
  )
import NanoUI.Frame.Redraw (needsRedraw, textFieldActive)
import NanoUI.Input
  ( Input (..)
  , clearEphemeral
  , inputDeltaTime
  , inputMouseClicks
  , inputMousePos
  , inputMousePressed
  , isHardQuitInput
  , splitFrame
  )
import NanoUI.Types (V2 (..))

-- | Standard upper bound for single-frame delta-time (50ms).
maxFrameDt :: Float
maxFrameDt = 0.05

-- | Wind forward to the next frame boundary after a timed-out event wait.
-- When pacing is active the backend requests a wait of ~period, but a one-shot
-- sleep lets frame starts drift by the scheduler's timer granularity (and land
-- late whenever the event waiter overruns), which reads as choppy animation on
-- uneven frame times. Sleep the bulk, then busy-wind the ≤1ms tail so frame
-- starts fall on uniform slices of the pacing period. The spin only runs when
-- an animation is actively presenting without vsync, and is bounded to about a
-- millisecond.
alignFrameStart :: Double -> Double -> IO ()
alignFrameStart periodSec lastT = do
  t0 <- getMonotonicTime
  let target = lastT + periodSec
      remain = target - t0
      bulkUs = max 0 (round ((remain - tailSlack) * 1e6))
  when (bulkUs > 0) (threadDelay bulkUs)
  fullSpin target
  where
    tailSlack = 2.5e-4
    fullSpin target = do
      now <- getMonotonicTime
      when (now < target) (fullSpin target)

-- | State for multi-click detection (double/triple click).
data ClickTrack = ClickTrack
  { ctTime :: !Double
  , ctPos :: !V2
  , ctCount :: !Int
  }

-- | Stamp multi-click counts into an 'Input' record with custom distance and time thresholds.
stampClicksWith :: Float -> Double -> IORef ClickTrack -> Input -> IO Input
stampClicksWith !distLimit !timeLimit ref inp
  | not (inputMousePressed inp) = pure inp
  | otherwise = do
      now <- getMonotonicTime
      prev <- readIORef ref
      let t = ctTime prev
          n = ctCount prev
          V2 x y = inputMousePos inp
          V2 px py = ctPos prev
          dx = x - px
          dy = y - py
          distSq = dx * dx + dy * dy
          close = distSq <= distLimit * distLimit
          quick = (now - t) <= timeLimit
          n' = if close && quick then min 3 (n + 1) else 1
      writeIORef ref ClickTrack {ctTime = now, ctPos = inputMousePos inp, ctCount = n'}
      pure (inp {inputMouseClicks = n'})

-- | Concurrency lock for drawing vs async callbacks (e.g. resize watchers).
newtype DrawingLock = DrawingLock (IORef Bool)

-- | Create a new unacquired drawing lock.
newDrawingLock :: IO DrawingLock
newDrawingLock = DrawingLock <$> newIORef False

-- | Attempt to execute an action under the drawing lock without blocking.
tryWithDrawingLock :: DrawingLock -> IO a -> IO (Maybe a)
tryWithDrawingLock (DrawingLock ref) act = mask $ \restore -> do
  ok <- atomicModifyIORef' ref $ \busy -> if busy then (True, False) else (True, True)
  if ok
    then Just <$> (restore act `finally` writeIORef ref False)
    else pure Nothing

-- | Centralized decision predicate: should the host backend redraw this frame?
shouldRedrawFrame ::
  Context ->
  Input ->       -- ^ Previous input
  Input ->       -- ^ Current input
  Bool ->        -- ^ Was animating on previous frame?
  Bool ->        -- ^ Continuous redraw requested?
  Bool ->        -- ^ Debug live refresh requested?
  IO Bool
shouldRedrawFrame ctx prevInp curInp wasAnim continuous wantDebug = do
  if continuous || wantDebug
    then pure True
    else do
      need <- needsRedraw ctx prevInp curInp
      dirty <- isDirty ctx
      anim <- anyAnimating ctx
      editing <- textFieldActive ctx
      let forceFinal = wasAnim && not anim
          pointerEdge =
            inputMousePressed curInp
              || inputMouseReleased curInp
              || inputMouseRightPressed curInp
              || inputMouseRightReleased curInp
          scrollEdge = inputScroll curInp /= V2 0 0
      pure (need || anim || forceFinal || dirty || editing || pointerEdge || scrollEdge)

-- | Universal backend session driver configuration.
data SessionDriver ev = SessionDriver
  { sdPollEvents    :: IO [ev]
    -- ^ Non-blocking poll for pending backend events.
  , sdWaitEvents    :: Int -> IO [ev]
    -- ^ Wait for events with a timeout in milliseconds (-1 indicates blocking wait).
  , sdApplyEvent    :: Input -> ev -> Input
    -- ^ Fold an event into the 'Input' state.
  , sdIsButtonEdge  :: ev -> Bool
    -- ^ Predicate identifying click/press boundaries where the event stream should be split.
  , sdIsHardQuit    :: ev -> Bool
    -- ^ Predicate for immediate OS/SIGINT hard-quit signals (e.g. Ctrl+C).
  , sdIsSessionQuit :: ev -> Bool
    -- ^ Predicate for window close requests.
  , sdSyncDisplay   :: Context -> Input -> IO (Context, Input)
    -- ^ Backend-specific display synchronization (window dimensions, DPI scale).
  , sdWaitTimeout   :: Context -> Bool -> IO Int
    -- ^ Compute event wait timeout in milliseconds (-1 = block, 0 = immediate/non-blocking, >0 = tick timeout).
  , sdAlignSec      :: Double
    -- ^ Frame pacing period in seconds for the timed-out wait path. Frame
    -- starts are wound onto a uniform grid of this period so animation
    -- cadence matches the host, instead of drifting with the event waiter's
    -- timer granularity.
  , sdShouldDraw    :: Context -> Input -> Input -> Bool -> IO Bool
    -- ^ Decision predicate: (ctx, prevInp, curInp, wasAnimating) -> should this frame be rendered?
  , sdDraw          :: Context -> Input -> Bool -> IO (Bool, Input)
    -- ^ Render frame: (ctx, curInp, forceFull) -> (dirtyAfterRender, syncedInput).
  , sdSkip          :: Context -> Input -> IO ()
    -- ^ Called when a frame is skipped.
  , sdOnCursor      :: Context -> Input -> IO ()
    -- ^ Sync the host cursor icon.
  , sdNoteLoop      :: Float -> IO ()
    -- ^ Record the frame delta-time in the debug sampler.
  , sdShouldQuit    :: Input -> Bool
    -- ^ Application-level quit predicate.
  , sdClickDistance :: !Float
    -- ^ Distance threshold for multi-click detection in pixels.
  , sdClickTime     :: !Double
    -- ^ Time threshold for multi-click detection in seconds.
  }

-- | Run an event-driven session loop until a termination event or user quit condition.
runSessionLoop ::
  SessionDriver ev ->
  Context ->
  Input ->
  IO ()
runSessionLoop drv ctx0 inp0 = do
  clickTracker <- newIORef ClickTrack {ctTime = 0, ctPos = V2 (-999) (-999), ctCount = 0}
  startT <- getMonotonicTime

  let waitForEvents timeout lastT
        | timeout < 0 = sdWaitEvents drv (-1)
        | otherwise = do
            polled <- sdPollEvents drv
            if not (null polled)
              then pure polled
              else do
                events <- sdWaitEvents drv timeout
                -- Only a timed-out paced wait needs frame alignment.
                when (timeout > 0 && null events) $
                  alignFrameStart (sdAlignSec drv) lastT
                pure events

      loop ctx inp queued lastT pendingDirty wasAnim = do
        pending <- if null queued
          then do
            timeout <- if pendingDirty
              then pure 0
              else sdWaitTimeout drv ctx wasAnim
            waitForEvents timeout lastT
          else pure queued

        let (group, rest) = splitFrame (sdIsButtonEdge drv) pending
        editActive <- textInputEditActive ctx
        let hardQuitEv = any (sdIsHardQuit drv) group && not editActive
            sessionQuitEv = any (sdIsSessionQuit drv) group
        if hardQuitEv || sessionQuitEv
          then pure ()
          else do
            now <- getMonotonicTime
            let !dt = min maxFrameDt (realToFrac (now - lastT))
            sdNoteLoop drv dt
            let inpFolded = foldl' (sdApplyEvent drv) (clearEphemeral inp {inputDeltaTime = dt}) group
            inpStamped <- stampClicksWith (sdClickDistance drv) (sdClickTime drv) clickTracker inpFolded
            (ctx', inpSynced) <- sdSyncDisplay drv ctx inpStamped
            -- Hard quit (e.g. Ctrl+C) is ignored while a text editor is active.
            editActiveSynced <- textInputEditActive ctx'
            if isHardQuitInput inpSynced && not editActiveSynced
              then pure ()
              else do
                shouldDraw <- if pendingDirty
                  then pure True
                  else sdShouldDraw drv ctx' inp inpSynced wasAnim
                -- Force a full present only on the settle frame where an
                -- animation just finished (wasAnim && not animNow). Passing
                -- wasAnim alone kept every frame of a running animation at
                -- DamageFull, defeating clip damage for animated widgets.
                animNow <- anyAnimating ctx'
                (dirtyOut, synced) <- if shouldDraw
                  then sdDraw drv ctx' inpSynced (wasAnim && not animNow)
                  else do
                    sdSkip drv ctx' inpSynced
                    sdOnCursor drv ctx' inpSynced
                    pure (pendingDirty, inpSynced)
                animAfter <- anyAnimating ctx'
                -- Open modals/overlays consume Escape/Quit before the app sees it.
                overlayQuit <- overlayConsumesQuit ctx' synced
                if sdShouldQuit drv synced && not overlayQuit
                  then pure ()
                  else loop ctx' synced rest now dirtyOut animAfter

  loop ctx0 inp0 [] startT False False
