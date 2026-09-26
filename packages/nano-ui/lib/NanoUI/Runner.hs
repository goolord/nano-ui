-- | The event loop the backends share: event waiting and frame pacing, click
-- counting, the redraw decision, quit handling, and the drawing lock. A
-- backend supplies a 'SessionDriver' for event translation and presentation.
module NanoUI.Runner
  ( -- * Drawing Lock
    DrawingLock (..)
  , newDrawingLock
  , tryWithDrawingLock
    -- * Redraw Decision
  , shouldRedrawFrame
    -- * Session loop
  , SessionDriver (..)
  , runSessionLoop
  ) where

import Control.Concurrent (threadDelay)
import Control.Exception (finally, mask)
import Control.Monad (forM_, unless, when)
import Data.Maybe (isJust)
import Numeric (showFFloat)
import System.Environment (lookupEnv)
import System.IO (BufferMode (LineBuffering), hFlush, hPutStrLn, hSetBuffering, stderr)
import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )
import GHC.Clock (getMonotonicTime)
import NanoUI.Internal.Context
import NanoUI.Internal.Debug
import NanoUI.Internal.Frame.Input (needsRedraw)
import NanoUI.Internal.Input
import NanoUI.Internal.NativeWindow (clearWindowClose, quitRequested, requestWindowClose)
import NanoUI.Internal.Tasks (cancelTasks)
import NanoUI.Internal.Types (V2 (..))

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

-- | Stamp multi-click counts into an 'Input' record: presses of the same
-- button within 5 pixels and 0.4 seconds of the previous one count up to a
-- triple click. The ref holds the time, position, buttons and count of the
-- previous press.
stampClicks :: IORef (Double, V2, MouseButtons, Int) -> Input -> IO Input
stampClicks ref inp
  | not (anyButtonPressed inp) = pure inp
  | otherwise = do
      now <- getMonotonicTime
      (t, V2 px py, prevButtons, n) <- readIORef ref
      let pos@(V2 x y) = inputMousePos inp
          buttons = inputButtonsPressed inp
          close = (x - px) * (x - px) + (y - py) * (y - py) <= 25
          !n' = if close && buttons == prevButtons && now - t <= 0.4 then min 3 (n + 1) else 1
      writeIORef ref (now, pos, buttons, n')
      pure (inp {inputMouseClicks = n'})

-- | Concurrency lock for drawing vs async callbacks (e.g. resize watchers).
newtype DrawingLock = DrawingLock (IORef Bool)

-- | Create a new unacquired drawing lock.
newDrawingLock :: IO DrawingLock
newDrawingLock = DrawingLock <$> newIORef False

-- | Attempt to execute an action under the drawing lock without blocking.
tryWithDrawingLock :: DrawingLock -> IO a -> IO (Maybe a)
tryWithDrawingLock (DrawingLock ref) act = mask $ \restore -> do
  ok <- atomicModifyIORef' ref $ \busy -> (True, not busy)
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
  Bool ->        -- ^ A frame is due without input: a timed wake or a debug readout refresh?
  IO Bool
shouldRedrawFrame ctx prevInp curInp wasAnim continuous refreshDue = do
  if continuous || refreshDue
    then pure True
    else do
      -- 'needsRedraw' already covers a dirty context and running animations,
      -- so an animation that just ended is the only animation case left: it
      -- needs one final frame.
      need <- needsRedraw ctx prevInp curInp
      let pointerEdge = anyButtonPressed curInp || anyButtonReleased curInp
          scrollEdge = inputScroll curInp /= V2 0 0
      pure (need || wasAnim || pointerEdge || scrollEdge)

-- | What a backend provides to 'runSessionLoop'.
data SessionDriver ev = SessionDriver
  { sdPollEvents    :: IO [ev]
    -- ^ Non-blocking poll for pending backend events.
  , sdWaitEvents    :: Int -> IO [ev]
    -- ^ Wait for events with a timeout in milliseconds (-1 indicates blocking wait).
  , sdApplyEvent    :: Input -> ev -> Input
    -- ^ Fold an event into the 'Input' state.
  , sdIsButtonEdge  :: ev -> Bool
    -- ^ Predicate identifying click/press boundaries where the event stream
    -- should be split. The loop also splits it after a command key where
    -- order would be lost ('NanoUI.Internal.Input.takeFrame').
  , sdIsSessionQuit :: ev -> Bool
    -- ^ Predicate for window close requests. One ends the session, or, for a
    -- window whose settings say not to ('NanoUI.wsExitOnCloseRequest'), is
    -- shown to the next frame ('NanoUI.winCloseRequested').
  , sdSyncDisplay   :: Context -> Input -> IO (Context, Input)
    -- ^ Backend-specific display synchronization (window dimensions, DPI scale).
  , sdDebug         :: DebugSamplerRef
    -- ^ The session's debug sampler: loop timing, skips, and the 4 Hz
    -- readout refresh.
  , sdContinuous    :: !Bool
    -- ^ Redraw every pass without waiting for events.
  , sdPacingMs      :: !Int
    -- ^ Event wait in milliseconds while something animates.
  , sdPresentPaces  :: IO Bool
    -- ^ Whether the last present waited for the display (vsync), so a running
    -- animation can loop without waiting and still be frame-locked.
  , sdAlignSec      :: Double
    -- ^ Frame pacing period in seconds for the timed-out wait path. Frame
    -- starts are wound onto a uniform grid of this period so animation
    -- cadence matches the host, instead of drifting with the event waiter's
    -- timer granularity.
  , sdShouldDraw    :: Context -> Input -> Input -> Bool -> Bool -> IO Bool
    -- ^ Decision predicate: (ctx, prevInp, curInp, wasAnimating, refreshDue) ->
    -- should this frame be rendered? Usually 'shouldRedrawFrame'.
  , sdDraw          :: Context -> Input -> Bool -> IO Bool
    -- ^ Render frame: (ctx, curInp, forceFull) -> dirtyAfterRender.
  , sdOnCursor      :: Context -> Input -> IO ()
    -- ^ Sync the host cursor icon after every pass.
  , sdShouldQuit    :: Input -> Bool
    -- ^ Application-level quit predicate.
  }

-- | NANO_LOOP_TRACE: on the first pass a second or more after the last line,
-- print to stderr the time covered, how many passes the loop made in it, how
-- many of them drew, and why. A busy loop prints a line a second. An idle
-- window prints nothing, and the line after a quiet spell covers all of it,
-- so whatever prints steadily is what keeps the process awake. The
-- reasons are @D@ a dirty context, @A@ an animation, @R@ a window redraw
-- request, and @T@ a timed wake or a debug readout refresh; a pass with none
-- of them was caused by input.
type LoopTrace = Maybe (IORef (Double, Int, Int, Int, [String]))

newLoopTrace :: Double -> IO LoopTrace
newLoopTrace now = do
  on <- isJust <$> lookupEnv "NANO_LOOP_TRACE"
  -- stderr is unbuffered by default, which writes a line a character at a
  -- time: enough system calls to show up in the measurement being taken.
  when on (hSetBuffering stderr LineBuffering)
  if on then Just <$> newIORef (now, 0, 0, 0, []) else pure Nothing

traceLoopPass :: LoopTrace -> Int -> Bool -> String -> IO ()
traceLoopPass lt nEvents drew why = forM_ lt $ \ref -> do
  now <- getMonotonicTime
  (t0, passes, draws, events, whys) <- readIORef ref
  let passes' = passes + 1
      draws' = if drew then draws + 1 else draws
      events' = events + nEvents
      whys' = if passes < 24 then (if null why then "-" else why) : whys else whys
  if now - t0 >= 1
    then do
      hPutStrLn stderr $
        "LOOP " ++ showFFloat (Just 1) (now - t0) "s passes=" ++ show passes' ++ " draws=" ++ show draws'
          ++ " events=" ++ show events' ++ " why=" ++ unwords (reverse whys')
      hFlush stderr
      writeIORef ref (now, 0, 0, 0, [])
    else writeIORef ref (t0, passes', draws', events', whys')

-- | Added to the wait for a timed wake, so the wait ends at or after the time
-- asked for. An OS wait can return a millisecond or two early, and a backend
-- that counts its timeout in whole milliseconds turns the fraction then left
-- into a wait of zero: without the pad the loop spins through that last
-- millisecond, dozens of empty passes for every wake. The frame comes this
-- much late instead, which nothing scheduled to the millisecond can notice;
-- animation is paced separately.
wakePadMs :: Int
wakePadMs = 2

-- | Event wait while only the debug readout needs frames: its refresh period.
debugHudTimeout :: Int
debugHudTimeout = round (debugRefreshSec * 1000)

-- | Run an event-driven session loop until the window is closed, Ctrl+C is
-- pressed outside a text field, a view calls 'NanoUI.quitUi', or
-- 'sdShouldQuit' says so. The background jobs the view started end with it.
runSessionLoop ::
  SessionDriver ev ->
  Context ->
  Input ->
  IO ()
runSessionLoop drv ctx0 inp0 = do
  clickTracker <- newIORef (0, V2 (-999) (-999), noButtons, 0)
  startT <- getMonotonicTime
  trace <- newLoopTrace startT

  let -- A paced wait is one a running animation times out of, so the frame it
      -- ends on is wound onto the pacing grid. Any other wait ends whenever
      -- its event or deadline arrives.
      waitForEvents timeout paced lastT
        | timeout < 0 = sdWaitEvents drv (-1)
        | otherwise = do
            polled <- sdPollEvents drv
            if not (null polled)
              then pure polled
              else do
                events <- sdWaitEvents drv timeout
                when (paced && timeout > 0 && null events) $
                  alignFrameStart (sdAlignSec drv) lastT
                pure events

      loop ctx inp queued lastT pendingDirty wasAnim = do
        (pending, refreshDue) <-
          if not (null queued)
            then pure (queued, False)
            else if pendingDirty
              then (,False) <$> waitForEvents 0 False lastT
              else do
                (debugActive, dueNow) <- debugCadence (sdDebug drv)
                animating <- anyAnimating ctx
                dirty <- isDirty ctx
                presentPaces <- sdPresentPaces drv
                wakeAt <- getWakeAt ctx
                waitT <- if wakeAt > 0 then getMonotonicTime else pure 0
                let paced = wasAnim || animating
                    -- Nothing moves by itself: sleep until input, or until
                    -- the earliest frame something asked for.
                    wakeMs
                      | wakeAt <= 0 = -1
                      | wakeAt <= waitT = 0
                      | otherwise = ceiling ((wakeAt - waitT) * 1000) + wakePadMs :: Int
                    idleMs
                      | not debugActive = wakeMs
                      | wakeMs < 0 = debugHudTimeout
                      | otherwise = min debugHudTimeout wakeMs
                    timeout
                      | sdContinuous drv || dueNow || dirty || (animating && presentPaces) = 0
                      | paced = sdPacingMs drv
                      | otherwise = idleMs
                events <- waitForEvents timeout paced lastT
                wakeDue <-
                  if wakeAt > 0
                    then (>= wakeAt) <$> getMonotonicTime
                    else pure False
                -- The frame this wake is for asks again if it needs another.
                -- Clearing it here keeps a frame that cannot run (the drawing
                -- lock is held) from turning the deadline into a spin.
                when wakeDue (clearWakeAt ctx)
                -- A readout wait that timed out ends on its refresh.
                let hudDue = timeout == debugHudTimeout && debugActive && null events
                pure (events, dueNow || wakeDue || hudDue)

        now <- getMonotonicTime
        let !dt = min maxFrameDt (realToFrac (now - lastT))
            (frameInp, group, rest) =
              takeFrame (sdApplyEvent drv) (sdIsButtonEdge drv) (clearEphemeral inp {inputDeltaTime = dt}) pending
        -- Hard quit is ignored while a text editor is active.
        hardQuit <-
          if isHardQuitInput frameInp then not <$> textInputEditActive ctx else pure False
        -- A close request ends the session, or asks the view.
        let closing = any (sdIsSessionQuit drv) group
        closeNow <- if closing && not hardQuit then requestWindowClose ctx else pure False
        unless (hardQuit || closeNow) $ do
          noteDebugLoop (sdDebug drv) dt
          inpStamped <- stampClicks clickTracker frameInp
          (ctx', inpSynced) <- sdSyncDisplay drv ctx inpStamped
          shouldDraw <- if pendingDirty
            then pure True
            else sdShouldDraw drv ctx' inp inpSynced wasAnim refreshDue
          -- Force a full present only on the settle frame where an
          -- animation just finished (wasAnim && not animNow), so running
          -- animations keep clip damage.
          animNow <- anyAnimating ctx'
          dirtyOut <- if shouldDraw
            then sdDraw drv ctx' inpSynced (wasAnim && not animNow)
            else pendingDirty <$ noteDebugSkip (sdDebug drv)
          sdOnCursor drv ctx' inpSynced
          when closing (clearWindowClose ctx')
          animAfter <- anyAnimating ctx'
          traceLoopPass trace (length group) shouldDraw $
            (if pendingDirty then "D" else "")
              ++ (if wasAnim || animAfter then "A" else "")
              ++ (if inputWindowRedraw inpSynced then "R" else "")
              ++ (if refreshDue then "T" else "")
          -- Open modals/overlays consume Escape/Quit before the app sees it.
          overlayQuit <- overlayConsumesQuit ctx' inpSynced
          -- The next pass was animating if this frame was, even when this
          -- frame's tick finished the animation: its view read the value
          -- before that tick, so one more (settle) frame draws the end value.
          quit <- quitRequested ctx'
          unless (quit || (sdShouldQuit drv inpSynced && not overlayQuit)) $
            loop ctx' inpSynced rest now dirtyOut animNow

  loop ctx0 inp0 [] startT False False `finally` cancelTasks ctx0
