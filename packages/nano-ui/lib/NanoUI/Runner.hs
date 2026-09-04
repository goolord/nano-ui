{-# LANGUAGE BangPatterns #-}

-- | Universal session loop helpers: timing, click tracking, drawing locks, redraw predicates, and lifecycle checks.
module NanoUI.Runner
  ( -- * Timing
    maxFrameDt
  , stepDeltaTime
    -- * Click Tracking
  , ClickTracker (..)
  , newClickTracker
  , stampClicks
  , stampClicksWith
    -- * Drawing Lock
  , DrawingLock (..)
  , newDrawingLock
  , tryWithDrawingLock
    -- * Redraw Decision
  , shouldRedrawFrame
    -- * Session Loop Helpers
  , checkSessionQuit
  , checkHardQuit
    -- * Universal Session Runner
  , SessionDriver (..)
  , defaultWaitTimeout
  , runSessionLoop
  ) where

import Control.Exception (finally)
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

-- | Advance monotonic clock and calculate clamped delta-time.
stepDeltaTime :: Double -> IO (Double, Float)
stepDeltaTime lastT = do
  now <- getMonotonicTime
  let !dt = min maxFrameDt (realToFrac (now - lastT))
  pure (now, dt)

-- | State for multi-click detection (double/triple click).
newtype ClickTracker = ClickTracker (IORef (Double, V2, Int))

-- | Create a new click tracker initialized to no previous clicks.
newClickTracker :: IO ClickTracker
newClickTracker = ClickTracker <$> newIORef (0, V2 (-999) (-999), 0)

-- | Stamp multi-click counts into an 'Input' record using standard pixel thresholds.
stampClicks :: ClickTracker -> Input -> IO Input
stampClicks = stampClicksWith 5.0 0.4

-- | Stamp multi-click counts into an 'Input' record with custom distance and time thresholds.
stampClicksWith :: Float -> Double -> ClickTracker -> Input -> IO Input
stampClicksWith !distLimit !timeLimit (ClickTracker ref) inp
  | not (inputMousePressed inp) = pure inp
  | otherwise = do
      now <- getMonotonicTime
      (t, pos, n) <- readIORef ref
      let V2 x y = inputMousePos inp
          V2 px py = pos
          dx = x - px
          dy = y - py
          distSq = dx * dx + dy * dy
          close = distSq <= distLimit * distLimit
          quick = (now - t) <= timeLimit
          n' = if close && quick then min 3 (n + 1) else 1
      writeIORef ref (now, inputMousePos inp, n')
      pure (inp {inputMouseClicks = n'})

-- | Concurrency lock for drawing vs async callbacks (e.g. resize watchers).
newtype DrawingLock = DrawingLock (IORef Bool)

-- | Create a new unacquired drawing lock.
newDrawingLock :: IO DrawingLock
newDrawingLock = DrawingLock <$> newIORef False

-- | Attempt to execute an action under the drawing lock without blocking.
tryWithDrawingLock :: DrawingLock -> IO a -> IO (Maybe a)
tryWithDrawingLock (DrawingLock ref) act = do
  ok <- atomicModifyIORef' ref $ \busy -> if busy then (True, False) else (True, True)
  if ok
    then Just <$> (act `finally` writeIORef ref False)
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
      pure (need || anim || forceFinal || dirty || editing)

-- | Check if the session should terminate, respecting modals/overlays consuming Escape/Quit.
checkSessionQuit :: Context -> (Input -> Bool) -> Input -> IO Bool
checkSessionQuit ctx shouldQuit inp = do
  overlayQuit <- overlayConsumesQuit ctx inp
  pure (shouldQuit inp && not overlayQuit)

-- | Check if hard quit (e.g. Ctrl+C) was requested while not inside an active text editor.
checkHardQuit :: Context -> Input -> IO Bool
checkHardQuit ctx inp = do
  editActive <- textInputEditActive ctx
  pure (isHardQuitInput inp && not editActive)

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

-- | Standard heuristic timeout: 0ms if actively dirty or animating, 16ms if settling animations or editing text, -1ms (block) when idle.
defaultWaitTimeout :: Context -> Bool -> IO Int
defaultWaitTimeout ctx wasAnim = do
  anim <- anyAnimating ctx
  dirty <- isDirty ctx
  editing <- textFieldActive ctx
  if anim || dirty
    then pure 0
    else if wasAnim || editing
      then pure 16
      else pure (-1)

-- | Run an event-driven session loop until a termination event or user quit condition.
runSessionLoop ::
  SessionDriver ev ->
  Context ->
  Input ->
  IO ()
runSessionLoop drv ctx0 inp0 = do
  ctxRef <- newIORef ctx0
  prevInpRef <- newIORef inp0
  pendingDirtyRef <- newIORef False
  wasAnimRef <- newIORef False
  clickTracker <- newClickTracker
  startT <- getMonotonicTime

  let loop inp queued lastT = do
        ctx <- readIORef ctxRef
        pending <- if null queued
          then do
            pendingDirty <- readIORef pendingDirtyRef
            wasAnimWait <- readIORef wasAnimRef
            timeout <- if pendingDirty
              then pure 0
              else sdWaitTimeout drv ctx wasAnimWait
            if timeout == 0
              then do
                polled <- sdPollEvents drv
                if not (null polled)
                  then pure polled
                  else sdWaitEvents drv 0
              else if timeout > 0
                then do
                  polled <- sdPollEvents drv
                  if not (null polled)
                    then pure polled
                    else sdWaitEvents drv timeout
                else sdWaitEvents drv (-1)
          else pure queued

        let (group, rest) = splitFrame (sdIsButtonEdge drv) pending
        editActive <- textInputEditActive ctx
        let hardQuitEv = any (sdIsHardQuit drv) group && not editActive
            sessionQuitEv = any (sdIsSessionQuit drv) group
        if hardQuitEv || sessionQuitEv
          then pure ()
          else do
            (now, dt) <- stepDeltaTime lastT
            sdNoteLoop drv dt
            let inpFolded = foldl' (sdApplyEvent drv) (clearEphemeral inp {inputDeltaTime = dt}) group
            inpStamped <- stampClicksWith (sdClickDistance drv) (sdClickTime drv) clickTracker inpFolded
            (ctx', inpSynced) <- sdSyncDisplay drv ctx inpStamped
            writeIORef ctxRef ctx'
            hardQuit <- checkHardQuit ctx' inpSynced
            if hardQuit
              then pure ()
              else do
                prevInp <- readIORef prevInpRef
                wasAnim <- readIORef wasAnimRef
                pendingDirty <- readIORef pendingDirtyRef
                shouldDraw <- if pendingDirty
                  then pure True
                  else sdShouldDraw drv ctx' prevInp inpSynced wasAnim
                synced <- if shouldDraw
                  then do
                    (dirtyOut, s) <- sdDraw drv ctx' inpSynced wasAnim
                    writeIORef pendingDirtyRef dirtyOut
                    writeIORef prevInpRef s
                    pure s
                  else do
                    sdSkip drv ctx' inpSynced
                    sdOnCursor drv ctx' inpSynced
                    writeIORef prevInpRef inpSynced
                    pure inpSynced
                animAfter <- anyAnimating ctx'
                writeIORef wasAnimRef animAfter
                shouldTerm <- checkSessionQuit ctx' (sdShouldQuit drv) synced
                if shouldTerm
                  then pure ()
                  else loop synced rest now

  loop inp0 [] startT

