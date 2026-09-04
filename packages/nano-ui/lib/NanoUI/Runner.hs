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
  , inputMouseClicks
  , inputMousePos
  , inputMousePressed
  , isHardQuitInput
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
