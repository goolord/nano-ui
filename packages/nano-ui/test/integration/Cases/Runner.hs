module Cases.Runner (runSessionLoopTest, runDrawingLockTest) where

import Control.Exception
  ( IOException
  , MaskingState (Unmasked)
  , getMaskingState
  , throwIO
  , try
  )
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import NanoUI (Input (..), V2 (..), emptyInput)
import NanoUI.Runner
import NanoUI.Testing (Context)
import NanoUI.Testing.Assert (assertEq)

-- Exercise queued edges, a dirty follow-up frame, skipped input, and blocking
-- waits without requiring a native window or depending on wall-clock timing.
runSessionLoopTest :: Context -> IORef Int -> IO ()
runSessionLoopTest ctx failed = do
  logRef <- newIORef []
  waits <- newIORef [(-1, [1, 2]), (0, []), (-1, []), (-1, [3 :: Int])]
  draws <- newIORef (0 :: Int)
  let
    note message = modifyIORef' logRef (<> [message])
    driver =
      SessionDriver
        { sdPollEvents = do
            note "poll"
            pure []
        , sdWaitEvents = \timeout -> do
            note ("wait " <> show timeout)
            (expected, events) <- atomicModifyIORef' waits $ \batches -> case batches of
              batch : rest -> (rest, batch)
              [] -> ([], (timeout, [3]))
            assertEq failed expected timeout
            pure events
        , sdApplyEvent = \inp event -> inp {inputMousePos = V2 (fromIntegral event) 0}
        , sdIsButtonEdge = const True
        , sdIsHardQuit = const False
        , sdIsSessionQuit = (== 3)
        , sdSyncDisplay = \c inp -> pure (c, inp)
        , sdWaitTimeout = \_ _ -> pure (-1)
        , sdAlignSec = 0
        , sdShouldDraw = \_ previous current _ -> do
            note ("decide " <> show (inputMousePos previous, inputMousePos current))
            pure (inputMousePos current == V2 1 0)
        , sdDraw = \_ inp _ -> do
            n <- atomicModifyIORef' draws (\n -> (n + 1, n + 1))
            note "draw"
            pure (n <= 2, inp {inputMousePos = V2 10 0})
        , sdSkip = \_ _ -> note "skip"
        , sdOnCursor = \_ _ -> note "cursor"
        , sdNoteLoop = const (pure ())
        , sdShouldQuit = const False
        , sdClickDistance = 4
        , sdClickTime = 0.5
        }
  runSessionLoop driver ctx emptyInput
  actual <- readIORef logRef
  assertEq
    failed
    [ "wait -1"
    , "decide " <> show (V2 0 0, V2 1 0)
    , "draw"
    , "draw"
    , "poll"
    , "wait 0"
    , "draw"
    , "wait -1"
    , "decide " <> show (V2 10 0, V2 10 0)
    , "skip"
    , "cursor"
    , "wait -1"
    ]
    actual
  assertEq failed 3 =<< readIORef draws

runDrawingLockTest :: Context -> IORef Int -> IO ()
runDrawingLockTest _ failed = do
  lock <- newDrawingLock
  result <- tryWithDrawingLock lock $ do
    assertEq failed Unmasked =<< getMaskingState
    assertEq failed Nothing =<< tryWithDrawingLock lock (pure ())
  assertEq failed (Just ()) result
  failure <-
    try (tryWithDrawingLock lock (throwIO (userError "draw failed"))) ::
      IO (Either IOException (Maybe ()))
  assertEq failed True (either (const True) (const False) failure)
  assertEq failed (Just ()) =<< tryWithDrawingLock lock (pure ())
