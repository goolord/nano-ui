-- | The RGFW session's wake: another thread can interrupt the loop's event wait.
module NanoUI.Rgfw.Wake (testRgfwWake) where

import Control.Concurrent (forkIO, getNumCapabilities, newEmptyMVar, setNumCapabilities, takeMVar, threadDelay, tryPutMVar, yield)
import Control.Exception (SomeException, try)
import Control.Monad (join, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import Foreign.C.Types (CInt (..))
import GHC.Clock (getMonotonicTime)
import NanoUI (Size (..), WindowSettings (..), askWake, defaultWindowSettings, label, quitUi, useTask)
import NanoUI.Backend.Rgfw (RgfwOptions (..), defaultRgfwOptions, runRgfwApp)
import System.CPUTime (getCPUTime)
import System.Environment (lookupEnv)
import System.IO (hFlush, stdout)
import System.Info (os)
import System.Timeout (timeout)

foreign import ccall "exit" c_exit :: CInt -> IO ()

-- | A background job's wake shows its result while the loop waits for
-- events; after a stream faster than frames stops, the loop idles instead of
-- spinning; 'quitUi' ends the session. Skipped without an X display. A missed
-- wake would block forever, so a failed check exits the process.
testRgfwWake :: IO ()
testRgfwWake = do
  display <- lookupEnv "DISPLAY"
  if os /= "linux" || maybe True null display
    then putStrLn "[SKIP] RGFW wake: no X display"
    else do
      -- Two capabilities let the producer wake the loop while it is drawing,
      -- not only while it waits.
      caps <- getNumCapabilities
      when (caps < 2) (setNumCapabilities 2)
      passes <- newIORef (0 :: Int)
      phase <- newIORef (0 :: Int)
      wakeRef <- newIORef (pure ())
      latest <- newIORef (0 :: Int)
      shown <- newEmptyMVar
      streamed <- newEmptyMVar
      let failNow msg = putStrLn ("[FAIL] RGFW wake: " ++ msg) >> hFlush stdout >> c_exit 1
          await what signal = timeout 5000000 (takeMVar signal) >>= maybe (failNow (what ++ " never showed")) pure
          next p = writeIORef phase p >> join (readIORef wakeRef)
          -- For half a second, write each value where the view reads it and
          -- wake, faster than frames; then write -1 to mark the end.
          produce wake = do
            t0 <- getMonotonicTime
            let go i = do
                  atomicWriteIORef latest i >> wake
                  t <- getMonotonicTime
                  if t - t0 < 0.5 then yield >> go (i + 1) else atomicWriteIORef latest (-1) >> wake
            go 1
          view = do
            liftIO (atomicModifyIORef' passes (\n -> (n + 1, ())))
            wake <- askWake
            liftIO (writeIORef wakeRef wake)
            liftIO (readIORef phase) >>= \case
              0 -> do
                r <- useTask ("job" :: String) (threadDelay 300000 >> pure (42 :: Int))
                when (r == Just 42) (liftIO (void (tryPutMVar shown ())))
                label "job"
              1 -> do
                _ <- useTask ("stream" :: String) (produce wake)
                v <- liftIO (readIORef latest)
                when (v == -1) (liftIO (void (tryPutMVar streamed ())))
                label "stream"
              _ -> quitUi
      _ <- forkIO $ do
        await "the job's result" shown
        next 1
        await "the stream's last value" streamed
        threadDelay 300000
        c0 <- getCPUTime
        f0 <- readIORef passes
        threadDelay 1000000
        c1 <- getCPUTime
        f1 <- readIORef passes
        let cpu = fromIntegral (c1 - c0) / 1e12 :: Double
        when (cpu > 0.3 || f1 /= f0) $
          failNow ("after the stream the loop used " ++ show cpu ++ " s of CPU and ran " ++ show (f1 - f0) ++ " passes in a second")
        next 2
      try (runRgfwApp defaultRgfwOptions {optWindow = defaultWindowSettings {wsSize = Size 200 150}} view) >>= \case
        Right () ->
          readIORef phase >>= \case
            2 -> putStrLn "[PASS] RGFW wake: a job's result and a stream show, the loop idles after, and quitUi ends it"
            _ -> failNow "the session ended by itself"
        Left (e :: SomeException) -> failNow ("the session ended with " ++ show e)
