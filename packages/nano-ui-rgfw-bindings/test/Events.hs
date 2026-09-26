module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (replicateM_, unless, void)
import Data.Bits ((.|.))
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import GHC.Clock (getMonotonicTime, getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import RGFW (Event (..), closeWindow, createWindowGL, pollEvent, stopWaitForEvent, waitForEvent, withEventBuffer)
import RGFW.Raw
import System.Environment (getArgs, lookupEnv)
import System.Info (os)
import Text.Printf (printf)

foreign import ccall unsafe "rgfw_test_event_size" nativeSize :: IO CSize

foreign import ccall unsafe "rgfw_test_event"
  writeEvent :: Ptr RGFW_event -> CInt -> IO ()

check :: (Eq a, Show a) => String -> a -> IO a -> IO ()
check label expected action = action >>= \actual -> unless (actual == expected) (fail (label ++ ": " ++ show actual))

main :: IO ()
main = withEventBuffer $ \event -> do
  size <- c_rgfw_event_size
  expectedSize <- nativeSize
  unless (size == expectedSize) (fail "RGFW event allocation size differs from C")
  -- Each C fixture writes one kind of event through the native types.
  let fixture kind tag = writeEvent event kind >> check "tag" (fromIntegral tag) (c_rgfw_event_type event)
      pair :: (Ptr RGFW_event -> IO a) -> (Ptr RGFW_event -> IO b) -> IO (a, b)
      pair a b = (,) <$> a event <*> b event
  fixture 0 rgfw_keyPressed
  check "key width excludes repeat/mod/state bytes" (fromIntegral rgfw_keyHome) (c_rgfw_event_key_value event)
  check "modifiers" (fromIntegral (rgfw_modControl .|. rgfw_modShift)) (c_rgfw_event_key_mod event)
  check "repeat" 1 (c_rgfw_event_key_repeat event)
  fixture 1 rgfw_keyChar
  check "Unicode character" 0x1f600 (c_rgfw_event_keyChar_value event)
  fixture 2 rgfw_mouseMotion
  check "motion" (-130, 245) (pair c_rgfw_event_mouse_x c_rgfw_event_mouse_y)
  fixture 3 rgfw_mouseScroll
  check "wheel" (1.25, -2.5) (pair c_rgfw_event_delta_x c_rgfw_event_delta_y)
  fixture 4 rgfw_mouseButtonPressed
  check "button" (fromIntegral rgfw_mouseRight) (c_rgfw_event_button_value event)
  fixture 5 rgfw_windowResized
  check "resize" (643, 481) (pair c_rgfw_event_update_w c_rgfw_event_update_h)
  fixture 6 rgfw_scaleUpdated
  check "scale" (1.25, 1.5) (pair c_rgfw_event_scale_x c_rgfw_event_scale_y)
  -- The ranges "RGFW.Raw" says are consecutive.
  check "function keys are consecutive" (23 :: Int) (pure (fromIntegral (rgfw_keyF24 - rgfw_keyF1)))
  check "keypad digits are consecutive" (8 :: Int) (pure (fromIntegral (rgfw_keyPad9 - rgfw_keyPad1)))
  args <- getArgs
  if args == ["--bench"]
    then do
      writeEvent event 0
      let loop !n !acc
            | n == (0 :: Int) = pure acc
            | otherwise = c_rgfw_event_key_value event >>= \key -> loop (n - 1) (acc + fromIntegral key :: Int)
      before <- getAllocationCounter
      t0 <- getMonotonicTimeNSec
      total <- loop 10000000 0
      t1 <- getMonotonicTimeNSec
      after <- getAllocationCounter
      unless (total == 10000000 * fromIntegral rgfw_keyHome) (fail "event benchmark checksum")
      printf "event-key: %.6f ns/read | %.3f B/read\n" (fromIntegral (t1 - t0) / 1e7 :: Double) (fromIntegral (before - after) / 1e7 :: Double)
    else do
      putStrLn "RGFW native event ABI: ok"
      stopWaitCheck event

-- | A stop from another thread ends a wait, one made before a wait makes it
-- return at once, and stops that piled up while nothing waited are drained
-- by the next wait, so the wait after that waits again rather than returning
-- at once for good. Needs an X display, and is skipped without one.
stopWaitCheck :: Ptr RGFW_event -> IO ()
stopWaitCheck event = do
  display <- lookupEnv "DISPLAY"
  if os /= "linux" || maybe True null display
    then putStrLn "RGFW stop wait: skipped (no X display)"
    else
      createWindowGL "stop wait" 0 0 64 64 rgfw_windowHide 3 2 >>= \case
        Nothing -> fail "RGFW stop wait: no window"
        Just win -> do
          let drain = pollEvent win event >>= \case
                EventNone -> pure ()
                _ -> drain
              -- Seconds a wait of up to @ms@ took, with no events queued before it.
              timedWait ms = do
                drain
                t0 <- getMonotonicTime
                waitForEvent ms
                subtract t0 <$> getMonotonicTime
              endsWithin limit what stop = do
                stop
                took <- timedWait 5000
                unless (took < limit) (fail ("RGFW stop wait: " ++ what ++ " ended the wait after " ++ show took ++ " s"))
          endsWithin 2 "a stop from another thread" (void (forkIO (threadDelay 100000 >> stopWaitForEvent)))
          endsWithin 1 "a stop before the wait" stopWaitForEvent
          endsWithin 1 "piled-up stops" (replicateM_ 100 stopWaitForEvent)
          -- An event can end a wait early; one of three waiting is enough.
          waited <- mapM (const (timedWait 200)) [1 .. 3 :: Int]
          unless (any (>= 0.15) waited) (fail ("RGFW stop wait: the waits after the drain returned at once: " ++ show waited))
          closeWindow win
          putStrLn "RGFW stop wait: ok"
