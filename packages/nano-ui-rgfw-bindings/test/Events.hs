module Main (main) where

import Control.Monad (unless)
import Data.Bits ((.|.))
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Ptr (Ptr)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getAllocationCounter)
import RGFW (withEventBuffer)
import RGFW.Raw
import System.Environment (getArgs)
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
    else putStrLn "RGFW native event ABI: ok"
