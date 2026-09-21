module Main (main) where

import Control.Monad (forM_, unless)
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

main :: IO ()
main = withEventBuffer $ \event -> do
  size <- c_rgfw_event_size
  expectedSize <- nativeSize
  unless (size == expectedSize) (fail "RGFW event allocation size differs from C")
  let
    check label expected action = do
      actual <- action
      unless (actual == expected) (fail (label ++ ": " ++ show actual))
  forM_
    ( zip
        [0 ..]
        [ rgfw_keyPressed
        , rgfw_keyChar
        , rgfw_mouseMotion
        , rgfw_mouseScroll
        , rgfw_mouseButtonPressed
        , rgfw_windowResized
        , rgfw_scaleUpdated
        ]
    ) $ \(kind, tag) -> do
    writeEvent event kind
    check "tag" (fromIntegral tag) (c_rgfw_event_type event)
    case kind of
      0 -> do
        check
          "key width excludes repeat/mod/state bytes"
          (fromIntegral rgfw_keyHome)
          (c_rgfw_event_key_value event)
        check
          "modifiers"
          (fromIntegral (rgfw_modControl .|. rgfw_modShift))
          (c_rgfw_event_key_mod event)
      1 -> check "Unicode character" 0x1f600 (c_rgfw_event_keyChar_value event)
      2 ->
        check
          "motion"
          (-130, 245)
          ((,) <$> c_rgfw_event_mouse_x event <*> c_rgfw_event_mouse_y event)
      3 ->
        check
          "wheel"
          (1.25, -2.5)
          ((,) <$> c_rgfw_event_delta_x event <*> c_rgfw_event_delta_y event)
      4 ->
        check "button" (fromIntegral rgfw_mouseRight) (c_rgfw_event_button_value event)
      5 ->
        check
          "resize"
          (643, 481)
          ((,) <$> c_rgfw_event_update_w event <*> c_rgfw_event_update_h event)
      6 ->
        check
          "scale"
          (1.25, 1.5)
          ((,) <$> c_rgfw_event_scale_x event <*> c_rgfw_event_scale_y event)
      _ -> fail "unknown fixture"
  args <- getArgs
  if args == ["--bench"]
    then do
      writeEvent event 0
      let
        loop !n !acc
          | n == (0 :: Int) = pure acc
          | otherwise = do
              key <- c_rgfw_event_key_value event
              loop (n - 1) (acc + fromIntegral key :: Int)
      before <- getAllocationCounter
      t0 <- getMonotonicTimeNSec
      total <- loop 10000000 0
      t1 <- getMonotonicTimeNSec
      after <- getAllocationCounter
      unless
        (total == 10000000 * fromIntegral rgfw_keyHome)
        (fail "event benchmark checksum")
      printf
        "event-key: %.6f ns/read | %.3f B/read\n"
        (fromIntegral (t1 - t0) / 1e7 :: Double)
        (fromIntegral (before - after) / 1e7 :: Double)
    else putStrLn "RGFW native event ABI: ok"
