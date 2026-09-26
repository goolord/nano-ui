{-# LANGUAGE DisambiguateRecordFields #-}

-- | SDL pointer translation: the middle, side and extra buttons, the
-- mouse's motion, and the pointer leaving the window. Events go through
-- SDL's own queue, on the dummy video driver.
module Main (main) where

import Control.Monad (forM_, unless)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, poke)
import NanoUI (V2 (..), buttonHeld, buttonPressed, buttonReleased)
import NanoUI.Backend
import NanoUI.Sdl.Internal.Input (SdlEvent (..), applyEvent, pollEvents)
import SDL3.Sys.Bindgen.Events (SDL_MouseButtonEvent (..), SDL_MouseMotionEvent (..), SDL_WindowEvent (..))
import SDL3.Sys.Bindgen.Events qualified as Events
import SDL3.Sys.Bindgen.Init (SDL_InitFlags (..), sDL_INIT_VIDEO)
import SDL3.Sys.Bindgen.Mouse (sDL_BUTTON_MIDDLE, sDL_BUTTON_X1, sDL_BUTTON_X2)
import SDL3.Sys.Events (pushEvent)
import SDL3.Sys.Init (initSafe, quitSafe)
import System.Environment (setEnv)
import System.Exit (exitFailure)

check :: String -> Bool -> IO ()
check name ok = do
  putStrLn ((if ok then "[PASS] " else "[FAIL] ") <> name)
  unless ok exitFailure

-- | Queue an event of SDL's and take back what the backend makes of the queue.
through :: Storable e => e -> IO [SdlEvent]
through ev = alloca $ \p -> do
  poke (castPtr p) ev
  pushEvent p >>= check "SDL takes the event"
  pollEvents

main :: IO ()
main = do
  setEnv "SDL_VIDEODRIVER" "dummy"
  initSafe (SDL_InitFlags (fromIntegral sDL_INIT_VIDEO)) >>= check "SDL starts"
  forM_
    [ ("the middle button goes down", True, sDL_BUTTON_MIDDLE, MouseMiddle)
    , ("the middle button comes up", False, sDL_BUTTON_MIDDLE, MouseMiddle)
    , ("X1 is back", True, sDL_BUTTON_X1, MouseBack)
    , ("X2 is forward", True, sDL_BUTTON_X2, MouseForward)
    , ("X2 comes up", False, sDL_BUTTON_X2, MouseForward)
    , ("a sixth button is another", True, 6, MouseOther 6)
    ]
    $ \(name, isDown, sdlButton, want) -> do
      events <-
        through
          SDL_MouseButtonEvent
            { type' = if isDown then Events.SDL_EVENT_MOUSE_BUTTON_DOWN else Events.SDL_EVENT_MOUSE_BUTTON_UP
            , reserved = 0, timestamp = 0, windowID = 0, which = 1, button = fromIntegral sdlButton
            , down = CBool (if isDown then 1 else 0), clicks = 1, padding = 0, x = 30, y = 40
            }
      check name $ case events of
        [EvMouseButton b d _ _] -> b == want && d == isDown
        _ -> False
  moved <-
    through
      SDL_MouseMotionEvent
        { type' = Events.SDL_EVENT_MOUSE_MOTION, reserved = 0, timestamp = 0, windowID = 0, which = 1
        , state = 0, x = 30, y = 40, xrel = 1, yrel = 1
        }
  check "the mouse moves the pointer" $ case moved of
    [EvMouseMotion (V2 30 40) _] -> True
    _ -> False
  left <-
    through
      SDL_WindowEvent
        { type' = Events.SDL_EVENT_WINDOW_MOUSE_LEAVE, reserved = 0, timestamp = 0, windowID = 0, data1 = 0, data2 = 0
        }
  check "the pointer leaves the window" (left == [EvMouseLeave])
  quitSafe
  let button b isDown inp = applyEvent inp (EvMouseButton b isDown (V2 5 5) (inputModifiers emptyInput))
      pressed = button MouseMiddle True emptyInput
      side = button MouseBack False (clearEphemeral (button MouseBack True emptyInput))
      gone = applyEvent pressed EvMouseLeave
  check "a middle press is held" (buttonHeld MouseMiddle pressed && buttonPressed MouseMiddle pressed)
  check "a side button is released" (buttonReleased MouseBack side && not (buttonHeld MouseBack side))
  check "leaving moves the pointer off the window and keeps the button" $
    let V2 x y = inputMousePos gone in x < -1000 && y < -1000 && buttonHeld MouseMiddle gone
