{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE PackageImports #-}

-- | SDL input translation: keycodes to 'Key', events folded into 'Input',
-- and native events through SDL's own queue.
module Input (inputChecks, queueChecks) where

import Control.Exception (IOException, try)
import Control.Monad (forM_, unless)
import Data.Foldable (toList)
import Data.Word (Word32)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, poke)
import NanoUI (Pressable (..), V2 (..))
import NanoUI.Backend
import SDL3.Sys.Bindgen.Events
  (SDL_MouseButtonEvent (..), SDL_MouseMotionEvent (..), SDL_RenderEvent (..), SDL_WindowEvent (..))
import SDL3.Sys.Bindgen.Events qualified as Events
import SDL3.Sys.Bindgen.Keycode
  ( SDL_Keymod (..), sDLK_1, sDLK_A, sDLK_EQUALS, sDLK_F1, sDLK_F12, sDLK_F13, sDLK_F24, sDLK_KP_4, sDLK_KP_5
  , sDLK_KP_ENTER, sDLK_KP_PLUS, sDLK_LSHIFT, sDLK_PAGEDOWN, sDLK_RETURN, sDLK_SPACE, sDL_KMOD_NONE, sDL_KMOD_NUM
  )
import SDL3.Sys.Bindgen.Mouse (sDL_BUTTON_MIDDLE, sDL_BUTTON_X1, sDL_BUTTON_X2)
import SDL3.Sys.Events (pushEvent)
import "nano-ui-sdl" NanoUI.Sdl.Internal.Input (SdlEvent (..), applyEvent, pollEvents, sdlKey)

checks :: String -> [(String, Bool)] -> IO ()
checks what cs = do
  forM_ cs $ \(name, ok) -> unless ok (fail (what <> ": " <> name))
  putStrLn ("SDL " <> what <> ": ok")

-- | Keycodes and folded keyboard and pointer events; no SDL needed.
inputChecks :: IO ()
inputChecks = do
  let keyWith mods code = sdlKey code (SDL_Keymod (fromIntegral mods))
      key = keyWith sDL_KMOD_NONE
      keypad = keyWith sDL_KMOD_NUM
      ctrl = noModifiers {modCtrl = True}
      shift = noModifiers {modShift = True}
      folded = foldl' applyEvent emptyInput
      chord = folded [EvModifiers ctrl, EvKey (KeyChar 's') True ctrl]
      letGo = folded [EvKey (KeyChar 's') True ctrl, EvKey (KeyChar 's') False ctrl, EvModifiers noModifiers]
      enterHeld = folded [EvKey KeyEnter True noModifiers, EvKey KeyEnter True noModifiers]
      blurred = applyEvent chord EvFocusLost
      typed = folded [EvKey (KeyChar 'x') True shift, EvText "X"]
      button b isDown inp = applyEvent inp (EvMouseButton b isDown (V2 5 5) (inputModifiers emptyInput))
      pressed = button MouseMiddle True emptyInput
      side = button MouseBack False (clearEphemeral (button MouseBack True emptyInput))
      dragOut = applyEvent pressed EvMouseLeave
      gone = applyEvent (button MouseMiddle False pressed) EvMouseLeave
      -- A click is read with the modifiers of the whole batch, which may
      -- already have let go of a key released just after it.
      ctrlClick = button MouseLeft True (applyEvent emptyInput (EvModifiers ctrl))
  checks "input translation"
    [ ("a letter is its character key", key sDLK_A == Just (KeyChar 'a'))
    , ("a digit and a symbol are character keys", map key [sDLK_1, sDLK_EQUALS] == map (Just . KeyChar) "1=")
    , ("Space and PageDown are named", key sDLK_SPACE == Just KeySpace && key sDLK_PAGEDOWN == Just KeyPageDown)
    , ("Return is Enter", key sDLK_RETURN == Just KeyEnter)
    , ("F1, F12, F13 and F24", map key [sDLK_F1, sDLK_F12, sDLK_F13, sDLK_F24] == map (Just . KeyF) [1, 12, 13, 24])
    , ("the keypad types with Num Lock", keypad sDLK_KP_4 == Just (KeyChar '4'))
    , ("the keypad navigates without Num Lock", key sDLK_KP_4 == Just KeyLeft && key sDLK_KP_5 == Nothing)
    , ("keypad Enter and plus", key sDLK_KP_ENTER == Just KeyEnter && keypad sDLK_KP_PLUS == Just (KeyChar '+'))
    , ("a modifier key is no key", key sDLK_LSHIFT == Nothing)
    , ( "a chord is a key with its modifiers and types nothing"
      , toList (inputKeys chord) == [KeyChar 's'] && inputModifiers chord == ctrl && inputChars chord == ""
      )
    , ("a pressed key is held", toList (inputKeysHeld chord) == [KeyChar 's'])
    , ( "a release is reported, and a modifier let go clears it"
      , toList (inputKeysReleased letGo) == [KeyChar 's'] && null (inputKeysHeld letGo)
          && inputModifiers letGo == noModifiers
      )
    , ( "Enter repeats, and a repeat is no new press"
      , toList (inputKeys enterHeld) == [KeyEnter, KeyEnter] && toList (inputKeysNew enterHeld) == [KeyEnter]
      )
    , ( "losing the keyboard lets go of the keys and modifiers held"
      , null (inputKeysHeld blurred) && toList (inputKeysReleased blurred) == [KeyChar 's']
          && inputModifiers blurred == noModifiers
      )
    , ( "typed text keeps the modifiers of the key that typed it"
      , inputChars typed == "X" && inputModifiers typed == shift
      )
    , ("a middle press is held", heldIn MouseMiddle pressed && pressedIn MouseMiddle pressed)
    , ("a side button is released", releasedIn MouseBack side && not (heldIn MouseBack side))
    , ("leaving moves the pointer off the window", let V2 x y = inputMousePos gone in x < -1000 && y < -1000)
    , ( "leaving during a drag keeps the pointer and the button"
      , inputMousePos dragOut == V2 5 5 && heldIn MouseMiddle dragOut
      )
    , ("a click keeps the modifiers of the keys before it", inputModifiers ctrlClick == ctrl)
    ]

-- | Push a raw SDL event and return what the backend decodes from the queue.
through :: Storable e => e -> IO [SdlEvent]
through ev = alloca $ \p -> do
  poke (castPtr p) ev
  pushEvent p >>= \ok -> unless ok (fail "SDL_PushEvent failed")
  pollEvents

-- | Native pointer, window, render and theme events, decoded from SDL's
-- queue, emptied first. SDL's video subsystem must be running.
queueChecks :: IO ()
queueChecks = do
  _ <- pollEvents
  -- SDL_EVENT_SYSTEM_THEME_CHANGED; only the type field is set.
  theme <- through (0x108 :: Word32)
  buttons <- mapM pushButton cases
  moved <- through (SDL_MouseMotionEvent Events.SDL_EVENT_MOUSE_MOTION 0 0 0 1 0 30 40 1 1)
  left <- through (SDL_WindowEvent Events.SDL_EVENT_WINDOW_MOUSE_LEAVE 0 0 0 0 0)
  targets <- through (SDL_RenderEvent Events.SDL_EVENT_RENDER_TARGETS_RESET 0 0 0)
  device <- through (SDL_RenderEvent Events.SDL_EVENT_RENDER_DEVICE_RESET 0 0 0)
  lost <- try (through (SDL_RenderEvent Events.SDL_EVENT_RENDER_DEVICE_LOST 0 0 0))
  checks "native events" $
    [ (name, case evs of [EvMouseButton b d _ _] -> b == want && d == isDown; _ -> False)
    | ((name, isDown, _, want), evs) <- zip cases buttons
    ]
      ++ [ ("the mouse moves the pointer", case moved of [EvMouseMotion (V2 30 40) _] -> True; _ -> False)
         , ("the pointer leaves the window", left == [EvMouseLeave])
         , ("lost render targets repaint the whole window", targets == [EvWindowRedraw])
         , ("a render device reset is told from lost render targets", device == [EvRenderDeviceReset])
         , ("a lost render device ends the session", either (\(_ :: IOException) -> True) (const False) lost)
         , ("the theme event", theme == [EvSystemThemeChanged])
         ]
  where
    cases =
      [ ("the middle button goes down", True, sDL_BUTTON_MIDDLE, MouseMiddle)
      , ("the middle button comes up", False, sDL_BUTTON_MIDDLE, MouseMiddle)
      , ("X1 is back", True, sDL_BUTTON_X1, MouseBack)
      , ("X2 is forward", True, sDL_BUTTON_X2, MouseForward)
      , ("X2 comes up", False, sDL_BUTTON_X2, MouseForward)
      , ("a sixth button is another", True, 6, MouseOther 6)
      ]
    pushButton (_, isDown, b, _) =
      through
        SDL_MouseButtonEvent
          { type' = if isDown then Events.SDL_EVENT_MOUSE_BUTTON_DOWN else Events.SDL_EVENT_MOUSE_BUTTON_UP
          , reserved = 0, timestamp = 0, windowID = 0, which = 1, button = fromIntegral b
          , down = CBool (if isDown then 1 else 0), clicks = 1, padding = 0, x = 30, y = 40
          }
