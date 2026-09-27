{-# LANGUAGE PackageImports #-}

-- | SDL keyboard translation: SDL keycodes to 'Key', key events folded into
-- 'Input' (auto-repeats included), and losing keyboard focus.
module Keyboard (keyboardTranslation) where

import Control.Monad (unless)
import Data.Foldable (toList)
import NanoUI.Backend (Input (..), Key (..), Modifiers (..), emptyInput, noModifiers)
import "nano-ui-sdl" NanoUI.Sdl.Internal.Input (SdlEvent (..), applyEvent, sdlKey)
import SDL3.Sys.Bindgen.Keycode
  ( SDL_Keymod (..), sDLK_1, sDLK_A, sDLK_EQUALS, sDLK_F1, sDLK_F12, sDLK_F13, sDLK_F24, sDLK_KP_4, sDLK_KP_5
  , sDLK_KP_ENTER, sDLK_KP_PLUS, sDLK_LSHIFT, sDLK_PAGEDOWN, sDLK_RETURN, sDLK_SPACE, sDL_KMOD_NONE, sDL_KMOD_NUM
  )

keyboardTranslation :: IO ()
keyboardTranslation = do
  let keyWith mods code = sdlKey code (SDL_Keymod (fromIntegral mods))
      key = keyWith sDL_KMOD_NONE
      keypad = keyWith sDL_KMOD_NUM
      check name ok = unless ok (fail ("keyboard: " <> name))
  check "a letter is its character key" (key sDLK_A == Just (KeyChar 'a'))
  check "a digit and a symbol are character keys" (key sDLK_1 == Just (KeyChar '1') && key sDLK_EQUALS == Just (KeyChar '='))
  check "Space and PageDown are named" (key sDLK_SPACE == Just KeySpace && key sDLK_PAGEDOWN == Just KeyPageDown)
  check "Return is Enter" (key sDLK_RETURN == Just KeyEnter)
  check "F1, F12, F13 and F24" $
    map key [sDLK_F1, sDLK_F12, sDLK_F13, sDLK_F24] == map (Just . KeyF) [1, 12, 13, 24]
  check "the keypad types with Num Lock" (keypad sDLK_KP_4 == Just (KeyChar '4'))
  check "the keypad navigates without Num Lock" (key sDLK_KP_4 == Just KeyLeft && key sDLK_KP_5 == Nothing)
  check "keypad Enter and plus" (key sDLK_KP_ENTER == Just KeyEnter && keypad sDLK_KP_PLUS == Just (KeyChar '+'))
  check "a modifier key is no key" (key sDLK_LSHIFT == Nothing)
  let ctrl = noModifiers {modCtrl = True}
      chord = foldl' applyEvent emptyInput [EvModifiers ctrl, EvKey (KeyChar 's') True ctrl]
      letGo = foldl' applyEvent emptyInput [EvKey (KeyChar 's') True ctrl, EvKey (KeyChar 's') False ctrl, EvModifiers noModifiers]
      enterHeld = foldl' applyEvent emptyInput [EvKey KeyEnter True noModifiers, EvKey KeyEnter True noModifiers]
      blurred = applyEvent chord EvFocusLost
  check "a chord is a key with its modifiers and types nothing" $
    toList (inputKeys chord) == [KeyChar 's'] && inputModifiers chord == ctrl && inputChars chord == ""
  check "a pressed key is held" (toList (inputKeysHeld chord) == [KeyChar 's'])
  check "a release is reported, and a modifier let go clears it" $
    toList (inputKeysReleased letGo) == [KeyChar 's'] && null (inputKeysHeld letGo) && inputModifiers letGo == noModifiers
  check "Enter repeats, and a repeat is no new press" $
    toList (inputKeys enterHeld) == [KeyEnter, KeyEnter] && toList (inputKeysNew enterHeld) == [KeyEnter]
  check "losing the keyboard lets go of the keys and modifiers held" $
    null (inputKeysHeld blurred) && toList (inputKeysReleased blurred) == [KeyChar 's'] && inputModifiers blurred == noModifiers
  putStrLn "SDL keyboard translation: ok"
