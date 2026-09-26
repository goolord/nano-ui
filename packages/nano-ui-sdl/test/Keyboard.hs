{-# LANGUAGE PackageImports #-}

-- | SDL keyboard translation: which 'Key' an SDL keycode is, whether it
-- repeats, and how key events fold into 'Input'.
module Keyboard (keyboardTranslation) where

import Control.Monad (unless)
import Data.Foldable (toList)
import NanoUI.Backend (Input (..), Key (..), Modifiers (..), emptyInput, keyRepeats, noModifiers)
import "nano-ui-sdl" NanoUI.Sdl.Internal.Input (SdlEvent (..), applyEvent, sdlKey)
import SDL3.Sys.Bindgen.Keycode
  ( SDL_Keymod (..), sDLK_1, sDLK_A, sDLK_EQUALS, sDLK_F1, sDLK_F12, sDLK_F13, sDLK_F24, sDLK_KP_4, sDLK_KP_5
  , sDLK_KP_ENTER, sDLK_KP_PLUS, sDLK_LSHIFT, sDLK_PAGEDOWN, sDLK_RETURN, sDLK_SPACE, sDL_KMOD_NONE, sDL_KMOD_NUM
  )

keyboardTranslation :: IO ()
keyboardTranslation = do
  let keyWith mods code = (\k -> (k, keyRepeats k)) <$> sdlKey code (SDL_Keymod (fromIntegral mods))
      key = keyWith sDL_KMOD_NONE
      keypad = keyWith sDL_KMOD_NUM
      check name ok = unless ok (fail ("keyboard: " <> name))
  check "a letter is its character key and repeats" (key sDLK_A == Just (KeyChar 'a', True))
  check "a digit and a symbol are character keys" (key sDLK_1 == Just (KeyChar '1', True) && key sDLK_EQUALS == Just (KeyChar '=', True))
  check "Space and PageDown are named" (key sDLK_SPACE == Just (KeySpace, True) && key sDLK_PAGEDOWN == Just (KeyPageDown, True))
  check "Enter does not repeat" (key sDLK_RETURN == Just (KeyEnter, False))
  check "F1, F12, F13 and F24" $
    map (fmap fst . key) [sDLK_F1, sDLK_F12, sDLK_F13, sDLK_F24] == map (Just . KeyF) [1, 12, 13, 24]
  check "the keypad types with Num Lock" (keypad sDLK_KP_4 == Just (KeyChar '4', True))
  check "the keypad navigates without Num Lock" (key sDLK_KP_4 == Just (KeyLeft, True) && key sDLK_KP_5 == Nothing)
  check "keypad Enter and plus" (key sDLK_KP_ENTER == Just (KeyEnter, False) && keypad sDLK_KP_PLUS == Just (KeyChar '+', True))
  check "a modifier key is no key" (key sDLK_LSHIFT == Nothing)
  let ctrl = noModifiers {modCtrl = True}
      pressed = foldl' applyEvent emptyInput [EvModifiers ctrl, EvKey (KeyChar 's') ctrl]
      released = foldl' applyEvent emptyInput [EvKey (KeyChar 's') ctrl, EvKeyUp (KeyChar 's') ctrl, EvModifiers noModifiers]
  check "a chord is a key with its modifiers and types nothing" $
    toList (inputKeys pressed) == [KeyChar 's'] && inputModifiers pressed == ctrl && inputChars pressed == ""
  check "a pressed key is held" (toList (inputKeysHeld pressed) == [KeyChar 's'])
  check "a release is reported, and a modifier let go clears it" $
    toList (inputKeysReleased released) == [KeyChar 's'] && null (inputKeysHeld released) && inputModifiers released == noModifiers
  putStrLn "SDL keyboard translation: ok"
