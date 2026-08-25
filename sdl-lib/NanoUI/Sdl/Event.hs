module NanoUI.Sdl.Event (SdlEvent (..)) where

import NanoUI (Key, Modifiers, V2 (..))

data SdlEvent
  = EvQuit
  | EvResize Int Int
  | EvDisplayScale
  | EvKey Key Modifiers
  | EvText String Modifiers
  | EvMouseMotion V2 Modifiers
  | EvMousePress V2 Modifiers
  | EvMouseRelease V2 Modifiers
  | EvScroll V2
  deriving (Eq, Show)
