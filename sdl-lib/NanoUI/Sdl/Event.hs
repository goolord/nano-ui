module NanoUI.Sdl.Event (SdlEvent (..)) where

import Data.Text (Text)
import NanoUI (Key, Modifiers, V2 (..))

data SdlEvent
  = EvQuit
  | EvResize Int Int
  | EvDisplayScale
  | EvKey Key Modifiers
  | EvText Text Modifiers
  | EvMouseMotion V2 Modifiers
  | EvMousePress V2 Modifiers Int
  | EvMouseRelease V2 Modifiers
  | EvMouseRightPress V2 Modifiers
  | EvMouseRightRelease V2 Modifiers
  | EvScroll V2
  | EvRefresh
  deriving (Eq, Show)
