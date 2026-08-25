-- | Terminal events, as produced by every platform driver. The Windows driver
-- builds these from console input records; POSIX builds them by decoding VT
-- byte sequences. Both funnel into the same 'NanoUI.Input.Input' folding.
module NanoUI.Term.Event
  ( MouseBtn (..)
  , MouseAction (..)
  , TermEvent (..)
  , noMods
  ) where

import NanoUI (Key, Modifiers (..))

data MouseBtn = BtnLeft | BtnMiddle | BtnRight
  deriving (Eq, Show)

data MouseAction
  = MousePress MouseBtn
  | MouseRelease (Maybe MouseBtn)
  | -- | Pointer moved with no button held: hover.
    MouseMove
  | -- | Pointer moved with a button held.
    MouseDrag MouseBtn
  | MouseScrollUp
  | MouseScrollDown
  deriving (Eq, Show)

data TermEvent
  = -- | Column and row are zero-based cell coordinates.
    EvMouse MouseAction Int Int Modifiers
  | EvKey Key Modifiers
  | EvChar Char Modifiers
  | EvResize Int Int
  deriving (Eq, Show)

noMods :: Modifiers
noMods = Modifiers False False False
