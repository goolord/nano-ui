-- | Terminal events. notcurses maps these from @ncinput@;
-- 'NanoUI.Term.Vt' decodes them from VT byte streams for tests.
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
  | MouseScrollLeft
  | MouseScrollRight
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
