module NanoUI.Input
  ( Key (..)
  , Modifiers (..)
  , Input (..)
  , emptyInput
  , inputChanged
  ) where

import NanoUI.Types (Size (..), V2 (..))

data Key
  = KeyBackspace
  | KeyDelete
  | KeyEnter
  | KeyEscape
  | KeyTab
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyHome
  | KeyEnd
  deriving (Eq, Show, Enum, Bounded)

data Modifiers = Modifiers
  { modShift :: Bool
  , modCtrl :: Bool
  , modAlt :: Bool
  }
  deriving (Eq, Show)

data Input = Input
  { inputMousePos :: V2
  , inputMouseDown :: Bool
  , inputMousePressed :: Bool
  , inputMouseReleased :: Bool
  , inputMouseRightDown :: Bool
  , inputMouseRightPressed :: Bool
  , inputMouseRightReleased :: Bool
  , inputMouseClicks :: Int
  , inputScroll :: V2
  , inputKeys :: [Key]
  , inputChars :: [Char]
  , inputModifiers :: Modifiers
  , inputWindowSize :: Size
  , inputDeltaTime :: Float
  }
  deriving (Eq, Show)

emptyInput :: Input
emptyInput =
  Input
    { inputMousePos = V2 0 0
    , inputMouseDown = False
    , inputMousePressed = False
    , inputMouseReleased = False
    , inputMouseRightDown = False
    , inputMouseRightPressed = False
    , inputMouseRightReleased = False
    , inputMouseClicks = 1
    , inputScroll = V2 0 0
    , inputKeys = []
    , inputChars = []
    , inputModifiers = Modifiers False False False
    , inputWindowSize = Size 800 600
    , inputDeltaTime = 0
    }

inputChanged :: Input -> Input -> Bool
inputChanged a b =
  inputMousePos a /= inputMousePos b
    || inputMouseDown a /= inputMouseDown b
    || inputMousePressed a /= inputMousePressed b
    || inputMouseReleased a /= inputMouseReleased b
    || inputMouseRightDown a /= inputMouseRightDown b
    || inputMouseRightPressed a /= inputMouseRightPressed b
    || inputMouseRightReleased a /= inputMouseRightReleased b
    || inputMouseClicks a /= inputMouseClicks b
    || inputScroll a /= inputScroll b
    || inputKeys a /= inputKeys b
    || inputChars a /= inputChars b
    || inputModifiers a /= inputModifiers b
    || inputWindowSize a /= inputWindowSize b
