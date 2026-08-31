{-# LANGUAGE StrictData #-}

module NanoUI.Input
  ( Key (..)
  , Modifiers (..)
  , Input (..)
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  , appendInputKey
  , inputKeysNull
  , inputKeysElem
  , foldInputKeys
  , inputKeysFromList
  , emptyInputKeys
  , stripInteractionInput
  ) where

import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
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
  { modShift :: !Bool
  , modCtrl :: !Bool
  , modAlt :: !Bool
  }
  deriving (Eq, Show)

data Input = Input
  { inputMousePos :: !V2
  , inputMouseDown :: !Bool
  , inputMousePressed :: !Bool
  , inputMouseReleased :: !Bool
  , inputMouseRightDown :: !Bool
  , inputMouseRightPressed :: !Bool
  , inputMouseRightReleased :: !Bool
  , inputMouseClicks :: !Int
  , inputScroll :: !V2
  , inputKeys :: Vector Key
  , inputChars :: !Text
  , inputModifiers :: !Modifiers
  , inputWindowSize :: !Size
  , inputDeltaTime :: {-# UNPACK #-} !Float
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
    , inputKeys = emptyInputKeys
    , inputChars = ""
    , inputModifiers = Modifiers False False False
    , inputWindowSize = Size 800 600
    , inputDeltaTime = 0
    }

{-# INLINE appendInputKey #-}
appendInputKey :: Key -> Vector Key -> Vector Key
appendInputKey k ks = V.snoc ks k

{-# INLINE inputKeysFromList #-}
inputKeysFromList :: [Key] -> Vector Key
inputKeysFromList = V.fromList

emptyInputKeys :: Vector Key
emptyInputKeys = V.empty

{-# INLINE inputKeysNull #-}
inputKeysNull :: Vector Key -> Bool
inputKeysNull = V.null

{-# INLINE inputKeysElem #-}
inputKeysElem :: Key -> Vector Key -> Bool
inputKeysElem = V.elem

{-# INLINE foldInputKeys #-}
foldInputKeys :: (a -> Key -> a) -> a -> Vector Key -> a
foldInputKeys = V.foldl

-- Buttons, keys, scroll, resize. Mouse motion alone does not count.
{-# INLINE inputInteracted #-}
inputInteracted :: Input -> Input -> Bool
inputInteracted a b =
  inputMouseDown a /= inputMouseDown b
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

{-# INLINE inputPointerHeld #-}
inputPointerHeld :: Input -> Bool
inputPointerHeld inp =
  inputMouseDown inp || inputMouseRightDown inp
