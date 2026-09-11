{-# LANGUAGE StrictData #-}

module NanoUI.Input
  ( Key (..)
  , Modifiers (..)
  , Input (..)
  , DropType (..)
  , DropEvent (..)
  , emptyDropEvents
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
  , UiCursorKind (..)
  , grabHoverKind
  , grabDragKind
  , clearEphemeral
  , isHardQuitInput
  , splitFrame
  ) where

import Data.Text (Text)
import qualified Data.Text as T
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

-- | OS-level drag-and-drop event kind, mirroring @SDL_EventType@ drop codes.
data DropType
  = DropBegin     -- ^ A drag enters the window; no position or payload yet.
  | DropPosition  -- ^ The drag pointer moved over the window; position available.
  | DropFile      -- ^ A file path was dropped; 'dropEventData' holds the path.
  | DropText      -- ^ Text was dropped; 'dropEventData' holds the text.
  | DropComplete  -- ^ The OS drag operation finished.
  deriving (Eq, Show)

-- | A single normalized drop payload surfaced to widgets.
data DropEvent = DropEvent
  { dropEventType :: !DropType
  , dropEventPos :: !(Maybe V2)
  , dropEventData :: !Text
  }
  deriving (Eq, Show)

data Input = Input
  { inputMousePos :: {-# UNPACK #-} !V2
  , inputMouseDown :: {-# UNPACK #-} !Bool
  , inputMousePressed :: {-# UNPACK #-} !Bool
  , inputMouseReleased :: {-# UNPACK #-} !Bool
  , inputMouseRightDown :: {-# UNPACK #-} !Bool
  , inputMouseRightPressed :: {-# UNPACK #-} !Bool
  , inputMouseRightReleased :: {-# UNPACK #-} !Bool
  , inputMouseClicks :: {-# UNPACK #-} !Int
  , inputScroll :: {-# UNPACK #-} !V2
  , inputKeys :: Vector Key
  , inputChars :: !Text
  , inputModifiers :: !Modifiers
  , inputWindowSize :: {-# UNPACK #-} !Size
  , inputDeltaTime :: {-# UNPACK #-} !Float
  , inputDrops :: Vector DropEvent
  , inputWindowRedraw :: {-# UNPACK #-} !Bool
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
    , inputDrops = emptyDropEvents
    , inputWindowRedraw = False
    }

data UiCursorKind
  = UiCursorDefault
  | UiCursorPointer
  | UiCursorText
  | UiCursorGrab
  | UiCursorGrabbing
  | UiCursorNsResize
  | UiCursorEwResize
  | UiCursorNwseResize
  | UiCursorNeswResize
  deriving (Eq, Show)

grabHoverKind :: Bool -> Input -> UiCursorKind
grabHoverKind onTarget inp = grabDragKind onTarget False inp

grabDragKind :: Bool -> Bool -> Input -> UiCursorKind
grabDragKind onTarget dragging inp
  | dragging = UiCursorGrabbing
  | onTarget, inputMouseDown inp = UiCursorGrabbing
  | onTarget = UiCursorGrab
  | otherwise = UiCursorDefault

clearEphemeral :: Input -> Input
clearEphemeral inp =
  inp
    { inputKeys = emptyInputKeys
    , inputChars = ""
    , inputMousePressed = False
    , inputMouseReleased = False
    , inputMouseRightPressed = False
    , inputMouseRightReleased = False
    , inputMouseClicks = 1
    , inputScroll = V2 0 0
    , inputDrops = emptyDropEvents
    , inputWindowRedraw = False
    }

isHardQuitInput :: Input -> Bool
isHardQuitInput inp =
  modCtrl (inputModifiers inp)
    && (T.elem 'c' (inputChars inp) || T.elem '\ETX' (inputChars inp))

splitFrame :: (a -> Bool) -> [a] -> ([a], [a])
splitFrame isEdge events =
  case break isEdge events of
    (before, edge : rest) -> (before ++ [edge], rest)
    (before, []) -> (before, [])

{-# INLINE appendInputKey #-}
appendInputKey :: Key -> Vector Key -> Vector Key
appendInputKey k ks = V.snoc ks k

{-# INLINE inputKeysFromList #-}
inputKeysFromList :: [Key] -> Vector Key
inputKeysFromList = V.fromList

emptyInputKeys :: Vector Key
emptyInputKeys = V.empty

emptyDropEvents :: Vector DropEvent
emptyDropEvents = V.empty

{-# INLINE inputKeysNull #-}
inputKeysNull :: Vector Key -> Bool
inputKeysNull = V.null

{-# INLINE inputKeysElem #-}
inputKeysElem :: Key -> Vector Key -> Bool
inputKeysElem = V.elem

{-# INLINE foldInputKeys #-}
foldInputKeys :: (a -> Key -> a) -> a -> Vector Key -> a
foldInputKeys = V.foldl'

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
    || inputDrops a /= inputDrops b

{-# INLINE inputPointerHeld #-}
inputPointerHeld :: Input -> Bool
inputPointerHeld inp =
  inputMouseDown inp || inputMouseRightDown inp

-- Rebuild UI after store mirrors update. Keep hover/drag; drop one-shot input.
{-# INLINE stripInteractionInput #-}
stripInteractionInput :: Input -> Input
stripInteractionInput inp =
  inp
    { inputMousePressed = False
    , inputMouseReleased = False
    , inputMouseRightPressed = False
    , inputMouseRightReleased = False
    , inputKeys = emptyInputKeys
    , inputChars = ""
    , inputScroll = V2 0 0
    , inputDrops = emptyDropEvents
    }
