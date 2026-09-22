{-# LANGUAGE StrictData #-}

-- | Implementation of "NanoUI.Input", plus the input rewrites the frame uses
-- for repeated passes and covered layers, and the runner's event batching.
module NanoUI.Internal.Input
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
  , appendDropEvent
  , MouseButton (..)
  , applyMouseButton
  , inputKeysNull
  , inputKeysElem
  , foldInputKeys
  , inputKeysFromList
  , emptyInputKeys
  , stripInteractionInput
  , withoutPointer
  , UiCursorKind (..)
  , grabHoverKind
  , grabDragKind
  , clearEphemeral
  , isHardQuitInput
  , splitFrame
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray, copySmallArray, emptySmallArray, newSmallArray, runSmallArray, sizeofSmallArray, smallArrayFromList)
import NanoUI.Internal.Types (Size (..), V2 (..))

-- | Navigation and editing key presses. Printable text belongs in 'inputChars'.
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
  deriving (Eq, Show, Enum)

-- | Modifier keys held while the frame's input is processed.
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

-- | Input for one frame. Positions and window sizes use logical pixels;
-- scroll values use wheel steps and delta time uses seconds. Held flags
-- persist between frames; press/release flags, text, keys, and drops are
-- events consumed once. Backends clear those events with 'clearEphemeral'.
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
  , inputKeys :: SmallArray Key
  , inputChars :: !Text
  , inputModifiers :: !Modifiers
  , inputWindowSize :: {-# UNPACK #-} !Size
  , inputDeltaTime :: {-# UNPACK #-} !Float
  , inputDrops :: SmallArray DropEvent
  , inputWindowRedraw :: {-# UNPACK #-} !Bool
  }
  deriving (Eq, Show)

-- | No events or held buttons, with an 800x600 window and zero elapsed time.
-- Override window size and delta time when driving headless frames.
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

-- | Backend-independent cursor shape requested by a hovered control.
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
  deriving (Eq, Show, Enum)

-- | Grab cursor over a target, becoming a closed hand while the left button is held.
grabHoverKind :: Bool -> Input -> UiCursorKind
grabHoverKind onTarget inp = grabDragKind onTarget False inp

-- | Choose a grab cursor, keeping the closed hand during a drag outside the target.
grabDragKind :: Bool -> Bool -> Input -> UiCursorKind
grabDragKind onTarget dragging inp
  | dragging = UiCursorGrabbing
  | onTarget, inputMouseDown inp = UiCursorGrabbing
  | onTarget = UiCursorGrab
  | otherwise = UiCursorDefault

-- | Clear one-shot events and the redraw flag, retaining held buttons,
-- pointer position, modifiers, window size, and delta time.
clearEphemeral :: Input -> Input
clearEphemeral inp = (stripInteractionInput inp) {inputMouseClicks = 1, inputWindowRedraw = False}

-- | Whether Ctrl+C or Ctrl+ETX requests an unconditional quit.
isHardQuitInput :: Input -> Bool
isHardQuitInput inp =
  modCtrl (inputModifiers inp)
    && (T.elem 'c' (inputChars inp) || T.elem '\ETX' (inputChars inp))

-- | Split after the first event satisfying the predicate. Including that edge
-- in the first batch keeps separate press/release transitions in separate frames.
splitFrame :: (a -> Bool) -> [a] -> ([a], [a])
splitFrame isEdge events =
  case break isEdge events of
    (before, edge : rest) -> (before ++ [edge], rest)
    (before, []) -> (before, [])

-- | Append a key in event order. Copies the small array.
{-# INLINE appendInputKey #-}
appendInputKey :: Key -> SmallArray Key -> SmallArray Key
appendInputKey k ks = snocSmallArray ks k

-- | The drops with one more at the end.
{-# INLINE appendDropEvent #-}
appendDropEvent :: DropEvent -> SmallArray DropEvent -> SmallArray DropEvent
appendDropEvent ev evs = snocSmallArray evs ev

-- A frame holds a few keys and drops, so each append copies.
snocSmallArray :: SmallArray a -> a -> SmallArray a
snocSmallArray xs x = runSmallArray $ do
  let n = sizeofSmallArray xs
  out <- newSmallArray (n + 1) x
  copySmallArray out 0 xs 0 n
  pure out

-- | Mouse buttons tracked by 'Input'.
data MouseButton = MouseLeft | MouseRight
  deriving (Eq, Show)

-- | Apply a button transition: the held state plus that frame's one-shot
-- pressed or released flag.
applyMouseButton :: MouseButton -> Bool -> Input -> Input
applyMouseButton MouseLeft True inp = inp {inputMouseDown = True, inputMousePressed = True}
applyMouseButton MouseLeft False inp = inp {inputMouseDown = False, inputMouseReleased = True}
applyMouseButton MouseRight True inp = inp {inputMouseRightDown = True, inputMouseRightPressed = True}
applyMouseButton MouseRight False inp = inp {inputMouseRightDown = False, inputMouseRightReleased = True}

-- | Copy a list of key events into the frame's array, preserving order.
{-# INLINE inputKeysFromList #-}
inputKeysFromList :: [Key] -> SmallArray Key
inputKeysFromList = smallArrayFromList

-- | Shared empty key-event array.
emptyInputKeys :: SmallArray Key
emptyInputKeys = emptySmallArray

-- | Shared empty drop-event array.
emptyDropEvents :: SmallArray DropEvent
emptyDropEvents = emptySmallArray

-- | Whether the frame contains no key events.
{-# INLINE inputKeysNull #-}
inputKeysNull :: SmallArray Key -> Bool
inputKeysNull ks = sizeofSmallArray ks == 0

-- | Whether a key occurs in the frame's events.
{-# INLINE inputKeysElem #-}
inputKeysElem :: Key -> SmallArray Key -> Bool
inputKeysElem = elem

-- | Strict left fold over keys in event order.
{-# INLINE foldInputKeys #-}
foldInputKeys :: (a -> Key -> a) -> a -> SmallArray Key -> a
foldInputKeys = foldl'

-- | Compare interaction fields, including buttons, keys, scroll, drops, and
-- window size. Pointer motion, elapsed time, and the redraw flag are ignored.
inputInteracted :: Input -> Input -> Bool
inputInteracted a b = quiet a /= quiet b
  where
    quiet i = i {inputMousePos = V2 0 0, inputDeltaTime = 0, inputWindowRedraw = False}

-- | Whether either tracked mouse button is held.
{-# INLINE inputPointerHeld #-}
inputPointerHeld :: Input -> Bool
inputPointerHeld inp =
  inputMouseDown inp || inputMouseRightDown inp

-- | Remove one-shot interaction events for a repeated view pass. Retains
-- pointer position and held buttons so hover and drag state remain available.
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

-- | The frame as it looks from somewhere the pointer does not reach: under a
-- menu, a dropdown or a panel in front, or outside the layer a held button
-- went down in. No buttons, no wheel, and the pointer itself far off any
-- widget, so reading the pointer there finds nothing to react to.
withoutPointer :: Input -> Input
withoutPointer inp =
  inp
    { inputMousePos = V2 (-1e6) (-1e6)
    , inputMouseDown = False
    , inputMousePressed = False
    , inputMouseReleased = False
    , inputMouseRightDown = False
    , inputMouseRightPressed = False
    , inputMouseRightReleased = False
    , inputScroll = V2 0 0
    }
