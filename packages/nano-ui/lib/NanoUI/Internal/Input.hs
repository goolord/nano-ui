{-# LANGUAGE StrictData #-}

-- | Implementation of "NanoUI.Input", plus the input rewrites the frame uses
-- for repeated passes and covered layers, and the runner's event batching.
module NanoUI.Internal.Input
  ( Key (..)
  , Modifiers (..)
  , noModifiers
  , modifiersFromBits
  , modPrimary
  , primaryModifiers
  , Input (..)
  , DropType (..)
  , DropEvent (..)
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  , appendInputKey
  , applyKey
  , keyRepeats
  , keypadKey
  , appendDropEvent
  , MouseButton (..)
  , applyMouseButton
  , inputKeysNull
  , inputKeysElem
  , foldInputKeys
  , inputKeysFromList
  , stripInteractionInput
  , withoutPointer
  , UiCursorKind (..)
  , CursorShape
  , cursorFallback
  , grabHoverKind
  , grabDragKind
  , clearEphemeral
  , isHardQuitInput
  , splitFrame
  , Composition (..)
  , applyComposition
  ) where

import Data.Bits (Bits, zeroBits, (.&.))
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray, copySmallArray, newSmallArray, runSmallArray, sizeofSmallArray, smallArrayFromList)
import NanoUI.Internal.Types (Size (..), V2 (..))
import System.Info (os)

-- | A key on the keyboard. Named keys have a constructor each; a key that
-- types a character is a 'KeyChar' of the character it types with no
-- modifier held: lower case for a letter, and the unshifted symbol otherwise,
-- so Shift+1 is @KeyChar \'1\'@ with 'modShift' set. The backend reads it
-- from the keyboard layout where it can. A key reports its press and release
-- ('inputKeys', 'inputKeysReleased') whatever modifiers are held, and the
-- text it types, if any, arrives in 'inputChars' as well.
data Key
  = KeyBackspace
  | KeyDelete
  | KeyEnter
  -- ^ Return, and Enter on the keypad.
  | KeyEscape
  | KeyTab
  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyHome
  | KeyEnd
  | KeyPageUp
  | KeyPageDown
  | KeyInsert
  | KeySpace
  | KeyF !Int
  -- ^ A function key, @KeyF 1@ to @KeyF 24@.
  | KeyPrintScreen
  | KeyPause
  | KeyCapsLock
  | KeyNumLock
  | KeyScrollLock
  | KeyMenu
  -- ^ The context-menu (application) key.
  | KeyChar !Char
  -- ^ A key that types a character; see 'Key'.
  deriving (Eq, Ord, Show)

-- | Modifier keys held while the frame's input is processed.
data Modifiers = Modifiers
  { modShift :: !Bool
  , modCtrl :: !Bool
  , modAlt :: !Bool
  -- ^ Alt, which macOS calls Option.
  , modSuper :: !Bool
  -- ^ Command on macOS, the Windows key on Windows, Super elsewhere.
  }
  deriving (Eq, Ord, Show)

-- | Both sets of modifiers held.
instance Semigroup Modifiers where
  Modifiers s c a u <> Modifiers s' c' a' u' = Modifiers (s || s') (c || c') (a || a') (u || u')

instance Monoid Modifiers where
  mempty = noModifiers

-- | No modifier held.
noModifiers :: Modifiers
noModifiers = Modifiers False False False False

-- | The modifiers held in a backend's bit mask, given its bits for Shift,
-- Ctrl, Alt and Super.
{-# INLINE modifiersFromBits #-}
modifiersFromBits :: Bits a => a -> a -> a -> a -> a -> Modifiers
modifiersFromBits m shift ctrl alt super = Modifiers (has shift) (has ctrl) (has alt) (has super)
  where
    has bit = m .&. bit /= zeroBits

-- | Whether the platform's command modifier is held: Command ('modSuper') on
-- macOS and Ctrl elsewhere. A chord's @M-@ ('NanoUI.parseShortcut') is it.
modPrimary :: Modifiers -> Bool
modPrimary = if os == "darwin" then modSuper else modCtrl

-- | The platform's command modifier alone ('modPrimary').
primaryModifiers :: Modifiers
primaryModifiers
  | os == "darwin" = noModifiers {modSuper = True}
  | otherwise = noModifiers {modCtrl = True}

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
-- scroll values use wheel steps and delta time uses seconds. Held flags and
-- held keys and the input method's composition persist between frames;
-- press/release flags, text, key presses and releases, and drops are events
-- consumed once. Backends clear those
-- events with 'clearEphemeral'.
data Input = Input
  { inputMousePos :: {-# UNPACK #-} !V2
  , inputMouseDown :: {-# UNPACK #-} !Bool
  , inputMousePressed :: {-# UNPACK #-} !Bool
  , inputMouseReleased :: {-# UNPACK #-} !Bool
  , inputMouseRightDown :: {-# UNPACK #-} !Bool
  , inputMouseRightPressed :: {-# UNPACK #-} !Bool
  , inputMouseRightReleased :: {-# UNPACK #-} !Bool
  , inputMouseMiddleDown :: {-# UNPACK #-} !Bool
  , inputMouseMiddlePressed :: {-# UNPACK #-} !Bool
  , inputMouseMiddleReleased :: {-# UNPACK #-} !Bool
  , inputMouseBackPressed :: {-# UNPACK #-} !Bool
  -- ^ The back side button (X1) went down this frame. Its release is not reported.
  , inputMouseForwardPressed :: {-# UNPACK #-} !Bool
  -- ^ The forward side button (X2) went down this frame.
  , inputMouseClicks :: {-# UNPACK #-} !Int
  , inputScroll :: {-# UNPACK #-} !V2
  , inputKeys :: SmallArray Key
  -- ^ Keys pressed this frame in event order, with a held key's auto-repeats.
  , inputKeysReleased :: SmallArray Key
  -- ^ Keys released this frame, in event order.
  , inputKeysHeld :: SmallArray Key
  -- ^ Keys down as the frame's events leave them, each once, in the order
  -- they went down.
  , inputChars :: !Text
  , inputModifiers :: !Modifiers
  , inputWindowSize :: {-# UNPACK #-} !Size
  , inputDeltaTime :: {-# UNPACK #-} !Float
  , inputDrops :: SmallArray DropEvent
  , inputWindowRedraw :: {-# UNPACK #-} !Bool
  , inputComposition :: !(Maybe Composition)
  -- ^ What an input method is composing, held until it changes or ends it
  -- ('applyComposition'). The text it commits arrives in 'inputChars'.
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
    , inputMouseMiddleDown = False
    , inputMouseMiddlePressed = False
    , inputMouseMiddleReleased = False
    , inputMouseBackPressed = False
    , inputMouseForwardPressed = False
    , inputMouseClicks = 1
    , inputScroll = V2 0 0
    , inputKeys = mempty
    , inputKeysReleased = mempty
    , inputKeysHeld = mempty
    , inputChars = ""
    , inputModifiers = noModifiers
    , inputWindowSize = Size 800 600
    , inputDeltaTime = 0
    , inputDrops = mempty
    , inputWindowRedraw = False
    , inputComposition = Nothing
    }

-- | Backend-independent cursor shape requested by a hovered control. The
-- shapes are CSS's cursors, and a backend shows each with the platform's
-- cursor of that name. Where the platform has none, it shows the closest
-- one it has: the SDL and RGFW backends show the 'cursorFallback'.
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
  | -- | The action is not allowed here: a slashed circle.
    UiCursorNotAllowed
  | -- | Busy, and not taking input: an hourglass or a spinner.
    UiCursorWait
  | -- | Busy, but still taking input: the arrow with a spinner.
    UiCursorProgress
  | -- | Help is available: the arrow with a question mark.
    UiCursorHelp
  | -- | Precise selection, as over a canvas.
    UiCursorCrosshair
  | -- | A cell or a set of cells can be selected: a thick plus.
    UiCursorCell
  | -- | What is under the pointer can be moved: arrows in four directions.
    UiCursorMove
  | -- | The content can be scrolled in any direction.
    UiCursorAllScroll
  | -- | A drop here copies.
    UiCursorCopy
  | -- | A drop here makes a link or shortcut.
    UiCursorAlias
  | -- | A context menu is available.
    UiCursorContextMenu
  | -- | A click zooms in: a magnifier with a plus.
    UiCursorZoomIn
  | -- | A click zooms out: a magnifier with a minus.
    UiCursorZoomOut
  | -- | A column can be resized sideways.
    UiCursorColResize
  | -- | A row can be resized up or down.
    UiCursorRowResize
  | -- | This and the seven after it are the one-way resize arrows, for an
    -- edge or corner that can move only one way, such as a pane's edge at
    -- its limit. Platforms without one-way arrows show the two-way ones.
    UiCursorNResize
  | UiCursorNeResize
  | UiCursorEResize
  | UiCursorSeResize
  | UiCursorSResize
  | UiCursorSwResize
  | UiCursorWResize
  | UiCursorNwResize
  deriving (Eq, Show, Enum, Bounded)

-- | The pointer shape a view asks for while the pointer is over part of it
-- ('NanoUI.withCursorShape', 'NanoUI.Widgets.Custom.widgetCursor'):
-- 'UiCursorKind' under the name the view API uses.
type CursorShape = UiCursorKind


-- | The shape a backend shows for a kind when it has only the cursors SDL
-- and RGFW have: the grab hands and all-scroll are the move arrows, a cell
-- the crosshair, column and row resizing the two-way arrows, and help, copy,
-- alias, context menu and the zooms the arrow. Every other kind is itself.
cursorFallback :: UiCursorKind -> UiCursorKind
cursorFallback = \case
  UiCursorGrab -> UiCursorMove
  UiCursorGrabbing -> UiCursorMove
  UiCursorAllScroll -> UiCursorMove
  UiCursorCell -> UiCursorCrosshair
  UiCursorColResize -> UiCursorEwResize
  UiCursorRowResize -> UiCursorNsResize
  k | k `elem` [UiCursorHelp, UiCursorCopy, UiCursorAlias, UiCursorContextMenu, UiCursorZoomIn, UiCursorZoomOut] -> UiCursorDefault
  k -> k

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

-- | Clear one-shot events and the redraw flag, retaining held buttons and
-- keys, pointer position, modifiers, window size, delta time, and the
-- composition.
clearEphemeral :: Input -> Input
clearEphemeral inp = (stripInteractionInput inp) {inputMouseClicks = 1, inputWindowRedraw = False}

-- | Whether Ctrl+C requests an unconditional quit: the C key pressed with
-- Ctrl, or a @c@ or ETX typed with it.
isHardQuitInput :: Input -> Bool
isHardQuitInput inp =
  modCtrl (inputModifiers inp)
    && ( inputKeysElem (KeyChar 'c') (inputKeys inp)
          || T.elem 'c' (inputChars inp)
          || T.elem '\ETX' (inputChars inp)
       )

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

-- | Apply a key going down ('True') or up: a press joins 'inputKeys' and,
-- unless the key is already down, 'inputKeysHeld'; a release joins
-- 'inputKeysReleased' and leaves the held keys.
applyKey :: Key -> Bool -> Input -> Input
applyKey k True inp =
  inp
    { inputKeys = appendInputKey k (inputKeys inp)
    , inputKeysHeld = if inputKeysElem k held then held else appendInputKey k held
    }
  where
    held = inputKeysHeld inp
applyKey k False inp =
  inp
    { inputKeysReleased = appendInputKey k (inputKeysReleased inp)
    , inputKeysHeld =
        if inputKeysElem k held then smallArrayFromList (filter (/= k) (toList held)) else held
    }
  where
    held = inputKeysHeld inp

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

-- | Whether a held key's auto-repeats count as presses: they do for the
-- keys that type, move the caret or delete, and not for Enter, Escape, Tab,
-- Insert, the function keys, the lock keys and the like. A backend drops the
-- repeats of a key that does not repeat.
keyRepeats :: Key -> Bool
keyRepeats = \case
  KeyChar _ -> True
  k -> k `elem` [KeyBackspace, KeyDelete, KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd, KeyPageUp, KeyPageDown, KeySpace]

-- | The key a keypad digit or point (@'0'@ to @'9'@, @'.'@) is: with Num
-- Lock on, the character it types; off, the navigation key printed on it,
-- which 5 has none of.
keypadKey :: Bool -> Char -> Maybe Key
keypadKey True c = Just (KeyChar c)
keypadKey False c =
  lookup c [('0', KeyInsert), ('1', KeyEnd), ('2', KeyDown), ('3', KeyPageDown), ('4', KeyLeft), ('6', KeyRight), ('7', KeyHome), ('8', KeyUp), ('9', KeyPageUp), ('.', KeyDelete)]

-- | Mouse buttons tracked by 'Input'. 'MouseBack' and 'MouseForward' are the
-- side buttons (X1 and X2) a browser navigates with.
data MouseButton = MouseLeft | MouseRight | MouseMiddle | MouseBack | MouseForward
  deriving (Eq, Show)

-- | Apply a button transition: the held state plus that frame's one-shot
-- pressed or released flag. The side buttons report only their press.
applyMouseButton :: MouseButton -> Bool -> Input -> Input
applyMouseButton MouseLeft True inp = inp {inputMouseDown = True, inputMousePressed = True}
applyMouseButton MouseLeft False inp = inp {inputMouseDown = False, inputMouseReleased = True}
applyMouseButton MouseRight True inp = inp {inputMouseRightDown = True, inputMouseRightPressed = True}
applyMouseButton MouseRight False inp = inp {inputMouseRightDown = False, inputMouseRightReleased = True}
applyMouseButton MouseMiddle True inp = inp {inputMouseMiddleDown = True, inputMouseMiddlePressed = True}
applyMouseButton MouseMiddle False inp = inp {inputMouseMiddleDown = False, inputMouseMiddleReleased = True}
applyMouseButton MouseBack True inp = inp {inputMouseBackPressed = True}
applyMouseButton MouseForward True inp = inp {inputMouseForwardPressed = True}
applyMouseButton _ False inp = inp

-- | Copy a list of key events into the frame's array, preserving order.
{-# INLINE inputKeysFromList #-}
inputKeysFromList :: [Key] -> SmallArray Key
inputKeysFromList = smallArrayFromList

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

-- | Whether the left, right or middle mouse button is held.
{-# INLINE inputPointerHeld #-}
inputPointerHeld :: Input -> Bool
inputPointerHeld inp =
  inputMouseDown inp || inputMouseRightDown inp || inputMouseMiddleDown inp

-- | Remove one-shot interaction events for a repeated view pass. Retains
-- pointer position, held buttons and held keys so hover and drag state remain
-- available, and the composition, which the focused field still shows.
stripInteractionInput :: Input -> Input
stripInteractionInput inp =
  inp
    { inputMousePressed = False
    , inputMouseReleased = False
    , inputMouseRightPressed = False
    , inputMouseRightReleased = False
    , inputMouseMiddlePressed = False
    , inputMouseMiddleReleased = False
    , inputMouseBackPressed = False
    , inputMouseForwardPressed = False
    , inputKeys = mempty
    , inputKeysReleased = mempty
    , inputChars = ""
    , inputScroll = V2 0 0
    , inputDrops = mempty
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
    , inputMouseMiddleDown = False
    , inputMouseMiddlePressed = False
    , inputMouseMiddleReleased = False
    , inputMouseBackPressed = False
    , inputMouseForwardPressed = False
    , inputScroll = V2 0 0
    }

-- | Text an input method (IME) is composing and has not committed, such as
-- the reading of Chinese or Japanese typed before it is converted. The
-- focused text field draws it at its caret, underlined, but its value does
-- not change until the input method commits the text, which arrives as typed
-- text.
data Composition = Composition
  { compositionText :: !Text
  -- ^ The text being composed. Empty text counts as no composition.
  , compositionCursor :: !Int
  -- ^ Where the input method's caret, or the start of its selection, is in
  -- 'compositionText', in characters.
  , compositionSelection :: !Int
  -- ^ How many characters from 'compositionCursor' the input method has
  -- selected, such as the clause it is converting; 0 for a plain caret.
  }
  deriving (Eq, Show)

-- | Apply an input method's composition update, as SDL's
-- @SDL_EVENT_TEXT_EDITING@ reports one: the text being composed, where its
-- caret or selection starts and how long the selection is, both in
-- characters. Empty text ends the composition. A position that is negative
-- (unset) or past the text is clamped into it: an unset caret sits at the
-- end.
applyComposition :: Text -> Int -> Int -> Input -> Input
applyComposition txt start len inp
  | T.null txt = inp {inputComposition = Nothing}
  | otherwise =
      let n = T.length txt
          cursor = if start < 0 then n else min n start
       in inp {inputComposition = Just (Composition txt cursor (clamp0 (n - cursor) len))}
  where
    clamp0 hi v = max 0 (min hi v)
