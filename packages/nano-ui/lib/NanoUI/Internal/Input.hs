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
  , modJump
  , modMacCommand
  , onMac
  , isCommandKey
  , shiftAtMost
  , Input (..)
  , Pressable (..)
  , anyButtonPressed
  , anyButtonReleased
  , inputMouseDown
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightDown
  , inputMouseRightPressed
  , inputMouseRightReleased
  , DropType (..)
  , DropEvent (..)
  , emptyInput
  , inputInteracted
  , inputPointerHeld
  , appendInputKey
  , applyKey
  , releaseAllKeys
  , keypadKey
  , appendDropEvent
  , MouseButton (..)
  , mouseButtonNumber
  , MouseButtons
  , noButtons
  , buttonsMember
  , buttonsToList
  , buttonsFromList
  , buttonsNull
  , buttonsInsert
  , buttonsDelete
  , buttonsFilterM
  , applyMouseButton
  , applyPointerLeave
  , inputKeysNull
  , inputKeysElem
  , foldInputKeys
  , inputKeysFromList
  , stripInteractionInput
  , withoutPointer
  , UiCursorKind (..)
  , cursorFallback
  , grabHoverKind
  , grabDragKind
  , clearEphemeral
  , isHardQuitInput
  , takeFrame
  , Composition (..)
  , applyComposition
  , InputPurpose (..)
  ) where

import Data.Bits (Bits, clearBit, countTrailingZeros, setBit, testBit, zeroBits, (.&.), (.|.))
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Primitive.SmallArray (SmallArray, copySmallArray, indexSmallArray, newSmallArray, runSmallArray, sizeofSmallArray, smallArrayFromList)
import Data.Word (Word32)
import NanoUI.Internal.Types (Size (..), V2 (..))
import System.Info (os)

-- | A key on the keyboard. A key that types a character is 'KeyChar' of the
-- character it types with no modifier held: lower case for a letter, the
-- unshifted symbol otherwise (Shift+1 is @KeyChar \'1\'@ with 'modShift').
-- Backends read it from the keyboard layout where they can. Presses and
-- releases are reported whatever modifiers are held; any text the key types
-- also arrives in 'inputChars'.
data Key
  = KeyBackspace
  | KeyDelete
  | KeyEnter
  -- ^ Return, or Enter on the keypad.
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
  -- ^ @KeyF 1@ to @KeyF 24@.
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

-- | Union: a modifier is held if either side holds it.
instance Semigroup Modifiers where
  Modifiers s c a u <> Modifiers s' c' a' u' = Modifiers (s || s') (c || c') (a || a') (u || u')

instance Monoid Modifiers where
  mempty = noModifiers

-- | No modifier held.
noModifiers :: Modifiers
noModifiers = Modifiers False False False False

-- | Read a backend's modifier bit mask, given its bits for Shift, Ctrl, Alt
-- and Super.
{-# INLINE modifiersFromBits #-}
modifiersFromBits :: Bits a => a -> a -> a -> a -> a -> Modifiers
modifiersFromBits m shift ctrl alt super = Modifiers (has shift) (has ctrl) (has alt) (has super)
  where
    has bit = m .&. bit /= zeroBits

-- | Whether the platform's command modifier is held: Command ('modSuper') on
-- macOS, Ctrl elsewhere. It is @M-@ in 'NanoUI.Shortcut.parseShortcut'.
modPrimary :: Modifiers -> Bool
modPrimary = if onMac then modSuper else modCtrl

-- | Only the platform's command modifier ('modPrimary').
primaryModifiers :: Modifiers
primaryModifiers
  | onMac = noModifiers {modSuper = True}
  | otherwise = noModifiers {modCtrl = True}

-- | Whether the word-jump modifier is held: Option ('modAlt') on macOS, Ctrl
-- elsewhere. It widens caret motion and deletion to a word (iced's @jump@).
modJump :: Modifiers -> Bool
modJump = if onMac then modAlt else modCtrl

-- | Whether Command ('modSuper') is held on macOS, where it moves the caret
-- or deletes to the end of the line. Always 'False' elsewhere.
modMacCommand :: Modifiers -> Bool
modMacCommand m = onMac && modSuper m

-- | Whether this is macOS, where Command is the command modifier and Option
-- types characters.
onMac :: Bool
onMac = os == "darwin"
{-# NOINLINE onMac #-}

-- | Whether no modifier other than Shift is held. Controls use this to tell
-- a plain key, such as an arrow on a slider, from a chord.
shiftAtMost :: Modifiers -> Bool
shiftAtMost m = not (modCtrl m || modAlt m || modSuper m)

-- | Whether a key pressed with these modifiers is a command rather than
-- typing. Named keys other than Space always are. A 'KeyChar' or Space is a
-- command with Super (Command) or Ctrl held, except that outside macOS
-- Ctrl+Alt is AltGr and types, while Alt alone is a command. On macOS,
-- Option types.
isCommandKey :: Modifiers -> Key -> Bool
isCommandKey mods = \case
  KeyChar _ -> chord
  KeySpace -> chord
  _ -> True
  where
    chord = modSuper mods || if onMac then modCtrl mods else modCtrl mods /= modAlt mods

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

-- | Input for one frame. Positions and window sizes are in logical pixels,
-- scroll in wheel steps, delta time in seconds. Held buttons, held keys and
-- the IME composition persist between frames. Presses, releases, text and
-- drops are one-shot events that backends clear with 'clearEphemeral'.
data Input = Input
  { inputMousePos :: {-# UNPACK #-} !V2
  , inputButtonsHeld :: {-# UNPACK #-} !MouseButtons
  -- ^ Buttons down after the frame's events ('heldIn').
  , inputButtonsPressed :: {-# UNPACK #-} !MouseButtons
  -- ^ Buttons that went down this frame ('pressedIn').
  , inputButtonsReleased :: {-# UNPACK #-} !MouseButtons
  -- ^ Buttons that came up this frame ('releasedIn').
  , inputMouseClicks :: {-# UNPACK #-} !Int
  -- ^ 1 for a single press; 2 or 3 for a double or triple click (a quick
  -- press near the previous one, with the same button).
  , inputScroll :: {-# UNPACK #-} !V2
  , inputKeys :: SmallArray Key
  -- ^ Keys pressed this frame in event order, auto-repeats included. A frame
  -- batched by 'takeFrame' has at most one command key ('isCommandKey'),
  -- last and possibly repeated. The frame's text came before it, and
  -- 'inputModifiers' are the ones it was pressed with.
  , inputKeysNew :: SmallArray Key
  -- ^ The presses in 'inputKeys' that are not auto-repeats, in event order.
  , inputKeysReleased :: SmallArray Key
  -- ^ Keys released this frame, in event order.
  , inputKeysHeld :: SmallArray Key
  -- ^ Keys down after the frame's events, each once, in press order.
  , inputChars :: !Text
  , inputModifiers :: !Modifiers
  , inputWindowSize :: {-# UNPACK #-} !Size
  , inputDeltaTime :: {-# UNPACK #-} !Float
  , inputDrops :: SmallArray DropEvent
  , inputWindowRedraw :: {-# UNPACK #-} !Bool
  , inputComposition :: !(Maybe Composition)
  -- ^ The IME's uncommitted text, kept until the IME changes or ends it
  -- ('applyComposition'). Committed text arrives in 'inputChars'.
  }
  deriving (Eq, Show)

-- | A key or mouse button: whether it went down this frame, came up, or is
-- held. Reads like 'NanoUI.Shortcut.shortcutIn' does for a chord:
--
-- > pressedIn KeyEscape inp
-- > heldIn MouseMiddle inp
--
-- These read raw input, regardless of focus or hover. In a view, use
-- 'NanoUI.keyPressed' and 'NanoUI.mousePressed', which only fire when the
-- view has the keyboard or the pointer.
class Pressable a where
  -- | Went down this frame, auto-repeats included.
  pressedIn :: a -> Input -> Bool

  -- | Went down this frame while up, so auto-repeats are excluded
  -- ('inputKeysNew'). Mouse buttons don't repeat, so for them this is
  -- 'pressedIn'.
  pressedOnceIn :: a -> Input -> Bool
  pressedOnceIn = pressedIn

  -- | Came up this frame.
  releasedIn :: a -> Input -> Bool

  -- | Down after the frame's events.
  heldIn :: a -> Input -> Bool

instance Pressable Key where
  {-# INLINE pressedIn #-}
  pressedIn k = inputKeysElem k . inputKeys
  {-# INLINE pressedOnceIn #-}
  pressedOnceIn k = inputKeysElem k . inputKeysNew
  {-# INLINE releasedIn #-}
  releasedIn k = inputKeysElem k . inputKeysReleased
  {-# INLINE heldIn #-}
  heldIn k = inputKeysElem k . inputKeysHeld

instance Pressable MouseButton where
  {-# INLINE pressedIn #-}
  pressedIn b = buttonsMember b . inputButtonsPressed
  {-# INLINE releasedIn #-}
  releasedIn b = buttonsMember b . inputButtonsReleased
  {-# INLINE heldIn #-}
  heldIn b = buttonsMember b . inputButtonsHeld

-- | Whether any mouse button went down this frame.
{-# INLINE anyButtonPressed #-}
anyButtonPressed :: Input -> Bool
anyButtonPressed = not . buttonsNull . inputButtonsPressed

-- | Whether any mouse button came up this frame.
{-# INLINE anyButtonReleased #-}
anyButtonReleased :: Input -> Bool
anyButtonReleased = not . buttonsNull . inputButtonsReleased

-- | Left and right button state under their nano-ui 0.1 field names.
inputMouseDown, inputMousePressed, inputMouseReleased :: Input -> Bool
inputMouseDown = heldIn MouseLeft
inputMousePressed = pressedIn MouseLeft
inputMouseReleased = releasedIn MouseLeft
{-# DEPRECATED inputMouseDown "Use heldIn MouseLeft" #-}
{-# DEPRECATED inputMousePressed "Use pressedIn MouseLeft" #-}
{-# DEPRECATED inputMouseReleased "Use releasedIn MouseLeft" #-}

inputMouseRightDown, inputMouseRightPressed, inputMouseRightReleased :: Input -> Bool
inputMouseRightDown = heldIn MouseRight
inputMouseRightPressed = pressedIn MouseRight
inputMouseRightReleased = releasedIn MouseRight
{-# DEPRECATED inputMouseRightDown "Use heldIn MouseRight" #-}
{-# DEPRECATED inputMouseRightPressed "Use pressedIn MouseRight" #-}
{-# DEPRECATED inputMouseRightReleased "Use releasedIn MouseRight" #-}

-- | No events or held buttons, with an 800x600 window and zero elapsed time.
-- Override window size and delta time when driving headless frames.
emptyInput :: Input
emptyInput =
  Input
    { inputMousePos = V2 0 0
    , inputButtonsHeld = noButtons
    , inputButtonsPressed = noButtons
    , inputButtonsReleased = noButtons
    , inputMouseClicks = 1
    , inputScroll = V2 0 0
    , inputKeys = mempty
    , inputKeysNew = mempty
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
-- shapes are CSS's cursors. Where the platform lacks one, the SDL and RGFW
-- backends show its 'cursorFallback'.
data UiCursorKind
  = -- | From a widget ('NanoUI.Widgets.Custom.widgetCursor'), no opinion:
    -- the enclosing 'NanoUI.withCursorShape' scope decides, or the arrow if
    -- there is none. As a scope's own shape, the arrow.
    UiCursorDefault
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
  | -- | This and the next seven are one-way resize arrows, for an edge or
    -- corner that can only move one way, such as a pane at its limit.
    -- Platforms without them show the two-way arrows.
    UiCursorNResize
  | UiCursorNeResize
  | UiCursorEResize
  | UiCursorSeResize
  | UiCursorSResize
  | UiCursorSwResize
  | UiCursorWResize
  | UiCursorNwResize
  | -- | No pointer shown, as over a video or a canvas that draws its own.
    UiCursorHidden
  deriving (Eq, Show, Enum, Bounded)

-- | The closest shape among the cursors SDL and RGFW have. Kinds they have
-- map to themselves.
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
  | onTarget, heldIn MouseLeft inp = UiCursorGrabbing
  | onTarget = UiCursorGrab
  | otherwise = UiCursorDefault

-- | Clear one-shot events and the redraw flag. Keeps held buttons and keys,
-- pointer position, modifiers, window size, delta time and the composition.
clearEphemeral :: Input -> Input
clearEphemeral inp = (stripInteractionInput inp) {inputMouseClicks = 1, inputWindowRedraw = False}

-- | Whether Ctrl+C was pressed (the C key, or a typed @c@ or ETX), which
-- requests an unconditional quit.
isHardQuitInput :: Input -> Bool
isHardQuitInput inp =
  modCtrl (inputModifiers inp)
    && ( pressedIn (KeyChar 'c') inp
          || T.elem 'c' (inputChars inp)
          || T.elem '\ETX' (inputChars inp)
       )

-- | Fold events into one frame's input, stopping where the frame must end.
-- Returns the input, the events taken, and the rest.
--
-- A frame ends after an edge (@isEdge@, a mouse press or release), so each
-- edge gets its own frame. It also ends after a command key ('isCommandKey')
-- when text, another key or a modifier change follows. A frame loses the
-- order of its text, keys and modifiers, so its text must come before its
-- one command key, and its modifiers must be those the key was pressed
-- with. Auto-repeats stay in the key's frame, so only mixed bursts cost
-- extra frames.
takeFrame :: (Input -> e -> Input) -> (e -> Bool) -> Input -> [e] -> (Input, [e], [e])
takeFrame apply isEdge = go []
  where
    go taken !inp = \case
      [] -> (inp, reverse taken, [])
      events@(e : rest)
        | commandEnds inp next -> (inp, reverse taken, events)
        | isEdge e -> (next, reverse (e : taken), rest)
        | otherwise -> go (e : taken) next rest
        where
          next = apply inp e

-- | Whether the event that turned @cur@ into @next@ must wait for the next
-- frame: @cur@ ends in a command key, and the event typed text, pressed a
-- different key or changed the modifiers.
commandEnds :: Input -> Input -> Bool
commandEnds cur next
  | n == 0 || not (isCommandKey mods k) = False
  | otherwise =
      inputModifiers next /= mods
        || inputChars next /= inputChars cur
        || (sizeofSmallArray (inputKeys next) > n && indexSmallArray (inputKeys next) n /= k)
  where
    n = sizeofSmallArray (inputKeys cur)
    k = indexSmallArray (inputKeys cur) (n - 1)
    mods = inputModifiers cur

-- | Append a key in event order. Copies the small array.
{-# INLINE appendInputKey #-}
appendInputKey :: Key -> SmallArray Key -> SmallArray Key
appendInputKey k ks = snocSmallArray ks k

-- | Apply a key press ('True') or release. A press joins 'inputKeys', and
-- also 'inputKeysNew' and 'inputKeysHeld' unless the key is already held
-- (an auto-repeat). A release joins 'inputKeysReleased' and leaves the held
-- keys. Backends pass auto-repeats in as presses.
applyKey :: Key -> Bool -> Input -> Input
applyKey k True inp
  | inputKeysElem k held = inp {inputKeys = appendInputKey k (inputKeys inp)}
  | otherwise =
      inp
        { inputKeys = appendInputKey k (inputKeys inp)
        , inputKeysNew = appendInputKey k (inputKeysNew inp)
        , inputKeysHeld = appendInputKey k held
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

-- | Release every held key and modifier. Backends call this when the window
-- loses keyboard focus, since keys released elsewhere send no release event.
-- Held keys join 'inputKeysReleased'.
releaseAllKeys :: Input -> Input
releaseAllKeys inp =
  inp
    { inputKeysReleased = inputKeysReleased inp <> inputKeysHeld inp
    , inputKeysHeld = mempty
    , inputModifiers = noModifiers
    }

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

-- | The key for a keypad digit or point (@'0'@ to @'9'@, @'.'@): the typed
-- character with Num Lock on, otherwise the navigation key printed on it
-- ('Nothing' for 5).
keypadKey :: Bool -> Char -> Maybe Key
keypadKey True c = Just (KeyChar c)
keypadKey False c =
  lookup c [('0', KeyInsert), ('1', KeyEnd), ('2', KeyDown), ('3', KeyPageDown), ('4', KeyLeft), ('6', KeyRight), ('7', KeyHome), ('8', KeyUp), ('9', KeyPageUp), ('.', KeyDelete)]

-- | A mouse button. 'MouseBack' and 'MouseForward' are the side buttons (X1
-- and X2) that browsers use for navigation.
data MouseButton
  = MouseLeft
  | MouseRight
  | MouseMiddle
  | MouseBack
  | MouseForward
  | MouseOther !Int
  -- ^ Any other button, by SDL's 1-based number ('mouseButtonNumber'), so
  -- the first extra button is @MouseOther 6@. 'MouseButtons' stores
  -- @MouseOther 1@ to @5@ as the named buttons and tracks buttons up to 32.
  deriving (Eq, Ord, Show)

-- | The button for an SDL button number: 1 to 5 are left, middle, right,
-- back and forward ('MouseOther').
mouseButtonNumber :: Int -> MouseButton
mouseButtonNumber = \case
  1 -> MouseLeft
  2 -> MouseMiddle
  3 -> MouseRight
  4 -> MouseBack
  5 -> MouseForward
  n -> MouseOther n

-- | The button's bit in 'MouseButtons': its number minus one, or -1 past 32.
{-# INLINE buttonBit #-}
buttonBit :: MouseButton -> Int
buttonBit = \case
  MouseLeft -> 0
  MouseMiddle -> 1
  MouseRight -> 2
  MouseBack -> 3
  MouseForward -> 4
  MouseOther n
    | n >= 1 && n <= 32 -> n - 1
    | otherwise -> -1

-- | A set of mouse buttons, as held, pressed or released in an 'Input' or
-- clicked with ('NanoUI.respClickedWith'). '<>' is union.
newtype MouseButtons = MouseButtons Word32
  deriving (Eq)

instance Show MouseButtons where
  showsPrec d bs = showParen (d > 10) (showString "buttonsFromList " . showsPrec 11 (buttonsToList bs))

instance Semigroup MouseButtons where
  {-# INLINE (<>) #-}
  MouseButtons a <> MouseButtons b = MouseButtons (a .|. b)

instance Monoid MouseButtons where
  mempty = noButtons

-- | No button.
noButtons :: MouseButtons
noButtons = MouseButtons 0

-- | Whether the set holds the button.
{-# INLINE buttonsMember #-}
buttonsMember :: MouseButton -> MouseButtons -> Bool
buttonsMember b (MouseButtons w) = let i = buttonBit b in i >= 0 && testBit w i

-- | Whether the set is empty.
{-# INLINE buttonsNull #-}
buttonsNull :: MouseButtons -> Bool
buttonsNull (MouseButtons w) = w == 0

-- | The set with the button added.
{-# INLINE buttonsInsert #-}
buttonsInsert :: MouseButton -> MouseButtons -> MouseButtons
buttonsInsert b bs@(MouseButtons w) = let i = buttonBit b in if i < 0 then bs else MouseButtons (setBit w i)

-- | The set without the button.
{-# INLINE buttonsDelete #-}
buttonsDelete :: MouseButton -> MouseButtons -> MouseButtons
buttonsDelete b bs@(MouseButtons w) = let i = buttonBit b in if i < 0 then bs else MouseButtons (clearBit w i)

-- | The buttons in the set, by number.
buttonsToList :: MouseButtons -> [MouseButton]
buttonsToList (MouseButtons w)
  | w == 0 = []
  | otherwise =
      let i = countTrailingZeros w
       in mouseButtonNumber (i + 1) : buttonsToList (MouseButtons (clearBit w i))

-- | The set of the buttons listed.
buttonsFromList :: [MouseButton] -> MouseButtons
buttonsFromList = foldr buttonsInsert noButtons

-- | The buttons that pass the test, each tested once, in number order.
{-# INLINE buttonsFilterM #-}
buttonsFilterM :: Monad m => (MouseButton -> m Bool) -> MouseButtons -> m MouseButtons
buttonsFilterM p (MouseButtons w0) = go w0 0
  where
    go 0 !acc = pure (MouseButtons acc)
    go w !acc = do
      let i = countTrailingZeros w
      keep <- p (mouseButtonNumber (i + 1))
      go (clearBit w i) (if keep then setBit acc i else acc)

-- | Apply a button press ('True') or release to the held, pressed and
-- released sets.
applyMouseButton :: MouseButton -> Bool -> Input -> Input
applyMouseButton b True inp =
  inp {inputButtonsHeld = buttonsInsert b (inputButtonsHeld inp), inputButtonsPressed = buttonsInsert b (inputButtonsPressed inp)}
applyMouseButton b False inp =
  inp {inputButtonsHeld = buttonsDelete b (inputButtonsHeld inp), inputButtonsReleased = buttonsInsert b (inputButtonsReleased inp)}

-- | The pointer left the window: move it off every widget, as
-- 'withoutPointer' does, so nothing stays hovered. Held buttons stay held
-- until their releases arrive.
applyPointerLeave :: Input -> Input
applyPointerLeave inp = inp {inputMousePos = offWindow}

-- | A point far outside any window.
offWindow :: V2
offWindow = V2 (-1e6) (-1e6)

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

-- | Whether any mouse button is held.
{-# INLINE inputPointerHeld #-}
inputPointerHeld :: Input -> Bool
inputPointerHeld = not . buttonsNull . inputButtonsHeld

-- | Remove one-shot interaction events for a repeated view pass. Retains
-- pointer position, held buttons and keys (so hover and drag survive), and
-- the composition, which the focused field still draws.
stripInteractionInput :: Input -> Input
stripInteractionInput inp =
  inp
    { inputButtonsPressed = noButtons
    , inputButtonsReleased = noButtons
    , inputKeys = mempty
    , inputKeysNew = mempty
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
    { inputMousePos = offWindow
    , inputButtonsHeld = noButtons
    , inputButtonsPressed = noButtons
    , inputButtonsReleased = noButtons
    , inputScroll = V2 0 0
    }

-- | Text an input method (IME) is composing but has not committed, such as
-- Chinese or Japanese input before conversion. The focused text field draws
-- it underlined at the caret. The field's value changes only when the IME
-- commits, and the committed text arrives as typed text.
data Composition = Composition
  { compositionText :: !Text
  -- ^ The text being composed. Empty text counts as no composition.
  , compositionCursor :: !Int
  -- ^ The IME's caret, or the start of its selection, in characters.
  , compositionSelection :: !Int
  -- ^ Characters selected from 'compositionCursor', such as the clause
  -- being converted; 0 for a plain caret.
  }
  deriving (Eq, Show)

-- | Apply an IME composition update, as SDL's @SDL_EVENT_TEXT_EDITING@
-- reports it: text, selection start and selection length, in characters.
-- Empty text ends the composition. A start past the text is clamped; a
-- negative (unset) one puts the caret at the end.
applyComposition :: Text -> Int -> Int -> Input -> Input
applyComposition txt start len inp
  | T.null txt = inp {inputComposition = Nothing}
  | otherwise =
      let n = T.length txt
          cursor = if start < 0 then n else min n start
       in inp {inputComposition = Just (Composition txt cursor (clamp0 (n - cursor) len))}
  where
    clamp0 hi v = max 0 (min hi v)

-- | The kind of text a widget asks the IME for (iced's
-- @input_method::Purpose@): normal text, a secret the IME should neither
-- show nor learn, or a number. On-screen keyboards pick their keys from it.
data InputPurpose = InputNormal | InputSecure | InputNumeric
  deriving (Eq, Show, Enum, Bounded)
