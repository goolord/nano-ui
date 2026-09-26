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
  , buttonHeld
  , buttonPressed
  , buttonReleased
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
modPrimary = if onMac then modSuper else modCtrl

-- | The platform's command modifier alone ('modPrimary').
primaryModifiers :: Modifiers
primaryModifiers
  | onMac = noModifiers {modSuper = True}
  | otherwise = noModifiers {modCtrl = True}

-- | Whether the modifier that widens a caret motion or a deletion to a word
-- is held: Option ('modAlt') on macOS and Ctrl elsewhere, as iced's @jump@.
modJump :: Modifiers -> Bool
modJump = if onMac then modAlt else modCtrl

-- | Whether Command ('modSuper') is held on macOS, where it takes a caret
-- motion or a deletion to the line's end. Never elsewhere.
modMacCommand :: Modifiers -> Bool
modMacCommand m = onMac && modSuper m

-- | Whether this is macOS, whose keys differ: Command is the command
-- modifier, and Option types.
onMac :: Bool
onMac = os == "darwin"
{-# NOINLINE onMac #-}

-- | Whether no modifier but Shift is held: a key a control acts on, such as
-- an arrow on a slider, is its own pressed so, and a chord otherwise.
shiftAtMost :: Modifiers -> Bool
shiftAtMost m = not (modCtrl m || modAlt m || modSuper m)

-- | Whether a key pressed with these modifiers is a command rather than
-- typing: every named key but Space, and a key that types ('KeyChar',
-- Space) held with modifiers that make a chord of it. A character key types
-- alone, with Shift, and with AltGr, which is Ctrl+Alt, or on macOS with
-- Option; Super (Command), Ctrl, and elsewhere than macOS Alt alone, make a
-- chord of it.
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

-- | Input for one frame. Positions and window sizes use logical pixels;
-- scroll values use wheel steps and delta time uses seconds. Held buttons and
-- held keys and the input method's composition persist between frames;
-- presses and releases, text, key presses and releases, and drops are events
-- consumed once. Backends clear those
-- events with 'clearEphemeral'.
data Input = Input
  { inputMousePos :: {-# UNPACK #-} !V2
  , inputButtonsHeld :: {-# UNPACK #-} !MouseButtons
  -- ^ The mouse buttons down as the frame's events leave them ('heldIn').
  , inputButtonsPressed :: {-# UNPACK #-} !MouseButtons
  -- ^ The mouse buttons that went down this frame ('pressedIn').
  , inputButtonsReleased :: {-# UNPACK #-} !MouseButtons
  -- ^ The mouse buttons that came up this frame ('releasedIn').
  , inputMouseClicks :: {-# UNPACK #-} !Int
  -- ^ 1 for this frame's press, or 2 or 3 when it came soon after the one
  -- before, near it and with the same button: a double or triple click.
  , inputScroll :: {-# UNPACK #-} !V2
  , inputKeys :: SmallArray Key
  -- ^ Keys pressed this frame in event order, with a held key's auto-repeats.
  -- A frame the session runner ('takeFrame') batches has at most one
  -- command key ('isCommandKey'), and it comes last, perhaps repeating: the
  -- frame's text came before it, and 'inputModifiers' are what it went down
  -- with.
  , inputKeysNew :: SmallArray Key
  -- ^ The presses of 'inputKeys' that are not auto-repeats: keys that went
  -- down this frame while up, in event order.
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

-- | A key or a mouse button, asked about in a frame's 'Input': whether it
-- went down this frame, came up, or is down. Keys and buttons read alike,
-- and like 'NanoUI.shortcutIn' for a chord:
--
-- > pressedIn KeyEscape inp
-- > heldIn MouseMiddle inp
--
-- These read the input as it stands, whatever has the keyboard or the
-- pointer. A view listens with 'NanoUI.keyPressed' and 'NanoUI.mousePressed'
-- instead, which stay quiet where the keys or the pointer are not the view's.
class Pressable a where
  -- | Whether it went down this frame; a held key's auto-repeats count.
  pressedIn :: a -> Input -> Bool

  -- | Whether it went down this frame while it was up: a key's
  -- auto-repeats do not count ('inputKeysNew'). A mouse button does not
  -- repeat, so for one this is 'pressedIn'.
  pressedOnceIn :: a -> Input -> Bool
  pressedOnceIn = pressedIn

  -- | Whether it came up this frame.
  releasedIn :: a -> Input -> Bool

  -- | Whether it is down as the frame's events leave it.
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
  pressedIn = buttonPressed
  {-# INLINE releasedIn #-}
  releasedIn = buttonReleased
  {-# INLINE heldIn #-}
  heldIn = buttonHeld

-- | 'heldIn', 'pressedIn' and 'releasedIn' for a mouse button alone, which
-- the frame's own steps call.
{-# INLINE buttonHeld #-}
buttonHeld :: MouseButton -> Input -> Bool
buttonHeld b = buttonsMember b . inputButtonsHeld

{-# INLINE buttonPressed #-}
buttonPressed :: MouseButton -> Input -> Bool
buttonPressed b = buttonsMember b . inputButtonsPressed

{-# INLINE buttonReleased #-}
buttonReleased :: MouseButton -> Input -> Bool
buttonReleased b = buttonsMember b . inputButtonsReleased

-- | Whether any mouse button went down this frame.
{-# INLINE anyButtonPressed #-}
anyButtonPressed :: Input -> Bool
anyButtonPressed = not . buttonsNull . inputButtonsPressed

-- | Whether any mouse button came up this frame.
{-# INLINE anyButtonReleased #-}
anyButtonReleased :: Input -> Bool
anyButtonReleased = not . buttonsNull . inputButtonsReleased

-- | The left button's held, pressed and released state, and the right
-- button's, under the names the fields had in nano-ui 0.1.
inputMouseDown, inputMousePressed, inputMouseReleased :: Input -> Bool
inputMouseDown = buttonHeld MouseLeft
inputMousePressed = buttonPressed MouseLeft
inputMouseReleased = buttonReleased MouseLeft
{-# DEPRECATED inputMouseDown "Use heldIn MouseLeft" #-}
{-# DEPRECATED inputMousePressed "Use pressedIn MouseLeft" #-}
{-# DEPRECATED inputMouseReleased "Use releasedIn MouseLeft" #-}

inputMouseRightDown, inputMouseRightPressed, inputMouseRightReleased :: Input -> Bool
inputMouseRightDown = buttonHeld MouseRight
inputMouseRightPressed = buttonPressed MouseRight
inputMouseRightReleased = buttonReleased MouseRight
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
-- shapes are CSS's cursors, and a backend shows each with the platform's
-- cursor of that name. Where the platform has none, it shows the closest
-- one it has: the SDL and RGFW backends show the 'cursorFallback'.
data UiCursorKind
  = -- | From a widget ('NanoUI.Widgets.Custom.widgetCursor'), no opinion:
    -- the 'NanoUI.withCursorShape' scope around it, if any, picks, and the
    -- arrow shows where none does. As a scope's shape
    -- (@withCursorShape UiCursorDefault@), the arrow.
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
  | -- | No pointer shown, as over a video or a canvas that draws its own.
    UiCursorHidden
  deriving (Eq, Show, Enum, Bounded)

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
  | onTarget, buttonHeld MouseLeft inp = UiCursorGrabbing
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

-- | Fold a batch of events into a frame's input, from @start@, as far as one
-- frame may take them: the frame's input, the events it took, and the rest,
-- for the frames after. A frame ends after an edge (@isEdge@, a mouse
-- button going down or up), so each press and release has a frame. It also
-- ends after a command key ('isCommandKey') when text, another key or a
-- change of modifiers comes next: within a frame the order of text, keys and
-- modifiers is lost, so a frame's text comes before its one command key,
-- and its modifiers are those the key went down with. Auto-repeats of the
-- key stay in its frame, so a burst takes extra frames and steady typing
-- none.
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

-- | Whether the event that took a frame's input from @cur@ to @next@ must
-- wait for the next frame: @cur@'s last key press is a command key, and the
-- event typed text, pressed another key or changed the modifiers.
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

-- | Apply a key going down ('True') or up: a press joins 'inputKeys', and,
-- unless the key is already down (an auto-repeat), 'inputKeysNew' and
-- 'inputKeysHeld'; a release joins 'inputKeysReleased' and leaves the held
-- keys. A backend passes every auto-repeat of a held key in as a press.
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

-- | Release every held key and modifier, as a backend does when its window
-- loses the keyboard: the keys let go elsewhere send no release, and would
-- otherwise stay held. Each held key joins 'inputKeysReleased'.
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

-- | The key a keypad digit or point (@'0'@ to @'9'@, @'.'@) is: with Num
-- Lock on, the character it types; off, the navigation key printed on it,
-- which 5 has none of.
keypadKey :: Bool -> Char -> Maybe Key
keypadKey True c = Just (KeyChar c)
keypadKey False c =
  lookup c [('0', KeyInsert), ('1', KeyEnd), ('2', KeyDown), ('3', KeyPageDown), ('4', KeyLeft), ('6', KeyRight), ('7', KeyHome), ('8', KeyUp), ('9', KeyPageUp), ('.', KeyDelete)]

-- | A mouse button. 'MouseBack' and 'MouseForward' are the side buttons (X1
-- and X2) a browser navigates with.
data MouseButton
  = MouseLeft
  | MouseRight
  | MouseMiddle
  | MouseBack
  | MouseForward
  | MouseOther !Int
  -- ^ Any other button, by its number: buttons count from 1 as SDL numbers
  -- them, left, middle, right, back and forward first, so the first button
  -- past those is @MouseOther 6@ ('mouseButtonNumber'). A set holds
  -- @MouseOther 1@ to @MouseOther 5@ as the buttons named above, and tracks
  -- the buttons up to 32.
  deriving (Eq, Ord, Show)

-- | The button with a number, counting left, middle, right, back and
-- forward as 1 to 5, as SDL numbers them ('MouseOther').
mouseButtonNumber :: Int -> MouseButton
mouseButtonNumber = \case
  1 -> MouseLeft
  2 -> MouseMiddle
  3 -> MouseRight
  4 -> MouseBack
  5 -> MouseForward
  n -> MouseOther n

-- | The button's bit in a 'MouseButtons': its number less one, or -1 for a
-- number past 32, which a set cannot hold.
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

-- | A set of mouse buttons: those held, pressed or released in an 'Input',
-- or those a widget was clicked with ('NanoUI.respClickedWith'). '<>' is the
-- union.
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

-- | The buttons of the set that pass the test, each tested once, in number
-- order. An empty set tests nothing.
{-# INLINE buttonsFilterM #-}
buttonsFilterM :: Monad m => (MouseButton -> m Bool) -> MouseButtons -> m MouseButtons
buttonsFilterM p (MouseButtons w0) = go w0 0
  where
    go 0 !acc = pure (MouseButtons acc)
    go w !acc = do
      let i = countTrailingZeros w
      keep <- p (mouseButtonNumber (i + 1))
      go (clearBit w i) (if keep then setBit acc i else acc)

-- | Apply a button going down ('True') or up: it joins the held buttons and
-- this frame's presses, or leaves the held ones and joins the releases.
applyMouseButton :: MouseButton -> Bool -> Input -> Input
applyMouseButton b True inp =
  inp {inputButtonsHeld = buttonsInsert b (inputButtonsHeld inp), inputButtonsPressed = buttonsInsert b (inputButtonsPressed inp)}
applyMouseButton b False inp =
  inp {inputButtonsHeld = buttonsDelete b (inputButtonsHeld inp), inputButtonsReleased = buttonsInsert b (inputButtonsReleased inp)}

-- | The pointer left the window: move it far off every widget, as
-- 'withoutPointer' does, so nothing stays hovered. Held buttons stay held;
-- their releases come as usual.
applyPointerLeave :: Input -> Input
applyPointerLeave inp = inp {inputMousePos = offWindow}

-- | A point off every widget, far outside any window.
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
-- pointer position, held buttons and held keys so hover and drag state remain
-- available, and the composition, which the focused field still shows.
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

-- | What a widget taking text asks the input method for, as iced's
-- @input_method::Purpose@: ordinary text, a secret such as a password,
-- which the input method should neither show nor learn, or a number. An
-- on-screen keyboard shows the keys for it.
data InputPurpose = InputNormal | InputSecure | InputNumeric
  deriving (Eq, Show, Enum, Bounded)
