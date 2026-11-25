{-# OPTIONS_HADDOCK not-home #-}

-- | Translation of RGFW events into 'NanoUI.Input.Input': key and modifier
-- tables, the cursor table, and the decoding of a drained event queue. The
-- session ("NanoUI.Rgfw.Internal.Session") polls the queue and folds the
-- decoded events into each frame's input.
module NanoUI.Rgfw.Internal.Input
  ( RgfwEvent (..)
  , pollRgfwEvents
  , prepareRgfwEvent
  , decodeRgfwEvents
  , applyRgfwEvent
  , isRgfwButtonEdge
  , isRgfwClose
  , mapRgfwCursor
  ) where

import Data.Bits (complement, (.&.), (.|.))
import Data.Char (chr, isControl, isDigit, toLower)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import Data.Word (Word8, Word32)
import Foreign.Ptr (Ptr)
import NanoUI (V2 (..), v2Add)
import NanoUI.Backend
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , applyKey
  , applyMouseButton
  , applyPointerLeave
  , cursorFallback
  , keypadKey
  , modifiersFromBits
  , mouseButtonNumber
  , releaseAllKeys
  )
import NanoUI.Backend (UiCursorKind (..))
import qualified RGFW as R

-- | The key for an RGFW key code and its modifier bits.
mapRgfwKey :: Word32 -> Word8 -> Maybe Key
mapRgfwKey k m
  | Just named <- lookup k namedRgfwKeys = Just named
  | Just c <- lookup k keypadRgfwKeys = keypadKey (m .&. R.rgfw_modNumLock /= 0) c
  | k >= R.rgfw_keyF1 && k <= R.rgfw_keyF24 = Just (KeyF (fromIntegral (k - R.rgfw_keyF1) + 1))
  -- Codes below 128 are the ASCII characters the keys type unshifted on a US
  -- layout.
  | k > 32 && k < 127 = Just (KeyChar (toLower (chr (fromIntegral k))))
  | otherwise = Nothing

namedRgfwKeys :: [(Word32, Key)]
namedRgfwKeys =
  [ (R.rgfw_keyEscape, KeyEscape)
  , (R.rgfw_keyReturn, KeyEnter)
  , (R.rgfw_keyPadReturn, KeyEnter)
  , (R.rgfw_keyTab, KeyTab)
  , (R.rgfw_keyBackSpace, KeyBackspace)
  , (R.rgfw_keyDelete, KeyDelete)
  , (R.rgfw_keyLeft, KeyLeft)
  , (R.rgfw_keyRight, KeyRight)
  , (R.rgfw_keyUp, KeyUp)
  , (R.rgfw_keyDown, KeyDown)
  , (R.rgfw_keyHome, KeyHome)
  , (R.rgfw_keyEnd, KeyEnd)
  , (R.rgfw_keyPageUp, KeyPageUp)
  , (R.rgfw_keyPageDown, KeyPageDown)
  , (R.rgfw_keyInsert, KeyInsert)
  , (R.rgfw_keySpace, KeySpace)
  , (R.rgfw_keyPrintScreen, KeyPrintScreen)
  , (R.rgfw_keyPause, KeyPause)
  , (R.rgfw_keyCapsLock, KeyCapsLock)
  , (R.rgfw_keyNumLock, KeyNumLock)
  , (R.rgfw_keyScrollLock, KeyScrollLock)
  , (R.rgfw_keyMenu, KeyMenu)
  , (R.rgfw_keyPadSlash, KeyChar '/')
  , (R.rgfw_keyPadMultiply, KeyChar '*')
  , (R.rgfw_keyPadMinus, KeyChar '-')
  , (R.rgfw_keyPadPlus, KeyChar '+')
  , (R.rgfw_keyPadEqual, KeyChar '=')
  ]

-- | Keypad digit and point codes, by the character each types ('keypadKey').
keypadRgfwKeys :: [(Word32, Char)]
keypadRgfwKeys =
  zip
    [R.rgfw_keyPad0, R.rgfw_keyPad1, R.rgfw_keyPad2, R.rgfw_keyPad3, R.rgfw_keyPad4, R.rgfw_keyPad5, R.rgfw_keyPad6, R.rgfw_keyPad7, R.rgfw_keyPad8, R.rgfw_keyPad9, R.rgfw_keyPadPeriod]
    "0123456789."

modsFromRgfw :: Word8 -> Modifiers
modsFromRgfw m = modifiersFromBits m R.rgfw_modShift R.rgfw_modControl R.rgfw_modAlt R.rgfw_modSuper

-- | The modifier bits of a key event, counting the key itself. On X11 and
-- Windows RGFW reads the bits before it records the key, so a modifier key's
-- press lacks its own bit and its release still has it. On macOS the bits
-- are already current.
ownModifier :: Word32 -> Bool -> Word8 -> Word8
ownModifier k down m = case lookup k modifierRgfwKeys of
  Just (bit, _) -> if down then m .|. bit else m .&. complement bit
  Nothing -> m

-- | Modifier keys, with their bit and the key on the other side.
modifierRgfwKeys :: [(Word32, (Word8, Word32))]
modifierRgfwKeys =
  [ (R.rgfw_keyShiftL, (R.rgfw_modShift, R.rgfw_keyShiftR))
  , (R.rgfw_keyShiftR, (R.rgfw_modShift, R.rgfw_keyShiftL))
  , (R.rgfw_keyControlL, (R.rgfw_modControl, R.rgfw_keyControlR))
  , (R.rgfw_keyControlR, (R.rgfw_modControl, R.rgfw_keyControlL))
  , (R.rgfw_keyAltL, (R.rgfw_modAlt, R.rgfw_keyAltR))
  , (R.rgfw_keyAltR, (R.rgfw_modAlt, R.rgfw_keyAltL))
  , (R.rgfw_keySuperL, (R.rgfw_modSuper, R.rgfw_keySuperR))
  , (R.rgfw_keySuperR, (R.rgfw_modSuper, R.rgfw_keySuperL))
  ]

-- | The RGFW standard cursor for a cursor kind, after 'cursorFallback'.
-- 'R.rgfw_mouseArrow' stands for the platform's default arrow.
mapRgfwCursor :: UiCursorKind -> Word8
mapRgfwCursor kind = case cursorFallback kind of
  UiCursorPointer    -> R.rgfw_mousePointingHand
  UiCursorText       -> R.rgfw_mouseIbeam
  UiCursorNsResize   -> R.rgfw_mouseResizeNS
  UiCursorEwResize   -> R.rgfw_mouseResizeEW
  UiCursorNwseResize -> R.rgfw_mouseResizeNWSE
  UiCursorNeswResize -> R.rgfw_mouseResizeNESW
  UiCursorNotAllowed -> R.rgfw_mouseNotAllowed
  UiCursorWait       -> R.rgfw_mouseWait
  UiCursorProgress   -> R.rgfw_mouseProgress
  UiCursorCrosshair  -> R.rgfw_mouseCrosshair
  UiCursorMove       -> R.rgfw_mouseResizeAll
  UiCursorNResize    -> R.rgfw_mouseResizeN
  UiCursorNeResize   -> R.rgfw_mouseResizeNE
  UiCursorEResize    -> R.rgfw_mouseResizeE
  UiCursorSeResize   -> R.rgfw_mouseResizeSE
  UiCursorSResize    -> R.rgfw_mouseResizeS
  UiCursorSwResize   -> R.rgfw_mouseResizeSW
  UiCursorWResize    -> R.rgfw_mouseResizeW
  UiCursorNwResize   -> R.rgfw_mouseResizeNW
  _                  -> R.rgfw_mouseArrow

-- | An RGFW event translated for the input fold.
data RgfwEvent
  = RgfwEvClose
  | RgfwEvResize -- ^ window size or monitor scale changed (read back at sync)
  | RgfwEvMotion !Float !Float
  | RgfwEvButton !Word8 !Bool
  | RgfwEvLeave -- ^ the pointer left the window
  | RgfwEvScroll !Float !Float
  | RgfwEvChar !Char -- ^ typed character
  | RgfwEvKey !Word32 !Word8 !Bool -- ^ key, modifiers, down (including repeats) or up
  | RgfwEvFocusLost -- ^ the window lost keyboard focus
  | RgfwEvRedraw -- ^ the window was uncovered or restored: paint it all

-- | Drain the RGFW queue, recording scale changes for the next sync, which
-- reads the window size itself. Character keys are remapped to the current
-- layout (@layoutKey@).
pollRgfwEvents :: R.Window -> Ptr R.RGFW_event -> IORef Float -> IORef Float -> IO [RgfwEvent]
pollRgfwEvents win evPtr scaleRef monScaleRef = do
  raw <- drain []
  scale <- readIORef scaleRef
  pure (decodeRgfwEvents scale raw)
  where
    drain acc =
      R.pollEvent win evPtr >>= \case
        R.EventNone -> pure (reverse acc)
        ev -> prepareRgfwEvent monScaleRef ev >>= \ev' -> drain (ev' : acc)

-- | Read one raw event before decoding: record a scale change for the next
-- sync, and remap a key to the current layout.
prepareRgfwEvent :: IORef Float -> R.Event -> IO R.Event
prepareRgfwEvent monScaleRef ev = case ev of
  R.EventScaleUpdate sx _ -> ev <$ writeIORef monScaleRef (if sx > 0 then sx else 1)
  R.EventKeyPress k m -> relayout R.EventKeyPress k m
  R.EventKeyRepeat k m -> relayout R.EventKeyRepeat k m
  -- A modifier key let go while the one on the other side is down
  -- leaves the modifier held, as that key's repeat would.
  R.EventKeyRelease k m
    | Just (_, twin) <- lookup k modifierRgfwKeys ->
        (\held -> if held then R.EventKeyRepeat twin m else ev) <$> R.isKeyDown twin
    | otherwise -> relayout R.EventKeyRelease k m
  _ -> pure ev
  where
    relayout event k m = (\k' -> event k' m) <$> layoutKey k
    -- RGFW key codes are what the key types on a US layout. Letter and
    -- punctuation keys are remapped to the current layout when the result is
    -- ASCII. Digits stay, since some layouts type symbols there unshifted.
    layoutKey k
      | k > 32 && k < 127 && not (isDigit (chr (fromIntegral k))) =
          maybe k (\mapped -> if mapped > 32 && mapped < 127 then mapped else k) <$> R.physicalToMappedKey k
      | otherwise = pure k

-- | Translate a batch of raw events in queue order. Pointer positions are
-- divided by the logical scale. Control characters are dropped: some
-- platforms send Ctrl+letter as one, and the key event already reports the
-- chord.
decodeRgfwEvents :: Float -> [R.Event] -> [RgfwEvent]
decodeRgfwEvents scale = mapMaybe $ \case
  R.EventWindowClose -> Just RgfwEvClose
  R.EventWindowResize _ _ -> Just RgfwEvResize
  R.EventScaleUpdate _ _ -> Just RgfwEvResize
  R.EventMouseMotion x y -> Just (RgfwEvMotion (fromIntegral x / scale) (fromIntegral y / scale))
  R.EventMouseButton btn down -> Just (RgfwEvButton btn down)
  -- RGFW's wheel is positive up and left; the input's is down and right.
  R.EventMouseScroll dx dy -> Just (RgfwEvScroll (negate dx) (negate dy))
  R.EventOther t
    | t == R.rgfw_mouseLeave -> Just RgfwEvLeave
    | t == R.rgfw_windowFocusOut -> Just RgfwEvFocusLost
    | t == R.rgfw_windowRefresh || t == R.rgfw_windowRestored -> Just RgfwEvRedraw
  R.EventKeyPress k m -> Just (RgfwEvKey k m True)
  R.EventKeyRepeat k m -> Just (RgfwEvKey k m True)
  R.EventKeyRelease k m -> Just (RgfwEvKey k m False)
  -- Every character but the controls types, joiners and bidi marks included.
  -- On macOS, RGFW passes on the private-use characters AppKit gives arrow
  -- and function keys (U+F700 to U+F747), which type nothing. The rest of
  -- the private-use area types, the Apple logo (U+F8FF) among it.
  R.EventKeyChar ch | not (isControl ch) && not (ch >= '\xF700' && ch <= '\xF747') -> Just (RgfwEvChar ch)
  _ -> Nothing

-- | Accumulate a decoded event into frame input. The caller handles close
-- and resize events separately; motion coordinates are already scaled by decoding.
applyRgfwEvent :: Input -> RgfwEvent -> Input
applyRgfwEvent inp ev = case ev of
  RgfwEvClose -> inp
  RgfwEvResize -> inp
  RgfwEvMotion x y -> inp {inputMousePos = V2 x y}
  -- RGFW numbers buttons from 0 in 'mouseButtonNumber' order: left, middle,
  -- right, back, forward, then the rest.
  RgfwEvButton btn down -> applyMouseButton (mouseButtonNumber (fromIntegral btn + 1)) down inp
  -- A drag keeps its pointer ('applyPointerLeave').
  RgfwEvLeave -> applyPointerLeave inp
  RgfwEvScroll dx dy -> inp {inputScroll = v2Add (inputScroll inp) (V2 dx dy)}
  RgfwEvChar c -> inp {inputChars = T.snoc (inputChars inp) c}
  -- 'applyKey' detects auto-repeat because the key is already held.
  RgfwEvKey k m down ->
    (maybe inp (\key -> applyKey key down inp) (mapRgfwKey k m)) {inputModifiers = modsFromRgfw (ownModifier k down m)}
  RgfwEvFocusLost -> releaseAllKeys inp
  RgfwEvRedraw -> inp {inputWindowRedraw = True}

-- | Whether an event presses or releases a mouse button, and whether it
-- closes the window.
isRgfwButtonEdge, isRgfwClose :: RgfwEvent -> Bool
isRgfwButtonEdge = \case RgfwEvButton {} -> True; _ -> False
isRgfwClose = \case RgfwEvClose -> True; _ -> False
