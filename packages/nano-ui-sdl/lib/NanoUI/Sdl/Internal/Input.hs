{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | SDL3 event polling and waiting, and translation of SDL events into
-- 'NanoUI.Input.Input'.
module NanoUI.Sdl.Internal.Input
  ( SdlEvent (..)
  , pollEvents
  , waitEvent
  , applyEvent
  , isButtonEdge
  , sdlKey
  ) where

import Control.Monad (mfilter)
import Data.Bits ((.&.))
import Data.Char (chr, isPrint, toLower)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import Data.Word (Word32)
import Foreign.C.Types (CFloat, CUInt)
import Data.Maybe (fromMaybe)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybePeek)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import GHC.Records.Compat (getField)
import SDL3.Sys.Bindgen.Runtime.CBool qualified as CBool
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import NanoUI (V2 (..), v2Add)
import NanoUI.Backend
import NanoUI.Sdl.Internal.Display (refreshEventType, takeRefreshEvent)
import SDL3.Sys.Bindgen.Events
  ( SDL_Event (..)
  , SDL_EventType (..)
  , SDL_KeyboardEvent
  )
import SDL3.Sys.Bindgen.Events qualified as Events
import SDL3.Sys.Events (pollEventSafe, waitEventSafe, waitEventTimeoutSafe)
import SDL3.Sys.Bindgen.Keycode
  ( SDL_Keycode (..)
  , SDL_Keymod (..)
  , sDLK_APPLICATION
  , sDLK_BACKSPACE
  , sDLK_CAPSLOCK
  , sDLK_DELETE
  , sDLK_DOWN
  , sDLK_END
  , sDLK_ESCAPE
  , sDLK_F1
  , sDLK_F12
  , sDLK_F13
  , sDLK_F24
  , sDLK_HOME
  , sDLK_INSERT
  , sDLK_KP_0
  , sDLK_KP_1
  , sDLK_KP_2
  , sDLK_KP_3
  , sDLK_KP_4
  , sDLK_KP_5
  , sDLK_KP_6
  , sDLK_KP_7
  , sDLK_KP_8
  , sDLK_KP_9
  , sDLK_KP_DIVIDE
  , sDLK_KP_ENTER
  , sDLK_KP_EQUALS
  , sDLK_KP_MINUS
  , sDLK_KP_MULTIPLY
  , sDLK_KP_PERIOD
  , sDLK_KP_PLUS
  , sDLK_LEFT
  , sDLK_MENU
  , sDLK_NUMLOCKCLEAR
  , sDLK_PAGEDOWN
  , sDLK_PAGEUP
  , sDLK_PAUSE
  , sDLK_PRINTSCREEN
  , sDLK_RETURN
  , sDLK_RIGHT
  , sDLK_SCANCODE_MASK
  , sDLK_SCROLLLOCK
  , sDLK_SPACE
  , sDLK_TAB
  , sDLK_UP
  , sDL_KMOD_ALT
  , sDL_KMOD_CTRL
  , sDL_KMOD_GUI
  , sDL_KMOD_NUM
  , sDL_KMOD_SHIFT
  )
import SDL3.Sys.Bindgen.Mouse (sDL_BUTTON_LEFT, sDL_BUTTON_MIDDLE, sDL_BUTTON_RIGHT, sDL_BUTTON_X1, sDL_BUTTON_X2)
import SDL3.Sys.Bindgen.Stdinc (Uint32 (..))
import SDL3.Sys.Keyboard (getModState)

-- | Copied SDL event data. Pointer positions use SDL window coordinates until
-- display synchronisation converts them to nano-ui's logical coordinates.
data SdlEvent
  = EvQuit
  | EvWindowChanged
  -- ^ The window's size or pixel density changed; display synchronisation
  -- reads both again.
  | EvKey Key Modifiers
  -- ^ A key went down, or repeated while held.
  | EvKeyUp Key Modifiers
  | EvModifiers Modifiers
  -- ^ A key nano-ui has no 'Key' for, a modifier key among them, went down
  -- or up; only the modifiers it leaves held are kept.
  | EvText Text Modifiers
  | EvMouseMotion V2 Modifiers
  | EvMouseButton MouseButton Bool V2 Modifiers
  -- ^ A button went down ('True') or up at a point.
  | EvScroll V2
  | EvDrop DropEvent
  | EvRefresh
  | EvWindowRedraw
  deriving (Eq, Show)

-- | Drain every pending event, oldest first.
pollEvents :: IO [SdlEvent]
pollEvents =
  alloca $ \(p :: Ptr SDL_Event) -> do
    refreshTy <- refreshEventType
    let drain acc = do
          got <- pollEventSafe p
          if got
            then decodeEvent refreshTy p >>= \ev -> drain (maybe acc (: acc) ev)
            else pure (reverse acc)
    drain []

-- | Wait for one native event, for up to the supplied milliseconds or, when
-- negative, for as long as it takes. 'Nothing' covers a timeout, an SDL
-- failure and an ignored event, so it does not imply a quit request. Call
-- from the display thread.
waitEvent :: Int -> IO (Maybe SdlEvent)
waitEvent ms =
  alloca $ \p -> do
    got <- if ms < 0 then waitEventSafe p else waitEventTimeoutSafe p (fromIntegral ms)
    if got then refreshEventType >>= \ty -> decodeEvent ty p else pure Nothing

-- | Translate one SDL event, given the refresh event type; 'Nothing' for
-- events the UI ignores.
decodeEvent :: Word32 -> Ptr SDL_Event -> IO (Maybe SdlEvent)
decodeEvent refreshTy p = do
  Uint32 w <- peek p.type'
  if refreshTy /= 0 && w == refreshTy
    then Just EvRefresh <$ takeRefreshEvent
    else case SDL_EventType (fromIntegral w) of
      Events.SDL_EVENT_QUIT -> pure (Just EvQuit)
      Events.SDL_EVENT_WINDOW_RESIZED -> pure (Just EvWindowChanged)
      Events.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED -> pure (Just EvWindowChanged)
      Events.SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED -> pure (Just EvWindowChanged)
      -- The window manager damaged our window surface (occlusion, compositor
      -- effects, restore). The backbuffer contents are gone; the next present
      -- must be full or stale regions flash.
      Events.SDL_EVENT_WINDOW_EXPOSED -> pure (Just EvWindowRedraw)
      Events.SDL_EVENT_WINDOW_RESTORED -> pure (Just EvWindowRedraw)
      Events.SDL_EVENT_KEY_DOWN -> keyDown <$> peek p.key
      Events.SDL_EVENT_KEY_UP -> Just . keyUp <$> peek p.key
      Events.SDL_EVENT_TEXT_INPUT -> textInput p
      Events.SDL_EVENT_MOUSE_MOTION -> do
        me <- peek p.motion
        Just . EvMouseMotion (v2 (getField @"x" me) (getField @"y" me)) <$> peekModifiers
      Events.SDL_EVENT_MOUSE_BUTTON_DOWN -> mouseButton p True
      Events.SDL_EVENT_MOUSE_BUTTON_UP -> mouseButton p False
      Events.SDL_EVENT_MOUSE_WHEEL -> do
        we <- peek p.wheel
        pure (Just (EvScroll (v2 (getField @"x" we) (negate (getField @"y" we)))))
      Events.SDL_EVENT_DROP_FILE -> dropEvent p DropFile
      Events.SDL_EVENT_DROP_TEXT -> dropEvent p DropText
      Events.SDL_EVENT_DROP_BEGIN -> dropEvent p DropBegin
      Events.SDL_EVENT_DROP_COMPLETE -> dropEvent p DropComplete
      Events.SDL_EVENT_DROP_POSITION -> dropEvent p DropPosition
      _ -> pure Nothing

v2 :: CFloat -> CFloat -> V2
v2 x y = V2 (realToFrac x) (realToFrac y)

-- | A key press. A held key's auto-repeats are presses too, for the keys
-- that repeat ('keyRepeats').
keyDown :: SDL_KeyboardEvent -> Maybe SdlEvent
keyDown ke =
  case sdlKey (keyCode ke) (keyMods ke) of
    Just k
      | keyRepeats k || not (CBool.toBool (getField @"repeat" ke)) -> Just (EvKey k mods)
      | otherwise -> Nothing
    Nothing -> Just (EvModifiers mods)
  where
    mods = modFromKeymod (keyMods ke)

keyUp :: SDL_KeyboardEvent -> SdlEvent
keyUp ke = maybe (EvModifiers mods) (`EvKeyUp` mods) (sdlKey (keyCode ke) (keyMods ke))
  where
    mods = modFromKeymod (keyMods ke)

keyCode :: SDL_KeyboardEvent -> CUInt
keyCode ke = fromIntegral (getField @"key" ke :: SDL_Keycode)

keyMods :: SDL_KeyboardEvent -> SDL_Keymod
keyMods ke = getField @"mod" ke

-- | The key an SDL keycode names, given the modifier state it came with. SDL
-- reports a key that types a character by the character it types unmodified
-- in the current layout (with Latin letters on a non-Latin layout), which is
-- the 'KeyChar'.
sdlKey :: CUInt -> SDL_Keymod -> Maybe Key
sdlKey code km
  | Just named <- lookup code namedKeys = Just named
  | Just typed <- lookup code keypadKeys = keypadKey (word32 km .&. word32 sDL_KMOD_NUM /= 0) typed
  | code >= sDLK_F1 && code <= sDLK_F12 = Just (KeyF (fromIntegral (code - sDLK_F1) + 1))
  | code >= sDLK_F13 && code <= sDLK_F24 = Just (KeyF (fromIntegral (code - sDLK_F13) + 13))
  | code .&. sDLK_SCANCODE_MASK == 0 && code <= 0x10FFFF, isPrint c = Just (KeyChar (toLower c))
  | otherwise = Nothing
  where
    c = chr (fromIntegral code)

namedKeys :: [(CUInt, Key)]
namedKeys =
  [ (sDLK_ESCAPE, KeyEscape)
  , (sDLK_RETURN, KeyEnter)
  , (sDLK_TAB, KeyTab)
  , (sDLK_BACKSPACE, KeyBackspace)
  , (sDLK_DELETE, KeyDelete)
  , (sDLK_LEFT, KeyLeft)
  , (sDLK_RIGHT, KeyRight)
  , (sDLK_UP, KeyUp)
  , (sDLK_DOWN, KeyDown)
  , (sDLK_HOME, KeyHome)
  , (sDLK_END, KeyEnd)
  , (sDLK_PAGEUP, KeyPageUp)
  , (sDLK_PAGEDOWN, KeyPageDown)
  , (sDLK_INSERT, KeyInsert)
  , (sDLK_SPACE, KeySpace)
  , (sDLK_PRINTSCREEN, KeyPrintScreen)
  , (sDLK_PAUSE, KeyPause)
  , (sDLK_CAPSLOCK, KeyCapsLock)
  , (sDLK_NUMLOCKCLEAR, KeyNumLock)
  , (sDLK_SCROLLLOCK, KeyScrollLock)
  , (sDLK_APPLICATION, KeyMenu)
  , (sDLK_MENU, KeyMenu)
  , (sDLK_KP_ENTER, KeyEnter)
  , (sDLK_KP_DIVIDE, KeyChar '/')
  , (sDLK_KP_MULTIPLY, KeyChar '*')
  , (sDLK_KP_MINUS, KeyChar '-')
  , (sDLK_KP_PLUS, KeyChar '+')
  , (sDLK_KP_EQUALS, KeyChar '=')
  ]

-- | The keypad digits and point, by the character each types ('keypadKey').
keypadKeys :: [(CUInt, Char)]
keypadKeys =
  zip [sDLK_KP_0, sDLK_KP_1, sDLK_KP_2, sDLK_KP_3, sDLK_KP_4, sDLK_KP_5, sDLK_KP_6, sDLK_KP_7, sDLK_KP_8, sDLK_KP_9, sDLK_KP_PERIOD] "0123456789."

textInput :: Ptr SDL_Event -> IO (Maybe SdlEvent)
textInput p = do
  te <- peek p.text
  mods <- peekModifiers
  txt <- maybePeek TF.peekCString (PtrConst.unsafeToPtr (getField @"text" te))
  pure ((`EvText` mods) <$> mfilter (not . T.null) txt)

mouseButton :: Ptr SDL_Event -> Bool -> IO (Maybe SdlEvent)
mouseButton p down = do
  be <- peek p.button
  mods <- peekModifiers
  let btn = fromIntegral (getField @"button" be)
      press b = EvMouseButton b down (v2 (getField @"x" be) (getField @"y" be)) mods
      buttons =
        [ (sDL_BUTTON_LEFT, MouseLeft)
        , (sDL_BUTTON_RIGHT, MouseRight)
        , (sDL_BUTTON_MIDDLE, MouseMiddle)
        , (sDL_BUTTON_X1, MouseBack)
        , (sDL_BUTTON_X2, MouseForward)
        ]
  pure (press <$> lookup btn buttons)

dropEvent :: Ptr SDL_Event -> DropType -> IO (Maybe SdlEvent)
dropEvent p ty = do
  de <- peek p.drop
  let -- A drag entering or finishing has no position.
      pos
        | ty == DropBegin || ty == DropComplete = Nothing
        | otherwise = Just (v2 (getField @"x" de) (getField @"y" de))
  payload <- fromMaybe "" <$> maybePeek TF.peekCString (PtrConst.unsafeToPtr (getField @"data'" de))
  pure (Just (EvDrop (DropEvent ty pos payload)))

peekModifiers :: IO Modifiers
peekModifiers = modFromKeymod <$> getModState

modFromKeymod :: SDL_Keymod -> Modifiers
modFromKeymod km = modifiersFromBits (word32 km) (word32 sDL_KMOD_SHIFT) (word32 sDL_KMOD_CTRL) (word32 sDL_KMOD_ALT) (word32 sDL_KMOD_GUI)

word32 :: Integral a => a -> Word32
word32 = fromIntegral

-- | Accumulate one event into a frame's input. Window changes and quits need
-- session handling; this does not update size or convert coordinates.
applyEvent :: Input -> SdlEvent -> Input
applyEvent inp ev =
  case ev of
    EvKey k mods -> (applyKey k True inp) {inputModifiers = mods}
    EvKeyUp k mods -> (applyKey k False inp) {inputModifiers = mods}
    EvModifiers mods -> inp {inputModifiers = mods}
    EvText txt mods ->
      inp {inputChars = inputChars inp <> txt, inputModifiers = mods}
    EvMouseMotion pos mods ->
      inp {inputMousePos = pos, inputModifiers = mods}
    EvMouseButton btn down pos mods ->
      (applyMouseButton btn down inp) {inputMousePos = pos, inputModifiers = mods}
    EvScroll delta -> inp {inputScroll = v2Add (inputScroll inp) delta}
    EvDrop dropEv -> inp {inputDrops = appendDropEvent dropEv (inputDrops inp)}
    EvWindowRedraw -> inp {inputWindowRedraw = True}
    -- A wake asks for a frame, not a repaint: the session runs one, and its
    -- damage decides what is presented, if anything.
    EvRefresh -> inp
    EvQuit -> inp
    EvWindowChanged -> inp

-- | Whether an event is a button press or release and should end an input batch.
isButtonEdge :: SdlEvent -> Bool
isButtonEdge = \case
  EvMouseButton {} -> True
  _ -> False
