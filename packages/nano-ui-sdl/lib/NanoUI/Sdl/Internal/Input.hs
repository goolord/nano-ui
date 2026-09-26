{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | SDL3 event polling and waiting, translation of SDL events into
-- 'NanoUI.Input.Input', and keeping SDL's text input in step with the
-- focused text field.
module NanoUI.Sdl.Internal.Input
  ( SdlEvent (..)
  , pollEvents
  , waitEvent
  , applyEvent
  , isButtonEdge
  , sdlKey
  , TextInputSync
  , newTextInputSync
  , syncTextInput
  ) where

import Control.Monad (mfilter, void, when)
import Data.Bits ((.&.))
import Data.Char (chr, isPrint, toLower)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.ByteString qualified as BS
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import Data.Word (Word32)
import Foreign.C.Types (CChar, CFloat, CUInt)
import Data.Foldable (for_)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (maybePeek, with)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import GHC.Records.Compat (getField)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import NanoUI (Rect (..), V2 (..), WidgetId (..), v2Add)
import NanoUI.Backend
import NanoUI.Testing (Context)
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
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Stdinc (Uint32 (..))
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Bindgen.Keyboard (SDL_TextInputType (..), sDL_PROP_TEXTINPUT_TYPE_NUMBER)
import SDL3.Sys.Bindgen.Keyboard qualified as Keyboard
import SDL3.Sys.Keyboard (getModState, setTextInputAreaSafe, startTextInputWithPropertiesSafe, stopTextInputSafe)
import SDL3.Sys.Properties (createPropertiesSafe, destroyPropertiesSafe, setNumberPropertySafe)

-- | Copied SDL event data. Pointer positions use SDL window coordinates until
-- display synchronisation converts them to nano-ui's logical coordinates.
data SdlEvent
  = EvQuit
  | EvWindowChanged
  -- ^ The window's size or pixel density changed; display synchronisation
  -- reads both again.
  | EvSystemThemeChanged
  -- ^ The desktop switched between light and dark; display synchronisation
  -- reads which.
  | EvKey Key Bool Modifiers
  -- ^ A key went down ('True', an auto-repeat of a held key too) or up.
  | EvModifiers Modifiers
  -- ^ A key nano-ui has no 'Key' for, a modifier key among them, went down
  -- or up; only the modifiers it leaves held are kept.
  | EvText Text Modifiers
  | EvMouseMotion V2 Modifiers
  | EvMouseButton MouseButton Bool V2 Modifiers
  -- ^ A button went down ('True') or up at a point.
  | EvMouseLeave
  -- ^ The pointer left the window.
  | EvScroll V2
  | EvDrop DropEvent
  | EvRefresh
  | EvWindowRedraw
  | EvEditing Text Int Int
  -- ^ The input method's composition changed: its text, and where its caret
  -- or selection starts and how long that is ('applyComposition'). Empty
  -- text ends it.
  | EvFocusLost
  -- ^ The window lost the keyboard. The keys held go up elsewhere
  -- ('releaseAllKeys'), and the composition ends.
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
      Events.SDL_EVENT_SYSTEM_THEME_CHANGED -> pure (Just EvSystemThemeChanged)
      -- The window manager damaged our window surface (occlusion, compositor
      -- effects, restore). The backbuffer contents are gone; the next present
      -- must be full or stale regions flash.
      Events.SDL_EVENT_WINDOW_EXPOSED -> pure (Just EvWindowRedraw)
      Events.SDL_EVENT_WINDOW_RESTORED -> pure (Just EvWindowRedraw)
      Events.SDL_EVENT_KEY_DOWN -> Just . keyEvent True <$> peek p.key
      Events.SDL_EVENT_KEY_UP -> Just . keyEvent False <$> peek p.key
      Events.SDL_EVENT_TEXT_INPUT -> textInput p
      Events.SDL_EVENT_TEXT_EDITING -> textEditing p
      -- SDL stops text input while the window is in the background, which
      -- drops the input method's composition without always saying so. End
      -- it here, or a field would go on showing it and giving it its keys.
      Events.SDL_EVENT_WINDOW_FOCUS_LOST -> pure (Just EvFocusLost)
      Events.SDL_EVENT_MOUSE_MOTION -> do
        me <- peek p.motion
        Just . EvMouseMotion (v2 (getField @"x" me) (getField @"y" me)) <$> peekModifiers
      Events.SDL_EVENT_WINDOW_MOUSE_LEAVE -> pure (Just EvMouseLeave)
      Events.SDL_EVENT_MOUSE_BUTTON_DOWN -> Just <$> mouseButton p True
      Events.SDL_EVENT_MOUSE_BUTTON_UP -> Just <$> mouseButton p False
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

-- | A key going down ('True') or up. A held key's auto-repeats are presses
-- too, which 'applyKey' tells from the first by the key being held already.
keyEvent :: Bool -> SDL_KeyboardEvent -> SdlEvent
keyEvent down ke = maybe (EvModifiers mods) (\k -> EvKey k down mods) (sdlKey (keyCode ke) (keyMods ke))
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
  txt <- peekText (getField @"text" te)
  pure ((`EvText` mods) <$> mfilter (not . T.null) txt)

-- | The input method's composition. With @SDL_HINT_IME_IMPLEMENTED_UI@ set
-- to @composition@, which the session sets, SDL sends it rather than letting
-- the input method draw it over the window.
textEditing :: Ptr SDL_Event -> IO (Maybe SdlEvent)
textEditing p = do
  ee <- peek p.edit
  txt <- fromMaybe "" <$> peekText (getField @"text" ee)
  let int v = fromIntegral v :: Int
  pure (Just (EvEditing txt (int (getField @"start" ee)) (int (getField @"length" ee))))

-- | A button going down or up. SDL numbers the buttons as
-- 'mouseButtonNumber' does, left, middle, right, X1 and X2 from 1, and
-- reports any further button by its number past those.
mouseButton :: Ptr SDL_Event -> Bool -> IO SdlEvent
mouseButton p down = do
  be <- peek p.button
  let btn = mouseButtonNumber (fromIntegral (getField @"button" be))
  EvMouseButton btn down (v2 (getField @"x" be) (getField @"y" be)) <$> peekModifiers

dropEvent :: Ptr SDL_Event -> DropType -> IO (Maybe SdlEvent)
dropEvent p ty = do
  de <- peek p.drop
  let -- A drag entering or finishing has no position.
      pos
        | ty == DropBegin || ty == DropComplete = Nothing
        | otherwise = Just (v2 (getField @"x" de) (getField @"y" de))
  payload <- fromMaybe "" <$> peekText (getField @"data'" de)
  pure (Just (EvDrop (DropEvent ty pos payload)))

-- | The text an event's UTF-8 string holds, or 'Nothing' for a null one.
peekText :: PtrConst.PtrConst CChar -> IO (Maybe Text)
peekText = maybePeek TF.peekCString . PtrConst.unsafeToPtr

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
    EvKey k down mods -> (applyKey k down inp) {inputModifiers = mods}
    EvModifiers mods -> inp {inputModifiers = mods}
    EvText txt mods ->
      inp {inputChars = inputChars inp <> txt, inputModifiers = mods}
    EvMouseMotion pos mods ->
      inp {inputMousePos = pos, inputModifiers = mods}
    EvMouseButton btn down pos mods ->
      (applyMouseButton btn down inp) {inputMousePos = pos, inputModifiers = mods}
    EvMouseLeave -> applyPointerLeave inp
    EvScroll delta -> inp {inputScroll = v2Add (inputScroll inp) delta}
    EvDrop dropEv -> inp {inputDrops = appendDropEvent dropEv (inputDrops inp)}
    EvEditing txt start len -> applyComposition txt start len inp
    EvFocusLost -> releaseAllKeys (applyComposition "" 0 0 inp)
    EvWindowRedraw -> inp {inputWindowRedraw = True}
    -- A wake asks for a frame, not a repaint: the session runs one, and its
    -- damage decides what is presented, if anything.
    EvRefresh -> inp
    EvQuit -> inp
    EvWindowChanged -> inp
    EvSystemThemeChanged -> inp

-- | Whether an event is a button press or release and should end an input batch.
isButtonEdge :: SdlEvent -> Bool
isButtonEdge = \case
  EvMouseButton {} -> True
  _ -> False

-- | What SDL's text input last heard: the widget that had the keyboard, what
-- text input runs for ('Nothing' while it is stopped), and the text input
-- area, in window coordinates.
newtype TextInputSync = TextInputSync (IORef (WidgetId, Maybe InputPurpose, Maybe (SDL_Rect, Int32)))

-- | A sync that has told SDL nothing yet, with text input stopped.
newTextInputSync :: IO TextInputSync
newTextInputSync = TextInputSync <$> newIORef (WidgetId 0, Nothing, Nothing)

-- | Bring SDL's text input up to date after a frame drawn with @inp@ in
-- window @win@, whose window coordinates are @zoom@ layout units. Text input
-- runs while a widget takes text ('textInputArea'), for what it takes
-- ('InputPurpose'), and stops while none does, so no input method composes
-- where nothing shows it and no on-screen keyboard stays up; it restarts
-- when the purpose changes, and when the focus moved while the input method
-- was composing, which drops the composition rather than letting it carry
-- over into the next widget. The input method gets the widget's area, so its
-- candidate window sits by the caret. Says whether it started text input,
-- afresh or again. Every frame the backend draws calls it.
syncTextInput :: TextInputSync -> Ptr SDL_Window -> Float -> Context -> Input -> IO Bool
syncTextInput (TextInputSync ref) win zoom ctx inp = do
  focus <- getFocusId ctx
  area <- textInputArea ctx
  (lastFocus, running, lastArea) <- readIORef ref
  let purpose = textInputAreaPurpose <$> area
      moved = focus /= lastFocus && isJust (inputComposition inp)
      restart = isJust running && isJust purpose && (moved || purpose /= running)
      started = isJust purpose && (isNothing running || restart)
      native = toWindow <$> area
  when (isJust running && (isNothing purpose || restart)) $ void (stopTextInputSafe win)
  when started $ for_ purpose (startTextInput win)
  for_ native $ \(r, cursor) ->
    when (started || native /= lastArea) $
      -- A safe call: the input method may take a round trip to answer.
      with r $ \rp -> void (setTextInputAreaSafe win (PtrConst.unsafeFromPtr rp) cursor)
  writeIORef ref (focus, purpose, native)
  pure started
  where
    toWindow TextInputArea {textInputAreaRect = Rect x y w h, textInputAreaCursor = cursor} =
      let at :: Integral b => Float -> b
          at v = round (v * zoom)
       in (SDL_Rect (at x) (at y) (max 1 (at w)) (max 1 (at h)), at cursor)

-- | Start text input for what a widget takes, which an on-screen keyboard
-- shows the keys for: a password's input method hides what it types, and a
-- number's keyboard has digits.
startTextInput :: Ptr SDL_Window -> InputPurpose -> IO ()
startTextInput win purpose = do
  props <- createPropertiesSafe
  _ <- BS.useAsCString sDL_PROP_TEXTINPUT_TYPE_NUMBER $ \name ->
    setNumberPropertySafe props (PtrConst.unsafeFromPtr name) (fromIntegral textType)
  _ <- startTextInputWithPropertiesSafe win props
  destroyPropertiesSafe props
  where
    SDL_TextInputType textType = case purpose of
      InputNormal -> Keyboard.SDL_TEXTINPUT_TYPE_TEXT
      InputSecure -> Keyboard.SDL_TEXTINPUT_TYPE_TEXT_PASSWORD_HIDDEN
      InputNumeric -> Keyboard.SDL_TEXTINPUT_TYPE_NUMBER
