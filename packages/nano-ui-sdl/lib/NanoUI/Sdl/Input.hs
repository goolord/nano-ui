{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | SDL3 event polling and waiting, and translation of SDL events into
-- 'NanoUI.Input.Input'.
module NanoUI.Sdl.Input
  ( SdlEvent (..)
  , pollEvents
  , waitEvent
  , waitEventTimeout
  , applyEvent
  , isHardQuit
  , isButtonEdge
  ) where

import Data.Bits ((.&.))
import Data.IORef (readIORef)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import Data.Word (Word32)
import Foreign.C.Types (CFloat)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (..))
import GHC.Records.Compat (getField)
import SDL3.Sys.Bindgen.Runtime.CBool qualified as CBool
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import NanoUI
  ( Input (..)
  , Key (..)
  , Modifiers (..)
  , DropEvent (..)
  , DropType (..)
  , V2 (..)
  , appendInputKey
  , v2Add
  )
import NanoUI.Input (MouseButton (..), appendDropEvent, applyMouseButton)
import NanoUI.Sdl.Display (refreshEventType, takeRefreshEvent)
import SDL3.Sys.Bindgen.Events
  ( SDL_Event (..)
  , SDL_EventType (..)
  , SDL_KeyboardEvent
  , data SDL_EVENT_DROP_BEGIN
  , data SDL_EVENT_DROP_COMPLETE
  , data SDL_EVENT_DROP_FILE
  , data SDL_EVENT_DROP_POSITION
  , data SDL_EVENT_DROP_TEXT
  , data SDL_EVENT_KEY_DOWN
  , data SDL_EVENT_MOUSE_BUTTON_DOWN
  , data SDL_EVENT_MOUSE_BUTTON_UP
  , data SDL_EVENT_MOUSE_MOTION
  , data SDL_EVENT_MOUSE_WHEEL
  , data SDL_EVENT_QUIT
  , data SDL_EVENT_TEXT_INPUT
  , data SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED
  , data SDL_EVENT_WINDOW_EXPOSED
  , data SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED
  , data SDL_EVENT_WINDOW_RESIZED
  , data SDL_EVENT_WINDOW_RESTORED
  )
import SDL3.Sys.Events (pollEventSafe, waitEventSafe, waitEventTimeoutSafe)
import SDL3.Sys.Bindgen.Keycode
  ( SDL_Keycode (..)
  , SDL_Keymod (..)
  , sDLK_BACKSPACE
  , sDLK_DELETE
  , sDLK_DOWN
  , sDLK_END
  , sDLK_ESCAPE
  , sDLK_HOME
  , sDLK_LEFT
  , sDLK_RETURN
  , sDLK_RIGHT
  , sDLK_TAB
  , sDLK_UP
  , sDL_KMOD_ALT
  , sDL_KMOD_CTRL
  , sDL_KMOD_SHIFT
  )
import SDL3.Sys.Bindgen.Mouse (sDL_BUTTON_LEFT, sDL_BUTTON_RIGHT)
import SDL3.Sys.Bindgen.Stdinc (Sint32 (..), Uint32 (..))
import SDL3.Sys.Keyboard (getModState)

-- | Copied SDL event data. Pointer positions use SDL window coordinates until
-- display synchronisation converts them to nano-ui's logical coordinates.
data SdlEvent
  = EvQuit
  | EvResize Int Int
  | EvDisplayScale
  | EvKey Key Modifiers
  | EvText Text Modifiers
  | EvMouseMotion V2 Modifiers
  | EvMousePress V2 Modifiers
  | EvMouseRelease V2 Modifiers
  | EvMouseRightPress V2 Modifiers
  | EvMouseRightRelease V2 Modifiers
  | EvScroll V2
  | EvDrop DropEvent
  | EvRefresh
  | EvWindowRedraw
  deriving (Eq, Show)

-- | Drain every pending event, oldest first.
pollEvents :: IO [SdlEvent]
pollEvents =
  alloca $ \(p :: Ptr SDL_Event) -> do
    refreshTy <- readIORef refreshEventType
    let drain acc = do
          got <- pollEventSafe p
          if got
            then decodeEvent refreshTy p >>= \ev -> drain (maybe acc (: acc) ev)
            else pure (reverse acc)
    drain []

-- | Wait for one native event. 'Nothing' means SDL failed to return an event
-- or the decoded event is ignored, so it does not imply a quit request.
waitEvent :: IO (Maybe SdlEvent)
waitEvent =
  alloca $ \p -> do
    got <- waitEventSafe p
    if got then readIORef refreshEventType >>= \ty -> decodeEvent ty p else pure Nothing

-- | Wait up to the supplied milliseconds. 'Nothing' also covers a timeout
-- or ignored event. Call from the display thread.
waitEventTimeout :: Int -> IO (Maybe SdlEvent)
waitEventTimeout ms =
  alloca $ \p -> do
    got <- waitEventTimeoutSafe p (fromIntegral ms)
    if got then readIORef refreshEventType >>= \ty -> decodeEvent ty p else pure Nothing

-- | Translate one SDL event, given the refresh event type; 'Nothing' for
-- events the UI ignores.
decodeEvent :: Word32 -> Ptr SDL_Event -> IO (Maybe SdlEvent)
decodeEvent refreshTy p = do
  Uint32 w <- peek p.type'
  if refreshTy /= 0 && w == refreshTy
    then Just EvRefresh <$ takeRefreshEvent
    else case SDL_EventType (fromIntegral w) of
      SDL_EVENT_QUIT -> pure (Just EvQuit)
      SDL_EVENT_WINDOW_RESIZED -> Just <$> windowResized p
      -- Pixel size changes are ignored here; syncDisplay re-queries logical size.
      SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED -> pure (Just EvDisplayScale)
      SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED -> pure (Just EvDisplayScale)
      -- The window manager damaged our window surface (occlusion, compositor
      -- effects, restore). The backbuffer contents are gone; the next present
      -- must be full or stale regions flash.
      SDL_EVENT_WINDOW_EXPOSED -> pure (Just EvWindowRedraw)
      SDL_EVENT_WINDOW_RESTORED -> pure (Just EvWindowRedraw)
      SDL_EVENT_KEY_DOWN -> keyDown p
      SDL_EVENT_TEXT_INPUT -> textInput p
      SDL_EVENT_MOUSE_MOTION -> Just <$> mouseMotion p
      SDL_EVENT_MOUSE_BUTTON_DOWN -> mouseButton p True
      SDL_EVENT_MOUSE_BUTTON_UP -> mouseButton p False
      SDL_EVENT_MOUSE_WHEEL -> Just <$> mouseWheel p
      SDL_EVENT_DROP_FILE -> Just <$> dropEvent p DropFile
      SDL_EVENT_DROP_TEXT -> Just <$> dropEvent p DropText
      SDL_EVENT_DROP_BEGIN -> Just <$> dropEvent p DropBegin
      SDL_EVENT_DROP_COMPLETE -> Just <$> dropEvent p DropComplete
      SDL_EVENT_DROP_POSITION -> Just <$> dropEvent p DropPosition
      _ -> pure Nothing

keyDown :: Ptr SDL_Event -> IO (Maybe SdlEvent)
keyDown p = do
  ke <- peek p.key
  let mods = keyModifiers ke
      code = fromIntegral (getField @"key" ke :: SDL_Keycode) :: Word32
      repeating = CBool.toBool (getField @"repeat" ke)
  pure $
    case mapSpecialKey code of
      Just k
        | not repeating || isRepeatableKey k -> Just (EvKey k mods)
        | otherwise -> Nothing
      Nothing
        -- Ctrl chords produce no text-input event; report the printable key
        -- symbol (SDL folds Shift into it, so Ctrl+Shift+= arrives as '+').
        | modCtrl mods && code >= 32 && code <= 126 ->
            Just (EvText (T.singleton (toEnum (fromIntegral code))) mods)
        | otherwise -> Nothing

textInput :: Ptr SDL_Event -> IO (Maybe SdlEvent)
textInput p = do
  te <- peek p.text
  mods <- peekModifiers
  let textPtr = PtrConst.unsafeToPtr (getField @"text" te)
  if textPtr == nullPtr
    then pure Nothing
    else do
      txt <- TF.peekCString textPtr
      pure (if T.null txt then Nothing else Just (EvText txt mods))

mouseMotion :: Ptr SDL_Event -> IO SdlEvent
mouseMotion p = do
  me <- peek p.motion
  mods <- peekModifiers
  let x = getField @"x" me :: CFloat
      y = getField @"y" me :: CFloat
  pure (EvMouseMotion (V2 (realToFrac x) (realToFrac y)) mods)

mouseButton :: Ptr SDL_Event -> Bool -> IO (Maybe SdlEvent)
mouseButton p down = do
  be <- peek p.button
  mods <- peekModifiers
  let x = getField @"x" be :: CFloat
      y = getField @"y" be :: CFloat
      pos = V2 (realToFrac x) (realToFrac y)
      btn = getField @"button" be
  pure $
    if btn == fromIntegral sDL_BUTTON_LEFT
      then Just (if down then EvMousePress pos mods else EvMouseRelease pos mods)
      else
        if btn == fromIntegral sDL_BUTTON_RIGHT
          then Just (if down then EvMouseRightPress pos mods else EvMouseRightRelease pos mods)
          else Nothing

mouseWheel :: Ptr SDL_Event -> IO SdlEvent
mouseWheel p = do
  we <- peek p.wheel
  let x = getField @"x" we :: CFloat
      y = getField @"y" we :: CFloat
  pure (EvScroll (V2 (realToFrac x) (negate (realToFrac y))))

windowResized :: Ptr SDL_Event -> IO SdlEvent
windowResized p = do
  we <- peek p.window
  let Sint32 w = getField @"data1" we
      Sint32 h = getField @"data2" we
  pure (EvResize (fromIntegral w) (fromIntegral h))

dropEvent :: Ptr SDL_Event -> DropType -> IO SdlEvent
dropEvent p ty = do
  de <- peek p.drop
  let x = getField @"x" de :: CFloat
      y = getField @"y" de :: CFloat
      at = Just (V2 (realToFrac x) (realToFrac y))
      pos =
        case ty of
          DropPosition -> at
          DropFile -> at
          DropText -> at
          _ -> Nothing
      dataPtr = PtrConst.unsafeToPtr (getField @"data'" de)
  payload <-
    if dataPtr == nullPtr
      then pure ""
      else TF.peekCString dataPtr
  pure (EvDrop (DropEvent ty pos payload))

peekModifiers :: IO Modifiers
peekModifiers = modFromKeymod <$> getModState

modFromKeymod :: SDL_Keymod -> Modifiers
modFromKeymod km =
  let m = word32 km
   in Modifiers
        { modShift = m .&. word32 sDL_KMOD_SHIFT /= 0
        , modCtrl = m .&. word32 sDL_KMOD_CTRL /= 0
        , modAlt = m .&. word32 sDL_KMOD_ALT /= 0
        }

keyModifiers :: SDL_KeyboardEvent -> Modifiers
keyModifiers ke = modFromKeymod (getField @"mod" ke)

word32 :: Integral a => a -> Word32
word32 = fromIntegral

mapSpecialKey :: Word32 -> Maybe Key
mapSpecialKey k
  | k == word32 sDLK_ESCAPE = Just KeyEscape
  | k == word32 sDLK_RETURN = Just KeyEnter
  | k == word32 sDLK_TAB = Just KeyTab
  | k == word32 sDLK_BACKSPACE = Just KeyBackspace
  | k == word32 sDLK_DELETE = Just KeyDelete
  | k == word32 sDLK_LEFT = Just KeyLeft
  | k == word32 sDLK_RIGHT = Just KeyRight
  | k == word32 sDLK_UP = Just KeyUp
  | k == word32 sDLK_DOWN = Just KeyDown
  | k == word32 sDLK_HOME = Just KeyHome
  | k == word32 sDLK_END = Just KeyEnd
  | otherwise = Nothing

isRepeatableKey :: Key -> Bool
isRepeatableKey KeyBackspace = True
isRepeatableKey KeyDelete = True
isRepeatableKey KeyLeft = True
isRepeatableKey KeyRight = True
isRepeatableKey KeyUp = True
isRepeatableKey KeyDown = True
isRepeatableKey KeyHome = True
isRepeatableKey KeyEnd = True
isRepeatableKey _ = False

-- | Accumulate one event into a frame's input. Resize, scale, refresh, and quit
-- need session handling; this does not update size or convert coordinates.
applyEvent :: Input -> SdlEvent -> Input
applyEvent inp ev =
  case ev of
    EvQuit -> inp
    EvDisplayScale -> inp
    EvResize _ _ -> inp
    EvKey k mods -> inp {inputKeys = appendInputKey k (inputKeys inp), inputModifiers = mods}
    EvText txt mods ->
      inp {inputChars = inputChars inp <> txt, inputModifiers = mods}
    EvMouseMotion pos mods ->
      inp {inputMousePos = pos, inputModifiers = mods}
    EvMousePress pos mods ->
      (applyMouseButton MouseLeft True inp) {inputMousePos = pos, inputModifiers = mods}
    EvMouseRelease pos mods ->
      (applyMouseButton MouseLeft False inp) {inputMousePos = pos, inputModifiers = mods}
    EvMouseRightPress pos mods ->
      (applyMouseButton MouseRight True inp) {inputMousePos = pos, inputModifiers = mods}
    EvMouseRightRelease pos mods ->
      (applyMouseButton MouseRight False inp) {inputMousePos = pos, inputModifiers = mods}
    EvScroll delta -> inp {inputScroll = v2Add (inputScroll inp) delta}
    EvDrop dropEv -> inp {inputDrops = appendDropEvent dropEv (inputDrops inp)}
    -- A wake asks for a frame, not a repaint: the session runs one, and its
    -- damage decides what is presented, if anything.
    EvRefresh -> inp
    EvWindowRedraw -> inp {inputWindowRedraw = True}

-- | Whether an event is a left/right press or release and should end an input batch.
isButtonEdge :: SdlEvent -> Bool
isButtonEdge ev =
  case ev of
    EvMousePress _ _ -> True
    EvMouseRelease _ _ -> True
    EvMouseRightPress _ _ -> True
    EvMouseRightRelease _ _ -> True
    _ -> False

-- | Ctrl+C text or an ETX character, independent of popup dismissal.
isHardQuit :: SdlEvent -> Bool
isHardQuit ev =
  case ev of
    EvText txt mods -> (txt == "c" && modCtrl mods) || txt == "\ETX"
    _ -> False
