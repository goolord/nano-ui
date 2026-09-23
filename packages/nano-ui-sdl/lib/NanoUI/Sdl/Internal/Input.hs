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
  ) where

import Control.Monad (mfilter)
import Data.Bits ((.&.))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Foreign as TF
import Data.Word (Word32)
import Foreign.C.Types (CFloat)
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
      SDL_EVENT_QUIT -> pure (Just EvQuit)
      SDL_EVENT_WINDOW_RESIZED -> pure (Just EvWindowChanged)
      SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED -> pure (Just EvWindowChanged)
      SDL_EVENT_WINDOW_DISPLAY_SCALE_CHANGED -> pure (Just EvWindowChanged)
      -- The window manager damaged our window surface (occlusion, compositor
      -- effects, restore). The backbuffer contents are gone; the next present
      -- must be full or stale regions flash.
      SDL_EVENT_WINDOW_EXPOSED -> pure (Just EvWindowRedraw)
      SDL_EVENT_WINDOW_RESTORED -> pure (Just EvWindowRedraw)
      SDL_EVENT_KEY_DOWN -> keyDown <$> peek p.key
      SDL_EVENT_TEXT_INPUT -> textInput p
      SDL_EVENT_MOUSE_MOTION -> do
        me <- peek p.motion
        Just . EvMouseMotion (v2 (getField @"x" me) (getField @"y" me)) <$> peekModifiers
      SDL_EVENT_MOUSE_BUTTON_DOWN -> mouseButton p True
      SDL_EVENT_MOUSE_BUTTON_UP -> mouseButton p False
      SDL_EVENT_MOUSE_WHEEL -> do
        we <- peek p.wheel
        pure (Just (EvScroll (v2 (getField @"x" we) (negate (getField @"y" we)))))
      SDL_EVENT_DROP_FILE -> dropEvent p DropFile
      SDL_EVENT_DROP_TEXT -> dropEvent p DropText
      SDL_EVENT_DROP_BEGIN -> dropEvent p DropBegin
      SDL_EVENT_DROP_COMPLETE -> dropEvent p DropComplete
      SDL_EVENT_DROP_POSITION -> dropEvent p DropPosition
      _ -> pure Nothing

v2 :: CFloat -> CFloat -> V2
v2 x y = V2 (realToFrac x) (realToFrac y)

keyDown :: SDL_KeyboardEvent -> Maybe SdlEvent
keyDown ke =
  case lookup code specialKeys of
    Just (k, repeatable)
      | repeatable || not (CBool.toBool (getField @"repeat" ke)) -> Just (EvKey k mods)
      | otherwise -> Nothing
    Nothing
      -- Ctrl chords produce no text-input event; report the printable key
      -- symbol (SDL folds Shift into it, so Ctrl+Shift+= arrives as '+').
      | modCtrl mods && code >= 32 && code <= 126 ->
          Just (EvText (T.singleton (toEnum (fromIntegral code))) mods)
      | otherwise -> Nothing
  where
    mods = modFromKeymod (getField @"mod" ke)
    code = fromIntegral (getField @"key" ke :: SDL_Keycode) :: Word32

-- | The keys the UI takes by name, and whether holding one down repeats it.
specialKeys :: [(Word32, (Key, Bool))]
specialKeys =
  [ (word32 sDLK_ESCAPE, (KeyEscape, False))
  , (word32 sDLK_RETURN, (KeyEnter, False))
  , (word32 sDLK_TAB, (KeyTab, False))
  , (word32 sDLK_BACKSPACE, (KeyBackspace, True))
  , (word32 sDLK_DELETE, (KeyDelete, True))
  , (word32 sDLK_LEFT, (KeyLeft, True))
  , (word32 sDLK_RIGHT, (KeyRight, True))
  , (word32 sDLK_UP, (KeyUp, True))
  , (word32 sDLK_DOWN, (KeyDown, True))
  , (word32 sDLK_HOME, (KeyHome, True))
  , (word32 sDLK_END, (KeyEnd, True))
  ]

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
  pure (press <$> lookup btn [(sDL_BUTTON_LEFT, MouseLeft), (sDL_BUTTON_RIGHT, MouseRight)])

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
modFromKeymod km =
  let m = word32 km
   in Modifiers
        { modShift = m .&. word32 sDL_KMOD_SHIFT /= 0
        , modCtrl = m .&. word32 sDL_KMOD_CTRL /= 0
        , modAlt = m .&. word32 sDL_KMOD_ALT /= 0
        }

word32 :: Integral a => a -> Word32
word32 = fromIntegral

-- | Accumulate one event into a frame's input. Window changes and quits need
-- session handling; this does not update size or convert coordinates.
applyEvent :: Input -> SdlEvent -> Input
applyEvent inp ev =
  case ev of
    EvKey k mods -> inp {inputKeys = appendInputKey k (inputKeys inp), inputModifiers = mods}
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
