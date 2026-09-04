{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module NanoUI.Sdl.Input
  ( SdlEvent (..)
  , pollEvents
  , waitEvent
  , waitEventTimeout
  , applyEvent
  , clearEphemeral
  , isHardQuit
  , isHardQuitInput
  , isButtonEdge
  , splitFrame
  ) where

import Data.Bits ((.&.))
import Data.Primitive.SmallArray
  ( SmallArray
  , emptySmallArray
  , indexSmallArray
  , sizeofSmallArray
  , smallArrayFromListN
  )
import qualified Data.Text as T
import Data.Text (Text)
import Data.Word (Word32)
import Foreign.C.String (peekCString)
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
  , V2 (..)
  , appendInputKey
  , v2Add
  )
import NanoUI.Input (clearEphemeral, isHardQuitInput)
import qualified NanoUI.Input as Inp
import NanoUI.Sdl.Display (readRefreshEventType)
import SDL3.Sys.Bindgen.Events
  ( SDL_Event (..)
  , SDL_KeyboardEvent
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
import SDL3.Sys.Keyboard (getModStateSafe)

data SdlEvent
  = EvQuit
  | EvResize Int Int
  | EvDisplayScale
  | EvKey Key Modifiers
  | EvText Text Modifiers
  | EvMouseMotion V2 Modifiers
  | EvMousePress V2 Modifiers Int
  | EvMouseRelease V2 Modifiers
  | EvMouseRightPress V2 Modifiers
  | EvMouseRightRelease V2 Modifiers
  | EvScroll V2
  | EvRefresh
  deriving (Eq, Show)

singletonEv :: SdlEvent -> SmallArray SdlEvent
singletonEv ev = smallArrayFromListN 1 [ev]

pollEvents :: IO (SmallArray SdlEvent)
pollEvents = drain emptySmallArray
  where
    drain acc =
      alloca $ \(p :: Ptr SDL_Event) -> do
        got <- pollEventSafe p
        if got
          then do
            evs <- decodeEvent p
            if sizeofSmallArray evs == 0
              then drain acc
              else drain (acc <> evs)
          else pure acc

waitEvent :: IO (SmallArray SdlEvent)
waitEvent =
  alloca $ \(p :: Ptr SDL_Event) -> do
    got <- waitEventSafe p
    if got
      then decodeEvent p
      else pure emptySmallArray

waitEventTimeout :: Int -> IO (SmallArray SdlEvent)
waitEventTimeout ms =
  alloca $ \(p :: Ptr SDL_Event) -> do
    got <- waitEventTimeoutSafe p (fromIntegral ms)
    if got
      then decodeEvent p
      else pure emptySmallArray

decodeEvent :: Ptr SDL_Event -> IO (SmallArray SdlEvent)
decodeEvent p = do
  Uint32 w <- peek p.type'
  refreshTy <- readRefreshEventType
  if refreshTy /= 0 && w == refreshTy
    then pure (singletonEv EvRefresh)
    else decodeEventType p w
  where
    decodeEventType p' w' =
      case w' of
        256 -> pure (singletonEv EvQuit)
        518 -> windowResized p'
        -- Pixel size changes are ignored here; syncDisplay re-queries logical size.
        519 -> pure (singletonEv EvDisplayScale)
        532 -> pure (singletonEv EvDisplayScale)
        768 -> keyDown p'
        771 -> textInput p'
        1024 -> mouseMotion p'
        1025 -> mouseButton p' True
        1026 -> mouseButton p' False
        1027 -> mouseWheel p'
        _ -> pure emptySmallArray

keyDown :: Ptr SDL_Event -> IO (SmallArray SdlEvent)
keyDown p = do
  ke <- peek p.key
  let mods = keyModifiers ke
      code = fromIntegral (getField @"key" ke :: SDL_Keycode) :: Word32
      repeating = CBool.toBool (getField @"repeat" ke)
  pure $
    case mapSpecialKey code of
      Just k ->
        if not repeating || isRepeatableKey k
          then singletonEv (EvKey k mods)
          else emptySmallArray
      Nothing
        | modCtrl mods && code == sdlCtrlAKeycode -> singletonEv (EvText "a" mods)
        | modCtrl mods && code == sdlCtrlCKeycode -> singletonEv (EvText "c" mods)
        | modCtrl mods && code == sdlCtrlVKeycode -> singletonEv (EvText "v" mods)
        | modCtrl mods && code == sdlCtrlXKeycode -> singletonEv (EvText "x" mods)
        | otherwise -> emptySmallArray

textInput :: Ptr SDL_Event -> IO (SmallArray SdlEvent)
textInput p = do
  te <- peek p.text
  mods <- peekModifiers
  let textPtr = PtrConst.unsafeToPtr (getField @"text" te)
  if textPtr == nullPtr
    then pure emptySmallArray
    else do
      str <- peekCString textPtr
      if null str then pure emptySmallArray else pure (singletonEv (EvText (T.pack str) mods))

mouseMotion :: Ptr SDL_Event -> IO (SmallArray SdlEvent)
mouseMotion p = do
  me <- peek p.motion
  mods <- peekModifiers
  let x = getField @"x" me :: CFloat
      y = getField @"y" me :: CFloat
  pure (singletonEv (EvMouseMotion (mousePos (realToFrac x) (realToFrac y)) mods))

mouseButton :: Ptr SDL_Event -> Bool -> IO (SmallArray SdlEvent)
mouseButton p down = do
  be <- peek p.button
  mods <- peekModifiers
  let x = getField @"x" be :: CFloat
      y = getField @"y" be :: CFloat
      pos = mousePos (realToFrac x) (realToFrac y)
      btn = getField @"button" be
      clicks = fromIntegral (getField @"clicks" be) :: Int
  if btn == fromIntegral sDL_BUTTON_LEFT
    then pure (singletonEv (if down then EvMousePress pos mods (max 1 clicks) else EvMouseRelease pos mods))
    else
      if btn == fromIntegral sDL_BUTTON_RIGHT
        then pure (singletonEv (if down then EvMouseRightPress pos mods else EvMouseRightRelease pos mods))
        else pure emptySmallArray

mouseWheel :: Ptr SDL_Event -> IO (SmallArray SdlEvent)
mouseWheel p = do
  we <- peek p.wheel
  let x = getField @"x" we :: CFloat
      y = getField @"y" we :: CFloat
  pure (singletonEv (EvScroll (V2 (realToFrac x) (negate (realToFrac y)))))

windowResized :: Ptr SDL_Event -> IO (SmallArray SdlEvent)
windowResized p = do
  we <- peek p.window
  let Sint32 w = getField @"data1" we
      Sint32 h = getField @"data2" we
  pure (singletonEv (EvResize (fromIntegral w) (fromIntegral h)))

peekModifiers :: IO Modifiers
peekModifiers = modFromKeymod <$> getModStateSafe

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

sdlCtrlAKeycode, sdlCtrlCKeycode, sdlCtrlVKeycode, sdlCtrlXKeycode :: Word32
sdlCtrlAKeycode = 97
sdlCtrlCKeycode = 99
sdlCtrlVKeycode = 118
sdlCtrlXKeycode = 120

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

mousePos :: Float -> Float -> V2
mousePos x y = V2 x y

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
    EvMousePress pos mods clicks ->
      inp
        { inputMousePos = pos
        , inputModifiers = mods
        , inputMouseDown = True
        , inputMousePressed = True
        , inputMouseClicks = max 1 clicks
        }
    EvMouseRelease pos mods ->
      inp
        { inputMousePos = pos
        , inputModifiers = mods
        , inputMouseDown = False
        , inputMouseReleased = True
        }
    EvMouseRightPress pos mods ->
      inp
        { inputMousePos = pos
        , inputModifiers = mods
        , inputMouseRightDown = True
        , inputMouseRightPressed = True
        }
    EvMouseRightRelease pos mods ->
      inp
        { inputMousePos = pos
        , inputModifiers = mods
        , inputMouseRightDown = False
        , inputMouseRightReleased = True
        }
    EvScroll delta -> inp {inputScroll = v2Add (inputScroll inp) delta}
    EvRefresh -> inp

isButtonEdge :: SdlEvent -> Bool
isButtonEdge ev =
  case ev of
    EvMousePress {} -> True
    EvMouseRelease _ _ -> True
    EvMouseRightPress _ _ -> True
    EvMouseRightRelease _ _ -> True
    _ -> False

splitFrame :: SmallArray SdlEvent -> (SmallArray SdlEvent, SmallArray SdlEvent)
splitFrame events =
  let len = sizeofSmallArray events
      lst = [indexSmallArray events i | i <- [0 .. len - 1]]
      (group, rest) = Inp.splitFrame isButtonEdge lst
   in ( smallArrayFromListN (length group) group
      , smallArrayFromListN (length rest) rest
      )

isHardQuit :: SdlEvent -> Bool
isHardQuit ev =
  case ev of
    EvText txt mods -> (txt == "c" && modCtrl mods) || txt == "\ETX"
    _ -> False
