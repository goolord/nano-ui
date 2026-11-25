{-# OPTIONS_HADDOCK not-home #-}

-- | Browser events, as @web/nano-ui-sdl-web.js@ queues them, decoded and
-- folded into 'Input'.
--
-- Each event is an 'eventRecordSize'-byte record: kind, three integer
-- fields (@a@, @b@, @c@) and the modifier bits as 32-bit integers, then
-- three 32-bit floats (@x@, @y@, @z@). The kinds and their fields:
--
-- *  1 resize: the canvas's size or pixel ratio changed (read at sync)
-- *  2 motion: @x@, @y@ in CSS pixels
-- *  3 button: @a@ the DOM button number, @b@ 1 down or 0 up
-- *  4 leave: the pointer left the canvas
-- *  5 scroll: @x@, @y@ in wheel notches, positive right and down
-- *  6 key: @a@ the key ('decodeKey'), @b@ 1 down or 0 up
-- *  7 character typed: @a@ the code point
-- *  8 composition: @a@ characters to follow as kind 9, @b@ the caret,
--    @c@ the selection's length
-- *  9 composition character: @a@ the code point
-- * 10 focus lost
-- * 11 redraw: the drawing buffer was cleared or the GL context restored
--
-- The modifier bits are 1 Shift, 2 Ctrl, 4 Alt, 8 Meta and 16 Num Lock.
module NanoUI.SdlWeb.Internal.Input
  ( WebEvent (..)
  , decodeEvents
  , PartialComposition
  , decodeEventBatch
  , finishComposition
  , decodeKey
  , applyWebEvent
  , isButtonEdge
  , cssCursor
  ) where

import Data.Bits ((.&.))
import Data.Char (chr, isControl, isUpper, toLower)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekByteOff)
import NanoUI (V2 (..), v2Add)
import NanoUI.Backend
  ( Input (..)
  , Key (..)
  , Modifiers
  , UiCursorKind (..)
  , applyComposition
  , applyKey
  , applyMouseButton
  , applyPointerLeave
  , keypadKey
  , modifiersFromBits
  , mouseButtonNumber
  , releaseAllKeys
  )
import NanoUI.SdlWeb.Internal.Host (eventRecordSize)

-- | A decoded browser event. Positions are in layout units.
data WebEvent
  = EvResize
  | EvMotion !Float !Float !Modifiers
  | EvButton !Int !Bool !Modifiers
  | EvLeave
  | EvScroll !Float !Float
  | EvKey !(Maybe Key) !Bool !Modifiers
  -- ^ 'Nothing' for a modifier key, which changes only the modifiers.
  | EvChar !Char
  | EvComposition !Text !Int !Int
  | EvFocusLost
  | EvRedraw
  deriving (Eq, Show)

-- | Decode @n@ records from a buffer. CSS pixels become layout units at the
-- zoom: the UI scale on top of the device pixel ratio. A composition whose
-- characters run past the buffer ends with those it has; a caller reading
-- the queue in batches uses 'decodeEventBatch' to carry it on.
decodeEvents :: Float -> Ptr Word8 -> Int -> IO [WebEvent]
decodeEvents zoom buf n = do
  (evs, partial) <- decodeEventBatch zoom Nothing buf n
  pure (evs ++ maybe [] (pure . finishComposition) partial)

-- | A composition (kind 8) whose characters (kind 9) run past the end of a
-- batch: how many are still to come, those read so far, newest first, and
-- the caret and selection length.
data PartialComposition = PartialComposition !Int ![Char] !Int !Int
  deriving (Eq, Show)

-- | The composition event for what has arrived of one.
finishComposition :: PartialComposition -> WebEvent
finishComposition (PartialComposition _ chars caret len) = EvComposition (T.pack (reverse chars)) caret len

-- | 'decodeEvents' for one batch of a queue read in batches: it continues a
-- composition the batch before left unfinished, and answers one this batch
-- leaves unfinished, for the next.
decodeEventBatch :: Float -> Maybe PartialComposition -> Ptr Word8 -> Int -> IO ([WebEvent], Maybe PartialComposition)
decodeEventBatch zoom carried buf n = case carried of
  Nothing -> go 0
  Just partial -> composing partial 0
  where
    at i k = fromIntegral <$> (peekByteOff buf (i * eventRecordSize + k * 4) :: IO Int32)
    -- Take a composition's characters from record @i@ on, as far as they
    -- go in this batch. A record of another kind ends it early.
    composing p@(PartialComposition left chars caret len) i
      | left <= 0 = first (finishComposition p :) <$> go i
      | i >= n = pure ([], Just p)
      | otherwise = do
          kind <- at i 0
          if kind /= (9 :: Int)
            then first (finishComposition p :) <$> go i
            else do
              ch <- chr . clampChar <$> at i 1
              composing (PartialComposition (left - 1) (ch : chars) caret len) (i + 1)
    first f (a, b) = (f a, b)
    go i
      | i >= n = pure ([], Nothing)
      | otherwise = do
          let float k = peekByteOff buf (i * eventRecordSize + k * 4) :: IO Float
              next ev = first (ev :) <$> go (i + 1)
          kind <- at i 0
          a <- at i 1
          b <- at i 2
          c <- at i 3
          mods <- (\m -> modifiersFromBits (m :: Int) 1 2 4 8) <$> at i 4
          numLock <- (\m -> m .&. (16 :: Int) /= 0) <$> at i 4
          x <- float 5
          y <- float 6
          case kind :: Int of
            1 -> next EvResize
            2 -> next (EvMotion (x / zoom) (y / zoom) mods)
            3 -> next (EvButton a (b /= 0) mods)
            4 -> next EvLeave
            5 -> next (EvScroll x y)
            6 -> next (EvKey (decodeKey numLock a) (b /= 0) mods)
            7
              | typed a -> next (EvChar (chr a))
              | otherwise -> go (i + 1)
            -- The composition's characters follow as kind 9 records.
            8 -> composing (PartialComposition (max 0 a) [] b c) (i + 1)
            10 -> next EvFocusLost
            11 -> next EvRedraw
            _ -> go (i + 1)
    clampChar v = if v < 0 || v > 0x10FFFF then 0xFFFD else v
    -- Control characters type nothing: a chord's key event reports it.
    typed v = v > 0 && v <= 0x10FFFF && not (isControl (chr v))

-- | The key a key record names. A positive code is a key that types that
-- character (lowercase); a negative one a named key: -1 to -21 in the order
-- of 'Key' from 'KeyBackspace' to 'KeyMenu', -(100 + n) for F@n@, and
-- -(300 + c) for the keypad key that types @c@ under Num Lock. Zero is a
-- modifier key.
decodeKey :: Bool -> Int -> Maybe Key
decodeKey numLock code
  | code > 0 = Just (KeyChar (lowerChar (chr (min 0x10FFFF code))))
  | code <= -300 = keypadKey numLock (chr (negate code - 300))
  | code <= -101 && code >= -124 = Just (KeyF (negate code - 100))
  | code < 0 && code >= -21 = Just (named !! (negate code - 1))
  | otherwise = Nothing
  where
    lowerChar ch = if isUpper ch then toLower ch else ch
    named =
      [ KeyBackspace, KeyDelete, KeyEnter, KeyEscape, KeyTab
      , KeyLeft, KeyRight, KeyUp, KeyDown, KeyHome, KeyEnd, KeyPageUp, KeyPageDown
      , KeyInsert, KeySpace, KeyPrintScreen, KeyPause, KeyCapsLock, KeyNumLock
      , KeyScrollLock, KeyMenu
      ]

-- | Fold a decoded event into the frame's input. Resize events change
-- nothing here: the sync reads the canvas.
applyWebEvent :: Input -> WebEvent -> Input
applyWebEvent inp = \case
  EvResize -> inp
  EvMotion x y mods -> inp {inputMousePos = V2 x y, inputModifiers = mods}
  -- DOM buttons are numbered from 0 in 'mouseButtonNumber' order: left,
  -- middle, right, back, forward.
  EvButton btn down mods -> (applyMouseButton (mouseButtonNumber (btn + 1)) down inp) {inputModifiers = mods}
  -- A drag keeps its pointer ('applyPointerLeave').
  EvLeave -> applyPointerLeave inp
  EvScroll dx dy -> inp {inputScroll = v2Add (inputScroll inp) (V2 dx dy)}
  -- 'applyKey' tells auto-repeats from presses because the key is held.
  EvKey key down mods -> (maybe inp (\k -> applyKey k down inp) key) {inputModifiers = mods}
  EvChar ch -> inp {inputChars = T.snoc (inputChars inp) ch}
  EvComposition txt caret len -> applyComposition txt caret len inp
  EvFocusLost -> releaseAllKeys inp
  EvRedraw -> inp {inputWindowRedraw = True}

-- | Presses and releases end a frame's batch of events, so each frame sees
-- at most one edge per button.
isButtonEdge :: WebEvent -> Bool
isButtonEdge = \case EvButton {} -> True; _ -> False

-- | The CSS cursor for a cursor kind. Browsers have every kind, so none
-- falls back: each constructor's name after @UiCursor@ in kebab case is its
-- CSS name ('UiCursorNwseResize' is @nwse-resize@), and 'UiCursorHidden'
-- is @none@.
cssCursor :: UiCursorKind -> Text
cssCursor UiCursorHidden = "none"
cssCursor kind = T.pack (kebab (drop (length ("UiCursor" :: String)) (show kind)))
  where
    kebab [] = []
    kebab (c : cs) = toLower c : concatMap (\ch -> if isUpper ch then ['-', toLower ch] else [ch]) cs
