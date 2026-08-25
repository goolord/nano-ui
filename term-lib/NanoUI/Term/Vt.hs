-- | Incremental decoder for VT input byte streams, used by the POSIX driver.
--
-- Terminal input arrives in arbitrarily sized reads, so 'decode' returns the
-- bytes it could not yet interpret and the caller prepends them to the next
-- read. A lone @ESC@ is inherently ambiguous (Escape key, or the start of a
-- sequence that has not arrived yet), so it is held back as leftover and only
-- turned into a key by 'flushPending' once the input has gone idle.
--
-- Unlike vty's parser, an unrecognised sequence consumes only its own bytes;
-- it never discards the rest of the buffer, so a stray report can't swallow a
-- click that arrived in the same read.
module NanoUI.Term.Vt
  ( decode
  , flushPending
  , enableMouse
  , disableMouse
  ) where

import Data.Bits ((.&.), (.|.), shiftL, testBit)
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Word (Word8)
import NanoUI (Key (..), Modifiers (..))
import NanoUI.Term.Event
  ( MouseAction (..)
  , MouseBtn (..)
  , TermEvent (..)
  , noMods
  )
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

-- | Ask the terminal for SGR-encoded any-motion mouse reporting. 1000 is
-- press/release, 1002 adds drag, 1003 adds motion with no button held (hover),
-- and 1006 switches to the SGR encoding so coordinates are not capped at 223.
enableMouse :: ByteString
enableMouse = BS8.pack "\ESC[?1000h\ESC[?1002h\ESC[?1003h\ESC[?1006h"

disableMouse :: ByteString
disableMouse = BS8.pack "\ESC[?1006l\ESC[?1003l\ESC[?1002l\ESC[?1000l"

data Step
  = -- | Not enough bytes yet; wait for more input.
    Incomplete
  | -- | Recognised but not mapped to an event; consume this many bytes.
    Skip !Int
  | Emit !TermEvent !Int

-- | Decode as many events as possible, returning the undecoded tail.
decode :: ByteString -> ([TermEvent], ByteString)
decode = go []
  where
    go acc bs
      | BS.null bs = (reverse acc, BS.empty)
      | otherwise =
          case step bs of
            Incomplete -> (reverse acc, bs)
            Skip n -> go acc (BS.drop n bs)
            Emit ev n -> go (ev : acc) (BS.drop n bs)

-- | Interpret leftover bytes that will never be completed because input went
-- idle. Only a lone @ESC@ carries meaning; anything else is malformed.
flushPending :: ByteString -> [TermEvent]
flushPending bs
  | bs == BS.singleton 0x1b = [EvKey KeyEscape noMods]
  | otherwise = []

step :: ByteString -> Step
step bs =
  case BS.head bs of
    0x1b -> escape bs
    0x0d -> Emit (EvKey KeyEnter noMods) 1
    0x0a -> Emit (EvKey KeyEnter noMods) 1
    0x09 -> Emit (EvKey KeyTab noMods) 1
    0x08 -> Emit (EvKey KeyBackspace noMods) 1
    0x7f -> Emit (EvKey KeyBackspace noMods) 1
    b
      | b >= 0x01 && b <= 0x1a -> Emit (EvChar (chr (fromIntegral b + 96)) ctrlOnly) 1
      | b < 0x20 -> Skip 1
      | b < 0x80 -> Emit (EvChar (chr (fromIntegral b)) noMods) 1
      | otherwise -> utf8 bs
  where
    ctrlOnly = Modifiers False True False

escape :: ByteString -> Step
escape bs
  | BS.length bs < 2 = Incomplete
  | otherwise =
      case BS.index bs 1 of
        0x5b -> csi bs
        0x4f -> ss3 bs
        0x1b -> Emit (EvKey KeyEscape noMods) 1
        b
          | b >= 0x20 && b < 0x7f ->
              Emit (EvChar (chr (fromIntegral b)) altOnly) 2
          | otherwise -> Skip 2
  where
    altOnly = Modifiers False False True

-- | @ESC [ params final@, where parameter and intermediate bytes are
-- 0x20..0x3f and the final byte is 0x40..0x7e.
csi :: ByteString -> Step
csi bs =
  let body = BS.drop 2 bs
      (params, rest) = BS.span (\c -> c >= 0x20 && c < 0x40) body
   in if BS.null rest
        then Incomplete
        else
          let final = BS.head rest
              consumed = 2 + BS.length params + 1
           in if not (BS.null params) && BS.head params == 0x3c
                then sgrMouse (BS.drop 1 params) final consumed
                else
                  if BS.null params && final == 0x4d
                    then x10Mouse bs
                    else csiKey params final consumed

-- | SGR mouse: @ESC [ < code ; col ; row (M|m)@, coordinates one-based.
sgrMouse :: ByteString -> Word8 -> Int -> Step
sgrMouse params final consumed =
  case map BS8.readInt (BS8.split ';' params) of
    [Just (code, _), Just (col, _), Just (row, _)]
      | testBit code 6 && code .&. 3 >= 2 -> Skip consumed
      | otherwise ->
          Emit (mouseEvent code (col - 1) (row - 1) (final == 0x4d)) consumed
    _ -> Skip consumed

-- | X10 mouse: @ESC [ M code col row@, each a byte biased by 32. Only reached
-- if the terminal ignored the SGR request; coordinates above 223 cannot be
-- represented in this encoding.
x10Mouse :: ByteString -> Step
x10Mouse bs
  | BS.length bs < 6 = Incomplete
  | otherwise =
      let code = fromIntegral (BS.index bs 3) - 32
          col = fromIntegral (BS.index bs 4) - 32 - 1
          row = fromIntegral (BS.index bs 5) - 32 - 1
          -- X10 has no separate release code: button bits 3 means "released".
          press = code .&. 3 /= 3 || code .&. 32 /= 0
       in Emit (mouseEvent code col row press) 6

-- | Shared decoding of the button/modifier bitfield used by both encodings.
mouseEvent :: Int -> Int -> Int -> Bool -> TermEvent
mouseEvent code col row press = EvMouse action col row mods
  where
    mods =
      Modifiers
        { modShift = testBit code 2
        , modCtrl = testBit code 4
        , modAlt = testBit code 3
        }
    motion = testBit code 5
    wheel = testBit code 6
    action
      | wheel = if code .&. 1 == 0 then MouseScrollUp else MouseScrollDown
      | not press = MouseRelease (button (code .&. 3))
      | motion = maybe MouseMove MouseDrag (button (code .&. 3))
      | otherwise = maybe MouseMove MousePress (button (code .&. 3))
    button n =
      case n of
        0 -> Just BtnLeft
        1 -> Just BtnMiddle
        2 -> Just BtnRight
        _ -> Nothing

csiKey :: ByteString -> Word8 -> Int -> Step
csiKey params final consumed =
  case final of
    0x41 -> key KeyUp
    0x42 -> key KeyDown
    0x43 -> key KeyRight
    0x44 -> key KeyLeft
    0x48 -> key KeyHome
    0x46 -> key KeyEnd
    0x5a -> Emit (EvKey KeyTab (Modifiers True False False)) consumed
    0x7e ->
      case leadingInt params of
        Just 1 -> key KeyHome
        Just 3 -> key KeyDelete
        Just 4 -> key KeyEnd
        Just 7 -> key KeyHome
        Just 8 -> key KeyEnd
        _ -> Skip consumed
    _ -> Skip consumed
  where
    key k = Emit (EvKey k (csiMods params)) consumed

ss3 :: ByteString -> Step
ss3 bs
  | BS.length bs < 3 = Incomplete
  | otherwise =
      case BS.index bs 2 of
        0x41 -> key KeyUp
        0x42 -> key KeyDown
        0x43 -> key KeyRight
        0x44 -> key KeyLeft
        0x48 -> key KeyHome
        0x46 -> key KeyEnd
        _ -> Skip 3
  where
    key k = Emit (EvKey k noMods) 3

-- | @ESC [ 1 ; 5 A@ style modifiers: the second parameter is 1 plus a bitmask
-- of shift (1), alt (2) and ctrl (4).
csiMods :: ByteString -> Modifiers
csiMods params =
  case BS8.split ';' params of
    [_, m] ->
      case BS8.readInt m of
        Just (v, _) ->
          let bits = v - 1
           in Modifiers
                { modShift = bits .&. 1 /= 0
                , modAlt = bits .&. 2 /= 0
                , modCtrl = bits .&. 4 /= 0
                }
        Nothing -> noMods
    _ -> noMods

leadingInt :: ByteString -> Maybe Int
leadingInt params = fst <$> BS8.readInt params

utf8 :: ByteString -> Step
utf8 bs =
  let b0 = BS.head bs
      len
        | b0 .&. 0xe0 == 0xc0 = 2
        | b0 .&. 0xf0 == 0xe0 = 3
        | b0 .&. 0xf8 == 0xf0 = 4
        | otherwise = 0
   in if len == 0
        then Skip 1
        else
          if BS.length bs < len
            then Incomplete
            else
              let payload = BS.take len bs
                  initial =
                    case len of
                      2 -> fromIntegral (b0 .&. 0x1f)
                      3 -> fromIntegral (b0 .&. 0x0f)
                      _ -> fromIntegral (b0 .&. 0x07)
                  cp = BS.foldl' addByte initial (BS.drop 1 payload)
                  addByte acc b = (acc `shiftL` 6) .|. fromIntegral (b .&. 0x3f)
               in if cp > 0x10ffff
                    then Skip len
                    else Emit (EvChar (chr cp) noMods) len
