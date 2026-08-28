{-# LANGUAGE CPP #-}

-- | Read the terminal default fg/bg for adaptive TUI theming.
--
-- Prefer OSC 10/11 (truecolor) when stdin is a TTY. On Windows, fall back to
-- the visible-window origin cell attribute (not cursor @wAttributes@). ANSI
-- index mapping is last resort when OSC is unavailable.
module NanoUI.Term.Palette
  ( queryTerminalColors
  , newAdaptiveTerminalContext
  ) where

import Control.Exception (bracketOnError)
import Data.Bits ((.&.), shiftR)
import Data.Char (isDigit, ord)
import Data.List (isPrefixOf)
import Data.Word (Word8, Word16)
import NanoUI
  ( Color
  , Context
  , colorRGBA
  , contrastRatio
  , monospaceMetrics
  , newContext
  , terminalDefaultBg
  , terminalDefaultFg
  , terminalThemeFromColors
  , withExternalText
  , withFontMetrics
  , withTheme
  )
import System.IO
  ( BufferMode (..)
  , hFlush
  , hGetBuffering
  , hGetChar
  , hIsTerminalDevice
  , hSetBuffering
  , hWaitForInput
  , stdin
  , stdout
  )

#if defined(mingw32_HOST_OS)
import Control.Exception (bracket)
import Data.Bits ((.|.))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable (..))
import System.Win32.Console
  ( CONSOLE_SCREEN_BUFFER_INFO (..)
  , COORD (..)
  , getConsoleScreenBufferInfo
  , leftPos
  , topPos
  )
import System.Win32.File
  ( closeHandle
  , createFile
  , fILE_SHARE_READ
  , fILE_SHARE_WRITE
  , gENERIC_READ
  , gENERIC_WRITE
  , oPEN_EXISTING
  )
import System.Win32.Types (DWORD, HANDLE)
#endif

-- | Default fg/bg from the connected terminal, or 'terminalDefaultFg' /
-- 'terminalDefaultBg' when the palette cannot be read.
queryTerminalColors :: IO (Color, Color)
queryTerminalColors =
  queryPlatformColors >>= \case
    Just pair | paletteUsable pair -> pure pair
    _ -> pure (terminalDefaultFg, terminalDefaultBg)

-- Reject identical or near-identical fg/bg from a bad console read.
paletteUsable :: (Color, Color) -> Bool
paletteUsable (fg, bg) = contrastRatio fg bg >= 3

-- | Terminal test/runtime context using the same palette query as 'runTermApp'.
newAdaptiveTerminalContext :: IO Context
newAdaptiveTerminalContext = do
  (fg, bg) <- queryTerminalColors
  ctx <- newContext
  pure
    ( withExternalText
        (withTheme (withFontMetrics ctx (monospaceMetrics 1)) (terminalThemeFromColors fg bg))
        True
    )

queryPlatformColors :: IO (Maybe (Color, Color))
queryPlatformColors =
  queryOscColors >>= \case
    Just pair -> pure (Just pair)
    Nothing ->
#if defined(mingw32_HOST_OS)
      queryWindowsConsoleColors
#else
      pure Nothing
#endif

-- | OSC 10 (fg) and 11 (bg). Works on Windows Terminal, iTerm2, kitty, etc.
queryOscColors :: IO (Maybe (Color, Color))
queryOscColors = do
  tty <- hIsTerminalDevice stdin
  if not tty
    then pure Nothing
    else
      bracketOnError
        (do
            buf <- hGetBuffering stdin
            bout <- hGetBuffering stdout
            hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
            pure (buf, bout))
        ( \(buf, bout) -> do
            hSetBuffering stdin buf
            hSetBuffering stdout bout
        )
        ( \_ -> do
            putStr "\ESC]10;?\a\ESC]11;?\a"
            hFlush stdout
            resp <- readOscBuffer oscTimeoutMs
            pure (parseOscPair resp)
        )

oscTimeoutMs :: Int
oscTimeoutMs = 400

readOscBuffer :: Int -> IO String
readOscBuffer budget = go budget ""
  where
    go 0 acc = pure acc
    go ms acc = do
      ready <- hWaitForInput stdin 1
      if ready
        then do
          c <- hGetChar stdin
          go (ms - 1) (acc ++ [c])
        else
          if null acc
            then go (ms - 1) acc
            else if ms > 50
              then go (ms - 10) acc
              else pure acc

parseOscPair :: String -> Maybe (Color, Color)
parseOscPair buf = do
  fg <- parseOscColor buf 10
  bg <- parseOscColor buf 11
  pure (fg, bg)

parseOscColor :: String -> Int -> Maybe Color
parseOscColor buf osc =
  let tag = "\ESC]" ++ show osc ++ ";"
   in case dropPrefix tag buf of
        Nothing -> Nothing
        Just rest ->
          let payload = takeWhile (\c -> c /= '\a' && c /= '\ESC') rest
           in parseColorPayload payload

dropPrefix :: String -> String -> Maybe String
dropPrefix pre s =
  if pre `isPrefixOf` s
    then Just (drop (length pre) s)
    else
      case s of
        (_ : t) -> dropPrefix pre t
        [] -> Nothing

parseColorPayload :: String -> Maybe Color
parseColorPayload payload
  | null payload = Nothing
  | "#" `isPrefixOf` payload = parseHexColor (drop 1 payload)
  | "rgb:" `isPrefixOf` payload = parseRgbSpec (drop 4 payload)
  | otherwise = Nothing

parseHexColor :: String -> Maybe Color
parseHexColor hex
  | length hex >= 6 =
      case (readHex (take 2 hex), readHex (take 2 (drop 2 hex)), readHex (take 2 (drop 4 hex))) of
        (Just r, Just g, Just b) ->
          Just (colorRGBA (fromIntegral r) (fromIntegral g) (fromIntegral b) 255)
        _ -> Nothing
  | otherwise = Nothing
  where
    readHex pair =
      if length pair == 2 && all isHex (take 2 pair)
        then Just (hexByte pair)
        else Nothing
    isHex c =
      isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
    hexByte pair =
      case pair of
        [a, b] -> digitVal a * 16 + digitVal b
        _ -> 0
    digitVal c
      | isDigit c = ord c - ord '0'
      | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
      | otherwise = ord c - ord 'A' + 10

parseRgbSpec :: String -> Maybe Color
parseRgbSpec spec =
  case break (== '/') spec of
    (r, '/' : rest1) ->
      case break (== '/') rest1 of
        (g, '/' : b) -> rgbParts r g b
        _ -> Nothing
    _ -> Nothing
  where
    rgbParts r g b = do
      rv <- readChannel r
      gv <- readChannel g
      bv <- readChannel b
      pure (colorRGBA (fromIntegral rv) (fromIntegral gv) (fromIntegral bv) 255)
    readChannel ch =
      if all isHex ch
        then
          let v = foldl (\acc c -> acc * 16 + digitVal c) 0 ch
              scale = if length ch > 4 then 16 ^ (length ch - 2) else 1
           in Just (min 255 (v `div` max 1 scale))
        else Nothing
    digitVal c
      | isDigit c = ord c - ord '0'
      | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
      | otherwise = ord c - ord 'A' + 10
    isHex c =
      isDigit c || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

#if defined(mingw32_HOST_OS)

foreign import ccall unsafe "windows.h ReadConsoleOutputAttribute"
  c_ReadConsoleOutputAttribute ::
    HANDLE ->
    Ptr Word16 ->
    DWORD ->
    Ptr COORD ->
    Ptr DWORD ->
    IO Bool

queryWindowsConsoleColors :: IO (Maybe (Color, Color))
queryWindowsConsoleColors =
  bracket (openConsole "CONOUT$") closeHandle $ \h -> do
    info <- getConsoleScreenBufferInfo h
    let win = srWindow info
        origin = COORD (leftPos win) (topPos win)
    allocaArray 1 $ \attrPtr ->
      alloca $ \readPtr ->
        alloca $ \coordPtr -> do
          poke coordPtr origin
          ok <- c_ReadConsoleOutputAttribute h attrPtr 1 coordPtr readPtr
          if not ok
            then pure Nothing
            else do
              attr <- peek attrPtr
              let fgIdx = fromIntegral (attr .&. 0x0F) :: Word8
                  bgIdx = fromIntegral ((attr .&. 0xF0) `shiftR` 4) :: Word8
              pure (Just (ansi16 fgIdx, ansi16 bgIdx))

openConsole :: String -> IO HANDLE
openConsole name =
  createFile
    name
    (gENERIC_READ .|. gENERIC_WRITE)
    (fILE_SHARE_READ .|. fILE_SHARE_WRITE)
    Nothing
    oPEN_EXISTING
    0
    Nothing

-- ANSI index to RGB when OSC is unavailable (approximate; prefer OSC on WT).
ansi16 :: Word8 -> Color
ansi16 n =
  case n of
    0 -> colorRGBA 12 12 12 255
    1 -> colorRGBA 197 15 31 255
    2 -> colorRGBA 19 161 14 255
    3 -> colorRGBA 193 156 0 255
    4 -> colorRGBA 0 55 218 255
    5 -> colorRGBA 136 23 152 255
    6 -> colorRGBA 58 150 221 255
    7 -> colorRGBA 204 204 204 255
    8 -> colorRGBA 118 118 118 255
    9 -> colorRGBA 231 72 86 255
    10 -> colorRGBA 22 198 12 255
    11 -> colorRGBA 249 241 165 255
    12 -> colorRGBA 59 120 255 255
    13 -> colorRGBA 180 0 158 255
    14 -> colorRGBA 97 214 214 255
    15 -> colorRGBA 242 242 242 255
    _ -> colorRGBA 204 204 204 255

#endif
