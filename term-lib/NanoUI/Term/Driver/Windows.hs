-- | Windows driver, reading native console input records.
--
-- Windows can also be driven through VT escape sequences, but native records
-- report pointer motion unconditionally, whereas VT mouse reporting only
-- arrives if the emulator implements DECSET 1003. Reading records directly
-- means hover works on plain conhost as well as Windows Terminal, and mouse
-- coordinates are never subject to the 223-column limit of legacy encodings.
module NanoUI.Term.Driver.Windows
  ( withDriver
  ) where

import Control.Exception (bracket, bracket_)
import Data.Bits (shiftR, testBit, (.&.), (.|.))
import Data.ByteString.Builder (hPutBuilder)
import Data.Char (chr)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff)
import NanoUI (Key (..), Modifiers (..))
import NanoUI.Term.Driver.Types (Driver (..))
import NanoUI.Term.Event
  ( MouseAction (..)
  , MouseBtn (..)
  , TermEvent (..)
  )
import System.IO (BufferMode (BlockBuffering), hFlush, hSetBinaryMode, hSetBuffering, stdout)
import System.Win32.Console
  ( CONSOLE_SCREEN_BUFFER_INFO (..)
  , COORD (..)
  , INPUT_RECORD (..)
  , KEY_EVENT_RECORD (..)
  , MOUSE_EVENT_RECORD (..)
  , SMALL_RECT (..)
  , eNABLE_EXTENDED_FLAGS
  , eNABLE_MOUSE_INPUT
  , eNABLE_PROCESSED_OUTPUT
  , eNABLE_VIRTUAL_TERMINAL_PROCESSING
  , eNABLE_WINDOW_INPUT
  , getConsoleMode
  , getConsoleScreenBufferInfo
  , readConsoleInput
  , setConsoleMode
  , setConsoleOutputCP
  )
import System.Win32.Event (waitForSingleObject)
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

-- | Console input state that has to be remembered between records: the button
-- mask from the previous mouse record (to tell a press from a release) and a
-- pending high surrogate from a split astral character.
data WinState = WinState
  { wsButtons :: !DWORD
  , wsSurrogate :: !(Maybe Int)
  , wsOrigin :: !(Int, Int)
  }

withDriver :: (Driver -> IO a) -> IO a
withDriver act =
  bracket (openConsole "CONIN$") closeHandle $ \hIn ->
    bracket (openConsole "CONOUT$") closeHandle $ \hOut -> do
      inMode0 <- getConsoleMode hIn
      outMode0 <- getConsoleMode hOut
      setConsoleOutputCP 65001
      hSetBinaryMode stdout True
      hSetBuffering stdout (BlockBuffering Nothing)
      st <- newIORef WinState {wsButtons = 0, wsSurrogate = Nothing, wsOrigin = (0, 0)}
      let enter = do
            -- Deliberately omits ENABLE_QUICK_EDIT_MODE: with quick edit on,
            -- the console consumes drags for text selection instead of
            -- forwarding them. ENABLE_EXTENDED_FLAGS makes that omission take
            -- effect. Line/echo/processed input stay off so keys arrive raw.
            setConsoleMode hIn (eNABLE_MOUSE_INPUT .|. eNABLE_WINDOW_INPUT .|. eNABLE_EXTENDED_FLAGS)
            setConsoleMode hOut (eNABLE_VIRTUAL_TERMINAL_PROCESSING .|. eNABLE_PROCESSED_OUTPUT)
          leave = do
            setConsoleMode hIn inMode0
            setConsoleMode hOut outMode0
      bracket_ enter leave $ do
        refreshViewport st
        act
          Driver
            { drvSize = consoleSize
            , drvRead = readEvents hIn st
            , drvWrite = hPutBuilder stdout
            , drvFlush = hFlush stdout
            , drvRefreshViewport = refreshViewport st
            }

-- | Always talk to the console itself rather than the process's standard
-- handles, which may be redirected to a pipe or file.
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

-- | Reopened per query because the VT alternate screen buffer swaps which
-- screen buffer @CONOUT$@ resolves to.
consoleSize :: IO (Int, Int)
consoleSize =
  bracket (openConsole "CONOUT$") closeHandle $ \h -> do
    info <- getConsoleScreenBufferInfo h
    let win = srWindow info
        w = fromIntegral (rightPos win) - fromIntegral (leftPos win) + 1
        h' = fromIntegral (bottomPos win) - fromIntegral (topPos win) + 1
    pure (max 1 w, max 1 h')

-- | Mouse records are in buffer coordinates; the visible window may start
-- lower when scrollback exists.
windowOrigin :: HANDLE -> IO (Int, Int)
windowOrigin h = do
  info <- getConsoleScreenBufferInfo h
  let win = srWindow info
  pure (fromIntegral (leftPos win), fromIntegral (topPos win))

refreshViewport :: IORef WinState -> IO ()
refreshViewport st = do
  (ox, oy) <- bracket (openConsole "CONOUT$") closeHandle windowOrigin
  s <- readIORef st
  writeIORef st s {wsOrigin = (ox, oy)}

recordBatch :: Int
recordBatch = 64

wAIT_OBJECT_0 :: DWORD
wAIT_OBJECT_0 = 0

wAIT_FAILED :: DWORD
wAIT_FAILED = 0xffffffff

readEvents :: HANDLE -> IORef WinState -> Int -> IO [TermEvent]
readEvents hIn st timeoutMs = do
  ready <- waitForSingleObject hIn (fromIntegral timeoutMs)
  if ready == wAIT_OBJECT_0
    then allocaArray recordBatch $ \buf -> do
      n <- readConsoleInput hIn recordBatch buf
      concat <$> mapM (convertAt buf) [0 .. n - 1]
    else
      if ready == wAIT_FAILED
        then fail "waitForSingleObject on console input failed"
        else pure []
  where
    convertAt :: Ptr INPUT_RECORD -> Int -> IO [TermEvent]
    convertAt buf i = peekElemOff buf i >>= convert st

convert :: IORef WinState -> INPUT_RECORD -> IO [TermEvent]
convert st record =
  case record of
    KeyEvent (KEY_EVENT_RECORD down repeatCount vk _ ch ctrlState)
      | not down -> pure []
      | otherwise -> keyRecord st (fromIntegral vk) (fromEnum ch) ctrlState (fromIntegral repeatCount)
    MouseEvent (MOUSE_EVENT_RECORD (COORD x y) buttons ctrlState flags) ->
      mouseRecord st (fromIntegral x) (fromIntegral y) buttons ctrlState flags
    WindowBufferSizeEvent _ -> resizeEvents st
    _ -> pure []

resizeEvents :: IORef WinState -> IO [TermEvent]
resizeEvents st =
  bracket (openConsole "CONOUT$") closeHandle $ \h -> do
    info <- getConsoleScreenBufferInfo h
    let win = srWindow info
        origin = (fromIntegral (leftPos win), fromIntegral (topPos win))
        w = fromIntegral (rightPos win) - fromIntegral (leftPos win) + 1
        h' = fromIntegral (bottomPos win) - fromIntegral (topPos win) + 1
    s <- readIORef st
    writeIORef st s {wsOrigin = origin}
    pure [EvResize (max 1 w) (max 1 h')]

keyRecord :: IORef WinState -> Int -> Int -> DWORD -> Int -> IO [TermEvent]
keyRecord st vk ch ctrlState repeatCount =
  let n = max 1 repeatCount
   in case specialKey vk of
        Just k -> pure (replicate n (EvKey k mods))
        Nothing
          | ch == 0 -> pure []
          | isHighSurrogate ch -> do
              s <- readIORef st
              writeIORef st s {wsSurrogate = Just ch}
              pure []
          | otherwise -> do
              s <- readIORef st
              case wsSurrogate s of
                Just hi | isLowSurrogate ch -> do
                  writeIORef st s {wsSurrogate = Nothing}
                  pure (replicate n (EvChar (chr (combine hi ch)) mods))
                _ -> do
                  writeIORef st s {wsSurrogate = Nothing}
                  pure (replicate n (EvChar (chr ch) mods))
  where
    mods =
      Modifiers
        { modShift = testBit ctrlState 4
        , modCtrl = testBit ctrlState 2 || testBit ctrlState 3
        , modAlt = testBit ctrlState 0 || testBit ctrlState 1
        }
    isHighSurrogate c = c >= 0xd800 && c < 0xdc00
    isLowSurrogate c = c >= 0xdc00 && c < 0xe000
    combine hi lo = 0x10000 + ((hi - 0xd800) * 0x400) + (lo - 0xdc00)

specialKey :: Int -> Maybe Key
specialKey vk =
  case vk of
    0x08 -> Just KeyBackspace
    0x09 -> Just KeyTab
    0x0d -> Just KeyEnter
    0x1b -> Just KeyEscape
    0x23 -> Just KeyEnd
    0x24 -> Just KeyHome
    0x25 -> Just KeyLeft
    0x26 -> Just KeyUp
    0x27 -> Just KeyRight
    0x28 -> Just KeyDown
    0x2e -> Just KeyDelete
    _ -> Nothing

mouseRecord :: IORef WinState -> Int -> Int -> DWORD -> DWORD -> DWORD -> IO [TermEvent]
mouseRecord st x y buttons ctrlState flags = do
  s <- readIORef st
  writeIORef st s {wsButtons = buttons}
  let (ox, oy) = wsOrigin s
      col = x - ox
      row = y - oy
      previous = wsButtons s
      held = buttonOf buttons
      action
        | testBit flags 2 = Just (if wheelDelta > 0 then MouseScrollUp else MouseScrollDown)
        | -- Horizontal wheel; no corresponding input axis.
          testBit flags 3 =
            Nothing
        | testBit flags 0 =
            -- Motion: a held button makes it a drag, otherwise it is hover.
            Just (maybe MouseMove MouseDrag held)
        | buttons == 0 = Just (MouseRelease (buttonOf previous))
        | otherwise = MousePress <$> newlyPressed previous buttons
  pure (maybe [] (\a -> [EvMouse a col row mods]) action)
  where
    mods =
      Modifiers
        { modShift = testBit ctrlState 4
        , modCtrl = testBit ctrlState 2 || testBit ctrlState 3
        , modAlt = testBit ctrlState 0 || testBit ctrlState 1
        }
    -- The wheel delta is a signed 16-bit value in the high word.
    wheelDelta :: Int
    wheelDelta =
      let hi = fromIntegral (buttons `shiftR` 16) :: Int
       in if hi > 0x7fff then hi - 0x10000 else hi
    buttonOf mask
      | mask .&. 0x1 /= 0 = Just BtnLeft
      | mask .&. 0x4 /= 0 = Just BtnMiddle
      | mask .&. 0x2 /= 0 = Just BtnRight
      | otherwise = Nothing
    newlyPressed prev cur = buttonOf (cur .&. complementMask prev)
    complementMask prev = 0xffff .&. (0xffff - (prev .&. 0xffff))
