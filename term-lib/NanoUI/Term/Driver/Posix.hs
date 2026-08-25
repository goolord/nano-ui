-- | POSIX driver: termios raw mode plus the shared VT decoder.
--
-- Unlike Windows there is no native event record to read, so mouse reporting
-- is requested with DECSET sequences and decoded from the byte stream. Reads
-- carry a leftover buffer between calls because a sequence can be split across
-- reads.
module NanoUI.Term.Driver.Posix
  ( withDriver
  ) where

import Control.Exception (bracket_)
import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteString, hPutBuilder)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import NanoUI.Term.Driver.Types (Driver (..))
import NanoUI.Term.Driver.WinSize (terminalSize)
import NanoUI.Term.Event (TermEvent (..))
import NanoUI.Term.Vt (decode, disableMouse, enableMouse, flushPending)
import System.IO
  ( BufferMode (BlockBuffering, NoBuffering)
  , hFlush
  , hSetBinaryMode
  , hSetBuffering
  , stdin
  , stdout
  )
import System.Posix.IO (stdInput)
import System.Posix.Signals (Handler (Catch), installHandler, sigWINCH)
import System.Posix.Terminal
  ( TerminalAttributes
  , TerminalMode (..)
  , TerminalState (Immediately)
  , getTerminalAttributes
  , setTerminalAttributes
  , withMinInput
  , withTime
  , withoutMode
  )
import qualified Data.ByteString as BS
import qualified GHC.IO.Handle as Handle

withDriver :: (Driver -> IO a) -> IO a
withDriver act = do
  attrs0 <- getTerminalAttributes stdInput
  leftover <- newIORef BS.empty
  resized <- newIORef False
  hSetBinaryMode stdin True
  hSetBuffering stdin NoBuffering
  hSetBinaryMode stdout True
  hSetBuffering stdout (BlockBuffering Nothing)
  _ <- installHandler sigWINCH (Catch (writeIORef resized True)) Nothing
  let enter = do
        setTerminalAttributes stdInput (rawAttributes attrs0) Immediately
        hPutBuilder stdout (byteString enableMouse)
        hFlush stdout
      leave = do
        hPutBuilder stdout (byteString disableMouse)
        hFlush stdout
        setTerminalAttributes stdInput attrs0 Immediately
  bracket_ enter leave $
    act
      Driver
        { drvSize = terminalSize
        , drvRead = readEvents leftover resized
        , drvWrite = hPutBuilder stdout
        , drvFlush = hFlush stdout
        }

-- | Canonical mode, echo, signal generation and CR translation all have to go,
-- otherwise input arrives line-buffered and rewritten.
rawAttributes :: TerminalAttributes -> TerminalAttributes
rawAttributes attrs =
  flip withTime 0 $
    flip withMinInput 0 $
      foldl
        withoutMode
        attrs
        [ ProcessInput
        , EnableEcho
        , KeyboardInterrupts
        , ExtendedFunctions
        , MapCRtoLF
        , StartStopOutput
        , InterruptOnBreak
        , CheckParity
        , StripHighBit
        , ProcessOutput
        ]

readChunk :: Int
readChunk = 4096

readEvents :: IORef ByteString -> IORef Bool -> Int -> IO [TermEvent]
readEvents leftover resized timeoutMs = do
  resizeBefore <- drainResize resized
  ready <- Handle.hWaitForInput stdin timeoutMs
  resizeAfter <- drainResize resized
  let resizeEvents = resizeBefore ++ resizeAfter
  if not ready
    then do
      -- Input went idle, so a held-back lone ESC really was the Escape key.
      pending <- readIORef leftover
      if BS.null pending
        then pure resizeEvents
        else do
          writeIORef leftover BS.empty
          pure (resizeEvents ++ flushPending pending)
    else do
      chunk <- BS.hGetSome stdin readChunk
      pending <- readIORef leftover
      let (evs, rest) = decode (pending <> chunk)
      writeIORef leftover rest
      pure (resizeEvents ++ evs)

drainResize :: IORef Bool -> IO [TermEvent]
drainResize resized = do
  wasResized <- readIORef resized
  if wasResized
    then do
      writeIORef resized False
      (w, h) <- terminalSize
      pure [EvResize w h]
    else pure []
