-- | A tiny persistent shell: type a command, press Enter, see its output.
module SdlTerminal (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (IOException, bracket, catch, finally)
import Control.Monad (void, when)
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Text.IO qualified as T
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Monad (askInput)
import Streaming.Prelude qualified as S
import System.IO (hFlush, hGetChar)
import System.IO.Error (isEOFError)
import System.Process

main :: IO ()
main = withCreateProcess (shell "exec /bin/sh 2>&1")
  {std_in = CreatePipe, std_out = CreatePipe, create_group = True} $ \input output _ process ->
  case (input, output) of
    (Just hin, Just hout) -> do
      logRef <- newIORef ""
      let append text = atomicModifyIORef' logRef (\s -> (T.takeEnd 16000 (s <> text), ()))
          readOutput = S.mapM_ (append . T.singleton) (S.repeatM (hGetChar hout))
            `catch` \(e :: IOException) -> append (if isEOFError e then "\n[shell exited]" else T.pack (show e))
      bracket (forkIO readOutput) killThread $ \_ -> flip finally (interruptProcessGroupOf process) $
        runSdlApp defaultSdlOptions
          {sdlWindowTitle = "nano-ui Shell", sdlWindowSize = Size 900 600
          , sdlWindowResizable = False, sdlAppContinuous = True
          , sdlAppTheme = Just tomorrowNightMinDarkTheme} $ do
          (command, setCommand) <- useText ""
          inp <- askInput
          let has k = inputKeysElem k (inputKeys inp)
              typed = if modCtrl (inputModifiers inp) then "" else inputChars inp
              line = (if has KeyBackspace then T.dropEnd 1 else id) (command <> typed)
          setCommand (if has KeyEnter then "" else line)
          when (has KeyEnter) $ uiIO $ do
            append ("$ " <> line <> "\n")
            (T.hPutStrLn hin line >> hFlush hin)
              `catch` \(e :: IOException) -> append (T.pack (show e) <> "\n")
          outputText <- uiIO (readIORef logRef)
          columnWith (grow . gap 0 . padAll 12) $ do
            mapM_ mono (reverse (take 20 (reverse (T.lines outputText))))
            flex
            void separator
            mono ("$ " <> line <> "▏")
    _ -> fail "Could not open shell pipes"
