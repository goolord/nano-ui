-- | The notepad on a hidden window: the menu bar opens, runs items and
-- closes, chords close an open menu, editing commands act on the
-- selection, and Save writes UTF-8.
module NotepadSelftest
  ( selftest
  ) where

import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Text.Encoding qualified as TE
import DemoApp (withHiddenWindow)
import Drive
import GHC.IO.Encoding (getLocaleEncoding, setLocaleEncoding)
import NanoUI
import NanoUI.Backend (runNanoUI)
import NanoUI.Shortcut
import Notepad qualified
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, latin1, openTempFile)

selftest :: IO ()
selftest = do
  state <- Notepad.newNotepad
  withHiddenWindow 1000 720 (V2 500 400) id $ \ctx env base -> runDrive ctx env base (Notepad.notepadUi state) $ do
    frame
    frame
    expect "Ready"
    click "File"
    expect "Save As..."
    clickRow "New"
    expectNot "Save As..."
    expect "New document"
    -- A command's chord closes the open menu, as its row does.
    click "File"
    chord (cmdOrCtrl <> key 'n')
    expectNot "Save As..."
    -- Select All from the Edit menu keeps the editor focused, so the next
    -- keystroke replaces the selection.
    clickAt (V2 500 300)
    typeText "abc"
    click "Edit"
    clickRow "Select All"
    typeText "Z"
    _ <- find "Z"
    expectNot "abc"
    -- Edit > Cut takes only the selection.
    typeText "12"
    chord (shift <> key KeyLeft)
    click "Edit"
    clickRow "Cut"
    expect "Z1"
    expectNot "Z12"
    -- Ctrl+= and Ctrl+- zoom the editor font; the status bar tracks it.
    chord (cmdOrCtrl <> key '=')
    expect "Zoom: 110%"
    chord (cmdOrCtrl <> key '-')
    expect "Zoom: 100%"
    -- Ctrl++ (Ctrl+Shift+=), bound apart from the rows, closes an open menu
    -- as they do.
    click "View"
    chord (cmdOrCtrl <> shift <> key '=')
    expectNot "Zoom In"
    expect "Zoom: 110%"
    -- Exit would end the session, so only check it is offered.
    click "File"
    expect "Exit"
    clickAt (V2 500 300)
    click "Help"
    click "About nano-ui Notepad"
    expect "built with nano-ui"
    -- Save writes the document's UTF-8 bytes with its own line ends whatever
    -- the locale: under Latin-1 a text-mode write cannot encode the arrow.
    let contents = "caf\233\n\8594 \26085\n"
    (saved, bytes) <- liftIO $ do
      (file, h) <- (`openTempFile` "notepad.txt") =<< getTemporaryDirectory
      hClose h
      saved <-
        bracket (getLocaleEncoding <* setLocaleEncoding latin1) setLocaleEncoding $ \_ ->
          runNanoUI ctx base (Notepad.writeDocument file (textDocument contents))
      (saved,) <$> BS.readFile file <* removeFile file
    check "Save did not write the document as UTF-8" (saved && bytes == TE.encodeUtf8 contents)
  putStrLn "notepad selftest: ok"
