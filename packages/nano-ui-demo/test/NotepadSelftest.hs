-- | Headless UI test for the notepad: drives it on a hidden window and
-- asserts the menu bar opens, activates items, closes, and that the Help menu
-- opens the About modal.
module NotepadSelftest
  ( selftest
  ) where

import Control.Monad (void, when)
import Data.Text qualified as T
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Shortcut
import NanoUI.Testing (collectOverlayTextSpans, collectTextSpans)
import NanoUI.Testing.Harness
  ( chordInp
  , clickPos
  , findExact
  , findRightmost
  , expectText
  , hasText
  , requireSpan
  )
import DemoApp (withHiddenWindow)
import SdlNotepad qualified

selftest :: IO ()
selftest = do
  state <- SdlNotepad.newNotepad
  withHiddenWindow 1000 720 (V2 500 400) id $ \ctx env base -> do
      let notepadUi = SdlNotepad.notepadUi state
      let
        drawFrame inp = void (sdlDrawFrame ctx notepadUi env inp False)
        click = clickPos drawFrame base
        clickOn name spans = click =<< requireSpan ("selftest: " <> T.unpack name) (findExact name spans)
        chord c = drawFrame (chordInp c base) >> drawFrame base
        overlay = collectOverlayTextSpans ctx base

      mapM_ drawFrame [base, base]

      spans0 <- collectTextSpans ctx
      expectText "selftest: status bar missing" "Ready" spans0

      clickOn "File" spans0
      spansFile <- overlay
      expectText "selftest: File menu did not open" "Save As..." spansFile
      click =<< requireSpan "selftest: New item" (findRightmost "New" spansFile)
      baseSpansNew <- collectTextSpans ctx
      overlaySpansNew <- overlay
      when (hasText "Save As..." overlaySpansNew) $
        fail "selftest: File menu did not close"
      expectText "selftest: New action not run" "New document" baseSpansNew

      -- Focus the editor and type; Select All from the Edit menu must keep the
      -- field focused so the selection highlights and the next keystroke
      -- replaces the selection.
      click (V2 500 300)
      drawFrame base {inputChars = "abc"}
      drawFrame base
      clickOn "Edit" baseSpansNew
      click =<< requireSpan "selftest: Select All item" . findRightmost "Select All" =<< overlay
      drawFrame base {inputChars = "Z"}
      drawFrame base
      spansReplaced <- collectTextSpans ctx
      expectText "selftest: Select All did not keep focus" "Z" spansReplaced
      when (hasText "abc" spansReplaced) $
        fail "selftest: typed text was not replaced"

      -- Edit > Cut takes only the selection, not the whole text.
      drawFrame base {inputChars = "12"}
      drawFrame base
      chord (shift <> key KeyLeft)
      clickOn "Edit" =<< collectTextSpans ctx
      click =<< requireSpan "selftest: Cut item" . findRightmost "Cut" =<< overlay
      spansCut <- collectTextSpans ctx
      expectText "selftest: Edit > Cut removed more than the selection" "Z1" spansCut
      when (hasText "Z12" spansCut) $
        fail "selftest: Edit > Cut did not cut the selection"

      -- Ctrl+= / Ctrl+- zoom the editor font only; the status bar tracks it.
      chord (ctrl <> key '=')
      expectText "selftest: Ctrl+= did not zoom in" "Zoom: 110%" =<< collectTextSpans ctx
      chord (ctrl <> key '-')
      expectText "selftest: Ctrl+- did not zoom out" "Zoom: 100%" =<< collectTextSpans ctx

      -- The File menu offers Exit; activating it ends the session (via
      -- 'quitUi', after confirming unsaved changes), so the selftest only
      -- checks the item is present and closes the menu again.
      clickOn "File" =<< collectTextSpans ctx
      expectText "selftest: File menu missing Exit item" "Exit" =<< overlay
      click (V2 500 300) -- dismiss the menu without activating Exit

      clickOn "Help" baseSpansNew
      spansHelp <- overlay
      expectText "selftest: Help menu did not open" "About nano-ui Notepad" spansHelp
      clickOn "About nano-ui Notepad" spansHelp
      expectText "selftest: About modal did not open" "built with nano-ui" =<< overlay
      putStrLn "notepad selftest: ok"
