-- | Headless UI test for the notepad: drives it on a hidden window and
-- asserts the menu bar opens, activates items, closes, and that the Help menu
-- opens the About modal.
module NotepadSelftest
  ( selftest
  ) where

import Control.Monad (void, when)
import NanoUI
import NanoUI.Backend (emptyInput)
import NanoUI.Backend.Sdl
import NanoUI.Testing (collectOverlayTextSpans, collectTextSpans, newPixelContext)
import NanoUI.Testing.Harness
  ( clickPos
  , findExact
  , findRightmost
  , expectText
  , hasText
  , requireSpan
  )
import SdlNotepad (notepadUi)

selftest :: IO ()
selftest = do
  ctx0 <- newPixelContext
  withSdl
    defaultSdlOptions
      { sdlWindowHidden = True
      , sdlWindowSize = Size 1000 720
      , sdlWindowResizable = False
      }
    ctx0
    $ \ctx env -> do
      let
        base = emptyInput {inputWindowSize = Size 1000 720, inputMousePos = V2 500 400}
        drawFrame inp = void (sdlDrawFrame ctx notepadUi env inp False)

      mapM_ drawFrame [base, base]

      spans0 <- collectTextSpans ctx
      expectText "selftest: status bar missing" "Ready" spans0
      filePos <- requireSpan "selftest: File menu" (findExact "File" spans0)

      clickPos drawFrame base filePos
      spansFile <- collectOverlayTextSpans ctx base
      expectText "selftest: File menu did not open" "Save As..." spansFile
      newPos <- requireSpan "selftest: New item" (findExact "New" spansFile)
      clickPos drawFrame base newPos
      baseSpansNew <- collectTextSpans ctx
      overlaySpansNew <- collectOverlayTextSpans ctx base
      when (hasText "Save As..." overlaySpansNew) $
        fail "selftest: File menu did not close"
      expectText "selftest: New action not run" "New document" baseSpansNew

      -- Focus the editor and type; Select All from the Edit menu must keep the
      -- field focused so the selection highlights and the next keystroke
      -- replaces the selection.
      clickPos drawFrame base (V2 500 300)
      drawFrame base {inputChars = "abc"}
      drawFrame base
      editPos <- requireSpan "selftest: Edit menu" (findExact "Edit" baseSpansNew)
      clickPos drawFrame base editPos
      spansEdit <- collectOverlayTextSpans ctx base
      selectAllPos <-
        requireSpan "selftest: Select All item" (findRightmost "Select All" spansEdit)
      clickPos drawFrame base selectAllPos
      drawFrame base {inputChars = "Z"}
      drawFrame base
      spansReplaced <- collectTextSpans ctx
      expectText "selftest: Select All did not keep focus" "Z" spansReplaced
      when (hasText "abc" spansReplaced) $
        fail "selftest: typed text was not replaced"

      -- Ctrl+= / Ctrl+- zoom the editor font only; the status bar tracks it.
      drawFrame base {inputChars = "=", inputModifiers = Modifiers False True False}
      drawFrame base
      spansZoomIn <- collectTextSpans ctx
      expectText "selftest: Ctrl+= did not zoom in" "Zoom: 110%" spansZoomIn
      drawFrame base {inputChars = "-", inputModifiers = Modifiers False True False}
      drawFrame base
      spansZoomOut <- collectTextSpans ctx
      expectText "selftest: Ctrl+- did not zoom out" "Zoom: 100%" spansZoomOut

      -- The File menu offers Exit; activating it terminates the process (via
      -- 'exitSuccess'), so the selftest only checks the item is present and
      -- closes the menu again.
      filePos2 <-
        requireSpan "selftest: File menu (exit)" . findExact "File"
          =<< collectTextSpans ctx
      clickPos drawFrame base filePos2
      spansFile2 <- collectOverlayTextSpans ctx base
      expectText "selftest: File menu missing Exit item" "Exit" spansFile2
      clickPos drawFrame base (V2 500 300) -- dismiss the menu without activating Exit

      helpPos <- requireSpan "selftest: Help menu" (findExact "Help" baseSpansNew)
      clickPos drawFrame base helpPos
      spansHelp <- collectOverlayTextSpans ctx base
      expectText "selftest: Help menu did not open" "About nano-ui Notepad" spansHelp
      aboutPos <-
        requireSpan "selftest: About item" (findExact "About nano-ui Notepad" spansHelp)
      clickPos drawFrame base aboutPos
      spansAbout <- collectOverlayTextSpans ctx base
      expectText "selftest: About modal did not open" "built with nano-ui" spansAbout
      putStrLn "notepad selftest: ok"
