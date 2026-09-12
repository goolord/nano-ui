-- | A small notepad application built on the SDL3 backend.
--
-- It is intentionally self-contained: the menu bar is implemented locally on
-- top of the generic 'popup' primitive, and the whole application is one
-- 'NanoUI' function driven by local state hooks.
--
-- Run with @cabal run nano-ui-sdl-notepad@.
module SdlNotepad (main, notepadUi) where

import Control.Exception (SomeException, try)
import Control.Monad (unless, void, when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Monad (askContext, askInput)
import NanoUI.Testing (collectOverlayTextSpans, collectTextSpans)
import NanoUI.Testing.Harness
  ( clickPos
  , findExact
  , findRightmost
  , hasText
  , requireSpan
  )
import NanoUI.Widgets.TextArea (applyTextAreaMenuAction)
import System.Directory (getTemporaryDirectory, removeFile)
import System.Environment (getArgs)
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
-- Application entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  if "--selftest" `elem` args
    then selftest
    else
      runSdlApp
        defaultSdlOptions
          { sdlWindowTitle = "nano-ui Notepad"
          , sdlWindowSize = Size 1000 720
          , sdlAppTheme = Just tomorrowNightMinDarkTheme
          , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
          }
        notepadUi

--------------------------------------------------------------------------------
-- Headless self-test
--------------------------------------------------------------------------------

-- | Drives the notepad on a hidden window and asserts the menu bar opens,
-- activates items, closes, and that the Help menu opens the About modal. Run
-- with @cabal run nano-ui-sdl-notepad -- --selftest@.
selftest :: IO ()
selftest = do
  ctx0 <- newSdlContext
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
        clickAt2 pos = clickPos drawFrame base pos

      -- readFileFast round-trips a temp file.
      tmpDir <- getTemporaryDirectory
      let
        tmpPath = tmpDir <> "/nano-ui-notepad-read.txt"
      writeFile tmpPath "hello read\nsecond line"
      raw <- readFileFast tmpPath
      let
        decoded = TE.decodeUtf8 raw
      unless (decoded == "hello read\nsecond line") $
        fail "selftest: readFileFast round-trip failed"
      writeFile tmpPath ""
      rawEmpty <- readFileFast tmpPath
      unless (BS.null rawEmpty) $ fail "selftest: readFileFast empty file failed"
      removeFile tmpPath

      mapM_ drawFrame [base, base]

      spans0 <- collectTextSpans ctx
      unless (hasText "Ready" spans0) $ fail "selftest: status bar missing"
      filePos <- requireSpan "selftest: File menu" (findExact "File" spans0)

      clickAt2 filePos
      spansFile <- collectOverlayTextSpans ctx base
      unless (hasText "Save As..." spansFile) $
        fail "selftest: File menu did not open"
      newPos <- requireSpan "selftest: New item" (findExact "New" spansFile)
      clickAt2 newPos
      baseSpansNew <- collectTextSpans ctx
      overlaySpansNew <- collectOverlayTextSpans ctx base
      when (hasText "Save As..." overlaySpansNew) $
        fail "selftest: File menu did not close"
      unless (hasText "New document" baseSpansNew) $
        fail "selftest: New action not run"

      -- Focus the editor and type; Select All from the Edit menu must keep the
      -- field focused so the selection highlights and the next keystroke
      -- replaces the selection.
      clickAt2 (V2 500 300)
      drawFrame base {inputChars = "abc"}
      drawFrame base
      editPos <- requireSpan "selftest: Edit menu" (findExact "Edit" baseSpansNew)
      clickAt2 editPos
      spansEdit <- collectOverlayTextSpans ctx base
      selectAllPos <-
        requireSpan "selftest: Select All item" (findRightmost "Select All" spansEdit)
      clickAt2 selectAllPos
      drawFrame base {inputChars = "Z"}
      drawFrame base
      spansReplaced <- collectTextSpans ctx
      unless (hasText "Z" spansReplaced) $
        fail "selftest: Select All did not keep focus"
      when (hasText "abc" spansReplaced) $
        fail "selftest: typed text was not replaced"

      -- Ctrl+= / Ctrl+- zoom the editor font only; the status bar tracks it.
      drawFrame base {inputChars = "=", inputModifiers = Modifiers False True False}
      drawFrame base
      spansZoomIn <- collectTextSpans ctx
      unless (hasText "Zoom: 110%" spansZoomIn) $
        fail "selftest: Ctrl+= did not zoom in"
      drawFrame base {inputChars = "-", inputModifiers = Modifiers False True False}
      drawFrame base
      spansZoomOut <- collectTextSpans ctx
      unless (hasText "Zoom: 100%" spansZoomOut) $
        fail "selftest: Ctrl+- did not zoom out"

      -- The File menu offers Exit; activating it terminates the process (via
      -- 'exitSuccess'), so the selftest only checks the item is present and
      -- closes the menu again.
      filePos2 <-
        requireSpan "selftest: File menu (exit)" . findExact "File"
          =<< collectTextSpans ctx
      clickAt2 filePos2
      spansFile2 <- collectOverlayTextSpans ctx base
      unless (hasText "Exit" spansFile2) $
        fail "selftest: File menu missing Exit item"
      clickAt2 (V2 500 300) -- dismiss the menu without activating Exit

      helpPos <- requireSpan "selftest: Help menu" (findExact "Help" baseSpansNew)
      clickAt2 helpPos
      spansHelp <- collectOverlayTextSpans ctx base
      unless (hasText "About nano-ui Notepad" spansHelp) $
        fail "selftest: Help menu did not open"
      aboutPos <-
        requireSpan "selftest: About item" (findExact "About nano-ui Notepad" spansHelp)
      clickAt2 aboutPos
      spansAbout <- collectOverlayTextSpans ctx base
      unless (hasText "built with nano-ui" spansAbout) $
        fail "selftest: About modal did not open"
      putStrLn "notepad selftest: ok"

--------------------------------------------------------------------------------
-- The application
--------------------------------------------------------------------------------

notepadUi :: NanoUI ()
notepadUi = do
  ------------------------------------------------------------------ hooks ---
  (docText, setDocText) <- useText ""
  (docPath, setDocPath) <- useText ""
  (docDirty, setDocDirty) <- useFlag False
  (docGen, setDocGen) <- useInt 0
  (openMenu, setOpenMenu) <- useText ""
  (statusMsg, setStatusMsg) <- useText "Ready"
  (showStatus, setShowStatus) <- useFlag True
  (aboutOpen, setAboutOpen) <- useFlag False
  (editorId, setEditorId) <- useState (WidgetId 0)
  (openDlg, setOpenDlg) <- useState (Nothing :: Maybe FileDialogId)
  (saveDlg, setSaveDlg) <- useState (Nothing :: Maybe FileDialogId)
  (zoom, setZoom) <- useFloat 1.0

  ---------------------------------------------------------------- zoom ---
  inp <- askInput
  let
    ctrlDown = modCtrl (inputModifiers inp)
    typed = inputChars inp
  when
    (ctrlDown && (T.any (== '+') typed || T.any (== '=') typed))
    (setZoom (min 4.0 (zoom * 1.1)))
  when
    (ctrlDown && (T.any (== '-') typed || T.any (== '_') typed))
    (setZoom (max 0.5 (zoom / 1.1)))
  when (ctrlDown && T.any (== '0') typed) (setZoom 1.0)

  ----------------------------------------------------------- file dialogs ---
  useFileDialog openDlg setOpenDlg $ \chosenPaths ->
    for_ (listToMaybe chosenPaths) $ \filePath -> do
      setOpenMenu ""
      setDocGen (docGen + 1)
      loaded <-
        uiIO (try (readFileFast filePath) :: IO (Either SomeException BS.ByteString))
      case loaded of
        Left _ -> setStatusMsg ("Could not open " <> T.pack filePath)
        Right raw -> do
          -- Lenient decode: malformed bytes become U+FFFD instead of throwing.
          setDocText (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') raw)
          setDocPath (T.pack filePath)
          setDocDirty False
          setStatusMsg ("Opened " <> T.pack filePath)
  useFileDialog saveDlg setSaveDlg $ \chosenPaths ->
    for_ (listToMaybe chosenPaths) $ \filePath -> do
      setOpenMenu ""
      saved <- writeDocument filePath docText
      if saved
        then do
          setDocPath (T.pack filePath)
          setDocDirty False
          setStatusMsg ("Saved " <> T.pack filePath)
        else setStatusMsg ("Could not save " <> T.pack filePath)

  ------------------------------------------------------------- menu data ---
  let
    closeThen action = setOpenMenu "" >> action

    newDocument = do
      setDocText ""
      setDocPath ""
      setDocGen (docGen + 1)
      setDocDirty False
      setStatusMsg "New document"

    openDocument = do
      mHandle <- askOpenFileDialog defaultFileDialogOptions
      setOpenDlg mHandle

    saveDocument forceDialog =
      if forceDialog || T.null docPath
        then do
          mHandle <- askSaveFileDialog defaultFileDialogOptions
          setSaveDlg mHandle
        else do
          saved <- writeDocument (T.unpack docPath) docText
          if saved
            then do
              setDocDirty False
              setStatusMsg ("Saved " <> docPath)
            else setStatusMsg ("Could not save " <> docPath)

    editAction itemIndex = do
      setOpenMenu ""
      ctx <- askContext
      uiIO (applyTextAreaMenuAction ctx editorId itemIndex)

    fileMenu = do
      item "New" (closeThen newDocument)
      item "Open..." (closeThen openDocument)
      item "Save" (closeThen (saveDocument False))
      itemShortcut "Save As..." "Ctrl+Shift+S" (closeThen (saveDocument True))
      menuSeparator
      itemShortcut "Exit" "Esc" (closeThen (uiIO exitSuccess))

    editMenu = do
      itemShortcut "Cut" "Ctrl+X" (editAction 0)
      itemShortcut "Copy" "Ctrl+C" (editAction 1)
      itemShortcut "Paste" "Ctrl+V" (editAction 2)
      menuSeparator
      itemShortcut "Select All" "Ctrl+A" (editAction 3)

    viewMenu = do
      item
        (if showStatus then "Hide Status Bar" else "Show Status Bar")
        (closeThen (setShowStatus (not showStatus)))
      menuSeparator
      itemShortcut "Zoom In" "Ctrl++" (closeThen (setZoom (min 4.0 (zoom * 1.1))))
      itemShortcut "Zoom Out" "Ctrl+-" (closeThen (setZoom (max 0.5 (zoom / 1.1))))
      itemShortcut "Reset Zoom" "Ctrl+0" (closeThen (setZoom 1.0))
      menuSeparator
      item "Document Statistics" (closeThen (setStatusMsg (documentStats docText)))

    helpMenu = do
      item "About nano-ui Notepad" (closeThen (setAboutOpen True))
      menuItemDisabled "nano-ui on GitHub"

  --------------------------------------------------------------- layout ---
  columnWith (grow . gap 0) $ do
    menuBar
      openMenu
      setOpenMenu
      [ ("File", fileMenu)
      , ("Edit", editMenu)
      , ("View", viewMenu)
      , ("Help", helpMenu)
      ]
    void $ separator

    (editorResp, editorText) <-
      keyed docGen $
        textAreaWith
          ( grow . minW 240 . minH 160 . fontSizeScale zoom $
              defaultLayout
          )
          docText
    when (respChanged editorResp) $ do
      setDocText editorText
      setDocDirty True
    when (respId editorResp /= editorId) (setEditorId (respId editorResp))

    when showStatus $ do
      void $ separator
      statusBar docPath docDirty docText statusMsg zoom

  --------------------------------------------------------------- overlays ---
  (aboutResp, _) <-
    modal aboutOpen "About" $ do
      heading "nano-ui Notepad"
      muted "A menu-bar notepad built with nano-ui on SDL3."
      muted "File, Edit, View and Help are wired to real actions."
      rowWith fillW $ do
        flex
        clickButton "Close" (setAboutOpen False)
  onClick aboutResp (setAboutOpen False)

--------------------------------------------------------------------------------
-- Local menu-bar widget
--------------------------------------------------------------------------------

-- | A horizontal menu bar. @openMenu@ is the label of the currently open
-- drop-down (the empty text when every menu is closed). Each entry pairs a
-- label with its drop-down body; item actions are expected to close the menu.
menuBar :: Text -> (Text -> NanoUI ()) -> [(Text, NanoUI ())] -> NanoUI ()
menuBar openMenu setOpen entries = do
  rowWith (tight . fillW . fixedH 28) $ do
    for_ entries $ \(menuLabel, body) -> do
      let
        isOpen = openMenu == menuLabel
      btn <- menuButton menuLabel isOpen
      let
        cfg =
          (defaultPopupConfig (AnchorRect (respRect btn)))
            { cfgPlacement = PlacementBelow
            , cfgOffset = 0
            }
      when (respClicked btn) (setOpen (if isOpen then "" else menuLabel))
      when
        (not isOpen && not (T.null openMenu) && respHovered btn)
        (setOpen menuLabel)
      (popupResp, _) <- popup isOpen cfg (columnWith (tight . gap 0) body)
      when (respClicked popupResp) (setOpen "")
    flex

-- | Drop-down item; runs @action@ on click.
item :: Text -> NanoUI () -> NanoUI ()
item title action = do
  resp <- menuItem title
  when (respClicked resp) action

-- | Drop-down item with a shortcut hint; runs @action@ on click.
itemShortcut :: Text -> Text -> NanoUI () -> NanoUI ()
itemShortcut title shortcut action = do
  resp <- menuItemWithShortcut title shortcut
  when (respClicked resp) action

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Poll a pending dialog handle; consume each result exactly once.
useFileDialog ::
  Maybe FileDialogId
  -> (Maybe FileDialogId -> NanoUI ())
  -> ([FilePath] -> NanoUI ())
  -> NanoUI ()
useFileDialog mHandle clear consume =
  for_ mHandle $ \handle ->
    pollFileDialogUi handle >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected chosenPaths -> consume chosenPaths >> clear Nothing
      _finished -> clear Nothing

writeDocument :: FilePath -> Text -> NanoUI Bool
writeDocument filePath contents = do
  result <-
    uiIO (try (TIO.writeFile filePath contents) :: IO (Either SomeException ()))
  pure (either (const False) (const True) result)

-- | Open a file with a single strict read; the caller lenient-decodes the
-- bytes into 'Text'.
readFileFast :: FilePath -> IO BS.ByteString
readFileFast = BS.readFile

statusBar :: Text -> Bool -> Text -> Text -> Float -> NanoUI ()
statusBar path dirty contents message zoomVal =
  rowWith (tight . gap 12 . fillW . padXY 8 4) $ do
    void $ labelEx (tight . fontMuted $ defaultLayout) message
    flex
    void $ labelEx (tight . fontMuted $ defaultLayout) (documentLabel path dirty)
    void $ labelEx (tight . fontMuted $ defaultLayout) (documentStats contents)
    void $ labelEx (tight . fontMuted $ defaultLayout) (zoomLabel zoomVal)

zoomLabel :: Float -> Text
zoomLabel zoomVal =
  "Zoom: " <> T.pack (show (round (zoomVal * 100) :: Int)) <> "%"

documentLabel :: Text -> Bool -> Text
documentLabel path dirty =
  (if T.null path then "Untitled" else path) <> (if dirty then " *" else "")

documentStats :: Text -> Text
documentStats contents =
  "Words: "
    <> T.pack (show (length (T.words contents)))
    <> "   Chars: "
    <> T.pack (show (T.length contents))
