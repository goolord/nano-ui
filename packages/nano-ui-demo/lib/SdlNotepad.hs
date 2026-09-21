-- | A small notepad application built on the SDL3 backend.
--
-- It is self-contained: the menu bar is implemented locally on
-- top of the generic 'popup' primitive, and the whole application is one
-- 'NanoUI' function driven by local state hooks.
--
-- Run with @cabal run nano-ui-sdl-notepad@.
module SdlNotepad
  ( main
  , notepadUi
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import DemoApp (useFileDialog)
import NanoUI
import NanoUI.Backend.Sdl
import System.Exit (exitSuccess)

--------------------------------------------------------------------------------
-- Application entry point
--------------------------------------------------------------------------------

main :: IO ()
main =
  runSdlApp
    defaultSdlOptions
      { sdlWindowTitle = "nano-ui Notepad"
      , sdlWindowSize = Size 1000 720
      , sdlAppTheme = Just tomorrowNightMinDarkTheme
      , sdlAppShouldQuit = \inp -> inputKeysElem KeyEscape (inputKeys inp)
      }
    notepadUi

--------------------------------------------------------------------------------
-- The application
--------------------------------------------------------------------------------

notepadUi :: NanoUI ()
notepadUi = do
  ------------------------------------------------------------------ hooks ---
  (doc, setDoc) <- useState emptyDocument
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
        liftIO (try (BS.readFile filePath) :: IO (Either SomeException BS.ByteString))
      case loaded of
        Left _ -> setStatusMsg ("Could not open " <> T.pack filePath)
        Right raw -> do
          -- Lenient decode: malformed bytes become U+FFFD instead of throwing.
          setDoc (textDocument (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') raw))
          setDocPath (T.pack filePath)
          setDocDirty False
          setStatusMsg ("Opened " <> T.pack filePath)
  useFileDialog saveDlg setSaveDlg $ \chosenPaths ->
    for_ (listToMaybe chosenPaths) $ \filePath -> do
      setOpenMenu ""
      saved <- writeDocument filePath doc
      if saved
        then do
          setDocPath (T.pack filePath)
          setDocDirty False
          setStatusMsg ("Saved " <> T.pack filePath)
        else setStatusMsg ("Could not save " <> T.pack filePath)

  ------------------------------------------------------------- menu data ---
  let
    newDocument = do
      setDoc emptyDocument
      setDocPath ""
      setDocGen (docGen + 1)
      setDocDirty False
      setStatusMsg "New document"

    saveDocument forceDialog =
      if forceDialog || T.null docPath
        then do
          mHandle <- askSaveFileDialog defaultFileDialogOptions
          setSaveDlg mHandle
        else do
          saved <- writeDocument (T.unpack docPath) doc
          if saved
            then do
              setDocDirty False
              setStatusMsg ("Saved " <> docPath)
            else setStatusMsg ("Could not save " <> docPath)

    editAction cmd = do
      setOpenMenu ""
      runTextCommand editorId cmd

    fileMenu = do
      whenM (menuItem "New") (setOpenMenu "" >> newDocument)
      whenM (menuItem "Open...") $ do
        setOpenMenu ""
        askOpenFileDialog defaultFileDialogOptions >>= setOpenDlg
      whenM (menuItem "Save") (setOpenMenu "" >> saveDocument False)
      whenM (menuItemShortcut "Save As..." "Ctrl+Shift+S") (setOpenMenu "" >> saveDocument True)
      menuSeparator
      whenM (menuItemShortcut "Exit" "Esc") (setOpenMenu "" >> liftIO exitSuccess)

    editMenu = do
      canUndo <- textCanUndo editorId
      canRedo <- textCanRedo editorId
      if canUndo
        then whenM (menuItemShortcut "Undo" "Ctrl+Z") (editAction Undo)
        else menuItemDisabled "Undo"
      if canRedo
        then whenM (menuItemShortcut "Redo" "Ctrl+Shift+Z") (editAction Redo)
        else menuItemDisabled "Redo"
      menuSeparator
      whenM (menuItemShortcut "Cut" "Ctrl+X") (editAction Cut)
      whenM (menuItemShortcut "Copy" "Ctrl+C") (editAction Copy)
      whenM (menuItemShortcut "Paste" "Ctrl+V") (editAction Paste)
      menuSeparator
      whenM (menuItemShortcut "Select All" "Ctrl+A") (editAction SelectAll)

    viewMenu = do
      whenM
        (menuItem (if showStatus then "Hide Status Bar" else "Show Status Bar"))
        (setOpenMenu "" >> setShowStatus (not showStatus))
      menuSeparator
      whenM (menuItemShortcut "Zoom In" "Ctrl++") (setOpenMenu "" >> setZoom (min 4.0 (zoom * 1.1)))
      whenM (menuItemShortcut "Zoom Out" "Ctrl+-") (setOpenMenu "" >> setZoom (max 0.5 (zoom / 1.1)))
      whenM (menuItemShortcut "Reset Zoom" "Ctrl+0") (setOpenMenu "" >> setZoom 1.0)
      menuSeparator
      whenM (menuItem "Document Statistics") (setOpenMenu "" >> setStatusMsg (documentStats (documentText doc)))

    helpMenu = do
      whenM (menuItem "About nano-ui Notepad") (setOpenMenu "" >> setAboutOpen True)
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
    separator

    (editorResp, editorDoc) <-
      keyed docGen $
        textAreaDocumentWith'
          (grow . minW 240 . minH 160 . fontSizeScale zoom)
          doc
    -- respChanged also pulses for cursor moves, which hand back the same
    -- document.
    when (respChanged editorResp && not (sameDocument editorDoc doc)) $ do
      setDoc editorDoc
      setDocDirty True
    when (respId editorResp /= editorId) (setEditorId (respId editorResp))

    when showStatus $ do
      separator
      statusBar docPath docDirty doc statusMsg zoom

  --------------------------------------------------------------- overlays ---
  (aboutResp, _) <-
    modal aboutOpen "About" $ do
      heading "nano-ui Notepad"
      labelWith fontMuted "A menu-bar notepad built with nano-ui on SDL3."
      labelWith fontMuted "File, Edit, View and Help are wired to real actions."
      rowWith fillW $ do
        flex
        whenM (button "Close") (setAboutOpen False)
  when (respClicked aboutResp) (setAboutOpen False)

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
      btn <- menuButton' menuLabel isOpen
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

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

writeDocument :: FilePath -> TextDocument -> NanoUI Bool
writeDocument filePath doc = do
  result <-
    liftIO (try (TIO.writeFile filePath (documentText doc)) :: IO (Either SomeException ()))
  pure (isRight result)

-- | The status bar. It shows the line count, which the document keeps; the
-- word and character counts read the whole text, so View > Document
-- Statistics computes them when asked.
statusBar :: Text -> Bool -> TextDocument -> Text -> Float -> NanoUI ()
statusBar path dirty doc message zoomVal =
  rowWith (tight . gap 12 . fillW . padXY 8 4) $ do
    labelWith (tight . fontMuted) message
    flex
    labelWith (tight . fontMuted)
      ((if T.null path then "Untitled" else path) <> (if dirty then " *" else ""))
    labelWith (tight . fontMuted) ("Lines: " <> T.pack (show (documentLineCount doc)))
    labelWith (tight . fontMuted)
      ("Zoom: " <> T.pack (show (round (zoomVal * 100) :: Int)) <> "%")

documentStats :: Text -> Text
documentStats contents =
  "Words: "
    <> T.pack (show (length (T.words contents)))
    <> "   Chars: "
    <> T.pack (show (T.length contents))
