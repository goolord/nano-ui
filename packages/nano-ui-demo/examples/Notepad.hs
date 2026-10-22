-- | A small notepad application built on the SDL3 backend.
--
-- It is self-contained: the menu bar is implemented locally on
-- top of the generic 'popup' primitive, and the whole application is one
-- 'NanoUI' function over local state in Vars.
--
-- Run with @cabal run nano-ui-sdl-notepad@.
module Notepad
  ( main
  , NotepadState
  , newNotepad
  , notepadUi
  , writeDocument
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Either (isRight)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import NanoUI
import NanoUI.Backend.Sdl
import NanoUI.Shortcut

--------------------------------------------------------------------------------
-- Application entry point
--------------------------------------------------------------------------------

main :: IO ()
main = do
  state <- newNotepad
  runSdlApp
    defaultSdlOptions
      { -- A close request goes to the view, which may ask about unsaved changes.
        sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui Notepad", wsSize = Size 1000 720, wsExitOnCloseRequest = False}
      , sdlAppTheme = Just tomorrowNightMinDarkTheme
      }
    (notepadUi state)

--------------------------------------------------------------------------------
-- The application
--------------------------------------------------------------------------------

data NotepadState = NotepadState
  !(Var TextDocument)
  !(Var WidgetId)
  !(Var (Maybe FileDialogId))
  !(Var (Maybe FileDialogId))

-- | A notepad's private document and dialog state. Allocate one per
-- instance before starting the SDL session, and draw it with 'notepadUi'.
newNotepad :: IO NotepadState
newNotepad =
  NotepadState
    <$> newVar emptyDocument
    <*> newVar (WidgetId 0)
    <*> newVar Nothing
    <*> newVar Nothing

notepadUi :: NotepadState -> NanoUI ()
notepadUi (NotepadState doc editorId openDlg saveDlg) = do
  ------------------------------------------------------------------ state ---
  docPath <- useVar ("" :: Text)
  docDirty <- useVar False
  docGen <- useVar (0 :: Int)
  openMenu <- useVar ("" :: Text)
  statusMsg <- useVar ("Ready" :: Text)
  showStatus <- useVar True
  aboutOpen <- useVar False
  confirmExit <- useVar False
  zoom <- useVar (1.0 :: Float)

  ----------------------------------------------------------- file dialogs ---
  useFileDialog openDlg $ \chosenPaths ->
    for_ (listToMaybe chosenPaths) $ \filePath -> do
      loaded <-
        liftIO (try (BS.readFile filePath) :: IO (Either SomeException BS.ByteString))
      case loaded of
        Left _ -> writeVar statusMsg ("Could not open " <> T.pack filePath)
        Right raw -> do
          -- Lenient decode: malformed bytes become U+FFFD instead of throwing.
          writeVar doc (textDocument (TE.decodeUtf8With (\_ _ -> Just '\xFFFD') raw))
          -- A new editor, without the old document's caret and undo history.
          modifyVar docGen (+ 1)
          writeVar docPath (T.pack filePath)
          writeVar docDirty False
          writeVar statusMsg ("Opened " <> T.pack filePath)
  useFileDialog saveDlg $ \chosenPaths ->
    for_ (listToMaybe chosenPaths) $ \filePath -> do
      saved <- writeDocument filePath =<< readVar doc
      if saved
        then do
          writeVar docPath (T.pack filePath)
          writeVar docDirty False
          writeVar statusMsg ("Saved " <> T.pack filePath)
        else writeVar statusMsg ("Could not save " <> T.pack filePath)

  ------------------------------------------------------------- menu data ---
  let
    newDocument = do
      writeVar doc emptyDocument
      writeVar docPath ""
      modifyVar docGen (+ 1)
      writeVar docDirty False
      writeVar statusMsg "New document"

    saveDocument forceDialog = do
      path <- readVar docPath
      if forceDialog || T.null path
        then writeVar saveDlg . Just =<< askSaveFileDialog defaultFileDialogOptions
        else do
          saved <- writeDocument (T.unpack path) =<< readVar doc
          if saved
            then do
              writeVar docDirty False
              writeVar statusMsg ("Saved " <> path)
            else writeVar statusMsg ("Could not save " <> path)

    zoomIn = modifyVar zoom (\z -> min 4.0 (z * 1.1))
    zoomOut = modifyVar zoom (\z -> max 0.5 (z / 1.1))

    -- A menu row and its action; running it closes the menu.
    item menuRow action = whenM menuRow (writeVar openMenu "" >> action)
    editItem name chord cmd =
      item (menuItemShortcut name chord) (readVar editorId >>= \editor -> runTextCommand editor cmd)

    -- Commands whose chords work with every menu closed. They are bound
    -- below, and their rows show the chord.
    newCmd = ("New", cmdOrCtrl <> key 'n', newDocument)
    openCmd = ("Open...", cmdOrCtrl <> key 'o', writeVar openDlg . Just =<< askOpenFileDialog defaultFileDialogOptions)
    saveCmd = ("Save", cmdOrCtrl <> key 's', saveDocument False)
    saveAsCmd = ("Save As...", cmdOrCtrl <> shift <> key 's', saveDocument True)
    -- Quit, or ask first when there are unsaved changes. The question puts
    -- the About dialog away, so one modal shows at a time.
    exitApp = do
      dirty <- readVar docDirty
      if dirty then writeVar aboutOpen False >> writeVar confirmExit True else quitUi
    exitCmd = ("Exit", cmdOrCtrl <> key 'q', exitApp)
    zoomInCmd = ("Zoom In", cmdOrCtrl <> key '=', zoomIn)
    zoomOutCmd = ("Zoom Out", cmdOrCtrl <> key '-', zoomOut)
    resetZoomCmd = ("Reset Zoom", cmdOrCtrl <> key '0', writeVar zoom 1.0)
    commandItem (name, chord, action) = item (menuItemShortcut name chord) action

    fileMenu = do
      commandItem newCmd
      commandItem openCmd
      commandItem saveCmd
      commandItem saveAsCmd
      menuSeparator
      commandItem exitCmd

    editMenu = do
      editor <- readVar editorId
      canUndo <- textCanUndo editor
      canRedo <- textCanRedo editor
      if canUndo then editItem "Undo" (cmdOrCtrl <> key 'z') Undo else menuItemDisabled "Undo"
      if canRedo then editItem "Redo" (cmdOrCtrl <> shift <> key 'z') Redo else menuItemDisabled "Redo"
      menuSeparator
      editItem "Cut" (cmdOrCtrl <> key 'x') Cut
      editItem "Copy" (cmdOrCtrl <> key 'c') Copy
      editItem "Paste" (cmdOrCtrl <> key 'v') Paste
      menuSeparator
      editItem "Select All" (cmdOrCtrl <> key 'a') SelectAll

    viewMenu = do
      statusShown <- readVar showStatus
      item (menuItem (if statusShown then "Hide Status Bar" else "Show Status Bar")) (writeVar showStatus (not statusShown))
      menuSeparator
      commandItem zoomInCmd
      commandItem zoomOutCmd
      commandItem resetZoomCmd
      menuSeparator
      item (menuItem "Document Statistics") (writeVar statusMsg . documentStats . documentText =<< readVar doc)

    helpMenu = do
      item (menuItem "About nano-ui Notepad") (writeVar aboutOpen True)
      menuItemDisabled "nano-ui on GitHub"

  ------------------------------------------------------------- shortcuts ---
  -- A menu row's chord works only while its menu is open, so the commands'
  -- chords are also bound here, ahead of the rows, and close an open menu as
  -- the rows do; so does a close request. The editor handles its own editing
  -- chords. Ctrl++ is Ctrl+Shift+= on a US layout, or keypad plus.
  for_ [newCmd, openCmd, saveCmd, saveAsCmd, exitCmd, zoomInCmd, zoomOutCmd, resetZoomCmd] $
    \(_, chord, action) -> item (shortcut chord) action
  item (or <$> traverse shortcut [cmdOrCtrl <> shift <> key '=', cmdOrCtrl <> key '+']) zoomIn
  item (winCloseRequested <$> askWindow) exitApp

  --------------------------------------------------------------- layout ---
  columnWith (grow . gap 0) $ do
    menuBar
      openMenu
      [ ("File", fileMenu)
      , ("Edit", editMenu)
      , ("View", viewMenu)
      , ("Help", helpMenu)
      ]
    separator

    -- Read after the menus, which can replace the document or change the
    -- zoom.
    gen <- readVar docGen
    zoomVal <- readVar zoom
    editorResp <-
      withKey gen $
        textAreaDocumentWith'
          (grow . minW 240 . minH 160 . fontSizeScale zoomVal)
          doc
    -- respChanged reports an edit, not a caret move.
    when (respChanged editorResp) (writeVar docDirty True)
    writeVar editorId (respId editorResp)

    whenM (readVar showStatus) $ do
      separator
      statusBar docPath docDirty doc statusMsg zoomVal

  -------------------------------------------------------------- the window ---
  -- Cheap to set every frame: the window updates only when the title changes.
  -- Set after the editor, which marks the document changed.
  path <- readVar docPath
  dirty <- readVar docDirty
  setWindowTitleUi $
    (if T.null path then "Untitled" else path) <> (if dirty then " *" else "") <> " - nano-ui Notepad"

  --------------------------------------------------------------- overlays ---
  modal aboutOpen "About" $ do
    heading "nano-ui Notepad"
    labelWith fontMuted "A menu-bar notepad built with nano-ui on SDL3."
    labelWith fontMuted "File, Edit, View and Help are wired to real actions."
    rowWith fillW $ do
      flex
      whenM (button "Close") (writeVar aboutOpen False)
  modal confirmExit "Unsaved changes" $ do
    label "Discard the changes to this document and exit?"
    rowWith fillW $ do
      flex
      whenM (button "Discard") quitUi
      whenM (button "Cancel") (writeVar confirmExit False)

--------------------------------------------------------------------------------
-- Local menu-bar widget
--------------------------------------------------------------------------------

-- | A horizontal menu bar. @openMenu@ holds the label of the currently open
-- drop-down (the empty text when every menu is closed). Each entry pairs a
-- label with its drop-down body; item actions are expected to close the menu.
menuBar :: Var Text -> [(Text, NanoUI ())] -> NanoUI ()
menuBar openMenu entries = do
  current <- readVar openMenu
  rowWith (tight . fillW . fixedH 28) $ do
    for_ entries $ \(menuLabel, body) -> do
      let isOpen = current == menuLabel
      -- Each title covers the bar's height, its text in the middle.
      btn <- menuButtonWith' fillH menuLabel isOpen
      when (respClicked btn) (writeVar openMenu (if isOpen then "" else menuLabel))
      when (not isOpen && not (T.null current) && respHovered btn) (writeVar openMenu menuLabel)
      let cfg = (defaultPopupConfig (AnchorRect (respRect btn))) {cfgPlacement = PlacementBelow, cfgOffset = 0}
      -- The open flag is derived from the open menu's name, so the popup
      -- gets a constant and closing it is written here.
      (popupResp, _) <- popup' (constVar isOpen) cfg (columnWith (tight . gap 0) body)
      when (respClicked popupResp) (writeVar openMenu "")

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Write the document as UTF-8 with its own line ends, the bytes the editor
-- reads back.
writeDocument :: FilePath -> TextDocument -> NanoUI Bool
writeDocument filePath doc = do
  result <-
    liftIO (try (BS.writeFile filePath (TE.encodeUtf8 (documentText doc))) :: IO (Either SomeException ()))
  pure (isRight result)

-- | The status bar. It shows the line count, which the document keeps; the
-- word and character counts read the whole text, so View > Document
-- Statistics computes them when asked.
statusBar :: Var Text -> Var Bool -> Var TextDocument -> Var Text -> Float -> NanoUI ()
statusBar docPath docDirty docVar statusMsg zoomVal = do
  path <- readVar docPath
  dirty <- readVar docDirty
  doc <- readVar docVar
  message <- readVar statusMsg
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
