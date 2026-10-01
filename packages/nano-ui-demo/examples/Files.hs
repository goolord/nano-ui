-- | Files: open, edit and save a text file, with native dialogs and drag and drop.
--
-- A native file dialog does not block the view. 'askOpenFileDialog' opens it
-- and hands back a handle at once; the view keeps the handle in state and
-- polls it with 'pollFileDialogUi' every frame until the user picks a file or
-- cancels. A result is delivered exactly once, so the poll that sees it also
-- forgets the handle ('pollDialog' below). The loop sleeps while the dialog is
-- open and wakes when it closes.
--
-- Reading the chosen file is slow work, so it runs on a worker thread with
-- 'useTaskStatus', keyed by the path (see Tasks.hs). The file's text then
-- lives in an ordinary 'useText' hook that the 'textArea' edits. A flag
-- records unsaved changes, and 'setWindowTitleUi' shows it in the title bar;
-- the setter does nothing while the title is unchanged, so it runs every frame.
--
-- Run it with @cabal run nano-ui-example-files@.
module Main (main) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (unless, when)
import qualified Data.ByteString as BS
import Data.Foldable (for_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import NanoUI
import NanoUI.Backend.Sdl
  ( FileDialogId
  , FileDialogResult (..)
  , SdlOptions (..)
  , askOpenFileDialog
  , askSaveFileDialog
  , defaultFileDialogOptions
  , defaultSdlOptions
  , pollFileDialogUi
  , runSdlApp
  )
import NanoUI.Shortcut (ctrl, key)
import System.FilePath (takeFileName)

-- | The handles that positional hooks cannot hold: the two pending dialogs
-- and the file-reading job.
data App = App
  { appOpenDialog :: !(StateCell (Maybe FileDialogId))
  , appSaveDialog :: !(StateCell (Maybe FileDialogId))
  , appRead :: !(Task Text Text)
  }

newApp :: IO App
newApp = App <$> newState Nothing <*> newState Nothing <*> newTask

main :: IO ()
main = do
  app <- newApp
  runSdlApp
    defaultSdlOptions
      { sdlAppShouldQuit = pressedOnceIn KeyEscape
      , sdlWindowSettings = defaultWindowSettings {wsTitle = "Files", wsSize = Size 760 560}
      }
    (view app)

view :: App -> NanoUI ()
view app = do
  (text, setText) <- useText ""
  (path, setPath) <- useText "" -- "" while the text has never been saved
  (dirty, setDirty) <- useFlag False
  (reading, setReading) <- useText "" -- the file being read, or ""
  (status, setStatus) <- useText "Open a file, or drop one on the strip below."
  (hovering, setHovering) <- useFlag False
  (openDlg, setOpenDlg) <- useState (appOpenDialog app)
  (saveDlg, setSaveDlg) <- useState (appSaveDialog app)

  let askOpen = setOpenDlg . Just =<< askOpenFileDialog defaultFileDialogOptions
      askSave = setSaveDlg . Just =<< askSaveFileDialog defaultFileDialogOptions
      -- Writing a small file is quick, so it happens right here. A large one
      -- would go through a task like the read below.
      writeTo file = do
        result <- liftIO (try (BS.writeFile file (TE.encodeUtf8 text)))
        case result of
          Left (err :: IOException) -> setStatus ("Could not save: " <> T.pack (displayException err))
          Right () -> do
            setPath (T.pack file)
            setDirty False
            setStatus ("Saved " <> T.pack file)
      save = if T.null path then askSave else writeTo (T.unpack path)

  pollDialog openDlg setOpenDlg (setReading . T.pack)
  pollDialog saveDlg setSaveDlg writeTo

  whenM (shortcut (ctrl <> key 'o')) askOpen
  whenM (shortcut (ctrl <> key 's')) save

  -- The read job runs only while there is a file to read. Once it finishes,
  -- clearing 'reading' stops calling the hook, which releases the job.
  scope . unless (T.null reading) $ do
    job <- useTaskStatus (appRead app) reading (readUtf8 (T.unpack reading))
    case job of
      TaskRunning _ -> pure ()
      TaskDone contents -> do
        setText contents
        setPath reading
        setDirty False
        setReading ""
        setStatus ("Opened " <> reading)
      TaskFailed err _ -> do
        setReading ""
        setStatus ("Could not open: " <> T.pack (displayException err))

  let name = if T.null path then "Untitled" else T.pack (takeFileName (T.unpack path))
  setWindowTitleUi (name <> (if dirty then "*" else "") <> " — Files")

  columnWith (padAll 12 . gap 8 . grow) $ do
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      whenM (button "Open…") askOpen
      whenM (button "Save As…") askSave
      scope (unless (T.null reading) spinner)
      muted status
    -- The zone reports hover and drops for its own rect. Its hover is known
    -- only after it is declared, so the label shows last frame's, kept in
    -- a hook.
    (_, _, target) <-
      dropZone (fillW . padXY 12 8) $
        labelWith fontMuted (if hovering then "Release to open the file" else "Drop a file here to open it")
    when (dropHovered target /= hovering) (setHovering (dropHovered target))
    when (dropReceived target) $ for_ (listToMaybe (dropFiles target)) setReading
    (editor, text') <- textAreaWith' grow text
    -- respChanged also fires for caret moves, which return the same text.
    when (respChanged editor && text' /= text) $ do
      setText text'
      setDirty True

-- | Poll a pending dialog once a frame. When it finishes, forget the handle
-- and pass on the first chosen path; a cancel just forgets it.
pollDialog :: Maybe FileDialogId -> (Maybe FileDialogId -> NanoUI ()) -> (FilePath -> NanoUI ()) -> NanoUI ()
pollDialog pending setPending onPath =
  for_ pending $ \dialog ->
    pollFileDialogUi dialog >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected chosen -> setPending Nothing >> for_ (listToMaybe chosen) onPath
      _ -> setPending Nothing

-- | Read a file as UTF-8, turning malformed bytes into U+FFFD instead of
-- failing. The decoded 'Text' is strict, so the worker does all the work.
readUtf8 :: FilePath -> IO Text
readUtf8 file = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') <$> BS.readFile file
