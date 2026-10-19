-- | Files: open, edit and save a text file, with native dialogs and drag and drop.
--
-- A native file dialog does not block the view. 'askOpenFileDialog' opens it
-- and returns a handle immediately; the view keeps the handle in state and
-- polls it with 'pollFileDialogUi' every frame until the user picks a file or
-- cancels. A result is delivered once, so the poll that receives it also
-- clears the handle ('pollDialog' below). The loop sleeps while the dialog is
-- open and wakes when it closes.
--
-- Reading the chosen file is slow work, so it runs on a worker thread with
-- 'useTaskStatus', keyed by the path (see Tasks.hs). The file's text then
-- lives in an ordinary 'Var' that the 'textArea' edits. A flag records
-- unsaved changes, and 'setWindowTitleUi' shows it in the title bar; it does
-- nothing while the title is unchanged, so it runs every frame.
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

-- | State the view needs beyond what positional Vars hold: the two pending
-- dialogs (a handle is no positional slot type) and the file-reading job.
data App = App
  { appOpenDialog :: !(Var (Maybe FileDialogId))
  , appSaveDialog :: !(Var (Maybe FileDialogId))
  , appRead :: !(Task Text Text)
  }

newApp :: IO App
newApp = App <$> newVar Nothing <*> newVar Nothing <*> newTask

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
  text <- useVar ""
  path <- useVar "" -- "" while the text has never been saved
  dirty <- useVar False
  reading <- useVar "" -- the file being read, or ""
  status <- useVar "Open a file, or drop one on the strip below."
  hovering <- useVar False

  let askOpen = writeVar (appOpenDialog app) . Just =<< askOpenFileDialog defaultFileDialogOptions
      askSave = writeVar (appSaveDialog app) . Just =<< askSaveFileDialog defaultFileDialogOptions
      -- Writing a small file is quick, so it happens right here. A large one
      -- would go through a task like the read below.
      writeTo file = do
        contents <- readVar text
        result <- liftIO (try (BS.writeFile file (TE.encodeUtf8 contents)))
        case result of
          Left (err :: IOException) -> writeVar status ("Could not save: " <> T.pack (displayException err))
          Right () -> do
            writeVar path (T.pack file)
            writeVar dirty False
            writeVar status ("Saved " <> T.pack file)
      save = do
        saved <- readVar path
        if T.null saved then askSave else writeTo (T.unpack saved)

  pollDialog (appOpenDialog app) (writeVar reading . T.pack)
  pollDialog (appSaveDialog app) writeTo

  whenM (shortcut (ctrl <> key 'o')) askOpen
  whenM (shortcut (ctrl <> key 's')) save

  -- The read job runs only while there is a file to read. Once it finishes,
  -- clearing 'reading' stops calling 'useTaskStatus', which releases the job.
  file <- readVar reading
  scope . unless (T.null file) $ do
    job <- useTaskStatus (appRead app) file (readUtf8 (T.unpack file))
    case job of
      TaskRunning _ -> pure ()
      TaskDone contents -> do
        writeVar text contents
        writeVar path file
        writeVar dirty False
        writeVar reading ""
        writeVar status ("Opened " <> file)
      TaskFailed err _ -> do
        writeVar reading ""
        writeVar status ("Could not open: " <> T.pack (displayException err))

  saved <- readVar path
  unsaved <- readVar dirty
  let name = if T.null saved then "Untitled" else T.pack (takeFileName (T.unpack saved))
  setWindowTitleUi (name <> (if unsaved then "*" else "") <> " — Files")

  columnWith (padAll 12 . gap 8 . grow) $ do
    rowWith (tight . gap 8 . alignMid . fillW) $ do
      whenM (button "Open…") askOpen
      whenM (button "Save As…") askSave
      scope (unless (T.null file) spinner)
      muted =<< readVar status
    -- The zone reports hover and drops for its own rect. Its hover is known
    -- only after it is declared, so the label shows what was stored last;
    -- a change runs the view again to show it.
    over <- readVar hovering
    (_, _, target) <-
      dropZone (fillW . padXY 12 8) $
        labelWith fontMuted (if over then "Release to open the file" else "Drop a file here to open it")
    writeVar hovering (dropHovered target)
    when (dropReceived target) $ for_ (listToMaybe (dropFiles target)) (writeVar reading)
    -- respChanged reports an edit, not a caret move.
    edited <- textAreaWith' grow text
    when (respChanged edited) (writeVar dirty True)

-- | Poll a pending dialog once a frame. When it finishes, clear the handle
-- and pass on the first chosen path; a cancel just clears it.
pollDialog :: Var (Maybe FileDialogId) -> (FilePath -> NanoUI ()) -> NanoUI ()
pollDialog pending onPath =
  readVar pending >>= \handle -> for_ handle $ \dialog ->
    pollFileDialogUi dialog >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected chosen -> writeVar pending Nothing >> for_ (listToMaybe chosen) onPath
      _ -> writeVar pending Nothing

-- | Read a file as UTF-8, turning malformed bytes into U+FFFD instead of
-- failing. The decoded 'Text' is strict, so the worker does all the work.
readUtf8 :: FilePath -> IO Text
readUtf8 file = TE.decodeUtf8With (\_ _ -> Just '\xFFFD') <$> BS.readFile file
