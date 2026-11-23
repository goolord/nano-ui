-- | What the widget cookbook ("Demo") takes from the browser backend. The
-- page has no font directories, no file system and no native dialogs, so
-- the font list is the bundled Inter, the data files are compiled into the
-- module, and the File Dialogs section says what it would do on the
-- desktop. The SDL build has a module of the same name in @examples/sdl@
-- with the same exports.
module DemoPlatform
  ( backendLabel
  , uiFontFamilies
  , setUiFontFamily
  , readDataFile
  , FileDialogState
  , newFileDialogState
  , fileDialogSection
  , debugStats
  , diagnosticsRows
  , formatExplainRows
  ) where

import Data.ByteString qualified as BS
import Data.Text (Text)
import NanoUI
import NanoUI.Backend.SdlWeb (formatExplainRows)

import DemoFiles (dataFiles)

-- | The backend's name, shown in the cookbook's title bar.
backendLabel :: Text
backendLabel = "Web"

-- | The fonts the page has: the bundled Inter.
uiFontFamilies :: IO [Text]
uiFontFamilies = pure ["Inter"]

-- | Inter is the only family, so there is nothing to change to.
setUiFontFamily :: Text -> NanoUI ()
setUiFontFamily _ = pure ()

-- | A file from the package's @data@ directory, by its path there
-- (@data/lick.gif@). A path that was not embedded throws, as reading a
-- missing file does.
readDataFile :: FilePath -> IO BS.ByteString
readDataFile path =
  maybe (ioError (userError (path <> ": not embedded in the web build"))) pure $
    lookup path [("data/" <> name, bytes) | (name, bytes) <- dataFiles]

-- | The browser has no native file dialogs, so there is no state to keep.
data FileDialogState = FileDialogState

-- | The (empty) state.
newFileDialogState :: IO FileDialogState
newFileDialogState = pure FileDialogState

-- | The section the desktop build fills with an Open File button.
fileDialogSection :: FileDialogState -> Var Text -> NanoUI ()
fileDialogSection FileDialogState _ = do
  heading "File Dialogs"
  muted "Native file dialogs open from the desktop build (nano-ui-sdl). A page has none."

-- | The browser backend does not sample frame statistics.
debugStats :: NanoUI ()
debugStats = muted "Frame statistics come from the desktop backend's sampler, which the web build does not have."

-- | The Diagnostics tab's rows: none in the browser.
diagnosticsRows :: NanoUI ()
diagnosticsRows = muted "The web build does not sample frame statistics. Run nano-ui-sdl-demo for them."
