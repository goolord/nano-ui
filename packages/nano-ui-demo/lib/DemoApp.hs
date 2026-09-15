-- | File-dialog polling shared by the SDL demo executables.
module DemoApp
  ( useFileDialog
  ) where

import Data.Foldable (for_)
import NanoUI (NanoUI)
import NanoUI.Backend.Sdl
  ( FileDialogId
  , FileDialogResult (..)
  , pollFileDialogUi
  )

-- | Poll a pending dialog handle; on completion clear it and hand the chosen
-- paths to the caller. Anything other than 'FileDialogPending' dismisses the
-- handle, so each result is consumed exactly once.
useFileDialog ::
  Maybe FileDialogId ->
  (Maybe FileDialogId -> NanoUI ()) ->
  ([FilePath] -> NanoUI ()) ->
  NanoUI ()
useFileDialog mdid clear onPaths =
  for_ mdid $ \did ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected paths -> onPaths paths >> clear Nothing
      _done -> clear Nothing
