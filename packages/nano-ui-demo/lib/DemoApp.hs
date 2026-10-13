-- | What the SDL demo executables and their self-tests share: file-dialog
-- polling and a hidden window to drive a UI on.
module DemoApp
  ( useFileDialog
  , withHiddenWindow
  ) where

import Data.Foldable (for_)
import NanoUI (Input (..), NanoUI, Size (..), V2 (..), Var, WindowMode (..), WindowSettings (..), defaultWindowSettings, readVar, writeVar)
import NanoUI.Backend (emptyInput)
import NanoUI.Backend.Sdl
  ( FileDialogId
  , FileDialogResult (..)
  , SdlEnv
  , SdlOptions (..)
  , defaultSdlOptions
  , pollFileDialogUi
  , withSdl
  )
import NanoUI.Testing (Context, newPixelContext)

-- | Poll the dialog handle held in @dialog@; on completion clear it and hand
-- the chosen paths to the caller. Anything other than 'FileDialogPending'
-- dismisses the handle, so each result is consumed exactly once.
useFileDialog ::
  Var (Maybe FileDialogId) ->
  ([FilePath] -> NanoUI ()) ->
  NanoUI ()
useFileDialog dialog onPaths = do
  pending <- readVar dialog
  for_ pending $ \did ->
    pollFileDialogUi did >>= \case
      FileDialogPending -> pure ()
      FileDialogSelected paths -> onPaths paths >> writeVar dialog Nothing
      _done -> writeVar dialog Nothing

-- | Run @k@ on a hidden, fixed @w@ by @h@ SDL window over a pixel-snapped
-- context, with @opts@ applied to the window options. @k@ also gets the
-- event-free input for that size with the pointer at @mouse@.
withHiddenWindow ::
  Float ->
  Float ->
  V2 ->
  (SdlOptions -> SdlOptions) ->
  (Context -> SdlEnv -> Input -> IO a) ->
  IO a
withHiddenWindow w h mouse opts k = do
  ctx0 <- newPixelContext
  let base = defaultSdlOptions {sdlWindowSettings = defaultWindowSettings {wsMode = Hidden, wsSize = Size w h, wsResizable = False}}
  withSdl (opts base) ctx0 $ \ctx env ->
    k ctx env emptyInput {inputWindowSize = Size w h, inputMousePos = mouse}
