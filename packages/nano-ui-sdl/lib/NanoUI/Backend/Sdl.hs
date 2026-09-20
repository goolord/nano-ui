-- | SDL3 backend: event loop, rendering, and application runners.
module NanoUI.Backend.Sdl
  ( RgbaImage (..)
  , SdlDebugSnapshot (..)
  , SdlEnv (..)
  , SdlOptions (..)
  , askSdlDebug
  , setSdlUiFont
  , setSdlUiScale
  , defaultSdlOptions
  , FileFilter (..)
  , FileDialogOptions (..)
  , FileDialogId (..)
  , FileDialogResult (..)
  , defaultFileDialogOptions
  , openFileDialog
  , saveFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog
  , askOpenFileDialog
  , askSaveFileDialog
  , askOpenFolderDialog
  , pollFileDialogUi
  , NanoUIFont (..)
  , listFontFamilies
  , runSdlApp
  , runSdlAppReduce
  , sdlDrawFrame
  , syncDisplay
  , withSdl
  , withSdlBench
  , saveScreenshot
  ) where

import Control.Monad (unless)
import Data.Foldable (foldlM)
import Data.IORef (newIORef)
import Data.Primitive.SmallArray (SmallArray)
import Data.Typeable (Typeable)
import NanoUI (NanoUI)
import NanoUI.Sdl.Runner (askSdlDebug, drawReduceEff, sdlDrawFrame, setSdlUiFont, setSdlUiScale)
import NanoUI.Sdl.Session (runSdlSession)
import NanoUI.Sdl.Debug (SdlDebugSnapshot (..))
import NanoUI.Sdl.Window (RgbaImage (..), SdlEnv (..), SdlOptions (..), defaultSdlOptions, saveScreenshot, syncDisplay, withSdl, withSdlBench)
import NanoUI.Sdl.Dialog
  ( FileDialogId (..)
  , FileDialogOptions (..)
  , FileDialogResult (..)
  , FileFilter (..)
  , askOpenFileDialog
  , askOpenFolderDialog
  , askSaveFileDialog
  , defaultFileDialogOptions
  , openFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog
  , pollFileDialogUi
  , saveFileDialog
  )
import NanoUI.Sdl.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Font.Search (listFontFamilies)
import NanoUI.Testing (Context, newPixelContext, registerImage, runEff, withTheme)

-- | Open an SDL window and run a view until close or the quit predicate fires.
-- Owns and releases the native resources. Call from the application's display
-- thread; the view is rebuilt for each requested frame.
runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = do
  ctx <- sdlContext options
  runSdlSession options ctx (const (pure ())) (sdlAppShouldQuit options) $ \c ->
    sdlDrawFrame c ui

-- | Run a model-driven view, folding emitted messages through the update
-- function in emission order. Messages of other runtime types are ignored.
-- Use the widgets in "NanoUI.Emit" to emit changes.
runSdlAppReduce ::
  (Typeable msg, Eq model) =>
  SdlOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppReduce options update model view = do
  ctx <- sdlContext options
  modelRef <- newIORef model
  runSdlSession options ctx (const (pure ())) (sdlAppShouldQuit options) $
    drawReduceEff runEff update modelRef view

sdlContext :: SdlOptions -> IO Context
sdlContext options = do
  ctx0 <- newPixelContext
  themed <- maybe (pure ctx0) (withTheme ctx0) (sdlAppTheme options)
  ok <- registerImages themed (sdlAppImages options)
  unless ok $ fail "registerImage failed"
  pure themed

registerImages :: Context -> SmallArray RgbaImage -> IO Bool
registerImages ctx images =
  foldlM (\ok img -> if ok then registerRgbaImage ctx img else pure False) True images

registerRgbaImage :: Context -> RgbaImage -> IO Bool
registerRgbaImage ctx img =
  registerImage
    ctx
    (rgbaImageId img)
    (rgbaImageWidth img)
    (rgbaImageHeight img)
    (rgbaImagePixels img)
