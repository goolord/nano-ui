-- | SDL3 backend: event loop, rendering, and application runners.
module NanoUI.Backend.Sdl
  ( RgbaImage (..)
  , SdlDebugSnapshot (..)
  , SdlEnv (..)
  , SdlOptions (..)
  , askSdlDebug
  , setSdlUiFont
  , isDebugActive
  , newSdlDebugSampler
  , readSdlDebug
  , takeDebugLive
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
  , newSdlContext
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
import NanoUI.Sdl.Runner (askSdlDebug, drawReduceEff, newSdlContext, sdlDrawFrame, setSdlUiFont)
import NanoUI.Sdl.Session (runSdlSession)
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , isDebugActive
  , newSdlDebugSampler
  , readSdlDebug
  , takeDebugLive
  )
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
import NanoUI.Testing (Context, registerImage, runEff, withTheme)

runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = do
  ctx <- sdlContext options
  runSdlSession options ctx (const (pure ())) (sdlAppShouldQuit options) $ \c ->
    sdlDrawFrame c ui

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
  ctx0 <- newSdlContext
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
