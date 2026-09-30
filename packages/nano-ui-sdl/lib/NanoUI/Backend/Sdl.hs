-- | SDL3 backend: event loop, rendering, and application runners.
module NanoUI.Backend.Sdl
  ( RgbaImage (..)
  , SdlDebugSnapshot (..)
  , SdlOptions (..)
  , RenderDriver (..)
  , askSdlDebug
  , setSdlUiFont
  , setSdlUiScale
  , defaultSdlOptions
  , FileFilter (..)
  , FileDialogOptions (..)
  , FileDialogId
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
  , peekFileDialogUi
    -- * Explicit sessions

    -- | View-side operations find the open session themselves. For the
    -- IO-level ones above ('openFileDialog', 'pollFileDialog') and below, or
    -- to hand the session to another thread, take it with 'askSdlEnv'.
  , SdlEnv
  , askSdlEnv
  , sdlDebugSnapshot
    -- * External URLs
  , openUrl
    -- * The window

    -- | 'WindowSettings' and the view-side window functions come from the
    -- core ("NanoUI"): 'NanoUI.askWindow', 'NanoUI.setWindowTitleUi',
    -- 'NanoUI.moveWindowUi', 'NanoUI.quitUi' and others. The rest of this
    -- section is specific to SDL.
  , WindowSettings (..)
  , defaultWindowSettings
  , WindowPosition (..)
  , WindowMode (..)
  , windowZoom
  , windowResizable
  , WindowDecorations (..)
  , setWindowDecorations
  , setWindowDecorationsUi
  , setWindowShadow
  , setWindowShadowUi

    -- * Window chrome
  , WindowChrome (..)
  , defaultWindowChrome
  , defaultResizeBorder
  , setWindowChrome
  , clearWindowChrome
  , clearWindowChromeUi
  , CaptionOptions (..)
  , defaultCaptionOptions
  , windowCaption
  , windowCaptionWith
  , setWindowChromeUi
  , NanoUIFont (..)
  , listFontFamilies
  , runSdlApp
  , runSdlAppReduce
  , sdlDrawFrame
  , syncDisplay
  , withSdl
  , withSdlBench
  , saveScreenshot
  , captureScreenshot
  ) where

import Data.IORef (newIORef, readIORef, writeIORef)
import NanoUI (NanoUI, WindowMode (..), WindowPosition (..), WindowSettings (..), defaultWindowSettings)
import NanoUI.Sdl.Internal.Runner (askSdlDebug, drawFrameWith, sdlDebugSnapshot, sdlDrawFrame, setSdlUiFont, setSdlUiScale)
import NanoUI.Sdl.Internal.Session (runSdlSession)
import NanoUI.Sdl.Internal.Debug (SdlDebugSnapshot (..))
import NanoUI.Sdl.Internal.Window (RenderDriver (..), RgbaImage (..), SdlEnv, SdlOptions (..), askSdlEnv, captureScreenshot, defaultSdlOptions, saveScreenshot, syncDisplay, windowZoom, withSdl, withSdlBench)
import NanoUI.Sdl.Internal.Dialog
import NanoUI.Sdl.Internal.Chrome
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Internal.Font.Search (listFontFamilies)
import NanoUI.Sdl.Internal.Url (openUrl)
import NanoUI.Emit (NanoUIE, runFrameReduce)

-- | Open an SDL window and run a view until close or the quit predicate fires.
-- Owns and releases the native resources. Call from the application's display
-- thread; the view is rebuilt for each requested frame. Allocate owned state
-- once in IO first and pass it to the view:
--
-- > main = do
-- >   state <- newEditor
-- >   runSdlApp defaultSdlOptions (editorUi state)
--
-- SDL-specific view operations (file dialogs, fonts, window chrome, debug
-- data) act on this session without being handed it.
runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = runSdlSession options (`sdlDrawFrame` ui)

-- | Run a model-driven view, folding emitted messages through the update
-- function in emission order. The view's message type must match the reducer.
-- Use the adapters in "NanoUI.Emit" to emit changes from ordinary widgets.
runSdlAppReduce ::
  Eq model =>
  SdlOptions
  -> (msg -> model -> model)
  -> model
  -> (model -> NanoUIE msg ())
  -> IO ()
runSdlAppReduce options update model view = do
  modelRef <- newIORef model
  runSdlSession options $ \ctx env inp forceFull ->
    drawFrameWith ctx env inp forceFull $ do
      m <- readIORef modelRef
      (_, m', _, drawData, dirty) <- runFrameReduce update ctx inp m view
      (drawData, dirty) <$ writeIORef modelRef m'
