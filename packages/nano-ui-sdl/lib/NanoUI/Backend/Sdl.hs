-- | SDL3 backend: event loop, rendering, and application runners.
module NanoUI.Backend.Sdl
  ( RgbaImage (..)
  , SdlDebugSnapshot (..)
  , SdlEnv (..)
  , SdlOptions (..)
  , RenderDriver (..)
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
    -- * Window chrome
  , WindowChrome (..)
  , defaultWindowChrome
  , defaultResizeBorder
  , setWindowChrome
  , clearWindowChrome
  , setWindowTitle
  , setWindowSize
  , minimizeWindow
  , maximizeWindow
  , restoreWindow
  , toggleMaximized
  , windowMaximized
  , windowResizable
  , windowZoom
  , WindowDecorations (..)
  , setWindowDecorations
  , setWindowShadow
  , CaptionOptions (..)
  , defaultCaptionOptions
  , windowCaption
  , windowCaptionWith
  , setWindowTitleUi
  , setWindowChromeUi
  , minimizeWindowUi
  , toggleMaximizedUi
  , windowMaximizedUi
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

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Typeable (Typeable)
import NanoUI (NanoUI)
import NanoUI.Sdl.Internal.Runner (askSdlDebug, drawFrameWith, sdlDrawFrame, setSdlUiFont, setSdlUiScale)
import NanoUI.Sdl.Internal.Session (runSdlSession)
import NanoUI.Sdl.Internal.Debug (SdlDebugSnapshot (..))
import NanoUI.Sdl.Internal.Window (RenderDriver (..), RgbaImage (..), SdlEnv (..), SdlOptions (..), WindowDecorations (..), defaultSdlOptions, saveScreenshot, syncDisplay, windowZoom, withSdl, withSdlBench)
import NanoUI.Sdl.Internal.Dialog
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
import NanoUI.Sdl.Internal.Chrome
  ( WindowChrome (..)
  , clearWindowChrome
  , defaultResizeBorder
  , defaultWindowChrome
  , maximizeWindow
  , minimizeWindow
  , minimizeWindowUi
  , restoreWindow
  , setWindowChrome
  , setWindowChromeUi
  , setWindowSize
  , setWindowTitle
  , setWindowDecorations
  , setWindowShadow
  , setWindowTitleUi
  , toggleMaximized
  , toggleMaximizedUi
  , CaptionOptions (..)
  , defaultCaptionOptions
  , windowCaption
  , windowCaptionWith
  , windowMaximized
  , windowMaximizedUi
  , windowResizable
  )
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Internal.Font.Search (listFontFamilies)
import NanoUI.Testing (runFrameReduce)

-- | Open an SDL window and run a view until close or the quit predicate fires.
-- Owns and releases the native resources. Call from the application's display
-- thread; the view is rebuilt for each requested frame.
runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = runSdlSession options (`sdlDrawFrame` ui)

-- | Run a model-driven view, folding emitted messages through the update
-- function in emission order. Messages of other runtime types are ignored.
-- Use the adapters in "NanoUI.Emit" to emit changes from ordinary widgets.
runSdlAppReduce ::
  (Typeable msg, Eq model) =>
  SdlOptions
  -> (msg -> model -> model)
  -> model
  -> (model -> NanoUI ())
  -> IO ()
runSdlAppReduce options update model view = do
  modelRef <- newIORef model
  runSdlSession options $ \ctx env inp forceFull ->
    drawFrameWith ctx env inp forceFull $ do
      m <- readIORef modelRef
      (_, m', _, drawData, dirty) <- runFrameReduce update ctx inp m view
      (drawData, dirty) <$ writeIORef modelRef m'
