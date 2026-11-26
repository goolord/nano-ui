-- |
-- Module      : NanoUI.Backend.Sdl
-- Description : SDL3 desktop backend for nano-ui
-- Copyright   : (c) 2026 Zachary Churchill
-- License     : MIT
-- Maintainer  : zacharyachurchill@gmail.com
--
-- Desktop SDL3 backend with HarfBuzz/SDL3_ttf TrueType font rendering,
-- system font discovery, native file dialogs, client-side window chrome,
-- and GPU fragment shader support.
--
-- Call 'runSdlApp' with 'defaultSdlOptions' and a 'NanoUI' view to open a
-- window, or 'runSdlAppReduce' for an Elm-style "NanoUI.Emit" reducer loop.
module NanoUI.Backend.Sdl
  ( -- * Running
    runSdlApp
  , runSdlAppReduce
  , SdlOptions (..)
  , defaultSdlOptions
  , RenderDriver (..)
  , RgbaImage (..)

    -- * Fonts and scale
  , NanoUIFont (..)
  , listFontFamilies
  , setSdlUiFont
  , setSdlUiScale

    -- * File dialogs
  , FileFilter (..)
  , FileDialogOptions (..)
  , FileDialogId
  , FileDialogResult (..)
  , defaultFileDialogOptions
  , askOpenFileDialog
  , askSaveFileDialog
  , askOpenFolderDialog
  , pollFileDialogUi
  , peekFileDialogUi
  , useFileDialog
  , openFileDialog
  , saveFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog

    -- * External URLs
  , openUrl

    -- * The window

    -- | 'WindowSettings' and the view-side window functions come from the
    -- core ("NanoUI"): 'NanoUI.askWindow', 'NanoUI.setWindowTitle',
    -- 'NanoUI.moveWindow', 'NanoUI.quit' and others. The rest of this
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

    -- * Debugging
  , SdlDebugSnapshot (..)
  , CoreDebugSnapshot (..)
  , askSdlDebug
  , sdlDebugSnapshot
  , formatFpsRows
  , formatDrawRows
  , formatExplainRows

    -- * Screenshots
  , saveScreenshot
  , captureScreenshot

    -- * Explicit sessions

    -- | View-side operations look up the active session from the context.
    -- For the IO-level operations above ('openFileDialog', 'pollFileDialog',
    -- 'setWindowChrome', 'saveScreenshot' and others), retrieve it with
    -- 'askSdlEnv'. They call SDL, so call them on the session's display
    -- thread; only 'cancelFileDialog' is safe from another thread.
  , SdlEnv
  , askSdlEnv
  , withSdl
  , withSdlBench
  , syncDisplay
  , sdlDrawFrame
  , resetRenderTextures
  ) where

import NanoUI (NanoUI, WindowMode (..), WindowPosition (..), WindowSettings (..), defaultWindowSettings)
import NanoUI.Internal.Debug (CoreDebugSnapshot (..), formatDrawRows, formatExplainRows, formatFpsRows)
import NanoUI.Sdl.Internal.Runner (askSdlDebug, drawFrameWith, resetRenderTextures, sdlDebugSnapshot, sdlDrawFrame, sdlDrawFrameSettling, setSdlUiFont, setSdlUiScale)
import NanoUI.Sdl.Internal.Session (runSdlSession)
import NanoUI.Sdl.Internal.Debug (SdlDebugSnapshot (..))
import NanoUI.Sdl.Internal.Window (RenderDriver (..), RgbaImage (..), SdlEnv, SdlOptions (..), askSdlEnv, captureScreenshot, defaultSdlOptions, saveScreenshot, syncDisplay, windowZoom, withSdl, withSdlBench)
import NanoUI.Sdl.Internal.Dialog
import NanoUI.Sdl.Internal.Chrome
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Internal.Font.Search (listFontFamilies)
import NanoUI.Sdl.Internal.Url (openUrl)
import NanoUI.Emit (NanoUIE, newReducer, reduceFrame)

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
-- data) act on this session without taking it as an explicit argument.
runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = runSdlSession options (`sdlDrawFrameSettling` ui)

-- | Run a model-driven view, folding emitted messages through the update
-- function in emission order. Use the adapters in "NanoUI.Emit" to emit
-- changes from ordinary widgets.
runSdlAppReduce ::
  Eq model =>
  SdlOptions
  -> (msg -> model -> model)
  -> model
  -> (model -> NanoUIE msg ())
  -> IO ()
runSdlAppReduce options update model view = do
  reducer <- newReducer update model
  runSdlSession options $ \ctx env inp forceFull settleResize ->
    drawFrameWith ctx env inp forceFull settleResize $ \i ->
      (\(_, drawData, dirty) -> (drawData, dirty)) <$> reduceFrame reducer ctx i view
