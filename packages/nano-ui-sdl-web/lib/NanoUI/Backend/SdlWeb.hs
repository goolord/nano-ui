-- | Run nano-ui views in a browser canvas with the SDL backend's text:
-- SDL_ttf shapes and rasterises glyphs (FreeType and HarfBuzz), the same
-- glyph atlas layout holds them, and WebGL2 draws the frame's triangles as
-- @SDL_RenderGeometry@ does on the desktop. Build the program with
-- @wasm32-wasi-ghc@ and load it with @web/nano-ui-sdl-web.js@; see the
-- package README.
--
-- > main :: IO ()
-- > main = runSdlWebApp defaultSdlWebOptions (label "Hello")
-- >
-- > foreign export javascript "nanoMain" main :: IO ()
module NanoUI.Backend.SdlWeb
  ( -- * Running
    runSdlWebApp
  , runSdlWebAppReduce
  , SdlWebOptions (..)
  , defaultSdlWebOptions
  , WebFont (..)
  , browserAvailable

    -- * Fonts
  , setSdlWebUiFont
  , listWebFontFamilies

    -- * File dialogs
  , FileFilter (..)
  , FileDialogOptions (..)
  , defaultFileDialogOptions
  , WebFile (..)
  , openWebFiles
  , openWebFolder
  , saveWebFile

    -- * Debugging
  , SdlWebDebugSnapshot (..)
  , CoreDebugSnapshot (..)
  , askSdlWebDebug
  , formatFpsRows
  , formatDrawRows
  , formatExplainRows
  ) where

import NanoUI (NanoUI)
import NanoUI.Emit (NanoUIE, newReducer, reduceFrame)
import NanoUI.Backend (runFrame)
import NanoUI.Internal.Debug (CoreDebugSnapshot (..), formatDrawRows, formatExplainRows, formatFpsRows)
import NanoUI.SdlWeb.Internal.Debug (SdlWebDebugSnapshot (..))
import NanoUI.SdlWeb.Internal.Files
  ( FileDialogOptions (..)
  , FileFilter (..)
  , WebFile (..)
  , defaultFileDialogOptions
  , openWebFiles
  , openWebFolder
  , saveWebFile
  )
import NanoUI.SdlWeb.Internal.Host (browserAvailable, browserHost)
import NanoUI.SdlWeb.Internal.Session
  ( SdlWebOptions (..)
  , WebFont (..)
  , askSdlWebDebug
  , defaultSdlWebOptions
  , listWebFontFamilies
  , runWebSession
  , setSdlWebUiFont
  )

-- | Run a view in the page's canvas until it quits. The view is rebuilt for
-- each frame. In a native build this throws: the program runs only as
-- WebAssembly in a browser ('browserAvailable').
runSdlWebApp :: SdlWebOptions -> NanoUI () -> IO ()
runSdlWebApp opts ui = do
  host <- browserHost
  runWebSession host opts $ \ctx inp ->
    (\(_, drawData, dirty) -> (drawData, dirty)) <$> runFrame ctx inp ui

-- | Run a model-driven view, folding emitted messages through the update
-- function in emission order, as @runSdlAppReduce@ does on the desktop.
runSdlWebAppReduce ::
  Eq model =>
  SdlWebOptions
  -> (msg -> model -> model)
  -> model
  -> (model -> NanoUIE msg ())
  -> IO ()
runSdlWebAppReduce opts update model view = do
  host <- browserHost
  reducer <- newReducer update model
  runWebSession host opts $ \ctx inp ->
    (\(_, drawData, dirty) -> (drawData, dirty)) <$> reduceFrame reducer ctx inp view
