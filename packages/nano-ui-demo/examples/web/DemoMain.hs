{-# LANGUAGE CPP #-}

-- | The widget cookbook ("Demo") in a browser canvas, with the web backend.
-- Built with @wasm32-wasi-ghc@ it is a reactor module whose @nanoMain@
-- export the page calls (see nano-ui-sdl-web's README); a native build only
-- checks that it compiles, and running it says it needs a browser.
module Main (main) where

import NanoUI
import NanoUI.Backend.SdlWeb (SdlWebOptions (..), defaultSdlWebOptions, runSdlWebApp)

import Demo (demoUi, newDemo)

main :: IO ()
main = runSdlWebApp webOptions . demoUi =<< newDemo

#if defined(wasm32_HOST_ARCH)
foreign export javascript "nanoMain" main :: IO ()
#endif

-- | The desktop demo's theme and title. The canvas is the page's, so there
-- is no window size, and the page has nothing to quit to.
webOptions :: SdlWebOptions
webOptions =
  defaultSdlWebOptions
    { webTheme = Just defaultTheme
    , webWindowSettings = defaultWindowSettings {wsTitle = "nano-ui widget cookbook"}
    }
