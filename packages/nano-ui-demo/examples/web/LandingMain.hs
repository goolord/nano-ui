{-# LANGUAGE CPP #-}

-- | WebAssembly entry point for the landing-page demo (@site/index.html@).
-- Built with @wasm32-wasi-ghc@ as a reactor module whose @nanoMain@ export
-- the page invokes; native builds simply verify compilation.
module Main (main) where

import Data.Maybe (isNothing)
import NanoUI
import NanoUI.Backend.SdlWeb (SdlWebOptions (..), defaultSdlWebOptions, runSdlWebApp)
import System.Environment (lookupEnv)

import Landing (landingView)

-- | @site/index.html@ passes @NANO_UI_REDUCED_MOTION=1@ when the browser
-- requests reduced motion, starting Bloom with rotation turned off.
main :: IO ()
main = do
  still <- lookupEnv "NANO_UI_REDUCED_MOTION"
  runSdlWebApp webOptions (landingView (isNothing still))

#if defined(wasm32_HOST_ARCH)
foreign export javascript "nanoMain" main :: IO ()
#endif

-- | Follow the browser's light/dark preference to match @site/index.html@.
-- Keep @wsTitle@ identical to the page's @<title>@. Because the demo uses
-- ASCII-only text covered by the bundled Inter font, no fallback font needs
-- to be fetched.
webOptions :: SdlWebOptions
webOptions =
  defaultSdlWebOptions
    { webThemeFor = Just defaultThemeFor
    , webWindowSettings = defaultWindowSettings {wsTitle = "nano-ui: an immediate-mode GUI toolkit for Haskell"}
    , webFallbackFonts = []
    }
