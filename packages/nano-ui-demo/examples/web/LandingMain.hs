{-# LANGUAGE CPP #-}

-- | The program on the project's landing page (@site/index.html@), in a
-- browser canvas. Built with @wasm32-wasi-ghc@ it is a reactor module whose
-- @nanoMain@ export the page calls; a native build only checks that it
-- compiles.
module Main (main) where

import Data.Maybe (isNothing)
import NanoUI
import NanoUI.Backend.SdlWeb (SdlWebOptions (..), defaultSdlWebOptions, runSdlWebApp)
import System.Environment (lookupEnv)

import Landing (landingView)

-- | The page sets @NANO_UI_REDUCED_MOTION@ in the program's environment
-- when the browser asks for reduced motion, and Bloom then starts with its
-- spin off.
main :: IO ()
main = do
  still <- lookupEnv "NANO_UI_REDUCED_MOTION"
  runSdlWebApp webOptions (landingView (isNothing still))

#if defined(wasm32_HOST_ARCH)
foreign export javascript "nanoMain" main :: IO ()
#endif

-- | The theme follows the page's colour scheme, as the landing page's own
-- colours do. The backend sets the document's title, so the title here is
-- the landing page's own @<title>@; keep the two the same. The view's text
-- is ASCII, which the bundled Inter covers, so no fallback font is fetched
-- and the page needs no font file beside it.
webOptions :: SdlWebOptions
webOptions =
  defaultSdlWebOptions
    { webThemeFor = Just defaultThemeFor
    , webWindowSettings = defaultWindowSettings {wsTitle = "nano-ui: an immediate-mode GUI toolkit for Haskell"}
    , webFallbackFonts = []
    }
