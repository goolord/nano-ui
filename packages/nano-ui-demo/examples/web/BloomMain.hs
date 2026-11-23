{-# LANGUAGE CPP #-}

-- | "Bloom" in a browser canvas: the demo on the project's landing page
-- (@site/index.html@). Built with @wasm32-wasi-ghc@ it is a reactor module
-- whose @nanoMain@ export the page calls; a native build only checks that
-- it compiles.
module Main (main) where

import NanoUI
import NanoUI.Backend.SdlWeb (SdlWebOptions (..), defaultSdlWebOptions, runSdlWebApp)

import Bloom (bloomView)

main :: IO ()
main = runSdlWebApp webOptions bloomView

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
