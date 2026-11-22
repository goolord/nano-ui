-- | Generate a self-contained HTML visual glossary of nano-ui styling
-- modifiers, widget surface mappings, and unexpected interactions, with every
-- specimen rendered headlessly by @nano-ui-svg@ ('frameToSvg').
--
-- The page's stylesheet and script live in @assets/glossary.css@ and
-- @assets/glossary.js@ and are spliced into the generated document, so the
-- output stays one portable file while the visual and interactive layers are
-- edited as plain CSS and JavaScript rather than Haskell string literals.
-- 'generateBookVisuals' also writes the layout-chapter and widget-gallery
-- SVGs used by the documentation book.
--
-- Run:
--
-- > cabal run -j1 nano-ui-svg-glossary -- [docs/styling-glossary.html]
module Main (main) where

import Data.IORef (writeIORef)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Glossary.BookVisuals (generateBookVisuals)
import Glossary.Html (buildHtmlDocument, buildSvgBanksValue, buildTrapsValue, buildWidgetMetaValue)
import Glossary.Render (encodeJsonText, loadAssets, sdlFontCtxRef)
import Glossary.Specimens
  ( generateInteractions
  , generateProbeMatrix
  , generateRainbowBanner
  , generateStateStrip
  , generateTypoDemos
  )
import NanoUI
import NanoUI.Backend.Sdl (NanoUIFont (..), SdlOptions (..), defaultSdlOptions, withSdl)
import NanoUI.Testing (newPixelContext)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), hSetEncoding, utf8, withFile)

main :: IO ()
main = do
  args <- getArgs
  let outPath = case args of
        (p : _) -> p
        [] -> "docs/styling-glossary.html"
      sdlOpts =
        defaultSdlOptions
          { sdlWindowSettings =
              defaultWindowSettings
                { wsTitle = "nano-ui-svg-glossary"
                , wsMode = Hidden
                , wsSize = Size 800 600
                , wsResizable = False
                }
          , sdlAppVsync = False
          , sdlAppFont = DefaultFont
          , sdlAppMonoFont = DefaultFont
          , sdlAppFontSize = 14
          , sdlAppUiScale = 1
          }
  assets <- loadAssets
  ctxSeed <- newPixelContext
  withSdl sdlOpts ctxSeed $ \sdlCtx _env -> do
    writeIORef sdlFontCtxRef (Just sdlCtx)
    banner <- generateRainbowBanner
    stateStrip <- generateStateStrip
    probeRows <- generateProbeMatrix
    typoDemos <- generateTypoDemos
    interactions <- generateInteractions
    generateBookVisuals
    let html = buildHtmlDocument assets banner stateStrip probeRows typoDemos interactions
    withFile outPath WriteMode $ \h -> do
      hSetEncoding h utf8
      TIO.hPutStr h html
    putStrLn ("wrote " <> outPath <> " (" <> show (T.length html) <> " chars)")
    createDirectoryIfMissing True "book/theme"
    let banksJs =
          "window.NANO_UI_BANKS = "
            <> encodeJsonText (buildSvgBanksValue (snd banner) (snd stateStrip) probeRows typoDemos)
            <> ";\nwindow.NANO_UI_WIDGET_META = "
            <> encodeJsonText (buildWidgetMetaValue probeRows)
            <> ";\nwindow.NANO_UI_TRAPS = "
            <> encodeJsonText (buildTrapsValue interactions)
            <> ";\n"
    withFile "book/theme/nano-ui-banks.js" WriteMode $ \h -> do
      hSetEncoding h utf8
      TIO.hPutStr h banksJs
    putStrLn "wrote book/theme/nano-ui-banks.js"
