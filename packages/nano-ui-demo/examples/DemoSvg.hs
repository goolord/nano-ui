-- | Render the demo's Plots tab to an SVG file, headlessly.
--
-- The frame itself is rendered by "NanoUI.Draw.FrameSvg" (@nano-ui-svg@):
-- widget chrome from the vertex buffer, the plots and canvas paths from
-- their cached draw ops, and text from the raster spans. Run:
--
-- > cabal run nano-ui-demo-svg -- [OUT.svg]
module Main (main) where

import Control.Monad (void)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Demo qualified
import DemoApp (withHiddenWindow)
import NanoUI (V2 (..))
import NanoUI.Backend (damageFullNow, runFrame)
import NanoUI.Draw.FrameSvg (defaultFrameSvgConfig, enableSvgExport, frameToSvg)
import NanoUI.Testing (collectTextSpans)
import NanoUI.Testing.Harness (clickPos, findExact, requireSpan)
import System.Environment (getArgs)

-- The demo's canonical window, and one tall enough that the whole Plots tab
-- (four charts, the diagram, and the canvas paths) lays out without a
-- scroller clipping any of it.
width, height :: Float
width = 1280
height = 1200

main :: IO ()
main = do
  args <- getArgs
  let
    out = case args of
      (p : _) -> p
      [] -> "demo.svg"
  st <- Demo.newDemo
  svg <- capture st
  TIO.writeFile out svg
  putStrLn ("wrote " <> out <> ": " <> show (T.length svg) <> " chars")

-- | Drive the demo onto its Plots tab and capture one fully-painted frame.
capture :: Demo.DemoState -> IO T.Text
capture st =
  withHiddenWindow width height (V2 (-10) (-10)) id $ \ctx _env base -> do
    -- Recorded shapes and text as spans, as 'frameToSvg' reads the frame.
    enableSvgExport ctx
    let
      ui = Demo.demoUi st
      draw inp = void (runFrame ctx inp ui)
    draw base
    draw base
    tabSpans <- collectTextSpans ctx
    plotsTab <- requireSpan "demo-svg: no Plots tab" (findExact "Plots" tabSpans)
    clickPos draw base plotsTab
    draw base
    draw base
    -- One frame that paints the whole window, whatever the damage was.
    (_, drawData, _) <- runFrame ctx base (ui >> damageFullNow)
    frameToSvg defaultFrameSvgConfig ctx base drawData
