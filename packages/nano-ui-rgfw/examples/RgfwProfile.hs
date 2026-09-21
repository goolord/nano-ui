-- | Frame loop of the RGFW demo on the OpenGL path, in a hidden window, for
-- profiling (+RTS -p).
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (replicateM_)
import NanoUI
  ( Input (..)
  , Size (..)
  , Theme (..)
  , V2 (..)
  , emptyInput
  )
import NanoUI.Testing (collectRasterSpans, runFrame)
import NanoUI.Rgfw.Context (newRgfwContext)
import NanoUI.Rgfw.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Gl (freeGlRenderer, newGlRenderer, renderArenaGl)
import qualified RGFW as R
import RgfwDemoCommon (appView, currentTheme, dpiScale, initialModel, physScaleFor, themeForChoice)

iterations :: Int
iterations = 500

main :: IO ()
main = do
  let !physW = 1680
      !physH = 1040
      m = initialModel
      theme = themeForChoice (currentTheme m)
      userScale = physScaleFor (dpiScale m)
      !scale = if userScale > 0.0 then userScale else 1.0
      !logW = max 1 (round (fromIntegral physW / scale) :: Int)
      !logH = max 1 (round (fromIntegral physH / scale) :: Int)
      inp =
        emptyInput
          { inputWindowSize = Size (fromIntegral logW) (fromIntegral logH)
          , inputMousePos = V2 400 300
          }
  bracket
    (R.createWindowGL "nano-ui-rgfw-profile" 0 0 physW physH R.rgfw_windowHide 3 2)
    (mapM_ R.closeWindow) $ \case
      Nothing -> fail "Failed to create a hidden RGFW window with an OpenGL 3.2 context."
      Just win -> bracket newGlRenderer freeGlRenderer $ \renderer -> do
        ctx <- newRgfwContext theme
        let runSingleFrame = do
              (_, _, draw, _) <- runFrame ctx inp (appView m)
              (baseSpans, overlaySpans) <- collectRasterSpans ctx inp
              renderArenaGl renderer getCozetteFont scale physW physH (themeWindow theme) draw baseSpans overlaySpans
              R.swapBuffersGL win
        -- Warmup
        runSingleFrame
        replicateM_ iterations runSingleFrame
        putStrLn ("profiled " ++ show iterations ++ " RGFW demo frames")
