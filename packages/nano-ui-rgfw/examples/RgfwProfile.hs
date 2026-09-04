{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM_, void)
import Effectful (runEff)
import NanoUI
  ( Input (..)
  , Size (..)
  , V2 (..)
  , WidgetId (..)
  , emptyInput
  )
import NanoUI.Context
  ( Context (..)
  , withFontMetrics
  )
import NanoUI.Testing (newPixelContext, runFrameEff)
import NanoUI.Rgfw.Font.Cozette (cozetteMetrics, getCozetteFont)
import NanoUI.Rgfw.Render (renderArena)
import NanoUI.Rgfw.Surface (clearScreen, freeRgfwSurface, newOffscreenRgfwSurface, packColor)
import NanoUI.Rgfw.Theme (RgfwTheme (..))
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

  surf <- newOffscreenRgfwSurface physW physH
  ctx0 <- newPixelContext
  let ctx = withFontMetrics ctx0 cozetteMetrics
      font = getCozetteFont
      inp =
        emptyInput
          { inputWindowSize = Size (fromIntegral logW) (fromIntegral logH)
          , inputMousePos = V2 400 300
          }
      na = ctxNodeArena ctx

  let runSingleFrame = do
        void $ runFrameEff runEff ctx inp (appView m)
        clearScreen surf (packColor (thBackground theme))
        renderArena surf font scale theme ctx na (WidgetId 0) (WidgetId 0) (WidgetId 0)

  -- Warmup
  runSingleFrame

  -- Profile loop
  replicateM_ iterations runSingleFrame

  freeRgfwSurface surf
  putStrLn ("profiled " ++ show iterations ++ " RGFW demo frames")
