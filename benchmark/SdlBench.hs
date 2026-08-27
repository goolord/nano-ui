{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (replicateM_, void)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import NanoUI
import NanoUI.Backend.Sdl (SdlEnv (..), sdlDrawFrame, syncDisplay, withSdlBench)
import System.IO (hSetEncoding, stderr, stdout)
import Test.Tasty.Bench
#if defined(mingw32_HOST_OS)
import System.Win32 (setConsoleCP, setConsoleOutputCP)
#endif

benchWindowSize :: Size
benchWindowSize = Size 800 600

benchInput :: Input
benchInput =
  emptyInput
    { inputWindowSize = benchWindowSize
    , inputMousePos = V2 400 300
    , inputMouseDown = True
    }

smallUi, mediumUi, largeUi :: UI ()
smallUi =
  column (defaultLayout {layoutGap = 8}) $ do
    void (button "OK")
    void (label "Hello")

mediumUi =
  column
    (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutGap = 8})
    ( do
        replicateM_ 12 $
          row (defaultLayout {layoutGap = 8, layoutWrap = True}) $
            replicateM_ 8 (void (button "OK"))
        void (label "nano-ui SDL bench")
    )

largeUi =
  column
    (defaultLayout {layoutWidth = Grow 1, layoutHeight = Grow 1, layoutGap = 6})
    ( do
        replicateM_ 20 $
          row (defaultLayout {layoutGap = 6, layoutWrap = True}) $
            replicateM_ 10 (void (button "Item"))
        replicateM_ 8 (void (label "Status line with a bit of text"))
    )

configureBenchIO :: IO ()
configureBenchIO = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
#if defined(mingw32_HOST_OS)
  void $ setConsoleCP 65001
  void $ setConsoleOutputCP 65001
#endif

main :: IO ()
main = do
  configureBenchIO
  ctx0 <- newSdlContext
  withSdlBench ctx0 $ \ctx sdlEnv -> do
    (ctx', inp) <- syncDisplay ctx sdlEnv benchInput
    warmup ctx' sdlEnv inp
    configureBenchIO
    defaultMain
      [ bgroup
          "ui/runFrame"
          [ benchRunFrame ctx' inp smallUi "small"
          , benchRunFrame ctx' inp mediumUi "medium"
          , benchRunFrame ctx' inp largeUi "large"
          ]
      , bgroup
          "sdl3/draw"
          [ benchDraw ctx' sdlEnv inp smallUi "small"
          , benchDraw ctx' sdlEnv inp mediumUi "medium"
          , benchDraw ctx' sdlEnv inp largeUi "large"
          ]
      ]

warmup :: Context -> SdlEnv -> Input -> IO ()
warmup ctx sdlEnv inp = do
  void (runFrame ctx inp mediumUi)
  void (sdlDrawFrame ctx mediumUi sdlEnv inp False)
  void (runFrame ctx inp mediumUi)
  void (sdlDrawFrame ctx mediumUi sdlEnv inp False)

benchRunFrame :: Context -> Input -> UI () -> String -> Benchmark
benchRunFrame ctx inp ui name =
  bench name $ whnfIO (void . runFrame ctx inp $ ui)

benchDraw :: Context -> SdlEnv -> Input -> UI () -> String -> Benchmark
benchDraw ctx sdlEnv inp ui name =
  bench name $ whnfIO (void . sdlDrawFrame ctx ui sdlEnv inp $ False)
