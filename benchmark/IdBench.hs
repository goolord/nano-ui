{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_, replicateM, replicateM_, void, when)
import GHC.Stats (RTSStats (..), getRTSStats)
import NanoUI
import NanoUI.Testing (newContext, runFrame)
import System.Exit (exitFailure)
import System.Mem (performGC)
import Test.Tasty.Bench

benchInput :: Input
benchInput = emptyInput {inputWindowSize = Size 1 1}

idBurst :: NanoUI ()
idBurst = void (replicateM 4096 nextId)

scopedWidgets :: NanoUI ()
scopedWidgets =
  column (defaultLayout {layoutGap = 2})
    $ replicateM_ 32
    $ row (defaultLayout {layoutGap = 2})
    $ replicateM_ 32 (void nextId)

measureFrameAlloc :: NanoUI a -> IO Integer
measureFrameAlloc ui = do
  ctx <- newContext
  _ <- runFrame ctx benchInput (pure ())
  performGC
  before <- getRTSStats
  _ <- runFrame ctx benchInput ui
  after <- getRTSStats
  pure (fromIntegral (allocated_bytes after - allocated_bytes before) :: Integer)

main :: IO ()
main = do
  forM_
    [ ("burst4096", idBurst)
    , ("scopedWidgets", scopedWidgets)
    ]
    $ \(name, ui) -> do
      alloc <- measureFrameAlloc ui
      when (alloc > 0) $
        putStrLn
          ("FAIL: " ++ name ++ " allocated " ++ show alloc ++ " bytes during runFrame")
      when (alloc > 0) exitFailure
  defaultMain
    [ bgroup
        "id/nextId"
        [ bench "burst4096" $ whnfIO $ do
            ctx <- newContext
            void (runFrame ctx benchInput idBurst)
        , bench "scopedWidgets" $ whnfIO $ do
            ctx <- newContext
            void (runFrame ctx benchInput scopedWidgets)
        ]
    ]
