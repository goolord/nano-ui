-- | Frame loop of the RGFW demo on the OpenGL path, in a hidden window: prints
-- the mean wall time and allocation of each workload, and can be profiled
-- with +RTS -p.
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, replicateM_, unless)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Stats (RTSStats (..), getRTSStats)
import System.Environment (lookupEnv)
import System.Mem (performGC)
import Text.Printf (printf)
import Text.Read (readMaybe)
import qualified Data.Text as T
import NanoUI
  ( Input (..)
  , NanoUI
  , Size (..)
  , Theme (..)
  , V2 (..)
  , columnWith
  , fillW
  , gap
  , label
  , tight
  )
import NanoUI.Backend (emptyInput)
import NanoUI.Testing (collectRasterSpans, damageIsEmpty, runFrame, takeDamage)
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl (freeGlRenderer, newGlRenderer, renderArenaGl)
import qualified RGFW as R
import RgfwDemoCommon (Model (..), appView, currentTheme, dpiScale, initialModel, physScaleFor, themeForChoice)

iterations :: Int
iterations = 500

physW, physH :: Int
physW = 1680
physH = 1040

-- | Mean wall time and allocation per run after a short warmup.
measure :: String -> IO () -> IO ()
measure name action = do
  requested <- lookupEnv "NANO_PROFILE_ITERATIONS"
  let runs = max 1 (maybe iterations id (requested >>= readMaybe))
  replicateM_ 20 action
  performGC
  s0 <- getRTSStats
  t0 <- getMonotonicTimeNSec
  replicateM_ runs action
  t1 <- getMonotonicTimeNSec
  performGC
  s1 <- getRTSStats
  let perIter :: Double -> Double
      perIter total = total / fromIntegral runs
  printf "%-34s : %9.4f ms/frame  |  %10.2f KB alloc/frame\n" name
    (perIter (fromIntegral (t1 - t0) / 1e6))
    (perIter (fromIntegral (allocated_bytes s1 - allocated_bytes s0) / 1024))

main :: IO ()
main = do
  let m = initialModel
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
        let present ctx force frameInp view = do
              (_, _, draw, _) <- runFrame ctx frameInp view
              damage <- takeDamage ctx
              unless (damageIsEmpty damage && not force) $ do
                (baseSpans, overlaySpans) <- collectRasterSpans ctx frameInp
                renderArenaGl renderer getCozetteFont scale physW physH (themeWindow theme) draw baseSpans overlaySpans
                R.swapBuffersGL win
            fresh = do
              ctx <- newRgfwContext theme
              present ctx True inp (appView m)
              pure ctx
        ctxFull <- fresh
        measure "Demo, forced full" (present ctxFull True inp (appView m))
        ctxHover <- fresh
        hoverRef <- newIORef (0 :: Int)
        measure "Demo, hover sweep" $ do
          k <- tick hoverRef
          -- Down the left column, where each step crosses a widget edge.
          present ctxHover False inp {inputMousePos = V2 120 (fromIntegral (60 + k `mod` 500))} (appView m)
        ctxCount <- fresh
        countRef <- newIORef (0 :: Int)
        measure "Demo, counter changes" $ do
          k <- tick countRef
          present ctxCount False inp (appView m {counter = k})
        ctxFar <- fresh
        farRef <- newIORef (0 :: Int)
        measure "Demo, counter + far hover" $ do
          k <- tick farRef
          present ctxFar False inp {inputMousePos = V2 (fromIntegral (logW - 60)) (fromIntegral (logH - 40 - k `mod` 2 * 20))} (appView m {counter = k})
        -- The bound: a window full of text, where every glyph is a quad.
        ctxWall <- newRgfwContext theme
        present ctxWall True inp (textWall 0)
        measure "Text wall, forced full" (present ctxWall True inp (textWall 0))
        wallRef <- newIORef (0 :: Int)
        measure "Text wall, counter changes" $ do
          k <- tick wallRef
          present ctxWall False inp (textWall k)
        putStrLn ("profiled RGFW demo frames at " ++ show physW ++ "x" ++ show physH)
  where
    tick :: IORef Int -> IO Int
    tick ref = readIORef ref <* modifyIORef' ref (+ 1)

-- | A counter over enough wrapped paragraphs to fill the window.
textWall :: Int -> NanoUI ()
textWall k = columnWith (tight . gap 4 . fillW) $ do
  label (T.pack ("frame " <> show k))
  forM_ paragraphs label
  where
    ws = T.words "the quick brown fox jumps over a lazy dog while seven wizards quietly hex bold nymphs and pack my box with five dozen liquor jugs"
    paragraphs =
      [ T.unwords [ws !! ((i * 7 + j * 3) `mod` length ws) | j <- [0 .. 119 :: Int]]
      | i <- [1 .. 16 :: Int]
      ]
