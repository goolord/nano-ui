-- | Frame loop of the RGFW demo on the OpenGL path, in a hidden window: prints
-- the mean wall time and allocation of each workload, and can be profiled
-- with +RTS -p.
module Main (main) where

import Control.Exception (bracket)
import Control.Monad (forM_, unless, void, when)
import qualified Data.ByteString as BS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import NanoUI
  ( AlignX (..)
  , AlignY (..)
  , FontWeight (..)
  , Input (..)
  , NanoUI
  , Rect (..)
  , Size (..)
  , TextFont (..)
  , V2 (..)
  , canvas
  , colorRGBA
  , columnWith
  , defaultTextFont
  , drawText
  , drawTextWith
  , fillH
  , fillW
  , fixedH
  , flex
  , gap
  , inlineText
  , label
  , richText
  , rowWith
  , strong
  , tight
  )
import NanoUI.Backend (Damage (..), emptyInput)
import NanoUI.Testing (ctxPaintFull, collectRasterSpans, damageIsEmpty, runFrame, takeDamage, takeDamagePieces)
import NanoUI.Testing.Harness (measureRuns)
import NanoUI.Emit (runNanoUIE)
import NanoUI.Rgfw.Internal.Context (newRgfwContext)
import NanoUI.Rgfw.Internal.Font.Cozette (getCozetteFont)
import NanoUI.Rgfw.Internal.Gl (freeGlRenderer, newGlRenderer, readRetainedPixels, renderArenaGl)
import qualified RGFW as R
import RgfwDemoCommon (Model (..), currentTheme, dpiScale, initialModel, physScaleFor, themeForChoice)
import RgfwDemoCommon qualified as Demo

iterations :: Int
iterations = 500

physW, physH :: Int
physW = 1680
physH = 1040

-- | Mean wall time and allocation per run after a short warmup.
measure :: String -> IO () -> IO ()
measure = measureRuns 20 iterations

main :: IO ()
main = do
  let appView model = void (runNanoUIE (Demo.appView model))
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
        -- As the runner draws a frame: a forced frame paints in full, and
        -- any other paints only its damage.
        -- Returns whether the frame drew anything.
        let present ctx force frameInp view = do
              writeIORef (ctxPaintFull ctx) force
              (_, draw, _) <- runFrame ctx frameInp view
              damage <- takeDamage ctx
              let drawn = force || not (damageIsEmpty damage)
              when drawn $ do
                (baseSpans, overlaySpans) <- collectRasterSpans ctx frameInp
                pieces <- if force then pure [] else takeDamagePieces ctx
                renderArenaGl renderer getCozetteFont scale physW physH
                  (if force then DamageFull else damage) pieces draw baseSpans overlaySpans
                R.swapBuffersGL win
              pure drawn
            -- Partial frames 16 ms apart until nothing changes, so hover
            -- fades finish, then the same state painted in full must give
            -- the same pixels.
            check name ctx frameInp view = do
              let settle :: Int -> IO ()
                  settle n = do
                    drawn <- present ctx False frameInp {inputDeltaTime = 0.016} view
                    when drawn $
                      if n < 200 then settle (n + 1) else fail ("never settled: " ++ name)
              settle (0 :: Int)
              partial <- readRetainedPixels renderer physW physH
              _ <- present ctx True frameInp view
              full <- readRetainedPixels renderer physW physH
              unless (partial == full) $
                fail ("partial repaint differs from a full one: " ++ name ++ ", "
                  ++ show (length (filter id (BS.zipWith (/=) partial full))) ++ " bytes")
            fresh = do
              ctx <- newRgfwContext theme
              _ <- present ctx True inp (appView m)
              pure ctx
        ctxFull <- fresh
        measure "Demo, forced full" (void (present ctxFull True inp (appView m)))
        ctxHover <- fresh
        hoverRef <- newIORef (0 :: Int)
        measure "Demo, hover sweep" $ do
          k <- tick hoverRef
          -- Down the left column, where each step crosses a widget edge.
          void (present ctxHover False inp {inputMousePos = V2 120 (fromIntegral (60 + k `mod` 500))} (appView m))
        ctxCount <- fresh
        countRef <- newIORef (0 :: Int)
        measure "Demo, counter changes" $ do
          k <- tick countRef
          void (present ctxCount False inp (appView m {counter = k}))
        ctxFar <- fresh
        farRef <- newIORef (0 :: Int)
        measure "Demo, counter + far hover" $ do
          k <- tick farRef
          void $ present ctxFar False inp {inputMousePos = V2 (fromIntegral (logW - 60)) (fromIntegral (logH - 40 - k `mod` 2 * 20))} (appView m {counter = k})
        -- The bound: a window full of text, where every glyph is a quad.
        ctxWall <- newRgfwContext theme
        _ <- present ctxWall True inp (textWall 0)
        measure "Text wall, forced full" (void (present ctxWall True inp (textWall 0)))
        wallRef <- newIORef (0 :: Int)
        measure "Text wall, counter changes" $ do
          k <- tick wallRef
          void (present ctxWall False inp (textWall k))
        -- Two small changes in opposite corners, whose bounding box is most
        -- of the window.
        ctxCorners <- newRgfwContext theme
        _ <- present ctxCorners True inp (textCorners 0)
        cornersRef <- newIORef (0 :: Int)
        measure "Text wall, corner counters" $ do
          k <- tick cornersRef
          void (present ctxCorners False inp (textCorners k))
        -- Text in drawings, which the paint records as spans.
        ctxDrawn <- newRgfwContext theme
        _ <- present ctxDrawn True inp (drawingText 0)
        measure "Drawing text, forced full" (void (present ctxDrawn True inp (drawingText 0)))
        drawnRef <- newIORef (0 :: Int)
        measure "Drawing text, counter changes" $ do
          k <- tick drawnRef
          void (present ctxDrawn False inp (drawingText k))
        ctxCheck <- fresh
        forM_ [60, 90 .. 700 :: Int] $ \y ->
          check ("hover at y=" ++ show y) ctxCheck inp {inputMousePos = V2 120 (fromIntegral y)} (appView m)
        forM_ [1 .. 5 :: Int] $ \k ->
          check ("counter " ++ show k) ctxCheck inp (appView m {counter = k})
        forM_ [1 .. 3 :: Int] $ \k -> check ("text wall " ++ show k) ctxWall inp (textWall k)
        forM_ [1 .. 3 :: Int] $ \k -> check ("corners " ++ show k) ctxCorners inp (textCorners k)
        -- Other contexts drew since: start from this one's own pixels.
        _ <- present ctxDrawn True inp (drawingText 0)
        forM_ [1 .. 3 :: Int] $ \k -> check ("drawing text " ++ show k) ctxDrawn inp (drawingText k)
        forM_ [40, 140 .. 440 :: Int] $ \y ->
          check ("drawing text, hover at y=" ++ show y) ctxDrawn inp {inputMousePos = V2 120 (fromIntegral y)} (drawingText 3)
        putStrLn "partial repaints match full ones"
        putStrLn ("profiled RGFW demo frames at " ++ show physW ++ "x" ++ show physH)
  where
    tick :: IORef Int -> IO Int
    tick ref = readIORef ref <* modifyIORef' ref (+ 1)

-- | A counter over enough wrapped paragraphs to fill the window.
textWall :: Int -> NanoUI ()
textWall k = columnWith (tight . gap 4 . fillW) $ do
  label (T.pack ("frame " <> show k))
  forM_ paragraphs label

-- | The text wall with a second counter in the bottom-right corner.
textCorners :: Int -> NanoUI ()
textCorners k = columnWith (tight . gap 4 . fillW . fillH) $ do
  textWall k
  flex
  rowWith (tight . fillW) $ do
    flex
    label (T.pack ("frame " <> show k))

-- | A counter in a canvas of labels, plain and bold, over rich text
-- paragraphs: all of it drawing text.
drawingText :: Int -> NanoUI ()
drawingText k = columnWith (tight . gap 4 . fillW) $ do
  void $ canvas (fillW . fixedH 200) $ \(Rect x y w _) ->
    forM_ [0 .. 11 :: Int] $ \r -> forM_ [0 .. 5 :: Int] $ \c -> do
      let at = V2 (x + fromIntegral c * w / 6) (y + fromIntegral r * 16)
          txt = T.pack (if r + c == 0 then "frame " <> show k else "cell " <> show (r * 6 + c))
      if even (r + c)
        then drawText at AlignStart AlignTop txt ink
        else drawTextWith defaultTextFont {textFontWeight = WeightBold} at AlignStart AlignTop txt ink
  forM_ (take 8 paragraphs) $ \p -> richText [inlineText p, strong " and bold"]
  where
    ink = colorRGBA 0xC5 0xC8 0xC6 255

paragraphs :: [T.Text]
paragraphs =
  [ T.unwords [ws !! ((i * 7 + j * 3) `mod` length ws) | j <- [0 .. 119 :: Int]]
  | i <- [1 .. 16 :: Int]
  ]
  where
    ws = T.words "the quick brown fox jumps over a lazy dog while seven wizards quietly hex bold nymphs and pack my box with five dozen liquor jugs"
