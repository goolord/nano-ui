module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_, void)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.SmallArray (SmallArray)
import NanoUI
import NanoUI.Svg (rasterizeSvg)
import NanoUI.Testing (newContext, runFrame)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

-- Enough frames for a stable time profile without an interactive window.
iterations :: Int
iterations = 3000

-- | A grid of buttons and labels: the ordinary widget path.
widgetScene :: NanoUI ()
widgetScene =
  columnWith
    (grow . gap 8)
    ( do
        replicateM_ 12 $
          gridWith 8 (gap 8) $
            replicateM_ 8 (void (button "OK"))
        label "nano-ui profile loop"
    )

-- | A thousand rects: enough ops that building them costs more than replaying
-- them, which is the case a content key is for.
canvasOps :: CustomDrawContext -> Rect -> SmallArray DrawOp
canvasOps cdc (Rect x y w h) = runCanvas $ do
  let side = 32 :: Int
      cw = w / fromIntegral side
      ch = h / fromIntegral side
      accent = themeAccent (cdcTheme cdc)
  forM_ [0 .. side - 1] $ \i ->
    forM_ [0 .. side - 1] $ \j -> do
      let fx = x + fromIntegral i * cw
          fy = y + fromIntegral j * ch
          tint = fromIntegral ((i * side + j) `mod` 255) / 255
      drawRect (Rect fx fy (cw - 1) (ch - 1)) (lerpColor accent (colorRGBA 255 255 255 255) tint)

-- | 'canvasOps', counting the frames that actually build the ops. The count
-- says which path a scene took: one build for a keyed widget the frames reuse,
-- one per frame for an unkeyed one.
{-# NOINLINE countedCanvasOps #-}
countedCanvasOps :: CustomDrawContext -> Rect -> SmallArray DrawOp
countedCanvasOps cdc rect = unsafePerformIO $ do
  modifyIORef' buildCount (+ 1)
  pure (canvasOps cdc rect)

{-# NOINLINE buildCount #-}
buildCount :: IORef Int
buildCount = unsafePerformIO (newIORef 0)

-- | An op-heavy custom widget. Pass 0 for the unkeyed path, which rebuilds and
-- compares its ops every frame, or a content key, which reuses them while it
-- is unchanged.
canvasScene :: Int -> NanoUI ()
canvasScene key =
  void $
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = fixedWH 512 512 defaultLayout
        , widgetContent = key
        , widgetDraw = countedCanvasOps
        }

-- | A focused text area over a long document, typing into its middle: the
-- editor path, whose per-frame cost must not grow with the document.
textAreaScene :: IORef Text -> NanoUI ()
textAreaScene ref = column $ do
  txt <- textAreaWith grow =<< uiIO (readIORef ref)
  uiIO (writeIORef ref txt)

clockIcon :: Text
clockIcon =
  "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>\
  \<circle cx='12' cy='12' r='10'/><path d='M12 6v6l4 2'/></svg>"

starIcon :: Text
starIcon =
  "<svg viewBox='0 0 24 24'><path fill='#e0a030' d='M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z'/></svg>"

longDocument :: Text
longDocument = T.intercalate "\n" [T.pack ("line " ++ show i ++ " of a long document") | i <- [1 .. 100000 :: Int]]

main :: IO ()
main = do
  args <- getArgs
  ctx <- newContext
  case args of
    ("svg" : _) -> do
      -- A stroked icon with round caps and joins and a filled one, at a small
      -- and a large size; a varying size keeps each raster from being shared.
      let parsed = mapM parseSvg [clockIcon, starIcon]
      case parsed of
        Left err -> fail err
        Right docs ->
          forM_ [1 .. 500 :: Int] $ \i ->
            forM_ docs $ \doc -> do
              let white = colorRGBA 255 255 255 255
              void (evaluate (rasterizeSvg (16 + i `mod` 2) 16 white doc))
              void (evaluate (rasterizeSvg (128 + i `mod` 2) 128 white doc))
      putStrLn "profiled 1000 rasterizations of two icons at 16 and 128 px"
    ("textarea" : _) -> do
      ref <- newIORef longDocument
      let inp = emptyInput {inputWindowSize = Size 800 600}
          frame i = void (runFrame ctx i (textAreaScene ref))
      frame inp
      frame inp {inputKeys = inputKeysFromList [KeyTab]}
      replicateM_ 50 (frame inp {inputKeys = inputKeysFromList (replicate 100 KeyDown)})
      forM_ (take 1000 (cycle "typing into the middle ")) $ \c ->
        frame inp {inputChars = T.singleton c}
      putStrLn "profiled 1000 textarea keystroke frames"
    _ -> do
      let inp =
            emptyInput
              { inputWindowSize = Size 800 600
              , inputMousePos = V2 400 300
              , inputMouseDown = True
              }
          (name, ui) = case args of
            ("canvas" : _) -> ("canvas", canvasScene 0)
            ("canvas-keyed" : _) -> ("canvas-keyed", canvasScene 1)
            _ -> ("widgets", widgetScene)
      replicateM_ iterations (void (runFrame ctx inp ui))
      builds <- readIORef buildCount
      putStrLn ("profiled " ++ show iterations ++ " " ++ name ++ " frames, op builds: " ++ show builds)
