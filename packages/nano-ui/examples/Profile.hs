module Main (main) where

import Control.Exception (evaluate)
import Control.Monad (forM_, replicateM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.SmallArray (SmallArray)
import NanoUI
import NanoUI.Backend (applyMouseButton, emptyInput, inputKeysFromList)
import NanoUI.Svg (rasterizeSvg)
import NanoUI.Testing (newContext, runFrame, uiCursorKind)
import GHC.Clock (getMonotonicTime)
import System.Environment (getArgs)
import System.IO.Unsafe (unsafePerformIO)

-- Enough frames for a stable time profile without an interactive window.
iterations :: Int
iterations = 3000

-- | A floating window above @n@ rows of a scroll area.
windowScene :: Int -> NanoUI ()
windowScene n = columnWith (fillW . fillH) $ do
  void $ scroll2DWith (fillW . fillH) $ columnWith (tight . fillW) $
    forM_ [1 .. n] $ \i -> rowWith (tight . fillW) $ do
      label (T.pack ("row " <> show i))
      void (button (T.pack ("b" <> show i)))
  void $ window True "Tools" $ columnWith (tight . gap 4 . minW 200) $ do
    label "a floating window"
    void (button "ok")

-- | @n@ rows of a label, a checkbox and a button in a scroll area, with a
-- text field on top: a large arena for the pointer to move over.
pointerScene :: Int -> NanoUI ()
pointerScene n = columnWith (fillW . fillH) $ do
  void (textInput "")
  void $ scrollWith (fillW . fillH) $ columnWith (tight . fillW) $
    forM_ [1 .. n] $ \i -> rowWith (tight . fillW) $ do
      label (T.pack ("row " <> show i))
      void (checkbox "c" False)
      void (button (T.pack ("b" <> show i)))

-- | 60 rows of 5 to 154 grow columns, with content widths up to 40 pixels
-- and grow weights of 0.5 to 3 from a fixed pseudo-random sequence, under a
-- label that changes every frame so the layout is solved again. Sharing a
-- row's space locks the columns whose content outgrows their share, and in
-- about one row in four that takes more than two passes.
growScene :: Int -> NanoUI ()
growScene frame = columnWith (tight . gap 0 . fillW . fillH) $ do
  label (T.pack (show frame))
  forM_ (take 60 (iterate (lcg . lcg . lcg) 7)) $ \seed ->
    rowWith (tight . gap 0 . fillW) $ forM_ (growRow seed) $ \(c, g) ->
      columnWith (\l -> (tight l) {layoutWidth = Grow g}) (spacer (Fixed c) (Fixed 1))
 where
  lcg x = (x * 1103515245 + 12345) `mod` 2147483648 :: Int
  growRow seed =
    let k = 5 + seed `mod` 150
        xs = take (2 * k) (drop 1 (iterate lcg seed))
     in [(fromIntegral (a `mod` 4000) / 100, [1, 2, 3, 0.5] !! (b `mod` 4)) | (a, b) <- pairs xs]
  pairs (a : b : r) = (a, b) : pairs r
  pairs _ = []

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
textAreaScene :: IORef TextDocument -> NanoUI ()
textAreaScene ref = column $ do
  doc <- textAreaDocumentWith grow =<< liftIO (readIORef ref)
  liftIO (writeIORef ref doc)

-- | 'textAreaScene' over 'Text', which joins the document on every frame
-- that edits it.
textAreaTextScene :: IORef Text -> NanoUI ()
textAreaTextScene ref = column $ do
  txt <- textAreaWith grow =<< liftIO (readIORef ref)
  liftIO (writeIORef ref txt)

clockIcon :: ByteString
clockIcon =
  "<svg viewBox='0 0 24 24' fill='none' stroke='currentColor' stroke-width='2' stroke-linecap='round' stroke-linejoin='round'>\
  \<circle cx='12' cy='12' r='10'/><path d='M12 6v6l4 2'/></svg>"

starIcon :: ByteString
starIcon =
  "<svg viewBox='0 0 24 24'><path fill='#e0a030' d='M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z'/></svg>"

longDocument :: Text
longDocument = T.intercalate "\n" [T.pack ("line " ++ show i ++ " of a long document") | i <- [1 .. 100000 :: Int]]

main :: IO ()
main = do
  args <- getArgs
  ctx <- newContext
  let
    -- Focus the text area, move 5,000 lines down and type 1,000 characters.
    typeIntoMiddle scene = do
      let inp = emptyInput {inputWindowSize = Size 800 600}
          frame i = void (runFrame ctx i scene)
      frame inp
      frame inp {inputKeys = inputKeysFromList [KeyTab]}
      replicateM_ 50 (frame inp {inputKeys = inputKeysFromList (replicate 100 KeyDown)})
      forM_ (take 1000 (cycle "typing into the middle ")) $ \c ->
        frame inp {inputChars = T.singleton c}
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
    ("window" : rest) -> do
      -- A floating window over 3000 rows of a scroll area, held still or,
      -- with "drag", dragged back and forth by its title bar.
      let inp = emptyInput {inputWindowSize = Size 1280 800, inputMousePos = V2 5 790, inputDeltaTime = 0.016}
          drag = rest == ["drag"]
          ui = windowScene 3000
          grab = V2 1014 22
      replicateM_ 5 (void (runFrame ctx inp ui))
      when drag $
        void (runFrame ctx (applyMouseButton MouseLeft True inp {inputMousePos = grab}) ui)
      forM_ [1 .. 300 :: Int] $ \i -> do
        let V2 gx gy = grab
            step = inp {inputMousePos = V2 (gx - 100 + fromIntegral (i `mod` 2) * 6) (gy + 50), inputButtonsHeld = buttonsFromList [MouseLeft]}
        void (runFrame ctx (if drag then step else inp) ui)
      putStrLn ("profiled 300 window frames" ++ if drag then ", dragging" else "")
    ("pointer" : _) -> do
      -- The pointer sweeping down over 3000 rows, pressing and releasing
      -- every tenth frame, with the cursor queried each frame as a backend
      -- does: hover, press and cursor hit tests over a large arena.
      let inp = emptyInput {inputWindowSize = Size 1280 800, inputDeltaTime = 0.016}
          ui = pointerScene 3000
          at i = inp {inputMousePos = V2 (fromIntegral (40 + (i * 7) `mod` 200)) (fromIntegral (40 + (i * 13) `mod` 740))}
          frames = 2000 :: Int
      replicateM_ 5 (void (runFrame ctx inp ui))
      cursorTime <- newIORef 0
      t0 <- getMonotonicTime
      forM_ [1 .. frames] $ \i -> do
        let base = at i
            fi = case i `mod` 10 of
              0 -> applyMouseButton MouseLeft True base
              1 -> applyMouseButton MouseLeft False base
              _ -> base
        void (runFrame ctx fi ui)
        c0 <- getMonotonicTime
        void (evaluate =<< uiCursorKind ctx fi)
        c1 <- getMonotonicTime
        modifyIORef' cursorTime (+ (c1 - c0))
      t1 <- getMonotonicTime
      ct <- readIORef cursorTime
      let perFrame t = show (t * 1000 / fromIntegral frames) ++ " ms"
      putStrLn ("profiled " ++ show frames ++ " pointer frames: " ++ perFrame (t1 - t0) ++ "/frame, cursor query " ++ perFrame ct)
    ("grow" : _) -> do
      let inp = emptyInput {inputWindowSize = Size 1280 800, inputDeltaTime = 0.016}
          frames = 2000 :: Int
      replicateM_ 5 (void (runFrame ctx inp (growScene 0)))
      t0 <- getMonotonicTime
      forM_ [1 .. frames] $ \i -> void (runFrame ctx inp (growScene i))
      t1 <- getMonotonicTime
      putStrLn ("profiled " ++ show frames ++ " grow frames: " ++ show ((t1 - t0) * 1000 / fromIntegral frames) ++ " ms/frame")
    ("textarea" : _) -> do
      ref <- newIORef (textDocument longDocument)
      typeIntoMiddle (textAreaScene ref)
      putStrLn "profiled 1000 textarea keystroke frames"
    ("textarea-text" : _) -> do
      ref <- newIORef longDocument
      typeIntoMiddle (textAreaTextScene ref)
      putStrLn "profiled 1000 textarea-text keystroke frames"
    _ -> do
      let inp =
            emptyInput
              { inputWindowSize = Size 800 600
              , inputMousePos = V2 400 300
              , inputButtonsHeld = buttonsFromList [MouseLeft]
              }
          (name, ui) = case args of
            ("canvas" : _) -> ("canvas", canvasScene 0)
            ("canvas-keyed" : _) -> ("canvas-keyed", canvasScene 1)
            _ -> ("widgets", widgetScene)
      replicateM_ iterations (void (runFrame ctx inp ui))
      builds <- readIORef buildCount
      putStrLn ("profiled " ++ show iterations ++ " " ++ name ++ " frames, op builds: " ++ show builds)
