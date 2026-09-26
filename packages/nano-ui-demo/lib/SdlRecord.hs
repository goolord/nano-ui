-- | Records a scripted tour of the SDL demo to numbered BMP frames. It draws
-- the UI on a hidden window, drives it with eased mouse moves, clicks, drags
-- and typing, reads each frame back from the renderer, and paints a mouse
-- pointer over it. @scripts/record-demo.sh@ turns the frames into a video.
--
-- Run via @cabal run nano-ui-sdl-demo -- --record DIR@. DIR also gets
-- @frames.txt@, an ffmpeg concat list holding each frame's real duration.
module SdlRecord
    ( record
    ) where

import Control.Concurrent (threadDelay)
import DemoApp (withHiddenWindow)
import Control.Monad (forM_, replicateM_, unless, void, when)
import GHC.Clock (getMonotonicTime)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import NanoUI
import NanoUI.Backend (applyMouseButton)
import NanoUI.Backend.Sdl
import NanoUI.Testing (Context, collectOverlayTextSpans, collectTextSpans)
import NanoUI.Testing.Harness (DemoSpan, findExact, findHeader, findRightmost, keyInp)
import System.FilePath ((</>))
import Text.Printf (printf)
import qualified Codec.Picture as JP
import qualified Data.Text as T

-- | Frames per second of the recording.
fps :: Double
fps = 30

winW, winH :: Float
winW = 1280
winH = 800

-- | A recording session: the frame loop and the pointer it draws.
data Rec = Rec
  { recDraw :: Input -> IO ()
  , recCtx :: Context
  , recEnv :: SdlEnv
  , recBase :: Input
  , recDir :: FilePath
  , recStart :: Double
  , recFrame :: IORef Int
  , recLast :: IORef Double
  , recMouse :: IORef V2
  , recDown :: IORef Bool
  , recLog :: IORef [(V2, Bool, Double)]
  -- ^ Newest first: pointer, button held, time since the start.
  }

-- | Record the tour of @ui@ into @dir@, which must exist.
record :: FilePath -> NanoUI () -> IO ()
record dir ui = do
  logRef <- newIORef []
  withHiddenWindow winW winH (V2 (-10) (-10)) id $ \ctx env idle -> do
    (ctx', base) <- syncDisplay ctx env idle
    -- The first frames load fonts; the Graphics tab loads its images once.
    replicateM_ 3 (void (sdlDrawFrame ctx' ui env base True))
    t0 <- getMonotonicTime
    r <-
      Rec (\inp -> void (sdlDrawFrame ctx' ui env inp False)) ctx' env base dir t0
        <$> newIORef 0
        <*> newIORef t0
        <*> newIORef (V2 (winW * 0.6) (winH * 0.5))
        <*> newIORef False
        <*> pure logRef
    tour r
  entries <- reverse <$> readIORef logRef
  -- Durations come from the real clock, so a slow frame stays on screen as
  -- long as it took to draw.
  let times = [t | (_, _, t) <- entries]
      durs = zipWith (-) (drop 1 times) times ++ [1 / fps]
  forM_ (zip3 [0 :: Int ..] entries durs) $ \(i, (pos, down, _), _) ->
    drawPointer (dir </> frameName i) pos down
  writeFile (dir </> "frames.txt") $
    concat [printf "file '%s'\nduration %.4f\n" (frameName i) d | (i, d) <- zip [0 :: Int ..] durs]
      ++ printf "file '%s'\n" (frameName (length entries - 1))
  let total = sum durs
  printf "record: %d frames, %.1f s, %.1f fps\n" (length entries) total (fromIntegral (length entries) / total)

frameName :: Int -> FilePath
frameName = printf "f%05d.bmp"

------------------------------------------------------------------------------
-- Frames and gestures
------------------------------------------------------------------------------

-- | Draw and save one frame, paced to 'fps'.
frame :: Rec -> (Input -> Input) -> IO ()
frame r f = do
  n <- readIORef (recFrame r)
  now <- getMonotonicTime
  let due = recStart r + fromIntegral n / fps
  when (now < due) $ threadDelay (round ((due - now) * 1e6))
  t <- getMonotonicTime
  prev <- readIORef (recLast r)
  pos <- readIORef (recMouse r)
  down <- readIORef (recDown r)
  let inp = f (recBase r) {inputMousePos = pos, inputButtonsHeld = if down then left else noButtons, inputDeltaTime = realToFrac (t - prev)}
  recDraw r inp
  ok <- saveScreenshot (recEnv r) (recDir r </> frameName n)
  unless ok $ fail "record: could not read back the frame"
  writeIORef (recLast r) t
  writeIORef (recFrame r) (n + 1)
  modifyIORef' (recLog r) ((inputMousePos inp, buttonHeld MouseLeft inp, t - recStart r) :)

frames :: Double -> Int
frames secs = max 1 (round (secs * fps))

wait :: Rec -> Double -> IO ()
wait r secs = replicateM_ (frames secs) (frame r id)

-- | Glide the pointer to @to@ over @secs@, easing in and out.
moveTo :: Rec -> V2 -> Double -> IO ()
moveTo r to secs = do
  from <- readIORef (recMouse r)
  let n = frames secs
  forM_ [1 .. n] $ \i -> do
    let s = ease (fromIntegral i / fromIntegral n)
    writeIORef (recMouse r) (lerpV from to s)
    frame r id

ease :: Float -> Float
ease s = if s < 0.5 then 4 * s * s * s else 1 - (-2 * s + 2) ** 3 / 2

lerpV :: V2 -> V2 -> Float -> V2
lerpV (V2 ax ay) (V2 bx by) s = V2 (ax + (bx - ax) * s) (ay + (by - ay) * s)

-- | The left button alone.
left :: MouseButtons
left = buttonsFromList [MouseLeft]

press, release :: Rec -> IO ()
press r = do
  writeIORef (recDown r) True
  frame r (\i -> i {inputButtonsPressed = left})
release r = do
  writeIORef (recDown r) False
  frame r (\i -> i {inputButtonsReleased = left})

click :: Rec -> IO ()
click r = press r >> frame r id >> release r >> wait r 0.2

rightClick :: Rec -> IO ()
rightClick r = do
  frame r (applyMouseButton MouseRight True)
  frame r (applyMouseButton MouseRight False)
  wait r 0.25

-- | Move to @pos@ and click there.
clickAt :: Rec -> V2 -> IO ()
clickAt r pos = moveTo r pos 0.35 >> wait r 0.1 >> click r

-- | Press where the pointer is, drag to @to@ over @secs@, and release.
dragTo :: Rec -> V2 -> Double -> IO ()
dragTo r to secs = press r >> wait r 0.1 >> moveTo r to secs >> wait r 0.1 >> release r >> wait r 0.2

typeText :: Rec -> T.Text -> IO ()
typeText r txt = forM_ (T.unpack txt) $ \c -> do
  frame r (\i -> i {inputChars = T.singleton c})
  frame r id

key :: Rec -> Key -> IO ()
key r k = frame r (keyInp k) >> frame r id

-- | Turn the wheel @notches@ times under the pointer; positive scrolls down.
wheel :: Rec -> Float -> IO ()
wheel r notches = do
  replicateM_ (round (abs notches)) $
    frame r (\i -> i {inputScroll = V2 0 (signum notches)}) >> frame r id
  wait r 0.4


------------------------------------------------------------------------------
-- Finding widgets
------------------------------------------------------------------------------

spans :: Rec -> IO [DemoSpan]
spans r = do
  base <- collectTextSpans (recCtx r)
  over <- collectOverlayTextSpans (recCtx r) (recBase r)
  pure (base ++ over)

-- | The centre of the text span found by @finder@, or a failure naming it.
find :: Rec -> (T.Text -> [DemoSpan] -> Maybe V2) -> T.Text -> IO V2
find r finder needle = do
  ss <- spans r
  maybe (fail ("record: no span " <> show needle)) pure (finder needle ss)

exact, rightmost, header :: Rec -> T.Text -> IO V2
exact r = find r findExact
rightmost r = find r findRightmost
header r = find r findHeader

off :: V2 -> Float -> Float -> V2
off (V2 x y) dx dy = V2 (x + dx) (y + dy)

------------------------------------------------------------------------------
-- The tour
------------------------------------------------------------------------------

tour :: Rec -> IO ()
tour r = do
  wait r 0.6
  -- Controls: a checkbox, a slider, a select, text, a theme, menus.
  clickAt r =<< rightmost r "Feature"
  -- The Volume label sits above the slider's left end; the slider starts at
  -- 50 and spans the column.
  vol <- rightmost r "Volume"
  moveTo r (off vol 198 22) 0.4
  dragTo r (off vol 330 22) 0.7
  clickAt r =<< exact r "Medium"
  clickAt r =<< exact r "High"
  clickAt r =<< exact r "Enter name"
  typeText r "Ada Lovelace"
  wait r 0.3
  clickAt r =<< exact r "Tomorrow Night Min"
  wait r 0.4
  -- The colour picker's square sits left of its Current swatch.
  cur <- exact r "Current"
  moveTo r (off cur (-190) 40) 0.4
  dragTo r (off cur (-100) (-40)) 0.7
  moveTo r (off cur (-57) 60) 0.3
  dragTo r (off cur (-57) (-60)) 0.7
  wait r 0.3
  wheel r 6
  tip <- exact r "Hover for Tooltip"
  moveTo r tip 0.5
  wait r 1.0
  menuBtn <- exact r "Right-click Menu"
  moveTo r menuBtn 0.5
  rightClick r
  clickAt r =<< rightmost r "Copy"
  wait r 0.4
  wheel r (-6)
  -- Graphics: images, icons, an animated GIF, a progress bar.
  clickAt r =<< exact r "Graphics"
  wait r 2.0
  -- Typography: toggles and a live size slider.
  clickAt r =<< exact r "Typography"
  clickAt r =<< exact r "Bold"
  clickAt r =<< exact r "Underline"
  size <- rightmost r "px"
  moveTo r (off size (-300) 22) 0.4
  dragTo r (off size (-80) 22) 0.8
  wait r 0.4
  -- List: a tree and a debounced search.
  clickAt r =<< exact r "List"
  clickAt r =<< exact r "README.md"
  clickAt r =<< rightmost r "Filter people"
  typeText r "eng"
  wait r 0.8
  -- Table: sort one way, then the other.
  clickAt r =<< exact r "Table"
  clickAt r =<< header r "Name"
  clickAt r =<< header r "Name"
  wait r 0.4
  -- Panes: split, maximize and restore, then drag a pane onto another.
  clickAt r =<< exact r "Panes"
  clickAt r =<< exact r "+"
  clickAt r =<< exact r "="
  clickAt r =<< exact r "M"
  wait r 0.3
  clickAt r =<< exact r "R"
  p1 <- exact r "Pane 1"
  moveTo r p1 0.4
  dragTo r (off p1 500 120) 0.9
  wait r 0.4
  -- Plots.
  clickAt r =<< exact r "Plots"
  moveTo r (V2 800 330) 0.6
  moveTo r (V2 520 340) 1.0
  wait r 0.8
  -- Diagnostics, then the Debug window, dragged by its title.
  clickAt r =<< exact r "Diagnostics"
  wait r 0.6
  clickAt r =<< exact r "Debug"
  wait r 0.4
  title <- rightmost r "Debug"
  moveTo r title 0.3
  dragTo r (off title (-300) 120) 0.8
  -- The About modal, closed with Escape.
  clickAt r =<< exact r "About"
  wait r 0.8
  key r KeyEscape
  wait r 0.8

------------------------------------------------------------------------------
-- The pointer
------------------------------------------------------------------------------

-- | Paint an arrow pointer onto a saved frame, tip at @pos@. A held button
-- adds a ring around the tip.
drawPointer :: FilePath -> V2 -> Bool -> IO ()
drawPointer path (V2 px py) down = do
  img <- either (fail . ("record: " <>)) (pure . JP.convertRGB8) =<< JP.readBitmap path
  let tipX = round px :: Int
      tipY = round py :: Int
      ring x y =
        let d = sqrt (fromIntegral ((x - tipX) ^ (2 :: Int) + (y - tipY) ^ (2 :: Int))) :: Float
         in down && d >= 11 && d <= 14
      pixel x y
        | cy >= 0 && cy < length arrow && cx >= 0 && cx < length line =
            case line !! cx of
              'X' -> Just (JP.PixelRGB8 20 20 20)
              '.' -> Just (JP.PixelRGB8 250 250 250)
              _ -> Nothing
        | otherwise = Nothing
        where
          cx = x - tipX
          cy = y - tipY
          line = arrow !! cy
      blend (JP.PixelRGB8 r g b) = JP.PixelRGB8 (mix r 255) (mix g 196) (mix b 64)
      mix :: JP.Pixel8 -> Int -> JP.Pixel8
      mix a c = fromIntegral ((fromIntegral a * 2 + c * 3) `div` 5)
  JP.writeBitmap path $
    JP.generateImage
      ( \x y ->
          let under = JP.pixelAt img x y
           in case pixel x y of
                Just p -> p
                Nothing -> if ring x y then blend under else under
      )
      (JP.imageWidth img)
      (JP.imageHeight img)

arrow :: [String]
arrow =
  [ "X"
  , "XX"
  , "X.X"
  , "X..X"
  , "X...X"
  , "X....X"
  , "X.....X"
  , "X......X"
  , "X.......X"
  , "X........X"
  , "X.........X"
  , "X......XXXXX"
  , "X...X..X"
  , "X..XX..X"
  , "X.X  X..X"
  , "XX   X..X"
  , "X     X..X"
  , "      X..X"
  , "       XX"
  ]
