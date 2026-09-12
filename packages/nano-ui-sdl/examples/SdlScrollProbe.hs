-- | Deterministic scroll-stepping probe against the real SDL3 render path.
--
-- Steps a scrollable page (State card kv rows + Controls form widgets) by a
-- known wheel delta per frame, saving the presented frame each step so banded
-- text/geometry dephase can be reproduced offline and measured from the saved
-- screenshots. The live demo (trackpad, scale 2) shows per-band +/-1 logical
-- px jumps between adjacent scroll frames; this probe drives the exact same
-- pipeline with controlled deltas to reproduce and bisect that artifact.

module Main (main) where

import Control.Monad (void, when)
import Data.IORef (newIORef, readIORef, writeIORef)
import GHC.Conc (threadDelay)
import GHC.IO.Encoding (setLocaleEncoding, utf8)
import NanoUI
import NanoUI.Backend.Sdl (SdlEnv (..), SdlOptions (..), defaultSdlOptions, newSdlContext, saveScreenshot, sdlDrawFrame)
import NanoUI.Sdl.Session (runSdlSession)
import NanoUI.Testing (Context)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO (hFlush, hSetEncoding, stderr, stdout)
import System.IO.Unsafe (unsafePerformIO)

probeWindow :: Size
probeWindow = Size 720 929

probeWidth :: Float
probeWidth = 720

probeHeight :: Float
probeHeight = 929

probeOutRoot :: FilePath
probeOutRoot = "/tmp/opencode/sdlprobe"

-- | Mirrors the demo's outer page: a scrollable column of kv cards plus the
-- Controls-tab form widgets (the rows that showed banded dephase live).
scrollProbeUi :: NanoUI ()
scrollProbeUi =
  scrollWith (tight . grow) $
    columnWith (padAll 6 . gap 12 . fillW) $ do
      panelWith (padXY 14 10 . gap 6 . fillW) $ do
        heading "State"
        kv "Feature" "on"
        kv "Volume" "50"
        kv "Theme" "Tomorrow at Midnight Min"
        kv "Theme radio" "Tomorrow Min"
        kv "Name" "nano"
        kv "Notes" "Edit me."
        sep
      panelWith (padXY 14 10 . gap 6 . fillW) $ do
        heading "Controls"
        void (checkbox "Feature" False)
        void (label "Volume")
        void (slider 0 100 50)
        let qualities = ["Low", "Medium", "High"]
        void (selectLabeled "Quality" qualities 1)
        void (label "Accent")
        void (colorPicker (NanoUI.colorRGBA 204 102 102 255))
        muted "Theme"
        void (radioFieldset ["Light", "Dark", "System"] 1)
        muted "Name"
        void (textInputWithPlaceholder "Enter name" "")
        muted "Notes"
        void (textArea "Edit me.\nSecond line.")
        rowWith (tight . gap 8 . fillW) $ do
          void (button "Hover for Tooltip")
          void (button "Right-click Menu")
        sep
      panelWith (padXY 14 10 . gap 6 . fillW) $ do
        heading "Popups"
        kv "Value" "right-aligned value"
        kv "Checked" "off"
        kv "Notes" "third card"
        sep
      panelWith (padXY 14 10 . gap 6 . fillW) $ do
        heading "Fourth"
        kv "Volume" "50"
        kv "Theme" "Tomorrow at Midnight Min"
        kv "Name" "nano"
        kv "Notes" "Edit me."
        sep
      panelWith (padXY 14 10 . gap 6 . fillW) $ do
        heading "Fifth"
        muted "Notes"
        void (textArea "Edit me.\nLine2\nLine3\nLine4\nLine5\nLine6\nLine7\nLine8\nLine9\nLine10")
        void (selectLabeled "Theme" ["Tomorrow at Midnight Min"] 0)
        sep
      panelWith (padXY 14 10 . gap 6 . fillW) $ do
        heading "Sixth"
        kv "Feature" "on"
        kv "Value" "bottom of the page"

probeOpts :: SdlOptions
probeOpts =
  defaultSdlOptions
    { sdlWindowSize = probeWindow
    , sdlWindowTitle = "nano-ui-scrollprobe"
    , sdlWindowResizable = False
    , sdlAppTheme = Just tomorrowNightMinDarkTheme
    , sdlAppVsync = False
    , sdlAppContinuous = True
    }

-- | Drive one sweep through the real session loop. 'drawFn' injects a fixed
-- wheel delta and mirrors the session drawing states, snapshotting each frame.
runSweep :: Float -> Int -> IO ()
runSweep delta n = do
  ctx0 <- newSdlContext
  frameRef <- newIORef (0 :: Int)
  done <- newIORef False
  let outDir = probeOutRoot </> ("d" ++ show delta)
  createDirectoryIfMissing True outDir
  let shouldQuit _ = unsafePerformIO (readIORef done)
      drawFn _c env inp _force = do
        i <- readIORef frameRef
        when (i <= 1) $ do
          sc <- readIORef (sdlScaleRef env)
          putStrLn ("  scaleRef=" ++ show sc ++ " winLogical=" ++ show (inputWindowSize inp))
        if i > n
          then do
            writeIORef done True
            pure (False, inp)
          else do
            let inp' =
                  inp
                    { inputScroll = V2 0 delta
                    , inputMousePos = V2 (probeWidth / 2) (probeHeight / 2)
                    }
            void (sdlDrawFrame _c scrollProbeUi env inp' True)
            saveFrame env _c inp' (outDir </> ("frame_" ++ show i ++ ".bmp"))
            writeIORef frameRef (i + 1)
            pure (True, inp)
      setup env = do
        sc <- readIORef (sdlScaleRef env)
        putStrLn ("  setup scaleRef=" ++ show sc)
  runSdlSession probeOpts ctx0 setup shouldQuit drawFn
  threadDelay 200000

saveFrame :: SdlEnv -> Context -> Input -> FilePath -> IO ()
saveFrame env _ctx _inp path = do
  ok <- saveScreenshot env path
  putStrLn ("  saved " ++ path ++ " -> " ++ show ok)
  hFlush stdout

main :: IO ()
main = do
  setLocaleEncoding utf8
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  putStrLn "-- sweep 1: fine fractional scroll (delta 0.2) --"
  runSweep 0.5 4
  putStrLn "-- sweep 2: finer fractional scroll (delta 0.1) --"
  runSweep 0.1 2
  putStrLn "-- sweep 3: half-notch (delta 0.5) --"
  runSweep 0.5 12
  putStrLn "-- sweep 4: fractional page offset (delta 0.033, 0.66 logical/frame) --"
  runSweep 0.033 60
  putStrLn "probe complete"