module Cases.NativeWindow (tests) where

import Data.ByteString qualified as BS
import Data.Maybe (fromJust, isJust)
import Spec

tests :: [Spec]
tests =
  [ spec "native-window-without-host" runWithoutHostTest
  , spec "native-window-pixels" runPixelsTest
  , spec "native-window-screenshot-requests" runScreenshotRequestsTest
  , spec "native-window-settings-at-install" runInstallTest
  , spec "native-window-setters" runSettersTest
  , spec "native-window-commands" runCommandsTest
  , spec "native-window-state" runStateTest
  , spec "native-window-screenshot-from-a-click" runScreenshotFromClickTest
  , spec "native-window-use-screenshot" runUseScreenshotTest
  ]

inp :: Input
inp = withInput 200 100

-- | Install a window host for a window opened with @settings@. It records
-- its calls, newest first.
recordingHost :: WindowSettings -> Context -> IO (IORef [String])
recordingHost settings ctx = do
  calls <- newIORef []
  let note c = modifyIORef' calls (c :)
  installWindowHost ctx settings $
    defaultWindowHost
      { hostSetTitle = \t -> note ("title " ++ show t)
      , hostSetIcon = \img -> note ("icon " ++ show (rgbaWidth img))
      , hostSetMinSize = \s -> note ("min " ++ show s)
      , hostSetMaxSize = \s -> note ("max " ++ show s)
      , hostSetOpacity = \o -> note ("opacity " ++ show o)
      , hostSetMode = \m -> note ("mode " ++ show m)
      , hostMove = \x y -> note ("move " ++ show (x, y))
      , hostCenter = note "center"
      , hostResize = \s -> note ("resize " ++ show s)
      , hostMinimize = note "minimize"
      , hostMaximize = note "maximize"
      , hostRestore = note "restore"
      }
  pure calls

-- | What a view asks of the host in a frame, oldest first.
asked :: Context -> IORef [String] -> NanoUI a -> IO [String]
asked ctx calls v = warmup ctx inp v >> reverse <$> readIORef calls <* writeIORef calls []

solid :: Int -> Int -> RgbaPixels
solid w h = fromJust (rgbaPixels w h (BS.replicate (w * h * 4) 255))

-- | Without a window, a screenshot is answered at once with nothing,
-- requests do nothing, and the window reports the frame's size, focused and
-- in its normal state.
runWithoutHostTest :: Context -> IORef Int -> IO ()
runWithoutHostTest ctx failed = do
  answers <- newIORef []
  st <- evalUi ctx inp $ do
    requestScreenshot (\shot -> modifyIORef' answers (fmap screenshotScale shot :))
    setWindowTitleUi "title"
    setWindowIconUi (solid 2 2)
    setWindowMinSizeUi (Just (Size 10 10))
    setWindowModeUi Fullscreen
    moveWindowUi 5 6
    toggleMaximizedUi
    quitUi
    askWindow
  assertEq failed [Nothing] =<< readIORef answers
  assertEq failed defaultWindowState {winSize = Size 200 100} st
  assertEq failed False =<< quitRequested ctx
  shoot <- evalUi ctx inp askScreenshot
  assertEq failed Nothing =<< shoot
  -- A close request ends a session with no window to ask.
  assert failed =<< requestWindowClose ctx

-- | Pixels are built only from 4 bytes per pixel and a positive size.
runPixelsTest :: Context -> IORef Int -> IO ()
runPixelsTest _ failed = do
  let bytes n = BS.replicate n 7
  assertEq failed [True, False, False, False, False] $
    map isJust [rgbaPixels 2 3 (bytes 24), rgbaPixels 2 3 (bytes 23), rgbaPixels 2 3 (bytes 25), rgbaPixels 0 3 (bytes 0), rgbaPixels (-2) (-3) (bytes 24)]
  assertEq failed (Just (2, 3, 24)) ((\p -> (rgbaWidth p, rgbaHeight p, BS.length (rgbaBytes p))) <$> rgbaPixels 2 3 (bytes 24))

-- | Screenshots wait for the backend until after their frame, then are
-- answered once, in order, from a single capture at the last reported scale.
-- Answering requests a frame.
runScreenshotRequestsTest :: Context -> IORef Int -> IO ()
runScreenshotRequestsTest ctx failed = do
  calls <- recordingHost defaultWindowSettings ctx
  reportWindowState ctx defaultWindowState {winScale = 2}
  answers <- newIORef ([] :: [(String, Maybe (Int, Float))])
  captures <- newIORef (0 :: Int)
  let capture = Just (solid 3 2) <$ modifyIORef' captures (+ 1)
      ask names = warmup ctx inp . (label "shots" >>) . forM_ names $ \name ->
        requestScreenshot (\s -> modifyIORef' answers ((name, (\sh -> (rgbaWidth (screenshotPixels sh), screenshotScale sh)) <$> s) :))
      expect as n = assertEq failed (as, n) =<< ((,) <$> readIORef answers <*> readIORef captures)
      two = [("second", Just (3, 2)), ("first", Just (3, 2))]
  ask []
  -- Nothing waiting: the capture does not run.
  answerScreenshots ctx capture
  expect [] 0
  ask ["first", "second"]
  expect [] 0
  clearDirty ctx
  answerScreenshots ctx capture
  expect two 1
  assert failed =<< isDirty ctx
  answerScreenshots ctx capture
  expect two 1
  -- A backend that cannot capture, or a host replaced first, answers with nothing.
  ask ["failed"]
  answerScreenshots ctx (pure Nothing)
  expect (("failed", Nothing) : two) 1
  ask ["replaced"]
  _ <- recordingHost defaultWindowSettings ctx
  expect (("replaced", Nothing) : ("failed", Nothing) : two) 1
  -- Nothing was asked of the window itself.
  assertEq failed [] =<< readIORef calls

-- | Installing a host applies, through it, the settings a window does not
-- open with: size limits, icon, opacity and position. Setters then act only
-- on values that differ from the opening settings.
runInstallTest :: Context -> IORef Int -> IO ()
runInstallTest ctx failed = do
  let settings =
        defaultWindowSettings
          { wsTitle = "opened", wsMinSize = Just (Size 100 50), wsIcon = Just (solid 2 2), wsOpacity = 0.5
          , wsPosition = WindowPositionAt 30 20, wsMode = Fullscreen
          }
  calls <- recordingHost settings ctx
  assertEq failed ["min Just (Size {sizeW = 100.0, sizeH = 50.0})", "icon 2", "opacity 0.5", "move (30,20)"] . reverse =<< readIORef calls
  writeIORef calls []
  assertEq failed [] =<< asked ctx calls (setWindowTitleUi "opened" >> setWindowModeUi Fullscreen >> setWindowOpacityUi 0.5)
  assertEq failed ["center"] =<< readIORef =<< recordingHost defaultWindowSettings {wsPosition = WindowPositionCentered} ctx
  assertEq failed [] =<< readIORef =<< recordingHost defaultWindowSettings ctx

-- | Setters reach the host only on a change; opacity is kept to 0..1.
runSettersTest :: Context -> IORef Int -> IO ()
runSettersTest ctx failed = do
  calls <- recordingHost defaultWindowSettings ctx
  let view (title, icon, minSize, opacity, mode) = do
        setWindowTitleUi title
        setWindowIconUi (solid icon icon)
        setWindowMinSizeUi minSize
        setWindowMaxSizeUi Nothing
        setWindowOpacityUi opacity
        setWindowModeUi mode
      first = ("a", 2, Just (Size 100 50), 0.5, Windowed)
  forM_
    [ (first, ["title \"a\"", "icon 2", "min Just (Size {sizeW = 100.0, sizeH = 50.0})", "opacity 0.5"])
    , (first, [])
    , (first, [])
    , (("a", 4, Just (Size 100 50), 7, Hidden), ["icon 4", "opacity 1.0", "mode Hidden"])
    , (("b", 4, Nothing, 1, Hidden), ["title \"b\"", "min Nothing"])
    ]
    $ \(v, expect) -> assertEq failed expect =<< asked ctx calls (view v)

-- | Commands act on every call; toggling maximizes or restores based on the
-- window state the backend last reported.
runCommandsTest :: Context -> IORef Int -> IO ()
runCommandsTest ctx failed = do
  calls <- recordingHost defaultWindowSettings ctx
  let commands = moveWindowUi 5 6 >> centerWindowUi >> resizeWindowUi (Size 300 200) >> minimizeWindowUi >> maximizeWindowUi >> restoreWindowUi
  replicateM_ 2 $
    assertEq failed ["move (5,6)", "center", "resize Size {sizeW = 300.0, sizeH = 200.0}", "minimize", "maximize", "restore"]
      =<< asked ctx calls commands
  assertEq failed ["maximize"] =<< asked ctx calls toggleMaximizedUi
  reportWindowState ctx defaultWindowState {winMaximized = True}
  assertEq failed ["restore"] =<< asked ctx calls toggleMaximizedUi

-- | A view reads the state the backend reported, at the frame's size. A
-- change requests a frame only once a view has read the state.
runStateTest :: Context -> IORef Int -> IO ()
runStateTest ctx failed = do
  _ <- recordingHost defaultWindowSettings ctx
  let reported = defaultWindowState {winSize = Size 9 9, winScale = 2, winPosition = Just (10, 20), winFocused = False, winFullscreen = True}
      changed st = clearDirty ctx >> reportWindowState ctx st >> isDirty ctx
  assertEq failed False =<< changed reported
  assertEq failed reported {winSize = Size 200 100} =<< evalUi ctx inp askWindow
  assertEq failed False =<< changed reported
  assertEq failed True =<< changed reported {winFocused = True}

-- | A screenshot requested from a click is requested once, even though the
-- click's hook write reruns the view.
runScreenshotFromClickTest :: Context -> IORef Int -> IO ()
runScreenshotFromClickTest ctx failed = do
  _ <- recordingHost defaultWindowSettings ctx
  answers <- newIORef (0 :: Int)
  passes <- newIORef (0 :: Int)
  let view = do
        uiIO (modifyIORef' passes (+ 1))
        (shots, setShots) <- useInt 0
        resp <- button' "Shot"
        when (respClicked resp) (requestScreenshot (\_ -> modifyIORef' answers (+ 1)) >> setShots (shots + 1))
        pure resp
  resp <- warmup2 ctx inp view
  let (press, release) = clickPair inp (centerOf resp)
  warmup ctx press view
  writeIORef passes 0
  warmup ctx release view
  assertEq failed 2 =<< readIORef passes
  answerScreenshots ctx (pure (Just (solid 1 1)))
  assertEq failed 1 =<< readIORef answers

-- | 'useScreenshot' waits in a background task for the next frame on screen,
-- wakes the loop, and returns the screenshot. The same key captures only
-- once.
runUseScreenshotTest :: Context -> IORef Int -> IO ()
runUseScreenshotTest ctx failed = do
  _ <- recordingHost defaultWindowSettings ctx
  wait <- newWakeSignal ctx
  captures <- newIORef (0 :: Int)
  let capture = Just (solid 4 4) <$ modifyIORef' captures (+ 1)
      ui = fmap (rgbaWidth . screenshotPixels) <$> useScreenshot ("shot" :: String)
      -- Run frames, answering each as a backend would, until the view has
      -- its screenshot.
      go :: Int -> IO (Maybe Int)
      go n = do
        shot <- evalUi ctx inp ui
        answerScreenshots ctx capture
        if isJust shot || n == 0 then pure shot else wait 2000000 >> go (n - 1)
  assertEq failed (Just 4) =<< go 20
  assertEq failed 1 =<< readIORef captures
  replicateM_ 3 (evalUi ctx inp ui >> answerScreenshots ctx capture)
  assertEq failed 1 =<< readIORef captures
  cancelTasks ctx
