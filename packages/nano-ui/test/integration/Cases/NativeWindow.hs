{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cases.NativeWindow (tests) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, try)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import Data.Maybe (fromJust, isJust, isNothing)
import Spec
import GHC.TypeLits (symbolSing)
import NanoUI.Internal.Context (Context (..))
import NanoUI.Testing.Sim
import System.Timeout (timeout)

tests :: [Spec]
tests =
  [ spec "native-window-without-host" runWithoutHostTest
  , spec "native-window-screenshot-requests" runScreenshotRequestsTest
  , spec "native-window-screenshot-lifecycle" runScreenshotLifecycleTest
  , spec "native-window-setters" runSettersTest
  , spec "native-window-commands-and-state" runCommandsTest
  ]

type instance BackendSession ('Custom "test") = Int

inp :: Input
inp = withInput 200 100

-- | Install a host for a window opened with @settings@, recording its calls newest first.

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

-- | A fresh context with a recording host for a default window.
recording :: IO (Context, IORef [String])
recording = newContext >>= \ctx -> (,) ctx <$> recordingHost defaultWindowSettings ctx

-- | What a view asks of the host in a frame, oldest first.
asked :: Context -> IORef [String] -> NanoUI a -> IO [String]
asked ctx calls v = warmup ctx inp v >> reverse <$> readIORef calls <* writeIORef calls []

solid :: Int -> Int -> RgbaPixels
solid w h = fromJust (rgbaPixels w h (BS.replicate (w * h * 4) 255))

-- | Whether the action threw an 'IOException'.
throws :: IO a -> IO Bool
throws action = isLeft <$> try @IOException action

-- | Pixels need 4 bytes a pixel and a positive size. Without a window, requests do
-- nothing, a screenshot is answered with nothing, and the state is the frame's. A
-- backend session value is found by its own singleton only, and leaves with the session.
runWithoutHostTest :: Context -> IORef Int -> IO ()
runWithoutHostTest ctx failed = do
  let bytes n = BS.replicate n 7
      sized = [(2, 3, 24), (2, 3, 23), (2, 3, 25), (0, 3, 0), (-2, -3, 24)]
  assertEq failed [True, False, False, False, False] [isJust (rgbaPixels w h (bytes n)) | (w, h, n) <- sized]
  assertEq failed Nothing (rgbaPixels (maxBound `div` 2 + 1) 4 BS.empty)
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
  assertEq failed Nothing =<< join (evalUi ctx inp askScreenshot)
  assertEq failed noWindowCapabilities =<< warmup2 ctx inp askWindowCapabilities
  assert failed =<< requestWindowClose ctx
  let caps = allWindowCapabilities {wcMove = False}
  installWindowHost ctx defaultWindowSettings defaultWindowHost {hostCapabilities = caps}
  assertEq failed caps =<< warmup2 ctx inp askWindowCapabilities
  let here = SCustom (symbolSing @"test")
  separate <- newContext
  withBackendSession ctx here 7 $ do
    assertEq failed (Just 7) =<< warmup2 ctx {ctxHotId = ctxHotId ctx} inp (askBackendSession here)
    assert failed . isNothing =<< backendSession SSdl ctx
    assert failed . isNothing =<< backendSession (SCustom (symbolSing @"other")) ctx
    assert failed . isNothing =<< backendSession here separate
  assert failed . isNothing =<< backendSession here ctx

-- | Screenshots are answered once each after their frame, in order, from one capture
-- at the reported scale, requesting a frame; failures and throws still answer all.
runScreenshotRequestsTest :: Context -> IORef Int -> IO ()
runScreenshotRequestsTest ctx failed = do
  calls <- recordingHost defaultWindowSettings ctx
  reportWindowState ctx defaultWindowState {winScale = 2}
  answers <- newIORef ([] :: [(String, Maybe (Int, Float))])
  captures <- newIORef (0 :: Int)
  let capture = Just (solid 3 2) <$ modifyIORef' captures (+ 1)
      shot sh = (rgbaWidth (screenshotPixels sh), screenshotScale sh)
      ask names = warmup ctx inp . (label "shots" >>) . forM_ names $ \name ->
        requestScreenshot (\s -> modifyIORef' answers ((name, shot <$> s) :))
      expectNow as n = assertEq failed (as, n) =<< ((,) <$> readIORef answers <*> readIORef captures)
      two = [("second", Just (3, 2)), ("first", Just (3, 2))]
  ask []
  answerScreenshots ctx capture
  expectNow [] 0
  ask ["first", "second"]
  expectNow [] 0
  clearDirty ctx
  answerScreenshots ctx capture
  expectNow two 1
  assert failed =<< isDirty ctx
  answerScreenshots ctx capture
  expectNow two 1
  ask ["failed"]
  answerScreenshots ctx (pure Nothing)
  expectNow (("failed", Nothing) : two) 1
  ask ["replaced"]
  _ <- recordingHost defaultWindowSettings ctx
  expectNow (("replaced", Nothing) : ("failed", Nothing) : two) 1
  assertEq failed [] =<< readIORef calls
  done <- newIORef []
  let request action = evalUi ctx inp (requestScreenshot action)
      note s = modifyIORef' done (isJust s :)
      broken = ioError (userError "capture failed")
  request note >> request note
  assert failed =<< throws (answerScreenshots ctx broken)
  assertEq failed [False, False] =<< readIORef done
  writeIORef done []
  request (const broken) >> request note
  assert failed =<< throws (answerScreenshots ctx (pure (Just (solid 1 1))))
  assertEq failed [True] =<< readIORef done
  events <- newIORef ([] :: [String])
  request (\_ -> modifyIORef' events ("answer" :))
  let event e = modifyIORef' events (e :)
  answerScreenshotsAfter ctx (Nothing <$ event "capture") (event "present")
  assertEq failed ["answer", "present", "capture"] =<< readIORef events
  request (const broken) >> request note
  assert failed =<< throws (closeWindowHost ctx)
  assertEq failed [False, True] =<< readIORef done
  closeWindowHost ctx

-- | 'useScreenshot' waits in a task for the next frame on screen; a key captures once.
-- A screenshot requested from a click is requested once, though the click reruns the
-- view. Closing or replacing a window answers pending requests; a closed host is not
-- called.
runScreenshotLifecycleTest :: Context -> IORef Int -> IO ()
runScreenshotLifecycleTest base failed = do
  job <- newTask
  _ <- recordingHost defaultWindowSettings base
  woken <- newWakeSignal base
  captures <- newIORef (0 :: Int)
  let capture = Just (solid 4 4) <$ modifyIORef' captures (+ 1)
      ui = fmap (rgbaWidth . screenshotPixels) <$> useScreenshot job ("shot" :: String)
      answered = evalUi base inp ui <* answerScreenshots base capture
      go :: Int -> IO (Maybe Int)
      go n = answered >>= \s -> if isJust s || n == 0 then pure s else woken 2000000 >> go (n - 1)
  assertEq failed (Just 4) =<< go 20
  replicateM_ 3 answered
  assertEq failed 1 =<< readIORef captures
  cancelTasks base
  answers <- newIORef (0 :: Int)
  passes <- newIORef (0 :: Int)
  let view = do
        liftIO (modifyIORef' passes (+ 1))
        shotsVar <- useVar (0 :: Int)
        shots <- readVar shotsVar
        whenM (button "Shot") $
          requestScreenshot (\_ -> modifyIORef' answers (+ 1)) >> writeVar shotsVar (shots + 1)
  runSim base failed inp view $ do
    _ <- press "Shot"
    liftIO (writeIORef passes 0)
    _ <- release
    checkEq 2 =<< liftIO (readIORef passes)
  answerScreenshots base (pure (Just (solid 1 1)))
  assertEq failed 1 =<< readIORef answers
  ctx <- newContext
  _ <- recordingHost defaultWindowSettings ctx
  oldShoot <- evalUi ctx inp askScreenshot
  _ <- recordingHost defaultWindowSettings ctx
  assertEq failed (Just Nothing) =<< timeout 1000000 oldShoot
  wait <- newWakeSignal ctx
  shoot <- evalUi ctx inp askScreenshot
  answer <- newEmptyMVar
  _ <- forkIO (shoot >>= putMVar answer)
  _ <- wait 1000000
  closeWindowHost ctx
  assertEq failed (Just Nothing) =<< timeout 1000000 (takeMVar answer)
  assertEq failed (Just Nothing) =<< timeout 1000000 shoot
  closeWindowHost ctx
  calls <- recordingHost defaultWindowSettings ctx
  closeWindowHost ctx
  evalUi ctx inp (setWindowTitleUi "closed")
  assertEq failed [] =<< readIORef calls

-- | Installing a host applies the settings a window does not open with; setters then
-- reach it only on a change, retrying after a failure. A size limit set past the
-- other moves the other first, so the host never sees a minimum above the maximum.
runSettersTest :: Context -> IORef Int -> IO ()
runSettersTest ctx failed = do
  let sz w h = "Just (Size {sizeW = " ++ show (w :: Float) ++ ", sizeH = " ++ show (h :: Float) ++ "})"
      opened =
        defaultWindowSettings
          { wsTitle = "opened", wsMinSize = Just (Size 100 50), wsIcon = Just (solid 2 2), wsOpacity = 0.5
          , wsPosition = WindowPositionAt 30 20, wsMode = Fullscreen
          }
  calls <- recordingHost opened ctx
  assertEq failed ["min " ++ sz 100 50, "icon 2", "opacity 0.5", "move (30,20)"] . reverse =<< readIORef calls
  writeIORef calls []
  let unchanged = setWindowTitleUi "opened" >> setWindowModeUi Fullscreen >> setWindowOpacityUi 0.5
      centered = defaultWindowSettings {wsPosition = WindowPositionCentered}
  assertEq failed [] =<< asked ctx calls unchanged
  assertEq failed ["center"] =<< readIORef =<< recordingHost centered ctx
  assertEq failed [] =<< readIORef =<< recordingHost defaultWindowSettings ctx
  (c1, calls1) <- recording
  let view (title, icon, minSize, opacity, mode) = do
        setWindowTitleUi title
        setWindowIconUi (solid icon icon)
        setWindowMinSizeUi minSize
        setWindowMaxSizeUi Nothing
        setWindowOpacityUi opacity
        setWindowModeUi mode
      first = ("a", 2, Just (Size 100 50), 0.5, Windowed)
  forM_
    [ (first, ["title \"a\"", "icon 2", "min " ++ sz 100 50, "opacity 0.5"])
    , (first, [])
    , (first, [])
    , (("a", 4, Just (Size 100 50), 7, Hidden), ["icon 4", "opacity 1.0", "mode Hidden"])
    , (("b", 4, Nothing, 1, Hidden), ["title \"b\"", "min Nothing"])
    ]
    $ \(v, want) -> assertEq failed want =<< asked c1 calls1 (view v)
  (c2, calls2) <- recording
  forM_
    [ (setWindowMaxSizeUi (Just (Size 300 200)), ["max " ++ sz 300 200])
    , (setWindowMinSizeUi (Just (Size 400 100)), ["max " ++ sz 400 200, "min " ++ sz 400 100])
    , (setWindowMaxSizeUi (Just (Size 350 50)), ["min " ++ sz 350 50, "max " ++ sz 350 50])
    , (setWindowMinSizeUi (Just (Size 0 80)), ["max " ++ sz 350 80, "min " ++ sz 0 80])
    , (setWindowMaxSizeUi Nothing, ["max Nothing"])
    , (setWindowMinSizeUi (Just (Size 500 500)), ["min " ++ sz 500 500])
    ]
    $ \(v, want) -> assertEq failed want =<< asked c2 calls2 v
  c3 <- newContext
  attempts <- newIORef (0 :: Int)
  installWindowHost c3 defaultWindowSettings defaultWindowHost
    { hostSetTitle = \_ -> do
        modifyIORef' attempts (+ 1)
        n <- readIORef attempts
        when (n == 1) (ioError (userError "native failure"))
    }
  assert failed =<< throws (evalUi c3 inp (setWindowTitleUi "retry"))
  replicateM_ 2 (evalUi c3 inp (setWindowTitleUi "retry"))
  assertEq failed 2 =<< readIORef attempts
  -- A native limit is scaled, rounded and outset; 0, NaN or infinity is none, and a
  -- huge one stops at Int32's top.
  forM_
    [ ((302, 0), sizeLimitAt 1.5 (2, 3) (Just (Size 200 (-1))))
    , ((0, 908), sizeLimitAt 1 (8, 8) (Just (Size (1 / 0) 900)))
    , ((0, 900), sizeLimitAt 1 (0, 0) (Just (Size (0 / 0) 900)))
    , ((0x7FFFFFFF, 0), sizeLimitAt 1 (8, 0) (Just (Size 3e9 0)))
    , ((0x7FFFFFFF, 0x7FFFFFFF), sizeLimitAt 2 (16, 39) (Just (Size 1e20 3.4e38)))
    ]
    $ uncurry (assertEq failed)

-- | Commands act on every call; toggling follows the reported state. A state change
-- wakes only a view that read it, and repaints one that paints it.
runCommandsTest :: Context -> IORef Int -> IO ()
runCommandsTest ctx failed = do
  calls <- recordingHost defaultWindowSettings ctx
  let commands = do
        moveWindowUi 5 6 >> centerWindowUi >> resizeWindowUi (Size 300 200)
        minimizeWindowUi >> maximizeWindowUi >> restoreWindowUi
  replicateM_ 2 $
    assertEq failed ["move (5,6)", "center", "resize " ++ show (Size 300 200), "minimize", "maximize", "restore"]
      =<< asked ctx calls commands
  assertEq failed ["maximize"] =<< asked ctx calls toggleMaximizedUi
  reportWindowState ctx defaultWindowState {winMaximized = True}
  assertEq failed ["restore"] =<< asked ctx calls toggleMaximizedUi
  (c, _) <- recording
  let reported =
        defaultWindowState
          {winSize = Size 9 9, winScale = 2, winPosition = Just (10, 20), winFocused = False, winFullscreen = True}
      changed st = clearDirty c >> reportWindowState c st >> isDirty c
      ui = do
        st <- askWindow
        label "focus"
        box (fixedWH 40 40) (if winFocused st then colorRGBA 0 255 0 255 else colorRGBA 128 128 128 255)
  assertEq failed False =<< changed reported
  assertEq failed reported {winSize = Size 200 100} =<< evalUi c inp askWindow
  assertEq failed False =<< changed reported
  replicateM_ 3 (runFrame c inp ui)
  assert failed . (/= DamageFull) =<< takeDamage c
  assertEq failed True =<< changed reported {winFocused = True}
  _ <- runFrame c inp ui
  assertEq failed DamageFull =<< takeDamage c

