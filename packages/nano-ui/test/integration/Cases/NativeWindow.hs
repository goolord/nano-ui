module Cases.NativeWindow (tests) where

import Data.ByteString qualified as BS
import Spec

tests :: [Spec]
tests =
  [ spec "native-window-without-host" runWithoutHostTest
  , spec "native-window-screenshot-requests" runScreenshotRequestsTest
  , spec "native-window-setters" runSettersTest
  , spec "native-window-screenshot-from-a-click" runScreenshotFromClickTest
  ]

inp :: Input
inp = withInput 200 100

-- | Install a window host that records what it is asked, newest first.
recordingHost :: Context -> IO (IORef [String])
recordingHost ctx = do
  calls <- newIORef []
  let note c = modifyIORef' calls (c :)
  installWindowHost ctx $
    WindowHost
      { hostSetIcon = \img -> note ("icon " ++ show (rgbaImageWidth img))
      , hostSetMinSize = \s -> note ("min " ++ show s)
      , hostSetMaxSize = \s -> note ("max " ++ show s)
      , hostSetPosition = \p -> note ("position " ++ show p)
      , hostSetOpacity = \o -> note ("opacity " ++ show o)
      }
  pure calls

solid :: Int -> Int -> RgbaImage
solid w h = RgbaImage (ImageId 0) w h (BS.replicate (w * h * 4) 255)

-- | Without a window a screenshot is answered at once with nothing, and setters do nothing.
runWithoutHostTest :: Context -> IORef Int -> IO ()
runWithoutHostTest ctx failed = do
  answers <- newIORef []
  _ <- runFrame ctx inp $ do
    requestScreenshot (\shot -> modifyIORef' answers (fmap rgbaImageWidth shot :))
    setWindowIconUi (solid 2 2)
    setWindowMinSizeUi (Just (Size 10 10))
    setWindowPositionUi WindowPositionCentered
    setWindowOpacityUi 0.5
  assertEq failed [Nothing] =<< readIORef answers

-- | Screenshots wait for the backend, after their frame, and are answered once,
-- in order, from one capture, which asks for a frame.
runScreenshotRequestsTest :: Context -> IORef Int -> IO ()
runScreenshotRequestsTest ctx failed = do
  calls <- recordingHost ctx
  answers <- newIORef ([] :: [(String, Maybe Int)])
  captures <- newIORef (0 :: Int)
  let capture = Just (solid 3 2) <$ modifyIORef' captures (+ 1)
      ask names = warmup ctx inp . (label "shots" >>) . forM_ names $ \name ->
        requestScreenshot (\s -> modifyIORef' answers ((name, rgbaImageWidth <$> s) :))
      expect as n = assertEq failed (as, n) =<< ((,) <$> readIORef answers <*> readIORef captures)
      two = [("second", Just 3), ("first", Just 3)]
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
  _ <- recordingHost ctx
  expect (("replaced", Nothing) : ("failed", Nothing) : two) 1
  -- Nothing was asked of the window itself.
  assertEq failed [] =<< readIORef calls

-- | A screenshot asked for from a click is asked once, though the click's hook
-- write runs the view again.
runScreenshotFromClickTest :: Context -> IORef Int -> IO ()
runScreenshotFromClickTest ctx failed = do
  _ <- recordingHost ctx
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

-- | Setters reach the host only on a change, but every move does (the user
-- moves the window too).
runSettersTest :: Context -> IORef Int -> IO ()
runSettersTest ctx failed = do
  calls <- recordingHost ctx
  let frame v = warmup ctx inp v >> readIORef calls <* writeIORef calls []
      view (icon, minSize, opacity) = do
        setWindowIconUi (solid icon icon)
        setWindowMinSizeUi minSize
        setWindowMaxSizeUi Nothing
        setWindowOpacityUi opacity
      first = (2, Just (Size 100 50), 0.5)
  -- The same again does nothing; a change reaches the host alone; opacity is kept to 0..1.
  forM_ [(first, ["opacity 0.5", "max Nothing", "min Just (Size {sizeW = 100.0, sizeH = 50.0})", "icon 2"]), (first, []), (first, [])
        , ((4, Just (Size 100 50), 7), ["opacity 1.0", "icon 4"]), ((4, Nothing, 1), ["min Nothing"])] $ \(v, expect) ->
    assertEq failed expect =<< frame (view v)
  replicateM_ 2 $ assertEq failed ["position WindowPositionAt 5 6"] =<< frame (setWindowPositionUi (WindowPositionAt 5 6))
