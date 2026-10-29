{-# LANGUAGE PackageImports #-}

-- | Window options as SDL reports them, screenshots from retained and
-- continuous sessions, transparent-window alpha, and the session's opening
-- frames, resize-watch frames and render device resets.
module WindowChecks (windowChecks) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad (replicateM_, unless, void, when)
import Data.Bits (zeroBits, (.&.))
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, nub)
import Data.Maybe (fromJust, isNothing)
import Data.Primitive.SmallArray (smallArrayFromList)
import Foreign.C.String (peekCString)
import Foreign.C.Types (CBool (..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable, peek, poke)
import GHC.Clock (getMonotonicTime)
import NanoUI
import NanoUI.Backend (emptyInput, runNanoUI)
import NanoUI.Emit (emit)
import NanoUI.Sdl.Internal.DialogState qualified as Dialog
import NanoUI.Testing (Context, isDirty, newPixelContext)
import NanoUI.Testing.Assert (require, requireEq)
import SDL3.Sys.Blendmode qualified as Blend
import SDL3.Sys.Bindgen.Events
  ( SDL_MouseButtonEvent (SDL_MouseButtonEvent), SDL_QuitEvent (SDL_QuitEvent), SDL_RenderEvent (SDL_RenderEvent)
  , SDL_WindowEvent (SDL_WindowEvent)
  )
import SDL3.Sys.Bindgen.Events qualified as Events
import SDL3.Sys.Bindgen.Mouse (sDL_BUTTON_MIDDLE, sDL_BUTTON_X1)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Events (pushEventSafe)
import SDL3.Sys.Video
  (getWindowFlags, getWindowMaximumSize, getWindowMinimumSize, getWindowOpacity, getWindowSize, getWindowTitle)
import SDL3.Sys.Video qualified as SDL
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import "nano-ui-sdl" NanoUI.Backend.Sdl
import "nano-ui-sdl" NanoUI.Sdl.Internal.Window (sdlUiScaleRef, sdlWindow, sdlTransparent)

-- | Run all checks. @gpu@ means a real display with a GPU renderer, which
-- adds checks the dummy driver cannot support: icon, opacity, position, and
-- custom blend modes for transparency.
windowChecks :: String -> Bool -> IO ()
windowChecks drivers gpu = do
  mapM_ (optionChecks gpu) [1, 2]
  mapM_ screenshotChecks [False, True]
  transparencyChecks gpu
  reducerEnvironmentCheck
  openingFramesCheck
  openingCloseCheck
  resizeFailureCheck
  uiScaleNaNCheck
  dialogStateChecks
  putStrLn ("SDL window options, screenshots and transparency (" ++ drivers ++ "): ok")

-- | Small hidden window, bundled font, scale 1 (one layout unit per pixel).
options :: SdlOptions
options =
  defaultSdlOptions
    { sdlWindowSettings = defaultWindowSettings {wsMode = Hidden, wsSize = Size 200 100}
    , sdlAppFont = DefaultFont, sdlAppMonoFont = DefaultFont, sdlAppVsync = False, sdlAppUiScale = 1
    }

withWindow :: (WindowSettings -> WindowSettings) -> SdlOptions
withWindow f = options {sdlWindowSettings = f (sdlWindowSettings options)}

-- | A session with page and window colour @backdrop@.
session :: SdlOptions -> Color -> (Context -> SdlEnv -> IO a) -> IO a
session opts backdrop act = do
  ctx <- newPixelContext >>= (`withTheme` pageColor backdrop (windowColor backdrop defaultTheme))
  withSdl opts ctx act

-- | Draw one frame; @full@ forces a full repaint. Returns the view's result.
frame :: Context -> SdlEnv -> Bool -> NanoUI a -> IO a
frame ctx env full ui = do
  (ctx', inp) <- syncDisplay ctx env emptyInput {inputWindowSize = Size 200 100}
  out <- newIORef Nothing
  void (sdlDrawFrame ctx' (ui >>= liftIO . writeIORef out . Just) env inp full)
  maybe (fail "the view did not run") pure =<< readIORef out

-- | Draw a frame that requests a screenshot; fail unless exactly one arrives.
shoot :: String -> Context -> SdlEnv -> Bool -> NanoUI () -> IO Screenshot
shoot name ctx env full ui = do
  shots <- newIORef []
  frame ctx env full (requestScreenshot (\s -> modifyIORef' shots (s :)) >> ui)
  readIORef shots >>= \case
    [Just img] -> pure img
    [Nothing] -> fail (name ++ ": the screenshot could not be read back")
    other -> fail (name ++ ": one screenshot wanted, got " ++ show (length other))


-- A reducer view reaches its own session without being handed it, on every
-- pass including the follow-up frame after reducing a message. Once the
-- session closes, no view can reach it, and a dialog asked for outside one
-- fails instead of touching a window.
reducerEnvironmentCheck :: IO ()
reducerEnvironmentCheck = do
  observed <- newIORef []
  completed <- timeout 10000000 $
    runSdlAppReduce options (+) (0 :: Int) $ \model -> do
      live <- askSdlEnv
      title <- liftIO (traverse windowTitle live)
      liftIO (modifyIORef' observed ((model, title) :))
      if model == 0 then emit 1 else quitUi
  require "reducer terminates" (completed == Just ())
  seen <- readIORef observed
  require "reducer sees both models and its session's native title"
    (any ((== 0) . fst) seen && any ((== 1) . fst) seen && all ((== Just "nano-ui") . snd) seen)
  ctx <- newPixelContext
  (closed, dialog) <- runNanoUI ctx emptyInput $
    (,) <$> askSdlEnv <*> (pollFileDialogUi =<< askOpenFileDialog defaultFileDialogOptions)
  require "a closed session is out of reach" (isNothing closed)
  requireEq "a dialog outside a session fails" FileDialogFailed dialog

windowTitle :: SdlEnv -> IO String
windowTitle env = getWindowTitle (sdlWindow env) >>= peekCString . castPtr . PtrConst.unsafeToPtr

-- | Put an event on SDL's queue, as the desktop would.
push :: Storable e => e -> IO ()
push ev = alloca $ \p -> do
  poke (castPtr p) ev
  pushEventSafe p >>= require "SDL takes the event"

-- | A mouse button going down at (30, 40).
pushPress :: Integral b => b -> IO ()
pushPress b =
  push (SDL_MouseButtonEvent Events.SDL_EVENT_MOUSE_BUTTON_DOWN 0 0 0 1 (fromIntegral b) (CBool 1) 1 0 30 40)

-- | Count the view's passes: the first is 1.
countPass :: IORef Int -> NanoUI Int
countPass ref = liftIO (atomicModifyIORef' ref (\n -> (n + 1, n + 1)))

-- | The opening frames are drawn before the loop starts. A press taken while
-- the first draws is seen by one frame, not replayed by the next; one taken
-- while the second draws is seen too, not dropped. After a render device
-- reset, which empties every texture, the next frame repaints in full with
-- the textures made again, text included. A wake a second after each pass
-- draws a frame should the loop wait for input that never comes.
openingFramesCheck :: IO ()
openingFramesCheck = do
  passes <- newIORef 0
  seen <- newIORef []
  shots <- newIORef []
  redrawn <- newIORef False
  runSdlApp options $ do
    wakeAfter 1
    n <- countPass passes
    inp <- askInput
    label "Text after a reset"
    let shot = requestScreenshot (\s -> modifyIORef' shots (s :))
    liftIO $ modifyIORef' seen ([b | b <- [MouseMiddle, MouseBack], pressedIn b inp] ++)
    when (n == 1) $ liftIO (pushPress sDL_BUTTON_MIDDLE)
    when (n == 2) $ do
      shot
      liftIO (pushPress sDL_BUTTON_X1 >> push (SDL_RenderEvent Events.SDL_EVENT_RENDER_DEVICE_RESET 0 0 0))
    when (n == 3) $ shot >> liftIO (writeIORef redrawn (inputWindowRedraw inp)) >> quitUi
  readIORef seen >>= requireEq "presses during the opening frames, each seen once" [MouseBack, MouseMiddle]
  readIORef redrawn >>= require "a device reset repaints the window"
  readIORef shots >>= \case
    [Just after, Just before] -> do
      require "text before the reset" (length (nub (pixelsOf before)) > 1)
      require "text drawn from the remade textures" (after == before)
    other -> fail ("two screenshots wanted, got " ++ show (length other))

-- | A close request taken during the opening frames closes the window, as
-- one in the loop does.
openingCloseCheck :: IO ()
openingCloseCheck = do
  firstAt <- newIORef Nothing
  lost <- newIORef False
  runSdlApp options $ do
    wakeAfter 1
    now <- liftIO getMonotonicTime
    liftIO (readIORef firstAt) >>= \case
      Nothing -> liftIO $ do
        writeIORef firstAt (Just now)
        push (SDL_QuitEvent Events.SDL_EVENT_QUIT 0 0)
      Just t0 -> when (now > t0 + 0.5) $ liftIO (writeIORef lost True) >> quitUi
  readIORef lost >>= require "a close request during the opening frames closes the window" . not

-- | A frame the resize watch draws (a live resize) that throws ends the
-- session with the exception, as a frame of the loop's own does, rather
-- than the process. The watch runs where SDL sends the event: here a thread
-- of the test's, which only the software renderer may draw from.
resizeFailureCheck :: IO ()
resizeFailureCheck = do
  software <- (== Just "software") <$> lookupEnv "SDL_RENDER_DRIVER"
  when software $ do
    passes <- newIORef 0
    outcome <- try $ runSdlApp options $ do
      wakeAfter 2
      n <- countPass passes
      width <- windowWidth
      when (width < 150) $ liftIO (ioError (userError "too narrow"))
      -- Once the loop waits, double the UI scale behind the session's back
      -- and send the event a resize would.
      live <- askSdlEnv
      when (n == 1) $ liftIO $ for_ live $ \env -> void $ forkIO $ do
        threadDelay 50000
        writeIORef (sdlUiScaleRef env) 2
        push (SDL_WindowEvent Events.SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED 0 0 0 200 100)
      -- The wake's frame: nothing drew the resize.
      when (n >= 3) quitUi
    require "the resize frame's exception leaves the session" $
      either (\(e :: IOException) -> "too narrow" `isInfixOf` show e) (const False) outcome

-- | A UI scale that is not a number follows the display, as zero does, and
-- asking for it again asks for no frame.
uiScaleNaNCheck :: IO ()
uiScaleNaNCheck =
  session options (colorRGBA 0 0 0 255) $ \ctx env -> do
    replicateM_ 3 (frame ctx env True (setSdlUiScale (0 / 0)))
    isDirty ctx >>= require "asking again for a UI scale that is not a number asks for no frame" . not

-- | A file dialog's result: pending until it completes, delivered once,
-- and never revived by a late or abandoned callback.
dialogStateChecks :: IO ()
dialogStateChecks = do
  ref <- newIORef Dialog.FileDialogPending
  let complete = Dialog.completeResult ref
      chosen = Dialog.FileDialogSelected ["chosen.txt"]
  mapM_
    (\(name, act, want) -> act >> Dialog.takeResult ref >>= requireEq name want)
    [ ("a pending poll is repeatable", pure (), Dialog.FileDialogPending)
    , ("a second pending poll", pure (), Dialog.FileDialogPending)
    , ("completion delivered once", complete chosen, chosen)
    , ("completed handle consumed", pure (), Dialog.FileDialogUnknown)
    , ("a late callback cannot revive it", complete Dialog.FileDialogCancelled, Dialog.FileDialogUnknown)
    , ( "an abandoned callback is discarded"
      , writeIORef ref Dialog.FileDialogUnknown >> complete (Dialog.FileDialogSelected ["abandoned.txt"])
      , Dialog.FileDialogUnknown
      )
    ]


-- | Size limits (in layout units, so doubled in window coordinates at zoom
-- 2) at creation and from a view, title, scale, resize and mode. With @gpu@,
-- also opacity, icon and position.
optionChecks :: Bool -> Int -> IO ()
optionChecks gpu zoom =
  session
    (withWindow limits) {sdlAppUiScale = fromIntegral zoom}
    (colorRGBA 0 0 0 255)
    $ \ctx env -> do
      let after :: (Eq a, Show a) => String -> IO () -> IO a -> a -> IO ()
          after name act get want = act >> get >>= requireEq (name ++ " at zoom " ++ show zoom) want
          ui v = frame ctx env True (v >> label "window")
          pair get = alloca $ \pw -> alloca $ \ph -> do
            get (sdlWindow env) pw ph >>= require "SDL did not report a size of the window"
            (,) <$> (fromIntegral <$> peek pw) <*> (fromIntegral <$> peek ph)
          zoomed (w, h) = (zoom * w, zoom * h)
          minimal name act = after name act (pair getWindowMinimumSize) . zoomed
          maximal name act = after name act (pair getWindowMaximumSize) . zoomed
          flags = getWindowFlags (sdlWindow env)
          hidden = (\f -> f .&. SDL.SDL_WINDOW_HIDDEN /= zeroBits) <$> flags
          title = windowTitle env
          askedWindow = frame ctx env True askWindow
      minimal "minimum size at creation" (pure ()) (150, 80)
      maximal "maximum size at creation" (pure ()) (900, 700)
      minimal "minimum size from a view" (ui (setWindowMinSizeUi (Just (Size 170 90)))) (170, 90)
      minimal "minimum size taken off from a view" (ui (setWindowMinSizeUi Nothing)) (0, 0)
      maximal "maximum width alone from a view" (ui (setWindowMaxSizeUi (Just (Size 600 0)))) (600, 0)
      after "the title from a view" (ui (setWindowTitleUi "renamed")) title "renamed"
      after "the scale a view reads" (pure ()) (winScale <$> askedWindow) (fromIntegral zoom)
      -- X11 resizes asynchronously; sync before reading the size.
      let size = SDL.syncWindowSafe (sdlWindow env) >> pair getWindowSize
      after "a resize from a view" (ui (resizeWindowUi (Size 300 150))) size (zoomed (300, 150))
      after "a hidden window" (pure ()) hidden True
      after "shown from a view" (ui (setWindowModeUi Windowed)) hidden False
      after "hidden from a view" (ui (setWindowModeUi Hidden)) hidden True
      when gpu $ do
        let opacity = round . (* 100) <$> getWindowOpacity (sdlWindow env) :: IO Int
            moved name act at = after name act (winPosition <$> askedWindow) (Just at)
        after "the opacity at creation" (pure ()) opacity 50
        after "the opacity from a view" (ui (setWindowOpacityUi 0.75)) opacity 75
        ui (setWindowIconUi (fromJust (rgbaPixels 2 2 (BS.pack (concat (replicate 4 [200, 30, 60, 255]))))))
        moved "the position at creation" (pure ()) (30, 20)
        moved "the position from a view" (ui (moveWindowUi 60 70)) (60, 70)
  where
    limits w =
      w
        { wsMinSize = Just (Size 150 80), wsMaxSize = Just (Size 900 700), wsOpacity = 0.5
        , wsPosition = WindowPositionAt 30 20
        }

red :: Color
red = colorRGBA 200 30 60 255

-- | RGBA bytes of one screenshot pixel.
pixelAt :: Screenshot -> Int -> Int -> [Int]
pixelAt shot x y =
  [fromIntegral (BS.index (rgbaBytes img) ((y * rgbaWidth img + x) * 4 + c)) | c <- [0 .. 3]]
  where
    img = screenshotPixels shot

-- | Every pixel's RGBA bytes, row by row.
pixelsOf :: Screenshot -> [[Int]]
pixelsOf shot = [pixelAt shot x y | y <- [0 .. rgbaHeight img - 1], x <- [0 .. rgbaWidth img - 1]]
  where
    img = screenshotPixels shot

-- | Alphas of all pixels whose RGB matches the colour.
alphasOf :: Color -> Screenshot -> [Int]
alphasOf c shot = [p !! 3 | p <- pixelsOf shot, take 3 p == map fromIntegral [colorR c, colorG c, colorB c]]

-- | A 40x20 box, versioned by its colour so a colour change repaints it.
solidBox :: Color -> NanoUI ()
solidBox c =
  void (drawingVersioned (fromIntegral (colorToWord32 c)) (fixedWH 40 20) (\r -> smallArrayFromList [FillRect r c]))

-- | A screenshot is answered after its frame is drawn (a skipped frame
-- returns what is on screen), and answering requests another frame. In a
-- continuous session the frame is read before it is presented.
screenshotChecks :: Bool -> IO ()
screenshotChecks continuous =
  session options {sdlAppContinuous = continuous} (colorRGBA 12 34 56 255) $ \ctx env -> do
    let name s = (if continuous then "continuous: " else "retained: ") ++ s
    frame ctx env True (solidBox (colorRGBA 0 0 255 255))
    img <- shoot (name "a frame that repaints the box") ctx env False (solidBox red)
    let size = (rgbaWidth (screenshotPixels img), rgbaHeight (screenshotPixels img), screenshotScale img)
    requireEq (name "screenshot size") (200, 100, 1) size
    requireEq (name "the box's pixels") (replicate 800 255) (alphasOf red img)
    requireEq (name "the window's colour") [12, 34, 56, 255] (pixelAt img 199 99)
    isDirty ctx >>= require (name "an answered screenshot asks for another frame")
    unless continuous $ do
      captureScreenshot env >>= require (name "captureScreenshot reads the same frame") . (== Just img)
      skipped <- shoot (name "a skipped frame") ctx env False (solidBox red)
      require (name "a skipped frame's screenshot") (skipped == img)

-- | A transparent window keeps the window colour's alpha, a partial repaint
-- matches a full one, and a GPU renderer keeps the alpha where the window
-- colour is painted over itself.
transparencyChecks :: Bool -> IO ()
transparencyChecks gpu =
  session (withWindow (\w -> w {wsTransparent = True})) backdrop $ \ctx env -> do
    -- The software renderer lacks custom blend modes and draws as if opaque.
    let exact = maybe False ((/= Blend.SDL_BLENDMODE_BLEND) . fst) (sdlTransparent env)
        -- An opaque box, a translucent tinted box, and the window colour
        -- painted twice, as a page scroller paints over the backdrop.
        shot full tint = shoot "transparent" ctx env full . column $ do
          solidBox red
          solidBox tint
          when exact $ void $ drawing (fixedWH 60 20) $ \r -> smallArrayFromList (replicate 2 (FillRect r backdrop))
        keepsAlpha img = all (== 128) (alphasOf backdrop img)
    require "a transparent window has blend modes" (sdlTransparent env /= Nothing)
    when gpu $ require "a GPU renderer draws a transparent window with custom blend modes" exact
    first <- shot True (colorRGBA 0 200 0 100)
    requireEq "transparent: the window's colour" [10, 20, 30, 128] (pixelAt first 199 99)
    require "transparent: the backdrop keeps its alpha" (keepsAlpha first)
    requireEq "transparent: an opaque box stays opaque" (replicate 800 255) (alphasOf red first)
    -- Repainting only the translucent box must not blend over the old box
    -- or old backdrop.
    second <- shot False (colorRGBA 0 0 200 100)
    require "transparent: the change was drawn" (second /= first)
    full <- shot True (colorRGBA 0 0 200 100)
    require "transparent: a change comes out as a full repaint does" (full == second)
    require "transparent: the backdrop keeps its alpha after a change" (keepsAlpha second)
  where
    backdrop = colorRGBA 10 20 30 128
