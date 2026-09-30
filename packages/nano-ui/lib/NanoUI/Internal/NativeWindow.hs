-- | The native window a view runs in: its opening settings
-- ('WindowSettings'), its state each frame ('askWindow'), the requests a view
-- makes of it (title, icon, size limits, mode, position, screenshots,
-- quitting), and the RGBA pixels used for icons and screenshots.
--
-- A backend opens the window from a 'WindowSettings', installs a
-- 'WindowHost' that carries out requests, and reports the window's state
-- once a frame. Without a host, as under a test context, requests do
-- nothing, 'requestScreenshot' answers 'Nothing', and 'askWindow' describes
-- a focused window the size of the frame's input.
module NanoUI.Internal.NativeWindow
  ( -- * Pixels
    RgbaPixels
  , rgbaPixels
  , rgbaWidth
  , rgbaHeight
  , rgbaBytes
  , Screenshot (..)

    -- * Settings
  , WindowSettings (..)
  , defaultWindowSettings
  , sizeLimitAt
  , WindowPosition (..)
  , WindowMode (..)

    -- * State
  , WindowState (..)
  , defaultWindowState
  , askWindow
  , WindowCapabilities (..)
  , noWindowCapabilities
  , allWindowCapabilities
  , askWindowCapabilities

    -- * From a view
  , setWindowTitleUi
  , setWindowIconUi
  , setWindowMinSizeUi
  , setWindowMaxSizeUi
  , setWindowOpacityUi
  , setWindowModeUi
  , moveWindowUi
  , centerWindowUi
  , resizeWindowUi
  , minimizeWindowUi
  , maximizeWindowUi
  , restoreWindowUi
  , toggleMaximizedUi
  , quitUi
  , requestScreenshot
  , askScreenshot
  , useScreenshot

    -- * Backends
  , WindowHost (..)
  , defaultWindowHost
  , installWindowHost
  , closeWindowHost
  , reportWindowState
  , answerScreenshots
  , answerScreenshotsAfter
  , requestWindowClose
  , clearWindowClose
  , quitRequested
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (SomeException, finally, mask, throwIO, try)
import Control.Monad (join, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import NanoUI.Internal.Context (Context (ctxNativeWindow), markDirty, markDirtyCovered, wakeFromThread)
import NanoUI.Internal.Host (askHostIO, clearHost, setHost)
import NanoUI.Internal.NativeWindow.Types
import NanoUI.Internal.Monad (NanoUI, windowSize, withContext)
import NanoUI.Internal.Tasks (Task, useTask)
import NanoUI.Internal.Types (Size (..))

-- | Pixels @width@ by @height@, or 'Nothing' when either is not positive or
-- the byte count is not @4 * width * height@. To draw them, register them
-- with @registerImageRgba iid (rgbaWidth p) (rgbaHeight p) (rgbaBytes p)@.
rgbaPixels :: Int -> Int -> ByteString -> Maybe RgbaPixels
rgbaPixels w h bytes
  | w > 0 && h > 0 && toInteger (BS.length bytes) == toInteger w * toInteger h * 4 = Just (RgbaPixels w h bytes)
  | otherwise = Nothing

-- | A resizable, opaque 1280x800 window titled @"nano-ui"@, placed by the
-- desktop, with no size limits and the desktop's icon. Closing it ends the
-- session.
defaultWindowSettings :: WindowSettings
defaultWindowSettings =
  WindowSettings
    { wsTitle = "nano-ui"
    , wsSize = Size 1280 800
    , wsPosition = WindowPositionDefault
    , wsMinSize = Nothing
    , wsMaxSize = Nothing
    , wsIcon = Nothing
    , wsResizable = True
    , wsMode = Windowed
    , wsTransparent = False
    , wsOpacity = 1
    , wsExitOnCloseRequest = True
    }

-- | A size limit in layout units ('wsMinSize', 'wsMaxSize') in whole native
-- units at @scale@ per layout unit, plus @outset@ on each axis, such as a
-- desktop frame the native limit includes. A missing limit, or an axis of
-- zero or less, is 0 on that axis: no limit.
sizeLimitAt :: Float -> (Int, Int) -> Maybe Size -> (Int, Int)
sizeLimitAt scale (outW, outH) limit = (axis w outW, axis h outH)
  where
    Size w h = fromMaybe (Size 0 0) limit
    axis v outset = if v <= 0 then 0 else round (v * scale) + outset

-- | The state without a window, as under a test context: focused, scale 1,
-- unknown position, no special mode. 'askWindow' fills in the frame's size.
defaultWindowState :: WindowState
defaultWindowState =
  WindowState
    { winSize = Size 0 0
    , winScale = 1
    , winPosition = Nothing
    , winFocused = True
    , winMaximized = False
    , winMinimized = False
    , winFullscreen = False
    , winCloseRequested = False
    }

-- | The window as the backend reported it at the start of this frame, plus
-- whether it was asked to close. Its 'winSize' is 'NanoUI.windowSize'. Once
-- a view reads it, a change (in focus, say) triggers a frame; a view that
-- never reads it pays nothing.
askWindow :: NanoUI WindowState
askWindow = do
  size <- windowSize
  withContext $ \ctx ->
    askHostIO (ctxNativeWindow ctx) >>= \case
      Nothing -> pure defaultWindowState {winSize = size}
      Just nw -> do
        writeIORef (nwStateRead nw) True
        (\st -> st {winSize = size}) <$> readIORef (nwState nw)

-- | What a host supports, for a view to check before offering a control
-- ('WindowCapabilities'). Nothing without a window, as under a test context.
askWindowCapabilities :: NanoUI WindowCapabilities
askWindowCapabilities =
  withContext $ \ctx ->
    maybe noWindowCapabilities (hostCapabilities . nwHost) <$> askHostIO (ctxNativeWindow ctx)

-- | Supports no request.
noWindowCapabilities :: WindowCapabilities
noWindowCapabilities = WindowCapabilities False False False False False False False False False

-- | Supports every request.
allWindowCapabilities :: WindowCapabilities
allWindowCapabilities = WindowCapabilities True True True True True True True True True

-- | A host that does nothing and says so ('noWindowCapabilities'). A backend
-- updates it with the operations it has and sets 'hostCapabilities' to match.
defaultWindowHost :: WindowHost
defaultWindowHost =
  WindowHost
    { hostCapabilities = noWindowCapabilities
    , hostSetTitle = \_ -> pure ()
    , hostSetIcon = \_ -> pure ()
    , hostSetMinSize = \_ -> pure ()
    , hostSetMaxSize = \_ -> pure ()
    , hostSetOpacity = \_ -> pure ()
    , hostSetMode = \_ -> pure ()
    , hostMove = \_ _ -> pure ()
    , hostCenter = pure ()
    , hostResize = \_ -> pure ()
    , hostMinimize = pure ()
    , hostMaximize = pure ()
    , hostRestore = pure ()
    }

-- | Install the host for the context's window. The backend has already
-- opened the window from @settings@ with its title, size, mode,
-- resizability and transparency; this applies the size limits, icon,
-- opacity and position through the host, in that order. Call it once before
-- the first frame. A later call replaces the host and answers the old one's
-- pending screenshots with 'Nothing'.
installWindowHost :: Context -> WindowSettings -> WindowHost -> IO ()
installWindowHost ctx settings host = do
  closeWindowHost ctx
  nw <-
    NativeWindow host
      <$> newIORef settings {wsMinSize = Nothing, wsMaxSize = Nothing, wsIcon = Nothing, wsOpacity = 1}
      <*> newIORef defaultWindowState
      <*> newIORef False
      <*> newIORef False
      <*> newIORef (Just [])
  setHost (ctxNativeWindow ctx) nw
  setMinSize nw (wsMinSize settings)
  setMaxSize nw (wsMaxSize settings)
  traverse_ (setIcon nw) (wsIcon settings)
  setOpacity nw (wsOpacity settings)
  case wsPosition settings of
    WindowPositionDefault -> pure ()
    WindowPositionCentered -> hostCenter host
    WindowPositionAt x y -> hostMove host x y

-- | End native-window access before releasing the platform window. Pending
-- screenshots receive Nothing, including actions obtained before closing but
-- invoked afterwards. Idempotent; call even if the opening frame failed.
-- Every queued callback is attempted before a callback exception propagates.
closeWindowHost :: Context -> IO ()
closeWindowHost ctx = mask $ \_ -> do
  old <- askHostIO (ctxNativeWindow ctx)
  clearHost (ctxNativeWindow ctx)
  traverse_ (\nw -> do
    waiting <- atomicModifyIORef' (nwShots nw) (Nothing,)
    completeScreenshots Nothing (reverse (fromMaybe [] waiting))) old

-- | Report the window's state once a frame, before the view runs. The
-- 'winSize' and 'winCloseRequested' passed in are ignored; they come from
-- the frame input and the session loop. A change triggers a frame if a view
-- reads the state.
reportWindowState :: Context -> WindowState -> IO ()
reportWindowState ctx st =
  withHost ctx $ \nw -> do
    old <- readIORef (nwState nw)
    let new = st {winCloseRequested = winCloseRequested old}
    unless (new == old) $ do
      writeIORef (nwState nw) new
      read' <- readIORef (nwStateRead nw)
      -- The view may paint the state (a colour on focus) where no diff sees
      -- it, so the frame repaints whole.
      when read' (markDirty ctx)

-- | For the session loop when the window is asked to close. Returns 'True'
-- when the session should end ('wsExitOnCloseRequest' set, or no host).
-- Otherwise sets 'winCloseRequested' and requests a frame; call
-- 'clearWindowClose' after that frame runs.
requestWindowClose :: Context -> IO Bool
requestWindowClose ctx =
  askHostIO (ctxNativeWindow ctx) >>= \case
    Nothing -> pure True
    Just nw -> do
      exits <- wsExitOnCloseRequest <$> readIORef (nwSettings nw)
      unless exits $ do
        modifyIORef' (nwState nw) (\s -> s {winCloseRequested = True})
        markDirtyCovered ctx
      pure exits

-- | Clear 'winCloseRequested' after the frame that saw it.
clearWindowClose :: Context -> IO ()
clearWindowClose ctx = withHost ctx $ \nw -> modifyIORef' (nwState nw) (\s -> s {winCloseRequested = False})

-- | Whether a view called 'quitUi'. The session loop checks after each
-- frame.
quitRequested :: Context -> IO Bool
quitRequested ctx = maybe (pure False) (readIORef . nwQuit) =<< askHostIO (ctxNativeWindow ctx)

withHost :: Context -> (NativeWindow -> IO ()) -> IO ()
withHost ctx act = askHostIO (ctxNativeWindow ctx) >>= traverse_ act

withNativeWindow :: (NativeWindow -> IO ()) -> NanoUI ()
withNativeWindow act = withContext (`withHost` act)

-- | Apply a setting through the host only when it differs from the current
-- one.
setting :: Eq a => (WindowSettings -> a) -> (a -> WindowSettings -> WindowSettings) -> (WindowHost -> a -> IO ()) -> NativeWindow -> a -> IO ()
setting get put apply nw v = do
  s <- readIORef (nwSettings nw)
  unless (get s == v) $ do
    apply (nwHost nw) v
    modifyIORef' (nwSettings nw) (put v)

-- | Set one size limit. On an axis where the minimum would pass the maximum,
-- the limit being set wins and the other moves to it, and goes to the host
-- first, so the host never sees a crossed pair: most desktops reject one
-- (SDL keeps the old limit) and a Wayland compositor disconnects the client.
setMinSize, setMaxSize :: NativeWindow -> Maybe Size -> IO ()
setMinSize nw v = do
  other <- wsMaxSize <$> readIORef (nwSettings nw)
  putMaxSize nw (yieldLimit (>) v other)
  putMinSize nw v
setMaxSize nw v = do
  other <- wsMinSize <$> readIORef (nwSettings nw)
  putMinSize nw (yieldLimit (<) v other)
  putMaxSize nw v

putMinSize, putMaxSize :: NativeWindow -> Maybe Size -> IO ()
putMinSize = setting wsMinSize (\v s -> s {wsMinSize = v}) hostSetMinSize
putMaxSize = setting wsMaxSize (\v s -> s {wsMaxSize = v}) hostSetMaxSize

-- | @other@ with each axis that @crosses@ the limit @new@ sets moved to
-- @new@'s. An axis of zero or less, or a missing limit, crosses nothing.
yieldLimit :: (Float -> Float -> Bool) -> Maybe Size -> Maybe Size -> Maybe Size
yieldLimit crosses (Just (Size nw nh)) (Just (Size ow oh)) = Just (Size (axis nw ow) (axis nh oh))
  where
    axis n o = if n > 0 && o > 0 && crosses n o then n else o
yieldLimit _ _ other = other

setIcon :: NativeWindow -> RgbaPixels -> IO ()
setIcon nw = setting wsIcon (\v s -> s {wsIcon = v}) (traverse_ . hostSetIcon) nw . Just

setOpacity :: NativeWindow -> Float -> IO ()
setOpacity nw = setting wsOpacity (\v s -> s {wsOpacity = v}) hostSetOpacity nw . max 0 . min 1

-- | Set the window title ('wsTitle').
--
-- This and the setters below do nothing when the value matches what the
-- window already has, so calling them every frame costs only a comparison.
setWindowTitleUi :: Text -> NanoUI ()
setWindowTitleUi t = withNativeWindow (\nw -> setting wsTitle (\v s -> s {wsTitle = v}) hostSetTitle nw t)

-- | Set the window icon ('wsIcon').
setWindowIconUi :: RgbaPixels -> NanoUI ()
setWindowIconUi icon = withNativeWindow (`setIcon` icon)

-- | Set the smallest size, in layout units, the user can resize the window
-- to, or remove the limit with 'Nothing'. A zero axis is unlimited. The size
-- is converted at the UI scale when the limit is set, and not again if the
-- scale changes later. Where it passes the maximum size, the maximum is
-- raised to it; 'setWindowMaxSizeUi' likewise lowers a minimum above it.
setWindowMinSizeUi :: Maybe Size -> NanoUI ()
setWindowMinSizeUi s = withNativeWindow (`setMinSize` s)

-- | Set the largest size the user can resize the window to, as for
-- 'setWindowMinSizeUi'.
setWindowMaxSizeUi :: Maybe Size -> NanoUI ()
setWindowMaxSizeUi s = withNativeWindow (`setMaxSize` s)

-- | Set the opacity of the whole window, decorations included, from 0
-- (invisible) to 1 (opaque), clamped, where the backend and desktop support
-- it ('wsOpacity'). For a see-through background use 'wsTransparent'.
setWindowOpacityUi :: Float -> NanoUI ()
setWindowOpacityUi o = withNativeWindow (`setOpacity` o)

-- | Make the window windowed, fullscreen or hidden ('wsMode').
--
-- > (fullscreen, toggleFullscreen) <- useToggle False
-- > whenM (shortcut (key (KeyF 11))) toggleFullscreen
-- > setWindowModeUi (if fullscreen then Fullscreen else Windowed)
setWindowModeUi :: WindowMode -> NanoUI ()
setWindowModeUi m = withNativeWindow (\nw -> setting wsMode (\v s -> s {wsMode = v}) hostSetMode nw m)

-- | Move the window's top-left corner to a desktop point: window coordinates
-- on SDL, pixels on RGFW.
--
-- This and the commands below act on every call, because the user can also
-- move, resize, maximize and restore the window. Call them from an event,
-- not every frame. Some desktops ignore placement (Wayland does), and a
-- maximized or fullscreen window stays put.
moveWindowUi :: Int -> Int -> NanoUI ()
moveWindowUi x y = withNativeWindow (\nw -> hostMove (nwHost nw) x y)

-- | Centre the window on its display.
centerWindowUi :: NanoUI ()
centerWindowUi = withNativeWindow (hostCenter . nwHost)

-- | Resize the view to a size in layout units, excluding the desktop's
-- decorations.
resizeWindowUi :: Size -> NanoUI ()
resizeWindowUi s = withNativeWindow (\nw -> hostResize (nwHost nw) s)

-- | Minimize the window to the taskbar.
minimizeWindowUi :: NanoUI ()
minimizeWindowUi = withNativeWindow (hostMinimize . nwHost)

-- | Maximize the window, leaving the desktop's panels visible.
maximizeWindowUi :: NanoUI ()
maximizeWindowUi = withNativeWindow (hostMaximize . nwHost)

-- | Return a maximized or minimized window to its previous size.
restoreWindowUi :: NanoUI ()
restoreWindowUi = withNativeWindow (hostRestore . nwHost)

-- | Maximize the window, or restore it if it is maximized. Reads
-- 'winMaximized', since the desktop can maximize the window itself (on a
-- title bar double-click, say).
toggleMaximizedUi :: NanoUI ()
toggleMaximizedUi = withNativeWindow $ \nw -> do
  maxed <- winMaximized <$> readIORef (nwState nw)
  (if maxed then hostRestore else hostMaximize) (nwHost nw)

-- | End the session after this frame is drawn; the backend's runner closes
-- the window and returns. With 'wsExitOnCloseRequest' off, this is how the
-- window closes.
quitUi :: NanoUI ()
quitUi = withNativeWindow (\nw -> writeIORef (nwQuit nw) True)

-- | Request a screenshot of the frame this view is building. The action runs
-- on the UI thread after that frame is presented and before the next one
-- starts. It gets 'Nothing' when the backend cannot capture, immediately if
-- there is no window.
--
-- Each call is answered once. Request from an event such as a click, not
-- every frame, or the view never stops redrawing and capturing. Keep the
-- action short (an 'IORef' write, a 'Control.Concurrent.forkIO' of the
-- encoding), since the next frame waits on it. A frame follows each answer,
-- so the view can show the result. 'useScreenshot' returns the screenshot to
-- the view directly.
requestScreenshot :: (Maybe Screenshot -> IO ()) -> NanoUI ()
requestScreenshot answer = withContext $ \ctx -> askHostIO (ctxNativeWindow ctx) >>= maybe (answer Nothing) (`queueScreenshot` answer)

-- | Queue an answer for 'answerScreenshots'.
queueScreenshot :: NativeWindow -> (Maybe Screenshot -> IO ()) -> IO ()
queueScreenshot nw answer = do
  accepted <- atomicModifyIORef' (nwShots nw) $ \case
    Nothing -> (Nothing, False)
    Just waiting -> (Just (answer : waiting), True)
  unless accepted (answer Nothing)

-- | An action for another thread, such as a 'NanoUI.useTaskStatus' job. It
-- wakes the loop, waits for the next frame to be presented, and returns a
-- screenshot of it, as 'requestScreenshot' does. Without a window it returns
-- 'Nothing' at once, also after that window is closed or replaced. On the UI
-- thread it deadlocks while the window is open.
--
-- > shoot <- askScreenshot
-- > saved <- useTaskStatus task shots (shoot >>= traverse_ (savePng "shot.png"))
askScreenshot :: NanoUI (IO (Maybe Screenshot))
askScreenshot = withContext $ \ctx -> maybe (pure Nothing) (shoot ctx) <$> askHostIO (ctxNativeWindow ctx)
  where
    shoot ctx nw = do
      box <- newEmptyMVar
      queueScreenshot nw (putMVar box)
      wakeFromThread ctx
      takeMVar box

-- | A screenshot per key. The first frame with a new key requests one, which
-- arrives a frame or two later; until then it returns the previous key's
-- screenshot, as 'useTask' does. Allocate @task <- newTask@ during setup;
-- change the key to take another screenshot. This consumes no widget id.
--
-- > (shots, setShots) <- useInt 0
-- > whenM (button "Screenshot") (setShots (shots + 1))
-- > shot <- if shots == 0 then pure Nothing else useScreenshot task shots
useScreenshot :: Eq k => Task k (Maybe Screenshot) -> k -> NanoUI (Maybe Screenshot)
useScreenshot owner k = join <$> (useTask owner k =<< askScreenshot)

-- | Answer every screenshot request since the last call, in request order,
-- with one run of @capture@, which does not run when none are pending. A
-- backend calls this once a frame is presented, and not while it has no
-- frame to capture. The scale is the last one given to 'reportWindowState'.
--
-- Answers are view code that usually changes what a view reads, outside
-- anything the damage pass diffs, so answering any requests a frame that
-- repaints whole.
answerScreenshots :: Context -> IO (Maybe RgbaPixels) -> IO ()
answerScreenshots ctx capture = answerScreenshotsAfter ctx capture (pure ())

-- | Capture only if requests are pending, run the presentation action, then
-- deliver the answers. For a backbuffer that must be read before presentation.
-- Capture or presentation failure answers the batch with Nothing before
-- propagating the exception. A throwing callback cannot strand later answers.
answerScreenshotsAfter :: Context -> IO (Maybe RgbaPixels) -> IO () -> IO ()
answerScreenshotsAfter ctx capture present = mask $ \restore -> do
  host <- askHostIO (ctxNativeWindow ctx)
  waiting <- maybe (pure [])
    (\nw -> atomicModifyIORef' (nwShots nw) (\pending -> (fmap (const []) pending, reverse (fromMaybe [] pending)))) host
  if null waiting then restore present else do
    outcome <- try @SomeException $ restore $ do
      scale <- maybe (pure 1) (fmap winScale . readIORef . nwState) host
      shot <- fmap (`Screenshot` scale) <$> capture
      present
      pure shot
    delivered <- try @SomeException $
      completeScreenshots (either (const Nothing) id outcome) waiting
        `finally` markDirty ctx
    either throwIO (const (either throwIO pure delivered)) outcome

completeScreenshots :: Maybe Screenshot -> [Maybe Screenshot -> IO ()] -> IO ()
completeScreenshots shot answers = mask $ \restore -> do
  results <- mapM (try @SomeException . restore . ($ shot)) answers
  case [e | Left e <- results] of
    e : _ -> throwIO e
    [] -> pure ()
