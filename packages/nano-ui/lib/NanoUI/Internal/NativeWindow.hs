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
  , WindowPosition (..)
  , WindowMode (..)

    -- * State
  , WindowState (..)
  , defaultWindowState
  , askWindow

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
  , reportWindowState
  , answerScreenshots
  , requestWindowClose
  , clearWindowClose
  , quitRequested
  ) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Monad (join, unless, when)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context, askHostIO, markDirtyCovered, setHost, wakeFromThread)
import NanoUI.Internal.Monad (Ui, windowSize, withContext)
import NanoUI.Internal.Tasks (useTask)
import NanoUI.Internal.Types (Size (..))

-- | RGBA8 pixels: rows from the top, tightly packed, four bytes a pixel,
-- straight alpha. Used for window icons and screenshots. Build them with
-- 'rgbaPixels', which checks the byte count against the size.
data RgbaPixels = RgbaPixels
  { rgbaWidth :: !Int
  , rgbaHeight :: !Int
  , rgbaBytes :: !ByteString
  -- ^ Exactly @4 * width * height@ bytes.
  }
  deriving (Eq)

-- | Shows the size only.
instance Show RgbaPixels where
  showsPrec d (RgbaPixels w h _) =
    showParen (d > 10) (showString "RgbaPixels " . shows w . showString "x" . shows h)

-- | Pixels @width@ by @height@, or 'Nothing' when either is not positive or
-- the byte count is not @4 * width * height@. To draw them, register them
-- with @registerImageRgba iid (rgbaWidth p) (rgbaHeight p) (rgbaBytes p)@.
rgbaPixels :: Int -> Int -> ByteString -> Maybe RgbaPixels
rgbaPixels w h bytes
  | w > 0 && h > 0 && BS.length bytes == w * h * 4 = Just (RgbaPixels w h bytes)
  | otherwise = Nothing

-- | A frame as presented, at the window's pixel size. Alpha is as drawn, so
-- where nothing covers the background it is the alpha of the theme's window
-- colour. 'screenshotScale' is pixels per layout unit: the pixel size is
-- @screenshotScale@ times 'NanoUI.windowSize'.
data Screenshot = Screenshot
  { screenshotPixels :: !RgbaPixels
  , screenshotScale :: !Float
  }
  deriving (Eq, Show)

-- | How a window opens, for any backend (the SDL backend's
-- @sdlWindowSettings@, the RGFW backend's @optWindow@). Sizes are in layout
-- units, as for 'NanoUI.windowSize'; the backend converts them at the scale
-- the window opens at.
data WindowSettings = WindowSettings
  { wsTitle :: !Text
  -- ^ Shown in the title bar, taskbar and window switcher (default:
  -- @"nano-ui"@). See 'setWindowTitleUi'.
  , wsSize :: !Size
  -- ^ The view's size (default: 1280x800). See 'resizeWindowUi'.
  , wsPosition :: !WindowPosition
  -- ^ Where the window opens (default: 'WindowPositionDefault').
  , wsMinSize :: !(Maybe Size)
  -- ^ The smallest size the user can resize to (default: no limit). A zero
  -- axis is unlimited. See 'setWindowMinSizeUi'.
  , wsMaxSize :: !(Maybe Size)
  -- ^ The largest size the user can resize to (default: no limit).
  , wsIcon :: !(Maybe RgbaPixels)
  -- ^ The window icon (default: the desktop's). 32 or 64 pixels square suits
  -- most desktops.
  , wsResizable :: !Bool
  -- ^ Whether the user may resize the window (default: 'True').
  , wsMode :: !WindowMode
  -- ^ Windowed, fullscreen or hidden (default: 'Windowed').
  -- See 'setWindowModeUi'.
  , wsTransparent :: !Bool
  -- ^ Let the desktop show through where the theme's window colour
  -- ('NanoUI.windowColor') is translucent (default: 'False'). Works on the
  -- SDL backend with a compositor; RGFW windows are always opaque.
  , wsOpacity :: !Float
  -- ^ Opacity of the whole window, decorations included, from 0 to 1
  -- (default: 1). The SDL backend supports it; RGFW does not.
  -- See 'setWindowOpacityUi'.
  , wsExitOnCloseRequest :: !Bool
  -- ^ End the session when the window is asked to close, by its close
  -- button, the window manager or a platform quit request (default:
  -- 'True'). With 'False' the view sees 'winCloseRequested' instead and
  -- calls 'quitUi' when ready, for example after asking about unsaved work.
  }
  deriving (Eq, Show)

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

-- | Where a window opens ('wsPosition').
data WindowPosition
  = WindowPositionDefault
  -- ^ Where the desktop puts new windows. RGFW cannot ask the desktop, so it
  -- centres the window.
  | WindowPositionCentered
  -- ^ Centred on the display.
  | WindowPositionAt !Int !Int
  -- ^ The top-left corner, in desktop coordinates (see 'moveWindowUi').
  deriving (Eq, Show)

-- | Windowed, filling the display, or not shown.
data WindowMode = Windowed | Fullscreen | Hidden
  deriving (Eq, Show, Enum, Bounded)

-- | The window as of this frame ('askWindow').
data WindowState = WindowState
  { winSize :: !Size
  -- ^ The view's size in layout units ('NanoUI.windowSize').
  , winScale :: !Float
  -- ^ Pixels per layout unit: pixel density times UI scale.
  , winPosition :: !(Maybe (Int, Int))
  -- ^ The top-left corner in desktop coordinates, when the desktop reports
  -- it (Wayland does not).
  , winFocused :: !Bool
  -- ^ Whether the window has keyboard focus.
  , winMaximized :: !Bool
  , winMinimized :: !Bool
  , winFullscreen :: !Bool
  , winCloseRequested :: !Bool
  -- ^ Whether the window was asked to close since the last frame. Only set
  -- when 'wsExitOnCloseRequest' is off; the session continues until the
  -- view calls 'quitUi'. It lasts one frame, like a click, so keep any
  -- follow-up in your own state:
  --
  -- > (confirming, setConfirming) <- useFlag False
  -- > closing <- winCloseRequested <$> askWindow
  -- > when (closing && not unsaved) quitUi
  -- > when (closing && unsaved) (setConfirming True)
  }
  deriving (Eq, Show)

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
askWindow :: Ui :> es => Eff es WindowState
askWindow = do
  size <- windowSize
  withContext $ \ctx ->
    askHostIO ctx >>= \case
      Nothing -> pure defaultWindowState {winSize = size}
      Just nw -> do
        writeIORef (nwStateRead nw) True
        (\st -> st {winSize = size}) <$> readIORef (nwState nw)

-- | How a backend carries out a view's window requests. Each field runs on
-- the UI thread, in the middle of a view. Sizes are in layout units,
-- converted at the window's current scale; a zero axis, or 'Nothing', means
-- no limit.
--
-- Build one by updating 'defaultWindowHost', whose fields do nothing, so a
-- field added later is a no-op for existing backends:
--
-- > installWindowHost ctx settings
-- >   defaultWindowHost {hostSetTitle = setTitle win, hostMove = moveWindow win}
data WindowHost = WindowHost
  { hostSetTitle :: Text -> IO ()
  , hostSetIcon :: RgbaPixels -> IO ()
  , hostSetMinSize :: Maybe Size -> IO ()
  , hostSetMaxSize :: Maybe Size -> IO ()
  , hostSetOpacity :: Float -> IO ()
  -- ^ From 0 (invisible) to 1 (opaque).
  , hostSetMode :: WindowMode -> IO ()
  , hostMove :: Int -> Int -> IO ()
  -- ^ The top-left corner, in desktop coordinates.
  , hostCenter :: IO ()
  , hostResize :: Size -> IO ()
  -- ^ The view's size, excluding the desktop's decorations.
  , hostMinimize :: IO ()
  , hostMaximize :: IO ()
  , hostRestore :: IO ()
  -- ^ Undo a maximize or minimize.
  }

-- | A host that does nothing.
defaultWindowHost :: WindowHost
defaultWindowHost =
  WindowHost
    { hostSetTitle = \_ -> pure ()
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

-- | The installed host and its state: the window's current settings (so a
-- view setting the same value every frame costs a comparison), the state the
-- backend last reported, whether a view has read that state, whether a view
-- called 'quitUi', and pending screenshot requests, newest first.
data NativeWindow = NativeWindow
  { nwHost :: !WindowHost
  , nwSettings :: !(IORef WindowSettings)
  , nwState :: !(IORef WindowState)
  , nwStateRead :: !(IORef Bool)
  , nwQuit :: !(IORef Bool)
  , nwShots :: !(IORef [Maybe Screenshot -> IO ()])
  }

-- | Install the host for the context's window. The backend has already
-- opened the window from @settings@ with its title, size, mode,
-- resizability and transparency; this applies the size limits, icon,
-- opacity and position through the host, in that order. Call it once before
-- the first frame. A later call replaces the host and answers the old one's
-- pending screenshots with 'Nothing'.
installWindowHost :: Context -> WindowSettings -> WindowHost -> IO ()
installWindowHost ctx settings host = do
  answerScreenshots ctx (pure Nothing)
  nw <-
    NativeWindow host
      <$> newIORef settings {wsMinSize = Nothing, wsMaxSize = Nothing, wsIcon = Nothing, wsOpacity = 1}
      <*> newIORef defaultWindowState
      <*> newIORef False
      <*> newIORef False
      <*> newIORef []
  setHost ctx nw
  setMinSize nw (wsMinSize settings)
  setMaxSize nw (wsMaxSize settings)
  traverse_ (setIcon nw) (wsIcon settings)
  setOpacity nw (wsOpacity settings)
  case wsPosition settings of
    WindowPositionDefault -> pure ()
    WindowPositionCentered -> hostCenter host
    WindowPositionAt x y -> hostMove host x y

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
      when read' (markDirtyCovered ctx)

-- | For the session loop when the window is asked to close. Returns 'True'
-- when the session should end ('wsExitOnCloseRequest' set, or no host).
-- Otherwise sets 'winCloseRequested' and requests a frame; call
-- 'clearWindowClose' after that frame runs.
requestWindowClose :: Context -> IO Bool
requestWindowClose ctx =
  askHostIO ctx >>= \case
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
quitRequested ctx = maybe (pure False) (readIORef . nwQuit) =<< askHostIO ctx

withHost :: Context -> (NativeWindow -> IO ()) -> IO ()
withHost ctx act = askHostIO ctx >>= traverse_ act

withNativeWindow :: Ui :> es => (NativeWindow -> IO ()) -> Eff es ()
withNativeWindow act = withContext (`withHost` act)

-- | Apply a setting through the host only when it differs from the current
-- one.
setting :: Eq a => (WindowSettings -> a) -> (a -> WindowSettings -> WindowSettings) -> (WindowHost -> a -> IO ()) -> NativeWindow -> a -> IO ()
setting get put apply nw v = do
  s <- readIORef (nwSettings nw)
  unless (get s == v) $ writeIORef (nwSettings nw) (put v s) >> apply (nwHost nw) v

setMinSize, setMaxSize :: NativeWindow -> Maybe Size -> IO ()
setMinSize = setting wsMinSize (\v s -> s {wsMinSize = v}) hostSetMinSize
setMaxSize = setting wsMaxSize (\v s -> s {wsMaxSize = v}) hostSetMaxSize

setIcon :: NativeWindow -> RgbaPixels -> IO ()
setIcon nw = setting wsIcon (\v s -> s {wsIcon = v}) (traverse_ . hostSetIcon) nw . Just

setOpacity :: NativeWindow -> Float -> IO ()
setOpacity nw = setting wsOpacity (\v s -> s {wsOpacity = v}) hostSetOpacity nw . max 0 . min 1

-- | Set the window title ('wsTitle').
--
-- This and the setters below do nothing when the value matches what the
-- window already has, so calling them every frame costs only a comparison.
setWindowTitleUi :: Ui :> es => Text -> Eff es ()
setWindowTitleUi t = withNativeWindow (\nw -> setting wsTitle (\v s -> s {wsTitle = v}) hostSetTitle nw t)

-- | Set the window icon ('wsIcon').
setWindowIconUi :: Ui :> es => RgbaPixels -> Eff es ()
setWindowIconUi icon = withNativeWindow (`setIcon` icon)

-- | Set the smallest size, in layout units, the user can resize the window
-- to, or remove the limit with 'Nothing'. A zero axis is unlimited. The size
-- is converted at the UI scale when the limit is set, and not again if the
-- scale changes later.
setWindowMinSizeUi :: Ui :> es => Maybe Size -> Eff es ()
setWindowMinSizeUi s = withNativeWindow (`setMinSize` s)

-- | Set the largest size the user can resize the window to, as for
-- 'setWindowMinSizeUi'.
setWindowMaxSizeUi :: Ui :> es => Maybe Size -> Eff es ()
setWindowMaxSizeUi s = withNativeWindow (`setMaxSize` s)

-- | Set the opacity of the whole window, decorations included, from 0
-- (invisible) to 1 (opaque), clamped, where the backend and desktop support
-- it ('wsOpacity'). For a see-through background use 'wsTransparent'.
setWindowOpacityUi :: Ui :> es => Float -> Eff es ()
setWindowOpacityUi o = withNativeWindow (`setOpacity` o)

-- | Make the window windowed, fullscreen or hidden ('wsMode').
--
-- > (fullscreen, toggleFullscreen) <- useToggle False
-- > whenM (shortcut (key (KeyF 11))) toggleFullscreen
-- > setWindowModeUi (if fullscreen then Fullscreen else Windowed)
setWindowModeUi :: Ui :> es => WindowMode -> Eff es ()
setWindowModeUi m = withNativeWindow (\nw -> setting wsMode (\v s -> s {wsMode = v}) hostSetMode nw m)

-- | Move the window's top-left corner to a desktop point: window coordinates
-- on SDL, pixels on RGFW.
--
-- This and the commands below act on every call, because the user can also
-- move, resize, maximize and restore the window. Call them from an event,
-- not every frame. Some desktops ignore placement (Wayland does), and a
-- maximized or fullscreen window stays put.
moveWindowUi :: Ui :> es => Int -> Int -> Eff es ()
moveWindowUi x y = withNativeWindow (\nw -> hostMove (nwHost nw) x y)

-- | Centre the window on its display.
centerWindowUi :: Ui :> es => Eff es ()
centerWindowUi = withNativeWindow (hostCenter . nwHost)

-- | Resize the view to a size in layout units, excluding the desktop's
-- decorations.
resizeWindowUi :: Ui :> es => Size -> Eff es ()
resizeWindowUi s = withNativeWindow (\nw -> hostResize (nwHost nw) s)

-- | Minimize the window to the taskbar.
minimizeWindowUi :: Ui :> es => Eff es ()
minimizeWindowUi = withNativeWindow (hostMinimize . nwHost)

-- | Maximize the window, leaving the desktop's panels visible.
maximizeWindowUi :: Ui :> es => Eff es ()
maximizeWindowUi = withNativeWindow (hostMaximize . nwHost)

-- | Return a maximized or minimized window to its previous size.
restoreWindowUi :: Ui :> es => Eff es ()
restoreWindowUi = withNativeWindow (hostRestore . nwHost)

-- | Maximize the window, or restore it if it is maximized. Reads
-- 'winMaximized', since the desktop can maximize the window itself (on a
-- title bar double-click, say).
toggleMaximizedUi :: Ui :> es => Eff es ()
toggleMaximizedUi = withNativeWindow $ \nw -> do
  maxed <- winMaximized <$> readIORef (nwState nw)
  (if maxed then hostRestore else hostMaximize) (nwHost nw)

-- | End the session after this frame is drawn; the backend's runner closes
-- the window and returns. With 'wsExitOnCloseRequest' off, this is how the
-- window closes.
quitUi :: Ui :> es => Eff es ()
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
requestScreenshot :: Ui :> es => (Maybe Screenshot -> IO ()) -> Eff es ()
requestScreenshot answer = withContext $ \ctx -> askHostIO ctx >>= maybe (answer Nothing) (`queueScreenshot` answer)

-- | Queue an answer for 'answerScreenshots'.
queueScreenshot :: NativeWindow -> (Maybe Screenshot -> IO ()) -> IO ()
queueScreenshot nw answer = atomicModifyIORef' (nwShots nw) (\waiting -> (answer : waiting, ()))

-- | An action for another thread, such as a 'NanoUI.useTaskStatus' job. It
-- wakes the loop, waits for the next frame to be presented, and returns a
-- screenshot of it, as 'requestScreenshot' does. Without a window it returns
-- 'Nothing' at once. On the UI thread it deadlocks.
--
-- > shoot <- askScreenshot
-- > saved <- useTaskStatus shots (shoot >>= traverse_ (savePng "shot.png"))
askScreenshot :: Ui :> es => Eff es (IO (Maybe Screenshot))
askScreenshot = withContext $ \ctx -> maybe (pure Nothing) (shoot ctx) <$> askHostIO ctx
  where
    shoot ctx nw = do
      box <- newEmptyMVar
      queueScreenshot nw (putMVar box)
      wakeFromThread ctx
      takeMVar box

-- | A screenshot per key. The first frame with a new key requests one, which
-- arrives a frame or two later; until then it returns the previous key's
-- screenshot, as 'useTask' does. Change the key to take another. Like any
-- hook, call it conditionally only inside 'NanoUI.scope'.
--
-- > (shots, setShots) <- useInt 0
-- > whenM (button "Screenshot") (setShots (shots + 1))
-- > shot <- scope (if shots == 0 then pure Nothing else useScreenshot shots)
useScreenshot :: (Eq k, Typeable k, Ui :> es) => k -> Eff es (Maybe Screenshot)
useScreenshot k = join <$> (useTask k =<< askScreenshot)

-- | Answer every screenshot request since the last call, in request order,
-- with one run of @capture@, which does not run when none are pending. A
-- backend calls this once a frame is presented, and not while it has no
-- frame to capture. The scale is the last one given to 'reportWindowState'.
--
-- Answers are view code that usually changes what a view reads, so
-- answering any requests a frame.
answerScreenshots :: Context -> IO (Maybe RgbaPixels) -> IO ()
answerScreenshots ctx capture =
  withHost ctx $ \nw -> do
    waiting <- atomicModifyIORef' (nwShots nw) ([],)
    unless (null waiting) $ do
      scale <- winScale <$> readIORef (nwState nw)
      shot <- fmap (`Screenshot` scale) <$> capture
      mapM_ ($ shot) (reverse waiting)
      markDirtyCovered ctx
