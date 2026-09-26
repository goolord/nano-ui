-- | The native window a view runs in: how it opens ('WindowSettings'), what
-- a view reads of it ('askWindow'), what a view asks of it while it runs --
-- a title, an icon, limits on its size, a mode, a move, a screenshot, the
-- end of the session -- and the pixels it takes and gives back.
--
-- A backend opens its window from a 'WindowSettings', installs a
-- 'WindowHost' that says what it does with each request, and reports the
-- window's state once a frame. A view asks through the functions here, which
-- do nothing without a host, as under a test context: 'requestScreenshot'
-- answers 'Nothing', and 'askWindow' describes a focused window the size of
-- the frame's input.
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

--------------------------------------------------------------------------------
-- Pixels
--------------------------------------------------------------------------------

-- | Pixels as RGBA8: a positive width and height, and tightly packed bytes,
-- four a pixel (red, green, blue, straight alpha), in rows from the top. A
-- window takes them as its icon, and a screenshot comes back as them. Made
-- with 'rgbaPixels', which checks that the bytes are as many as the size
-- says, so nothing that takes them has to.
data RgbaPixels = RgbaPixels
  { rgbaWidth :: !Int
  , rgbaHeight :: !Int
  , rgbaBytes :: !ByteString
  -- ^ Exactly @4 * width * height@ of them.
  }
  deriving (Eq)

-- | The size alone: the bytes are too many to show.
instance Show RgbaPixels where
  showsPrec d (RgbaPixels w h _) =
    showParen (d > 10) (showString "RgbaPixels " . shows w . showString "x" . shows h)

-- | Pixels @width@ by @height@, or 'Nothing' when either is not positive or
-- the bytes are not four a pixel. Register them to draw them with
-- @registerImageRgba iid (rgbaWidth p) (rgbaHeight p) (rgbaBytes p)@.
rgbaPixels :: Int -> Int -> ByteString -> Maybe RgbaPixels
rgbaPixels w h bytes
  | w > 0 && h > 0 && BS.length bytes == w * h * 4 = Just (RgbaPixels w h bytes)
  | otherwise = Nothing

-- | A frame as it went to the window: its pixels, at the size the window's
-- pixels are, with the alpha the frame was drawn with, which is the theme's
-- window colour's wherever nothing covers it; and how many of them a layout
-- unit is, so the pixels are @screenshotScale@ times 'NanoUI.windowSize'.
data Screenshot = Screenshot
  { screenshotPixels :: !RgbaPixels
  , screenshotScale :: !Float
  }
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Settings
--------------------------------------------------------------------------------

-- | How a window opens, for any backend: the SDL backend's @sdlWindowSettings@
-- and the RGFW backend's @optWindow@. Sizes are in layout units, the units
-- of 'NanoUI.windowSize', which the backend converts at the scale the window
-- opens at.
data WindowSettings = WindowSettings
  { wsTitle :: !Text
  -- ^ What the title bar, the taskbar and the window switcher show
  -- (default: @"nano-ui"@). 'setWindowTitleUi' changes it.
  , wsSize :: !Size
  -- ^ The view's size (default: 1280x800). 'resizeWindowUi' changes it.
  , wsPosition :: !WindowPosition
  -- ^ Where the window opens (default: 'WindowPositionDefault').
  , wsMinSize :: !(Maybe Size)
  -- ^ The smallest the user may make the window (default: no limit). An
  -- axis of zero has no limit. 'setWindowMinSizeUi' changes it.
  , wsMaxSize :: !(Maybe Size)
  -- ^ The largest the user may make the window (default: no limit).
  , wsIcon :: !(Maybe RgbaPixels)
  -- ^ The window's icon (default: the desktop's). 32 or 64 pixels square
  -- suits most desktops, which scale it to the sizes they show.
  , wsResizable :: !Bool
  -- ^ Whether the user may resize the window (default: 'True').
  , wsMode :: !WindowMode
  -- ^ Windowed, fullscreen or hidden (default: 'Windowed').
  -- 'setWindowModeUi' changes it.
  , wsTransparent :: !Bool
  -- ^ Let the desktop show through where the theme's window colour
  -- ('NanoUI.windowColor') is translucent (default: 'False'). The SDL
  -- backend's windows can, given a compositor; RGFW's are always opaque.
  , wsOpacity :: !Float
  -- ^ Fade the whole window, frame and all, from 0 to 1 (default: 1), where
  -- the backend and the desktop allow: the SDL backend's windows fade, and
  -- RGFW's do not. 'setWindowOpacityUi' changes it.
  , wsExitOnCloseRequest :: !Bool
  -- ^ Whether the session ends when the window is asked to close, by its
  -- close button, the window manager or the platform asking the app to quit
  -- (default: 'True'). With 'False' the view is told instead
  -- ('winCloseRequested') and decides, with 'quitUi' to end it: to ask
  -- about unsaved work first, say.
  }
  deriving (Eq, Show)

-- | A resizable 1280x800 window titled @"nano-ui"@, where the desktop puts
-- it, with no size limits, the desktop's icon, opaque, ending the session
-- when it is closed.
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
  -- ^ Where the desktop puts a new window. RGFW cannot ask the desktop and
  -- centres it.
  | WindowPositionCentered
  -- ^ Centred on the display.
  | WindowPositionAt !Int !Int
  -- ^ The window's top-left corner, in the desktop's coordinates
  -- ('moveWindowUi').
  deriving (Eq, Show)

-- | Whether a window is in a window of its own, fills its display, or is not
-- shown at all.
data WindowMode = Windowed | Fullscreen | Hidden
  deriving (Eq, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- State
--------------------------------------------------------------------------------

-- | The window as it is this frame ('askWindow').
data WindowState = WindowState
  { winSize :: !Size
  -- ^ The view's size in layout units: 'NanoUI.windowSize'.
  , winScale :: !Float
  -- ^ The window's pixels a layout unit: its pixel density times the UI
  -- scale.
  , winPosition :: !(Maybe (Int, Int))
  -- ^ The window's top-left corner in the desktop's coordinates, as far as
  -- the desktop says: Wayland does not.
  , winFocused :: !Bool
  -- ^ Whether the window has the keyboard.
  , winMaximized :: !Bool
  , winMinimized :: !Bool
  , winFullscreen :: !Bool
  , winCloseRequested :: !Bool
  -- ^ Whether the window was asked to close since the last frame, which
  -- only a window with 'wsExitOnCloseRequest' off is told; the session goes
  -- on until the view calls 'quitUi'. It is set for one frame, like a
  -- click, so keep what it asks for in state of your own:
  --
  -- > (confirming, setConfirming) <- useFlag False
  -- > closing <- winCloseRequested <$> askWindow
  -- > when (closing && not unsaved) quitUi
  -- > when (closing && unsaved) (setConfirming True)
  }
  deriving (Eq, Show)

-- | What a view reads without a window, as under a test context: a focused
-- window of scale 1 in no known place, in no special mode. Its size is the
-- frame's.
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

-- | The window as the backend reported it as this frame began, and whether
-- it was asked to close. 'NanoUI.windowSize' is its 'winSize'. A view that
-- reads it gets a frame when it changes, a window's focus say; one that
-- never does pays nothing for it.
askWindow :: Ui :> es => Eff es WindowState
askWindow = do
  size <- windowSize
  withContext $ \ctx ->
    askHostIO ctx >>= \case
      Nothing -> pure defaultWindowState {winSize = size}
      Just nw -> do
        writeIORef (nwStateRead nw) True
        (\st -> st {winSize = size}) <$> readIORef (nwState nw)

--------------------------------------------------------------------------------
-- The host
--------------------------------------------------------------------------------

-- | What a backend does with what a view asks of its window. Each runs on
-- the UI thread in the middle of a view. The sizes are in layout units,
-- which the backend converts at the scale its window is at; an axis of zero,
-- or no size at all, has no limit.
--
-- Build one from 'defaultWindowHost', whose every field does nothing, with a
-- record update of the fields the backend can do, so a field added later
-- does nothing on it rather than break it:
--
-- > installWindowHost ctx settings
-- >   defaultWindowHost {hostSetTitle = setTitle win, hostMove = moveWindow win}
data WindowHost = WindowHost
  { hostSetTitle :: Text -> IO ()
  , hostSetIcon :: RgbaPixels -> IO ()
  , hostSetMinSize :: Maybe Size -> IO ()
  -- ^ Limit how small the user may make the window, or take the limit off.
  , hostSetMaxSize :: Maybe Size -> IO ()
  , hostSetOpacity :: Float -> IO ()
  -- ^ Fade the whole window, from 0 (invisible) to 1 (opaque).
  , hostSetMode :: WindowMode -> IO ()
  , hostMove :: Int -> Int -> IO ()
  -- ^ Put the window's top-left corner at a point of the desktop.
  , hostCenter :: IO ()
  -- ^ Centre the window on its display.
  , hostResize :: Size -> IO ()
  -- ^ Make the view this size.
  , hostMinimize :: IO ()
  , hostMaximize :: IO ()
  , hostRestore :: IO ()
  -- ^ Give a maximized or minimized window back its size.
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

-- | The installed host, and what goes with it: the settings as the window
-- has them now, so that a view asking every frame for what it has costs a
-- comparison; the state the backend reported; whether a view has read it;
-- whether a view asked to quit; and the screenshots waiting, newest first.
data NativeWindow = NativeWindow
  { nwHost :: !WindowHost
  , nwSettings :: !(IORef WindowSettings)
  , nwState :: !(IORef WindowState)
  , nwStateRead :: !(IORef Bool)
  , nwQuit :: !(IORef Bool)
  , nwShots :: !(IORef [Maybe Screenshot -> IO ()])
  }

-- | Say what the context's window does with a view's requests, once the
-- backend has opened it from @settings@: with their title and size, in their
-- mode, and resizable and transparent as they say. This applies the rest
-- through the host, as a view would: the size limits, the icon, the opacity
-- and then the position. A backend calls this once, before the first frame;
-- a later call replaces the host and answers the screenshots waiting on the
-- old one with 'Nothing'.
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

-- | Report the window's state, once a frame before the view runs: the
-- backend's part of 'WindowState'. Its 'winSize' is the frame input's and its
-- 'winCloseRequested' the session loop's, so what is passed for them does
-- not matter. A change asks for a frame, if a view reads the state.
reportWindowState :: Context -> WindowState -> IO ()
reportWindowState ctx st =
  withHost ctx $ \nw -> do
    old <- readIORef (nwState nw)
    let new = st {winCloseRequested = winCloseRequested old}
    unless (new == old) $ do
      writeIORef (nwState nw) new
      read' <- readIORef (nwStateRead nw)
      when read' (markDirtyCovered ctx)

-- | For a session loop, when the window is asked to close: 'True' when the
-- session should end ('wsExitOnCloseRequest', or no window installed).
-- Otherwise the frame after sees 'winCloseRequested', which asks for one;
-- call 'clearWindowClose' once it has run.
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

-- | End what 'requestWindowClose' began, after the frame that saw it.
clearWindowClose :: Context -> IO ()
clearWindowClose ctx = withHost ctx $ \nw -> modifyIORef' (nwState nw) (\s -> s {winCloseRequested = False})

-- | Whether a view called 'quitUi': a session loop checks after each frame
-- and ends.
quitRequested :: Context -> IO Bool
quitRequested ctx = maybe (pure False) (readIORef . nwQuit) =<< askHostIO ctx

withHost :: Context -> (NativeWindow -> IO ()) -> IO ()
withHost ctx act = askHostIO ctx >>= traverse_ act

withNativeWindow :: Ui :> es => (NativeWindow -> IO ()) -> Eff es ()
withNativeWindow act = withContext (`withHost` act)

--------------------------------------------------------------------------------
-- From a view
--------------------------------------------------------------------------------

-- | Change a setting the window keeps, when it is not what the window has.
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

-- | Set the window's title ('wsTitle').
--
-- This and the other setters below act only when the value is not what the
-- window has, whether it opened with it or a view set it, so a view may call
-- them every frame with what it wants for the cost of a comparison.
setWindowTitleUi :: Ui :> es => Text -> Eff es ()
setWindowTitleUi t = withNativeWindow (\nw -> setting wsTitle (\v s -> s {wsTitle = v}) hostSetTitle nw t)

-- | Set the window's icon ('wsIcon').
setWindowIconUi :: Ui :> es => RgbaPixels -> Eff es ()
setWindowIconUi icon = withNativeWindow (`setIcon` icon)

-- | Keep the window from being resized smaller than a size in layout units,
-- or take the limit off with 'Nothing'. An axis of zero has no limit. The
-- backend converts the size at the UI scale in effect when it changes, and
-- a later change of scale does not convert it again.
setWindowMinSizeUi :: Ui :> es => Maybe Size -> Eff es ()
setWindowMinSizeUi s = withNativeWindow (`setMinSize` s)

-- | Keep the window from being resized larger than a size in layout units,
-- or take the limit off with 'Nothing', as for 'setWindowMinSizeUi'.
setWindowMaxSizeUi :: Ui :> es => Maybe Size -> Eff es ()
setWindowMaxSizeUi s = withNativeWindow (`setMaxSize` s)

-- | Fade the whole window, frame and all, from 0 (invisible) to 1 (opaque),
-- where the backend and the desktop allow ('wsOpacity'). This is not
-- transparency, for which see 'wsTransparent'.
setWindowOpacityUi :: Ui :> es => Float -> Eff es ()
setWindowOpacityUi o = withNativeWindow (`setOpacity` o)

-- | Put the window in a window of its own, fill its display with it, or hide
-- it ('wsMode').
--
-- > (fullscreen, toggleFullscreen) <- useToggle False
-- > whenM (shortcut (key (KeyF 11))) toggleFullscreen
-- > setWindowModeUi (if fullscreen then Fullscreen else Windowed)
setWindowModeUi :: Ui :> es => WindowMode -> Eff es ()
setWindowModeUi m = withNativeWindow (\nw -> setting wsMode (\v s -> s {wsMode = v}) hostSetMode nw m)

-- | Move the window's top-left corner to a point of the desktop: the SDL
-- backend's window coordinates, RGFW's pixels.
--
-- This and the other commands below act on every call, since the user
-- moves, resizes, maximizes and restores the window too: call them when the
-- window should change, from an event, not every frame. A desktop may not
-- let a window place itself (Wayland does not), and a maximized or
-- fullscreen window stays put.
moveWindowUi :: Ui :> es => Int -> Int -> Eff es ()
moveWindowUi x y = withNativeWindow (\nw -> hostMove (nwHost nw) x y)

-- | Centre the window on its display.
centerWindowUi :: Ui :> es => Eff es ()
centerWindowUi = withNativeWindow (hostCenter . nwHost)

-- | Make the view a size in layout units, whatever frame the desktop keeps
-- around it.
resizeWindowUi :: Ui :> es => Size -> Eff es ()
resizeWindowUi s = withNativeWindow (\nw -> hostResize (nwHost nw) s)

-- | Put the window away to the taskbar.
minimizeWindowUi :: Ui :> es => Eff es ()
minimizeWindowUi = withNativeWindow (hostMinimize . nwHost)

-- | Fill the screen with the window, keeping the desktop's panels.
maximizeWindowUi :: Ui :> es => Eff es ()
maximizeWindowUi = withNativeWindow (hostMaximize . nwHost)

-- | Give a maximized or minimized window back the size it had.
restoreWindowUi :: Ui :> es => Eff es ()
restoreWindowUi = withNativeWindow (hostRestore . nwHost)

-- | Maximize a window that is not, restore one that is: which it is comes
-- from 'winMaximized', since the desktop maximizes a window by itself when
-- its title bar is double-clicked.
toggleMaximizedUi :: Ui :> es => Eff es ()
toggleMaximizedUi = withNativeWindow $ \nw -> do
  maxed <- winMaximized <$> readIORef (nwState nw)
  (if maxed then hostRestore else hostMaximize) (nwHost nw)

-- | End the session once this frame is drawn: the backend's runner closes
-- the window and returns. For a window that does not close by itself
-- ('wsExitOnCloseRequest' off), this is how it closes.
quitUi :: Ui :> es => Eff es ()
quitUi = withNativeWindow (\nw -> writeIORef (nwQuit nw) True)

-- | Ask for a screenshot of the window: the frame this view is building,
-- once it is on screen. The action gets it after that frame is presented,
-- on the UI thread, before the next frame starts. It gets 'Nothing' when the
-- backend cannot capture, which is straight away when the view is not
-- running in a window.
--
-- Each call is answered once, so ask from an event, such as a click, rather
-- than every frame: a view that asks every frame gets a frame and a capture
-- after every frame. Keep the action short (a write to an 'IORef', a
-- 'Control.Concurrent.forkIO' of the encoding), since the next frame waits
-- on it; the frame after the answer runs by itself, for a view that shows
-- what came back. 'useScreenshot' hands the view the screenshot itself.
requestScreenshot :: Ui :> es => (Maybe Screenshot -> IO ()) -> Eff es ()
requestScreenshot answer = withContext $ \ctx -> askHostIO ctx >>= maybe (answer Nothing) (`queueScreenshot` answer)

-- | Add an answer to the screenshots waiting ('answerScreenshots').
queueScreenshot :: NativeWindow -> (Maybe Screenshot -> IO ()) -> IO ()
queueScreenshot nw answer = atomicModifyIORef' (nwShots nw) (\waiting -> (answer : waiting, ()))

-- | An action for another thread, such as a 'NanoUI.useTaskStatus' job, that
-- waits for the next frame to be on screen and returns a screenshot of it,
-- as 'requestScreenshot' answers. It wakes the loop for that frame. Without
-- a window it returns 'Nothing' at once. Run on the UI thread, it would wait
-- for a frame that cannot come.
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

-- | A screenshot of the window for each key: the first frame with a key
-- asks for one, which comes a frame or two later, as 'useTask' returns its
-- job's result (and until then the last key's screenshot). Bump the key to
-- take another; like any hook, call it on some frames and not others only
-- inside 'NanoUI.scope'.
--
-- > (shots, setShots) <- useInt 0
-- > whenM (button "Screenshot") (setShots (shots + 1))
-- > shot <- scope (if shots == 0 then pure Nothing else useScreenshot shots)
useScreenshot :: (Eq k, Typeable k, Ui :> es) => k -> Eff es (Maybe Screenshot)
useScreenshot k = join <$> (useTask k =<< askScreenshot)

-- | Answer the screenshots views have asked for since the last answer, with
-- one run of @capture@, in the order they were asked; with none waiting
-- @capture@ does not run. A backend calls this once a frame is on screen,
-- with a capture of that frame, and leaves the requests waiting when it has
-- no frame to capture yet. The screenshot's scale is the one it last
-- reported ('reportWindowState').
--
-- The answers are views' own code, and they generally change what a view
-- reads, so this asks for another frame when it answers any.
answerScreenshots :: Context -> IO (Maybe RgbaPixels) -> IO ()
answerScreenshots ctx capture =
  withHost ctx $ \nw -> do
    waiting <- atomicModifyIORef' (nwShots nw) ([],)
    unless (null waiting) $ do
      scale <- winScale <$> readIORef (nwState nw)
      shot <- fmap (`Screenshot` scale) <$> capture
      mapM_ ($ shot) (reverse waiting)
      markDirtyCovered ctx
