-- | The native window a view runs in: the images it takes and gives back,
-- where it opens, and what a view may ask of it while it runs -- a
-- screenshot, an icon, limits on its size, a position and an opacity.
--
-- A backend says what it does with each of those by installing a
-- 'WindowHost' on the context. A view asks through the functions here, which
-- do nothing without one, as under a test context, and 'requestScreenshot'
-- answers 'Nothing'.
module NanoUI.Internal.NativeWindow
  ( RgbaImage (..)
  , WindowPosition (..)
  , WindowHost (..)
  , installWindowHost
  , answerScreenshots
  , requestScreenshot
  , setWindowIconUi
  , setWindowMinSizeUi
  , setWindowMaxSizeUi
  , setWindowPositionUi
  , setWindowOpacityUi
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context (Context, askHostIO, markDirtyCovered, setHost)
import NanoUI.Internal.Monad (Ui, withContext)
import NanoUI.Internal.Types (ImageId, Size)

-- | An image as RGBA8 pixels: a positive width and height and tightly packed
-- bytes, four a pixel (red, green, blue, straight alpha) in rows from the
-- top. The SDL runner registers images like this before the first frame
-- (@sdlAppImages@), a window takes one as its icon, and a screenshot comes
-- back as one.
--
-- The id is what the image is registered under for 'NanoUI.image' to draw.
-- An icon or a screenshot has no use for it, and a screenshot comes with
-- @ImageId 0@, which no image is registered under: give it an id of its own
-- ('NanoUI.freshImageId') before registering it.
data RgbaImage = RgbaImage
  { rgbaImageId :: !ImageId
  , rgbaImageWidth :: !Int
  , rgbaImageHeight :: !Int
  , rgbaImagePixels :: !ByteString
  }
  deriving (Eq)

-- | Where a window goes: when it opens, or when a view moves it
-- ('setWindowPositionUi').
data WindowPosition
  = WindowPositionDefault
  -- ^ Wherever the desktop puts a new window. Moving a window that is open
  -- already to this leaves it where it is.
  | WindowPositionCentered
  -- ^ Centred on the display the window is on.
  | WindowPositionAt !Int !Int
  -- ^ The window's top-left corner, in the desktop's coordinates: the SDL
  -- backend's window coordinates, RGFW's pixels.
  deriving (Eq, Show)

-- | What a backend does with what a view asks of its window. Each runs on
-- the UI thread in the middle of a view. The sizes are in layout units, the
-- units of 'NanoUI.windowSize', which the backend converts to its own; an
-- axis of zero, or no size at all, has no limit.
data WindowHost = WindowHost
  { hostSetIcon :: RgbaImage -> IO ()
  -- ^ Give the window an icon.
  , hostSetMinSize :: Maybe Size -> IO ()
  -- ^ Limit how small the user may make the window, or take the limit off.
  , hostSetMaxSize :: Maybe Size -> IO ()
  -- ^ Limit how large the user may make the window, or take the limit off.
  , hostSetPosition :: WindowPosition -> IO ()
  -- ^ Move the window; 'WindowPositionDefault' leaves it where it is.
  , hostSetOpacity :: Float -> IO ()
  -- ^ Fade the whole window, from 0 (invisible) to 1 (opaque).
  }

-- | The installed host, with the screenshots waiting on it and what the view
-- last set, so that a view asking every frame for what it asked before
-- costs a comparison.
data NativeWindow = NativeWindow
  { nwHost :: !WindowHost
  , nwShots :: !(IORef [Maybe RgbaImage -> IO ()])
  -- ^ Newest first.
  , nwIcon :: !(IORef (Maybe RgbaImage))
  , nwMinSize :: !(IORef (Maybe (Maybe Size)))
  , nwMaxSize :: !(IORef (Maybe (Maybe Size)))
  , nwOpacity :: !(IORef (Maybe Float))
  }

-- | Say what the context's window does with a view's requests. A backend
-- calls this once, before the first frame; a later call replaces the host
-- and answers the screenshots waiting on the old one with 'Nothing'.
installWindowHost :: Context -> WindowHost -> IO ()
installWindowHost ctx host = do
  answerScreenshots ctx (pure Nothing)
  nw <-
    NativeWindow host
      <$> newIORef []
      <*> newIORef Nothing
      <*> newIORef Nothing
      <*> newIORef Nothing
      <*> newIORef Nothing
  setHost ctx nw

-- | Answer the screenshots views have asked for since the last answer, with
-- one run of @capture@, in the order they were asked; with none waiting
-- @capture@ does not run. A backend calls this once a frame is on screen,
-- with a capture of that frame, and leaves the requests waiting when it has
-- no frame to capture yet.
--
-- The answers are views' own code, and they generally change what a view
-- reads, so this asks for another frame when it answers any.
answerScreenshots :: Context -> IO (Maybe RgbaImage) -> IO ()
answerScreenshots ctx capture =
  askHostIO ctx >>= traverse_ (\nw -> do
    waiting <- atomicModifyIORef' (nwShots nw) ([],)
    unless (null waiting) $ do
      shot <- capture
      mapM_ ($ shot) (reverse waiting)
      markDirtyCovered ctx)

-- | Ask for a screenshot of the window: the frame this view is building,
-- once it is on screen. The action gets it after that frame is presented,
-- on the UI thread, before the next frame starts: at the size the window's
-- pixels are, with the alpha the frame was drawn with, which is the theme's
-- window colour's wherever nothing covers it. It gets 'Nothing' when the
-- backend cannot capture, which is straight away when the view is not
-- running in a window.
--
-- Each call is answered once, so ask from an event, such as a click, rather
-- than every frame: a view that asks every frame gets a frame and a capture
-- after every frame. Keep the action short (a write to an 'IORef', a
-- 'Control.Concurrent.forkIO' of the encoding), since the next frame waits
-- on it; the frame after the answer runs by itself, for a view that shows
-- what came back.
requestScreenshot :: Ui :> es => (Maybe RgbaImage -> IO ()) -> Eff es ()
requestScreenshot answer =
  withContext $ \ctx ->
    askHostIO ctx >>= \case
      Nothing -> answer Nothing
      Just nw -> modifyIORef' (nwShots nw) (answer :)

-- | Set the window's icon. The desktop scales it to the sizes it shows; 32
-- or 64 pixels square suits most.
setWindowIconUi :: Ui :> es => RgbaImage -> Eff es ()
setWindowIconUi icon = withNativeWindow $ \nw -> whenChanged (nwIcon nw) icon (hostSetIcon (nwHost nw) icon)

-- | Keep the window from being resized smaller than a size in layout units,
-- or take the limit off with 'Nothing'. An axis of zero has no limit. The
-- backend converts the size at the UI scale in effect when it changes, and
-- a later change of scale does not convert it again.
setWindowMinSizeUi :: Ui :> es => Maybe Size -> Eff es ()
setWindowMinSizeUi s = withNativeWindow $ \nw -> whenChanged (nwMinSize nw) s (hostSetMinSize (nwHost nw) s)

-- | Keep the window from being resized larger than a size in layout units,
-- or take the limit off with 'Nothing'. An axis of zero has no limit, and
-- the size is converted as for 'setWindowMinSizeUi'.
setWindowMaxSizeUi :: Ui :> es => Maybe Size -> Eff es ()
setWindowMaxSizeUi s = withNativeWindow $ \nw -> whenChanged (nwMaxSize nw) s (hostSetMaxSize (nwHost nw) s)

-- | Move the window. Unlike the other setters here, every call moves it,
-- since the user moves the window too: call it when it should move, not
-- every frame. A desktop may not let a window place itself (Wayland does
-- not), and a maximized or fullscreen window stays put.
setWindowPositionUi :: Ui :> es => WindowPosition -> Eff es ()
setWindowPositionUi p = withNativeWindow $ \nw -> hostSetPosition (nwHost nw) p

-- | Fade the whole window, frame and all, from 0 (invisible) to 1 (opaque),
-- where the backend and the desktop allow: the SDL backend fades it, and the
-- RGFW backend does nothing. This is not transparency: for a window the
-- desktop shows through, see the SDL backend's @sdlWindowTransparent@.
setWindowOpacityUi :: Ui :> es => Float -> Eff es ()
setWindowOpacityUi o = withNativeWindow $ \nw ->
  let clamped = max 0 (min 1 o) in whenChanged (nwOpacity nw) clamped (hostSetOpacity (nwHost nw) clamped)

withNativeWindow :: Ui :> es => (NativeWindow -> IO ()) -> Eff es ()
withNativeWindow act = withContext $ \ctx -> askHostIO ctx >>= traverse_ act

-- | Run @apply@ when @v@ is not what the view set last, and remember it.
whenChanged :: Eq a => IORef (Maybe a) -> a -> IO () -> IO ()
whenChanged ref v apply = do
  old <- readIORef ref
  unless (old == Just v) $ writeIORef ref (Just v) >> apply
