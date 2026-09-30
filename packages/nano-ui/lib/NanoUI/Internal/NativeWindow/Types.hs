-- | Window values and backend callbacks, independent of the UI context.
module NanoUI.Internal.NativeWindow.Types
  ( RgbaPixels (..)
  , Screenshot (..)
  , WindowSettings (..)
  , WindowPosition (..)
  , WindowMode (..)
  , WindowState (..)
  , WindowHost (..)
  , NativeWindow (..)
  )
where

import Data.ByteString (ByteString)
import Data.IORef (IORef)
import Data.Text (Text)
import NanoUI.Internal.Types (Size)

-- | RGBA8, top-to-bottom tightly packed rows with straight alpha. Construct
-- through 'NanoUI.rgbaPixels' to check the byte count.
data RgbaPixels = RgbaPixels
  { rgbaWidth :: !Int
  , rgbaHeight :: !Int
  , rgbaBytes :: !ByteString
  -- ^ Exactly four bytes per pixel.
  }
  deriving Eq

instance Show RgbaPixels where
  showsPrec d (RgbaPixels w h _) =
    showParen
      (d > 10)
      (showString "RgbaPixels " . shows w . showString "x" . shows h)

-- | Presented pixels and pixels per logical layout unit.
data Screenshot = Screenshot
  { screenshotPixels :: !RgbaPixels
  , screenshotScale :: !Float
  }
  deriving (Eq, Show)

-- | Backend-independent initial window settings. Sizes are in layout units.
data WindowSettings = WindowSettings
  { wsTitle :: !Text
  -- ^ Title bar/taskbar text, initially "nano-ui".
  , wsSize :: !Size
  -- ^ Initial logical view size, normally 1280 by 800.
  , wsPosition :: !WindowPosition
  , wsMinSize :: !(Maybe Size)
  -- ^ Nothing or a zero axis means no limit.
  , wsMaxSize :: !(Maybe Size)
  , wsIcon :: !(Maybe RgbaPixels)
  , wsResizable :: !Bool
  , wsMode :: !WindowMode
  , wsTransparent :: !Bool
  -- ^ Desktop transparency where the theme background is translucent. SDL
  -- supports it with a compositor; RGFW windows are opaque.
  , wsOpacity :: !Float
  -- ^ Whole-window opacity, including decorations, from 0 to 1 (SDL only).
  , wsExitOnCloseRequest :: !Bool
  -- ^ True ends the session on a platform close request. False exposes
  -- 'winCloseRequested' so the view can confirm unsaved work before quitting.
  }
  deriving (Eq, Show)

data WindowPosition
  = -- | Desktop placement (centred by RGFW).
    WindowPositionDefault
  | WindowPositionCentered
  | -- | Top-left corner in desktop coordinates.
    WindowPositionAt !Int !Int
  deriving (Eq, Show)

data WindowMode = Windowed | Fullscreen | Hidden
  deriving (Eq, Show, Enum, Bounded)

-- | The window as reported at frame start.
data WindowState = WindowState
  { winSize :: !Size
  -- ^ View size in logical layout units.
  , winScale :: !Float
  -- ^ Physical pixels per layout unit.
  , winPosition :: !(Maybe (Int, Int))
  -- ^ Desktop coordinates, when available (not reported on Wayland).
  , winFocused :: !Bool
  , winMaximized :: !Bool
  , winMinimized :: !Bool
  , winFullscreen :: !Bool
  , winCloseRequested :: !Bool
  -- ^ A one-frame request when automatic close handling is disabled.
  }
  deriving (Eq, Show)

-- | Backend operations invoked on the UI thread. Sizes are in layout units;
-- zero axes or absent size limits mean unlimited. Build from defaultWindowHost.
data WindowHost = WindowHost
  { hostSetTitle :: Text -> IO ()
  , hostSetIcon :: RgbaPixels -> IO ()
  , hostSetMinSize :: Maybe Size -> IO ()
  , hostSetMaxSize :: Maybe Size -> IO ()
  , hostSetOpacity :: Float -> IO ()
  , hostSetMode :: WindowMode -> IO ()
  , hostMove :: Int -> Int -> IO ()
  , hostCenter :: IO ()
  , hostResize :: Size -> IO ()
  , hostMinimize :: IO ()
  , hostMaximize :: IO ()
  , hostRestore :: IO ()
  }

-- | Concrete session-owned host, last settings/state, and pending callbacks.
data NativeWindow = NativeWindow
  { nwHost :: !WindowHost
  , nwSettings :: !(IORef WindowSettings)
  , nwState :: !(IORef WindowState)
  , nwStateRead :: !(IORef Bool)
  , nwQuit :: !(IORef Bool)
  , nwShots :: !(IORef [Maybe Screenshot -> IO ()])
  }
