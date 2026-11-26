-- | The native resources and per-window state of an SDL session
-- ('SdlEnv'), with the records it keeps of the retained frame ('Retain')
-- and of vertical sync ('PresentProbe'). "NanoUI.Sdl.Internal.Window"
-- opens and closes them.
module NanoUI.Sdl.Internal.Env
  ( SdlEnv (..)
  , PresentProbe (..)
  , Retain (..)
  , emptyRetain
  , dropRetainRef
  ) where

import Control.Monad (unless)
import Data.Int (Int32)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text (Text)
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI.Internal.Context (Context)
import NanoUI.Internal.Debug (DebugSamplerRef)
import NanoUI.Sdl.Internal.Chrome.Types (ChromeState)
import NanoUI.Sdl.Internal.Cursor (SdlCursors)
import NanoUI.Sdl.Internal.Debug (SdlDebugSnapshot)
import NanoUI.Sdl.Internal.Font (SdlFontCache)
import NanoUI.Sdl.Internal.Image (ImageAtlas)
import NanoUI.Sdl.Internal.Input (TextInputSync)
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont)
import NanoUI.Sdl.Internal.Render (RenderBatch)
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Video (SDL_Window)
import SDL3.Sys.Blendmode (SDL_BlendMode)
import SDL3.Sys.Render (destroyTexture)

-- | Native resources owned by a 'NanoUI.Backend.Sdl.withSdl'
-- callback. Window, renderer, and font handles must stay on the display
-- thread and must not outlive the callback. Use the supplied context, which
-- contains this environment as host data.
data SdlEnv = SdlEnv
  { sdlWindow :: Ptr SDL_Window
  , sdlRenderer :: Ptr SDL_Renderer
  , sdlRendererName :: !Text
  , sdlBatch :: RenderBatch
  , sdlFontRequestRef :: !(IORef NanoUIFont)
  , sdlFontAppliedRef :: !(IORef NanoUIFont)
  , sdlForcedScale :: !(Maybe Float)
  -- ^ NANO_FORCE_SCALE override of the pixel density, read at startup.
  , sdlScaleRef :: IORef Float
  -- ^ Backbuffer pixels per layout unit: the pixel density times the zoom.
  , sdlUiScaleRef :: !(IORef Float)
  -- ^ The requested UI scale; see 'NanoUI.Backend.Sdl.sdlAppUiScale'.
  , sdlPointerSeen :: !(IORef Bool)
  -- ^ Whether a pointer event has arrived, so the input's position, which
  -- the events set in layout units, can be trusted over a query.
  , sdlImages :: ImageAtlas
  , sdlCursors :: SdlCursors
  , sdlDebug :: DebugSamplerRef
  , sdlDebugPublished :: IORef (Maybe SdlDebugSnapshot)
  , sdlFrameTrace :: !Bool
  -- ^ Whether @NANO_FRAME_TRACE@ was set when the session opened.
  , sdlRetain :: IORef Retain
  , sdlLastPresented :: IORef Bool
  , sdlVsync :: !Bool
  , sdlRefreshPeriod :: !Double
  -- ^ The refresh period of the window's display when it opened, in
  -- seconds; 'sdlRefreshPeriodRef' follows it to other displays.
  , sdlRefreshPeriodRef :: !(IORef Double)
  -- ^ The refresh period of the window's current display, in seconds, read
  -- again when the window moves to another display or the display's mode
  -- changes ('NanoUI.Sdl.Internal.Pacing.refreshDisplayRate').
  , sdlPresentProbe :: !(IORef PresentProbe)
  -- ^ Whether presents wait for vertical sync as 'sdlVsync' asks
  -- ('NanoUI.Sdl.Internal.Pacing.notePresent').
  , sdlContinuous :: !Bool
  , sdlTransparent :: !(Maybe (SDL_BlendMode, SDL_BlendMode))
  -- ^ For a transparent window, the blend modes for drawing and for
  -- presenting the retained frame; 'Nothing' when opaque. See
  -- 'NanoUI.Sdl.Internal.Window.transparentBlends'.
  , sdlCachedCtx :: !(IORef Context)
  , sdlFontCache :: !SdlFontCache
  , sdlChromeState :: !ChromeState
  -- ^ What a borderless window's own title bar is for; see
  -- "NanoUI.Sdl.Internal.Chrome".
  , sdlTextInput :: !TextInputSync
  -- ^ The focused-field state last sent to SDL's text input; updated each
  -- drawn frame.
  , sdlSizeLimits :: !(Maybe (IORef (Int32, Int32, Int32, Int32)))
  -- ^ On a 'NanoUI.Sdl.Internal.Display.waylandToplevel' window, its
  -- minimum and maximum width and height in window coordinates (zero for
  -- none), which nano-ui sends to the compositor before each present
  -- instead of giving them to SDL. 'Nothing' elsewhere, where SDL keeps
  -- them.
  , sdlSizeLimitsSent :: !(IORef Bool)
  -- ^ Whether the limits last sent from 'sdlSizeLimits' were any, so none
  -- are sent again only to clear them.
  }

-- | What a session with 'sdlVsync' has seen of its presents. A driver can
-- turn vertical sync off whatever the application asks, and then a present
-- returns at once; a loop that trusted it to wait would draw an animation
-- as fast as it can.
data PresentProbe
  = PresentProbe !Double !Int
  -- ^ When the last present returned ('GHC.Clock.getMonotonicTime'), and
  -- how many presents in a row came less than half a refresh after the one
  -- before.
  | VsyncIgnored
  -- ^ 'NanoUI.Sdl.Internal.Pacing.vsyncProbeFrames' presents in a row came
  -- that soon: presents do not wait, and the session paces animation by
  -- time instead.
  deriving (Eq, Show)

-- | The retained framebuffer. The texture is allocated in blocks larger than
-- the window, so a resize drag reuses it instead of creating a render target
-- per pixel of movement; only the top-left used area is drawn and presented.
data Retain = Retain
  { retainTexture :: !(Ptr SDL_Texture)
  , retainCapW :: !Int
  , retainCapH :: !Int
  -- ^ The texture's allocated size in pixels.
  , retainW :: !Int
  , retainH :: !Int
  -- ^ The pixel size the last frame used.
  , retainScale :: !Float
  }

-- | Empty retained framebuffer state before allocation or after teardown.
emptyRetain :: Retain
emptyRetain = Retain nullPtr 0 0 0 0 0

-- | Destroy the retained texture in a reference, if any, and reset it.
dropRetainRef :: IORef Retain -> IO ()
dropRetainRef ref = do
  tex <- retainTexture <$> readIORef ref
  unless (tex == nullPtr) $ writeIORef ref emptyRetain >> destroyTexture tex
