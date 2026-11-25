{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
-- The 'BackendSession' instance for 'Sdl' is an orphan by construction: the
-- core owns the backend kind, and this package owns its session type.
{-# OPTIONS_GHC -Wno-orphans #-}

-- | SDL session resources, window options, and font/display synchronisation.
-- It also re-exports the session record ("NanoUI.Sdl.Internal.Env"), the
-- present probe and pacing helpers ("NanoUI.Sdl.Internal.Pacing") and the
-- screenshots ("NanoUI.Sdl.Internal.Screenshot"), so this exposed module
-- keeps every name it has exported.
module NanoUI.Sdl.Internal.Window
  ( RgbaImage (..)
  , SdlEnv (..)
  , PresentProbe (..)
  , Retain (..)
  , SdlOptions (..)
  , RenderDriver (..)
  , defaultSdlOptions
  , withSdl
  , withSdlBench
  , openChildWindow
  , registerAppImages
  , sdlPacingMs
  , sdlLivePacingMs
  , refreshDisplayRate
  , notePresent
  , restartPresentProbe
  , presentPaces
  , askSdlEnv
  , withSdlEnv
  , syncDisplay
  , windowZoom
  , saveScreenshot
  , captureScreenshot
  , captureFrame
  ) where

import Control.Exception (IOException, catch, finally, onException)
import Control.Monad (forM_, mfilter, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire (Acquire, mkAcquire)
import Data.Acquire qualified as Acquire
import Data.Acquire (allocateAcquire)
import Control.Monad.Trans.Resource (closeInternalState, createInternalState, runInternalState)
import Data.Bits (zeroBits, (.|.))
import Data.ByteString qualified as BS
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.IORef (newIORef, readIORef, writeIORef)
import System.Environment (lookupEnv)
import System.Info (os)
import Text.Read (readMaybe)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Foreign qualified as TextForeign
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Utils (fromBool, maybePeek)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import NanoUI (Appearance, ImageId, Input (..), NanoUI, ShaderCapabilities (..), Size (..), Theme, V2 (..), WindowMode (..), WindowSettings (..), defaultWindowSettings, unlessM)
import NanoUI.Backend
  ( Backend (..), BackendSession, SBackend (..), askBackendSession, setBackendSession, withBackendSession, cancelTasks
  , closeWindowHost, installWindowHost, newPixelContext, registerImage, reportWindowState, setShaderCapabilities, setSystemAppearance
  , inBoundThread, setWakeLoopChecked, withClipboard, withTheme
  )
import NanoUI.Internal.Context (Context (..), followSystemTheme, setDrawSnapScale)
import NanoUI.Sdl.Internal.Display
import NanoUI.Sdl.Internal.Env (PresentProbe (..), Retain (..), SdlEnv (..))
import NanoUI.Sdl.Internal.Pacing
  ( notePresent
  , presentPaces
  , refreshDisplayRate
  , refreshPeriodOf
  , restartPresentProbe
  , sdlLivePacingMs
  , sdlPacingMs
  )
import NanoUI.Sdl.Internal.Screenshot (captureFrame, captureScreenshot, saveScreenshot)
import NanoUI.Sdl.Internal.Error (sdlFail)
import NanoUI.Sdl.Internal.Chrome.Types (clearChromeState, newChromeState)
import NanoUI.Sdl.Internal.Frame (WindowDecorations (..), applyDecorations, framedWindow)
import NanoUI.Sdl.Internal.Cursor (destroyCursors, initCursors)
import NanoUI.Sdl.Internal.Font
import NanoUI.Ttf.Internal.Support (syncSdlFontCache)
import NanoUI.Sdl.Internal.Font.Search (searchFonts, startFontIndex)
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont (..))
import NanoUI.Internal.Debug (newDebugSampler)
import NanoUI.Sdl.Internal.Image (destroyImageAtlas, newImageAtlas)
import NanoUI.Sdl.Internal.Input (newTextInputSync)
import NanoUI.Sdl.Internal.Render (destroyRenderBatch, newRenderBatch)
import NanoUI.Sdl.Internal.WindowOptions
import SDL3.Sys.Bindgen.Hints
  ( sDL_HINT_ASSERT
  , sDL_HINT_IME_IMPLEMENTED_UI
  , sDL_HINT_RENDER_DRIVER
  , sDL_HINT_RENDER_VSYNC
  , sDL_HINT_VIDEO_DRIVER
  )
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window, SDL_WindowFlags)
import SDL3.Sys.Bindgen.Init (SDL_InitFlags (..), sDL_INIT_VIDEO)
import SDL3.Sys.Clipboard (getClipboardTextSafe, setClipboardTextSafe)
import SDL3.Sys.Hints (resetHint, setHint)
import SDL3.Sys.Blendmode (SDL_BlendMode, composeCustomBlendMode)
import SDL3.Sys.Blendmode qualified as Blend
import SDL3.Sys.Init (initSafe, quitSafe)
import SDL3.Sys.Keyboard (stopTextInputSafe)
import SDL3.Sys.Render
  ( createWindowAndRendererSafe
  , destroyRendererSafe
  , destroyTexture
  , getRendererName
  , setRenderDrawBlendMode
  , setRenderScale
  , setRenderVSyncSafe
  )
import SDL3.Sys.Stdinc (free)
import SDL3.Sys.Video (destroyWindowSafe, getWindowDisplayScale)
import SDL3.Sys.Video qualified as SDL

-- | An image registered before the first frame ('sdlAppImages') under the id
-- 'NanoUI.image' draws it by. Width and height are positive; pixels are
-- tightly packed RGBA8, top row first.
data RgbaImage = RgbaImage
  { rgbaImageId :: !ImageId
  , rgbaImageWidth :: !Int
  , rgbaImageHeight :: !Int
  , rgbaImagePixels :: !BS.ByteString
  }
  deriving (Eq)

-- | Application-owned SDL settings.
data SdlOptions = SdlOptions
  { sdlWindowSettings :: !WindowSettings
  -- ^ Title, size, position, size limits, icon, mode, transparency,
  -- opacity and close behaviour (default: 'defaultWindowSettings'). Sizes
  -- are layout units converted at the opening zoom; a later
  -- 'NanoUI.Backend.Sdl.setSdlUiScale' does not resize the window. A
  -- 'WindowPositionDefault' window opens where the desktop puts it, or
  -- centred when 'sdlAppUiScale' enlarges it. A transparent window
  -- ('wsTransparent') repaints fully on any change. Where translucent
  -- colours overlap, the resulting alpha depends on the render driver: most
  -- keep the larger alpha, SDL's OpenGL renderer weights the new alpha by
  -- itself, and the software renderer adds them.
  , sdlWindowDecorations :: !WindowDecorations
  -- ^ How much of the desktop's title bar and frame the window keeps
  -- (default: 'DecorationsFull'). 'DecorationsFrame' is for a view that
  -- draws its own title bar ('NanoUI.Backend.Sdl.windowCaption');
  -- 'NanoUI.Backend.Sdl.setWindowDecorations' changes it afterwards.
  , sdlWindowAlwaysOnTop :: !Bool
  -- ^ Keep the window on top of other windows (default: 'False').
  , sdlAppVsync :: !Bool
  -- ^ Enable vertical synchronization (default: 'True').
  , sdlRenderDriver :: !RenderDriver
  -- ^ Which SDL render driver to ask for (default: 'RenderDriverAuto').
  -- An @SDL_RENDER_DRIVER@ in the environment wins over this.
  , sdlAppContinuous :: !Bool
  -- ^ Redraw on every pass without waiting for events, paced by vsync when
  -- 'sdlAppVsync' is on, or by the display's refresh period when the driver
  -- ignores vsync (default: 'False'). With 'sdlAppVsync' off the passes
  -- run without waiting.
  , sdlExplainLayout :: !Bool
  -- ^ Start with the layout overlay on (default: 'False'). A view toggles
  -- it with @explainLayout@.
  , sdlAppFont :: !NanoUIFont
  -- ^ UI font (default: installed sans-serif search, falling back to bundled Inter).
  , sdlAppMonoFont :: !NanoUIFont
  -- ^ Monospace font (default: installed monospace search, falling back to Inter).
  , sdlAppFontSize :: !Float
  -- ^ Base font size in points (default: 16).
  , sdlAppTheme :: !(Maybe Theme)
  -- ^ Initial UI theme override (default: 'Nothing').
  , sdlAppThemeFor :: !(Maybe (Maybe Appearance -> Theme))
  -- ^ Picks the theme from the desktop's light or dark setting and follows
  -- its changes, e.g. @'NanoUI.lightDark' light dark@ (default: 'Nothing').
  -- Overrides 'sdlAppTheme' when set; see 'NanoUI.followSystemTheme'.
  , sdlAppShouldQuit :: !(Input -> Bool)
  -- ^ Predicate on user input to trigger application exit (default: @const False@).
  , sdlAppImages :: !(SmallArray RgbaImage)
  -- ^ Initial RGBA textures registered before the first frame.
  , sdlAppUiScale :: !Float
  -- ^ Zoom of the whole UI, on top of the window's pixel density (default:
  -- 1). Zero or less follows the display's content scale, which on Windows
  -- is the desktop's scaling setting. 'NanoUI.Backend.Sdl.setSdlUiScale'
  -- changes it at runtime.
  }

-- | Resizable 1280x800 window with vsync, 16-point text, UI scale 1, installed
-- font lookup, and bundled-font fallback. Continuous rendering is disabled.
defaultSdlOptions :: SdlOptions
defaultSdlOptions =
  SdlOptions
    { sdlWindowSettings = defaultWindowSettings
    , sdlWindowDecorations = DecorationsFull
    , sdlWindowAlwaysOnTop = False
    , sdlAppVsync = True
    , sdlRenderDriver = RenderDriverAuto
    , sdlAppContinuous = False
    , sdlExplainLayout = False
    , sdlAppFont =
        FontSearch
          [ "Inter"
          , "Montserrat"
          , "Work Sans"
          , "Roboto"
          , "Open Sans"
          , "Helvetica Neue"
          ]
    , sdlAppMonoFont =
        FontSearch
          [ "Consolas"
          , "Courier New"
          , "Liberation Mono"
          , "DejaVu Sans Mono"
          , "monospace"
          ]
    , sdlAppFontSize = 16
    , sdlAppTheme = Nothing
    , sdlAppThemeFor = Nothing
    , sdlAppShouldQuit = const False
    , sdlAppImages = mempty
    , sdlAppUiScale = 1
    }

-- | SDL_WINDOW_HIGH_PIXEL_DENSITY: without it the window's surface
-- gets scale 1.0 even on a 2x / HiDPI output, so the compositor upscales the
-- whole window (blurry "looks upscaled"). With it, SDL_GetWindowPixelDensity
-- returns the real output scale where window coordinates are points (macOS,
-- Wayland), the window keeps its logical size, and the pixel buffer (and
-- therefore the retain texture, glyph atlas, and fonts) rasterizes at the
-- native pixel density. On Windows window coordinates are already pixels, so
-- the density stays 1 whatever the desktop scaling.
windowFlags :: SdlOptions -> SDL_WindowFlags
windowFlags opts =
  SDL.SDL_WINDOW_HIGH_PIXEL_DENSITY
    .|. flag (wsResizable settings) SDL.SDL_WINDOW_RESIZABLE
    .|. flag (wsMode settings == Fullscreen) SDL.SDL_WINDOW_FULLSCREEN
    .|. flag (sdlWindowDecorations opts /= DecorationsFull) SDL.SDL_WINDOW_BORDERLESS
    .|. flag (sdlWindowAlwaysOnTop opts) SDL.SDL_WINDOW_ALWAYS_ON_TOP
    .|. flag (wsMode settings == Hidden) SDL.SDL_WINDOW_HIDDEN
    .|. flag (wsTransparent settings) SDL.SDL_WINDOW_TRANSPARENT
  where
    settings = sdlWindowSettings opts
    flag on bit = if on then bit else zeroBits

-- | The zoom a UI scale setting asks for: the setting itself, or for zero or
-- less the display's content scale beyond the pixel density.
resolveZoom :: Ptr SDL_Window -> Float -> IO Float
resolveZoom win setting
  | setting > 0 = pure setting
  | otherwise = do
      display <- getWindowDisplayScale win
      density <- queryWindowPixelDensity win
      pure (if display > 0 then max 0.25 (display / density) else 1)

-- | Window coordinates per layout unit: the zoom the window is running at,
-- which is what 'sdlAppUiScale' asks for or what the display's content scale
-- works out to. A frame is laid out in window coordinates divided by this.
windowZoom :: SdlEnv -> IO Float
windowZoom env = resolveZoom (sdlWindow env) =<< readIORef (sdlUiScaleRef env)

-- | Synchronise display scale, requested fonts, and logical input coordinates.
-- Call on the display thread before drawing, then use both returned values;
-- font changes may replace the context's metrics and invalidate cached layout.
syncDisplay :: Context -> SdlEnv -> Input -> IO (Context, Input)
syncDisplay ctx env inp = do
  density <- maybe (queryWindowPixelDensity (sdlWindow env)) pure (sdlForcedScale env)
  zoom <- windowZoom env
  let scale = density * zoom
  oldScale <- readIORef (sdlScaleRef env)
  let scaleChanged = scaleMoved scale oldScale
  when scaleChanged $ do
    -- Presents leave the renderer at 1:1 pixels; re-assert it only when the
    -- pixel density moves.
    unlessM (setRenderScale (sdlRenderer env) 1 1) (sdlFail "SDL_SetRenderScale failed")
    writeIORef (sdlScaleRef env) scale
    setDrawSnapScale ctx scale
  -- Runtime font-family switch: the app publishes its requested family through
  -- 'setSdlUiFont'; resolve and apply it here, on the display thread, before
  -- the next frame so the atlas, metrics, and text resolver agree.
  requested <- readIORef (sdlFontRequestRef env)
  applied <- readIORef (sdlFontAppliedRef env)
  let fontChanged = requested /= applied
  when (scaleChanged || fontChanged) $ do
    source <-
      if fontChanged
        then resolveNanoUIFont requested
        else sdlFontCacheSource (sdlFontCache env)
    writeIORef (sdlFontAppliedRef env) requested
    writeIORef (sdlCachedCtx env) =<< syncSdlFontCache (sdlFontCache env) ctx source withSdlClipboard
  -- SDL updates the appearance before queueing the theme event that wakes
  -- us, and the query is free.
  setSystemAppearance ctx =<< querySystemAppearance
  reportWindowState ctx =<< queryWindowState (sdlWindow env) scale
  queried <- queryWindowLogicalSize (sdlWindow env)
  let winSize = case (queried, inputWindowSize inp) of
        (Size 0 0, Size 0 0) -> wsSize defaultWindowSettings
        (Size 0 0, s) -> s
        (Size sw sh, _) -> Size (sw / zoom) (sh / zoom)
  -- The events set the input's position, each its own and already in
  -- layout units, so a press keeps the point it happened at. SDL is asked
  -- only before any pointer event, or when the zoom moved what those units
  -- are. Off the window, SDL keeps the last position inside it: the input's
  -- own, moved off every widget when the pointer left, stays.
  inWindow <- pointerInWindow (sdlWindow env)
  seen <- readIORef (sdlPointerSeen env)
  mouse <-
    if inWindow /= 0 && (not seen || scaleChanged)
      then (\(V2 mx my) -> V2 (mx / zoom) (my / zoom)) <$> queryMouseWindowPos
      else pure (inputMousePos inp)
  ctxMeasured <- readIORef (sdlCachedCtx env)
  pure (ctxMeasured, inp {inputWindowSize = winSize, inputMousePos = mouse})

-- | Open native resources around an action and release them on exit, including
-- exceptions. Supplies an SDL-equipped context. This does not run an event
-- loop or apply the high-level runner's initial theme/image registration.
withSdl :: SdlOptions -> Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdl opts = withSdlWindow hints (windowFlags opts) opts
  where
    hints = do
      setSdlHint sDL_HINT_RENDER_VSYNC (if sdlAppVsync opts then "1" else "0")
      -- Text fields draw IME composition at their caret, so ask SDL for
      -- SDL_EVENT_TEXT_EDITING instead of letting the IME draw it. The
      -- candidate list stays the IME's, placed by SDL_SetTextInputArea.
      setSdlHint sDL_HINT_IME_IMPLEMENTED_UI "composition"
      -- SDL3 only auto-picks Wayland when the compositor has the fifo-v1 /
      -- commit-timing-v1 protocols. Without them (sway, wlroots, many
      -- others) it selects X11/XWayland, giving a scale-1 window on a
      -- scale-2 (or fractional) output that the compositor upscales, so
      -- text looks blurred. Native Wayland with
      -- SDL_WINDOW_HIGH_PIXEL_DENSITY rasterizes at the real output scale.
      -- X11 follows in the list, for an SDL that cannot start Wayland (built
      -- without it, or the compositor out of reach) where XWayland runs: a
      -- driver named alone is the only one SDL tries. An explicit
      -- SDL_VIDEO_DRIVER wins, and pure X11 sessions are left alone.
      wayland <- lookupEnv "WAYLAND_DISPLAY"
      driver <- lookupEnv "SDL_VIDEO_DRIVER"
      when (isJust wayland && isNothing driver) $
        setSdlHint sDL_HINT_VIDEO_DRIVER "wayland,x11"

-- | 'withSdl' for measurements: a hidden 800x600 window, bundled fonts,
-- scale 1, continuous drawing, and no vsync. The window is hidden only: on
-- Windows it must not be resizable as well.
withSdlBench :: Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdlBench =
  withSdlWindow
    (setSdlHint sDL_HINT_ASSERT "always_ignore" >> setSdlHint sDL_HINT_RENDER_VSYNC "0")
    SDL.SDL_WINDOW_HIDDEN
    defaultSdlOptions
      { sdlWindowSettings = defaultWindowSettings {wsTitle = "nano-ui-bench", wsSize = Size 800 600}
      , sdlAppVsync = False
      , sdlAppContinuous = True
      , sdlAppFont = DefaultFont
      , sdlAppMonoFont = DefaultFont
      }

-- | Which SDL render driver a session asks for.
data RenderDriver
  = RenderDriverAuto
  -- ^ nano-ui picks: SDL 3.4's @"gpu"@ driver (Vulkan or Metal, enabling
  -- SPIR-V and MSL shaders with zero setup) when available, falling back to
  -- GL on Windows and SDL's own order elsewhere if GPU creation fails or on
  -- SDL 3.2.
  | RenderDriverGpu
  -- ^ Require SDL 3.4's @"gpu"@ renderer with SPIR-V and MSL shaders enabled.
  | RenderDriverSdlDefault
  -- ^ Leave SDL's own preference order alone.
  | RenderDriverNamed !BS.ByteString
  -- ^ An explicit driver name, e.g. @"d3d11"@ or @"opengl"@.
  deriving (Eq, Show)

foreign import ccall safe "nano_ui_create_window_and_gpu_renderer"
  c_createWindowAndGpuRenderer ::
    CString -> CInt -> CInt -> Word64 -> Ptr (Ptr SDL_Window) -> Ptr (Ptr SDL_Renderer) -> IO CInt

foreign import ccall unsafe "nano_ui_renderer_has_gpu"
  c_rendererHasGpu :: Ptr SDL_Renderer -> IO CInt

setSdlHint :: BS.ByteString -> BS.ByteString -> IO ()
setSdlHint name value =
  BS.useAsCString name $ \cname ->
    BS.useAsCString value $ \cvalue ->
      void $ setHint (PtrConst.unsafeFromPtr cname) (PtrConst.unsafeFromPtr cvalue)

-- | Create the window and renderer under the render driver the options ask
-- for, unless @SDL_RENDER_DRIVER@ in the environment names one.
--
-- 'RenderDriverAuto' asks for GL on Windows. Windows' modal size loop hands
-- the app one step at a time and cannot take the next until the app returns.
-- SDL's D3D11 renderer presents through a two-buffer flip-model swap chain,
-- so a present blocks until a back buffer comes free -- about a refresh --
-- even with vsync off. A drag presents a newly sized swap chain every step,
-- so those stalls land in the size loop and the window (and the pointer with
-- it) judders; the bigger the window, the worse. The GL renderer does not
-- block that way. Measured on a 120Hz display: present p99 6.4ms under d3d11
-- against a flawless drag under opengl. Benchmarks take the same driver as
-- the apps they stand in for, so their present numbers are numbers a user
-- can see.
--
-- That guess is dropped again if the creation fails under it, and the
-- creation tried once more. A Windows machine whose GL will not create a
-- context -- a remote desktop session, a VM on the basic display adapter --
-- then opens on whatever SDL can give rather than failing to start. A driver
-- the caller chose is not retried: its failure is theirs to see, and a
-- creation that failed for some other reason should report that reason once.
withRenderDriver :: RenderDriver -> IO a -> IO a
withRenderDriver driver create = do
  fromEnv <- isJust <$> lookupEnv "SDL_RENDER_DRIVER"
  case driver of
    _ | fromEnv -> create
    RenderDriverNamed name -> setSdlHint sDL_HINT_RENDER_DRIVER name >> create
    RenderDriverAuto | os == "mingw32" -> do
      setSdlHint sDL_HINT_RENDER_DRIVER "opengl"
      create `catch` \(_ :: IOException) -> do
        void $ BS.useAsCString sDL_HINT_RENDER_DRIVER (resetHint . PtrConst.unsafeFromPtr)
        create
    _ -> create

-- | Open a session: set the hints, then open a window with the flags.
withSdlWindow :: IO () -> SDL_WindowFlags -> SdlOptions -> Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdlWindow hints flags opts ctx act =
  -- SDL's GL renderer -- what 'RenderDriverAuto' asks for on Windows --
  -- keeps its context current on the OS thread that created it, and the
  -- same thread pumps the window's messages, so the session runs on one.
  inBoundThread $ withTtf $ do
    -- The font directories are walked on a thread of their own from here,
    -- so the first character no loaded font has does not wait for the walk
    -- inside a frame.
    void startFontIndex
    hints
    fontSource <- resolveNanoUIFont (sdlAppFont opts)
    monoSource <- resolveNanoUIFont (sdlAppMonoFont opts)
    Acquire.with (startSdl >> startSdlWindow opts flags ctx fontSource monoSource Nothing) $ \(ctx', env) ->
      withBackendSession ctx' SSdl env (act ctx' env)

-- | An SDL session stores its 'SdlEnv' on the context so view-side operations
-- can look it up without taking an explicit handle.
type instance BackendSession 'Sdl = SdlEnv

-- | Look up the 'SdlEnv' of the SDL session driving this view, for IO-level
-- SDL operations ('NanoUI.Sdl.Internal.Dialog.openFileDialog' and similar),
-- which run on the display thread like the view. Returns 'Nothing' when the
-- context is not driven by an SDL session (such as in headless tests).
askSdlEnv :: NanoUI (Maybe SdlEnv)
askSdlEnv = askBackendSession SSdl

-- | Run with the open session's environment, or answer @outside@ when no
-- session is open.
withSdlEnv :: a -> (SdlEnv -> IO a) -> NanoUI a
withSdlEnv outside k = askSdlEnv >>= maybe (pure outside) (liftIO . k)

acquireWindowAndRenderer ::
  SdlOptions -> SDL_WindowFlags -> Acquire (Ptr SDL_Window, Ptr SDL_Renderer)
acquireWindowAndRenderer opts flags =
  mkAcquire
    ( do
        fromEnv <- isJust <$> lookupEnv "SDL_RENDER_DRIVER"
        let createStandard =
              withRenderDriver (sdlRenderDriver opts) $
                TextForeign.withCString (wsTitle settings) $ \titlePtr -> do
                  (ok, win, ren) <-
                    outPair (createWindowAndRendererSafe (PtrConst.unsafeFromPtr titlePtr) (round w) (round h) flags)
                  unless ok $ sdlFail "SDL_CreateWindowAndRenderer failed"
                  pure (win, ren)
            tryGpu =
              TextForeign.withCString (wsTitle settings) $ \titlePtr -> do
                (ok, win, ren) <-
                  outPair (c_createWindowAndGpuRenderer titlePtr (round w) (round h) (fromIntegral flags))
                pure (if ok /= 0 then Just (win, ren) else Nothing)
        case sdlRenderDriver opts of
          _ | fromEnv -> createStandard
          RenderDriverGpu ->
            tryGpu >>= maybe (sdlFail "SDL 3.4 GPU renderer creation failed") pure
          RenderDriverAuto ->
            tryGpu >>= maybe createStandard pure
          _ -> createStandard
    )
    ( \(win, ren) -> do
        void $ setRenderScale ren 1 1
        destroyRendererSafe ren
        destroyWindowSafe win
    )
  where
    settings = sdlWindowSettings opts
    Size w h = wsSize settings

-- | Start SDL's video subsystem and register the wake event, for the
-- session's windows.
startSdl :: Acquire ()
startSdl = do
  mkAcquire
    (unlessM (initSafe (SDL_InitFlags (fromIntegral sDL_INIT_VIDEO))) (sdlFail "SDL_Init(SDL_INIT_VIDEO) failed"))
    (const quitSafe)
  liftIO $ unlessM initRefreshEvent (sdlFail "SDL_RegisterEvents failed for refresh wake")

-- | Open a window, its renderer and what draws into it. A further window
-- shares its session's first window's cursors and its font and UI scale
-- requests, so 'NanoUI.Backend.Sdl.setSdlUiFont' and
-- 'NanoUI.Backend.Sdl.setSdlUiScale' reach every window.
startSdlWindow ::
  SdlOptions -> SDL_WindowFlags -> Context -> FontSource -> FontSource -> Maybe SdlEnv -> Acquire (Context, SdlEnv)
startSdlWindow opts flags ctx fontSource monoSource first = do
  let settings = sdlWindowSettings opts
  -- NANO_FORCE_SCALE: debug override of the pixel density.
  sdlForcedScale <- liftIO $ mfilter (> 0) . (>>= readMaybe) <$> lookupEnv "NANO_FORCE_SCALE"
  -- Before the window, so that it is released after the window is gone: the
  -- window holds the hit test this frees.
  sdlChromeState <- mkAcquire (newChromeState (sdlWindowDecorations opts) (framedWindow flags)) clearChromeState
  (sdlWindow, sdlRenderer) <- acquireWindowAndRenderer opts flags
  density <- liftIO $ queryWindowPixelDensity sdlWindow
  zoom <- liftIO $ resolveZoom sdlWindow (sdlAppUiScale opts)
  -- The requested size is logical, so the window grows with the zoom.
  liftIO $ when (scaleMoved zoom 1) $ zoomWindow sdlWindow (wsSize settings) zoom
  -- After the zoom: SDL sizes a borderless window as though its view were
  -- the whole of it, so the desktop's frame goes on around a view that is
  -- already the size asked for, and the window grows by the frame.
  liftIO $ when (sdlWindowDecorations opts /= DecorationsFull) (applyDecorations sdlWindow (sdlWindowDecorations opts))
  let
    scale = density * zoom
  liftIO $ setDrawSnapScale ctx scale
  liftIO $ do
    hasGpu <- (/= 0) <$> c_rendererHasGpu sdlRenderer
    setShaderCapabilities ctx (ShaderCapabilities hasGpu hasGpu)
  sdlRefreshPeriod <- liftIO $ refreshPeriodOf sdlWindow
  sdlRefreshPeriodRef <- liftIO $ newIORef sdlRefreshPeriod
  sdlPresentProbe <- liftIO $ newIORef (PresentProbe 0 0)
  sdlRendererName <-
    liftIO $ fromMaybe "unknown" <$> (maybePeek TextForeign.peekCString . PtrConst.unsafeToPtr =<< getRendererName sdlRenderer)
  sdlScaleRef <- liftIO $ newIORef scale
  sdlUiScaleRef <- liftIO $ maybe (newIORef (sdlAppUiScale opts)) (pure . sdlUiScaleRef) first
  sdlPointerSeen <- liftIO $ newIORef False
  sdlFontRequestRef <- liftIO $ maybe (newIORef (sdlAppFont opts)) (pure . sdlFontRequestRef) first
  sdlFontAppliedRef <- liftIO $ newIORef =<< maybe (pure (sdlAppFont opts)) (readIORef . sdlFontAppliedRef) first
  glyphAtlas <- mkAcquire (newGlyphAtlas sdlRenderer) destroyGlyphAtlas
  sdlImages <- mkAcquire newImageAtlas destroyImageAtlas
  sdlCursors <- maybe (mkAcquire initCursors destroyCursors) (pure . sdlCursors) first
  sdlDebug <- liftIO newDebugSampler
  sdlDebugPublished <- liftIO (newIORef Nothing)
  sdlFrameTrace <- liftIO $ isJust <$> lookupEnv "NANO_FRAME_TRACE"
  sdlRetain <- mkAcquire (newIORef (Retain nullPtr 0 0 0 0 0)) $ \ref -> do
    tex <- retainTexture <$> readIORef ref
    unless (tex == nullPtr) $ destroyTexture tex
  sdlFontCache <-
    mkAcquire
      (newSdlFontCache fontSource monoSource glyphAtlas (sdlAppFontSize opts) sdlScaleRef)
      destroySdlFontCache
  sdlCachedCtx <-
    liftIO $ newIORef . withSdlClipboard =<< withSdlFontCache sdlFontCache ctx
  let
    sdlVsync = sdlAppVsync opts
    sdlContinuous = sdlAppContinuous opts
  sdlTransparent <-
    liftIO $
      if wsTransparent settings
        then Just <$> transparentBlends sdlRenderer
        else pure Nothing
  liftIO $ unlessM (setRenderScale sdlRenderer 1 1) (sdlFail "SDL_SetRenderScale failed")
  -- Text input runs only while a widget takes text ('syncTextInput'); this
  -- stops it when the session ends.
  mkAcquire
    (void (setRenderVSyncSafe sdlRenderer (fromBool sdlVsync)))
    (const (void (stopTextInputSafe sdlWindow)))
  sdlLastPresented <- liftIO $ newIORef False
  sdlTextInput <- liftIO newTextInputSync
  -- Decided while the window is shown, before the settings apply its limits.
  sdlSizeLimits <- liftIO $ do
    toplevel <- waylandToplevel sdlWindow
    if toplevel /= 0 then Just <$> newIORef (0, 0, 0, 0) else pure Nothing
  sdlSizeLimitsSent <- liftIO $ newIORef False
  sdlBatch <- mkAcquire (newRenderBatch sdlRenderer) destroyRenderBatch
  let
    env = SdlEnv {..}
  ctx' <- liftIO $ readIORef sdlCachedCtx
  -- Before the wake action, so the first frame has the right theme without
  -- a wake.
  liftIO $ setSystemAppearance ctx' =<< querySystemAppearance
  -- Background hook jobs are cancelled with the session, before SDL quits,
  -- including when a host drives frames itself inside 'withSdl'. Ending it
  -- reads the live context: holding this one until then would keep the
  -- opening fonts' caches alive after a reload.
  mkAcquire (setWakeLoopChecked ctx' tryPushRefreshEvent) $ \_ -> do
    live <- readIORef sdlCachedCtx
    closeWindowHost live `finally` cancelTasks live
  -- The remaining settings go through the host, as from a view. This runs
  -- after the decorations (size limits account for the frame) and after the
  -- zoom (which centres the enlarged window).
  liftIO $ installWindowHost ctx' settings (windowHostFor sdlWindow (windowZoom env) sdlSizeLimits)
  liftIO $ reportWindowState ctx' =<< queryWindowState sdlWindow scale
  pure (ctx', env)

-- | Open a further window of a session, for a view's
-- 'NanoUI.nativeWindow': a context of its own with the declaring view's
-- theme and the session's images, and a window with the settings, the full
-- desktop frame, no vsync (the session's first window paces the loop), and
-- the session's fonts and UI scale. Answers the context, the environment,
-- and the action that closes the window, which also cancels the view's
-- background jobs. Call on the display thread.
openChildWindow :: SdlOptions -> SdlEnv -> Context -> WindowSettings -> IO (Context, SdlEnv, IO ())
openChildWindow opts first parent settings = do
  base <- newPixelContext
  themeFor <- readIORef (ctxThemeFor parent)
  ctx <- maybe (withTheme base =<< readIORef (ctxTheme parent)) (\pick -> base <$ followSystemTheme base pick) themeFor
  registerAppImages opts ctx
  fontSource <- sdlFontCacheSource (sdlFontCache first)
  uiScale <- readIORef (sdlUiScaleRef first)
  let childOpts =
        opts
          { sdlWindowSettings = settings
          , sdlWindowDecorations = DecorationsFull
          , sdlWindowAlwaysOnTop = False
          , sdlAppVsync = False
          , sdlAppContinuous = False
          , sdlAppUiScale = uiScale
          }
      monoSource = sdlFontCacheMonoSource (sdlFontCache first)
  -- The window's resources outlive this call: closing the state frees them.
  st <- createInternalState
  (_, (ctx', env)) <-
    runInternalState (allocateAcquire (startSdlWindow childOpts (windowFlags childOpts) ctx fontSource monoSource (Just first))) st
      `onException` closeInternalState st
  setBackendSession ctx' SSdl env
  pure (ctx', env, closeInternalState st)

-- | Register 'sdlAppImages' on a context.
registerAppImages :: SdlOptions -> Context -> IO ()
registerAppImages opts ctx =
  forM_ (sdlAppImages opts) $ \(RgbaImage image w h pixels) ->
    unlessM (registerImage ctx image w h pixels) (fail "registerImage failed")

-- | The blend modes a transparent window draws and presents with.
--
-- A frame can paint the window colour several times (the backdrop, then a
-- page-sized scroller), and ordinary blending accumulates translucent
-- alpha. Here colour blends normally but alpha takes the maximum, so the
-- window colour over itself is unchanged and the retained frame holds
-- straight alpha for screenshots. Presenting premultiplies, as compositors
-- expect.
--
-- SDL's OpenGL renderer uses one operation for colour and alpha, so it
-- cannot take the maximum. It weights the new alpha by itself instead:
-- the window colour over itself is still unchanged, but a translucent pixel
-- over a more opaque one ends slightly more transparent than either. The
-- software renderer supports neither and blends as for an opaque window,
-- so overlapping alpha accumulates.
transparentBlends :: Ptr SDL_Renderer -> IO (SDL_BlendMode, SDL_BlendMode)
transparentBlends ren = do
  let straightColour =
        composeCustomBlendMode Blend.SDL_BLENDFACTOR_SRC_ALPHA Blend.SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA Blend.SDL_BLENDOPERATION_ADD
      firstSupported = \case
        [] -> pure Nothing
        mode : rest -> setRenderDrawBlendMode ren mode >>= \ok -> if ok then pure (Just mode) else firstSupported rest
  keepLarger <- straightColour Blend.SDL_BLENDFACTOR_ONE Blend.SDL_BLENDFACTOR_ONE Blend.SDL_BLENDOPERATION_MAXIMUM
  selfWeighted <- straightColour Blend.SDL_BLENDFACTOR_SRC_ALPHA Blend.SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA Blend.SDL_BLENDOPERATION_ADD
  present <-
    composeCustomBlendMode
      Blend.SDL_BLENDFACTOR_SRC_ALPHA
      Blend.SDL_BLENDFACTOR_ZERO
      Blend.SDL_BLENDOPERATION_ADD
      Blend.SDL_BLENDFACTOR_ONE
      Blend.SDL_BLENDFACTOR_ZERO
      Blend.SDL_BLENDOPERATION_ADD
  maybe (Blend.SDL_BLENDMODE_BLEND, Blend.SDL_BLENDMODE_NONE) (,present) <$> firstSupported [keepLarger, selfWeighted]

-- | Resolve a font request. A search falls back to bundled Inter when no
-- family matches; an explicit file path is passed through, and loading it can
-- still fail later.
resolveNanoUIFont :: NanoUIFont -> IO FontSource
resolveNanoUIFont = \case
  DefaultFont -> pure embeddedFontSource
  FontFilePath path -> pure (FontFromPath path)
  FontSearch names -> maybe embeddedFontSource FontFromPath <$> searchFonts names

-- | Route the context's clipboard through SDL (UTF-8 text both ways).
withSdlClipboard :: Context -> Context
withSdlClipboard ctx = withClipboard ctx readClipboard writeClipboard
  where
    -- Both are safe calls. Taking the clipboard on Windows sends the window
    -- WM_DESTROYCLIPBOARD and clipboard-update messages, and on X11 SDL pumps
    -- events while the clipboard's owner answers: either can run this
    -- program's resize and hit-test callbacks.
    writeClipboard txt = TextForeign.withCString txt (setClipboardTextSafe . PtrConst.unsafeFromPtr)
    -- Another program's text need not be valid UTF-8.
    readClipboard = do
      ptr <- getClipboardTextSafe
      -- SDL_free takes a null pointer too.
      txt <- maybePeek (fmap TE.decodeUtf8Lenient . BS.packCString) ptr `finally` free (castPtr ptr)
      pure (mfilter (not . T.null) txt)

