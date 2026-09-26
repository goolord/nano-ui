{-# LANGUAGE RecordWildCards #-}

-- | SDL session resources, window options, font/display synchronisation, and screenshots.
module NanoUI.Sdl.Internal.Window
  ( RgbaImage (..)
  , SdlEnv (..)
  , Retain (..)
  , SdlOptions (..)
  , RenderDriver (..)
  , defaultSdlOptions
  , withSdl
  , withSdlBench
  , syncDisplay
  , windowZoom
  , saveScreenshot
  , captureScreenshot
  , captureFrame
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (IOException, catch)
import Control.Monad (mfilter, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire (Acquire, mkAcquire)
import Data.Acquire qualified as Acquire
import Data.Bits ((.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BSI
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Environment (lookupEnv)
import System.Info (os)
import Text.Read (readMaybe)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as TextForeign
import Foreign.C.String (withCString)
import Foreign.Marshal.Utils (copyBytes, maybePeek, with)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import NanoUI (Appearance, Input (..), RgbaImage (..), RgbaPixels, Screenshot (..), Size (..), Theme, V2 (..), WindowMode (..), WindowSettings (..), defaultWindowSettings, rgbaPixels)
import NanoUI.Backend (cancelTasks, installWindowHost, reportWindowState, setSystemAppearance, setWakeLoop)
import NanoUI.Internal.Context (Context (..), setDrawSnapScale)
import NanoUI.Testing (clearMeasureCache, damageFull, markDirty, setHost, withClipboard)
import NanoUI.Sdl.Internal.Display
import NanoUI.Sdl.Internal.Chrome.Types (ChromeState, clearChromeState, newChromeState)
import NanoUI.Sdl.Internal.Frame (WindowDecorations (..), applyDecorations)
import NanoUI.Sdl.Internal.Cursor (SdlCursors (..), destroyCursors, initCursors)
import NanoUI.Sdl.Internal.Font
import NanoUI.Sdl.Internal.Font.Search (searchFonts)
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont (..))
import NanoUI.Internal.Debug (DebugSamplerRef, newDebugSampler)
import NanoUI.Sdl.Internal.Image (ImageAtlas, destroyImageAtlas, newImageAtlas)
import NanoUI.Sdl.Internal.Input (TextInputSync, newTextInputSync)
import NanoUI.Sdl.Internal.Render (RenderBatch, destroyRenderBatch, newRenderBatch)
import NanoUI.Sdl.Internal.WindowOptions
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Hints
  ( sDL_HINT_ASSERT
  , sDL_HINT_IME_IMPLEMENTED_UI
  , sDL_HINT_RENDER_DRIVER
  , sDL_HINT_RENDER_VSYNC
  , sDL_HINT_VIDEO_DRIVER
  )
import SDL3.Sys.Bindgen.Pixels qualified as Pixels
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Surface (SDL_Surface)
import SDL3.Sys.Bindgen.Surface qualified as Surface
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window, SDL_WindowFlags (..))
import SDL3.Sys.Bindgen.Init (SDL_InitFlags (..), sDL_INIT_VIDEO)
import SDL3.Sys.Clipboard (getClipboardText, setClipboardText)
import SDL3.Sys.Hints (resetHint, setHint)
import SDL3.Sys.Blendmode (SDL_BlendMode, composeCustomBlendMode)
import SDL3.Sys.Blendmode qualified as Blend
import SDL3.Sys.Init (initSafe, quitSafe)
import SDL3.Sys.Keyboard (startTextInputSafe, stopTextInputSafe)
import SDL3.Sys.Render
  ( createWindowAndRendererSafe
  , destroyRendererSafe
  , destroyTexture
  , getRendererName
  , renderReadPixels
  , setRenderDrawBlendMode
  , setRenderScale
  , setRenderTarget
  , setRenderVSync
  )
import SDL3.Sys.Stdinc (free)
import SDL3.Sys.Surface (convertSurface, destroySurface, saveBMP)
import SDL3.Sys.Video (destroyWindowSafe, getWindowDisplayScale)

-- | Application-owned SDL settings.
data SdlOptions = SdlOptions
  { sdlWindowSettings :: !WindowSettings
  -- ^ The window: its title, size, position, size limits, icon, mode,
  -- transparency and opacity, and whether it closes by itself (default:
  -- 'defaultWindowSettings'). Sizes are in layout units, converted at the
  -- zoom the window opens at: a later 'NanoUI.Backend.Sdl.setSdlUiScale'
  -- leaves them where they were. A 'WindowPositionDefault' window opens
  -- where the desktop puts it, or centred when 'sdlAppUiScale' grows it. A
  -- transparent window ('wsTransparent') repaints in full when anything in
  -- it changes; where translucent colours overlap, the alpha depends on the
  -- render driver: most keep the more opaque one, SDL's OpenGL renderer
  -- weighs the colour drawn by its own alpha, and the software renderer adds
  -- them up.
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
  -- ^ Continuous unthrottled rendering without waiting for events (default: 'False').
  , sdlExplainLayout :: !Bool
  -- ^ Start with the layout overlay on, which outlines every layout node
  -- (default: 'False'). A view turns it on and off with @explainLayout@.
  , sdlAppFont :: !NanoUIFont
  -- ^ UI font (default: installed sans-serif search, falling back to bundled Inter).
  , sdlAppMonoFont :: !NanoUIFont
  -- ^ Monospace font (default: installed monospace search, falling back to Inter).
  , sdlAppFontSize :: !Float
  -- ^ Base font size in points (default: 16).
  , sdlAppTheme :: !(Maybe Theme)
  -- ^ Initial UI theme override (default: 'Nothing').
  , sdlAppThemeFor :: !(Maybe (Maybe Appearance -> Theme))
  -- ^ The theme for the desktop's light or dark setting, followed as it
  -- changes, such as @'NanoUI.lightDark' light dark@ (default: 'Nothing').
  -- Set, it replaces 'sdlAppTheme'; see 'NanoUI.followSystemTheme'.
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

-- | SDL_WINDOW_HIGH_PIXEL_DENSITY (0x2000): without it the window's surface
-- gets scale 1.0 even on a 2x / HiDPI output, so the compositor upscales the
-- whole window (blurry "looks upscaled"). With it, SDL_GetWindowPixelDensity
-- returns the real output scale where window coordinates are points (macOS,
-- Wayland), the window keeps its logical size, and the pixel buffer (and
-- therefore the retain texture, glyph atlas, and fonts) rasterizes at the
-- native pixel density. On Windows window coordinates are already pixels, so
-- the density stays 1 whatever the desktop scaling.
windowFlags :: SdlOptions -> SDL_WindowFlags
windowFlags opts =
  SDL_WindowFlags $
    0x0000000000002000
      .|. flag (wsResizable settings) 0x0000000000000020
      .|. flag (wsMode settings == Fullscreen) 0x0000000000000001
      .|. flag (sdlWindowDecorations opts /= DecorationsFull) 0x0000000000000010
      .|. flag (sdlWindowAlwaysOnTop opts) 0x0000000000010000
      .|. flag (wsMode settings == Hidden) 0x0000000000000008
      .|. flag (wsTransparent settings) 0x0000000040000000
  where
    settings = sdlWindowSettings opts
    flag on bit = if on then bit else 0

scaleEpsilon :: Float
scaleEpsilon = 0.001

-- | Native resources owned by a 'withSdl' callback. Window, renderer, and font
-- handles must stay on the display thread and must not outlive the callback.
-- Use the supplied context, which contains this environment as host data.
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
  -- ^ The requested UI scale; see 'sdlAppUiScale'.
  , sdlImages :: ImageAtlas
  , sdlCursors :: SdlCursors
  , sdlDebug :: DebugSamplerRef
  , sdlFrameTrace :: !Bool
  -- ^ Whether @NANO_FRAME_TRACE@ was set when the session opened.
  , sdlRetain :: IORef Retain
  , sdlLastPresented :: IORef Bool
  , sdlVsync :: !Bool
  , sdlRefreshPeriod :: !Double
  , sdlContinuous :: !Bool
  , sdlTransparent :: !(Maybe (SDL_BlendMode, SDL_BlendMode))
  -- ^ For a transparent window ('wsTransparent'), the blend modes
  -- frames draw with and the retained frame goes to the window with;
  -- 'Nothing' for an opaque one. See 'transparentBlends'.
  , sdlCachedCtx :: !(IORef Context)
  , sdlFontCache :: !SdlFontCache
  , sdlChromeState :: !ChromeState
  -- ^ What a borderless window's own title bar is for; see
  -- "NanoUI.Sdl.Internal.Chrome".
  , sdlTextInput :: !TextInputSync
  -- ^ What SDL's text input last heard of the focused field, which every
  -- frame drawn brings up to date.
  }

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
  let scaleChanged = abs (scale - oldScale) > scaleEpsilon
  when scaleChanged $ do
    -- Presents leave the renderer at 1:1 pixels; re-assert it only when the
    -- pixel density moves.
    setRenderScale (sdlRenderer env) 1 1 >>= (`unless` fail "SDL_SetRenderScale failed")
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
    reloadSdlFontCache (sdlFontCache env) source
    writeIORef (sdlCachedCtx env) . withSdlClipboard =<< withSdlFontCache (sdlFontCache env) ctx
    clearMeasureCache ctx
    -- Glyphs change under rects and texts that may not: repaint everything.
    damageFull ctx
    markDirty ctx
  -- The desktop's light or dark setting. SDL updates it before queueing
  -- its theme event, which wakes the loop for this; reading it is free.
  setSystemAppearance ctx =<< querySystemAppearance
  reportWindowState ctx =<< queryWindowState (sdlWindow env) scale
  queried <- queryWindowLogicalSize (sdlWindow env)
  let winSize = case (queried, inputWindowSize inp) of
        (Size 0 0, Size 0 0) -> wsSize defaultWindowSettings
        (Size 0 0, s) -> s
        (Size sw sh, _) -> Size (sw / zoom) (sh / zoom)
  V2 mx my <- queryMouseWindowPos
  let mouse = V2 (mx / zoom) (my / zoom)
  ctxMeasured <- readIORef (sdlCachedCtx env)
  pure (ctxMeasured, inp {inputWindowSize = winSize, inputMousePos = mouse})

-- | Open native resources around an action and release them on exit, including
-- exceptions. Supplies an SDL-equipped context. This does not run an event
-- loop or apply the high-level runner's initial theme/image registration.
withSdl :: SdlOptions -> Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdl = withSdlWindow False

-- | 'withSdl' for measurements: a hidden 800x600 window, bundled fonts,
-- scale 1, continuous drawing, and no vsync or text-input setup.
withSdlBench :: Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdlBench =
  withSdlWindow
    True
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
  -- ^ nano-ui picks: GL on Windows, for the presentation stall a live resize
  -- hits under D3D11, and SDL's own order elsewhere, where it picks well.
  | RenderDriverSdlDefault
  -- ^ Leave SDL's own preference order alone.
  | RenderDriverNamed !BS.ByteString
  -- ^ An explicit driver name, e.g. @"d3d11"@ or @"opengl"@.
  deriving (Eq, Show)

setSdlHint :: BS.ByteString -> BS.ByteString -> IO ()
setSdlHint name value =
  BS.useAsCString name $ \cname ->
    BS.useAsCString value $ \cvalue ->
      void $ setHint (PtrConst.unsafeFromPtr cname) (PtrConst.unsafeFromPtr cvalue)

-- | Run a window/renderer creation under a render driver nano-ui guessed at,
-- and if it fails, drop the hint and try once more. A Windows machine whose
-- GL will not create a context -- a remote desktop session, a VM on the basic
-- display adapter -- then opens on whatever SDL can give rather than failing
-- to start. Pass 'False' when the driver is the caller's own choice or when
-- no hint was set: their failure is theirs to see, and a creation that failed
-- for some other reason should report that reason once.
retryWithoutRenderDriver ::
  Bool -> IO (Ptr SDL_Window, Ptr SDL_Renderer) -> IO (Ptr SDL_Window, Ptr SDL_Renderer)
retryWithoutRenderDriver False create = create
retryWithoutRenderDriver True create =
  create `catch` \(_ :: IOException) -> do
    void $ BS.useAsCString sDL_HINT_RENDER_DRIVER (resetHint . PtrConst.unsafeFromPtr)
    create

-- | Open a session. A bench session is a hidden window with bench hints and
-- no vsync setup or text input.
withSdlWindow :: Bool -> SdlOptions -> Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdlWindow bench opts ctx act =
  inBoundThread $ withTtf $ do
    if bench
      then do
        setSdlHint sDL_HINT_ASSERT "always_ignore"
        setSdlHint sDL_HINT_RENDER_VSYNC "0"
      else do
        setSdlHint sDL_HINT_RENDER_VSYNC (if sdlAppVsync opts then "1" else "0")
        -- The text fields draw what an input method is composing at their
        -- caret, so SDL sends it (SDL_EVENT_TEXT_EDITING) rather than the
        -- input method drawing it over the window. Its candidate list stays
        -- the input method's own, placed by SDL_SetTextInputArea.
        setSdlHint sDL_HINT_IME_IMPLEMENTED_UI "composition"
        -- SDL3 only auto-picks Wayland when the compositor has the fifo-v1 /
        -- commit-timing-v1 protocols. Without them (sway, wlroots, many
        -- others) it selects X11/XWayland, giving a scale-1 window on a
        -- scale-2 (or fractional) output that the compositor upscales, so
        -- text looks blurred. Native Wayland with
        -- SDL_WINDOW_HIGH_PIXEL_DENSITY rasterizes at the real output scale.
        -- An explicit SDL_VIDEO_DRIVER wins, and pure X11 sessions are left
        -- alone.
        wayland <- lookupEnv "WAYLAND_DISPLAY"
        driver <- lookupEnv "SDL_VIDEO_DRIVER"
        when (isJust wayland && isNothing driver) $
          setSdlHint sDL_HINT_VIDEO_DRIVER "wayland"
    -- Windows' modal size loop hands the app one step at a time and cannot
    -- take the next until the app returns. SDL's D3D11 renderer presents
    -- through a two-buffer flip-model swap chain, so a present blocks until a
    -- back buffer comes free -- about a refresh -- even with vsync off. A drag
    -- presents a newly sized swap chain every step, so those stalls land in
    -- the size loop and the window (and the pointer with it) judders; the
    -- bigger the window, the worse. The GL renderer does not block that way.
    -- Measured on a 120Hz display: present p99 6.4ms under d3d11 against a
    -- flawless drag under opengl. Benchmarks take the same driver as the apps
    -- they stand in for, so their present numbers are numbers a user can see.
    renderDriver <- lookupEnv "SDL_RENDER_DRIVER"
    let
      -- The @SDL_RENDER_DRIVER@ hint to set, if any.
      requested = case sdlRenderDriver opts of
        _ | isJust renderDriver -> Nothing
        RenderDriverNamed name -> Just name
        RenderDriverAuto | os == "mingw32" -> Just "opengl"
        _ -> Nothing
      -- Only a driver nano-ui chose for the caller is worth dropping again.
      guessed = isJust requested && sdlRenderDriver opts == RenderDriverAuto
    for_ requested (setSdlHint sDL_HINT_RENDER_DRIVER)
    fontSource <- resolveNanoUIFont (sdlAppFont opts)
    monoSource <- resolveNanoUIFont (sdlAppMonoFont opts)
    Acquire.with (startSdlWindow bench opts ctx guessed fontSource monoSource) (uncurry act)
  where
    -- SDL's GL renderer -- what 'RenderDriverAuto' asks for on Windows --
    -- keeps its context current on the OS thread that created it, and the
    -- same thread pumps the window's messages. An unbound caller can be moved
    -- between OS threads across a safe foreign call, so pin the session to one.
    inBoundThread a = if rtsSupportsBoundThreads then runInBoundThread a else a

startSdlWindow ::
  Bool -> SdlOptions -> Context -> Bool -> FontSource -> FontSource -> Acquire (Context, SdlEnv)
startSdlWindow bench opts ctx guessedDriver fontSource monoSource = do
  mkAcquire
    (initSafe (SDL_InitFlags (fromIntegral sDL_INIT_VIDEO)) >>= (`unless` fail "SDL_Init(SDL_INIT_VIDEO) failed"))
    (const quitSafe)
  liftIO $ initRefreshEvent >>= (`unless` fail "SDL_RegisterEvents failed for refresh wake")
  let
    settings = sdlWindowSettings opts
    Size w h = wsSize settings
  -- NANO_FORCE_SCALE: debug override of the pixel density.
  sdlForcedScale <- liftIO $ mfilter (> 0) . (>>= readMaybe) <$> lookupEnv "NANO_FORCE_SCALE"
  -- Before the window, so that it is released after the window is gone: the
  -- window holds the hit test this frees.
  sdlChromeState <- mkAcquire newChromeState clearChromeState
  (sdlWindow, sdlRenderer) <-
    mkAcquire
      ( retryWithoutRenderDriver guessedDriver $
          TextForeign.withCString (wsTitle settings) $ \titlePtr -> do
            -- A bench window is hidden only: on Windows it must not be
            -- resizable as well.
            let flags = if bench then SDL_WindowFlags 0x0000000000000008 else windowFlags opts
            (ok, win, ren) <-
              outPair (createWindowAndRendererSafe (PtrConst.unsafeFromPtr titlePtr) (round w) (round h) flags)
            unless ok $ fail "SDL_CreateWindowAndRenderer failed"
            pure (win, ren)
      )
      ( \(win, ren) -> do
          void $ setRenderScale ren 1 1
          destroyRendererSafe ren
          destroyWindowSafe win
      )
  density <- liftIO $ queryWindowPixelDensity sdlWindow
  zoom <- liftIO $ resolveZoom sdlWindow (sdlAppUiScale opts)
  -- The requested size is logical, so the window grows with the zoom.
  liftIO $ when (abs (zoom - 1) > scaleEpsilon) $ zoomWindow sdlWindow (wsSize settings) zoom
  -- After the zoom: SDL sizes a borderless window as though its view were
  -- the whole of it, so the desktop's frame goes on around a view that is
  -- already the size asked for, and the window grows by the frame.
  liftIO $ when (sdlWindowDecorations opts /= DecorationsFull) (applyDecorations sdlWindow (sdlWindowDecorations opts))
  let
    scale = density * zoom
  liftIO $ setDrawSnapScale ctx scale
  refreshHz <- liftIO $ queryWindowRefreshHz sdlWindow
  sdlRendererName <-
    liftIO $ fromMaybe "unknown" <$> (maybePeek TextForeign.peekCString . PtrConst.unsafeToPtr =<< getRendererName sdlRenderer)
  sdlScaleRef <- liftIO $ newIORef scale
  sdlUiScaleRef <- liftIO $ newIORef (sdlAppUiScale opts)
  sdlFontRequestRef <- liftIO $ newIORef (sdlAppFont opts)
  sdlFontAppliedRef <- liftIO $ newIORef (sdlAppFont opts)
  glyphAtlas <- mkAcquire (newGlyphAtlas sdlRenderer) destroyGlyphAtlas
  sdlImages <- mkAcquire newImageAtlas destroyImageAtlas
  sdlCursors <- mkAcquire initCursors destroyCursors
  sdlDebug <- liftIO newDebugSampler
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
    sdlRefreshPeriod = if refreshHz > 0 then 1 / fromIntegral refreshHz else 1 / 60
    sdlVsync = sdlAppVsync opts
    sdlContinuous = sdlAppContinuous opts
  sdlTransparent <-
    liftIO $
      if wsTransparent settings && not bench
        then Just <$> transparentBlends sdlRenderer
        else pure Nothing
  liftIO $ setRenderScale sdlRenderer 1 1 >>= (`unless` fail "SDL_SetRenderScale failed")
  unless bench $
    mkAcquire
      ( void (setRenderVSync sdlRenderer (if sdlVsync then 1 else 0))
          >> void (startTextInputSafe sdlWindow)
      )
      (const (void (stopTextInputSafe sdlWindow)))
  sdlLastPresented <- liftIO $ newIORef False
  sdlTextInput <- liftIO newTextInputSync
  sdlBatch <- mkAcquire (newRenderBatch sdlRenderer) destroyRenderBatch
  let
    env = SdlEnv {..}
  ctx' <- liftIO $ readIORef sdlCachedCtx
  -- Before the wake action: the first frame is drawn in the right theme and
  -- needs no wake for it.
  liftIO $ setSystemAppearance ctx' =<< querySystemAppearance
  liftIO $ setHost ctx' env
  -- The jobs the view's background hooks started end with the session and
  -- before SDL does, also where a host runs frames itself inside 'withSdl'.
  mkAcquire (setWakeLoop ctx' pushRefreshEvent) (const (cancelTasks ctx'))
  -- After the decorations, whose frame the size limits leave room for, and
  -- after the zoom, which centres the window it grows: the rest of the
  -- settings, applied through the host as a view would.
  liftIO $ installWindowHost ctx' settings (windowHostFor sdlWindow (windowZoom env))
  liftIO $ reportWindowState ctx' =<< queryWindowState sdlWindow scale
  pure (ctx', env)

-- | The blend modes a transparent window draws with and is presented with.
--
-- A frame paints the window colour more than once in places: the backdrop,
-- then a page-sized scroller over it. Over a translucent colour, ordinary
-- blending adds up the alpha each time. Colour blends as usual here, but
-- alpha keeps the larger of the two, so the window colour painted over
-- itself stays the window colour and the retained frame holds straight
-- alpha, which a screenshot reads. The copy to the window premultiplies it,
-- which is what compositors take.
--
-- SDL's OpenGL renderer blends colour and alpha with one operation, so it
-- cannot keep the larger alpha. It weighs the alpha drawn by itself
-- instead, which keeps the window colour over itself too, and leaves a
-- translucent pixel over a more opaque one a little more transparent than
-- either. The software renderer has neither, and draws as for an opaque
-- window: its alpha adds up where translucent colours overlap.
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
    writeClipboard txt = TextForeign.withCString txt (setClipboardText . PtrConst.unsafeFromPtr)
    readClipboard = do
      ptr <- getClipboardText
      txt <- maybePeek TextForeign.peekCString ptr
      -- SDL_free takes a null pointer too.
      free (castPtr ptr)
      pure (mfilter (not . T.null) txt)

-- | Write the last presented frame to a BMP file. A retained session reads
-- its retained texture, since SDL leaves the window backbuffer undefined
-- after a present; a direct-to-window session reads the backbuffer.
saveScreenshot :: SdlEnv -> FilePath -> IO Bool
saveScreenshot env path = do
  surface <- readFrame env . retainTexture =<< readIORef (sdlRetain env)
  if surface == nullPtr
    then pure False
    else withCString path $ \cpath -> do
      ok <- saveBMP surface (PtrConst.unsafeFromPtr cpath)
      destroySurface surface
      pure ok

-- | The last presented frame, read as 'saveScreenshot' reads it, as a view's
-- 'NanoUI.requestScreenshot' is answered: the window's pixels (its logical
-- size times the display scale), with the alpha the frame was drawn with,
-- which is the theme's window colour's wherever nothing covers it.
-- 'Nothing' when SDL cannot read the frame back.
--
-- A direct-to-window session ('sdlAppContinuous') has a frame to read only
-- between drawing and presenting it: from a view, use
-- 'NanoUI.requestScreenshot', which reads it then.
captureScreenshot :: SdlEnv -> IO (Maybe Screenshot)
captureScreenshot env = do
  scale <- readIORef (sdlScaleRef env)
  fmap (`Screenshot` scale) <$> (captureFrame env . retainTexture =<< readIORef (sdlRetain env))

-- | The pixels of the frame drawn to a target: the retained texture, or null
-- for the window backbuffer, which is only worth reading before the frame
-- is presented.
captureFrame :: SdlEnv -> Ptr SDL_Texture -> IO (Maybe RgbaPixels)
captureFrame env target = do
  surface <- readFrame env target
  if surface == nullPtr
    then pure Nothing
    else do
      rgba <- convertSurface surface Pixels.SDL_PIXELFORMAT_RGBA32
      destroySurface surface
      if rgba == nullPtr
        then pure Nothing
        else do
          Surface.SDL_Surface _ _ sw sh pitch pixels _ _ <- peek rgba
          let w = fromIntegral sw
              h = fromIntegral sh
              rowBytes = w * 4
          bytes <- BSI.create (h * rowBytes) $ \dst ->
            for_ [0 .. h - 1] $ \y ->
              copyBytes (dst `plusPtr` (y * rowBytes)) (castPtr pixels `plusPtr` (y * fromIntegral pitch)) rowBytes
          destroySurface rgba
          pure (rgbaPixels w h bytes)

-- | Read a frame back into a new surface: the used area of the retained
-- texture, or with a null target the window backbuffer. Null on failure.
readFrame :: SdlEnv -> Ptr SDL_Texture -> IO (Ptr SDL_Surface)
readFrame env tex = do
  r <- readIORef (sdlRetain env)
  let ren = sdlRenderer env
  if tex == nullPtr
    then renderReadPixels ren (PtrConst.unsafeFromPtr nullPtr)
    else do
      void $ setRenderTarget ren tex
      s <- with (SDL_Rect 0 0 (fromIntegral (retainW r)) (fromIntegral (retainH r))) $ \rp ->
        renderReadPixels ren (PtrConst.unsafeFromPtr rp)
      void $ setRenderTarget ren nullPtr
      pure s
