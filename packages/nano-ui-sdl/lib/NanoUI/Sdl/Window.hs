-- | SDL session resources, window options, font/display synchronisation, and screenshots.
module NanoUI.Sdl.Window
  ( RgbaImage (..)
  , SdlEnv (..)
  , Retain (..)
  , noRetain
  , SdlOptions (..)
  , RenderDriver (..)
  , defaultSdlOptions
  , withSdl
  , withSdlBench
  , syncDisplay
  , saveScreenshot
  ) where

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (IOException, catch)
import Control.Monad (unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Acquire (Acquire, mkAcquire)
import Data.Acquire qualified as Acquire
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Foldable (for_)
import Data.Maybe (isJust, isNothing)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Environment (lookupEnv)
import System.Info (os)
import Text.Read (readMaybe)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Text.Foreign qualified as TextForeign
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (ImageId, Input (..), Size (..), Theme, V2 (..))
import NanoUI.Context (Context (..), setDrawSnapScale)
import NanoUI.Testing (clearMeasureCache, damageFull, markDirty, setHost, setWakeLoop)
import NanoUI.Sdl.Display
  ( defaultFontSize
  , initRefreshEvent
  , pushRefreshEvent
  , queryMouseWindowPos
  , queryWindowPixelDensity
  , queryWindowLogicalSize
  , queryWindowRefreshHz
  , zoomWindow
  )
import NanoUI.Sdl.Clipboard (withSdlClipboard)
import NanoUI.Sdl.Cursor (SdlCursors (..), destroyCursors, initCursors)
import NanoUI.Sdl.Font
  ( FontSource (..)
  , GlyphAtlas
  , SdlFontCache
  , destroyGlyphAtlas
  , destroySdlFontCache
  , newGlyphAtlas
  , newSdlFontCache
  , reloadSdlFontCache
  , sdlFontCacheSource
  , withSdlFontCache
  , withTtf
  )
import NanoUI.Sdl.Font.Resolve
  ( embeddedFontSource
  , resolveNanoUIFont
  , defaultFontSearch
  , defaultFontSearchMono
  )
import NanoUI.Sdl.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Debug (SdlDebugSampler, newSdlDebugSampler)
import NanoUI.Sdl.Dialog.Types (DialogState (..), clearDialogState, newDialogState)
import NanoUI.Sdl.Image (ImageAtlas, destroyImageAtlas, newImageAtlas)
import NanoUI.Sdl.Render (RenderBatch, destroyRenderBatch, newRenderBatch)
import SDL3.Sys.Bindgen.Rect (SDL_Rect (..))
import SDL3.Sys.Bindgen.Hints
  ( sDL_HINT_ASSERT
  , sDL_HINT_RENDER_DRIVER
  , sDL_HINT_RENDER_VSYNC
  , sDL_HINT_VIDEO_DRIVER
  )
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window, SDL_WindowFlags (..))
import SDL3.Sys.Bindgen.Init (SDL_InitFlags (..), sDL_INIT_VIDEO)
import SDL3.Sys.Hints (resetHint, setHint)
import SDL3.Sys.Init (initSafe, quitSafe)
import SDL3.Sys.Keyboard (startTextInputSafe, stopTextInputSafe)
import SDL3.Sys.Render
  ( createWindowAndRendererSafe
  , destroyRendererSafe
  , destroyTexture
  , getRendererName
  , renderReadPixels
  , setRenderScale
  , setRenderTarget
  , setRenderVSync
  )
import SDL3.Sys.Surface (destroySurface, saveBMP)
import SDL3.Sys.Video (destroyWindowSafe, getWindowDisplayScale)

-- | Initial image: positive pixel width/height and tightly packed RGBA8 bytes,
-- four bytes per pixel in row order. The high-level runners register these
-- assets before the first frame.
data RgbaImage = RgbaImage
  { rgbaImageId :: !ImageId
  , rgbaImageWidth :: !Int
  , rgbaImageHeight :: !Int
  , rgbaImagePixels :: !ByteString
  }

-- | Application-owned SDL settings.
data SdlOptions = SdlOptions
  { sdlWindowTitle :: !Text
  -- ^ Window title (default: @"nano-ui"@).
  , sdlWindowSize :: !Size
  -- ^ Initial window size in logical units (default: 1280x800).
  , sdlWindowResizable :: !Bool
  -- ^ Allow the window to be resized (default: 'True').
  , sdlWindowFullscreen :: !Bool
  -- ^ Open the window in fullscreen mode (default: 'False').
  , sdlWindowBorderless :: !Bool
  -- ^ Create a borderless window (default: 'False').
  , sdlWindowAlwaysOnTop :: !Bool
  -- ^ Keep the window on top of other windows (default: 'False').
  , sdlWindowHidden :: !Bool
  -- ^ Start the window hidden (default: 'False').
  , sdlAppVsync :: !Bool
  -- ^ Enable vertical synchronization (default: 'True').
  , sdlRenderDriver :: !RenderDriver
  -- ^ Which SDL render driver to ask for (default: 'RenderDriverAuto').
  -- An @SDL_RENDER_DRIVER@ in the environment wins over this.
  , sdlAppContinuous :: !Bool
  -- ^ Continuous unthrottled rendering without waiting for events (default: 'False').
  , sdlAppFont :: !NanoUIFont
  -- ^ UI font (default: installed sans-serif search, falling back to bundled Inter).
  , sdlAppMonoFont :: !NanoUIFont
  -- ^ Monospace font (default: installed monospace search, falling back to Inter).
  , sdlAppFontSize :: !Float
  -- ^ Base font size in points (default: 16).
  , sdlAppTheme :: !(Maybe Theme)
  -- ^ Initial UI theme override (default: 'Nothing').
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
    { sdlWindowTitle = "nano-ui"
    , sdlWindowSize = defaultWindowSize
    , sdlWindowResizable = True
    , sdlWindowFullscreen = False
    , sdlWindowBorderless = False
    , sdlWindowAlwaysOnTop = False
    , sdlWindowHidden = False
    , sdlAppVsync = True
    , sdlRenderDriver = RenderDriverAuto
    , sdlAppContinuous = False
    , sdlAppFont = defaultFontSearch
    , sdlAppMonoFont = defaultFontSearchMono
    , sdlAppFontSize = defaultFontSize
    , sdlAppTheme = Nothing
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
      .|. flag sdlWindowResizable 0x0000000000000020
      .|. flag sdlWindowFullscreen 0x0000000000000001
      .|. flag sdlWindowBorderless 0x0000000000000010
      .|. flag sdlWindowAlwaysOnTop 0x0000000000010000
      .|. flag sdlWindowHidden 0x0000000000000008
  where
    flag field bit = if field opts then bit else 0

-- Hidden only. Do not combine with resizable for bench windows on Windows.
sdlWindowHiddenFlag :: SDL_WindowFlags
sdlWindowHiddenFlag = SDL_WindowFlags 0x0000000000000008

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
  , sdlGlyphAtlas :: GlyphAtlas
  , sdlImages :: ImageAtlas
  , sdlCursors :: SdlCursors
  , sdlDebug :: SdlDebugSampler
  , sdlRetain :: IORef Retain
  , sdlLastPresented :: IORef Bool
  , sdlVsync :: !Bool
  , sdlRefreshPeriod :: !Double
  , sdlContinuous :: !Bool
  , sdlCachedCtx :: !(IORef Context)
  , sdlFontCache :: !SdlFontCache
  , sdlDialogState :: !DialogState
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

noRetain :: Retain
noRetain = Retain nullPtr 0 0 0 0 0

defaultWindowSize :: Size
defaultWindowSize = Size 1280 800

-- | The zoom a UI scale setting asks for: the setting itself, or for zero or
-- less the display's content scale beyond the pixel density.
resolveZoom :: Ptr SDL_Window -> Float -> IO Float
resolveZoom win setting
  | setting > 0 = pure setting
  | otherwise = do
      display <- getWindowDisplayScale win
      density <- queryWindowPixelDensity win
      pure (if display > 0 then max 0.25 (display / density) else 1)

-- | Synchronise display scale, requested fonts, and logical input coordinates.
-- Call on the display thread before drawing, then use both returned values;
-- font changes may replace the context's metrics and invalidate cached layout.
syncDisplay :: Context -> SdlEnv -> Input -> IO (Context, Input)
syncDisplay ctx env inp = do
  density <- maybe (queryWindowPixelDensity (sdlWindow env)) pure (sdlForcedScale env)
  zoom <- resolveZoom (sdlWindow env) =<< readIORef (sdlUiScaleRef env)
  let scale = density * zoom
  oldScale <- readIORef (sdlScaleRef env)
  let scaleChanged = abs (scale - oldScale) > scaleEpsilon
  when scaleChanged $ do
    -- Presents leave the renderer at 1:1 pixels; re-assert it only when the
    -- pixel density moves.
    ok <- setRenderScale (sdlRenderer env) 1 1
    unless ok $ fail "SDL_SetRenderScale failed"
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
  queried <- queryWindowLogicalSize (sdlWindow env)
  let unzoom (Size sw sh) = Size (sw / zoom) (sh / zoom)
      winSize =
        case unzoom queried of
          Size 0 0 ->
            case inputWindowSize inp of
              Size 0 0 -> defaultWindowSize
              s -> s
          s -> s
  V2 mx my <- queryMouseWindowPos
  let mouse = V2 (mx / zoom) (my / zoom)
  ctxMeasured <- readIORef (sdlCachedCtx env)
  pure (ctxMeasured, inp {inputWindowSize = winSize, inputMousePos = mouse})

-- | Everything a window session is opened with, besides the context.
data WindowConfig = WindowConfig
  { wcTitle :: !Text
  , wcSize :: !Size
  , wcFlags :: !SDL_WindowFlags
  , wcBench :: !Bool
  -- ^ Hidden benchmark window: bench hints, no vsync setup or text input.
  , wcVsync :: !Bool
  , wcContinuous :: !Bool
  , wcUiFont :: !NanoUIFont
  , wcMonoFont :: !NanoUIFont
  , wcFontSize :: !Float
  , wcUiScale :: !Float
  , wcRenderDriver :: !RenderDriver
  }

-- | Open native resources around an action and release them on exit, including
-- exceptions. Supplies an SDL-equipped context. This does not run an event
-- loop or apply the high-level runner's initial theme/image registration.
withSdl :: SdlOptions -> Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdl opts ctx =
  withSdlWindow
    ctx
    WindowConfig
      { wcTitle = sdlWindowTitle opts
      , wcSize = sdlWindowSize opts
      , wcFlags = windowFlags opts
      , wcBench = False
      , wcVsync = sdlAppVsync opts
      , wcContinuous = sdlAppContinuous opts
      , wcUiFont = sdlAppFont opts
      , wcMonoFont = sdlAppMonoFont opts
      , wcFontSize = sdlAppFontSize opts
      , wcUiScale = sdlAppUiScale opts
      , wcRenderDriver = sdlRenderDriver opts
      }

-- | 'withSdl' for measurements: a hidden 800x600 window, bundled fonts,
-- scale 1, continuous drawing, and no vsync or text-input setup.
withSdlBench :: Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdlBench ctx =
  withSdlWindow
    ctx
    WindowConfig
      { wcTitle = "nano-ui-bench"
      , wcSize = Size 800 600
      , wcFlags = sdlWindowHiddenFlag
      , wcBench = True
      , wcVsync = False
      , wcContinuous = True
      , wcUiFont = DefaultFont
      , wcMonoFont = DefaultFont
      , wcFontSize = defaultFontSize
      , wcUiScale = 1
      , wcRenderDriver = RenderDriverAuto
      }

-- | Which SDL render driver a session asks for.
data RenderDriver
  = RenderDriverAuto
  -- ^ nano-ui picks; see 'preferredRenderDriver'.
  | RenderDriverSdlDefault
  -- ^ Leave SDL's own preference order alone.
  | RenderDriverNamed !BS.ByteString
  -- ^ An explicit driver name, e.g. @"d3d11"@ or @"opengl"@.
  deriving (Eq, Show)

-- | The render driver to ask SDL for, as an @SDL_RENDER_DRIVER@ hint value.
-- 'Nothing' leaves SDL to its own order. The default is 'RenderDriverAuto',
-- which on Windows prefers GL over D3D11 for the presentation stall a live
-- resize hits; elsewhere SDL already picks well.
preferredRenderDriver :: RenderDriver -> Maybe BS.ByteString
preferredRenderDriver = \case
  RenderDriverSdlDefault -> Nothing
  RenderDriverNamed name -> Just name
  RenderDriverAuto
    | os == "mingw32" -> Just "opengl"
    | otherwise -> Nothing

-- | Set an SDL hint by name.
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

withSdlWindow :: Context -> WindowConfig -> (Context -> SdlEnv -> IO a) -> IO a
withSdlWindow ctx cfg act =
  inBoundThread $ withTtf $ do
    if wcBench cfg
      then do
        setSdlHint sDL_HINT_ASSERT "always_ignore"
        setSdlHint sDL_HINT_RENDER_VSYNC "0"
      else do
        setSdlHint sDL_HINT_RENDER_VSYNC (if wcVsync cfg then "1" else "0")
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
      requested
        | isJust renderDriver = Nothing
        | otherwise = preferredRenderDriver (wcRenderDriver cfg)
      -- Only a driver nano-ui chose for the caller is worth dropping again.
      guessed = isJust requested && wcRenderDriver cfg == RenderDriverAuto
    for_ requested (setSdlHint sDL_HINT_RENDER_DRIVER)
    fontSource <- resolveNanoUIFont (wcUiFont cfg)
    monoSource <- resolveNanoUIFont (wcMonoFont cfg)
    Acquire.with (startSdlWindow ctx cfg guessed fontSource monoSource) (uncurry act)
  where
    -- SDL's GL renderer -- what 'RenderDriverAuto' asks for on Windows --
    -- keeps its context current on the OS thread that created it, and the
    -- same thread pumps the window's messages. An unbound caller can be moved
    -- between OS threads across a safe foreign call, so pin the session to one.
    inBoundThread a = if rtsSupportsBoundThreads then runInBoundThread a else a

startSdlWindow ::
  Context -> WindowConfig -> Bool -> FontSource -> FontSource -> Acquire (Context, SdlEnv)
startSdlWindow ctx cfg guessedDriver fontSource monoSource = do
  mkAcquire
    ( do
        videoOk <- initSafe (SDL_InitFlags (fromIntegral sDL_INIT_VIDEO))
        unless videoOk $ fail "SDL_Init(SDL_INIT_VIDEO) failed"
    )
    (const quitSafe)
  liftIO $ do
    refreshOk <- initRefreshEvent
    unless refreshOk $ fail "SDL_RegisterEvents failed for refresh wake"
  let
    Size w h = wcSize cfg
    bench = wcBench cfg
  -- NANO_FORCE_SCALE: debug override of the pixel density.
  forcedEnv <- liftIO $ lookupEnv "NANO_FORCE_SCALE"
  let
    forcedScale = case forcedEnv >>= readMaybe of
      Just s | s > 0 -> Just s
      _ -> Nothing
  (win, ren) <-
    mkAcquire
      ( retryWithoutRenderDriver guessedDriver $
          TextForeign.withCString (wcTitle cfg) $ \titlePtr ->
          alloca $ \winPtr -> alloca $ \renPtr -> do
            ok <-
              createWindowAndRendererSafe
                (PtrConst.unsafeFromPtr titlePtr)
                (round w)
                (round h)
                (wcFlags cfg)
                winPtr
                renPtr
            unless ok $ fail "SDL_CreateWindowAndRenderer failed"
            (,) <$> peek winPtr <*> peek renPtr
      )
      ( \(win, ren) -> do
          void $ setRenderScale ren 1 1
          destroyRendererSafe ren
          destroyWindowSafe win
      )
  density <- liftIO $ queryWindowPixelDensity win
  zoom <- liftIO $ resolveZoom win (wcUiScale cfg)
  -- The requested size is logical, so the window grows with the zoom.
  liftIO $ when (abs (zoom - 1) > scaleEpsilon) $ zoomWindow win (wcSize cfg) zoom
  let
    scale = density * zoom
  liftIO $ setDrawSnapScale ctx scale
  refreshHz <- liftIO $ queryWindowRefreshHz win
  rendererName <-
    liftIO $
      getRendererName ren >>= \name ->
        if PtrConst.unsafeToPtr name == nullPtr
          then pure "unknown"
          else TextForeign.peekCString (PtrConst.unsafeToPtr name)
  scaleRef <- liftIO $ newIORef scale
  uiScaleRef <- liftIO $ newIORef (wcUiScale cfg)
  fontRequestRef <- liftIO $ newIORef (wcUiFont cfg)
  fontAppliedRef <- liftIO $ newIORef (wcUiFont cfg)
  glyphAtlas <- mkAcquire (newGlyphAtlas ren) destroyGlyphAtlas
  images <- mkAcquire newImageAtlas destroyImageAtlas
  cursors <- mkAcquire initCursors destroyCursors
  debug <- liftIO newSdlDebugSampler
  retain <- mkAcquire (newIORef noRetain) $ \ref -> do
    tex <- retainTexture <$> readIORef ref
    unless (tex == nullPtr) $ destroyTexture tex
  fontCache <-
    mkAcquire
      ( newSdlFontCache
          fontSource
          embeddedFontSource
          monoSource
          embeddedFontSource
          glyphAtlas
          (wcFontSize cfg)
          scaleRef
      )
      destroySdlFontCache
  cachedCtx <-
    liftIO $ newIORef . withSdlClipboard =<< withSdlFontCache fontCache ctx
  let
    refreshPeriod = if refreshHz > 0 then 1 / fromIntegral refreshHz else 1 / 60
  liftIO $ do
    scaleOk <- setRenderScale ren 1 1
    unless scaleOk $ fail "SDL_SetRenderScale failed"
  unless bench $
    mkAcquire
      ( void (setRenderVSync ren (if wcVsync cfg then 1 else 0))
          >> void (startTextInputSafe win)
      )
      (const (void (stopTextInputSafe win)))
  dialogState <- mkAcquire newDialogState clearDialogState
  lastPresented <- liftIO $ newIORef False
  batch <- mkAcquire (newRenderBatch ren) destroyRenderBatch
  let
    env =
      SdlEnv
        { sdlWindow = win
        , sdlRenderer = ren
        , sdlRendererName = rendererName
        , sdlBatch = batch
        , sdlFontRequestRef = fontRequestRef
        , sdlFontAppliedRef = fontAppliedRef
        , sdlForcedScale = forcedScale
        , sdlScaleRef = scaleRef
        , sdlUiScaleRef = uiScaleRef
        , sdlGlyphAtlas = glyphAtlas
        , sdlImages = images
        , sdlCursors = cursors
        , sdlDebug = debug
        , sdlRetain = retain
        , sdlLastPresented = lastPresented
        , sdlVsync = wcVsync cfg
        , sdlRefreshPeriod = refreshPeriod
        , sdlContinuous = wcContinuous cfg
        , sdlCachedCtx = cachedCtx
        , sdlFontCache = fontCache
        , sdlDialogState = dialogState
        }
  ctx' <- liftIO $ readIORef cachedCtx
  liftIO $ setHost ctx' env >> setWakeLoop ctx' pushRefreshEvent
  pure (ctx', env)

-- | Write the last presented frame to a BMP file. A retained session reads
-- its retained texture, since SDL leaves the window backbuffer undefined
-- after a present; a direct-to-window session reads the backbuffer.
saveScreenshot :: SdlEnv -> FilePath -> IO Bool
saveScreenshot env path = do
  r <- readIORef (sdlRetain env)
  let tex = retainTexture r
      ren = sdlRenderer env
  surface <-
    if tex == nullPtr
      then renderReadPixels ren (PtrConst.unsafeFromPtr nullPtr)
      else do
        void $ setRenderTarget ren tex
        s <- with (SDL_Rect 0 0 (fromIntegral (retainW r)) (fromIntegral (retainH r))) $ \rp ->
          renderReadPixels ren (PtrConst.unsafeFromPtr rp)
        void $ setRenderTarget ren nullPtr
        pure s
  if surface == nullPtr
    then pure False
    else withCString path $ \cpath -> do
      ok <- saveBMP surface (PtrConst.unsafeFromPtr cpath)
      destroySurface surface
      pure ok
