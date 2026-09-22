{-# LANGUAGE RecordWildCards #-}

-- | SDL session resources, window options, font/display synchronisation, and screenshots.
module NanoUI.Sdl.Internal.Window
  ( RgbaImage (..)
  , SdlEnv (..)
  , Retain (..)
  , SdlOptions (..)
  , WindowDecorations (..)
  , RenderDriver (..)
  , defaultSdlOptions
  , withSdl
  , withSdlBench
  , syncDisplay
  , windowZoom
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
import Data.Text qualified as T
import Data.Text.Foreign qualified as TextForeign
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (ImageId, Input (..), Size (..), Theme, V2 (..))
import NanoUI.Internal.Context (Context (..), setDrawSnapScale)
import NanoUI.Testing (clearMeasureCache, damageFull, markDirty, setHost, setWakeLoop, withClipboard)
import NanoUI.Sdl.Internal.Display
  ( initRefreshEvent
  , pushRefreshEvent
  , queryMouseWindowPos
  , queryWindowPixelDensity
  , queryWindowLogicalSize
  , queryWindowRefreshHz
  , zoomWindow
  )
import NanoUI.Sdl.Internal.Chrome.Types (ChromeState, clearChromeState, newChromeState)
import NanoUI.Sdl.Internal.Frame (WindowDecorations (..), applyDecorations)
import NanoUI.Sdl.Internal.Cursor (SdlCursors (..), destroyCursors, initCursors)
import NanoUI.Sdl.Internal.Font
  ( FontSource (..)
  , GlyphAtlas
  , SdlFontCache
  , destroyGlyphAtlas
  , destroySdlFontCache
  , embeddedFontSource
  , newGlyphAtlas
  , newSdlFontCache
  , reloadSdlFontCache
  , sdlFontCacheSource
  , withSdlFontCache
  , withTtf
  )
import NanoUI.Sdl.Internal.Font.Search (searchFonts)
import NanoUI.Sdl.Internal.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Internal.Debug (SdlDebugSampler, newSdlDebugSampler)
import NanoUI.Sdl.Internal.Dialog.Types (DialogState (..), newDialogState)
import NanoUI.Sdl.Internal.Image (ImageAtlas, destroyImageAtlas, newImageAtlas)
import NanoUI.Sdl.Internal.Render (RenderBatch, destroyRenderBatch, newRenderBatch)
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
import SDL3.Sys.Clipboard (getClipboardText, setClipboardText)
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
import SDL3.Sys.Stdinc (free)
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
  , sdlWindowDecorations :: !WindowDecorations
  -- ^ How much of the desktop's title bar and frame the window keeps
  -- (default: 'DecorationsFull'). 'DecorationsFrame' is for a view that
  -- draws its own title bar ('NanoUI.Backend.Sdl.windowCaption');
  -- 'NanoUI.Backend.Sdl.setWindowDecorations' changes it afterwards.
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
    , sdlWindowDecorations = DecorationsFull
    , sdlWindowAlwaysOnTop = False
    , sdlWindowHidden = False
    , sdlAppVsync = True
    , sdlRenderDriver = RenderDriverAuto
    , sdlAppContinuous = False
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
      .|. flag ((/= DecorationsFull) . sdlWindowDecorations) 0x0000000000000010
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
  , sdlChromeState :: !ChromeState
  -- ^ What a borderless window's own title bar is for; see
  -- "NanoUI.Sdl.Internal.Chrome".
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
  let winSize = case (queried, inputWindowSize inp) of
        (Size 0 0, Size 0 0) -> defaultWindowSize
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
      { sdlWindowTitle = "nano-ui-bench"
      , sdlWindowSize = Size 800 600
      , sdlAppVsync = False
      , sdlAppContinuous = True
      , sdlAppFont = DefaultFont
      , sdlAppMonoFont = DefaultFont
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
        | otherwise = preferredRenderDriver (sdlRenderDriver opts)
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
    ( do
        videoOk <- initSafe (SDL_InitFlags (fromIntegral sDL_INIT_VIDEO))
        unless videoOk $ fail "SDL_Init(SDL_INIT_VIDEO) failed"
    )
    (const quitSafe)
  liftIO $ do
    refreshOk <- initRefreshEvent
    unless refreshOk $ fail "SDL_RegisterEvents failed for refresh wake"
  let
    Size w h = sdlWindowSize opts
  -- NANO_FORCE_SCALE: debug override of the pixel density.
  forcedEnv <- liftIO $ lookupEnv "NANO_FORCE_SCALE"
  let
    sdlForcedScale = case forcedEnv >>= readMaybe of
      Just s | s > 0 -> Just s
      _ -> Nothing
  -- Before the window, so that it is released after the window is gone: the
  -- window holds the hit test this frees.
  sdlChromeState <- mkAcquire newChromeState clearChromeState
  (sdlWindow, sdlRenderer) <-
    mkAcquire
      ( retryWithoutRenderDriver guessedDriver $
          TextForeign.withCString (sdlWindowTitle opts) $ \titlePtr ->
          alloca $ \winPtr -> alloca $ \renPtr -> do
            ok <-
              createWindowAndRendererSafe
                (PtrConst.unsafeFromPtr titlePtr)
                (round w)
                (round h)
                (if bench then sdlWindowHiddenFlag else windowFlags opts)
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
  density <- liftIO $ queryWindowPixelDensity sdlWindow
  zoom <- liftIO $ resolveZoom sdlWindow (sdlAppUiScale opts)
  -- The requested size is logical, so the window grows with the zoom.
  liftIO $ when (abs (zoom - 1) > scaleEpsilon) $ zoomWindow sdlWindow (sdlWindowSize opts) zoom
  -- After the zoom: SDL sizes a borderless window as though its view were
  -- the whole of it, so the desktop's frame goes on around a view that is
  -- already the size asked for, and the window grows by the frame.
  liftIO $ when (sdlWindowDecorations opts /= DecorationsFull) (applyDecorations sdlWindow (sdlWindowDecorations opts))
  let
    scale = density * zoom
  liftIO $ setDrawSnapScale ctx scale
  refreshHz <- liftIO $ queryWindowRefreshHz sdlWindow
  sdlRendererName <-
    liftIO $
      getRendererName sdlRenderer >>= \name ->
        if PtrConst.unsafeToPtr name == nullPtr
          then pure "unknown"
          else TextForeign.peekCString (PtrConst.unsafeToPtr name)
  sdlScaleRef <- liftIO $ newIORef scale
  sdlUiScaleRef <- liftIO $ newIORef (sdlAppUiScale opts)
  sdlFontRequestRef <- liftIO $ newIORef (sdlAppFont opts)
  sdlFontAppliedRef <- liftIO $ newIORef (sdlAppFont opts)
  sdlGlyphAtlas <- mkAcquire (newGlyphAtlas sdlRenderer) destroyGlyphAtlas
  sdlImages <- mkAcquire newImageAtlas destroyImageAtlas
  sdlCursors <- mkAcquire initCursors destroyCursors
  sdlDebug <- liftIO newSdlDebugSampler
  sdlRetain <- mkAcquire (newIORef (Retain nullPtr 0 0 0 0 0)) $ \ref -> do
    tex <- retainTexture <$> readIORef ref
    unless (tex == nullPtr) $ destroyTexture tex
  sdlFontCache <-
    mkAcquire
      (newSdlFontCache fontSource monoSource sdlGlyphAtlas (sdlAppFontSize opts) sdlScaleRef)
      destroySdlFontCache
  sdlCachedCtx <-
    liftIO $ newIORef . withSdlClipboard =<< withSdlFontCache sdlFontCache ctx
  let
    sdlRefreshPeriod = if refreshHz > 0 then 1 / fromIntegral refreshHz else 1 / 60
    sdlVsync = sdlAppVsync opts
    sdlContinuous = sdlAppContinuous opts
  liftIO $ do
    scaleOk <- setRenderScale sdlRenderer 1 1
    unless scaleOk $ fail "SDL_SetRenderScale failed"
  unless bench $
    mkAcquire
      ( void (setRenderVSync sdlRenderer (if sdlVsync then 1 else 0))
          >> void (startTextInputSafe sdlWindow)
      )
      (const (void (stopTextInputSafe sdlWindow)))
  sdlDialogState <- liftIO newDialogState
  sdlLastPresented <- liftIO $ newIORef False
  sdlBatch <- mkAcquire (newRenderBatch sdlRenderer) destroyRenderBatch
  let
    env = SdlEnv {..}
  ctx' <- liftIO $ readIORef sdlCachedCtx
  liftIO $ setHost ctx' env >> setWakeLoop ctx' pushRefreshEvent
  pure (ctx', env)

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
      if ptr == nullPtr
        then pure Nothing
        else do
          txt <- TextForeign.peekCString ptr
          free (castPtr ptr)
          pure (if T.null txt then Nothing else Just txt)

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
