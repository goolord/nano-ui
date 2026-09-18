module NanoUI.Sdl.Window
  ( RgbaImage (..)
  , SdlEnv (..)
  , Retain (..)
  , noRetain
  , SdlOptions (..)
  , defaultSdlOptions
  , withSdl
  , withSdlBench
  , syncDisplay
  , saveScreenshot
  ) where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Maybe (isJust, isNothing)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Text.Foreign qualified as TextForeign
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (ImageId, Input (..), Size (..), Theme, V2 (..))
import NanoUI.Context (Context (..), setDrawSnapScale)
import NanoUI.Testing (clearMeasureCache, markDirty, setHost, setWakeLoop)
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
import SDL3.Sys.Bindgen.Hints (sDL_HINT_ASSERT, sDL_HINT_RENDER_VSYNC, sDL_HINT_VIDEO_DRIVER)
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window, SDL_WindowFlags (..))
import SDL3.Sys.Bindgen.Init (SDL_InitFlags (..), sDL_INIT_VIDEO)
import SDL3.Sys.Hints (setHint)
import SDL3.Sys.Init (initSafe, quitSafe)
import SDL3.Sys.Keyboard (startTextInputSafe, stopTextInputSafe)
import SDL3.Sys.Render
  ( createWindowAndRendererSafe
  , destroyRendererSafe
  , destroyTexture
  , getRendererName
  , renderReadPixels
  , setRenderScale
  , setRenderVSync
  )
import SDL3.Sys.Surface (destroySurface, saveBMP)
import SDL3.Sys.Video (destroyWindowSafe, getWindowDisplayScale)

-- | Initial RGBA asset uploaded before the first frame.
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
  , sdlAppContinuous :: !Bool
  -- ^ Continuous unthrottled rendering without waiting for events (default: 'False').
  , sdlAppFont :: !NanoUIFont
  -- ^ UI font (default: embedded Inter).
  , sdlAppMonoFont :: !NanoUIFont
  -- ^ Monospace font (default: embedded Inter).
  , sdlAppFontSize :: !Float
  -- ^ Base font size in points (default: 16).
  , sdlAppTheme :: !(Maybe Theme)
  -- ^ Initial UI theme override (default: 'Nothing').
  , sdlAppShouldQuit :: !(Input -> Bool)
  -- ^ Predicate on user input to trigger application exit (default: @const False@).\
  , sdlAppImages :: !(SmallArray RgbaImage)
  -- ^ Initial RGBA textures registered before the first frame.
  , sdlAppUiScale :: !Float
  -- ^ Zoom of the whole UI, on top of the window's pixel density (default:
  -- 1). Zero or less follows the display's content scale, which on Windows
  -- is the desktop's scaling setting. 'NanoUI.Backend.Sdl.setSdlUiScale'
  -- changes it at runtime.
  }

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

-- Layout in logical coordinates (window coordinates over the zoom);
-- draw/text rasterize at native pixel density.
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
  }

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
      }

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
      }

withSdlWindow :: Context -> WindowConfig -> (Context -> SdlEnv -> IO a) -> IO a
withSdlWindow ctx cfg act =
  withTtf $ do
    let hint name value =
          BS.useAsCString name $ \cname ->
            BS.useAsCString value $ \cvalue ->
              void $ setHint (PtrConst.unsafeFromPtr cname) (PtrConst.unsafeFromPtr cvalue)
    if wcBench cfg
      then do
        hint sDL_HINT_ASSERT "always_ignore"
        hint sDL_HINT_RENDER_VSYNC "0"
      else do
        hint sDL_HINT_RENDER_VSYNC (if wcVsync cfg then "1" else "0")
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
          hint sDL_HINT_VIDEO_DRIVER "wayland"
    fontSource <- resolveNanoUIFont (wcUiFont cfg)
    monoSource <- resolveNanoUIFont (wcMonoFont cfg)
    bracket
      (startSdlWindow ctx cfg fontSource monoSource)
      (\(_, env) -> stopSdlWindow (wcBench cfg) env)
      (uncurry act)

startSdlWindow :: Context -> WindowConfig -> FontSource -> FontSource -> IO (Context, SdlEnv)
startSdlWindow ctx cfg fontSource monoSource = do
  videoOk <- initSafe (SDL_InitFlags (fromIntegral sDL_INIT_VIDEO))
  unless videoOk $ fail "SDL_Init(SDL_INIT_VIDEO) failed"
  refreshOk <- initRefreshEvent
  unless refreshOk $ fail "SDL_RegisterEvents failed for refresh wake"
  let Size w h = wcSize cfg
      bench = wcBench cfg
  -- NANO_FORCE_SCALE: debug override of the pixel density.
  forcedEnv <- lookupEnv "NANO_FORCE_SCALE"
  let forcedScale = case forcedEnv >>= readMaybe of
        Just s | s > 0 -> Just s
        _ -> Nothing
  env <-
    TextForeign.withCString (wcTitle cfg) $ \titlePtr ->
      alloca $ \winPtr ->
        alloca $ \renPtr -> do
          ok <-
            createWindowAndRendererSafe
              (PtrConst.unsafeFromPtr titlePtr)
              (round w)
              (round h)
              (wcFlags cfg)
              winPtr
              renPtr
          unless ok $ fail "SDL_CreateWindowAndRenderer failed"
          win <- peek winPtr
          ren <- peek renPtr
          density <- queryWindowPixelDensity win
          zoom <- resolveZoom win (wcUiScale cfg)
          -- The requested size is logical, so the window grows with the zoom.
          when (abs (zoom - 1) > scaleEpsilon) $
            zoomWindow win (wcSize cfg) zoom
          let scale = density * zoom
          setDrawSnapScale ctx scale
          refreshHz <- queryWindowRefreshHz win
          rendererName <- getRendererName ren >>= \name ->
            if PtrConst.unsafeToPtr name == nullPtr then pure "unknown" else TextForeign.peekCString (PtrConst.unsafeToPtr name)
          scaleRef <- newIORef scale
          uiScaleRef <- newIORef (wcUiScale cfg)
          fontRequestRef <- newIORef (wcUiFont cfg)
          fontAppliedRef <- newIORef (wcUiFont cfg)
          glyphAtlas <- newGlyphAtlas ren
          images <- newImageAtlas
          cursors <- initCursors
          debug <- newSdlDebugSampler
          retain <- newIORef noRetain
          fontCache <-
            newSdlFontCache
              fontSource
              embeddedFontSource
              monoSource
              embeddedFontSource
              glyphAtlas
              (wcFontSize cfg)
              scaleRef
          cachedCtx <- newIORef . withSdlClipboard =<< withSdlFontCache fontCache ctx
          let refreshPeriod =
                if refreshHz > 0
                  then 1 / fromIntegral refreshHz
                  else 1 / 60
          scaleOk <- setRenderScale ren 1 1
          unless scaleOk $ fail "SDL_SetRenderScale failed"
          unless bench $ do
            void $ setRenderVSync ren (if wcVsync cfg then 1 else 0)
            void $ startTextInputSafe win
          dialogState <- newDialogState
          lastPresented <- newIORef False
          batch <- newRenderBatch ren
          pure
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
  ctx' <- readIORef (sdlCachedCtx env)
  setHost ctx' env
  setWakeLoop ctx' pushRefreshEvent
  pure (ctx', env)

stopSdlWindow :: Bool -> SdlEnv -> IO ()
stopSdlWindow bench env = do
  clearDialogState (sdlDialogState env)
  tex <- retainTexture <$> readIORef (sdlRetain env)
  unless (tex == nullPtr) $ destroyTexture tex
  destroyRenderBatch (sdlBatch env)
  destroyCursors (sdlCursors env)
  destroyImageAtlas (sdlImages env)
  destroySdlFontCache (sdlFontCache env)
  destroyGlyphAtlas (sdlGlyphAtlas env)
  unless bench $ void $ stopTextInputSafe (sdlWindow env)
  void $ setRenderScale (sdlRenderer env) 1 1
  destroyRendererSafe (sdlRenderer env)
  destroyWindowSafe (sdlWindow env)
  quitSafe

saveScreenshot :: SdlEnv -> FilePath -> IO Bool
saveScreenshot env path = do
  surface <- renderReadPixels (sdlRenderer env) (PtrConst.unsafeFromPtr nullPtr)
  if surface == nullPtr
    then pure False
    else withCString path $ \cpath -> do
      ok <- saveBMP surface (PtrConst.unsafeFromPtr cpath)
      destroySurface surface
      pure ok
