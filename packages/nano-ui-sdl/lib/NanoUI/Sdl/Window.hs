module NanoUI.Sdl.Window
  ( RgbaImage (..)
  , SdlEnv (..)
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
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Text.Foreign qualified as TextForeign
import Foreign.C.String (CString, withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (ImageId, Input (..), Size (..), Theme)
import NanoUI.Context (Context (..), setDrawSnapScale)
import NanoUI.Testing (clearMeasureCache, markDirty, setHost, setWakeLoop)
import NanoUI.Sdl.Display
  ( defaultFontSize
  , defaultUiScale
  , destroyTexture
  , initBenchHints
  , initRefreshEvent
  , initSdlHints
  , pushRefreshEvent
  , queryMouseWindowPos
  , queryWindowDisplayScale
  , queryWindowLogicalSize
  , queryWindowRefreshHz
  , setRenderScale
  , setRenderVSync
  )
import NanoUI.Sdl.Clipboard (withSdlClipboard)
import NanoUI.Sdl.Cursor (SdlCursors (..), destroyCursors, initCursors)
import NanoUI.Sdl.Font
  ( SdlFont
  , FontSource (..)
  , GlyphAtlas
  , closeFont
  , destroyGlyphAtlas
  , newGlyphAtlas
  , openFontSourceWithFallback
  , registerGlyphAtlasRewarm
  , resetGlyphAtlas
  , warmGlyphAtlas
  , withTtf
  , buildGlyphFontMetrics
  , withTtfMeasureGlyph
  , SdlFontCache
  , newSdlFontCache
  , destroySdlFontCache
  , resetSdlFontCache
  , setSdlFontCacheSource
  , withTtfFontCache
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
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import SDL3.Sys.Bindgen.Runtime.PtrConst qualified as PtrConst
import SDL3.Sys.Bindgen.Video (SDL_Window, SDL_WindowFlags (..))
import SDL3.Sys.Bindgen.Init (SDL_InitFlags (..))
import SDL3.Sys.Init (initSafe, quitSafe)
import SDL3.Sys.Keyboard (startTextInputSafe, stopTextInputSafe)
import SDL3.Sys.Render (createWindowAndRendererSafe, destroyRendererSafe)
import SDL3.Sys.Video (destroyWindowSafe)

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
    }

-- | SDL_WINDOW_HIGH_PIXEL_DENSITY (0x2000): without it the window's surface
-- gets scale 1.0 even on a 2x / HiDPI output, so the compositor upscales the
-- whole window (blurry "looks upscaled"). With it, SDL_GetWindowDisplayScale
-- returns the real output scale, the window keeps its logical size, and the
-- pixel buffer (and therefore the retain texture, glyph atlas, and fonts)
-- rasterizes at the native pixel density.
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
  , sdlBatch :: RenderBatch
  , sdlFontSourceRef :: !(IORef FontSource)
  , sdlMonoFontSource :: !FontSource
  , sdlFontRequestRef :: !(IORef NanoUIFont)
  , sdlFontAppliedRef :: !(IORef NanoUIFont)
  , sdlFontSize :: !Float
  , sdlForcedScale :: !(Maybe Float)
  -- ^ NANO_FORCE_SCALE override of the display scale, read at startup.
  , sdlScaleRef :: IORef Float
  , sdlFontRef :: IORef SdlFont
  , sdlMonoFontRef :: IORef SdlFont
  , sdlGlyphAtlas :: GlyphAtlas
  , sdlImages :: ImageAtlas
  , sdlCursors :: SdlCursors
  , sdlDebug :: SdlDebugSampler
  , sdlRetain :: IORef (Ptr (), Int, Int, Float)
  , sdlLastPresented :: IORef Bool
  , sdlVsync :: !Bool
  , sdlRefreshPeriod :: !Double
  , sdlContinuous :: !Bool
  , sdlCachedCtx :: !(IORef Context)
  , sdlFontCache :: !SdlFontCache
  , sdlDialogState :: !DialogState
  }

defaultWindowSize :: Size
defaultWindowSize = Size 1280 800

-- Layout in logical coordinates; draw/text rasterize at native pixel density.
syncDisplay :: Context -> SdlEnv -> Input -> IO (Context, Input)
syncDisplay ctx env inp = do
  scale <- maybe (queryWindowDisplayScale (sdlWindow env)) pure (sdlForcedScale env)
  oldScale <- readIORef (sdlScaleRef env)
  let scaleChanged = abs (scale - oldScale) > scaleEpsilon
  when scaleChanged $ do
    -- Presents leave the renderer at 1:1 pixels; re-assert it only when the
    -- display scale moves.
    unlessM (setRenderScale (sdlRenderer env) defaultUiScale) $
      fail "SDL_SetRenderScale failed"
    writeIORef (sdlScaleRef env) scale
    setDrawSnapScale ctx scale
  -- Runtime font-family switch: the app publishes its requested family through
  -- 'setSdlUiFont'; resolve and apply it here, on the display thread, before
  -- the next frame so the atlas, metrics, and text resolver agree.
  requested <- readIORef (sdlFontRequestRef env)
  applied <- readIORef (sdlFontAppliedRef env)
  let fontChanged = requested /= applied
  when fontChanged $ do
    newSource <- resolveNanoUIFont requested
    writeIORef (sdlFontSourceRef env) newSource
    setSdlFontCacheSource (sdlFontCache env) newSource
    writeIORef (sdlFontAppliedRef env) requested
  when (scaleChanged || fontChanged) (rebuildScaledFonts ctx env scale)
  queried <- queryWindowLogicalSize (sdlWindow env)
  let winSize =
        case queried of
          Size 0 0 ->
            case inputWindowSize inp of
              Size 0 0 -> defaultWindowSize
              s -> s
          s -> s
  mouse <- queryMouseWindowPos
  ctxMeasured <- readIORef (sdlCachedCtx env)
  pure (withSdlClipboard ctxMeasured, inp {inputWindowSize = winSize, inputMousePos = mouse})

-- | Reopen the base sans/mono fonts at @scale@, rebuild metrics and the text
-- resolver, and invalidate cached measurements. Shared by the DPI-change and
-- runtime font-family-switch paths in 'syncDisplay'.
rebuildScaledFonts :: Context -> SdlEnv -> Float -> IO ()
rebuildScaledFonts ctx env scale = do
  uiSource <- readIORef (sdlFontSourceRef env)
  oldFont <- readIORef (sdlFontRef env)
  closeFont oldFont
  newFont <- openFontSourceWithFallback uiSource embeddedFontSource (sdlFontSize env * scale)
  writeIORef (sdlFontRef env) newFont
  oldMono <- readIORef (sdlMonoFontRef env)
  closeFont oldMono
  newMono <- openFontSourceWithFallback (sdlMonoFontSource env) embeddedFontSource (sdlFontSize env * scale)
  writeIORef (sdlMonoFontRef env) newMono
  -- resetGlyphAtlas re-warms the (already updated) base fonts through the
  -- registered hook and bumps the epoch so run caches self-clear.
  resetGlyphAtlas (sdlGlyphAtlas env)
  let ga = sdlGlyphAtlas env
  fm <- buildGlyphFontMetrics ga newFont scale
  monoFm <- buildGlyphFontMetrics ga newMono scale
  let ctx' = withTtfFontCache (sdlFontCache env) (withTtfMeasureGlyph ctx newFont fm monoFm scale)
  resetSdlFontCache (sdlFontCache env) scale newFont fm newMono monoFm
  writeIORef (sdlCachedCtx env) ctx'
  clearMeasureCache ctx
  markDirty ctx

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
      }

withSdlWindow :: Context -> WindowConfig -> (Context -> SdlEnv -> IO a) -> IO a
withSdlWindow ctx cfg act =
  withTtf $ do
    if wcBench cfg then initBenchHints else initSdlHints (wcVsync cfg)
    fontSource <- resolveNanoUIFont (wcUiFont cfg)
    monoSource <- resolveNanoUIFont (wcMonoFont cfg)
    bracket
      (startSdlWindow ctx cfg fontSource monoSource)
      (\(_, env) -> stopSdlWindow (wcBench cfg) env)
      (uncurry act)

startSdlWindow :: Context -> WindowConfig -> FontSource -> FontSource -> IO (Context, SdlEnv)
startSdlWindow ctx cfg fontSource monoSource = do
  unlessM (initSafe (SDL_InitFlags 32)) $
    fail "SDL_Init(SDL_INIT_VIDEO) failed"
  unlessM initRefreshEvent $
    fail "SDL_RegisterEvents failed for refresh wake"
  let Size w h = wcSize cfg
      fontSize = wcFontSize cfg
      bench = wcBench cfg
  -- NANO_FORCE_SCALE: debug override of the display scale.
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
          scale <- queryWindowDisplayScale win
          setDrawSnapScale ctx scale
          refreshHz <- queryWindowRefreshHz win
          font <- openFontSourceWithFallback fontSource embeddedFontSource (fontSize * scale)
          monoFont <- openFontSourceWithFallback monoSource embeddedFontSource (fontSize * scale)
          scaleRef <- newIORef scale
          fontRef <- newIORef font
          monoFontRef <- newIORef monoFont
          fontSourceRef <- newIORef fontSource
          fontRequestRef <- newIORef (wcUiFont cfg)
          fontAppliedRef <- newIORef (wcUiFont cfg)
          glyphAtlas <- newGlyphAtlas ren
          -- Re-warm the base fonts after every atlas reset (DPI change,
          -- font switch, exhaustion recovery) so the next frame does not
          -- pay cold glyph misses. The hook reads the font refs lazily, so
          -- it always warms the live fonts, never one already closed.
          registerGlyphAtlasRewarm glyphAtlas $ do
            warmSans <- readIORef fontRef
            warmGlyphAtlas glyphAtlas warmSans
            warmMono <- readIORef monoFontRef
            warmGlyphAtlas glyphAtlas warmMono
          warmGlyphAtlas glyphAtlas font
          warmGlyphAtlas glyphAtlas monoFont
          images <- newImageAtlas
          cursors <- initCursors
          debug <- newSdlDebugSampler
          retain <- newIORef (nullPtr, 0, 0, 0)
          fm <- buildGlyphFontMetrics glyphAtlas font scale
          monoFm <- buildGlyphFontMetrics glyphAtlas monoFont scale
          fontCache <-
            newSdlFontCache
              fontSource
              embeddedFontSource
              monoSource
              embeddedFontSource
              glyphAtlas
              fontSize
              scale
              font
              fm
              monoFont
              monoFm
          cachedCtx <- newIORef (withTtfFontCache fontCache (withTtfMeasureGlyph ctx font fm monoFm scale))
          let refreshPeriod =
                if refreshHz > 0
                  then 1 / fromIntegral refreshHz
                  else 1 / 60
          unlessM (setRenderScale ren defaultUiScale) $
            fail "SDL_SetRenderScale failed"
          unless bench $ do
            void $ setRenderVSync ren (wcVsync cfg)
            void $ startTextInputSafe win
          dialogState <- newDialogState
          lastPresented <- newIORef False
          batch <- newRenderBatch ren
          pure
            SdlEnv
              { sdlWindow = win
              , sdlRenderer = ren
              , sdlBatch = batch
              , sdlFontSourceRef = fontSourceRef
              , sdlMonoFontSource = monoSource
              , sdlFontRequestRef = fontRequestRef
              , sdlFontAppliedRef = fontAppliedRef
              , sdlFontSize = fontSize
              , sdlForcedScale = forcedScale
              , sdlScaleRef = scaleRef
              , sdlFontRef = fontRef
              , sdlMonoFontRef = monoFontRef
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
  ctxMeasured <- readIORef (sdlCachedCtx env)
  let ctx' = withSdlClipboard ctxMeasured
  setHost ctx' env
  setWakeLoop ctx' pushRefreshEvent
  pure (ctx', env)

stopSdlWindow :: Bool -> SdlEnv -> IO ()
stopSdlWindow bench env = do
  clearDialogState (sdlDialogState env)
  (tex, _, _, _) <- readIORef (sdlRetain env)
  destroyTexture tex
  destroyRenderBatch (sdlBatch env)
  destroyCursors (sdlCursors env)
  destroyImageAtlas (sdlImages env)
  destroySdlFontCache (sdlFontCache env)
  destroyGlyphAtlas (sdlGlyphAtlas env)
  font <- readIORef (sdlFontRef env)
  closeFont font
  monoFont <- readIORef (sdlMonoFontRef env)
  closeFont monoFont
  unless bench $ void $ stopTextInputSafe (sdlWindow env)
  void $ setRenderScale (sdlRenderer env) defaultUiScale
  destroyRendererSafe (sdlRenderer env)
  destroyWindowSafe (sdlWindow env)
  quitSafe

unlessM :: IO Bool -> IO () -> IO ()
unlessM p act = do
  ok <- p
  unless ok act

foreign import ccall unsafe "nano_ui_save_screenshot"
  c_nano_ui_save_screenshot :: Ptr SDL_Renderer -> CString -> IO Bool

saveScreenshot :: SdlEnv -> FilePath -> IO Bool
saveScreenshot env path =
  withCString path $ \cpath ->
    c_nano_ui_save_screenshot (sdlRenderer env) cpath
