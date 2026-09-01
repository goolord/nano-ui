module NanoUI.Sdl.Window
  ( RgbaImage (..)
  , SdlEnv (..)
  , SdlOptions (..)
  , defaultSdlOptions
  , defaultWindowSize
  , withSdl
  , withSdlBench
  , acquireSdlBench
  , releaseSdlBench
  , syncDisplay
  ) where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Text.Foreign qualified as TextForeign
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (FontMetrics (..), ImageId, Input (..), Size (..), Theme)
import NanoUI.Testing (Context, clearMeasureCache, markDirty, setHost, setWakeLoop)
import NanoUI.Sdl.Display
  ( defaultFontSize
  , defaultUiScale
  , initBenchHints
  , initRefreshEvent
  , initSdlHints
  , pushRefreshEvent
  , queryMouseWindowPos
  , queryWindowDisplayScale
  , queryWindowLogicalSize
  , retainDestroy
  , setRenderScale
  , setRenderVSync
  , windowToLogicalCoords
  )
import NanoUI.Sdl.Clipboard (withSdlClipboard)
import NanoUI.Sdl.Cursor (SdlCursors (..), destroyCursors, initCursors)
import NanoUI.Sdl.Font
  ( SdlFont
  , GlyphAtlas
  , closeFont
  , destroyGlyphAtlas
  , findFontPath
  , findMonoFontPath
  , newGlyphAtlas
  , openFont
  , resetGlyphAtlas
  , warmGlyphAtlas
  , withTtf
  , buildGlyphFontMetrics
  , withTtfMeasureGlyph
  )
import NanoUI.Sdl.Debug (SdlDebugSampler, newSdlDebugSampler)
import NanoUI.Sdl.Image (ImageAtlas, destroyImageAtlas, newImageAtlas)
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
  , sdlAppFontPath :: !(Maybe FilePath)
  -- ^ Optional custom TrueType font file path.
  , sdlAppMonoFontPath :: !(Maybe FilePath)
  -- ^ Optional custom monospace TrueType font file path.
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
    , sdlAppFontPath = Nothing
    , sdlAppMonoFontPath = Nothing
    , sdlAppFontSize = defaultFontSize
    , sdlAppTheme = Nothing
    , sdlAppShouldQuit = const False
    , sdlAppImages = mempty
    }

computeWindowFlags :: Bool -> Bool -> Bool -> Bool -> Bool -> SDL_WindowFlags
computeWindowFlags resizable fullscreen borderless alwaysOnTop hidden =
  SDL_WindowFlags $
    (if resizable then 0x0000000000000020 else 0)
      .|. (if fullscreen then 0x0000000000000001 else 0)
      .|. (if borderless then 0x0000000000000010 else 0)
      .|. (if alwaysOnTop then 0x0000000000010000 else 0)
      .|. (if hidden then 0x0000000000000008 else 0)

-- Hidden only. Do not combine with resizable for bench windows on Windows.
sdlWindowHiddenFlag :: SDL_WindowFlags
sdlWindowHiddenFlag = SDL_WindowFlags 0x0000000000000008

benchWindowSize :: Size
benchWindowSize = Size 800 600

scaleEpsilon :: Float
scaleEpsilon = 0.001

data SdlEnv = SdlEnv
  { sdlWindow :: Ptr SDL_Window
  , sdlRenderer :: Ptr SDL_Renderer
  , sdlFontPath :: FilePath
  , sdlMonoFontPath :: FilePath
  , sdlFontSize :: !Float
  , sdlScaleRef :: IORef Float
  , sdlFontRef :: IORef SdlFont
  , sdlMonoFontRef :: IORef SdlFont
  , sdlGlyphAtlas :: GlyphAtlas
  , sdlImages :: ImageAtlas
  , sdlCursors :: SdlCursors
  , sdlDebug :: IORef SdlDebugSampler
  , sdlRetain :: IORef (Ptr (), Int, Int, Float)
  , sdlVsync :: !Bool
  , sdlCachedFm :: !(IORef FontMetrics)
  , sdlCachedMonoFm :: !(IORef FontMetrics)
  , sdlCachedCtx :: !(IORef Context)
  }

defaultWindowSize :: Size
defaultWindowSize = Size 1280 800

-- Layout in logical coordinates; draw/text rasterize at native pixel density.
syncDisplay :: Context -> SdlEnv -> Input -> IO (Context, Input)
syncDisplay ctx env inp = do
  scale <- queryWindowDisplayScale (sdlWindow env)
  unlessM (setRenderScale (sdlRenderer env) defaultUiScale) $
    fail "SDL_SetRenderScale failed"
  oldScale <- readIORef (sdlScaleRef env)
  when (abs (scale - oldScale) > scaleEpsilon) $ do
    writeIORef (sdlScaleRef env) scale
    oldFont <- readIORef (sdlFontRef env)
    closeFont oldFont
    newFont <- openFont (sdlFontPath env) (sdlFontSize env * scale)
    writeIORef (sdlFontRef env) newFont
    oldMono <- readIORef (sdlMonoFontRef env)
    closeFont oldMono
    newMono <- openFont (sdlMonoFontPath env) (sdlFontSize env * scale)
    writeIORef (sdlMonoFontRef env) newMono
    resetGlyphAtlas (sdlGlyphAtlas env)
    warmGlyphAtlas (sdlGlyphAtlas env) newFont
    warmGlyphAtlas (sdlGlyphAtlas env) newMono
    let ga = sdlGlyphAtlas env
        fm = buildGlyphFontMetrics ga newFont scale
        monoFm = buildGlyphFontMetrics ga newMono scale
        ctx' = withTtfMeasureGlyph ctx newFont newMono fm monoFm scale
    writeIORef (sdlCachedFm env) fm
    writeIORef (sdlCachedMonoFm env) monoFm
    writeIORef (sdlCachedCtx env) ctx'
    clearMeasureCache ctx
    markDirty ctx
  queried <- queryWindowLogicalSize (sdlWindow env) scale
  let winSize =
        case queried of
          Size 0 0 ->
            case inputWindowSize inp of
              Size 0 0 -> defaultWindowSize
              s -> s
          s -> s
  inpSized <- syncInput env scale inp {inputWindowSize = winSize}
  ctxMeasured <- readIORef (sdlCachedCtx env)
  let ctx' = withSdlClipboard ctxMeasured
  pure (ctx', inpSized)

syncInput :: SdlEnv -> Float -> Input -> IO Input
syncInput _env scale inp = do
  mPos <- queryMouseWindowPos
  pure $
    case mPos of
      Just windowPos -> inp {inputMousePos = windowToLogicalCoords scale windowPos}
      Nothing -> inp

withSdl :: SdlOptions -> Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdl opts ctx act =
  let Size w h = sdlWindowSize opts
      flags =
        computeWindowFlags
          (sdlWindowResizable opts)
          (sdlWindowFullscreen opts)
          (sdlWindowBorderless opts)
          (sdlWindowAlwaysOnTop opts)
          (sdlWindowHidden opts)
   in withSdlWindow
        ctx
        (sdlWindowTitle opts)
        w
        h
        flags
        False
        (sdlAppVsync opts)
        (sdlAppFontPath opts)
        (sdlAppMonoFontPath opts)
        (sdlAppFontSize opts)
        act

withSdlBench :: Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdlBench ctx act =
  let Size w h = benchWindowSize
   in withSdlWindow
        ctx
        "nano-ui-bench"
        w
        h
        sdlWindowHiddenFlag
        True
        False
        Nothing
        Nothing
        defaultFontSize
        act

withSdlWindow ::
  Context ->
  Text ->
  Float ->
  Float ->
  SDL_WindowFlags ->
  Bool ->
  Bool ->
  Maybe FilePath ->
  Maybe FilePath ->
  Float ->
  (Context -> SdlEnv -> IO a) ->
  IO a
withSdlWindow ctx title w h flags bench vsync mFont mMono fontSize act =
  withTtf $ do
    if bench then initBenchHints else initSdlHints vsync
    fontPath <- resolveFontPath mFont
    monoPath <- resolveMonoFontPath mMono fontPath
    bracket
      (startSdlWindow ctx title w h flags bench vsync fontPath monoPath fontSize)
      (\(_, env) -> stopSdlWindow bench env)
      $ \(ctx', env) -> act ctx' env

resolveFontPath :: Maybe FilePath -> IO FilePath
resolveFontPath (Just p) = pure p
resolveFontPath Nothing =
  findFontPath >>= \case
    Nothing ->
      fail
        ( "No TrueType font found. Install a system font or set NANO_UI_FONT "
            <> "to a .ttf path."
        )
    Just p -> pure p

resolveMonoFontPath :: Maybe FilePath -> FilePath -> IO FilePath
resolveMonoFontPath (Just p) _ = pure p
resolveMonoFontPath Nothing fontPath =
  findMonoFontPath >>= \case
    Nothing -> pure fontPath
    Just p -> pure p

startSdlWindow ::
  Context ->
  Text ->
  Float ->
  Float ->
  SDL_WindowFlags ->
  Bool ->
  Bool ->
  FilePath ->
  FilePath ->
  Float ->
  IO (Context, SdlEnv)
startSdlWindow ctx title w h flags bench vsync fontPath monoPath fontSize = do
  unlessM (initSafe (SDL_InitFlags 32)) $
    fail "SDL_Init(SDL_INIT_VIDEO) failed"
  unlessM initRefreshEvent $
    fail "SDL_RegisterEvents failed for refresh wake"
  env <-
    TextForeign.withCString title $ \titlePtr ->
      alloca $ \winPtr ->
        alloca $ \renPtr -> do
          ok <-
            createWindowAndRendererSafe
              (PtrConst.unsafeFromPtr titlePtr)
              (round w)
              (round h)
              flags
              winPtr
              renPtr
          unless ok $ fail "SDL_CreateWindowAndRenderer failed"
          win <- peek winPtr
          ren <- peek renPtr
          scale <- queryWindowDisplayScale win
          font <- openFont fontPath (fontSize * scale)
          monoFont <- openFont monoPath (fontSize * scale)
          scaleRef <- newIORef scale
          fontRef <- newIORef font
          monoFontRef <- newIORef monoFont
          glyphAtlas <- newGlyphAtlas ren
          warmGlyphAtlas glyphAtlas font
          warmGlyphAtlas glyphAtlas monoFont
          images <- newImageAtlas
          cursors <- initCursors
          debug <- newSdlDebugSampler
          retain <- newIORef (nullPtr, 0, 0, 0)
          let ga = glyphAtlas
              fm = buildGlyphFontMetrics ga font scale
              monoFm = buildGlyphFontMetrics ga monoFont scale
          cachedFm <- newIORef fm
          cachedMonoFm <- newIORef monoFm
          cachedCtx <- newIORef (withTtfMeasureGlyph ctx font monoFont fm monoFm scale)
          unlessM (setRenderScale ren defaultUiScale) $
            fail "SDL_SetRenderScale failed"
          unless bench $ void $ setRenderVSync ren vsync
          when (not bench) $ void $ startTextInputSafe win
          pure
            SdlEnv
              { sdlWindow = win
              , sdlRenderer = ren
              , sdlFontPath = fontPath
              , sdlMonoFontPath = monoPath
              , sdlFontSize = fontSize
              , sdlScaleRef = scaleRef
              , sdlFontRef = fontRef
              , sdlMonoFontRef = monoFontRef
              , sdlGlyphAtlas = glyphAtlas
              , sdlImages = images
              , sdlCursors = cursors
              , sdlDebug = debug
              , sdlRetain = retain
              , sdlVsync = vsync
              , sdlCachedFm = cachedFm
              , sdlCachedMonoFm = cachedMonoFm
              , sdlCachedCtx = cachedCtx
              }
  ctxMeasured <- readIORef (sdlCachedCtx env)
  let ctx' = withSdlClipboard ctxMeasured
  setHost ctx' env
  setWakeLoop ctx' pushRefreshEvent
  pure (ctx', env)

stopSdlWindow :: Bool -> SdlEnv -> IO ()
stopSdlWindow bench env = do
  (tex, _, _, _) <- readIORef (sdlRetain env)
  retainDestroy tex
  destroyCursors (sdlCursors env)
  destroyImageAtlas (sdlImages env)
  destroyGlyphAtlas (sdlGlyphAtlas env)
  font <- readIORef (sdlFontRef env)
  closeFont font
  monoFont <- readIORef (sdlMonoFontRef env)
  closeFont monoFont
  when (not bench) $ void $ stopTextInputSafe (sdlWindow env)
  void $ setRenderScale (sdlRenderer env) defaultUiScale
  destroyRendererSafe (sdlRenderer env)
  destroyWindowSafe (sdlWindow env)
  quitSafe

acquireSdlBench :: Context -> IO (Context, SdlEnv)
acquireSdlBench ctx =
  withTtf $ do
    initBenchHints
    fontPath <- resolveFontPath Nothing
    monoPath <- resolveMonoFontPath Nothing fontPath
    let Size w h = benchWindowSize
    startSdlWindow ctx "nano-ui-bench" w h sdlWindowHiddenFlag True False fontPath monoPath defaultFontSize

releaseSdlBench :: SdlEnv -> IO ()
releaseSdlBench env = withTtf $ stopSdlWindow True env

unlessM :: IO Bool -> IO () -> IO ()
unlessM p act = do
  ok <- p
  unless ok act
