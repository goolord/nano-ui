module NanoUI.Sdl.Window
  ( SdlEnv (..)
  , withSdl
  , withSdlBench
  , acquireSdlBench
  , releaseSdlBench
  , defaultWindowSize
  , syncDisplay
  ) where

import Control.Exception (bracket)
import Control.Monad (unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peek)
import NanoUI (Context, Input (..), Size (..), markDirty, setHost, setWakeLoop)
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
  , windowToLogicalCoords
  )
import NanoUI.Sdl.Clipboard (withSdlClipboard)
import NanoUI.Sdl.Cursor (SdlCursors (..), destroyCursors, initCursors)
import NanoUI.Sdl.Font
  ( SdlFont
  , TextCache
  , closeFont
  , destroyTextCache
  , findFontPath
  , findMonoFontPath
  , newTextCache
  , openFont
  , withTtf
  , withTtfMeasureScaled
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

sdlWindowResizable :: SDL_WindowFlags
sdlWindowResizable = SDL_WindowFlags 0x0000000000000020

-- Hidden only. Do not combine with resizable for bench windows on Windows.
sdlWindowHidden :: SDL_WindowFlags
sdlWindowHidden = SDL_WindowFlags 0x0000000000000008

benchWindowSize :: Size
benchWindowSize = Size 800 600

scaleEpsilon :: Float
scaleEpsilon = 0.001

data SdlEnv = SdlEnv
  { sdlWindow :: Ptr SDL_Window
  , sdlRenderer :: Ptr SDL_Renderer
  , sdlFontPath :: FilePath
  , sdlMonoFontPath :: FilePath
  , sdlScaleRef :: IORef Float
  , sdlFontRef :: IORef SdlFont
  , sdlMonoFontRef :: IORef SdlFont
  , sdlTextCache :: TextCache
  , sdlImages :: ImageAtlas
  , sdlCursors :: SdlCursors
  , sdlDebug :: IORef SdlDebugSampler
  , sdlRetain :: IORef (Ptr (), Int, Int)
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
    newFont <- openFont (sdlFontPath env) (defaultFontSize * scale)
    writeIORef (sdlFontRef env) newFont
    oldMono <- readIORef (sdlMonoFontRef env)
    closeFont oldMono
    newMono <- openFont (sdlMonoFontPath env) (defaultFontSize * scale)
    writeIORef (sdlMonoFontRef env) newMono
    destroyTextCache (sdlTextCache env)
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
  font <- readIORef (sdlFontRef env)
  monoFont <- readIORef (sdlMonoFontRef env)
  let ctx' = withTtfMeasureScaled ctx font monoFont scale
  pure (ctx', inpSized)

syncInput :: SdlEnv -> Float -> Input -> IO Input
syncInput _env scale inp = do
  mPos <- queryMouseWindowPos
  pure $
    case mPos of
      Just windowPos -> inp {inputMousePos = windowToLogicalCoords scale windowPos}
      Nothing -> inp

withSdl :: Context -> String -> Size -> (Context -> SdlEnv -> IO a) -> IO a
withSdl ctx title size act = withSdlWindow ctx title size sdlWindowResizable False act

withSdlBench :: Context -> (Context -> SdlEnv -> IO a) -> IO a
withSdlBench ctx act =
  withSdlWindow ctx "nano-ui-bench" benchWindowSize sdlWindowHidden True act

withSdlWindow ::
  Context ->
  String ->
  Size ->
  SDL_WindowFlags ->
  Bool ->
  (Context -> SdlEnv -> IO a) ->
  IO a
withSdlWindow ctx title (Size w h) flags bench act =
  withTtf $ do
    if bench then initBenchHints else initSdlHints
    fontPath <- resolveFontPath
    monoPath <- resolveMonoFontPath fontPath
    bracket
      (startSdlWindow ctx title w h flags bench fontPath monoPath)
      (\(_, env) -> stopSdlWindow bench env)
      $ \(ctx', env) -> act ctx' env

resolveFontPath :: IO FilePath
resolveFontPath =
  findFontPath >>= \case
    Nothing ->
      fail
        ( "No TrueType font found. Install a system font or set NANO_UI_FONT "
            <> "to a .ttf path."
        )
    Just p -> pure p

resolveMonoFontPath :: FilePath -> IO FilePath
resolveMonoFontPath fontPath =
  findMonoFontPath >>= \case
    Nothing -> pure fontPath
    Just p -> pure p

startSdlWindow ::
  Context ->
  String ->
  Float ->
  Float ->
  SDL_WindowFlags ->
  Bool ->
  FilePath ->
  FilePath ->
  IO (Context, SdlEnv)
startSdlWindow ctx title w h flags bench fontPath monoPath = do
  unlessM (initSafe (SDL_InitFlags 32)) $
    fail "SDL_Init(SDL_INIT_VIDEO) failed"
  unlessM initRefreshEvent $
    fail "SDL_RegisterEvents failed for refresh wake"
  env <-
    withCString title $ \titlePtr ->
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
          font <- openFont fontPath (defaultFontSize * scale)
          monoFont <- openFont monoPath (defaultFontSize * scale)
          scaleRef <- newIORef scale
          fontRef <- newIORef font
          monoFontRef <- newIORef monoFont
          cache <- newTextCache ren
          images <- newImageAtlas
          cursors <- initCursors
          debug <- newSdlDebugSampler
          retain <- newIORef (nullPtr, 0, 0)
          unlessM (setRenderScale ren defaultUiScale) $
            fail "SDL_SetRenderScale failed"
          when (not bench) $ void $ startTextInputSafe win
          pure
            SdlEnv
              { sdlWindow = win
              , sdlRenderer = ren
              , sdlFontPath = fontPath
              , sdlMonoFontPath = monoPath
              , sdlScaleRef = scaleRef
              , sdlFontRef = fontRef
              , sdlMonoFontRef = monoFontRef
              , sdlTextCache = cache
              , sdlImages = images
              , sdlCursors = cursors
              , sdlDebug = debug
              , sdlRetain = retain
              }
  scale <- readIORef (sdlScaleRef env)
  font <- readIORef (sdlFontRef env)
  monoFont <- readIORef (sdlMonoFontRef env)
  let ctx' = withSdlClipboard (withTtfMeasureScaled ctx font monoFont scale)
  setHost ctx' env
  setWakeLoop ctx' pushRefreshEvent
  pure (ctx', env)

stopSdlWindow :: Bool -> SdlEnv -> IO ()
stopSdlWindow bench env = do
  (tex, _, _) <- readIORef (sdlRetain env)
  retainDestroy tex
  destroyCursors (sdlCursors env)
  destroyImageAtlas (sdlImages env)
  destroyTextCache (sdlTextCache env)
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
    fontPath <- resolveFontPath
    monoPath <- resolveMonoFontPath fontPath
    let Size w h = benchWindowSize
    startSdlWindow ctx "nano-ui-bench" w h sdlWindowHidden True fontPath monoPath

releaseSdlBench :: SdlEnv -> IO ()
releaseSdlBench env = withTtf $ stopSdlWindow True env

unlessM :: IO Bool -> IO () -> IO ()
unlessM p act = do
  ok <- p
  unless ok act
