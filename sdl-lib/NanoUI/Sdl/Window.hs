module NanoUI.Sdl.Window
  ( SdlEnv (..)
  , withSdl
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
import NanoUI (Context, Input (..), Size (..), markDirty, setWakeLoop)
import NanoUI.Sdl.Display
  ( defaultFontSize
  , defaultUiScale
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

scaleEpsilon :: Float
scaleEpsilon = 0.001

data SdlEnv = SdlEnv
  { sdlWindow :: Ptr SDL_Window
  , sdlRenderer :: Ptr SDL_Renderer
  , sdlFontPath :: FilePath
  , sdlScaleRef :: IORef Float
  , sdlFontRef :: IORef SdlFont
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
    destroyTextCache (sdlTextCache env)
    markDirty ctx
  queried <- queryWindowLogicalSize (sdlWindow env) scale
  let winSize =
        case queried of
          Size 0 0 -> inputWindowSize inp
          s -> s
  inpSized <- syncInput env scale inp {inputWindowSize = winSize}
  font <- readIORef (sdlFontRef env)
  let ctx' = withTtfMeasureScaled ctx font scale
  pure (ctx', inpSized)

syncInput :: SdlEnv -> Float -> Input -> IO Input
syncInput _env scale inp = do
  mPos <- queryMouseWindowPos
  pure $
    case mPos of
      Just windowPos -> inp {inputMousePos = windowToLogicalCoords scale windowPos}
      Nothing -> inp

withSdl :: Context -> String -> Size -> (Context -> SdlEnv -> IO a) -> IO a
withSdl ctx title (Size w h) act =
  withTtf $ do
    initSdlHints
    fontPath <-
      findFontPath >>= \case
        Nothing ->
          fail
            ( "No TrueType font found. Install a system font or set NANO_UI_FONT "
                <> "to a .ttf path."
            )
        Just p -> pure p
    let startup = do
          unlessM (initSafe (SDL_InitFlags 32)) $
            fail "SDL_Init(SDL_INIT_VIDEO) failed"
          unlessM initRefreshEvent $
            fail "SDL_RegisterEvents failed for refresh wake"
          withCString title $ \titlePtr ->
            alloca $ \winPtr ->
              alloca $ \renPtr -> do
                ok <-
                  createWindowAndRendererSafe
                    (PtrConst.unsafeFromPtr titlePtr)
                    (round w)
                    (round h)
                    sdlWindowResizable
                    winPtr
                    renPtr
                unless ok $ fail "SDL_CreateWindowAndRenderer failed"
                win <- peek winPtr
                ren <- peek renPtr
                scale <- queryWindowDisplayScale win
                font <- openFont fontPath (defaultFontSize * scale)
                scaleRef <- newIORef scale
                fontRef <- newIORef font
                cache <- newTextCache
                images <- newImageAtlas
                cursors <- initCursors
                debug <- newSdlDebugSampler
                retain <- newIORef (nullPtr, 0, 0)
                unlessM (setRenderScale ren defaultUiScale) $
                  fail "SDL_SetRenderScale failed"
                _ <- startTextInputSafe win
                pure
                  SdlEnv
                    { sdlWindow = win
                    , sdlRenderer = ren
                    , sdlFontPath = fontPath
                    , sdlScaleRef = scaleRef
                    , sdlFontRef = fontRef
                    , sdlTextCache = cache
                    , sdlImages = images
                    , sdlCursors = cursors
                    , sdlDebug = debug
                    , sdlRetain = retain
                    }
        teardown env = do
          (tex, _, _) <- readIORef (sdlRetain env)
          retainDestroy tex
          destroyCursors (sdlCursors env)
          destroyImageAtlas (sdlImages env)
          destroyTextCache (sdlTextCache env)
          font <- readIORef (sdlFontRef env)
          closeFont font
          void $ stopTextInputSafe (sdlWindow env)
          void $ setRenderScale (sdlRenderer env) defaultUiScale
          destroyRendererSafe (sdlRenderer env)
          destroyWindowSafe (sdlWindow env)
          quitSafe
    bracket startup teardown $ \env -> do
      scale <- readIORef (sdlScaleRef env)
      font <- readIORef (sdlFontRef env)
      let ctx' = withSdlClipboard (withTtfMeasureScaled ctx font scale)
      setWakeLoop ctx' pushRefreshEvent
      act ctx' env

unlessM :: IO Bool -> IO () -> IO ()
unlessM p act = do
  ok <- p
  unless ok act
