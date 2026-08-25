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
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import NanoUI (Context, Input (..), Size (..), markDirty, withExternalText)
import NanoUI.Sdl.Display
  ( defaultFontSize
  , defaultUiScale
  , queryMouseWindowPos
  , queryWindowDisplayScale
  , queryWindowLogicalSize
  , setRenderScale
  , windowToRenderCoords
  )
import NanoUI.Sdl.Font
  ( SdlFont
  , TextCache
  , closeFont
  , destroyTextCache
  , findFontPath
  , newTextCache
  , openFont
  , withTtf
  , withTtfMeasure
  )
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
  }

defaultWindowSize :: Size
defaultWindowSize = Size 960 640

-- Layout stays in logical coordinates; SDL render scale maps to pixels. Font stays
-- at the base point size so text is not double-scaled with the renderer.
syncDisplay :: Context -> SdlEnv -> Input -> IO (Context, Input)
syncDisplay ctx env inp = do
  scale <- queryWindowDisplayScale (sdlWindow env)
  ok <- setRenderScale (sdlRenderer env) scale
  unless ok $ fail "SDL_SetRenderScale failed"
  oldScale <- readIORef (sdlScaleRef env)
  when (abs (scale - oldScale) > scaleEpsilon) $ do
    writeIORef (sdlScaleRef env) scale
    markDirty ctx
  queried <- queryWindowLogicalSize (sdlWindow env) scale
  let winSize =
        case queried of
          Size 0 0 -> inputWindowSize inp
          s -> s
  inpSized <- syncInput env inp {inputWindowSize = winSize}
  pure (ctx, inpSized)

syncInput :: SdlEnv -> Input -> IO Input
syncInput env inp = do
  windowPos <-
    queryMouseWindowPos >>= \case
      Just pos -> pure pos
      Nothing -> pure (inputMousePos inp)
  pos <- windowToRenderCoords (sdlRenderer env) windowPos
  pure inp {inputMousePos = pos}

withSdl :: Context -> String -> Size -> (Context -> SdlEnv -> IO a) -> IO a
withSdl ctx title (Size w h) act =
  withTtf $ do
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
                font <- openFont fontPath defaultFontSize
                scaleRef <- newIORef scale
                fontRef <- newIORef font
                cache <- newTextCache
                unlessM (setRenderScale ren scale) $
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
                    }
        teardown env = do
          destroyTextCache (sdlTextCache env)
          font <- readIORef (sdlFontRef env)
          closeFont font
          void $ stopTextInputSafe (sdlWindow env)
          void $ setRenderScale (sdlRenderer env) defaultUiScale
          destroyRendererSafe (sdlRenderer env)
          destroyWindowSafe (sdlWindow env)
          quitSafe
    bracket startup teardown $ \env -> do
      font <- readIORef (sdlFontRef env)
      let ctx' = withExternalText (withTtfMeasure ctx font) True
      act ctx' env

unlessM :: IO Bool -> IO () -> IO ()
unlessM p act = do
  ok <- p
  unless ok act
