module NanoUI.Sdl.Window
  ( SdlEnv (..)
  , withSdl
  , defaultWindowSize
  ) where

import Control.Exception (bracket)
import Control.Monad (unless, void)
import Foreign.C.String (withCString)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import NanoUI (Size (..))
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

data SdlEnv = SdlEnv
  { sdlWindow :: Ptr SDL_Window
  , sdlRenderer :: Ptr SDL_Renderer
  , sdlSize :: Size
  }
  deriving (Show)

defaultWindowSize :: Size
defaultWindowSize = Size 960 640

withSdl :: String -> Size -> (SdlEnv -> IO a) -> IO a
withSdl title (Size w h) act =
  bracket startup teardown act
  where
    startup = do
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
            _ <- startTextInputSafe win
            pure
              SdlEnv
                { sdlWindow = win
                , sdlRenderer = ren
                , sdlSize = Size w h
                }
    teardown env = do
      void $ stopTextInputSafe (sdlWindow env)
      destroyRendererSafe (sdlRenderer env)
      destroyWindowSafe (sdlWindow env)
      quitSafe

unlessM :: IO Bool -> IO () -> IO ()
unlessM p act = do
  ok <- p
  unless ok act
