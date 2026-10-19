-- | What the SDL demo executables and their self-tests share: a hidden window
-- to drive a UI on.
module DemoApp
  ( withHiddenWindow
  ) where

import NanoUI (Input (..), Size (..), V2 (..), WindowMode (..), WindowSettings (..), defaultWindowSettings)
import NanoUI.Backend (emptyInput)
import NanoUI.Backend.Sdl
  ( SdlEnv
  , SdlOptions (..)
  , defaultSdlOptions
  , withSdl
  )
import NanoUI.Testing (Context, newPixelContext)

-- | Run @k@ on a hidden, fixed @w@ by @h@ SDL window over a pixel-snapped
-- context, with @opts@ applied to the window options. @k@ also gets the
-- event-free input for that size with the pointer at @mouse@.
withHiddenWindow ::
  Float ->
  Float ->
  V2 ->
  (SdlOptions -> SdlOptions) ->
  (Context -> SdlEnv -> Input -> IO a) ->
  IO a
withHiddenWindow w h mouse opts k = do
  ctx0 <- newPixelContext
  let base = defaultSdlOptions {sdlWindowSettings = defaultWindowSettings {wsMode = Hidden, wsSize = Size w h, wsResizable = False}}
  withSdl (opts base) ctx0 $ \ctx env ->
    k ctx env emptyInput {inputWindowSize = Size w h, inputMousePos = mouse}
