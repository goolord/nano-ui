{-# LANGUAGE DataKinds #-}

-- | SDL3 backend: event loop, rendering, and application runners.
module NanoUI.Backend.Sdl
  ( RgbaImage (..)
  , SdlDebugSnapshot (..)
  , SdlOptions (..)
  , askSdlDebug
  , defaultSdlOptions
  , runSdlApp
  , runSdlAppReduce
  ) where

import Control.Monad (unless)
import Data.ByteString (ByteString)
import Data.IORef (newIORef)
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, type (:>))
import NanoUI
  ( ImageId
  , Input (..)
  , NanoUI
  , Theme
  , Ui
  )
import NanoUI.Sdl.Runner (askSdlDebug, drawEff, drawReduceEff, newSdlContext, runSdlSession)
import NanoUI.Sdl.Debug (SdlDebugSnapshot (..))
import NanoUI.Testing (Context, registerImage, runEff, withTheme)

-- | Initial RGBA asset uploaded before the first frame.
data RgbaImage = RgbaImage
  { rgbaImageId :: !ImageId
  , rgbaImageWidth :: !Int
  , rgbaImageHeight :: !Int
  , rgbaImagePixels :: !ByteString
  }

-- | Application-owned SDL settings.
data SdlOptions = SdlOptions
  { sdlAppTheme :: !(Maybe Theme)
  , sdlAppShouldQuit :: !(Input -> Bool)
  , sdlAppImages :: ![RgbaImage]
  , sdlAppVsync :: !Bool
  }

defaultSdlOptions :: SdlOptions
defaultSdlOptions =
  SdlOptions
    { sdlAppTheme = Nothing
    , sdlAppShouldQuit = const False
    , sdlAppImages = []
    , sdlAppVsync = True
    }

runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = do
  ctx <- sdlContext options
  runSdlAppWithQuit options ctx (sdlAppShouldQuit options) ui

runSdlAppReduce ::
  (Typeable msg, Eq model) =>
  SdlOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppReduce options update model view = do
  ctx <- sdlContext options
  runSdlAppWithQuitReduce options update ctx model (sdlAppShouldQuit options) view

sdlContext :: SdlOptions -> IO Context
sdlContext options = do
  ctx0 <- newSdlContext
  let themed = maybe ctx0 (withTheme ctx0) (sdlAppTheme options)
  ok <- registerImages themed (sdlAppImages options)
  unless ok $ fail "registerImage failed"
  pure themed

registerImages :: Context -> [RgbaImage] -> IO Bool
registerImages ctx images =
  and <$> mapM (registerRgbaImage ctx) images

registerRgbaImage :: Context -> RgbaImage -> IO Bool
registerRgbaImage ctx img =
  registerImage
    ctx
    (rgbaImageId img)
    (rgbaImageWidth img)
    (rgbaImageHeight img)
    (rgbaImagePixels img)

runSdlAppWithQuit :: SdlOptions -> Context -> (Input -> Bool) -> NanoUI () -> IO ()
runSdlAppWithQuit options = runSdlAppWithQuitEff (sdlAppVsync options) runEff

runSdlAppWithQuitEff ::
  IOE :> es =>
  Bool ->
  (forall x. Eff es x -> IO x) ->
  Context ->
  (Input -> Bool) ->
  Eff (Ui : es) () ->
  IO ()
runSdlAppWithQuitEff vsync unlift ctx shouldQuit ui =
  runSdlSession vsync ctx (const (pure ())) shouldQuit $ \c env i force ->
    drawEff unlift c ui env i force

runSdlAppWithQuitReduce ::
  (Typeable msg, Eq model) =>
  SdlOptions ->
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppWithQuitReduce options = runSdlAppWithQuitReduceEff (sdlAppVsync options) runEff

runSdlAppWithQuitReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  Bool ->
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> Eff (Ui : es) ()) ->
  IO ()
runSdlAppWithQuitReduceEff vsync unlift update ctx model0 shouldQuit view = do
  modelRef <- newIORef model0
  runSdlSession vsync ctx (const (pure ())) shouldQuit $ \c env i force ->
    drawReduceEff unlift update modelRef view c env i force
