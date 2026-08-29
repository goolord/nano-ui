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
  }

defaultSdlOptions :: SdlOptions
defaultSdlOptions =
  SdlOptions
    { sdlAppTheme = Nothing
    , sdlAppShouldQuit = const False
    , sdlAppImages = []
    }

runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = do
  ctx <- sdlContext options
  runSdlAppWithQuit ctx (sdlAppShouldQuit options) ui

runSdlAppReduce ::
  (Typeable msg, Eq model) =>
  SdlOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppReduce options update model view = do
  ctx <- sdlContext options
  runSdlAppWithQuitReduce update ctx model (sdlAppShouldQuit options) view

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

runSdlAppWithQuit :: Context -> (Input -> Bool) -> NanoUI () -> IO ()
runSdlAppWithQuit = runSdlAppWithQuitEff runEff

runSdlAppWithQuitEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  (Input -> Bool) ->
  Eff (Ui : es) () ->
  IO ()
runSdlAppWithQuitEff unlift ctx shouldQuit ui =
  runSdlSession ctx (const (pure ())) shouldQuit $ \c env i force ->
    drawEff unlift c ui env i force

runSdlAppWithQuitReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppWithQuitReduce = runSdlAppWithQuitReduceEff runEff

runSdlAppWithQuitReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> Eff (Ui : es) ()) ->
  IO ()
runSdlAppWithQuitReduceEff unlift update ctx model0 shouldQuit view = do
  modelRef <- newIORef model0
  runSdlSession ctx (const (pure ())) shouldQuit $ \c env i force ->
    drawReduceEff unlift update modelRef view c env i force
