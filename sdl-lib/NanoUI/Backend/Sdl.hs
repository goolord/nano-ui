{-# LANGUAGE DataKinds #-}

-- | SDL3 backend: event loop, rendering, and application runners.
module NanoUI.Backend.Sdl
  ( RgbaImage (..)
  , SdlDebugSnapshot (..)
  , SdlEnv (..)
  , SdlOptions (..)
  , askSdlDebug
  , defaultSdlOptions
  , newSdlContext
  , NanoUIFont (..)
  , runSdlApp
  , runSdlAppReduce
  , sdlDrawFrame
  , syncDisplay
  , withSdl
  , withSdlBench
  ) where

import Control.Monad (unless)
import Data.Foldable (foldlM)
import Data.IORef (newIORef)
import Data.Primitive.SmallArray (SmallArray)
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, type (:>))
import NanoUI
  ( Input (..)
  , NanoUI
  , Ui
  )
import NanoUI.Sdl.Runner (askSdlDebug, drawEff, drawReduceEff, newSdlContext, runSdlSession, sdlDrawFrame)
import NanoUI.Sdl.Debug (SdlDebugSnapshot (..))
import NanoUI.Sdl.Window (RgbaImage (..), SdlEnv (..), SdlOptions (..), defaultSdlOptions, syncDisplay, withSdl, withSdlBench)
import NanoUI.Sdl.NanoUIFont (NanoUIFont (..))
import NanoUI.Testing (Context, registerImage, runEff, withTheme)

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

registerImages :: Context -> SmallArray RgbaImage -> IO Bool
registerImages ctx images =
  foldlM (\ok img -> if ok then registerRgbaImage ctx img else pure False) True images

registerRgbaImage :: Context -> RgbaImage -> IO Bool
registerRgbaImage ctx img =
  registerImage
    ctx
    (rgbaImageId img)
    (rgbaImageWidth img)
    (rgbaImageHeight img)
    (rgbaImagePixels img)

runSdlAppWithQuit :: SdlOptions -> Context -> (Input -> Bool) -> NanoUI () -> IO ()
runSdlAppWithQuit options = runSdlAppWithQuitEff options runEff

runSdlAppWithQuitEff ::
  IOE :> es =>
  SdlOptions ->
  (forall x. Eff es x -> IO x) ->
  Context ->
  (Input -> Bool) ->
  Eff (Ui : es) () ->
  IO ()
runSdlAppWithQuitEff options unlift ctx shouldQuit ui =
  runSdlSession options ctx (const (pure ())) shouldQuit $ \c env i force ->
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
runSdlAppWithQuitReduce options = runSdlAppWithQuitReduceEff options runEff

runSdlAppWithQuitReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  SdlOptions ->
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  model ->
  (Input -> Bool) ->
  (model -> Eff (Ui : es) ()) ->
  IO ()
runSdlAppWithQuitReduceEff options unlift update ctx model0 shouldQuit view = do
  modelRef <- newIORef model0
  runSdlSession options ctx (const (pure ())) shouldQuit $ \c env i force ->
    drawReduceEff unlift update modelRef view c env i force
