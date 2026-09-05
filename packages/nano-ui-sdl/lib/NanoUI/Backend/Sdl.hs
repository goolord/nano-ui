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
  , saveScreenshot
  , saveFontRenderText
  , queryFontKerning
  , queryFontPairKerning
  , debugFontPair
  , dumpFontLayout
  ) where

import Control.Monad (unless)
import Data.Char (ord)
import Data.Foldable (foldlM)
import Data.IORef (newIORef)
import Data.Primitive.SmallArray (SmallArray)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, type (:>))
import Foreign.C.String (withCString)
import NanoUI
  ( FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , Input (..)
  , NanoUI
  , Ui
  )
import NanoUI.Sdl.Runner (askSdlDebug, drawEff, drawReduceEff, newSdlContext, runSdlSession, sdlDrawFrame)
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  )
import NanoUI.Sdl.Font
  ( CachedFontEntry (..)
  , SdlFont (..)
  , getOrLoadCachedFont
  , ttfDumpLayout
  , ttfGetKerning
  , ttfGetPairKerning
  , ttfDebugPair
  , ttfSaveRenderText
  , withUtf8
  )
import NanoUI.Sdl.Window (RgbaImage (..), SdlEnv (..), SdlOptions (..), defaultSdlOptions, saveScreenshot, syncDisplay, withSdl, withSdlBench)
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
  themed <- maybe (pure ctx0) (withTheme ctx0) (sdlAppTheme options)
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

saveFontRenderText :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Text -> FilePath -> IO Bool
saveFontRenderText env sz weight style var txt path = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  withUtf8 txt $ \ctext _ ->
    withCString path $ \cpath ->
      ttfSaveRenderText (sfFont (cfeFont entry)) ctext cpath

queryFontKerning :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Char -> Char -> IO Int
queryFontKerning env sz weight style var c1 c2 = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  let cp1 = fromIntegral (ord c1)
      cp2 = fromIntegral (ord c2)
  fromIntegral <$> ttfGetKerning (sfFont (cfeFont entry)) cp1 cp2

queryFontPairKerning :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Char -> Char -> IO Int
queryFontPairKerning env sz weight style var c1 c2 = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  let cp1 = fromIntegral (ord c1)
      cp2 = fromIntegral (ord c2)
  fromIntegral <$> ttfGetPairKerning (sfFont (cfeFont entry)) cp1 cp2

debugFontPair :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Char -> Char -> IO ()
debugFontPair env sz weight style var c1 c2 = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  let cp1 = fromIntegral (ord c1)
      cp2 = fromIntegral (ord c2)
  ttfDebugPair (sfFont (cfeFont entry)) cp1 cp2

dumpFontLayout :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO ()
dumpFontLayout env sz weight style var txt = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  withUtf8 txt $ \ctext _ -> ttfDumpLayout (sfFont (cfeFont entry)) ctext
