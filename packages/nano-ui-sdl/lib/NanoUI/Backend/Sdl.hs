-- | SDL3 backend: event loop, rendering, and application runners.
module NanoUI.Backend.Sdl
  ( RgbaImage (..)
  , SdlDebugSnapshot (..)
  , SdlEnv (..)
  , SdlOptions (..)
  , askSdlDebug
  , setSdlUiFont
  , isDebugActive
  , newSdlDebugSampler
  , readSdlDebug
  , takeDebugLive
  , defaultSdlOptions
  , FileFilter (..)
  , FileDialogOptions (..)
  , FileDialogId (..)
  , FileDialogResult (..)
  , defaultFileDialogOptions
  , openFileDialog
  , saveFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog
  , askOpenFileDialog
  , askSaveFileDialog
  , askOpenFolderDialog
  , pollFileDialogUi
  , newSdlContext
  , NanoUIFont (..)
  , listFontFamilies
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
import Foreign.C.String (withCString)
import Foreign.C.Types (CUInt)
import Foreign.Ptr (Ptr)
import NanoUI
  ( FontStyle
  , FontVariant
  , FontWeight
  , NanoUI
  )
import NanoUI.Sdl.Runner (askSdlDebug, drawReduceEff, newSdlContext, sdlDrawFrame, setSdlUiFont)
import NanoUI.Sdl.Session (runSdlSession)
import NanoUI.Sdl.Debug
  ( SdlDebugSnapshot (..)
  , isDebugActive
  , newSdlDebugSampler
  , readSdlDebug
  , takeDebugLive
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
import NanoUI.Sdl.Dialog
  ( FileDialogId (..)
  , FileDialogOptions (..)
  , FileDialogResult (..)
  , FileFilter (..)
  , askOpenFileDialog
  , askOpenFolderDialog
  , askSaveFileDialog
  , defaultFileDialogOptions
  , openFileDialog
  , openFolderDialog
  , pollFileDialog
  , cancelFileDialog
  , pollFileDialogUi
  , saveFileDialog
  )
import NanoUI.Sdl.NanoUIFont (NanoUIFont (..))
import NanoUI.Sdl.Font.Search (listFontFamilies)
import NanoUI.Testing (Context, registerImage, runEff, withTheme)

runSdlApp :: SdlOptions -> NanoUI () -> IO ()
runSdlApp options ui = do
  ctx <- sdlContext options
  runSdlSession options ctx (const (pure ())) (sdlAppShouldQuit options) $ \c ->
    sdlDrawFrame c ui

runSdlAppReduce ::
  (Typeable msg, Eq model) =>
  SdlOptions ->
  (msg -> model -> model) ->
  model ->
  (model -> NanoUI ()) ->
  IO ()
runSdlAppReduce options update model view = do
  ctx <- sdlContext options
  modelRef <- newIORef model
  runSdlSession options ctx (const (pure ())) (sdlAppShouldQuit options) $
    drawReduceEff runEff update modelRef view

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

-- | Run a native query on a codepoint pair of the cached font for a size and
-- style.
fontPairQuery ::
  (Ptr () -> CUInt -> CUInt -> IO a) ->
  SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Char -> Char -> IO a
fontPairQuery query env sz weight style var c1 c2 = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  query (sfFont (cfeFont entry)) (fromIntegral (ord c1)) (fromIntegral (ord c2))

saveFontRenderText :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Text -> FilePath -> IO Bool
saveFontRenderText env sz weight style var txt path = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  withUtf8 txt $ \ctext _ -> withCString path (ttfSaveRenderText (sfFont (cfeFont entry)) ctext)

queryFontKerning :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Char -> Char -> IO Int
queryFontKerning env sz weight style var c1 c2 =
  fromIntegral <$> fontPairQuery ttfGetKerning env sz weight style var c1 c2

queryFontPairKerning :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Char -> Char -> IO Int
queryFontPairKerning env sz weight style var c1 c2 =
  fromIntegral <$> fontPairQuery ttfGetPairKerning env sz weight style var c1 c2

debugFontPair :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Char -> Char -> IO ()
debugFontPair = fontPairQuery ttfDebugPair

dumpFontLayout :: SdlEnv -> Float -> FontWeight -> FontStyle -> FontVariant -> Text -> IO ()
dumpFontLayout env sz weight style var txt = do
  entry <- getOrLoadCachedFont (sdlFontCache env) sz weight style var
  withUtf8 txt $ \ctext _ -> ttfDumpLayout (sfFont (cfeFont entry)) ctext
