-- | Browser SDL_ttf font cache and in-memory glyph atlas integration built
-- over "NanoUI.Ttf.Internal.Font", with fallback fonts for the characters
-- the UI font lacks: the page has no system fonts to find them in, so they
-- are fonts the session loaded at start.
module NanoUI.SdlWeb.Internal.Font
  ( FontSource (..)
  , WebFallbacks
  , newWebFallbacks
  , webFontHooks
  , withTtf
  , fontSourceLabel
  , embeddedFontSource
  , GlyphAtlas
  , newGlyphAtlas
  , destroyGlyphAtlas
  , prepareGlyphAtlasForFrame
  , markGlyphPagesDirty
  , glyphPageUploads
  , closeEvictedFonts
  , glyphAtlasFull
  , glyphAtlasSize
  , SdlFontCache
  , sfcGlyphAtlas
  , newSdlFontCache
  , destroySdlFontCache
  , reloadSdlFontCache
  , sdlFontCacheSource
  , withSdlFontCache
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.ByteString (ByteString)
import Data.Char (isPrint, ord)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (catMaybes)
import Data.Word (Word8)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..))
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI.Backend (glyphAtlasPages)
import NanoUI.Ttf.Internal.Font
  ( AtlasOps (..)
  , FontHooks (..)
  , FontSource (..)
  , GlyphAtlas (..)
  , SdlFont (..)
  , SdlFontCache (..)
  , attachFallback
  , closeEvictedFonts
  , defaultFontHooks
  , destroyGlyphAtlas
  , destroySdlFontCache
  , embeddedFontSource
  , fontSourceLabel
  , glyphAtlasFull
  , glyphAtlasSize
  , openFontBytes
  , prepareGlyphAtlasForFrame
  , reloadSdlFontCache
  , sdlFontCacheSource
  , ttfCloseFont
  , ttfHasGlyph
  , withSdlFontCache
  )
import NanoUI.Ttf.Internal.Font qualified as Ttf

-- | The fallback fonts: each one's bytes, which SDL_ttf reads for as long as
-- the session runs, a probe of each opened when a character first needs
-- it, and which fallback covers each character asked about so far.
data WebFallbacks = WebFallbacks
  { wfSources :: ![ByteString]
  , wfProbes :: !(IORef (IM.IntMap (Ptr ())))
  , wfChars :: !(IORef (IM.IntMap (Maybe Int)))
  }

-- | Fallbacks from font files' bytes, tried in order.
newWebFallbacks :: [ByteString] -> IO WebFallbacks
newWebFallbacks sources = WebFallbacks sources <$> newIORef IM.empty <*> newIORef IM.empty

-- | Font hooks that attach a fallback to a face for each character it
-- lacks, as nano-ui-sdl does with the system's fonts, and close the probes
-- before SDL_ttf shuts down.
webFontHooks :: WebFallbacks -> FontHooks
webFontHooks wf = defaultFontHooks {fhEnsureCoverage = ensureCoverage wf, fhOnTtfQuit = closeProbes wf}

ensureCoverage :: WebFallbacks -> SdlFont -> Text -> IO ()
ensureCoverage wf sf txt =
  unless (null (wfSources wf) || T.isAscii txt) $
    forM_ (T.unpack txt) $ \c ->
      when (ord c >= 128 && isPrint c) $ do
        has <- ttfHasGlyph (sfFont sf) (fromIntegral (ord c))
        unless has $ fallbackFor wf (ord c) >>= traverse_ (uncurry (attachFallback sf))

-- | The first fallback with a glyph for a codepoint, and its probe.
fallbackFor :: WebFallbacks -> Int -> IO (Maybe (Int, Ptr ()))
fallbackFor wf cp =
  IM.lookup cp <$> readIORef (wfChars wf) >>= \case
    Just known -> traverse withProbe known
    Nothing -> do
      found <- search (zip [0 ..] (wfSources wf))
      modifyIORef' (wfChars wf) (IM.insert cp (fst <$> found))
      pure found
  where
    search [] = pure Nothing
    search ((i, _) : rest) = do
      (_, probe) <- withProbe i
      has <- if probe == nullPtr then pure False else ttfHasGlyph probe (fromIntegral cp)
      if has then pure (Just (i, probe)) else search rest
    withProbe i =
      IM.lookup i <$> readIORef (wfProbes wf) >>= \case
        Just probe -> pure (i, probe)
        Nothing -> do
          probe <- openFontBytes (wfSources wf !! i) 12
          modifyIORef' (wfProbes wf) (IM.insert i probe)
          pure (i, probe)

closeProbes :: WebFallbacks -> IO ()
closeProbes wf = do
  readIORef (wfProbes wf) >>= mapM_ (\p -> unless (p == nullPtr) (ttfCloseFont p))
  writeIORef (wfProbes wf) IM.empty

-- | Bracket an action with SDL_ttf initialisation, closing the hooks'
-- probes before it shuts down.
withTtf :: FontHooks -> IO a -> IO a
withTtf = Ttf.withTtf

-- | Open the base sans and mono fonts at the pixel density, with the hooks'
-- fallbacks.
newSdlFontCache :: FontHooks -> FontSource -> FontSource -> GlyphAtlas -> Float -> IORef Float -> IO SdlFontCache
newSdlFontCache = Ttf.newSdlFontCache

-- | Create a CPU-backed glyph atlas whose dirty sub-rects upload to WebGL2.
newGlyphAtlas :: IO GlyphAtlas
newGlyphAtlas = do
  atlas <- textAtlasCreate
  when (atlas == nullPtr) $ fail "nano_ui_web_atlas_create failed (glyph)"
  Ttf.newGlyphAtlas
    AtlasOps
      { aoHandle = atlas
      , aoDestroy = textAtlasDestroy atlas
      , aoReset = textAtlasReset atlas
      , aoInsertSurface = textAtlasInsertSurface atlas
      }

-- | The glyph pages changed since the last call: each page's number, its
-- pixels ('glyphAtlasSize' square, RGBA, 4 bytes a pixel) and the changed
-- rect as x, y, width and height.
glyphPageUploads :: GlyphAtlas -> IO [(Int, Ptr Word8, (Int, Int, Int, Int))]
glyphPageUploads ga = fmap catMaybes . forM [0 .. glyphAtlasPages - 1] $ \page ->
  allocaArray 4 $ \out -> do
    let handle = aoHandle (gaOps ga)
    changed <- textAtlasTakeDirty handle (fromIntegral page) out
    if changed == 0
      then pure Nothing
      else do
        rect <- peekArray 4 out
        pixels <- textAtlasPixels handle (fromIntegral page)
        pure $ case map fromIntegral rect of
          [x, y, w, h] -> Just (page, pixels, (x, y, w, h))
          _ -> Nothing

-- | Upload every open page in full with the next 'glyphPageUploads', after
-- the GPU lost its textures.
markGlyphPagesDirty :: GlyphAtlas -> IO ()
markGlyphPagesDirty = textAtlasMarkAllDirty . aoHandle . gaOps

foreign import ccall unsafe "nano_ui_web_atlas_create"
  textAtlasCreate :: IO (Ptr ())

foreign import ccall unsafe "nano_ui_web_atlas_destroy"
  textAtlasDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_web_atlas_reset"
  textAtlasReset :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_web_atlas_pixels"
  textAtlasPixels :: Ptr () -> CInt -> IO (Ptr Word8)

foreign import ccall unsafe "nano_ui_web_atlas_take_dirty"
  textAtlasTakeDirty :: Ptr () -> CInt -> Ptr CInt -> IO CBool

foreign import ccall unsafe "nano_ui_web_atlas_mark_all_dirty"
  textAtlasMarkAllDirty :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_web_atlas_insert_surface"
  textAtlasInsertSurface :: Ptr () -> Ptr () -> Ptr CFloat -> IO CBool
