-- | Browser SDL_ttf font cache and in-memory glyph atlas integration built
-- over "NanoUI.Ttf.Internal.Font".
module NanoUI.SdlWeb.Internal.Font
  ( FontSource (..)
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

import Control.Monad (forM, when)
import Data.IORef (IORef)
import Data.Maybe (catMaybes)
import Data.Word (Word8)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..))
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr (Ptr, nullPtr)
import NanoUI.Backend (glyphAtlasPages)
import NanoUI.Ttf.Internal.Font
  ( AtlasOps (..)
  , FontSource (..)
  , GlyphAtlas (..)
  , SdlFontCache (..)
  , closeEvictedFonts
  , defaultFontHooks
  , destroyGlyphAtlas
  , destroySdlFontCache
  , embeddedFontSource
  , fontSourceLabel
  , glyphAtlasFull
  , glyphAtlasSize
  , prepareGlyphAtlasForFrame
  , reloadSdlFontCache
  , sdlFontCacheSource
  , withSdlFontCache
  )
import NanoUI.Ttf.Internal.Font qualified as Ttf

-- | Bracket an action with SDL_ttf initialisation.
withTtf :: IO a -> IO a
withTtf = Ttf.withTtf defaultFontHooks

-- | Open the base sans and mono fonts at the pixel density.
newSdlFontCache :: FontSource -> FontSource -> GlyphAtlas -> Float -> IORef Float -> IO SdlFontCache
newSdlFontCache = Ttf.newSdlFontCache defaultFontHooks

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
