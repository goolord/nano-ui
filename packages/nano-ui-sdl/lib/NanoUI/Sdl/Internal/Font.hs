-- | Desktop SDL_ttf font cache, system fallback discovery, bold-face lookup,
-- and SDL_Renderer glyph atlas integration built over "NanoUI.Ttf.Internal.Font".
module NanoUI.Sdl.Internal.Font
  ( FontSource (..)
  , withTtf
  , fontSourceLabel
  , embeddedFontSource
  , newGlyphAtlas
  , destroyGlyphAtlas
  , prepareGlyphAtlasForFrame
  , dropGlyphTextures
  , closeEvictedFonts
  , sdlPathEncoding
  , glyphAtlasFull
  , glyphAtlasHandle
  , SdlFontCache
  , newSdlFontCache
  , destroySdlFontCache
  , reloadSdlFontCache
  , sdlFontCacheSource
  , sdlFontCacheMonoSource
  , withSdlFontCache
  ) where

import Control.Exception (IOException, catch)
import Control.Monad (forM, when)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict qualified as Map
import Foreign.C.Types (CBool (..), CFloat (..))
import Foreign.Ptr (Ptr, nullPtr)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (TextEncoding, getFileSystemEncoding, mkTextEncoding)
import NanoUI.Sdl.Internal.Font.Search (boldFaceCandidates, searchFontFamilies)
import NanoUI.Ttf.Internal.Font
  ( AtlasOps (..)
  , FontHooks (..)
  , FontSource (..)
  , GlyphAtlas (..)
  , SdlFontCache (..)
  , closeEvictedFonts
  , destroyGlyphAtlas
  , destroySdlFontCache
  , embeddedFontSource
  , fontSourceLabel
  , glyphAtlasFull
  , glyphAtlasHandle
  , prepareGlyphAtlasForFrame
  , reloadSdlFontCache
  , sdlFontCacheMonoSource
  , sdlFontCacheSource
  , ttfCloseFont
  , ttfIsBoldOf
  , ttfOpenFont
  , withSdlFontCache
  )
import NanoUI.Ttf.Internal.Font qualified as Ttf
import NanoUI.Ttf.Internal.Support (CoveragePool, closeCoveragePool, ensurePoolCoverage, newCoveragePool)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import System.IO.Unsafe (unsafePerformIO)
import System.Info (os)

-- | Create a desktop SDL_Renderer-backed glyph atlas.
newGlyphAtlas :: Ptr SDL_Renderer -> IO GlyphAtlas
newGlyphAtlas ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed (glyph)"
  Ttf.newGlyphAtlas
    AtlasOps
      { aoHandle = atlas
      , aoDestroy = textAtlasDestroy atlas
      , aoReset = textAtlasReset atlas
      , aoInsertSurface = textAtlasInsertSurface atlas
      }

-- | Destroy the glyph pages' textures after a render device reset, scheduling
-- an atlas reset at the next frame start.
dropGlyphTextures :: SdlFontCache -> IO ()
dropGlyphTextures cache = do
  let ga = sfcGlyphAtlas cache
  textAtlasDropTextures (aoHandle (gaOps ga))
  writeIORef (gaFull ga) True

-- | Bracket an action with SDL_ttf initialisation, closing desktop coverage
-- probes before @TTF_Quit@.
withTtf :: IO a -> IO a
withTtf = Ttf.withTtf desktopHooks

-- | Open the base sans and mono fonts at the pixel density with desktop
-- fallback coverage and bold-face discovery enabled.
newSdlFontCache :: FontSource -> FontSource -> GlyphAtlas -> Float -> IORef Float -> IO SdlFontCache
newSdlFontCache sans mono ga basePt scaleRef = do
  boldFacesRef <- newIORef Map.empty
  let hooks = desktopHooks {fhBoldSource = findBoldFace boldFacesRef}
  Ttf.newSdlFontCache hooks sans mono ga basePt scaleRef

desktopHooks :: FontHooks
desktopHooks =
  FontHooks
    { fhEnsureCoverage = ensurePoolCoverage desktopCoveragePool
    , fhBoldSource = \_ _ -> pure Nothing
    , fhPathEncoding = sdlPathEncoding
    , fhOnTtfQuit = closeCoveragePool desktopCoveragePool
    }

-- | The encoding of paths SDL takes and gives: UTF-8 (with roundtrip) on
-- Windows, and the file system encoding elsewhere.
sdlPathEncoding :: IO TextEncoding
sdlPathEncoding
  | os == "mingw32" = mkTextEncoding "UTF-8//ROUNDTRIP"
  | otherwise = getFileSystemEncoding

findBoldFace :: IORef (Map.Map FilePath (Maybe FontSource)) -> FontSource -> IO (Ptr ()) -> IO (Maybe FontSource)
findBoldFace boldFacesRef src getRegular = case src of
  FontFromMemory {} -> pure Nothing
  FontFromPath path ->
    Map.lookup path <$> readIORef boldFacesRef >>= \case
      Just known -> pure known
      Nothing -> do
        regular <- getRegular
        found <- fmap FontFromPath <$> (firstM (isBoldOf regular) =<< boldFaceCandidates path)
        found <$ modifyIORef' boldFacesRef (Map.insert path found)
  where
    isBoldOf regular candidate = do
      face <- Ttf.openFontPath desktopHooks candidate 12
      if face == nullPtr
        then pure False
        else (/= 0) <$> ttfIsBoldOf face regular <* ttfCloseFont face
    firstM _ [] = pure Nothing
    firstM p (x : xs) = p x >>= \ok -> if ok then pure (Just x) else firstM p xs

{-# NOINLINE desktopCoveragePool #-}
desktopCoveragePool :: CoveragePool SBS.ShortByteString
desktopCoveragePool =
  unsafePerformIO $
    newCoveragePool findSources (\path -> SBS.useAsCString path (`ttfOpenFont` 12))
  where
    findSources = do
      files <- searchFontFamilies coverageFamilies `catch` \(_ :: IOException) -> pure []
      enc <- sdlPathEncoding
      forM files $ \path -> GHC.withCStringLen enc path (fmap SBS.toShort . BS.packCStringLen)

coverageFamilies :: [String]
coverageFamilies =
  [ "Noto Sans", "DejaVu Sans"
  , "Noto Sans Arabic", "Noto Sans Hebrew", "Noto Sans Devanagari", "Noto Sans Bengali"
  , "Noto Sans Tamil", "Noto Sans Telugu", "Noto Sans Gujarati", "Noto Sans Gurmukhi"
  , "Noto Sans Kannada", "Noto Sans Malayalam", "Noto Sans Sinhala", "Noto Sans Thai"
  , "Noto Sans Lao", "Noto Sans Khmer", "Noto Sans Myanmar", "Noto Sans Armenian"
  , "Noto Sans Georgian", "Noto Sans Ethiopic", "Noto Sans CJK", "Noto Sans CJK SC", "Noto Sans CJK JP"
  , "Noto Sans Symbols", "Noto Sans Symbols 2", "Noto Sans Math"
  , "Segoe UI", "Segoe UI Symbol", "Nirmala UI", "Leelawadee UI", "Microsoft YaHei"
  , "Yu Gothic", "Malgun Gothic", "Arial Unicode MS"
  , "Geeza Pro", "Kohinoor Devanagari", "Thonburi", "PingFang SC", "Hiragino Sans"
  , "Apple SD Gothic Neo", "Apple Symbols"
  ]

foreign import ccall unsafe "nano_ui_text_atlas_create"
  textAtlasCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_destroy"
  textAtlasDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_reset"
  textAtlasReset :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_drop_textures"
  textAtlasDropTextures :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  textAtlasInsertSurface :: Ptr () -> Ptr () -> Ptr CFloat -> IO CBool
