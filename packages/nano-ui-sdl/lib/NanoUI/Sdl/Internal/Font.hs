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
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.Char (isPrint, ord)
import Data.Foldable (traverse_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Map.Strict qualified as Map
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, sizeofSmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
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
  , SdlFont (..)
  , SdlFontCache (..)
  , attachFallback
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
  , ttfHasGlyph
  , ttfIsBoldOf
  , ttfOpenFont
  , withSdlFontCache
  )
import NanoUI.Ttf.Internal.Font qualified as Ttf
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
-- probes before 'TTF_Quit'.
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
    { fhEnsureCoverage = ensureCoverage
    , fhBoldSource = \_ _ -> pure Nothing
    , fhPathEncoding = sdlPathEncoding
    , fhOnTtfQuit = closeCoverageProbes
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

-- | Open fallback fonts covering characters this face lacks.
ensureCoverage :: SdlFont -> Text -> IO ()
ensureCoverage sf txt =
  unless (T.isAscii txt) $
    forM_ (T.unpack txt) $ \c ->
      when (ord c >= 128 && isPrint c) $ do
        has <- ttfHasGlyph (sfFont sf) (fromIntegral (ord c))
        unless has $ coverageSourceFor c >>= traverse_ (uncurry (attachFallback sf))

data Coverage = Coverage
  { covSources :: !(SmallArray SBS.ShortByteString)
  , covProbes :: !(IM.IntMap (Ptr ()))
  , covChars :: !(IM.IntMap (Maybe Int))
  }

{-# NOINLINE coverageRef #-}
coverageRef :: IORef (Maybe Coverage)
coverageRef = unsafePerformIO (newIORef Nothing)

coverageSourceFor :: Char -> IO (Maybe (Int, Ptr ()))
coverageSourceFor c = do
  cov <- readIORef coverageRef >>= maybe findSources pure
  let cp = ord c
      search probes i
        | i >= sizeofSmallArray (covSources cov) = pure (Nothing, probes)
        | otherwise = do
            let open = SBS.useAsCString (indexSmallArray (covSources cov) i) (`ttfOpenFont` 12)
            probe <- maybe open pure (IM.lookup i probes)
            has <- if probe == nullPtr then pure False else ttfHasGlyph probe (fromIntegral cp)
            let probes' = IM.insert i probe probes
            if has then pure (Just i, probes') else search probes' (i + 1)
  (source, probes) <- case IM.lookup cp (covChars cov) of
    Just known -> pure (known, covProbes cov)
    Nothing -> do
      found@(first, opened) <- search (covProbes cov) 0
      found <$ writeIORef coverageRef (Just cov {covProbes = opened, covChars = IM.insert cp first (covChars cov)})
  pure (source >>= \i -> (i,) <$> IM.lookup i probes)
  where
    findSources = do
      files <- searchFontFamilies coverageFamilies `catch` \(_ :: IOException) -> pure []
      enc <- sdlPathEncoding
      sources <- forM files $ \path -> GHC.withCStringLen enc path (fmap SBS.toShort . BS.packCStringLen)
      pure (Coverage (smallArrayFromList sources) IM.empty IM.empty)

closeCoverageProbes :: IO ()
closeCoverageProbes =
  readIORef coverageRef >>= \case
    Nothing -> pure ()
    Just cov -> do
      forM_ (covProbes cov) $ \probe -> unless (probe == nullPtr) (ttfCloseFont probe)
      writeIORef coverageRef (Just cov {covProbes = IM.empty, covChars = IM.empty})

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
