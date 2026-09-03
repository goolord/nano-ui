module NanoUI.Sdl.Font
  ( SdlFont (..)
  , FontSource (..)
  , GlyphAtlas
  , withTtf
  , openFont
  , openFontFromMemory
  , openFontSource
  , openFontSourceWithFallback
  , closeFont
  , fontSourceLabel
  , newGlyphAtlas
  , destroyGlyphAtlas
  , resetGlyphAtlas
  , warmGlyphAtlas
  , withTtfMeasure
  , withTtfMeasureScaled
  , withTtfMeasureGlyph
  , ttfFontMetricsScaled
  , buildGlyphFontMetrics
  , measureTtfText
  , glyphAtlasTexture
  ) where

import Control.Exception (SomeException, bracket, catch, throwIO)
import Control.Monad (when)
import Data.Char (ord)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Primitive.SmallArray
  ( indexSmallArray
  , smallArrayFromList
  )
import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUInt (..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import GHC.IO (unsafePerformIO)
import qualified Data.ByteString as BS
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import NanoUI (FontMetrics (..), GlyphQuad (..), monospaceMetrics)
import NanoUI.Testing
  ( Context
  , withExternalText
  , withFontMetrics
  , withMeasureText
  , withMonoFontMetrics
  , wrapMeasureCache
  )
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import qualified Data.Map.Strict as Map
import qualified Data.Text.Foreign as TF

data SdlFont = SdlFont
  { sfFont :: Ptr ()
  , sfLineSkip :: Float
  , sfAscent :: Float
  , sfSpaceAdvance :: Float
  , sfPath :: FilePath
  , sfTempPath :: !(Maybe FilePath)
  }

data FontSource
  = FontFromPath !FilePath
  | FontFromMemory !ByteString !FilePath
  deriving (Show)

fontSourceLabel :: FontSource -> FilePath
fontSourceLabel (FontFromPath p) = p
fontSourceLabel (FontFromMemory _ label) = label

-- | Per-glyph atlas slot. UVs are normalised to [0,1] within the atlas texture.
data GlyphSlot = GlyphSlot
  { gsW :: {-# UNPACK #-} !Float -- pixel width of glyph image
  , gsH :: {-# UNPACK #-} !Float -- pixel height of glyph image
  , gsU0 :: {-# UNPACK #-} !Float
  , gsV0 :: {-# UNPACK #-} !Float
  , gsU1 :: {-# UNPACK #-} !Float
  , gsV1 :: {-# UNPACK #-} !Float
  , gsOffX :: {-# UNPACK #-} !Float -- bearing x (pixels, at font scale)
  , gsOffY :: {-# UNPACK #-} !Float -- bearing y (pixels, at font scale)
  , gsAdvX :: {-# UNPACK #-} !Float -- horizontal advance (pixels, at font scale)
  }

-- | (font pointer, Unicode codepoint)
type GlyphKey = (Ptr (), Char)

data GlyphAtlas = GlyphAtlas
  { gaAtlas :: !(Ptr ())
  , gaEntries :: !(IORef (Map.Map GlyphKey GlyphSlot))
  }

{-# NOINLINE measureScratch #-}
measureScratch :: ForeignPtr CFloat
measureScratch = unsafePerformIO (mallocForeignPtrBytes (2 * sizeOf (0 :: CFloat)))

{-# NOINLINE surfaceScratch #-}
surfaceScratch :: ForeignPtr (Ptr ())
surfaceScratch = unsafePerformIO (mallocForeignPtrBytes (sizeOf (nullPtr :: Ptr ())))

{-# NOINLINE insertScratch #-}
insertScratch :: ForeignPtr CFloat
insertScratch = unsafePerformIO (mallocForeignPtrBytes (4 * sizeOf (0 :: CFloat)))

{-# NOINLINE glyphMetricsScratch #-}
glyphMetricsScratch :: ForeignPtr CInt
glyphMetricsScratch = unsafePerformIO (mallocForeignPtrBytes (5 * sizeOf (0 :: CInt)))

newGlyphAtlas :: Ptr SDL_Renderer -> IO GlyphAtlas
newGlyphAtlas ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed (glyph)"
  entries <- newIORef Map.empty
  pure GlyphAtlas {gaAtlas = atlas, gaEntries = entries}

destroyGlyphAtlas :: GlyphAtlas -> IO ()
destroyGlyphAtlas ga = textAtlasDestroy (gaAtlas ga)

resetGlyphAtlas :: GlyphAtlas -> IO ()
resetGlyphAtlas ga = do
  writeIORef (gaEntries ga) Map.empty
  textAtlasReset (gaAtlas ga)

-- | Pre-rasterise printable ASCII into the glyph atlas to avoid cold misses
-- on the first rendered frame.
warmGlyphAtlas :: GlyphAtlas -> SdlFont -> IO ()
warmGlyphAtlas ga sf =
  mapM_ (\c -> lookupOrInsertGlyph ga sf c) [' ' .. '~']

-- | Look up or insert a glyph into the atlas.  Returns 'Nothing' for
-- characters that have no glyph (e.g. control characters).
lookupOrInsertGlyph :: GlyphAtlas -> SdlFont -> Char -> IO (Maybe GlyphSlot)
lookupOrInsertGlyph ga sf c = do
  let !key = (sfFont sf, c)
  entries <- readIORef (gaEntries ga)
  case Map.lookup key entries of
    Just slot -> pure (Just slot)
    Nothing   -> insertGlyph ga sf key c

insertGlyph :: GlyphAtlas -> SdlFont -> GlyphKey -> Char -> IO (Maybe GlyphSlot)
insertGlyph ga sf key c = do
  let !cp = fromIntegral (ord c) :: CUInt
  mMetrics <- withForeignPtr glyphMetricsScratch $ \p -> do
    let pMinX = p
        pMaxX = plusPtr pMinX (sizeOf (0 :: CInt))
        pMinY = plusPtr pMaxX (sizeOf (0 :: CInt))
        pMaxY = plusPtr pMinY (sizeOf (0 :: CInt))
        pAdv  = plusPtr pMaxY (sizeOf (0 :: CInt))
    ok <- ttfGlyphMetrics (sfFont sf) cp pMinX pMaxX pMinY pMaxY pAdv
    if not ok
      then pure Nothing
      else do
        minX <- peek pMinX
        _maxX <- peek pMaxX
        _minY <- peek pMinY
        maxY <- peek pMaxY
        adv  <- peek pAdv
        pure (Just (fromIntegral minX, fromIntegral maxY, fromIntegral adv))
  case mMetrics of
    Nothing -> pure Nothing
    Just (minX, maxY, adv) -> do
      mSurf <- withForeignPtr surfaceScratch $ \sp -> do
        poke sp nullPtr
        ok <- ttfRenderGlyphSurface (sfFont sf) cp sp
        if not ok
          then pure Nothing
          else do
            surf <- peek sp
            if surf == nullPtr then pure Nothing else pure (Just surf)
      case mSurf of
        Nothing   -> pure Nothing
        Just surf -> do
          -- Try to insert; if atlas is full, evict all entries and retry.
          mPos <- tryInsert (gaAtlas ga) surf >>= \case
            Just p  -> pure (Just p)
            Nothing -> do
              writeIORef (gaEntries ga) Map.empty
              textAtlasReset (gaAtlas ga)
              tryInsert (gaAtlas ga) surf
          freeSurface surf
          case mPos of
            Nothing -> pure Nothing
            Just (px, py, tw, th) -> do
              (atW, atH) <- atlasSize (gaAtlas ga)
              -- TTF_GetGlyphImage is a tight bitmap. Place it with the font
              -- bearings: pen + minX, lineTop + (ascent - maxY). Do not clamp
              -- minX; monospace glyphs are often centered (minX > 0).
              let !offX = minX
                  !offY = sfAscent sf - maxY
                  !slot =
                    GlyphSlot
                      { gsW    = tw
                      , gsH    = th
                      , gsU0   = px / atW
                      , gsV0   = py / atH
                      , gsU1   = (px + tw) / atW
                      , gsV1   = (py + th) / atH
                      , gsOffX = offX
                      , gsOffY = offY
                      , gsAdvX = adv
                      }
              modifyIORef' (gaEntries ga) (Map.insert key slot)
              pure (Just slot)

-- | Build a 'FontMetrics' that populates 'fmGlyph' from the glyph atlas,
-- so 'pushText' can emit real textured quads.  This must be called after
-- the atlas has been warmed (or lazily, as missing glyphs are inserted on
-- first use).  The returned metrics work at *logical* (unscaled) coordinates;
-- scale is the display pixel ratio already baked into the font's point size.
--
-- Standard ASCII (0..127) lookups are backed by a high-performance 'SmallArray'
-- fast path for branchless O(1) in-memory indexing without Map traversal or IORef reads.
buildGlyphFontMetrics :: GlyphAtlas -> SdlFont -> Float -> FontMetrics
buildGlyphFontMetrics ga sf scale =
  let !inv = if scale > 0 then scale else 1
      baseFm = ttfFontMetricsScaled sf scale

      -- Precompute ASCII 0..127 fast lookup arrays backed by SmallArray
      !asciiSlots = unsafePerformIO $
        mapM (\i -> lookupOrInsertGlyph ga sf (toEnum i)) [0 .. 127]

      !asciiQuads =
        smallArrayFromList
          [ case mSlot of
              Nothing -> Nothing
              Just gs ->
                Just
                  GlyphQuad
                    { gqX  = gsOffX gs / inv
                    , gqY  = gsOffY gs / inv
                    , gqW  = gsW    gs / inv
                    , gqH  = gsH    gs / inv
                    , gqU0 = gsU0   gs
                    , gqV0 = gsV0   gs
                    , gqU1 = gsU1   gs
                    , gqV1 = gsV1   gs
                    }
          | mSlot <- asciiSlots
          ]

      !asciiAdvances =
        smallArrayFromList
          [ case mSlot of
              Nothing -> sfSpaceAdvance sf / inv
              Just gs -> gsAdvX gs / inv
          | mSlot <- asciiSlots
          ]

      {-# INLINE glyphLookup #-}
      glyphLookup !c =
        let !cp = ord c
         in if (fromIntegral cp :: Word) < 128
              then indexSmallArray asciiQuads cp
              else unsafePerformIO $ do
                mSlot <- lookupOrInsertGlyph ga sf c
                case mSlot of
                  Nothing -> pure Nothing
                  Just gs ->
                    pure $
                      Just
                        GlyphQuad
                          { gqX  = gsOffX gs / inv
                          , gqY  = gsOffY gs / inv
                          , gqW  = gsW    gs / inv
                          , gqH  = gsH    gs / inv
                          , gqU0 = gsU0   gs
                          , gqV0 = gsV0   gs
                          , gqU1 = gsU1   gs
                          , gqV1 = gsV1   gs
                          }

      {-# INLINE advanceLookup #-}
      advanceLookup !c =
        let !cp = ord c
         in if (fromIntegral cp :: Word) < 128
              then indexSmallArray asciiAdvances cp
              else unsafePerformIO $ do
                mSlot <- lookupOrInsertGlyph ga sf c
                pure $! case mSlot of
                  Nothing -> sfSpaceAdvance sf / inv
                  Just gs -> gsAdvX gs / inv
   in baseFm
        { fmGlyph   = glyphLookup
        , fmAdvance = advanceLookup
        }

-- | Return the SDL_Texture backing the glyph atlas, for passing to the renderer.
glyphAtlasTexture :: GlyphAtlas -> IO (Ptr ())
glyphAtlasTexture ga = textAtlasTexture (gaAtlas ga)

-- ---------------------------------------------------------------------------
withTtf :: IO a -> IO a
withTtf act =
  bracket startup shutdown $ \_ -> act
  where
    startup = do
      ok <- ttfInit
      when (not ok) $ fail "TTF_Init failed"
    shutdown _ = ttfQuit

openFont :: FilePath -> Float -> IO SdlFont
openFont path ptsize =
  withCString path $ \cpath -> do
    font <- ttfOpenFont cpath (realToFrac ptsize)
    when (font == nullPtr) $
      fail ("TTF_OpenFont failed for " ++ path)
    readSdlFont path Nothing font

openFontFromMemory :: ByteString -> FilePath -> Float -> IO SdlFont
openFontFromMemory bs label ptsize =
  unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    (fontPtr, mTemp) <-
      ttfOpenFontMemory (castPtr ptr) (fromIntegral len) (realToFrac ptsize) >>= \f ->
        if f /= nullPtr
          then pure (f, Nothing)
          else openFontFromMemoryTemp bs ptsize
    when (fontPtr == nullPtr) $
      fail ("TTF_OpenFont failed for in-memory font " ++ label)
    readSdlFont label mTemp fontPtr

openFontFromMemoryTemp :: ByteString -> Float -> IO (Ptr (), Maybe FilePath)
openFontFromMemoryTemp bs openPt = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "nano-ui-font-"
  BS.hPut h bs
  hClose h
  withCString path $ \cpath -> do
    font <- ttfOpenFont cpath (realToFrac openPt)
    if font == nullPtr
      then removeFile path >> pure (nullPtr, Nothing)
      else pure (font, Just path)

readSdlFont :: FilePath -> Maybe FilePath -> Ptr () -> IO SdlFont
readSdlFont path mTemp font = do
  lineSkip <- ttfLineSkip font
  ascent <- ttfAscent font
  spaceAdv <- ttfSpaceAdvance font
  pure
    SdlFont
      { sfFont = font
      , sfLineSkip = realToFrac lineSkip
      , sfAscent = realToFrac ascent
      , sfSpaceAdvance = realToFrac spaceAdv
      , sfPath = path
      , sfTempPath = mTemp
      }

openFontSource :: FontSource -> Float -> IO SdlFont
openFontSource (FontFromPath path) ptsize = openFont path ptsize
openFontSource (FontFromMemory bs label) ptsize =
  openFontFromMemory bs label ptsize

openFontSourceWithFallback :: FontSource -> FontSource -> Float -> IO SdlFont
openFontSourceWithFallback primary fallback ptsize =
  openFontSource primary ptsize
    `catch` \(e :: SomeException) ->
      if fontSourcesSame primary fallback
        then throwIO e
        else openFontSource fallback ptsize
          `catch` \(_ :: SomeException) -> throwIO e

fontSourcesSame :: FontSource -> FontSource -> Bool
fontSourcesSame (FontFromPath a) (FontFromPath b) = a == b
fontSourcesSame (FontFromMemory _ la) (FontFromMemory _ lb) = la == lb
fontSourcesSame _ _ = False

closeFont :: SdlFont -> IO ()
closeFont sf = do
  ttfCloseFont (sfFont sf)
  mapM_ removeFile (sfTempPath sf)

withTtfMeasure :: Context -> SdlFont -> SdlFont -> Context
withTtfMeasure ctx font monoFont = withTtfMeasureScaled ctx font monoFont 1.0

withTtfMeasureScaled :: Context -> SdlFont -> SdlFont -> Float -> Context
withTtfMeasureScaled ctx sf monoSf scale =
  let fm = ttfFontMetricsScaled sf scale
      monoFm = ttfFontMetricsScaled monoSf scale
      measure txt = measureTtfTextScaled sf scale txt
      ctx1 =
        withExternalText
          ( withMeasureText
              (withMonoFontMetrics (withFontMetrics ctx fm) monoFm)
              measure
          )
          True
   in wrapMeasureCache scale ctx1 measure

-- | Like 'withTtfMeasureScaled' but uses glyph-atlas-backed 'FontMetrics'
-- (produced by 'buildGlyphFontMetrics') so that 'pushText' emits real
-- per-glyph textured quads into the draw arena.  Text measurement still
-- uses the SDL_ttf string-size path for accurate layout.
withTtfMeasureGlyph ::
  Context ->
  SdlFont ->
  SdlFont ->
  FontMetrics -> -- ^ glyph-atlas fm for primary font
  FontMetrics -> -- ^ glyph-atlas fm for mono font
  Float ->
  Context
withTtfMeasureGlyph ctx sf _monoSf fm monoFm scale =
  let measure txt = measureTtfTextScaled sf scale txt
      ctx1 =
        withExternalText
          ( withMeasureText
              (withMonoFontMetrics (withFontMetrics ctx fm) monoFm)
              measure
          )
          False
   in wrapMeasureCache scale ctx1 measure

ttfFontMetricsScaled :: SdlFont -> Float -> FontMetrics
ttfFontMetricsScaled sf scale =
  let inv = if scale > 0 then scale else 1
   in (monospaceMetrics (sfLineSkip sf / inv))
        { fmAscent = sfAscent sf / inv
        , fmAdvance = const (sfSpaceAdvance sf / inv)
        }

measureTtfTextScaled :: SdlFont -> Float -> Text -> IO (Float, Float)
measureTtfTextScaled sf scale txt = do
  (w, h) <- measureTtfText sf txt
  let inv = if scale > 0 then scale else 1
  pure (w / inv, h / inv)

measureTtfText :: SdlFont -> Text -> IO (Float, Float)
measureTtfText sf txt
  -- TTF_GetStringSize treats length 0 as NUL-terminated. Empty Text is a
  -- byte-array slice, not a C string, so strlen would read heap garbage and
  -- the caret would jump. Same for T.take 0 of a non-empty value.
  | T.null txt = pure (0, sfLineSkip sf)
  | otherwise =
      withUtf8 txt $ \cstr len ->
        withForeignPtr measureScratch $ \wp -> do
          let hp = plusPtr wp (sizeOf (0 :: CFloat))
          ok <- ttfStringSize (sfFont sf) cstr len wp hp
          if ok
            then do
              w <- peek wp
              h <- peek hp
              pure (realToFrac w, realToFrac h)
            else pure (0, sfLineSkip sf)

tryInsert :: Ptr () -> Ptr () -> IO (Maybe (Float, Float, Float, Float))
tryInsert atlas surf =
  withForeignPtr insertScratch $ \px -> do
    let py = plusPtr px (sizeOf (0 :: CFloat))
        tw = plusPtr py (sizeOf (0 :: CFloat))
        th = plusPtr tw (sizeOf (0 :: CFloat))
    ok <- textAtlasInsertSurface atlas surf px py tw th
    if ok
      then do
        x <- realToFrac <$> peek px
        y <- realToFrac <$> peek py
        w <- realToFrac <$> peek tw
        h <- realToFrac <$> peek th
        pure (Just (x, y, w, h))
      else pure Nothing

atlasSize :: Ptr () -> IO (Float, Float)
atlasSize atlas =
  withForeignPtr insertScratch $ \w -> do
    let h = plusPtr w (sizeOf (0 :: CFloat))
    ok <- textAtlasSize atlas w h
    when (not ok) $ fail "text atlas size failed"
    (,) <$> (realToFrac <$> peek w) <*> (realToFrac <$> peek h)

withUtf8 :: Text -> (CString -> CSize -> IO a) -> IO a
withUtf8 txt act =
  TF.useAsPtr txt $ \ptr len ->
    act (castPtr ptr) (fromIntegral len)

foreign import ccall unsafe "nano_ui_ttf_init"
  ttfInit :: IO Bool

foreign import ccall unsafe "nano_ui_ttf_quit"
  ttfQuit :: IO ()

foreign import ccall unsafe "nano_ui_ttf_open_font"
  ttfOpenFont :: CString -> CFloat -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_open_font_memory"
  ttfOpenFontMemory :: Ptr () -> CSize -> CFloat -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_close_font"
  ttfCloseFont :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_ttf_line_skip"
  ttfLineSkip :: Ptr () -> IO CFloat

foreign import ccall unsafe "nano_ui_ttf_ascent"
  ttfAscent :: Ptr () -> IO CFloat

foreign import ccall unsafe "nano_ui_ttf_string_size"
  ttfStringSize :: Ptr () -> CString -> CSize -> Ptr CFloat -> Ptr CFloat -> IO Bool

foreign import ccall unsafe "nano_ui_ttf_space_advance"
  ttfSpaceAdvance :: Ptr () -> IO CFloat

foreign import ccall unsafe "nano_ui_text_atlas_create"
  textAtlasCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_destroy"
  textAtlasDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_reset"
  textAtlasReset :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_texture"
  textAtlasTexture :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_size"
  textAtlasSize :: Ptr () -> Ptr CFloat -> Ptr CFloat -> IO Bool

foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  textAtlasInsertSurface ::
    Ptr () ->
    Ptr () ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    IO Bool

foreign import ccall unsafe "SDL_DestroySurface"
  freeSurface :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_ttf_glyph_metrics"
  ttfGlyphMetrics ::
    Ptr () ->   -- font
    CUInt ->    -- codepoint
    Ptr CInt -> -- out_minx
    Ptr CInt -> -- out_maxx
    Ptr CInt -> -- out_miny
    Ptr CInt -> -- out_maxy
    Ptr CInt -> -- out_advance
    IO Bool

foreign import ccall unsafe "nano_ui_ttf_render_glyph_surface"
  ttfRenderGlyphSurface ::
    Ptr () ->        -- font
    CUInt ->         -- codepoint
    Ptr (Ptr ()) ->  -- out_surface
    IO Bool
