module NanoUI.Sdl.Font
  ( SdlFont (..)
  , TextCache
  , GlyphAtlas
  , withTtf
  , openFont
  , closeFont
  , findFontPath
  , findMonoFontPath
  , newTextCache
  , destroyTextCache
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
  , renderTextSpans
  , glyphAtlasTexture
  ) where

import Control.Exception (IOException, bracket, catch)
import Control.Monad (filterM, when)
import Data.Bits (shiftR, (.&.))
import Data.Char (ord)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Primitive.SmallArray
  ( SmallArray
  , indexSmallArray
  , sizeofSmallArray
  , smallArrayFromList
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUInt (..))
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import GHC.IO (unsafePerformIO)
import NanoUI (Color (..), FontMetrics (..), GlyphQuad (..), Rect (..), hasMonoFontMarker, monospaceMetrics, stripMonoFontMarker)
import NanoUI.Testing
  ( Context
  , withExternalText
  , withFontMetrics
  , withMeasureText
  , withMonoFontMetrics
  , wrapMeasureCache
  )
import NanoUI.Sdl.Batch (RenderBatch, batchTextureDst, flushRenderBatch)
import NanoUI.Sdl.Render (logicalClipKey, setLogicalClipKey)
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE

data SdlFont = SdlFont
  { sfFont :: Ptr ()
  , sfLineSkip :: Float
  , sfAscent :: Float
  , sfSpaceAdvance :: Float
  , sfPath :: FilePath
  }

data TextSlot = TextSlot
  { tsW :: {-# UNPACK #-} !Float
  , tsH :: {-# UNPACK #-} !Float
  , tsU0 :: {-# UNPACK #-} !Float
  , tsV0 :: {-# UNPACK #-} !Float
  , tsU1 :: {-# UNPACK #-} !Float
  , tsV1 :: {-# UNPACK #-} !Float
  , tsAtlasW :: {-# UNPACK #-} !Float
  , tsAtlasH :: {-# UNPACK #-} !Float
  , tsTex :: !(Ptr ())
  }

data TextCache = TextCache
  { tcAtlas :: !(Ptr ())
  , tcEntries :: !(IORef (Map.Map CacheKey TextSlot))
  }

type CacheKey = (Ptr (), Text, Word32)

textCacheLimit :: Int
textCacheLimit = 256

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

newTextCache :: Ptr SDL_Renderer -> IO TextCache
newTextCache ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed"
  entries <- newIORef Map.empty
  pure TextCache {tcAtlas = atlas, tcEntries = entries}

destroyTextCache :: TextCache -> IO ()
destroyTextCache cache = textAtlasDestroy (tcAtlas cache)

-- ---------------------------------------------------------------------------
-- Glyph Atlas

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
  -- Get glyph metrics at the current font scale.
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
        _maxY <- peek pMaxY
        adv  <- peek pAdv
        pure
          ( Just
              ( fromIntegral minX  :: Float
              , fromIntegral adv   :: Float
              )
          )
  case mMetrics of
    Nothing -> pure Nothing
    Just (minX, adv) -> do
      -- Render a white-on-alpha surface for the glyph.
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
              -- In SDL3_ttf, TTF_RenderGlyph_Blended returns a full font-height
              -- surface aligned to the font baseline. Its top is already at lineTop (y=0).
              let !offX = min 0 minX
                  !offY = 0
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
        }

closeFont :: SdlFont -> IO ()
closeFont = ttfCloseFont . sfFont

withTtfMeasure :: Context -> SdlFont -> SdlFont -> Context
withTtfMeasure ctx font monoFont = withTtfMeasureScaled ctx font monoFont 1.0

withTtfMeasureScaled :: Context -> SdlFont -> SdlFont -> Float -> Context
withTtfMeasureScaled ctx sf monoSf scale =
  let fm = ttfFontMetricsScaled sf scale
      monoFm = ttfFontMetricsScaled monoSf scale
      measure txt =
        if hasMonoFontMarker txt
          then measureTtfTextScaled monoSf scale (stripMonoFontMarker txt)
          else measureTtfTextScaled sf scale txt
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
withTtfMeasureGlyph ctx sf monoSf fm monoFm scale =
  let measure txt =
        if hasMonoFontMarker txt
          then measureTtfTextScaled monoSf scale (stripMonoFontMarker txt)
          else measureTtfTextScaled sf scale txt
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
measureTtfText sf txt =
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

renderTextSpans ::
  RenderBatch ->
  Ptr SDL_Renderer ->
  Float ->
  SdlFont ->
  SdlFont ->
  TextCache ->
  [(Rect, Text, Color, Color, Rect)] ->
  IO ()
renderTextSpans _ _ _ _ _ _ [] = pure ()
renderTextSpans batch ren scale font monoFont cache spans = do
  let go _ [] = pure ()
      go !prevClip ((Rect x y _ _, txt, fg, _bg, clip) : rest) = do
        let !clipKey = logicalClipKey scale clip
        when (prevClip /= Just clipKey) $ do
          flushRenderBatch batch
          setLogicalClipKey ren clipKey
        drawSpan batch scale font monoFont cache txt fg x y
        go (Just clipKey) rest
  go Nothing spans
  flushRenderBatch batch

drawSpan :: RenderBatch -> Float -> SdlFont -> SdlFont -> TextCache -> Text -> Color -> Float -> Float -> IO ()
drawSpan _ _ _ _ _ txt _ _ _
  | T.null txt = pure ()
drawSpan batch scale font monoFont cache txt col x y =
  let (pick, shown) =
        if hasMonoFontMarker txt
          then (monoFont, stripMonoFontMarker txt)
          else (font, txt)
   in drawSpanFont batch scale pick cache shown col x y

{-# INLINE drawSpanFont #-}
drawSpanFont :: RenderBatch -> Float -> SdlFont -> TextCache -> Text -> Color -> Float -> Float -> IO ()
drawSpanFont _ _ _ _ txt _ _ _
  | T.null txt = pure ()
drawSpanFont batch scale font cache txt col x y = do
  let !keyCol = colorWord col
      !cacheKey = (sfFont font, txt, keyCol)
  mSlot <-
    lookupCache cache cacheKey >>= \case
      Just hit -> pure (Just hit)
      Nothing -> do
        flushRenderBatch batch
        withUtf8 txt $ \cstr len ->
          createCached font cache cacheKey cstr len col
  case mSlot of
    Nothing -> pure ()
    Just (TextSlot aw ah u0 v0 u1 v1 atW atH tex) -> do
      let !px = x * scale
          !py = y * scale
      batchTextureDst
        batch
        tex
        atW
        atH
        px
        py
        aw
        ah
        u0
        v0
        u1
        v1
        255
        255
        255
        255

createCached ::
  SdlFont ->
  TextCache ->
  CacheKey ->
  CString ->
  CSize ->
  Color ->
  IO (Maybe TextSlot)
createCached font cache cacheKey cstr len col =
  withForeignPtr surfaceScratch $ \(surfPtr :: Ptr (Ptr ())) -> do
    poke surfPtr nullPtr
    ok <-
      ttfRenderSurface
        (sfFont font)
        cstr
        len
        r
        g
        b
        a
        surfPtr
        nullPtr
        nullPtr
    when (not ok) $ fail "TTF render failed"
    sz <- Map.size <$> readIORef (tcEntries cache)
    when (sz >= textCacheLimit) $ resetTextCache cache
    surf <- peek surfPtr
    mSlot <- insertSurface cache surf
    freeSurface surf
    case mSlot of
      Nothing -> pure Nothing
      Just (px, py, tw, th) -> do
        (atW, atH) <- atlasSize (tcAtlas cache)
        tex <- textAtlasTexture (tcAtlas cache)
        let slot =
              TextSlot
                { tsW = tw
                , tsH = th
                , tsU0 = px / atW
                , tsV0 = py / atH
                , tsU1 = (px + tw) / atW
                , tsV1 = (py + th) / atH
                , tsAtlasW = atW
                , tsAtlasH = atH
                , tsTex = tex
                }
        insertCache cache cacheKey slot
        pure (Just slot)
  where
    (r, g, b, a) = unpackColor col

insertSurface :: TextCache -> Ptr () -> IO (Maybe (Float, Float, Float, Float))
insertSurface cache surf = do
  let atlas = tcAtlas cache
  tryInsert atlas surf >>= \case
    Just slot -> pure (Just slot)
    Nothing -> do
      clearTextCacheEntries cache
      textAtlasReset atlas
      tryInsert atlas surf

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

clearTextCacheEntries :: TextCache -> IO ()
clearTextCacheEntries cache =
  writeIORef (tcEntries cache) Map.empty

resetTextCache :: TextCache -> IO ()
resetTextCache cache = do
  clearTextCacheEntries cache
  textAtlasReset (tcAtlas cache)

insertCache :: TextCache -> CacheKey -> TextSlot -> IO ()
insertCache cache key val =
  modifyIORef' (tcEntries cache) (Map.insert key val)

lookupCache :: TextCache -> CacheKey -> IO (Maybe TextSlot)
lookupCache cache key = do
  entries <- readIORef (tcEntries cache)
  pure $! Map.lookup key entries

atlasSize :: Ptr () -> IO (Float, Float)
atlasSize atlas =
  withForeignPtr insertScratch $ \w -> do
    let h = plusPtr w (sizeOf (0 :: CFloat))
    ok <- textAtlasSize atlas w h
    when (not ok) $ fail "text atlas size failed"
    (,) <$> (realToFrac <$> peek w) <*> (realToFrac <$> peek h)

pathExists :: FilePath -> IO (Maybe FilePath)
pathExists p = do
  ok <- doesFileExist p
  pure (if ok then Just p else Nothing)

firstExistingPath :: SmallArray FilePath -> IO (Maybe FilePath)
firstExistingPath paths = go 0
  where
    len = sizeofSmallArray paths
    go i
      | i >= len = pure Nothing
      | otherwise = do
          let p = indexSmallArray paths i
          ok <- doesFileExist p
          if ok then pure (Just p) else go (i + 1)

joinDir :: FilePath -> FilePath -> FilePath
joinDir dir name = dir ++ "/" ++ name

-- Distro layouts differ (Arch: /usr/share/fonts/Adwaita, Debian: truetype/).
-- Check known paths first, then look up preferred file names under common roots.
resolveFont :: SmallArray FilePath -> SmallArray FilePath -> Maybe String -> IO (Maybe FilePath)
resolveFont candidates names envPath =
  case envPath of
    Just p -> pathExists p
    Nothing -> do
      hit <- firstExistingPath candidates
      case hit of
        Just p -> pure (Just p)
        Nothing -> findNamedFont names

findMonoFontPath :: IO (Maybe FilePath)
findMonoFontPath = lookupEnv "NANO_UI_MONO_FONT" >>= resolveFont monoFontCandidates monoFontFileNames

findFontPath :: IO (Maybe FilePath)
findFontPath = lookupEnv "NANO_UI_FONT" >>= resolveFont fontCandidates fontFileNames

fontSearchRoots :: IO (SmallArray FilePath)
fontSearchRoots = do
  home <- lookupEnv "HOME"
  profile <- lookupEnv "USERPROFILE"
  let userRoots =
        maybe [] (\h -> [joinDir h ".local/share/fonts", joinDir h ".fonts", joinDir h ".nix-profile/share/fonts"]) home
          ++ maybe [] (\p -> [p ++ "\\AppData\\Local\\Microsoft\\Windows\\Fonts"]) profile
  pure $
    smallArrayFromList
      ( [ "/usr/share/fonts"
        , "/usr/local/share/fonts"
        , "/usr/share/fonts/TTF"
        , "/usr/share/fonts/OTF"
        , "/usr/share/fonts/truetype"
        , "/usr/share/fonts/opentype"
        , "/run/current-system/sw/share/fonts"
        , "C:\\Windows\\Fonts"
        ]
          ++ userRoots
      )

fontCandidates :: SmallArray FilePath
fontCandidates =
  smallArrayFromList
    [ "/usr/share/fonts/Adwaita/AdwaitaSans-Regular.ttf"
    , "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf"
    , "/usr/share/fonts/TTF/DejaVuSans.ttf"
    , "/usr/share/fonts/liberation-sans/LiberationSans-Regular.ttf"
    , "/usr/share/fonts/truetype/liberation/LiberationSans-Regular.ttf"
    , "/usr/share/fonts/noto/NotoSans-Regular.ttf"
    , "/usr/share/fonts/truetype/noto/NotoSans-Regular.ttf"
    , "/usr/share/fonts/truetype/freefont/FreeSans.ttf"
    , "/System/Library/Fonts/SFNS.ttf"
    , "/System/Library/Fonts/Helvetica.ttc"
    , "C:\\Windows\\Fonts\\segoeui.ttf"
    , "C:\\Windows\\Fonts\\arial.ttf"
    ]

fontFileNames :: SmallArray FilePath
fontFileNames =
  smallArrayFromList
    [ "AdwaitaSans-Regular.ttf"
    , "DejaVuSans.ttf"
    , "LiberationSans-Regular.ttf"
    , "NotoSans-Regular.ttf"
    , "FreeSans.ttf"
    , "segoeui.ttf"
    , "arial.ttf"
    ]

monoFontCandidates :: SmallArray FilePath
monoFontCandidates =
  smallArrayFromList
    [ "/usr/share/fonts/Adwaita/AdwaitaMono-Regular.ttf"
    , "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
    , "/usr/share/fonts/TTF/DejaVuSansMono.ttf"
    , "/usr/share/fonts/liberation-mono/LiberationMono-Regular.ttf"
    , "/usr/share/fonts/truetype/liberation/LiberationMono-Regular.ttf"
    , "/usr/share/fonts/noto/NotoSansMono-Regular.ttf"
    , "/usr/share/fonts/truetype/noto/NotoSansMono-Regular.ttf"
    , "/usr/share/fonts/truetype/freefont/FreeMono.ttf"
    , "/System/Library/Fonts/SFNSMono.ttf"
    , "/System/Library/Fonts/Menlo.ttc"
    , "C:\\Windows\\Fonts\\consola.ttf"
    , "C:\\Windows\\Fonts\\cour.ttf"
    ]

monoFontFileNames :: SmallArray FilePath
monoFontFileNames =
  smallArrayFromList
    [ "AdwaitaMono-Regular.ttf"
    , "DejaVuSansMono.ttf"
    , "LiberationMono-Regular.ttf"
    , "NotoSansMono.ttf"
    , "FreeMono.ttf"
    , "consola.ttf"
    , "cour.ttf"
    ]

findNamedFont :: SmallArray FilePath -> IO (Maybe FilePath)
findNamedFont names = do
  roots <- fontSearchRoots
  let nRoots = sizeofSmallArray roots
      nNames = sizeofSmallArray names
      walkRoots !i
        | i >= nRoots = pure Nothing
        | otherwise = do
            let root = indexSmallArray roots i
            doesDirectoryExist root >>= \case
              False -> walkRoots (i + 1)
              True ->
                scanDir root >>= \case
                  Just hit -> pure (Just hit)
                  Nothing -> walkRoots (i + 1)
      scanDir dir = do
        entries <- listDirectory dir `catch` \(_ :: IOException) -> pure []
        let nEntries = length entries
            matchDirect = goDirect entries 0
            goDirect [] _ = Nothing
            goDirect (e : rest) !idx
              | idx >= nEntries = Nothing
              | otherwise =
                  if matchesName e
                    then Just (joinDir dir e)
                    else goDirect rest (idx + 1)
        case matchDirect of
          Just hit -> pure (Just hit)
          Nothing -> do
            subdirs <- filterM (\e -> doesDirectoryExist (joinDir dir e)) entries
            searchSubdirs subdirs
      searchSubdirs [] = pure Nothing
      searchSubdirs (d : rest) = do
        scanDir (joinDir d "") >>= \case
          Just hit -> pure (Just hit)
          Nothing -> searchSubdirs rest
      matchesName name = go 0
        where
          go !j
            | j >= nNames = False
            | otherwise = name == indexSmallArray names j || go (j + 1)
  walkRoots 0

withUtf8 :: Text -> (CString -> CSize -> IO a) -> IO a
withUtf8 txt act =
  let bs = TE.encodeUtf8 txt
   in BS.useAsCStringLen bs $ \(cstr, len) -> act cstr (fromIntegral len)

unpackColor :: Color -> (Word8, Word8, Word8, Word8)
unpackColor (Color w) =
  ( fromIntegral ((w `shiftR` 24) .&. 0xFF)
  , fromIntegral ((w `shiftR` 16) .&. 0xFF)
  , fromIntegral ((w `shiftR` 8) .&. 0xFF)
  , fromIntegral (w .&. 0xFF)
  )

colorWord :: Color -> Word32
colorWord (Color w) = w

foreign import ccall unsafe "nano_ui_ttf_init"
  ttfInit :: IO Bool

foreign import ccall unsafe "nano_ui_ttf_quit"
  ttfQuit :: IO ()

foreign import ccall unsafe "nano_ui_ttf_open_font"
  ttfOpenFont :: CString -> CFloat -> IO (Ptr ())

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

foreign import ccall unsafe "nano_ui_ttf_render_surface"
  ttfRenderSurface ::
    Ptr () ->
    CString ->
    CSize ->
    Word8 ->
    Word8 ->
    Word8 ->
    Word8 ->
    Ptr (Ptr ()) ->
    Ptr CFloat ->
    Ptr CFloat ->
    IO Bool

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
