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
  , registerGlyphAtlasRewarm
  , prepareGlyphAtlasForFrame
  , takeGlyphAtlasResetFlag
  , warmGlyphAtlas
  , withTtfMeasure
  , withTtfMeasureScaled
  , withTtfMeasureGlyph
  , ttfFontMetricsScaled
  , buildGlyphFontMetrics
  , measureTtfText
  , measureTtfTextScaled
  , glyphAtlasTexture
  , SdlFontCache
  , CachedFontEntry (..)
  , newSdlFontCache
  , destroySdlFontCache
  , resetSdlFontCache
  , setSdlFontCacheSource
  , withTtfFontCache
  , getOrLoadCachedFont
  , ttfSetFontStyle
  , ttfSaveRenderText
  , ttfGetKerning
  , ttfGetPairKerning
  , ttfDebugPair
  , ttfDumpLayout
  , withUtf8
  ) where

import Control.Exception (SomeException, bracket, catch, throwIO)
import Control.Monad (unless, when)
import Data.Bits ((.|.), shiftL)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Data.Char (ord)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Set as Set
import Data.Primitive.SmallArray
  ( indexSmallArray
  , newSmallArray
  , readSmallArray
  , smallArrayFromList
  , writeSmallArray
  )
import Data.Primitive.PrimArray (indexPrimArray, primArrayFromList)
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUInt (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, poke, sizeOf)
import Data.Unique (hashUnique, newUnique)
import qualified Data.ByteString as BS
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import NanoUI
  ( FontMetrics (..)
  , FontBackend (..)
  , FontStyle (..)
  , FontVariant (..)
  , FontWeight (..)
  , GlyphQuad (..)
  , RunQuad (..)
  , monospaceMetrics
  )
import NanoUI.Testing
  ( Context
  , withExternalText
  , withFontMetrics
  , withFontResolver
  , withMeasureText
  , withMonoFontMetrics
  , wrapMeasureCache
  )
import SDL3.Sys.Bindgen.Render (SDL_Renderer)
import qualified Data.Map.Strict as Map
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Foreign as TF

data SdlFont = SdlFont
  { sfId :: !Word64
  , sfFont :: Ptr ()
  , sfLineSkip :: Float
  , sfAscent :: Float
  , sfSpaceAdvance :: Float
  , sfPath :: FilePath
  , sfTempPath :: !(Maybe FilePath)
  , sfAlive :: !(IORef Bool)
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

-- | (font ID, Unicode codepoint)
type GlyphKey = (Word64, Char)

data GlyphAtlas = GlyphAtlas
  { gaAtlas :: !(Ptr ())
  , gaEntries :: !(IORef (Map.Map GlyphKey (Maybe GlyphSlot)))
  , gaEpoch :: !(IORef Word64)
  , -- | An insertion failed (atlas out of space) during the last frame; the
    -- atlas must be reset at the next frame start, before any quad is
    -- recorded, so the reset can never wipe the texture underneath
    -- already-recorded text.
    gaNeedsReset :: !(IORef Bool)
  , -- | The atlas was reset (or ran out of space) since the flag was last
    -- cleared at frame start. Observed by the runner after the UI pass: a
    -- set flag means the frame being built holds stale-UV or unplaceable
    -- text quads and must not be presented.
    gaResetFlag :: !(IORef Bool)
  , -- | Actions to run after every reset (re-warming the base fonts).
    gaRewarmHooks :: !(IORef [IO ()])
  , gaAlive :: !(IORef Bool)
  }

newFontId :: IO Word64
newFontId = fromIntegral . hashUnique <$> newUnique

-- Backend effects are confined to the owning SDL thread. Retained snapshots
-- may outlive a window or a cache entry, but must never query a freed handle.
ensureFontAlive :: SdlFont -> IO ()
ensureFontAlive sf = do
  alive <- readIORef (sfAlive sf)
  unless alive (fail "font backend used after closeFont")

ensureAtlasAlive :: GlyphAtlas -> IO ()
ensureAtlasAlive ga = do
  alive <- readIORef (gaAlive ga)
  unless alive (fail "font backend used after destroyGlyphAtlas")

-- | Maximum number of shaped-run cache entries per 'FontMetrics'. Dynamic,
-- ever-changing text (FPS counters, timers, percentages, mouse positions)
-- generates unique strings over time; without a bound the run cache (and
-- the atlas rectangles its renders occupy) would grow without limit. The
-- cap sits well above a realistic frame's string working set so steady
-- static text is never evicted (re-rendering an evicted run leaks its old
-- atlas rectangle); atlas exhaustion itself is recovered by the deferred
-- reset in 'prepareGlyphAtlasForFrame', which also clears the whole cache.
runCacheCap :: Int
runCacheCap = 1024

-- Entry-count limits alone do not bound retained text: edited oversized lines
-- can otherwise keep thousands of full-document versions alive per font.
cacheableText :: Text -> Bool
cacheableText = (<= 4096) . T.length

getGlyphAdvance :: SdlFont -> CUInt -> IO (Maybe Float)
getGlyphAdvance sf cp =
  allocaBytes (5 * sizeOf (0 :: CInt)) $ \p -> do
    let pMinX = p
        pMaxX = plusPtr pMinX (sizeOf (0 :: CInt))
        pMinY = plusPtr pMaxX (sizeOf (0 :: CInt))
        pMaxY = plusPtr pMinY (sizeOf (0 :: CInt))
        pAdv  = plusPtr pMaxY (sizeOf (0 :: CInt))
    ok <- ttfGlyphMetrics (sfFont sf) cp pMinX pMaxX pMinY pMaxY pAdv
    if ok
      then do
        adv <- peek pAdv
        pure (Just (fromIntegral adv))
      else pure Nothing

-- Metric-only geometry has no atlas lifetime and never rasterises a surface.
getGlyphGeometry :: SdlFont -> Float -> Char -> IO (Maybe GlyphQuad)
getGlyphGeometry sf inv c = allocaBytes (5 * sizeOf (0 :: CInt)) $ \p -> do
  let pMaxX = plusPtr p (sizeOf (0 :: CInt))
      pMinY = plusPtr pMaxX (sizeOf (0 :: CInt))
      pMaxY = plusPtr pMinY (sizeOf (0 :: CInt))
      pAdv = plusPtr pMaxY (sizeOf (0 :: CInt))
  ok <- ttfGlyphMetrics (sfFont sf) (fromIntegral (ord c)) p pMaxX pMinY pMaxY pAdv
  if not ok then pure Nothing else do
    minX <- fromIntegral <$> peek p
    maxX <- fromIntegral <$> peek pMaxX
    minY <- fromIntegral <$> peek pMinY
    maxY <- fromIntegral <$> peek pMaxY
    pure $! Just (GlyphQuad (minX / inv) ((sfAscent sf - maxY) / inv)
      ((maxX - minX) / inv) ((maxY - minY) / inv) 0 0 0 0)

newGlyphAtlas :: Ptr SDL_Renderer -> IO GlyphAtlas
newGlyphAtlas ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed (glyph)"
  entries <- newIORef Map.empty
  epoch <- newIORef 0
  needsReset <- newIORef False
  resetFlag <- newIORef False
  rewarms <- newIORef []
  alive <- newIORef True
  pure
    GlyphAtlas
      { gaAtlas = atlas
      , gaEntries = entries
      , gaEpoch = epoch
      , gaNeedsReset = needsReset
      , gaResetFlag = resetFlag
      , gaRewarmHooks = rewarms
      , gaAlive = alive
      }

destroyGlyphAtlas :: GlyphAtlas -> IO ()
destroyGlyphAtlas ga = do
  alive <- atomicModifyIORef' (gaAlive ga) (\open -> (False, open))
  when alive $ textAtlasDestroy (gaAtlas ga)

-- | Register an action to run after every atlas reset. The SDL backend
-- registers a hook that re-warms the base fonts' ASCII glyphs
-- ('warmGlyphAtlas'); the hook reads the current base fonts from their
-- 'IORef's lazily, so a reset always warms the live fonts and never one
-- that has since been closed.
registerGlyphAtlasRewarm :: GlyphAtlas -> IO () -> IO ()
registerGlyphAtlasRewarm ga hook = modifyIORef' (gaRewarmHooks ga) (hook :)

resetGlyphAtlas :: GlyphAtlas -> IO ()
resetGlyphAtlas ga = do
  modifyIORef' (gaEpoch ga) (+1)
  writeIORef (gaEntries ga) Map.empty
  writeIORef (gaNeedsReset ga) False
  textAtlasReset (gaAtlas ga)
  hooks <- readIORef (gaRewarmHooks ga)
  mapM_ id hooks
  writeIORef (gaResetFlag ga) True

-- | An atlas insertion failed: the atlas is out of space. The reset is
-- deferred to the next frame start ('prepareGlyphAtlasForFrame') so quads
-- already recorded this frame keep sampling valid pixels, and the frame
-- itself is marked invalid so the runner drops it instead of presenting
-- text that could not be placed.
markAtlasExhausted :: GlyphAtlas -> IO ()
markAtlasExhausted ga = do
  writeIORef (gaNeedsReset ga) True
  writeIORef (gaResetFlag ga) True

-- | Frame-start atlas maintenance: reset the atlas if an insertion failed
-- during the previous frame, then clear the mid-frame reset flag. Must run
-- before the frame's UI pass records any quads.
prepareGlyphAtlasForFrame :: GlyphAtlas -> IO ()
prepareGlyphAtlasForFrame ga = do
  needs <- readIORef (gaNeedsReset ga)
  when needs $ resetGlyphAtlas ga
  writeIORef (gaResetFlag ga) False

-- | Test-and-clear the mid-frame reset flag. 'True' means the atlas was
-- reset (or ran out of space) while the frame was being built, so quads
-- recorded before that point may hold stale UVs; the caller must not
-- present that frame.
takeGlyphAtlasResetFlag :: GlyphAtlas -> IO Bool
takeGlyphAtlasResetFlag ga = atomicModifyIORef' (gaResetFlag ga) (\v -> (False, v))

-- | Pre-rasterise printable ASCII into the glyph atlas to avoid cold misses
-- on the first rendered frame.
warmGlyphAtlas :: GlyphAtlas -> SdlFont -> IO ()
warmGlyphAtlas ga sf =
  mapM_ (\c -> lookupOrInsertGlyph ga sf c) [' ' .. '~']

-- | Look up or insert a glyph into the atlas.  Returns 'Nothing' for
-- characters that have no glyph (e.g. control characters).
lookupOrInsertGlyph :: GlyphAtlas -> SdlFont -> Char -> IO (Maybe GlyphSlot)
lookupOrInsertGlyph ga sf c = do
  let !key = (sfId sf, c)
  entries <- readIORef (gaEntries ga)
  case Map.lookup key entries of
    Just mSlot -> pure mSlot
    Nothing    -> insertGlyph ga sf key c

insertGlyph :: GlyphAtlas -> SdlFont -> GlyphKey -> Char -> IO (Maybe GlyphSlot)
insertGlyph ga sf key c = do
  let !cp = fromIntegral (ord c) :: CUInt
  mMetrics <- allocaBytes (5 * sizeOf (0 :: CInt)) $ \p -> do
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
    Nothing -> do
      modifyIORef' (gaEntries ga) (Map.insert key Nothing)
      pure Nothing
    Just (minX, maxY, adv) -> do
      mSurf <- alloca $ \sp -> do
        poke sp nullPtr
        ok <- ttfRenderGlyphSurface (sfFont sf) cp sp
        if not ok
          then pure Nothing
          else do
            surf <- peek sp
            if surf == nullPtr then pure Nothing else pure (Just surf)
      case mSurf of
        Nothing -> do
          modifyIORef' (gaEntries ga) (Map.insert key Nothing)
          pure Nothing
        Just surf -> do
          -- If the atlas is full, defer the reset to the next frame start
          -- (see 'markAtlasExhausted'): wiping the texture here would leave
          -- quads already recorded this frame sampling blank pixels, making
          -- all earlier text vanish for one frame. The glyph is unavailable
          -- for the rest of the current frame, which is dropped.
          mPos <- tryInsert (gaAtlas ga) surf
          freeSurface surf
          case mPos of
            Nothing -> do
              markAtlasExhausted ga
              modifyIORef' (gaEntries ga) (Map.insert key Nothing)
              pure Nothing
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
              modifyIORef' (gaEntries ga) (Map.insert key (Just slot))
              pure (Just slot)

-- | ASCII glyph-cache slot. The cached 'Maybe' is shared on every hit, so a
-- warm lookup returns the same heap object instead of rebuilding
-- @Just GlyphQuad@ on each character.
data CachedQuad
  = UncachedQuad
  -- Preserve the cached object even with -funbox-strict-fields: unpacking it
  -- defeats sharing and reconstructs the lookup result on every hit.
  | Cached {-# NOUNPACK #-} !(Maybe GlyphQuad)

-- | Build immutable metric snapshots and explicit IO rasterisation callbacks.
-- Font queries happen in 'fbPrepare'; atlas insertion happens in 'fbDrawRun'
-- and 'fbDrawGlyph'. All coordinates are logical (unscaled).
--
-- Standard ASCII (0..127) lookups are backed by a high-performance 'SmallMutableArray'
-- fast path for branchless O(1) in-memory indexing, with automatic cache invalidation
-- whenever the underlying glyph atlas is reset.
{-# NOINLINE buildGlyphFontMetrics #-}
buildGlyphFontMetrics :: GlyphAtlas -> SdlFont -> Float -> IO FontMetrics
buildGlyphFontMetrics ga sf scale = do
  let !inv = if scale > 0 then scale else 1
      baseFm = ttfFontMetricsScaled sf scale

  -- Precompute ASCII 0..127 advances once directly from FreeType without atlas rasterization
  advs <- mapM (getGlyphAdvance sf) [0 .. 127 :: CUInt]
  -- Reuse fixed ASCII geometry across dynamic labels, so preparing a fresh
  -- counter string does not query native glyph metrics for each character.
  asciiGeometry <- smallArrayFromList <$> mapM (getGlyphGeometry sf inv) ['\0' .. '\127']
  let !asciiAdvances =
        primArrayFromList
          [ case mAdv of
              Nothing  -> sfSpaceAdvance sf / inv
              Just adv -> adv / inv
          | mAdv <- advs
          ]

  -- Cache of ASCII 0..127 glyph quads with epoch-based invalidation
  asciiCacheArr <- newSmallArray 128 UncachedQuad
  initEpoch <- readIORef (gaEpoch ga)
  asciiEpochRef <- newIORef initEpoch

  -- Non-ASCII glyph quads are memoised per font here (keyed by codepoint) so
  -- repeated text still hits a shared value instead of rebuilding the record
  -- on every character. Invalidated with the atlas epoch.
  nonAsciiCacheRef <- newIORef IM.empty
  nonAsciiEpochRef <- newIORef initEpoch

  -- Kerning pairs are sparse and each miss costs a shaped 2-glyph
  -- layout, so a pair cache keeps the hot pen loops off the FFI
  -- boundary after first contact. Keyed by packed codepoint pair on this
  -- 'FontMetrics' (the font id is implicit).
  kernCacheRef <- newIORef (IM.empty, 0 :: Int)

  -- Shaped text runs: whole strings rendered through SDL3_ttf so GPOS
  -- kerning, ligatures, and contextual positioning are preserved.  Run
  -- quads are cached per font keyed by text; the cache is validated
  -- against the atlas epoch so a reset drops every stale entry on the
  -- next lookup (Nothing entries are for runs too large for the atlas,
  -- which draw per-glyph instead).  The cache is bounded by 'runCacheCap'.
  -- Metric snapshots have their own bounded cache and survive atlas resets.
  -- Run quads are fully evaluated by the effectful drawing path.
  preparedRef <- newIORef (Map.empty, Seq.empty)
  runCacheRef <- newIORef Map.empty
  runOrderRef <- newIORef Seq.empty -- insertion order; evict the oldest
  initRunEpoch <- readIORef (gaEpoch ga)
  runEpochRef <- newIORef initRunEpoch

  let
    slotToQuad !gs =
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

    resetAsciiCache !epoch = do
      writeIORef asciiEpochRef epoch
      mapM_ (\i -> writeSmallArray asciiCacheArr i UncachedQuad) [0 .. 127 :: Int]

    lookupAsciiQuad !cp = do
      curEpoch <- readIORef (gaEpoch ga)
      lastEpoch <- readIORef asciiEpochRef
      when (curEpoch /= lastEpoch) $ resetAsciiCache curEpoch
      cached <- readSmallArray asciiCacheArr cp
      case cached of
        Cached mq -> pure mq
        UncachedQuad -> do
          mSlot <- lookupOrInsertGlyph ga sf (toEnum cp)
          newEpoch <- readIORef (gaEpoch ga)
          if newEpoch /= curEpoch
            then do
              -- The atlas was reset during insertion: this slot's UVs are
              -- already stale, so do not cache them.
              resetAsciiCache newEpoch
              pure (fmap slotToQuad mSlot)
            else do
              let !mq = fmap slotToQuad mSlot
              writeSmallArray asciiCacheArr cp (Cached mq)
              pure mq

    lookupNonAsciiQuad !c = do
      curEpoch <- readIORef (gaEpoch ga)
      lastEpoch <- readIORef nonAsciiEpochRef
      when (curEpoch /= lastEpoch) $ do
        writeIORef nonAsciiEpochRef curEpoch
        writeIORef nonAsciiCacheRef IM.empty
      m <- readIORef nonAsciiCacheRef
      case IM.lookup (ord c) m of
        Just mq -> pure mq
        Nothing -> do
          mSlot <- lookupOrInsertGlyph ga sf c
          newEpoch <- readIORef (gaEpoch ga)
          if newEpoch /= curEpoch
            then pure (fmap slotToQuad mSlot)
            else do
              let !mq = fmap slotToQuad mSlot
              modifyIORef' nonAsciiCacheRef (IM.insert (ord c) mq)
              pure mq

    {-# NOINLINE glyphLookup #-}
    glyphLookup !c = do
      ensureFontAlive sf
      ensureAtlasAlive ga
      let !cp = ord c
      if (fromIntegral cp :: Word) < 128
             then lookupAsciiQuad cp
             else lookupNonAsciiQuad c

    {-# NOINLINE advanceLookup #-}
    advanceLookup !c =
      let !cp = ord c
       in if (fromIntegral cp :: Word) < 128
             then pure (indexPrimArray asciiAdvances cp)
             else do
              mAdv <- getGlyphAdvance sf (fromIntegral cp)
              pure $! case mAdv of
                Nothing  -> sfSpaceAdvance sf / inv
                Just adv -> adv / inv

    {-# NOINLINE kernLookup #-}
    kernLookup !prev !c = do
      -- The cache lives on this 'FontMetrics', so the font id is constant and
      -- the pair can be packed into a single Int key: no tuple on the hot path.
      let !pk = (ord prev `shiftL` 21) .|. ord c
      (m, count) <- readIORef kernCacheRef
      case IM.lookup pk m of
        Just k -> pure k
        Nothing -> do
          raw <- ttfGetKerning (sfFont sf) (fromIntegral (ord prev) :: CUInt) (fromIntegral (ord c) :: CUInt)
          let !k = fromIntegral raw / inv
              !remaining = if count >= 4096 then IM.deleteMin m else m
              !m' = IM.insert pk k remaining
              !count' = min 4096 (count + 1)
          writeIORef kernCacheRef $! (m', count')
          pure k

    {-# NOINLINE runLookup #-}
    runLookup !txt = do
      ensureFontAlive sf
      ensureAtlasAlive ga
      ep <- readIORef (gaEpoch ga)
      runEp <- readIORef runEpochRef
      when (runEp /= ep) $ do
        -- The atlas was reset: every cached run quad is stale.
        writeIORef runEpochRef ep
        writeIORef runCacheRef Map.empty
        writeIORef runOrderRef Seq.empty
      -- The cache is per 'FontMetrics', so the font id is implicit and the
      -- key is the text alone: no per-lookup tuple allocation.
      let !key = txt
      m <- readIORef runCacheRef
      case Map.lookup key m of
        Just rq -> pure rq
        Nothing -> makeRunQuad key txt

    -- Bounded insert into the run cache: at 'runCacheCap' entries the
    -- oldest entry is dropped in amortized O(1), so ever-changing
    -- text cannot grow the cache without limit.
    cacheRun !key !_ | not (cacheableText key) = pure ()
    cacheRun !key !rq = do
      m <- readIORef runCacheRef
      order <- readIORef runOrderRef
      let (!mEvict, !orderEvict)
            | Map.size m >= runCacheCap
            , victim Seq.:< rest <- Seq.viewl order =
                (Map.delete victim m, rest)
            | otherwise = (m, order)
      writeIORef runCacheRef $! Map.insert key rq mEvict
      writeIORef runOrderRef $! orderEvict Seq.|> key

    -- Mirrors NANO_UI_TEXT_ATLAS_PAD in nano_ui_text_atlas.c.
    runAtlasPad :: Float
    runAtlasPad = 1

    -- Mirrors NANO_UI_TEXT_ATLAS_SIZE in nano_ui_text_atlas.c.
    runAtlasTexSize :: Float
    runAtlasTexSize = 2048

    -- A UV rect that always samples transparent pixels: column 4 sits
    -- right of the 4px white patch (columns 0..3) and left of the first
    -- slot (allocations start at x = 5), and the final row is never
    -- written because every slot keeps 1px of padding. A run that could
    -- not be placed draws nothing instead of garbage.
    deadRunUv :: (Float, Float, Float, Float)
    deadRunUv =
      let !u = 4.5 / runAtlasTexSize
          !v = (runAtlasTexSize - 0.5) / runAtlasTexSize
       in (u, v, u, v)

    -- A shaped run is rasterised as one whole-run atlas surface. A run larger
    -- than the atlas can never be inserted: resetting the atlas would not
    -- help and a mid-frame reset would wipe every live glyph, so
    -- already-recorded quads would sample blank pixels and vanish. Oversized
    -- runs stay uncached and fall back to the per-glyph path in pushText /
    -- lineWidth, which measures and draws with the same advances and kerning.
    {-# NOINLINE makeRunQuad #-}
    makeRunQuad !key !txt
      | T.null txt = pure Nothing
      | otherwise = do
          (w, h) <- withUtf8 txt $ \cstr len ->
            allocaBytes (2 * sizeOf (0 :: CFloat)) $ \wp -> do
              let hp = plusPtr wp (sizeOf (0 :: CFloat))
              ok <- ttfStringSize (sfFont sf) cstr len wp hp
              if not ok
                then pure (0, 0)
                else (,) <$> (realToFrac <$> peek wp) <*> (realToFrac <$> peek hp)
          if w <= 0 && h <= 0
            then pure Nothing
            else do
              (atW, atH) <- atlasSize (gaAtlas ga)
              let tooBig = w + 2 * runAtlasPad > atW || h + 2 * runAtlasPad > atH
              if tooBig
                then do
                  cacheRun key Nothing
                  pure Nothing
                else do
                  (!u0, !v0, !u1, !v1) <- renderRun txt
                  let !rq =
                        RunQuad
                          { rqX = 0
                          , rqY = 0
                          , rqW = w / inv
                          , rqH = h / inv
                          , rqU0 = u0
                          , rqV0 = v0
                          , rqU1 = u1
                          , rqV1 = v1
                          , rqAdvance = w / inv
                          }
                  cacheRun key (Just rq)
                  pure (Just rq)
      where
        renderRun t =
          withUtf8 t $ \cstr len -> do
            mPos <- alloca $ \sp -> do
              poke sp nullPtr
              ok <- ttfRenderTextSurface (sfFont sf) cstr len sp
              if not ok
                then pure Nothing
                else do
                  surf <- peek sp
                  mPos <- tryInsert (gaAtlas ga) surf
                  freeSurface surf
                  pure mPos
            case mPos of
              Nothing -> do
                -- Atlas exhausted mid-frame: never wipe the texture here
                -- (quads already recorded this frame sample it). Flag the
                -- exhaustion so the runner drops this frame and the atlas
                -- resets at the next frame start; the failing run draws
                -- nothing in the meantime.
                markAtlasExhausted ga
                pure deadRunUv
              Just (px, py, tw, th) -> do
                (atW, atH) <- atlasSize (gaAtlas ga)
                pure (px / atW, py / atH, (px + tw) / atW, (py + th) / atH)

    glyphGeometry c
      | ord c < 128 = pure (indexSmallArray asciiGeometry (ord c))
      | otherwise = getGlyphGeometry sf inv c

    backend = FontBackend prepareText runLookup glyphLookup

    prepareText txt = do
      ensureFontAlive sf
      (entries, order) <- readIORef preparedRef
      case Map.lookup txt entries of
        Just fm -> pure fm
        Nothing -> do
          let chars = T.foldl' (\m c -> IM.insert (ord c) c m) IM.empty (" HxM" <> txt)
          advances <- traverse advanceLookup chars
          geometry <- traverse glyphGeometry chars
          let gather !pairs !previous remaining = case T.uncons remaining of
                Nothing -> pure pairs
                Just (c, rest) -> do
                  let key = (ord previous `shiftL` 21) .|. ord c
                  pairs' <- if IM.member key pairs then pure pairs else do
                    k <- kernLookup previous c
                    pure $! IM.insert key k pairs
                  gather pairs' c rest
          kerns <- gather IM.empty ' ' ("xM" <> txt)
          (w, h) <- measureTtfText sf txt
          let run
                | T.null txt || w + 2 * runAtlasPad > runAtlasTexSize || h + 2 * runAtlasPad > runAtlasTexSize = Nothing
                | otherwise = Just (RunQuad 0 0 (w / inv) (h / inv) 0 0 0 0 (w / inv))
              !fm = baseFm
                { fmAdvance = \c ->
                    let cp = ord c
                     in if cp < 128 then indexPrimArray asciiAdvances cp
                          else IM.findWithDefault (sfSpaceAdvance sf / inv) cp advances
                , fmKerning = \a b -> IM.findWithDefault 0 ((ord a `shiftL` 21) .|. ord b) kerns
                , fmGlyph = \c -> IM.findWithDefault Nothing (ord c) geometry
                , fmRun = \t -> if t == txt then run else Nothing
                , fmBackend = Just backend
                , fmSnapScale = inv
                }
              (!remaining, !order')
                | Map.size entries >= runCacheCap
                , victim Seq.:< rest <- Seq.viewl order = (Map.delete victim entries, rest)
                | otherwise = (entries, order)
              !entries' = Map.insert txt fm remaining
              !nextOrder = order' Seq.|> txt
          when (cacheableText txt) $ writeIORef preparedRef $! (entries', nextOrder)
          pure fm

  prepareText ""

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
  fid <- newFontId
  alive <- newIORef True
  lineSkip <- ttfLineSkip font
  ascent <- ttfAscent font
  spaceAdv <- ttfSpaceAdvance font
  pure
    SdlFont
      { sfId = fid
      , sfAlive = alive
      , sfFont = font
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
  alive <- atomicModifyIORef' (sfAlive sf) (\open -> (False, open))
  when alive $ do
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
measureTtfText sf txt = do
  ensureFontAlive sf
  measureTtfTextOpen sf txt

measureTtfTextOpen :: SdlFont -> Text -> IO (Float, Float)
measureTtfTextOpen sf txt
  -- TTF_GetStringSize treats length 0 as NUL-terminated. Empty Text is a
  -- byte-array slice, not a C string, so strlen would read heap garbage and
  -- the caret would jump. Same for T.take 0 of a non-empty value.
  | T.null txt = pure (0, sfLineSkip sf)
  | otherwise =
      withUtf8 txt $ \cstr len ->
        allocaBytes (2 * sizeOf (0 :: CFloat)) $ \wp -> do
          let hp = plusPtr wp (sizeOf (0 :: CFloat))
          ok <- ttfStringSize (sfFont sf) cstr len wp hp
          if ok
            then do
              w <- peek wp
              h <- peek hp
              let ww = realToFrac w
                  hh = realToFrac h
              pure (ww, hh)
            else pure (0, sfLineSkip sf)

tryInsert :: Ptr () -> Ptr () -> IO (Maybe (Float, Float, Float, Float))
tryInsert atlas surf =
  allocaBytes (4 * sizeOf (0 :: CFloat)) $ \px -> do
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
  allocaBytes (2 * sizeOf (0 :: CFloat)) $ \w -> do
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

foreign import ccall unsafe "nano_ui_ttf_render_text_surface"
  ttfRenderTextSurface ::
    Ptr () ->        -- font
    CString ->       -- text
    CSize ->         -- length
    Ptr (Ptr ()) ->  -- out_surface
    IO Bool

foreign import ccall unsafe "nano_ui_ttf_set_font_style"
  ttfSetFontStyle :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "nano_ui_ttf_save_render_text"
  ttfSaveRenderText :: Ptr () -> CString -> CString -> IO Bool

foreign import ccall unsafe "nano_ui_ttf_get_kerning"
  ttfGetKerning :: Ptr () -> CUInt -> CUInt -> IO CInt

foreign import ccall unsafe "nano_ui_ttf_get_pair_kerning"
  ttfGetPairKerning :: Ptr () -> CUInt -> CUInt -> IO CInt

foreign import ccall unsafe "nano_ui_ttf_dump_layout"
  ttfDumpLayout :: Ptr () -> CString -> IO ()

foreign import ccall unsafe "nano_ui_ttf_debug_pair"
  ttfDebugPair :: Ptr () -> CUInt -> CUInt -> IO ()

-- ---------------------------------------------------------------------------
-- Dynamic font cache for crisp text rendering at arbitrary sizes and styles

data FontCacheKey = FontCacheKey
  { fckVariant :: !FontVariant
  , fckPtKey   :: !Int -- round (targetPt * 2)
  , fckBold    :: !Bool
  , fckItalic  :: !Bool
  } deriving (Eq, Ord, Show)

data CachedFontEntry = CachedFontEntry
  { cfeFont    :: !SdlFont
  , cfeFm      :: !FontMetrics
  , cfeMeasure :: !(Text -> IO (Float, Float))
  }

data DynamicCache = DynamicCache
  { dcEntries :: !(Map.Map FontCacheKey CachedFontEntry)
  , dcLru     :: ![FontCacheKey]
  }

makeCachedFontEntry :: SdlFont -> FontMetrics -> Float -> IO CachedFontEntry
makeCachedFontEntry font fm scale = do
  measCache <- newIORef Map.empty
  let baseMeas txt = measureTtfTextScaled font scale txt
      meas txt = do
        m <- readIORef measCache
        case Map.lookup txt m of
          Just sz -> pure sz
          Nothing -> do
            sz <- baseMeas txt
            modifyIORef' measCache (Map.insert txt sz)
            pure sz
  pure CachedFontEntry
    { cfeFont    = font
    , cfeFm      = fm
    , cfeMeasure = meas
    }

data SdlFontCache = SdlFontCache
  { sfcPrimarySourceRef :: !(IORef FontSource)
  , sfcFallbackSource :: !FontSource
  , sfcMonoSource     :: !FontSource
  , sfcMonoFallback   :: !FontSource
  , sfcGlyphAtlas     :: !GlyphAtlas
  , sfcBasePt         :: !Float
  , sfcScaleRef       :: !(IORef Float)
  , sfcBaseEntries    :: !(IORef (CachedFontEntry, CachedFontEntry))
  , sfcDynamicCache   :: !(IORef DynamicCache)
  }

newSdlFontCache ::
  FontSource -> -- ^ primary font source
  FontSource -> -- ^ fallback font source
  FontSource -> -- ^ mono font source
  FontSource -> -- ^ mono fallback font source
  GlyphAtlas ->
  Float ->      -- ^ base font size (pt)
  Float ->      -- ^ initial display scale
  SdlFont ->    -- ^ initial base sans font
  FontMetrics -> -- ^ initial base sans metrics
  SdlFont ->    -- ^ initial base mono font
  FontMetrics -> -- ^ initial base mono metrics
  IO SdlFontCache
newSdlFontCache primary fallback mono monoFb ga basePt scale baseFont baseFm monoFont monoFm = do
  scaleRef <- newIORef scale
  primaryRef <- newIORef primary
  sansEntry <- makeCachedFontEntry baseFont baseFm scale
  monoEntry <- makeCachedFontEntry monoFont monoFm scale
  baseEntriesRef <- newIORef (sansEntry, monoEntry)
  cacheRef <- newIORef (DynamicCache Map.empty [])
  pure
    SdlFontCache
      { sfcPrimarySourceRef = primaryRef
      , sfcFallbackSource = fallback
      , sfcMonoSource     = mono
      , sfcMonoFallback   = monoFb
      , sfcGlyphAtlas     = ga
      , sfcBasePt         = basePt
      , sfcScaleRef       = scaleRef
      , sfcBaseEntries    = baseEntriesRef
      , sfcDynamicCache   = cacheRef
      }

-- | Point the cache's primary (sans) family at a new source. Dynamic-size
-- entries created afterwards resolve against it; call 'resetSdlFontCache'
-- with a freshly opened base font to rebuild the base entry too.
setSdlFontCacheSource :: SdlFontCache -> FontSource -> IO ()
setSdlFontCacheSource cache src = writeIORef (sfcPrimarySourceRef cache) src

destroySdlFontCache :: SdlFontCache -> IO ()
destroySdlFontCache cache = do
  dc <- readIORef (sfcDynamicCache cache)
  writeIORef (sfcDynamicCache cache) (DynamicCache Map.empty [])
  let closedFonts = map cfeFont (Map.elems (dcEntries dc))
      closedIds = map sfId closedFonts
  mapM_ closeFont closedFonts
  let idSet = Set.fromList closedIds
  modifyIORef' (gaEntries (sfcGlyphAtlas cache)) (Map.filterWithKey (\(fid, _) _ -> not (Set.member fid idSet)))

resetSdlFontCache ::
  SdlFontCache ->
  Float ->
  SdlFont ->
  FontMetrics ->
  SdlFont ->
  FontMetrics ->
  IO ()
resetSdlFontCache cache newScale newBaseFont newBaseFm newMonoFont newMonoFm = do
  dc <- readIORef (sfcDynamicCache cache)
  writeIORef (sfcDynamicCache cache) (DynamicCache Map.empty [])
  writeIORef (sfcScaleRef cache) newScale
  sansEntry <- makeCachedFontEntry newBaseFont newBaseFm newScale
  monoEntry <- makeCachedFontEntry newMonoFont newMonoFm newScale
  writeIORef (sfcBaseEntries cache) (sansEntry, monoEntry)
  let closedFonts = map cfeFont (Map.elems (dcEntries dc))
      closedIds = map sfId closedFonts
  mapM_ closeFont closedFonts
  let idSet = Set.fromList closedIds
  modifyIORef' (gaEntries (sfcGlyphAtlas cache)) (Map.filterWithKey (\(fid, _) _ -> not (Set.member fid idSet)))

evictOldestIfNeeded :: GlyphAtlas -> DynamicCache -> IO DynamicCache
evictOldestIfNeeded ga dc
  | length (dcLru dc) < 48 = pure dc
  | otherwise =
      case reverse (dcLru dc) of
        [] -> pure dc
        (victim : _) -> do
          case Map.lookup victim (dcEntries dc) of
            Just victimEntry -> do
              let vSf = cfeFont victimEntry
                  vId = sfId vSf
              closeFont vSf
              modifyIORef' (gaEntries ga) (Map.filterWithKey (\(fid, _) _ -> fid /= vId))
            Nothing -> pure ()
          pure DynamicCache
            { dcEntries = Map.delete victim (dcEntries dc)
            , dcLru     = filter (/= victim) (dcLru dc)
            }

getOrLoadCachedFont ::
  SdlFontCache ->
  Float ->
  FontWeight ->
  FontStyle ->
  FontVariant ->
  IO CachedFontEntry
getOrLoadCachedFont cache sz weight style var = do
  let basePt = sfcBasePt cache
      rawPt = if sz > 0 then sz else basePt
      -- Quantize dynamic sizes to 0.5 pt increments so dragging sliders
      -- doesn't create hundreds of redundant TTF_Font instances.
      targetPt = fromIntegral (round (rawPt * 2.0) :: Int) / 2.0
      ptKey = round (targetPt * 2.0) :: Int
      isBold = weight == WeightBold
      isItalic = style == FontStyleItalic
      basePtKey = round (basePt * 2.0) :: Int
      isBase =
        ptKey == basePtKey && not isBold && not isItalic
  if isBase
    then do
      (sansEntry, monoEntry) <- readIORef (sfcBaseEntries cache)
      pure (if var == FontMono then monoEntry else sansEntry)
    else do
      let key = FontCacheKey var ptKey isBold isItalic
      dc <- readIORef (sfcDynamicCache cache)
      case Map.lookup key (dcEntries dc) of
        Just entry -> do
          case dcLru dc of
            (h:_) | h == key -> pure ()
            _ -> writeIORef (sfcDynamicCache cache) dc { dcLru = key : filter (/= key) (dcLru dc) }
          pure entry
        Nothing -> do
          dcClean <- evictOldestIfNeeded (sfcGlyphAtlas cache) dc
          scale <- readIORef (sfcScaleRef cache)
          primarySans <- readIORef (sfcPrimarySourceRef cache)
          let (primary, fallback) =
                if var == FontMono
                  then (sfcMonoSource cache, sfcMonoFallback cache)
                  else (primarySans, sfcFallbackSource cache)
              rasterPt = targetPt * (if scale > 0 then scale else 1.0)
          font <- openFontSourceWithFallback primary fallback rasterPt
          let boldBit = if isBold then 0x01 else 0
              italicBit = if isItalic then 0x02 else 0
              flags = boldBit .|. italicBit
          when (flags /= 0) $ ttfSetFontStyle (sfFont font) flags
          -- Note: We intentionally do NOT call warmGlyphAtlas here.
          -- Dynamic fonts insert only glyphs actually drawn on screen.
          fm <- buildGlyphFontMetrics (sfcGlyphAtlas cache) font scale
          entry <- makeCachedFontEntry font fm scale
          let newDc = DynamicCache
                { dcEntries = Map.insert key entry (dcEntries dcClean)
                , dcLru     = key : dcLru dcClean
                }
          writeIORef (sfcDynamicCache cache) newDc
          pure entry

withTtfFontCache :: SdlFontCache -> Context -> Context
withTtfFontCache cache ctx =
  withFontResolver ctx (resolveSdlFont cache) (resolveSdlMeasure cache)

resolveSdlFont ::
  SdlFontCache ->
  Float ->
  FontWeight ->
  FontStyle ->
  FontVariant ->
  IO (FontMetrics, Bool)
resolveSdlFont cache sz weight style var = do
  entry <- getOrLoadCachedFont cache sz weight style var
  pure (cfeFm entry, True)

resolveSdlMeasure ::
  SdlFontCache ->
  Float ->
  FontWeight ->
  FontStyle ->
  FontVariant ->
  Text ->
  IO (Float, Float)
resolveSdlMeasure cache sz weight style var txt = do
  entry <- getOrLoadCachedFont cache sz weight style var
  cfeMeasure entry txt
