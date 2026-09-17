module NanoUI.Sdl.Font
  ( FontSource (..)
  , GlyphAtlas
  , withTtf
  , fontSourceLabel
  , newGlyphAtlas
  , destroyGlyphAtlas
  , prepareGlyphAtlasForFrame
  , takeGlyphAtlasResetFlag
  , glyphAtlasTexture
  , SdlFontCache
  , newSdlFontCache
  , destroySdlFontCache
  , reloadSdlFontCache
  , sdlFontCacheSource
  , withSdlFontCache
  ) where

import Control.Exception (SomeException, bracket, catch, throwIO)
import Control.Monad (forM, forM_, unless, void, when)
import Data.Bits ((.&.), (.|.), shiftL)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (advancePtr, allocaArray)
import Data.Char (isPrint, isSpace, ord)
import NanoUI.Bidi (BidiRun (..), bidiRuns, needsBidi)
import NanoUI.Sdl.Font.Search (searchFontFamilies)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable (..))
import qualified Data.IntSet as IS
import Data.Primitive.SmallArray
  ( SmallArray
  , indexSmallArray
  , newSmallArray
  , readSmallArray
  , sizeofSmallArray
  , smallArrayFromList
  , writeSmallArray
  )
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, newPrimArray, primArrayFromList, readPrimArray, setPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Int (Int32)
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import qualified Data.ByteString.Short as SBS
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Data.Text.Unsafe (lengthWord8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CFloat (..), CInt (..), CSize (..), CUInt (..))
import Foreign.Ptr (IntPtr (..), Ptr, castPtr, intPtrToPtr, nullPtr, plusPtr, ptrToIntPtr)
import Foreign.Storable (peek, peekElemOff, poke, sizeOf)
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
  , ShapedGlyphs (..)
  , ShapedText (..)
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
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Foreign as TF

data SdlFont = SdlFont
  { sfId :: !Word64
  , sfFont :: Ptr ()
  , sfLineSkip :: Float
  , sfAscent :: Float
  , sfSpaceAdvance :: Float
  , sfTempPath :: !(Maybe FilePath)
  , sfAlive :: !(IORef Bool)
  , sfPointSize :: !Float
  -- ^ The size the font was opened at, which its fallbacks open at too.
  , sfFallbacks :: !(IORef (IM.IntMap SdlFont))
  -- ^ Fonts shaping falls back to for characters this one lacks, by their
  -- place in 'coverageFamilies': each is attached, at this font's size, the
  -- first time a text needs a character only it covers.
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

data GlyphAtlas = GlyphAtlas
  { gaAtlas :: !(Ptr ())
  , -- | Glyph slots by font id, then codepoint. Closing a font drops its inner
    -- map instead of scanning every glyph.
    gaEntries :: !(IORef (IM.IntMap (IM.IntMap (Maybe GlyphSlot))))
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
  , -- | Glyph slots by font id, then glyph index: what shaped text draws.
    gaIndexEntries :: !(IORef (IM.IntMap (IM.IntMap (Maybe GlyphSlot))))
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
cacheableText txt = T.compareLength txt 4096 /= GT

-- | A hash map bounded by entry count: 'insertBounded' into a full cache evicts
-- the first key of 'bcOrder' and returns its value so the owner can release
-- it. Hashing a text key once beats comparing it at every level of a tree.
data BoundedCache k v = BoundedCache
  { bcEntries :: !(HM.HashMap k v)
  , bcOrder :: !(Seq.Seq k)
  -- ^ Each key once, oldest first. Its length is the entry count, which
  -- 'HM.size' would have to count.
  }

emptyBounded :: BoundedCache k v
emptyBounded = BoundedCache HM.empty Seq.empty

insertBounded :: Hashable k => Int -> k -> v -> BoundedCache k v -> (BoundedCache k v, Maybe v)
insertBounded cap k v (BoundedCache m order) =
  case HM.alterF (\old -> (old, Just v)) k m of
    (Just _, m') -> (BoundedCache m' order, Nothing)
    (Nothing, m')
      | Seq.length order >= cap
      , victim Seq.:<| rest <- order ->
          let (evicted, m'') = HM.alterF (\old -> (old, Nothing)) victim m'
           in (BoundedCache m'' (rest Seq.|> k), evicted)
      | otherwise -> (BoundedCache m' (order Seq.|> k), Nothing)

-- Native glyph measurements have one representation, shared by metric-only
-- preparation and atlas placement. Pixel bearings are unscaled here.
data GlyphMetrics = GlyphMetrics
  { gmMinX :: !Float
  , gmMaxX :: !Float
  , gmMinY :: !Float
  , gmMaxY :: !Float
  , gmAdvance :: !Float
  }

getGlyphMetrics :: SdlFont -> CUInt -> IO (Maybe GlyphMetrics)
getGlyphMetrics sf cp = allocaArray 5 $ \p -> do
  ok <-
    ttfGlyphMetrics
      (sfFont sf)
      cp
      p
      (p `advancePtr` 1)
      (p `advancePtr` 2)
      (p `advancePtr` 3)
      (p `advancePtr` 4)
  let
    metric i = fromIntegral <$> peekElemOff p i
  if ok
    then
      Just
        <$> (GlyphMetrics <$> metric 0 <*> metric 1 <*> metric 2 <*> metric 3 <*> metric 4)
    else pure Nothing

getGlyphAdvance :: SdlFont -> CUInt -> IO (Maybe Float)
getGlyphAdvance sf cp = fmap gmAdvance <$> getGlyphMetrics sf cp

-- Metric-only geometry has no atlas lifetime and never rasterises a surface.
getGlyphGeometry :: SdlFont -> Float -> Char -> IO (Maybe GlyphQuad)
getGlyphGeometry sf inv c =
  fmap (metricsGlyphQuad sf inv) <$> getGlyphMetrics sf (fromIntegral (ord c))

metricsGlyphQuad :: SdlFont -> Float -> GlyphMetrics -> GlyphQuad
metricsGlyphQuad sf inv metrics =
  GlyphQuad
    (gmMinX metrics / inv)
    ((sfAscent sf - gmMaxY metrics) / inv)
    ((gmMaxX metrics - gmMinX metrics) / inv)
    ((gmMaxY metrics - gmMinY metrics) / inv)
    0
    0
    0
    0

newGlyphAtlas :: Ptr SDL_Renderer -> IO GlyphAtlas
newGlyphAtlas ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed (glyph)"
  entries <- newIORef IM.empty
  indexEntries <- newIORef IM.empty
  epoch <- newIORef 0
  needsReset <- newIORef False
  resetFlag <- newIORef False
  rewarms <- newIORef []
  alive <- newIORef True
  pure
    GlyphAtlas
      { gaAtlas = atlas
      , gaEntries = entries
      , gaIndexEntries = indexEntries
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

-- | Register an action to run after every atlas reset (DPI change, font
-- switch, exhaustion recovery). 'newSdlFontCache' registers one that re-warms
-- the base fonts' ASCII glyphs, so the next frame pays no cold glyph misses.
registerGlyphAtlasRewarm :: GlyphAtlas -> IO () -> IO ()
registerGlyphAtlasRewarm ga hook = modifyIORef' (gaRewarmHooks ga) (hook :)

resetGlyphAtlas :: GlyphAtlas -> IO ()
resetGlyphAtlas ga = do
  modifyIORef' (gaEpoch ga) (+1)
  writeIORef (gaEntries ga) IM.empty
  writeIORef (gaIndexEntries ga) IM.empty
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
  entries <- readIORef (gaEntries ga)
  case IM.lookup (fromIntegral (sfId sf)) entries >>= IM.lookup (ord c) of
    Just mSlot -> pure mSlot
    Nothing -> do
      let !cp = fromIntegral (ord c) :: CUInt
      mMetrics <- getGlyphMetrics sf cp
      mSlot <- case mMetrics of
        Nothing -> pure Nothing
        Just metrics ->
          placeGlyphImage ga (ttfRenderGlyphSurface (sfFont sf) cp) >>= \case
            Nothing -> pure Nothing
            Just slot -> do
              -- TTF_GetGlyphImage is a tight bitmap. Place it with the font
              -- bearings: pen + minX, lineTop + (ascent - maxY). Do not clamp
              -- minX; monospace glyphs are often centered (minX > 0).
              let !placed = slot {gsOffX = gmMinX metrics, gsOffY = sfAscent sf - gmMaxY metrics, gsAdvX = gmAdvance metrics}
              pure (Just placed)
      modifyIORef' (gaEntries ga) (IM.insertWith IM.union (fromIntegral (sfId sf)) (IM.singleton (ord c) mSlot))
      pure mSlot

-- | Look up or insert a glyph by font and glyph index, the way shaped text
-- names glyphs. Glyphs are keyed by the font's id, which is never reused, and
-- rendered through its handle.
lookupOrInsertGlyphIndex :: GlyphAtlas -> Int -> Int -> Int -> IO (Maybe GlyphSlot)
lookupOrInsertGlyphIndex ga fontKey handle gi = do
  entries <- readIORef (gaIndexEntries ga)
  case IM.lookup fontKey entries >>= IM.lookup gi of
    Just mSlot -> pure mSlot
    Nothing -> do
      mSlot <- placeGlyphImage ga (ttfRenderGlyphIndexSurface (intPtrToPtr (IntPtr handle)) (fromIntegral gi))
      modifyIORef' (gaIndexEntries ga) (IM.insertWith IM.union fontKey (IM.singleton gi mSlot))
      pure mSlot

-- | Render a glyph image into a surface and copy it into the atlas, as a slot
-- with no bearings or advance. 'Nothing' when there is no image or no room.
-- A full atlas is reset at the next frame start (see 'markAtlasExhausted'):
-- wiping the texture here would leave quads already recorded this frame
-- sampling blank pixels. The glyph is unavailable for the rest of the frame,
-- which is dropped.
placeGlyphImage :: GlyphAtlas -> (Ptr (Ptr ()) -> IO Bool) -> IO (Maybe GlyphSlot)
placeGlyphImage ga render = do
  surf <- alloca $ \sp -> do
    poke sp nullPtr
    ok <- render sp
    if ok then peek sp else pure nullPtr
  if surf == nullPtr
    then pure Nothing
    else do
      mPos <- tryInsert (gaAtlas ga) surf
      freeSurface surf
      case mPos of
        Nothing -> do
          markAtlasExhausted ga
          pure Nothing
        Just (px, py, tw, th) -> do
          let !slot =
                GlyphSlot
                  { gsW = tw
                  , gsH = th
                  , gsU0 = px / glyphAtlasSize
                  , gsV0 = py / glyphAtlasSize
                  , gsU1 = (px + tw) / glyphAtlasSize
                  , gsV1 = (py + th) / glyphAtlasSize
                  , gsOffX = 0
                  , gsOffY = 0
                  , gsAdvX = 0
                  }
          pure (Just slot)

-- | Width and height of the glyph atlas texture; mirrors
-- NANO_UI_TEXT_ATLAS_SIZE in nano_ui_text_atlas.c.
glyphAtlasSize :: Float
glyphAtlasSize = 2048

-- | A line shaped by SDL_ttf: its layout for measuring and caret placement,
-- its measured width and height, and per glyph nine numbers (glyph index, destination x y w h,
-- source x y w h, in raster pixels) with the index of the font that has it
-- among the line's font and its fallbacks.
data Shaped = Shaped
  { shapedText :: !ShapedText
  , shapedSize :: !(Float, Float)
  -- ^ Kept whole so measuring a cached line allocates nothing.
  , _shapedGlyphs :: !(PrimArray Int32)
  , _shapedFontIndices :: !(PrimArray Int32)
  , _shapedFonts :: !(SmallArray SdlFont)
  }

-- | The pieces of a line shaped one at a time, in visual order: character
-- start, end, and the SDL_ttf direction (0 for a line in one direction).
-- SDL_ttf keeps a right-to-left text's edge spaces on the side they are
-- stored, so they are shaped apart and placed on the side they read.
shapingRuns :: Text -> [(Int, Int, CInt)]
shapingRuns txt
  | needsBidi txt = concatMap directed (bidiRuns txt)
  | otherwise = [(0, T.length txt, 0)]
  where
    directed r
      | not (runRightToLeft r) = [(runStart r, runEnd r, 4)]
      | otherwise =
          let run = T.take (runEnd r - runStart r) (T.drop (runStart r) txt)
              lead = T.length (T.takeWhile isSpace run)
              trail = T.length (T.takeWhileEnd isSpace run)
              coreStart = runStart r + lead
              coreEnd = runEnd r - trail
           in if coreStart >= coreEnd
                then [(runStart r, runEnd r, 4)]
                else
                  [(coreEnd, runEnd r, 4) | trail > 0]
                    ++ [(coreStart, coreEnd, 5)]
                    ++ [(runStart r, coreStart, 4) | lead > 0]

-- | Shape one line with the font and its fallbacks. A line mixing
-- directions is split into direction runs, each shaped on its own and placed
-- in visual order, since SDL_ttf shapes a text in one direction. Caret
-- positions come from the clusters: a cluster's characters share its width,
-- from its left edge in a left-to-right run and from its right edge in a
-- right-to-left one.
shapeLine :: SdlFont -> Float -> Text -> IO Shaped
shapeLine sf inv txt = do
  ensureFontAlive sf
  fallbacks <- IM.elems <$> readIORef (sfFallbacks sf)
  let fontList = sf : fallbacks
      n = T.length txt
      totalBytes = lengthWord8 txt
      runs = shapingRuns txt
      pieceCount = length runs
      ascii = totalBytes == n
  -- The byte each character starts at, and the character starting at each
  -- byte (n inside a character and at the end). ASCII needs neither.
  byteOfChar <- newPrimArray (if ascii then 0 else n + 1)
  charOfByte <- newPrimArray (if ascii then 0 else totalBytes + 1)
  let byteAt i = if ascii then pure i else readPrimArray byteOfChar i
      charAt b = if ascii then pure (min n b) else readPrimArray charOfByte (min totalBytes b)
      indexChars !i !b t = do
        writePrimArray byteOfChar i b
        case T.uncons t of
          Nothing -> pure ()
          Just (c, rest) -> do
            writePrimArray charOfByte b i
            indexChars (i + 1) (b + utf8Length c) rest
  unless ascii $ do
    setPrimArray charOfByte 0 (totalBytes + 1) n
    indexChars 0 0 txt
  size <- fromIntegral <$> ttfShapedSize
  allocaBytes (size * max 1 pieceCount) $ \outs -> do
    let resultOf p = outs `plusPtr` (p * size)
    -- Shape every piece first, so the output arrays are sized once.
    let shapeAll !_ [] = pure ()
        shapeAll !p ((start, end, dir) : rest) = do
          withUtf8 (T.take (end - start) (T.drop start txt)) $ \cstr len -> do
            ok <- ttfShape (sfFont sf) cstr len dir (resultOf p)
            unless ok $ ttfShapedFree (resultOf p)
          shapeAll (p + 1) rest
    shapeAll 0 runs
    let countGlyphs !p !acc
          | p >= pieceCount = pure acc
          | otherwise = do
              g <- ttfShapedInt (resultOf p) 2
              countGlyphs (p + 1) (acc + fromIntegral g)
    glyphCount <- countGlyphs 0 0
    glyphs <- newPrimArray (glyphCount * 9)
    fontIndices <- newPrimArray glyphCount
    -- Caret stops by character in raster pixels, NaN where no cluster
    -- starts; a later cluster covering a character wins.
    stops <- newPrimArray n
    setPrimArray stops 0 n (0 / 0 :: Float)
    let fillPieces !p !pen !height !g0 !inkEnd !endStop pieces = case pieces of
          [] -> pure (pen, height, inkEnd, endStop)
          (start, _, _) : rest -> do
            let result = resultOf p
            byteStart <- byteAt start
            w <- fromIntegral <$> ttfShapedInt result 0
            h <- fromIntegral <$> ttfShapedInt result 1
            nGlyphs <- fromIntegral <$> ttfShapedInt result 2
            nClusters <- fromIntegral <$> ttfShapedInt result 3
            glyphPtr <- castPtr <$> ttfShapedPtr result 0
            fontPtr <- castPtr <$> ttfShapedPtr result 1
            clusterPtr <- castPtr <$> ttfShapedPtr result 2
            let glyphInt :: Int -> IO Int32
                glyphInt k = fromIntegral <$> peekElemOff (glyphPtr :: Ptr CInt) k
                fontIndex ptr = go 0 fontList
                  where
                    go !k (f : fs) = if sfFont f == ptr then k else go (k + 1) fs
                    go !_ [] = 0
                -- SDL_ttf's ten numbers a glyph start with its text offset,
                -- which carets take from the clusters instead.
                copyGlyphs !i !ink
                  | i >= nGlyphs = pure ink
                  | otherwise = do
                      let o = (g0 + i) * 9
                          field !k
                            | k > 9 = pure ()
                            | otherwise = do
                                v <- glyphInt (i * 10 + k)
                                writePrimArray glyphs (o + k - 1) (if k == 2 then v + fromIntegral pen else v)
                                field (k + 1)
                      field 1
                      x <- glyphInt (i * 10 + 2)
                      gw <- glyphInt (i * 10 + 4)
                      ptr <- peekElemOff (fontPtr :: Ptr (Ptr ())) i
                      writePrimArray fontIndices (g0 + i) (fontIndex ptr)
                      copyGlyphs (i + 1) (max ink (fromIntegral x + pen + fromIntegral gw))
                clusterInt :: Int -> IO Int
                clusterInt k = fromIntegral <$> peekElemOff (clusterPtr :: Ptr CInt) k
                -- A cluster's characters share its width, from its left edge
                -- left to right and from its right edge right to left.
                placeClusters !i !end
                  | i >= nClusters = pure end
                  | otherwise = do
                      off0 <- clusterInt (i * 5)
                      len <- clusterInt (i * 5 + 1)
                      x0 <- clusterInt (i * 5 + 2)
                      cw <- clusterInt (i * 5 + 3)
                      flags <- clusterInt (i * 5 + 4)
                      if len <= 0
                        then placeClusters (i + 1) end
                        else do
                          let off = off0 + byteStart
                              x = fromIntegral (x0 + pen) :: Float
                              fw = fromIntegral cw
                              rtl = flags .&. 0xFF == 5
                          c0 <- charAt off
                          c1 <- charAt (off + len)
                          let k = max 1 (c1 - c0)
                              stop j
                                | c0 + j >= n = pure ()
                                | rtl = writePrimArray stops (c0 + j) (x + fw - fw * fromIntegral j / fromIntegral k)
                                | otherwise = writePrimArray stops (c0 + j) (x + fw * fromIntegral j / fromIntegral k)
                          forM_ [0 .. k - 1] stop
                          let end'
                                | isNaN end && c1 == n = if rtl then x else x + fw
                                | otherwise = end
                          placeClusters (i + 1) end'
            ink <- if glyphPtr == nullPtr then pure inkEnd else copyGlyphs 0 inkEnd
            end <- if clusterPtr == nullPtr then pure endStop else placeClusters 0 endStop
            ttfShapedFree result
            fillPieces (p + 1) (pen + w) (max height h) (g0 + nGlyphs) ink end rest
    (total, height, inkEnd, endStop) <- fillPieces 0 0 (0 :: Int) 0 0 (0 / 0) runs
    carets <- newPrimArray (n + 1)
    let fillCarets !i !prev
          | i >= n = pure ()
          | otherwise = do
              v <- readPrimArray stops i
              let v' = if isNaN v then prev else v / inv
              writePrimArray carets i v'
              fillCarets (i + 1) v'
    fillCarets 0 0
    writePrimArray carets n ((if isNaN endStop then fromIntegral total else endStop) / inv)
    caretArr <- unsafeFreezePrimArray carets
    glyphArr <- unsafeFreezePrimArray glyphs
    indexArr <- unsafeFreezePrimArray fontIndices
    let !width = fromIntegral total / inv
        !measured = (width, fromIntegral height / inv)
    pure (Shaped (ShapedText width (fromIntegral inkEnd / inv) caretArr) measured glyphArr indexArr (smallArrayFromList fontList))
  where
    utf8Length c
      | ord c < 0x80 = 1
      | ord c < 0x800 = 2
      | ord c < 0x10000 = 3
      | otherwise = 4 :: Int

-- | Open the fonts that cover what this one lacks, the first time a text
-- has a character it cannot draw.
ensureCoverage :: SdlFont -> Text -> IO ()
ensureCoverage sf txt =
  unless (T.all (\c -> ord c < 128) txt) $
    forM_ (T.unpack txt) $ \c ->
      when (ord c >= 128 && isPrint c) $ do
        -- Whether the font or a fallback it already has draws the character.
        has <- ttfHasGlyph (sfFont sf) (fromIntegral (ord c))
        unless has $
          coverageSourceFor c >>= \case
            Just (source, probe) -> attachFallback sf source probe
            Nothing -> pure ()

-- | Open the coverage source @source@ at the font's size, sharing the
-- source's stream with its probe, and attach it. Fallbacks stay in
-- 'coverageFamilies' order, so a character two of them draw comes from the
-- one listed first whichever was attached first.
attachFallback :: SdlFont -> Int -> Ptr () -> IO ()
attachFallback sf source probe = do
  attached <- readIORef (sfFallbacks sf)
  unless (IM.member source attached) $ do
    copy <- ttfCopyFont probe (realToFrac (sfPointSize sf))
    unless (copy == nullPtr) $ do
      fallback <- readSdlFont (sfPointSize sf) Nothing copy
      let attached' = IM.insert source fallback attached
      case IM.lookupMax attached of
        Just (lastSource, _) | lastSource > source -> do
          forM_ attached $ \f -> ttfRemoveFallback (sfFont sf) (sfFont f)
          forM_ attached' $ \f -> ttfAddFallback (sfFont sf) (sfFont f)
        _ -> void (ttfAddFallback (sfFont sf) copy)
      writeIORef (sfFallbacks sf) attached'

-- | What the process knows about coverage fonts: the installed files, found
-- once; a probe font for each opened so far (null when it failed to open),
-- which every size copies from; and the first source drawing each character
-- asked about (-1 for none).
data Coverage = Coverage
  { covSources :: !(SmallArray SBS.ShortByteString)
  -- ^ Paths as the file system's bytes, kept compactly for the session and
  -- handed to SDL_ttf as they are.
  , covProbes :: !(IM.IntMap (Ptr ()))
  , covChars :: !(IM.IntMap Int)
  }

{-# NOINLINE coverageRef #-}
coverageRef :: IORef (Maybe Coverage)
coverageRef = unsafePerformIO (newIORef Nothing)

-- | The first coverage source, in 'coverageFamilies' order, that draws the
-- character, and its probe. Probes open only as far down the list as a
-- search goes, once a session, whatever the number of font sizes.
coverageSourceFor :: Char -> IO (Maybe (Int, Ptr ()))
coverageSourceFor c = do
  cov0 <-
    readIORef coverageRef >>= \case
      Just cov -> pure cov
      Nothing -> do
        files <- searchFontFamilies coverageFamilies `catch` \(_ :: SomeException) -> pure []
        -- The file system encoding turns a path back into the bytes it was
        -- read from, including bytes that are not valid in that encoding.
        enc <- getFileSystemEncoding
        sources <- forM files $ \path -> GHC.withCStringLen enc path $ \cstr -> SBS.toShort <$> BS.packCStringLen cstr
        pure (Coverage (smallArrayFromList sources) IM.empty IM.empty)
  let cp = ord c
      probeOf cov i = case IM.lookup i (covProbes cov) of
        Just probe -> pure (probe, cov)
        Nothing -> do
          probe <- SBS.useAsCString (indexSmallArray (covSources cov) i) $ \cpath -> ttfOpenFont cpath 12
          pure (probe, cov {covProbes = IM.insert i probe (covProbes cov)})
      search cov i
        | i >= sizeofSmallArray (covSources cov) = pure (-1, cov)
        | otherwise = do
            (probe, cov') <- probeOf cov i
            has <- if probe == nullPtr then pure False else ttfHasGlyph probe (fromIntegral cp)
            if has then pure (i, cov') else search cov' (i + 1)
  (source, cov1) <- case IM.lookup cp (covChars cov0) of
    Just known -> pure (known, cov0)
    Nothing -> do
      (found, cov') <- search cov0 0
      pure (found, cov' {covChars = IM.insert cp found (covChars cov')})
  writeIORef coverageRef (Just cov1)
  pure $ case IM.lookup source (covProbes cov1) of
    Just probe | source >= 0 -> Just (source, probe)
    _ -> Nothing

-- | Close the coverage probes, before SDL_ttf shuts down. Fallbacks copied
-- from them keep their shared streams open until they close themselves.
closeCoverageProbes :: IO ()
closeCoverageProbes =
  readIORef coverageRef >>= \case
    Nothing -> pure ()
    Just cov -> do
      forM_ (covProbes cov) $ \probe -> unless (probe == nullPtr) (ttfCloseFont probe)
      writeIORef coverageRef (Just cov {covProbes = IM.empty})

-- | Fallback families in the order shaping tries them: broad Latin, Greek
-- and Cyrillic first, then scripts, then symbols, across Linux, Windows and
-- macOS names.
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

-- | ASCII glyph-cache slot. The cached 'Maybe' is shared on every hit, so a
-- warm lookup returns the same heap object instead of rebuilding
-- @Just GlyphQuad@ on each character.
data CachedQuad
  = UncachedQuad
  -- Preserve the cached object even with -funbox-strict-fields: unpacking it
  -- defeats sharing and reconstructs the lookup result on every hit.
  | Cached {-# NOUNPACK #-} !(Maybe GlyphQuad)

-- | Build immutable metric snapshots and explicit IO rasterisation callbacks.
-- Font queries happen in 'fbPrepare'; atlas insertion happens in 'fbDrawShaped'
-- and 'fbDrawGlyph'. All coordinates are logical (unscaled).
--
-- Standard ASCII (0..127) lookups are backed by a 'SmallMutableArray'
-- fast path for branchless O(1) in-memory indexing, with automatic cache invalidation
-- whenever the underlying glyph atlas is reset.
{-# NOINLINE buildGlyphFontMetrics #-}
buildGlyphFontMetrics :: GlyphAtlas -> SdlFont -> Float -> IO (FontMetrics, Text -> IO (Float, Float))
buildGlyphFontMetrics ga sf scale = do
  let !inv = if scale > 0 then scale else 1
      baseFm = ttfFontMetricsScaled sf scale

  -- Query ASCII metrics once for both advances and geometry, without atlas rasterization.
  asciiMetrics <- mapM (getGlyphMetrics sf) [0 .. 127 :: CUInt]
  -- Reuse fixed ASCII geometry across dynamic labels, so preparing a fresh
  -- counter string does not query native glyph metrics for each character.
  let !asciiGeometry = smallArrayFromList (map (fmap (metricsGlyphQuad sf inv)) asciiMetrics)
      !asciiAdvances =
        primArrayFromList
          [ case metrics of
              Nothing  -> sfSpaceAdvance sf / inv
              Just m -> gmAdvance m / inv
          | metrics <- asciiMetrics
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
  kernCacheRef <- newIORef emptyBounded

  -- Shaped lines: SDL3_ttf lays each string out with its kerning,
  -- ligatures, contextual forms, fallback fonts and right-to-left runs. A
  -- line's layout is kept with its metric snapshot, which survives atlas
  -- resets; the glyph quads drawn from it hold atlas UVs, so their cache is
  -- dropped with the atlas epoch. Both caches are bounded by 'runCacheCap'.
  preparedRef <- newIORef emptyBounded
  shapedRef <- newIORef emptyBounded
  quadCacheRef <- newIORef emptyBounded
  initQuadEpoch <- readIORef (gaEpoch ga)
  quadEpochRef <- newIORef initQuadEpoch

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
      cache <- readIORef kernCacheRef
      case HM.lookup pk (bcEntries cache) of
        Just k -> pure k
        Nothing -> do
          raw <- ttfGetKerning (sfFont sf) (fromIntegral (ord prev) :: CUInt) (fromIntegral (ord c) :: CUInt)
          let !k = fromIntegral raw / inv
          -- At most 4096 pairs per font.
          writeIORef kernCacheRef $! fst (insertBounded 4096 pk k cache)
          pure k

    -- The glyph quads of a shaped line, from the atlas. Quads are cached per
    -- text and dropped with the atlas epoch, when their UVs go stale.
    {-# NOINLINE shapedLookup #-}
    shapedLookup !txt
      | T.null txt = pure Nothing
      | otherwise = do
          ensureFontAlive sf
          ensureAtlasAlive ga
          ep <- readIORef (gaEpoch ga)
          quadEp <- readIORef quadEpochRef
          when (quadEp /= ep) $ do
            writeIORef quadEpochRef ep
            writeIORef quadCacheRef emptyBounded
          cache <- readIORef quadCacheRef
          -- Entries are kept wrapped so a hit returns them without allocating.
          case HM.lookup txt (bcEntries cache) of
            Just quads -> pure quads
            Nothing -> do
              shaped <- shapeOf txt
              quads <- Just <$> placeGlyphs shaped
              epAfter <- readIORef (gaEpoch ga)
              when (cacheableText txt && epAfter == ep) $
                modifyIORef' quadCacheRef (fst . insertBounded runCacheCap txt quads)
              pure quads

    -- Put a shaped line's glyphs in the atlas. A glyph the atlas has no room
    -- for draws nothing, and the atlas resets before the next frame.
    placeGlyphs (Shaped _ _ glyphs fontIndices fonts) = do
      let !count = sizeofPrimArray fontIndices
      out <- newPrimArray (count * 8)
      let go !i
            | i >= count = pure ()
            | otherwise = do
                let g k = fromIntegral (indexPrimArray glyphs (i * 9 + k)) :: Float
                    o = i * 8
                    write k v = writePrimArray out (o + k) v
                    font = indexSmallArray fonts (fromIntegral (indexPrimArray fontIndices i))
                    IntPtr handle = ptrToIntPtr (sfFont font)
                mSlot <- lookupOrInsertGlyphIndex ga (fromIntegral (sfId font)) handle (fromIntegral (indexPrimArray glyphs (i * 9)))
                write 0 (g 1 / inv)
                write 1 (g 2 / inv)
                write 2 (g 3 / inv)
                write 3 (g 4 / inv)
                case mSlot of
                  Just slot -> do
                    -- A glyph drawn in part samples only its source rect.
                    let sx = g 5
                        sy = g 6
                        sw = g 7
                        sh = g 8
                        u0 = gsU0 slot + sx / glyphAtlasSize
                        v0 = gsV0 slot + sy / glyphAtlasSize
                        u1 = if sw > 0 then u0 + sw / glyphAtlasSize else gsU1 slot
                        v1 = if sh > 0 then v0 + sh / glyphAtlasSize else gsV1 slot
                    write 4 u0
                    write 5 v0
                    write 6 u1
                    write 7 v1
                  Nothing -> do
                    let (u, v, _, _) = deadUv
                    write 4 u
                    write 5 v
                    write 6 u
                    write 7 v
                go (i + 1)
      go 0
      ShapedGlyphs <$> unsafeFreezePrimArray out

    -- A UV rect that always samples transparent pixels: column 4 sits
    -- right of the 4px white patch (columns 0..3) and left of the first
    -- slot (allocations start at x = 5), and the final row is never
    -- written because every slot keeps 1px of padding.
    deadUv :: (Float, Float, Float, Float)
    deadUv =
      let !u = 4.5 / glyphAtlasSize
          !v = (glyphAtlasSize - 0.5) / glyphAtlasSize
       in (u, v, u, v)

    -- The shaped layout of a line, shared by measuring, preparing and
    -- drawing it. Fonts that cover characters this one lacks join it before
    -- the line is shaped.
    shapeOf !txt = do
      shapedCache <- readIORef shapedRef
      case HM.lookup txt (bcEntries shapedCache) of
        Just shaped -> pure shaped
        Nothing -> do
          ensureCoverage sf txt
          shaped <- shapeLine sf inv txt
          when (cacheableText txt) $
            writeIORef shapedRef $! fst (insertBounded runCacheCap txt shaped shapedCache)
          pure shaped

    -- The width shaping draws with, so layout and drawing agree.
    measure !txt
      | T.null txt = pure emptySize
      | otherwise = shapedSize <$> shapeOf txt
    !emptySize = (0, sfLineSkip sf / inv)

    glyphGeometry c
      | ord c < 128 = pure (indexSmallArray asciiGeometry (ord c))
      | otherwise = getGlyphGeometry sf inv c

    backend = FontBackend prepareText shapedLookup glyphLookup

    prepareText txt = do
      ensureFontAlive sf
      prepared <- readIORef preparedRef
      case HM.lookup txt (bcEntries prepared) of
        Just fm -> pure fm
        Nothing -> do
          let insertChar m c = IM.insert (ord c) c m
              chars = T.foldl' insertChar (T.foldl' insertChar IM.empty " HxM") txt
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
          seedKerns <- gather IM.empty ' ' "xM"
          kerns <- gather seedKerns 'M' txt
          shaped <- if T.null txt then pure Nothing else Just <$> shapeOf txt
          let !layout = fmap shapedText shaped
          let !fm = baseFm
                { fmAdvance = \c ->
                    let cp = ord c
                     in if cp < 128 then indexPrimArray asciiAdvances cp
                          else IM.findWithDefault (sfSpaceAdvance sf / inv) cp advances
                , fmKerning = \a b -> IM.findWithDefault 0 ((ord a `shiftL` 21) .|. ord b) kerns
                , fmGlyph = \c -> IM.findWithDefault Nothing (ord c) geometry
                , fmShape = \t -> if t == txt then layout else Nothing
                , fmBackend = Just backend
                , fmSnapScale = inv
                }
          when (cacheableText txt) $
            writeIORef preparedRef $! fst (insertBounded runCacheCap txt fm prepared)
          pure fm

  fm <- prepareText ""
  pure (fm, measure)

-- | Return the SDL_Texture backing the glyph atlas, for passing to the renderer.
glyphAtlasTexture :: GlyphAtlas -> IO (Ptr SDL_Texture)
glyphAtlasTexture ga = textAtlasTexture (gaAtlas ga)

-- ---------------------------------------------------------------------------
withTtf :: IO a -> IO a
withTtf act =
  bracket startup shutdown $ \_ -> act
  where
    startup = do
      ok <- ttfInit
      when (not ok) $ fail "TTF_Init failed"
    shutdown _ = closeCoverageProbes >> ttfQuit

openFont :: FilePath -> Float -> IO SdlFont
openFont path ptsize =
  withCString path $ \cpath -> do
    font <- ttfOpenFont cpath (realToFrac ptsize)
    when (font == nullPtr) $
      fail ("TTF_OpenFont failed for " ++ path)
    readSdlFont ptsize Nothing font

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
    readSdlFont ptsize mTemp fontPtr

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

-- | Wrap an open TTF font; @mTemp@ is a temp file to delete on close.
readSdlFont :: Float -> Maybe FilePath -> Ptr () -> IO SdlFont
readSdlFont ptsize mTemp font = do
  fid <- newFontId
  alive <- newIORef True
  fallbacks <- newIORef IM.empty
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
      , sfTempPath = mTemp
      , sfPointSize = ptsize
      , sfFallbacks = fallbacks
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
    readIORef (sfFallbacks sf) >>= mapM_ closeFont

-- | Install glyph-atlas-backed 'FontMetrics' (from 'buildGlyphFontMetrics')
-- so that 'pushText' emits per-glyph textured quads into the draw arena.
-- Text measurement uses the primary font's shaped lines.
withTtfMeasureGlyph ::
  Context ->
  (Text -> IO (Float, Float)) -> -- ^ primary font measurement
  FontMetrics -> -- ^ glyph-atlas fm for primary font
  FontMetrics -> -- ^ glyph-atlas fm for mono font
  Float ->
  Context
withTtfMeasureGlyph ctx measure fm monoFm scale =
  let ctx1 =
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

foreign import ccall unsafe "nano_ui_ttf_space_advance"
  ttfSpaceAdvance :: Ptr () -> IO CFloat

foreign import ccall unsafe "nano_ui_text_atlas_create"
  textAtlasCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_destroy"
  textAtlasDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_reset"
  textAtlasReset :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_texture"
  textAtlasTexture :: Ptr () -> IO (Ptr SDL_Texture)

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

foreign import ccall unsafe "nano_ui_ttf_shape"
  ttfShape :: Ptr () -> CString -> CSize -> CInt -> Ptr () -> IO Bool

foreign import ccall unsafe "nano_ui_ttf_shaped_free"
  ttfShapedFree :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_ttf_shaped_size"
  ttfShapedSize :: IO CSize

foreign import ccall unsafe "nano_ui_ttf_shaped_int"
  ttfShapedInt :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "nano_ui_ttf_shaped_ptr"
  ttfShapedPtr :: Ptr () -> CInt -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_render_glyph_index_surface"
  ttfRenderGlyphIndexSurface :: Ptr () -> CUInt -> Ptr (Ptr ()) -> IO Bool

foreign import ccall unsafe "nano_ui_ttf_has_glyph"
  ttfHasGlyph :: Ptr () -> CUInt -> IO Bool

foreign import ccall unsafe "nano_ui_ttf_add_fallback"
  ttfAddFallback :: Ptr () -> Ptr () -> IO Bool

foreign import ccall unsafe "nano_ui_ttf_remove_fallback"
  ttfRemoveFallback :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_ttf_copy_font"
  ttfCopyFont :: Ptr () -> CFloat -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_get_kerning"
  ttfGetKerning :: Ptr () -> CUInt -> CUInt -> IO CInt

-- ---------------------------------------------------------------------------
-- Font cache: the base sans and mono faces, plus fonts opened per size and
-- variant on demand

-- | A font variant and its point size key, @round (targetPt * 2)@.
data FontCacheKey = FontCacheKey !FontVariant !Int
  deriving (Eq)

instance Hashable FontCacheKey where
  hashWithSalt s (FontCacheKey variant ptKey) =
    s `hashWithSalt` fromEnum variant `hashWithSalt` ptKey

data CachedFontEntry = CachedFontEntry
  { cfeFont    :: !SdlFont
  , cfeFm      :: !FontMetrics
  , cfeMeasure :: !(Text -> IO (Float, Float))
  }

data SdlFontCache = SdlFontCache
  { sfcPrimarySourceRef :: !(IORef FontSource)
  , sfcFallbackSource :: !FontSource
  , sfcMonoSource     :: !FontSource
  , sfcMonoFallback   :: !FontSource
  , sfcGlyphAtlas     :: !GlyphAtlas
  , sfcBasePt         :: !Float
  , sfcScaleRef       :: !(IORef Float)
  -- ^ The display scale, owned by the window and read here.
  , sfcBaseEntries    :: !(IORef (CachedFontEntry, CachedFontEntry))
  , sfcDynamicCache   :: !(IORef (BoundedCache FontCacheKey CachedFontEntry))
  }

-- | Open the base sans and mono fonts at the display scale, and re-warm them
-- into the glyph atlas after every atlas reset.
newSdlFontCache ::
  FontSource -> -- ^ primary font source
  FontSource -> -- ^ fallback font source
  FontSource -> -- ^ mono font source
  FontSource -> -- ^ mono fallback font source
  GlyphAtlas ->
  Float ->      -- ^ base font size (pt)
  IORef Float -> -- ^ display scale
  IO SdlFontCache
newSdlFontCache primary fallback mono monoFb ga basePt scaleRef = do
  scale <- readIORef scaleRef
  primaryRef <- newIORef primary
  sansEntry <- openCachedFont ga scale primary fallback basePt
  monoEntry <- openCachedFont ga scale mono monoFb basePt
  baseEntriesRef <- newIORef (sansEntry, monoEntry)
  cacheRef <- newIORef emptyBounded
  -- The hook reads the base entries when it runs, so a reset always warms the
  -- live fonts, never ones already closed.
  let rewarm = do
        (sans, monoBase) <- readIORef baseEntriesRef
        warmGlyphAtlas ga (cfeFont sans)
        warmGlyphAtlas ga (cfeFont monoBase)
  registerGlyphAtlasRewarm ga rewarm
  rewarm
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

-- | A font from a source (or its fallback) at a point size, rasterised at
-- the display scale, with its glyph metrics.
openCachedFont :: GlyphAtlas -> Float -> FontSource -> FontSource -> Float -> IO CachedFontEntry
openCachedFont ga scale primary fallback pt = do
  font <- openFontSourceWithFallback primary fallback (pt * scale)
  (fm, measure) <- buildGlyphFontMetrics ga font scale
  pure (CachedFontEntry font fm measure)

-- | The primary (sans) family's source, for the debug readout.
sdlFontCacheSource :: SdlFontCache -> IO FontSource
sdlFontCacheSource cache = readIORef (sfcPrimarySourceRef cache)

-- | Close fonts and drop their glyphs from the shared atlas index.
closeCachedFonts :: GlyphAtlas -> [SdlFont] -> IO ()
closeCachedFonts ga fonts = do
  fallbacks <- concat <$> mapM (fmap IM.elems . readIORef . sfFallbacks) fonts
  let handles = IS.fromList [fromIntegral (sfId f) | f <- fonts ++ fallbacks]
  mapM_ closeFont fonts
  modifyIORef' (gaEntries ga) (`IM.withoutKeys` IS.fromList (map (fromIntegral . sfId) fonts))
  modifyIORef' (gaIndexEntries ga) (`IM.withoutKeys` handles)

-- | Close every open font, base and dynamic.
destroySdlFontCache :: SdlFontCache -> IO ()
destroySdlFontCache cache = do
  dynamic <- atomicModifyIORef' (sfcDynamicCache cache) (\c -> (emptyBounded, c))
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  closeCachedFonts (sfcGlyphAtlas cache) (cfeFont sans : cfeFont mono : map cfeFont (HM.elems (bcEntries dynamic)))

-- | Reopen the base fonts from @source@ at the current display scale, close
-- every dynamic size, and reset the glyph atlas, which re-warms the new base
-- fonts.
reloadSdlFontCache :: SdlFontCache -> FontSource -> IO ()
reloadSdlFontCache cache source = do
  destroySdlFontCache cache
  writeIORef (sfcPrimarySourceRef cache) source
  scale <- readIORef (sfcScaleRef cache)
  let ga = sfcGlyphAtlas cache
  sansEntry <- openCachedFont ga scale source (sfcFallbackSource cache) (sfcBasePt cache)
  monoEntry <- openCachedFont ga scale (sfcMonoSource cache) (sfcMonoFallback cache) (sfcBasePt cache)
  writeIORef (sfcBaseEntries cache) (sansEntry, monoEntry)
  resetGlyphAtlas ga

-- | Install the cache's base fonts as the context's measurement and glyph
-- metrics, and its sizes and variants as the font resolver.
withSdlFontCache :: SdlFontCache -> Context -> IO Context
withSdlFontCache cache ctx = do
  scale <- readIORef (sfcScaleRef cache)
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  pure (withFontResolver (withTtfMeasureGlyph ctx (cfeMeasure sans) (cfeFm sans) (cfeFm mono) scale) (resolveSdlFont cache) (resolveSdlMeasure cache))

-- | The open font for a size and variant. Weight and style pick nothing
-- here: they are drawn synthetically over the regular face, because SDL_ttf's
-- style flags change its layout boxes but not the glyph images shaped text
-- draws, so a styled face would not line up.
getOrLoadCachedFont ::
  SdlFontCache ->
  Float ->
  FontWeight ->
  FontStyle ->
  FontVariant ->
  IO CachedFontEntry
getOrLoadCachedFont cache sz _weight _style var = do
  let basePt = sfcBasePt cache
      rawPt = if sz > 0 then sz else basePt
      -- Quantize dynamic sizes to 0.5 pt increments so dragging sliders
      -- doesn't create hundreds of redundant TTF_Font instances.
      targetPt = fromIntegral (round (rawPt * 2.0) :: Int) / 2.0
      ptKey = round (targetPt * 2.0) :: Int
      basePtKey = round (basePt * 2.0) :: Int
      isBase =
        ptKey == basePtKey
  if isBase
    then do
      (sansEntry, monoEntry) <- readIORef (sfcBaseEntries cache)
      pure (if var == FontMono then monoEntry else sansEntry)
    else do
      let key = FontCacheKey var ptKey
      dynamic <- readIORef (sfcDynamicCache cache)
      case HM.lookup key (bcEntries dynamic) of
        Just entry -> do
          -- Least recently used goes first: move a hit to the back of the
          -- eviction order, so fonts drawn every frame are never closed.
          case Seq.viewr (bcOrder dynamic) of
            _ Seq.:> newest | newest == key -> pure ()
            _ ->
              -- Each key appears in the order once, and a hot key sits near
              -- the back, so search from the right and delete that one entry.
              let order = bcOrder dynamic
               in writeIORef (sfcDynamicCache cache) $!
                    dynamic {bcOrder = maybe order (`Seq.deleteAt` order) (Seq.elemIndexR key order) Seq.|> key}
          pure entry
        Nothing -> do
          scale <- readIORef (sfcScaleRef cache)
          primarySans <- readIORef (sfcPrimarySourceRef cache)
          let (primary, fallback) =
                if var == FontMono
                  then (sfcMonoSource cache, sfcMonoFallback cache)
                  else (primarySans, sfcFallbackSource cache)
          -- Dynamic fonts are not warmed: they insert only glyphs drawn.
          entry <- openCachedFont (sfcGlyphAtlas cache) scale primary fallback targetPt
          -- At most 48 dynamic sizes stay open.
          let (dynamic', evicted) = insertBounded 48 key entry dynamic
          writeIORef (sfcDynamicCache cache) $! dynamic'
          mapM_ (closeCachedFonts (sfcGlyphAtlas cache) . pure . cfeFont) evicted
          pure entry

resolveSdlFont ::
  SdlFontCache ->
  Float ->
  FontWeight ->
  FontStyle ->
  FontVariant ->
  IO (FontMetrics, Bool)
resolveSdlFont cache sz weight style var = do
  entry <- getOrLoadCachedFont cache sz weight style var
  pure (cfeFm entry, False)

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
