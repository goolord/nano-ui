{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}

-- | SDL_ttf font handles, fallback selection, measurement snapshots, and glyph
-- atlas caching. Native operations run on the owning display thread; pure
-- snapshots remain usable after their font handles close.
module NanoUI.Sdl.Internal.Font
  ( FontSource (..)
  , GlyphAtlas
  , withTtf
  , fontSourceLabel
  , embeddedFontSource
  , newGlyphAtlas
  , destroyGlyphAtlas
  , prepareGlyphAtlasForFrame
  , takeGlyphAtlasResetFlag
  , glyphAtlasTextures
  , SdlFontCache
  , newSdlFontCache
  , destroySdlFontCache
  , reloadSdlFontCache
  , sdlFontCacheSource
  , withSdlFontCache
  ) where

import Control.Exception (SomeException, bracket_, catch, throwIO)
import Control.Monad (forM, forM_, unless, void, when, zipWithM_)
import Data.Bits ((.&.), (.|.), shiftL)
import Data.Foldable (traverse_)
import Data.List (delete)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray)
import Data.Char (isPrint, isSpace, ord)
import NanoUI.Bidi (BidiRun (..), bidiRuns, needsBidi)
import NanoUI.Sdl.Internal.Font.Inter (fontInterBytes, fontInterLabel)
import NanoUI.Sdl.Internal.Font.Search (searchFontFamilies)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.IntSet as IS
import Data.Primitive.SmallArray
  ( SmallArray
  , indexSmallArray
  , sizeofSmallArray
  , smallArrayFromList
  )
import Data.Primitive.PrimArray (PrimArray, indexPrimArray, newPrimArray, primArrayFromList, readPrimArray, setPrimArray, sizeofPrimArray, unsafeFreezePrimArray, writePrimArray)
import Data.Int (Int32)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Short as SBS
import qualified GHC.Foreign as GHC
import GHC.IO.Encoding (getFileSystemEncoding)
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
import Data.Text.Unsafe (lengthWord8)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..), CSize (..), CUInt (..))
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff, poke)
import Data.Unique (hashUnique, newUnique)
import qualified Data.ByteString as BS
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import NanoUI (FontVariant (..))
import NanoUI.Backend
  ( FontBackend (..)
  , FontMetrics (..)
  , GlyphQuad (..)
  , ShapedGlyphs (..)
  , ShapedText (..)
  , drawShaped
  , monospaceMetrics
  )
import NanoUI.Testing
  ( Context
  , glyphAtlasPages
  , withFontMetrics
  , withFontResolver
  , withMonoFontMetrics
  , wrapMeasureCache
  )
import SDL3.Sys.Bindgen.Render (SDL_Renderer, SDL_Texture)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text.Foreign as TF

data SdlFont = SdlFont
  { sfId :: !Int
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
  deriving (Eq, Show)

fontSourceLabel :: FontSource -> FilePath
fontSourceLabel (FontFromPath p) = p
fontSourceLabel (FontFromMemory _ label) = label

-- | Bundled Inter, which a font that will not open falls back to.
embeddedFontSource :: FontSource
embeddedFontSource = FontFromMemory fontInterBytes fontInterLabel

-- | Per-glyph atlas slot. UVs are normalised to [0,1] within the page, plus
-- the page's number in u, as 'ShapedGlyphs' carries them.
data GlyphSlot = GlyphSlot
  { gsU0 :: {-# UNPACK #-} !Float
  , gsV0 :: {-# UNPACK #-} !Float
  , gsU1 :: {-# UNPACK #-} !Float
  , gsV1 :: {-# UNPACK #-} !Float
  }

data GlyphAtlas = GlyphAtlas
  { gaAtlas :: !(Ptr ())
  , gaEpoch :: !(IORef Int)
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
    -- Closing a font drops its inner map instead of scanning every glyph.
    gaIndexEntries :: !(IORef (IM.IntMap (IM.IntMap (Maybe GlyphSlot))))
  , gaAlive :: !(IORef Bool)
  }

-- Backend effects are confined to the owning SDL thread. Retained snapshots
-- may outlive a window or a cache entry, but must never query a freed handle.
ensureAlive :: String -> IORef Bool -> IO ()
ensureAlive closer alive =
  readIORef alive >>= (`unless` fail ("font backend used after " ++ closer))

-- | Entries per generation of each shaped-run cache on a 'FontMetrics'.
-- Dynamic, ever-changing text (FPS counters, timers, percentages, mouse
-- positions) generates unique strings over time, so the caches are bounded.
-- Atlas exhaustion is recovered by the deferred reset in
-- 'prepareGlyphAtlasForFrame', which also clears the quad cache.
runCacheCap :: Int
runCacheCap = 1024

-- | Kerning pairs per generation of a font's pair cache.
kernCacheCap :: Int
kernCacheCap = 4096

-- | What a text counts for against a cache's entry limit: one per 4096 bytes
-- begun. An entry count alone would not bound what a cache holds, since an
-- edited long line would keep a whole version per keystroke; this keeps the
-- bound of a cache of short texts, and a long line that stays the same, as
-- one scrolled sideways, is shaped once rather than every frame.
textWeight :: Text -> Int
textWeight txt = 1 + lengthWord8 txt `quot` 4096

-- | A hash map kept in two generations, a cheap stand-in for least recently
-- used. Inserts go to the young map; a hit in the old map moves the entry up.
-- When the young map fills it becomes the old one and the old one is dropped,
-- so an entry survives while it is used once a generation, however many
-- one-off keys (wrap probes, table cells) pass through. Plain insertion order
-- evicted the oldest key even when every frame used it. Hashing a text key
-- once beats comparing it at every level of a tree.
-- The fields are the young map, its size and the old map. The size is the sum
-- of its entries' weights; an entry heavier than a whole generation is not
-- kept.
data GenCache k v = GenCache !(HM.HashMap k v) !Int !(HM.HashMap k v)

emptyGen :: GenCache k v
emptyGen = GenCache HM.empty 0 HM.empty

-- | Insert an entry of weight @w@ into a cache of @cap@ per generation.
insertGen :: Hashable k => Int -> Int -> k -> v -> GenCache k v -> GenCache k v
insertGen cap w k v cache@(GenCache young n old)
  | w > cap = cache
  | n + w > cap = GenCache (HM.singleton k v) w young
  | otherwise = GenCache (HM.insert k v young) (n + w) old

-- | The entry for @k@, or what @make@ returns, which is kept. Entries weigh
-- @w k@; an old one used again moves to the young map. Inlined so a hit
-- returns the stored value without boxing it in a 'Just' on the way: the SDL
-- bench's warm-lookup gate measures exactly that.
{-# INLINE cachedGen #-}
cachedGen :: Hashable k => Int -> (k -> Int) -> IORef (GenCache k v) -> k -> IO v -> IO v
cachedGen cap w ref k make = do
  cache@(GenCache young _ old) <- readIORef ref
  case HM.lookup k young of
    Just v -> pure v
    Nothing -> case HM.lookup k old of
      Just v -> do
        writeIORef ref $! insertGen cap (w k) k v cache
        pure v
      Nothing -> do
        v <- make
        modifyIORef' ref (insertGen cap (w k) k v)
        pure v

-- | A glyph's native measurements, shared by metric-only preparation and
-- atlas placement, in unscaled pixels: min x, max x, min y, max y, advance.
data GlyphMetrics = GlyphMetrics !Float !Float !Float !Float !Float

getGlyphMetrics :: SdlFont -> Int -> IO (Maybe GlyphMetrics)
getGlyphMetrics sf cp = allocaArray 5 $ \p -> do
  let at = advancePtr p
      metric i = fromIntegral <$> peekElemOff p i
  ok <- ttfGlyphMetrics (sfFont sf) (fromIntegral cp) p (at 1) (at 2) (at 3) (at 4)
  if ok
    then Just <$> (GlyphMetrics <$> metric 0 <*> metric 1 <*> metric 2 <*> metric 3 <*> metric 4)
    else pure Nothing

-- | Metric-only geometry: no atlas lifetime, and no surface rasterised.
metricsGlyphQuad :: SdlFont -> Float -> GlyphMetrics -> GlyphQuad
metricsGlyphQuad sf inv (GlyphMetrics minX maxX minY maxY _) =
  GlyphQuad (minX / inv) ((sfAscent sf - maxY) / inv) ((maxX - minX) / inv) ((maxY - minY) / inv)
    0 0 0 0

newGlyphAtlas :: Ptr SDL_Renderer -> IO GlyphAtlas
newGlyphAtlas ren = do
  atlas <- textAtlasCreate ren
  when (atlas == nullPtr) $ fail "nano_ui_text_atlas_create failed (glyph)"
  GlyphAtlas atlas
    <$> newIORef 0
    <*> newIORef False
    <*> newIORef False
    <*> newIORef IM.empty
    <*> newIORef True

destroyGlyphAtlas :: GlyphAtlas -> IO ()
destroyGlyphAtlas ga = do
  alive <- atomicModifyIORef' (gaAlive ga) (\open -> (False, open))
  when alive $ textAtlasDestroy (gaAtlas ga)

-- | Test-and-clear the mid-frame reset flag. 'True' means the atlas was
-- reset (or ran out of space) while the frame was being built, so quads
-- recorded before that point may hold stale UVs; the caller must not
-- present that frame.
takeGlyphAtlasResetFlag :: GlyphAtlas -> IO Bool
takeGlyphAtlasResetFlag ga = atomicModifyIORef' (gaResetFlag ga) (\v -> (False, v))

-- | Look up or insert a glyph by font and glyph index, the way shaped text
-- names glyphs. Glyphs are keyed by the font's id, which is never reused, and
-- rendered through its handle.
lookupOrInsertGlyph :: GlyphAtlas -> SdlFont -> Int -> IO (Maybe GlyphSlot)
lookupOrInsertGlyph ga font gi = do
  entries <- readIORef (gaIndexEntries ga)
  case IM.lookup (sfId font) entries >>= IM.lookup gi of
    Just mSlot -> pure mSlot
    Nothing -> do
      mSlot <- placeGlyphImage ga font gi
      modifyIORef' (gaIndexEntries ga) (IM.insertWith IM.union (sfId font) (IM.singleton gi mSlot))
      pure mSlot

-- | Render a glyph image into a surface and copy it into the atlas, on a new
-- page once the last one is full. 'Nothing' when there is no image or no
-- room on any page.
-- A full atlas is reset at the next frame start ('prepareGlyphAtlasForFrame'):
-- wiping the texture here would leave quads already recorded this frame
-- sampling blank pixels. The glyph is unavailable for the rest of the frame,
-- which is marked invalid so the runner drops it instead of presenting text
-- that could not be placed.
placeGlyphImage :: GlyphAtlas -> SdlFont -> Int -> IO (Maybe GlyphSlot)
placeGlyphImage ga font gi = do
  surf <- alloca $ \sp -> do
    poke sp nullPtr
    ok <- ttfRenderGlyphIndexSurface (sfFont font) (fromIntegral gi) sp
    if ok /= 0 then peek sp else pure nullPtr
  if surf == nullPtr
    then pure Nothing
    else alloca $ \pagePtr -> allocaArray 4 $ \out -> do
      -- The page, and the x, y, width and height in pixels on it.
      let at = advancePtr out
      placed <- textAtlasInsertSurface (gaAtlas ga) surf pagePtr out (at 1) (at 2) (at 3)
      freeSurface surf
      if placed == 0
        then do
          writeIORef (gaNeedsReset ga) True
          writeIORef (gaResetFlag ga) True
          pure Nothing
        else do
          page <- fromIntegral <$> peek pagePtr
          [x, y, w, h] <- map ((/ glyphAtlasSize) . realToFrac) <$> peekArray 4 out
          pure (Just (GlyphSlot (page + x) y (page + x + w) (y + h)))

-- | Width and height of the glyph atlas texture; mirrors
-- NANO_UI_TEXT_ATLAS_SIZE in nano_ui_text_atlas.c.
glyphAtlasSize :: Float
glyphAtlasSize = 2048

-- | A UV point that always samples transparent pixels, for a glyph the atlas
-- had no room for: column 4 sits right of the 4px white patch (columns 0..3)
-- and left of the first slot (allocations start at x = 5), and the final row
-- is never written because every slot keeps 1px of padding.
deadU, deadV :: Float
deadU = 4.5 / glyphAtlasSize
deadV = (glyphAtlasSize - 0.5) / glyphAtlasSize

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
  ensureAlive "closeFont" (sfAlive sf)
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
    forM_ (zip [0 ..] runs) $ \(p, (start, end, dir)) ->
      TF.useAsPtr (T.take (end - start) (T.drop start txt)) $ \ptr len -> do
        ok <- ttfShape (sfFont sf) (castPtr ptr) (fromIntegral len) dir (resultOf p)
        when (ok == 0) $ ttfShapedFree (resultOf p)
    glyphCount <- sum <$> mapM (\p -> fromIntegral <$> ttfShapedInt (resultOf p) 2) [0 .. pieceCount - 1]
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

-- | Open the fonts that cover what this one lacks, the first time a text
-- has a character it cannot draw.
ensureCoverage :: SdlFont -> Text -> IO ()
ensureCoverage sf txt =
  unless (T.all (\c -> ord c < 128) txt) $
    forM_ (T.unpack txt) $ \c ->
      when (ord c >= 128 && isPrint c) $ do
        -- Whether the font or a fallback it already has draws the character.
        has <- ttfHasGlyph (sfFont sf) (fromIntegral (ord c))
        unless has $ coverageSourceFor c >>= traverse_ (uncurry (attachFallback sf))

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
  cov0 <- readIORef coverageRef >>= maybe findSources pure
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
  where
    findSources = do
      files <- searchFontFamilies coverageFamilies `catch` \(_ :: SomeException) -> pure []
      -- The file system encoding turns a path back into the bytes it was
      -- read from, including bytes that are not valid in that encoding.
      enc <- getFileSystemEncoding
      sources <- forM files $ \path -> GHC.withCStringLen enc path (fmap SBS.toShort . BS.packCStringLen)
      pure (Coverage (smallArrayFromList sources) IM.empty IM.empty)

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

-- | Build immutable metric snapshots and explicit IO rasterisation callbacks.
-- Font queries happen in 'fbPrepare' and atlas insertion in 'fbDrawShaped'.
-- Every non-empty line is shaped, so the per-character glyph fallback draws
-- nothing. All coordinates are logical (unscaled).
{-# NOINLINE buildGlyphFontMetrics #-}
buildGlyphFontMetrics :: GlyphAtlas -> SdlFont -> Float -> IO (FontMetrics, Text -> IO (Float, Float))
buildGlyphFontMetrics ga sf scale = do
  let !inv = if scale > 0 then scale else 1
      advanceOf = maybe (sfSpaceAdvance sf / inv) (\(GlyphMetrics _ _ _ _ adv) -> adv / inv)

  -- Query ASCII metrics once for both advances and geometry, without atlas rasterization.
  asciiMetrics <- mapM (getGlyphMetrics sf) [0 .. 127]
  -- Reuse fixed ASCII geometry across dynamic labels, so preparing a fresh
  -- counter string does not query native glyph metrics for each character.
  let !asciiGeometry = smallArrayFromList (map (fmap (metricsGlyphQuad sf inv)) asciiMetrics)
      !asciiAdvances = primArrayFromList (map advanceOf asciiMetrics)

  -- Kerning pairs are sparse and each miss costs a shaped 2-glyph
  -- layout, so a pair cache keeps the hot pen loops off the FFI
  -- boundary after first contact. Keyed by packed codepoint pair on this
  -- 'FontMetrics' (the font id is implicit).
  kernCacheRef <- newIORef emptyGen

  -- Shaped lines: SDL3_ttf lays each string out with its kerning,
  -- ligatures, contextual forms, fallback fonts and right-to-left runs. A
  -- line's layout is kept with its metric snapshot, which survives atlas
  -- resets; the glyph quads drawn from it hold atlas UVs, so their cache is
  -- dropped with the atlas epoch. Each cache keeps two generations of
  -- 'runCacheCap' entries.
  preparedRef <- newIORef emptyGen
  shapedRef <- newIORef emptyGen
  quadCacheRef <- newIORef emptyGen
  quadEpochRef <- newIORef =<< readIORef (gaEpoch ga)

  let
    {-# NOINLINE advanceLookup #-}
    advanceLookup !c
      | ord c < 128 = pure (indexPrimArray asciiAdvances (ord c))
      | otherwise = getGlyphMetrics sf (ord c) >>= \m -> pure $! advanceOf m

    {-# NOINLINE kernLookup #-}
    kernLookup !prev !c = do
      -- The cache lives on this 'FontMetrics', so the font id is constant and
      -- the pair can be packed into a single Int key: no tuple on the hot path.
      let !pk = (ord prev `shiftL` 21) .|. ord c
      cachedGen kernCacheCap (const 1) kernCacheRef pk $ do
        raw <- ttfGetKerning (sfFont sf) (fromIntegral (ord prev) :: CUInt) (fromIntegral (ord c) :: CUInt)
        pure $! fromIntegral raw / inv

    -- The glyph quads of a shaped line, from the atlas. Quads are cached per
    -- text and dropped with the atlas epoch, when their UVs go stale.
    {-# NOINLINE shapedLookup #-}
    shapedLookup !txt
      | T.null txt = pure Nothing
      | otherwise = do
          ensureAlive "closeFont" (sfAlive sf)
          ensureAlive "destroyGlyphAtlas" (gaAlive ga)
          ep <- readIORef (gaEpoch ga)
          quadEp <- readIORef quadEpochRef
          when (quadEp /= ep) $ do
            writeIORef quadEpochRef ep
            writeIORef quadCacheRef emptyGen
          -- Entries are kept wrapped so a hit returns them without allocating.
          -- Placing glyphs never resets the atlas (a full one resets at the
          -- next frame start), so these quads belong to this epoch.
          cachedGen runCacheCap textWeight quadCacheRef txt $
            Just <$> (placeGlyphs =<< shapeOf txt)

    -- Put a shaped line's glyphs in the atlas. A glyph the atlas has no room
    -- for draws nothing, and the atlas resets before the next frame.
    placeGlyphs (Shaped _ _ glyphs fontIndices fonts) = do
      let !count = sizeofPrimArray fontIndices
      out <- newPrimArray (count * 8)
      forM_ [0 .. count - 1] $ \i -> do
        let g k = fromIntegral (indexPrimArray glyphs (i * 9 + k)) :: Float
            write k = writePrimArray out (i * 8 + k)
            font = indexSmallArray fonts (fromIntegral (indexPrimArray fontIndices i))
        mSlot <- lookupOrInsertGlyph ga font (fromIntegral (indexPrimArray glyphs (i * 9)))
        forM_ [0 .. 3] $ \k -> write k (g (k + 1) / inv)
        case mSlot of
          Just slot -> do
            -- A glyph drawn in part samples only its source rect.
            let u0 = gsU0 slot + g 5 / glyphAtlasSize
                v0 = gsV0 slot + g 6 / glyphAtlasSize
            write 4 u0
            write 5 v0
            write 6 (if g 7 > 0 then u0 + g 7 / glyphAtlasSize else gsU1 slot)
            write 7 (if g 8 > 0 then v0 + g 8 / glyphAtlasSize else gsV1 slot)
          Nothing -> zipWithM_ write [4 ..] [deadU, deadV, deadU, deadV]
      ShapedGlyphs <$> unsafeFreezePrimArray out

    -- The shaped layout of a line, shared by measuring, preparing and
    -- drawing it. Fonts that cover characters this one lacks join it before
    -- the line is shaped.
    shapeOf !txt =
      cachedGen runCacheCap textWeight shapedRef txt $ do
        ensureCoverage sf txt
        shapeLine sf inv txt

    -- The width shaping draws with, so layout and drawing agree.
    measure !txt
      | T.null txt = pure emptySize
      | otherwise = shapedSize <$> shapeOf txt
    !emptySize = (0, sfLineSkip sf / inv)

    glyphGeometry c
      | ord c < 128 = pure (indexSmallArray asciiGeometry (ord c))
      | otherwise = fmap (metricsGlyphQuad sf inv) <$> getGlyphMetrics sf (ord c)

    backend = FontBackend prepareText shapedLookup

    prepareText txt = do
      ensureAlive "closeFont" (sfAlive sf)
      cachedGen runCacheCap textWeight preparedRef txt $ do
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
        pure $!
          (monospaceMetrics (sfLineSkip sf / inv))
            { fmAscent = sfAscent sf / inv
            , fmAdvance = \c ->
                let cp = ord c
                 in if cp < 128 then indexPrimArray asciiAdvances cp
                      else IM.findWithDefault (sfSpaceAdvance sf / inv) cp advances
            , fmKerning = \a b -> IM.findWithDefault 0 ((ord a `shiftL` 21) .|. ord b) kerns
            , fmGlyph = \c -> IM.findWithDefault Nothing (ord c) geometry
            , fmShape = \t -> if t == txt then layout else Nothing
            , fmBackend = Just backend
            , fmSnapScale = inv
            }

  fm <- prepareText ""
  pure (fm, measure)

-- | The SDL_Textures of the glyph atlas's pages, by page, for passing to the
-- renderer. A page never opened has a null texture.
glyphAtlasTextures :: GlyphAtlas -> IO (Int -> Ptr SDL_Texture)
glyphAtlasTextures ga = do
  pages <- mapM (textAtlasTexture (gaAtlas ga) . fromIntegral) [0 .. glyphAtlasPages - 1]
  pure (\page -> fromMaybe nullPtr (listToMaybe (drop page pages)))

-- ---------------------------------------------------------------------------
withTtf :: IO a -> IO a
withTtf =
  bracket_
    (ttfInit >>= (`unless` fail "TTF_Init failed"))
    (closeCoverageProbes >> ttfQuit)

-- | Open a font at a point size, or bundled Inter when it will not open.
openFontSource :: FontSource -> Float -> IO SdlFont
openFontSource source ptsize =
  open source `catch` \(e :: SomeException) ->
    if source == embeddedFontSource
      then throwIO e
      else open embeddedFontSource `catch` \(_ :: SomeException) -> throwIO e
  where
    pt = realToFrac ptsize
    open (FontFromPath path) = do
      font <- withCString path (`ttfOpenFont` pt)
      when (font == nullPtr) $ fail ("TTF_OpenFont failed for " ++ path)
      readSdlFont ptsize Nothing font
    open (FontFromMemory bs label) = do
      mem <- unsafeUseAsCStringLen bs $ \(ptr, len) -> ttfOpenFontMemory (castPtr ptr) (fromIntegral len) pt
      (font, temp) <-
        if mem /= nullPtr
          then pure (mem, Nothing)
          else do
            (path, h) <- getTemporaryDirectory >>= (`openTempFile` "nano-ui-font-")
            BS.hPut h bs
            hClose h
            font <- withCString path (`ttfOpenFont` pt)
            if font == nullPtr then (nullPtr, Nothing) <$ removeFile path else pure (font, Just path)
      when (font == nullPtr) $ fail ("TTF_OpenFont failed for in-memory font " ++ label)
      readSdlFont ptsize temp font

-- | Wrap an open TTF font; @sfTempPath@ is a temp file to delete on close.
readSdlFont :: Float -> Maybe FilePath -> Ptr () -> IO SdlFont
readSdlFont sfPointSize sfTempPath sfFont = do
  sfId <- hashUnique <$> newUnique
  sfAlive <- newIORef True
  sfFallbacks <- newIORef IM.empty
  sfLineSkip <- fromIntegral <$> ttfLineSkip sfFont
  sfAscent <- fromIntegral <$> ttfAscent sfFont
  sfSpaceAdvance <- realToFrac <$> ttfSpaceAdvance sfFont
  pure SdlFont {..}

closeFont :: SdlFont -> IO ()
closeFont sf = do
  alive <- atomicModifyIORef' (sfAlive sf) (\open -> (False, open))
  when alive $ do
    ttfCloseFont (sfFont sf)
    mapM_ removeFile (sfTempPath sf)
    readIORef (sfFallbacks sf) >>= mapM_ closeFont

-- Header-checked imports also adapt C bool to Haskell Bool at the ABI boundary.
-- The ccall imports of nano-ui's own bool functions below return CBool: a C
-- bool defines only the low byte of the return register, which a Haskell
-- Bool result would read in full.
foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_Init"
  ttfInit :: IO Bool

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_Quit"
  ttfQuit :: IO ()

foreign import ccall unsafe "nano_ui_ttf_open_font"
  ttfOpenFont :: CString -> CFloat -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_open_font_memory"
  ttfOpenFontMemory :: Ptr () -> CSize -> CFloat -> IO (Ptr ())

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_CloseFont"
  ttfCloseFont :: Ptr () -> IO ()

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_GetFontLineSkip"
  ttfLineSkip :: Ptr () -> IO CInt

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_GetFontAscent"
  ttfAscent :: Ptr () -> IO CInt

foreign import ccall unsafe "nano_ui_ttf_space_advance"
  ttfSpaceAdvance :: Ptr () -> IO CFloat

foreign import ccall unsafe "nano_ui_text_atlas_create"
  textAtlasCreate :: Ptr SDL_Renderer -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_text_atlas_destroy"
  textAtlasDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_reset"
  textAtlasReset :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_text_atlas_texture"
  textAtlasTexture :: Ptr () -> CInt -> IO (Ptr SDL_Texture)

foreign import ccall unsafe "nano_ui_text_atlas_insert_surface"
  textAtlasInsertSurface ::
    Ptr () ->
    Ptr () ->
    Ptr CInt ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    Ptr CFloat ->
    IO CBool

foreign import ccall unsafe "SDL_DestroySurface"
  freeSurface :: Ptr () -> IO ()

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_GetGlyphMetrics"
  ttfGlyphMetrics ::
    Ptr () ->   -- font
    CUInt ->    -- codepoint
    Ptr CInt -> -- out_minx
    Ptr CInt -> -- out_maxx
    Ptr CInt -> -- out_miny
    Ptr CInt -> -- out_maxy
    Ptr CInt -> -- out_advance
    IO Bool

foreign import ccall unsafe "nano_ui_ttf_shape"
  ttfShape :: Ptr () -> CString -> CSize -> CInt -> Ptr () -> IO CBool

foreign import ccall unsafe "nano_ui_ttf_shaped_free"
  ttfShapedFree :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_ttf_shaped_size"
  ttfShapedSize :: IO CSize

foreign import ccall unsafe "nano_ui_ttf_shaped_int"
  ttfShapedInt :: Ptr () -> CInt -> IO CInt

foreign import ccall unsafe "nano_ui_ttf_shaped_ptr"
  ttfShapedPtr :: Ptr () -> CInt -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_render_glyph_index_surface"
  ttfRenderGlyphIndexSurface :: Ptr () -> CUInt -> Ptr (Ptr ()) -> IO CBool

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_FontHasGlyph"
  ttfHasGlyph :: Ptr () -> CUInt -> IO Bool

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_AddFallbackFont"
  ttfAddFallback :: Ptr () -> Ptr () -> IO Bool

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_RemoveFallbackFont"
  ttfRemoveFallback :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_ttf_copy_font"
  ttfCopyFont :: Ptr () -> CFloat -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_get_kerning"
  ttfGetKerning :: Ptr () -> CUInt -> CUInt -> IO CInt

-- ---------------------------------------------------------------------------
-- Font cache: the base sans and mono faces, plus fonts opened per size on
-- demand

data CachedFontEntry = CachedFontEntry
  { cfeFont    :: !SdlFont
  , cfeFm      :: !FontMetrics
  , cfeMeasure :: !(Text -> IO (Float, Float))
  }

data SdlFontCache = SdlFontCache
  { sfcGlyphAtlas     :: !GlyphAtlas
  , sfcBasePt         :: !Float
  , sfcScaleRef       :: !(IORef Float)
  -- ^ The window pixel density, owned by the window and read here.
  , sfcMonoSource     :: !FontSource
  , sfcSansSourceRef  :: !(IORef FontSource)
  , sfcBaseEntries    :: !(IORef (CachedFontEntry, CachedFontEntry))
  , sfcDynamicCache   :: !(IORef (IM.IntMap CachedFontEntry, [Int]))
  -- ^ Fonts at other sizes, and their keys, most recently used first.
  }

-- | Open the base sans and mono fonts at the pixel density, and warm them
-- into the glyph atlas.
newSdlFontCache :: FontSource -> FontSource -> GlyphAtlas -> Float -> IORef Float -> IO SdlFontCache
newSdlFontCache sans mono ga basePt scaleRef = do
  scale <- readIORef scaleRef
  base <- (,) <$> openCachedFont ga scale sans basePt <*> openCachedFont ga scale mono basePt
  cache <-
    SdlFontCache ga basePt scaleRef mono
      <$> newIORef sans
      <*> newIORef base
      <*> newIORef (IM.empty, [])
  warmBaseFonts cache
  pure cache

-- | A font from a source (or bundled Inter) at a point size, rasterised at
-- the pixel density, with its glyph metrics.
openCachedFont :: GlyphAtlas -> Float -> FontSource -> Float -> IO CachedFontEntry
openCachedFont ga scale source pt = do
  font <- openFontSource source (pt * scale)
  uncurry (CachedFontEntry font) <$> buildGlyphFontMetrics ga font scale

-- | Shape printable ASCII in the base fonts, which places its glyphs, so a
-- frame after an atlas reset pays no cold glyph misses. The base entries are
-- read when this runs, so it warms the live fonts, never ones already closed.
warmBaseFonts :: SdlFontCache -> IO ()
warmBaseFonts cache = do
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  forM_ [sans, mono] $ \e -> drawShaped (cfeFm e) (T.pack [' ' .. '~'])

-- | Empty the glyph atlas (DPI change, font switch, exhaustion recovery) and
-- warm the base fonts again.
resetGlyphAtlas :: SdlFontCache -> IO ()
resetGlyphAtlas cache = do
  let ga = sfcGlyphAtlas cache
  modifyIORef' (gaEpoch ga) (+ 1)
  writeIORef (gaIndexEntries ga) IM.empty
  writeIORef (gaNeedsReset ga) False
  textAtlasReset (gaAtlas ga)
  warmBaseFonts cache
  writeIORef (gaResetFlag ga) True

-- | Frame-start atlas maintenance: reset the atlas if an insertion failed
-- during the previous frame, then clear the mid-frame reset flag. Must run
-- before the frame's UI pass records any quads.
prepareGlyphAtlasForFrame :: SdlFontCache -> IO ()
prepareGlyphAtlasForFrame cache = do
  needs <- readIORef (gaNeedsReset (sfcGlyphAtlas cache))
  when needs $ resetGlyphAtlas cache
  writeIORef (gaResetFlag (sfcGlyphAtlas cache)) False

-- | The primary (sans) family's source, for the debug readout.
sdlFontCacheSource :: SdlFontCache -> IO FontSource
sdlFontCacheSource cache = readIORef (sfcSansSourceRef cache)

-- | Close fonts and drop their glyphs from the shared atlas index.
closeCachedFonts :: GlyphAtlas -> [SdlFont] -> IO ()
closeCachedFonts ga fonts = do
  fallbacks <- concat <$> mapM (fmap IM.elems . readIORef . sfFallbacks) fonts
  let handles = IS.fromList (map sfId (fonts ++ fallbacks))
  mapM_ closeFont fonts
  modifyIORef' (gaIndexEntries ga) (`IM.withoutKeys` handles)

-- | Close every open font, base and dynamic.
destroySdlFontCache :: SdlFontCache -> IO ()
destroySdlFontCache cache = do
  (dynamic, _) <- atomicModifyIORef' (sfcDynamicCache cache) (\c -> ((IM.empty, []), c))
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  closeCachedFonts (sfcGlyphAtlas cache) (map cfeFont (sans : mono : IM.elems dynamic))

-- | Reopen the base fonts from @source@ at the current pixel density, close
-- every dynamic size, and reset the glyph atlas, which re-warms the new base
-- fonts.
reloadSdlFontCache :: SdlFontCache -> FontSource -> IO ()
reloadSdlFontCache cache source = do
  destroySdlFontCache cache
  writeIORef (sfcSansSourceRef cache) source
  scale <- readIORef (sfcScaleRef cache)
  let open src = openCachedFont (sfcGlyphAtlas cache) scale src (sfcBasePt cache)
  writeIORef (sfcBaseEntries cache) =<< (,) <$> open source <*> open (sfcMonoSource cache)
  resetGlyphAtlas cache

-- | Install the cache's base fonts as the context's measurement and glyph
-- metrics, and its sizes and variants as the font resolver. Text measurement
-- uses the sans font's shaped lines.
withSdlFontCache :: SdlFontCache -> Context -> IO Context
withSdlFontCache cache ctx = do
  scale <- readIORef (sfcScaleRef cache)
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  let metrics = withMonoFontMetrics (withFontMetrics ctx (cfeFm sans)) (cfeFm mono)
  pure $
    withFontResolver
      (wrapMeasureCache scale metrics (cfeMeasure sans))
      (\sz _ _ var -> (\e -> (cfeFm e, False)) <$> getOrLoadCachedFont cache sz var)
      (\sz _ _ var txt -> getOrLoadCachedFont cache sz var >>= (`cfeMeasure` txt))

-- | The open font for a size and variant. Weight and style pick nothing
-- here: they are drawn synthetically over the regular face, because SDL_ttf's
-- style flags change its layout boxes but not the glyph images shaped text
-- draws, so a styled face would not line up. Every variant but 'FontMono'
-- is the sans face.
getOrLoadCachedFont :: SdlFontCache -> Float -> FontVariant -> IO CachedFontEntry
getOrLoadCachedFont cache sz var
  | ptKey == round (sfcBasePt cache * 2) =
      (if mono then snd else fst) <$> readIORef (sfcBaseEntries cache)
  | otherwise = do
      (entries, order) <- readIORef (sfcDynamicCache cache)
      case IM.lookup key entries of
        Just entry -> do
          -- Least recently used goes first: move a hit to the front, so
          -- fonts drawn every frame are never closed. A hot key sits near the
          -- front, so 'delete' copies little of the order.
          case order of
            newest : _ | newest == key -> pure ()
            _ -> writeIORef (sfcDynamicCache cache) (entries, key : delete key order)
          pure entry
        Nothing -> do
          scale <- readIORef (sfcScaleRef cache)
          source <- if mono then pure (sfcMonoSource cache) else readIORef (sfcSansSourceRef cache)
          -- Dynamic fonts are not warmed: they insert only glyphs drawn.
          entry <- openCachedFont (sfcGlyphAtlas cache) scale source (fromIntegral ptKey / 2)
          -- At most 48 dynamic sizes stay open.
          let (kept, evicted) = splitAt 48 (key : order)
          writeIORef (sfcDynamicCache cache) (foldr IM.delete (IM.insert key entry entries) evicted, kept)
          closeCachedFonts (sfcGlyphAtlas cache) (map cfeFont (mapMaybe (`IM.lookup` entries) evicted))
          pure entry
  where
    mono = var == FontMono
    -- Sizes are quantized to 0.5 pt so dragging a slider does not open
    -- hundreds of redundant TTF_Font instances.
    ptKey = round ((if sz > 0 then sz else sfcBasePt cache) * 2) :: Int
    key = ptKey * 2 + fromEnum mono
