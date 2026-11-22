{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}

-- | Shared SDL3_ttf font handles, shaping, measurement snapshots, and glyph
-- atlas caching. Parameterized over backend-specific atlas operations
-- ('AtlasOps') and optional platform hooks ('FontHooks') so both desktop SDL
-- and browser WebGL backends share one implementation.
module NanoUI.Ttf.Internal.Font
  ( AtlasOps (..)
  , FontHooks (..)
  , defaultFontHooks
  , SdlFont (..)
  , FontSource (..)
  , fontSourceLabel
  , embeddedFontSource
  , GlyphSlot (..)
  , GlyphAtlas (..)
  , glyphAtlasSize
  , newGlyphAtlas
  , destroyGlyphAtlas
  , glyphAtlasFull
  , glyphAtlasHandle
  , SdlFontCache (..)
  , newSdlFontCache
  , destroySdlFontCache
  , reloadSdlFontCache
  , prepareGlyphAtlasForFrame
  , beginTtfFrame
  , ttfFrameDropped
  , closeEvictedFonts
  , sdlFontCacheSource
  , sdlFontCacheMonoSource
  , withSdlFontCache
  , withTtf
  , openFontPath
  , readSdlFont
  , closeFont
  , ttfOpenFont
  , ttfCloseFont
  , ttfHasGlyph
  , ttfAddFallback
  , ttfRemoveFallback
  , ttfCopyFont
  , ttfIsBoldOf
  ) where

import Control.Exception (IOException, bracket_, catch, finally, onException, throwIO)
import Control.Monad (foldM_, forM_, unless, when, zipWithM_)
import Control.Monad.Primitive (RealWorld)
import Data.Bits ((.&.))
import Data.ByteString (ByteString)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Data.Char (isSpace, ord)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.Int (Int32)
import Data.IntMap.Strict qualified as IM
import Data.IntSet qualified as IS
import Data.List (delete, elemIndex)
import Data.Maybe (isJust, mapMaybe)
import Data.Primitive.PrimArray
  ( MutablePrimArray
  , PrimArray
  , indexPrimArray
  , newPrimArray
  , primArrayFromList
  , readPrimArray
  , setPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Primitive.PrimVar (PrimVar, newPrimVar, readPrimVar, writePrimVar)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Foreign qualified as TF
import Data.Text.Internal.Encoding.Utf8 (utf8Length)
import Data.Text.Unsafe (lengthWord8)
import Data.Unique (hashUnique, newUnique)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CFloat (..), CInt (..), CSize (..), CUInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray)
import Foreign.Marshal.Utils (fillBytes, with)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (peek, peekElemOff)
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (TextEncoding, utf8)
import NanoUI (FontStyle (..), FontVariant (..), FontWeight (..), unlessM, whenM)
import NanoUI.Backend
import NanoUI.Bidi (BidiRun (..), bidiRuns, needsBidi)
import NanoUI.Internal.Context.Types (GenCache, cachedGen, emptyGenCache)
import NanoUI.Testing
import NanoUI.Ttf.Internal.Header
  ( clusterFlags
  , clusterLength
  , clusterOffset
  , clusterWidth
  , clusterX
  , directionAuto
  , directionLtr
  , directionMask
  , directionRtl
  , glyphAtlasPixels
  , glyphDstH
  , glyphDstW
  , glyphDstX
  , glyphDstY
  , glyphIndex
  , glyphSrcH
  , glyphSrcW
  , glyphSrcX
  , glyphSrcY
  , shapedClusterCount
  , shapedClusterInts
  , shapedClusters
  , shapedGlyphCount
  , shapedGlyphFonts
  , shapedGlyphInts
  , shapedGlyphs
  , shapedHeight
  , shapedWidth
  , whitePatchSize
  )
import NanoUI.Ttf.Internal.Inter (fontInterBytes, fontInterLabel)

-- | Native glyph-atlas operations supplied by the concrete backend (SDL
-- renderer textures on desktop, CPU pixel pages for WebGL upload on web).
data AtlasOps = AtlasOps
  { aoHandle :: !(Ptr ())
  , aoDestroy :: !(IO ())
  , aoReset :: !(IO ())
  , aoInsertSurface :: !(Ptr () -> Ptr CFloat -> IO CBool)
  }

-- | Optional platform hooks for desktop font discovery and path encoding.
data FontHooks = FontHooks
  { fhEnsureCoverage :: !(SdlFont -> Text -> IO ())
  -- ^ Attach fallback fonts covering missing glyphs before shaping.
  , fhBoldSource :: !(FontSource -> IO (Ptr ()) -> IO (Maybe FontSource))
  -- ^ Discover a native bold face beside a regular font file, if available.
  , fhPathEncoding :: !(IO TextEncoding)
  -- ^ Encoding used for font file paths passed to SDL_ttf.
  , fhOnTtfQuit :: !(IO ())
  -- ^ Cleanup action run before 'TTF_Quit' (e.g. closing coverage probes).
  }

-- | Hooks with no system font search, no bold-face lookup, and UTF-8 paths.
defaultFontHooks :: FontHooks
defaultFontHooks =
  FontHooks
    { fhEnsureCoverage = \_ _ -> pure ()
    , fhBoldSource = \_ _ -> pure Nothing
    , fhPathEncoding = pure utf8
    , fhOnTtfQuit = pure ()
    }

-- | An open SDL3_ttf font handle with its cached metrics and attached fallbacks.
data SdlFont = SdlFont
  { sfId :: !Int
  , sfFont :: !(Ptr ())
  , sfLineSkip :: !Float
  , sfAscent :: !Float
  , sfSpaceAdvance :: !Float
  , sfAlive :: !(IORef Bool)
  , sfPointSize :: !Float
  -- ^ The size the font was opened at, which its fallbacks open at too.
  , sfFallbacks :: !(IORef (IM.IntMap SdlFont))
  -- ^ Fallback fonts attached to this face, indexed by source slot.
  }

-- | Where a font face is loaded from.
data FontSource
  = FontFromPath !FilePath
  | FontFromMemory !ByteString !FilePath
  deriving (Eq, Show)

-- | Human-readable label for a 'FontSource'.
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

-- | Shared glyph atlas bookkeeping over backend 'AtlasOps'.
data GlyphAtlas = GlyphAtlas
  { gaOps :: !AtlasOps
  , gaEpoch :: !(IORef Int)
  , -- | An insertion failed (every page out of space) since the last reset.
    -- The atlas is reset at the next frame start, before any quad is recorded.
    gaFull :: !(IORef Bool)
  , -- | Glyph slots by font id, then glyph index: what shaped text draws.
    gaIndexEntries :: !(IORef (IM.IntMap (IM.IntMap (Maybe GlyphSlot))))
  , -- | Frames begun ('prepareGlyphAtlasForFrame'), by which the text caches
    -- count what a frame looks up ('RunCache').
    gaFrame :: !(IORef Int)
  }

-- | Wrap backend 'AtlasOps' in a 'GlyphAtlas'.
newGlyphAtlas :: AtlasOps -> IO GlyphAtlas
newGlyphAtlas ops =
  GlyphAtlas ops <$> newIORef 0 <*> newIORef False <*> newIORef IM.empty <*> newIORef 0

-- | Destroy the underlying native glyph atlas.
destroyGlyphAtlas :: GlyphAtlas -> IO ()
destroyGlyphAtlas = aoDestroy . gaOps

-- | Whether a glyph could not be placed since the atlas was last reset.
glyphAtlasFull :: SdlFontCache -> IO Bool
glyphAtlasFull = readIORef . gaFull . sfcGlyphAtlas

-- | The glyph atlas's native handle.
glyphAtlasHandle :: SdlFontCache -> Ptr ()
glyphAtlasHandle = aoHandle . gaOps . sfcGlyphAtlas

ensureAlive :: SdlFont -> IO ()
ensureAlive sf = unlessM (readIORef (sfAlive sf)) (fail "font backend used after closeFont")

runCacheCap :: Int
runCacheCap = 1024

data RunCache v = RunCache
  { rcGens :: !(IORef (GenCache Text v))
  , rcFrame :: !(PrimVar RealWorld Int)
  , rcLooked :: !(PrimVar RealWorld Int)
  , rcCap :: !(PrimVar RealWorld Int)
  }

newRunCache :: IO (RunCache v)
newRunCache = RunCache <$> newIORef emptyGenCache <*> newPrimVar 0 <*> newPrimVar 0 <*> newPrimVar runCacheCap

{-# INLINE cachedRun #-}
cachedRun :: GlyphAtlas -> RunCache v -> Text -> IO v -> IO v
cachedRun ga RunCache {..} txt make = do
  frame <- readIORef (gaFrame ga)
  counted <- readPrimVar rcFrame
  looked <- readPrimVar rcLooked
  let !w = textWeight txt
  if frame == counted
    then writePrimVar rcLooked (looked + w)
    else do
      writePrimVar rcFrame frame
      writePrimVar rcLooked w
      writePrimVar rcCap (max runCacheCap (2 * looked))
  cap <- readPrimVar rcCap
  cachedGen cap textWeight rcGens txt make

textWeight :: Text -> Int
textWeight txt = 1 + lengthWord8 txt `quot` 4096

-- | A glyph's native measurements in unscaled pixels: min x, max x, min y, max y, advance.
data GlyphMetrics = GlyphMetrics !Float !Float !Float !Float !Float

getGlyphMetrics :: SdlFont -> Int -> IO (Maybe GlyphMetrics)
getGlyphMetrics sf cp = allocaArray 5 $ \p -> do
  let at = advancePtr p
      metric i = fromIntegral <$> peekElemOff p i
  ok <- ttfGlyphMetrics (sfFont sf) (fromIntegral cp) p (at 1) (at 2) (at 3) (at 4)
  if ok
    then Just <$> (GlyphMetrics <$> metric 0 <*> metric 1 <*> metric 2 <*> metric 3 <*> metric 4)
    else pure Nothing

metricsGlyphQuad :: SdlFont -> Float -> GlyphMetrics -> GlyphQuad
metricsGlyphQuad sf inv (GlyphMetrics minX maxX minY maxY _) =
  GlyphQuad
    (minX / inv)
    ((sfAscent sf - maxY) / inv)
    ((maxX - minX) / inv)
    ((maxY - minY) / inv)
    0
    0
    0
    0

lookupOrInsertGlyph :: GlyphAtlas -> SdlFont -> Int -> IO (Maybe GlyphSlot)
lookupOrInsertGlyph ga font gi = do
  entries <- readIORef (gaIndexEntries ga)
  case IM.lookup (sfId font) entries >>= IM.lookup gi of
    Just mSlot -> pure mSlot
    Nothing -> do
      mSlot <- placeGlyphImage ga font gi
      modifyIORef' (gaIndexEntries ga) (IM.insertWith IM.union (sfId font) (IM.singleton gi mSlot))
      pure mSlot

placeGlyphImage :: GlyphAtlas -> SdlFont -> Int -> IO (Maybe GlyphSlot)
placeGlyphImage ga font gi = do
  surf <- with nullPtr $ \sp -> ttfRenderGlyphIndexSurface (sfFont font) (fromIntegral gi) sp >> peek sp
  if surf == nullPtr
    then pure Nothing
    else allocaArray 4 $ \uv -> do
      placed <- aoInsertSurface (gaOps ga) surf uv
      sdlDestroySurface surf
      if placed == 0
        then Nothing <$ writeIORef (gaFull ga) True
        else do
          [u0, v0, u1, v1] <- map realToFrac <$> peekArray 4 uv
          pure (Just (GlyphSlot u0 v0 u1 v1))

-- | Width and height of the glyph atlas texture: NANO_UI_GLYPH_ATLAS_SIZE.
glyphAtlasSize :: Float
glyphAtlasSize = fromIntegral glyphAtlasPixels

-- | The UVs of a texel no glyph or shape covers, for a glyph with no slot:
-- the padding column just right of page 0's white patch, on its bottom row.
deadU, deadV :: Float
deadU = (fromIntegral whitePatchSize + 0.5) / glyphAtlasSize
deadV = (glyphAtlasSize - 0.5) / glyphAtlasSize

data Shaped = Shaped
  { shapedText :: !ShapedText
  , shapedSize :: !(Float, Float)
  , _shapedGlyphs :: !(PrimArray Int32)
  -- ^ 'glyphRecordInts' ints a glyph: the C side's glyph from 'glyphIndex'
  -- on, its x moved right by the pen of the piece it was shaped in.
  , _shapedFontIndices :: !(PrimArray Int32)
  , _shapedFonts :: !(SmallArray SdlFont)
  }

-- | Ints a glyph takes in '_shapedGlyphs': the C side's glyph without its
-- text offset, which nothing after shaping reads.
glyphRecordInts :: Int
glyphRecordInts = shapedGlyphInts - glyphIndex

-- | Where an int of the C side's glyph ('glyphDstX' and the rest) is in a
-- glyph record of '_shapedGlyphs'.
recordField :: Int -> Int
recordField k = k - glyphIndex

-- | The runs of a line to shape apart, as start and end characters and the
-- direction to shape in. Text that needs no bidi runs ('needsBidi') is one
-- run, whose direction SDL_ttf picks. A right-to-left run is shaped right
-- to left without its leading and trailing spaces, which shape left to
-- right.
shapingRuns :: Text -> [(Int, Int, CInt)]
shapingRuns txt
  | needsBidi txt = concatMap directed (bidiRuns txt)
  | otherwise = [(0, T.length txt, auto)]
  where
    auto = fromIntegral directionAuto
    ltr = fromIntegral directionLtr
    rtl = fromIntegral directionRtl
    directed r
      | not (runRightToLeft r) = [(runStart r, runEnd r, ltr)]
      | otherwise =
          let run = T.take (runEnd r - runStart r) (T.drop (runStart r) txt)
              lead = T.length (T.takeWhile isSpace run)
              trail = T.length (T.takeWhileEnd isSpace run)
              coreStart = runStart r + lead
              coreEnd = runEnd r - trail
           in if coreStart >= coreEnd
                then [(runStart r, runEnd r, ltr)]
                else
                  [(coreEnd, runEnd r, ltr) | trail > 0]
                    ++ [(coreStart, coreEnd, rtl)]
                    ++ [(runStart r, coreStart, ltr) | lead > 0]

-- | Shape a line run by run ('shapingRuns'), each run after the last, into
-- its glyphs ('copyShapedGlyphs'), the x of every caret position
-- ('placeClusters', 'fillCarets'), its size and the right edge of its ink.
-- Sizes are in layout units: device pixels divided by @inv@.
shapeLine :: SdlFont -> Float -> Text -> IO Shaped
shapeLine sf inv txt = do
  ensureAlive sf
  fallbacks <- IM.elems <$> readIORef (sfFallbacks sf)
  let fontList = sf : fallbacks
      fontPtrs = map sfFont fontList
      fontIndex ptr = maybe 0 fromIntegral (elemIndex ptr fontPtrs)
      n = T.length txt
      totalBytes = lengthWord8 txt
      runs = shapingRuns txt
      pieceCount = length runs
      ascii = totalBytes == n
  charOfByte <- newPrimArray (if ascii then 0 else totalBytes + 1)
  let charAt b = if ascii then pure (min n b) else readPrimArray charOfByte (min totalBytes b)
      indexChars !i !b t = case T.uncons t of
        Nothing -> pure ()
        Just (c, rest) -> do
          writePrimArray charOfByte b i
          indexChars (i + 1) (b + utf8Length c) rest
  unless ascii $ do
    setPrimArray charOfByte 0 (totalBytes + 1) n
    indexChars 0 0 txt
  size <- fromIntegral <$> ttfShapedSize
  let totalBuf = size * max 1 pieceCount
  allocaBytes totalBuf $ \outs -> do
    fillBytes outs 0 totalBuf
    let resultOf p = outs `plusPtr` (p * size)
        freeAll = forM_ [0 .. pieceCount - 1] (ttfShapedFree . resultOf)
    flip finally freeAll $ do
      forM_ (zip [0 ..] runs) $ \(p, (start, end, dir)) ->
        TF.useAsPtr (T.take (end - start) (T.drop start txt)) $ \ptr len -> do
          ok <- ttfShape (sfFont sf) (castPtr ptr) (fromIntegral len) dir (resultOf p)
          when (ok == 0) $ ttfShapedFree (resultOf p)
      glyphCount <- sum <$> mapM (\p -> shapedInt (resultOf p) shapedGlyphCount) [0 .. pieceCount - 1]
      glyphs <- newPrimArray (glyphCount * glyphRecordInts)
      fontIndices <- newPrimArray glyphCount
      stops <- newPrimArray n
      setPrimArray stops 0 n (0 / 0 :: Float)
      let fillPieces !p !pen !height !g0 !inkEnd !endStop pieces = case pieces of
            [] -> pure (pen, height, inkEnd, endStop)
            (start, _, _) : rest -> do
              let result = resultOf p
                  byteStart = if ascii then start else lengthWord8 (T.take start txt)
              w <- shapedInt result shapedWidth
              h <- shapedInt result shapedHeight
              nGlyphs <- shapedInt result shapedGlyphCount
              nClusters <- shapedInt result shapedClusterCount
              glyphPtr <- castPtr <$> ttfShapedPtr result (fromIntegral shapedGlyphs)
              fontPtr <- castPtr <$> ttfShapedPtr result (fromIntegral shapedGlyphFonts)
              clusterPtr <- castPtr <$> ttfShapedPtr result (fromIntegral shapedClusters)
              ink <-
                if glyphPtr == nullPtr
                  then pure inkEnd
                  else copyShapedGlyphs glyphs fontIndices fontIndex glyphPtr fontPtr nGlyphs g0 pen inkEnd
              end <-
                if clusterPtr == nullPtr
                  then pure endStop
                  else placeClusters stops charAt n clusterPtr nClusters byteStart pen endStop
              ttfShapedFree result
              fillPieces (p + 1) (pen + w) (max height h) (g0 + nGlyphs) ink end rest
      (total, height, inkEnd, endStop) <- fillPieces 0 0 (0 :: Int) 0 0 (0 / 0) runs
      caretArr <- fillCarets stops n inv (if isNaN endStop then fromIntegral total else endStop)
      glyphArr <- unsafeFreezePrimArray glyphs
      indexArr <- unsafeFreezePrimArray fontIndices
      let !width = fromIntegral total / inv
          !measured = (width, fromIntegral height / inv)
      pure (Shaped (ShapedText width (fromIntegral inkEnd / inv) caretArr) measured glyphArr indexArr (smallArrayFromList fontList))

-- | A field of a shaped line ('shapedWidth' and the rest).
{-# INLINE shapedInt #-}
shapedInt :: Ptr () -> Int -> IO Int
shapedInt result field = fromIntegral <$> ttfShapedInt result (fromIntegral field)

-- | Copy a shaped piece's @nGlyphs@ glyphs into the line's glyph records
-- from record @g0@ on, moving each right by the piece's @pen@, and note
-- each glyph's font as its index among the line's fonts. Answers the right
-- edge of the line's ink so far, from @ink0@.
{-# INLINE copyShapedGlyphs #-}
copyShapedGlyphs ::
  MutablePrimArray RealWorld Int32 ->
  MutablePrimArray RealWorld Int32 ->
  (Ptr () -> Int32) ->
  Ptr CInt ->
  Ptr (Ptr ()) ->
  Int ->
  Int ->
  Int ->
  Int ->
  IO Int
copyShapedGlyphs glyphs fontIndices fontIndex glyphPtr fontPtr nGlyphs g0 pen = go 0
  where
    go !i !ink
      | i >= nGlyphs = pure ink
      | otherwise = do
          let src = i * shapedGlyphInts
              dst = (g0 + i) * glyphRecordInts
          forM_ [glyphIndex .. shapedGlyphInts - 1] $ \k -> do
            v <- fromIntegral <$> peekElemOff glyphPtr (src + k)
            writePrimArray glyphs (dst + recordField k) (if k == glyphDstX then v + fromIntegral pen else v)
          x1 <- readPrimArray glyphs (dst + recordField glyphDstX)
          gw <- readPrimArray glyphs (dst + recordField glyphDstW)
          ptr <- peekElemOff fontPtr i
          writePrimArray fontIndices (g0 + i) (fontIndex ptr)
          go (i + 1) (max ink (fromIntegral (x1 + gw) :: Int))

-- | Write the caret x of each character a shaped piece's @nClusters@
-- clusters cover into @stops@, by character index: a cluster's width split
-- evenly between its characters, right to left in a right-to-left cluster.
-- @byteStart@ is the piece's first byte in the line, @pen@ its x, and
-- @charAt@ the character a byte of the line starts. Answers the x where the
-- line's last character ends once a cluster reaches it, else @end0@.
{-# INLINE placeClusters #-}
placeClusters ::
  MutablePrimArray RealWorld Float ->
  (Int -> IO Int) ->
  Int ->
  Ptr CInt ->
  Int ->
  Int ->
  Int ->
  Float ->
  IO Float
placeClusters stops charAt n clusterPtr nClusters byteStart pen = go 0
  where
    clusterInt :: Int -> Int -> IO Int
    clusterInt i k = fromIntegral <$> peekElemOff clusterPtr (i * shapedClusterInts + k)
    go !i !end
      | i >= nClusters = pure end
      | otherwise = do
          len <- clusterInt i clusterLength
          if len <= 0
            then go (i + 1) end
            else do
              off0 <- clusterInt i clusterOffset
              x0 <- clusterInt i clusterX
              cw <- clusterInt i clusterWidth
              flags <- clusterInt i clusterFlags
              let off = off0 + byteStart
                  x = fromIntegral (x0 + pen) :: Float
                  fw = fromIntegral cw
                  rtl = flags .&. directionMask == directionRtl
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
              go (i + 1) end'

-- | The caret x before each of the line's @n@ characters and after the last
-- (@endStop@), in layout units. A character no cluster placed ('NaN' in
-- @stops@) takes the caret before it.
fillCarets :: MutablePrimArray RealWorld Float -> Int -> Float -> Float -> IO (PrimArray Float)
fillCarets stops n inv endStop = do
  carets <- newPrimArray (n + 1)
  let fillCaret prev i = do
        v <- readPrimArray stops i
        let v' = if isNaN v then prev else v / inv
        v' <$ writePrimArray carets i v'
  foldM_ fillCaret 0 [0 .. n - 1]
  writePrimArray carets n (endStop / inv)
  unsafeFreezePrimArray carets

placeShapedGlyphs :: GlyphAtlas -> Float -> Shaped -> IO ShapedGlyphs
placeShapedGlyphs ga inv (Shaped _ _ glyphs fontIndices fonts) = do
  let !count = sizeofPrimArray fontIndices
  out <- newPrimArray (count * 8)
  forM_ [0 .. count - 1] $ \i -> do
    let field k = indexPrimArray glyphs (i * glyphRecordInts + recordField k)
        g k = fromIntegral (field k) :: Float
        write k = writePrimArray out (i * 8 + k)
        font = indexSmallArray fonts (fromIntegral (indexPrimArray fontIndices i))
    mSlot <- lookupOrInsertGlyph ga font (fromIntegral (field glyphIndex))
    -- The quad: x, y, width and height, then u0, v0, u1 and v1.
    write 0 (g glyphDstX / inv)
    write 1 (g glyphDstY / inv)
    write 2 (g glyphDstW / inv)
    write 3 (g glyphDstH / inv)
    case mSlot of
      Just slot -> do
        let u0 = gsU0 slot + g glyphSrcX / glyphAtlasSize
            v0 = gsV0 slot + g glyphSrcY / glyphAtlasSize
        write 4 u0
        write 5 v0
        write 6 (if g glyphSrcW > 0 then u0 + g glyphSrcW / glyphAtlasSize else gsU1 slot)
        write 7 (if g glyphSrcH > 0 then v0 + g glyphSrcH / glyphAtlasSize else gsV1 slot)
      Nothing -> zipWithM_ write [4 ..] [deadU, deadV, deadU, deadV]
  ShapedGlyphs <$> unsafeFreezePrimArray out

{-# NOINLINE buildGlyphFontMetrics #-}
buildGlyphFontMetrics :: FontHooks -> GlyphAtlas -> SdlFont -> Float -> IO (FontMetrics, Text -> IO (Float, Float))
buildGlyphFontMetrics hooks ga sf scale = do
  let !inv = if scale > 0 then scale else 1
      !spaceAdvance = sfSpaceAdvance sf / inv
      advanceOf = maybe spaceAdvance (\(GlyphMetrics _ _ _ _ adv) -> adv / inv)

  asciiMetrics <- mapM (getGlyphMetrics sf) [0 .. 127]
  let !asciiGeometry = smallArrayFromList (map (fmap (metricsGlyphQuad sf inv)) asciiMetrics)
      !asciiAdvances = primArrayFromList (map advanceOf asciiMetrics)
  rawKernXM <- ttfGetKerning (sfFont sf) (fromIntegral (ord 'x')) (fromIntegral (ord 'M'))
  let !kernXM = fromIntegral rawKernXM / inv

  preparedRun <- newRunCache
  shapedRef <- newIORef emptyGenCache
  quadRun <- newRunCache
  quadEpochRef <- newIORef =<< readIORef (gaEpoch ga)

  let
    {-# NOINLINE shapedLookup #-}
    shapedLookup !txt
      | T.null txt = pure Nothing
      | otherwise = do
          ensureAlive sf
          ep <- readIORef (gaEpoch ga)
          quadEp <- readIORef quadEpochRef
          when (quadEp /= ep) $ do
            writeIORef quadEpochRef ep
            writeIORef (rcGens quadRun) emptyGenCache
          cachedRun ga quadRun txt $
            Just <$> (placeShapedGlyphs ga inv =<< shapeOf txt)

    shapeOf !txt =
      cachedGen runCacheCap textWeight shapedRef txt $ do
        fhEnsureCoverage hooks sf txt
        shapeLine sf inv txt

    measure !txt
      | T.null txt = pure emptySize
      | otherwise = shapedSize <$> shapeOf txt
    !emptySize = (0, sfLineSkip sf / inv)

    backend = FontBackend prepareText shapedLookup

    prepareText txt = do
      ensureAlive sf
      cachedRun ga preparedRun txt $ do
        advances <-
          if T.isAscii txt
            then pure IM.empty
            else do
              let insertNonAscii m c
                    | ord c < 128 = m
                    | otherwise = IM.insert (ord c) c m
                  others = T.foldl' insertNonAscii IM.empty txt
              IM.map advanceOf <$> traverse (getGlyphMetrics sf . ord) others
        shaped <- if T.null txt then pure Nothing else Just <$> shapeOf txt
        let !layout = fmap shapedText shaped
        pure $!
          (monospaceMetrics (sfLineSkip sf / inv))
            { fmAscent = sfAscent sf / inv
            , fmAdvance = \c ->
                let cp = ord c
                 in if cp < 128
                      then indexPrimArray asciiAdvances cp
                      else IM.findWithDefault spaceAdvance cp advances
            , fmKerning = \a b -> if a == 'x' && b == 'M' then kernXM else 0
            , fmGlyph = \c ->
                let cp = ord c
                 in if cp < 128 then indexSmallArray asciiGeometry cp else Nothing
            , fmShape = \t -> if t == txt then layout else Nothing
            , fmBackend = Just backend
            , fmSnapScale = inv
            }

  fm <- prepareText ""
  pure (fm, measure)

-- | Bracket an action with 'TTF_Init' and 'TTF_Quit', running 'fhOnTtfQuit'
-- before shutting down SDL_ttf.
withTtf :: FontHooks -> IO a -> IO a
withTtf hooks =
  bracket_
    (unlessM ttfInit (ttfFail "TTF_Init failed"))
    (fhOnTtfQuit hooks >> ttfQuit)

-- | Open a font at a point size, or bundled Inter when it will not open.
openFontSource :: FontHooks -> Bool -> FontSource -> Float -> IO (FontSource, SdlFont)
openFontSource hooks grid source ptsize =
  open source `catch` \(e :: IOException) ->
    if source == embeddedFontSource
      then throwIO e
      else open embeddedFontSource `catch` \(_ :: IOException) -> throwIO e
  where
    pt = realToFrac ptsize
    open src = do
      font <- case src of
        FontFromPath path -> openFontPath hooks path pt
        FontFromMemory bs _ ->
          unsafeUseAsCStringLen bs $ \(ptr, len) -> ttfOpenFontMemory (castPtr ptr) (fromIntegral len) pt
      when (font == nullPtr) $ ttfFail ("TTF_OpenFont failed for " ++ fontSourceLabel src)
      when grid (ttfHintForGrid font)
      (src,) <$> readSdlFont ptsize font

-- | Throw an 'IOError' for a failed SDL_ttf call, with SDL's last error
-- message appended when there is one.
ttfFail :: String -> IO a
ttfFail what = do
  p <- sdlGetError
  err <- if p == nullPtr then pure "" else GHC.peekCString utf8 p
  ioError (userError (if null err then what else what ++ ": " ++ err))

-- | Open a font file at a point size using the configured path encoding.
openFontPath :: FontHooks -> FilePath -> CFloat -> IO (Ptr ())
openFontPath hooks path pt = fhPathEncoding hooks >>= \enc -> GHC.withCString enc path (`ttfOpenFont` pt)

-- | Wrap an open TTF font handle.
readSdlFont :: Float -> Ptr () -> IO SdlFont
readSdlFont sfPointSize sfFont = do
  sfId <- hashUnique <$> newUnique
  sfAlive <- newIORef True
  sfFallbacks <- newIORef IM.empty
  sfLineSkip <- fromIntegral <$> ttfLineSkip sfFont
  sfAscent <- fromIntegral <$> ((+) <$> ttfAscent sfFont <*> ttfHalfLeading sfFont)
  sfSpaceAdvance <- realToFrac <$> ttfSpaceAdvance sfFont
  pure SdlFont {..}

-- | Close a font handle and all of its attached fallbacks.
closeFont :: SdlFont -> IO ()
closeFont sf = do
  whenM (atomicModifyIORef' (sfAlive sf) (False,)) $ do
    ttfCloseFont (sfFont sf)
    readIORef (sfFallbacks sf) >>= mapM_ closeFont

-- ---------------------------------------------------------------------------
-- Font cache

data CachedFontEntry = CachedFontEntry
  { cfeFont :: !SdlFont
  , cfeFm :: !FontMetrics
  , cfeMeasure :: !(Text -> IO (Float, Float))
  }

-- | Base sans and mono faces plus an LRU cache of dynamic sizes and bold faces.
data SdlFontCache = SdlFontCache
  { sfcHooks :: !FontHooks
  , sfcGlyphAtlas :: !GlyphAtlas
  , sfcBasePt :: !Float
  , sfcScaleRef :: !(IORef Float)
  , sfcMonoSource :: !FontSource
  , sfcSansSourceRef :: !(IORef FontSource)
  , sfcBaseEntries :: !(IORef (CachedFontEntry, CachedFontEntry))
  , sfcDynamicCache :: !(IORef (IM.IntMap CachedFontEntry, [Int]))
  , sfcEvicted :: !(IORef [SdlFont])
  }

-- | Open the base sans and mono fonts at the pixel density and warm them.
newSdlFontCache :: FontHooks -> FontSource -> FontSource -> GlyphAtlas -> Float -> IORef Float -> IO SdlFontCache
newSdlFontCache hooks sans mono ga basePt scaleRef = do
  scale <- readIORef scaleRef
  (sans', sansEntry) <- openCachedFont hooks ga scale False sans basePt
  (mono', monoEntry) <- openCachedFont hooks ga scale True mono basePt
  cache <-
    SdlFontCache hooks ga basePt scaleRef mono'
      <$> newIORef sans'
      <*> newIORef (sansEntry, monoEntry)
      <*> newIORef (IM.empty, [])
      <*> newIORef []
  warmBaseFonts cache
  pure cache

openCachedFont :: FontHooks -> GlyphAtlas -> Float -> Bool -> FontSource -> Float -> IO (FontSource, CachedFontEntry)
openCachedFont hooks ga scale grid source pt = do
  (opened, font) <- openFontSource hooks grid source (pt * scale)
  (opened,) . uncurry (CachedFontEntry font) <$> buildGlyphFontMetrics hooks ga font scale

warmBaseFonts :: SdlFontCache -> IO ()
warmBaseFonts cache = do
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  forM_ [sans, mono] $ \e -> drawShaped (cfeFm e) (T.pack [' ' .. '~'])

resetGlyphAtlas :: SdlFontCache -> IO ()
resetGlyphAtlas cache = do
  let ga = sfcGlyphAtlas cache
  modifyIORef' (gaEpoch ga) (+ 1)
  writeIORef (gaIndexEntries ga) IM.empty
  writeIORef (gaFull ga) False
  aoReset (gaOps ga)
  warmBaseFonts cache

-- | Frame-start atlas maintenance: count the frame, and reset the atlas if an
-- insertion failed since the last reset.
prepareGlyphAtlasForFrame :: SdlFontCache -> IO Bool
prepareGlyphAtlasForFrame cache = do
  modifyIORef' (gaFrame (sfcGlyphAtlas cache)) (+ 1)
  full <- glyphAtlasFull cache
  full <$ when full (resetGlyphAtlas cache)

-- | The font work a backend does before a frame's UI pass records any text,
-- answering whether the glyph atlas was reset ('prepareGlyphAtlasForFrame').
-- A reset emptied the glyph slots a kept draw's text points at, so the
-- frame repaints whole ('damageFull') rather than reuse one. Fonts the size
-- cache evicted close now, and the measure caches that can still hold their
-- metrics are dropped ('clearMeasureCache'), before the pass measures text.
beginTtfFrame :: SdlFontCache -> Context -> IO Bool
beginTtfFrame cache ctx = do
  fresh <- prepareGlyphAtlasForFrame cache
  when fresh (damageFull ctx)
  whenM (closeEvictedFonts cache) (clearMeasureCache ctx)
  pure fresh

-- | Whether to drop the frame just built instead of presenting it: its UI
-- pass ran out of glyph atlas room, so some of its text has no quads. A
-- dropped frame damages the whole window and marks the context dirty, so
-- the next frame, after 'beginTtfFrame' resets the atlas, repaints it all
-- and the screen keeps the last good frame meanwhile. @fresh@ is what
-- 'beginTtfFrame' answered for this frame: a frame that filled an atlas
-- reset at its own start (a glyph larger than a page, or more glyphs than
-- the pages hold) would fill it again after every reset, so it is presented
-- with those glyphs missing instead.
ttfFrameDropped :: SdlFontCache -> Context -> Bool -> IO Bool
ttfFrameDropped cache ctx fresh = do
  dropped <- (&& not fresh) <$> glyphAtlasFull cache
  when dropped $ do
    damageFull ctx
    markDirty ctx
  pure dropped

-- | The primary (sans) family's source, for the debug readout.
sdlFontCacheSource :: SdlFontCache -> IO FontSource
sdlFontCacheSource cache = readIORef (sfcSansSourceRef cache)

-- | The monospace family's source.
sdlFontCacheMonoSource :: SdlFontCache -> FontSource
sdlFontCacheMonoSource = sfcMonoSource

closeCachedFonts :: GlyphAtlas -> [SdlFont] -> IO ()
closeCachedFonts ga fonts = do
  fallbacks <- concat <$> mapM (fmap IM.elems . readIORef . sfFallbacks) fonts
  let handles = IS.fromList (map sfId (fonts ++ fallbacks))
  mapM_ closeFont fonts
  modifyIORef' (gaIndexEntries ga) (`IM.withoutKeys` handles)

-- | Close every open font, base, dynamic and evicted.
destroySdlFontCache :: SdlFontCache -> IO ()
destroySdlFontCache cache = do
  (dynamic, _) <- atomicModifyIORef' (sfcDynamicCache cache) ((IM.empty, []),)
  evicted <- atomicModifyIORef' (sfcEvicted cache) ([],)
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  closeCachedFonts (sfcGlyphAtlas cache) (map cfeFont (sans : mono : IM.elems dynamic) ++ evicted)

-- | Close fonts evicted since the last call, answering whether there were any.
closeEvictedFonts :: SdlFontCache -> IO Bool
closeEvictedFonts cache = do
  evicted <- atomicModifyIORef' (sfcEvicted cache) ([],)
  not (null evicted) <$ closeCachedFonts (sfcGlyphAtlas cache) evicted

-- | Reopen base fonts at the current pixel density, closing old fonts only
-- after the replacements open cleanly.
reloadSdlFontCache :: SdlFontCache -> FontSource -> IO ()
reloadSdlFontCache cache source = do
  scale <- readIORef (sfcScaleRef cache)
  let open grid src = openCachedFont (sfcHooks cache) (sfcGlyphAtlas cache) scale grid src (sfcBasePt cache)
  (opened, sans) <- open False source
  mono <- snd <$> open True (sfcMonoSource cache) `onException` closeFont (cfeFont sans)
  (dynamic, _) <- atomicModifyIORef' (sfcDynamicCache cache) ((IM.empty, []),)
  evicted <- atomicModifyIORef' (sfcEvicted cache) ([],)
  (oldSans, oldMono) <- readIORef (sfcBaseEntries cache)
  writeIORef (sfcSansSourceRef cache) opened
  writeIORef (sfcBaseEntries cache) (sans, mono)
  closeCachedFonts (sfcGlyphAtlas cache) (map cfeFont (oldSans : oldMono : IM.elems dynamic) ++ evicted)
  resetGlyphAtlas cache

-- | Install the cache's base fonts and resolver on a 'Context'.
withSdlFontCache :: SdlFontCache -> Context -> IO Context
withSdlFontCache cache ctx = do
  scale <- readIORef (sfcScaleRef cache)
  (sans, mono) <- readIORef (sfcBaseEntries cache)
  let metrics = withFontSize (withMonoFontMetrics (withFontMetrics ctx (cfeFm sans)) (cfeFm mono)) (sfcBasePt cache)
  pure $
    withFontResolver
      (wrapMeasureCache scale metrics (cfeMeasure sans))
      (\sz weight style var -> (\(e, native) -> (cfeFm e, native)) <$> resolveCachedFont cache sz weight style var)
      (\sz weight style var txt -> resolveCachedFont cache sz weight style var >>= (`cfeMeasure` txt) . fst)

resolveCachedFont :: SdlFontCache -> Float -> FontWeight -> FontStyle -> FontVariant -> IO (CachedFontEntry, Bool)
resolveCachedFont cache sz weight style var = do
  bold <-
    if style == FontStyleNormal && weight `elem` [WeightSemiBold, WeightBold, WeightExtraBold, WeightBlack]
      then do
        src <- variantSource cache var
        fhBoldSource (sfcHooks cache) src (sfFont . cfeFont <$> baseEntry cache var)
      else pure Nothing
  (,isJust bold) <$> getOrLoadCachedFont cache sz var bold

variantSource :: SdlFontCache -> FontVariant -> IO FontSource
variantSource cache var
  | var == FontMono = pure (sfcMonoSource cache)
  | otherwise = readIORef (sfcSansSourceRef cache)

baseEntry :: SdlFontCache -> FontVariant -> IO CachedFontEntry
baseEntry cache var = (if var == FontMono then snd else fst) <$> readIORef (sfcBaseEntries cache)

getOrLoadCachedFont :: SdlFontCache -> Float -> FontVariant -> Maybe FontSource -> IO CachedFontEntry
getOrLoadCachedFont cache sz var bold
  | null bold && ptKey == round (sfcBasePt cache * 2) = baseEntry cache var
  | otherwise = do
      (entries, order) <- readIORef (sfcDynamicCache cache)
      case IM.lookup key entries of
        Just entry -> do
          unless (key `elem` take 8 order) $ do
            let order' = key : delete key order
            length order' `seq` writeIORef (sfcDynamicCache cache) (entries, order')
          pure entry
        Nothing -> do
          scale <- readIORef (sfcScaleRef cache)
          source <- maybe (variantSource cache var) pure bold
          entry <- snd <$> openCachedFont (sfcHooks cache) (sfcGlyphAtlas cache) scale mono source (fromIntegral ptKey / 2)
          let opened = key : order
              (kept, evicted) = if length opened > 48 then splitAt 32 opened else (opened, [])
          writeIORef (sfcDynamicCache cache) (foldr IM.delete (IM.insert key entry entries) evicted, kept)
          modifyIORef' (sfcEvicted cache) (map cfeFont (mapMaybe (`IM.lookup` entries) evicted) ++)
          pure entry
  where
    mono = var == FontMono
    ptKey = round ((if sz > 0 then sz else sfcBasePt cache) * 2) :: Int
    key = ptKey * 4 + fromEnum (not (null bold)) * 2 + fromEnum mono

-- ---------------------------------------------------------------------------
-- SDL3_ttf FFI

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_Init"
  ttfInit :: IO Bool

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_Quit"
  ttfQuit :: IO ()

foreign import ccall unsafe "SDL_GetError"
  sdlGetError :: IO CString

foreign import capi unsafe "SDL3/SDL.h SDL_DestroySurface"
  sdlDestroySurface :: Ptr () -> IO ()

-- | Open a font file via the nano-ui C helper.
foreign import ccall unsafe "nano_ui_ttf_open_font"
  ttfOpenFont :: CString -> CFloat -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_open_font_memory"
  ttfOpenFontMemory :: Ptr () -> CSize -> CFloat -> IO (Ptr ())

-- | Close a TTF_Font handle.
foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_CloseFont"
  ttfCloseFont :: Ptr () -> IO ()

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_GetFontLineSkip"
  ttfLineSkip :: Ptr () -> IO CInt

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_GetFontAscent"
  ttfAscent :: Ptr () -> IO CInt

foreign import ccall unsafe "nano_ui_ttf_half_leading"
  ttfHalfLeading :: Ptr () -> IO CInt

-- | Whether a font face is a native bold face of a regular face's family.
foreign import ccall unsafe "nano_ui_ttf_is_bold_of"
  ttfIsBoldOf :: Ptr () -> Ptr () -> IO CBool

foreign import ccall unsafe "nano_ui_ttf_hint_for_grid"
  ttfHintForGrid :: Ptr () -> IO ()

foreign import ccall unsafe "nano_ui_ttf_space_advance"
  ttfSpaceAdvance :: Ptr () -> IO CFloat

foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_GetGlyphMetrics"
  ttfGlyphMetrics ::
    Ptr () ->
    CUInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
    Ptr CInt ->
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

-- | Whether a font or its fallbacks have a glyph for a Unicode codepoint.
foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_FontHasGlyph"
  ttfHasGlyph :: Ptr () -> CUInt -> IO Bool

-- | Attach a fallback font to a primary font.
foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_AddFallbackFont"
  ttfAddFallback :: Ptr () -> Ptr () -> IO Bool

-- | Remove a fallback font from a primary font.
foreign import capi unsafe "SDL3_ttf/SDL_ttf.h TTF_RemoveFallbackFont"
  ttfRemoveFallback :: Ptr () -> Ptr () -> IO ()

-- | Clone a font handle at a new point size sharing its underlying stream.
foreign import ccall unsafe "nano_ui_ttf_copy_font"
  ttfCopyFont :: Ptr () -> CFloat -> IO (Ptr ())

foreign import ccall unsafe "nano_ui_ttf_get_kerning"
  ttfGetKerning :: Ptr () -> CUInt -> CUInt -> IO CInt
