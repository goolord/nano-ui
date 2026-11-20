{-# LANGUAGE StrictData #-}
-- | Whole-layout cache snapshots, subtree spans, and floating-subtree placement cache.
module NanoUI.Internal.Layout.Arena.Cache
  ( LayoutCache (..)
  , CustomMeasureRecord
  , PlacedFloat (..)
  , newLayoutCache
  , captureLayoutCache
  , layoutSigMatches
  , restoreLayoutCache
  , restoreScrollExtents
  , subtreeSpan
  , snapshotPlaced
  , restorePlaced
  ) where

import Control.Monad (foldM, forM_, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Primitive.PrimArray
  ( PrimArray
  , copyMutablePrimArray
  , copyPrimArray
  , newPrimArray
  , readPrimArray
  , sizeofPrimArray
  , unsafeFreezePrimArray
  , writePrimArray
  )
import Data.Word (Word8, Word32, Word64)
import NanoUI.Internal.Layout.Arena.Query (forClassNodes_)
import NanoUI.Internal.Layout.Arena.Types
import NanoUI.Internal.Types (Rect (..))

-- | Cached layout signature and solved geometry for whole-layout reuse. The
-- backing arrays are reused; only cache misses capture a new solved frame.
data LayoutCache = LayoutCache
  { lcCap :: !Int
  , lcCount :: !Int
  , lcSig :: !Word64
  -- ^ The input signature ('naInputSig') the solve was captured with.
  , lcMeasures :: !(IntMap CustomMeasureRecord)
  -- ^ Per custom-measured node, what the solve's measure recorded.
  -- Checked by the frame runner because a custom measure may read state
  -- outside the arena that is not reflected in the input signature. Empty
  -- when no node uses custom measurement.
  , lcMeasureHooks :: !IntSet
  -- ^ The widgets with a custom measure registered at capture. Frame checks
  -- the set is unchanged, since gaining a hook changes a node's size without
  -- touching the arena.
  , lcSub :: !(IOArr Word64)
  -- ^ The restore keys ('naSubHash') of the captured frame. A node whose
  -- current key matches this was measured the same way last solve, and the
  -- solve restores its measured size instead of measuring again.
  , lcMeasured :: !(IOArr Float)
  -- ^ Per captured node, the width and height its measure took (two floats).
  , lcOffered :: !(IOArr Float)
  -- ^ The captured 'naOffered'.
  , lcUnsnapped :: !(IOArr Float)
  -- ^ The captured 'naUnsnapped'.
  , lcGeom :: !(IOArr Float)
  -- ^ The captured 'naArrGeom'.
  , lcStyle :: !(IOArr Float)
  -- ^ Per captured scroll node, its 'StyleScrollContentW' and 'StyleNodeValue'
  -- (two floats).
  , lcTags :: !(IOArr Word8)
  -- ^ The captured 'naArrTags'. A restore reads only 'TagScrollBarSlot'.
  , lcPlaced :: !(IORef (IntMap PlacedFloat))
  -- ^ Per floating node, what placing it over the captured solve left
  -- ('NanoUI.Internal.Layout.Solve.placeFloatingNodes'). Emptied at capture.
  }

-- | What placing a floating node (modal, window, popup) at a rect left in
-- its subtree: the geometry rows and the extents its scroll containers were
-- raised to. Placing it at the same rect over the same solve leaves the
-- same, so a frame can put this back instead.
data PlacedFloat = PlacedFloat
  { pfRect :: !Rect
  -- ^ The rect placement gave the node.
  , pfGeom :: !(PrimArray Float)
  -- ^ The 'naArrGeom' rows of the subtree, which starts at the node.
  , pfScroll :: ![(NodeIdx, Float, Float)]
  -- ^ Each scroll container's 'StyleScrollContentW' and 'StyleNodeValue'.
  }

-- | A custom measure's (available width, available height, measured width,
-- measured height), as raw float words.
type CustomMeasureRecord = (Word32, Word32, Word32, Word32)

-- | Empty layout cache with at least 16 slots. Capture a solved frame before reuse.
newLayoutCache :: Int -> IO LayoutCache
newLayoutCache cap0 = do
  let !cap = max 16 cap0
  lcSub <- newPrimArray cap
  lcMeasured <- newPrimArray (cap * 2)
  lcOffered <- newPrimArray (cap * 4)
  lcUnsnapped <- newPrimArray (cap * 4)
  lcGeom <- newPrimArray (cap * geomStride)
  lcStyle <- newPrimArray (cap * 2)
  lcTags <- newPrimArray (cap * tagStride)
  lcPlaced <- newIORef IM.empty
  pure (LayoutCache cap 0 0 IM.empty IS.empty lcSub lcMeasured lcOffered lcUnsnapped lcGeom lcStyle lcTags lcPlaced)

-- | Snapshot the current (post-solve) arena form, constraints and rects.
captureLayoutCache :: NodeArena -> LayoutCache -> IO LayoutCache
captureLayoutCache na lc0 = do
  n <- arenaCount na
  sig <- getInputSignature na
  subA <- readIORef (naSubHash na)
  measuredA <- readIORef (naMeasured na)
  offeredA <- readIORef (naOffered na)
  unsnappedA <- readIORef (naUnsnapped na)
  -- The copies below overwrite everything a capture reads, so a cache too
  -- small for the arena is replaced rather than grown.
  lc <- if n <= lcCap lc0 then pure lc0 else newLayoutCache (max n (lcCap lc0 * 2))
  a <- arenaArrays na
  copyMutablePrimArray (lcGeom lc) 0 (naArrGeom a) 0 (n * geomStride)
  forClassNodes_ na BackdropNodes $ \i -> do
    nt <- readTagEnum a i TagNodeType
    when (isScrollNode nt) $
      copyMutablePrimArray (lcStyle lc) (i * 2) (naArrStyle a) (i * styleStride + fromEnum StyleScrollContentW) 2
  copyMutablePrimArray (lcTags lc) 0 (naArrTags a) 0 (n * tagStride)
  copyMutablePrimArray (lcSub lc) 0 subA 0 n
  copyMutablePrimArray (lcMeasured lc) 0 measuredA 0 (n * 2)
  copyMutablePrimArray (lcOffered lc) 0 offeredA 0 (n * 4)
  copyMutablePrimArray (lcUnsnapped lc) 0 unsnappedA 0 (n * 4)
  writeIORef (lcPlaced lc) IM.empty
  pure lc {lcCount = n, lcSig = sig}

-- | Whether the frame's layout inputs hash to what the cache captured.
layoutSigMatches :: NodeArena -> LayoutCache -> IO Bool
layoutSigMatches na lc = do
  n <- arenaCount na
  if n <= 0 || n /= lcCount lc
    then pure False
    else (== lcSig lc) <$> getInputSignature na

-- | Restore only solver outputs. Rebuilt paint values/colors must survive a
-- cache hit; copying the entire cached style array would revert them.
restoreLayoutCache :: NodeArena -> LayoutCache -> IO ()
restoreLayoutCache na lc = do
  a <- arenaArrays na
  let !n = lcCount lc
  copyMutablePrimArray (naArrGeom a) 0 (lcGeom lc) 0 (n * geomStride)
  -- Scroll content width, node value (the content height) and scrollbar
  -- slot. Scroll containers are backdrop nodes, so walk that class, not the
  -- whole arena.
  forClassNodes_ na BackdropNodes $ \i -> do
    nt <- readTagEnum a i TagNodeType
    when (isScrollNode nt) $ do
      let !slotOff = i * tagStride + fromEnum TagScrollBarSlot
      restoreScrollExtents a lc i
      readPrimArray (lcTags lc) slotOff >>= writePrimArray (naArrTags a) slotOff

-- | Put back scroll container @i@'s captured content width and node value
-- (the content height), which the position pass raises.
{-# INLINE restoreScrollExtents #-}
restoreScrollExtents :: NodeArenaArrays -> LayoutCache -> NodeIdx -> IO ()
restoreScrollExtents a lc i =
  copyMutablePrimArray (naArrStyle a) (i * styleStride + fromEnum StyleScrollContentW) (lcStyle lc) (i * 2) 2

-- | The number of nodes in @idx@'s subtree when they are the indices from
-- @idx@ on, as a view that adds each node under the open container leaves
-- them, or else 'Nothing'.
subtreeSpan :: NodeArena -> NodeIdx -> IO (Maybe Int)
subtreeSpan na idx = do
  a <- arenaArrays na
  let node (!count, !hi) i = readTree a i TreeFirstChild >>= kids (count + 1, max hi i)
      kids acc c
        | c < 0 = pure acc
        | otherwise = node acc c >>= \acc' -> readTree a c TreeNextSibling >>= kids acc'
  (count, hi) <- node (0 :: Int, idx) idx
  pure (if hi - idx + 1 == count then Just count else Nothing)

-- | Copy what placement left in the @n@ nodes of @idx@'s subtree, which
-- start at @idx@ ('subtreeSpan'), placed at @rect@.
snapshotPlaced :: NodeArena -> NodeIdx -> Int -> Rect -> IO PlacedFloat
snapshotPlaced na idx n rect = do
  a <- arenaArrays na
  geom <- newPrimArray (n * geomStride)
  copyMutablePrimArray geom 0 (naArrGeom a) (idx * geomStride) (n * geomStride)
  let scrollFrom i acc = do
        nt <- readTagEnum a i TagNodeType
        if isScrollNode nt
          then (\cw v -> (i, cw, v) : acc) <$> readStyle a i StyleScrollContentW <*> readStyle a i StyleNodeValue
          else pure acc
  scroll <- foldM (flip scrollFrom) [] [idx + n - 1, idx + n - 2 .. idx]
  PlacedFloat rect <$> unsafeFreezePrimArray geom <*> pure scroll

-- | Put back what 'snapshotPlaced' copied from @idx@'s subtree.
restorePlaced :: NodeArena -> NodeIdx -> PlacedFloat -> IO ()
restorePlaced na idx PlacedFloat {pfGeom, pfScroll} = do
  a <- arenaArrays na
  copyPrimArray (naArrGeom a) (idx * geomStride) pfGeom 0 (sizeofPrimArray pfGeom)
  forM_ pfScroll $ \(i, cw, v) -> do
    writeStyle a i StyleScrollContentW cw
    writeStyle a i StyleNodeValue v

