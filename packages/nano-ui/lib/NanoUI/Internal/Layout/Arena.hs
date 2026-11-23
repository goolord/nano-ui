{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Layout nodes for one frame, stored in flat mutable arrays.
--
-- A node is an index ('NodeIdx') into those arrays. Indices count up in the
-- order the view adds nodes, so a parent's index is lower than its children's.
-- The strided arrays give every node a row of fixed width, and a column such
-- as 'GeomX' or 'TreeParent' names a slot in the row: node @idx@ keeps
-- geometry column @col@ at element @idx * geomStride + fromEnum col@ of
-- 'naArrGeom'. 'NodeArenaArrays' lists the arrays, and accessors such as
-- 'getNodeRect' and 'getParent' hide the arithmetic.
--
-- The widgets ("NanoUI.Internal.Widgets.Node") fill the arena while the view runs,
-- "NanoUI.Internal.Layout.Solve" writes every node's rect into it, and the frame's
-- input, paint and damage passes read it. 'resetNodeArena' empties it before
-- the view runs again, and a node index means nothing after that.
--
-- The arena also owns the solver's scratch buffers and memos, so that they
-- stay allocated from one frame to the next. The 'LayoutCache' defined here
-- lets a frame whose layout inputs did not change skip the solve.
module NanoUI.Internal.Layout.Arena
  ( NodeIdx
  , IOArr
  , NodeType (..)
  , NodeArenaArrays (..)
  , isWidgetNode
  , packsNodeFont
  , labelFontShift
  , nodeFontBits
  , hasCenteredLabel
  , isContainerNode
  , isScrollNode
  , isFloatingNode
  , NodeClass (..)
  , SizingTag (..)
  , DirTag (..)
  , NodeArena (..)
  , WidthMemo
  , IdIndex
  , FlexScratch (..)
  , newNodeArena
  , resetNodeArena
  , arenaCount
  , topModalNode
  , floatingNodeCount
  , layeredNodeCount
  , arenaArrays
  , GeomCol (..)
  , StyleCol (..)
  , TagCol (..)
  , TreeCol (..)
  , geomStride
  , styleStride
  , tagStride
  , treeStride
  , readGeom
  , writeGeom
  , readStyle
  , readTagEnum
  , writeTagEnum
  , readTree
  , writeTree
  , addNode
  , addNodeFromLayout
  , rootAttachParent
  , setNodeText
  , getParent
  , getFirstChild
  , getNextSibling
  , getNodeType
  , getDirection
  , getScrollContentW
  , setScrollContentW
  , AxisSizing (..)
  , axisSizing
  , readAxisSizing
  , getWidthSizing
  , getHeightSizing
  , getPadding
  , parentIsRow
  , getAlignX
  , getAlignY
  , getFlow
  , hasPinnedBelow
  , hasScrollerBelow
  , getPointerMode
  , getNodeRect
  , setRect
  , getClipRect
  , getClipBounds
  , setClipRect
  , getText
  , getOptions
  , setOptions
  , ImageNode (..)
  , setImageNode
  , getImageNode
  , setImageId
  , getImageId
  , getWidgetId
  , setWidgetId
  , lookupNodeByWidgetId
  , lookupNodeByKey
  , getIdSuperseded
  , getStyleIdx
  , setStyleIdx
  , getNodeValue
  , setNodeValue
  , setSolvedValue
  , getNodeFontSize
  , getNodeFontColor
  , getNodeScope
  , getArenaScope
  , setArenaScope
  , getScopeSignature
  , getNodePaintHash
  , getPaintSignature
  , ensureScratchCapacity
  , AxisSnapshot (..)
  , ensureAxisSnapshot
  , memoizeWidth
  , forFloatingNodes_
  , floatingNodesInPaintOrder
  , forChildNodes_
  , foldFlowChildrenM
  , flowChildrenInOrder
  , foldPlacedChildrenM
  , forChildrenInPaintOrder_
  , childrenTopFirst
  , firstChildOnTopJustM
  , drawnOver
  , AdornRows (..)
  , adornRows
  , findNodeRevM
  , findClassNodeM
  , findClassNodeRevM
  , foldClassNodesM
  , classNodes
  , foldClassNodeRevM
  , forClassNodes_
  , walkFloatingAncestors
  , findNodeM
  , foldNodesM
  , firstChildJustM
  , walkAncestors
  , LayoutCache (..)
  , CustomMeasureRecord
  , PlacedFloat (..)
  , newLayoutCache
  , captureLayoutCache
  , layoutSigMatches
  , computeSubtreeHashes
  , subtreeArrays
  , positionArrays
  , restoreLayoutCache
  , restoreScrollExtents
  , subtreeSpan
  , snapshotPlaced
  , restorePlaced
  ) where

import Control.Monad (forM_, unless, when)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Hashable (Hashable, hash)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.Array (newArray, readArray, sizeofMutableArray, writeArray)
import Data.Primitive.PrimArray
  ( copyMutablePrimArray
  , getSizeofMutablePrimArray
  , newPrimArray
  , readPrimArray
  , setPrimArray
  , writePrimArray
  )
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word64)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Image (ImageLook (..), Rotation (..))
import NanoUI.Internal.Layout.Arena.Cache
import NanoUI.Internal.Layout.Arena.Query
import NanoUI.Internal.Layout.Arena.Types
import NanoUI.Internal.Store (ptrEq)
import NanoUI.Internal.Style (Direction (..), Flow (..), Layout (..), Padding (..), PointerMode (..))
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..), newZeroedPrimArray)

-- | An empty arena. It starts with room for 256 nodes (@initialCapacity@), 64
-- children of one container in the solver's buffers, and 256 nesting depths.
-- All three grow when a view needs more.
newNodeArena :: IO NodeArena
newNodeArena = do
  let cap = initialCapacity
  naCount <- newIORef 0
  naCapacity <- newIORef cap
  naArrays <- newIORef =<< newNodeArenaArrays cap
  naScratch <- newIORef =<< newFlexScratch 64
  naSnapLevels <- newIORef =<< newArray 256 =<< newAxisSnapshot 0
  naFrameTag <- newIORef 1
  naWrapMemo <- newIORef =<< newWidthMemo cap
  naFitMemo <- newIORef =<< newWidthMemo cap
  naEpoch <- newIORef 1
  naIndex <- newIORef =<< newIdIndex 1024
  naScope <- newIORef 0
  naScopeSig <- newIORef 0
  naInputSig <- newZeroedPrimArray 1
  naPaintSig <- newZeroedPrimArray 1
  -- Zeroed: the stores start as the shared 'T.empty' and '[]', which pass the
  -- same-object check, and 0 marks a hash that was never taken.
  naTextHash <- newIORef =<< newZeroedPrimArray cap
  naOptionsHash <- newIORef =<< newZeroedPrimArray cap
  naOwnHash <- newIORef =<< newPrimArray cap
  naPaintHash <- newIORef =<< newPrimArray cap
  naSubHash <- newIORef =<< newPrimArray cap
  naMeasured <- newIORef =<< newPrimArray (cap * 2)
  naOffered <- newIORef =<< newPrimArray (cap * 4)
  naUnsnapped <- newIORef =<< newPrimArray (cap * 4)
  naSpans <- newIORef =<< newPrimArray cap
  naClassNodes <- newIORef =<< newPrimArray (cap * nodeClassCount)
  naClassCounts <- newZeroedPrimArray nodeClassCount
  naImages <- newIORef =<< newArray 0 noImageNode
  naImageCount <- newZeroedPrimArray 1
  naHighWater <- newIORef 0
  pure NodeArena {..}


-- | Begin an empty frame while retaining array capacity. Invalidates node
-- indices, width memos, and widget-id lookups, and resets paint-scope and
-- floating-panel state. 'addNode' initialises each reused slot.
resetNodeArena :: NodeArena -> IO ()
resetNodeArena na = do
  prevCount <- readIORef (naCount na)
  highWater <- readIORef (naHighWater na)
  -- Release boxed references in slots beyond the frame that just finished,
  -- while keeping live slots [0 .. prevCount - 1] intact so 'setNodeText' and
  -- 'setOptions' keep their pointer-equality hash reuse across steady frames.
  if prevCount < highWater
    then do
      a <- readIORef (naArrays na)
      th <- readIORef (naTextHash na)
      oh <- readIORef (naOptionsHash na)
      let clearAt !i = when (i < highWater) $ do
            writeArray (naArrTextStore a) i T.empty
            writeArray (naArrOptionsStore a) i []
            writeArray (naArrOptionsLast a) i []
            clearAt (i + 1)
      clearAt prevCount
      setPrimArray th prevCount (highWater - prevCount) 0
      setPrimArray oh prevCount (highWater - prevCount) 0
      writeIORef (naHighWater na) prevCount
    else when (prevCount > highWater) (writeIORef (naHighWater na) prevCount)
  prevImages <- readPrimArray (naImageCount na) 0
  when (prevImages > 0) $ do
    imgs <- readIORef (naImages na)
    let clearImg !i = when (i < prevImages) $ do
          writeArray imgs i noImageNode
          clearImg (i + 1)
    clearImg 0
  writeIORef (naCount na) 0
  writeIORef (naScope na) 0
  writeIORef (naScopeSig na) 0
  writePrimArray (naInputSig na) 0 0
  writePrimArray (naPaintSig na) 0 0
  setPrimArray (naClassCounts na) 0 nodeClassCount 0
  writePrimArray (naImageCount na) 0 0
  -- 0 marks a memo entry that was never written, so the tag wraps to 1. On
  -- wrap-around, entries from the previous use of tag 1 would read as live,
  -- so untag them all.
  !ft <- readIORef (naFrameTag na)
  writeIORef (naFrameTag na) (if ft == maxBound then 1 else ft + 1)
  when (ft == maxBound) $ forM_ [naWrapMemo na, naFitMemo na] $ \ref -> do
    WidthMemo tags _ <- readIORef ref
    getSizeofMutablePrimArray tags >>= \c -> setPrimArray tags 0 c 0
  -- Bumping the epoch empties the id index. On wrap-around, stale entries
  -- from the previous use of that epoch would read as live, so clear it.
  !ep <- readIORef (naEpoch na)
  let !ep' = ep + 1
  IdIndex slots mask live <- readIORef (naIndex na)
  writePrimArray live 0 0
  if ep' == 0
    then writeIORef (naEpoch na) 1 >> setPrimArray slots 0 (2 * (mask + 1)) 0
    else writeIORef (naEpoch na) ep'


{-# NOINLINE ensureCapacity #-}
ensureCapacity :: NodeArena -> Int -> IO ()
ensureCapacity na needed = do
  cap <- readIORef (naCapacity na)
  when (needed >= cap) $ do
    let newCap = cap * 2
    newA <- readIORef (naArrays na) >>= growNodeArenaArrays cap newCap
    forM_ [naWrapMemo na, naFitMemo na] $ \ref -> do
      WidthMemo tags slots <- readIORef ref
      wmTags <- growPrimArrayCopy tags cap newCap 0
      wmSlots <- growPrimArrayCopy slots (cap * memoStride) (newCap * memoStride) 0
      writeIORef ref WidthMemo {..}
    writeIORef (naArrays na) newA
    let growRef r old new = readIORef r >>= \a -> writeIORef r =<< growPrimArrayCopy a old new 0
    growRef (naTextHash na) cap newCap
    growRef (naOptionsHash na) cap newCap
    growRef (naOwnHash na) cap newCap
    growRef (naPaintHash na) cap newCap
    growRef (naSubHash na) cap newCap
    growRef (naMeasured na) (cap * 2) (newCap * 2)
    growRef (naOffered na) (cap * 4) (newCap * 4)
    growRef (naUnsnapped na) (cap * 4) (newCap * 4)
    growRef (naSpans na) cap newCap
    -- Each class list starts at a multiple of the capacity, so the lists move
    -- apart as it grows.
    oldClass <- readIORef (naClassNodes na)
    newClass <- newPrimArray (newCap * nodeClassCount)
    forM_ [0 .. nodeClassCount - 1] $ \c -> do
      k <- readPrimArray (naClassCounts na) c
      copyMutablePrimArray newClass (c * newCap) oldClass (c * cap) k
    writeIORef (naClassNodes na) newClass
    writeIORef (naCapacity na) newCap

-- | Copy of @a@ with room for @newCap@ nodes; new slots are zero or empty.
growNodeArenaArrays :: Int -> Int -> NodeArenaArrays -> IO NodeArenaArrays
growNodeArenaArrays cap newCap a = do
  naArrGeom <- growPrimArrayCopy (naArrGeom a) (cap * geomStride) (newCap * geomStride) 0
  naArrStyle <- growPrimArrayCopy (naArrStyle a) (cap * styleStride) (newCap * styleStride) 0
  naArrTags <- growPrimArrayCopy (naArrTags a) (cap * tagStride) (newCap * tagStride) 0
  naArrTree <- growPrimArrayCopy (naArrTree a) (cap * treeStride) (newCap * treeStride) 0
  naArrTextStore <- growBoxedStoreCopy T.empty (naArrTextStore a) cap newCap
  naArrOptionsStore <- growBoxedStoreCopy [] (naArrOptionsStore a) cap newCap
  naArrOptionsLast <- growBoxedStoreCopy [] (naArrOptionsLast a) cap newCap
  naArrFontColor <- growPrimArrayCopy (naArrFontColor a) cap newCap 0
  naArrScope <- growPrimArrayCopy (naArrScope a) cap newCap 0
  naArrImageId <- growPrimArrayCopy (naArrImageId a) cap newCap 0
  pure NodeArenaArrays {..}

-- | Keep a non-negative parent; otherwise attach to node 0 if it exists, or
-- return -1 for an empty arena. This keeps top-level floating nodes reachable
-- from the page root.
rootAttachParent :: NodeArena -> Int -> IO Int
rootAttachParent na parent
  | parent >= 0 = pure parent
  | otherwise = do
      n <- arenaCount na
      pure (if n > 0 then 0 else -1)

-- | Append a node laid out by the given 'Layout' and link it at the head of
-- its parent's child list. The parent must be -1 or an existing node index.
-- The caller assigns widget identity, text and type-specific style data.
{-# INLINE addNode #-}
addNode :: NodeArena -> NodeType -> Int -> Layout -> IO NodeIdx
addNode na nt parent Layout {..} = do
  idx <- readIORef (naCount na)
  ensureCapacity na (idx + 1)
  let (wTag, wVal) = sizingTag layoutWidth
      (hTag, hVal) = sizingTag layoutHeight
      pad = layoutPadding
      -- Scroll containers and grids have their own layout, and paint and hit
      -- tests visit their children in line order.
      !flow
        | not (isContainerNode nt) || isScrollNode nt || layoutGridCols > 0 || layoutGridMinColW > 0 = Line
        | otherwise = layoutFlow
      !lineGap = if layoutLineGap < 0 then layoutGap else layoutLineGap
      -- Floating nodes are placed separately, and a root has no parent to
      -- pin to.
      !pinned = isJust layoutPin && parent >= 0 && not (isFloatingNode nt)
      !(V2 pinX pinY) = fromMaybe (V2 0 0) layoutPin
  a <- arenaArrays na
  -- 'PointerPass' is inherited by all descendants.
  !pointerMode <-
    if parent < 0 || layoutPointer == PointerPass
      then pure layoutPointer
      else (\p -> if p == PointerPass then p else layoutPointer) <$> readTagEnum a parent TagPointer

  setPrimArray (naArrGeom a) (idx * geomStride) geomStride 0

  writeStyle a idx StyleWVal wVal
  writeStyle a idx StyleHVal hVal
  writeStyle a idx StylePadL (padL pad)
  writeStyle a idx StylePadR (padR pad)
  writeStyle a idx StylePadT (padT pad)
  writeStyle a idx StylePadB (padB pad)
  writeStyle a idx StyleGap layoutGap
  writeStyle a idx StyleMinW layoutMinW
  writeStyle a idx StyleMinH layoutMinH
  writeStyle a idx StyleMaxW layoutMaxW
  writeStyle a idx StyleMaxH layoutMaxH
  let !valuesOff = fromEnum StyleScrollContentW
  setPrimArray (naArrStyle a) (idx * styleStride + valuesOff) (styleStride - valuesOff) 0
  writeStyle a idx StyleGridMinColW layoutGridMinColW
  writeStyle a idx StyleFontSize layoutFontSize
  writeStyle a idx StyleLineGap lineGap
  writeStyle a idx StylePinX pinX
  writeStyle a idx StylePinY pinY
  writeStyle a idx StyleAspect layoutAspect

  setPrimArray (naArrTags a) (idx * tagStride) tagStride 0
  writeTagEnum a idx TagNodeType nt
  writeTagEnum a idx TagDirection $ case layoutDirection of
    Row | flow /= Layered -> DirRow
    _ -> DirColumn
  writeTagEnum a idx TagWSizing wTag
  writeTagEnum a idx TagHSizing hTag
  writeTagEnum a idx TagAlignX layoutAlignX
  writeTagEnum a idx TagAlignY layoutAlignY
  writeTagEnum a idx TagFlow flow
  writeTagEnum a idx TagPinned pinned
  writeTagEnum a idx TagPointer pointerMode
  writeTagEnum a idx TagLineAlign layoutLineAlign

  setPrimArray (naArrTree a) (idx * treeStride) treeStride 0
  writeTree a idx TreeParent parent
  writeTree a idx TreeFirstChild (-1)
  writeTree a idx TreeNextSibling (-1)
  writeTree a idx TreeTextIdx (-1)
  writeTree a idx TreeGridCols layoutGridCols

  -- The creation inputs from a constant seed, with no position: the node's
  -- paint hash starts here. Its own hash, the layout cache's key, adds its
  -- index and parent.
  let !content =
        foldl'
          (\acc (t, v) -> mixTagged acc t v)
          (0x6E616E6F2D7569 :: Word64)
          [ (0x4e54, fromIntegral (fromEnum nt))
          , (0x4449, fromIntegral (fromEnum layoutDirection))
          , (0x5754, fromIntegral (fromEnum wTag))
          , (0x5746, fromIntegral (castFloatToWord32 wVal))
          , (0x4854, fromIntegral (fromEnum hTag))
          , (0x4846, fromIntegral (castFloatToWord32 hVal))
          , (0x504c, fromIntegral (castFloatToWord32 (padL pad)))
          , (0x5052, fromIntegral (castFloatToWord32 (padR pad)))
          , (0x5054, fromIntegral (castFloatToWord32 (padT pad)))
          , (0x5042, fromIntegral (castFloatToWord32 (padB pad)))
          , (0x4741, fromIntegral (castFloatToWord32 layoutGap))
          , (0x4d57, fromIntegral (castFloatToWord32 layoutMinW))
          , (0x4d48, fromIntegral (castFloatToWord32 layoutMinH))
          , (0x5857, fromIntegral (castFloatToWord32 layoutMaxW))
          , (0x5848, fromIntegral (castFloatToWord32 layoutMaxH))
          , (0x4743, fromIntegral layoutGridCols)
          , (0x474d, fromIntegral (castFloatToWord32 layoutGridMinColW))
          , (0x4648, fromIntegral (castFloatToWord32 layoutFontSize))
          , (0x4158, fromIntegral (fromEnum layoutAlignX))
          , (0x4159, fromIntegral (fromEnum layoutAlignY))
          , (0x464c, fromIntegral (fromEnum flow))
          , (0x4c47, fromIntegral (castFloatToWord32 lineGap))
          , (0x5049, fromIntegral (fromEnum pinned))
          , (0x5058, fromIntegral (castFloatToWord32 pinX))
          , (0x5059, fromIntegral (castFloatToWord32 pinY))
          , (0x4153, fromIntegral (castFloatToWord32 layoutAspect) .|. fromIntegral (fromEnum layoutLineAlign) `shiftL` 32)
          ]
      !nodeSig = mixTagged (mixTagged content 0x4958 (fromIntegral idx)) 0x5041 (fromIntegral (parent + 1))
  mixInputSig na 0x4e4f nodeSig
  ownA <- readIORef (naOwnHash na)
  writePrimArray ownA idx nodeSig
  scope <- readIORef (naScope na)
  writePrimArray (naArrScope a) idx scope
  -- Bit 1 marks inert (input-only) scopes, which have no visual effect and
  -- must not trigger paint-scope invalidation.
  let !paintScope = scope .&. complement 2
  paintA <- readIORef (naPaintHash na)
  writePrimArray paintA idx (mixTagged content 0x5343 (fromIntegral paintScope))

  -- The font colour is paint state, so the input signature leaves it out.
  case layoutFontColor of
    Nothing -> writePrimArray (naArrFontColor a) idx 0
    Just (Color w) -> do
      writePrimArray (naArrFontColor a) idx (0x100000000 .|. fromIntegral w)
      mixPaintSig na idx 0x4643 (fromIntegral w)
  when (paintScope /= 0) $ do
    sig <- readIORef (naScopeSig na)
    writeIORef (naScopeSig na) $! (sig * 0x100000001b3) `xor` (fromIntegral idx `shiftL` 32 .|. fromIntegral paintScope)
  writeArray (naArrOptionsStore a) idx []

  when (parent >= 0) $ do
    fc <- readTree a parent TreeFirstChild
    writeTree a idx TreeNextSibling fc
    writeTree a parent TreeFirstChild idx
    cc <- readTree a parent TreeChildCount
    writeTree a parent TreeChildCount (cc + 1)
    when pinned $ markAncestorTag a TagPinnedBelow parent
  when (isFloatingNode nt) $ pushClassNode na FloatingNodes idx
  when (nt == NodePanel || nt == NodeScrollContainer) $ pushClassNode na BackdropNodes idx
  when (flow == Layered || pinned) $ pushClassNode na LayeredNodes idx
  when (isWidgetNode nt || isScrollNode nt || pointerMode == PointerBlock) $ do
    pushClassNode na PointerNodes idx
    when (isScrollNode nt || nt == NodeTextArea || pointerMode == PointerBlock) $
      markAncestorTag a TagScrollerBelow idx
  when (nt == NodeDrawing) $ pushClassNode na DrawingNodes idx
  writeIORef (naCount na) (idx + 1)
  pure idx

-- | Mark a node and its ancestors with @tag@, stopping at the first one
-- already marked. Top-level so 'addNode' allocates no closure when the
-- branch is not taken.
markAncestorTag :: NodeArenaArrays -> TagCol -> Int -> IO ()
markAncestorTag a tag = go
  where
    go !p = when (p >= 0) $ do
      marked <- readTagEnum a p tag
      unless marked $ writeTagEnum a p tag True >> readTree a p TreeParent >>= go

-- | Append node @idx@ to the list of class @c@. 'addNode' has made room.
{-# INLINE pushClassNode #-}
pushClassNode :: NodeArena -> NodeClass -> NodeIdx -> IO ()
pushClassNode na c idx = do
  let ci = fromEnum c
  k <- readPrimArray (naClassCounts na) ci
  cap <- readIORef (naCapacity na)
  arr <- readIORef (naClassNodes na)
  writePrimArray arr (ci * cap + k) idx
  writePrimArray (naClassCounts na) ci (k + 1)

-- | 'addNode' as a call instead of inlined, for the view's many call sites.
addNodeFromLayout :: NodeArena -> NodeType -> Int -> Layout -> IO NodeIdx
addNodeFromLayout na nt parent l = addNode na nt parent l

-- | Assign text to a live node and mark its text slot as present. Re-setting
-- the same 'Text' object reuses its cached hash, so a steady frame hashes no
-- text bytes.
{-# INLINE setNodeText #-}
setNodeText :: NodeArena -> NodeIdx -> Text -> IO ()
setNodeText na idx txt = do
  a <- arenaArrays na
  old <- readArray (naArrTextStore a) idx
  h <- cachedHash (naTextHash na) idx (old `ptrEq` txt) 0x54455854 txt
  writeArray (naArrTextStore a) idx txt
  writeTree a idx TreeTextIdx idx
  mixNodeInput na idx 0x5458 h


-- | Set image node @idx@'s look and natural size. The size is a layout
-- input, so it is mixed into the node's hash.
setImageNode :: NodeArena -> NodeIdx -> ImageNode -> IO ()
setImageNode na idx node@ImageNode {inWidth = w, inHeight = h} = do
  k <- readPrimArray (naImageCount na) 0
  arr0 <- readIORef (naImages na)
  let cap = sizeofMutableArray arr0
  arr <-
    if k < cap
      then pure arr0
      else do
        grown <- growBoxedStoreCopy noImageNode arr0 cap (max 16 (2 * cap))
        grown <$ writeIORef (naImages na) grown
  writeArray arr k node
  writePrimArray (naImageCount na) 0 (k + 1)
  a <- arenaArrays na
  writeTree a idx TreeStyleIdx (k + 1)
  mixNodeInput na idx 0x494d (fromIntegral (castFloatToWord32 w) `shiftL` 32 .|. fromIntegral (castFloatToWord32 h))
  mixPaintSig na idx 0x4c4b (lookWord (inLook node))

-- | A hash of everything a look draws with, for 'getPaintSignature'.
lookWord :: ImageLook -> Word64
lookWord look =
  foldl'
    (\acc (t, v) -> mixTagged acc t v)
    0
    [ (1, fromIntegral (fromEnum (lookFit look)))
    , (2, fromIntegral (fromEnum (lookAlignX look)))
    , (3, fromIntegral (fromEnum (lookAlignY look)))
    , (4, floatWord (lookOpacity look))
    , (5, case lookRotation look of RotateFloating r -> floatWord r; RotateSolid r -> 0x100000000 .|. floatWord r)
    , (6, maybe 0 (const 1) crop)
    , (7, maybe 0 (\(Rect x y _ _) -> floatWord x `shiftL` 32 .|. floatWord y) crop)
    , (8, maybe 0 (\(Rect _ _ w h) -> floatWord w `shiftL` 32 .|. floatWord h) crop)
    , (9, floatWord (lookScale look))
    , (10, let Color w = lookTint look in fromIntegral w)
    ]
  where
    crop = lookCrop look
    floatWord = fromIntegral . castFloatToWord32

-- | Set image node @idx@'s image id: the 'NanoUI.ImageId'
-- it draws, or 0 for none. Layout does not read it, so it is paint state
-- ('getPaintSignature').
{-# INLINE setImageId #-}
setImageId :: NodeArena -> NodeIdx -> Int -> IO ()
setImageId na idx tid = do
  a <- arenaArrays na
  writePrimArray (naArrImageId a) idx tid
  mixPaintSig na idx 0x4944 (fromIntegral tid)

-- | Replace a node's choice labels for this frame. Re-setting the same list
-- object reuses its cached hash, like 'setNodeText'.
{-# INLINE setOptions #-}
setOptions :: NodeArena -> NodeIdx -> [Text] -> IO ()
setOptions na idx opts = do
  a <- arenaArrays na
  -- Compared with the list last hashed here, not the slot 'addNode' just
  -- cleared: against that, every list would rehash and an empty one would
  -- take an old list's hash.
  old <- readArray (naArrOptionsLast a) idx
  h <- cachedHash (naOptionsHash na) idx (old `ptrEq` opts) 0x4f505453 opts
  writeArray (naArrOptionsStore a) idx opts
  writeArray (naArrOptionsLast a) idx opts

  mixNodeInput na idx 0x4f50 h

-- | The node's value hash from @ref@ when @same@ says the value is the
-- object last hashed there, else a fresh hash of @x@, salted and cached.
{-# INLINE cachedHash #-}
cachedHash :: Hashable a => IORef (IOArr Word64) -> NodeIdx -> Bool -> Word64 -> a -> IO Word64
cachedHash ref idx same salt x = do
  arr <- readIORef ref
  c <- readPrimArray arr idx
  if same && c /= 0
    then pure c
    else do
      let !h = fromIntegral (hash x) `xor` salt
      h <$ writePrimArray arr idx h

-- | Assign a node's identity and index nonzero ids for lookup. Assign once per
-- node: this does not remove a mapping previously stored under another id.
-- Lookups of the id now find this node, and an earlier node with the same id
-- is marked superseded ('getIdSuperseded').
{-# INLINE setWidgetId #-}
setWidgetId :: NodeArena -> NodeIdx -> WidgetId -> IO ()
setWidgetId na idx wid = do
  a <- arenaArrays na
  let WidgetId w = wid
  writeTree a idx TreeWidgetId (fromIntegral w)
  mixNodeInput na idx 0x5749 w
  when (hashWidgetId wid /= 0) $ do
    !ep <- readIORef (naEpoch na)
    prev <- indexWidgetId na ep w idx
    when (prev >= 0 && prev /= idx) $ writeTagEnum a prev TagIdSuperseded True

-- | Node most recently indexed under this id in the current frame. Returns
-- 'Nothing' for zero, an unknown id, or an entry from an earlier frame.
{-# INLINE lookupNodeByWidgetId #-}
lookupNodeByWidgetId :: NodeArena -> WidgetId -> IO (Maybe NodeIdx)
lookupNodeByWidgetId na (WidgetId key)
  | key == 0 = pure Nothing
  | otherwise = do
      !ep <- readIORef (naEpoch na)
      IdIndex slots mask _ <- readIORef (naIndex na)
      probeSlot slots mask ep key (\_ -> pure Nothing) (\_ v -> pure (Just (slotNode v)))

-- | Index node @idx@ under the nonzero id @key@ in epoch @ep@. Returns the
-- node previously indexed under @key@ this epoch, or -1. The insert's probe
-- finds it for free.
{-# INLINE indexWidgetId #-}
indexWidgetId :: NodeArena -> Word32 -> Word64 -> Int -> IO Int
indexWidgetId na ep key idx = do
  ii@(IdIndex slots mask live) <- readIORef (naIndex na)
  let !val = fromIntegral ep `shiftL` 32 .|. (fromIntegral idx .&. 0xFFFFFFFF)
  probeSlot slots mask ep key
    ( \s -> do
        writePrimArray slots (2 * s) key
        writePrimArray slots (2 * s + 1) val
        n <- readPrimArray live 0
        writePrimArray live 0 (n + 1)
        -- Keep the load factor at or below one half so probes stay short.
        when (2 * (n + 1) > mask + 1) $ writeIORef (naIndex na) =<< growIdIndex ep ii
        pure (-1)
    )
    (\s v -> slotNode v <$ writePrimArray slots (2 * s + 1) val)

-- | Double the table, keeping only entries of epoch @ep@.
growIdIndex :: Word32 -> IdIndex -> IO IdIndex
growIdIndex ep (IdIndex slots mask live) = do
  new@(IdIndex slots' mask' live') <- newIdIndex (2 * (mask + 1))
  writePrimArray live' 0 =<< readPrimArray live 0
  forM_ [0 .. mask] $ \s -> do
    v <- readPrimArray slots (2 * s + 1)
    when (v `shiftR` 32 == fromIntegral ep) $ do
      k <- readPrimArray slots (2 * s)
      -- Keys are distinct, so the probe always ends at a free slot.
      probeSlot slots' mask' ep k
        (\t -> writePrimArray slots' (2 * t) k >> writePrimArray slots' (2 * t + 1) v)
        (\_ _ -> pure ())
  pure new

-- | 'lookupNodeByWidgetId' using the id's integer store key.
{-# INLINE lookupNodeByKey #-}
lookupNodeByKey :: NodeArena -> Int -> IO (Maybe NodeIdx)
lookupNodeByKey na key = lookupNodeByWidgetId na (WidgetId (fromIntegral key))

-- | Set the type-specific numeric value paint reads. It is paint state
-- ('getPaintSignature'), not a layout input: values the solver works out go
-- through 'setSolvedValue'.
{-# INLINE setNodeValue #-}
setNodeValue :: NodeArena -> NodeIdx -> Float -> IO ()
setNodeValue na idx v = do
  setSolvedValue na idx v
  mixPaintSig na idx 0x5641 (fromIntegral (castFloatToWord32 v))

-- | Hash over everything the view put in the arena that paint reads: the
-- input signature ('naInputSig'), the node count, the scopes
-- ('getScopeSignature'), and the paint state the input signature leaves
-- out, mixed as it is written: node values ('setNodeValue'), font colours,
-- the style index of boxes, images and drawings, and images' ids and looks.
-- Two frames with the same signature have identical arena paint inputs; the
-- damage pass covers solved layout, widget state in the store, and theme
-- changes.
getPaintSignature :: NodeArena -> IO Word64
getPaintSignature na = do
  input <- getInputSignature na
  count <- arenaCount na
  scope <- getScopeSignature na
  paint <- readPrimArray (naPaintSig na) 0
  pure (mixTagged (mixTagged (mixTagged input 1 (fromIntegral count)) 2 scope) 3 paint)

-- | Compute every node's restore key ('naSubHash'). A first pass, parents
-- before children, parks each node's ancestor chain (its ancestors' own
-- hashes) in its slot. A second pass, from the last node down so each child's
-- key is final before its parent reads it, folds the node's own hash and chain
-- with its children's keys. Child positions mix in, so reordering children
-- counts as a change. Run after the view has built and before the solve.
computeSubtreeHashes :: NodeArena -> IO ()
computeSubtreeHashes na = do
  n <- arenaCount na
  a <- arenaArrays na
  own <- readIORef (naOwnHash na)
  sub <- readIORef (naSubHash na)
  let chain !i
        | i >= n = pure ()
        | otherwise = do
            p <- readTree a i TreeParent
            anc <-
              if p < 0
                then pure 0
                else mixTagged <$> readPrimArray sub p <*> pure 0x414e <*> readPrimArray own p
            writePrimArray sub i anc
            chain (i + 1)
      fold !i
        | i < 0 = pure ()
        | otherwise = do
            o <- readPrimArray own i
            anc <- readPrimArray sub i
            fc <- readTree a i TreeFirstChild
            (cnt, acc) <- walkKids a sub 0 o fc
            writePrimArray sub i (mixTagged acc cnt (o `xor` anc))
            fold (i - 1)
  chain 0
  fold (n - 1)

-- | Fold a child list (most recently added first) into the parent's subtree
-- hash, each child's position mixed in so reordering counts as a change.
walkKids :: NodeArenaArrays -> IOArr Word64 -> Word64 -> Word64 -> NodeIdx -> IO (Word64, Word64)
walkKids a sub !pos !h !c
  | c < 0 = pure (pos, h)
  | otherwise = do
      sh <- readPrimArray sub c
      nxt <- readTree a c TreeNextSibling
      walkKids a sub (pos + 1) (mixTagged h pos sh) nxt

-- | The subtree-hash and measured-size arrays the solver reads and writes
-- during a solve. Do not retain them across arena growth.
subtreeArrays :: NodeArena -> IO (IOArr Word64, IOArr Float)
subtreeArrays na = (,) <$> readIORef (naSubHash na) <*> readIORef (naMeasured na)

-- | The offered-rect, unsnapped-rect and span arrays ('naOffered',
-- 'naUnsnapped', 'naSpans') the solver writes during a solve. Do not retain
-- them across arena growth.
positionArrays :: NodeArena -> IO (IOArr Float, IOArr Float, IOArr Int)
positionArrays na = (,,) <$> readIORef (naOffered na) <*> readIORef (naUnsnapped na) <*> readIORef (naSpans na)

-- | Store a style code encoded for this node's type. Part of the layout
-- input signature, except on box, image, and drawing nodes: their style code
-- is paint data (a colour, a version), which goes in the paint signature
-- ('getPaintSignature') instead.
{-# INLINE setStyleIdx #-}
setStyleIdx :: NodeArena -> NodeIdx -> Int -> IO ()
setStyleIdx na idx v = do
  a <- arenaArrays na
  writeTree a idx TreeStyleIdx v
  nt <- readTagEnum a idx TagNodeType
  if nt == NodeBox || nt == NodeImage || nt == NodeDrawing
    then mixPaintSig na idx 0x5354 (fromIntegral v)
    else mixNodeInput na idx 0x5354 (fromIntegral v)

-- | The snapshot buffers for nesting depth @depth@, with room for at least
-- @needed@ entries. Each depth keeps its buffers across frames, so nothing is
-- allocated in steady state once they are big enough, and nesting depth has
-- no fixed limit.
{-# NOINLINE ensureAxisSnapshot #-}
ensureAxisSnapshot :: NodeArena -> Int -> Int -> IO AxisSnapshot
ensureAxisSnapshot na depth needed = do
  levels0 <- readIORef (naSnapLevels na)
  let !sz = sizeofMutableArray levels0
  levels <-
    if depth < sz
      then pure levels0
      else do
        empty <- newAxisSnapshot 0
        levels <- growBoxedStoreCopy empty levels0 sz (max (depth + 1) (sz * 2))
        levels <$ writeIORef (naSnapLevels na) levels
  s <- readArray levels depth
  cap <- getSizeofMutablePrimArray (asIdx s)
  if needed <= cap
    then pure s
    else do
      s' <- newAxisSnapshot (max needed (max 64 (cap * 2)))
      s' <$ writeArray levels depth s'

newAxisSnapshot :: Int -> IO AxisSnapshot
newAxisSnapshot cap = AxisSnapshot <$> newPrimArray cap <*> newPrimArray cap

-- | Memoize @compute@ for node @idx@ at width @key@ in one of the arena's
-- per-frame memos. Only float noise (under a thousandth of a pixel) shares
-- an entry: text that fits 144 px exactly wraps at 143.8, so a wider
-- tolerance can hand back a different line count than paint draws.
{-# INLINE memoizeWidth #-}
memoizeWidth :: NodeArena -> IORef WidthMemo -> NodeIdx -> Float -> IO (Float, Float) -> IO (Float, Float)
memoizeWidth na ref idx key compute = do
  ft <- readIORef (naFrameTag na)
  WidthMemo tags slots <- readIORef ref
  tag <- readPrimArray tags idx
  let !base = idx * memoStride
  hit <-
    if tag /= ft
      then pure False
      else do
        k <- readPrimArray slots base
        pure (abs (k - key) <= 1.0e-3)
  if hit
    then (,) <$> readPrimArray slots (base + 1) <*> readPrimArray slots (base + 2)
    else do
      r@(x, y) <- compute
      writePrimArray tags idx ft
      writePrimArray slots base key
      writePrimArray slots (base + 1) x
      writePrimArray slots (base + 2) y
      pure r

-- | The flex scratch, with room for at least @needed@ entries. Growing it
-- replaces the buffers without copying: a container fills them only after
-- asking for room, and reads its children back from its snapshot once a
-- child may have used them.
{-# INLINE ensureScratchCapacity #-}
ensureScratchCapacity :: NodeArena -> Int -> IO FlexScratch
ensureScratchCapacity na needed = do
  s <- readIORef (naScratch na)
  if needed <= fsCap s then pure s else growScratch na (max needed (fsCap s * 2))

{-# NOINLINE growScratch #-}
growScratch :: NodeArena -> Int -> IO FlexScratch
growScratch na cap = do
  s <- newFlexScratch cap
  s <$ writeIORef (naScratch na) s
