{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# OPTIONS_HADDOCK not-home #-}

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
  , scrollableNodeCount
  , textFieldNodeCount
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
  , readPadding
  , getPadding
  , parentIsRow
  , getAlignX
  , readAlignX
  , readAlignY
  , readItemsAlign
  , effectiveAlignX
  , effectiveAlignY
  , getFlow
  , hasPinnedBelow
  , hasScrollerBelow
  , hasBaselineChild
  , getPointerMode
  , getNodeRect
  , setRect
  , getClipRect
  , getClipBounds
  , setClipRect
  , writeClipRectRaw
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
  , readStyleIdx
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
  , mixNodePaintHash
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
  , flowChildEnds
  , foldPlacedChildrenM
  , forChildrenInPaintOrder_
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
  , restoreLayoutCache
  , restoreScrollExtents
  , subtreeSpan
  , snapshotPlaced
  , restorePlaced
  ) where

import Control.Monad (forM_, unless, when)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Hashable (Hashable, hash)
import Data.IORef (newIORef, readIORef, writeIORef)
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
import Data.Primitive.PrimVar (newPrimVar, readPrimVar, writePrimVar)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word32, Word64)
import GHC.Float (castFloatToWord32)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Image (ImageLook (..), Rotation (..))
import NanoUI.Internal.Layout.Arena.Cache
import NanoUI.Internal.Layout.Arena.Query
import NanoUI.Internal.Layout.Arena.Types
import NanoUI.Internal.Store (ptrEq)
import NanoUI.Internal.Style (AlignY (..), Direction (..), Flow (..), ItemsAlign (..), Layout (..), Padding (..), PointerMode (..))
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..), forUpTo_, newZeroedPrimArray)

-- | An empty arena. It starts with room for 256 nodes (@initialCapacity@), 64
-- children of one container in the solver's buffers, and 256 nesting depths.
-- All three grow when a view needs more.
newNodeArena :: IO NodeArena
newNodeArena = do
  let cap = initialCapacity
  naCount <- newPrimVar 0
  naCapacity <- newPrimVar cap
  naArrays <- newIORef =<< newNodeArenaArrays cap
  naScratch <- newIORef =<< newFlexScratch 64
  naSnapLevels <- newIORef =<< newArray 256 =<< newAxisSnapshot 0
  naFrameTag <- newPrimVar 1
  naEpoch <- newPrimVar 1
  naIndex <- newIORef =<< newIdIndex 1024
  naScope <- newPrimVar 0
  naScopeSig <- newPrimVar 0
  naInputSig <- newZeroedPrimArray 1
  naPaintSig <- newZeroedPrimArray 1
  naClassCounts <- newZeroedPrimArray nodeClassCount
  naImages <- newIORef =<< newArray 0 noImageNode
  naImageCount <- newZeroedPrimArray 1
  naHighWater <- newPrimVar 0
  pure NodeArena {..}

-- | Begin an empty frame while retaining array capacity. Invalidates node
-- indices, width memos, and widget-id lookups, and resets paint-scope and
-- floating-panel state. 'addNode' initialises each reused slot.
resetNodeArena :: NodeArena -> IO ()
resetNodeArena na = do
  prevCount <- readPrimVar (naCount na)
  highWater <- readPrimVar (naHighWater na)
  -- Release boxed references in slots beyond the frame that just finished,
  -- while keeping live slots [0 .. prevCount - 1] intact so 'setNodeText' and
  -- 'setOptions' keep their pointer-equality hash reuse across steady frames.
  if prevCount < highWater
    then do
      a <- readIORef (naArrays na)
      let clearAt !i = when (i < highWater) $ do
            writeArray (naArrTextStore a) i T.empty
            writeArray (naArrOptionsStore a) i []
            writeArray (naArrOptionsLast a) i []
            clearAt (i + 1)
      clearAt prevCount
      setPrimArray (naArrTextHash a) prevCount (highWater - prevCount) 0
      setPrimArray (naArrOptionsHash a) prevCount (highWater - prevCount) 0
      writePrimVar (naHighWater na) prevCount
    else when (prevCount > highWater) (writePrimVar (naHighWater na) prevCount)
  prevImages <- readPrimArray (naImageCount na) 0
  when (prevImages > 0) $ do
    imgs <- readIORef (naImages na)
    forUpTo_ prevImages (\i -> writeArray imgs i noImageNode)
  writePrimVar (naCount na) 0
  writePrimVar (naScope na) 0
  writePrimVar (naScopeSig na) 0
  writePrimArray (naInputSig na) 0 0
  writePrimArray (naPaintSig na) 0 0
  setPrimArray (naClassCounts na) 0 nodeClassCount 0
  writePrimArray (naImageCount na) 0 0
  -- 0 marks a memo entry that was never written, so the tag wraps to 1. On
  -- wrap-around, entries from the previous use of tag 1 would read as live,
  -- so untag them all.
  !ft <- readPrimVar (naFrameTag na)
  writePrimVar (naFrameTag na) (if ft == maxBound then 1 else ft + 1)
  when (ft == maxBound) $ do
    a <- readIORef (naArrays na)
    forM_ [naArrWrapMemo a, naArrFitMemo a] $ \(WidthMemo tags _) ->
      getSizeofMutablePrimArray tags >>= \c -> setPrimArray tags 0 c 0
  -- Bumping the epoch empties the id index. On wrap-around, stale entries
  -- from the previous use of that epoch would read as live, so clear it.
  !ep <- readPrimVar (naEpoch na)
  let !ep' = ep + 1
  IdIndex slots mask live <- readIORef (naIndex na)
  writePrimArray live 0 0
  if ep' == 0
    then writePrimVar (naEpoch na) 1 >> setPrimArray slots 0 (2 * (mask + 1)) 0
    else writePrimVar (naEpoch na) ep'

{-# NOINLINE ensureCapacity #-}
ensureCapacity :: NodeArena -> Int -> IO ()
ensureCapacity na needed = do
  cap <- readPrimVar (naCapacity na)
  when (needed >= cap) $ do
    let newCap = cap * 2
    newA <- readIORef (naArrays na) >>= growNodeArenaArrays (naClassCounts na) cap newCap
    writeIORef (naArrays na) newA
    writePrimVar (naCapacity na) newCap

-- | Copy of @a@ with room for @newCap@ nodes; new slots are zero or empty.
growNodeArenaArrays :: IOArr Int -> Int -> Int -> NodeArenaArrays -> IO NodeArenaArrays
growNodeArenaArrays classCounts cap newCap a = do
  let growMemo (WidthMemo tags slots) =
        WidthMemo <$> growPrimArrayCopy tags cap newCap 0 <*> growPrimArrayCopy slots (cap * memoStride) (newCap * memoStride) 0
  cn <- newPrimArray (newCap * nodeClassCount)
  forM_ [0 .. nodeClassCount - 1] $ \c -> do
    k <- readPrimArray classCounts c
    copyMutablePrimArray cn (c * newCap) (naArrClassNodes a) (c * cap) k
  NodeArenaArrays
    <$> growPrimArrayCopy (naArrGeom a) (cap * geomStride) (newCap * geomStride) 0
    <*> growPrimArrayCopy (naArrStyle a) (cap * styleStride) (newCap * styleStride) 0
    <*> growPrimArrayCopy (naArrTags a) (cap * tagStride) (newCap * tagStride) 0
    <*> growPrimArrayCopy (naArrTree a) (cap * treeStride) (newCap * treeStride) 0
    <*> growPrimArrayCopy (naArrStyleIdx a) cap newCap 0
    <*> growBoxedStoreCopy T.empty (naArrTextStore a) cap newCap
    <*> growBoxedStoreCopy [] (naArrOptionsStore a) cap newCap
    <*> growBoxedStoreCopy [] (naArrOptionsLast a) cap newCap
    <*> growPrimArrayCopy (naArrFontColor a) cap newCap 0
    <*> growPrimArrayCopy (naArrScope a) cap newCap 0
    <*> growPrimArrayCopy (naArrImageId a) cap newCap 0
    <*> growPrimArrayCopy (naArrTextHash a) cap newCap 0
    <*> growPrimArrayCopy (naArrOptionsHash a) cap newCap 0
    <*> growPrimArrayCopy (naArrOwnHash a) cap newCap 0
    <*> growPrimArrayCopy (naArrPaintHash a) cap newCap 0
    <*> growPrimArrayCopy (naArrSubHash a) cap newCap 0
    <*> growPrimArrayCopy (naArrMeasured a) (cap * 2) (newCap * 2) 0
    <*> growPrimArrayCopy (naArrOffered a) (cap * 4) (newCap * 4) 0
    <*> growPrimArrayCopy (naArrUnsnapped a) (cap * 4) (newCap * 4) 0
    <*> growPrimArrayCopy (naArrSpans a) cap newCap 0
    <*> pure cn
    <*> growMemo (naArrWrapMemo a)
    <*> growMemo (naArrFitMemo a)

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
  idx <- readPrimVar (naCount na)
  cap <- readPrimVar (naCapacity na)
  when (idx + 1 >= cap) $ ensureCapacity na (idx + 1)
  let (wTag0, wVal0) = sizingTag layoutWidth
      (hTag0, hVal0) = sizingTag layoutHeight
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
      !axByte =
        fromIntegral (fromEnum layoutAlignX)
          .|. (if layoutAlignExplicit .&. 1 /= 0 then 0x80 else 0 :: Word8)
      !ayByte =
        fromIntegral (fromEnum layoutAlignY)
          .|. (if layoutAlignExplicit .&. 2 /= 0 then 0x80 else 0 :: Word8)
  a <- arenaArrays na
  -- 'ItemsStretch' promotes an unaligned fit-sized flow child to grow along
  -- its parent's cross axis (width in a column, height in a row or grid row).
  -- Return a plain unboxed Int tag (0 none, 1 stretch W, 2 stretch H) so the
  -- common unstretched path allocates no tuples or boxed Floats.
  !stretch <-
    if parent >= 0
      && not pinned
      && not (isFloatingNode nt)
      && ((wTag0 == SizingFit && axByte == 0) || (hTag0 == SizingFit && ayByte == 0))
      then do
        pItems <- readTagEnum a parent TagItemsAlign
        if pItems == ItemsStretch
          then stretchAxis a parent wTag0 axByte hTag0 ayByte
          else pure 0
      else pure 0
  let !wTag = if stretch == 1 then SizingGrow else wTag0
      !wVal = if stretch == 1 then 1 else wVal0
      !hTag = if stretch == 2 then SizingGrow else hTag0
      !hVal = if stretch == 2 then 1 else hVal0
  -- 'PointerPass' is inherited by all descendants.
  !pointerMode <-
    if parent < 0 || layoutPointer == PointerPass
      then pure layoutPointer
      else do
        p <- readTagEnum a parent TagPointer
        pure $! if p == PointerPass then p else layoutPointer

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
  writeStyle a idx StyleScrollContentW 0
  writeStyle a idx StyleNodeValue 0
  writeStyle a idx StyleGridMinColW layoutGridMinColW
  writeStyle a idx StyleFontSize layoutFontSize
  writeStyle a idx StyleLineGap lineGap
  writeStyle a idx StylePinX pinX
  writeStyle a idx StylePinY pinY
  writeStyle a idx StyleAspect layoutAspect

  writeTagEnum a idx TagNodeType nt
  writeTagEnum a idx TagDirection $ case layoutDirection of
    Row | flow /= Layered -> DirRow
    _ -> DirColumn
  writeTagEnum a idx TagWSizing wTag
  writeTagEnum a idx TagHSizing hTag
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagScrollBarSlot) 0
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagAlignX) axByte
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagAlignY) ayByte
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagIdSuperseded) 0
  writeTagEnum a idx TagFlow flow
  writeTagEnum a idx TagPinned pinned
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagPinnedBelow) 0
  writeTagEnum a idx TagPointer pointerMode
  writeTagEnum a idx TagLineAlign layoutLineAlign
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagScrollerBelow) 0
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagSteadyHeight) 0
  writeTagEnum a idx TagItemsAlign layoutItemsAlign
  writeTagEnum a idx TagJustify layoutJustify
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagHasOptions) 0
  writePrimArray (naArrTags a) (idx * tagStride + fromEnum TagBaselineChild) 0

  writePrimArray (naArrStyleIdx a) idx 0
  writeTree a idx TreeParent parent
  writeTree a idx TreeFirstChild (-1)
  writeTree a idx TreeNextSibling (-1)
  writeTree a idx TreeChildCount 0
  writeTree a idx TreeWidgetId 0
  writeTree a idx TreeTextIdx (-1)
  writeTree a idx TreeGridCols layoutGridCols

  -- The creation inputs from a constant seed, with no position: the node's
  -- paint hash starts here. Its own hash, the layout cache's key, adds its
  -- index and parent.
  let packW :: Word32 -> Word32 -> Word64
      packW hi lo = fromIntegral hi `shiftL` 32 .|. fromIntegral lo
      !enumsWord =
        fromIntegral (fromEnum nt)
          .|. (fromIntegral (fromEnum layoutDirection) `shiftL` 8)
          .|. (fromIntegral (fromEnum wTag) `shiftL` 16)
          .|. (fromIntegral (fromEnum hTag) `shiftL` 24)
          .|. (fromIntegral axByte `shiftL` 32)
          .|. (fromIntegral ayByte `shiftL` 40)
          .|. (fromIntegral (fromEnum layoutItemsAlign) `shiftL` 48)
          .|. (fromIntegral (fromEnum layoutJustify) `shiftL` 56)
      !flagsWord =
        fromIntegral (fromEnum flow)
          .|. (fromIntegral (fromEnum pinned) `shiftL` 8)
          .|. (fromIntegral (fromEnum layoutLineAlign) `shiftL` 16)
      !content =
        let !h1 = mixTagged 0x6E616E6F2D7569 1 enumsWord
            !h2 = mixTagged h1 2 flagsWord
            !h3 = mixTagged h2 3 (packF2 wVal hVal)
            !h4 = mixTagged h3 4 (packF2 (padL pad) (padR pad))
            !h5 = mixTagged h4 5 (packF2 (padT pad) (padB pad))
            !h6 = mixTagged h5 6 (packF2 layoutGap lineGap)
            !h7 = mixTagged h6 7 (packF2 layoutMinW layoutMinH)
            !h8 = mixTagged h7 8 (packF2 layoutMaxW layoutMaxH)
            !h9 = mixTagged h8 9 (packW (fromIntegral layoutGridCols) (castFloatToWord32 layoutGridMinColW))
            !h10 = mixTagged h9 10 (packF2 layoutFontSize layoutAspect)
         in mixTagged h10 11 (packF2 pinX pinY)
      !nodeSig = mixTagged (mixTagged content 0x4958 (fromIntegral idx)) 0x5041 (fromIntegral (parent + 1))
  mixInputSig na 0x4e4f nodeSig
  writePrimArray (naArrOwnHash a) idx nodeSig
  scope <- readPrimVar (naScope na)
  writePrimArray (naArrScope a) idx scope
  -- Bit 1 marks inert (input-only) scopes, which have no visual effect and
  -- must not trigger paint-scope invalidation.
  let !paintScope = scope .&. complement 2
  writePrimArray (naArrPaintHash a) idx (mixTagged content 0x5343 (fromIntegral paintScope))

  -- The font colour is paint state, so the input signature leaves it out.
  case layoutFontColor of
    Nothing -> writePrimArray (naArrFontColor a) idx 0
    Just (Color w) -> do
      writePrimArray (naArrFontColor a) idx (0x100000000 .|. fromIntegral w)
      mixPaintSig na idx 0x4643 (fromIntegral w)
  when (paintScope /= 0) $ do
    sig <- readPrimVar (naScopeSig na)
    writePrimVar (naScopeSig na) $! (sig * 0x100000001b3) `xor` (fromIntegral idx `shiftL` 32 .|. fromIntegral paintScope)

  when (parent >= 0) $ do
    fc <- readTree a parent TreeFirstChild
    writeTree a idx TreeNextSibling fc
    writeTree a parent TreeFirstChild idx
    cc <- readTree a parent TreeChildCount
    writeTree a parent TreeChildCount (cc + 1)
    when pinned $ markAncestorTag a TagPinnedBelow parent
    when (not pinned && not (isFloatingNode nt) && layoutAlignY == AlignBaseline) $
      writeTagEnum a parent TagBaselineChild True
  when (isFloatingNode nt) $ pushClassNode na a FloatingNodes idx
  when (nt == NodePanel || nt == NodeScrollContainer) $ pushClassNode na a BackdropNodes idx
  when (flow == Layered || pinned) $ pushClassNode na a LayeredNodes idx
  when (isWidgetNode nt || isScrollNode nt || pointerMode == PointerBlock) $ do
    pushClassNode na a PointerNodes idx
    when (isScrollNode nt || nt == NodeTextArea || pointerMode == PointerBlock) $
      markAncestorTag a TagScrollerBelow idx
  when (isScrollNode nt || nt == NodeTextArea) $ pushClassNode na a ScrollableNodes idx
  when (nt == NodeTextInput || nt == NodeTextArea) $ pushClassNode na a TextFieldNodes idx
  when (nt == NodeDrawing) $ pushClassNode na a DrawingNodes idx
  writePrimVar (naCount na) (idx + 1)
  pure idx

-- | Cross-axis stretch promotion for a flow child whose parent has
-- 'ItemsStretch': 0 for none, 1 to promote width, 2 to promote height.
{-# NOINLINE stretchAxis #-}
stretchAxis :: NodeArenaArrays -> NodeIdx -> SizingTag -> Word8 -> SizingTag -> Word8 -> IO Int
stretchAxis a parent wTag0 axByte hTag0 ayByte = do
  pFlow <- readTagEnum a parent TagFlow
  if pFlow == Layered
    then pure 0
    else do
      pDir <- readTagEnum a parent TagDirection
      pGridCols <- readTree a parent TreeGridCols
      pMinColW <- readStyle a parent StyleGridMinColW
      let !stretchY = pGridCols > 0 || pMinColW > 0 || pDir == DirRow
      pure $!
        if not stretchY && wTag0 == SizingFit && axByte == 0
          then 1
          else if stretchY && hTag0 == SizingFit && ayByte == 0
            then 2
            else 0

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
pushClassNode :: NodeArena -> NodeArenaArrays -> NodeClass -> NodeIdx -> IO ()
pushClassNode na a c idx = do
  let ci = fromEnum c
  k <- readPrimArray (naClassCounts na) ci
  cap <- readPrimVar (naCapacity na)
  writePrimArray (naArrClassNodes a) (ci * cap + k) idx
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
  let !same = old `ptrEq` txt
  h <- cachedHash (naArrTextHash a) idx same 0x54455854 txt
  unless same $ writeArray (naArrTextStore a) idx txt
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
  writePrimArray (naArrStyleIdx a) idx (fromIntegral (k + 1))
  mixNodeInput na idx 0x494d (packF2 w h)
  mixPaintSig na idx 0x4c4b (lookWord (inLook node))

-- | A hash of everything a look draws with, for 'getPaintSignature'.
lookWord :: ImageLook -> Word64
lookWord look =
  let !h1 =
        mixTagged 0 1 $
          fromIntegral (fromEnum (lookFit look))
            .|. (fromIntegral (fromEnum (lookAlignX look)) `shiftL` 8)
            .|. (fromIntegral (fromEnum (lookAlignY look)) `shiftL` 16)
            .|. maybe 0 (const 0x1000000) crop
      !h2 = mixTagged h1 2 (packF2 (lookOpacity look) (lookScale look))
      !h3 =
        mixTagged h2 3 $ case lookRotation look of
          RotateFloating r -> fromIntegral (castFloatToWord32 r)
          RotateSolid r -> 0x100000000 .|. fromIntegral (castFloatToWord32 r)
      !h4 = mixTagged h3 4 (maybe 0 (\(Rect x y _ _) -> packF2 x y) crop)
      !h5 = mixTagged h4 5 (maybe 0 (\(Rect _ _ w h) -> packF2 w h) crop)
      Color tintWord = lookTint look
   in mixTagged h5 6 (fromIntegral tintWord)
  where
    crop = lookCrop look

{-# INLINE packF2 #-}
packF2 :: Float -> Float -> Word64
packF2 x y = fromIntegral (castFloatToWord32 x) `shiftL` 32 .|. fromIntegral (castFloatToWord32 y)

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
  let !same = old `ptrEq` opts
  h <- cachedHash (naArrOptionsHash a) idx same 0x4f505453 opts
  writeTagEnum a idx TagHasOptions True
  unless same $ do
    writeArray (naArrOptionsStore a) idx opts
    writeArray (naArrOptionsLast a) idx opts
  mixNodeInput na idx 0x4f50 h

-- | The node's value hash from @arr@ when @same@ says the value is the
-- object last hashed there, else a fresh hash of @x@, salted and cached.
{-# INLINE cachedHash #-}
cachedHash :: Hashable a => IOArr Word64 -> NodeIdx -> Bool -> Word64 -> a -> IO Word64
cachedHash arr idx same salt x = do
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
    !ep <- readPrimVar (naEpoch na)
    prev <- indexWidgetId na ep w idx
    when (prev >= 0 && prev /= idx) $ writeTagEnum a prev TagIdSuperseded True

-- | Node most recently indexed under this id in the current frame. Returns
-- 'Nothing' for zero, an unknown id, or an entry from an earlier frame.
{-# INLINE lookupNodeByWidgetId #-}
lookupNodeByWidgetId :: NodeArena -> WidgetId -> IO (Maybe NodeIdx)
lookupNodeByWidgetId na (WidgetId key)
  | key == 0 = pure Nothing
  | otherwise = do
      !ep <- readPrimVar (naEpoch na)
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

-- | Mix extra paint-only state (such as a shader hash and packed uniform hash)
-- into node @idx@'s paint hash and the arena's frame paint signature.
{-# INLINE mixNodePaintHash #-}
mixNodePaintHash :: NodeArena -> NodeIdx -> Word64 -> IO ()
mixNodePaintHash na idx h = mixPaintSig na idx 0x5348 h

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

-- | Compute every node's restore key ('naArrSubHash'). A first pass, parents
-- before children, parks each node's ancestor chain (its ancestors' own
-- hashes) in its slot. A second pass, from the last node down so each child's
-- key is final before its parent reads it, folds the node's own hash and chain
-- with its children's keys. Child positions mix in, so reordering children
-- counts as a change. Run after the view has built and before the solve.
computeSubtreeHashes :: NodeArena -> IO ()
computeSubtreeHashes na = do
  n <- arenaCount na
  a <- arenaArrays na
  let own = naArrOwnHash a
      sub = naArrSubHash a
      chain !i
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

-- | Store a style code encoded for this node's type. Part of the layout
-- input signature, except on box, image, and drawing nodes: their style code
-- is paint data (a colour, a version), which goes in the paint signature
-- ('getPaintSignature') instead.
{-# INLINE setStyleIdx #-}
setStyleIdx :: NodeArena -> NodeIdx -> Word64 -> IO ()
setStyleIdx na idx v = do
  a <- arenaArrays na
  writePrimArray (naArrStyleIdx a) idx v
  nt <- readTagEnum a idx TagNodeType
  if nt == NodeBox || nt == NodeImage || nt == NodeDrawing || nt == NodeShader
    then mixPaintSig na idx 0x5354 v
    else mixNodeInput na idx 0x5354 v

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
  s@(AxisSnapshot idxArr _) <- readArray levels depth
  cap <- getSizeofMutablePrimArray idxArr
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
memoizeWidth :: NodeArena -> WidthMemo -> NodeIdx -> Float -> IO (Float, Float) -> IO (Float, Float)
memoizeWidth na (WidthMemo tags slots) idx key compute = do
  ft <- readPrimVar (naFrameTag na)
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
