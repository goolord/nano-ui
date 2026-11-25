-- | Pure tree model and geometry for "NanoUI.Widgets.PaneGrid". A
-- 'GridNode' is a binary split tree. Each split has an axis, a ratio in
-- @[0,1]@ for its first (A) side, and two subtrees. Panes and splits have
-- unique 'Word64' ids, so pane state stays keyed to a pane as it moves.
module NanoUI.Internal.Widgets.SplitPane
  ( GridAxis (..)
  , CenterDropMode (..)
  , GridNode (..)
  , PaneDrop (..)
  , treePanes
  , treeSize
  , treeMaxId
  , paneExist
  , MinTree (..)
  , buildMinTree
  , minTreeDims
  , splitMinChildren
  , alongAxis
  , mainLen
  , layoutNode
  , DividerInfo (..)
  , dividerLength
  , clampRatio
  , treeSplit
  , treeAddTab
  , treeSelectTab
  , treeSetRatio
  , pinnedSide
  , reflowFixed
  , treeRemovePane
  , DropPreview (..)
  , dropPreviewTreeSized
  , dropTargetForPane
  , dropTargetForPaneWith
  , nearestPane
  , bestPane
  , topLevelDropTarget
  ) where

import Control.Applicative ((<|>))
import Data.List (elemIndex, find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Word (Word64)
import NanoUI.Internal.GridNode (CenterDropMode (..), GridAxis (..), GridNode (..))
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp, clamp01, rectContains, rectH, rectNonEmpty, rectW, rectX, rectY)

-- | Result of dropping a dragged pane on a target pane.
data PaneDrop
  = DropSwap Word64
      -- ^ Drop on the center of the pane: the two panes swap places.
  | DropTab Word64 (Maybe Int)
      -- ^ Drop on a pane center (when 'CenterTab' is enabled) or tab bar:
      -- merge the dragged pane into the target's 'PaneTabs' stack at an optional
      -- index (or at the end when 'Nothing').
  | DropSplit Word64 GridAxis Bool
      -- ^ Drop near an edge: the target pane splits along the axis and the
      -- dragged pane moves into the new child. 'True' puts the dragged pane on
      -- the A (left/top) side, 'False' on the B (right/bottom) side.
  | DropTop GridAxis Bool
      -- ^ Drop on the grid's outer edge: the whole tree is wrapped in a new
      -- top-level split. 'True' puts the dragged pane on the A side.
  deriving (Eq, Show)

forceWords :: [Word64] -> [Word64]
forceWords [] = []
forceWords (!x : xs) = let !rest = forceWords xs in x : rest

normalizeTabs :: Word64 -> [Word64] -> Word64 -> Maybe GridNode
normalizeTabs gid pids0 active =
  case forceWords pids0 of
    [] -> Nothing
    [only] -> Just (Pane only)
    pids@(first : _) ->
      let !act = if elem active pids then active else first
       in Just (PaneTabs gid pids act)

-- | Fold a tree bottom-up: @onPane@ for each pane leaf, @onTabs@ for each
-- tabbed stack, and @onSplit@ for each split (id, axis, ratio) with its
-- already-folded A and B sides.
foldGrid ::
  (Word64 -> r) ->
  (Word64 -> [Word64] -> Word64 -> r) ->
  (Word64 -> GridAxis -> Float -> r -> r -> r) ->
  GridNode ->
  r
foldGrid onPane onTabs onSplit = go
  where
    go (Pane pid) = onPane pid
    go (PaneTabs gid pids active) = onTabs gid pids active
    go (Split sid axis ratio a b) = onSplit sid axis ratio (go a) (go b)

-- | Pane ids in the tree (depth-first, A then B, and tab-order within 'PaneTabs').
treePanes :: GridNode -> [Word64]
treePanes = foldGrid pure (\_ pids _ -> pids) (\_ _ _ a b -> a <> b)

-- | Number of panes.
treeSize :: GridNode -> Int
treeSize = foldGrid (const 1) (\_ pids _ -> length pids) (\_ _ _ a b -> a + b)

-- | The largest pane, tab-group, or split id in the tree.
treeMaxId :: GridNode -> Word64
treeMaxId = foldGrid id (\gid pids _ -> foldl' max gid pids) (\sid _ _ a b -> maximum [sid, a, b])

-- | Does a pane with the given id exist?
paneExist :: GridNode -> Word64 -> Bool
paneExist t p = foldGrid (== p) (\_ pids _ -> elem p pids) (\_ _ _ a b -> a || b) t

-- | Cached bottom-up subtree minimum extents so tree traversals read each
-- split's child minima in O(1).
data MinTree
  = MinLeaf {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  | MinBranch {-# UNPACK #-} !Float {-# UNPACK #-} !Float !MinTree !MinTree

-- | Total minimum @(width, height)@ of a 'MinTree'.
{-# INLINE minTreeDims #-}
minTreeDims :: MinTree -> (Float, Float)
minTreeDims (MinLeaf w h) = (w, h)
minTreeDims (MinBranch w h _ _) = (w, h)

-- | Extract the child minimum trees for a 'Split' node, rebuilding them if the
-- supplied 'MinTree' does not match the branch shape.
{-# INLINE splitMinChildren #-}
splitMinChildren :: Float -> Float -> GridNode -> GridNode -> MinTree -> (MinTree, MinTree)
splitMinChildren _ _ _ _ (MinBranch _ _ l r) = (l, r)
splitMinChildren minSize spacing a b _ = (buildMinTree minSize spacing a, buildMinTree minSize spacing b)

-- | Compute the minimum @(width, height)@ for every node of @GridNode@ in a
-- single O(P) bottom-up pass.
buildMinTree :: Float -> Float -> GridNode -> MinTree
buildMinTree minSize spacing = go
  where
    go (Pane _) = MinLeaf minSize minSize
    go (PaneTabs _ _ _) = MinLeaf minSize minSize
    go (Split _ axis _ a b) =
      let ma = go a
          mb = go b
          (wa, ha) = minTreeDims ma
          (wb, hb) = minTreeDims mb
          (w, h) = case axis of
            AxisV -> (wa + spacing + wb, max ha hb)
            AxisH -> (max wa wb, ha + spacing + hb)
       in MinBranch w h ma mb

-- | A rect seen along a split's main axis: as it is for 'AxisV', with x and y
-- (and width and height) swapped for 'AxisH', so per-axis geometry is written
-- once, for a vertical divider. Its own inverse.
alongAxis :: GridAxis -> Rect -> Rect
alongAxis AxisV r = r
alongAxis AxisH (Rect x y w h) = Rect y x h w

-- | Extent of a region along a split's main axis.
mainLen :: GridAxis -> Rect -> Float
mainLen ax = rectW . alongAxis ax

-- | The minima of two cached subtrees along @axis@.
splitMinsTree :: GridAxis -> MinTree -> MinTree -> (Float, Float)
splitMinsTree axis ma mb =
  let (wa, ha) = minTreeDims ma
      (wb, hb) = minTreeDims mb
   in case axis of
        AxisV -> (wa, wb)
        AxisH -> (ha, hb)

-- | A-side extent for a split along its main axis, honouring the subtree
-- minima. The ratio shares out the extent left after the gutter between the
-- sides, so a 0.5 split gives both sides the same length. Falls back to the
-- raw share when the region is too small to satisfy both minima.
splitLength :: Float -> Float -> Float -> Float -> Float -> Float
splitLength spacing avail minA minB ratio
  | avail <= 0 = 0
  | lo <= hi = clamp lo hi share
  | otherwise = clamp 0 avail share
  where
    share = ratio * max 0 (avail - spacing)
    lo = minA
    hi = avail - spacing - minB

-- | Carve a region at offset @d@ along the main axis into (A, B, divider band).
splitBounds :: GridAxis -> Float -> Rect -> Float -> (Rect, Rect, Rect)
splitBounds ax spacing r d =
  let Rect x y w h = alongAxis ax r
   in ( alongAxis ax (Rect x y (max 0 d) h)
      , alongAxis ax (Rect (x + d + spacing) y (max 0 (w - d - spacing)) h)
      , alongAxis ax (Rect (x + d) y spacing h)
      )

-- | A split's divider: id, axis, ratio, the split's region, the spacing
-- band, and each side's minimum extent along the axis.
data DividerInfo = DividerInfo
  { diSplitId :: {-# UNPACK #-} !Word64
  , diAxis :: !GridAxis
  , diRegion :: !Rect
  , diBand :: !Rect
  , diRatio :: {-# UNPACK #-} !Float
  , diMins :: !(Float, Float)
  }
  deriving (Eq, Show)

-- | The A side's extent for a ratio in the divider's region ('splitLength').
dividerLength :: Float -> DividerInfo -> Float -> Float
dividerLength spacing d = uncurry (splitLength spacing (mainLen (diAxis d) (diRegion d))) (diMins d)

-- | Clamp a proposed ratio for a divider so both of its sides keep at least
-- their minimum size within its region.
clampRatio :: Float -> DividerInfo -> Float -> Float
clampRatio spacing d r0
  | usable <= 0 = r0
  | otherwise = dividerLength spacing d r0 / usable
  where
    usable = mainLen (diAxis d) (diRegion d) - spacing

-- | Lay out a tree into per-pane regions and divider bands within 'Rect'.
-- Dividers are reported parent-before-child so dragging a divider resizes its
-- immediate subtrees relative to the same region.
layoutNode :: Float -> Float -> GridNode -> Rect -> (Map Word64 Rect, [DividerInfo])
layoutNode minSize spacing root = go root (buildMinTree minSize spacing root)
  where
    go sp mt r = case sp of
      Pane pid -> (M.singleton pid r, [])
      PaneTabs _ _ active -> (M.singleton active r, [])
      Split sid axis ratio0 a b ->
        let (ma, mb) = splitMinChildren minSize spacing a b mt
            mins = splitMinsTree axis ma mb
            (rA, rB, band) = splitBounds axis spacing r (uncurry (splitLength spacing (mainLen axis r)) mins ratio0)
            self = DividerInfo sid axis r band ratio0 mins
            (regionsA, divsA) = go a ma rA
            (regionsB, divsB) = go b mb rB
         in (M.union regionsA regionsB, self : divsA <> divsB)

-- | Split the pane (first arg) along the axis with a 0.5 ratio, inserting the
-- new pane. @newOnA@ places the new pane on the A (left/top) side of the new
-- split; 'False' puts it on the B (right/bottom) side. Returns the updated
-- tree (unchanged if the pane does not exist).
treeSplit :: Word64 -> Word64 -> GridAxis -> Bool -> Word64 -> GridNode -> GridNode
treeSplit targetPaneId splitId axis newOnA newPaneId = foldGrid onPane onTabs Split
  where
    onPane p
      | p /= targetPaneId = Pane p
      | otherwise = splitBeside splitId axis newOnA newPaneId (Pane p)
    onTabs gid pids active
      | notElem targetPaneId pids = PaneTabs gid pids active
      | otherwise = splitBeside splitId axis newOnA newPaneId (PaneTabs gid pids active)

splitBeside :: Word64 -> GridAxis -> Bool -> Word64 -> GridNode -> GridNode
splitBeside splitId axis newOnA newPane node
  | newOnA = Split splitId axis 0.5 (Pane newPane) node
  | otherwise = Split splitId axis 0.5 node (Pane newPane)

-- | Merge @newPaneId@ into the 'Pane' or 'PaneTabs' stack containing @targetPaneId@,
-- using @groupId@ when promoting a single 'Pane' into 'PaneTabs', and activating
-- @newPaneId@.
treeAddTab :: Word64 -> Word64 -> Maybe Int -> Word64 -> GridNode -> GridNode
treeAddTab targetPaneId groupId mIdx newPaneId = foldGrid onPane onTabs Split
  where
    insertAt xs x = case mIdx of
      Nothing -> forceWords (xs ++ [x])
      Just i ->
        let k = clamp 0 (length xs) i
            (pre, post) = splitAt k xs
         in forceWords (pre ++ [x] ++ post)
    onPane p
      | p /= targetPaneId = Pane p
      | otherwise = PaneTabs groupId (insertAt [p] newPaneId) newPaneId
    onTabs gid pids active
      | notElem targetPaneId pids = PaneTabs gid pids active
      | otherwise =
          let clean = filter (/= newPaneId) pids
           in PaneTabs gid (insertAt clean newPaneId) newPaneId

-- | Set the active pane in whichever 'PaneTabs' stack contains @paneId@.
treeSelectTab :: Word64 -> GridNode -> GridNode
treeSelectTab paneId =
  foldGrid
    Pane
    (\gid pids active -> PaneTabs gid pids (if elem paneId pids then paneId else active))
    Split

-- | Set the raw ratio of a split (clamped to @[0,1]@).
treeSetRatio :: Word64 -> Float -> GridNode -> GridNode
treeSetRatio splitId r =
  foldGrid Pane PaneTabs (\sid ax r0 -> Split sid ax (if sid == splitId then clamp01 r else r0))

-- | Whether this side of a split is itself a pinned pane. A pin only fixes
-- a pane's extent along its direct parent split's axis.
pinnedSide :: (Word64 -> Bool) -> GridNode -> Bool
pinnedSide isFixed = \case
  Pane p -> isFixed p
  PaneTabs _ pids active -> isFixed active || any isFixed pids
  Split{} -> False

-- | The A-side ratio that gives this region's split an A extent of @d@. The
-- inverse of 'splitLength', clamped to the subtree minima so the ratio
-- matches the real split. Zero when the region has no room beside the gutter.
lengthRatio :: Float -> Float -> Float -> Float -> Float -> Float
lengthRatio spacing avail minA minB d
  | usable <= 0 = 0
  | otherwise = clamp01 (splitLength spacing avail minA minB (d / usable) / usable)
  where
    usable = avail - spacing

-- | Re-ratio a tree for a resized region so the panes the predicate selects
-- keep their extent along their parent split's axis. The other side of that
-- split ('pinnedSide') absorbs the change. All other splits keep their
-- ratio, including splits pinned on both sides or neither and splits with no
-- room beside the gutter. A pinned pane squeezed below its extent keeps the
-- smaller extent when the region grows back.
reflowFixed :: (Word64 -> Bool) -> Float -> Float -> Rect -> Rect -> GridNode -> GridNode
reflowFixed isFixed minSize spacing oldR0 newR0 root = go oldR0 newR0 root (buildMinTree minSize spacing root)
  where
    go _ _ n@(Pane _) _ = n
    go _ _ n@(PaneTabs _ _ _) _ = n
    go oldR newR (Split sid axis ratio a b) mt =
      Split sid axis ratio' (go oldA newA a ma) (go oldB newB b mb)
      where
        (ma, mb) = splitMinChildren minSize spacing a b mt
        (mA, mB) = splitMinsTree axis ma mb
        oldAvail = mainLen axis oldR
        newAvail = mainLen axis newR
        dOld = splitLength spacing oldAvail mA mB ratio
        fixedA = pinnedSide isFixed a
        fixedB = pinnedSide isFixed b
        -- A-side extent that preserves the pinned side. The pinned extent is
        -- rounded to a whole unit so per-frame reflows during a resize don't
        -- drift through ratio round trips.
        wanted
          | fixedA = whole dOld
          | otherwise = (newAvail - spacing) - whole (oldAvail - spacing - dOld)
        whole v = fromIntegral (round v :: Int)
        ratio'
          | fixedA == fixedB = ratio
          | oldAvail - spacing <= 0 || newAvail - spacing <= 0 = ratio
          | otherwise = lengthRatio spacing newAvail mA mB wanted
        (oldA, oldB, _) = splitBounds axis spacing oldR dOld
        (newA, newB, _) = splitBounds axis spacing newR (splitLength spacing newAvail mA mB ratio')

-- | Remove a pane. The sibling subtree absorbs its space. The tree comes back
-- unchanged if the pane does not exist, and @Nothing@ when removing it would
-- empty the tree.
treeRemovePane :: Word64 -> GridNode -> Maybe GridNode
treeRemovePane pid = foldGrid onPane onTabs onSplit
  where
    onPane p = if p == pid then Nothing else Just (Pane p)
    onTabs gid pids active
      | notElem pid pids = Just (PaneTabs gid pids active)
      | otherwise =
          let rest = filter (/= pid) pids
              nextAct
                | active /= pid = active
                | Just idx <- elemIndex pid pids
                , not (null rest) =
                    rest !! min (length rest - 1) idx
                | otherwise = fromMaybe active (listToMaybe rest)
           in normalizeTabs gid rest nextAct
    onSplit sid ax r0 ma mb = liftA2 (Split sid ax r0) ma mb <|> ma <|> mb

-- | Swap two panes by id (content follows the pane id).
treeSwapPanes :: Word64 -> Word64 -> GridNode -> GridNode
treeSwapPanes a b =
  let swap p = if p == a then b else if p == b then a else p
   in foldGrid (Pane . swap) (\gid pids act -> PaneTabs gid (forceWords (map swap pids)) (swap act)) Split

findTabGroup :: Word64 -> GridNode -> Maybe Word64
findTabGroup p =
  foldGrid
    (const Nothing)
    (\gid pids _ -> if elem p pids then Just gid else Nothing)
    (\_ _ _ l r -> l <|> r)

-- | Move a pane onto a drop target. Center drops swap or tab-merge the panes;
-- edge drops split the target pane with the given fresh split id and move the
-- dragged pane into the new child; top-level drops wrap the whole tree in a
-- new root split with the dragged pane on one side.
treeMovePane :: Word64 -> Word64 -> PaneDrop -> GridNode -> Maybe GridNode
treeMovePane moved splitId dt tree
  | not (paneExist tree moved) = Nothing
  | otherwise =
      case dt of
        DropSwap tgt -> onTarget tgt (Just (treeSwapPanes moved tgt tree))
        DropTab tgt mIdx ->
          let gid = fromMaybe splitId (findTabGroup tgt tree)
           in onTarget tgt (treeAddTab tgt gid mIdx moved <$> treeRemovePane moved tree)
        DropSplit tgt axis onA -> onTarget tgt (treeSplit tgt splitId axis onA moved <$> treeRemovePane moved tree)
        -- Removing the only pane leaves nothing to wrap.
        DropTop axis onA -> splitBeside splitId axis onA moved <$> treeRemovePane moved tree
  where
    -- A drop onto a pane needs another pane of the tree.
    onTarget tgt r = if tgt /= moved && paneExist tree tgt then r else Nothing

-- | Classify a drop point on a target pane into the 'PaneDrop' the drop
-- performs under 'CenterSwap'.
dropTargetForPane :: Rect -> V2 -> Word64 -> PaneDrop
dropTargetForPane = dropTargetForPaneWith CenterSwap

-- | Like 'dropTargetForPane', choosing whether a center drop swaps panes
-- ('CenterSwap') or merges them into a 'PaneTabs' stack ('CenterTab').
dropTargetForPaneWith :: CenterDropMode -> Rect -> V2 -> Word64 -> PaneDrop
dropTargetForPaneWith centerMode r (V2 mx my) tgt
  | not (rectNonEmpty r) = centerDrop
  | tx < 0.25 = DropSplit tgt AxisV True
  | tx > 0.75 = DropSplit tgt AxisV False
  | ty < 0.25 = DropSplit tgt AxisH True
  | ty > 0.75 = DropSplit tgt AxisH False
  | otherwise = centerDrop
  where
    centerDrop = case centerMode of
      CenterSwap -> DropSwap tgt
      CenterTab -> DropTab tgt Nothing
    tx = (mx - rectX r) / rectW r
    ty = (my - rectY r) / rectH r

-- | The laid-out pane whose region is closest to the point: the pane under
-- it, or across a gutter the pane on the nearer side. A pointer crossing a
-- gutter therefore always has a drop target, and 'dropTargetForPane' reads a
-- point just outside a region as that region's near edge. 'Nothing' only when
-- no pane has been laid out.
nearestPane :: Map Word64 Rect -> V2 -> Maybe (Word64, Rect)
nearestPane regions (V2 mx my) = bestPane (Just . dist) regions
  where
    dist (Rect x y w h) =
      let dx = max 0 (max (x - mx) (mx - (x + w)))
          dy = max 0 (max (y - my) (my - (y + h)))
       in dx * dx + dy * dy

-- | The laid-out pane with the least score, among those the scorer accepts;
-- the first in id order on a tie.
bestPane :: (Rect -> Maybe Float) -> Map Word64 Rect -> Maybe (Word64, Rect)
bestPane score regions =
  snd <$> listToMaybe (sortOn fst [(s, (p, r)) | (p, r) <- M.toList regions, rectNonEmpty r, Just s <- [score r]])

-- | Classify a drop point against the grid's outer boundary. If the pointer
-- sits inside the grid within @band@ px of an edge, return the 'DropTop'
-- target for that edge; otherwise 'Nothing'. Checked before pane-level drops
-- so the outermost edge always restructures the whole grid. A pointer outside
-- the grid is no target at all, so releasing there cancels the drag.
topLevelDropTarget :: Float -> Rect -> V2 -> Maybe PaneDrop
topLevelDropTarget band r@(Rect l t w h) p@(V2 x y)
  | not (rectContains r p) = Nothing
  | x <= l + band = Just (DropTop AxisV True)
  | x >= l + w - band = Just (DropTop AxisV False)
  | y <= t + band = Just (DropTop AxisH True)
  | y >= t + h - band = Just (DropTop AxisH False)
  | otherwise = Nothing

-- | A simulated drop, laid out: everything needed to draw the grid as the
-- drop will leave it without laying the tree out a second time.
data DropPreview = DropPreview
  { dpTree :: !GridNode
    -- ^ The tree the drop produces.
  , dpRegions :: !(Map Word64 Rect)
    -- ^ Its pane regions ('layoutNode'), the dragged pane's included.
  , dpDividers :: ![DividerInfo]
    -- ^ Its dividers ('layoutNode').
  , dpRect :: !Rect
    -- ^ The dragged pane's region.
  }
  deriving (Eq, Show)

-- | Preview a drop by simulating it ('treeMovePane') and laying out the
-- result ('layoutNode') in the grid rect. 'dpRect' is exactly where the
-- dragged pane lands. Other panes move too, so draw the rest of the grid
-- from 'dpTree'.
--
-- @spacing@ must be the gutter actually laid out between panes. The new
-- split, if any, gets @splitId@. With a @source@ rect (from the committed
-- layout) the pane keeps its extent along its old parent split's axis,
-- clamped to the destination's minima; center swaps ignore it. 'Nothing'
-- when the drop is impossible.
dropPreviewTreeSized :: Maybe Rect -> Float -> Float -> GridNode -> Word64 -> Word64 -> Rect -> PaneDrop -> Maybe DropPreview
dropPreviewTreeSized source minSize spacing tree moved splitId baseRect dt = do
  t' <- treeMovePane moved splitId dt tree
  let sized = case (source, dt) of
        (Just r, DropSplit _ axis onA) -> retain r axis onA t'
        (Just r, DropTop axis onA) -> retain r axis onA t'
        _ -> t'
      sourceAxis (Pane _) = Nothing
      sourceAxis (PaneTabs _ _ _) = Nothing
      sourceAxis (Split _ axis _ a b)
        | leafContains moved a || leafContains moved b = Just axis
        | otherwise = sourceAxis a <|> sourceAxis b
      leafContains p (Pane q) = p == q
      leafContains p (PaneTabs _ ps _) = elem p ps
      leafContains _ Split{} = False
      retain r axis onA t =
        case find ((== splitId) . diSplitId) (snd (layoutNode minSize spacing t baseRect)) of
          Just d
            | let usable = mainLen axis (diRegion d) - spacing
            , usable > 0 ->
                let share = mainLen (fromMaybe axis (sourceAxis tree)) r / usable
                 in treeSetRatio splitId (clampRatio spacing d (if onA then share else 1 - share)) t
          _ -> t
      (regions, dividers) = layoutNode minSize spacing sized baseRect
  r <- M.lookup moved regions
  pure (DropPreview sized regions dividers r)
