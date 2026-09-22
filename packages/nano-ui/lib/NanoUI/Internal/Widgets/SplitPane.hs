-- | Pure pane-grid tree model and geometry, modelled on iced's @PaneGrid@.
--
-- A 'GridNode' is a binary split tree of panes. Each split stores an axis
-- ('AxisV' = vertical divider splitting width, 'AxisH' = horizontal divider
-- splitting height), a ratio in @[0,1]@ for the first (A) side, and the two
-- child subtrees. Every pane and split has a globally unique 'Word64' id so
-- pane state can be keyed by pane id regardless of position in the tree.
--
-- All functions here are pure; the interactive wrapper in
-- "NanoUI.Widgets.PaneGrid" persists a 'GridNode' as a "Data.Dynamic" value
-- in the widget store.
module NanoUI.Internal.Widgets.SplitPane
  ( GridAxis (..)
  , GridNode (..)
  , PaneDrop (..)
  , treePanes
  , treeSize
  , treeMaxId
  , paneExist
  , subtreeMin
  , alongAxis
  , mainLen
  , layoutNode
  , DividerInfo (..)
  , dividerLength
  , clampRatio
  , treeSplit
  , treeSetRatio
  , pinnedSide
  , reflowFixed
  , treeRemovePane
  , DropPreview (..)
  , dropPreviewTreeSized
  , dropTargetForPane
  , nearestPane
  , bestPane
  , topLevelDropTarget
  ) where

import Control.Applicative ((<|>))
import Data.List (find, sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Word (Word64)
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp, clamp01, rectH, rectHit, rectNonEmpty, rectW, rectX, rectY)

-- | Divider orientation. 'AxisV' draws a vertical divider (panes left/right),
-- 'AxisH' draws a horizontal divider (panes stacked top/bottom).
data GridAxis = AxisV | AxisH
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Binary split tree node. Pane and split ids share one monotonic counter.
-- Positional (non-record) so the multi-constructor type keeps total fields.
data GridNode
  = Split
      !Word64
      -- ^ Split id.
      !GridAxis
      -- ^ Orientation of the divider.
      !Float
      -- ^ Ratio in @[0,1]@ for the A side.
      !GridNode
      -- ^ Left / top subtree.
      !GridNode
      -- ^ Right / bottom subtree.
  | Pane
      !Word64
      -- ^ Pane id.
  deriving (Eq, Show)

-- | Result of dropping a dragged pane on a target pane.
data PaneDrop
  = DropSwap Word64
      -- ^ Drop on the center of the pane: the two panes swap places.
  | DropSplit Word64 GridAxis Bool
      -- ^ Drop near an edge: the target pane splits along the axis and the
      -- dragged pane moves into the new child. 'True' puts the dragged pane on
      -- the A (left/top) side, 'False' on the B (right/bottom) side.
  | DropTop GridAxis Bool
      -- ^ Drop on the outer edge of the whole grid: the entire tree is wrapped
      -- in a new top-level split and the dragged pane takes one side, so the
      -- rest of the grid collapses onto the other. 'True' puts the dragged
      -- pane on the A (left/top) side, 'False' on the B (right/bottom) side.
  deriving (Eq, Show)

-- | Fold a tree bottom-up: @onPane@ for each pane id, @onSplit@ for each
-- split (id, axis, ratio) with its already-folded A and B sides. The sides
-- are passed lazily, so a short-circuiting @onSplit@ stops early.
foldGrid :: (Word64 -> r) -> (Word64 -> GridAxis -> Float -> r -> r -> r) -> GridNode -> r
foldGrid onPane onSplit = go
  where
    go (Pane pid) = onPane pid
    go (Split sid axis ratio a b) = onSplit sid axis ratio (go a) (go b)

-- | Pane ids in the tree (depth-first, A then B).
treePanes :: GridNode -> [Word64]
treePanes = foldGrid pure (\_ _ _ a b -> a <> b)

-- | Number of panes.
treeSize :: GridNode -> Int
treeSize = foldGrid (const 1) (\_ _ _ a b -> a + b)

-- | The largest pane or split id in the tree.
treeMaxId :: GridNode -> Word64
treeMaxId = foldGrid id (\sid _ _ a b -> maximum [sid, a, b])

-- | Does a pane with the given id exist?
paneExist :: GridNode -> Word64 -> Bool
paneExist t p = foldGrid (== p) (\_ _ _ a b -> a || b) t

-- | Minimum (width, height) that must be reserved for a subtree under a
-- @minSize@ per-pane floor and @spacing@ between every split level.
subtreeMin :: Float -> Float -> GridNode -> (Float, Float)
subtreeMin minSize spacing = \case
  Pane _ -> (minSize, minSize)
  Split _ axis _ a b ->
    let (wa, ha) = subtreeMin minSize spacing a
        (wb, hb) = subtreeMin minSize spacing b
     in case axis of
          AxisV -> (wa + spacing + wb, max ha hb)
          AxisH -> (max wa wb, ha + spacing + hb)

-- | A rect seen along a split's main axis: as it is for 'AxisV', with x and y
-- (and width and height) swapped for 'AxisH', so per-axis geometry is written
-- once, for a vertical divider. Its own inverse.
alongAxis :: GridAxis -> Rect -> Rect
alongAxis AxisV r = r
alongAxis AxisH (Rect x y w h) = Rect y x h w

-- | Extent of a region along a split's main axis.
mainLen :: GridAxis -> Rect -> Float
mainLen ax = rectW . alongAxis ax

-- | The minima of a split's two subtrees along its main axis: widths for
-- 'AxisV' (panes left/right), heights for 'AxisH' (panes stacked).
splitMins :: Float -> Float -> GridAxis -> GridNode -> GridNode -> (Float, Float)
splitMins minSize spacing axis a b = (along a, along b)
  where
    along n = (if axis == AxisV then fst else snd) (subtreeMin minSize spacing n)

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
   in ( alongAxis ax (Rect x y d h)
      , alongAxis ax (Rect (x + d + spacing) y (w - d - spacing) h)
      , alongAxis ax (Rect (x + d) y spacing h)
      )

-- | Per-split divider information: the split's own region (where the ratio
-- applies), the exact spacing band, the axis / ratio / id, and the minima
-- its two sides keep along the axis.
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
layoutNode minSize spacing sp r =
  case sp of
    Pane pid -> (M.singleton pid r, [])
    Split sid axis ratio0 a b ->
      let mins = splitMins minSize spacing axis a b
          (rA, rB, band) = splitBounds axis spacing r (uncurry (splitLength spacing (mainLen axis r)) mins ratio0)
          self = DividerInfo sid axis r band ratio0 mins
          (regionsA, divsA) = layoutNode minSize spacing a rA
          (regionsB, divsB) = layoutNode minSize spacing b rB
       in (M.union regionsA regionsB, self : divsA <> divsB)

-- | Split the pane (first arg) along the axis with a 0.5 ratio, inserting the
-- new pane. @newOnA@ places the new pane on the A (left/top) side of the new
-- split; 'False' puts it on the B (right/bottom) side. Returns the updated
-- tree (unchanged if the pane does not exist).
treeSplit :: Word64 -> Word64 -> GridAxis -> Bool -> Word64 -> GridNode -> GridNode
treeSplit targetPaneId splitId axis newOnA newPaneId = foldGrid onPane Split
  where
    onPane p
      | p /= targetPaneId = Pane p
      | otherwise = splitBeside splitId axis newOnA newPaneId (Pane p)

-- | A new even split of @node@ and the pane @newPane@, on the A side when
-- @newOnA@.
splitBeside :: Word64 -> GridAxis -> Bool -> Word64 -> GridNode -> GridNode
splitBeside splitId axis newOnA newPane node
  | newOnA = Split splitId axis 0.5 (Pane newPane) node
  | otherwise = Split splitId axis 0.5 node (Pane newPane)

-- | Set the raw ratio of a split (clamped to @[0,1]@).
treeSetRatio :: Word64 -> Float -> GridNode -> GridNode
treeSetRatio splitId r =
  foldGrid Pane (\sid ax r0 -> Split sid ax (if sid == splitId then clamp01 r else r0))

-- | Does this side of a split hold a pinned pane of its own? Only an
-- immediate 'Pane' counts. A pin fixes a pane's extent along the axis of the
-- split the pane hangs directly off, and every split above that one keeps
-- sharing its region out by ratio, so pinning a sidebar's width leaves the
-- height of whatever row it sits in free, and a second pinned pane deeper in
-- the tree is a matter for its own split rather than one that cancels the
-- first out.
pinnedSide :: (Word64 -> Bool) -> GridNode -> Bool
pinnedSide isFixed = \case
  Pane p -> isFixed p
  Split{} -> False

-- | The A-side ratio that gives a split of this region the extent @d@, after
-- the subtree minima have had their say: the inverse of 'splitLength', and
-- already clamped, so a region with no room for the extent leaves a ratio
-- that says what the split really does rather than one that would spring the
-- side open the moment the pin came off. Zero for a region with no room
-- beside the gutter, where an extent says nothing about a share.
lengthRatio :: Float -> Float -> Float -> Float -> Float -> Float
lengthRatio spacing avail minA minB d
  | usable <= 0 = 0
  | otherwise = clamp01 (splitLength spacing avail minA minB (d / usable) / usable)
  where
    usable = avail - spacing

-- | Re-ratio a tree for a region that changed size, so that the panes the
-- predicate picks keep their extent along their parent split's axis. The
-- space the change adds or takes is charged to the other side of the split a
-- pinned pane hangs directly off ('pinnedSide'); every other split keeps its
-- ratio and so keeps sharing its region out as it did, which is what carries
-- the difference down to a pinned pane nested deeper.
--
-- A split with a pinned pane on both sides, or on neither, has no one side to
-- charge the difference to and is left to its ratio. So is a split whose old
-- or new region has no room beside the gutter.
--
-- The result is still an ordinary ratio tree. A pinned pane gives way after
-- all once the region is too small to hold it and its neighbours' minima,
-- and, having given way, is pinned at the extent it gave way to: an extent is
-- all the tree remembers, so a region that grows back does not know what the
-- pane was pinned at before it had to shrink.
reflowFixed :: (Word64 -> Bool) -> Float -> Float -> Rect -> Rect -> GridNode -> GridNode
reflowFixed isFixed minSize spacing = go
  where
    go _ _ n@(Pane _) = n
    go oldR newR (Split sid axis ratio a b) =
      Split sid axis ratio' (go oldA newA a) (go oldB newB b)
      where
        (mA, mB) = splitMins minSize spacing axis a b
        oldAvail = mainLen axis oldR
        newAvail = mainLen axis newR
        dOld = splitLength spacing oldAvail mA mB ratio
        fixedA = pinnedSide isFixed a
        fixedB = pinnedSide isFixed b
        -- The A-side extent the reflow asks for: the one it had when A is the
        -- pinned side, and the one that leaves B the extent it had when B is.
        --
        -- The pinned side is kept to a whole unit. A reflow that runs on
        -- every frame of a resize drag would otherwise walk it a fraction at
        -- a time: the extent is kept as a ratio of a region that is changing
        -- size, and going out to a ratio and back again does not land on
        -- quite the same number twice. It is the pinned side's own extent
        -- that is rounded, not A's: a B side kept as what A leaves over would
        -- take every step's rounding of A with it, and a region that grows
        -- by a fraction of a unit a step walks it off by that much each time.
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

-- | Remove a pane. The sibling subtree absorbs its space. @Nothing@ if the
-- pane does not exist or removing it would empty the tree.
treeRemovePane :: Word64 -> GridNode -> Maybe GridNode
treeRemovePane pid = foldGrid onPane onSplit
  where
    onPane p = if p == pid then Nothing else Just (Pane p)
    onSplit sid ax r0 ma mb = liftA2 (Split sid ax r0) ma mb <|> ma <|> mb

-- | Swap two panes by id (content follows the pane id).
treeSwapPanes :: Word64 -> Word64 -> GridNode -> GridNode
treeSwapPanes a b = foldGrid (\p -> Pane (if p == a then b else if p == b then a else p)) Split

-- | Move a pane onto a drop target. Center drops swap the two panes; edge
-- drops split the target pane with the given fresh split id and move the
-- dragged pane into the new child; top-level drops wrap the whole tree in a
-- new root split with the dragged pane on one side.
treeMovePane :: Word64 -> Word64 -> PaneDrop -> GridNode -> Maybe GridNode
treeMovePane moved splitId dt tree
  | not (paneExist tree moved) = Nothing
  | otherwise =
      case dt of
        DropSwap tgt -> onTarget tgt (Just (treeSwapPanes moved tgt tree))
        DropSplit tgt axis onA -> onTarget tgt (treeSplit tgt splitId axis onA moved <$> treeRemovePane moved tree)
        -- Removing the only pane leaves nothing to wrap.
        DropTop axis onA -> splitBeside splitId axis onA moved <$> treeRemovePane moved tree
  where
    -- A drop onto a pane needs another pane of the tree.
    onTarget tgt r = if tgt /= moved && paneExist tree tgt then r else Nothing

-- | Classify a drop point on a target pane into the 'PaneDrop' the drop
-- performs: the pane's center swaps the two panes, an edge zone splits the
-- target along that edge's axis with the dragged pane on the near side.
dropTargetForPane :: Rect -> V2 -> Word64 -> PaneDrop
dropTargetForPane r (V2 mx my) tgt
  | not (rectNonEmpty r) = DropSwap tgt
  | tx < 0.25 = DropSplit tgt AxisV True
  | tx > 0.75 = DropSplit tgt AxisV False
  | ty < 0.25 = DropSplit tgt AxisH True
  | ty > 0.75 = DropSplit tgt AxisH False
  | otherwise = DropSwap tgt
  where
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
  | not (rectHit r p) = Nothing
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

-- | Drop preview for a drop target. The rect to highlight ('dpRect') comes
-- from simulating the drop ('treeMovePane') and laying the resulting tree out ('layoutNode') into the grid rect, so it is exactly the region the
-- dragged pane will occupy after the drop, accounting for the restructuring
-- that removing the pane causes (its parent split collapses and sibling
-- subtrees expand) and for @spacing@ and min-size floors. Estimating the rect
-- from the target's pre-drop bounds goes wrong wherever mixed 'AxisV' /
-- 'AxisH' splits make those two layouts diverge. @spacing@ must be the gutter
-- actually laid out between panes: 'NanoUI.Widgets.PaneGrid' passes
-- @pgSpacing + 2 * pgLeeway@, not @pgSpacing@, or the preview regions drift
-- from the on-screen layout. 'Nothing' when the drop cannot be performed
-- (unknown pane ids, 'DropTop' on a single-pane grid).
--
-- The rect is only meaningful inside the post-drop layout: the other panes
-- move too (a swap sends the target to the dragged pane's old slot, a
-- top-level drop squeezes the whole grid into one half), so a caller that
-- highlights it should draw the rest of the grid from 'dpTree' as well,
-- not from the pre-drop tree.
--
-- The drop's new split, if any, takes @splitId@, so passing the id the real
-- drop will use keeps the split's identity across the drop. With a @source@
-- rect the source pane keeps its extent along its parent split's axis,
-- transferred to the destination axis, so a thin left/right pane stays thin
-- when moved to the top/bottom. The requested size is clamped to the
-- destination's subtree minima, and center swaps ignore it. Pass the source
-- rect from the committed layout, never the preview layout.
dropPreviewTreeSized :: Maybe Rect -> Float -> Float -> GridNode -> Word64 -> Word64 -> Rect -> PaneDrop -> Maybe DropPreview
dropPreviewTreeSized source minSize spacing tree moved splitId baseRect dt = do
  t' <- treeMovePane moved splitId dt tree
  let sized = case (source, dt) of
        (Just r, DropSplit _ axis onA) -> retain r axis onA t'
        (Just r, DropTop axis onA) -> retain r axis onA t'
        _ -> t'
      sourceAxis (Pane _) = Nothing
      sourceAxis (Split _ axis _ a b)
        | a == Pane moved || b == Pane moved = Just axis
        | otherwise = sourceAxis a <|> sourceAxis b
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
