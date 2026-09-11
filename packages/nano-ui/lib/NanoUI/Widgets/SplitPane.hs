{-# LANGUAGE LambdaCase #-}

-- | Pure pane-grid tree model and geometry (iced 'PaneGrid'-style).
--
-- A 'GridNode' is a binary split tree of panes. Each split stores an axis
-- ('AxisV' = vertical divider splitting width, 'AxisH' = horizontal divider
-- splitting height), a ratio in @[0,1]@ for the first (A) side, and the two
-- child subtrees. Every pane and split has a globally unique 'Word64' id so
-- pane state can be keyed by pane id regardless of position in the tree.
--
-- All functions here are pure; the interactive wrapper in
-- 'NanoUI.Widgets.PaneGrid' persists a 'GridNode' as a 'Data.Dynamic' value
-- in the widget store.
module NanoUI.Widgets.SplitPane
  ( GridAxis (..)
  , GridNode (..)
  , DropTarget (..)
  , treePanes
  , treeSize
  , paneExist
  , subtreeMin
  , mainMins
  , mainLen
  , splitLength
  , layoutNode
  , DividerInfo (..)
  , treeSplit
  , treeSetRatio
  , treeRemovePane
  , treeMovePane
  , clampTreeRatio
  , dropPreview
  , dropTargetForPane
  , topLevelDropTarget
  ) where

import Control.Applicative ((<|>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import NanoUI.Types (Rect (..), V2 (..), clamp, clamp01, rectH, rectW, rectX, rectY, v2X, v2Y)

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
data DropTarget
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

-- | Pane ids in the tree (depth-first, A then B).
treePanes :: GridNode -> [Word64]
treePanes = \case
  Pane pid -> [pid]
  Split _ _ _ a b -> treePanes a <> treePanes b

-- | Number of panes.
treeSize :: GridNode -> Int
treeSize = \case
  Pane _ -> 1
  Split _ _ _ a b -> treeSize a + treeSize b

-- | Does a pane with the given id exist?
paneExist :: GridNode -> Word64 -> Bool
paneExist t p = go t
  where
    go (Pane pid) = pid == p
    go (Split _ _ _ a b) = go a || go b

-- | Minimum (width, height) that must be reserved for a subtree under a
-- 'minSize' per-pane floor and 'spacing' between every split level.
subtreeMin :: Float -> Float -> GridNode -> (Float, Float)
subtreeMin minSize spacing = \case
  Pane _ -> (minSize, minSize)
  Split _ axis _ a b ->
    let (wa, ha) = subtreeMin minSize spacing a
        (wb, hb) = subtreeMin minSize spacing b
     in case axis of
          AxisV -> (wa + spacing + wb, max ha hb)
          AxisH -> (max wa wb, ha + spacing + hb)

-- | Extent of a region along a split's main axis.
mainLen :: GridAxis -> Rect -> Float
mainLen AxisV = rectW
mainLen AxisH = rectH

-- | The subtree minima that apply along a split's main axis: widths for
-- 'AxisV' (panes left/right), heights for 'AxisH' (panes stacked).
mainMins :: GridAxis -> (Float, Float) -> (Float, Float) -> (Float, Float)
mainMins AxisV (wa, _) (wb, _) = (wa, wb)
mainMins AxisH (_, ha) (_, hb) = (ha, hb)

-- | A-side extent for a split along its main axis, honouring the subtree
-- minima and the gutter between the sides. Falls back to the raw ratio when
-- the region is too small to satisfy both minima.
splitLength :: Float -> Float -> Float -> Float -> Float -> Float
splitLength spacing avail minA minB ratio
  | avail <= 0 = 0
  | lo <= hi = clamp lo hi (ratio * avail)
  | otherwise = clamp 0 avail (ratio * avail)
  where
    lo = minA
    hi = avail - spacing - minB

-- | Carve a region at offset @d@ along the main axis into (A, B, divider band).
splitBounds :: GridAxis -> Float -> Rect -> Float -> (Rect, Rect, Rect)
splitBounds AxisV spacing r d =
  let avail = rectW r
   in ( r {rectW = d}
      , r {rectX = rectX r + d + spacing, rectW = avail - d - spacing}
      , Rect (rectX r + d) (rectY r) spacing (rectH r)
      )
splitBounds AxisH spacing r d =
  let avail = rectH r
   in ( r {rectH = d}
      , r {rectY = rectY r + d + spacing, rectH = avail - d - spacing}
      , Rect (rectX r) (rectY r + d) (rectW r) spacing
      )

-- | Per-split divider information: the split's own region (where the ratio
-- applies), the exact spacing band, and the axis / ratio / id.
data DividerInfo = DividerInfo
  { diSplitId :: {-# UNPACK #-} !Word64
  , diAxis :: !GridAxis
  , diRegion :: !Rect
  , diBand :: !Rect
  , diRatio :: {-# UNPACK #-} !Float
  }
  deriving (Eq, Show)

-- | Lay out a tree into per-pane regions and divider bands within 'Rect'.
-- Dividers are reported parent-before-child so dragging a divider resizes its
-- immediate subtrees relative to the same region.
layoutNode :: Float -> Float -> GridNode -> Rect -> (Map Word64 Rect, [DividerInfo])
layoutNode minSize spacing sp r =
  case sp of
    Pane pid -> (M.singleton pid r, [])
    Split sid axis ratio0 a b ->
      let (wa, ha) = subtreeMin minSize spacing a
          (wb, hb) = subtreeMin minSize spacing b
          (mA, mB) = mainMins axis (wa, ha) (wb, hb)
          (rA, rB, band) = splitBounds axis spacing r (splitLength spacing (mainLen axis r) mA mB ratio0)
          self = DividerInfo sid axis r band ratio0
          (regionsA, divsA) = layoutNode minSize spacing a rA
          (regionsB, divsB) = layoutNode minSize spacing b rB
       in (M.union regionsA regionsB, self : divsA <> divsB)

-- | Split the pane (first arg) along the axis with a 0.5 ratio, inserting the
-- new pane. 'newOnA' places the new pane on the A (left/top) side of the new
-- split; 'False' puts it on the B (right/bottom) side. Returns the updated
-- tree (unchanged if the pane does not exist).
treeSplit :: Word64 -> Word64 -> GridAxis -> Bool -> Word64 -> GridNode -> GridNode
treeSplit targetPaneId splitId axis newOnA newPaneId = go
  where
    go (Pane p)
      | p == targetPaneId =
          if newOnA
            then Split splitId axis 0.5 (Pane newPaneId) (Pane p)
            else Split splitId axis 0.5 (Pane p) (Pane newPaneId)
      | otherwise = Pane p
    go (Split sid0 ax r a b) = Split sid0 ax r (go a) (go b)

-- | Set the raw ratio of a split (clamped to @[0,1]@).
treeSetRatio :: Word64 -> Float -> GridNode -> GridNode
treeSetRatio splitId r = go
  where
    go (Pane p) = Pane p
    go (Split sid0 ax r0 a b)
      | sid0 == splitId = Split sid0 ax (clamp01 r) a b
      | otherwise = Split sid0 ax r0 (go a) (go b)

-- | Remove a pane. The sibling subtree absorbs its space. @Nothing@ if the
-- pane does not exist or removing it would empty the tree.
treeRemovePane :: Word64 -> GridNode -> Maybe GridNode
treeRemovePane pid = go
  where
    go (Pane p)
      | p == pid = Nothing
      | otherwise = Just (Pane p)
    go (Split sid0 ax r0 a b) =
      case (go a, go b) of
        (Nothing, Just b') -> Just b'
        (Just a', Nothing) -> Just a'
        (Just a', Just b') -> Just (Split sid0 ax r0 a' b')
        (Nothing, Nothing) -> Nothing

-- | Swap two panes by id (content follows the pane id).
treeSwapPanes :: Word64 -> Word64 -> GridNode -> GridNode
treeSwapPanes a b = go
  where
    go (Pane p)
      | p == a = Pane b
      | p == b = Pane a
      | otherwise = Pane p
    go (Split sid0 ax r0 x y) = Split sid0 ax r0 (go x) (go y)

-- | Move a pane onto a drop target. Center drops swap the two panes; edge
-- drops split the target pane with the given fresh split id and move the
-- dragged pane into the new child; top-level drops wrap the whole tree in a
-- new root split with the dragged pane on one side.
treeMovePane :: Word64 -> Word64 -> DropTarget -> GridNode -> Maybe GridNode
treeMovePane moved splitId dt tree
  | not (paneExist tree moved) = Nothing
  | otherwise =
      case dt of
        DropSwap tgt
          | tgt == moved -> Nothing
          | not (paneExist tree tgt) -> Nothing
          | otherwise -> Just (treeSwapPanes moved tgt tree)
        DropSplit tgt axis onA
          | tgt == moved -> Nothing
          | not (paneExist tree tgt) -> Nothing
          | otherwise -> do
              t' <- treeRemovePane moved tree
              Just (treeSplit tgt splitId axis onA moved t')
        DropTop axis onA
          | treeSize tree <= 1 -> Nothing
          | otherwise -> do
              t' <- treeRemovePane moved tree
              Just
                ( if onA
                    then Split splitId axis 0.5 (Pane moved) t'
                    else Split splitId axis 0.5 t' (Pane moved)
                )

-- | Find the split node with a given id (or 'Nothing').
findSplitNode :: GridNode -> Word64 -> Maybe GridNode
findSplitNode (Pane _) _ = Nothing
findSplitNode s@(Split sid0 _ _ a b) target
  | sid0 == target = Just s
  | otherwise = findSplitNode a target <|> findSplitNode b target

-- | Clamp a proposed ratio for a split so both subtrees keep at least their
-- minimum size within the given region.
clampTreeRatio :: GridNode -> Word64 -> Rect -> Float -> Float -> Float -> Float
clampTreeRatio tree splitId region spacing minSize r0 =
  case findSplitNode tree splitId of
    Nothing -> r0
    Just (Pane _) -> r0
    Just (Split _ ax _ a b) ->
      let avail = mainLen ax region
       in if avail <= 0
            then r0
            else
              let (wa, ha) = subtreeMin minSize spacing a
                  (wb, hb) = subtreeMin minSize spacing b
                  (mA, mB) = mainMins ax (wa, ha) (wb, hb)
               in splitLength spacing avail mA mB r0 / avail

-- | Which drop zone a pointer falls into for a target pane rect.
data EdgeZone = ZoneCenter | ZoneLeft | ZoneRight | ZoneTop | ZoneBottom

-- | Classify a drop point into a zone of the target pane.
edgeZone :: Rect -> V2 -> EdgeZone
edgeZone r mouse =
  let w = rectW r
      h = rectH r
   in if w <= 0 || h <= 0
        then ZoneCenter
        else
          let tx = (v2X mouse - rectX r) / w
              ty = (v2Y mouse - rectY r) / h
           in if tx < 0.25
                then ZoneLeft
                else if tx > 0.75
                  then ZoneRight
                  else if ty < 0.25
                    then ZoneTop
                    else if ty > 0.75
                      then ZoneBottom
                      else ZoneCenter

-- | Classify a drop point on a target pane into the 'DropTarget' the drop
-- performs: the pane's center swaps the two panes, an edge zone splits the
-- target along that edge's axis with the dragged pane on the near side.
dropTargetForPane :: Rect -> V2 -> Word64 -> DropTarget
dropTargetForPane r mouse tgt =
  case edgeZone r mouse of
    ZoneCenter -> DropSwap tgt
    ZoneLeft -> DropSplit tgt AxisV True
    ZoneRight -> DropSplit tgt AxisV False
    ZoneTop -> DropSplit tgt AxisH True
    ZoneBottom -> DropSplit tgt AxisH False

-- | Classify a drop point against the grid's outer boundary. If the pointer
-- sits within @band@ px of a grid edge, return the 'DropTop' target for that
-- edge; otherwise 'Nothing'. Checked before pane-level drops so the outermost
-- edge always restructures the whole grid.
topLevelDropTarget :: Float -> Rect -> V2 -> Maybe DropTarget
topLevelDropTarget band r mouse
  | rectW r <= 0 || rectH r <= 0 = Nothing
  | otherwise =
      let x = v2X mouse
          y = v2Y mouse
          l = rectX r
          t = rectY r
          w = rectW r
          h = rectH r
       in if x <= l + band
            then Just (DropTop AxisV True)
            else if x >= l + w - band
              then Just (DropTop AxisV False)
              else if y <= t + band
                then Just (DropTop AxisH True)
                else if y >= t + h - band
                  then Just (DropTop AxisH False)
                  else Nothing

-- | Drop preview for a drop target: the rect to highlight and the
-- 'DropTarget' the drop performs. The highlight is found by simulating the
-- drop ('treeMovePane' with a throwaway split id) and laying the resulting
-- tree out ('layoutNode') into the grid rect, so it is exactly the region the
-- dragged pane will occupy after the drop — accounting for the restructuring
-- that removing the pane causes (its parent split collapses and sibling
-- subtrees expand) and for 'spacing' and min-size floors. Estimating the rect
-- from the target's pre-drop bounds goes wrong wherever mixed 'AxisV' /
-- 'AxisH' splits make those two layouts diverge. @spacing@ must be the gutter
-- actually laid out between panes — 'NanoUI.Widgets.PaneGrid' passes
-- @pgSpacing + 2 * pgLeeway@, not @pgSpacing@ — or the preview regions drift
-- from the on-screen layout. 'Nothing' when the drop cannot be performed
-- (unknown pane ids, 'DropTop' on a single-pane grid).
dropPreview :: Float -> Float -> GridNode -> Word64 -> Rect -> DropTarget -> Maybe (Rect, DropTarget)
dropPreview minSize spacing tree moved baseRect dt = do
  t' <- treeMovePane moved 0 dt tree
  let (regions, _) = layoutNode minSize spacing t' baseRect
  r <- M.lookup moved regions
  pure (r, dt)
