-- | Interactive pane grid with resizable dividers, modelled on iced's
-- @PaneGrid@.
--
-- The grid is a binary split tree ('GridNode')
-- persisted per widget as a "Data.Dynamic" value in the widget store.
--
-- Panes are rendered through the user-provided 'pgViewPane', which receives a
-- 'PaneGridCtx' with immediate-mode actions to split, close, maximize, or
-- restore the pane. Dividers can be dragged to resize; panes can be grabbed by
-- their pick rect and dropped onto another pane (center = swap, edge = split)
-- or onto the grid's outer edge to restructure the whole grid at top level;
-- arrow keys navigate between panes; @m@/@x@ maximize/close and @Escape@
-- restores while the grid is focused.
--
-- A dragged pane is lifted out of the grid. While it hovers over a drop
-- target the grid is laid out as that drop will leave it, the pane's landing
-- slot empty under a highlight, and releasing stores exactly the tree on
-- screen. Releasing outside the grid cancels the drag.
module NanoUI.Widgets.PaneGrid
  ( GridAxis (..)
  , GridNode (..)
  , PaneGridConfig (..)
  , defaultPaneGridConfig
  , PaneGridCtx (..)
  , PaneView (..)
  , PaneGridResponse (..)
  , paneGrid
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Hashable (hash)
import Data.List (find, minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Primitive.SmallArray (SmallArray)
import Data.Word (Word64)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , bumpMirror
  , damageWidget
  , getFocusId
  , getFocusVisible
  , getPrevRect
  , getStore
  , intKey
  , markDirty
  , markEscapeConsumed
  , overlayConsumesQuit
  , registerCustomDrawing
  , registerFocusable
  , setStore
  , modifyStore
  )
import NanoUI.Internal.Draw (DrawOp)
import NanoUI.Internal.Input
  ( Input (..)
  , Key (..)
  , UiCursorKind (..)
  , inputChars
  , inputKeys
  , inputKeysElem
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  )
import NanoUI.Internal.Monad (Ui, askContext, askInput, nextId, releaseFocus, uiIO, withIdFrame, withKey)
import NanoUI.Internal.Id (IdContext (..), WidgetId, hashWidgetId)
import NanoUI.Internal.Frame.Hit (nodeInteractionHit, scrollHitRect)
import NanoUI.Internal.Frame.Input (isInteractiveNode)
import NanoUI.Internal.Store
  ( Slot (..)
  , WidgetStore
  , deleteSlot
  , fieldDyn
  , fieldInt
  , fieldPoint
  , findSlot
  , flagSlot
  , insertDyn
  , insertSlot
  , lookupDyn
  , lookupSlot
  , slotKey
  )
import NanoUI.Internal.Style
  ( AlignX (..)
  , AlignY (..)
  , Layout (..)
  , Sizing (..)
  , Style (..)
  , Theme (..)
  , defaultLayout
  , gap
  , minH
  , minW
  , tight
  , fadeAlpha
  , separatorTrackColor
  )
import NanoUI.Internal.Types
  ( DamageBounds (..)
  , Rect (..)
  , V2 (..)
  , clamp01
  , lerpColor
  , rectHit
  , rectH
  , rectInflate
  , rectNonEmpty
  , rectW
  , rectX
  , rectY
  , v2X
  , v2Y
  )
import NanoUI.Internal.Widgets.Behavior (KeyNav (..), dragThresholdPx, useKeyNav)
import NanoUI.Internal.Widgets.Custom
  ( CustomWidgetSpec (..)
  , CustomDrawContext (..)
  , contentKey
  , defaultCustomWidgetSpec
  , customWidget
  , drawRect
  , drawRoundedRect
  , drawStroke
  , drawStrokeRoundedRect
  , drawText
  , runCanvas
  )
import NanoUI.Internal.Widgets.Layout (column', row')
import NanoUI.Internal.Layout.Arena (NodeType (..), arenaCount, getNodeType, getWidgetId)
import NanoUI.Internal.Widgets.Node
  ( container
  , containerResponse
  , tagContainer
  )
import NanoUI.Internal.Widgets.SplitPane
  ( DividerInfo (..)
  , GridAxis (..)
  , GridNode (..)
  , clampTreeRatio
  , pinnedSide
  , reflowFixed
  , DropPreview (..)
  , dropPreviewTreeSized
  , dropTargetForPane
  , layoutNode
  , mainLen
  , mainMins
  , nearestPane
  , paneExist
  , splitLength
  , subtreeMin
  , topLevelDropTarget
  , treePanes
  , treeRemovePane
  , treeSetRatio
  , treeSize
  , treeSplit
  )

-- -----------------------------------------------------------------------------
-- Public API
-- -----------------------------------------------------------------------------

-- | Configuration for a pane grid. 'pgViewPane' can run arbitrary widget code,
-- so the config carries the caller's effect row.
data PaneGridConfig es = PaneGridConfig
  { pgLayout :: !(Layout -> Layout)
    -- ^ Layout modifier for the grid container (default 'id'); pass
    -- @fillW . fillH@ to fill the parent area.
  , pgSpacing :: !Float
    -- ^ Gutter between panes per split level (default 4).
  , pgMinSize :: !Float
    -- ^ Minimum size in logical pixels any pane may shrink to (default 40).
  , pgLeeway :: !Float
    -- ^ Extra grab margin on each side of a divider, added to 'pgSpacing' to
    -- form the divider's real layout gutter. The resize cursor and grab work
    -- anywhere in that gutter while only 'pgSpacing' is drawn crisp, so the
    -- interaction space is far wider than the visible line (default 6).
  , pgEdgeBand :: !Float
    -- ^ Thickness of the grid's outer edge that acts as a top-level drop zone
    -- (default 20). Dragging a pane into this band restructures the whole grid
    -- instead of a single pane: the tree is wrapped in a new top-level split
    -- with the dragged pane on that side.
  , pgPreserveDragSize :: !Bool
    -- ^ Retain the dragged pane's extent along its original parent split's
    -- axis, transferring width to height (or vice versa) when the drop changes
    -- orientation, subject to available space and subtree minima. The
    -- preview shows the same size as the committed drop. Center swaps are
    -- unaffected (default 'False', which splits the destination equally).
  , pgFixedPanes :: !(Word64 -> Bool)
    -- ^ Panes that keep their size when the grid's own rect changes size
    -- (default @const False@, every pane scaling with the grid). A pinned
    -- pane holds its extent along its parent split's axis -- its width
    -- between left/right panes, its height between stacked ones -- and the
    -- other side of that split takes all of the difference. It still moves to
    -- its neighbour's minimum when the grid grows too small to hold both, and
    -- the divider still drags it to any width; what it stops doing is
    -- following the grid.
    --
    -- Only the pane's own split is pinned, so the splits above it still
    -- share their regions out as they did: a pinned left sidebar keeps its
    -- width without freezing the height of the row it sits in, and a grid
    -- with a pinned pane at each end keeps both, the panes between them
    -- taking the difference.
  , pgInitial :: !(Maybe GridNode)
    -- ^ The split tree the grid starts from on its first frame (default
    -- 'Nothing', a single pane). A grid whose last pane is closed starts
    -- again from one fresh pane, not from this tree. Pane and split ids are
    -- the caller's to choose: unique within the tree, from 1 and below
    -- @2^63@ (0 is the grid's "none"), and the ids 'pgViewPane' is asked
    -- for; panes the grid makes later take ids above all of them. A ratio
    -- is honoured from the first frame, and a pinned pane ('pgFixedPanes')
    -- keeps the extent the ratio gives it once the grid has a size:
    --
    -- > pgInitial = Just (Split 3 AxisV 0.25 (Pane 1) (Pane 2))
  , pgFocusable :: !Bool
    -- ^ Whether the grid is a Tab stop whose arrow, @m@, @x@ and Escape keys
    -- act on its panes (default 'True'). Turn it off for a grid that only
    -- lays out and resizes panes whose content owns the keyboard.
  , pgViewPane :: !(Word64 -> PaneGridCtx es -> Eff es PaneView)
    -- ^ Renders the content of one pane.
  }

-- | Default spacing and drag margins with empty pane bodies. Set 'pgViewPane'
-- to render application content and 'pgLayout' to constrain the grid.
defaultPaneGridConfig :: PaneGridConfig es
defaultPaneGridConfig =
  PaneGridConfig
    { pgLayout = id
    , pgSpacing = 4
    , pgMinSize = 40
    , pgLeeway = 6
    , pgEdgeBand = 20
    , pgPreserveDragSize = False
    , pgFixedPanes = const False
    , pgInitial = Nothing
    , pgFocusable = True
    , pgViewPane = \_ _ -> pure (PaneView "" False Nothing)
    }

-- | Actions handed to a pane so it can mutate the grid immediately.
data PaneGridCtx es = PaneGridCtx
  { pgcPaneId :: !Word64
  , pgcRect :: !Rect
    -- ^ Prev-frame screen rect of this pane (zero until it has been laid
    -- out once; the whole grid rect while maximized; its rect in the
    -- previewed layout while a drop is previewed). Use it to build
    -- 'pvDragPick' handles such as a title-bar sub-rect.
  , pgcMaximized :: !Bool
    -- ^ True when this pane currently fills the whole grid.
  , pgcDragging :: !Bool
    -- ^ True while this pane's drag is armed. Once the drag threshold is
    -- crossed, the pane is not rendered until release: its space closes up,
    -- or, over a drop target, its landing slot is shown empty.
  , pgcDndActive :: !Bool
    -- ^ True while any pane drag-and-drop gesture is in progress.
  , pgcSplit :: !(GridAxis -> Eff es Word64)
    -- ^ Split this pane along the axis; returns the new pane id.
  , pgcClose :: !(Eff es ())
  , pgcMaximize :: !(Eff es ())
  , pgcRestore :: !(Eff es ())
  }

-- | What a pane renders to this frame. The pane's content (including any title
-- bar / header) is drawn entirely by the caller in 'pgViewPane'; a header is
-- purely optional and nothing here depends on one existing.
data PaneView = PaneView
  { pvTitle :: !Text
    -- ^ Label shown (abbreviated to fit) on the compact drag indicator.
  , pvDraggable :: !Bool
    -- ^ Grab the pane anywhere inside its own region to drag-and-drop it. This
    -- is the easy way to reorder panes without drawing a dedicated handle.
    -- Interactive children keep their pointer presses. Pane still needs a
    -- drag only on a sub-region? see 'pvDragPick'.
  , pvDragPick :: !(Maybe Rect)
    -- ^ Optional absolute sub-region (e.g. just a title bar; position it via
    -- 'pgcRect') that also starts a drag. Both handles combine: the pane drags
    -- if the press lands in this rect or (when 'pvDraggable') anywhere in the
    -- pane. 'Nothing' here and 'pvDraggable' 'False' makes the pane immovable.
  }
  deriving (Eq, Show)

-- | Outcome of one frame of the grid. The pane list, focus, and maximize
-- fields report the state after this pass: actions run by pane content
-- ('pgcSplit', 'pgcClose', ...) and the keyboard handling below take effect
-- in these values and from the next frame's layout onward.
data PaneGridResponse = PaneGridResponse
  { pgrChanged :: !Bool
    -- ^ Any structural or maximize change happened this frame.
  , pgrPaneCount :: !Int
    -- ^ Number of panes (0 once the last pane has been closed).
  , pgrPanes :: ![Word64]
    -- ^ Live pane ids, depth-first.
  , pgrFocusedPane :: !Word64
    -- ^ Focused pane id, 0 when the grid has no panes.
  , pgrMaximizedPane :: !Word64
    -- ^ Maximized pane id, 0 when none.
  }
  deriving (Eq, Show)

-- -----------------------------------------------------------------------------
-- Internal state
-- -----------------------------------------------------------------------------

data RenderedPane = RenderedPane
  { rpPaneId :: !Word64
  , rpView :: !PaneView
  , rpControlHit :: !Bool
  }

-- | Per-frame shared environment.
data GridEnv es = GridEnv
  { geCtx :: !Context
  , geKey :: !Int
  , gePaneScope :: !IdContext
    -- ^ Pane identity is rooted at the grid widget, independent of split
    -- ancestry so rearranging or temporarily collapsing splits preserves state.
  , geCfg :: !(PaneGridConfig es)
  , geGutter :: !Float
  , geThickness :: !Float
  , geMinSize :: !Float
  , geLeeway :: !Float
  , geRegions :: !(Map Word64 Rect)
    -- ^ Prev-frame pane regions; drives hit testing and 'pgcRect'.
  , geBaseRect :: !Rect
    -- ^ Prev-frame rect of the grid's root container.
  , geTree :: !GridNode
  , geSeed :: !Word64
    -- ^ Next fresh split / pane id ('SlotPaneNext'); strictly monotonic per
    -- grid, so ids are never reused and state keyed by pane id cannot
    -- collide with a closed pane's state.
  , geDrag0 :: !Int
  , geLifted :: !Bool
    -- ^ The dragged pane is lifted out of the grid: its content is not
    -- rendered, and where the visible tree still holds it (a drop preview)
    -- its slot is left empty.
  , geMax :: !Word64
  , geChangedRef :: !(IORef Bool)
  , geMakeCtx :: Word64 -> Rect -> Bool -> PaneGridCtx es
  }

-- | Computed drag-and-drop interaction state for one frame.
data DragInfo = DragInfo
  { dgiActive :: !Bool
  , dgiMoved :: !Bool
  , dgiGhost :: !(Maybe Rect)
  , dgiZone :: !(Maybe DropPreview)
    -- ^ The drop under the pointer, laid out. The same tree is shown while
    -- hovering and stored on release, so the preview cannot disagree with the
    -- drop.
  }

-- -----------------------------------------------------------------------------
-- Tree + focus state
-- -----------------------------------------------------------------------------

-- | The largest pane or split id in a tree.
treeMaxId :: GridNode -> Word64
treeMaxId = \case
  Pane p -> p
  Split sid _ _ a b -> maximum [sid, treeMaxId a, treeMaxId b]

-- | The grid's split tree persisted in the widget store, if seeded.
lookupTree :: Int -> WidgetStore -> Maybe GridNode
lookupTree = lookupDyn

-- | A stored pane id that still exists in the tree, else 0.
validPane :: GridNode -> Int -> Word64
validPane t n =
  let p = fromIntegral n
   in if paneExist t p then p else 0

-- | Focused pane: a maximized pane wins, then the stored focus if the pane
-- still exists, then the first pane in the tree.
resolveFocus :: GridNode -> Word64 -> Word64 -> Word64
resolveFocus tree maxPane focus0
  | maxPane /= 0 = maxPane
  | paneExist tree focus0 = focus0
  | otherwise = fromMaybe 1 (listToMaybe (treePanes tree))

-- -----------------------------------------------------------------------------
-- Entry point
-- -----------------------------------------------------------------------------

-- | Stateful split-pane workspace with draggable tabs and dividers. Keep its
-- widget identity stable; pane callbacks receive actions for splits, closes,
-- and maximisation through 'PaneGridCtx'.
paneGrid :: (Ui :> es) => PaneGridConfig es -> Eff es PaneGridResponse
paneGrid cfg = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  -- A grid that is no Tab stop gives up any focus it still has, from a frame
  -- when it was one, so its ring does not stay drawn round a pane.
  if pgFocusable cfg then uiIO (registerFocusable ctx wid) else releaseFocus wid
  let key = intKey wid
      gestK = slotKey SlotPaneGest key
      grabK = slotKey SlotPaneGrab key
      focusK = slotKey SlotPaneFocus key
      maxK = slotKey SlotPaneMax key
      seedK = slotKey SlotPaneNext key
      spacing = max 0 (pgSpacing cfg)
      minSize = max 0 (pgMinSize cfg)
      leeway = max 0 (pgLeeway cfg)
      edgeBand = max 0 (pgEdgeBand cfg)
      gutter = spacing + 2 * leeway
  st <- uiIO (getStore ctx)
  (tree0, seed1) <- case lookupTree key st of
    Just t ->
      -- Init seeded the store before the tree existed, so the stored seed
      -- is already above every id in the tree.
      pure (t, fromIntegral (findSlot fieldInt 1 seedK st))
    Nothing -> do
      let mStored = lookupSlot fieldInt seedK st
          stored = maybe 1 (max 1 . fromIntegral) mStored
          (start, next) = case pgInitial cfg of
            -- A stored seed is a grid that had a tree and closed its last
            -- pane. It starts again from one fresh pane, like any other grid,
            -- so no id a closed pane's state is kept under comes back.
            Just t | Nothing <- mStored -> (t, max stored (treeMaxId t + 1))
            _ -> (Pane stored, stored + 1)
      uiIO . setStore ctx . bumpMirror $
        insertSlot fieldInt seedK (fromIntegral next) (insertDyn key start st)
      pure (start, next)
  mPrev <- uiIO (getPrevRect ctx wid)
  let baseRect = fromMaybe (Rect 0 0 0 0) mPrev
      spanK = slotKey SlotPaneSpan key
      curSpan = (rectW baseRect, rectH baseRect)
  -- A pinned pane follows its own extent, not the grid's, so a grid whose
  -- rect changed size re-ratios its splits before anything is laid out from
  -- them. The span the tree was last fitted to is kept beside it rather than
  -- recovered from the rect history, so a frame that only moves the grid, or
  -- that never reaches it at all, leaves the pins alone. A size the grid
  -- reaches over several frames is charged to the pinned pane by the part of
  -- the difference each frame brings. No span yet is a grid that has never
  -- been fitted, which is nothing to reflow.
  tree <- case lookupSlot fieldPoint spanK st of
    Just prevSpan | prevSpan == curSpan -> pure tree0
    mPrevSpan -> do
      let reflowed = case mPrevSpan of
            Just (pw, ph)
              | any (pgFixedPanes cfg) (treePanes tree0) ->
                  reflowFixed (pgFixedPanes cfg) minSize gutter (baseRect {rectW = pw, rectH = ph}) baseRect tree0
            _ -> tree0
      uiIO . modifyStore ctx $
        insertSlot fieldPoint spanK curSpan
          . (if reflowed == tree0 then id else insertDyn key reflowed)
      pure reflowed
  let drag0 = findSlot fieldInt 0 gestK st
      maxPane = validPane tree (findSlot fieldInt 0 maxK st)
      focus0 = findSlot fieldInt 0 focusK st
      focusedInit = resolveFocus tree maxPane (fromIntegral focus0)
      mouse = inputMousePos inp
      (regions, dividers) = layoutNode minSize gutter tree baseRect
  changedRef <- uiIO (newIORef False)
  let mGrab = lookupSlot fieldPoint grabK st
      dgi =
        computeDragInfo
          drag0
          (flagSlot grabK st)
          DragGeom
            { dgMinSize = minSize
            , dgGutter = gutter
            , dgTree = tree
            , dgBaseRect = baseRect
            , dgBand = edgeBand
            , dgRegions = regions
            , dgSeed = seed1
            , dgPreserveSize = pgPreserveDragSize cfg
            }
          mGrab
          mouse
      dgiShown = dgiActive dgi && dgiMoved dgi && inputMouseDown inp
      -- The committed tree stays in the store for cancellation. The live
      -- layout shows the drop under the pointer as it will land: the post-drop
      -- tree, with the dragged pane's slot left empty for the highlight. The
      -- highlight alone over the pre-drop panes would not line up with them,
      -- since a drop moves the other panes too. With no drop target, the
      -- dragged pane's space just closes up.
      (visibleTree, (visibleRegions, visibleDividers))
        | not dgiShown = (Just tree, (regions, dividers))
        | Just dp <- dgiZone dgi = (Just (dpTree dp), (dpRegions dp, dpDividers dp))
        | otherwise =
            maybe
              (Nothing, (M.empty, []))
              (\t -> (Just t, layoutNode minSize gutter t baseRect))
              (treeRemovePane (fromIntegral drag0) tree)
      divMap = M.fromList [(diSplitId d, d) | d <- visibleDividers]
      env =
        GridEnv
          { geCtx = ctx
          , geKey = key
          , gePaneScope = IdContext (hashWidgetId wid) 0
          , geCfg = cfg
          , geGutter = gutter
          , geThickness = spacing
          , geMinSize = minSize
          , geLeeway = leeway
          , geRegions = visibleRegions
          , geBaseRect = baseRect
          , geTree = tree
          , geSeed = seed1
          , geDrag0 = drag0
          , geLifted = dgiShown
          , geMax = maxPane
          , geChangedRef = changedRef
          , geMakeCtx = \pid rect dragging ->
              PaneGridCtx
                { pgcPaneId = pid
                , pgcRect = rect
                , pgcMaximized = maxPane == pid
                , pgcDragging = dragging
                , pgcDndActive = dgiMoved dgi
                , pgcSplit = \axis -> splitPane env pid axis
                , pgcClose = closePane env pid
                , pgcMaximize = maximizePane env pid
                , pgcRestore = restorePane env
                }
          }

  -- Root container. Tagged so its solved rect resolves via getPrevRect for
  -- next frame's geometry.
  container NodeContainer (gridRootLayout minSize (pgLayout cfg)) $ do
    tagContainer wid
    if maxPane /= 0
      then void (renderMaxPane env maxPane)
      else do
        rendered <- maybe (pure []) (renderNode env divMap) visibleTree
        runGestures env dividers rendered dgi
        when (dgiShown && rectNonEmpty baseRect) $
          drawDragOverlay env wid rendered (dgiGhost dgi) (dpRect <$> dgiZone dgi)
        -- Keyboard focus also rings the focused pane, so the arrow keys show
        -- where they moved; the grid's own ring says the grid holds focus.
        ringPane <- uiIO ((&&) <$> getFocusVisible ctx <*> ((== wid) <$> getFocusId ctx))
        when (ringPane && not dgiShown) $
          forM_ (M.lookup focusedInit visibleRegions) $ \r ->
            uiIO $ registerCustomDrawing ctx wid (contentKey [1, rectX r, rectY r, rectW r, rectH r]) $ \cdc _ ->
              runCanvas (drawStrokeRoundedRect (rectInflate (-2) r) 2 1.5 (themeAccent (cdcTheme cdc)))

  -- Keyboard navigation for the focused grid. Escape restores a maximized
  -- pane unless something earlier in the pass already consumed it (e.g. a
  -- dismissable popup inside a pane); the grid then claims the key so
  -- neither a nested overlay nor the app also acts on it.
  focusedNow <- uiIO (getFocusId ctx)
  when (pgFocusable cfg && focusedNow == wid) $ do
    nav <- useKeyNav wid
    let ch = inputChars inp
        cur = focusedInit
    when (knLeft nav) $ moveFocus env cur (-1, 0)
    when (knRight nav) $ moveFocus env cur (1, 0)
    when (knUp nav) $ moveFocus env cur (0, -1)
    when (knDown nav) $ moveFocus env cur (0, 1)
    when (knLeft nav || knRight nav || knUp nav || knDown nav) $
      uiIO (damageWidget ctx wid (DamageInflated 0))
    when (T.any (== 'm') ch) $ maximizePane env cur
    when (T.any (== 'x') ch) $ closePane env cur
    when (inputKeysElem KeyEscape (inputKeys inp)) $ do
      taken <- uiIO (overlayConsumesQuit ctx inp)
      unless taken $ do
        restorePane env
        uiIO (markEscapeConsumed ctx)

  changed <- uiIO (readIORef changedRef)
  stEnd <- uiIO (getStore ctx)
  let treeEnd = lookupTree key stEnd
      maxEnd = maybe 0 (\t -> validPane t (findSlot fieldInt 0 maxK stEnd)) treeEnd
      focusEnd = maybe 0 (\t -> resolveFocus t maxEnd (fromIntegral (findSlot fieldInt 0 focusK stEnd))) treeEnd
  pure
    PaneGridResponse
      { pgrChanged = changed
      , pgrPaneCount = maybe 0 treeSize treeEnd
      , pgrPanes = maybe [] treePanes treeEnd
      , pgrFocusedPane = focusEnd
      , pgrMaximizedPane = maxEnd
      }

-- -----------------------------------------------------------------------------
-- Layout helpers
-- -----------------------------------------------------------------------------

gridRootLayout :: Float -> (Layout -> Layout) -> Layout
gridRootLayout minSize f = f (minW minSize . minH minSize $ fillLay)

sizingLay :: Sizing -> Sizing -> Layout
sizingLay wSiz hSiz = tight . gap 0 $ defaultLayout {layoutWidth = wSiz, layoutHeight = hSiz}

-- | Zero-gap, zero-padding, grow-to-fill layout.
fillLay :: Layout
fillLay = sizingLay (Grow 1) (Grow 1)

-- | Sizing for the side of a split that carries the ratio: a percent along
-- the main axis. The other side grows into the remainder, so between them the
-- two follow the grid as it resizes.
splitSideLay :: GridAxis -> Float -> Layout
splitSideLay axis p = axisLay axis (Percent p)

-- | Sizing for a side of a split whose region has no length yet: a grow
-- weight along the main axis, so the two sides share the region left after
-- the gutter in the split's ratio. A zero weight would not grow at all.
splitWeightLay :: GridAxis -> Float -> Layout
splitWeightLay axis w = axisLay axis (Grow (max 1.0e-3 w))

-- | Sizing for the side of a split that holds a pinned pane: its extent in
-- pixels along the main axis. A percent of this frame's real width is what
-- makes an unpinned side track a resize, so a pinned one is laid out in the
-- length it had instead and the grid grows past it. The ratio is reflowed to
-- match on the next frame ('reflowFixed'), so the two never disagree for
-- longer than the frame the resize arrived on.
pinnedSideLay :: GridAxis -> Float -> Layout
pinnedSideLay axis n = axisLay axis (Fixed (max 0 n))

-- | @s@ along the axis's main direction, growing across it.
axisLay :: GridAxis -> Sizing -> Layout
axisLay AxisV s = sizingLay s (Grow 1)
axisLay AxisH s = sizingLay (Grow 1) s

minSized :: Layout -> Float -> Float -> Layout
minSized l minW_ minH_ = l {layoutMinW = minW_, layoutMinH = minH_}

-- | The pane content wrapper: fills its cell, never below one minimum pane.
paneLay :: Float -> Layout
paneLay m = minSized fillLay m m

-- Percent of the main-axis extent, of positive length, taken by an A side of
-- the given length.
splitPct :: Float -> Float -> Float
splitPct avail d = d / avail * 100

-- -----------------------------------------------------------------------------
-- Rendering
-- -----------------------------------------------------------------------------

renderMaxPane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es [RenderedPane]
renderMaxPane env pid =
  renderPane env pid (geBaseRect env) (paneLay (geMinSize env)) False

-- | Enter a pane's grid-relative identity scope while leaving the split tree's
-- layout scopes intact. Consume one sibling just as 'withKey' does.
withPaneKey :: (Ui :> es) => GridEnv es -> Word64 -> Eff es a -> Eff es a
withPaneKey env pid =
  withIdFrame (\parent -> (parent {siblingId = siblingId parent + 1}, gePaneScope env)) . withKey pid

-- | Render one pane's content via 'pgViewPane' under the pane's stable key.
renderPane ::
  (Ui :> es) =>
  GridEnv es ->
  Word64 ->
  Rect ->
  Layout ->
  Bool ->
  Eff es [RenderedPane]
renderPane env pid rect lay dragging =
  withPaneKey env pid $ do
    inp <- askInput
    let ctx = geCtx env
        arena = ctxNodeArena ctx
    start <- uiIO (arenaCount arena)
    let ctxt = geMakeCtx env pid rect dragging
    (view, _) <- containerResponse NodeContainer lay (pgViewPane (geCfg env) pid ctxt)
    -- Press ownership must be checked against previous solved child rects:
    -- ctxActiveId is only finalized after this frame's UI has been built.
    controlHit <-
      if not (inputMousePressed inp)
        then pure False
        else uiIO $ do
          end <- arenaCount arena
          let hitFrom idx
                | idx >= end = pure False
                | otherwise = do
                    nt <- getNodeType arena idx
                    hit <-
                      if isInteractiveNode nt
                        then do
                          child <- getWidgetId arena idx
                          r <- scrollHitRect ctx child
                          maybe (pure False) (\childRect -> nodeInteractionHit ctx idx childRect (inputMousePos inp)) r
                        else pure False
                    if hit then pure True else hitFrom (idx + 1)
          hitFrom start
    pure [RenderedPane pid view controlHit]

renderNode ::
  (Ui :> es) =>
  GridEnv es ->
  Map Word64 DividerInfo ->
  GridNode ->
  Eff es [RenderedPane]
renderNode env dividers = \case
  Pane pid
    -- The lifted pane's landing slot in a drop preview: an empty cell of the
    -- pane's size, which the drop-zone highlight fills.
    | geLifted env && draggingPane env pid ->
        [] <$ container NodeContainer (paneLay (geMinSize env)) (pure ())
    | otherwise ->
        renderPane env pid (paneRect env pid) (paneLay (geMinSize env)) (draggingPane env pid)
  Split sid0 ax ratio a b ->
    withKey sid0 $ do
      let (wa, ha) = subtreeMin (geMinSize env) (geGutter env) a
          (wb, hb) = subtreeMin (geMinSize env) (geGutter env) b
          mDiv = M.lookup sid0 dividers
          avail = maybe 0 (mainLen ax . diRegion) mDiv
          (mA, mB) = mainMins ax (wa, ha) (wb, hb)
          -- The A side's extent as 'layoutNode' clamped it, which the two
          -- sizings below express either as a share of the split's region or
          -- as the length itself.
          dA = splitLength (geGutter env) avail mA mB (maybe 0.5 diRatio mDiv)
          pinA = pinnedSide (pgFixedPanes (geCfg env)) a
          pinB = pinnedSide (pgFixedPanes (geCfg env)) b
          (aLay, bLay)
            -- A region not laid out yet -- the grid's first frame, or a split
            -- made this frame -- has no length to take a share of or to pin
            -- a side at. The two sides share out what the solver gives them
            -- after the gutter, in the split's own ratio.
            | avail <= 0 =
                ( minSized (splitWeightLay ax (clamp01 ratio)) wa ha
                , minSized (splitWeightLay ax (1 - clamp01 ratio)) wb hb
                )
            | pinA == pinB =
                (minSized (splitSideLay ax (splitPct avail dA)) wa ha, minSized fillLay wb hb)
            | pinA = (minSized (pinnedSideLay ax dA) wa ha, minSized fillLay wb hb)
            | otherwise =
                (minSized fillLay wa ha, minSized (pinnedSideLay ax (avail - dA - geGutter env)) wb hb)
          inner = do
            a' <- container NodeContainer aLay (renderNode env dividers a)
            dividerWidget env ax
            b' <- container NodeContainer bLay (renderNode env dividers b)
            pure (a' <> b')
      case ax of
        AxisV -> row' fillLay inner
        AxisH -> column' fillLay inner

-- | Prev-frame rect of a pane; zero until the pane has been laid out once.
paneRect :: GridEnv es -> Word64 -> Rect
paneRect env pid = fromMaybe (Rect 0 0 0 0) (M.lookup pid (geRegions env))

-- | Is this the pane being drag-and-dropped? A resize gesture (negative id)
-- wraps to a huge 'Word64' and never matches a pane id.
draggingPane :: GridEnv es -> Word64 -> Bool
draggingPane env pid = fromIntegral (geDrag0 env) == pid

-- | The divider: a 'NodeDrawing' spanning the full gutter (visible thickness
-- plus the invisible grab halo on each side). Its widget rect covers the whole
-- gutter, so the resize cursor and grab apply across the halo; the gutter is
-- drawn as a faint rail with the crisp 'geThickness' strip in the middle, so
-- the whole interaction space reads as one divider.
dividerWidget :: (Ui :> es) => GridEnv es -> GridAxis -> Eff es ()
dividerWidget env axis = do
  void $
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = axisLay axis (Fixed (geGutter env))
        , widgetContent = contentKey [if axis == AxisV then 1 else 2, geThickness env, geLeeway env]
        , widgetDraw = \cdc rect -> drawDivider cdc rect axis (geThickness env) (geLeeway env)
        , widgetCursor = Just (const (if axis == AxisV then UiCursorEwResize else UiCursorNsResize))
        }

drawDivider :: CustomDrawContext -> Rect -> GridAxis -> Float -> Float -> SmallArray DrawOp
drawDivider cdc rect axis thickness leeway =
  runCanvas $ do
    let theme = cdcTheme cdc
        panel = themePanel theme
        rail = lerpColor (styleBg panel) (themeSeparator theme) 0.12
        track = separatorTrackColor panel theme
        trackRect = case axis of
          AxisV -> Rect (rectX rect + leeway) (rectY rect) thickness (rectH rect)
          AxisH -> Rect (rectX rect) (rectY rect + leeway) (rectW rect) thickness
    drawRect rect rail
    drawRect trackRect track
    when (cdcHovered cdc || cdcPressed cdc) $ do
      -- Full accent while grabbed; a calmer tint while merely hovering.
      let line
            | cdcPressed cdc = themeAccent theme
            | otherwise = lerpColor (themeAccent theme) (styleBg panel) 0.45
      case axis of
        AxisV ->
          let cx = rectX rect + rectW rect / 2
           in drawStroke (V2 cx (rectY rect)) (V2 cx (rectY rect + rectH rect)) 2 line
        AxisH ->
          let cy = rectY rect + rectH rect / 2
           in drawStroke (V2 (rectX rect) cy) (V2 (rectX rect + rectW rect) cy) 2 line

-- | Drag ghost + drop-zone highlight, drawn on top of the grid via a custom
-- drawing registered on the grid's root container. Registering on the
-- container (instead of adding a flex sibling) keeps the overlay out of the
-- layout, so it never squeezes the panes and is clipped to the full grid rect.
drawDragOverlay ::
  (Ui :> es) =>
  GridEnv es ->
  WidgetId ->
  [RenderedPane] ->
  Maybe Rect ->
  Maybe Rect ->
  Eff es ()
drawDragOverlay env wid rendered ghost zone = do
  st <- uiIO (getStore (geCtx env))
  let ctx = geCtx env
      dragPane = fromIntegral (geDrag0 env)
      cached = lookupDyn (slotKey SlotPaneGrab (geKey env)) st
      title = maybe (fromMaybe "" cached) pvTitle (fmap rpView (find ((== dragPane) . rpPaneId) rendered))
      rectKey = maybe [0, 0, 0, 0, 0] (\(Rect x y w h) -> [1, x, y, w, h])
      key = contentKey (2 : fromIntegral (hash title) : rectKey ghost ++ rectKey zone)
  uiIO $
    registerCustomDrawing ctx wid key (\cdc _ -> drawOverlay (cdcTheme cdc) title ghost zone)

-- | A compact, translucent drag indicator leaves the full-size drop preview
-- visible. The indicator is offset from the pointer so it cannot obscure aim.
drawOverlay :: Theme -> Text -> Maybe Rect -> Maybe Rect -> SmallArray DrawOp
drawOverlay theme title ghost zone =
  runCanvas $ do
    let accent = themeAccent theme
        win = themeFloatingWindow theme
        panelFill = fadeAlpha accent 48
        panelBorder = fadeAlpha accent 128
        previewFill = fadeAlpha accent 32
        shortTitle = if T.length title > 12 then T.take 11 title <> "…" else title
    forM_ ghost $ \gr -> do
      drawRoundedRect gr 2 panelFill
      drawStrokeRoundedRect gr 2 2 panelBorder
      when (not (T.null title)) $
        drawText (V2 (rectX gr + 6) (rectY gr + 6)) AlignStart AlignTop shortTitle (fadeAlpha (styleFg win) 160)
    forM_ zone $ \zr -> do
      drawRoundedRect zr 2 previewFill
      drawStrokeRoundedRect (rectInflate (-1) zr) 2 2 accent

-- -----------------------------------------------------------------------------
-- Gestures
-- -----------------------------------------------------------------------------

-- | Grid geometry 'computeDragInfo' needs for the current frame.
data DragGeom = DragGeom
  { dgMinSize :: !Float
    -- ^ Per-pane size floor used by the preview layout.
  , dgGutter :: !Float
    -- ^ Layout gutter between panes ('pgSpacing' + 2 * 'pgLeeway').
  , dgTree :: !GridNode
    -- ^ Current split tree.
  , dgBaseRect :: !Rect
    -- ^ Prev-frame rect of the grid's root container.
  , dgBand :: !Float
    -- ^ Thickness of the grid's outer top-level drop band.
  , dgRegions :: !(Map Word64 Rect)
    -- ^ Prev-frame pane regions.
  , dgPreserveSize :: !Bool
  , dgSeed :: !Word64
    -- ^ Id the drop's new split takes ('geSeed').
  }

-- | Pure drag-and-drop geometry for the current frame. Geometry is computed
-- for as long as the gesture id is armed (not just while the button is held),
-- so the drop zone is still resolvable on the frame the button is released.
-- 'dgBaseRect' is the grid's own rect: its outer band (thickness 'dgBand') is
-- a top-level drop zone, and the pointer there restructures the whole grid;
-- otherwise the pane nearest the pointer is the target, and a pointer outside
-- the grid has none. Every candidate is resolved through 'dropPreviewTreeSized',
-- which simulates the drop and lays the tree back out with the grid's real
-- 'dgGutter' and 'dgMinSize', so the highlighted rect is the exact region the
-- pane lands in even when removing it reshapes the rest of a mixed-split grid.
--
-- Targets are hit-tested against the grid with the dragged pane removed, never
-- against the previewed layout on screen: the target is then a function of
-- the pointer alone, and showing a preview cannot change which drop it is.
computeDragInfo :: Int -> Bool -> DragGeom -> Maybe (Float, Float) -> V2 -> DragInfo
computeDragInfo drag0 latched geom mGrab mouse
  | drag0 <= 0 = DragInfo False False Nothing Nothing
  | otherwise =
      let DragGeom{dgMinSize = minSize, dgGutter = gutter, dgTree = tree, dgBaseRect = baseRect, dgBand = band, dgRegions = regions, dgSeed = seed} = geom
          pid = fromIntegral drag0
          mFrom = M.lookup pid regions
          (gx, gy) = fromMaybe (0, 0) mGrab
          moved = latched || case mFrom of
            Just (Rect px py _ _) ->
              let vx = v2X mouse - (px + gx)
                  vy = v2Y mouse - (py + gy)
               in vx * vx + vy * vy > dragThresholdPx * dragThresholdPx
            Nothing -> False
          ghost = case mFrom of
            Just _
              | moved -> Just (Rect (v2X mouse + 12) (v2Y mouse + 12) 112 28)
            _ -> Nothing
          targetRegions = maybe M.empty (\t -> fst (layoutNode minSize gutter t baseRect)) (treeRemovePane pid tree)
          preview = dropPreviewTreeSized (if dgPreserveSize geom then mFrom else Nothing) minSize gutter tree pid seed baseRect
          zone = case topLevelDropTarget band baseRect mouse of
            Just dt -> preview dt
            Nothing
              | rectHit baseRect mouse ->
                  nearestPane targetRegions mouse >>= \(q, r) -> preview (dropTargetForPane r mouse q)
              | otherwise -> Nothing
       in DragInfo True moved ghost zone

-- | Apply resize / drag transitions, writing to the widget store.
runGestures ::
  (Ui :> es) =>
  GridEnv es ->
  [DividerInfo] ->
  [RenderedPane] ->
  DragInfo ->
  Eff es ()
runGestures env dividers rendered dgi = do
  ctx <- askContext
  inp <- askInput
  let regions = geRegions env
      mouse = inputMousePos inp
      press = inputMousePressed inp
      down = inputMouseDown inp
      drag0 = geDrag0 env
      busy = drag0 /= 0
      -- diBand already spans spacing + both leeway margins. Inflating it
      -- again steals presses from the neighboring pane, especially headers.
      hitDiv =
        find
          (\d -> rectHit (diBand d) mouse)
          dividers
      -- The pane whose pick rect (or, when 'pvDraggable', whole region) is
      -- under the pointer.
      pickHit =
        listToMaybe
          [ p
          | pane <- rendered
          , let p = rpPaneId pane
                v = rpView pane
          , maybe False (`rectHit` mouse) (pvDragPick v)
              || (pvDraggable v && maybe False (`rectHit` mouse) (M.lookup p regions))
          ]
      gestK = slotKey SlotPaneGest (geKey env)
      grabK = slotKey SlotPaneGrab (geKey env)
  -- A press arms the gesture slot (negative split id for a resize, pane id
  -- for a drag) together with its start state in one store write. The resize
  -- start keeps the divider's ratio and the pointer's main-axis coordinate so
  -- drag frames move the divider by delta instead of snapping it to the
  -- pointer; the drag start keeps the title and the grab offset (mouse - pane
  -- origin) for the drag threshold.
  when (press && not busy && not (any rpControlHit rendered)) $ do
    case hitDiv of
      Just d ->
        storeWrite env True $
          insertSlot fieldInt gestK (negate (fromIntegral (diSplitId d)))
            . insertSlot fieldPoint (slotKey SlotPaneResize (geKey env)) (diRatio d, mouseMain d mouse)
      Nothing ->
        forM_ pickHit $ \pid -> do
          let title = maybe "" (pvTitle . rpView) (find ((== pid) . rpPaneId) rendered)
              (gx, gy) = maybe (0, 0) (\(Rect px py _ _) -> (v2X mouse - px, v2Y mouse - py)) (M.lookup pid regions)
          storeWrite env True $
            insertDyn grabK title
              . insertSlot fieldInt gestK (fromIntegral pid)
              . deleteSlot fieldInt grabK
              . insertSlot fieldPoint grabK (gx, gy)
  when (drag0 < 0 && down) $ do
    let sid = fromIntegral (negate drag0)
    forM_ (find ((== sid) . diSplitId) dividers) $ \d -> do
      st <- uiIO (getStore ctx)
      let (ratio0, main0) =
            findSlot fieldPoint (diRatio d, mouseMain d mouse) (slotKey SlotPaneResize (geKey env)) st
          -- The ratio shares out the region minus the divider gutter.
          usable = mainLen (diAxis d) (diRegion d) - geGutter env
          r0 =
            if usable <= 0
              then ratio0
              else ratio0 + (mouseMain d mouse - main0) / usable
          r' = clampTreeRatio (geTree env) sid (diRegion d) (geGutter env) (geMinSize env) r0
       in putTree env (Just (treeSetRatio sid r' (geTree env)))
  when (drag0 < 0 && not down) $ writeGest env 0
  -- Keep the loop at the display cadence while a pane is being dragged: the
  -- ghost follows the pointer, and without a dirty flag the debug HUD's slow
  -- refresh paces the whole frame (4 fps). Window / scroll / resize drags mark
  -- dirty every frame for the same reason.
  when (drag0 > 0 && down) $ uiIO (markDirty ctx)
  when (drag0 > 0 && down && dgiMoved dgi) $
    storeWrite env False (insertSlot fieldInt (slotKey SlotPaneGrab (geKey env)) 1)
  -- A drop clears the gesture and, when it moved the pane, stores the new
  -- tree, seed and focus in the same write.
  when (drag0 > 0 && not down) $ do
    let moved = fromIntegral drag0 :: Word64
        -- The previewed tree is the drop: it was built with 'geSeed'.
        dropped
          | dgiMoved dgi = dpTree <$> dgiZone dgi
          | otherwise = Nothing
    storeWrite env True $
      deleteSlot fieldInt gestK . case dropped of
        Nothing -> id
        Just t' ->
          insertDyn (geKey env) t'
            . insertSlot fieldInt (slotKey SlotPaneNext (geKey env)) (fromIntegral (geSeed env + 1))
            . insertSlot fieldInt (slotKey SlotPaneFocus (geKey env)) (fromIntegral moved)
    when (isJust dropped) (markChanged env)

mouseMain :: DividerInfo -> V2 -> Float
mouseMain d mouse = case diAxis d of
  AxisV -> v2X mouse
  AxisH -> v2Y mouse

-- -----------------------------------------------------------------------------
-- Keyboard navigation
-- -----------------------------------------------------------------------------

moveFocus ::
  (Ui :> es) =>
  GridEnv es ->
  Word64 ->
  (Float, Float) ->
  Eff es ()
moveFocus env cur dir =
  case neighborPane (geRegions env) cur dir of
    Just pid -> putPaneSlot False SlotPaneFocus env pid
    Nothing -> pure ()

neighborPane :: Map Word64 Rect -> Word64 -> (Float, Float) -> Maybe Word64
neighborPane regions cur (dx, dy) =
  case M.lookup cur regions of
    Nothing -> Nothing
    Just curR ->
      let (cx, cy) = centerOf curR
          scored =
            [ (pid, s)
            | (pid, r) <- M.toList regions
            , pid /= cur
            , rectNonEmpty r
            , let (px, py) = centerOf r
                  vx = px - cx
                  vy = py - cy
                  dotv = vx * dx + vy * dy
            , dotv > 0
            , let s = abs (vx * dy - vy * dx) / dotv
            ]
       in case scored of
            [] -> Nothing
            _ -> Just (fst (minimumBy (comparing snd) scored))

centerOf :: Rect -> (Float, Float)
centerOf r = (rectX r + rectW r / 2, rectY r + rectH r / 2)

-- -----------------------------------------------------------------------------
-- Store mutation helpers
-- -----------------------------------------------------------------------------

splitPane :: (Ui :> es) => GridEnv es -> Word64 -> GridAxis -> Eff es Word64
splitPane env pid axis = do
  let splitId = geSeed env
      newPane = geSeed env + 1
  putTree env (Just (treeSplit pid splitId axis False newPane (geTree env)))
  putSeed env (geSeed env + 2)
  putPaneSlot False SlotPaneFocus env newPane
  pure newPane

closePane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
closePane env pid =
  case treeRemovePane pid (geTree env) of
    Nothing -> putTree env Nothing
    Just t' -> do
      putTree env (Just t')
      when (geMax env == pid) (putPaneSlot True SlotPaneMax env 0)

maximizePane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
maximizePane env pid = do
  let v = if geMax env == pid then 0 else pid
  putPaneSlot True SlotPaneMax env v
  -- Maximizing hides the dividers and every other pane, so an armed drag or
  -- resize gesture could never complete; cancel it instead of leaking it.
  when (v /= 0) (writeGest env 0)

restorePane :: (Ui :> es) => GridEnv es -> Eff es ()
restorePane env = putPaneSlot True SlotPaneMax env 0

-- | One store round-trip. @mirror@ bumps the mirror generation so the
-- running frame rebuilds its UI and layout with the new value (see
-- 'NanoUI.Internal.Frame'); the store write itself wakes the renderer.
storeWrite ::
  (Ui :> es) =>
  GridEnv es ->
  Bool ->
  (WidgetStore -> WidgetStore) ->
  Eff es ()
storeWrite env mirror f =
  uiIO $ modifyStore (geCtx env) ((if mirror then bumpMirror else id) . f)

-- | Flag 'pgrChanged' for this frame.
markChanged :: (Ui :> es) => GridEnv es -> Eff es ()
markChanged env = uiIO (writeIORef (geChangedRef env) True)

-- | Structural change (mirror + 'pgrChanged'): store the tree, or remove it
-- entirely when the last pane was closed. The pane-id seed keeps counting
-- across a removal, so the re-seeded pane gets a fresh id and state keyed by
-- pane id never collides with a closed pane's state.
putTree :: (Ui :> es) => GridEnv es -> Maybe GridNode -> Eff es ()
putTree env mTree = do
  storeWrite env True (maybe (deleteSlot fieldDyn (geKey env)) (insertDyn (geKey env)) mTree)
  markChanged env

-- | Gesture slot: 0 none, positive = dragged pane id, negative = resized
-- split id.
writeGest :: (Ui :> es) => GridEnv es -> Int -> Eff es ()
writeGest env n = storeWrite env True (if n == 0 then deleteSlot fieldInt k else insertSlot fieldInt k n)
  where
    k = slotKey SlotPaneGest (geKey env)

-- | Write a pane-id slot (maximized or focused pane) when it differs,
-- bumping the mirror; @structural@ also flags 'pgrChanged'.
putPaneSlot :: (Ui :> es) => Bool -> Slot -> GridEnv es -> Word64 -> Eff es ()
putPaneSlot structural slot env v = do
  let k = slotKey slot (geKey env)
      n = fromIntegral v
  st <- uiIO (getStore (geCtx env))
  when (findSlot fieldInt 0 k st /= n) $ do
    storeWrite env True (insertSlot fieldInt k n)
    when structural (markChanged env)

-- | Advance the next-id seed ('SlotPaneNext').
putSeed :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
putSeed env v =
  storeWrite env False (insertSlot fieldInt (slotKey SlotPaneNext (geKey env)) (fromIntegral v))
