-- | Interactive pane grid with resizable dividers, modelled on iced's
-- @PaneGrid@. The layout is a binary split tree ('GridNode') kept in the
-- widget store.
--
-- 'pgViewPane' renders each pane and receives a 'PaneGridCtx' with actions
-- to split, close, maximize or restore it. Drag a divider to resize. Drag a
-- pane by its handle ('paneDragHandle') onto another pane (center swaps,
-- edge splits) or onto the grid's outer edge (new top-level split). While
-- the grid has focus, arrow keys move between panes, @m@ maximizes, @x@
-- closes and Escape restores. 'pgrTree' reads the tree back and 'pgTree'
-- replaces it.
--
-- During a drag the grid is laid out as the drop would leave it, with the
-- landing slot empty and highlighted. Releasing commits exactly that tree.
-- Releasing outside the grid cancels the drag.
--
-- For external sources, 'pgrDropTarget' reports a destination and preview
-- rectangle. The application resolves target priority and calls
-- 'commitPaneDrop' only when it chooses to create a pane.
module NanoUI.Widgets.PaneGrid
  ( GridAxis (..)
  , GridNode (..)
  , PaneTree (..)
  , validateGridTree
  , PaneGridConfig (..)
  , defaultPaneGridConfig
  , PaneGridCtx (pgcPaneId, pgcRect, pgcMaximized, pgcDragging, pgcDndActive, pgcSplit, pgcClose, pgcMaximize, pgcRestore)
  , PaneView (..)
  , PaneGridResponse (..)
  , PaneEdge (..)
  , PaneDropLocation (..)
  , PaneGridDrop, pgdLocation, pgdRect, pgdPane
  , commitPaneDrop
  , paneGrid
  , paneDragHandle
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Hashable (hash)
import Data.List (find)
import Data.Set qualified as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (NanoUI, (<&&>), askContext, askInput, damageWidgetNow, focusedWidget, freshWidget, lastRect, releaseFocus, requestFrame, liftIO, takeEscape, whenM, withIdFrame, withKey)
import NanoUI.Internal.Id (IdContext (..), WidgetId, hashWidgetId)
import NanoUI.Internal.Frame.Hit (nodeInteractionHit)
import NanoUI.Internal.Frame.Scroll.Geometry (padContentClip)
import NanoUI.Internal.Shortcut qualified as Shortcut
import NanoUI.Internal.Store (fieldGrid, insertSlot, lookupSlot)
import NanoUI.Internal.Store.Types (GridState (..), Gesture (..))
import NanoUI.Internal.Style
import NanoUI.Internal.Types
import NanoUI.Internal.Widgets.Behavior (KeyNav (..), dragThresholdPx, useKeyNav)
import NanoUI.Internal.Widgets.Cursor (withCursorShape)
import NanoUI.Widgets.Custom
import NanoUI.Internal.Widgets.Layout (column', row', rowWith')
import NanoUI.Internal.Layout.Arena (NodeType (..), arenaCount, getNodeType, getWidgetId, isWidgetNode)
import NanoUI.Internal.Widgets.Node
import NanoUI.Internal.Widgets.Shortcut (shortcutOnce)
import NanoUI.Internal.Widgets.SplitPane

-- -----------------------------------------------------------------------------
-- Public API
-- -----------------------------------------------------------------------------

-- | Who owns the arrangement. Initial trees are read only when this widget
-- identity first appears; controlled trees are authoritative on every pass.
-- In either mode Nothing is an empty workspace, including after the last close.
data PaneTree = InitialTree !(Maybe GridNode) | ControlledTree !(Maybe GridNode)
  deriving (Eq, Show)

-- | Validate a persisted/application-built tree before using it. Pane and split
-- ids share one namespace; ratios must be finite and between zero and one.
-- 'paneGrid' also validates any tree it adopts, failing before rendering it.
validateGridTree :: GridNode -> Either String ()
validateGridTree tree = void (walk S.empty tree)
  where
    insertId seen ident
      | ident == 0 || ident >= 2 ^ (63 :: Int) = Left "pane grid: ids must be in [1, 2^63)"
      | S.member ident seen = Left "pane grid: duplicate pane or split id"
      | otherwise = Right (S.insert ident seen)
    walk seen (Pane ident) = insertId seen ident
    walk seen (Split ident _ ratio left right) = do
      next <- insertId seen ident
      if isNaN ratio || isInfinite ratio || ratio < 0 || ratio > 1
        then Left "pane grid: ratio must be finite and in [0, 1]"
        else walk next left >>= (`walk` right)

-- | Configuration for a pane grid.
data PaneGridConfig = PaneGridConfig
  { pgLayout :: !(Layout -> Layout)
    -- ^ Layout modifier for the grid container (default 'id'); pass
    -- @fillW . fillH@ to fill the parent area.
  , pgSpacing :: !Float
    -- ^ Gutter between panes per split level (default 4).
  , pgMinSize :: !Float
    -- ^ Minimum size in logical pixels any pane may shrink to (default 40).
  , pgLeeway :: !Float
    -- ^ Extra grab margin on each side of a divider (default 6). The divider's
    -- gutter is 'pgSpacing' plus this margin on both sides. The whole gutter
    -- grabs and shows the resize cursor; only 'pgSpacing' is drawn solid.
  , pgEdgeBand :: !Float
    -- ^ Width of the drop zone along the grid's outer edge (default 20). A
    -- drop there wraps the tree in a new top-level split with the dragged
    -- pane on that side.
  , pgPreserveDragSize :: !Bool
    -- ^ Keep the dragged pane's size along its old parent split's axis,
    -- swapping width and height when the drop changes orientation, within the
    -- available space and minimum sizes (default 'False': the target splits
    -- evenly). Center swaps are unaffected.
  , pgFixedPanes :: !(Word64 -> Bool)
    -- ^ Panes that keep their size when the grid is resized (default
    -- @const False@). A pinned pane keeps its extent along its parent split's
    -- axis and the other side absorbs the change. It still yields to its
    -- neighbour's minimum, and its divider can still be dragged. Only that one
    -- split is pinned: a pinned sidebar keeps its width, but its row's height
    -- still follows the grid.
  , pgTree :: !PaneTree
    -- ^ 'InitialTree' seeds internally owned state once. 'ControlledTree'
    -- adopts the caller's arrangement on every pass: retain 'pgrTree' to accept
    -- an edit, or keep the old value to reject it. A replacement cancels any
    -- gesture on the replaced arrangement. Both modes permit an empty grid.
    --
    -- Allocate @arrangementCell <- newState (Just startLayout)@ during setup,
    -- then read and update it in the view:
    --
    -- > (arrangement, setArrangement) <- useState arrangementCell
    -- > resp <- paneGrid cfg {pgTree = ControlledTree arrangement}
    -- > setArrangement (pgrTree resp)
    -- > whenM (button "Reset layout") (setArrangement (Just startLayout))
    --
    -- Closing the last pane leaves the grid empty until the caller supplies
    -- another controlled tree or changes the grid's widget identity.
    --
    -- Pane and split ids are the caller's: unique within the tree, from 1 to
    -- below @2^63@ (0 means "none"). Panes the grid creates later get higher
    -- ids. Ratios apply from the first frame, and pinned panes keep the size
    -- their ratio gives them:
    --
    -- > pgTree = InitialTree (Just (Split 3 AxisV 0.25 (Pane 1) (Pane 2)))
  , pgDividerColor :: !(Maybe Color)
    -- ^ The colour of the gutters between panes (default 'Nothing': a faint
    -- tint of the panel colour with a line down the middle from
    -- 'themeSeparator'). The hover and drag highlight is drawn over it.
  , pgFocusable :: !Bool
    -- ^ Whether the grid is a Tab stop whose arrow, @m@, @x@ and Escape keys
    -- act on its panes (default 'True'). Turn it off when pane content needs
    -- those keys.
  , pgViewPane :: !(Word64 -> PaneGridCtx -> NanoUI PaneView)
    -- ^ Renders the content of one pane. The pane keeps the size the split
    -- gives it whatever its content needs, and clips what does not fit.
  }

-- | Default spacing and drag margins with empty pane bodies. Set 'pgViewPane'
-- to render application content and 'pgLayout' to constrain the grid.
defaultPaneGridConfig :: PaneGridConfig
defaultPaneGridConfig =
  PaneGridConfig
    { pgLayout = id
    , pgSpacing = 4
    , pgMinSize = 40
    , pgLeeway = 6
    , pgEdgeBand = 20
    , pgPreserveDragSize = False
    , pgFixedPanes = const False
    , pgTree = InitialTree (Just (Pane 1))
    , pgDividerColor = Nothing
    , pgFocusable = True
    , pgViewPane = \_ _ -> pure (PaneView "" False)
    }

-- | Actions handed to a pane so it can mutate the grid immediately.
data PaneGridCtx = PaneGridCtx
  { pgcPaneId :: !Word64
  , pgcRect :: !Rect
    -- ^ This pane's screen rect from the previous frame. Zero until laid
    -- out, the grid's content rect while maximized, and the previewed rect
    -- during a drop.
  , pgcMaximized :: !Bool
    -- ^ True when this pane currently fills the whole grid.
  , pgcDragging :: !Bool
    -- ^ True while this pane's drag is armed. Once past the drag threshold,
    -- the pane is not rendered until release.
  , pgcDndActive :: !Bool
    -- ^ True while any pane drag-and-drop gesture is in progress.
  , pgcSplit :: !(GridAxis -> NanoUI Word64)
    -- ^ Split this pane along the axis; returns the new pane id.
  , pgcClose :: !(NanoUI ())
  , pgcMaximize :: !(NanoUI ())
  , pgcRestore :: !(NanoUI ())
  , pgcHandle :: !(IORef (Maybe Rect))
    -- ^ Where the pane's 'paneDragHandle' was laid out last frame.
  }

-- | How a pane can be dragged this frame. 'pgViewPane' draws all of the
-- pane's content, including any title bar.
data PaneView = PaneView
  { pvTitle :: !Text
    -- ^ Label shown (abbreviated to fit) on the compact drag indicator.
  , pvDraggable :: !Bool
    -- ^ A press anywhere in the pane not taken by an interactive child starts
    -- a drag. Without it, only a press on the pane's 'paneDragHandle' does,
    -- and a pane with neither cannot be moved.
  }
  deriving (Eq, Show)

-- | The grid's state after this frame, including pane actions and keys
-- handled during it. The layout reflects them from the next frame.
data PaneGridResponse = PaneGridResponse
  { pgrChanged :: !Bool
    -- ^ Any structural or maximize change happened this frame, including
    -- each step of a divider drag.
  , pgrCommitted :: !Bool
    -- ^ The split tree changed and nothing is being dragged: a divider or a
    -- pane was let go after moving, or a split or close ran. The time to
    -- save 'pgrTree'. Not set for a maximize or restore, which leave the
    -- tree as it is, nor for a tree the caller passed in 'pgTree'.
  , pgrTree :: !(Maybe GridNode)
    -- ^ The split tree, 'Nothing' once the last pane has been closed. Pass
    -- it back as 'ControlledTree' in 'pgTree' to accept changes.
  , pgrPaneCount :: !Int
    -- ^ Number of panes (0 once the last pane has been closed).
  , pgrPanes :: ![Word64]
    -- ^ Live pane ids, depth-first.
  , pgrFocusedPane :: !Word64
    -- ^ Focused pane id, 0 when the grid has no panes.
  , pgrMaximizedPane :: !Word64
    -- ^ Maximized pane id, 0 when none.
  , pgrDropTarget :: !(Maybe PaneGridDrop)
    -- ^ Destination at the pointer, independent of any drag. Centers, gutters,
    -- covered/clipped areas, maximization and internal gestures yield Nothing.
    -- Resolve target priority before calling 'commitPaneDrop'.
  }
  deriving (Eq, Show)

data PaneEdge = PaneLeft | PaneRight | PaneAbove | PaneBelow
  deriving (Eq, Show)

-- | Split beside one pane, or around the whole grid.
data PaneDropLocation
  = BesidePane !Word64 !PaneEdge
  | OutsideGrid !PaneEdge
  deriving (Eq, Show)

-- | A destination and its proposed new pane rectangle. Querying it neither
-- paints a preview nor changes the grid. Keep it within the current context;
-- commits reject an already-used target or a changed grid snapshot.
data PaneGridDrop = PaneGridDrop !PaneDropLocation Rect !Word64 !WidgetId !GridState GridNode
  deriving (Eq, Show)

pgdLocation :: PaneGridDrop -> PaneDropLocation
pgdLocation (PaneGridDrop location _ _ _ _ _) = location

-- | Preview rectangle in window coordinates, laid out only when read.
pgdRect :: PaneGridDrop -> Rect
pgdRect (PaneGridDrop _ rect _ _ _ _) = rect

-- | Pane under the pointer, also at outer edges; use it to reject destinations.
pgdPane :: PaneGridDrop -> Word64
pgdPane (PaneGridDrop _ _ pane _ _ _) = pane

-- | Insert and focus a fresh pane at an accepted destination. Returns its id
-- and the committed tree, or Nothing if the grid changed since the target
-- was obtained. The application decides when to call this (normally on
-- DragReleased), assigns the content, and saves the returned tree if controlled.
-- The earlier 'PaneGridResponse' remains a pre-commit snapshot.
commitPaneDrop :: PaneGridDrop -> NanoUI (Maybe (Word64, GridNode))
commitPaneDrop (PaneGridDrop _ _ _ wid expected tree) = do
  ctx <- askContext
  let key = intKey wid
      pane = gsSeed expected + 1
  current <- lookupSlot fieldGrid key <$> liftIO (getStore ctx)
  if current /= Just expected
    then pure Nothing
    else do
      liftIO . modifyStore ctx . insertSlot fieldGrid key $
        expected {gsTree = Just tree, gsSeed = pane + 1, gsFocus = pane}
      pure (Just (pane, tree))

-- -----------------------------------------------------------------------------
-- Internal state
-- -----------------------------------------------------------------------------

data RenderedPane = RenderedPane
  { rpPaneId :: !Word64
  , rpView :: !PaneView
  , rpControlHit :: !Bool
  , rpHandle :: !(Maybe Rect)
    -- ^ Where the pane's 'paneDragHandle' was laid out last frame.
  }

-- | Per-frame shared environment.
data GridEnv = GridEnv
  { geCtx :: !Context
  , geKey :: !Int
  , gePaneScope :: !IdContext
    -- ^ Pane ids are rooted at the grid, not the split tree, so rearranging
    -- splits keeps pane state.
  , geCfg :: !(PaneGridConfig)
  , geGutter :: !Float
  , geThickness :: !Float
  , geMinSize :: !Float
  , geLeeway :: !Float
  , geRegions :: !(Map Word64 Rect)
    -- ^ Prev-frame pane regions; drives hit testing and 'pgcRect'.
  , geState :: !GridState
    -- ^ The state this frame was built from.
  , geLifted :: !Bool
    -- ^ The dragged pane is lifted out: not rendered, and its slot left
    -- empty in a drop preview.
  , geMakeCtx :: Word64 -> Rect -> Bool -> IORef (Maybe Rect) -> PaneGridCtx
  }

-- -----------------------------------------------------------------------------
-- Tree + focus state
-- -----------------------------------------------------------------------------

-- | The maximized pane (0 = none) and the focused pane of a state over its
-- tree. A stored pane id counts while the pane still exists; a maximized pane
-- has the focus, and with no stored focus the first pane does.
paneFocus :: GridNode -> GridState -> (Word64, Word64)
paneFocus tree g = (maxPane, focus)
  where
    maxPane = if paneExist tree (gsMax g) then gsMax g else 0
    focus
      | maxPane /= 0 = maxPane
      | paneExist tree (gsFocus g) = gsFocus g
      | otherwise = fromMaybe 1 (listToMaybe (treePanes tree))

-- -----------------------------------------------------------------------------
-- Entry point
-- -----------------------------------------------------------------------------

-- | Stateful split-pane workspace with draggable tabs and dividers. Keep its
-- widget identity stable; pane callbacks receive actions for splits, closes,
-- and maximisation through 'PaneGridCtx'.
paneGrid :: PaneGridConfig -> NanoUI PaneGridResponse
paneGrid cfg = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  -- If Tab focus was turned off, drop any focus the grid still holds.
  if pgFocusable cfg then liftIO (registerFocusable ctx wid) else releaseFocus wid
  let key = intKey wid
      spacing = max 0 (pgSpacing cfg)
      minSize = max 0 (pgMinSize cfg)
      leeway = max 0 (pgLeeway cfg)
      edgeBand = max 0 (pgEdgeBand cfg)
      gutter = spacing + 2 * leeway
  stored <- lookupSlot fieldGrid key <$> liftIO (getStore ctx)
  let given = case pgTree cfg of InitialTree t -> t; ControlledTree t -> t
      adopt = case pgTree cfg of
        InitialTree _ -> maybe True (const False) stored
        ControlledTree _ -> maybe True ((/= given) . gsTree) stored
  when adopt $ forM_ given $ \t ->
    either (liftIO . fail) pure (validateGridTree t)
  Rect ox oy ow oh <- fromMaybe (Rect 0 0 0 0) <$> lastRect wid
  let lay = pgLayout cfg (paneLay minSize)
      -- The panes share the grid's content box, inside its padding.
      baseRect = padContentClip ox oy ow oh (layoutPadding lay)
      st = fromMaybe (GridState Nothing 1 0 0 Nothing NoGesture) stored
      started
        | adopt = st {gsTree = given, gsSeed = max (gsSeed st) (maybe 1 ((+ 1) . treeMaxId) given), gsGesture = NoGesture}
        | otherwise = st
      curSpan = (rectW baseRect, rectH baseRect)
      -- On a size change, re-ratio pinned panes' splits before layout. A move
      -- or a first fit needs no reflow.
      reflow tree0 = case gsSpan started of
        Just (pw, ph)
          | (pw, ph) /= curSpan
          , any (pgFixedPanes cfg) (treePanes tree0) ->
              reflowFixed (pgFixedPanes cfg) minSize gutter (baseRect {rectW = pw, rectH = ph}) baseRect tree0
        _ -> tree0
      arrangement = reflow <$> gsTree started
      gs = started {gsTree = arrangement, gsSpan = Just curSpan}
  when (Just gs /= stored) $ liftIO (modifyStore ctx (insertSlot fieldGrid key gs))
  let (maxPane, focused) = maybe (0, 0) (`paneFocus` gs) arrangement
      mouse = inputMousePos inp
      (regions, dividers) = maybe (M.empty, []) (\t -> layoutNode minSize gutter t baseRect) arrangement
      -- Drop geometry, computed while the drag is armed so the release frame
      -- can still resolve it. The target is the outer edge band, else the
      -- pane nearest the pointer, else nothing outside the grid. Hit tests
      -- use the grid without the dragged pane, not the on-screen preview, so
      -- showing the preview cannot change the target. 'dropPreviewTreeSized'
      -- lays out the result of the drop.
      (dragMoved, dragZone, remaining) = case (arrangement, gsGesture gs) of
        (Just tree, Drag pid press moved0 _) ->
          let mFrom = M.lookup pid regions
              V2 dx dy = v2Sub mouse press
              moved = moved0 || dx * dx + dy * dy > dragThresholdPx * dragThresholdPx
              rest = treeRemovePane pid tree
              targetRegions = maybe M.empty (\t -> fst (layoutNode minSize gutter t baseRect)) rest
              preview = dropPreviewTreeSized (if pgPreserveDragSize cfg then mFrom else Nothing) minSize gutter tree pid (gsSeed gs) baseRect
              zone = case topLevelDropTarget edgeBand baseRect mouse of
                Just dt -> preview dt
                Nothing
                  | rectHit baseRect mouse ->
                      nearestPane targetRegions mouse >>= \(q, r) -> preview (dropTargetForPane r mouse q)
                  | otherwise -> Nothing
           in (moved, zone, rest)
        _ -> (False, Nothing, Nothing)
      lifted = dragMoved && heldIn MouseLeft inp
      -- The store keeps the committed tree so a drag can be cancelled. The
      -- screen shows the post-drop tree with an empty slot, or the tree
      -- without the dragged pane when there is no target.
      (visibleTree, (visibleRegions, visibleDividers))
        | not lifted = (arrangement, (regions, dividers))
        | Just dp <- dragZone = (Just (dpTree dp), (dpRegions dp, dpDividers dp))
        | otherwise = (remaining, maybe (M.empty, []) (\t -> layoutNode minSize gutter t baseRect) remaining)
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
          , geState = gs
          , geLifted = lifted
          , geMakeCtx = \pid rect dragging handle ->
              PaneGridCtx
                { pgcPaneId = pid
                , pgcRect = rect
                , pgcMaximized = maxPane == pid
                , pgcDragging = dragging
                , pgcDndActive = dragMoved
                , pgcSplit = splitPane env pid
                , pgcClose = closePane env pid
                , pgcMaximize = maximizePane env pid
                , pgcRestore = restorePane env
                , pgcHandle = handle
                }
          }

  -- Tagged so next frame reads the grid's rect.
  container NodeContainer lay $ do
    tagContainer wid
    if maxPane /= 0
      then void (renderPane env maxPane baseRect)
      else do
        rendered <- maybe (pure []) (renderNode env (M.fromList [(diSplitId d, d) | d <- visibleDividers])) visibleTree
        runGestures wid env dividers rendered dragMoved dragZone
        when (lifted && rectNonEmpty baseRect) $
          drawDragOverlay env wid mouse (dpRect <$> dragZone)
        -- Keyboard focus also rings the focused pane, so the arrow keys show
        -- where they moved; the grid's own ring says the grid holds focus.
        ringPane <- liftIO ((&&) <$> getFocusVisible ctx <*> ((== wid) <$> getFocusId ctx))
        when (ringPane && not lifted) $
          forM_ (M.lookup focused visibleRegions) $ \r ->
            liftIO $ registerCustomDrawing ctx wid (contentKey [1, rectX r, rectY r, rectW r, rectH r]) $ \cdc _ ->
              runCanvasFor cdc (drawStrokeRoundedRect (rectInflate (-2) r) 2 1.5 (themeAccent (cdcTheme cdc)))

  -- Keys for the focused grid. Escape restores a maximized pane and is
  -- consumed, unless something earlier (such as a popup in a pane) took it
  -- or it closes a menu or dropdown ('takeEscape').
  focusedNow <- focusedWidget
  when (pgFocusable cfg && focusedNow == wid) $ do
    nav <- useKeyNav wid
    forM_ [(knLeft, (-1, 0)), (knRight, (1, 0)), (knUp, (0, -1)), (knDown, (0, 1))] $
      \(k, dir) -> when (k nav) (moveFocus env focused dir)
    when (knLeft nav || knRight nav || knUp nav || knDown nav) $
      damageWidgetNow wid (DamageInflated 0)
    -- Unmodified letters, taken as a 'shortcutOnce': a shortcut declared
    -- earlier for the same key keeps it from the grid, and the grid keeps it
    -- from one declared later.
    whenM (shortcutOnce (Shortcut.key 'm')) (maximizePane env focused)
    whenM (shortcutOnce (Shortcut.key 'x')) (closePane env focused)
    whenM takeEscape (restorePane env)

  end <- fromMaybe gs . lookupSlot fieldGrid key <$> liftIO (getStore ctx)
  covered <- liftIO (pointerCovered ctx wid)
  clip <- liftIO (getPrevClipRect ctx wid)
  let dropTarget
        | covered || not (maybe True (`rectHit` mouse) clip) = Nothing
        | gsGesture gs /= NoGesture || gsGesture end /= NoGesture = Nothing
        | maxPane /= 0 || gsMax end /= 0 || gsTree end /= gsTree gs = Nothing
        | not (rectHit baseRect mouse) = Nothing
        | otherwise = do
            tree <- arrangement
            (q, r) <- find (\(_, r) -> rectHit r mouse) (M.toList regions)
            let seed = gsSeed end
                np = seed + 1
                edge AxisV True = PaneLeft
                edge AxisV False = PaneRight
                edge AxisH True = PaneAbove
                edge AxisH False = PaneBelow
            (location, proposed) <- case fromMaybe (dropTargetForPane r mouse q) (topLevelDropTarget edgeBand baseRect mouse) of
              DropSplit p ax onA -> Just (BesidePane p (edge ax onA), treeSplit p seed ax onA np tree)
              DropTop ax onA -> Just (OutsideGrid (edge ax onA), if onA
                then Split seed ax 0.5 (Pane np) tree
                else Split seed ax 0.5 tree (Pane np))
              DropSwap _ -> Nothing
            let rect = M.findWithDefault (Rect 0 0 0 0) np (fst (layoutNode minSize gutter proposed baseRect))
            pure (PaneGridDrop location rect q wid end proposed)
      (maxEnd, focusEnd) = maybe (0, 0) (`paneFocus` end) (gsTree end)
      -- A divider drag moves the tree on the frames it is held, so the frame
      -- that lets it go compares with the ratio it was pressed at.
      resized = case gsGesture gs of
        Resize sid ratio0 _ -> (treeSetRatio sid ratio0 <$> gsTree gs) /= gsTree gs
        _ -> False
  pure
    PaneGridResponse
      { pgrChanged = gsTree end /= gsTree gs || gsMax end /= gsMax gs
      , pgrCommitted = gsGesture end == NoGesture && (gsTree end /= gsTree gs || resized)
      , pgrTree = gsTree end
      , pgrPaneCount = maybe 0 treeSize (gsTree end)
      , pgrPanes = maybe [] treePanes (gsTree end)
      , pgrFocusedPane = focusEnd
      , pgrMaximizedPane = maxEnd
      , pgrDropTarget = dropTarget
      }

-- | A row, laid out like 'rowWith', that drags its pane: a press on it not
-- taken by a control inside it starts a drag, and the pointer shows a grab
-- cursor over it. Wrap a pane's title bar in it:
--
-- > paneDragHandle pctx (fillW . alignMid) $ do
-- >   label title
-- >   flex
-- >   whenM (button "Close") (pgcClose pctx)
paneDragHandle :: PaneGridCtx -> (Layout -> Layout) -> NanoUI a -> NanoUI a
paneDragHandle pctx f body = withCursorShape UiCursorGrab $ do
  (a, handle) <- rowWith' f body
  liftIO (writeIORef (pgcHandle pctx) (Just (respRect handle)))
  pure a

-- -----------------------------------------------------------------------------
-- Layout helpers
-- -----------------------------------------------------------------------------

-- | @s@ along the axis's main direction, growing across it, with no gap or
-- padding.
axisLay :: GridAxis -> Sizing -> Layout
axisLay ax s = tight . gap 0 $ case ax of
  AxisV -> defaultLayout {layoutWidth = s, layoutHeight = Grow 1}
  AxisH -> defaultLayout {layoutWidth = Grow 1, layoutHeight = s}

-- | Zero-gap, zero-padding, grow-to-fill layout.
fillLay :: Layout
fillLay = axisLay AxisV (Grow 1)

-- | The pane content wrapper: fills its cell, never below one minimum pane.
paneLay :: Float -> Layout
paneLay m = minW m (minH m fillLay)

-- -----------------------------------------------------------------------------
-- Rendering
-- -----------------------------------------------------------------------------

-- | Enter a pane's grid-relative id scope. Consumes one sibling slot, like
-- 'withKey'.
withPaneKey :: GridEnv -> Word64 -> NanoUI a -> NanoUI a
withPaneKey env pid =
  withIdFrame (\parent -> (parent {siblingId = siblingId parent + 1}, gePaneScope env)) . withKey pid

-- | Render one pane's content via 'pgViewPane' under the pane's stable key.
renderPane ::
  GridEnv ->
  Word64 ->
  Rect ->
  NanoUI [RenderedPane]
renderPane env pid rect =
  withPaneKey env pid $ do
    inp <- askInput
    let ctx = geCtx env
        arena = ctxNodeArena ctx
    start <- liftIO (arenaCount arena)
    handleRef <- liftIO (newIORef Nothing)
    let ctxt = geMakeCtx env pid rect (draggingPane env pid) handleRef
    view <- clipContainer (paneLay (geMinSize env)) (pgViewPane (geCfg env) pid ctxt)
    -- Whether a press landed on a control in the pane, using last frame's
    -- rects, since the active id is only settled after the view runs.
    controlHit <-
      if not (pressedIn MouseLeft inp)
        then pure False
        else liftIO $ do
          end <- arenaCount arena
          let hitFrom idx
                | idx >= end = pure False
                | otherwise = do
                    nt <- getNodeType arena idx
                    hit <- pure (isWidgetNode nt) <&&> do
                      child <- getWidgetId arena idx
                      r <- getPrevRect ctx child
                      maybe (pure False) (\childRect -> nodeInteractionHit ctx idx childRect (inputMousePos inp)) r
                    if hit then pure True else hitFrom (idx + 1)
          hitFrom start
    handleRect <- liftIO (readIORef handleRef)
    pure [RenderedPane pid view controlHit handleRect]

renderNode ::
  GridEnv ->
  Map Word64 DividerInfo ->
  GridNode ->
  NanoUI [RenderedPane]
renderNode env dividers = \case
  Pane pid
    -- The lifted pane's landing slot in a drop preview: an empty cell of the
    -- pane's size, which the drop-zone highlight fills.
    | geLifted env && draggingPane env pid ->
        [] <$ container NodeContainer (paneLay (geMinSize env)) (pure ())
    -- Its prev-frame rect, zero until the pane has been laid out once.
    | otherwise ->
        renderPane env pid (M.findWithDefault (Rect 0 0 0 0) pid (geRegions env))
  Split sid ax ratio a b ->
    withKey sid $ do
      let (wa, ha) = subtreeMin (geMinSize env) (geGutter env) a
          (wb, hb) = subtreeMin (geMinSize env) (geGutter env) b
          mDiv = M.lookup sid dividers
          avail = maybe 0 (mainLen ax . diRegion) mDiv
          -- The A side's extent after 'layoutNode' clamped it.
          dA = maybe 0 (\d -> dividerLength (geGutter env) d ratio) mDiv
          pinA = pinnedSide (pgFixedPanes (geCfg env)) a
          pinB = pinnedSide (pgFixedPanes (geCfg env)) b
          -- A zero weight would not grow at all.
          weight w = axisLay ax (Grow (max 1.0e-3 w))
          (aSide, bSide)
            -- Not laid out yet (first frame or new split): split the space by
            -- the ratio.
            | avail <= 0 = (weight (clamp01 ratio), weight (1 - clamp01 ratio))
            -- A percent for A, the rest for B, so both follow a resize.
            | pinA == pinB = (axisLay ax (Percent (dA / avail * 100)), fillLay)
            -- A pinned side keeps its pixel length; 'reflowFixed' updates the
            -- ratio next frame.
            | pinA = (axisLay ax (Fixed (max 0 dA)), fillLay)
            | otherwise = (fillLay, axisLay ax (Fixed (max 0 (avail - dA - geGutter env))))
      (if ax == AxisV then row' else column') fillLay $ do
        a' <- container NodeContainer (minW wa (minH ha aSide)) (renderNode env dividers a)
        dividerWidget env ax
        b' <- container NodeContainer (minW wb (minH hb bSide)) (renderNode env dividers b)
        pure (a' <> b')

-- | Is this the pane being drag-and-dropped?
draggingPane :: GridEnv -> Word64 -> Bool
draggingPane env pid = case gsGesture (geState env) of
  Drag p _ _ _ -> p == pid
  _ -> False

-- | The divider: a drawing that covers the whole gutter, so all of it grabs.
-- Painted as a faint rail with a solid 'geThickness' strip in the middle.
dividerWidget :: GridEnv -> GridAxis -> NanoUI ()
dividerWidget env axis =
  void $
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = axisLay axis (Fixed (geGutter env))
        , widgetContent = contentKeyOf [keyPart (axis == AxisV), keyPart thickness, keyPart leeway, keyPart (colorToWord32 <$> color)]
        , widgetDraw = \cdc rect -> runCanvasFor cdc $ do
            let theme = cdcTheme cdc
                panel = themePanel theme
                -- The gutter as a vertical one; its centre line as a
                -- zero-width rect down it.
                Rect x y w h = alongAxis axis rect
                Rect lx ly lw lh = alongAxis axis (Rect (x + w / 2) y 0 h)
                -- Full accent while grabbed; a calmer tint while merely hovering.
                line
                  | cdcPressed cdc = themeAccent theme
                  | otherwise = lerpColor (themeAccent theme) (fromMaybe (styleBg panel) color) 0.45
            case color of
              Just c -> drawRect rect c
              Nothing -> do
                drawRect rect (lerpColor (styleBg panel) (themeSeparator theme) 0.12)
                drawRect (alongAxis axis (Rect (x + leeway) y thickness h)) (scrollBarTrackColor panel theme)
            when (cdcHovered cdc || cdcPressed cdc) $
              drawStroke (V2 lx ly) (V2 (lx + lw) (ly + lh)) 2 line
        , widgetCursor = Just (\_ _ _ -> if axis == AxisV then UiCursorEwResize else UiCursorNsResize)
        }
  where
    thickness = geThickness env
    leeway = geLeeway env
    color = pgDividerColor (geCfg env)

-- | The drag indicator and drop-zone highlight, drawn over the grid by its
-- root container so they stay out of the layout. The indicator is small,
-- translucent and offset from the pointer so the preview stays visible.
drawDragOverlay :: GridEnv -> WidgetId -> V2 -> Maybe Rect -> NanoUI ()
drawDragOverlay env wid (V2 mx my) zone =
  liftIO $ registerCustomDrawing (geCtx env) wid key $ \cdc _ -> runCanvasFor cdc $ do
    let theme = cdcTheme cdc
        accent = themeAccent theme
        shortTitle = if T.length title > 12 then T.take 11 title <> "…" else title
    drawRoundedRect ghost 2 (fadeAlpha accent 48)
    drawStrokeRoundedRect ghost 2 2 (fadeAlpha accent 128)
    unless (T.null title) $
      drawText (V2 (mx + 18) (my + 18)) AlignStart AlignTop shortTitle (fadeAlpha (styleFg (themeFloatingWindow theme)) 160)
    forM_ zone $ \zr -> do
      drawRoundedRect zr 2 (fadeAlpha accent 32)
      drawStrokeRoundedRect (rectInflate (-1) zr) 2 2 accent
  where
    title = case gsGesture (geState env) of
      Drag _ _ _ t -> t
      _ -> ""
    ghost = Rect (mx + 12) (my + 12) 112 28
    key = contentKey (2 : fromIntegral (hash title) : mx : my : maybe [0, 0, 0, 0, 0] (\(Rect x y w h) -> [1, x, y, w, h]) zone)

-- -----------------------------------------------------------------------------
-- Gestures
-- -----------------------------------------------------------------------------

-- | Arm, run and finish the resize and drag gestures.
runGestures ::
  WidgetId ->
  GridEnv ->
  [DividerInfo] ->
  [RenderedPane] ->
  Bool ->
  Maybe DropPreview ->
  NanoUI ()
runGestures wid env dividers rendered moved zone = do
  inp <- askInput
  covered <- liftIO (pointerCovered (geCtx env) wid)
  clip <- liftIO (getPrevClipRect (geCtx env) wid)
  disabled <- liftIO (isDisabled (geCtx env) wid)
  let mouse = inputMousePos inp
      down = heldIn MouseLeft inp
      setGesture mirror g = void (updateGrid env mirror (\s -> s {gsGesture = g}))
      -- diBand already includes the leeway; inflating it would steal presses
      -- from neighbouring panes.
      hitDiv = find (\d -> rectHit (diBand d) mouse) dividers
      -- The pane whose handle (or, when 'pvDraggable', whole region) is
      -- under the pointer.
      pickHit = flip find rendered $ \pane ->
        let v = rpView pane
         in any (`rectHit` mouse) (rpHandle pane)
              || (pvDraggable v && any (`rectHit` mouse) (M.lookup (rpPaneId pane) (geRegions env)))
  when (pressedIn MouseLeft inp && not (covered || disabled)
      && maybe True (`rectHit` mouse) clip
      && gsGesture (geState env) == NoGesture && not (any rpControlHit rendered)) $
    case (hitDiv, pickHit) of
      (Just d, _) -> setGesture True (Resize (diSplitId d) (diRatio d) (mouseMain (diAxis d) mouse))
      (_, Just pane) -> setGesture True (Drag (rpPaneId pane) mouse False (pvTitle (rpView pane)))
      _ -> pure ()
  case gsGesture (geState env) of
    NoGesture -> pure ()
    Resize sid ratio0 main0
      | down ->
          forM_ (find ((== sid) . diSplitId) dividers) $ \d -> do
            -- The ratio shares out the region minus the divider gutter.
            let usable = mainLen (diAxis d) (diRegion d) - geGutter env
                r0 = if usable <= 0 then ratio0 else ratio0 + (mouseMain (diAxis d) mouse - main0) / usable
            void . updateGrid env True $ \s ->
              s {gsTree = treeSetRatio sid (clampRatio (geGutter env) d r0) <$> gsTree s}
      | otherwise -> setGesture True NoGesture
    Drag pid press moved0 title
      -- The indicator follows the pointer, so keep requesting frames.
      | down -> do
          requestFrame
          when (moved && not moved0) $ setGesture False (Drag pid press True title)
      -- One write clears the gesture and stores the previewed tree, which
      -- was built with this frame's seed.
      | otherwise ->
          void . updateGrid env True $ \s -> case zone of
            Just dp | moved -> s {gsGesture = NoGesture, gsTree = Just (dpTree dp), gsSeed = max (gsSeed s) (gsSeed (geState env) + 1), gsFocus = pid}
            _ -> s {gsGesture = NoGesture}

mouseMain :: GridAxis -> V2 -> Float
mouseMain AxisV = v2X
mouseMain AxisH = v2Y

-- -----------------------------------------------------------------------------
-- Keyboard navigation
-- -----------------------------------------------------------------------------

moveFocus :: GridEnv -> Word64 -> (Float, Float) -> NanoUI ()
moveFocus env cur dir =
  forM_ (neighborPane (geRegions env) cur dir) $ \p -> updateGrid env True (\s -> s {gsFocus = p})

-- | The pane nearest the line from @cur@'s center in direction @(dx, dy)@.
neighborPane :: Map Word64 Rect -> Word64 -> (Float, Float) -> Maybe Word64
neighborPane regions cur (dx, dy) = do
  (cx, cy) <- centerOf <$> M.lookup cur regions
  let score r =
        let (px, py) = centerOf r
            vx = px - cx
            vy = py - cy
            dotv = vx * dx + vy * dy
         in if dotv > 0 then Just (abs (vx * dy - vy * dx) / dotv) else Nothing
  fst <$> bestPane score (M.delete cur regions)

centerOf :: Rect -> (Float, Float)
centerOf r = (rectX r + rectW r / 2, rectY r + rectH r / 2)

-- -----------------------------------------------------------------------------
-- Store mutation helpers
-- -----------------------------------------------------------------------------

-- | Update the grid's stored state and return it. With @mirror@, the
-- running frame rebuilds its view from the new state. A no-op update leaves
-- the store untouched.
updateGrid :: GridEnv -> Bool -> (GridState -> GridState) -> NanoUI GridState
updateGrid env mirror f = liftIO $ do
  st <- getStore (geCtx env)
  let old = fromMaybe (geState env) (lookupSlot fieldGrid (geKey env) st)
      new = f old
  when (new /= old) $
    setStore (geCtx env) ((if mirror then bumpMirror else id) (insertSlot fieldGrid (geKey env) new st))
  pure new

-- | Split a pane, focusing and returning the new one.
splitPane :: GridEnv -> Word64 -> GridAxis -> NanoUI Word64
splitPane env pid axis =
  fmap gsFocus . updateGrid env True $ \s ->
    let seed = gsSeed s
     in s {gsTree = treeSplit pid seed axis False (seed + 1) <$> gsTree s, gsSeed = seed + 2, gsFocus = seed + 1}

closePane :: GridEnv -> Word64 -> NanoUI ()
closePane env pid =
  void . updateGrid env True $ \s ->
    s {gsTree = gsTree s >>= treeRemovePane pid, gsMax = if gsMax s == pid then 0 else gsMax s, gsGesture = NoGesture}

-- | Maximizing hides the dividers and every other pane, so an armed drag or
-- resize gesture could never complete; cancel it instead of leaking it.
maximizePane :: GridEnv -> Word64 -> NanoUI ()
maximizePane env pid =
  void . updateGrid env True $ \s ->
    if gsMax s == pid then s {gsMax = 0} else s {gsMax = pid, gsGesture = NoGesture}

restorePane :: GridEnv -> NanoUI ()
restorePane env = void (updateGrid env True (\s -> s {gsMax = 0}))
