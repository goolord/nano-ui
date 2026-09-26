-- | Interactive pane grid with resizable dividers, modelled on iced's
-- @PaneGrid@: a binary split tree ('GridNode') kept in the widget store.
--
-- 'pgViewPane' renders each pane and gets a 'PaneGridCtx' of actions to
-- split, close, maximize or restore it. Dividers drag to resize; a pane
-- grabbed by its pick rect drops onto another pane (center = swap, edge =
-- split) or onto the grid's outer edge (a new top-level split). While the
-- grid is focused, arrow keys move between panes, @m@/@x@ maximize/close and
-- Escape restores.
--
-- While a dragged pane hovers over a drop target the grid is laid out as the
-- drop will leave it, the landing slot empty under a highlight, and releasing
-- stores exactly that tree. Releasing outside the grid cancels the drag.
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
import Data.Hashable (hash)
import Data.List (find)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word64)
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Input
import NanoUI.Internal.Monad (Ui, (<&&>), askInput, damageWidgetNow, focusedWidget, freshWidget, lastRect, releaseFocus, requestFrame, uiIO, withIdFrame, withKey)
import NanoUI.Internal.Id (IdContext (..), WidgetId, hashWidgetId)
import NanoUI.Internal.Frame.Hit (nodeInteractionHit)
import NanoUI.Internal.Store (insertDyn, lookupDyn)
import NanoUI.Internal.Style
import NanoUI.Internal.Types
import NanoUI.Internal.Widgets.Behavior (KeyNav (..), dragThresholdPx, useKeyNav)
import NanoUI.Widgets.Custom
import NanoUI.Internal.Widgets.Layout (column', row')
import NanoUI.Internal.Layout.Arena (NodeType (..), arenaCount, getNodeType, getWidgetId, isWidgetNode)
import NanoUI.Internal.Widgets.Node
import NanoUI.Internal.Widgets.SplitPane

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
    -- form its layout gutter: the whole gutter grabs and shows the resize
    -- cursor, while only 'pgSpacing' is drawn crisp (default 6).
  , pgEdgeBand :: !Float
    -- ^ Thickness of the grid's outer edge that acts as a top-level drop zone:
    -- a drop there wraps the tree in a new top-level split with the dragged
    -- pane on that side (default 20).
  , pgPreserveDragSize :: !Bool
    -- ^ Keep the dragged pane's extent along its original parent split's
    -- axis, moving width to height (or back) when the drop changes
    -- orientation, within the space and subtree minima. Center swaps are
    -- unaffected (default 'False': the destination splits equally).
  , pgFixedPanes :: !(Word64 -> Bool)
    -- ^ Panes that keep their size when the grid's rect changes size
    -- (default @const False@). A pinned pane holds its extent along its
    -- parent split's axis and the other side of that split takes the
    -- difference; it still gives way to its neighbour's minimum, and its
    -- divider still drags. Only the pane's own split is pinned, so a pinned
    -- sidebar keeps its width without freezing the height of its row.
  , pgInitial :: !(Maybe GridNode)
    -- ^ The split tree the grid starts from on its first frame (default
    -- 'Nothing', a single pane); a grid whose last pane closes starts again
    -- from one fresh pane. Pane and split ids are the caller's: unique within
    -- the tree, from 1 and below @2^63@ (0 is the grid's "none"); panes the
    -- grid makes later take ids above all of them. Ratios apply from the
    -- first frame, and a pinned pane keeps the extent its ratio gives it:
    --
    -- > pgInitial = Just (Split 3 AxisV 0.25 (Pane 1) (Pane 2))
  , pgFocusable :: !Bool
    -- ^ Whether the grid is a Tab stop whose arrow, @m@, @x@ and Escape keys
    -- act on its panes (default 'True'). Turn it off when the panes' content
    -- owns the keyboard.
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
    -- ^ Prev-frame screen rect of this pane (zero until laid out; the grid's
    -- while maximized; its previewed rect during a drop preview), for
    -- building 'pvDragPick' handles such as a title bar.
  , pgcMaximized :: !Bool
    -- ^ True when this pane currently fills the whole grid.
  , pgcDragging :: !Bool
    -- ^ True while this pane's drag is armed. Past the drag threshold the
    -- pane is not rendered until release.
  , pgcDndActive :: !Bool
    -- ^ True while any pane drag-and-drop gesture is in progress.
  , pgcSplit :: !(GridAxis -> Eff es Word64)
    -- ^ Split this pane along the axis; returns the new pane id.
  , pgcClose :: !(Eff es ())
  , pgcMaximize :: !(Eff es ())
  , pgcRestore :: !(Eff es ())
  }

-- | How a pane can be dragged this frame. The pane's content, any title bar
-- included, is drawn by 'pgViewPane' itself.
data PaneView = PaneView
  { pvTitle :: !Text
    -- ^ Label shown (abbreviated to fit) on the compact drag indicator.
  , pvDraggable :: !Bool
    -- ^ A press anywhere in the pane that no interactive child takes drags it.
  , pvDragPick :: !(Maybe Rect)
    -- ^ An absolute sub-region, such as a title bar placed from 'pgcRect',
    -- that also starts a drag. With this 'Nothing' and 'pvDraggable' 'False'
    -- the pane cannot be moved.
  }
  deriving (Eq, Show)

-- | Outcome of one frame of the grid: the state after this pass, including
-- the pane actions and keys it ran, which the layout shows from next frame.
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

-- | A grid's state between frames: one value in the widget store, under the
-- grid's key.
data GridState = GridState
  { gsTree :: !(Maybe GridNode)
    -- ^ 'Nothing' once the last pane has been closed: the next frame starts
    -- again from one fresh pane.
  , gsSeed :: !Word64
    -- ^ Next fresh split / pane id. Strictly monotonic per grid, across a
    -- closed last pane too, so ids are never reused and state keyed by pane
    -- id cannot collide with a closed pane's state.
  , gsFocus :: !Word64
    -- ^ Keyboard-navigation focus (0 = none: the first pane).
  , gsMax :: !Word64
    -- ^ Maximized pane (0 = none).
  , gsSpan :: !(Maybe (Float, Float))
    -- ^ The (width, height) of the grid's rect the tree was last fitted to;
    -- a different one reflows the splits of pinned panes ('pgFixedPanes').
    -- Tracked even with nothing pinned, so a pin turned on later reflows
    -- from the size the tree really holds.
  , gsGesture :: !Gesture
  }
  deriving (Eq)

-- | The pointer gesture a press on the grid armed. It lasts until the button
-- comes up.
data Gesture
  = NoGesture
  | Resize !Word64 !Float !Float
    -- ^ A divider drag: the split's id, and its ratio and the pointer's
    -- main-axis coordinate at the press, so the divider moves by the pointer's
    -- delta instead of snapping to it.
  | Drag !Word64 !V2 !Bool !Text
    -- ^ A pane drag: the pane's id, the pointer at the press (for the drag
    -- threshold), whether the pointer has crossed the threshold since, and
    -- the pane's title for the drag indicator.
  deriving (Eq)

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
    -- ^ Pane ids are rooted at the grid, not the split tree, so rearranging
    -- splits keeps pane state.
  , geCfg :: !(PaneGridConfig es)
  , geGutter :: !Float
  , geThickness :: !Float
  , geMinSize :: !Float
  , geLeeway :: !Float
  , geRegions :: !(Map Word64 Rect)
    -- ^ Prev-frame pane regions; drives hit testing and 'pgcRect'.
  , geState :: !GridState
    -- ^ The state this frame was built from.
  , geLifted :: !Bool
    -- ^ The dragged pane is lifted out: not rendered, its slot in a drop
    -- preview left empty.
  , geMakeCtx :: Word64 -> Rect -> Bool -> PaneGridCtx es
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
paneGrid :: (Ui :> es) => PaneGridConfig es -> Eff es PaneGridResponse
paneGrid cfg = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  -- A grid that is no Tab stop gives up any focus left from when it was one.
  if pgFocusable cfg then uiIO (registerFocusable ctx wid) else releaseFocus wid
  let key = intKey wid
      spacing = max 0 (pgSpacing cfg)
      minSize = max 0 (pgMinSize cfg)
      leeway = max 0 (pgLeeway cfg)
      edgeBand = max 0 (pgEdgeBand cfg)
      gutter = spacing + 2 * leeway
  stored <- lookupDyn key <$> uiIO (getStore ctx)
  baseRect <- fromMaybe (Rect 0 0 0 0) <$> lastRect wid
  let (tree0, started) = case stored of
        Just st@GridState {gsTree = Just t} -> (t, st)
        Nothing | Just t <- pgInitial cfg -> (t, GridState (Just t) (treeMaxId t + 1) 0 0 Nothing NoGesture)
        -- A grid that closed its last pane starts again from a fresh pane
        -- whose id no closed pane had.
        _ ->
          let seed = maybe 1 gsSeed stored
           in (Pane seed, GridState (Just (Pane seed)) (seed + 1) 0 0 Nothing NoGesture)
      curSpan = (rectW baseRect, rectH baseRect)
      -- A grid whose rect changed size re-ratios its pinned panes' splits
      -- before anything is laid out from them. A grid that only moved, or was
      -- never fitted, has nothing to reflow.
      tree = case gsSpan started of
        Just (pw, ph)
          | (pw, ph) /= curSpan
          , any (pgFixedPanes cfg) (treePanes tree0) ->
              reflowFixed (pgFixedPanes cfg) minSize gutter (baseRect {rectW = pw, rectH = ph}) baseRect tree0
        _ -> tree0
      gs = started {gsTree = Just tree, gsSpan = Just curSpan}
  when (Just gs /= stored) $ uiIO (modifyStore ctx (insertDyn key gs))
  let (maxPane, focused) = paneFocus tree gs
      mouse = inputMousePos inp
      (regions, dividers) = layoutNode minSize gutter tree baseRect
      -- Drop geometry, kept while the drag is armed so the frame that
      -- releases the button can still resolve it. The grid's outer band is a
      -- top-level target, else the pane nearest the pointer, and outside the
      -- grid nothing. Targets are hit-tested against the grid without the
      -- dragged pane, never against the preview on screen, so showing a
      -- preview cannot change which drop it is; 'dropPreviewTreeSized' lays
      -- the result out as the drop will leave it.
      (dragMoved, dragZone, remaining) = case gsGesture gs of
        Drag pid press moved0 _ ->
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
      lifted = dragMoved && buttonHeld MouseLeft inp
      -- The store keeps the committed tree, for cancelling. On screen is the
      -- post-drop tree with the dragged pane's slot empty, or with no target
      -- the tree without it.
      (visibleTree, (visibleRegions, visibleDividers))
        | not lifted = (Just tree, (regions, dividers))
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
          , geMakeCtx = \pid rect dragging ->
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
                }
          }

  -- Tagged so next frame reads the grid's rect.
  container NodeContainer (pgLayout cfg (paneLay minSize)) $ do
    tagContainer wid
    if maxPane /= 0
      then void (renderPane env maxPane baseRect)
      else do
        rendered <- maybe (pure []) (renderNode env (M.fromList [(diSplitId d, d) | d <- visibleDividers])) visibleTree
        runGestures env dividers rendered dragMoved dragZone
        when (lifted && rectNonEmpty baseRect) $
          drawDragOverlay env wid mouse (dpRect <$> dragZone)
        -- Keyboard focus also rings the focused pane, so the arrow keys show
        -- where they moved; the grid's own ring says the grid holds focus.
        ringPane <- uiIO ((&&) <$> getFocusVisible ctx <*> ((== wid) <$> getFocusId ctx))
        when (ringPane && not lifted) $
          forM_ (M.lookup focused visibleRegions) $ \r ->
            uiIO $ registerCustomDrawing ctx wid (contentKey [1, rectX r, rectY r, rectW r, rectH r]) $ \cdc _ ->
              runCanvasFor cdc (drawStrokeRoundedRect (rectInflate (-2) r) 2 1.5 (themeAccent (cdcTheme cdc)))

  -- Keys for the focused grid. Escape restores a maximized pane unless
  -- something earlier (a popup in a pane) took it, and then claims it.
  focusedNow <- focusedWidget
  when (pgFocusable cfg && focusedNow == wid) $ do
    nav <- useKeyNav wid
    let ch = inputChars inp
    forM_ [(knLeft, (-1, 0)), (knRight, (1, 0)), (knUp, (0, -1)), (knDown, (0, 1))] $
      \(k, dir) -> when (k nav) (moveFocus env focused dir)
    when (knLeft nav || knRight nav || knUp nav || knDown nav) $
      damageWidgetNow wid (DamageInflated 0)
    when (T.any (== 'm') ch) $ maximizePane env focused
    when (T.any (== 'x') ch) $ closePane env focused
    when (inputKeysElem KeyEscape (inputKeys inp)) $ do
      taken <- uiIO (overlayConsumesQuit ctx inp)
      unless taken $ do
        restorePane env
        uiIO (markEscapeConsumed ctx)

  end <- fromMaybe gs . lookupDyn key <$> uiIO (getStore ctx)
  let (maxEnd, focusEnd) = maybe (0, 0) (`paneFocus` end) (gsTree end)
  pure
    PaneGridResponse
      { pgrChanged = gsTree end /= gsTree gs || gsMax end /= gsMax gs
      , pgrPaneCount = maybe 0 treeSize (gsTree end)
      , pgrPanes = maybe [] treePanes (gsTree end)
      , pgrFocusedPane = focusEnd
      , pgrMaximizedPane = maxEnd
      }

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

-- | Enter a pane's grid-relative id scope, consuming one sibling as 'withKey'
-- does.
withPaneKey :: (Ui :> es) => GridEnv es -> Word64 -> Eff es a -> Eff es a
withPaneKey env pid =
  withIdFrame (\parent -> (parent {siblingId = siblingId parent + 1}, gePaneScope env)) . withKey pid

-- | Render one pane's content via 'pgViewPane' under the pane's stable key.
renderPane ::
  (Ui :> es) =>
  GridEnv es ->
  Word64 ->
  Rect ->
  Eff es [RenderedPane]
renderPane env pid rect =
  withPaneKey env pid $ do
    inp <- askInput
    let ctx = geCtx env
        arena = ctxNodeArena ctx
    start <- uiIO (arenaCount arena)
    let ctxt = geMakeCtx env pid rect (draggingPane env pid)
    (view, _) <- containerResponse NodeContainer (paneLay (geMinSize env)) (pgViewPane (geCfg env) pid ctxt)
    -- Whether a press landed on a control in the pane, from last frame's
    -- rects: the active id is only settled after the view.
    controlHit <-
      if not (buttonPressed MouseLeft inp)
        then pure False
        else uiIO $ do
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
    -- Its prev-frame rect, zero until the pane has been laid out once.
    | otherwise ->
        renderPane env pid (M.findWithDefault (Rect 0 0 0 0) pid (geRegions env))
  Split sid ax ratio a b ->
    withKey sid $ do
      let (wa, ha) = subtreeMin (geMinSize env) (geGutter env) a
          (wb, hb) = subtreeMin (geMinSize env) (geGutter env) b
          mDiv = M.lookup sid dividers
          avail = maybe 0 (mainLen ax . diRegion) mDiv
          -- The A side's extent as 'layoutNode' clamped it.
          dA = maybe 0 (\d -> dividerLength (geGutter env) d ratio) mDiv
          pinA = pinnedSide (pgFixedPanes (geCfg env)) a
          pinB = pinnedSide (pgFixedPanes (geCfg env)) b
          -- A zero weight would not grow at all.
          weight w = axisLay ax (Grow (max 1.0e-3 w))
          (aSide, bSide)
            -- A region not laid out yet (the first frame, or a new split)
            -- shares out what the solver gives it in the split's ratio.
            | avail <= 0 = (weight (clamp01 ratio), weight (1 - clamp01 ratio))
            -- A percent for A and the rest for B follow the grid as it resizes.
            | pinA == pinB = (axisLay ax (Percent (dA / avail * 100)), fillLay)
            -- A pinned side keeps its pixel length; 'reflowFixed' brings the
            -- ratio in line on the next frame.
            | pinA = (axisLay ax (Fixed (max 0 dA)), fillLay)
            | otherwise = (fillLay, axisLay ax (Fixed (max 0 (avail - dA - geGutter env))))
      (if ax == AxisV then row' else column') fillLay $ do
        a' <- container NodeContainer (minW wa (minH ha aSide)) (renderNode env dividers a)
        dividerWidget env ax
        b' <- container NodeContainer (minW wb (minH hb bSide)) (renderNode env dividers b)
        pure (a' <> b')

-- | Is this the pane being drag-and-dropped?
draggingPane :: GridEnv es -> Word64 -> Bool
draggingPane env pid = case gsGesture (geState env) of
  Drag p _ _ _ -> p == pid
  _ -> False

-- | The divider: a drawing over the whole gutter, which all grabs, painted as
-- a faint rail with the crisp 'geThickness' strip in the middle.
dividerWidget :: (Ui :> es) => GridEnv es -> GridAxis -> Eff es ()
dividerWidget env axis =
  void $
    customWidget
      defaultCustomWidgetSpec
        { widgetLayout = axisLay axis (Fixed (geGutter env))
        , widgetContent = contentKey [if axis == AxisV then 1 else 2, thickness, leeway]
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
                  | otherwise = lerpColor (themeAccent theme) (styleBg panel) 0.45
            drawRect rect (lerpColor (styleBg panel) (themeSeparator theme) 0.12)
            drawRect (alongAxis axis (Rect (x + leeway) y thickness h)) (scrollBarTrackColor panel theme)
            when (cdcHovered cdc || cdcPressed cdc) $
              drawStroke (V2 lx ly) (V2 (lx + lw) (ly + lh)) 2 line
        , widgetCursor = Just (\_ _ _ -> if axis == AxisV then UiCursorEwResize else UiCursorNsResize)
        }
  where
    thickness = geThickness env
    leeway = geLeeway env

-- | The drag indicator and drop-zone highlight, drawn over the grid by its
-- root container so they stay out of the layout. The indicator is small,
-- translucent and offset from the pointer, so the preview stays visible.
drawDragOverlay :: (Ui :> es) => GridEnv es -> WidgetId -> V2 -> Maybe Rect -> Eff es ()
drawDragOverlay env wid (V2 mx my) zone =
  uiIO $ registerCustomDrawing (geCtx env) wid key $ \cdc _ -> runCanvasFor cdc $ do
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
  (Ui :> es) =>
  GridEnv es ->
  [DividerInfo] ->
  [RenderedPane] ->
  Bool ->
  Maybe DropPreview ->
  Eff es ()
runGestures env dividers rendered moved zone = do
  inp <- askInput
  let mouse = inputMousePos inp
      down = buttonHeld MouseLeft inp
      setGesture mirror g = void (updateGrid env mirror (\s -> s {gsGesture = g}))
      -- diBand already spans the leeway; inflating it would steal presses
      -- from the neighbouring panes.
      hitDiv = find (\d -> rectHit (diBand d) mouse) dividers
      -- The pane whose pick rect (or, when 'pvDraggable', whole region) is
      -- under the pointer.
      pickHit = flip find rendered $ \pane ->
        let v = rpView pane
         in any (`rectHit` mouse) (pvDragPick v)
              || (pvDraggable v && any (`rectHit` mouse) (M.lookup (rpPaneId pane) (geRegions env)))
  when (buttonPressed MouseLeft inp && gsGesture (geState env) == NoGesture && not (any rpControlHit rendered)) $
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
      -- The indicator follows the pointer, so keep frames coming.
      | down -> do
          requestFrame
          when (moved && not moved0) $ setGesture False (Drag pid press True title)
      -- A drop clears the gesture and stores the previewed tree, built with
      -- this frame's seed, in the same write.
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

moveFocus :: (Ui :> es) => GridEnv es -> Word64 -> (Float, Float) -> Eff es ()
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

-- | Rewrite the grid's stored state and return it. @mirror@ has the running
-- frame rebuild its view with the new state; a rewrite that changes nothing
-- leaves the store alone.
updateGrid :: (Ui :> es) => GridEnv es -> Bool -> (GridState -> GridState) -> Eff es GridState
updateGrid env mirror f = uiIO $ do
  st <- getStore (geCtx env)
  let old = fromMaybe (geState env) (lookupDyn (geKey env) st)
      new = f old
  when (new /= old) $
    setStore (geCtx env) ((if mirror then bumpMirror else id) (insertDyn (geKey env) new st))
  pure new

-- | Split a pane, focusing and returning the new one.
splitPane :: (Ui :> es) => GridEnv es -> Word64 -> GridAxis -> Eff es Word64
splitPane env pid axis =
  fmap gsFocus . updateGrid env True $ \s ->
    let seed = gsSeed s
     in s {gsTree = treeSplit pid seed axis False (seed + 1) <$> gsTree s, gsSeed = seed + 2, gsFocus = seed + 1}

closePane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
closePane env pid =
  void . updateGrid env True $ \s ->
    s {gsTree = gsTree s >>= treeRemovePane pid, gsMax = if gsMax s == pid then 0 else gsMax s}

-- | Maximizing hides the dividers and every other pane, so an armed drag or
-- resize gesture could never complete; cancel it instead of leaking it.
maximizePane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
maximizePane env pid =
  void . updateGrid env True $ \s ->
    if gsMax s == pid then s {gsMax = 0} else s {gsMax = pid, gsGesture = NoGesture}

restorePane :: (Ui :> es) => GridEnv es -> Eff es ()
restorePane env = void (updateGrid env True (\s -> s {gsMax = 0}))
