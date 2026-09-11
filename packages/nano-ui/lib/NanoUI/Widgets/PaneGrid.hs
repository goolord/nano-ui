{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Interactive pane grid with resizable dividers (iced 'PaneGrid'-style).
--
-- The grid is a binary split tree ('NanoUI.Widgets.SplitPane.GridNode')
-- persisted per widget as a 'Data.Dynamic' value in the widget store.
--
-- Panes are rendered through the user-provided 'pgViewPane', which receives a
-- 'PaneGridCtx' with immediate-mode actions to split, close, maximize, or
-- restore the pane. Dividers can be dragged to resize; panes can be grabbed by
-- their pick rect and dropped onto another pane (center = swap, edge = split)
-- or onto the grid's outer edge to restructure the whole grid at top level;
-- arrow keys navigate between panes; @m@/@x@ maximize/close and @Escape@
-- restores while the grid is focused.
module NanoUI.Widgets.PaneGrid
  ( GridAxis (..)
  , PaneGridConfig (..)
  , defaultPaneGridConfig
  , PaneGridCtx (..)
  , PaneView (..)
  , PaneGridResponse (..)
  , paneGrid
  ) where

import Control.Monad (forM_, unless, void, when)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Dynamic (fromDynamic, toDyn)
import Data.IntMap.Strict qualified as IM
import Data.List (find, minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector (Vector)
import Data.Word (Word64)
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , bumpMirror
  , getFocusId
  , getPrevRect
  , getStore
  , intKey
  , markDirty
  , markEscapeConsumed
  , menuPointerGestureActive
  , overlayConsumesQuit
  , registerCustomDrawing
  , registerFocusable
  , setStore
  )
import NanoUI.Draw (DrawOp)
import NanoUI.Input
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
import NanoUI.Monad (Ui, askContext, askInput, nextId, uiIO, withKey)
import NanoUI.Id (WidgetId)
import NanoUI.Store
  ( WidgetStore (..)
  , slotKey
  , slotPaneFocus
  , slotPaneGest
  , slotPaneGrab
  , slotPaneMax
  , slotPaneNext
  , slotPaneResize
  )
import NanoUI.Style
  ( AlignX (..)
  , AlignY (..)
  , Direction (..)
  , Layout (..)
  , Padding (..)
  , Sizing (..)
  , Style (..)
  , Theme (..)
  , defaultLayout
  , fadeAlpha
  , separatorTrackColor
  )
import NanoUI.Types
  ( Rect (..)
  , V2 (..)
  , colorRGBA
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
import NanoUI.Widgets.Behavior (KeyNav (..), dragThresholdPx, useKeyNav)
import NanoUI.Widgets.Custom
  ( CustomWidgetSpec (..)
  , CustomDrawContext (..)
  , defaultCustomWidgetSpec
  , customWidget_
  , drawRect
  , drawRoundedRect
  , drawStroke
  , drawStrokeRoundedRect
  , drawText
  , runCanvas
  )
import NanoUI.Widgets.Layout (column', row')
import NanoUI.Layout.Arena (NodeType (..))
import NanoUI.Widgets.Node
  ( container
  , containerResponse
  , tagContainer
  )
import NanoUI.Widgets.SplitPane
  ( DividerInfo (..)
  , DropTarget (..)
  , GridAxis (..)
  , GridNode (..)
  , clampTreeRatio
  , dropPreview
  , dropTargetForPane
  , layoutNode
  , mainLen
  , mainMins
  , paneExist
  , splitLength
  , subtreeMin
  , topLevelDropTarget
  , treeMovePane
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
    -- ^ Minimum physical size any pane may shrink to (default 40).
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
  , pgViewPane :: !(Word64 -> PaneGridCtx es -> Eff es PaneView)
    -- ^ Renders the content of one pane.
  }

defaultPaneGridConfig :: PaneGridConfig es
defaultPaneGridConfig =
  PaneGridConfig
    { pgLayout = id
    , pgSpacing = 4
    , pgMinSize = 40
    , pgLeeway = 6
    , pgEdgeBand = 20
    , pgViewPane = \_ _ -> pure (PaneView "" False Nothing)
    }

-- | Actions handed to a pane so it can mutate the grid immediately.
data PaneGridCtx es = PaneGridCtx
  { pgcPaneId :: !Word64
  , pgcRect :: !Rect
    -- ^ Prev-frame screen rect of this pane (zero until it has been laid
    -- out once; the whole grid rect while maximized). Use it to build
    -- 'pvDragPick' handles such as a title-bar sub-rect.
  , pgcMaximized :: !Bool
    -- ^ True when this pane currently fills the whole grid.
  , pgcDragging :: !Bool
    -- ^ True when this pane is the one being drag-and-dropped.
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
    -- ^ Label shown on the floating ghost while the pane is dragged.
  , pvDraggable :: !Bool
    -- ^ Grab the pane anywhere inside its own region to drag-and-drop it. This
    -- is the easy way to reorder panes without drawing a dedicated handle.
    -- Pane still needs a drag only on a sub-region? see 'pvDragPick'.
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
  }

-- | Per-frame shared environment.
data GridEnv es = GridEnv
  { geCtx :: !Context
  , geKey :: !Int
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
  , geGestK :: !Int
  , geGrabK :: !Int
  , geFocusK :: !Int
  , geMaxK :: !Int
  , geSeedK :: !Int
  , geSeed :: !Word64
    -- ^ Next fresh split / pane id ('slotPaneNext'); strictly monotonic per
    -- grid, so ids are never reused and state keyed by pane id cannot
    -- collide with a closed pane's state.
  , geResizeK :: !Int
  , geDrag0 :: !Int
  , geMax :: !Word64
  , geChangedRef :: !(IORef Bool)
  , geMakeCtx :: Word64 -> Rect -> Bool -> PaneGridCtx es
  }

-- | Computed drag-and-drop interaction state for one frame.
data DragInfo = DragInfo
  { dgiActive :: !Bool
  , dgiMoved :: !Bool
  , dgiGhost :: !(Maybe Rect)
  , dgiZone :: !(Maybe (Rect, DropTarget))
  }

-- -----------------------------------------------------------------------------
-- Tree + focus state
-- -----------------------------------------------------------------------------

-- | The grid's split tree persisted in the widget store, if seeded.
lookupTree :: Int -> WidgetStore -> Maybe GridNode
lookupTree k st = IM.lookup k (storeDyn st) >>= fromDynamic

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

paneGrid :: (Ui :> es) => PaneGridConfig es -> Eff es PaneGridResponse
paneGrid cfg = do
  wid <- nextId
  ctx <- askContext
  inp <- askInput
  uiIO (registerFocusable ctx wid)
  let key = intKey wid
      gestK = slotKey slotPaneGest key
      grabK = slotKey slotPaneGrab key
      focusK = slotKey slotPaneFocus key
      maxK = slotKey slotPaneMax key
      resizeK = slotKey slotPaneResize key
      seedK = slotKey slotPaneNext key
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
      pure (t, fromIntegral (IM.findWithDefault 1 seedK (storeInt st)))
    Nothing -> do
      let seed = max 1 (fromIntegral (IM.findWithDefault 1 seedK (storeInt st)))
          start = Pane seed
      uiIO $
        setStore
          ctx
          ( bumpMirror
              ( st
                  { storeInt = IM.insert seedK (fromIntegral (seed + 1)) (storeInt st)
                  , storeDyn = IM.insert key (toDyn start) (storeDyn st)
                  }
              )
          )
      pure (start, seed + 1)
  mPrev <- uiIO (getPrevRect ctx wid)
  let baseRect = fromMaybe (Rect 0 0 0 0) mPrev
      drag0 = IM.findWithDefault 0 gestK (storeInt st)
      maxPane = validPane tree0 (IM.findWithDefault 0 maxK (storeInt st))
      focus0 = IM.findWithDefault 0 focusK (storeInt st)
      focusedInit = resolveFocus tree0 maxPane (fromIntegral focus0)
      mouse = inputMousePos inp
      (regions, dividers) = layoutNode minSize gutter tree0 baseRect
      divMap = M.fromList [(diSplitId d, d) | d <- dividers]
  changedRef <- uiIO (newIORef False)
  let mGrab = IM.lookup grabK (storePoint st)
      dgi =
        computeDragInfo
          drag0
          DragGeom
            { dgMinSize = minSize
            , dgGutter = gutter
            , dgTree = tree0
            , dgBaseRect = baseRect
            , dgBand = edgeBand
            , dgRegions = regions
            }
          mGrab
          mouse
      dgiShown = dgiActive dgi && (isJust (dgiGhost dgi) || isJust (dgiZone dgi))
      env =
        GridEnv
          { geCtx = ctx
          , geKey = key
          , geCfg = cfg
          , geGutter = gutter
          , geThickness = spacing
          , geMinSize = minSize
          , geLeeway = leeway
          , geRegions = regions
          , geBaseRect = baseRect
          , geTree = tree0
          , geGestK = gestK
          , geGrabK = grabK
          , geFocusK = focusK
          , geMaxK = maxK
          , geSeedK = seedK
          , geSeed = seed1
          , geResizeK = resizeK
          , geDrag0 = drag0
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
        rendered <- renderNode env divMap tree0
        runGestures env dividers rendered dgi
        when (dgiShown && rectNonEmpty baseRect) $
          drawDragOverlay env wid rendered (dgiGhost dgi) (fmap fst (dgiZone dgi))

  -- Keyboard navigation for the focused grid. Escape restores a maximized
  -- pane unless something earlier in the pass already consumed it (e.g. a
  -- dismissable popup inside a pane); the grid then claims the key so
  -- neither a nested overlay nor the app also acts on it.
  focusedNow <- uiIO (getFocusId ctx)
  when (focusedNow == wid) $ do
    nav <- useKeyNav wid
    let ch = inputChars inp
        cur = focusedInit
    when (knLeft nav) $ moveFocus env cur (-1, 0)
    when (knRight nav) $ moveFocus env cur (1, 0)
    when (knUp nav) $ moveFocus env cur (0, -1)
    when (knDown nav) $ moveFocus env cur (0, 1)
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
      maxEnd = maybe 0 (\t -> validPane t (IM.findWithDefault 0 maxK (storeInt stEnd))) treeEnd
      focusEnd =
        maybe
          0
          (\t -> resolveFocus t maxEnd (fromIntegral (IM.findWithDefault 0 focusK (storeInt stEnd))))
          treeEnd
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
gridRootLayout minSize f =
  f
    defaultLayout
      { layoutDirection = Column
      , layoutGap = 0
      , layoutPadding = Padding 0 0 0 0
      , layoutWidth = Grow 1
      , layoutHeight = Grow 1
      , layoutMinW = minSize
      , layoutMinH = minSize
      }

sizingLay :: Sizing -> Sizing -> Layout
sizingLay wSiz hSiz =
  defaultLayout
    { layoutDirection = Column
    , layoutPadding = Padding 0 0 0 0
    , layoutGap = 0
    , layoutWidth = wSiz
    , layoutHeight = hSiz
    }

-- | Zero-gap, zero-padding, grow-to-fill layout.
fillLay :: Layout
fillLay = sizingLay (Grow 1) (Grow 1)

-- | A-side sizing for a split: fixed percent along the main axis. The B side
-- grows into the remainder.
splitSideLay :: GridAxis -> Float -> Layout
splitSideLay AxisV p = sizingLay (Percent p) (Grow 1)
splitSideLay AxisH p = sizingLay (Grow 1) (Percent p)

minSized :: Layout -> Float -> Float -> Layout
minSized l minW_ minH_ = l {layoutMinW = minW_, layoutMinH = minH_}

-- | The pane content wrapper: fills its cell, never below one minimum pane.
paneLay :: Float -> Layout
paneLay m = minSized fillLay m m

-- Percent of the main-axis extent for side A, after min clamping.
splitPct :: Float -> Float -> Float -> Float -> Float -> Float
splitPct spacing avail minA minB ratio
  | avail <= 0 = 50
  | otherwise = splitLength spacing avail minA minB ratio / avail * 100

-- -----------------------------------------------------------------------------
-- Rendering
-- -----------------------------------------------------------------------------

renderMaxPane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es [RenderedPane]
renderMaxPane env pid =
  renderPane env pid (geBaseRect env) (paneLay (geMinSize env)) False

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
  withKey pid $ do
    let ctxt = geMakeCtx env pid rect dragging
    (view, _) <- containerResponse NodeContainer lay (pgViewPane (geCfg env) pid ctxt)
    pure [RenderedPane pid view]

renderNode ::
  (Ui :> es) =>
  GridEnv es ->
  Map Word64 DividerInfo ->
  GridNode ->
  Eff es [RenderedPane]
renderNode env dividers = \case
  Pane pid ->
    renderPane env pid (paneRect env pid) (paneLay (geMinSize env)) (draggingPane env pid)
  Split sid0 ax _ a b ->
    withKey sid0 $ do
      let (wa, ha) = subtreeMin (geMinSize env) (geGutter env) a
          (wb, hb) = subtreeMin (geMinSize env) (geGutter env) b
          mDiv = M.lookup sid0 dividers
          avail = maybe 0 (mainLen ax . diRegion) mDiv
          (mA, mB) = mainMins ax (wa, ha) (wb, hb)
          pct = splitPct (geGutter env) avail mA mB (maybe 0.5 diRatio mDiv)
          aLay = minSized (splitSideLay ax pct) wa ha
          bLay = minSized fillLay wb hb
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
    customWidget_
      defaultCustomWidgetSpec
        { widgetLayout = dLay
        , widgetDraw = \cdc rect -> drawDivider cdc rect axis (geThickness env) (geLeeway env)
        , widgetCursor = Just (const (if axis == AxisV then UiCursorEwResize else UiCursorNsResize))
        }
  where
    dLay = case axis of
      AxisV -> sizingLay (Fixed (geGutter env)) (Grow 1)
      AxisH -> sizingLay (Grow 1) (Fixed (geGutter env))

drawDivider :: CustomDrawContext -> Rect -> GridAxis -> Float -> Float -> Vector DrawOp
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
  let ctx = geCtx env
      dragPane = fromIntegral (geDrag0 env)
      title = maybe "" pvTitle (fmap rpView (find ((== dragPane) . rpPaneId) rendered))
  uiIO $
    registerCustomDrawing ctx wid (\cdc _ -> drawOverlay (cdcTheme cdc) title ghost zone)

-- | Ghost + drop-zone chrome. Corner radius 2 and the drop shadow follow the
-- house style of floating chrome ('NanoUI.Frame.Chrome').
drawOverlay :: Theme -> Text -> Maybe Rect -> Maybe Rect -> Vector DrawOp
drawOverlay theme title ghost zone =
  runCanvas $ do
    let accent = themeAccent theme
        win = themeFloatingWindow theme
        panelFill = lerpColor (styleBg win) accent 0.08
        panelBorder = lerpColor (styleBg win) (styleFg win) 0.45
        previewFill = fadeAlpha accent 32
    forM_ ghost $ \gr -> do
      -- Menu-style drop shadow: offset down-right, translucent black.
      drawRoundedRect (Rect (rectX gr + 3) (rectY gr + 3) (rectW gr) (rectH gr)) 2 (colorRGBA 0 0 0 72)
      drawRoundedRect gr 2 panelFill
      drawStrokeRoundedRect gr 2 2 panelBorder
      when (not (T.null title)) $
        drawText (V2 (rectX gr + 10) (rectY gr + 8)) AlignStart AlignTop title (styleFg win)
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
  }

-- | Pure drag-and-drop geometry for the current frame. Geometry is computed
-- for as long as the gesture id is armed (not just while the button is held),
-- so the drop zone is still resolvable on the frame the button is released.
-- 'dgBaseRect' is the grid's own rect: its outer band (thickness 'dgBand') is
-- a top-level drop zone, and the pointer there restructures the whole grid;
-- otherwise the pane under the pointer is the target. Every candidate is
-- resolved through 'dropPreview', which simulates the drop and lays the tree
-- back out with the grid's real 'dgGutter' and 'dgMinSize', so the
-- highlighted rect is the exact region the pane lands in even when removing
-- it reshapes the rest of a mixed-split grid.
computeDragInfo :: Int -> DragGeom -> Maybe (Float, Float) -> V2 -> DragInfo
computeDragInfo drag0 geom mGrab mouse
  | drag0 <= 0 = DragInfo False False Nothing Nothing
  | otherwise =
      let DragGeom{dgMinSize = minSize, dgGutter = gutter, dgTree = tree, dgBaseRect = baseRect, dgBand = band, dgRegions = regions} = geom
          pid = fromIntegral drag0
          mFrom = M.lookup pid regions
          (gx, gy) = fromMaybe (0, 0) mGrab
          moved = case mFrom of
            Just (Rect px py _ _) ->
              let vx = v2X mouse - (px + gx)
                  vy = v2Y mouse - (py + gy)
               in vx * vx + vy * vy > dragThresholdPx * dragThresholdPx
            Nothing -> False
          ghost = case mFrom of
            Just (Rect _ _ pw ph)
              | moved -> Just (Rect (v2X mouse - gx) (v2Y mouse - gy) pw ph)
            _ -> Nothing
          under =
            [ (q, r)
            | (q, r) <- M.toList regions
            , q /= pid
            , rectHit r mouse
            ]
          zone = case topLevelDropTarget band baseRect mouse of
            Just dt -> dropPreview minSize gutter tree pid baseRect dt
            Nothing -> case under of
              (q, r) : _ ->
                let dt = dropTargetForPane r mouse q
                 in dropPreview minSize gutter tree pid baseRect dt
              [] -> Nothing
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
      hitDiv =
        find
          (\d -> rectHit (rectInflate (geLeeway env) (diBand d)) mouse)
          dividers
      -- The pane whose pick rect (or, when 'pvDraggable', whole region) is
      -- under the pointer.
      pickHit =
        listToMaybe
          [ p
          | RenderedPane p v <- rendered
          , maybe False (`rectHit` mouse) (pvDragPick v)
              || (pvDraggable v && maybe False (`rectHit` mouse) (M.lookup p regions))
          ]
  menu <- uiIO (menuPointerGestureActive ctx)
  when (press && not busy && not menu) $ do
    case hitDiv of
      Just d -> do
        writeGest env (negate (fromIntegral (diSplitId d)))
        writeResizeStart env d mouse
      Nothing ->
        forM_ pickHit $ \pid -> do
          writeGest env (fromIntegral pid)
          writeGrab env $
            maybe (V2 0 0) (\(Rect px py _ _) -> V2 (v2X mouse - px) (v2Y mouse - py)) (M.lookup pid regions)
  when (drag0 < 0 && down) $ do
    let sid = fromIntegral (negate drag0)
    forM_ (find ((== sid) . diSplitId) dividers) $ \d -> do
      st <- uiIO (getStore ctx)
      let (ratio0, main0) =
            IM.findWithDefault (diRatio d, mouseMain d mouse) (geResizeK env) (storePoint st)
          avail = mainLen (diAxis d) (diRegion d)
          r0 =
            if avail <= 0
              then ratio0
              else ratio0 + (mouseMain d mouse - main0) / avail
          r' = clampTreeRatio (geTree env) sid (diRegion d) (geGutter env) (geMinSize env) r0
       in writeTree env (treeSetRatio sid r' (geTree env))
  when (drag0 < 0 && not down) $ writeGest env 0
  -- Keep the loop at the display cadence while a pane is being dragged: the
  -- ghost follows the pointer, and without a dirty flag the debug HUD's slow
  -- refresh paces the whole frame (4 fps). Window / scroll / resize drags mark
  -- dirty every frame for the same reason.
  when (drag0 > 0 && down) $ uiIO (markDirty ctx)
  when (drag0 > 0 && not down) $ do
    let moved = fromIntegral drag0
    when (dgiMoved dgi) $
      forM_ (dgiZone dgi) $ \(_, dt) ->
        forM_ (treeMovePane moved (geSeed env) dt (geTree env)) $ \t' -> do
          putSeed env (geSeed env + 1)
          writeTree env t'
          putFocus env moved
    writeGest env 0

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
    Just pid -> putFocus env pid
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
  writeTree env (treeSplit pid splitId axis False newPane (geTree env))
  putSeed env (geSeed env + 2)
  putFocus env newPane
  pure newPane

closePane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
closePane env pid =
  case treeRemovePane pid (geTree env) of
    Nothing -> clearTree env
    Just t' -> do
      writeTree env t'
      when (geMax env == pid) (putMax env 0)

maximizePane :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
maximizePane env pid = do
  let v = if geMax env == pid then 0 else pid
  putMax env v
  -- Maximizing hides the dividers and every other pane, so an armed drag or
  -- resize gesture could never complete; cancel it instead of leaking it.
  when (v /= 0) (writeGest env 0)

restorePane :: (Ui :> es) => GridEnv es -> Eff es ()
restorePane env = putMax env 0

-- | One store round-trip. @mirror@ bumps the mirror generation so the
-- running frame rebuilds its UI and layout with the new value (see
-- 'NanoUI.Frame'); @dirty@ wakes the renderer for a repaint even when no
-- further input arrives.
storeWrite ::
  (Ui :> es) =>
  GridEnv es ->
  Bool ->
  Bool ->
  (WidgetStore -> WidgetStore) ->
  Eff es ()
storeWrite env mirror dirty f = uiIO $ do
  let ctx = geCtx env
  st <- getStore ctx
  setStore ctx ((if mirror then bumpMirror else id) (f st))
  when dirty (markDirty ctx)

-- | Flag 'pgrChanged' for this frame.
markChanged :: (Ui :> es) => GridEnv es -> Eff es ()
markChanged env = uiIO (writeIORef (geChangedRef env) True)

-- | Structural change: mirror + dirty + 'pgrChanged'.
writeTree :: (Ui :> es) => GridEnv es -> GridNode -> Eff es ()
writeTree env t = do
  storeWrite env True True $ \st ->
    st {storeDyn = IM.insert (geKey env) (toDyn t) (storeDyn st)}
  markChanged env

-- | Remove the tree entirely (the last pane was closed). The pane-id seed
-- keeps counting across the reset, so the re-seeded pane gets a fresh id and
-- state keyed by pane id never collides with a closed pane's state.
clearTree :: (Ui :> es) => GridEnv es -> Eff es ()
clearTree env = do
  storeWrite env True True $ \st ->
    st {storeDyn = IM.delete (geKey env) (storeDyn st)}
  markChanged env

-- | Gesture slot: 0 none, positive = dragged pane id, negative = resized
-- split id.
writeGest :: (Ui :> es) => GridEnv es -> Int -> Eff es ()
writeGest env n =
  storeWrite env True False $ \st ->
    st
      { storeInt =
          if n == 0
            then IM.delete (geGestK env) (storeInt st)
            else IM.insert (geGestK env) n (storeInt st)
      }

-- | Grab offset (mouse - pane origin) captured when a pane drag starts, so
-- the ghost tracks the pointer under the grab point.
writeGrab :: (Ui :> es) => GridEnv es -> V2 -> Eff es ()
writeGrab env off =
  storeWrite env False False $ \st ->
    st {storePoint = IM.insert (geGrabK env) (v2X off, v2Y off) (storePoint st)}

-- | Record the divider's ratio and the pointer's main-axis coordinate when a
-- resize starts, so subsequent drag frames move the divider by delta instead of
-- snapping it to the pointer.
writeResizeStart :: (Ui :> es) => GridEnv es -> DividerInfo -> V2 -> Eff es ()
writeResizeStart env d mouse =
  storeWrite env False False $ \st ->
    st {storePoint = IM.insert (geResizeK env) (diRatio d, mouseMain d mouse) (storePoint st)}

putMax :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
putMax env v = do
  let k = geMaxK env
      n = fromIntegral v
  st <- uiIO (getStore (geCtx env))
  when (IM.findWithDefault 0 k (storeInt st) /= n) $ do
    storeWrite env True True (\st' -> st' {storeInt = IM.insert k n (storeInt st')})
    markChanged env

putFocus :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
putFocus env v = do
  let k = geFocusK env
      n = fromIntegral v
  st <- uiIO (getStore (geCtx env))
  when (IM.findWithDefault 0 k (storeInt st) /= n) $
    storeWrite env True False (\st' -> st' {storeInt = IM.insert k n (storeInt st')})

-- | Advance the next-id seed ('slotPaneNext').
putSeed :: (Ui :> es) => GridEnv es -> Word64 -> Eff es ()
putSeed env v =
  storeWrite env False False $ \st ->
    st {storeInt = IM.insert (geSeedK env) (fromIntegral v) (storeInt st)}
