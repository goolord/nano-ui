-- | Complete headless frames: view construction, layout, input resolution,
-- damage, and draw-buffer generation. Backends own event waits and presentation.
module NanoUI.Internal.Frame
  ( runFrame
  , runFrameEff
  , runFrameReduce
  , runFrameReduceEff
  , needsRedraw
  , pointerDragActive
  , textFieldActive
  , floatingPanelActive
  , debugPanelOpen
  , collectTextSpans
  , collectOverlayTextSpans
  , collectRasterSpans
  , widgetNodeCount
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  )
where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.Functor ((<&>))
import Data.IntMap.Strict qualified as IM
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Internal.Context
  ( Context (..)
  , PointerRoute (..)
  , beginThemeScopes
  , damageFull
  , damageRect
  , themeScopesChanged
  , FrameMsg (..)
  , clearDirty
  , clearWakeAt
  , decodeMessages
  , drainMessages
  , getStore
  , isDirty
  , lookupCustomMeasure
  , customMeasureHooks
  , lookupPopupConfig
  , markDirty
  , pruneDrawOpCache
  , resetDrawingScopeCache
  , stepScrollGlides
  , takeDamage
  , takeDamagePieces
  , tickAnimations
  , ensureMetricCaches
  , getsOverlay
  , modifyOverlay
  , OverlayState (..)
  )
import NanoUI.Internal.Context (beginFrameModal)
import NanoUI.Internal.Damage (FrameSnapshot (..), captureFrameSnapshot, updatePrevRects, writeDamage)
import NanoUI.Internal.Draw
  ( DrawData
  , Layer (..)
  , beginLayer
  , finishDraw
  , pushRect
  , resetDrawArena
  , setClip
  , setClipPieces
  )
import NanoUI.Internal.Frame.Cursor
  ( UiCursorKind (..)
  , cursorKindIs
  , pointerCursorWanted
  , uiCursorKind
  )
import NanoUI.Internal.Frame.Input
  ( armPointerPress
  , disarmPointerPress
  , finalizePointerPress
  , finalizePointerRelease
  , finalizeSelectFocus
  , finalizeTabFocus
  , finalizeTextInputFocus
  , pressTargets
  , refreshHover
  )
import NanoUI.Internal.Frame.Focus (constrainFocusToModal, syncWidgetLabels)
import NanoUI.Internal.Frame.Paint (lowerShapes)
import NanoUI.Internal.Frame.Redraw
  ( debugPanelOpen
  , floatingPanelActive
  , needsRedraw
  , pointerDragActive
  , textFieldActive
  )
import NanoUI.Internal.Frame.Scroll
  ( applyScrollOffsets
  , updateScrollDrag
  , updateScrollWheel
  )
import NanoUI.Internal.Frame.Select
  ( closeSelectOnOutsideClick
  , drawSelectOverlays
  , finalizeSelectKeyboard
  , finalizeSelectPick
  , overlayMenuRects
  , routePointer
  )
import NanoUI.Internal.Frame.Spans
  ( collectOverlayTextSpans
  , collectRasterSpans
  , collectTextSpans
  , widgetNodeCount
  )
import NanoUI.Internal.Frame.Overlay (drawModalOverlays, drawPopupOverlays, drawWindowOverlays)
import NanoUI.Internal.Frame.TextArea (finalizeTextFieldMouse)
import NanoUI.Internal.Frame.TextEdit
  ( closeTextEditMenuOnEscape
  , closeTextEditMenuOnOutsideClick
  , drawTextEditMenuOverlays
  , finalizeTextEditMenuPick
  , openTextEditMenu
  )
import NanoUI.Internal.Frame.Window
  ( contextMeasurers
  , lookupWindowPos
  , lookupWindowSize
  , persistWindowPositions
  , updateWindowDrag
  , updateWindowResize
  )
import NanoUI.Internal.Id (WidgetId (..), initialIdContext)
import NanoUI.Internal.Input (Input (..), inputMousePressed, stripInteractionInput, withoutPointer)
import NanoUI.Internal.Layout.Arena
  ( CustomMeasureRecord
  , LayoutCache (..)
  , NodeIdx
  , NodeType (..)
  , captureLayoutCache
  , floatingNodeCount
  , getNodeType
  , getWidgetId
  , layoutCacheEligible
  , layoutSigMatches
  , computeSubtreeHashes
  , newLayoutCache
  , resetNodeArena
  , restoreLayoutCache
  )
import NanoUI.Internal.Layout.Solve (placeModals, placePopups, placeWindows, runCustomMeasure, solveLayout)
import NanoUI.Internal.Monad (NanoUI, Ui, runUi, unlessM, whenM)
import NanoUI.Internal.Store (mirrorStoresChanged)
import NanoUI.Internal.Style (Theme (..))
import NanoUI.Internal.Types (Damage (..), Rect, Size (..), rectInflate, rectNonEmpty)

-- | Build, lay out, resolve input, and paint one headless frame. Returns the
-- view result, emitted messages, borrowed draw buffers, and whether state
-- needs a follow-up frame. A local-state change can rebuild the view within
-- this call, with one-shot input removed. Native presentation is the host's job.
runFrame :: Context -> Input -> NanoUI a -> IO (a, [FrameMsg], DrawData, Bool)
runFrame = runFrameEff runEff

-- | View this model, then apply decoded messages at frame end.
-- DrawData is from the pre-reduce model (one-frame lag). The idle
-- loop redraws when the reduced model differs.
runFrameReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> NanoUI a)
  -> IO (a, model, [msg], DrawData, Bool)
runFrameReduce = runFrameReduceEff runEff

-- | 'runFrameReduce' for a larger effect stack, with a runner that interprets
-- the remaining effects in IO.
runFrameReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x)
  -> (msg -> model -> model)
  -> Context
  -> Input
  -> model
  -> (model -> Eff (Ui : es) a)
  -> IO (a, model, [msg], DrawData, Bool)
runFrameReduceEff unlift update ctx inp model view = do
  (a, msgs, draw, dirty) <- runFrameEff unlift ctx inp (view model)
  let
    typed = decodeMessages msgs
    model' = foldl' (flip update) model typed
  when (model' /= model) (markDirty ctx)
  dirty' <- isDirty ctx
  pure (a, model', typed, draw, dirty || dirty')

-- | 'runFrame' with a runner for the effects remaining after 'Ui'. Run frames
-- serially on a context; its arenas and stores are mutable and reused.
runFrameEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x)
  -> Context
  -> Input
  -> Eff (Ui : es) a
  -> IO (a, [FrameMsg], DrawData, Bool)
runFrameEff unlift ctx frameInp ui = do
  ensureMetricCaches ctx
  snap <- captureFrameSnapshot ctx
  clearDirty ctx
  -- Timed wakes are re-requested by whatever is still built this frame.
  clearWakeAt ctx
  -- Decide what the pointer belongs to before anything reads it, against the
  -- frame the user saw. The view gets its input routed layer by layer, and
  -- each step below gets the input of what it serves, with no pointer in it
  -- unless that is where the pointer went, so none of them has to ask:
  -- @layerInp@ for scrolling, windows, presses, focus and text fields,
  -- @menuInp@ for the text-edit menu, and @dropInp@ for the dropdowns, which
  -- only that menu is drawn over. @frameInp@, the pointer whoever it belongs
  -- to, is for what watches the whole window: a press anywhere else closing
  -- a menu, hover, and what the overlays paint.
  route <- routePointer ctx frameInp
  let routedIf mine = if mine then frameInp else withoutPointer frameInp
      layerInp = routedIf (case route of RouteLayer _ -> True; _ -> False)
      menuInp = routedIf (route == RouteTextMenu)
      dropInp = routedIf (route /= RouteTextMenu)
  -- Wheel and thumb-drag input targets the previous frame's layout, so apply
  -- it while that arena is still intact, before it is reset for the new
  -- build. Settling offsets before the UI pass keeps build-time
  -- virtualization (table body rows) materialized for the range that will
  -- actually be visible, without a second build pass.
  updateScrollWheel ctx layerInp
  -- A glide advances with the wheel, before the build, for the same reason:
  -- the offset this frame renders at is the one virtualization must see.
  stepScrollGlides ctx (inputDeltaTime frameInp)
  updateScrollDrag ctx layerInp
  beginThemeScopes ctx True
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  resetUiBuildScopes ctx
  beginFrameModal ctx
  writeIORef (ctxReleaseClickedId ctx) (WidgetId 0)
  armPointerPress ctx frameInp
  result0 <- unlift (runUi ctx frameInp ui)
  -- Pending click is one-shot. Clear before a mirror rebuild so toggles do not fire twice.
  writeIORef (ctxClickedId ctx) (WidgetId 0)
  storeMid <- getStore ctx
  result <-
    if mirrorStoresChanged (fsStore snap) storeMid
      then do
        resetUiBuild ctx
        unlift (runUi ctx (stripInteractionInput frameInp) ui)
      else pure result0
  -- The store this frame's arena was built from. A view run again after a
  -- local-hook write can write the same state again (a pane divider being
  -- dragged stores the tree on every run), which is not a change the arena
  -- missed.
  storeBuilt <- getStore ctx
  -- Scopes only change how nodes look, which the rect and text diffs below
  -- cannot see, and custom widgets' cached ops hold the old theme's colours.
  whenM (themeScopesChanged ctx) $ do
    damageFull ctx
    modifyIORef' (ctxMetricGen ctx) (+ 1)
  -- Sync widget node values (checkbox/radio/tree) from the store before measure
  -- so labels and layout reflect the current state.
  syncWidgetLabels ctx
  let
    size@(Size w h) = inputWindowSize frameInp
  unlessM (tryReuseLayout ctx size) $
    solveLayoutAndCapture ctx w h
  movedResize <- updateWindowResize ctx layerInp w h
  movedWindow <- updateWindowDrag ctx layerInp
  -- A window moved or resized changes only where the floating panels go:
  -- the solve before placement stands, so place them again over it.
  when (movedResize || movedWindow) $
    unlessM (replaceFloating ctx size) $
      solveLayoutAndCapture ctx w h
  persistWindowPositions ctx
  applyScrollOffsets ctx
  -- A press on a menu or dropdown leaves nothing active, whatever a release
  -- that never arrived left behind.
  when (inputMousePressed frameInp && not (inputMousePressed layerInp)) $
    writeIORef (ctxActiveId ctx) (WidgetId 0)
  targets <- pressTargets ctx layerInp
  finalizePointerPress ctx targets
  finalizePointerRelease ctx layerInp
  disarmPointerPress ctx frameInp
  finalizeTextInputFocus ctx layerInp targets
  finalizeSelectFocus ctx targets
  finalizeTextFieldMouse ctx layerInp
  closeTextEditMenuOnOutsideClick ctx frameInp
  openTextEditMenu ctx layerInp
  finalizeTextEditMenuPick ctx menuInp
  closeTextEditMenuOnEscape ctx frameInp
  constrainFocusToModal ctx
  finalizeTabFocus ctx frameInp
  finalizeSelectKeyboard ctx frameInp
  finalizeSelectPick ctx dropInp
  closeSelectOnOutsideClick ctx frameInp
  storeAfter <- getStore ctx
  -- Node values follow the store, but no layout input does, so the solve
  -- stands unless the arena's inputs or a custom measure moved.
  when (mirrorStoresChanged storeBuilt storeAfter) $ do
    syncWidgetLabels ctx
    unlessM (tryReuseLayout ctx size) $
      solveLayoutAndCapture ctx w h
    applyScrollOffsets ctx
  updatePrevRects ctx
  refreshHover ctx frameInp
  tickAnimations ctx (inputDeltaTime frameInp)
  pruneDrawOpCache ctx
  -- Dropdowns and the text-edit menu are not in the arena, so nothing in the
  -- damage pass sees their rows change under the pointer, their filter or
  -- their scroll. Each open one repaints whole every frame, and a closed or
  -- moved one repaints where it was.
  menuRects <- overlayMenuRects ctx
  prevMenuRects <- getsOverlay ctx osPrevMenuRects
  unless (null menuRects && null prevMenuRects) $ do
    mapM_ (damageRect ctx) (menuRects ++ prevMenuRects)
    modifyOverlay ctx (\os -> os {osPrevMenuRects = menuRects})
  writeDamage ctx frameInp snap
  -- Clip frames repaint the damaged region of the retained texture, which
  -- preserves the other pixels. The region starts from the window backdrop,
  -- inflated by one
  -- logical pixel to cover the runner's outward pixel snap. Full-present
  -- frames (fresh retain, forced full, continuous) paint everything.
  paintFull <- readIORef (ctxPaintFull ctx)
  beginLayer (ctxDrawArena ctx) LayerBackground
  unless paintFull $ do
    damage <- takeDamage ctx
    paintDamageClip ctx damage =<< takeDamagePieces ctx
  lowerShapes ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawWindowOverlays ctx
  drawModalOverlays ctx size
  drawPopupOverlays ctx
  drawSelectOverlays ctx frameInp
  drawTextEditMenuOverlays ctx frameInp
  drawData <- finishDraw (ctxDrawArena ctx)
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  pure (result, msgs, drawData, dirtyAfterUi)

-- Second UI pass after mirror store write. Keeps ctxStore, animations, and
-- prev rects; only rebuilds node arena and id scopes.
resetUiBuild :: Context -> IO ()
resetUiBuild ctx = do
  beginThemeScopes ctx False
  resetNodeArena (ctxNodeArena ctx)
  resetUiBuildScopes ctx

-- | Start a clip frame from the window backdrop, as a full frame starts from a
-- window-coloured clear. Widgets with a transparent fill, such as an idle
-- menu-bar title, draw nothing over the pixels they covered, so without the
-- backdrop a hover that just ended would stay in the retain texture. Damage
-- in pieces paints a backdrop over each, and every command is cut to them.
paintDamageClip :: Context -> Damage -> [Rect] -> IO ()
paintDamageClip _ DamageFull _ = pure ()
paintDamageClip ctx (DamageClip r) pieces = do
  let da = ctxDrawArena ctx
      clip = rectInflate 1 r
      pieceClips = map (rectInflate 1) pieces
      backdrops = if null pieces then [clip] else pieceClips
  setClip da clip
  setClipPieces da pieceClips
  when (rectNonEmpty r) $ do
    theme <- readIORef (ctxTheme ctx)
    mapM_ (flip (pushRect da) (themeWindow theme)) backdrops

resetUiBuildScopes :: Context -> IO ()
resetUiBuildScopes ctx = do
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxIdContext ctx) initialIdContext
  writeIORef (ctxFocusablesCount ctx) 0
  writeIORef (ctxHotId ctx) (WidgetId 0)
  writeIORef (ctxCursorZones ctx) []
  resetDrawingScopeCache ctx

-- | Solve and place everything, floating panels included, then snapshot the
-- result for the next frame to reuse. The solve measures only the nodes
-- whose restore keys changed since the cache's capture; the keys are folded
-- here, so frames that reuse the whole layout never compute them, and a
-- re-solve after the arena changed sees current ones. A cache taken under
-- other font metrics measured text differently, so none of it is restored.
solveLayoutAndCapture :: Context -> Float -> Float -> IO ()
solveLayoutAndCapture ctx w h = do
  computeSubtreeHashes (ctxNodeArena ctx)
  gen <- readIORef (ctxMetricGen ctx)
  mCache <-
    readIORef (ctxLayoutCache ctx) <&> \case
      Just (c, _, cachedGen) | cachedGen == gen -> Just c
      _ -> Nothing
  measures <- solveLayout (ctxNodeArena ctx) (contextMeasurers ctx) w h mCache
  captureLayout ctx (Size w h) measures
  placeFloating ctx w h

-- | Place modals, windows and popups over a solved layout. Their places
-- depend on state outside the arena (window positions, popup anchors), so
-- they are placed every frame, including one whose solve was reused.
placeFloating :: Context -> Float -> Float -> IO ()
placeFloating ctx w h = do
  let na = ctxNodeArena ctx
      ms = contextMeasurers ctx
  floating <- floatingNodeCount na
  when (floating > 0) $ do
    placeModals na ms w h
    placeWindows na ms w h (lookupWindowPos ctx) (lookupWindowSize ctx)
    placePopups na ms w h (lookupPopupConfig ctx)

-- | Put back this frame's solve, as captured before placement, and place the
-- floating panels again. 'False' when there is no such capture.
replaceFloating :: Context -> Size -> IO Bool
replaceFloating ctx size = restoreCachedLayout ctx size (\_ -> pure True)

-- | Reuse solved geometry for unchanged layout inputs, checked by the input
-- signature and by every custom measure still returning its recorded size.
-- Floating panels are placed again over the reused solve ('placeFloating').
tryReuseLayout :: Context -> Size -> IO Bool
tryReuseLayout ctx size = restoreCachedLayout ctx size (layoutReuseValid ctx)

-- | Layout reuse is sound when the frame's layout inputs hash to what the
-- cache captured, the same widgets register custom measures, and every custom
-- measure still returns the sizes the captured solve recorded. The signature
-- covers the measure's declared inputs, but neither a hook's presence nor
-- what its closure reads is arena state, which only comparing the hooks and
-- running them again can check.
layoutReuseValid :: Context -> LayoutCache -> IO Bool
layoutReuseValid ctx lc = do
  okSig <- layoutSigMatches (ctxNodeArena ctx) lc
  hooks <- customMeasureHooks ctx
  -- A matching signature means the same node count, so every recorded index
  -- is in range. Each recorded measure runs again and must match.
  let stable (idx, r) rest = customMeasureRecord ctx idx >>= \c -> if c == Just r then rest else pure False
  if not okSig || hooks /= lcMeasureHooks lc
    then pure False
    else foldr stable (pure True) (IM.toList (lcMeasures lc))

-- | A drawing node's custom measurement as the layout cache records it
-- ('runCustomMeasure'). 'Nothing' for any other node or a drawing with no
-- measure hook.
customMeasureRecord :: Context -> NodeIdx -> IO (Maybe CustomMeasureRecord)
customMeasureRecord ctx idx = do
  let na = ctxNodeArena ctx
  nt <- getNodeType na idx
  if nt /= NodeDrawing
    then pure Nothing
    else
      getWidgetId na idx >>= lookupCustomMeasure ctx >>= \case
        Nothing -> pure Nothing
        Just fn -> Just <$> runCustomMeasure na (ctxFontMetrics ctx) fn idx

-- | Restore the cached solve for this size and font generation when @valid@
-- accepts it, and place the floating panels over it.
restoreCachedLayout :: Context -> Size -> (LayoutCache -> IO Bool) -> IO Bool
restoreCachedLayout ctx size@(Size w h) valid = do
  gen <- readIORef (ctxMetricGen ctx)
  readIORef (ctxLayoutCache ctx) >>= \case
    Just (c, cachedSize, cachedGen)
      | cachedSize == size && cachedGen == gen -> do
          ok <- valid c
          when ok $ do
            restoreLayoutCache (ctxNodeArena ctx) c
            placeFloating ctx w h
          pure ok
    _ -> pure False

-- | Snapshot the solved layout, before floating placement, so the next frame
-- can reuse it.
captureLayout :: Context -> Size -> IM.IntMap CustomMeasureRecord -> IO ()
captureLayout ctx size measures = do
  eligible <- layoutCacheEligible (ctxNodeArena ctx)
  if not eligible
    then writeIORef (ctxLayoutCache ctx) Nothing
    else do
      gen <- readIORef (ctxMetricGen ctx)
      mc <- readIORef (ctxLayoutCache ctx)
      c0 <- case mc of
        Just (c, _, _) -> pure c
        Nothing -> newLayoutCache 64
      c <- captureLayoutCache (ctxNodeArena ctx) c0
      -- The registered hooks, so reuse can tell when one appears or goes.
      hooks <- customMeasureHooks ctx
      let c' = c {lcMeasures = measures, lcMeasureHooks = hooks}
      writeIORef (ctxLayoutCache ctx) (Just (c', size, gen))
