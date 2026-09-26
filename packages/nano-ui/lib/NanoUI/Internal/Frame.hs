-- | Complete headless frames: view construction, layout, input resolution,
-- damage, and draw-buffer generation. Backends own event waits and presentation.
module NanoUI.Internal.Frame
  ( runFrame
  , runFrameEff
  , runFrameReduce
  , runFrameReduceEff
  )
where

import Control.Monad (unless, when)
import Data.IORef (modifyIORef', readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Damage (FrameSnapshot (..), captureFrameSnapshot, updatePrevRects, writeDamage)
import NanoUI.Internal.Draw
import NanoUI.Internal.Frame.Input
import NanoUI.Internal.Frame.Chrome (overlayMenuStyle, overlayWindowStyle, paintMenuPanel)
import NanoUI.Internal.Frame.Explain (explainFrame, paintExplainHover, paintExplainLayer, paintExplainPage)
import NanoUI.Internal.Frame.Paint (lowerShapes, walkChildren)
import NanoUI.Internal.Frame.Scroll
import NanoUI.Internal.Frame.Select
import NanoUI.Internal.Frame.TextArea (finalizeTextFieldMouse)
import NanoUI.Internal.Frame.TextEdit
import NanoUI.Internal.Frame.TextInput (claimComposition, settleInputMethod)
import NanoUI.Internal.Frame.Window
import NanoUI.Internal.Id (WidgetId (..), initialIdContext)
import NanoUI.Internal.Input (Input (..), Key (..), MouseButton (..), Pressable (..), inputKeysNull, stripInteractionInput, withoutPointer)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Layout.Solve (placeFloatingNodes, runCustomMeasure, solveLayout)
import NanoUI.Internal.Monad (NanoUI, Ui, runUi, whenM)
import NanoUI.Internal.Store (mirrorStoresChanged)
import NanoUI.Internal.Style (Padding (..), Theme (..), themeOverlayDim, themeSeparator)
import NanoUI.Internal.Tasks (sweepHeld)
import NanoUI.Internal.Types (Damage (..), Rect (..), Size (..), rectInflate, rectNonEmpty)
import NanoUI.Internal.Widgets.Overlay (windowChromeSepH, windowTitleBarH)
import NanoUI.Internal.Widgets.Sensor (beginSensors, updateSensors)

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
runFrameEff unlift ctx rawInp ui = do
  ensureMetricCaches ctx
  snap <- captureFrameSnapshot ctx
  themeBefore <- readIORef (ctxTheme ctx)
  clearDirty ctx
  -- Timed wakes are re-requested by whatever is still built this frame.
  clearWakeAt ctx
  -- A thread may have changed what the view reads before waking the loop,
  -- so repaint in full.
  takeThreadWake ctx
  -- The focused field gets any IME composition, and while one shows, the IME
  -- owns the keys.
  (frameInp, imeKeys) <- claimComposition ctx rawInp
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
  -- A widget covered by another gets no pointer.
  recordCoveredWidgets ctx route frameInp
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
  -- Read from the last frame's nodes, before the build resets them.
  recordFocusKind ctx imeKeys
  resetDrawArena (ctxDrawArena ctx)
  resetUiBuild ctx True
  beginFrameModal ctx
  -- An Escape the IME consumed must not also quit the app.
  when (inputKeysNull (inputKeys frameInp) && pressedIn KeyEscape rawInp) $
    markEscapeConsumed ctx
  writeIORef (ctxReleaseClickedId ctx) (WidgetId 0)
  armPointerPress ctx frameInp
  focusBefore <- readIORef (ctxFocusId ctx)
  result0 <- unlift (runUi ctx frameInp ui)
  -- Pending click is one-shot. Clear before a mirror rebuild so toggles do not fire twice.
  writeIORef (ctxClickedId ctx) (WidgetId 0)
  storeMid <- getStore ctx
  result <-
    if mirrorStoresChanged (fsStore snap) storeMid
      then do
        resetUiBuild ctx False
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
  settleViewTheme ctx themeBefore
  let
    size@(Size w h) = inputWindowSize frameInp
  layoutArena ctx size True
  movedResize <- updateWindowResize ctx layerInp w h
  movedWindow <- updateWindowDrag ctx layerInp
  -- A window moved or resized changes only where the floating panels go:
  -- the solve before placement stands, so place them again over it.
  when (movedResize || movedWindow) $
    layoutArena ctx size False
  persistWindowPositions ctx
  applyScrollOffsets ctx size
  -- A press on a menu or dropdown leaves nothing active, whatever a release
  -- that never arrived left behind.
  when (pressedIn MouseLeft frameInp && not (pressedIn MouseLeft layerInp)) $
    writeIORef (ctxActiveId ctx) (WidgetId 0)
  targets <- pressTargets ctx layerInp
  finalizePointerPress ctx targets
  finalizePointerRelease ctx layerInp
  disarmPointerPress ctx frameInp
  finalizeTextInputFocus ctx layerInp targets
  finalizeSelectFocus ctx targets
  finalizeTextFieldMouse ctx layerInp (ptFieldControl targets)
  closeTextEditMenuOnOutsideClick ctx frameInp
  openTextEditMenu ctx layerInp
  finalizeTextEditMenuPick ctx menuInp
  closeTextEditMenuOnEscape ctx frameInp
  finalizeFocusRequest ctx
  constrainFocusToModal ctx
  finalizeTabFocus ctx frameInp
  settleInputMethod ctx focusBefore
  finalizeSelectKeyboard ctx frameInp
  finalizeSelectPick ctx dropInp
  closeSelectOnOutsideClick ctx frameInp
  storeAfter <- getStore ctx
  -- No layout input follows the store, so the solve stands unless a custom
  -- measure moved.
  when (mirrorStoresChanged storeBuilt storeAfter) $ do
    layoutArena ctx size True
    applyScrollOffsets ctx size
  -- Layout is final, so the sensors record what the next view reads.
  updateSensors ctx size
  updatePrevRects ctx size
  refreshHover ctx frameInp
  refreshScrollBarHover ctx layerInp
  tickAnimations ctx (inputDeltaTime frameInp)
  sweepHeld ctx
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
  -- The layout overlay damages its own outlines, since the rect diffs below
  -- miss rows and columns that moved.
  explain <- getExplainLayout ctx
  when explain (explainFrame ctx frameInp)
  writeDamage ctx frameInp snap
  -- Clip frames repaint the damaged region of the retained texture, which
  -- preserves the other pixels ('paintDamageClip'). Full-present frames
  -- (fresh retain, forced full, continuous) paint everything.
  paintFull <- readIORef (ctxPaintFull ctx)
  beginLayer (ctxDrawArena ctx) LayerBackground
  unless paintFull $ do
    damage <- takeDamage ctx
    paintDamageClip ctx damage =<< takeDamagePieces ctx
  lowerShapes ctx
  -- Page outlines go above the page's scrollbars and below floating panels.
  when explain $ do
    beginLayer (ctxDrawArena ctx) LayerContent
    paintExplainPage ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawFloatingPanels ctx size explain
  drawSelectOverlays ctx frameInp
  drawTextEditMenuOverlays ctx frameInp
  when explain (paintExplainHover ctx)
  drawData <- finishDraw (ctxDrawArena ctx)
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  pure (result, msgs, drawData, dirtyAfterUi)

-- | Reset what a view run builds: the node arena, sensors, input method
-- request, and the container, id, focus, hover, cursor-zone, drawing and
-- layout-overlay scopes. A second run after a mirror store write
-- (@newFrame@ 'False') keeps the store, animations, prev rects, and the
-- theme scopes it compares against.
resetUiBuild :: Context -> Bool -> IO ()
resetUiBuild ctx newFrame = do
  beginThemeScopes ctx newFrame
  resetNodeArena (ctxNodeArena ctx)
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxIdContext ctx) initialIdContext
  writeIORef (ctxFocusablesCount ctx) 0
  -- The view re-requests the input method each run while a widget takes text.
  readIORef (ctxInputMethod ctx) >>= mapM_ (\_ -> writeIORef (ctxInputMethod ctx) Nothing)
  writeIORef (ctxHotId ctx) (WidgetId 0)
  writeIORef (ctxCursorZones ctx) []
  writeIORef (ctxCursorRegions ctx) []
  resetDrawingScopeCache ctx
  -- The layout overlay's scopes are ranges of the arena just emptied.
  explain <- readIORef (ctxExplain ctx)
  unless (null (esScopes explain)) $ writeIORef (ctxExplain ctx) explain {esScopes = []}
  beginSensors ctx

-- | Paint the floating panels over the page: windows with their title-bar
-- separator, the modal backdrop and the modals, then popups. Each is a
-- menu-style panel in its node's theme with its subtree clipped inside.
-- With @explain@, the layout overlay's outlines are drawn over each.
drawFloatingPanels :: Context -> Size -> Bool -> IO ()
drawFloatingPanels ctx@Context {ctxNodeArena = na, ctxDrawArena = da} (Size ww wh) explain = do
  let panels nt style after = forFloatingNodes_ na nt $ \idx -> do
        rect <- getNodeRect na idx
        theme <- nodeTheme ctx idx
        paintMenuPanel da theme (style theme) rect
        withClip da rect (walkChildren ctx idx)
        after theme idx rect
        when explain (paintExplainLayer ctx idx)
      plain _ _ _ = pure ()
  panels NodeWindow overlayWindowStyle $ \theme idx (Rect x y w _) -> do
    pad <- getPadding na idx
    let sepY = y + padT pad + windowTitleBarH - windowChromeSepH
        sepW = max 0 (w - padL pad - padR pad)
    pushRect da (Rect (x + padL pad) sepY sepW windowChromeSepH) (themeSeparator theme)
  whenM (isJust <$> topModalNode na) $ do
    theme <- readIORef (ctxTheme ctx)
    pushRect da (Rect 0 0 ww wh) (themeOverlayDim theme)
    panels NodeModal overlayMenuStyle plain
  panels NodePopup overlayMenuStyle plain

-- | Start a clip frame from the window backdrop, as a full frame starts from a
-- window-coloured clear. Widgets with a transparent fill, such as an idle
-- menu-bar title, draw nothing over the pixels they covered, so without the
-- backdrop a hover that just ended would stay in the retain texture. Damage
-- in pieces paints a backdrop over each, and every command is cut to them.
-- Each is inflated by a logical pixel to cover the runner's outward snap.
paintDamageClip :: Context -> Damage -> [Rect] -> IO ()
paintDamageClip _ DamageFull _ = pure ()
paintDamageClip ctx@Context {ctxDrawArena = da} (DamageClip r) pieces = do
  let clip = rectInflate 1 r
      pieceClips = map (rectInflate 1) pieces
      backdrops = if null pieces then [clip] else pieceClips
  setClip da clip
  setClipPieces da pieceClips
  when (rectNonEmpty r) $ do
    theme <- readIORef (ctxTheme ctx)
    mapM_ (flip (pushRect da) (themeWindow theme)) backdrops

-- | Lay out the arena and place the floating panels over it. The cached solve
-- is restored when it was taken at this size and font generation and @check@
-- accepts it ('layoutReuseValid'); otherwise the arena is solved and the
-- result, before placement, captured for the next frame. The solve measures
-- only the nodes whose restore keys changed since the cache's capture; the
-- keys are folded here, so frames that reuse the whole layout never compute
-- them. A cache taken under other font metrics measured text differently, so
-- none of it is restored. Floating panels depend on state outside the arena
-- (window positions, popup anchors), so they are placed every time.
layoutArena :: Context -> Size -> Bool -> IO ()
layoutArena ctx@Context {ctxNodeArena = na} size@(Size w h) check = do
  let ms = contextMeasurers ctx
  gen <- readIORef (ctxMetricGen ctx)
  cached <- readIORef (ctxLayoutCache ctx)
  reused <- case cached of
    Just (c, cachedSize, cachedGen)
      | cachedSize == size && cachedGen == gen -> do
          ok <- if check then layoutReuseValid ctx c else pure True
          when ok (restoreLayoutCache na c)
          pure ok
    _ -> pure False
  unless reused $ do
    computeSubtreeHashes na
    measures <- solveLayout na ms w h $ case cached of
      Just (c, _, cachedGen) | cachedGen == gen -> Just c
      _ -> Nothing
    n <- arenaCount na
    if n <= 0
      then writeIORef (ctxLayoutCache ctx) Nothing
      else do
        c <- captureLayoutCache na =<< maybe (newLayoutCache 64) (\(old, _, _) -> pure old) cached
        -- The registered hooks, so reuse can tell when one appears or goes.
        hooks <- customMeasureHooks ctx
        let c' = c {lcMeasures = measures, lcMeasureHooks = hooks}
        writeIORef (ctxLayoutCache ctx) (Just (c', size, gen))
  floating <- floatingNodeCount na
  when (floating > 0) $
    placeFloatingNodes na ms w h (lookupWindowPos ctx) (lookupWindowSize ctx) (lookupPopupConfig ctx)

-- | Layout reuse is sound when the frame's layout inputs hash to what the
-- cache captured, the same widgets register custom measures, and every custom
-- measure still returns the sizes the captured solve recorded. The signature
-- covers the measure's declared inputs, but neither a hook's presence nor
-- what its closure reads is arena state, which only comparing the hooks and
-- running them again can check.
layoutReuseValid :: Context -> LayoutCache -> IO Bool
layoutReuseValid ctx@Context {ctxNodeArena = na} lc = do
  okSig <- layoutSigMatches na lc
  hooks <- customMeasureHooks ctx
  -- A matching signature means the same node count, so every recorded index
  -- is in range. Each recorded measure runs again and must match: the
  -- drawing node's measure as 'runCustomMeasure' records it.
  let measureOf idx = do
        nt <- getNodeType na idx
        if nt /= NodeDrawing
          then pure Nothing
          else do
            mFn <- lookupCustomMeasure ctx =<< getWidgetId na idx
            traverse (\fn -> runCustomMeasure na (ctxFontMetrics ctx) fn idx) mFn
      stable (idx, r) rest = measureOf idx >>= \c -> if c == Just r then rest else pure False
  if not okSig || hooks /= lcMeasureHooks lc
    then pure False
    else foldr stable (pure True) (IM.toList (lcMeasures lc))
