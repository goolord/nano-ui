-- | Choose a cursor from active gestures and the solved widget geometry.
module NanoUI.Internal.Frame.Cursor
  ( UiCursorKind (..)
  , uiCursorKind
  , pointerCursorWanted
  , cursorKindIs
  )
where

import Control.Monad (forM, forM_, unless, when)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (asum, find)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import NanoUI.Internal.Context
import NanoUI.Internal.Font (sliderHitBounds)
import NanoUI.Internal.Frame.Hit
import NanoUI.Internal.Frame.Scroll (ScrollBarLayout (..), scrollBarsFor)
import NanoUI.Internal.Frame.Select (overlayMenuOwnerAt)
import NanoUI.Internal.Frame.TextArea (isMouseOnTextAreaScrollBarAt)
import NanoUI.Internal.Frame.TextEdit
import NanoUI.Internal.Frame.TextInput (nodeTextFieldGeom, searchClearHit)
import NanoUI.Internal.Frame.Window (windowResizeCursorKind)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Monad (ifM, whenM, (<&&>))
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains)
import NanoUI.Internal.WidgetText (hasFlag, numericStepperRects, textInputFlagNumeric)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)

-- | Cursor requested by current gestures and hit tests against the solved arena.
-- The backend maps this result to a native cursor shape.
uiCursorKind :: Context -> Input -> IO UiCursorKind
uiCursorKind ctx inp =
  -- The first query with an opinion wins; later ones do not run. The active
  -- widget, then the hot one, has an opinion unless it asks for the default.
  -- A 'withCursorShape' scope only fills in where nothing had one.
  fmap (fromMaybe UiCursorDefault) . runMaybeT . asum . map MaybeT $
    [ textEditMenuCursorKind ctx inp
    , selectDropdownCursorKind ctx inp
    , windowResizeCursorKind ctx inp
    , cursorZoneKind ctx inp
    , scrollThumbCursorKind ctx inp
    , textFieldHoverCursorKind ctx inp
    , widgetKind =<< readIORef (ctxActiveId ctx)
    , widgetKind =<< getHotId ctx
    , cursorRegionKind ctx inp
    ]
  where
    widgetKind wid = do
      kind <- cursorKindAt ctx wid (inputMousePos inp) inp
      pure (if kind == UiCursorDefault then Nothing else Just kind)

selectDropdownCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
selectDropdownCursorKind ctx inp = do
  -- A pick closes the dropdown with the button still down, and the pointer
  -- stays a pointer until it comes up.
  pickHeld <- getsInteraction ctx $ \s ->
    isPointerHeld s && case isPointerRoute s of
      RouteDropdown _ -> True
      _ -> False
  if pickHeld
    then pure (Just UiCursorPointer)
    else
      -- Over an open select's menu or a focused combo's. The text-edit menu
      -- never gets here: textEditMenuCursorKind runs first in uiCursorKind.
      (UiCursorPointer <$) <$> overlayMenuOwnerAt ctx (inputMousePos inp)

scrollThumbCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
scrollThumbCursorKind ctx@Context {ctxNodeArena = na} inp = do
  mDrag <- getsInteraction ctx isScrollDrag
  if heldIn MouseLeft inp && isJust mDrag
    then pure (Just UiCursorGrabbing)
    else do
      thumb <- findClassNodeM na PointerNodes $ \idx ->
        ((\nt -> nt == NodeTextArea || isScrollNode nt) <$> getNodeType na idx) <&&> do
          wid <- getWidgetId na idx
          any (\(_, layout, _) -> rectContains (sbThumb layout) (inputMousePos inp))
            <$> scrollBarsFor ctx idx wid
      pure (grabHoverKind True inp <$ thumb)

-- Field well, not the label. Independent of focus and hot. A search field's
-- clear button raises the pointer cursor; everywhere else over a field is text.
textFieldHoverCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textFieldHoverCursorKind ctx inp = do
  let
    mouse = inputMousePos inp
  mWid <- textFieldWidgetAtMouse ctx mouse
  forM mWid $ \wid -> do
    onClear <- searchClearHit ctx wid mouse
    onStepper <- withWidgetNode ctx wid False $ \idx -> numericStepperHit ctx idx mouse
    pure (if onClear || onStepper then UiCursorPointer else UiCursorText)

-- | Whether the pointer is over a numeric field's stepper, which takes the
-- pointer cursor rather than the text cursor.
numericStepperHit :: Context -> NodeIdx -> V2 -> IO Bool
numericStepperHit ctx idx mouse = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  if not (hasFlag textInputFlagNumeric si)
    then pure False
    else do
      Rect x y w h <- getNodeRect (ctxNodeArena ctx) idx
      let
        (up, down) = numericStepperRects x y w h
      pure (rectContains up mouse || rectContains down mouse)

-- | The cursor widget @wid@ asks for with the pointer at @mouse@: the default
-- unless the pointer is on the visible part of its node, and a custom
-- widget's own choice for its rect and the pointer, which it makes too while
-- a drag of it goes on off it.
cursorKindAt :: Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
cursorKindAt ctx wid mouse inp
  | hashWidgetId wid == 0 = pure UiCursorDefault
  | otherwise =
      ifM (isDisabled ctx wid) (pure UiCursorDefault) $
        -- Resolve the node through the arena's id index rather than building
        -- a type table of every widget for two lookups.
        withWidgetNode ctx wid UiCursorDefault $ \idx -> do
          visible <- nodePointVisible ctx idx mouse
          -- The widget's hit rect, where the pointer is not clipped off it.
          let hitRect = if visible then getPrevRect ctx wid else pure Nothing
              over kind r = if rectContains r mouse then kind else UiCursorDefault
              whenVisible kind = pure (if visible then kind else UiCursorDefault)
          mCursorFn <- (>>= cdrCursor) <$> lookupCustomDrawing ctx wid
          case mCursorFn of
            Just cursorFn -> do
              dragging <- pure (heldIn MouseLeft inp) <&&> ((== wid) <$> readIORef (ctxActiveId ctx))
              if visible || dragging
                then do
                  rect <- getNodeRect (ctxNodeArena ctx) idx
                  cdc <- mkCustomDrawContext ctx (ctxFontMetrics ctx) wid
                  pure (cursorFn cdc rect mouse)
                else pure UiCursorDefault
            Nothing ->
              getNodeType (ctxNodeArena ctx) idx >>= \case
                NodeButton -> whenVisible UiCursorPointer
                NodeSelect -> maybe UiCursorDefault (over UiCursorPointer) <$> hitRect
                NodeColorPicker -> pure UiCursorPointer
                NodeTextInput ->
                  hitRect >>= \case
                    Nothing -> pure UiCursorDefault
                    Just (Rect x y w h) -> do
                      (field, _) <- nodeTextFieldGeom ctx idx x y w h
                      onStepper <- numericStepperHit ctx idx mouse
                      pure (if onStepper then UiCursorPointer else over UiCursorText field)
                NodeTextArea ->
                  ifM (isMouseOnTextAreaScrollBarAt ctx idx mouse) (pure UiCursorDefault) $
                    maybe UiCursorDefault (over UiCursorText) <$> hitRect
                NodeSlider -> do
                  active <- readIORef (ctxActiveId ctx)
                  if active == wid && heldIn MouseLeft inp
                    then pure UiCursorGrabbing
                    else do
                      let onTrack (Rect x y w h) = rectContains (sliderHitBounds x y w h) mouse
                      maybe UiCursorDefault (\r -> grabDragKind (onTrack r) False inp) <$> hitRect
                _ -> pure UiCursorDefault

-- | The cursor of the newest zone the view registered under the pointer
-- ('ctxCursorZones'), or the resize arrow for a whole column-resize drag.
cursorZoneKind :: Context -> Input -> IO (Maybe UiCursorKind)
cursorZoneKind ctx inp = do
  dragging <- getsInteraction ctx isColumnResize
  if heldIn MouseLeft inp && dragging
    then pure (Just UiCursorEwResize)
    else do
      let mouse = inputMousePos inp
      fmap snd . find (\(r, _) -> rectContains r mouse) <$> readIORef (ctxCursorZones ctx)

-- | The shape of the innermost
-- 'NanoUI.Internal.Widgets.Cursor.withCursorShape' scope ('ctxCursorRegions')
-- that declared the node on top at the pointer ('nodeOnTopAt'), among the
-- nodes of the floating panel the pointer is confined to when there is one.
cursorRegionKind :: Context -> Input -> IO (Maybe UiCursorKind)
cursorRegionKind ctx inp =
  readIORef (ctxCursorRegions ctx) >>= \case
    [] -> pure Nothing
    regions -> do
      let mouse = inputMousePos inp
      top <- overlayHitRoot ctx mouse
      mIdx <- nodeOnTopAt ctx top mouse
      -- A scope comes before the scopes inside it, so the last one holding
      -- the node is the innermost.
      pure $ mIdx >>= \idx ->
        foldl' (\found (from, to, kind) -> if from <= idx && idx < to then Just kind else found) Nothing regions

-- | The node paint draws last among those whose visible part holds @mouse@:
-- in the floating panel @top@ when the pointer is confined to one, else on
-- the page, whose top-level nodes count in the order they were declared.
-- Paint draws a node's children over the node, in 'forChildrenInPaintOrder_'
-- (a layered container's later children and every pinned child over the rest), and each
-- floating panel as a layer of its own, over the page: windows, then modals,
-- then popups. A node other than a plain container clips its children to
-- itself, so where it misses the pointer none of them is looked at.
nodeOnTopAt :: Context -> Maybe NodeIdx -> V2 -> IO (Maybe NodeIdx)
nodeOnTopAt ctx@Context {ctxNodeArena = na} top mouse = do
  found <- newIORef Nothing
  let visit i = do
        hit <- nodePointVisible ctx i mouse
        when hit (writeIORef found (Just i))
        nt <- getNodeType na i
        when (hit || nt == NodeContainer) $
          forChildrenInPaintOrder_ na i visitLayer
      -- A floating panel is a layer of its own, not part of the one around it.
      visitLayer i = do
        floating <- isFloatingNode <$> getNodeType na i
        unless floating (visit i)
  case top of
    Just panel -> do
      visit panel
      -- The panels declared inside it, such as a menu opened in a modal, are
      -- drawn over it, in the order the frame paints panels. Only a modal
      -- confines the pointer while another panel is under it, so most frames
      -- find none.
      forM_ [NodeWindow, NodeModal, NodePopup] $ \nt ->
        forFloatingNodes_ na nt $ \i ->
          when (i > panel) $ whenM (nodeInSubtree ctx i panel) (visit i)
    Nothing -> do
      count <- arenaCount na
      forM_ [0 .. count - 1] $ \i -> do
        parent <- getParent na i
        when (parent < 0) (visitLayer i)
  readIORef found

-- | Whether 'uiCursorKind' requests the link/button pointer cursor.
pointerCursorWanted :: Context -> Input -> IO Bool
pointerCursorWanted ctx inp = cursorKindIs ctx inp UiCursorPointer

-- | Test the resolved cursor against a requested shape.
cursorKindIs :: Context -> Input -> UiCursorKind -> IO Bool
cursorKindIs ctx inp want = (== want) <$> uiCursorKind ctx inp
