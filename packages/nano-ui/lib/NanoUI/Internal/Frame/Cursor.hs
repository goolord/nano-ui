{-# LANGUAGE DataKinds #-}

-- | Choose a cursor from active gestures and the solved widget geometry.
module NanoUI.Internal.Frame.Cursor
  ( UiCursorKind (..)
  , uiCursorKind
  , pointerCursorWanted
  , cursorKindIs
  )
where

import Control.Monad (forM)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (asum)
import Data.IORef (readIORef)
import Data.IntMap.Strict qualified as IM
import Data.Maybe (fromMaybe, isJust)
import NanoUI.Internal.Context
  ( Context (..)
  , InteractionState (..)
  , PointerRoute (..)
  , WidgetStore (..)
  , getHotId
  , getScrollDrag
  , getStore
  , getsInteraction
  , isDisabled
  , lookupCustomCursor
  )
import NanoUI.Internal.Font (FontMetrics, sliderHandleSlack, sliderTrackBounds)
import NanoUI.Internal.Frame.Hit
  ( findNodeByWidgetId
  , nodePointVisible
  , scrollHitRect
  , withWidgetNode
  )
import NanoUI.Internal.Frame.Scroll (ScrollBarLayout (..), scrollBarsFor)
import NanoUI.Internal.Frame.Select (overlayMenuOwnerAt)
import NanoUI.Internal.Frame.TextArea.Content (isMouseOnTextAreaScrollBarAt)
import NanoUI.Internal.Frame.TextEdit.Menu
  ( textEditMenuCursorKind
  , textFieldWidgetAtMouse
  )
import NanoUI.Internal.Frame.TextInput (nodeTextFieldGeom, searchClearHit)
import NanoUI.Internal.Frame.Window (windowResizeCursorKind)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
  ( Input (..)
  , UiCursorKind (..)
  , grabDragKind
  , grabHoverKind
  , inputMouseDown
  , inputMousePos
  )
import NanoUI.Internal.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , findChildM
  , findNodeM
  , getDirection
  , getNodeType
  , getParent
  , getRect
  , getStyleIdx
  , getWidgetId
  , isScrollNode
  , walkAncestors
  )
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains)
import NanoUI.Internal.WidgetText (isTableHeaderStyle, numericStepperRects, textInputNumericMode)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)

-- | Cursor requested by current gestures and hit tests against the solved arena.
-- The backend maps this result to a native cursor shape.
uiCursorKind :: Context -> Input -> IO UiCursorKind
uiCursorKind ctx inp = do
  -- The first query with an opinion wins; later ones do not run.
  mKind <-
    runMaybeT . asum . map MaybeT $
      [ textEditMenuCursorKind ctx inp
      , selectDropdownCursorKind ctx inp
      , windowResizeCursorKind ctx inp
      , tableColResizeCursorKind ctx inp
      , scrollThumbCursorKind ctx inp
      , textFieldHoverCursorKind ctx inp
      ]
  case mKind of
    Just k -> pure k
    Nothing -> do
      let
        mouse = inputMousePos inp
      active <- readIORef (ctxActiveId ctx)
      activeKind <- cursorKindAt ctx active mouse inp
      if activeKind /= UiCursorDefault
        then pure activeKind
        else do
          hot <- getHotId ctx
          cursorKindAt ctx hot mouse inp

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
scrollThumbCursorKind ctx inp = do
  mDrag <- getScrollDrag ctx
  if inputMouseDown inp && isJust mDrag
    then pure (Just UiCursorGrabbing)
    else do
      onThumb <- scrollThumbHit ctx (inputMousePos inp)
      pure (if onThumb then Just (grabHoverKind True inp) else Nothing)

-- Field well, not the label. Independent of focus and hot. A search field's
-- clear button raises the pointer cursor; everywhere else over a field is text.
textFieldHoverCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textFieldHoverCursorKind ctx inp = do
  let
    mouse = inputMousePos inp
  mWid <- textFieldWidgetAtMouse ctx mouse
  case mWid of
    Nothing -> pure Nothing
    Just wid -> do
      onClear <- searchClearHit ctx wid mouse
      onStepper <- numericStepperHit ctx wid mouse
      pure (Just (if onClear || onStepper then UiCursorPointer else UiCursorText))

-- | Whether the pointer is over a numeric field's stepper, which takes the
-- pointer cursor rather than the text cursor.
numericStepperHit :: Context -> WidgetId -> V2 -> IO Bool
numericStepperHit ctx wid mouse =
  findNodeByWidgetId ctx wid >>= \case
    Nothing -> pure False
    Just idx -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      if not (textInputNumericMode si)
        then pure False
        else do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          let
            (up, down) = numericStepperRects x y w h
          pure (rectContains up mouse || rectContains down mouse)

scrollThumbHit :: Context -> V2 -> IO Bool
scrollThumbHit ctx mouse =
  fmap isJust . findNodeM na $ \idx ->
    ((\nt -> nt == NodeTextArea || isScrollNode nt) <$> getNodeType na idx) <&&> do
      wid <- getWidgetId na idx
      any (\(_, layout, _) -> rectContains (sbThumb layout) mouse)
        <$> scrollBarsFor ctx idx wid
 where
  na = ctxNodeArena ctx

cursorKindAt :: Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
cursorKindAt ctx wid mouse inp
  | hashWidgetId wid == 0 = pure UiCursorDefault
  | otherwise = do
      disabled <- isDisabled ctx wid
      if disabled
        then pure UiCursorDefault
        else do
          mCursorFn <- lookupCustomCursor ctx wid
          case mCursorFn of
            Just cursorFn -> do
              visible <- widgetVisibleAt ctx wid mouse
              if not visible
                then pure UiCursorDefault
                else cursorFn <$> mkCustomDrawContext ctx (ctxFontMetrics ctx) wid
            Nothing -> do
              -- Resolve the node through the arena's id index rather than
              -- building a type table of every widget for two lookups.
              mNodeType <-
                findNodeByWidgetId ctx wid >>= traverse (getNodeType (ctxNodeArena ctx))
              case mNodeType of
                Just NodeButton -> widgetPointerCursor ctx wid mouse
                Just NodeCheckbox -> widgetPointerCursor ctx wid mouse
                Just NodeRadio -> widgetPointerCursor ctx wid mouse
                Just NodeTree -> widgetPointerCursor ctx wid mouse
                Just NodeSelect -> selectCursorKind ctx wid mouse
                Just NodeColorPicker -> pure UiCursorPointer
                Just NodeTextInput -> textInputCursorKind ctx wid mouse
                Just NodeTextArea -> textAreaCursorKind ctx wid mouse
                Just NodeSlider -> sliderCursorKind ctx wid mouse inp
                _ -> pure UiCursorDefault

selectCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
selectCursorKind ctx wid mouse = do
  visible <- widgetVisibleAt ctx wid mouse
  if not visible
    then pure UiCursorDefault
    else do
      mrect <- scrollHitRect ctx wid
      pure
        ( if maybe False (`rectContains` mouse) mrect
            then UiCursorPointer
            else UiCursorDefault
        )

widgetVisibleAt :: Context -> WidgetId -> V2 -> IO Bool
widgetVisibleAt ctx wid mouse = do
  withWidgetNode ctx wid False $ \idx -> nodePointVisible ctx idx mouse

widgetPointerCursor :: Context -> WidgetId -> V2 -> IO UiCursorKind
widgetPointerCursor ctx wid mouse = do
  visible <- widgetVisibleAt ctx wid mouse
  pure (if visible then UiCursorPointer else UiCursorDefault)

sliderCursorKind :: Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
sliderCursorKind ctx wid mouse inp = do
  active <- readIORef (ctxActiveId ctx)
  if active == wid && inputMouseDown inp
    then pure UiCursorGrabbing
    else do
      visible <- widgetVisibleAt ctx wid mouse
      if not visible
        then pure UiCursorDefault
        else do
          mrect <- scrollHitRect ctx wid
          pure $
            case mrect of
              Nothing -> UiCursorDefault
              Just (Rect x y w h) ->
                let
                  Rect tx ty tw th = sliderTrackBounds x y w h
                  hitRect = Rect tx (ty - sliderHandleSlack) tw (th + 2 * sliderHandleSlack)
                 in
                  grabDragKind (rectContains hitRect mouse) False inp

textInputCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textInputCursorKind ctx wid mouse = do
  visible <- widgetVisibleAt ctx wid mouse
  if not visible
    then pure UiCursorDefault
    else do
      mIdx <- findNodeByWidgetId ctx wid
      mrect <- scrollHitRect ctx wid
      case (mIdx, mrect) of
        (Just idx, Just (Rect x y w h)) -> do
          (field, _) <- nodeTextFieldGeom ctx idx x y w h
          onStepper <- numericStepperHit ctx wid mouse
          pure $
            if onStepper
              then UiCursorPointer
              else if rectContains field mouse then UiCursorText else UiCursorDefault
        _ -> pure UiCursorDefault

textAreaCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textAreaCursorKind ctx wid mouse = do
  withWidgetNode ctx wid UiCursorDefault $ \idx -> do
    onScroll <- isMouseOnTextAreaScrollBarAt ctx idx mouse
    if onScroll
      then pure UiCursorDefault
      else textFieldCursorKind ctx wid mouse $ \_ x y w h ->
        Rect x y w h

textFieldCursorKind ::
  Context
  -> WidgetId
  -> V2
  -> (FontMetrics -> Float -> Float -> Float -> Float -> Rect)
  -> IO UiCursorKind
textFieldCursorKind ctx wid mouse fieldAt = do
  visible <- widgetVisibleAt ctx wid mouse
  if not visible
    then pure UiCursorDefault
    else do
      mrect <- scrollHitRect ctx wid
      pure $
        case mrect of
          Just (Rect x y w h)
            | rectContains (fieldAt (ctxFontMetrics ctx) x y w h) mouse -> UiCursorText
          _ -> UiCursorDefault

tableColResizeCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
tableColResizeCursorKind ctx inp = do
  store <- getStore ctx
  let
    dragging = any (\n -> n <= -1000 && n > -2000) (IM.elems (storeInt store))
    na = ctxNodeArena ctx
    V2 mx my = inputMousePos inp
  if inputMouseDown inp && dragging
    then pure (Just UiCursorEwResize)
    else do
      mEdge <-
        findNodeM na $ \idx -> do
          nt <- getNodeType na idx
          if nt /= NodeButton
            then pure False
            else do
              si <- getStyleIdx na idx
              if not (isTableHeaderStyle si)
                then pure False
                else do
                  (x, y, w, h) <- getRect na idx
                  -- The resize cursor spans the whole column height
                  -- (header plus body cells down to the body
                  -- scroller's bottom edge), matching the drag grab
                  -- zone: tableBodyScrollerBottom locates the same
                  -- body scroller whose rect the grab zone anchors
                  -- on (its prev-frame value, readable at build
                  -- time), so the two zones cannot disagree.
                  yBot <- fromMaybe (y + h) <$> tableBodyScrollerBottom ctx idx
                  pure (my >= y && my <= yBot && abs (mx - (x + w)) <= 4 && w > 0 && h > 0)
      pure (UiCursorEwResize <$ mEdge)

-- | Bottom edge of a table's body scroller, located structurally from one
-- of its header buttons: walk up to the first ancestor that has a direct
-- Column-direction scroll-container child (the pane column built by
-- tableSplitPanes) and take that child's rect bottom. Runs post-solve, so
-- the rect is current-frame. Nothing when no such scroller exists (the
-- caller falls back to the header button's own bottom).
tableBodyScrollerBottom :: Context -> NodeIdx -> IO (Maybe Float)
tableBodyScrollerBottom ctx idx = do
  parent <- getParent na idx
  mScroller <- walkAncestors na parent $ \p ->
    findChildM na p $ \c -> do
      nt <- getNodeType na c
      if isScrollNode nt
        then (== DirColumn) <$> getDirection na c
        else pure False
  forM mScroller $ \sc -> do
    (_, sy, _, sh) <- getRect na sc
    pure (sy + sh)
 where
  na = ctxNodeArena ctx

-- | Whether 'uiCursorKind' requests the link/button pointer cursor.
pointerCursorWanted :: Context -> Input -> IO Bool
pointerCursorWanted ctx inp = cursorKindIs ctx inp UiCursorPointer

-- | Test the resolved cursor against a requested shape.
cursorKindIs :: Context -> Input -> UiCursorKind -> IO Bool
cursorKindIs ctx inp want = (== want) <$> uiCursorKind ctx inp
