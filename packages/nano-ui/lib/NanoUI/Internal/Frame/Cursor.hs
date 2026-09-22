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
import Data.Foldable (asum, find)
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import NanoUI.Internal.Context
  ( Context (..)
  , InteractionState (..)
  , PointerRoute (..)
  , getHotId
  , getsInteraction
  , isDisabled
  , lookupCustomDrawing
  , CustomDrawingEntry (..)
  )
import NanoUI.Internal.Font (sliderHitBounds)
import NanoUI.Internal.Frame.Hit
  ( nodePointVisible
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
  ( NodeClass (PointerNodes)
  , NodeIdx
  , NodeType (..)
  , findClassNodeM
  , getNodeType
  , getRect
  , getStyleIdx
  , getWidgetId
  , isScrollNode
  )
import NanoUI.Internal.Monad ((<&&>))
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains)
import NanoUI.Internal.WidgetText (hasFlag, numericStepperRects, textInputFlagNumeric)
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
      , cursorZoneKind ctx inp
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
  mDrag <- getsInteraction ctx isScrollDrag
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
  forM mWid $ \wid -> do
    onClear <- searchClearHit ctx wid mouse
    onStepper <- numericStepperHit ctx wid mouse
    pure (if onClear || onStepper then UiCursorPointer else UiCursorText)

-- | Whether the pointer is over a numeric field's stepper, which takes the
-- pointer cursor rather than the text cursor.
numericStepperHit :: Context -> WidgetId -> V2 -> IO Bool
numericStepperHit ctx wid mouse =
  withWidgetNode ctx wid False $ \idx -> numericStepperHitAt ctx idx mouse

-- | 'numericStepperHit' for an already-resolved node.
numericStepperHitAt :: Context -> NodeIdx -> V2 -> IO Bool
numericStepperHitAt ctx idx mouse = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  if not (hasFlag textInputFlagNumeric si)
    then pure False
    else do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      let
        (up, down) = numericStepperRects x y w h
      pure (rectContains up mouse || rectContains down mouse)

scrollThumbHit :: Context -> V2 -> IO Bool
scrollThumbHit ctx mouse =
  fmap isJust . findClassNodeM na PointerNodes $ \idx ->
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
          mCursorFn <- (>>= cdrCursor) <$> lookupCustomDrawing ctx wid
          case mCursorFn of
            Just cursorFn -> do
              visible <- widgetVisibleAt ctx wid mouse
              if not visible
                then pure UiCursorDefault
                else cursorFn <$> mkCustomDrawContext ctx (ctxFontMetrics ctx) wid
            Nothing -> do
              -- Resolve the node through the arena's id index rather than
              -- building a type table of every widget for two lookups.
              withWidgetNode ctx wid UiCursorDefault $ \idx ->
                getNodeType (ctxNodeArena ctx) idx >>= \case
                  NodeButton -> widgetPointerCursor ctx idx mouse
                  NodeCheckbox -> widgetPointerCursor ctx idx mouse
                  NodeRadio -> widgetPointerCursor ctx idx mouse
                  NodeTree -> widgetPointerCursor ctx idx mouse
                  NodeSelect -> rectCursorKind UiCursorPointer ctx idx wid mouse
                  NodeColorPicker -> pure UiCursorPointer
                  NodeTextInput -> textInputCursorKind ctx idx wid mouse
                  NodeTextArea -> textAreaCursorKind ctx idx wid mouse
                  NodeSlider -> sliderCursorKind ctx idx wid mouse inp
                  _ -> pure UiCursorDefault

-- | @kind@ over the widget's visible rect, the default cursor elsewhere.
rectCursorKind :: UiCursorKind -> Context -> NodeIdx -> WidgetId -> V2 -> IO UiCursorKind
rectCursorKind kind ctx idx wid mouse = do
  mrect <- visibleHitRect ctx idx wid mouse
  pure (if maybe False (`rectContains` mouse) mrect then kind else UiCursorDefault)

-- | The widget's hit rect, or 'Nothing' when the pointer is clipped off it.
visibleHitRect :: Context -> NodeIdx -> WidgetId -> V2 -> IO (Maybe Rect)
visibleHitRect ctx idx wid mouse = do
  visible <- nodePointVisible ctx idx mouse
  if visible then scrollHitRect ctx wid else pure Nothing

widgetVisibleAt :: Context -> WidgetId -> V2 -> IO Bool
widgetVisibleAt ctx wid mouse = do
  withWidgetNode ctx wid False $ \idx -> nodePointVisible ctx idx mouse

widgetPointerCursor :: Context -> NodeIdx -> V2 -> IO UiCursorKind
widgetPointerCursor ctx idx mouse = do
  visible <- nodePointVisible ctx idx mouse
  pure (if visible then UiCursorPointer else UiCursorDefault)

sliderCursorKind :: Context -> NodeIdx -> WidgetId -> V2 -> Input -> IO UiCursorKind
sliderCursorKind ctx idx wid mouse inp = do
  active <- readIORef (ctxActiveId ctx)
  if active == wid && inputMouseDown inp
    then pure UiCursorGrabbing
    else do
      mrect <- visibleHitRect ctx idx wid mouse
      pure $
        case mrect of
          Nothing -> UiCursorDefault
          Just (Rect x y w h) ->
            grabDragKind (rectContains (sliderHitBounds x y w h) mouse) False inp

textInputCursorKind :: Context -> NodeIdx -> WidgetId -> V2 -> IO UiCursorKind
textInputCursorKind ctx idx wid mouse = do
  mrect <- visibleHitRect ctx idx wid mouse
  case mrect of
    Just (Rect x y w h) -> do
      (field, _) <- nodeTextFieldGeom ctx idx x y w h
      onStepper <- numericStepperHitAt ctx idx mouse
      pure $
        if onStepper
          then UiCursorPointer
          else if rectContains field mouse then UiCursorText else UiCursorDefault
    Nothing -> pure UiCursorDefault

textAreaCursorKind :: Context -> NodeIdx -> WidgetId -> V2 -> IO UiCursorKind
textAreaCursorKind ctx idx wid mouse = do
  onScroll <- isMouseOnTextAreaScrollBarAt ctx idx mouse
  if onScroll
    then pure UiCursorDefault
    else rectCursorKind UiCursorText ctx idx wid mouse

-- | The cursor of the newest zone the view registered under the pointer
-- ('ctxCursorZones'), or the resize arrow for a whole column-resize drag.
cursorZoneKind :: Context -> Input -> IO (Maybe UiCursorKind)
cursorZoneKind ctx inp = do
  dragging <- getsInteraction ctx isColumnResize
  if inputMouseDown inp && dragging
    then pure (Just UiCursorEwResize)
    else do
      let mouse = inputMousePos inp
      fmap snd . find (\(r, _) -> rectContains r mouse) <$> readIORef (ctxCursorZones ctx)

-- | Whether 'uiCursorKind' requests the link/button pointer cursor.
pointerCursorWanted :: Context -> Input -> IO Bool
pointerCursorWanted ctx inp = cursorKindIs ctx inp UiCursorPointer

-- | Test the resolved cursor against a requested shape.
cursorKindIs :: Context -> Input -> UiCursorKind -> IO Bool
cursorKindIs ctx inp want = (== want) <$> uiCursorKind ctx inp
