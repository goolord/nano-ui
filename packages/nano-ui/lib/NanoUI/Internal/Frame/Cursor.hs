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
import Data.Maybe (fromMaybe, isJust)
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
import NanoUI.Internal.Frame.TextArea (isMouseOnTextAreaScrollBarAt)
import NanoUI.Internal.Frame.TextEdit
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
import NanoUI.Internal.Monad (ifM, (<&&>))
import NanoUI.Internal.Types (Rect (..), V2 (..), rectContains)
import NanoUI.Internal.WidgetText (hasFlag, numericStepperRects, textInputFlagNumeric)
import NanoUI.Internal.Widgets.Custom (mkCustomDrawContext)

-- | Cursor requested by current gestures and hit tests against the solved arena.
-- The backend maps this result to a native cursor shape.
uiCursorKind :: Context -> Input -> IO UiCursorKind
uiCursorKind ctx inp =
  -- The first query with an opinion wins; later ones do not run. The active
  -- widget, then the hot one, has an opinion unless it asks for the default.
  fmap (fromMaybe UiCursorDefault) . runMaybeT . asum . map MaybeT $
    [ textEditMenuCursorKind ctx inp
    , selectDropdownCursorKind ctx inp
    , windowResizeCursorKind ctx inp
    , cursorZoneKind ctx inp
    , scrollThumbCursorKind ctx inp
    , textFieldHoverCursorKind ctx inp
    , widgetKind =<< readIORef (ctxActiveId ctx)
    , widgetKind =<< getHotId ctx
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
scrollThumbCursorKind ctx inp = do
  mDrag <- getsInteraction ctx isScrollDrag
  if inputMouseDown inp && isJust mDrag
    then pure (Just UiCursorGrabbing)
    else do
      let na = ctxNodeArena ctx
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
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      let
        (up, down) = numericStepperRects x y w h
      pure (rectContains up mouse || rectContains down mouse)

-- | The cursor widget @wid@ asks for with the pointer at @mouse@: the default
-- unless the pointer is on the visible part of its node, and a custom
-- widget's own choice.
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
          let hitRect = if visible then scrollHitRect ctx wid else pure Nothing
              over kind r = if rectContains r mouse then kind else UiCursorDefault
              whenVisible kind = pure (if visible then kind else UiCursorDefault)
          mCursorFn <- (>>= cdrCursor) <$> lookupCustomDrawing ctx wid
          case mCursorFn of
            Just cursorFn
              | visible -> cursorFn <$> mkCustomDrawContext ctx (ctxFontMetrics ctx) wid
              | otherwise -> pure UiCursorDefault
            Nothing ->
              getNodeType (ctxNodeArena ctx) idx >>= \case
                NodeButton -> whenVisible UiCursorPointer
                NodeCheckbox -> whenVisible UiCursorPointer
                NodeRadio -> whenVisible UiCursorPointer
                NodeTree -> whenVisible UiCursorPointer
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
                  if active == wid && inputMouseDown inp
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
