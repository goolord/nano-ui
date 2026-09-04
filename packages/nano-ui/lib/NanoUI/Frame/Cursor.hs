{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.Cursor
  ( UiCursorKind (..)
  , grabHoverKind
  , grabDragKind
  , uiCursorKind
  , pointerCursorWanted
  , cursorKindIs
  , textFieldHoverCursorKind
  ) where
import Data.IORef (readIORef)
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Context (Context (..), WidgetStore (..), getHotId, getScrollOffset, getScrollOffset2D, getStore, intKey, isDisabled, isSelectOpen)
import NanoUI.Font (FontMetrics, sliderTrackBounds)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , UiCursorKind (..)
  , grabDragKind
  , grabHoverKind
  , inputMouseDown
  , inputMousePos
  )
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeType (..)
  , arenaCount
  , getAspect
  , getDirection
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidgetId
  , isScrollNode
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Types (HostProfile, Rect (..), V2 (..), rectContains, v2X, v2Y)
import NanoUI.WidgetText (isTableHeaderStyle, selectOptions, sliderLabelText)
import NanoUI.Frame.Chrome (widgetNodeTypeTable)
import NanoUI.Frame.Hit (findNodeByWidgetId, scrollHitRect, nodePointVisible)
import NanoUI.Frame.Scroll (scrollBarLayout, ScrollBarLayout (..))
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollChromeSuppressed
  , scrollShowsChrome
  )
import NanoUI.Frame.Select (selectDropRect)
import NanoUI.Frame.TextEdit
  ( TextAreaGeom (..)
  , TextInputGeom (..)
  , textAreaGeom
  , textEditMenuCursorKind
  , textFieldWidgetAtMouse
  , textInputGeom
  )
import NanoUI.Frame.Window (windowResizeCursorKind)

uiCursorKind :: Context -> Input -> IO UiCursorKind
uiCursorKind ctx inp = do
  mMenu <- textEditMenuCursorKind ctx inp
  case mMenu of
    Just k -> pure k
    Nothing -> do
      let mouse = inputMousePos inp
      table <- widgetNodeTypeTable ctx
      mDrop <- selectDropdownCursorKind ctx inp
      case mDrop of
        Just k -> pure k
        Nothing -> do
          mResize <- windowResizeCursorKind ctx inp
          case mResize of
            Just k -> pure k
            Nothing -> do
              mCol <- tableColResizeCursorKind ctx inp
              case mCol of
                Just k -> pure k
                Nothing -> do
                  mScroll <- scrollThumbCursorKind ctx inp
                  case mScroll of
                    Just k -> pure k
                    Nothing -> do
                      mField <- textFieldHoverCursorKind ctx inp
                      case mField of
                        Just k -> pure k
                        Nothing -> do
                          active <- readIORef (ctxActiveId ctx)
                          activeKind <- cursorKindAt table ctx active mouse inp
                          if activeKind /= UiCursorDefault
                            then pure activeKind
                            else do
                              hot <- getHotId ctx
                              cursorKindAt table ctx hot mouse inp

selectDropdownCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
selectDropdownCursorKind ctx inp = do
  let mouse = inputMousePos inp
  dropPress <- readIORef (ctxSelectDropPress ctx)
  store <- getStore ctx
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= NodeSelect
              then go (idx + 1)
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                let key = intKey wid
                    open = isSelectOpen store key
                txt <- getText (ctxNodeArena ctx) idx
                let (_, opts) = selectOptions txt
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                let fm = ctxFontMetrics ctx
                    dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                    inDrop = rectContains dropRect mouse
                if inDrop && (open || dropPress)
                  then pure (Just UiCursorPointer)
                  else go (idx + 1)
  go 0

scrollThumbCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
scrollThumbCursorKind ctx inp = do
  mDrag <- readIORef (ctxScrollDrag ctx)
  let clicking = inputMouseDown inp
  if clicking && isJust mDrag
    then pure (Just UiCursorGrabbing)
    else do
      onThumb <- scrollThumbHit ctx (inputMousePos inp)
      if onThumb
        then pure (Just (grabHoverKind True inp))
        else pure Nothing

-- Field well, not the label. Independent of focus and hot.
textFieldHoverCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textFieldHoverCursorKind ctx inp = do
  mWid <- textFieldWidgetAtMouse ctx (inputMousePos inp)
  pure (UiCursorText <$ mWid)

scrollThumbHit :: Context -> V2 -> IO Bool
scrollThumbHit ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  go 0 count
  where
    go idx count
      | idx >= count = pure False
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if not (isScrollNode nt)
            then go (idx + 1) count
            else do
              si <- getStyleIdx (ctxNodeArena ctx) idx
              let cfg = decodeScrollConfig si
              wid <- getWidgetId (ctxNodeArena ctx) idx
              pad <- getPadding (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              dir <- getDirection (ctxNodeArena ctx) idx
              slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
              let fm = ctxFontMetrics ctx
                  thumbHit axis contentSize axisOff =
                    case scrollBarLayout (ctxHostProfile ctx) fm slot axis x y w h pad contentSize axisOff of
                      Just layout -> rectContains (sbThumb layout) mouse
                      Nothing -> False
              onThumb <-
                if isScrollStyle2D si
                  then do
                    contentH <- getNodeValue (ctxNodeArena ctx) idx
                    contentW <- getAspect (ctxNodeArena ctx) idx
                    V2 offX offY <- getScrollOffset2D ctx wid
                    let hitY =
                          scrollShowsChrome cfg True DirColumn
                            && thumbHit DirColumn contentH offY
                        hitX =
                          scrollShowsChrome cfg True DirRow
                            && thumbHit DirRow contentW offX
                    pure (hitY || hitX)
                  else
                    if scrollChromeSuppressed cfg False dir
                      then pure False
                      else do
                        contentSize <- getNodeValue (ctxNodeArena ctx) idx
                        off <- getScrollOffset ctx wid
                        pure (thumbHit dir contentSize off)
              if onThumb
                then pure True
                else go (idx + 1) count

cursorKindAt :: IM.IntMap NodeType -> Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
cursorKindAt table ctx wid mouse inp
  | hashWidgetId wid == 0 = pure UiCursorDefault
  | otherwise = do
      disabled <- isDisabled ctx wid
      if disabled
        then pure UiCursorDefault
        else
          case IM.lookup (intKey wid) table of
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
      pure $
        case mrect of
          Nothing -> UiCursorDefault
          Just rect ->
            if rectContains rect mouse
              then UiCursorPointer
              else UiCursorDefault

widgetVisibleAt :: Context -> WidgetId -> V2 -> IO Bool
widgetVisibleAt ctx wid mouse = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure False
    Just idx -> nodePointVisible ctx idx mouse

widgetPointerCursor :: Context -> WidgetId -> V2 -> IO UiCursorKind
widgetPointerCursor ctx wid mouse = do
  visible <- widgetVisibleAt ctx wid mouse
  pure (if visible then UiCursorPointer else UiCursorDefault)

sliderCursorKind :: Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
sliderCursorKind ctx wid mouse inp = do
  visible <- widgetVisibleAt ctx wid mouse
  if not visible
    then pure UiCursorDefault
    else do
      mrect <- scrollHitRect ctx wid
      active <- readIORef (ctxActiveId ctx)
      let fm = ctxFontMetrics ctx
          dragging = active == wid && inputMouseDown inp
      lbl <-
        findNodeByWidgetId ctx wid >>= \case
          Nothing -> pure T.empty
          Just idx -> do
            txt <- getText (ctxNodeArena ctx) idx
            pure (sliderLabelText txt)
      pure $
        case mrect of
          Nothing -> UiCursorDefault
          Just (Rect x y w h) ->
            grabDragKind (rectContains (sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h) mouse) dragging inp

textInputCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textInputCursorKind ctx wid mouse =
  textFieldCursorKind ctx wid mouse $ \host fm x y w h ->
    tigFieldRect (textInputGeom host fm x y w h)

textAreaCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textAreaCursorKind ctx wid mouse =
  textFieldCursorKind ctx wid mouse $ \host fm x y w h ->
    tagFieldRect (textAreaGeom host fm x y w h)

textFieldCursorKind ::
  Context ->
  WidgetId ->
  V2 ->
  (HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Rect) ->
  IO UiCursorKind
textFieldCursorKind ctx wid mouse fieldAt = do
  visible <- widgetVisibleAt ctx wid mouse
  if not visible
    then pure UiCursorDefault
    else do
      mrect <- scrollHitRect ctx wid
      case mrect of
        Nothing -> pure UiCursorDefault
        Just (Rect x y w h) ->
          let field = fieldAt (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h
           in pure $
                if rectContains field mouse
                  then UiCursorText
                  else UiCursorDefault

tableColResizeCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
tableColResizeCursorKind ctx inp = do
  store <- getStore ctx
  let dragging = any (\n -> n <= -1000 && n > -2000) (IM.elems (storeInt store))
  if dragging && inputMouseDown inp
    then pure (Just UiCursorEwResize)
    else do
      count <- arenaCount (ctxNodeArena ctx)
      let mouse = inputMousePos inp
          go idx
            | idx >= count = pure Nothing
            | otherwise = do
                nt <- getNodeType (ctxNodeArena ctx) idx
                if nt /= NodeButton
                  then go (idx + 1)
                  else do
                    si <- getStyleIdx (ctxNodeArena ctx) idx
                    if not (isTableHeaderStyle si)
                      then go (idx + 1)
                      else do
                        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                        let hitY = v2Y mouse >= y && v2Y mouse <= y + h
                            hitEdge = abs (v2X mouse - (x + w)) <= 4
                        if hitY && hitEdge && w > 0 && h > 0
                          then pure (Just UiCursorEwResize)
                          else go (idx + 1)
      go 0

pointerCursorWanted :: Context -> Input -> IO Bool
pointerCursorWanted ctx inp = cursorKindIs ctx inp UiCursorPointer

cursorKindIs :: Context -> Input -> UiCursorKind -> IO Bool
cursorKindIs ctx inp want = (== want) <$> uiCursorKind ctx inp

