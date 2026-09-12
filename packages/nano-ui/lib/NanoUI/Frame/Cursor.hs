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
import Data.Maybe (fromMaybe, isJust)
import qualified Data.IntMap.Strict as IM
import NanoUI.Context
  ( Context (..)
  , CustomDrawContext (..)
  , WidgetStore (..)
  , getFocusId
  , getHotId
  , getScrollDrag
  , getScrollOffset
  , getScrollOffset2D
  , getSelectDropPress
  , getStore
  , intKey
  , isDisabled
  , isSelectOpen
  , lookupCustomCursor
  )
import NanoUI.Font (FontMetrics, sliderHandleSlack, sliderTrackBounds)
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
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , getScrollContentW
  , getDirection
  , getFirstChild
  , getNextSibling
  , getNodeType
  , getNodeValue
  , getOptions
  , getPadding
  , getParent
  , getRect
  , getStyleIdx
  , getWidgetId
  , isScrollNode
  )
import NanoUI.Layout.Solve (scrollBarSlotOf)
import NanoUI.Types (HostProfile, Rect (..), V2 (..), isCellHost, rectContains, v2X, v2Y)
import NanoUI.WidgetText (isTableHeaderStyle)
import NanoUI.Frame.Chrome (widgetNodeTypeTable)
import NanoUI.Frame.Hit (findNodeByWidgetId, scrollHitRect, nodePointVisible)
import NanoUI.Frame.Scroll (ScrollBarLayout (..), scrollBarLayout, textAreaContentGeom)
import NanoUI.Frame.Scroll.Geometry
  ( decodeScrollConfig
  , isScrollStyle2D
  , scrollChromeSuppressed
  )
import NanoUI.Frame.Scroll.Geometry qualified as ScrollGeom (scrollBarLayouts2D)
import NanoUI.Frame.Select (overlayMenuOwnerAt, selectDropRect)
import NanoUI.Frame.TextEdit
  ( TextAreaGeom (..)
  , TextAreaScrollBarLayouts (..)
  , isMouseOnTextAreaScrollBarAt
  , searchClearHit
  , textAreaGeom
  , textAreaScrollBarLayouts
  , textEditMenuCursorKind
  , textFieldWidgetAtMouse
  , nodeTextFieldGeom
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
  dropPress <- getSelectDropPress ctx
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
                opts <- getOptions (ctxNodeArena ctx) idx
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                let fm = ctxFontMetrics ctx
                    dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                    inDrop = rectContains dropRect mouse
                if inDrop && (open || dropPress)
                  then pure (Just UiCursorPointer)
                  else go (idx + 1)
  mSel <- go 0
  case mSel of
    Just k -> pure (Just k)
    -- A focused combo's dropdown (visible while its field holds focus) is not
    -- a select: pointer over its menu like the select's. The text-input menu
    -- case inside overlayMenuOwnerAt is unreachable here —
    -- textEditMenuCursorKind runs first in uiCursorKind.
    Nothing -> do
      mOwner <- overlayMenuOwnerAt ctx mouse
      pure (if isJust mOwner then Just UiCursorPointer else Nothing)

scrollThumbCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
scrollThumbCursorKind ctx inp = do
  mDrag <- getScrollDrag ctx
  let clicking = inputMouseDown inp
  if clicking && isJust mDrag
    then pure (Just UiCursorGrabbing)
    else do
      onThumb <- scrollThumbHit ctx (inputMousePos inp)
      if onThumb
        then pure (Just (grabHoverKind True inp))
        else pure Nothing

-- Field well, not the label. Independent of focus and hot. A search field's
-- clear button raises the pointer cursor; everywhere else over a field is text.
textFieldHoverCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textFieldHoverCursorKind ctx inp = do
  let mouse = inputMousePos inp
  mWid <- textFieldWidgetAtMouse ctx mouse
  case mWid of
    Nothing -> pure Nothing
    Just wid -> do
      onClear <- searchClearHit ctx wid mouse
      pure (Just (if onClear then UiCursorPointer else UiCursorText))

scrollThumbHit :: Context -> V2 -> IO Bool
scrollThumbHit ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  go 0 count
  where
    go idx count
      | idx >= count = pure False
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeTextArea
            then do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (fm, field, _lineH, contentW, contentH, _lanes) <- textAreaContentGeom ctx idx
              let host = ctxHostProfile ctx
              V2 curX curY <- getScrollOffset2D ctx wid
              let layouts = textAreaScrollBarLayouts host fm field contentW contentH curX curY
                  hitV = case tasbVertical layouts of
                    Just layout -> rectContains (sbThumb layout) mouse
                    Nothing -> False
                  hitH = case tasbHorizontal layouts of
                    Just layout -> rectContains (sbThumb layout) mouse
                    Nothing -> False
              if hitV || hitH
                then pure True
                else go (idx + 1) count
            else if not (isScrollNode nt)
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
                    contentW <- getScrollContentW (ctxNodeArena ctx) idx
                    V2 offX offY <- getScrollOffset2D ctx wid
                    let (mV, mH) =
                          ScrollGeom.scrollBarLayouts2D
                            (ctxHostProfile ctx)
                            fm
                            slot
                            cfg
                            x
                            y
                            w
                            h
                            pad
                            contentW
                            contentH
                            offX
                            offY
                        hitLayout mLayout =
                          case mLayout of
                            Just layout -> rectContains (sbThumb layout) mouse
                            Nothing -> False
                    pure (hitLayout mV || hitLayout mH)
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
        else do
          mCursorFn <- lookupCustomCursor ctx wid
          case mCursorFn of
            Just cursorFn -> do
              visible <- widgetVisibleAt ctx wid mouse
              if not visible
                then pure UiCursorDefault
                else do
                  active <- readIORef (ctxActiveId ctx)
                  hot <- getHotId ctx
                  focused <- (== wid) <$> getFocusId ctx
                  theme <- readIORef (ctxTheme ctx)
                  let cdc =
                        CustomDrawContext
                          { cdcHovered = hot == wid
                          , cdcPressed = active == wid
                          , cdcFocused = focused
                          , cdcActive = active == wid
                          , cdcDisabled = disabled
                          , cdcTheme = theme
                          , cdcHost = ctxHostProfile ctx
                          , cdcFont = ctxFontMetrics ctx
                          }
                  pure (cursorFn cdc)
            Nothing ->
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
  active <- readIORef (ctxActiveId ctx)
  let dragging = active == wid && inputMouseDown inp
  if dragging
    then pure UiCursorGrabbing
    else do
      visible <- widgetVisibleAt ctx wid mouse
      if not visible
        then pure UiCursorDefault
        else do
          mrect <- scrollHitRect ctx wid
          let fm = ctxFontMetrics ctx
          pure $
            case mrect of
              Nothing -> UiCursorDefault
              Just (Rect x y w h) ->
                let tr = sliderTrackBounds (ctxHostProfile ctx) fm x y w h
                    hitRect =
                      if isCellHost (ctxHostProfile ctx)
                        then tr
                        else Rect (rectX tr) (rectY tr - sliderHandleSlack) (rectW tr) (rectH tr + 2 * sliderHandleSlack)
                 in grabDragKind (rectContains hitRect mouse) False inp

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
          pure (if rectContains field mouse then UiCursorText else UiCursorDefault)
        _ -> pure UiCursorDefault

textAreaCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textAreaCursorKind ctx wid mouse = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure UiCursorDefault
    Just idx -> do
      onScroll <- isMouseOnTextAreaScrollBarAt ctx idx mouse
      if onScroll
        then pure UiCursorDefault
        else
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
                        -- The resize cursor spans the whole column height
                        -- (header plus body cells down to the body
                        -- scroller's bottom edge), matching the drag grab
                        -- zone: tableBodyScrollerBottom locates the same
                        -- body scroller whose rect the grab zone anchors
                        -- on (its prev-frame value, readable at build
                        -- time), so the two zones cannot disagree.
                        mBot <- tableBodyScrollerBottom ctx idx
                        let yBot = fromMaybe (y + h) mBot
                            hitY = v2Y mouse >= y && v2Y mouse <= yBot
                            hitEdge = abs (v2X mouse - (x + w)) <= 4
                        if hitY && hitEdge && w > 0 && h > 0
                          then pure (Just UiCursorEwResize)
                          else go (idx + 1)
      go 0

-- | Bottom edge of a table's body scroller, located structurally from one
-- of its header buttons: walk up to the first ancestor that has a direct
-- Column-direction scroll-container child (the pane column built by
-- tableSplitPanes) and take that child's rect bottom. Runs post-solve, so
-- the rect is current-frame. Nothing when no such scroller exists (the
-- caller falls back to the header button's own bottom).
tableBodyScrollerBottom :: Context -> NodeIdx -> IO (Maybe Float)
tableBodyScrollerBottom ctx = goUp
  where
    na = ctxNodeArena ctx
    goUp i = do
      p <- getParent na i
      if p < 0
        then pure Nothing
        else do
          mScroller <- firstColumnScrollChild p
          case mScroller of
            Just sc -> do
              (_, sy, _, sh) <- getRect na sc
              pure (Just (sy + sh))
            Nothing -> goUp p
    firstColumnScrollChild p = do
      fc <- getFirstChild na p
      let go c
            | c < 0 = pure Nothing
            | otherwise = do
                nt <- getNodeType na c
                hit <-
                  if not (isScrollNode nt)
                    then pure False
                    else do
                      d <- getDirection na c
                      pure (d == DirColumn)
                if hit
                  then pure (Just c)
                  else getNextSibling na c >>= go
      go fc

pointerCursorWanted :: Context -> Input -> IO Bool
pointerCursorWanted ctx inp = cursorKindIs ctx inp UiCursorPointer

cursorKindIs :: Context -> Input -> UiCursorKind -> IO Bool
cursorKindIs ctx inp want = (== want) <$> uiCursorKind ctx inp

