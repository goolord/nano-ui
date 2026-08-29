{-# LANGUAGE DataKinds #-}

module NanoUI.Frame
  ( runFrame
  , runFrameEff
  , runFrameReduce
  , runFrameReduceEff
  , needsRedraw
  , needsRedrawIdle
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
  ) where

import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Damage (floatingPanelRects, updatePrevRects, writeDamage)
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , WidgetStore (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
  , WindowResizeDrag (..)
  , WindowResizeEdge (..)
  , anyAnimating
  , decodeMessages
  , drainMessages
  , getFocusables
  , getScrollOffset
  , getStore
  , intKey
  , isDirty
  , isDisabled
  , markDirty
  , getHotId
  , getPrevRect
  , setScrollOffset
  , setStore
  , startAnimation
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , animInProgress
  , clearTooltips
  , readTooltips
  , PendingTooltip (..)
  , ctxClipboardGet
  , clearMeasureCache
  , markEscapeConsumed
  , lookupImageUv
  , atlasTextureId
  )
import NanoUI.Draw
  ( DrawArena
  , DrawData
  , Layer (..)
  , beginLayer
  , currentLayer
  , finishDraw
  , pushLine
  , pushFilledTriangle
  , pushRect
  , pushBackdropDim
  , pushImage
  , pushRoundedRect
  , pushRoundedStroke
  , pushText
  , resetDrawArena
  , withClip
  )
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , fmLineHeight
  , layoutLineHeight
  , hasHeadingMarker
  , hasMonoFontMarker
  , hasMutedMarker
  , labelContentInset
  , resolveLayoutPadding
  , stripWidgetMarkers
  , lineWidth
  , textDisplayWidth
  , ScrollBarSlot (..)
  , scrollBarGeomFor
  , scrollBarOuterGap
  , scrollLayoutGutter
  , sliderTrackBounds
  , widgetContentInset
  , centeredTextY
  , alignedTextBox
  , wrapTextLines
  , wrapTextLinesIO
  )
import NanoUI.Host (HostProfile, isCellHost)
import NanoUI.WidgetMarkers
  ( buttonDisplayText
  , closeButtonDisplayText
  , isCloseButtonText
  , stripButtonBrackets
  )
import NanoUI.Icons (Icons (..), checkboxMark, terminalPaintColumns)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputInteracted, inputKeys, inputPointerHeld)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , SizingTag (..)
  , arenaCount
  , getAlignX
  , getDirection
  , getFirstChild
  , getHeightSizing
  , getMinMax
  , getNextSibling
  , getParent
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getStyleIdx
  , getText
  , getWidthSizing
  , getWidgetId
  , isWidgetNode
  , isContainerNode
  , isFloatingNode
  , isScrollNode
  , NodeType (NodeButton, NodeCheckbox, NodeSelect, NodeSlider, NodeTextInput, NodeModal, NodeImage, NodePanel, NodeWindow)
  , resetNodeArena
  , setNodeText
  , setNodeValue
  , setRect
  )
import NanoUI.Layout.Solve (placeModals, placeWindows, positionWindowNode, scrollBarSlotOf, solveLayout)
import Effectful (Eff, IOE, runEff, type (:>))
import NanoUI.Monad (NanoUI, Ui, runUi)
import NanoUI.Widgets (applyTextInputMenuAction)
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderLabelText
  , sliderPackRange
  , sliderParseRange
  , sliderPackTerminal
  , sliderValueText
  , textInputFieldHeight
  , textInputFieldText
  , textInputLabelGap
  , textInputTerminalText
  , selectParseOptions
  , selectDisplayText
  , selectChevronReserve
  , selectChevronCenterX
  )
import NanoUI.Style (Padding (..), Style (..), Theme (..), scrollBarThumbColor, scrollBarTrackColor, themeAccent, themeButton, themeFloatingWindow, themeInput, themeMuted, themeOverlayDim, themePanel, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), ImageId (..), Rect (..), Size (..), V2 (..), colorRGBA, lerpColor, rectContains, rectH, rectIntersect, rectOverlapArea, rectUnion, rectW, rectX, rectY, v2X, v2Y)

runFrame :: Context -> Input -> NanoUI a -> IO (a, [FrameMsg], DrawData, Bool)
runFrame = runFrameEff runEff

-- View this model, then apply decoded messages at frame end.
-- DrawData is from the pre-reduce model (one-frame lag). The idle
-- loop redraws when the reduced model differs.
runFrameReduce ::
  (Typeable msg, Eq model) =>
  (msg -> model -> model) ->
  Context ->
  Input ->
  model ->
  (model -> NanoUI a) ->
  IO (a, model, [msg], DrawData, Bool)
runFrameReduce = runFrameReduceEff runEff

runFrameReduceEff ::
  (IOE :> es, Typeable msg, Eq model) =>
  (forall x. Eff es x -> IO x) ->
  (msg -> model -> model) ->
  Context ->
  Input ->
  model ->
  (model -> Eff (Ui : es) a) ->
  IO (a, model, [msg], DrawData, Bool)
runFrameReduceEff unlift update ctx inp model view = do
  (a, msgs, draw, dirty) <- runFrameEff unlift ctx inp (view model)
  let typed = decodeMessages msgs
      model' = foldl' (flip update) model typed
  when (model' /= model) (markDirty ctx)
  dirty' <- isDirty ctx
  pure (a, model', typed, draw, dirty || dirty')

runFrameEff ::
  IOE :> es =>
  (forall x. Eff es x -> IO x) ->
  Context ->
  Input ->
  Eff (Ui : es) a ->
  IO (a, [FrameMsg], DrawData, Bool)
runFrameEff unlift ctx inp ui = do
  oldHot <- readIORef (ctxLastHotId ctx)
  oldActive <- readIORef (ctxActiveId ctx)
  oldFocus <- readIORef (ctxFocusId ctx)
  oldHotRect <- getPrevRect ctx oldHot
  oldActiveRect <- getPrevRect ctx oldActive
  oldFocusRect <- getPrevRect ctx oldFocus
  oldFloatingRects <- readIORef (ctxPrevFloatingRects ctx)
  oldRects <- readIORef (ctxPrevRects ctx)
  oldSize <- readIORef (ctxLastWindowSize ctx)
  oldStore <- getStore ctx
  wasDirty <- isDirty ctx
  writeIORef (ctxDirty ctx) False
  animKeys <- IM.keys . IM.filter animInProgress <$> readIORef (ctxAnimations ctx)
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  clearMeasureCache ctx
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxFocusables ctx) []
  writeIORef (ctxHotId ctx) (WidgetId 0)
  writeIORef (ctxWidgetNodeTypes ctx) Nothing
  unless (inputMouseDown inp) $
    writeIORef (ctxSelectDropPress ctx) False
  modalNow <- readIORef (ctxModalActive ctx)
  writeIORef (ctxModalWasActive ctx) modalNow
  writeIORef (ctxModalActive ctx) False
  writeIORef (ctxModalDepth ctx) 0
  writeIORef (ctxEscapeConsumed ctx) False
  clearTooltips ctx
  result <- unlift (runUi ctx inp ui)
  -- Terminal sliders embed the bar in node text; sync before measure so width is correct.
  syncWidgetLabels ctx
  let Size w h = inputWindowSize inp
  solveLayout (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) (ctxMeasureText ctx) w h
  placeModals (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) w h
  placeWindows (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) w h (lookupWindowPos ctx) (lookupWindowSize ctx)
  movedResize <- updateWindowResize ctx inp w h
  movedWindow <- updateWindowDrag ctx inp
  when (movedResize || movedWindow) $
    placeWindows (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) w h (lookupWindowPos ctx) (lookupWindowSize ctx)
  persistWindowPositions ctx
  updateScrollWheel ctx inp
  updateScrollDrag ctx inp
  applyScrollOffsets ctx
  finalizePointerPress ctx inp
  finalizePointerRelease ctx inp
  finalizeTextInputFocus ctx inp
  finalizeSelectFocus ctx inp
  finalizeTextInputMouse ctx inp
  closeTextInputMenuOnOutsideClick ctx inp
  openTextInputMenu ctx inp
  finalizeTextInputMenuPick ctx inp
  closeTextInputMenuOnEscape ctx inp
  constrainFocusToModal ctx
  finalizeTabFocus ctx inp
  finalizeSelectKeyboard ctx inp
  markSelectDropPress ctx inp
  finalizeSelectPick ctx inp
  closeSelectOnOutsideClick ctx inp
  syncWidgetLabels ctx
  refreshHover ctx inp
  tickAnimations ctx (inputDeltaTime inp)
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawWindowOverlays ctx
  drawModalOverlays ctx (inputWindowSize inp)
  drawSelectOverlays ctx inp
  drawTextInputMenuOverlays ctx inp
  drawTooltipOverlays ctx
  drawData <- finishDraw (ctxDrawArena ctx)
  updatePrevRects ctx
  overlayOpen <- overlayMenuOpen ctx
  writeDamage
    ctx
    inp
    wasDirty
    overlayOpen
    oldSize
    oldStore
    oldHot
    oldActive
    oldFocus
    oldHotRect
    oldActiveRect
    oldFocusRect
    oldFloatingRects
    oldRects
    animKeys
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  -- Keep mid-frame markDirty for the next loop. Open and close both flip
  -- the modal flag one frame after the click, so that follow-up must run.
  writeIORef (ctxDirty ctx) dirtyAfterUi
  pure (result, msgs, drawData, dirtyAfterUi)

needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw = needsRedraw' True

-- Terminal keeps the last blit while idle; SDL windows tick live for damage.
needsRedrawIdle :: Context -> Input -> Input -> IO Bool
needsRedrawIdle = needsRedraw' False

-- Window/scroll/resize drag marks dirty every frame. TUI must still poll input then.
pointerDragActive :: Context -> IO Bool
pointerDragActive ctx = do
  winDrag <- isJust <$> readIORef (ctxWindowDrag ctx)
  scrollDrag <- isJust <$> readIORef (ctxScrollDrag ctx)
  winResize <- isJust <$> readIORef (ctxWindowResize ctx)
  pure (winDrag || scrollDrag || winResize)

needsRedraw' :: Bool -> Context -> Input -> Input -> IO Bool
needsRedraw' includeLive ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  hover <- hoverWouldChange ctx inp
  mDrag <- readIORef (ctxScrollDrag ctx)
  mWinDrag <- readIORef (ctxWindowDrag ctx)
  overlay <- overlayMenuOpen ctx
  edit <- textFieldActive ctx
  winLive <- debugPanelOpen ctx
  let overlayMove = overlay && inputMousePos prev /= inputMousePos inp
  pure
    ( dirty
        || anim
        || inputInteracted prev inp
        || inputPointerHeld inp
        || hover
        || isJust mDrag
        || isJust mWinDrag
        || overlayMove
        || edit
        || (includeLive && winLive)
    )

-- Select dropdown or text-input menu is open. Overlay hover is not a widget id.
overlayMenuOpen :: Context -> IO Bool
overlayMenuOpen ctx = do
  store <- getStore ctx
  menu <- readIORef (ctxTextInputMenu ctx)
  pure (any id (IM.elems (storeSelectOpen store)) || isJust menu)

overlayMenuOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
overlayMenuOwnerAt ctx mouse = do
  menu <- readIORef (ctxTextInputMenu ctx)
  case menu of
    Just m | rectContains (textInputMenuRect m) mouse ->
      pure (Just (textInputMenuWidget m))
    _ -> openSelectOwnerAt ctx mouse

openSelectOwnerAt :: Context -> V2 -> IO (Maybe WidgetId)
openSelectOwnerAt ctx mouse = do
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
                if not (IM.findWithDefault False key (storeSelectOpen store))
                  then go (idx + 1)
                  else do
                    txt <- getText (ctxNodeArena ctx) idx
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                    let (_, opts) = selectParseOptions txt
                        dropRect = selectDropRect (ctxHostProfile ctx) (ctxFontMetrics ctx) x y w h (length opts)
                    if rectContains dropRect mouse
                      then pure (Just wid)
                      else go (idx + 1)
  go 0

-- Focused text field or its context menu. Keep the loop live so typed bytes
-- are not stuck behind SDL_WaitEvent.
textFieldActive :: Context -> IO Bool
textFieldActive ctx = do
  menu <- readIORef (ctxTextInputMenu ctx)
  if isJust menu
    then pure True
    else do
      focus <- readIORef (ctxFocusId ctx)
      if hashWidgetId focus == 0
        then pure False
        else do
          mIdx <- findNodeByWidgetId ctx focus
          case mIdx of
            Nothing -> pure False
            Just idx -> do
              nt <- getNodeType (ctxNodeArena ctx) idx
              pure (nt == NodeTextInput)

-- Last frame still has a floating node (modal or window). Used by backends to
-- decide whether overlay content might need periodic refresh (debug HUD).
floatingPanelActive :: Context -> IO Bool
floatingPanelActive ctx = do
  modal <- readIORef (ctxModalActive ctx)
  if modal
    then pure True
    else do
      count <- arenaCount (ctxNodeArena ctx)
      let go idx
            | idx >= count = pure False
            | otherwise = do
                nt <- getNodeType (ctxNodeArena ctx) idx
                if isFloatingNode nt
                  then pure True
                  else go (idx + 1)
      go 0

-- Floating window overlay (debug HUD). Uses persisted window store because
-- the node arena is empty between skipped idle frames.
debugPanelOpen :: Context -> IO Bool
debugPanelOpen ctx = do
  store <- getStore ctx
  if not (IM.null (storeWindow store))
    then pure True
    else do
      count <- arenaCount (ctxNodeArena ctx)
      let go idx
            | idx >= count = pure False
            | otherwise = do
                nt <- getNodeType (ctxNodeArena ctx) idx
                if nt == NodeWindow
                  then pure True
                  else go (idx + 1)
      go 0

hoverWouldChange :: Context -> Input -> IO Bool
hoverWouldChange ctx inp = do
  lastHot <- readIORef (ctxLastHotId ctx)
  nextHot <- probeHotId ctx (inputMousePos inp)
  pure (nextHot /= lastHot)

collectTextSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextSpans ctx = do
  floatCache <- buildFloatingAncestorMap ctx
  collectTextSpansCached ctx floatCache

collectOverlayTextSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpans ctx inp = do
  floatCache <- buildFloatingAncestorMap ctx
  collectOverlayTextSpansCached ctx inp floatCache

collectRasterSpans :: Context -> Input -> IO ([(Rect, T.Text, Color, Color, Rect)], [(Rect, T.Text, Color, Color, Rect)])
collectRasterSpans ctx inp = do
  floatCache <- buildFloatingAncestorMap ctx
  base <- collectTextSpansCached ctx floatCache
  overlay <- collectOverlayTextSpansCached ctx inp floatCache
  pure (base, overlay)

collectTextSpansCached :: Context -> IM.IntMap (Maybe NodeType) -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextSpansCached ctx floatCache = do
  count <- arenaCount (ctxNodeArena ctx)
  spans <-
    if count > 0
      then collectClippedSpans ctx floatCache 0 (Rect 0 0 1e9 1e9)
      else pure []
  panels <- floatingPanelRects ctx
  pure (filterOccludedBaseSpans panels spans)

collectOverlayTextSpansCached :: Context -> Input -> IM.IntMap (Maybe NodeType) -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpansCached ctx inp floatCache = do
  drops <- collectSelectDropdownSpans ctx inp
  menu <- collectTextInputMenuSpans ctx inp
  tips <- collectTooltipSpans ctx
  windows <- collectFloatingSpans ctx floatCache NodeWindow
  modals <- collectFloatingSpans ctx floatCache NodeModal
  pure (windows ++ modals ++ drops ++ menu ++ tips)

widgetNodeCount :: Context -> IO Int
widgetNodeCount ctx = arenaCount (ctxNodeArena ctx)

collectClippedSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> Rect -> IO [(Rect, T.Text, Color, Color, Rect)]
collectClippedSpans ctx floatCache idx clip = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  if isFloatingNode nt
    then pure []
    else collectClippedSpans' ctx floatCache idx nt clip

collectClippedSpans' :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> NodeType -> Rect -> IO [(Rect, T.Text, Color, Color, Rect)]
collectClippedSpans' ctx floatCache idx nt clip = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  pad <- getPadding (ctxNodeArena ctx) idx
  let nodeRect = Rect x y w h
      fm = ctxFontMetrics ctx
  mClipChildren <-
    if isScrollNode nt
      then do
        dir <- getDirection (ctxNodeArena ctx) idx
        contentSize <- getNodeValue (ctxNodeArena ctx) idx
        slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
        let content = scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
        pure (rectIntersect clip content)
      else
        if nt == NodePanel
          then pure (rectIntersect clip nodeRect)
          else pure (Just clip)
  case mClipChildren of
    Nothing -> pure []
    Just clipHere -> do
      here <-
        case nt of
          NodeSelect -> do
            spans <- collectNodeTextSpans ctx floatCache idx
            pure (tagSelectClippedSpans (ctxHostProfile ctx) clipHere x y w h fm spans)
          NodeTextInput
            | not (isCellHost (ctxHostProfile ctx)) -> do
                spans <- collectNodeTextSpans ctx floatCache idx
                pure (tagTextInputClippedSpans (ctxHostProfile ctx) clipHere x y w h fm spans)
          NodeSeparator
            | isCellHost (ctxHostProfile ctx) ->
                pure
                  ( tagClippedSpans
                      (Rect x y w h)
                      (terminalSeparatorSpans (ctxTheme ctx) x y w h)
                  )
          _ -> tagClippedSpans clipHere <$> collectNodeTextSpans ctx floatCache idx
      -- TUI modal chrome does not scroll (the inner body scroller does), so it
      -- has no track to cap.
      caps <-
        if isCellHost (ctxHostProfile ctx) && isScrollNode nt && nt /= NodeModal
          then terminalScrollCapSpans ctx idx x y w h pad clip
          else pure []
      childSpans <- walkChildSpans ctx floatCache idx clipHere
      pure (here ++ caps ++ childSpans)

walkChildSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> Rect -> IO [(Rect, T.Text, Color, Color, Rect)]
walkChildSpans ctx floatCache idx clip = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc
  where
    go ci
      | ci < 0 = pure []
      | otherwise = do
          spans <- collectClippedSpans ctx floatCache ci clip
          ns <- getNextSibling (ctxNodeArena ctx) ci
          rest <- go ns
          pure (spans ++ rest)

tagClippedSpans :: Rect -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagClippedSpans clip =
  concatMap
    ( \(rect, txt, fg, bg) ->
        case rectIntersect clip (padTextClipRect rect) of
          Nothing -> []
          Just clipHere -> [(rect, txt, fg, bg, clipHere)]
    )

filterOccludedBaseSpans :: IM.IntMap Rect -> [(Rect, T.Text, Color, Color, Rect)] -> [(Rect, T.Text, Color, Color, Rect)]
filterOccludedBaseSpans panels spans
  | IM.null panels = spans
  | otherwise = filter (not . occluded) spans
  where
    panelRects = IM.elems panels
    occluded (rect, _, _, _, clip) =
      case rectIntersect rect clip of
        Nothing -> True
        Just visible -> any (rectFullyInside visible) panelRects

-- Visible text is gone only when a floating panel covers every pixel.
rectFullyInside :: Rect -> Rect -> Bool
rectFullyInside (Rect ix iy iw ih) (Rect ox oy ow oh) =
  iw > 0
    && ih > 0
    && ix >= ox
    && iy >= oy
    && ix + iw <= ox + ow
    && iy + ih <= oy + oh

-- TTF measure is often a fraction narrower than the rendered texture.
textClipSlop :: Float
textClipSlop = 4

padTextClipRect :: Rect -> Rect
padTextClipRect (Rect x y w h) = Rect x y (w + textClipSlop) h

selectTextClip :: HostProfile -> Float -> Float -> Float -> Float -> FontMetrics -> Rect
selectTextClip host x y w h fm =
  let (ix, iy) = widgetContentInset host fm
   in Rect (x + ix) (y + iy) (max 0 (w - ix - selectChevronReserve)) (max 0 (h - 2 * iy))

tagSelectClippedSpans ::
  HostProfile -> Rect -> Float -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagSelectClippedSpans host parentClip x y w h fm =
  let textClip = padTextClipRect (selectTextClip host x y w h fm)
   in concatMap
        ( \(rect, txt, fg, bg) ->
            case rectIntersect parentClip textClip of
              Nothing -> []
              Just clip -> [(rect, txt, fg, bg, clip)]
        )

textInputFieldTextClip :: HostProfile -> TextInputGeom -> FontMetrics -> Rect
textInputFieldTextClip host geom fm =
  let field = tigFieldRect geom
      (ix, iy) = widgetContentInset host fm
   in Rect
        (rectX field + ix)
        (rectY field + iy)
        (max 0 (rectW field - 2 * ix))
        (max 0 (rectH field - 2 * iy))

tagTextInputClippedSpans ::
  HostProfile -> Rect -> Float -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagTextInputClippedSpans host parentClip x y w h fm spans =
  let geom = textInputGeom host fm x y w h
      fieldClip = textInputFieldTextClip host geom fm
      labelClip = Rect x y w (fmLineHeight fm)
      tagOne (rect, txt, fg, bg) =
        let clipRect = padTextClipRect rect
            isField = rectOverlapArea fieldClip clipRect > rectOverlapArea labelClip clipRect
            area = if isField then fieldClip else labelClip
         in case rectIntersect area clipRect of
              Nothing -> []
              Just local ->
                case rectIntersect parentClip local of
                  Nothing -> []
                  Just clip -> [(rect, txt, fg, bg, clip)]
   in concatMap tagOne spans

nodeLabelPaint :: Theme -> T.Text -> (T.Text, Color, Color)
nodeLabelPaint theme raw = labelPaintWith (themePanel theme) theme raw

labelPaintWith :: Style -> Theme -> T.Text -> (T.Text, Color, Color)
labelPaintWith style theme raw =
  labelPaintWithBg style (styleBg style) theme raw

labelPaintWithBg :: Style -> Color -> Theme -> T.Text -> (T.Text, Color, Color)
labelPaintWithBg style bg theme raw =
  let fg
        | hasHeadingMarker raw = themeAccent theme
        | hasMutedMarker raw = themeMuted theme
        | otherwise = styleFg style
   in (stripWidgetMarkers raw, fg, bg)

floatingLabelPaint ::
  IM.IntMap (Maybe NodeType) -> Context -> NodeIdx -> Theme -> T.Text -> (T.Text, Color, Color)
floatingLabelPaint floatCache ctx idx theme raw =
  let terminal = isCellHost (ctxHostProfile ctx)
   in case IM.lookup idx floatCache of
        Just (Just NodeWindow)
          | terminal -> labelPaintWith (themeFloatingWindow theme) theme raw
        Just (Just NodeModal)
          | terminal -> labelPaintWith (themeFloatingWindow theme) theme raw
        _ -> nodeLabelPaint theme raw

floatingAncestor :: Context -> NodeIdx -> IO (Maybe NodeType)
floatingAncestor ctx idx = go idx
  where
    go i
      | i < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) i
          if isFloatingNode nt
            then pure (Just nt)
            else do
              parent <- getParent (ctxNodeArena ctx) i
              go parent

buildFloatingAncestorMap :: Context -> IO (IM.IntMap (Maybe NodeType))
buildFloatingAncestorMap ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  foldM resolve IM.empty [0 .. count - 1]
  where
    resolve :: IM.IntMap (Maybe NodeType) -> Int -> IO (IM.IntMap (Maybe NodeType))
    resolve cache idx =
      if IM.member idx cache
        then pure cache
        else do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if isFloatingNode nt
            then pure (IM.insert idx (Just nt) cache)
            else do
              parent <- getParent (ctxNodeArena ctx) idx
              if parent < 0
                then pure (IM.insert idx Nothing cache)
                else do
                  cache' <- resolve cache parent
                  let ancestor = IM.findWithDefault Nothing parent cache'
                  pure (IM.insert idx ancestor cache')

collectNodeTextSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeIdx -> IO [(Rect, T.Text, Color, Color)]
collectNodeTextSpans ctx floatCache idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  if nt == NodeText
    then do
      raw <- getText (ctxNodeArena ctx) idx
      if T.null raw
        then pure []
        else do
          let (txt, fg, bg) = floatingLabelPaint floatCache ctx idx theme raw
          let (ix, _) = labelContentInset (ctxHostProfile ctx) fm
          ax <- getAlignX (ctxNodeArena ctx) idx
          (_, _, maxW, _) <- getMinMax (ctxNodeArena ctx) idx
          (wTag, _) <- getWidthSizing (ctxNodeArena ctx) idx
          (tw0, th0) <- ctxMeasureText ctx txt
          let hasNewlines = T.any (== '\n') txt
              wrapCap
                | maxW < 1e8 = max 0 maxW
                | wTag == SizingGrow && w > 0 = w
                | otherwise = maxW
              canWrap = wrapCap < 1e8
              wrapW = max 0 (wrapCap - 2 * ix)
              lineH = layoutLineHeight (ctxHostProfile ctx) fm
          if hasNewlines || (canWrap && wrapCap + 0.5 < tw0)
            then do
              textLines <-
                if isCellHost (ctxHostProfile ctx)
                  then pure (wrapTextLines (ctxHostProfile ctx) fm txt wrapW)
                  else wrapTextLinesIO (\t -> fmap fst (ctxMeasureText ctx t)) fm txt wrapW
              lineWs <-
                if isCellHost (ctxHostProfile ctx)
                  then pure (map (lineWidth fm) textLines)
                  else mapM (fmap fst . ctxMeasureText ctx) textLines
              pure
                [ ( Rect
                      tx
                      (centeredTextY (ctxHostProfile ctx) fm (y + fromIntegral i * lineH) lineH lineH)
                      tw
                      lineH
                  , line
                  , fg
                  , bg
                  )
                | (i, line, lw) <- zip3 [(0 :: Int) ..] textLines lineWs
                , let (tx, tw) = alignedTextBox ax x w ix lw
                ]
            else do
              let (tx, used) = alignedTextBox ax x w ix tw0
              pure [(Rect tx (centeredTextY (ctxHostProfile ctx) fm y h th0) used th0, txt, fg, bg)]
    else
      if isWidgetNode nt
        then widgetTextSpans ctx nt idx x y w h
        else pure []

displayText :: Context -> NodeType -> NodeIdx -> IO T.Text
displayText ctx nt idx = do
  txt <- getText (ctxNodeArena ctx) idx
  let terminal = isCellHost (ctxHostProfile ctx)
  if terminal
    then
      case nt of
        NodeSelect -> do
          store <- getStore ctx
          let (lbl, opts) = selectParseOptions txt
          wid <- getWidgetId (ctxNodeArena ctx) idx
          let picked = IM.findWithDefault 0 (intKey wid) (storeSelect store)
              open = IM.findWithDefault False (intKey wid) (storeSelectOpen store)
              opt =
                case drop picked opts of
                  (o : _) -> o
                  _ -> ""
              icons = ctxIcons ctx
              caret = if open then iconSelectOpen icons else iconSelectClosed icons
          pure (selectDisplayText lbl opt <> caret)
        NodeSlider -> pure (T.takeWhile (/= '\US') txt)
        -- Terminal keeps bracket text for TUI affordance; SDL strips via buttonDisplayText.
        NodeButton ->
          if isCloseButtonText txt
            then pure (closeButtonDisplayText txt)
            else pure txt
        NodeTextInput -> do
          value <- textInputValue ctx idx
          focused <- textInputFocused ctx idx
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          let cursor = IM.findWithDefault (length value) (intKey wid) (storeCursor store)
          pure (textInputTerminalText txt value cursor focused)
        _ -> pure txt
    else
      case nt of
        NodeCheckbox -> pure (checkboxLabelText txt)
        NodeTextInput -> do
          value <- textInputValue ctx idx
          focused <- textInputFocused ctx idx
          pure (textInputFieldText txt value focused)
        NodeSlider -> pure (sliderLabelText txt)
        NodeSelect -> do
          store <- getStore ctx
          let (lbl, opts) = selectParseOptions txt
          wid <- getWidgetId (ctxNodeArena ctx) idx
          let picked = IM.findWithDefault 0 (intKey wid) (storeSelect store)
              opt =
                case drop picked opts of
                  (o : _) -> o
                  _ -> ""
          pure (selectDisplayText lbl opt)
        NodeButton -> pure (buttonDisplayText txt)
        _ -> pure (stripButtonBrackets txt)

textInputValue :: Context -> NodeIdx -> IO String
textInputValue ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
  pure (IM.findWithDefault "" key (storeText store))

textInputFocused :: Context -> NodeIdx -> IO Bool
textInputFocused ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  focus <- readIORef (ctxFocusId ctx)
  pure (focus == wid)

data TextInputGeom = TextInputGeom
  { tigFieldRect :: Rect
  }

textInputGeom :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> TextInputGeom
textInputGeom host fm x y w _h =
  let labelH = layoutLineHeight host fm
      gap = textInputLabelGap fm
      fieldH = textInputFieldHeight fm
      fieldY = y + labelH + gap
   in TextInputGeom {tigFieldRect = Rect x fieldY w fieldH}

widgetHitRect :: Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO Rect
widgetHitRect ctx nt idx x y w h = do
  let fm = ctxFontMetrics ctx
  if not (isCellHost (ctxHostProfile ctx))
    then
      case nt of
        NodeTextInput -> pure (tigFieldRect (textInputGeom (ctxHostProfile ctx) fm x y w h))
        NodeButton -> do
          stored <- getText (ctxNodeArena ctx) idx
          if isCloseButtonText stored
            then pure (closeButtonHitRect (ctxHostProfile ctx) fm x y w h)
            else pure (Rect x y w h)
        _ -> pure (Rect x y w h)
    else
      case nt of
        NodeSlider -> do
          txt <- getText (ctxNodeArena ctx) idx
          let lbl = sliderLabelText (T.takeWhile (/= '\US') txt)
          pure (sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h)
        NodeButton -> do
          stored <- getText (ctxNodeArena ctx) idx
          txt <- displayText ctx nt idx
          if isCloseButtonText stored
            then pure (closeButtonHitRect (ctxHostProfile ctx) fm x y w h)
            else pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        NodeCheckbox -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt True)
        NodeSelect -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt False)
        NodeTextInput -> do
          txt <- displayText ctx nt idx
          pure (terminalTextHitRect (ctxHostProfile ctx) fm x y h txt False)
        _ -> pure (Rect x y w h)

terminalTextHitRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> T.Text -> Bool -> Rect
terminalTextHitRect host fm x y h txt atOrigin =
  let (ix, _) = widgetContentInset host fm
      tw = textDisplayWidth host fm txt
      th = layoutLineHeight host fm
      tx = if atOrigin then x else x + ix
      ty = centeredTextY host fm y h th
   in Rect tx ty tw th

-- Paint rect: one cell, centered in the 3-cell slot (Win32 / ASCII "X").
terminalClosePaintRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> T.Text -> Rect
terminalClosePaintRect host fm x y w h txt =
  let tw = fromIntegral (terminalPaintColumns txt)
      th = layoutLineHeight host fm
      lo = x
      hi = x + w - tw
      raw = x + (w - tw) / 2
      lead = fromIntegral (round (max lo (min hi raw)) :: Int)
   in Rect lead (centeredTextY host fm y h th) tw th

-- Hit rect: full close slot (cell host) or padded in the title bar (pixel host).
closeButtonHitRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Rect
closeButtonHitRect host _fm x y w h =
  if isCellHost host
    then Rect x y w h
    else
      -- Easier to tap; keep the target inside the title bar so inner east resize
      -- still works below the close control.
      Rect (x - 8) (y - 4) (w + 10) (h + 4)

data UiCursorKind
  = UiCursorDefault
  | UiCursorPointer
  | UiCursorText
  | UiCursorGrab
  | UiCursorGrabbing
  | UiCursorNsResize
  | UiCursorEwResize
  | UiCursorNwseResize
  | UiCursorNeswResize
  deriving (Eq, Show)

grabHoverKind :: Bool -> Input -> UiCursorKind
grabHoverKind onTarget inp = grabDragKind onTarget False inp

grabDragKind :: Bool -> Bool -> Input -> UiCursorKind
grabDragKind onTarget dragging inp
  | dragging = UiCursorGrabbing
  | onTarget, inputMouseDown inp = UiCursorGrabbing
  | onTarget = UiCursorGrab
  | otherwise = UiCursorDefault

uiCursorKind :: Context -> Input -> IO UiCursorKind
uiCursorKind ctx inp = do
  mMenu <- textInputMenuCursorKind ctx inp
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
              mScroll <- scrollThumbCursorKind ctx inp
              case mScroll of
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
                    open = IM.findWithDefault False key (storeSelectOpen store)
                txt <- getText (ctxNodeArena ctx) idx
                let (_, opts) = selectParseOptions txt
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
              wid <- getWidgetId (ctxNodeArena ctx) idx
              pad <- getPadding (ctxNodeArena ctx) idx
              contentSize <- getNodeValue (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              dir <- getDirection (ctxNodeArena ctx) idx
              off <- getScrollOffset ctx wid
              let fm = ctxFontMetrics ctx
              slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
              case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
                Nothing -> go (idx + 1) count
                Just layout ->
                  if rectContains (sbThumb layout) mouse
                    then pure True
                    else go (idx + 1) count

markSelectDropPress :: Context -> Input -> IO ()
markSelectDropPress ctx inp =
  when (inputMouseDown inp) $ do
    store <- getStore ctx
    when (any id (IM.elems (storeSelectOpen store))) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse (storeSelectOpen store)
      when hit $ writeIORef (ctxSelectDropPress ctx) True

cursorKindAt :: IM.IntMap NodeType -> Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
cursorKindAt table ctx wid mouse inp
  | hashWidgetId wid == 0 = pure UiCursorDefault
  | otherwise = do
      disabled <- isDisabled ctx wid
      if disabled
        then pure UiCursorDefault
        else
          case IM.lookup (intKey wid) table of
            Just NodeButton -> pure UiCursorPointer
            Just NodeCheckbox -> pure UiCursorPointer
            Just NodeSelect -> selectCursorKind ctx wid mouse
            Just NodeTextInput -> textInputCursorKind ctx wid mouse
            Just NodeSlider -> sliderCursorKind ctx wid mouse inp
            _ -> pure UiCursorDefault

selectCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
selectCursorKind ctx wid mouse = do
  mrect <- getPrevRect ctx wid
  pure $
    case mrect of
      Nothing -> UiCursorDefault
      Just rect ->
        if rectContains rect mouse
          then UiCursorPointer
          else UiCursorDefault

sliderCursorKind :: Context -> WidgetId -> V2 -> Input -> IO UiCursorKind
sliderCursorKind ctx wid mouse inp = do
  mrect <- getPrevRect ctx wid
  active <- readIORef (ctxActiveId ctx)
  let fm = ctxFontMetrics ctx
      dragging = active == wid && inputMouseDown inp
  lbl <-
    findNodeByWidgetId ctx wid >>= \case
      Nothing -> pure T.empty
      Just idx -> do
        txt <- getText (ctxNodeArena ctx) idx
        pure (sliderLabelText (T.takeWhile (/= '\US') txt))
  pure $
    case mrect of
      Nothing -> UiCursorDefault
      Just (Rect x y w h) ->
        grabDragKind (rectContains (sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h) mouse) dragging inp

textInputCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textInputCursorKind ctx wid mouse = do
  mrect <- getPrevRect ctx wid
  case mrect of
    Nothing -> pure UiCursorDefault
    Just (Rect x y w h) -> do
      let fm = ctxFontMetrics ctx
          field = tigFieldRect (textInputGeom (ctxHostProfile ctx) fm x y w h)
      pure $
        if rectContains field mouse
          then UiCursorText
          else UiCursorDefault

pointerCursorWanted :: Context -> Input -> IO Bool
pointerCursorWanted ctx inp = cursorKindIs ctx inp UiCursorPointer

cursorKindIs :: Context -> Input -> UiCursorKind -> IO Bool
cursorKindIs ctx inp want = (== want) <$> uiCursorKind ctx inp

widgetNodeTypeTable :: Context -> IO (IM.IntMap NodeType)
widgetNodeTypeTable ctx = do
  cached <- readIORef (ctxWidgetNodeTypes ctx)
  case cached of
    Just table -> pure table
    Nothing -> do
      count <- arenaCount (ctxNodeArena ctx)
      table <-
        if count <= 0
          then pure IM.empty
          else do
            let go idx acc
                  | idx >= count = pure acc
                  | otherwise = do
                      nt <- getNodeType (ctxNodeArena ctx) idx
                      acc' <-
                        if isWidgetNode nt
                          then do
                            wid <- getWidgetId (ctxNodeArena ctx) idx
                            pure (IM.insert (intKey wid) nt acc)
                          else pure acc
                      go (idx + 1) acc'
            go 0 IM.empty
      writeIORef (ctxWidgetNodeTypes ctx) (Just table)
      pure table

closeButtonStyle :: Theme -> Bool -> Float -> Style
closeButtonStyle theme isHot animT =
  let btn = themeButton theme
      panel = themePanel theme
      muted = lerpColor (styleFg btn) (styleBg panel) 0.42
      hot = styleFg btn
      fg
        | isHot = lerpColor muted hot (if animT > 0 then animT else 1)
        | otherwise = lerpColor muted hot animT
   in btn
        { styleBg = colorRGBA 0 0 0 0
        , styleHoverBg = colorRGBA 0 0 0 0
        , styleActiveBg = colorRGBA 0 0 0 0
        , styleBorderWidth = 0
        , styleFg = fg
        }

widgetTextSpans ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(Rect, T.Text, Color, Color)]
widgetTextSpans ctx nt idx x y w h = do
  fm <- pure (ctxFontMetrics ctx)
  terminal <- pure (isCellHost (ctxHostProfile ctx))
  style <- widgetVisualStyle ctx nt idx
  let fg = styleFg style
      bg = styleBg style
  if terminal
    then do
      txt <- displayText ctx nt idx
      if T.null txt
        then pure []
        else do
          let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
          (tw, th) <- ctxMeasureText ctx txt
          isClose <-
            if nt == NodeButton
              then isCloseButtonText <$> getText (ctxNodeArena ctx) idx
              else pure False
          if isClose
            then
              let closeRect = terminalClosePaintRect (ctxHostProfile ctx) fm x y w h txt
               in pure [(closeRect, txt, fg, bg)]
            else do
              let tx =
                    if nt == NodeButton || nt == NodeCheckbox
                      then x
                      else x + ix
                  textSpan =
                    [ ( Rect tx (centeredTextY (ctxHostProfile ctx) fm y h th) tw th
                      , txt
                      , fg
                      , bg
                      )
                    ]
              pure textSpan
    else do
      case nt of
        NodeTextInput -> do
          placements <- widgetTextPlacements ctx nt idx x y w h
          value <- textInputValue ctx idx
          focus <- textInputFocused ctx idx
          let theme = ctxTheme ctx
              windowBg = themeWindow theme
              labelFg = lerpColor fg windowBg 0.32
              placeholder = T.null (T.pack value) && not focus
              fieldFg
                | placeholder = lerpColor fg bg 0.40
                | otherwise = fg
          case placements of
            (lblPl : fieldPl : _) -> do
              let (lbl, lx, ly, lw, lh) = lblPl
                  (field, fx, fy, fw, fh) = fieldPl
              pure
                [ (Rect lx ly lw lh, lbl, labelFg, windowBg)
                , (Rect fx fy fw fh, field, fieldFg, bg)
                ]
            [lblPl] -> do
              let (lbl, lx, ly, lw, lh) = lblPl
              pure [(Rect lx ly lw lh, lbl, labelFg, windowBg)]
            _ -> pure []
        _ -> do
          placements <- widgetTextPlacements ctx nt idx x y w h
          pure
            [ (Rect px py tw th, txt, fg, bg)
            | (txt, px, py, tw, th) <- placements
            , not (T.null txt)
            ]

widgetTextPlacements ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(T.Text, Float, Float, Float, Float)]
widgetTextPlacements ctx nt idx x y w h = do
  let fm = ctxFontMetrics ctx
      terminal = isCellHost (ctxHostProfile ctx)
      (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
  case nt of
    NodeButton -> do
      stored <- getText (ctxNodeArena ctx) idx
      if not terminal && isCloseButtonText stored
        then pure []
        else do
          txt <- displayText ctx nt idx
          (tw, th) <- ctxMeasureText ctx txt
          pure [(txt, x + (w - tw) / 2, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
    NodeSelect -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      pure [(txt, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, min tw (w - ix - selectChevronReserve), th)]
    NodeCheckbox -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      let (cx, _) =
            if terminal
              then widgetContentInset (ctxHostProfile ctx) fm
              else labelContentInset (ctxHostProfile ctx) fm
          tx = x + cx + checkboxLeading (ctxHostProfile ctx) fm
          ty = centeredTextY (ctxHostProfile ctx) fm y h th
      pure [(txt, tx, ty, tw, th)]
    NodeSlider -> do
      lbl <- displayText ctx nt idx
      if terminal
        then do
          (lw, lh) <- ctxMeasureText ctx lbl
          let ty = centeredTextY (ctxHostProfile ctx) fm y lh lh
          pure [(lbl, x + ix, ty, lw, lh)]
        else do
          val <- sliderValue ctx idx
          let valTxt = sliderValueText val
              (lx, _) = labelContentInset (ctxHostProfile ctx) fm
          (lw, lh) <- ctxMeasureText ctx lbl
          (vw, vh) <- ctxMeasureText ctx valTxt
          let ty = centeredTextY (ctxHostProfile ctx) fm y lh lh
          pure
            [ (lbl, x + lx, ty, lw, lh)
            , (valTxt, x + w - lx - vw, centeredTextY (ctxHostProfile ctx) fm y vh vh, vw, vh)
            ]
    NodeTextInput -> do
      lbl <- getText (ctxNodeArena ctx) idx
      value <- textInputValue ctx idx
      focus <- textInputFocused ctx idx
      if terminal
        then do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          let cursor = IM.findWithDefault (length value) (intKey wid) (storeCursor store)
              shown = textInputTerminalText lbl value cursor focus
          (tw, th) <- ctxMeasureText ctx shown
          pure [(shown, x + ix, centeredTextY (ctxHostProfile ctx) fm y h th, tw, th)]
        else do
          let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              field = tigFieldRect geom
              fieldTxt = textInputFieldText lbl value focus
              labelH = layoutLineHeight (ctxHostProfile ctx) fm
          (lw, lh) <- ctxMeasureText ctx lbl
          (fw, fh) <- ctxMeasureText ctx fieldTxt
          pure
            [ (lbl, x, centeredTextY (ctxHostProfile ctx) fm y labelH lh, lw, lh)
            , (fieldTxt, x + ix, centeredTextY (ctxHostProfile ctx) fm (rectY field) (rectH field) fh, fw, fh)
            ]
    _ -> do
      txt <- displayText ctx nt idx
      ax <- getAlignX (ctxNodeArena ctx) idx
      let fm' =
            if hasMonoFontMarker txt
              then ctxMonoFontMetrics ctx
              else fm
      (tw, th) <- ctxMeasureText ctx txt
      let (tx, used) = alignedTextBox ax x w ix tw
      pure [(txt, tx, centeredTextY (ctxHostProfile ctx) fm' y h th, used, th)]

sliderValue :: Context -> NodeIdx -> IO Float
sliderValue ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  pure (IM.findWithDefault 0 (intKey wid) (storeSlider store))

-- Returns a style whose background already reflects hover/active state, so the
-- rect fill and the text cells agree on one color.
widgetVisualStyle :: Context -> NodeType -> NodeIdx -> IO Style
widgetVisualStyle ctx nt idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  hot <- readIORef (ctxHotId ctx)
  active <- readIORef (ctxActiveId ctx)
  focus <- readIORef (ctxFocusId ctx)
  animT <- getAnimationValue ctx wid
  mFloat <- floatingAncestor ctx idx
  storedText <-
    if nt == NodeButton
      then getText (ctxNodeArena ctx) idx
      else pure T.empty
  let isClose = nt == NodeButton && isCloseButtonText storedText
  let theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
      isFocus = focus == wid
      widKey = hashWidgetId wid
      isHot = wid == hot
      base =
        case nt of
          NodeTextInput -> themeInput theme
          NodeSelect ->
            let sel = themeButton theme
             in if isFocus
                  then sel {styleBorder = themeAccent theme}
                  else sel
          NodeSlider ->
            if terminal
              then themeInput theme
              else
                (themeInput theme)
                  { styleBg = colorRGBA 0 0 0 0
                  , styleHoverBg = colorRGBA 0 0 0 0
                  , styleActiveBg = colorRGBA 0 0 0 0
                  , styleBorderWidth = 0
                  }
          NodeCheckbox ->
            (themeButton theme)
              { styleBg = colorRGBA 0 0 0 0
              , styleHoverBg = colorRGBA 0 0 0 0
              , styleActiveBg = colorRGBA 0 0 0 0
              , styleBorderWidth = 0
              }
          NodeButton
            | isClose -> closeButtonStyle theme isHot animT
            | Just NodeWindow <- mFloat, terminal -> themeFloatingWindow theme
            | Just NodeModal <- mFloat, terminal -> themeFloatingWindow theme
          _ -> themeButton theme
      widgetBase =
        case mFloat of
          Just NodeModal
            | terminal -> base
            | nt == NodeCheckbox || nt == NodeSlider -> overlayModalStyle theme
            | otherwise -> base
          _ -> base
      bg
        | terminal, widKey == hashWidgetId active = styleActiveBg widgetBase
        | terminal, isHot = styleHoverBg widgetBase
        | terminal = styleBg widgetBase
        | nt == NodeTextInput, isFocus = styleActiveBg widgetBase
        | widKey == hashWidgetId active = styleActiveBg widgetBase
        | nt == NodeCheckbox || nt == NodeSlider || isClose = styleBg widgetBase
        | otherwise = hoverBackground widgetBase animT isHot
  pure widgetBase {styleBg = bg}

hoverBackground :: Style -> Float -> Bool -> Color
hoverBackground base val isHot
  | styleBg base == styleHoverBg base = styleBg base
  | isHot = lerpColor (styleBg base) (styleHoverBg base) (if val > 0 then val else 1)
  | otherwise = lerpColor (styleBg base) (styleHoverBg base) val

lowerShapes :: Context -> IO ()
lowerShapes ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  when (count > 0) $ lowerNode ctx 0

lowerNode :: Context -> NodeIdx -> IO ()
lowerNode ctx idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let rect = Rect x y w h
      fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
      da = ctxDrawArena ctx
  case nt of
    NodeContainer -> walkChildren ctx idx
    NodePanel -> do
      let style = themePanel theme
      fillStyledRect da terminal style rect
      strokeStyledRect da terminal style x y w h
      withClip da (borderContentClip style rect) $ walkChildren ctx idx
    NodeScrollContainer -> do
      let style = themeInput theme
      pad <- getPadding (ctxNodeArena ctx) idx
      (wTag, _) <- getWidthSizing (ctxNodeArena ctx) idx
      (hTag, _) <- getHeightSizing (ctxNodeArena ctx) idx
      -- Paint bounded wells; full-page Grow/Grow viewports stay on the window fill.
      -- Square corners so axis-aligned clip does not leave rounded leftover pixels.
      let paintWell = not (wTag == SizingGrow && hTag == SizingGrow)
          wellStyle = style {styleCornerRadius = 0}
      when paintWell $ do
        fillStyledRect da terminal wellStyle rect
        strokeStyledRect da terminal wellStyle x y w h
      dir <- getDirection (ctxNodeArena ctx) idx
      contentSize <- getNodeValue (ctxNodeArena ctx) idx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      let inner = scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
      withClip da inner $ walkChildren ctx idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      paintScrollChrome ctx da idx wid x y w h pad theme terminal
    NodeText -> do
      raw <- getText (ctxNodeArena ctx) idx
      let (txt, fg, _) = nodeLabelPaint theme raw
          (ix, _) = labelContentInset (ctxHostProfile ctx) fm
      ax <- getAlignX (ctxNodeArena ctx) idx
      when (not (ctxExternalText ctx) && not (T.null txt)) $ do
        (tw, th) <- ctxMeasureText ctx raw
        let (tx, used) = alignedTextBox ax x w ix tw
        pushRect da (Rect tx (centeredTextY (ctxHostProfile ctx) fm y h th) used th) fg
    NodeSeparator -> do
      let hair = 1
      when (not terminal) $
        if w >= h
          then pushRect da (Rect x (y + (h - hair) / 2) w hair) (themeSeparator theme)
          else pushRect da (Rect (x + (w - hair) / 2) y hair h) (themeSeparator theme)
    NodeTextInput
      | not terminal -> do
          style <- widgetVisualStyle ctx nt idx
          focus <- textInputFocused ctx idx
          let geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              fieldRect = tigFieldRect geom
              borderCol =
                if focus
                  then themeAccent theme
                  else styleBorder style
              fieldStyle = style {styleBorder = borderCol}
          fillStyledRect da False style fieldRect
          strokeStyledRect
            da
            False
            fieldStyle
            (rectX fieldRect)
            (rectY fieldRect)
            (rectW fieldRect)
            (rectH fieldRect)
          drawTextInputCaret da ctx idx x y w h style
    NodeSpacer -> pure ()
    NodeModal -> pure ()
    NodeWindow -> pure ()
    NodeBox -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      -- styleIdx holds RGBA Word32 bits; see `box` in NanoUI.Widgets.
      pushRect da rect (Color (fromIntegral si :: Word32))
    NodeImage -> do
      tex <- imageIdFromText <$> getText (ctxNodeArena ctx) idx
      mUv <- lookupImageUv ctx (ImageId tex)
      case mUv of
        Just (u0, v0, u1, v1)
          | not terminal ->
              pushImage da rect atlasTextureId u0 v0 u1 v1 (colorRGBA 255 255 255 255)
        _ -> pushRect da rect (themeAccent theme)
    _ -> do
      style <- widgetVisualStyle ctx nt idx
      value <- getNodeValue (ctxNodeArena ctx) idx
      storedText <-
        if nt == NodeButton
          then getText (ctxNodeArena ctx) idx
          else pure T.empty
      let isClose = nt == NodeButton && isCloseButtonText storedText
      let opaqueBg
            | isClose = False
            | terminal, nt == NodeButton = False
            | terminal, nt == NodeCheckbox = False
            | terminal, nt == NodeSlider = False
            | terminal, nt == NodeSelect = False
            | terminal, nt == NodeTextInput = False
            | terminal, nt == NodeText = False
            | terminal = True
            | otherwise =
                nt /= NodeCheckbox && nt /= NodeSlider && nt /= NodeTextInput
      when opaqueBg $ fillStyledRect da terminal style rect
      when (not terminal) $ do
        when opaqueBg $ strokeStyledRect da terminal style x y w h
        when (nt == NodeCheckbox) $
          drawCheckbox
            (ctxHostProfile ctx)
            da
            fm
            style
            x
            y
            h
            value
            (themeAccent theme)
            (styleBg (themeInput theme))
            (colorRGBA 255 255 255 255)
        when (nt == NodeSlider) $ do
          txt <- getText (ctxNodeArena ctx) idx
          let lbl = sliderLabelText (T.takeWhile (/= '\US') txt)
              track = sliderTrackBounds (ctxHostProfile ctx) fm lbl x y w h
              tx = rectX track
              ty = rectY track
              tw = rectW track
              th = rectH track
              trackR = 3
              fillW = max 0 (tw * clamp01 value)
              outline = styleBorder (themeInput theme)
              well = colorRGBA 72 48 48 255
              fill = colorRGBA 204 102 102 255
              bw = 1
              innerR = max 0 (trackR - bw)
              innerX = tx + bw
              innerY = ty + bw
              innerW = tw - 2 * bw
              innerH = th - 2 * bw
              innerFillW = max 0 (innerW * clamp01 value)
          pushRoundedRect da track trackR outline
          when (innerW > 0 && innerH > 0) $
            pushRoundedRect da (Rect innerX innerY innerW innerH) innerR well
          when (innerFillW > 0) $ do
            let fillR =
                  if innerFillW >= innerW - 0.5
                    then innerR
                    else min innerR (innerFillW / 2)
            pushRoundedRect da (Rect innerX innerY innerFillW innerH) fillR fill
          let handleD = 18
              handleCx = tx + max (handleD / 2) (min (tw - handleD / 2) fillW)
              handleHy = ty + (th - handleD) / 2
              handle = Rect (handleCx - handleD / 2) handleHy handleD handleD
              innerD = handleD - 2
              handleInner =
                Rect
                  (handleCx - innerD / 2)
                  (handleHy + (handleD - innerD) / 2)
                  innerD
                  innerD
          pushRoundedRect da handle (handleD / 2) (styleBorder (themeInput theme))
          pushRoundedRect da handleInner (innerD / 2) (colorRGBA 255 255 255 255)
        when isClose $
          drawCloseIcon (ctxHostProfile ctx) fm da x y w h (styleFg style)
        when (nt == NodeSelect) $
          drawSelectChevron da x y w h (styleFg style)
      placements <- widgetTextPlacements ctx nt idx x y w h
      when (terminal && not (ctxExternalText ctx)) $
        forM_ placements $ \(txt, px, py, _, _) ->
          when (not (T.null txt)) $
            pushText da fm px py txt (styleFg style)

drawCheckbox ::
  HostProfile ->
  DrawArena ->
  FontMetrics ->
  Style ->
  Float ->
  Float ->
  Float ->
  Float ->
  Color ->
  Color ->
  Color ->
  IO ()
drawCheckbox host da fm style x y h value accent well markCol = do
  let (ix, _) =
        if isCellHost host
          then widgetContentInset host fm
          else labelContentInset host fm
      box = checkboxBoxSize host fm
      bx = x + ix
      by = y + (h - box) / 2
      r = min 6 (box / 3.5)
      bw = 2
      outer = Rect bx by box box
      inner = Rect (bx + bw) (by + bw) (box - 2 * bw) (box - 2 * bw)
      innerR = max 0 (r - bw)
  if value >= 0.5
    then do
      pushRoundedRect da outer r accent
      drawCheckboxMark da bx by box markCol
    else do
      pushRoundedRect da outer r (styleBorder style)
      pushRoundedRect da inner innerR well

drawCheckboxMark :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
drawCheckboxMark da bx by box markCol = do
  let t = max 1.6 (box * 0.11)
      x0 = bx + box * 0.22
      y0 = by + box * 0.52
      x1 = bx + box * 0.42
      y1 = by + box * 0.72
      x2 = bx + box * 0.78
      y2 = by + box * 0.28
  pushLine da x0 y0 x1 y1 t markCol
  pushLine da x1 y1 x2 y2 t markCol

drawTextInputCaret :: DrawArena -> Context -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextInputCaret da ctx idx x y w h style = do
  let terminal = isCellHost (ctxHostProfile ctx)
  if terminal
    then pure ()
    else do
      focus <- textInputFocused ctx idx
      when focus $ do
        value <- textInputValue ctx idx
        wid <- getWidgetId (ctxNodeArena ctx) idx
        store <- getStore ctx
        let key = intKey wid
            cursor = IM.findWithDefault (length value) key (storeCursor store)
            anchor = IM.findWithDefault cursor key (storeSelAnchor store)
            selLo = min anchor cursor
            selHi = max anchor cursor
            hasSel = selLo < selHi
        lbl <- getText (ctxNodeArena ctx) idx
        let fm = ctxFontMetrics ctx
            geom = textInputGeom (ctxHostProfile ctx) fm x y w h
            fieldRect = tigFieldRect geom
            (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
            theme = ctxTheme ctx
            accent = themeAccent theme
            selBg = lerpColor accent (styleBg style) 0.55
        when hasSel $ do
          (wLo, _) <- ctxMeasureText ctx (T.pack (take selLo value))
          (wHi, _) <- ctxMeasureText ctx (T.pack (take selHi value))
          (_, ph) <- ctxMeasureText ctx (T.pack value)
          let ty = centeredTextY (ctxHostProfile ctx) fm (rectY fieldRect) (rectH fieldRect) ph
              selX = rectX fieldRect + ix + wLo
              selW = max 1 (wHi - wLo)
              selH = max 4 ph
          pushRect da (Rect selX ty selW selH) selBg
        let fieldTxt = textInputFieldText lbl value focus
            prefix = T.take (max 0 (min (T.length fieldTxt) cursor)) fieldTxt
        (pw, _) <- ctxMeasureText ctx prefix
        (_, ph) <- ctxMeasureText ctx fieldTxt
        let ty = centeredTextY (ctxHostProfile ctx) fm (rectY fieldRect) (rectH fieldRect) ph
            caretX = rectX fieldRect + ix + pw
            caretY = ty + 1
            caretH = max 4 (ph - 2)
        pushRect da (Rect caretX caretY 1 caretH) (styleFg style)

fillStyledRect :: DrawArena -> Bool -> Style -> Rect -> IO ()
fillStyledRect da terminal style rect =
  if terminal || styleCornerRadius style <= 0
    then pushRect da rect (styleBg style)
    else pushRoundedRect da rect (styleCornerRadius style) (styleBg style)

strokeStyledRect :: DrawArena -> Bool -> Style -> Float -> Float -> Float -> Float -> IO ()
strokeStyledRect da terminal style x y w h =
  when (not terminal && styleBorderWidth style > 0) $ do
    let bw = max 1 (styleBorderWidth style)
        col = styleBorder style
        r = styleCornerRadius style
    if r <= 0
      then strokeRect da x y w h bw col
      else strokeRoundedBorder da x y w h r bw col

strokeRoundedBorder ::
  DrawArena ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Float ->
  Color ->
  IO ()
strokeRoundedBorder da x y w h r bw col = do
  -- Half-pixel inset keeps the 1px AA fringe inside the clip. Do not snap
  -- the fill in C or this becomes a full layout pixel again.
  let inset = 0.5
      ox = x + inset
      oy = y + inset
      ow = max 0 (w - 2 * inset)
      oh = max 0 (h - 2 * inset)
      rr = min r (min (ow / 2) (oh / 2))
  pushRoundedStroke da (Rect ox oy ow oh) rr bw col

borderContentClip :: Style -> Rect -> Rect
borderContentClip style (Rect x y w h) =
  if styleBorderWidth style <= 0
    then Rect x y w h
    else
      let bw = max 1 (styleBorderWidth style)
       in Rect (x + bw) (y + bw) (max 0 (w - 2 * bw)) (max 0 (h - 2 * bw))

clamp01 :: Float -> Float
clamp01 v = max 0 (min 1 v)

scrollLineFor :: HostProfile -> Float
scrollLineFor host = if isCellHost host then 1 else scrollLine

scrollLine :: Float
scrollLine = 20

applyScrollOffsets :: Context -> IO ()
applyScrollOffsets ctx = do
  store <- getStore ctx
  when (any (> 0) (IM.elems (storeScroll store))) $ do
    count <- arenaCount (ctxNodeArena ctx)
    forM_ [0 .. count - 1] $ \idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      when (isScrollNode nt) $ do
        -- TUI modal chrome does not scroll; the inner body scroller does.
        let skipModal = isCellHost (ctxHostProfile ctx) && nt == NodeModal
        when (not skipModal) $ do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          off <- getScrollOffset ctx wid
          when (off > 0) $ do
            dir <- getDirection (ctxNodeArena ctx) idx
            case dir of
              DirColumn -> shiftDescendants ctx idx 0 (-off)
              DirRow -> shiftDescendants ctx idx (-off) 0

shiftDescendants :: Context -> NodeIdx -> Float -> Float -> IO ()
shiftDescendants ctx idx dx dy = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc
  where
    go ci
      | ci < 0 = pure ()
      | otherwise = do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) ci
          setRect (ctxNodeArena ctx) ci (x + dx) (y + dy) w h
          nt <- getNodeType (ctxNodeArena ctx) ci
          when (isContainerNode nt) (shiftDescendants ctx ci dx dy)
          ns <- getNextSibling (ctxNodeArena ctx) ci
          go ns

updateScrollWheel :: Context -> Input -> IO ()
updateScrollWheel ctx inp = do
  let scroll = inputScroll inp
  when (v2Y scroll /= 0 || v2X scroll /= 0) $ do
    mTarget <- pickScrollWheelTarget ctx (inputMousePos inp)
    case mTarget of
      Nothing -> pure ()
      Just wid -> void (tryApplyScrollWheelDelta ctx wid scroll)

-- Nested scrollers take the wheel only while hovered or while they own focus.
-- No leftover chain to the parent at a limit.
pickScrollWheelTarget :: Context -> V2 -> IO (Maybe WidgetId)
pickScrollWheelTarget ctx mouse = do
  hovered <- findScrollTargetUnderMouse ctx mouse
  case hovered of
    Just wid -> pure (Just wid)
    Nothing -> do
      focus <- readIORef (ctxFocusId ctx)
      if hashWidgetId focus == 0
        then pure Nothing
        else findScrollOwningWidget ctx focus

findScrollOwningWidget :: Context -> WidgetId -> IO (Maybe WidgetId)
findScrollOwningWidget ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> walkUp idx
  where
    walkUp i
      | i < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) i
          if isScrollNode nt
            then Just <$> getWidgetId (ctxNodeArena ctx) i
            else do
              p <- getParent (ctxNodeArena ctx) i
              walkUp p

findNodeByWidgetId :: Context -> WidgetId -> IO (Maybe NodeIdx)
findNodeByWidgetId ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            w' <- getWidgetId (ctxNodeArena ctx) idx
            if w' == wid
              then pure (Just idx)
              else go (idx + 1)
  go 0

tryApplyScrollWheelDelta :: Context -> WidgetId -> V2 -> IO Bool
tryApplyScrollWheelDelta ctx wid scroll = do
  mGeom <- scrollContainerGeom ctx wid
  case mGeom of
    Nothing -> pure False
    Just (_idx, dir, _x, _y, w, h, pad, contentSize) -> do
      cur <- getScrollOffset ctx wid
      let step = scrollLineFor (ctxHostProfile ctx)
      case dir of
        DirColumn -> applyAxis cur (h - padT pad - padB pad) contentSize (v2Y scroll * step)
        DirRow -> applyAxis cur (w - padL pad - padR pad) contentSize (v2X scroll * step)
  where
    applyAxis cur inner contentSize delta = do
      let maxOff = max 0 (contentSize - inner)
          newOff = max 0 (min maxOff (cur + delta))
      if newOff == cur
        then pure False
        else do
          setScrollOffset ctx wid newOff
          pure True

findScrollTargetUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findScrollTargetUnderMouse ctx mouse = do
  mWin <- topmostWindowAtMouse ctx mouse
  case mWin of
    Just idx -> do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      queryScrollTarget ctx idx mouse (Rect x y w h)
    Nothing -> do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) 0
      queryScrollTarget ctx 0 mouse (Rect x y w h)

queryScrollTarget :: Context -> NodeIdx -> V2 -> Rect -> IO (Maybe WidgetId)
queryScrollTarget ctx idx mouse parentClip = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  mClipHere <- scrollHitClip ctx idx nt parentClip
  case mClipHere of
    Nothing -> pure Nothing
    Just clip -> do
      childHit <- walkScrollSiblings ctx idx mouse clip
      case childHit of
        Just wid -> pure (Just wid)
        Nothing -> scrollHitSelf ctx idx mouse clip

walkScrollSiblings :: Context -> NodeIdx -> V2 -> Rect -> IO (Maybe WidgetId)
walkScrollSiblings ctx parent mouse clip = do
  fc <- getFirstChild (ctxNodeArena ctx) parent
  go fc
  where
    go ci
      | ci < 0 = pure Nothing
      | otherwise = do
          hit <- queryScrollTarget ctx ci mouse clip
          case hit of
            Just wid -> pure (Just wid)
            Nothing -> do
              ns <- getNextSibling (ctxNodeArena ctx) ci
              go ns

scrollHitSelf :: Context -> NodeIdx -> V2 -> Rect -> IO (Maybe WidgetId)
scrollHitSelf ctx idx mouse clip = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  if not (isScrollNode nt)
    then pure Nothing
    else
      if rectW clip > 0 && rectH clip > 0 && rectContains clip mouse
        then Just <$> getWidgetId (ctxNodeArena ctx) idx
        else pure Nothing

-- Same clip stack as collectClippedSpans': scroll viewport, then panel bounds.
scrollHitClip :: Context -> NodeIdx -> NodeType -> Rect -> IO (Maybe Rect)
scrollHitClip ctx idx nt parentClip = do
  pad <- getPadding (ctxNodeArena ctx) idx
  let fm = ctxFontMetrics ctx
  if isScrollNode nt
    then do
      (x, y, w, h) <- getScrollVisualRect ctx idx
      dir <- getDirection (ctxNodeArena ctx) idx
      contentSize <- getNodeValue (ctxNodeArena ctx) idx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      let local = scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
          lane = scrollChromeLane (ctxHostProfile ctx) fm slot dir x y w h pad
          hit = rectUnion local lane
      -- Window hang stays hittable: the window clip includes padR.
      pure (rectIntersect parentClip hit)
    else
      if nt == NodePanel
        then do
          (x, y, w, h) <- getScrollVisualRect ctx idx
          pure (rectIntersect parentClip (Rect x y w h))
        else pure (Just parentClip)

-- Layout position plus ancestor scroll shifts (before applyScrollOffsets runs).
getScrollVisualRect :: Context -> NodeIdx -> IO (Float, Float, Float, Float)
getScrollVisualRect ctx idx = do
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  (dx, dy) <- ancestorScrollShift ctx idx
  pure (x + dx, y + dy, w, h)

ancestorScrollShift :: Context -> NodeIdx -> IO (Float, Float)
ancestorScrollShift ctx idx = go idx (0, 0)
  where
    go i (sx, sy)
      | i <= 0 = pure (sx, sy)
      | otherwise = do
          p <- getParent (ctxNodeArena ctx) i
          if p < 0
            then pure (sx, sy)
            else do
              (sx', sy') <- parentScrollShift ctx p (sx, sy)
              go p (sx', sy')

parentScrollShift :: Context -> NodeIdx -> (Float, Float) -> IO (Float, Float)
parentScrollShift ctx p (sx, sy) = do
  nt <- getNodeType (ctxNodeArena ctx) p
  if isScrollNode nt
    then do
      wid <- getWidgetId (ctxNodeArena ctx) p
      off <- getScrollOffset ctx wid
      dir <- getDirection (ctxNodeArena ctx) p
      pure $
        case dir of
          DirColumn -> (sx, sy - off)
          DirRow -> (sx - off, sy)
    else pure (sx, sy)

finalizeTabFocus :: Context -> Input -> IO ()
finalizeTabFocus ctx inp =
  when (KeyTab `elem` inputKeys inp) $ do
    focusables <- getFocusables ctx
    let raw = filter (/= WidgetId 0) focusables
    ids <- filterModalFocusables ctx raw
    if null ids
      then pure ()
      else do
        cur <- readIORef (ctxFocusId ctx)
        let shift = modShift (inputModifiers inp)
            next = tabNext cur ids shift
        writeIORef (ctxFocusId ctx) next
        markDirty ctx

tabNext :: WidgetId -> [WidgetId] -> Bool -> WidgetId
tabNext cur ids shift =
  case ids of
    [] -> WidgetId 0
    _ ->
      let idx = findIndex (== cur) ids
          pick i = ids !! (i `mod` length ids)
       in case idx of
            Nothing -> ids !! 0
            Just i ->
              if shift
                then pick (i - 1 + length ids)
                else pick (i + 1)

closeSelectOnOutsideClick :: Context -> Input -> IO ()
closeSelectOnOutsideClick ctx inp =
  when (inputMousePressed inp) $ do
    store <- getStore ctx
    when (any id (IM.elems (storeSelectOpen store))) $ do
      let mouse = inputMousePos inp
      count <- arenaCount (ctxNodeArena ctx)
      hit <- openSelectHit ctx count mouse (storeSelectOpen store)
      unlessHit hit $
        setStore ctx (store {storeSelectOpen = IM.map (const False) (storeSelectOpen store)})

finalizeSelectKeyboard :: Context -> Input -> IO ()
finalizeSelectKeyboard ctx inp = do
  let keys = inputKeys inp
      wantNext = KeyDown `elem` keys || KeyRight `elem` keys
      wantPrev = KeyUp `elem` keys || KeyLeft `elem` keys
      wantEsc = KeyEscape `elem` keys
      wantEnter = KeyEnter `elem` keys
      wantStep = wantNext || wantPrev
  when (wantStep || wantEsc || wantEnter) $ do
    focus <- readIORef (ctxFocusId ctx)
    store <- getStore ctx
    mTarget <- pickSelectKeyboardTarget ctx focus store wantStep
    case mTarget of
      Nothing -> pure ()
      Just (wid, open) -> do
        allow <- widgetOverlayAllowed ctx wid
        when allow $
          case () of
            _ | wantEsc || wantEnter ->
                when open $ do
                  setStore ctx (store {storeSelectOpen = IM.insert (intKey wid) False (storeSelectOpen store)})
                  when wantEsc $ markEscapeConsumed ctx
                  markDirty ctx
            _ | wantStep -> do
                mIdx <- findNodeByWidgetId ctx wid
                case mIdx of
                  Nothing -> pure ()
                  Just idx -> do
                    txt <- getText (ctxNodeArena ctx) idx
                    let (_, opts) = selectParseOptions txt
                        n = length opts
                    if n <= 0
                      then pure ()
                      else do
                        let key = intKey wid
                            cur = IM.findWithDefault 0 key (storeSelect store)
                            delta = if wantNext then 1 else -1
                            next = max 0 (min (n - 1) (cur + delta))
                        when (next /= cur) $ do
                          setStore ctx (store {storeSelect = IM.insert key next (storeSelect store)})
                          markDirty ctx
            _ -> pure ()

pickSelectKeyboardTarget ::
  Context -> WidgetId -> WidgetStore -> Bool -> IO (Maybe (WidgetId, Bool))
pickSelectKeyboardTarget ctx focus store wantStep = do
  if wantStep
    then do
      mFocus <- selectWidgetIfAny ctx focus
      case mFocus of
        Just wid -> do
          let open = IM.findWithDefault False (intKey wid) (storeSelectOpen store)
          pure (Just (wid, open))
        Nothing -> do
          mOpen <- findOpenSelectWidget ctx
          case mOpen of
            Nothing -> pure Nothing
            Just w -> pure (Just (w, True))
    else do
      mOpen <- findOpenSelectWidget ctx
      case mOpen of
        Nothing -> pure Nothing
        Just w -> pure (Just (w, True))

selectWidgetIfAny :: Context -> WidgetId -> IO (Maybe WidgetId)
selectWidgetIfAny ctx wid
  | hashWidgetId wid == 0 = pure Nothing
  | otherwise = do
      mIdx <- findNodeByWidgetId ctx wid
      case mIdx of
        Nothing -> pure Nothing
        Just idx -> do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeSelect then pure (Just wid) else pure Nothing

findOpenSelectWidget :: Context -> IO (Maybe WidgetId)
findOpenSelectWidget ctx = do
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
                if IM.findWithDefault False (intKey wid) (storeSelectOpen store)
                  then pure (Just wid)
                  else go (idx + 1)
  go 0

finalizeSelectPick :: Context -> Input -> IO ()
finalizeSelectPick ctx inp =
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    count <- arenaCount (ctxNodeArena ctx)
    let go idx
          | idx >= count = pure ()
          | otherwise = do
              nt <- getNodeType (ctxNodeArena ctx) idx
              if nt /= NodeSelect
                then go (idx + 1)
                else do
                  wid <- getWidgetId (ctxNodeArena ctx) idx
                  store <- getStore ctx
                  let key = intKey wid
                  if not (IM.findWithDefault False key (storeSelectOpen store))
                    then go (idx + 1)
                    else do
                      allow <- widgetOverlayAllowed ctx wid
                      if not allow
                        then go (idx + 1)
                        else do
                          txt <- getText (ctxNodeArena ctx) idx
                          let (_, opts) = selectParseOptions txt
                          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                          let fm = ctxFontMetrics ctx
                              dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                          when (rectContains dropRect mouse) $
                            case selectDropPickIndex dropRect (selectItemH (ctxHostProfile ctx) h) (length opts) (v2Y mouse) of
                              Nothing -> pure ()
                              Just picked -> do
                                st <- getStore ctx
                                setStore
                                  ctx
                                  ( st
                                      { storeSelect = IM.insert key picked (storeSelect st)
                                      , storeSelectOpen = IM.insert key False (storeSelectOpen st)
                                      }
                                  )
                                writeIORef (ctxFocusId ctx) wid
                                markDirty ctx
                          go (idx + 1)
    go 0

openSelectHit :: Context -> Int -> V2 -> IM.IntMap Bool -> IO Bool
openSelectHit ctx count mouse opens = go 0
  where
    go idx
      | idx >= count = pure False
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeSelect
            then go (idx + 1)
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              let key = intKey wid
              if not (IM.findWithDefault False key opens)
                then go (idx + 1)
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  txt <- getText (ctxNodeArena ctx) idx
                  let fm = ctxFontMetrics ctx
                      (_, opts) = selectParseOptions txt
                      btnRect = Rect x y w h
                      dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                  if rectContains btnRect mouse || rectContains dropRect mouse
                    then pure True
                    else go (idx + 1)

unlessHit :: Bool -> IO () -> IO ()
unlessHit b act = when (not b) act

-- Hit-test widgets with solved layout rects so hover paint matches draw positions.
refreshHover :: Context -> Input -> IO ()
refreshHover ctx inp = do
  prevHot <- readIORef (ctxLastHotId ctx)
  newHot <- probeHotId ctx (inputMousePos inp)
  writeIORef (ctxHotId ctx) newHot
  writeIORef (ctxLastHotId ctx) newHot
  let terminal = isCellHost (ctxHostProfile ctx)
  when (prevHot /= newHot) $ do
    unless terminal $ do
      when (hashWidgetId prevHot /= 0) $ startAnimation ctx prevHot 1 0 0.12
      when (hashWidgetId newHot /= 0) $ startAnimation ctx newHot 0 1 0.12

-- Same walk as refreshHover: later nodes paint first, earlier widget hits win.
probeHotId :: Context -> V2 -> IO WidgetId
probeHotId ctx mouse = do
  mOverlay <- overlayMenuOwnerAt ctx mouse
  case mOverlay of
    Just wid -> pure wid
    Nothing -> do
      count <- arenaCount (ctxNodeArena ctx)
      if count <= 0
        then pure (WidgetId 0)
        else go (WidgetId 0) (count - 1)
  where
    go acc idx
      | idx < 0 = pure acc
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          acc' <-
            if not (isWidgetNode nt)
              then pure acc
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                if w > 0 && h > 0 && rectContains (Rect x y w h) mouse
                  then do
                    allow <- overlayHitAllowed ctx idx mouse
                    pure (if allow then wid else acc)
                  else pure acc
          go acc' (idx - 1)

finalizePointerPress :: Context -> Input -> IO ()
finalizePointerPress ctx inp =
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    count <- arenaCount (ctxNodeArena ctx)
    mWid <- findTopWidgetUnderMouse ctx count mouse isInteractiveNode
    case mWid of
      Nothing -> pure ()
      Just wid -> do
        disabled <- isDisabled ctx wid
        unless disabled $ writeIORef (ctxActiveId ctx) wid

findTopWidgetUnderMouse ::
  Context -> Int -> V2 -> (NodeType -> Bool) -> IO (Maybe WidgetId)
findTopWidgetUnderMouse ctx count mouse wanted = go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if not (wanted nt)
            then go (idx - 1)
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              rect <- widgetHitRect ctx nt idx x y w h
              if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
                then do
                  allow <- overlayHitAllowed ctx idx mouse
                  if allow then pure (Just wid) else go (idx - 1)
                else go (idx - 1)

isInteractiveNode :: NodeType -> Bool
isInteractiveNode nt =
  nt == NodeButton
    || nt == NodeCheckbox
    || nt == NodeSlider
    || nt == NodeSelect
    || nt == NodeTextInput

-- Clicks are finalized against solved layout rects; widgets only track press state.
finalizePointerRelease :: Context -> Input -> IO ()
finalizePointerRelease ctx inp =
  if not (inputMouseReleased inp)
    then pure ()
    else do
      active <- readIORef (ctxActiveId ctx)
      when (hashWidgetId active /= 0) $ do
        let mouse = inputMousePos inp
        count <- arenaCount (ctxNodeArena ctx)
        releasedOver <-
          if count <= 0
            then pure False
            else checkReleasedOver ctx count active mouse
        forM_ [0 .. count - 1] $ \idx -> do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          when (wid == active) $ do
            nt <- getNodeType (ctxNodeArena ctx) idx
            (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
            let rect = Rect x y w h
            when (w > 0 && h > 0 && rectContains rect mouse) $
              case nt of
                NodeCheckbox -> do
                  store <- getStore ctx
                  let key = intKey wid
                      current =
                        IM.findWithDefault False key (storeCheckbox store)
                      newVal = not current
                  setStore
                    ctx
                    ( store
                        { storeCheckbox = IM.insert key newVal (storeCheckbox store)
                        }
                    )
                _ -> pure ()
        writeIORef (ctxActiveId ctx) (WidgetId 0)
        when releasedOver $
          unless (isCellHost (ctxHostProfile ctx)) $
            setAnimationValue ctx active 1

checkReleasedOver :: Context -> Int -> WidgetId -> V2 -> IO Bool
checkReleasedOver ctx count active mouse = go 0
  where
    go idx
      | idx >= count = pure False
      | otherwise = do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          if wid /= active
            then go (idx + 1)
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              let rect = Rect x y w h
              pure (w > 0 && h > 0 && rectContains rect mouse)

-- Focus text inputs using solved layout rects so the caret appears on first press.
finalizeTextInputFocus :: Context -> Input -> IO ()
finalizeTextInputFocus ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- readIORef (ctxTextInputMenu ctx)
    let mouse = inputMousePos inp
    when (case mMenu of
            Just menu -> not (rectContains (textInputMenuRect menu) mouse)
            Nothing -> True) $ do
      prevFocus <- readIORef (ctxFocusId ctx)
      count <- arenaCount (ctxNodeArena ctx)
      mFocused <- findTextInputUnderMouse ctx count mouse
      case mFocused of
        Nothing -> do
          when (prevFocus /= WidgetId 0) $ markDirty ctx
          collapseTextInputSelection ctx prevFocus
          writeIORef (ctxFocusId ctx) (WidgetId 0)
          writeIORef (ctxTextInputMenu ctx) Nothing
        Just wid -> do
          writeIORef (ctxFocusId ctx) wid
          when (prevFocus /= wid) $ markDirty ctx

collapseTextInputSelection :: Context -> WidgetId -> IO ()
collapseTextInputSelection ctx wid =
  when (hashWidgetId wid /= 0) $ do
    store <- getStore ctx
    let key = intKey wid
        cur = IM.findWithDefault 0 key (storeCursor store)
    setStore ctx (store {storeSelAnchor = IM.insert key cur (storeSelAnchor store)})

finalizeSelectFocus :: Context -> Input -> IO ()
finalizeSelectFocus ctx inp =
  when (inputMousePressed inp) $ do
    count <- arenaCount (ctxNodeArena ctx)
    mWid <- findSelectUnderMouse ctx count (inputMousePos inp)
    case mWid of
      Nothing -> pure ()
      Just wid -> do
        disabled <- isDisabled ctx wid
        unless disabled $ do
          prev <- readIORef (ctxFocusId ctx)
          writeIORef (ctxFocusId ctx) wid
          when (prev /= wid) $ markDirty ctx

findSelectUnderMouse :: Context -> Int -> V2 -> IO (Maybe WidgetId)
findSelectUnderMouse ctx count mouse = go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeSelect
            then go (idx - 1)
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              allow <- widgetOverlayAllowed ctx wid
              if not allow
                then go (idx - 1)
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  txt <- getText (ctxNodeArena ctx) idx
                  store <- getStore ctx
                  let key = intKey wid
                      open = IM.findWithDefault False key (storeSelectOpen store)
                      fm = ctxFontMetrics ctx
                      (_, opts) = selectParseOptions txt
                      btnRect = Rect x y w h
                      dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                  if rectContains btnRect mouse || (open && rectContains dropRect mouse)
                    then pure (Just wid)
                    else go (idx - 1)

finalizeTextInputMouse :: Context -> Input -> IO ()
finalizeTextInputMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $ do
    mGeom <- textInputGeomForWidget ctx focus
    case mGeom of
      Nothing -> pure ()
      Just (fieldRect, contentX, value) -> do
        let mouse = inputMousePos inp
            inField = rectContains fieldRect mouse
        if inputMousePressed inp && inField
          then do
            idx <- textInputCharAtX ctx value contentX (v2X mouse)
            let clicks = max 1 (inputMouseClicks inp)
            applyTextInputClick ctx focus value idx clicks
            writeIORef (ctxTextInputDrag ctx) (Just (TextInputDrag focus idx clicks))
          else do
            mDrag <- readIORef (ctxTextInputDrag ctx)
            case mDrag of
              Just drag
                | textInputDragWidget drag == focus && (inputMouseDown inp || inputMouseReleased inp) -> do
                    idx <- textInputCharAtX ctx value contentX (v2X mouse)
                    applyTextInputDrag ctx focus value (textInputDragAnchor drag) idx (textInputDragClicks drag)
              _ -> pure ()
  when (inputMouseReleased inp) $
    writeIORef (ctxTextInputDrag ctx) Nothing

data TextCharClass = TextWord | TextSpace | TextOther
  deriving (Eq)

textCharClass :: Char -> TextCharClass
textCharClass c
  | isAlphaNum c || c == '_' = TextWord
  | isSpace c = TextSpace
  | otherwise = TextOther

textInputWordBounds :: String -> Int -> (Int, Int)
textInputWordBounds text raw
  | null text = (0, 0)
  | otherwise =
      let n = length text
          i = max 0 (min (n - 1) raw)
          cls = textCharClass (text !! i)
          lo = goLeft cls i
          hi = goRight cls n i + 1
       in (lo, hi)
  where
    goLeft cls i
      | i <= 0 = 0
      | textCharClass (text !! (i - 1)) == cls = goLeft cls (i - 1)
      | otherwise = i
    goRight cls n i
      | i + 1 >= n = i
      | textCharClass (text !! (i + 1)) == cls = goRight cls n (i + 1)
      | otherwise = i

applyTextInputClick :: Context -> WidgetId -> String -> Int -> Int -> IO ()
applyTextInputClick ctx wid value idx clicks
  | clicks >= 3 = updateTextInputSelection ctx wid 0 (length value)
  | clicks == 2 =
      let (lo, hi) = textInputWordBounds value idx
       in updateTextInputSelection ctx wid lo hi
  | otherwise = updateTextInputSelection ctx wid idx idx

applyTextInputDrag :: Context -> WidgetId -> String -> Int -> Int -> Int -> IO ()
applyTextInputDrag ctx wid value anchor idx clicks
  | clicks >= 3 = updateTextInputSelection ctx wid 0 (length value)
  | clicks == 2 =
      let (a0, a1) = textInputWordBounds value anchor
          (c0, c1) = textInputWordBounds value idx
       in updateTextInputSelection ctx wid (min a0 c0) (max a1 c1)
  | otherwise = updateTextInputSelection ctx wid anchor idx

data TextInputMenuRow
  = TextInputMenuSep
  | TextInputMenuItem Int T.Text
  deriving (Eq, Show)

textInputMenuRows :: [TextInputMenuRow]
textInputMenuRows =
  [ TextInputMenuItem 0 "Cut"
  , TextInputMenuItem 1 "Copy"
  , TextInputMenuSep
  , TextInputMenuItem 2 "Paste"
  , TextInputMenuSep
  , TextInputMenuItem 3 "Select All"
  ]

textInputMenuOuterPad :: Float
textInputMenuOuterPad = 6

textInputMenuItemPadX :: Float
textInputMenuItemPadX = 10

textInputMenuSepH :: HostProfile -> Float
textInputMenuSepH host = if isCellHost host then 1 else 9

textInputMenuCornerR :: Float
textInputMenuCornerR = 2

textInputMenuShadowOff :: Float
textInputMenuShadowOff = 3

textInputMenuMinW :: Float
textInputMenuMinW = 148

textInputMenuItemH :: HostProfile -> Float
textInputMenuItemH host = if isCellHost host then 1 else 28

textInputMenuRowH :: HostProfile -> TextInputMenuRow -> Float
textInputMenuRowH host = \case
  TextInputMenuSep -> textInputMenuSepH host
  TextInputMenuItem {} -> textInputMenuItemH host

textInputMenuContentH :: HostProfile -> Float
textInputMenuContentH host = sum (map (textInputMenuRowH host) textInputMenuRows)

overlayMenuStyle :: Theme -> Style
overlayMenuStyle theme =
  let panel = themePanel theme
      -- SDL panel hover matches panel fill, so the row would be invisible.
      hover =
        if styleHoverBg panel == styleBg panel
          then styleHoverBg (themeButton theme)
          else styleHoverBg panel
      selected = lerpColor (styleBg panel) (themeAccent theme) 0.22
   in panel
        { styleCornerRadius = textInputMenuCornerR
        , styleBorderWidth = 1
        , styleHoverBg = hover
        , styleActiveBg = selected
        }

overlayWindowStyle :: Theme -> Style
overlayWindowStyle theme =
  let win = themeFloatingWindow theme
   in win {styleCornerRadius = 2, styleBorderWidth = 1}

overlayModalStyle :: Theme -> Style
overlayModalStyle theme =
  let base = overlayMenuStyle theme
   in base {styleCornerRadius = 2, styleBorderWidth = 1}

textInputMenuStyle :: Theme -> Style
textInputMenuStyle = overlayMenuStyle

textInputMenuWidth :: Context -> IO Float
textInputMenuWidth ctx = do
  let labels = [lbl | TextInputMenuItem _ lbl <- textInputMenuRows]
  ws <- mapM (ctxMeasureText ctx) labels
  let maxTw = maximum (map fst ws)
  pure (max textInputMenuMinW (maxTw + 2 * textInputMenuItemPadX + 2 * textInputMenuOuterPad))

textInputMenuRectAt :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Size -> Rect
textInputMenuRectAt host _fm x y menuW win =
  let h = 2 * textInputMenuOuterPad + textInputMenuContentH host
      Size ww wh = win
      rx = max 0 (min x (ww - menuW))
      ry = max 0 (min y (wh - h))
   in Rect rx ry menuW h

textInputMenuContentRect :: HostProfile -> Rect -> FontMetrics -> Rect
textInputMenuContentRect host menuRect _fm =
  let pad = textInputMenuOuterPad
   in Rect
        (rectX menuRect + pad)
        (rectY menuRect + pad)
        (rectW menuRect - 2 * pad)
        (textInputMenuContentH host)

textInputMenuLayout :: HostProfile -> [(TextInputMenuRow, Float, Float)]
textInputMenuLayout host = go 0 textInputMenuRows
  where
    go _ [] = []
    go y (entry : rest) =
      let h = textInputMenuRowH host entry
       in (entry, y, h) : go (y + h) rest

textInputMenuPickAction :: HostProfile -> Rect -> FontMetrics -> V2 -> Maybe Int
textInputMenuPickAction host menuRect fm mouse =
  let content = textInputMenuContentRect host menuRect fm
      relY = v2Y mouse - rectY content
   in if relY < 0 || relY >= textInputMenuContentH host
        then Nothing
        else pick relY (textInputMenuLayout host)
  where
    pick _ [] = Nothing
    pick y ((TextInputMenuSep, _, h) : rest)
      | y < h = Nothing
      | otherwise = pick (y - h) rest
    pick y ((TextInputMenuItem action _, _, h) : rest)
      | y < h = Just action
      | otherwise = pick (y - h) rest

textInputMenuActionEnabled :: Context -> WidgetId -> Int -> IO Bool
textInputMenuActionEnabled ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      cursor = IM.findWithDefault (length text) key (storeCursor store)
      anchor = IM.findWithDefault cursor key (storeSelAnchor store)
      hasSel = anchor /= cursor
  mclip <- ctxClipboardGet ctx
  let clipTxt = maybe "" id mclip
  pure $
    case item of
      0 -> hasSel
      1 -> not (null text)
      2 -> not (null clipTxt)
      3 -> not (null text)
      _ -> False

textInputMenuItemFg :: Style -> Bool -> Color
textInputMenuItemFg style enabled =
  if enabled
    then styleFg style
    else lerpColor (styleFg style) (styleBg style) 0.55

pushMenuShadow :: DrawArena -> Rect -> Float -> IO ()
pushMenuShadow da menuRect r =
  let off = textInputMenuShadowOff
      shadowRect =
        Rect
          (rectX menuRect + off)
          (rectY menuRect + off)
          (rectW menuRect)
          (rectH menuRect)
      shadowCol = colorRGBA 0 0 0 72
   in pushRoundedRect da shadowRect r shadowCol

openTextInputMenu :: Context -> Input -> IO ()
openTextInputMenu ctx inp =
  when (inputMouseRightPressed inp) $ do
    focus <- readIORef (ctxFocusId ctx)
    when (hashWidgetId focus /= 0) $ do
      mGeom <- textInputGeomForWidget ctx focus
      case mGeom of
        Nothing -> pure ()
        Just (fieldRect, _, _) -> do
          let mouse = inputMousePos inp
          when (rectContains fieldRect mouse) $ do
            fm <- pure (ctxFontMetrics ctx)
            menuW <- textInputMenuWidth ctx
            let menuRect = textInputMenuRectAt (ctxHostProfile ctx) fm (v2X mouse) (v2Y mouse) menuW (inputWindowSize inp)
            writeIORef (ctxTextInputMenu ctx) (Just (TextInputMenu focus menuRect))
            markDirty ctx

finalizeTextInputMenuPick :: Context -> Input -> IO ()
finalizeTextInputMenuPick ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- readIORef (ctxTextInputMenu ctx)
    case mMenu of
      Nothing -> pure ()
      Just menu ->
        let mouse = inputMousePos inp
            rect = textInputMenuRect menu
         in when (rectContains rect mouse) $ do
              let fm = ctxFontMetrics ctx
              case textInputMenuPickAction (ctxHostProfile ctx) rect fm mouse of
                Nothing -> writeIORef (ctxTextInputMenu ctx) Nothing
                Just idx -> do
                  enabled <- textInputMenuActionEnabled ctx (textInputMenuWidget menu) idx
                  when enabled $
                    applyTextInputMenuAction ctx (textInputMenuWidget menu) idx

closeTextInputMenuOnOutsideClick :: Context -> Input -> IO ()
closeTextInputMenuOnOutsideClick ctx inp =
  when (inputMousePressed inp || inputMouseRightPressed inp) $ do
    mMenu <- readIORef (ctxTextInputMenu ctx)
    case mMenu of
      Nothing -> pure ()
      Just menu -> do
        let mouse = inputMousePos inp
        unless (rectContains (textInputMenuRect menu) mouse) $
          writeIORef (ctxTextInputMenu ctx) Nothing

closeTextInputMenuOnEscape :: Context -> Input -> IO ()
closeTextInputMenuOnEscape ctx inp =
  when (KeyEscape `elem` inputKeys inp) $
    readIORef (ctxTextInputMenu ctx) >>= \case
      Nothing -> pure ()
      Just _ -> do
        writeIORef (ctxTextInputMenu ctx) Nothing
        markEscapeConsumed ctx
        markDirty ctx

textInputMenuCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textInputMenuCursorKind ctx inp = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
  case mMenu of
    Nothing -> pure Nothing
    Just menu -> do
      let mouse = inputMousePos inp
          rect = textInputMenuRect menu
          fm = ctxFontMetrics ctx
      if not (rectContains rect mouse)
        then pure Nothing
        else
          case textInputMenuPickAction (ctxHostProfile ctx) rect fm mouse of
            Nothing -> pure Nothing
            Just idx -> do
              enabled <- textInputMenuActionEnabled ctx (textInputMenuWidget menu) idx
              pure (if enabled then Just UiCursorPointer else Just UiCursorDefault)

drawTextInputMenuOverlays :: Context -> Input -> IO ()
drawTextInputMenuOverlays ctx inp = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
  case mMenu of
    Nothing -> pure ()
    Just menu -> do
      allow <- widgetOverlayAllowed ctx (textInputMenuWidget menu)
      when allow $ do
        let fm = ctxFontMetrics ctx
        when (not (isCellHost (ctxHostProfile ctx))) $ do
          let da = ctxDrawArena ctx
              theme = ctxTheme ctx
              mouse = inputMousePos inp
              menuRect = textInputMenuRect menu
              menuStyle = textInputMenuStyle theme
              content = textInputMenuContentRect (ctxHostProfile ctx) menuRect fm
              r = styleCornerRadius menuStyle
              wid = textInputMenuWidget menu
          pushMenuShadow da menuRect r
          fillStyledRect da False menuStyle menuRect
          strokeStyledRect
            da
            False
            menuStyle
            (rectX menuRect)
            (rectY menuRect)
            (rectW menuRect)
            (rectH menuRect)
          forM_ (textInputMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, h) -> do
            let rowRect = Rect (rectX menuRect) (rectY content + relY) (rectW menuRect) h
            case entry of
              TextInputMenuSep -> do
                let sepCol = themeSeparator theme
                    margin = textInputMenuItemPadX
                    lineY = rectY rowRect + h / 2
                pushRect
                  da
                  (Rect (rectX rowRect + margin) lineY (rectW rowRect - 2 * margin) 1)
                  sepCol
              TextInputMenuItem action _ -> do
                enabled <- textInputMenuActionEnabled ctx wid action
                let hovered = enabled && rectContains rowRect mouse
                when hovered $ do
                  pushRect da rowRect (styleHoverBg menuStyle)
                  let accent = themeAccent theme
                      barRect = Rect (rectX rowRect) (rectY rowRect + 3) 2 (rectH rowRect - 6)
                  pushRoundedRect da barRect 1 accent

collectTextInputMenuSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextInputMenuSpans ctx inp = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
  case mMenu of
    Nothing -> pure []
    Just menu -> do
      let fm = ctxFontMetrics ctx
          theme = ctxTheme ctx
          mouse = inputMousePos inp
          menuRect = textInputMenuRect menu
          menuStyle = textInputMenuStyle theme
          content = textInputMenuContentRect (ctxHostProfile ctx) menuRect fm
          wid = textInputMenuWidget menu
      allow <- widgetOverlayAllowed ctx wid
      if not allow
        then pure []
        else if isCellHost (ctxHostProfile ctx)
        then terminalTextInputMenuSpans ctx menuRect content fm menuStyle mouse wid
        else do
          let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
              bg = styleBg menuStyle
          spans <-
            forM (textInputMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, h) -> do
              let rowRect = Rect (rectX menuRect) (rectY content + relY) (rectW menuRect) h
              case entry of
                TextInputMenuSep -> pure []
                TextInputMenuItem action lbl -> do
                  enabled <- textInputMenuActionEnabled ctx wid action
                  let fg = textInputMenuItemFg menuStyle enabled
                      hovered = enabled && rectContains rowRect mouse
                      rowBg =
                        if hovered
                          then styleHoverBg menuStyle
                          else bg
                  (tw, th) <- ctxMeasureText ctx lbl
                  let tx = rectX content + textInputMenuItemPadX + ix
                      ty = centeredTextY (ctxHostProfile ctx) fm (rectY content + relY) h th
                  pure [(Rect tx ty tw th, lbl, fg, rowBg, menuRect)]
          pure (concat spans)

terminalTextInputMenuSpans ::
  Context ->
  Rect ->
  Rect ->
  FontMetrics ->
  Style ->
  V2 ->
  WidgetId ->
  IO [(Rect, T.Text, Color, Color, Rect)]
terminalTextInputMenuSpans ctx menuRect content _fm menuStyle mouse wid = do
  let rx :: Int
      rx = round (rectX menuRect)
      wi :: Int
      wi = max 1 (round (rectW menuRect))
      innerW = max 0 (wi - 1)
      dropBg = styleBg menuStyle
      dropHoverBg = styleHoverBg menuStyle
      sepFg = themeSeparator (ctxTheme ctx)
  rows <-
    forM (textInputMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, _h) -> do
      let rowY :: Int
          rowY = round (rectY content + relY)
      case entry of
        TextInputMenuSep ->
          pure
            [ ( Rect (fromIntegral rx) (fromIntegral rowY) (fromIntegral wi) 1
              , T.replicate innerW (T.singleton '\x2500')
              , sepFg
              , dropBg
              , menuRect
              )
            ]
        TextInputMenuItem action lbl -> do
          enabled <- textInputMenuActionEnabled ctx wid action
          let fg = textInputMenuItemFg menuStyle enabled
              rowRect = Rect (rectX menuRect) (rectY content + relY) (rectW menuRect) (textInputMenuItemH (ctxHostProfile ctx))
              hovered = enabled && rectContains rowRect mouse
              rowBg = if hovered then dropHoverBg else dropBg
              rowText = T.singleton ' ' <> padDropText innerW lbl
          pure [(Rect (fromIntegral rx) (fromIntegral rowY) (fromIntegral wi) 1, rowText, fg, rowBg, menuRect)]
  pure (concat rows)

textInputGeomForWidget :: Context -> WidgetId -> IO (Maybe (Rect, Float, String))
textInputGeomForWidget ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  go 0 count
  where
    go idx count
      | idx >= count = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeTextInput
            then go (idx + 1) count
            else do
              w' <- getWidgetId (ctxNodeArena ctx) idx
              if w' /= wid
                then go (idx + 1) count
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  let fm = ctxFontMetrics ctx
                      field = tigFieldRect (textInputGeom (ctxHostProfile ctx) fm x y w h)
                      (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
                      contentX = rectX field + ix
                  value <- textInputValue ctx idx
                  pure (Just (field, contentX, value))

updateTextInputSelection :: Context -> WidgetId -> Int -> Int -> IO ()
updateTextInputSelection ctx wid anchor cursor = do
  store <- getStore ctx
  let key = intKey wid
      oldAnchor = IM.findWithDefault cursor key (storeSelAnchor store)
      oldCursor = IM.findWithDefault 0 key (storeCursor store)
  when (oldAnchor /= anchor || oldCursor /= cursor) $ do
    setStore
      ctx
      ( store
          { storeSelAnchor = IM.insert key anchor (storeSelAnchor store)
          , storeCursor = IM.insert key cursor (storeCursor store)
          }
      )
    markDirty ctx

textInputCharAtX :: Context -> String -> Float -> Float -> IO Int
textInputCharAtX ctx text startX mouseX = do
  let len = length text
      relX = max 0 (mouseX - startX)
  if len <= 0
    then pure 0
    else search 0 len relX
  where
    search lo hi x =
      if hi - lo <= 1
        then do
          (wLo, _) <- ctxMeasureText ctx (T.pack (take lo text))
          (wHi, _) <- ctxMeasureText ctx (T.pack (take hi text))
          if x - wLo <= wHi - x then pure lo else pure hi
        else do
          let mid = (lo + hi) `div` 2
          (wMid, _) <- ctxMeasureText ctx (T.pack (take mid text))
          if wMid <= x then search mid hi x else search lo mid x

findTextInputUnderMouse :: Context -> Int -> V2 -> IO (Maybe WidgetId)
findTextInputUnderMouse ctx count mouse = go 0
  where
    go idx
      | idx >= count = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeTextInput
            then do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              rect <- widgetHitRect ctx nt idx x y w h
              if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
                then do
                  allow <- overlayHitAllowed ctx idx mouse
                  if allow then pure (Just wid) else go (idx + 1)
                else go (idx + 1)
            else go (idx + 1)

syncWidgetLabels :: Context -> IO ()
syncWidgetLabels ctx = do
  store <- getStore ctx
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    wid <- getWidgetId (ctxNodeArena ctx) idx
    let key = intKey wid
    case nt of
      NodeCheckbox -> do
        txt <- getText (ctxNodeArena ctx) idx
        let body = checkboxLabelText txt
            val = IM.findWithDefault False key (storeCheckbox store)
            terminal = isCellHost (ctxHostProfile ctx)
            mark = if terminal then checkboxMark (ctxIcons ctx) val else ""
        setNodeText (ctxNodeArena ctx) idx (mark <> body)
        setNodeValue (ctxNodeArena ctx) idx (if val then 1 else 0)
      NodeSlider -> do
        let val = IM.findWithDefault 0 key (storeSlider store)
        txt <- getText (ctxNodeArena ctx) idx
        let (lbl, minV, maxV) = sliderParseRange txt
            frac = if maxV > minV then (val - minV) / (maxV - minV) else 0
            shown =
              if isCellHost (ctxHostProfile ctx)
                then sliderPackTerminal lbl frac val minV maxV
                else sliderPackRange lbl minV maxV
        setNodeText (ctxNodeArena ctx) idx shown
        setNodeValue (ctxNodeArena ctx) idx frac
      NodeButton -> do
        txt <- getText (ctxNodeArena ctx) idx
        when (not (isCellHost (ctxHostProfile ctx))) $
          setNodeText (ctxNodeArena ctx) idx (stripButtonBrackets txt)
      _ -> pure ()

walkChildren :: Context -> NodeIdx -> IO ()
walkChildren ctx idx = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc
  where
    go ci =
      if ci < 0
        then pure ()
        else do
          lowerNode ctx ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          go ns

modalTreeOpen :: Context -> IO Bool
modalTreeOpen ctx = do
  top <- topmostModalIdx ctx
  pure (isJust top)

topmostModalIdx :: Context -> IO (Maybe NodeIdx)
topmostModalIdx ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt == NodeModal then pure (Just idx) else go (idx - 1)

nodeInTopmostModal :: Context -> NodeIdx -> IO Bool
nodeInTopmostModal ctx idx = do
  mTop <- topmostModalIdx ctx
  case mTop of
    Nothing -> pure False
    Just top -> nodeInSubtree ctx idx top

nodeInSubtree :: Context -> NodeIdx -> NodeIdx -> IO Bool
nodeInSubtree ctx idx top = go idx
  where
    go i
      | i < 0 = pure False
      | i == top = pure True
      | otherwise = do
          parent <- getParent (ctxNodeArena ctx) i
          go parent

modalHitAllowed :: Context -> NodeIdx -> IO Bool
modalHitAllowed ctx idx = do
  mTop <- topmostModalIdx ctx
  case mTop of
    Nothing -> pure True
    Just top -> nodeInSubtree ctx idx top

overlayHitAllowed :: Context -> NodeIdx -> V2 -> IO Bool
overlayHitAllowed ctx idx mouse = do
  mModal <- topmostModalIdx ctx
  case mModal of
    Just _ -> modalHitAllowed ctx idx
    Nothing -> do
      mWin <- topmostWindowAtMouse ctx mouse
      case mWin of
        Nothing -> pure True
        Just widx -> nodeInSubtree ctx idx widx

topmostWindowAtMouse :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtMouse ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeWindow
            then go (idx - 1)
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              if w > 0 && h > 0 && rectContains (Rect x y w h) mouse
                then pure (Just idx)
                else go (idx - 1)

topmostWindowAtResizeHalo :: Context -> V2 -> IO (Maybe NodeIdx)
topmostWindowAtResizeHalo ctx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  go (count - 1)
  where
    go idx
      | idx < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeWindow
            then go (idx - 1)
            else do
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              if w <= 0 || h <= 0
                then go (idx - 1)
                else do
                  let rect = Rect x y w h
                  if rectContains (windowResizeHalo rect) mouse
                    then pure (Just idx)
                    else do
                      inInner <- windowInnerEastResizeHit ctx idx rect mouse
                      if inInner
                        then pure (Just idx)
                        else go (idx - 1)

windowInnerEastResizeHit :: Context -> NodeIdx -> Rect -> V2 -> IO Bool
windowInnerEastResizeHit ctx winIdx winRect mouse@(V2 mx _) = do
  let Rect x _ w _ = winRect
  pad <- getPadding (ctxNodeArena ctx) winIdx
  let pr = padR pad
  if mx < x + w - pr || mx > x + w
    then pure False
    else do
      mLane <- windowBodyScrollLane ctx winIdx
      pure (not (maybe False (`rectContains` mouse) mLane))

lookupWindowPos :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowPos ctx wid = do
  store <- getStore ctx
  pure (IM.lookup (intKey wid) (storeWindow store))

lookupWindowSize :: Context -> WidgetId -> IO (Maybe (Float, Float))
lookupWindowSize ctx wid = do
  store <- getStore ctx
  pure (IM.lookup (intKey wid) (storeWindowSize store))

persistWindowPositions :: Context -> IO ()
persistWindowPositions ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  store0 <- getStore ctx
  pos1 <- foldlPos 0 count (storeWindow store0)
  size1 <- foldlSize 0 count (storeWindowSize store0)
  let store1 = store0 {storeWindow = pos1, storeWindowSize = size1}
  when (store1 /= store0) $ setStore ctx store1
  where
    foldlPos idx count acc
      | idx >= count = pure acc
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          acc' <-
            if nt /= NodeWindow
              then pure acc
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (x, y, _, _) <- getRect (ctxNodeArena ctx) idx
                pure (IM.insert (intKey wid) (x, y) acc)
          foldlPos (idx + 1) count acc'
    foldlSize idx count acc
      | idx >= count = pure acc
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          acc' <-
            if nt /= NodeWindow
              then pure acc
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                (_, _, w, h) <- getRect (ctxNodeArena ctx) idx
                pure (IM.insert (intKey wid) (w, h) acc)
          foldlSize (idx + 1) count acc'

updateWindowDrag :: Context -> Input -> IO Bool
updateWindowDrag ctx inp = do
  resizing <- isJust <$> readIORef (ctxWindowResize ctx)
  if resizing
    then pure False
    else do
      drag <- readIORef (ctxWindowDrag ctx)
      case drag of
        Just (wid, gx, gy)
          | inputMouseDown inp -> do
              let V2 mx my = inputMousePos inp
                  pos = (mx - gx, my - gy)
              store <- getStore ctx
              setStore ctx (store {storeWindow = IM.insert (intKey wid) pos (storeWindow store)})
              markDirty ctx
              pure True
          | otherwise -> do
              writeIORef (ctxWindowDrag ctx) Nothing
              pure False
        Nothing
          | inputMousePressed inp -> do
              started <- tryStartWindowDrag ctx (inputMousePos inp)
              pure started
          | otherwise -> pure False

windowResizeHandle :: Float
windowResizeHandle = 12

windowResizeHalo :: Rect -> Rect
windowResizeHalo (Rect x y w h) =
  Rect (x - windowResizeHandle) (y - windowResizeHandle) (w + 2 * windowResizeHandle) (h + 2 * windowResizeHandle)

-- Handles sit outside the window. The right pad strip also resizes beside the bar.
windowResizeEdgeAt :: Rect -> V2 -> Maybe WindowResizeEdge
windowResizeEdgeAt (Rect x y w h) (V2 mx my) =
  let s = windowResizeHandle
      onL = mx >= x - s && mx < x
      onR = mx > x + w && mx <= x + w + s
      onT = my >= y - s && my < y
      onB = my > y + h && my <= y + h + s
   in if not (onL || onR || onT || onB)
        then Nothing
        else
          Just $
            case (onT, onB, onL, onR) of
              (True, _, True, _) -> ResizeNW
              (True, _, _, True) -> ResizeNE
              (_, True, True, _) -> ResizeSW
              (_, True, _, True) -> ResizeSE
              (True, _, _, _) -> ResizeN
              (_, True, _, _) -> ResizeS
              (_, _, True, _) -> ResizeW
              _ -> ResizeE

innerEastCornerEdge :: Padding -> Rect -> Float -> Maybe WindowResizeEdge
innerEastCornerEdge pad (Rect _ y _ h) my =
  let topBand = max 6 (min windowResizeHandle (padT pad))
      botBand = max 6 (min windowResizeHandle (padB pad))
   in case (my >= y && my < y + topBand, my > y + h - botBand && my <= y + h) of
        (True, _) -> Just ResizeNE
        (_, True) -> Just ResizeSE
        _ -> Just ResizeE

windowBodyScrollLane :: Context -> NodeIdx -> IO (Maybe Rect)
windowBodyScrollLane ctx winIdx = do
  fc <- getFirstChild (ctxNodeArena ctx) winIdx
  go fc
  where
    go ci
      | ci < 0 = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          case nt of
            NodeScrollContainer -> do
              slot <- scrollBarSlotOf (ctxNodeArena ctx) ci
              if slot /= ScrollBarWindow
                then go ns
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) ci
                  pad <- getPadding (ctxNodeArena ctx) ci
                  dir <- getDirection (ctxNodeArena ctx) ci
                  contentSize <- getNodeValue (ctxNodeArena ctx) ci
                  let fm = ctxFontMetrics ctx
                      innerH = h - padT pad - padB pad
                  if contentSize <= innerH
                    then go ns
                    else
                      pure
                        ( Just
                            (scrollChromeLane (ctxHostProfile ctx) fm slot dir x y w h pad)
                        )
            _ -> go ns

windowInnerResizeEdgeAt :: Context -> NodeIdx -> Rect -> V2 -> IO (Maybe WindowResizeEdge)
windowInnerResizeEdgeAt ctx winIdx winRect mouse = do
  hit <- windowInnerEastResizeHit ctx winIdx winRect mouse
  if hit
    then do
      pad <- getPadding (ctxNodeArena ctx) winIdx
      pure (innerEastCornerEdge pad winRect (v2Y mouse))
    else pure Nothing

windowResizeEdgeFor :: Context -> NodeIdx -> Rect -> V2 -> IO (Maybe WindowResizeEdge)
windowResizeEdgeFor ctx winIdx winRect mouse = do
  case windowResizeEdgeAt winRect mouse of
    Just edge -> pure (Just edge)
    Nothing -> windowInnerResizeEdgeAt ctx winIdx winRect mouse

cursorForResizeEdge :: WindowResizeEdge -> UiCursorKind
cursorForResizeEdge edge =
  case edge of
    ResizeN -> UiCursorNsResize
    ResizeS -> UiCursorNsResize
    ResizeE -> UiCursorEwResize
    ResizeW -> UiCursorEwResize
    ResizeNW -> UiCursorNwseResize
    ResizeSE -> UiCursorNwseResize
    ResizeNE -> UiCursorNeswResize
    ResizeSW -> UiCursorNeswResize

resizeFromEdge :: WindowResizeDrag -> V2 -> Float -> Float -> (Float, Float, Float, Float)
resizeFromEdge wrd (V2 mx my) winW winH =
  let dx = mx - wrdGrabX wrd
      dy = my - wrdGrabY wrd
      minW = wrdMinW wrd
      minH = wrdMinH wrd
      maxW = min (wrdMaxW wrd) winW
      maxH = min (wrdMaxH wrd) winH
      right0 = wrdStartX wrd + wrdStartW wrd
      bottom0 = wrdStartY wrd + wrdStartH wrd
      fromE = case wrdEdge wrd of
        ResizeE -> True
        ResizeNE -> True
        ResizeSE -> True
        _ -> False
      fromW = case wrdEdge wrd of
        ResizeW -> True
        ResizeNW -> True
        ResizeSW -> True
        _ -> False
      fromS = case wrdEdge wrd of
        ResizeS -> True
        ResizeSE -> True
        ResizeSW -> True
        _ -> False
      fromN = case wrdEdge wrd of
        ResizeN -> True
        ResizeNE -> True
        ResizeNW -> True
        _ -> False
      w0
        | fromE = wrdStartW wrd + dx
        | fromW = wrdStartW wrd - dx
        | otherwise = wrdStartW wrd
      h0
        | fromS = wrdStartH wrd + dy
        | fromN = wrdStartH wrd - dy
        | otherwise = wrdStartH wrd
      w = max minW (min maxW w0)
      h = max minH (min maxH h0)
      x0 = if fromW then right0 - w else wrdStartX wrd
      y0 = if fromN then bottom0 - h else wrdStartY wrd
      x = max 0 (min x0 (max 0 (winW - w)))
      y = max 0 (min y0 (max 0 (winH - h)))
   in (w, h, x, y)

updateWindowResize :: Context -> Input -> Float -> Float -> IO Bool
updateWindowResize ctx inp winW winH = do
  drag <- readIORef (ctxWindowResize ctx)
  case drag of
    Just wrd
      | inputMouseDown inp -> do
          let (nw, nh, nx, ny) = resizeFromEdge wrd (inputMousePos inp) winW winH
          store <- getStore ctx
          setStore
            ctx
            ( store
                { storeWindowSize = IM.insert (intKey (wrdWidget wrd)) (nw, nh) (storeWindowSize store)
                , storeWindow = IM.insert (intKey (wrdWidget wrd)) (nx, ny) (storeWindow store)
                }
            )
          relayoutWindow ctx winW winH (wrdWidget wrd) nw nh
          markDirty ctx
          pure True
      | otherwise -> do
          writeIORef (ctxWindowResize ctx) Nothing
          pure False
    Nothing
      | inputMousePressed inp -> tryStartWindowResize ctx (inputMousePos inp)
      | otherwise -> pure False

relayoutWindow :: Context -> Float -> Float -> WidgetId -> Float -> Float -> IO ()
relayoutWindow ctx winW winH wid nw nh = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure ()
    Just idx -> do
      (minW, minH, maxW, maxH) <- getMinMax (ctxNodeArena ctx) idx
      let w = max minW (min (min maxW winW) nw)
          h = max minH (min (min maxH winH) nh)
      mpos <- lookupWindowPos ctx wid
      (x, y, _, _) <- getRect (ctxNodeArena ctx) idx
      let (x0, y0) = maybe (x, y) id mpos
          x' = max 0 (min x0 (max 0 (winW - w)))
          y' = max 0 (min y0 (max 0 (winH - h)))
      positionWindowNode (ctxNodeArena ctx) (ctxHostProfile ctx) (ctxFontMetrics ctx) idx x' y' w h

tryStartWindowResize :: Context -> V2 -> IO Bool
tryStartWindowResize ctx mouse = do
  mWin <- topmostWindowAtResizeHalo ctx mouse
  case mWin of
    Nothing -> pure False
    Just idx -> do
      blocked <- resizeHaloBlocked ctx mouse idx
      overClose <- windowTitleHasInteractive ctx idx mouse
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      if blocked || overClose
        then pure False
        else do
          mEdge <- windowResizeEdgeFor ctx idx (Rect x y w h) mouse
          case mEdge of
            Nothing -> pure False
            Just edge -> do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (minW, minH, maxW, maxH) <- getMinMax (ctxNodeArena ctx) idx
              let V2 mx my = mouse
              writeIORef (ctxWindowResize ctx) $
                Just
                  WindowResizeDrag
                    { wrdWidget = wid
                    , wrdEdge = edge
                    , wrdGrabX = mx
                    , wrdGrabY = my
                    , wrdStartX = x
                    , wrdStartY = y
                    , wrdStartW = w
                    , wrdStartH = h
                    , wrdMinW = minW
                    , wrdMinH = minH
                    , wrdMaxW = maxW
                    , wrdMaxH = maxH
                    }
              markDirty ctx
              pure True

windowResizeCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
windowResizeCursorKind ctx inp = do
  mDrag <- readIORef (ctxWindowResize ctx)
  case mDrag of
    Just wrd | inputMouseDown inp -> pure (Just (cursorForResizeEdge (wrdEdge wrd)))
    Just _ -> pure Nothing
    Nothing -> do
      let mouse = inputMousePos inp
      mWin <- topmostWindowAtResizeHalo ctx mouse
      case mWin of
        Nothing -> pure Nothing
        Just idx -> do
          blocked <- resizeHaloBlocked ctx mouse idx
          overClose <- windowTitleHasInteractive ctx idx mouse
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          if blocked || overClose
            then pure Nothing
            else fmap cursorForResizeEdge <$> windowResizeEdgeFor ctx idx (Rect x y w h) mouse

-- Halo must not steal hits from page widgets or another window's interior.
resizeHaloBlocked :: Context -> V2 -> NodeIdx -> IO Bool
resizeHaloBlocked ctx mouse winIdx = do
  mInside <- topmostWindowAtMouse ctx mouse
  case mInside of
    Just other | other /= winIdx -> pure True
    _ -> do
      hot <- probeHotId ctx mouse
      if hashWidgetId hot == 0
        then pure False
        else do
          mHot <- findNodeByWidgetId ctx hot
          case mHot of
            Nothing -> pure False
            Just hotIdx -> not <$> nodeInSubtree ctx hotIdx winIdx

tryStartWindowDrag :: Context -> V2 -> IO Bool
tryStartWindowDrag ctx mouse = do
  mWin <- topmostWindowAtMouse ctx mouse
  case mWin of
    Nothing -> pure False
    Just idx -> do
      mTitle <- windowTitleRect ctx idx
      case mTitle of
        Nothing -> pure False
        Just title -> do
          let overTitle = rectContains title mouse
          overClose <- windowTitleHasInteractive ctx idx mouse
          if overTitle && not overClose
            then do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              (wx, wy, _, _) <- getRect (ctxNodeArena ctx) idx
              let V2 mx my = mouse
              writeIORef (ctxWindowDrag ctx) (Just (wid, mx - wx, my - wy))
              markDirty ctx
              pure True
            else pure False

windowTitleRect :: Context -> NodeIdx -> IO (Maybe Rect)
windowTitleRect ctx idx = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc Nothing
  where
    go ci best
      | ci < 0 = pure best
      | otherwise = do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) ci
          ns <- getNextSibling (ctxNodeArena ctx) ci
          let here = Rect x y w h
              best' =
                case best of
                  Nothing -> Just here
                  Just b -> if y < rectY b then Just here else Just b
          go ns best'

windowTitleHasInteractive :: Context -> NodeIdx -> V2 -> IO Bool
windowTitleHasInteractive ctx idx mouse = do
  count <- arenaCount (ctxNodeArena ctx)
  mWid <- findTopWidgetUnderMouse ctx count mouse isInteractiveNode
  case mWid of
    Nothing -> pure False
    Just wid -> do
      mNode <- findNodeByWidgetId ctx wid
      case mNode of
        Nothing -> pure False
        Just wi -> nodeInSubtree ctx wi idx

filterModalFocusables :: Context -> [WidgetId] -> IO [WidgetId]
filterModalFocusables ctx ids = do
  open <- modalTreeOpen ctx
  if not open
    then pure ids
    else filterM (widgetIdInModal ctx) ids

widgetIdInModal :: Context -> WidgetId -> IO Bool
widgetIdInModal ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  go 0 count
  where
    go idx count
      | idx >= count = pure False
      | otherwise = do
          w' <- getWidgetId (ctxNodeArena ctx) idx
          if w' == wid
            then nodeInTopmostModal ctx idx
            else go (idx + 1) count

widgetOverlayAllowed :: Context -> WidgetId -> IO Bool
widgetOverlayAllowed ctx wid = do
  open <- modalTreeOpen ctx
  if not open then pure True else widgetIdInModal ctx wid

constrainFocusToModal :: Context -> IO ()
constrainFocusToModal ctx = do
  open <- modalTreeOpen ctx
  when open $ do
    focus <- readIORef (ctxFocusId ctx)
    when (hashWidgetId focus /= 0) $ do
      ok <- widgetIdInModal ctx focus
      unless ok $ writeIORef (ctxFocusId ctx) (WidgetId 0)

drawWindowOverlays :: Context -> IO ()
drawWindowOverlays ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  let da = ctxDrawArena ctx
      theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
      style = overlayWindowStyle theme
  forM_ [0 .. count - 1] $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    when (nt == NodeWindow) $ do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      let rect = Rect x y w h
      when (not terminal) $ pushMenuShadow da rect (styleCornerRadius style)
      fillStyledRect da terminal style rect
      strokeStyledRect da terminal style x y w h
      withClip da rect $ walkChildren ctx idx

drawModalOverlays :: Context -> Size -> IO ()
drawModalOverlays ctx (Size ww wh) = do
  count <- arenaCount (ctxNodeArena ctx)
  let da = ctxDrawArena ctx
      theme = ctxTheme ctx
      fm = ctxFontMetrics ctx
      terminal = isCellHost (ctxHostProfile ctx)
  found <- modalTreeOpen ctx
  when found $ do
    when terminal $
      pushBackdropDim da (Rect 0 0 ww wh) (themeOverlayDim theme)
    when (not terminal) $
      pushRect da (Rect 0 0 ww wh) (themeOverlayDim theme)
    forM_ [0 .. count - 1] $ \idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      when (nt == NodeModal) $ do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        pad <- getPadding (ctxNodeArena ctx) idx
        wid <- getWidgetId (ctxNodeArena ctx) idx
        let rect = Rect x y w h
            style =
              if terminal
                then overlayWindowStyle theme
                else overlayModalStyle theme
        when (not terminal) $ pushMenuShadow da rect (styleCornerRadius style)
        fillStyledRect da terminal style rect
        strokeStyledRect da terminal style x y w h
        dir <- getDirection (ctxNodeArena ctx) idx
        contentSize <- getNodeValue (ctxNodeArena ctx) idx
        slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
        let clip =
              if terminal
                then terminalModalOuterClip (ctxHostProfile ctx) fm x y w h pad
                else scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
        withClip da clip $ walkChildren ctx idx
        when (not terminal) $
          paintScrollChrome ctx da idx wid x y w h pad theme terminal

collectFloatingSpans :: Context -> IM.IntMap (Maybe NodeType) -> NodeType -> IO [(Rect, T.Text, Color, Color, Rect)]
collectFloatingSpans ctx floatCache wanted = do
  count <- arenaCount (ctxNodeArena ctx)
  let fm = ctxFontMetrics ctx
      go idx
        | idx >= count = pure []
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= wanted
              then go (idx + 1)
              else do
                (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                pad <- getPadding (ctxNodeArena ctx) idx
                dir <- getDirection (ctxNodeArena ctx) idx
                contentSize <- getNodeValue (ctxNodeArena ctx) idx
                slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
                let clip =
                      if isCellHost (ctxHostProfile ctx) && nt == NodeModal
                        then terminalModalOuterClip (ctxHostProfile ctx) fm x y w h pad
                        else
                          if isScrollNode nt
                            then scrollContentClip (ctxHostProfile ctx) fm slot dir x y w h pad contentSize
                            else padContentClip (ctxHostProfile ctx) fm x y w h pad
                here <- walkChildSpans ctx floatCache idx clip
                rest <- go (idx + 1)
                pure (here ++ rest)
  go 0

strokeRect :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
strokeRect da x y w h bw col =
  let inset = 0.5
      ox = x + inset
      oy = y + inset
      ow = max 0 (w - 2 * inset)
      oh = max 0 (h - 2 * inset)
   in pushRoundedStroke da (Rect ox oy ow oh) 0 (max 1 bw) col

selectItemH :: HostProfile -> Float -> Float
selectItemH host rh = if isCellHost host then max 1 rh else 28

selectDropOuterPad :: HostProfile -> Float
selectDropOuterPad host = if isCellHost host then 0 else textInputMenuOuterPad

selectDropBg :: Style -> Color
selectDropBg st = styleBg st

selectDropActiveBg :: Style -> Color
selectDropActiveBg st = styleActiveBg st

selectDropHoverBg :: Style -> Color
selectDropHoverBg st = styleHoverBg st

-- The list hangs directly off the select, with no gap on any backend.
selectDropRect :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Int -> Rect
selectDropRect host _fm x y w h nOpts =
  let itemH = selectItemH host h
      pad = selectDropOuterPad host
   in Rect x (y + h) w (itemH * fromIntegral nOpts + 2 * pad)

selectDropItemY :: HostProfile -> FontMetrics -> Rect -> Float -> Int -> Float
selectDropItemY host _fm dropRect itemH i =
  rectY dropRect + selectDropOuterPad host + itemH * fromIntegral i

selectDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
selectDropPickIndex dropRect itemH nOpts mouseY =
  let innerH = itemH * fromIntegral nOpts
      pad = max 0 ((rectH dropRect - innerH) / 2)
      rel = mouseY - rectY dropRect - pad
   in if rel < 0 || rel >= innerH
        then Nothing
        else
          Just (max 0 (min (nOpts - 1) (floor (rel / max itemH 1))))

terminalDropRow :: Int -> Int -> Int -> T.Text -> Color -> Color -> Rect -> (Rect, T.Text, Color, Color, Rect)
terminalDropRow x y w txt fg bg clip =
  (Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) 1, txt, fg, bg, clip)

-- Title-bar rule and other column separators: glyphs, not a filled hairline.
terminalSeparatorSpans :: Theme -> Float -> Float -> Float -> Float -> [(Rect, T.Text, Color, Color)]
terminalSeparatorSpans theme x y w h =
  let sepFg = themeSeparator theme
      sepBg = colorRGBA 0 0 0 0
      hair = 1 :: Float
   in if w >= h
        then
          let wi :: Int
              wi = max 1 (round w)
              rowY :: Int
              rowY = round (y + (h - hair) / 2)
              rowX :: Int
              rowX = round x
           in
            [ ( Rect (fromIntegral rowX) (fromIntegral rowY) (fromIntegral wi) hair
              , T.replicate wi (T.singleton '\x2500')
              , sepFg
              , sepBg
              )
            ]
        else
          let hi :: Int
              hi = max 1 (round h)
              colX :: Int
              colX = round (x + (w - hair) / 2)
              colY :: Int
              colY = round y
           in
            [ ( Rect (fromIntegral colX) (fromIntegral (colY + i)) hair hair
              , T.singleton '\x2502'
              , sepFg
              , sepBg
              )
            | i <- [0 .. hi - 1]
            ]

-- Glyph tiers cap a vertical TUI scrollbar with carets. ASCII draws none, and a
-- track under three cells has no room for them.
terminalScrollCapSpans ::
  Context ->
  NodeIdx ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Rect ->
  IO [(Rect, T.Text, Color, Color, Rect)]
terminalScrollCapSpans ctx idx x y w h pad clip
  | T.null up && T.null down = pure []
  | otherwise = do
      dir <- getDirection (ctxNodeArena ctx) idx
      if dir /= DirColumn
        then pure []
        else do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          contentSize <- getNodeValue (ctxNodeArena ctx) idx
          off <- getScrollOffset ctx wid
          slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
          case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
            Just layout
              | rectH (sbTrack layout) >= 3
              , let trackW = rectW (sbTrack layout)
              , all (\t -> T.null t || fromIntegral (terminalPaintColumns t) <= trackW) [up, down] ->
                  do
              let track = sbTrack layout
                  fg = themeSeparator (ctxTheme ctx)
                  bg = colorRGBA 0 0 0 0
                  cell ty txt =
                    let pw = fromIntegral (terminalPaintColumns txt)
                     in (Rect (rectX track) ty pw 1, txt, fg, bg)
              pure $
                tagClippedSpans
                  clip
                  [ cell ty txt
                  | (ty, txt) <-
                      [ (rectY track, up)
                      , (rectY track + rectH track - 1, down)
                      ]
                  , not (T.null txt)
                  ]
            _ -> pure []
  where
    fm = ctxFontMetrics ctx
    icons = ctxIcons ctx
    up = iconScrollUp icons
    down = iconScrollDown icons

padDropText :: Int -> T.Text -> T.Text
padDropText n txt =
  let len = T.length txt
   in if len >= n then T.take n txt else txt <> T.replicate (n - len) (T.singleton ' ')

terminalSelectDropdownSpans ::
  Int ->
  Int ->
  Int ->
  [T.Text] ->
  Int ->
  Maybe Int ->
  Color ->
  Color ->
  Color ->
  Color ->
  Rect ->
  [(Rect, T.Text, Color, Color, Rect)]
terminalSelectDropdownSpans rx ry wi opts picked hoverIdx fg dropBg dropActiveBg dropHoverBg clip =
  let innerW = max 0 (wi - 1)
      itemRow opt = T.singleton ' ' <> padDropText innerW opt
      rowBg i =
        if Just i == hoverIdx
          then dropHoverBg
          else
            if i == picked
              then dropActiveBg
              else dropBg
   in [ terminalDropRow rx (ry + i) wi rowText fg (rowBg i) clip
      | (i, opt) <- zip [0 ..] opts
      , let rowText = if T.null opt then T.replicate wi (T.singleton ' ') else itemRow opt
      ]

drawCloseIcon :: HostProfile -> FontMetrics -> DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawCloseIcon host fm da x y w h col = do
  let s = min w h * 0.72
      th = s
      ty = centeredTextY host fm y h th
      tx = x + (w - s) / 2
      inset = s * 0.32
      t = max 1.6 (s * 0.10)
      x0 = tx + inset
      y0 = ty + inset
      x1 = tx + s - inset
      y1 = ty + s - inset
  pushLine da x0 y0 x1 y1 t col
  pushLine da x0 y1 x1 y0 t col

drawSelectChevron :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawSelectChevron da x y w h col = do
  let cx = selectChevronCenterX x w
      cy = y + h / 2
      hw = 4.2
      hh = 2.6
  pushFilledTriangle da (cx - hw) (cy - hh * 0.35) (cx + hw) (cy - hh * 0.35) cx (cy + hh) col

padContentClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
padContentClip host fm x y w h pad0 =
  let pad = resolveLayoutPadding host fm pad0
   in Rect
        (x + padL pad)
        (y + padT pad)
        (max 0 (w - padL pad - padR pad))
        (max 0 (h - padT pad - padB pad))

-- TUI modal: title and separator stay fixed; modal/2 wraps body in scroll.
-- Outer clip is the padded panel. Inner NodeScrollContainer clips overflow.
terminalModalOuterClip :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> Padding -> Rect
terminalModalOuterClip = padContentClip

scrollContentClip ::
  HostProfile ->
  FontMetrics ->
  ScrollBarSlot ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Rect
scrollContentClip host fm slot dir x y w h pad contentSize =
  let base = padContentClip host fm x y w h pad
      innerMain =
        case dir of
          DirColumn -> rectH base
          DirRow -> rectW base
      gutter = scrollLayoutGutter host fm slot contentSize innerMain
   in case dir of
        DirColumn -> Rect (rectX base) (rectY base) (max 0 (rectW base - gutter)) (rectH base)
        DirRow -> Rect (rectX base) (rectY base) (rectW base) (max 0 (rectH base - gutter))

-- List/page bars sit in the scroll rect. Window body hangs into the parent pad.
scrollChromeLane ::
  HostProfile -> FontMetrics -> ScrollBarSlot -> DirTag -> Float -> Float -> Float -> Float -> Padding -> Rect
scrollChromeLane host fm slot dir x y w h pad =
  let (barW, _) = scrollBarGeomFor host fm slot
      outer = scrollBarOuterGap host fm slot
      hang = slot == ScrollBarWindow
   in case dir of
        DirColumn ->
          let laneX =
                if hang
                  then x + w + outer
                  else max x (x + w - outer - barW)
           in Rect laneX (y + padT pad) barW (max 0 (h - padT pad - padB pad))
        DirRow ->
          let laneY =
                if hang
                  then y + h + outer
                  else max y (y + h - outer - barW)
           in Rect (x + padL pad) laneY (max 0 (w - padL pad - padR pad)) barW

data ScrollBarLayout = ScrollBarLayout
  { sbTrack :: Rect
  , sbThumb :: Rect
  , sbMaxOff :: Float
  }
  deriving (Eq, Show)

scrollBarLayout ::
  HostProfile ->
  FontMetrics ->
  ScrollBarSlot ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Maybe ScrollBarLayout
scrollBarLayout host fm slot dir x y w h pad contentSize off =
  let (barW, barMargin) = scrollBarGeomFor host fm slot
      minThumb = if isCellHost host then barW else 16
   in case dir of
    DirColumn ->
      let innerH = h - padT pad - padB pad
          maxOff = max 0 (contentSize - innerH)
       in if maxOff <= 0
            then Nothing
            else
              let lane = scrollChromeLane host fm slot DirColumn x y w h pad
                  trackX = rectX lane
                  trackY = y + padT pad + barMargin
                  trackH = max 0 (innerH - 2 * barMargin)
                  thumbH = max minThumb (trackH * innerH / contentSize)
                  ratio = off / maxOff
                  thumbY = trackY + ratio * (trackH - thumbH)
               in
                Just
                  ScrollBarLayout
                    { sbTrack = Rect trackX trackY barW trackH
                    , sbThumb = Rect trackX thumbY barW thumbH
                    , sbMaxOff = maxOff
                    }
    DirRow ->
      let innerW = w - padL pad - padR pad
          maxOff = max 0 (contentSize - innerW)
       in if maxOff <= 0
            then Nothing
            else
              let lane = scrollChromeLane host fm slot DirRow x y w h pad
                  trackY = rectY lane
                  trackX = x + padL pad + barMargin
                  trackW = max 0 (innerW - 2 * barMargin)
                  thumbW = max minThumb (trackW * innerW / contentSize)
                  ratio = off / maxOff
                  thumbX = trackX + ratio * (trackW - thumbW)
               in
                Just
                  ScrollBarLayout
                    { sbTrack = Rect trackX trackY trackW barW
                    , sbThumb = Rect thumbX trackY thumbW barW
                    , sbMaxOff = maxOff
                    }

scrollOffsetFromThumb ::
  DirTag -> ScrollBarLayout -> Float -> V2 -> Float
scrollOffsetFromThumb dir layout grabOff mouse =
  let maxOff = sbMaxOff layout
      track = sbTrack layout
      thumb = sbThumb layout
   in case dir of
        DirColumn ->
          let trackY = rectY track
              trackH = rectH track
              thumbH = rectH thumb
              thumbTop = v2Y mouse - grabOff
              ratio = (thumbTop - trackY) / max 1 (trackH - thumbH)
           in max 0 (min maxOff (ratio * maxOff))
        DirRow ->
          let trackX = rectX track
              trackW = rectW track
              thumbW = rectW thumb
              thumbLeft = v2X mouse - grabOff
              ratio = (thumbLeft - trackX) / max 1 (trackW - thumbW)
           in max 0 (min maxOff (ratio * maxOff))

updateScrollDrag :: Context -> Input -> IO ()
updateScrollDrag ctx inp = do
  mDrag <- readIORef (ctxScrollDrag ctx)
  if inputMouseReleased inp
    then writeIORef (ctxScrollDrag ctx) Nothing
    else
      case mDrag of
        Just (wid, grabOff) | inputMouseDown inp -> do
          mGeom <- scrollContainerGeom ctx wid
          case mGeom of
            Nothing -> pure ()
            Just (idx, dir, x, y, w, h, pad, contentSize) -> do
              off <- getScrollOffset ctx wid
              let fm = ctxFontMetrics ctx
              slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
              case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
                Nothing -> pure ()
                Just layout -> do
                  let newOff = scrollOffsetFromThumb dir layout grabOff (inputMousePos inp)
                  when (newOff /= off) $ setScrollOffset ctx wid newOff
        Nothing | inputMousePressed inp -> tryStartScrollDrag ctx inp
        _ -> pure ()

scrollContainerGeom ::
  Context -> WidgetId -> IO (Maybe (NodeIdx, DirTag, Float, Float, Float, Float, Padding, Float))
scrollContainerGeom ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure Nothing
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if not (isScrollNode nt)
              then go (idx + 1)
              else do
                w' <- getWidgetId (ctxNodeArena ctx) idx
                if w' /= wid
                  then go (idx + 1)
                  else do
                    dir <- getDirection (ctxNodeArena ctx) idx
                    pad <- getPadding (ctxNodeArena ctx) idx
                    contentSize <- getNodeValue (ctxNodeArena ctx) idx
                    (x, y, w, h) <- getScrollVisualRect ctx idx
                    pure (Just (idx, dir, x, y, w, h, pad, contentSize))
  go 0

tryStartScrollDrag :: Context -> Input -> IO ()
tryStartScrollDrag ctx inp =
  when (inputMousePressed inp) $ do
    let mouse = inputMousePos inp
    mTarget <- findScrollTargetUnderMouse ctx mouse
    case mTarget of
      Nothing -> pure ()
      Just wid -> tryStartScrollDragOn ctx wid mouse

tryStartScrollDragOn :: Context -> WidgetId -> V2 -> IO ()
tryStartScrollDragOn ctx wid mouse = do
  mGeom <- scrollContainerGeom ctx wid
  case mGeom of
    Nothing -> pure ()
    Just (idx, dir, x, y, w, h, pad, contentSize) -> do
      off <- getScrollOffset ctx wid
      let fm = ctxFontMetrics ctx
      slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
      case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
        Nothing -> pure ()
        Just layout -> do
          let thumb = sbThumb layout
              track = sbTrack layout
          if rectContains thumb mouse
            then do
              let grabOff =
                    case dir of
                      DirColumn -> v2Y mouse - rectY thumb
                      DirRow -> v2X mouse - rectX thumb
              writeIORef (ctxScrollDrag ctx) (Just (wid, grabOff))
            else
              when (rectContains track mouse) $ do
                let maxOff = sbMaxOff layout
                    thumbH = rectH thumb
                    thumbW = rectW thumb
                    newOff =
                      case dir of
                        DirColumn ->
                          let trackY = rectY track
                              trackH = rectH track
                              ratio =
                                (v2Y mouse - trackY - thumbH / 2)
                                  / max 1 (trackH - thumbH)
                           in max 0 (min maxOff (ratio * maxOff))
                        DirRow ->
                          let trackX = rectX track
                              trackW = rectW track
                              ratio =
                                (v2X mouse - trackX - thumbW / 2)
                                  / max 1 (trackW - thumbW)
                           in max 0 (min maxOff (ratio * maxOff))
                setScrollOffset ctx wid newOff
                let grabOff =
                      case dir of
                        DirColumn -> thumbH / 2
                        DirRow -> thumbW / 2
                writeIORef (ctxScrollDrag ctx) (Just (wid, grabOff))

paintScrollChrome ::
  Context ->
  DrawArena ->
  NodeIdx ->
  WidgetId ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Theme ->
  Bool ->
  IO ()
paintScrollChrome ctx da idx wid x y w h pad theme terminal = do
  layer <- currentLayer da
  let barLayer =
        case layer of
          LayerOverlay -> LayerChrome
          _ -> LayerContent
  beginLayer da barLayer
  drawScrollBar ctx da idx wid x y w h pad theme terminal
  beginLayer da layer

drawScrollBar ::
  Context ->
  DrawArena ->
  NodeIdx ->
  WidgetId ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Theme ->
  Bool ->
  IO ()
drawScrollBar ctx da idx wid x y w h pad theme terminal = do
  dir <- getDirection (ctxNodeArena ctx) idx
  contentSize <- getNodeValue (ctxNodeArena ctx) idx
  off <- getScrollOffset ctx wid
  let fm = ctxFontMetrics ctx
  slot <- scrollBarSlotOf (ctxNodeArena ctx) idx
  let base =
        case slot of
          ScrollBarWindow -> themeFloatingWindow theme
          _ -> themePanel theme
      trackBg = scrollBarTrackColor base theme terminal
      thumbCol = scrollBarThumbColor base theme terminal
  case scrollBarLayout (ctxHostProfile ctx) fm slot dir x y w h pad contentSize off of
    Nothing -> pure ()
    Just layout -> do
      let track = sbTrack layout
          thumb = sbThumb layout
      if terminal
        then do
          pushRect da track trackBg
          pushRect da thumb thumbCol
        else do
          let trackR = min 4 (min (rectW track) (rectH track) / 2)
              thumbR = min 4 (min (rectW thumb) (rectH thumb) / 2)
          pushRoundedRect da track trackR trackBg
          pushRoundedRect da thumb thumbR thumbCol

drawSelectOverlays :: Context -> Input -> IO ()
drawSelectOverlays ctx inp = do
  let mouse = inputMousePos inp
      da = ctxDrawArena ctx
      theme = ctxTheme ctx
      fm = ctxFontMetrics ctx
      terminal = isCellHost (ctxHostProfile ctx)
  count <- arenaCount (ctxNodeArena ctx)
  when (not terminal) $ do
    let go idx
          | idx >= count = pure ()
          | otherwise = do
              nt <- getNodeType (ctxNodeArena ctx) idx
              if nt /= NodeSelect
                then go (idx + 1)
                else do
                  wid <- getWidgetId (ctxNodeArena ctx) idx
                  store <- getStore ctx
                  let key = intKey wid
                  if not (IM.findWithDefault False key (storeSelectOpen store))
                    then go (idx + 1)
                    else do
                      allow <- widgetOverlayAllowed ctx wid
                      if not allow
                        then go (idx + 1)
                        else do
                          txt <- getText (ctxNodeArena ctx) idx
                          let (_, opts) = selectParseOptions txt
                          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                          let picked = IM.findWithDefault 0 key (storeSelect store)
                              itemH = selectItemH (ctxHostProfile ctx) h
                              dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                              dropStyle = overlayMenuStyle theme
                              r = styleCornerRadius dropStyle
                          pushMenuShadow da dropRect r
                          fillStyledRect da False dropStyle dropRect
                          strokeStyledRect
                            da
                            False
                            dropStyle
                            (rectX dropRect)
                            (rectY dropRect)
                            (rectW dropRect)
                            (rectH dropRect)
                          forM_ (zip ([0 ..] :: [Int]) opts) $ \(i, _opt) -> do
                            let iy = selectDropItemY (ctxHostProfile ctx) fm dropRect itemH i
                                itemRect = Rect (rectX dropRect) iy (rectW dropRect) itemH
                                hovered = rectContains itemRect mouse
                            when (hovered || i == picked) $ do
                              let bg =
                                    if hovered
                                      then styleHoverBg dropStyle
                                      else styleActiveBg dropStyle
                              pushRect da itemRect bg
                              when hovered $ do
                                let accent = themeAccent theme
                                    barRect = Rect (rectX itemRect) (rectY itemRect + 3) 2 (rectH itemRect - 6)
                                pushRoundedRect da barRect 1 accent
                          go (idx + 1)
    go 0

collectSelectDropdownSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectSelectDropdownSpans ctx inp = do
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
      mouse = inputMousePos inp
  count <- arenaCount (ctxNodeArena ctx)
  let go idx
        | idx >= count = pure []
        | otherwise = do
            nt <- getNodeType (ctxNodeArena ctx) idx
            if nt /= NodeSelect
              then go (idx + 1)
              else do
                wid <- getWidgetId (ctxNodeArena ctx) idx
                store <- getStore ctx
                let key = intKey wid
                if not (IM.findWithDefault False key (storeSelectOpen store))
                  then go (idx + 1)
                  else do
                    allow <- widgetOverlayAllowed ctx wid
                    if not allow
                      then go (idx + 1)
                      else do
                        txt <- getText (ctxNodeArena ctx) idx
                        let (_, opts) = selectParseOptions txt
                        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                        let itemH = selectItemH (ctxHostProfile ctx) h
                            dropRect = selectDropRect (ctxHostProfile ctx) fm x y w h (length opts)
                            picked = IM.findWithDefault 0 key (storeSelect store)
                            dropStyle = overlayMenuStyle theme
                            fg = styleFg dropStyle
                        if isCellHost (ctxHostProfile ctx)
                          then do
                            let wi = max 1 (round w)
                                rx = round (rectX dropRect)
                                ry = round (rectY dropRect)
                                dropBg = selectDropBg dropStyle
                                dropActiveBg = selectDropActiveBg dropStyle
                                dropHoverBg = selectDropHoverBg dropStyle
                                hoverIdx =
                                  selectDropPickIndex dropRect itemH (length opts) (v2Y mouse)
                            rest <- go (idx + 1)
                            pure
                              ( terminalSelectDropdownSpans rx ry wi opts picked hoverIdx fg dropBg dropActiveBg dropHoverBg dropRect
                                  ++ rest
                              )
                          else do
                            let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
                                dropBg = styleBg dropStyle
                            itemSpans <-
                              forM (zip ([0 ..] :: [Int]) opts) $ \(i, opt) ->
                                if T.null opt
                                  then pure []
                                  else do
                                    (tw, th) <- ctxMeasureText ctx opt
                                    let itemY = selectDropItemY (ctxHostProfile ctx) fm dropRect itemH i
                                        itemRect = Rect (rectX dropRect) itemY (rectW dropRect) itemH
                                        hovered = rectContains itemRect mouse
                                        rowBg
                                          | hovered = styleHoverBg dropStyle
                                          | i == picked = styleActiveBg dropStyle
                                          | otherwise = dropBg
                                        ty = centeredTextY (ctxHostProfile ctx) fm itemY itemH th
                                        tx = rectX dropRect + textInputMenuItemPadX + ix
                                    pure [(Rect tx ty tw th, opt, fg, rowBg, dropRect)]
                            rest <- go (idx + 1)
                            pure (concat itemSpans ++ rest)
  go 0

drawTooltipOverlays :: Context -> IO ()
drawTooltipOverlays ctx = do
  let da = ctxDrawArena ctx
      theme = ctxTheme ctx
      terminal = isCellHost (ctxHostProfile ctx)
  when (not terminal) $ do
    tips <- readTooltips ctx
    let panelStyle = themePanel theme
    forM_ tips $ \(PendingTooltip wid rect _) -> do
      allow <- widgetOverlayAllowed ctx wid
      when allow $ fillStyledRect da False panelStyle rect

collectTooltipSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTooltipSpans ctx = do
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  if isCellHost (ctxHostProfile ctx)
    then pure []
    else do
      tips <- readTooltips ctx
      filtered <- filterM (\(PendingTooltip wid _ _) -> widgetOverlayAllowed ctx wid) tips
      forM filtered $ \(PendingTooltip _ rect txt) -> do
        let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
            fg = styleFg (themePanel theme)
            bg = styleBg (themePanel theme)
        (tw, th) <- ctxMeasureText ctx txt
        let tx = rectX rect + ix
            ty = centeredTextY (ctxHostProfile ctx) fm (rectY rect) (rectH rect) th
            textRect = Rect tx ty tw th
        pure (textRect, txt, fg, bg, rect)

imageIdFromText :: T.Text -> Int
imageIdFromText txt =
  case reads (T.unpack txt) of
    [(n, "")] | n > 0 -> n
    _ -> 0
