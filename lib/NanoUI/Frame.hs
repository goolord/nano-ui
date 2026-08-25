module NanoUI.Frame
  ( runFrame
  , needsRedraw
  , collectTextSpans
  , collectOverlayTextSpans
  , pointerCursorWanted
  , cursorKindIs
  , uiCursorKind
  , UiCursorKind (..)
  , sliderTrackRect
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.List (findIndex)
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , WidgetStore (..)
  , anyAnimating
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
  , pushMessage
  , setScrollOffset
  , setStore
  , setPrevRect
  , startAnimation
  , setAnimationValue
  , tickAnimations
  , getAnimationValue
  , lerpColor
  , clearTooltips
  , readTooltips
  , PendingTooltip (..)
  )
import NanoUI.Draw
  ( DrawArena
  , DrawData
  , Layer (..)
  , beginLayer
  , finishDraw
  , pushLine
  , pushRect
  , pushRoundedRect
  , pushText
  , resetDrawArena
  , withClip
  )
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , fmLineHeight
  , isTerminalFont
  , labelContentInset
  , lineWidth
  , widgetContentInset
  , wrapTextLines
  , wrapTextLinesIO
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputChanged)
import NanoUI.Layout.Arena
  ( DirTag (..)
  , NodeIdx
  , NodeType (..)
  , arenaCount
  , getDirection
  , getFirstChild
  , getMinMax
  , getNextSibling
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getText
  , getWidgetId
  , isWidgetNode
  , isContainerNode
  , NodeType (NodeButton, NodeCheckbox, NodeSelect, NodeSlider, NodeTextInput)
  , resetNodeArena
  , setNodeText
  , setNodeValue
  , setRect
  )
import NanoUI.Layout.Solve (solveLayout)
import NanoUI.Monad (UI (..))
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
import NanoUI.Style (Padding (..), Style (..), Theme (..), themeAccent, themeButton, themeInput, themePanel, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), colorRGBA, rectContains, rectH, rectIntersect, rectOverlapArea, rectW, sliderTrackRect)

runFrame :: Context -> Input -> UI a -> IO (a, [FrameMsg], DrawData, Bool)
runFrame ctx inp ui = do
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxFocusables ctx) []
  writeIORef (ctxHotId ctx) (WidgetId 0)
  writeIORef (ctxWidgetNodeTypes ctx) Nothing
  unless (inputMouseDown inp) $
    writeIORef (ctxSelectDropPress ctx) False
  clearTooltips ctx
  result <- unUI ui ctx inp
  -- Terminal sliders embed the bar in node text; sync before measure so width is correct.
  syncWidgetLabels ctx
  let Size w h = inputWindowSize inp
  solveLayout (ctxNodeArena ctx) (ctxFontMetrics ctx) (ctxMeasureText ctx) w h
  updateScrollWheel ctx inp
  updateScrollDrag ctx inp
  applyScrollOffsets ctx
  finalizePointerPress ctx inp
  finalizePointerRelease ctx inp
  finalizeTextInputFocus ctx inp
  finalizeTabFocus ctx inp
  markSelectDropPress ctx inp
  finalizeSelectPick ctx inp
  closeSelectOnOutsideClick ctx inp
  syncWidgetLabels ctx
  refreshHover ctx inp
  tickAnimations ctx (inputDeltaTime inp)
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
  beginLayer (ctxDrawArena ctx) LayerOverlay
  drawSelectOverlays ctx inp
  drawTooltipOverlays ctx
  drawData <- finishDraw (ctxDrawArena ctx)
  updatePrevRects ctx
  msgs <- drainMessages ctx
  dirtyAfterUi <- isDirty ctx
  writeIORef (ctxDirty ctx) False
  pure (result, msgs, drawData, dirtyAfterUi)

needsRedraw :: Context -> Input -> Input -> IO Bool
needsRedraw ctx prev inp = do
  dirty <- isDirty ctx
  anim <- anyAnimating ctx
  pure (dirty || anim || inputChanged prev inp)

collectTextSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextSpans ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  if count > 0
    then collectClippedSpans ctx 0 (Rect 0 0 1e9 1e9)
    else pure []

collectOverlayTextSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpans ctx inp = do
  drops <- collectSelectDropdownSpans ctx inp
  tips <- collectTooltipSpans ctx
  pure (drops ++ tips)

collectClippedSpans :: Context -> NodeIdx -> Rect -> IO [(Rect, T.Text, Color, Color, Rect)]
collectClippedSpans ctx idx clip = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let nodeRect = Rect x y w h
      mClipChildren =
        case nt of
          NodeScrollContainer -> rectIntersect clip nodeRect
          _ -> Just clip
  case mClipChildren of
    Nothing -> pure []
    Just clipHere -> do
      fm <- pure (ctxFontMetrics ctx)
      here <-
        case nt of
          NodeSelect -> do
            spans <- collectNodeTextSpans ctx idx
            pure (tagSelectClippedSpans clipHere x y w h fm spans)
          NodeTextInput
            | not (isTerminalFont fm) -> do
                spans <- collectNodeTextSpans ctx idx
                pure (tagTextInputClippedSpans clipHere x y w h fm spans)
          _ -> tagClippedSpans clipHere <$> collectNodeTextSpans ctx idx
      childSpans <- walkChildSpans ctx idx clipHere
      pure (here ++ childSpans)

walkChildSpans :: Context -> NodeIdx -> Rect -> IO [(Rect, T.Text, Color, Color, Rect)]
walkChildSpans ctx idx clip = do
  fc <- getFirstChild (ctxNodeArena ctx) idx
  go fc
  where
    go ci
      | ci < 0 = pure []
      | otherwise = do
          spans <- collectClippedSpans ctx ci clip
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

-- TTF measure is often a fraction narrower than the rendered texture.
textClipSlop :: Float
textClipSlop = 4

padTextClipRect :: Rect -> Rect
padTextClipRect (Rect x y w h) = Rect x y (w + textClipSlop) h

selectTextClip :: Float -> Float -> Float -> Float -> FontMetrics -> Rect
selectTextClip x y w h fm =
  let (ix, iy) = widgetContentInset fm
   in Rect (x + ix) (y + iy) (max 0 (w - ix - selectChevronReserve)) (max 0 (h - 2 * iy))

tagSelectClippedSpans ::
  Rect -> Float -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagSelectClippedSpans parentClip x y w h fm =
  let textClip = padTextClipRect (selectTextClip x y w h fm)
   in concatMap
        ( \(rect, txt, fg, bg) ->
            case rectIntersect parentClip textClip of
              Nothing -> []
              Just clip -> [(rect, txt, fg, bg, clip)]
        )

textInputFieldTextClip :: TextInputGeom -> FontMetrics -> Rect
textInputFieldTextClip geom fm =
  let field = tigFieldRect geom
      (ix, iy) = widgetContentInset fm
   in Rect
        (rectX field + ix)
        (rectY field + iy)
        (max 0 (rectW field - 2 * ix))
        (max 0 (rectH field - 2 * iy))

tagTextInputClippedSpans ::
  Rect -> Float -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagTextInputClippedSpans parentClip x y w h fm spans =
  let geom = textInputGeom fm x y w h
      fieldClip = textInputFieldTextClip geom fm
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

collectNodeTextSpans :: Context -> NodeIdx -> IO [(Rect, T.Text, Color, Color)]
collectNodeTextSpans ctx idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  if nt == NodeText
    then do
      txt <- getText (ctxNodeArena ctx) idx
      if T.null txt
        then pure []
        else do
          let style = themePanel theme
              (ix, iy) = labelContentInset fm
              fg = styleFg style
              bg = styleBg style
          (_, _, maxW, _) <- getMinMax (ctxNodeArena ctx) idx
          let innerW = max 0 (w - ix)
              wrapW = if maxW < 1e8 then min maxW innerW else innerW
          if maxW < 1e8
            then do
              let lineH = fmLineHeight fm
              textLines <-
                if isTerminalFont fm
                  then pure (wrapTextLines fm txt wrapW)
                  else wrapTextLinesIO (\t -> fmap fst (ctxMeasureText ctx t)) fm txt wrapW
              lineWs <-
                if isTerminalFont fm
                  then pure (map (lineWidth fm) textLines)
                  else mapM (fmap fst . ctxMeasureText ctx) textLines
              pure
                [ ( Rect
                      (x + ix)
                      (y + iy + fromIntegral i * lineH)
                      (min lw innerW)
                      lineH
                  , line
                  , fg
                  , bg
                  )
                | (i, line, lw) <- zip3 [(0 :: Int) ..] textLines lineWs
                ]
            else do
              (tw, th) <- ctxMeasureText ctx txt
              pure [(Rect (x + ix) (y + iy) (min tw innerW) th, txt, fg, bg)]
    else
      if isWidgetNode nt
        then widgetTextSpans ctx nt idx x y w h
        else pure []

displayText :: Context -> NodeType -> NodeIdx -> IO T.Text
displayText ctx nt idx = do
  txt <- getText (ctxNodeArena ctx) idx
  let terminal = isTerminalFont (ctxFontMetrics ctx)
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
              caret = if open then " v" else " >"
          pure (selectDisplayText lbl opt <> caret)
        NodeSlider -> pure (T.takeWhile (/= '\US') txt)
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

textInputGeom :: FontMetrics -> Float -> Float -> Float -> Float -> TextInputGeom
textInputGeom fm x y w _h =
  let labelH = fmLineHeight fm
      gap = textInputLabelGap fm
      fieldH = textInputFieldHeight fm
      fieldY = y + labelH + gap
   in TextInputGeom {tigFieldRect = Rect x fieldY w fieldH}

widgetHitRect :: Context -> NodeType -> Float -> Float -> Float -> Float -> Rect
widgetHitRect ctx nt x y w h =
  case nt of
    NodeTextInput
      | not (isTerminalFont (ctxFontMetrics ctx)) ->
          tigFieldRect (textInputGeom (ctxFontMetrics ctx) x y w h)
    _ -> Rect x y w h

data UiCursorKind
  = UiCursorDefault
  | UiCursorPointer
  | UiCursorText
  | UiCursorGrab
  | UiCursorGrabbing
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
  let mouse = inputMousePos inp
  table <- widgetNodeTypeTable ctx
  mDrop <- selectDropdownCursorKind ctx inp
  case mDrop of
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
                    dropRect = selectDropRect fm x y w h (length opts)
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
          if nt /= NodeScrollContainer
            then go (idx + 1) count
            else do
              wid <- getWidgetId (ctxNodeArena ctx) idx
              pad <- getPadding (ctxNodeArena ctx) idx
              contentSize <- getNodeValue (ctxNodeArena ctx) idx
              (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
              dir <- getDirection (ctxNodeArena ctx) idx
              off <- getScrollOffset ctx wid
              let fm = ctxFontMetrics ctx
              case scrollBarLayout fm dir x y w h pad contentSize off of
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
  let dragging = active == wid && inputMouseDown inp
  pure $
    case mrect of
      Nothing -> UiCursorDefault
      Just (Rect x y w h) ->
        grabDragKind (rectContains (sliderTrackRect x y w h) mouse) dragging inp

textInputCursorKind :: Context -> WidgetId -> V2 -> IO UiCursorKind
textInputCursorKind ctx wid mouse = do
  mrect <- getPrevRect ctx wid
  case mrect of
    Nothing -> pure UiCursorDefault
    Just (Rect x y w h) -> do
      let fm = ctxFontMetrics ctx
          field = tigFieldRect (textInputGeom fm x y w h)
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

stripButtonBrackets :: T.Text -> T.Text
stripButtonBrackets txt =
  let t = T.strip txt
   in if T.isPrefixOf "[ " t && T.isSuffixOf " ]" t
        then T.strip $ T.dropEnd 2 $ T.drop 2 t
        else txt

widgetTextSpans ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(Rect, T.Text, Color, Color)]
widgetTextSpans ctx nt idx x y w h = do
  fm <- pure (ctxFontMetrics ctx)
  terminal <- pure (isTerminalFont fm)
  style <- widgetVisualStyle ctx nt idx
  let fg = styleFg style
      bg = styleBg style
  if terminal
    then do
      txt <- displayText ctx nt idx
      if T.null txt
        then pure []
        else do
          let (ix, iy) = widgetContentInset fm
          (tw, th) <- ctxMeasureText ctx txt
          let fill =
                T.replicate (max 1 (round w)) (T.singleton ' ')
              fullBg = [(Rect x y w h, fill, fg, bg)]
              textSpan = [(Rect (x + ix) (y + iy) tw th, txt, fg, bg)]
          pure (fullBg ++ textSpan)
    else do
      case nt of
        NodeTextInput -> do
          placements <- widgetTextPlacements ctx nt idx x y w h
          value <- textInputValue ctx idx
          focus <- textInputFocused ctx idx
          let theme = ctxTheme ctx
              windowBg = themeWindow theme
              labelFg = lerpColor fg windowBg 0.45
              placeholder = T.null (T.pack value) && not focus
              fieldFg
                | placeholder = lerpColor fg bg 0.55
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
      terminal = isTerminalFont fm
      (ix, iy) = widgetContentInset fm
  case nt of
    NodeButton -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      pure [(txt, x + (w - tw) / 2, y + (h - th) / 2, tw, th)]
    NodeSelect -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      pure [(txt, x + ix, y + (h - th) / 2, min tw (w - ix - selectChevronReserve), th)]
    NodeCheckbox -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      let tx = x + ix + checkboxLeading fm
          ty = y + (h - th) / 2
      pure [(txt, tx, ty, tw, th)]
    NodeSlider -> do
      lbl <- displayText ctx nt idx
      let ty = y + iy
      if terminal
        then do
          (lw, lh) <- ctxMeasureText ctx lbl
          pure [(lbl, x + ix, ty, lw, lh)]
        else do
          val <- sliderValue ctx idx
          let valTxt = sliderValueText val
          (lw, lh) <- ctxMeasureText ctx lbl
          (vw, vh) <- ctxMeasureText ctx valTxt
          pure
            [ (lbl, x + ix, ty, lw, lh)
            , (valTxt, x + w - ix - vw, ty, vw, vh)
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
          pure [(shown, x + ix, y + iy, tw, th)]
        else do
          let geom = textInputGeom fm x y w h
              field = tigFieldRect geom
              fieldTxt = textInputFieldText lbl value focus
          (lw, lh) <- ctxMeasureText ctx lbl
          (fw, fh) <- ctxMeasureText ctx fieldTxt
          let ty = rectY field + (rectH field - fh) / 2
          pure
            [ (lbl, x, y, lw, lh)
            , (fieldTxt, x + ix, ty, fw, fh)
            ]
    _ -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      pure [(txt, x + ix, y + iy, tw, th)]

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
  let theme = ctxTheme ctx
      fm = ctxFontMetrics ctx
      terminal = isTerminalFont fm
      isFocus = focus == wid
      base =
        case nt of
          NodeTextInput -> themeInput theme
          NodeSelect -> themeInput theme
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
            (themeInput theme)
              { styleBg = colorRGBA 0 0 0 0
              , styleHoverBg = colorRGBA 68 71 90 48
              , styleActiveBg = colorRGBA 68 71 90 80
              , styleBorderWidth = 0
              }
          _ -> themeButton theme
      widKey = hashWidgetId wid
      isHot = wid == hot
      bg
        | terminal, widKey == hashWidgetId active = styleActiveBg base
        | terminal, isHot = styleHoverBg base
        | terminal = styleBg base
        | nt == NodeTextInput, isFocus = styleActiveBg base
        | widKey == hashWidgetId active = styleActiveBg base
        | nt == NodeCheckbox || nt == NodeSlider = styleBg base
        | otherwise = hoverBackground base animT isHot
  pure base {styleBg = bg}

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
      terminal = isTerminalFont fm
      da = ctxDrawArena ctx
  case nt of
    NodeContainer -> do
      pad <- getPadding (ctxNodeArena ctx) idx
      let panelLike =
            padL pad >= 12 || padR pad >= 12 || padT pad >= 12 || padB pad >= 12
          paintBg = panelLike
      when paintBg $ do
        let style = themePanel theme
        fillStyledRect da terminal style rect
        strokeStyledRect da terminal style (styleBg style) x y w h
      walkChildren ctx idx
    NodeScrollContainer -> do
      let style = themeInput theme
      pad <- getPadding (ctxNodeArena ctx) idx
      fillStyledRect da terminal style rect
      strokeStyledRect da terminal style (styleBg style) x y w h
      let inner =
            Rect
              (x + padL pad)
              (y + padT pad)
              (w - padL pad - padR pad)
              (h - padT pad - padB pad)
      withClip da inner $ walkChildren ctx idx
      wid <- getWidgetId (ctxNodeArena ctx) idx
      drawScrollBar ctx da idx wid x y w h pad theme terminal
    NodeText -> do
      txt <- getText (ctxNodeArena ctx) idx
      let style = themePanel theme
          (ix, iy) = labelContentInset fm
      when (not (ctxExternalText ctx) && not (T.null txt)) $ do
        (tw, th) <- ctxMeasureText ctx txt
        pushRect da (Rect (x + ix) (y + iy) tw th) (styleFg style)
    NodeSeparator -> do
      let sepH = max 1 h
      pushRect da (Rect x (y + (h - sepH) / 2) w sepH) (themeSeparator theme)
    NodeTextInput
      | not terminal -> do
          style <- widgetVisualStyle ctx nt idx
          focus <- textInputFocused ctx idx
          let geom = textInputGeom fm x y w h
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
            (styleBg style)
            (rectX fieldRect)
            (rectY fieldRect)
            (rectW fieldRect)
            (rectH fieldRect)
          drawTextInputCaret da ctx idx x y w h style
    NodeSpacer -> pure ()
    _ -> do
      style <- widgetVisualStyle ctx nt idx
      value <- getNodeValue (ctxNodeArena ctx) idx
      let opaqueBg =
            isTerminalFont fm
              || (nt /= NodeCheckbox && nt /= NodeSlider && nt /= NodeTextInput)
      when opaqueBg $ fillStyledRect da terminal style rect
      when (not terminal) $ do
        when opaqueBg $ strokeStyledRect da terminal style (styleBg style) x y w h
        when (nt == NodeCheckbox) $
          drawCheckbox da fm style x y h value (themeAccent theme)
        when (nt == NodeSlider) $ do
          let trackRect = sliderTrackRect x y w h
              trackH = rectH trackRect
              trackY = rectY trackRect
              trackR = trackH / 2
              fillW = max 0 (w * clamp01 value)
              inputBg = styleBg (themeInput theme)
          pushRoundedRect da trackRect trackR inputBg
          when (fillW > 0) $ do
            let accentR =
                  if fillW >= w - 0.5
                    then trackR
                    else min trackR (fillW / 2)
            pushRoundedRect da (Rect x trackY fillW trackH) accentR (themeAccent theme)
          let handleD = max 10 (trackH * 1.5)
              handleCx = x + max (handleD / 2) (min (w - handleD / 2) fillW)
              handleHy = trackY + (trackH - handleD) / 2
          pushRoundedRect da (Rect (handleCx - handleD / 2) handleHy handleD handleD) (handleD / 2) (styleFg style)
        when (nt == NodeTextInput && terminal) $
          drawTextInputCaret da ctx idx x y w h style
        when (nt == NodeSelect) $
          drawSelectChevron da x y w h (styleFg style)
      placements <- widgetTextPlacements ctx nt idx x y w h
      when (terminal && not (ctxExternalText ctx)) $
        forM_ placements $ \(txt, px, py, _, _) ->
          when (not (T.null txt)) $
            pushText da fm px py txt (styleFg style)

drawCheckbox :: DrawArena -> FontMetrics -> Style -> Float -> Float -> Float -> Float -> Color -> IO ()
drawCheckbox da fm style x y h value accent = do
  let (ix, _) = widgetContentInset fm
      box = checkboxBoxSize fm
      bx = x + ix
      by = y + (h - box) / 2
      r = min 4 (box / 4)
      inset = max 2 (box * 0.2)
  pushRoundedRect da (Rect bx by box box) r (styleBorder style)
  pushRoundedRect da (Rect (bx + 1) (by + 1) (box - 2) (box - 2)) (max 0 (r - 1)) (styleBg style)
  when (value >= 0.5) $
    pushRoundedRect da (Rect (bx + inset) (by + inset) (box - 2 * inset) (box - 2 * inset)) (max 1 (r - 1)) accent

drawTextInputCaret :: DrawArena -> Context -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextInputCaret da ctx idx x y w h style = do
  let terminal = isTerminalFont (ctxFontMetrics ctx)
  if terminal
    then pure ()
    else do
      focus <- textInputFocused ctx idx
      when focus $ do
        value <- textInputValue ctx idx
        cursor <- do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          store <- getStore ctx
          pure (IM.findWithDefault (length value) (intKey wid) (storeCursor store))
        lbl <- getText (ctxNodeArena ctx) idx
        let fm = ctxFontMetrics ctx
            geom = textInputGeom fm x y w h
            fieldRect = tigFieldRect geom
            (ix, _) = widgetContentInset fm
            fieldTxt = textInputFieldText lbl value focus
            prefix = T.take (max 0 (min (T.length fieldTxt) cursor)) fieldTxt
        (pw, _) <- ctxMeasureText ctx prefix
        (_, ph) <- ctxMeasureText ctx fieldTxt
        let ty = rectY fieldRect + (rectH fieldRect - ph) / 2
            caretX = rectX fieldRect + ix + pw
            caretY = ty + 1
            caretH = max 4 (ph - 2)
        pushRect da (Rect caretX caretY 1 caretH) (styleFg style)

fillStyledRect :: DrawArena -> Bool -> Style -> Rect -> IO ()
fillStyledRect da terminal style rect =
  if terminal || styleCornerRadius style <= 0
    then pushRect da rect (styleBg style)
    else pushRoundedRect da rect (styleCornerRadius style) (styleBg style)

strokeStyledRect :: DrawArena -> Bool -> Style -> Color -> Float -> Float -> Float -> Float -> IO ()
strokeStyledRect da terminal style fillBg x y w h =
  when (not terminal && styleBorderWidth style > 0) $ do
    let bw = max 1 (styleBorderWidth style)
        col = styleBorder style
        r = styleCornerRadius style
    if r <= 0
      then strokeRect da x y w h bw col
      else do
        pushRoundedRect da (Rect x y w h) r col
        pushRoundedRect da (Rect (x + bw) (y + bw) (w - 2 * bw) (h - 2 * bw)) (max 0 (r - bw)) fillBg

clamp01 :: Float -> Float
clamp01 v = max 0 (min 1 v)

scrollLineFor :: FontMetrics -> Float
scrollLineFor fm = if isTerminalFont fm then 1 else scrollLine

scrollLine :: Float
scrollLine = 20

applyScrollOffsets :: Context -> IO ()
applyScrollOffsets ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    when (nt == NodeScrollContainer) $ do
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
    let mouse = inputMousePos inp
    mTarget <- findScrollTargetUnderMouse ctx mouse
    case mTarget of
      Nothing -> pure ()
      Just wid -> applyScrollWheelDelta ctx wid scroll

applyScrollWheelDelta :: Context -> WidgetId -> V2 -> IO ()
applyScrollWheelDelta ctx wid scroll = do
  mGeom <- scrollContainerGeom ctx wid
  case mGeom of
    Nothing -> pure ()
    Just (_idx, dir, _x, _y, w, h, pad, contentSize) -> do
      cur <- getScrollOffset ctx wid
      let step = scrollLineFor (ctxFontMetrics ctx)
      case dir of
        DirColumn -> do
          let inner = h - padT pad - padB pad
              maxOff = max 0 (contentSize - inner)
              delta = v2Y scroll * step
              newOff = max 0 (min maxOff (cur + delta))
          when (newOff /= cur) $ setScrollOffset ctx wid newOff
        DirRow -> do
          let inner = w - padL pad - padR pad
              maxOff = max 0 (contentSize - inner)
              delta = v2X scroll * step
              newOff = max 0 (min maxOff (cur + delta))
          when (newOff /= cur) $ setScrollOffset ctx wid newOff

findScrollTargetUnderMouse :: Context -> V2 -> IO (Maybe WidgetId)
findScrollTargetUnderMouse ctx mouse = queryScrollTarget ctx 0 mouse

queryScrollTarget :: Context -> NodeIdx -> V2 -> IO (Maybe WidgetId)
queryScrollTarget ctx idx mouse = do
  childHit <- walkScrollSiblings ctx idx mouse
  case childHit of
    Just wid -> pure (Just wid)
    Nothing -> scrollHitSelf ctx idx mouse

walkScrollSiblings :: Context -> NodeIdx -> V2 -> IO (Maybe WidgetId)
walkScrollSiblings ctx parent mouse = do
  fc <- getFirstChild (ctxNodeArena ctx) parent
  go fc
  where
    go ci
      | ci < 0 = pure Nothing
      | otherwise = do
          hit <- queryScrollTarget ctx ci mouse
          case hit of
            Just wid -> pure (Just wid)
            Nothing -> do
              ns <- getNextSibling (ctxNodeArena ctx) ci
              go ns

scrollHitSelf :: Context -> NodeIdx -> V2 -> IO (Maybe WidgetId)
scrollHitSelf ctx idx mouse = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  if nt /= NodeScrollContainer
    then pure Nothing
    else do
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      let viewport = Rect x y w h
      if w > 0 && h > 0 && rectContains viewport mouse
        then Just <$> getWidgetId (ctxNodeArena ctx) idx
        else pure Nothing

finalizeTabFocus :: Context -> Input -> IO ()
finalizeTabFocus ctx inp =
  when (KeyTab `elem` inputKeys inp) $ do
    focusables <- getFocusables ctx
    let ids = filter (/= WidgetId 0) focusables
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
                      txt <- getText (ctxNodeArena ctx) idx
                      let (_, opts) = selectParseOptions txt
                      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                      let fm = ctxFontMetrics ctx
                          dropRect = selectDropRect fm x y w h (length opts)
                      when (rectContains dropRect mouse) $
                        case selectDropPickIndex dropRect (selectItemH fm h) (length opts) (v2Y mouse) of
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
                      dropRect = selectDropRect fm x y w h (length opts)
                  if rectContains btnRect mouse || rectContains dropRect mouse
                    then pure True
                    else go (idx + 1)

unlessHit :: Bool -> IO () -> IO ()
unlessHit b act = when (not b) act

-- Hit-test widgets with solved layout rects so hover paint matches draw positions.
refreshHover :: Context -> Input -> IO ()
refreshHover ctx inp = do
  prevHot <- readIORef (ctxLastHotId ctx)
  writeIORef (ctxHotId ctx) (WidgetId 0)
  count <- arenaCount (ctxNodeArena ctx)
  let mouse = inputMousePos inp
      go idx = do
        nt <- getNodeType (ctxNodeArena ctx) idx
        when (isWidgetNode nt) $ do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          let rect = Rect x y w h
          when (w > 0 && h > 0 && rectContains rect mouse) $
            writeIORef (ctxHotId ctx) wid
        when (idx > 0) $ go (idx - 1)
  when (count > 0) $ go (count - 1)
  newHot <- readIORef (ctxHotId ctx)
  writeIORef (ctxLastHotId ctx) newHot
  let terminal = isTerminalFont (ctxFontMetrics ctx)
  when (prevHot /= newHot) $ do
    markDirty ctx
    unless terminal $ do
      when (hashWidgetId prevHot /= 0) $ startAnimation ctx prevHot 1 0 0.12
      when (hashWidgetId newHot /= 0) $ startAnimation ctx newHot 0 1 0.12

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
              let rect = widgetHitRect ctx nt x y w h
              if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
                then pure (Just wid)
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
                  txt <- getText (ctxNodeArena ctx) idx
                  pushMessage ctx (FrameMsg ("checkbox:" <> T.unpack (checkboxLabelText txt)))
                _ -> pure ()
        writeIORef (ctxActiveId ctx) (WidgetId 0)
        when releasedOver $
          unless (isTerminalFont (ctxFontMetrics ctx)) $
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
  when (inputMousePressed inp || inputMouseReleased inp) $ do
    prevFocus <- readIORef (ctxFocusId ctx)
    let mouse = inputMousePos inp
    count <- arenaCount (ctxNodeArena ctx)
    mFocused <- findTextInputUnderMouse ctx count mouse
    case mFocused of
      Nothing -> do
        when (prevFocus /= WidgetId 0) $ markDirty ctx
        writeIORef (ctxFocusId ctx) (WidgetId 0)
      Just wid -> do
        writeIORef (ctxFocusId ctx) wid
        when (prevFocus /= wid) $ markDirty ctx

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
              let rect = widgetHitRect ctx nt x y w h
              if rectW rect > 0 && rectH rect > 0 && rectContains rect mouse
                then pure (Just wid)
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
            terminal = isTerminalFont (ctxFontMetrics ctx)
            mark = if terminal then (if val then "[x] " else "[ ] ") else ""
        setNodeText (ctxNodeArena ctx) idx (mark <> body)
        setNodeValue (ctxNodeArena ctx) idx (if val then 1 else 0)
      NodeSlider -> do
        let val = IM.findWithDefault 0 key (storeSlider store)
        txt <- getText (ctxNodeArena ctx) idx
        let fm = ctxFontMetrics ctx
            (lbl, minV, maxV) = sliderParseRange txt
            frac = if maxV > minV then (val - minV) / (maxV - minV) else 0
            shown =
              if isTerminalFont fm
                then sliderPackTerminal lbl frac val minV maxV
                else sliderPackRange lbl minV maxV
        setNodeText (ctxNodeArena ctx) idx shown
        setNodeValue (ctxNodeArena ctx) idx frac
      NodeButton -> do
        txt <- getText (ctxNodeArena ctx) idx
        when (not (isTerminalFont (ctxFontMetrics ctx))) $
          setNodeText (ctxNodeArena ctx) idx (stripButtonBrackets txt)
      NodeTextInput -> do
        when (isTerminalFont (ctxFontMetrics ctx)) $ do
          focus <- readIORef (ctxFocusId ctx)
          txt <- getText (ctxNodeArena ctx) idx
          let value = IM.findWithDefault "" key (storeText store)
              cursor = IM.findWithDefault (length value) key (storeCursor store)
              lbl = txt
              focused = focus == wid
          setNodeText (ctxNodeArena ctx) idx (textInputTerminalText lbl value cursor focused)
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

strokeRect :: DrawArena -> Float -> Float -> Float -> Float -> Float -> Color -> IO ()
strokeRect da x y w h bw col =
  let t = max 1 bw
   in do
    pushLine da x y (x + w) y t col
    pushLine da x (y + h - t) (x + w) (y + h) t col
    pushLine da x y x (y + h) t col
    pushLine da (x + w - t) y (x + w) (y + h) t col

selectItemH :: FontMetrics -> Float -> Float
selectItemH fm rh = if isTerminalFont fm then max 1 rh else max rh 24

selectDropGap :: FontMetrics -> Float
selectDropGap fm = if isTerminalFont fm then 0 else 2

selectDropBg :: Style -> Color
selectDropBg st = styleBg st

selectDropActiveBg :: Style -> Color
selectDropActiveBg st = styleActiveBg st

selectDropHoverBg :: Style -> Color
selectDropHoverBg st = styleHoverBg st

selectDropRect :: FontMetrics -> Float -> Float -> Float -> Float -> Int -> Rect
selectDropRect fm x y w h nOpts =
  let itemH = selectItemH fm h
   in Rect x (y + h + selectDropGap fm) w (itemH * fromIntegral nOpts)

selectDropItemY :: Rect -> Float -> Int -> Float
selectDropItemY dropRect itemH i =
  rectY dropRect + itemH * fromIntegral i

selectDropPickIndex :: Rect -> Float -> Int -> Float -> Maybe Int
selectDropPickIndex dropRect itemH nOpts mouseY =
  let rel = mouseY - rectY dropRect
      innerH = itemH * fromIntegral nOpts
   in if rel < 0 || rel >= innerH
        then Nothing
        else
          Just (max 0 (min (nOpts - 1) (floor (rel / max itemH 1))))

terminalDropRow :: Int -> Int -> Int -> T.Text -> Color -> Color -> Rect -> (Rect, T.Text, Color, Color, Rect)
terminalDropRow x y w txt fg bg clip =
  (Rect (fromIntegral x) (fromIntegral y) (fromIntegral w) 1, txt, fg, bg, clip)

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

drawSelectChevron :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawSelectChevron da x y w h col = do
  let cx = selectChevronCenterX x w
      cy = y + h / 2
      sz = 3.5
  pushLine da (cx - sz) (cy - sz * 0.55) cx (cy + sz * 0.55) 1.5 col
  pushLine da cx (cy + sz * 0.55) (cx + sz) (cy - sz * 0.55) 1.5 col

scrollBarWidth :: Float
scrollBarWidth = 8

scrollBarMargin :: Float
scrollBarMargin = 3

scrollBarGeom :: FontMetrics -> (Float, Float)
scrollBarGeom fm =
  if isTerminalFont fm
    then (1, 0)
    else (scrollBarWidth, scrollBarMargin)

data ScrollBarLayout = ScrollBarLayout
  { sbTrack :: Rect
  , sbThumb :: Rect
  , sbMaxOff :: Float
  }
  deriving (Eq, Show)

scrollBarLayout ::
  FontMetrics ->
  DirTag ->
  Float ->
  Float ->
  Float ->
  Float ->
  Padding ->
  Float ->
  Float ->
  Maybe ScrollBarLayout
scrollBarLayout fm dir x y w h pad contentSize off =
  let (barW, barMargin) = scrollBarGeom fm
      minThumb = if isTerminalFont fm then barW else 16
   in case dir of
    DirColumn ->
      let innerH = h - padT pad - padB pad
          maxOff = max 0 (contentSize - innerH)
       in if maxOff <= 0
            then Nothing
            else
              let trackX = x + w - barW - barMargin
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
              let trackY = y + h - barW - barMargin
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
            Just (_idx, dir, x, y, w, h, pad, contentSize) -> do
              off <- getScrollOffset ctx wid
              let fm = ctxFontMetrics ctx
              case scrollBarLayout fm dir x y w h pad contentSize off of
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
            if nt /= NodeScrollContainer
              then go (idx + 1)
              else do
                w' <- getWidgetId (ctxNodeArena ctx) idx
                if w' /= wid
                  then go (idx + 1)
                  else do
                    dir <- getDirection (ctxNodeArena ctx) idx
                    pad <- getPadding (ctxNodeArena ctx) idx
                    contentSize <- getNodeValue (ctxNodeArena ctx) idx
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
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
    Just (_idx, dir, x, y, w, h, pad, contentSize) -> do
      off <- getScrollOffset ctx wid
      let fm = ctxFontMetrics ctx
      case scrollBarLayout fm dir x y w h pad contentSize off of
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
      trackBg = styleBg (themePanel theme)
      thumbCol = themeAccent theme
  case scrollBarLayout fm dir x y w h pad contentSize off of
    Nothing -> pure ()
    Just layout -> do
      let track = sbTrack layout
          thumb = sbThumb layout
      if terminal
        then do
          pushRect da track trackBg
          pushRect da thumb thumbCol
        else do
          let trackR = min 4 (scrollBarWidth / 2)
              thumbR = min 4 (min (rectW thumb) (rectH thumb) / 2)
          pushRoundedRect da track trackR trackBg
          pushRoundedRect da thumb thumbR thumbCol

drawSelectOverlays :: Context -> Input -> IO ()
drawSelectOverlays ctx inp = do
  let mouse = inputMousePos inp
      da = ctxDrawArena ctx
      theme = ctxTheme ctx
      fm = ctxFontMetrics ctx
      terminal = isTerminalFont fm
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
                      txt <- getText (ctxNodeArena ctx) idx
                      let (_, opts) = selectParseOptions txt
                      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                      let picked = IM.findWithDefault 0 key (storeSelect store)
                          itemH = selectItemH fm h
                          dropRect = selectDropRect fm x y w h (length opts)
                          dropStyle = themeInput theme
                          itemStyle = themeInput theme
                      fillStyledRect da False dropStyle dropRect
                      strokeStyledRect
                        da
                        False
                        dropStyle
                        (styleBg dropStyle)
                        (rectX dropRect)
                        (rectY dropRect)
                        (rectW dropRect)
                        (rectH dropRect)
                      forM_ (zip [0 ..] opts) $ \(i, _opt) -> do
                        let iy = rectY dropRect + itemH * fromIntegral i
                            itemRect = Rect (rectX dropRect) iy (rectW dropRect) itemH
                            hovered = rectContains itemRect mouse
                            bg =
                              if hovered
                                then styleHoverBg itemStyle
                                else
                                  if i == picked
                                    then styleActiveBg itemStyle
                                    else styleBg itemStyle
                        fillStyledRect da False (itemStyle {styleBg = bg}) itemRect
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
                    txt <- getText (ctxNodeArena ctx) idx
                    let (_, opts) = selectParseOptions txt
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                    let itemH = selectItemH fm h
                        dropRect = selectDropRect fm x y w h (length opts)
                        picked = IM.findWithDefault 0 key (storeSelect store)
                        itemStyle = themeInput theme
                        fg = styleFg itemStyle
                    if isTerminalFont fm
                      then do
                        let wi = max 1 (round w)
                            rx = round (rectX dropRect)
                            ry = round (rectY dropRect)
                            dropBg = selectDropBg itemStyle
                            dropActiveBg = selectDropActiveBg itemStyle
                            dropHoverBg = selectDropHoverBg itemStyle
                            hoverIdx =
                              selectDropPickIndex dropRect itemH (length opts) (v2Y mouse)
                        rest <- go (idx + 1)
                        pure
                          ( terminalSelectDropdownSpans rx ry wi opts picked hoverIdx fg dropBg dropActiveBg dropHoverBg dropRect
                              ++ rest
                          )
                      else do
                        let (ix, iy) = widgetContentInset fm
                            dropBg = styleBg itemStyle
                        itemSpans <-
                          forM (zip ([0 ..] :: [Int]) opts) $ \(i, opt) ->
                            if T.null opt
                              then pure []
                              else do
                                (tw, th) <- ctxMeasureText ctx opt
                                let itemY = selectDropItemY dropRect itemH i
                                    ty = itemY + iy
                                pure [(Rect (x + ix) ty tw th, opt, fg, dropBg, dropRect)]
                        rest <- go (idx + 1)
                        pure (concat itemSpans ++ rest)
  go 0

drawTooltipOverlays :: Context -> IO ()
drawTooltipOverlays ctx = do
  let da = ctxDrawArena ctx
      theme = ctxTheme ctx
      terminal = isTerminalFont (ctxFontMetrics ctx)
  when (not terminal) $ do
    tips <- readTooltips ctx
    let panelStyle = themePanel theme
    forM_ tips $ \(PendingTooltip rect _) ->
      fillStyledRect da False panelStyle rect

collectTooltipSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTooltipSpans ctx = do
  let fm = ctxFontMetrics ctx
      theme = ctxTheme ctx
  if isTerminalFont fm
    then pure []
    else do
      tips <- readTooltips ctx
      forM tips $ \(PendingTooltip rect txt) -> do
        let (ix, iy) = widgetContentInset fm
            fg = styleFg (themePanel theme)
            bg = styleBg (themePanel theme)
        (tw, th) <- ctxMeasureText ctx txt
        let tx = rectX rect + ix
            ty = rectY rect + iy
            textRect = Rect tx ty tw th
        pure (textRect, txt, fg, bg, rect)

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ \idx -> do
    wid <- getWidgetId (ctxNodeArena ctx) idx
    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
    when (hashWidgetId wid /= 0) $
      setPrevRect ctx wid (Rect x y w h)
