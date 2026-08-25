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
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.List (findIndex)
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , WidgetStore (..)
  , TextInputMenu (..)
  , TextInputDrag (..)
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
  , ctxClipboardGet
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
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputChanged, inputKeys)
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
import NanoUI.Style (Padding (..), Style (..), Theme (..), themeAccent, themeButton, themeInput, themePanel, themeSeparator, themeWindow)
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), colorRGBA, rectContains, rectH, rectIntersect, rectOverlapArea, rectW, rectX, rectY, sliderTrackRect, v2X, v2Y)

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
  finalizeTextInputMouse ctx inp
  closeTextInputMenuOnOutsideClick ctx inp
  openTextInputMenu ctx inp
  finalizeTextInputMenuPick ctx inp
  closeTextInputMenuOnEscape ctx inp
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
  drawTextInputMenuOverlays ctx inp
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
  menu <- collectTextInputMenuSpans ctx inp
  tips <- collectTooltipSpans ctx
  pure (drops ++ menu ++ tips)

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
            geom = textInputGeom fm x y w h
            fieldRect = tigFieldRect geom
            (ix, _) = widgetContentInset fm
            theme = ctxTheme ctx
            accent = themeAccent theme
            selBg = lerpColor accent (styleBg style) 0.55
        when hasSel $ do
          (wLo, _) <- ctxMeasureText ctx (T.pack (take selLo value))
          (wHi, _) <- ctxMeasureText ctx (T.pack (take selHi value))
          (_, ph) <- ctxMeasureText ctx (T.pack value)
          let ty = rectY fieldRect + (rectH fieldRect - ph) / 2
              selX = rectX fieldRect + ix + wLo
              selW = max 1 (wHi - wLo)
              selH = max 4 ph
          pushRect da (Rect selX ty selW selH) selBg
        let fieldTxt = textInputFieldText lbl value focus
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
textInputMenuOuterPad = 4

textInputMenuItemPadX :: Float
textInputMenuItemPadX = 10

textInputMenuSepH :: FontMetrics -> Float
textInputMenuSepH fm = if isTerminalFont fm then 1 else 9

textInputMenuCornerR :: Float
textInputMenuCornerR = 8

textInputMenuShadowOff :: Float
textInputMenuShadowOff = 3

textInputMenuMinW :: Float
textInputMenuMinW = 148

textInputMenuItemH :: FontMetrics -> Float
textInputMenuItemH fm = if isTerminalFont fm then 1 else 28

textInputMenuRowH :: FontMetrics -> TextInputMenuRow -> Float
textInputMenuRowH fm = \case
  TextInputMenuSep -> textInputMenuSepH fm
  TextInputMenuItem {} -> textInputMenuItemH fm

textInputMenuContentH :: FontMetrics -> Float
textInputMenuContentH fm = sum (map (textInputMenuRowH fm) textInputMenuRows)

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

textInputMenuStyle :: Theme -> Style
textInputMenuStyle = overlayMenuStyle

textInputMenuWidth :: Context -> IO Float
textInputMenuWidth ctx = do
  let labels = [lbl | TextInputMenuItem _ lbl <- textInputMenuRows]
  ws <- mapM (ctxMeasureText ctx) labels
  let maxTw = maximum (map fst ws)
  pure (max textInputMenuMinW (maxTw + 2 * textInputMenuItemPadX + 2 * textInputMenuOuterPad))

textInputMenuRectAt :: FontMetrics -> Float -> Float -> Float -> Size -> Rect
textInputMenuRectAt fm x y menuW win =
  let h = 2 * textInputMenuOuterPad + textInputMenuContentH fm
      Size ww wh = win
      rx = max 0 (min x (ww - menuW))
      ry = max 0 (min y (wh - h))
   in Rect rx ry menuW h

textInputMenuContentRect :: Rect -> FontMetrics -> Rect
textInputMenuContentRect menuRect fm =
  let pad = textInputMenuOuterPad
   in Rect
        (rectX menuRect + pad)
        (rectY menuRect + pad)
        (rectW menuRect - 2 * pad)
        (textInputMenuContentH fm)

textInputMenuLayout :: FontMetrics -> [(TextInputMenuRow, Float, Float)]
textInputMenuLayout fm = go 0 textInputMenuRows
  where
    go _ [] = []
    go y (entry : rest) =
      let h = textInputMenuRowH fm entry
       in (entry, y, h) : go (y + h) rest

textInputMenuPickAction :: Rect -> FontMetrics -> V2 -> Maybe Int
textInputMenuPickAction menuRect fm mouse =
  let content = textInputMenuContentRect menuRect fm
      relY = v2Y mouse - rectY content
   in if relY < 0 || relY >= textInputMenuContentH fm
        then Nothing
        else pick relY (textInputMenuLayout fm)
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
            let menuRect = textInputMenuRectAt fm (v2X mouse) (v2Y mouse) menuW (inputWindowSize inp)
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
              case textInputMenuPickAction rect fm mouse of
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
          case textInputMenuPickAction rect fm mouse of
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
      let fm = ctxFontMetrics ctx
      when (not (isTerminalFont fm)) $ do
        let da = ctxDrawArena ctx
            theme = ctxTheme ctx
            mouse = inputMousePos inp
            menuRect = textInputMenuRect menu
            menuStyle = textInputMenuStyle theme
            content = textInputMenuContentRect menuRect fm
            r = styleCornerRadius menuStyle
            wid = textInputMenuWidget menu
        pushMenuShadow da menuRect r
        fillStyledRect da False menuStyle menuRect
        strokeStyledRect
          da
          False
          menuStyle
          (styleBg menuStyle)
          (rectX menuRect)
          (rectY menuRect)
          (rectW menuRect)
          (rectH menuRect)
        forM_ (textInputMenuLayout fm) $ \(entry, relY, h) -> do
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
          content = textInputMenuContentRect menuRect fm
          wid = textInputMenuWidget menu
      if isTerminalFont fm
        then terminalTextInputMenuSpans ctx menuRect content fm menuStyle mouse wid
        else do
          let (ix, _) = widgetContentInset fm
              bg = styleBg menuStyle
          spans <-
            forM (textInputMenuLayout fm) $ \(entry, relY, h) -> do
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
                      ty = rectY content + relY + (h - th) / 2
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
terminalTextInputMenuSpans ctx menuRect content fm menuStyle mouse wid = do
  let rx :: Int
      rx = round (rectX menuRect)
      wi :: Int
      wi = max 1 (round (rectW menuRect))
      innerW = max 0 (wi - 1)
      dropBg = styleBg menuStyle
      dropHoverBg = styleHoverBg menuStyle
      sepFg = themeSeparator (ctxTheme ctx)
  rows <-
    forM (textInputMenuLayout fm) $ \(entry, relY, _h) -> do
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
              rowRect = Rect (rectX menuRect) (rectY content + relY) (rectW menuRect) (textInputMenuItemH fm)
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
                      field = tigFieldRect (textInputGeom fm x y w h)
                      (ix, _) = widgetContentInset fm
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
selectItemH fm rh = if isTerminalFont fm then max 1 rh else 28

selectDropGap :: FontMetrics -> Float
selectDropGap fm = if isTerminalFont fm then 0 else 4

selectDropOuterPad :: FontMetrics -> Float
selectDropOuterPad fm = if isTerminalFont fm then 0 else textInputMenuOuterPad

selectDropBg :: Style -> Color
selectDropBg st = styleBg st

selectDropActiveBg :: Style -> Color
selectDropActiveBg st = styleActiveBg st

selectDropHoverBg :: Style -> Color
selectDropHoverBg st = styleHoverBg st

selectDropRect :: FontMetrics -> Float -> Float -> Float -> Float -> Int -> Rect
selectDropRect fm x y w h nOpts =
  let itemH = selectItemH fm h
      pad = selectDropOuterPad fm
   in Rect x (y + h + selectDropGap fm) w (itemH * fromIntegral nOpts + 2 * pad)

selectDropItemY :: FontMetrics -> Rect -> Float -> Int -> Float
selectDropItemY fm dropRect itemH i =
  rectY dropRect + selectDropOuterPad fm + itemH * fromIntegral i

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
                          dropStyle = overlayMenuStyle theme
                          r = styleCornerRadius dropStyle
                      pushMenuShadow da dropRect r
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
                      forM_ (zip ([0 ..] :: [Int]) opts) $ \(i, _opt) -> do
                        let iy = selectDropItemY fm dropRect itemH i
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
                    txt <- getText (ctxNodeArena ctx) idx
                    let (_, opts) = selectParseOptions txt
                    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                    let itemH = selectItemH fm h
                        dropRect = selectDropRect fm x y w h (length opts)
                        picked = IM.findWithDefault 0 key (storeSelect store)
                        dropStyle = overlayMenuStyle theme
                        fg = styleFg dropStyle
                    if isTerminalFont fm
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
                        let (ix, _) = widgetContentInset fm
                            dropBg = styleBg dropStyle
                        itemSpans <-
                          forM (zip ([0 ..] :: [Int]) opts) $ \(i, opt) ->
                            if T.null opt
                              then pure []
                              else do
                                (tw, th) <- ctxMeasureText ctx opt
                                let itemY = selectDropItemY fm dropRect itemH i
                                    itemRect = Rect (rectX dropRect) itemY (rectW dropRect) itemH
                                    hovered = rectContains itemRect mouse
                                    rowBg
                                      | hovered = styleHoverBg dropStyle
                                      | i == picked = styleActiveBg dropStyle
                                      | otherwise = dropBg
                                    ty = itemY + (itemH - th) / 2
                                    tx = rectX dropRect + textInputMenuItemPadX + ix
                                pure [(Rect tx ty tw th, opt, fg, rowBg, dropRect)]
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
