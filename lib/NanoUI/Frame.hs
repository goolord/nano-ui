module NanoUI.Frame
  ( runFrame
  , needsRedraw
  , collectTextSpans
  ) where

import Control.Monad (forM_, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , FrameMsg (..)
  , WidgetStore (..)
  , anyAnimating
  , drainMessages
  , getStore
  , intKey
  , isDirty
  , markDirty
  , pushMessage
  , setStore
  , setPrevRect
  , tickAnimations
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
  )
import NanoUI.Font
  ( FontMetrics (..)
  , checkboxBoxSize
  , checkboxLeading
  , fmLineHeight
  , isTerminalFont
  , labelContentInset
  , widgetContentInset
  )
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input (Input (..), inputChanged)
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (..)
  , arenaCount
  , getFirstChild
  , getNextSibling
  , getNodeType
  , getNodeValue
  , getPadding
  , getRect
  , getText
  , getWidgetId
  , isWidgetNode
  , NodeType (NodeButton, NodeCheckbox, NodeSlider, NodeTextInput)
  , resetNodeArena
  , setNodeText
  , setNodeValue
  )
import NanoUI.Layout.Solve (solveLayout)
import NanoUI.Monad (UI (..))
import NanoUI.WidgetText
  ( checkboxLabelText
  , sliderLabelText
  , sliderPackRange
  , sliderParseRange
  , sliderValueText
  , textInputDisplayText
  )
import NanoUI.Widgets (sliderText, textInputText)
import NanoUI.Style (Padding (..), Style (..), themeAccent, themeButton, themeInput, themePanel, themeSeparator)
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), colorRGBA, rectContains)

runFrame :: Context -> Input -> UI a -> IO (a, [FrameMsg], DrawData, Bool)
runFrame ctx inp ui = do
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxHotId ctx) (WidgetId 0)
  result <- unUI ui ctx inp
  -- Terminal sliders embed the bar in node text; sync before measure so width is correct.
  syncWidgetLabels ctx
  let Size w h = inputWindowSize inp
  solveLayout (ctxNodeArena ctx) (ctxFontMetrics ctx) (ctxMeasureText ctx) w h
  finalizePointerRelease ctx inp
  finalizeTextInputFocus ctx inp
  syncWidgetLabels ctx
  refreshHover ctx inp
  tickAnimations ctx (inputDeltaTime inp)
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
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

collectTextSpans :: Context -> IO [(Rect, T.Text, Color, Color)]
collectTextSpans ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  if count > 0
    then walkChildrenSpans ctx 0
    else pure []

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
          (tw, th) <- ctxMeasureText ctx txt
          pure [(Rect (x + ix) (y + iy) tw th, txt, styleFg style, styleBg style)]
    else
      if isWidgetNode nt
        then widgetTextSpans ctx nt idx x y w h
        else pure []

displayText :: Context -> NodeType -> NodeIdx -> IO T.Text
displayText ctx nt idx = do
  txt <- getText (ctxNodeArena ctx) idx
  let terminal = isTerminalFont (ctxFontMetrics ctx)
  if terminal
    then pure txt
    else
      case nt of
        NodeCheckbox -> pure (checkboxLabelText txt)
        NodeTextInput -> do
          value <- textInputValue ctx idx
          focused <- textInputFocused ctx idx
          pure (textInputDisplayText (textInputLabel txt) value focused)
        NodeSlider -> pure (sliderLabelText txt)
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
      txt <- getText (ctxNodeArena ctx) idx
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
      (ix, iy) = widgetContentInset fm
  case nt of
    NodeButton -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      pure [(txt, x + (w - tw) / 2, y + (h - th) / 2, tw, th)]
    NodeCheckbox -> do
      txt <- displayText ctx nt idx
      (tw, th) <- ctxMeasureText ctx txt
      let tx = x + ix + checkboxLeading fm
          ty = y + (h - th) / 2
      pure [(txt, tx, ty, tw, th)]
    NodeSlider -> do
      lbl <- displayText ctx nt idx
      val <- sliderValue ctx idx
      let valTxt = sliderValueText val
      (lw, lh) <- ctxMeasureText ctx lbl
      (vw, vh) <- ctxMeasureText ctx valTxt
      let ty = y + iy
      pure
        [ (lbl, x + ix, ty, lw, lh)
        , (valTxt, x + w - ix - vw, ty, vw, vh)
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
  let theme = ctxTheme ctx
      base =
        case nt of
          NodeTextInput -> themeInput theme
          NodeSlider ->
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
      bg
        | widKey == hashWidgetId active = styleActiveBg base
        | widKey == hashWidgetId hot = styleHoverBg base
        | otherwise = styleBg base
  pure base {styleBg = bg}

walkChildrenSpans :: Context -> NodeIdx -> IO [(Rect, T.Text, Color, Color)]
walkChildrenSpans ctx idx = do
  here <- collectNodeTextSpans ctx idx
  fc <- getFirstChild (ctxNodeArena ctx) idx
  rest <- go fc
  pure (here ++ rest)
  where
    go ci =
      if ci < 0
        then pure []
        else do
          ns <- getNextSibling (ctxNodeArena ctx) ci
          child <- walkChildrenSpans ctx ci
          rest <- go ns
          pure (child ++ rest)

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
    NodeSpacer -> pure ()
    _ -> do
      style <- widgetVisualStyle ctx nt idx
      value <- getNodeValue (ctxNodeArena ctx) idx
      let opaqueBg =
            isTerminalFont fm
              || (nt /= NodeCheckbox && nt /= NodeSlider)
      when opaqueBg $ fillStyledRect da terminal style rect
      when (not terminal) $ do
        when opaqueBg $ strokeStyledRect da terminal style (styleBg style) x y w h
        when (nt == NodeCheckbox) $
          drawCheckbox da fm style x y h value (themeAccent theme)
        when (nt == NodeSlider) $ do
          let trackH = max 4 (h * 0.18)
              trackY = y + h - trackH - 2
              trackR = trackH / 2
              trackRect = Rect x trackY w trackH
          pushRoundedRect da trackRect trackR (styleBg (themeInput theme))
          pushRoundedRect da (Rect x trackY (w * clamp01 value) trackH) trackR (themeAccent theme)
        when (nt == NodeTextInput) $
          drawTextInputCaret da ctx idx x y w h style
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
  when (value >= 0.5) $
    pushRoundedRect da (Rect (bx + inset) (by + inset) (box - 2 * inset) (box - 2 * inset)) (max 1 (r - 1)) accent

drawTextInputCaret :: DrawArena -> Context -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextInputCaret da ctx idx x y _w _h style = do
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
        txt <- getText (ctxNodeArena ctx) idx
        let lbl = textInputLabel txt
            shown = T.pack value
            prefix = lbl <> ": " <> T.take (max 0 (min (T.length shown) cursor)) shown
        (pw, _) <- ctxMeasureText ctx prefix
        let fm = ctxFontMetrics ctx
            (ix, iy) = widgetContentInset fm
            caretX = x + ix + pw
            caretY = y + iy + 1
            caretH = max 4 (fmLineHeight fm - 2)
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

-- Hit-test widgets with solved layout rects so hover paint matches draw positions.
refreshHover :: Context -> Input -> IO ()
refreshHover ctx inp = do
  prevHot <- readIORef (ctxHotId ctx)
  writeIORef (ctxHotId ctx) (WidgetId 0)
  count <- arenaCount (ctxNodeArena ctx)
  let mouse = inputMousePos inp
  forM_ [0 .. count - 1] $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    when (isWidgetNode nt) $ do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
      let rect = Rect x y w h
      when (w > 0 && h > 0 && rectContains rect mouse) $
        writeIORef (ctxHotId ctx) wid
  newHot <- readIORef (ctxHotId ctx)
  when (prevHot /= newHot) $ markDirty ctx

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
              let rect = Rect x y w h
              if w > 0 && h > 0 && rectContains rect mouse
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
        frac <- getNodeValue (ctxNodeArena ctx) idx
        txt <- getText (ctxNodeArena ctx) idx
        let fm = ctxFontMetrics ctx
            (lbl, minV, maxV) = sliderParseRange txt
            shown =
              if isTerminalFont fm
                then sliderText lbl frac val
                else sliderPackRange lbl minV maxV
        setNodeText (ctxNodeArena ctx) idx shown
      NodeButton -> do
        txt <- getText (ctxNodeArena ctx) idx
        when (not (isTerminalFont (ctxFontMetrics ctx))) $
          setNodeText (ctxNodeArena ctx) idx (stripButtonBrackets txt)
      NodeTextInput -> do
        let value = IM.findWithDefault "" key (storeText store)
            cursor = IM.findWithDefault (length value) key (storeCursor store)
        focus <- readIORef (ctxFocusId ctx)
        txt <- getText (ctxNodeArena ctx) idx
        let lbl = textInputLabel txt
            terminal = isTerminalFont (ctxFontMetrics ctx)
            focused = focus == wid
            shown =
              if terminal
                then textInputText lbl value cursor focused
                else textInputDisplayText lbl value focused
        setNodeText (ctxNodeArena ctx) idx shown
      _ -> pure ()

textInputLabel :: T.Text -> T.Text
textInputLabel txt =
  let (lbl, _) = T.breakOn ": " txt
   in if T.null lbl then txt else lbl

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

updatePrevRects :: Context -> IO ()
updatePrevRects ctx = do
  count <- arenaCount (ctxNodeArena ctx)
  forM_ [0 .. count - 1] $ \idx -> do
    wid <- getWidgetId (ctxNodeArena ctx) idx
    (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
    when (hashWidgetId wid /= 0) $
      setPrevRect ctx wid (Rect x y w h)
