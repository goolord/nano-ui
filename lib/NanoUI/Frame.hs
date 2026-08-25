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
  , pushText
  , resetDrawArena
  )
import NanoUI.Font (isTerminalFont, labelContentInset, widgetContentInset)
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
  , getRect
  , getText
  , getWidgetId
  , isWidgetNode
  , NodeType (NodeCheckbox, NodeSlider, NodeTextInput)
  , resetNodeArena
  , setNodeText
  , setNodeValue
  )
import NanoUI.Layout.Solve (solveLayout)
import NanoUI.Monad (UI (..))
import NanoUI.Widgets (sliderText, textInputText)
import NanoUI.Style (Style (..), themeAccent, themeButton, themeInput, themePanel, themeSeparator)
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), rectContains)

runFrame :: Context -> Input -> UI a -> IO (a, [FrameMsg], DrawData, Bool)
runFrame ctx inp ui = do
  resetNodeArena (ctxNodeArena ctx)
  resetDrawArena (ctxDrawArena ctx)
  writeIORef (ctxContainerStack ctx) []
  writeIORef (ctxHotId ctx) (WidgetId 0)
  result <- unUI ui ctx inp
  let Size w h = inputWindowSize inp
  solveLayout (ctxNodeArena ctx) (ctxFontMetrics ctx) w h
  finalizePointerRelease ctx inp
  finalizeTextInputFocus ctx inp
  syncWidgetLabels ctx
  refreshHover ctx inp
  beginLayer (ctxDrawArena ctx) LayerBackground
  lowerShapes ctx
  drawData <- finishDraw (ctxDrawArena ctx)
  updatePrevRects ctx
  msgs <- drainMessages ctx
  tickAnimations ctx (inputDeltaTime inp)
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
      let style = themePanel theme
          (ix, iy) = labelContentInset fm
          rect = Rect (x + ix) (y + iy) w h
      pure [(rect, txt, styleFg style, styleBg style)]
    else
      if isWidgetNode nt
        then do
          txt <- getText (ctxNodeArena ctx) idx
          if T.null txt
            then pure []
            else do
              style <- widgetVisualStyle ctx nt idx
              let (ix, iy) = widgetContentInset fm
                  rect = Rect (x + ix) (y + iy) w h
                  textSpan = [(rect, txt, styleFg style, styleBg style)]
                  terminal = isTerminalFont fm
                  fullBg =
                    if terminal
                      then
                        let fill = T.replicate (max 1 (round w)) (T.singleton ' ')
                         in [(Rect x y w h, fill, styleFg style, styleBg style)]
                      else []
              pure (fullBg ++ textSpan)
        else pure []

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
          NodeSlider -> themeInput theme
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
      let style = themePanel theme
      pushRect da rect (styleBg style)
      strokeRect da x y w h (styleBorderWidth style) (styleBorder style)
      walkChildren ctx idx
    NodeText -> do
      txt <- getText (ctxNodeArena ctx) idx
      let style = themePanel theme
          (ix, iy) = labelContentInset fm
      pushRect da rect (styleBg style)
      when (not terminal) $
        pushText da fm (x + ix) (y + iy) txt (styleFg style)
    NodeSeparator ->
      pushRect da rect (themeSeparator theme)
    NodeSpacer -> pure ()
    _ -> do
      txt <- getText (ctxNodeArena ctx) idx
      style <- widgetVisualStyle ctx nt idx
      value <- getNodeValue (ctxNodeArena ctx) idx
      let (ix, iy) = widgetContentInset fm
      pushRect da rect (styleBg style)
      -- A one-cell-tall widget has no room for a frame, and the slider bar is
      -- already spelled out in its label text.
      when (not terminal) $ do
        strokeRect da x y w h (styleBorderWidth style) (styleBorder style)
        when (nt == NodeSlider) $
          pushRect da (Rect x (y + h - 1) (w * clamp01 value) 1) (themeAccent theme)
        when (not (T.null txt)) $
          pushText da fm (x + ix) (y + iy) txt (styleFg style)

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
                  pushMessage ctx (FrameMsg ("checkbox:" <> checkboxBody txt))
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
        let body = checkboxBody txt
            val = IM.findWithDefault False key (storeCheckbox store)
            mark = if val then "[x] " else "[ ] "
        setNodeText (ctxNodeArena ctx) idx (mark <> T.pack body)
        setNodeValue (ctxNodeArena ctx) idx (if val then 1 else 0)
      NodeSlider -> do
        let val = IM.findWithDefault 0 key (storeSlider store)
        frac <- getNodeValue (ctxNodeArena ctx) idx
        txt <- getText (ctxNodeArena ctx) idx
        let lbl = T.stripEnd $ T.takeWhile (/= '[') txt
        setNodeText (ctxNodeArena ctx) idx (sliderText lbl frac val)
      NodeTextInput -> do
        let value = IM.findWithDefault "" key (storeText store)
            cursor = IM.findWithDefault (length value) key (storeCursor store)
        focus <- readIORef (ctxFocusId ctx)
        txt <- getText (ctxNodeArena ctx) idx
        let lbl = textInputLabel txt
            focused = focus == wid
        setNodeText (ctxNodeArena ctx) idx (textInputText lbl value cursor focused)
      _ -> pure ()

checkboxBody :: T.Text -> String
checkboxBody txt =
  if T.isPrefixOf "[x] " txt
    then T.unpack (T.drop 4 txt)
    else
      if T.isPrefixOf "[ ] " txt
        then T.unpack (T.drop 4 txt)
        else T.unpack txt

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
