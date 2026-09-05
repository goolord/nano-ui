{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.TextEdit
  ( -- * Menu types and layout
    TextEditMenuRow (..)
  , textEditMenuRows
  , textCharAtX
  , textWordBounds
  , textEditMenuRect
  , textEditMenuRectAt
  , textEditMenuWidth
  , textEditMenuContentRect
  , textEditMenuLayout
  , textFieldMenuRect
  , openTextEditMenu
  , textFieldWidgetAtMouse
  , finalizeTextEditMenuPick
  , closeTextEditMenuOnOutsideClick
  , closeTextEditMenuOnEscape
  , drawTextEditMenuOverlays
  , collectTextEditMenuSpans
  , textEditMenuCursorKind
  , normalizeTextFieldClicks
    -- * Text geometry and char lookup
  , TextInputGeom (..)
  , textInputGeom
  , textInputFieldTextClip
  , tagTextInputClippedSpans
  , textInputGeomForWidget
  , syncTextInputScroll
    -- * Caret and selection drawing primitives
  , drawTextCaret
  , drawTextSelectionLine
  , drawTextInputCaret
  , drawTextInputSelection
  , drawTextAreaSelection
  , drawTextAreaContent
    -- * Selection & interaction
  , applyTextInputClick
  , applyTextInputDrag
  , updateTextInputSelection
  , collapseTextFieldSelection
  , collapseTextInputSelection
  , collapseTextAreaSelection
  , applyTextFieldMenuAction
  , textFieldMenuActionEnabled
    -- * Text area geometry and interaction
  , TextAreaGeom (..)
  , TextAreaHit (..)
  , textAreaGeom
  , textAreaFieldClip
  , textAreaFocused
  , textAreaValue
  , loadTextAreaStateAt
  , syncTextAreaViewport
  , textAreaHitForWidget
  , textAreaCursorAt
  , applyTextAreaClick
  , applyTextAreaDrag
  , updateTextAreaSelection
  , finalizeTextAreaMouse
  , finalizeTextFieldMouse
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , TextFieldClickCell (..)
  , TextInputDrag (..)
  , TextInputMenu (..)
  , WidgetStore (..)
  , getStore
  , getTextFieldClickCell
  , getTextInputDrag
  , getTextInputMenu
  , intKey
  , isDisabled
  , markDirty
  , markEscapeConsumed
  , setStore
  , setTextFieldClickCell
  , setTextInputDrag
  , setTextInputMenu
  , slotAnchor
  , slotCursor
  , slotKey
  )
import NanoUI.Draw (DrawArena, pushRect, pushRoundedRect, pushText, withClip)
import NanoUI.Font
  ( FontMetrics
  , centeredTextY
  , fmLineHeight
  , layoutLineHeight
  , textDisplayWidth
  , textIndexAtX
  , widgetContentInset
  )
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , overlayMenuStyle
  , padDropText
  , pushMenuShadow
  , strokeStyledRect
  , textInputFocused
  , textInputMenuItemPadX
  , textInputMenuOuterPad
  , textInputValue
  )
import NanoUI.Frame.Hit (findNodeByWidgetId, nodeClippedHit, overlayHitAllowed, widgetOverlayAllowed)
import NanoUI.Frame.Scroll.Geometry (padTextClipRect)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , UiCursorKind (..)
  , inputKeys
  , inputKeysElem
  , inputMouseClicks
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  , inputMouseRightPressed
  , inputWindowSize
  )
import NanoUI.Layout.Arena
  ( NodeIdx
  , NodeType (NodeTextArea, NodeTextInput)
  , findNodeRevM
  , getNodeType
  , getRect
  , getText
  , getWidgetId
  )
import NanoUI.Store (slotTextAreaCol, slotTextAreaRow, slotTextAreaViewport, slotTextInputScroll)
import NanoUI.Style (Style (..), Theme (..), styleBg, styleFg, themeAccent, themeSeparator)
import NanoUI.Types
  ( Color (..)
  , HostProfile
  , Rect (..)
  , Size (..)
  , V2 (..)
  , isCellHost
  , lerpColor
  , rectContains
  , rectH
  , rectIntersect
  , rectOverlapArea
  , rectW
  , rectX
  , rectY
  , v2X
  , v2Y
  )
import NanoUI.WidgetText (textInputFieldHeight, textInputFieldText, textInputLabelGap)
import NanoUI.Widgets.TextArea
  ( TextAreaState (..)
  , applyTextAreaMenuAction
  , loadTextAreaState
  , saveTextAreaState
  )
import qualified NanoUI.Widgets.TextArea as TA
import qualified NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextCommon
  ( menuActionEnabled
  , selectionBgColor
  , selectionCaretGeom
  , textSelectionForClick
  , textSelectionForDrag
  , textWordBounds
  )
import NanoUI.Widgets.TextInput (applyTextInputMenuAction)

textCharAtX :: Context -> Text -> Float -> Float -> IO Int
textCharAtX ctx text startX mouseX =
  let fm = ctxFontMetrics ctx
   in pure (textIndexAtX (ctxHostProfile ctx) fm text (max 0 (mouseX - startX)))

data TextEditMenuRow
  = TextEditMenuSep
  | TextEditMenuItem Int T.Text
  deriving (Eq, Show)

textEditMenuRows :: [TextEditMenuRow]
textEditMenuRows =
  [ TextEditMenuItem 0 "Cut"
  , TextEditMenuItem 1 "Copy"
  , TextEditMenuSep
  , TextEditMenuItem 2 "Paste"
  , TextEditMenuSep
  , TextEditMenuItem 3 "Select All"
  ]

textEditMenuSepH :: HostProfile -> Float
textEditMenuSepH host = if isCellHost host then 1 else 9

textEditMenuMinW :: Float
textEditMenuMinW = 148

textEditMenuItemH :: HostProfile -> Float
textEditMenuItemH host = if isCellHost host then 1 else 28

textEditMenuRowH :: HostProfile -> TextEditMenuRow -> Float
textEditMenuRowH host = \case
  TextEditMenuSep -> textEditMenuSepH host
  TextEditMenuItem {} -> textEditMenuItemH host

textEditMenuContentH :: HostProfile -> Float
textEditMenuContentH host = sum (map (textEditMenuRowH host) textEditMenuRows)

textEditMenuStyle :: Theme -> Style
textEditMenuStyle = overlayMenuStyle

textEditMenuWidth :: Context -> IO Float
textEditMenuWidth ctx = do
  let labels = [lbl | TextEditMenuItem _ lbl <- textEditMenuRows]
  ws <- mapM (ctxMeasureText ctx) labels
  let maxTw = maximum (map fst ws)
  pure (max textEditMenuMinW (maxTw + 2 * textInputMenuItemPadX + 2 * textInputMenuOuterPad))

textEditMenuRectAt :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Size -> Rect
textEditMenuRectAt host _fm x y menuW win =
  let h = 2 * textInputMenuOuterPad + textEditMenuContentH host
      Size ww wh = win
      rx = max 0 (min x (ww - menuW))
      ry = max 0 (min y (wh - h))
   in Rect rx ry menuW h

textEditMenuRect :: TextInputMenu -> Rect
textEditMenuRect = textInputMenuRect

textEditMenuContentRect :: HostProfile -> Rect -> FontMetrics -> Rect
textEditMenuContentRect host menuRect _fm =
  let pad = textInputMenuOuterPad
   in Rect
        (rectX menuRect + pad)
        (rectY menuRect + pad)
        (rectW menuRect - 2 * pad)
        (textEditMenuContentH host)

textEditMenuLayout :: HostProfile -> [(TextEditMenuRow, Float, Float)]
textEditMenuLayout host = go 0 textEditMenuRows
  where
    go _ [] = []
    go y (entry : rest) =
      let h = textEditMenuRowH host entry
       in (entry, y, h) : go (y + h) rest

textEditMenuPickAction :: HostProfile -> Rect -> FontMetrics -> V2 -> Maybe Int
textEditMenuPickAction host menuRect fm mouse =
  let content = textEditMenuContentRect host menuRect fm
      relY = v2Y mouse - rectY content
   in if relY < 0 || relY >= textEditMenuContentH host
        then Nothing
        else pick relY (textEditMenuLayout host)
  where
    pick _ [] = Nothing
    pick y ((TextEditMenuSep, _, h) : rest)
      | y < h = Nothing
      | otherwise = pick (y - h) rest
    pick y ((TextEditMenuItem action _, _, h) : rest)
      | y < h = Just action
      | otherwise = pick (y - h) rest

textEditMenuItemFg :: Style -> Bool -> Color
textEditMenuItemFg style enabled =
  if enabled
    then styleFg style
    else lerpColor (styleFg style) (styleBg style) 0.55

-- Same box as `textInputGeom` / `textAreaGeom` field rects.
textFieldRectAt :: Context -> NodeIdx -> IO Rect
textFieldRectAt ctx idx = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
  let host = ctxHostProfile ctx
      fm = ctxFontMetrics ctx
      labelH = layoutLineHeight host fm
      gap = textInputLabelGap fm
      fieldH =
        if nt == NodeTextInput
          then textInputFieldHeight fm
          else max 0 (h - labelH - gap)
  if h < labelH + gap + (if nt == NodeTextInput then fieldH else 1)
    then pure (Rect x y w h)
    else pure (Rect x (y + labelH + gap) w fieldH)

textFieldMenuRect :: Context -> WidgetId -> IO (Maybe Rect)
textFieldMenuRect ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if nt /= NodeTextInput && nt /= NodeTextArea
        then pure Nothing
        else Just <$> textFieldRectAt ctx idx

openTextEditMenu :: Context -> Input -> IO ()
openTextEditMenu ctx inp =
  when (inputMouseRightPressed inp) $ do
    let mouse = inputMousePos inp
    mWid <- textFieldWidgetAtMouse ctx mouse
    case mWid of
      Nothing -> pure ()
      Just wid -> do
        writeIORef (ctxFocusId ctx) wid
        fm <- pure (ctxFontMetrics ctx)
        menuW <- textEditMenuWidth ctx
        let menuRect = textEditMenuRectAt (ctxHostProfile ctx) fm (v2X mouse) (v2Y mouse) menuW (inputWindowSize inp)
        setTextInputMenu ctx (Just (TextInputMenu wid menuRect))
        markDirty ctx

textFieldWidgetAtMouse :: Context -> V2 -> IO (Maybe WidgetId)
textFieldWidgetAtMouse ctx mouse = do
  mIdx <-
    findNodeRevM (ctxNodeArena ctx) $ \idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if nt /= NodeTextInput && nt /= NodeTextArea
        then pure False
        else do
          wid <- getWidgetId (ctxNodeArena ctx) idx
          disabled <- isDisabled ctx wid
          if disabled
            then pure False
            else do
              field <- textFieldRectAt ctx idx
              hit <- nodeClippedHit ctx idx field mouse
              if not hit
                then pure False
                else overlayHitAllowed ctx idx mouse
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> Just <$> getWidgetId (ctxNodeArena ctx) idx

finalizeTextEditMenuPick :: Context -> Input -> IO ()
finalizeTextEditMenuPick ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- getTextInputMenu ctx
    case mMenu of
      Nothing -> pure ()
      Just menu ->
        let mouse = inputMousePos inp
            rect = textEditMenuRect menu
         in when (rectContains rect mouse) $ do
              let fm = ctxFontMetrics ctx
              case textEditMenuPickAction (ctxHostProfile ctx) rect fm mouse of
                Nothing -> setTextInputMenu ctx Nothing
                Just idx -> do
                  enabled <- textFieldMenuActionEnabled ctx (textInputMenuWidget menu) idx
                  if enabled
                    then applyTextFieldMenuAction ctx (textInputMenuWidget menu) idx
                    else do
                      setTextInputMenu ctx Nothing
                      markDirty ctx

closeTextEditMenuOnOutsideClick :: Context -> Input -> IO ()
closeTextEditMenuOnOutsideClick ctx inp =
  when (inputMousePressed inp || inputMouseRightPressed inp) $ do
    mMenu <- getTextInputMenu ctx
    case mMenu of
      Nothing -> pure ()
      Just menu -> do
        let mouse = inputMousePos inp
        unless (rectContains (textEditMenuRect menu) mouse) $
          setTextInputMenu ctx Nothing

closeTextEditMenuOnEscape :: Context -> Input -> IO ()
closeTextEditMenuOnEscape ctx inp =
  when (inputKeysElem KeyEscape (inputKeys inp)) $
    getTextInputMenu ctx >>= \case
      Nothing -> pure ()
      Just _ -> do
        setTextInputMenu ctx Nothing
        markEscapeConsumed ctx
        markDirty ctx

textEditMenuCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textEditMenuCursorKind ctx inp = do
  mMenu <- getTextInputMenu ctx
  case mMenu of
    Nothing -> pure Nothing
    Just menu -> do
      let mouse = inputMousePos inp
          rect = textEditMenuRect menu
          fm = ctxFontMetrics ctx
      if not (rectContains rect mouse)
        then pure Nothing
        else
          case textEditMenuPickAction (ctxHostProfile ctx) rect fm mouse of
            Nothing -> pure Nothing
            Just idx -> do
              enabled <- textFieldMenuActionEnabled ctx (textInputMenuWidget menu) idx
              pure (if enabled then Just UiCursorPointer else Just UiCursorDefault)

drawTextEditMenuOverlays :: Context -> Input -> IO ()
drawTextEditMenuOverlays ctx inp = do
  mMenu <- getTextInputMenu ctx
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
              menuRect = textEditMenuRect menu
              menuStyle = textEditMenuStyle theme
              content = textEditMenuContentRect (ctxHostProfile ctx) menuRect fm
              wid = textInputMenuWidget menu
          pushMenuShadow da menuRect (styleCornerRadius menuStyle)
          fillStyledRect da False menuStyle menuRect
          strokeStyledRect
            da
            False
            menuStyle
            (rectX menuRect)
            (rectY menuRect)
            (rectW menuRect)
            (rectH menuRect)
          forM_ (textEditMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, h) -> do
            let rowRect = Rect (rectX menuRect) (rectY content + relY) (rectW menuRect) h
            case entry of
              TextEditMenuSep -> do
                let sepCol = themeSeparator theme
                    margin = textInputMenuItemPadX
                    lineY = rectY rowRect + h / 2
                pushRect
                  da
                  (Rect (rectX rowRect + margin) lineY (rectW rowRect - 2 * margin) 1)
                  sepCol
              TextEditMenuItem action lbl -> do
                enabled <- textFieldMenuActionEnabled ctx wid action
                let hovered = enabled && rectContains rowRect mouse
                when hovered $ do
                  pushRect da rowRect (styleHoverBg menuStyle)
                  let accent = themeAccent theme
                      barRect = Rect (rectX rowRect) (rectY rowRect + 3) 2 (rectH rowRect - 6)
                  pushRoundedRect da barRect 1 accent
                unless (T.null lbl) $ do
                  (_tw, th) <- ctxMeasureText ctx lbl
                  let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
                      tx = rectX content + textInputMenuItemPadX + ix
                      ty = centeredTextY (ctxHostProfile ctx) fm (rectY content + relY) h th
                      fg = textEditMenuItemFg menuStyle enabled
                  pushText da fm tx ty lbl fg

collectTextEditMenuSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextEditMenuSpans ctx inp = do
  mMenu <- getTextInputMenu ctx
  case mMenu of
    Nothing -> pure []
    Just menu -> do
      let fm = ctxFontMetrics ctx
          theme = ctxTheme ctx
          mouse = inputMousePos inp
          menuRect = textEditMenuRect menu
          menuStyle = textEditMenuStyle theme
          content = textEditMenuContentRect (ctxHostProfile ctx) menuRect fm
          wid = textInputMenuWidget menu
      allow <- widgetOverlayAllowed ctx wid
      if not allow
        then pure []
        else if isCellHost (ctxHostProfile ctx)
        then terminalTextEditMenuSpans ctx menuRect content fm menuStyle mouse wid
        else do
          let (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
              bg = styleBg menuStyle
          spans <-
            forM (textEditMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, h) -> do
              let rowRect = Rect (rectX menuRect) (rectY content + relY) (rectW menuRect) h
              case entry of
                TextEditMenuSep -> pure []
                TextEditMenuItem action lbl -> do
                  enabled <- textFieldMenuActionEnabled ctx wid action
                  let fg = textEditMenuItemFg menuStyle enabled
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

terminalTextEditMenuSpans ::
  Context ->
  Rect ->
  Rect ->
  FontMetrics ->
  Style ->
  V2 ->
  WidgetId ->
  IO [(Rect, T.Text, Color, Color, Rect)]
terminalTextEditMenuSpans ctx menuRect content _fm menuStyle mouse wid = do
  let rx :: Int
      rx = round (rectX menuRect)
      wi :: Int
      wi = max 1 (round (rectW menuRect))
      innerW = max 0 (wi - 1)
      dropBg = styleBg menuStyle
      dropHoverBg = styleHoverBg menuStyle
      sepFg = themeSeparator (ctxTheme ctx)
  rows <-
    forM (textEditMenuLayout (ctxHostProfile ctx)) $ \(entry, relY, _h) -> do
      let rowY :: Int
          rowY = round (rectY content + relY)
      case entry of
        TextEditMenuSep ->
          pure
            [ ( Rect (fromIntegral rx) (fromIntegral rowY) (fromIntegral wi) 1
              , T.replicate innerW (T.singleton '\x2500')
              , sepFg
              , dropBg
              , menuRect
              )
            ]
        TextEditMenuItem action lbl -> do
          enabled <- textFieldMenuActionEnabled ctx wid action
          let fg = textEditMenuItemFg menuStyle enabled
              rowRect = Rect (rectX menuRect) (rectY content + relY) (rectW menuRect) (textEditMenuItemH (ctxHostProfile ctx))
              hovered = enabled && rectContains rowRect mouse
              rowBg = if hovered then dropHoverBg else dropBg
              rowText = T.singleton ' ' <> padDropText innerW lbl
          pure [(Rect (fromIntegral rx) (fromIntegral rowY) (fromIntegral wi) 1, rowText, fg, rowBg, menuRect)]
  pure (concat rows)

textFieldClickSameCell :: TextFieldClickCell -> TextFieldClickCell -> Bool
textFieldClickSameCell a b =
  textFieldClickWidget a == textFieldClickWidget b
    && textFieldClickMultiline a == textFieldClickMultiline b
    && if textFieldClickMultiline a
         then textFieldClickRow a == textFieldClickRow b
              && textFieldClickCol a == textFieldClickCol b
         else textFieldClickFlat a == textFieldClickFlat b

normalizeTextFieldClicks ::
  Context -> WidgetId -> Int -> Int -> Int -> Bool -> Int -> IO Int
normalizeTextFieldClicks ctx wid flat row col multiline rawClicks = do
  let cell =
        TextFieldClickCell
          { textFieldClickWidget = wid
          , textFieldClickFlat = flat
          , textFieldClickRow = row
          , textFieldClickCol = col
          , textFieldClickMultiline = multiline
          }
  if rawClicks <= 1
    then setTextFieldClickCell ctx (Just cell) >> pure rawClicks
    else do
      mPrev <- getTextFieldClickCell ctx
      if maybe False (textFieldClickSameCell cell) mPrev
        then pure rawClicks
        else setTextFieldClickCell ctx (Just cell) >> pure 1

data TextInputGeom = TextInputGeom
  { tigFieldRect :: Rect
  }
  deriving (Eq, Show)

textInputGeom :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> TextInputGeom
textInputGeom host fm x y w _h =
  let labelH = layoutLineHeight host fm
      gap = textInputLabelGap fm
      fieldH = textInputFieldHeight fm
      fieldY = y + labelH + gap
   in TextInputGeom {tigFieldRect = Rect x fieldY w fieldH}

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

drawTextCaret :: DrawArena -> Float -> Float -> Float -> Color -> IO ()
drawTextCaret da caretX caretY caretH fg =
  pushRect da (Rect caretX caretY 1 caretH) fg

drawTextSelectionLine :: DrawArena -> Float -> Float -> Float -> Float -> Color -> IO ()
drawTextSelectionLine da selX selY selW selH selBg =
  when (selW > 0) $
    pushRect da (Rect selX selY (max 1 selW) (max 4 selH)) selBg

computeTextInputScroll ::
  HostProfile ->
  FontMetrics ->
  Float ->
  Text ->
  Int ->
  Float ->
  Bool ->
  Float
computeTextInputScroll host fm viewportW value cursor oldScroll isFocused
  | not isFocused = 0
  | viewportW <= 0 = 0
  | otherwise =
      let prefix = T.take (max 0 (min (T.length value) cursor)) value
          caretRelX = textDisplayWidth host fm prefix
          totalTextW = textDisplayWidth host fm value
          maxScroll = max 0 (totalTextW + 1 - viewportW)
          s0
            | caretRelX < oldScroll = caretRelX
            | caretRelX + 1 > oldScroll + viewportW = caretRelX + 1 - viewportW
            | otherwise = oldScroll
       in max 0 (min maxScroll s0)

syncTextInputScroll :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO Float
syncTextInputScroll ctx idx x y w h = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
  value <- textInputValue ctx idx
  focus <- textInputFocused ctx idx
  let cursor = IM.findWithDefault (T.length value) (slotKey slotCursor key) (storeInt store)
      oldScroll = IM.findWithDefault 0 (slotKey slotTextInputScroll key) (storeFloat store)
      fm = ctxFontMetrics ctx
      host = ctxHostProfile ctx
      geom = textInputGeom host fm x y w h
      clip = textInputFieldTextClip host geom fm
      availW = rectW clip
      newScroll = computeTextInputScroll host fm availW value cursor oldScroll focus
  when (newScroll /= oldScroll) $ do
    setStore ctx (store {storeFloat = IM.insert (slotKey slotTextInputScroll key) newScroll (storeFloat store)})
  pure newScroll

drawTextInputSelection :: DrawArena -> Context -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextInputSelection da ctx idx x y w h style = do
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
            cursor = IM.findWithDefault (T.length value) (slotKey slotCursor key) (storeInt store)
            anchor = IM.findWithDefault cursor (slotKey slotAnchor key) (storeInt store)
            selLo = min anchor cursor
            selHi = max anchor cursor
            hasSel = selLo < selHi
        when hasSel $ do
          let fm = ctxFontMetrics ctx
              geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              fieldRect = tigFieldRect geom
              (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
              selBg = selectionBgColor (themeAccent (ctxTheme ctx)) (styleBg style)
              host = ctxHostProfile ctx
              wLo = textDisplayWidth host fm (T.take selLo value)
              wHi = textDisplayWidth host fm (T.take selHi value)
              lineH = layoutLineHeight host fm
              ty = centeredTextY host fm (rectY fieldRect) (rectH fieldRect) lineH
          scrollX <- syncTextInputScroll ctx idx x y w h
          let selX = rectX fieldRect + ix + wLo - scrollX
              selW = wHi - wLo
          drawTextSelectionLine da selX ty selW lineH selBg

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
            cursor = IM.findWithDefault (T.length value) (slotKey slotCursor key) (storeInt store)
        lbl <- getText (ctxNodeArena ctx) idx
        let fm = ctxFontMetrics ctx
            geom = textInputGeom (ctxHostProfile ctx) fm x y w h
            fieldRect = tigFieldRect geom
            (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
            fieldTxt = textInputFieldText lbl value focus
            prefix = T.take (max 0 (min (T.length fieldTxt) cursor)) fieldTxt
            host = ctxHostProfile ctx
            pw = textDisplayWidth host fm prefix
            lineH = layoutLineHeight host fm
            ty = centeredTextY host fm (rectY fieldRect) (rectH fieldRect) lineH
        scrollX <- syncTextInputScroll ctx idx x y w h
        let (caretX, caretY, caretH) = selectionCaretGeom (rectX fieldRect + ix - scrollX) ty pw lineH
        drawTextCaret da caretX caretY caretH (styleFg style)

applyTextInputClick :: Context -> WidgetId -> Text -> Int -> Int -> IO ()
applyTextInputClick ctx wid value idx clicks =
  let (lo, hi) = textSelectionForClick value idx clicks
   in updateTextInputSelection ctx wid lo hi

applyTextInputDrag :: Context -> WidgetId -> Text -> Int -> Int -> Int -> IO ()
applyTextInputDrag ctx wid value anchor idx clicks =
  let (lo, hi) = textSelectionForDrag value anchor idx clicks
   in updateTextInputSelection ctx wid lo hi

updateTextInputSelection :: Context -> WidgetId -> Int -> Int -> IO ()
updateTextInputSelection ctx wid anchor cursor = do
  store <- getStore ctx
  let key = intKey wid
      oldAnchor = IM.findWithDefault cursor (slotKey slotAnchor key) (storeInt store)
      oldCursor = IM.findWithDefault 0 (slotKey slotCursor key) (storeInt store)
  when (oldAnchor /= anchor || oldCursor /= cursor) $ do
    setStore
      ctx
      ( store
          { storeInt =
              IM.insert (slotKey slotAnchor key) anchor $
                IM.insert (slotKey slotCursor key) cursor (storeInt store)
          }
      )
    markDirty ctx

textInputGeomForWidget :: Context -> WidgetId -> IO (Maybe (Rect, Float, Text))
textInputGeomForWidget ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if nt /= NodeTextInput
        then pure Nothing
        else do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          let fm = ctxFontMetrics ctx
              geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              field = tigFieldRect geom
              (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
          scrollX <- syncTextInputScroll ctx idx x y w h
          let contentX = rectX field + ix - scrollX
          value <- textInputValue ctx idx
          pure (Just (field, contentX, value))

applyTextFieldMenuAction :: Context -> WidgetId -> Int -> IO ()
applyTextFieldMenuAction ctx wid item = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure ()
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      case nt of
        NodeTextInput -> applyTextInputMenuAction ctx wid item
        NodeTextArea -> applyTextAreaMenuAction ctx wid item
        _ -> pure ()

textFieldMenuActionEnabled :: Context -> WidgetId -> Int -> IO Bool
textFieldMenuActionEnabled ctx wid item = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
  mclip <- ctxClipboardGet ctx
  pure (menuActionEnabled (not (T.null text)) mclip item)

collapseTextFieldSelection :: Context -> WidgetId -> IO ()
collapseTextFieldSelection ctx wid =
  when (hashWidgetId wid /= 0) $ do
    mIdx <- findNodeByWidgetId ctx wid
    case mIdx of
      Nothing -> pure ()
      Just idx -> do
        nt <- getNodeType (ctxNodeArena ctx) idx
        case nt of
          NodeTextInput -> collapseTextInputSelection ctx wid
          NodeTextArea -> collapseTextAreaSelection ctx wid
          _ -> pure ()

collapseTextInputSelection :: Context -> WidgetId -> IO ()
collapseTextInputSelection ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      cur = IM.findWithDefault 0 (slotKey slotCursor key) (storeInt store)
  setStore ctx (store {storeInt = IM.insert (slotKey slotAnchor key) cur (storeInt store)})

collapseTextAreaSelection :: Context -> WidgetId -> IO ()
collapseTextAreaSelection ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      text = IM.findWithDefault "" key (storeText store)
      row = IM.findWithDefault 0 (slotKey slotTextAreaRow key) (storeInt store)
      col = IM.findWithDefault 0 (slotKey slotTextAreaCol key) (storeInt store)
      state = loadTextAreaState store key text
      state' = state {selectionAnchor = TB.Cursor row col}
  setStore ctx (saveTextAreaState key state' store)

data TextAreaGeom = TextAreaGeom
  { tagFieldRect :: !Rect
  , tagLineHeight :: !Float
  }
  deriving (Eq, Show)

data TextAreaHit = TextAreaHit
  { tahNodeIdx :: !NodeIdx
  , tahFieldRect :: !Rect
  , tahContentX :: !Float
  , tahLineH :: !Float
  , tahWidgetX :: !Float
  , tahWidgetY :: !Float
  , tahWidgetW :: !Float
  , tahWidgetH :: !Float
  }

textAreaGeom :: HostProfile -> FontMetrics -> Float -> Float -> Float -> Float -> TextAreaGeom
textAreaGeom host fm x y w h =
  let labelH = layoutLineHeight host fm
      gap = textInputLabelGap fm
      fieldY = y + labelH + gap
      fieldH = max 0 (h - labelH - gap)
      lineH = fmLineHeight fm
   in TextAreaGeom {tagFieldRect = Rect x fieldY w fieldH, tagLineHeight = lineH}

textAreaFieldClip :: HostProfile -> TextAreaGeom -> FontMetrics -> Rect
textAreaFieldClip host geom fm =
  let field = tagFieldRect geom
      (ix, iy) = widgetContentInset host fm
   in Rect
        (rectX field + ix)
        (rectY field + iy)
        (max 0 (rectW field - 2 * ix))
        (max 0 (rectH field - 2 * iy))

textAreaFocused :: Context -> NodeIdx -> IO Bool
textAreaFocused = textInputFocused

textAreaValue :: Context -> NodeIdx -> IO Text
textAreaValue = textInputValue

loadTextAreaStateAt :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO TA.TextAreaState
loadTextAreaStateAt ctx idx x y w h = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
      initial = IM.findWithDefault "" key (storeText store)
      fm = ctxFontMetrics ctx
      geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
      clip = textAreaFieldClip (ctxHostProfile ctx) geom fm
      vpW = rectW clip
      vpH = rectH clip
      lineH = tagLineHeight geom
      state0 = TA.loadTextAreaState store key initial
  pure (TA.setTextAreaViewport (realToFrac vpW, realToFrac vpH) (realToFrac lineH) state0)

syncTextAreaViewport :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO ()
syncTextAreaViewport ctx idx x y w h = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
      fm = ctxFontMetrics ctx
      geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
      clip = textAreaFieldClip (ctxHostProfile ctx) geom fm
      vp = (rectW clip, rectH clip)
  setStore ctx (store {storePoint = IM.insert (slotKey slotTextAreaViewport key) vp (storePoint store)})

drawTextAreaSelection ::
  DrawArena ->
  Context ->
  TA.TextAreaState ->
  TextAreaGeom ->
  HostProfile ->
  FontMetrics ->
  Theme ->
  Style ->
  IO ()
drawTextAreaSelection da _ctx state geom host fm theme style = do
  let anchor = TA.selectionAnchor state
      cursor = TB.getCursor (TA.buffer state)
  when (anchor /= cursor) $ do
    let (lo, hi) = TB.selectionRange anchor cursor
        lineTexts = TB.toLines (TA.buffer state)
        field = tagFieldRect geom
        lineH = tagLineHeight geom
        (ix, iy) = widgetContentInset host fm
        scrollYf = realToFrac (snd (TA.scrollOffset state))
        contentTop = rectY field + iy
        selBg = selectionBgColor (themeAccent theme) (styleBg style)
        loRow = TB.cursorRow lo
        loCol = TB.cursorCol lo
        hiRow = TB.cursorRow hi
        hiCol = TB.cursorCol hi
    forM_ [loRow .. hiRow] $ \row -> do
      let line =
            if row >= 0 && row < length lineTexts
              then lineTexts !! row
              else ""
          lineLen = T.length line
          clampCol c = max 0 (min lineLen c)
          startCol =
            clampCol
              ( if row == loRow
                  then loCol
                  else 0
              )
          endCol =
            clampCol
              ( if row == hiRow
                  then hiCol
                  else lineLen
              )
      when (startCol < endCol) $ do
        let wLo = textDisplayWidth host fm (T.take startCol line)
            wHi = textDisplayWidth host fm (T.take endCol line)
            selW = wHi - wLo
            ly = contentTop + fromIntegral row * lineH - scrollYf
            selX = rectX field + ix + wLo
            selH = max 4 lineH
        drawTextSelectionLine da selX ly selW selH selBg

drawTextAreaContent :: DrawArena -> Context -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextAreaContent da ctx idx x y w h style = do
  let terminal = isCellHost (ctxHostProfile ctx)
  if terminal
    then pure ()
    else do
      syncTextAreaViewport ctx idx x y w h
      focus <- textAreaFocused ctx idx
      let fm = ctxFontMetrics ctx
          host = ctxHostProfile ctx
          theme = ctxTheme ctx
          geom = textAreaGeom host fm x y w h
          field = tagFieldRect geom
          lineH = tagLineHeight geom
          clip = textAreaFieldClip host geom fm
          contentX = rectX clip
          contentTop = rectY clip
          fg = styleFg style
      state <- loadTextAreaStateAt ctx idx x y w h
      let buf = TA.buffer state
          lineTexts = TB.toLines buf
          (_, scrollY) = TA.scrollOffset state
          scrollYf = realToFrac scrollY
          fieldTop = rectY field
          fieldBottom = fieldTop + rectH field
      withClip da clip $ do
        when focus $
          drawTextAreaSelection da ctx state geom host fm theme style
        forM_ (zip [0 :: Int ..] lineTexts) $ \(row, line) -> do
          let ly = contentTop + fromIntegral row * lineH - scrollYf
          when (ly + lineH >= fieldTop && ly <= fieldBottom) $
            unless (T.null line) $ do
              pushText da fm contentX ly line fg
        when focus $ do
          let TB.Cursor row col = TB.getCursor buf
              currentLine =
                if row >= 0 && row < length lineTexts
                  then lineTexts !! row
                  else ""
              prefix = T.take col currentLine
              pw = textDisplayWidth host fm prefix
              (caretX, caretY, caretH) = selectionCaretGeom contentX (contentTop + fromIntegral row * lineH - scrollYf) pw lineH
          drawTextCaret da caretX caretY caretH fg

textAreaHitForWidget :: Context -> WidgetId -> IO (Maybe TextAreaHit)
textAreaHitForWidget ctx wid = do
  mIdx <- findNodeByWidgetId ctx wid
  case mIdx of
    Nothing -> pure Nothing
    Just idx -> do
      nt <- getNodeType (ctxNodeArena ctx) idx
      if nt /= NodeTextArea
        then pure Nothing
        else do
          (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
          let fm = ctxFontMetrics ctx
              geom = textAreaGeom (ctxHostProfile ctx) fm x y w h
              field = tagFieldRect geom
              clip = textAreaFieldClip (ctxHostProfile ctx) geom fm
          pure
            ( Just
                TextAreaHit
                  { tahNodeIdx = idx
                  , tahFieldRect = field
                  , tahContentX = rectX clip
                  , tahLineH = tagLineHeight geom
                  , tahWidgetX = x
                  , tahWidgetY = y
                  , tahWidgetW = w
                  , tahWidgetH = h
                  }
            )

textAreaCursorAt :: Context -> TA.TextAreaState -> TextAreaHit -> V2 -> IO (Int, Int)
textAreaCursorAt ctx state hit mouse = do
  let lineTexts = TB.toLines (TA.buffer state)
      lineCount = max 1 (length lineTexts)
      scrollYf = realToFrac (snd (TA.scrollOffset state))
      fm = ctxFontMetrics ctx
      (_, iy) = widgetContentInset (ctxHostProfile ctx) fm
      contentTop = rectY (tahFieldRect hit) + iy
      relY = v2Y mouse - contentTop + scrollYf
      rawRow = floor (relY / max 1 (tahLineH hit))
      row = max 0 (min (lineCount - 1) rawRow)
      line =
        if row < length lineTexts
          then lineTexts !! row
          else ""
  col <- textCharAtX ctx line (tahContentX hit) (v2X mouse)
  pure (row, col)

updateTextAreaSelection :: Context -> WidgetId -> TextAreaHit -> TB.Cursor -> TB.Cursor -> IO ()
updateTextAreaSelection ctx wid hit anchor cursor = do
  state0 <-
    loadTextAreaStateAt
      ctx
      (tahNodeIdx hit)
      (tahWidgetX hit)
      (tahWidgetY hit)
      (tahWidgetW hit)
      (tahWidgetH hit)
  let state1 = TA.setTextAreaSelection anchor cursor state0
  store <- getStore ctx
  setStore ctx (TA.saveTextAreaState (intKey wid) state1 store)
  markDirty ctx

applyTextAreaClick :: Context -> WidgetId -> TextAreaHit -> Int -> Int -> Int -> IO ()
applyTextAreaClick ctx wid hit row col clicks
  | clicks >= 3 = do
      state <-
        loadTextAreaStateAt
          ctx
          (tahNodeIdx hit)
          (tahWidgetX hit)
          (tahWidgetY hit)
          (tahWidgetW hit)
          (tahWidgetH hit)
      let end = TB.documentEnd (TA.buffer state)
      updateTextAreaSelection ctx wid hit (TB.Cursor 0 0) end
  | clicks == 2 = do
      state <-
        loadTextAreaStateAt
          ctx
          (tahNodeIdx hit)
          (tahWidgetX hit)
          (tahWidgetY hit)
          (tahWidgetW hit)
          (tahWidgetH hit)
      let lineTexts = TB.toLines (TA.buffer state)
          line =
            if row >= 0 && row < length lineTexts
              then lineTexts !! row
              else ""
          (lo, hi) = textWordBounds line col
          anchor = TB.Cursor row lo
          cursor = TB.Cursor row hi
      updateTextAreaSelection ctx wid hit anchor cursor
  | otherwise =
      updateTextAreaSelection ctx wid hit (TB.Cursor row col) (TB.Cursor row col)

applyTextAreaDrag :: Context -> WidgetId -> TextAreaHit -> Int -> Int -> Int -> Int -> Int -> IO ()
applyTextAreaDrag ctx wid hit anchorRow anchorCol row col clicks
  | clicks >= 3 = applyTextAreaClick ctx wid hit row col clicks
  | clicks == 2 = do
      state <-
        loadTextAreaStateAt
          ctx
          (tahNodeIdx hit)
          (tahWidgetX hit)
          (tahWidgetY hit)
          (tahWidgetW hit)
          (tahWidgetH hit)
      let lineTexts = TB.toLines (TA.buffer state)
          anchorLine =
            if anchorRow >= 0 && anchorRow < length lineTexts
              then lineTexts !! anchorRow
              else ""
          cursorLine =
            if row >= 0 && row < length lineTexts
              then lineTexts !! row
              else ""
          (a0, a1) = textWordBounds anchorLine anchorCol
          (c0, c1) = textWordBounds cursorLine col
          anchor = TB.Cursor anchorRow (min a0 c0)
          cursor = TB.Cursor row (max a1 c1)
      updateTextAreaSelection ctx wid hit anchor cursor
  | otherwise =
      updateTextAreaSelection ctx wid hit (TB.Cursor anchorRow anchorCol) (TB.Cursor row col)

finalizeTextAreaMouse :: Context -> Input -> WidgetId -> IO ()
finalizeTextAreaMouse ctx inp wid = do
  mHit <- textAreaHitForWidget ctx wid
  case mHit of
    Nothing -> pure ()
    Just hit -> do
      let mouse = inputMousePos inp
          getCursor = do
            state <-
              loadTextAreaStateAt
                ctx
                (tahNodeIdx hit)
                (tahWidgetX hit)
                (tahWidgetY hit)
                (tahWidgetW hit)
                (tahWidgetH hit)
            textAreaCursorAt ctx state hit mouse
      if inputMousePressed inp && rectContains (tahFieldRect hit) mouse
        then do
          (row, col) <- getCursor
          clicks <-
            normalizeTextFieldClicks
              ctx
              wid
              0
              row
              col
              True
              (max 1 (inputMouseClicks inp))
          applyTextAreaClick ctx wid hit row col clicks
          setTextInputDrag ctx (Just (TextInputDrag wid 0 row col True clicks))
        else do
          mDrag <- getTextInputDrag ctx
          case mDrag of
            Just drag
              | textInputDragWidget drag == wid
                  , textInputDragMultiline drag
                  , inputMouseDown inp || inputMouseReleased inp -> do
                  (row, col) <- getCursor
                  applyTextAreaDrag
                    ctx
                    wid
                    hit
                    (textInputDragAnchorRow drag)
                    (textInputDragAnchorCol drag)
                    row
                    col
                    (textInputDragClicks drag)
            _ -> pure ()

finalizeTextFieldMouse :: Context -> Input -> IO ()
finalizeTextFieldMouse ctx inp = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $ do
    mGeom <- textInputGeomForWidget ctx focus
    case mGeom of
      Just (fieldRect, contentX, value) -> do
        let mouse = inputMousePos inp
            inField = rectContains fieldRect mouse
        if inputMousePressed inp && inField
          then do
            idx <- textCharAtX ctx value contentX (v2X mouse)
            clicks <-
              normalizeTextFieldClicks
                ctx
                focus
                idx
                0
                0
                False
                (max 1 (inputMouseClicks inp))
            applyTextInputClick ctx focus value idx clicks
            setTextInputDrag ctx (Just (TextInputDrag focus idx 0 0 False clicks))
          else do
            mDrag <- getTextInputDrag ctx
            case mDrag of
              Just drag
                | textInputDragWidget drag == focus
                    , not (textInputDragMultiline drag)
                    , inputMouseDown inp || inputMouseReleased inp -> do
                    idx <- textCharAtX ctx value contentX (v2X mouse)
                    applyTextInputDrag ctx focus value (textInputDragAnchor drag) idx (textInputDragClicks drag)
              _ -> pure ()
      Nothing -> finalizeTextAreaMouse ctx inp focus
  when (inputMouseReleased inp) $
    setTextInputDrag ctx Nothing

