{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module NanoUI.Frame.TextInput
  ( TextInputGeom (..)
  , textInputGeom
  , drawTextInputCaret
  , drawTextInputSelection
  , textInputCharAtX
  , openTextInputMenu
  , finalizeTextInputMenuPick
  , closeTextInputMenuOnOutsideClick
  , closeTextInputMenuOnEscape
  , drawTextInputMenuOverlays
  , collectTextInputMenuSpans
  , textInputMenuCursorKind
  , collapseTextInputSelection
  , textInputGeomForWidget
  , applyTextInputClick
  , applyTextInputDrag
  , tagTextInputClippedSpans
  , textInputFieldTextClip
  , tagSelectClippedSpans
  , selectTextClip
  ) where


import Control.Monad (filterM, foldM, forM, forM_, unless, void, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Typeable (Typeable)
import Data.List (findIndex)
import Data.Maybe (isJust)
import Data.Word (Word32)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
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
  , stripMonoFontMarker
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
import NanoUI.Input (Input (..), Key (..), Modifiers (..), inputInteracted, inputKeys, inputKeysElem, inputPointerHeld, inputMouseDown, inputMousePos, inputMousePressed, inputMouseReleased, inputMouseRightPressed, inputScroll, inputDeltaTime, inputWindowSize, modShift)
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
  , NodeType (NodeButton, NodeCheckbox, NodeRadio, NodeSelect, NodeColorPicker, NodeSlider, NodeTextInput, NodeModal, NodeImage, NodePanel, NodeWindow, NodeContainer, NodeScrollContainer, NodeText, NodeSeparator, NodeSpacer, NodeBox)
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
import NanoUI.Frame.Clip (padTextClipRect)
import NanoUI.Frame.CursorKind (UiCursorKind (..))
import NanoUI.Frame.Chrome
  ( displayText
  , fillStyledRect
  , overlayMenuStyle
  , padDropText
  , pushMenuShadow
  , strokeStyledRect
  , textInputFocused
  , textInputMenuCornerR
  , textInputMenuItemPadX
  , textInputMenuOuterPad
  , textInputMenuShadowOff
  , textInputValue
  )
import NanoUI.Frame.Hit (widgetOverlayAllowed)

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
            cursor = IM.findWithDefault (T.length value) key (storeCursor store)
            anchor = IM.findWithDefault cursor key (storeSelAnchor store)
            selLo = min anchor cursor
            selHi = max anchor cursor
            hasSel = selLo < selHi
        when hasSel $ do
          let fm = ctxFontMetrics ctx
              geom = textInputGeom (ctxHostProfile ctx) fm x y w h
              fieldRect = tigFieldRect geom
              (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
              theme = ctxTheme ctx
              accent = themeAccent theme
              selBg = lerpColor accent (styleBg style) 0.55
          (wLo, _) <- ctxMeasureText ctx (T.take selLo value)
          (wHi, _) <- ctxMeasureText ctx (T.take selHi value)
          (_, ph) <- ctxMeasureText ctx value
          let ty = centeredTextY (ctxHostProfile ctx) fm (rectY fieldRect) (rectH fieldRect) ph
              selX = rectX fieldRect + ix + wLo
              selW = max 1 (wHi - wLo)
              selH = max 4 ph
          pushRect da (Rect selX ty selW selH) selBg

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
            cursor = IM.findWithDefault (T.length value) key (storeCursor store)
        lbl <- getText (ctxNodeArena ctx) idx
        let fm = ctxFontMetrics ctx
            geom = textInputGeom (ctxHostProfile ctx) fm x y w h
            fieldRect = tigFieldRect geom
            (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
            fieldTxt = textInputFieldText lbl value focus
            prefix = T.take (max 0 (min (T.length fieldTxt) cursor)) fieldTxt
        (pw, _) <- ctxMeasureText ctx prefix
        (_, ph) <- ctxMeasureText ctx fieldTxt
        let ty = centeredTextY (ctxHostProfile ctx) fm (rectY fieldRect) (rectH fieldRect) ph
            caretX = rectX fieldRect + ix + pw
            caretY = ty + 1
            caretH = max 4 (ph - 2)
        pushRect da (Rect caretX caretY 1 caretH) (styleFg style)

collapseTextInputSelection :: Context -> WidgetId -> IO ()
collapseTextInputSelection ctx wid =
  when (hashWidgetId wid /= 0) $ do
    store <- getStore ctx
    let key = intKey wid
        cur = IM.findWithDefault 0 key (storeCursor store)
    setStore ctx (store {storeSelAnchor = IM.insert key cur (storeSelAnchor store)})

data TextCharClass = TextWord | TextSpace | TextOther
  deriving (Eq)

textCharClass :: Char -> TextCharClass
textCharClass c
  | isAlphaNum c || c == '_' = TextWord
  | isSpace c = TextSpace
  | otherwise = TextOther

-- Word bounds for double-click selection. Uses T.index (UTF-16 code units), not
-- grapheme clusters. Fine for ASCII identifiers and typical terminal input.
textInputWordBounds :: Text -> Int -> (Int, Int)
textInputWordBounds text raw
  | T.null text = (0, 0)
  | otherwise =
      let n = T.length text
          i = max 0 (min (n - 1) raw)
          cls = textCharClass (T.index text i)
          lo = goLeft cls i
          hi = goRight cls n i + 1
       in (lo, hi)
  where
    goLeft cls i
      | i <= 0 = 0
      | textCharClass (T.index text (i - 1)) == cls = goLeft cls (i - 1)
      | otherwise = i
    goRight cls n i
      | i + 1 >= n = i
      | textCharClass (T.index text (i + 1)) == cls = goRight cls n (i + 1)
      | otherwise = i

applyTextInputClick :: Context -> WidgetId -> Text -> Int -> Int -> IO ()
applyTextInputClick ctx wid value idx clicks
  | clicks >= 3 = updateTextInputSelection ctx wid 0 (T.length value)
  | clicks == 2 =
      let (lo, hi) = textInputWordBounds value idx
       in updateTextInputSelection ctx wid lo hi
  | otherwise = updateTextInputSelection ctx wid idx idx

applyTextInputDrag :: Context -> WidgetId -> Text -> Int -> Int -> Int -> IO ()
applyTextInputDrag ctx wid value anchor idx clicks
  | clicks >= 3 = updateTextInputSelection ctx wid 0 (T.length value)
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

textInputMenuSepH :: HostProfile -> Float
textInputMenuSepH host = if isCellHost host then 1 else 9

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
      cursor = IM.findWithDefault (T.length text) key (storeCursor store)
      anchor = IM.findWithDefault cursor key (storeSelAnchor store)
      hasSel = anchor /= cursor
  mclip <- ctxClipboardGet ctx
  let clipTxt = maybe "" id mclip
  pure $
    case item of
      0 -> hasSel
      1 -> not (T.null text)
      2 -> not (T.null clipTxt)
      3 -> not (T.null text)
      _ -> False

textInputMenuItemFg :: Style -> Bool -> Color
textInputMenuItemFg style enabled =
  if enabled
    then styleFg style
    else lerpColor (styleFg style) (styleBg style) 0.55

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
  when (inputKeysElem KeyEscape (inputKeys inp)) $
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
              TextInputMenuItem action lbl -> do
                enabled <- textInputMenuActionEnabled ctx wid action
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
                      fg = textInputMenuItemFg menuStyle enabled
                      (fm', shown) = if hasMonoFontMarker lbl
                                       then (ctxMonoFontMetrics ctx, stripMonoFontMarker lbl)
                                       else (fm, lbl)
                  pushText da fm' tx ty shown fg

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

textInputGeomForWidget :: Context -> WidgetId -> IO (Maybe (Rect, Float, Text))
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

textInputCharAtX :: Context -> Text -> Float -> Float -> IO Int
textInputCharAtX ctx text startX mouseX = do
  let len = T.length text
      relX = max 0 (mouseX - startX)
  if len <= 0
    then pure 0
    else search 0 len relX
  where
    search lo hi x =
      if hi - lo <= 1
        then do
          (wLo, _) <- ctxMeasureText ctx (T.take lo text)
          (wHi, _) <- ctxMeasureText ctx (T.take hi text)
          if x - wLo <= wHi - x then pure lo else pure hi
        else do
          let mid = (lo + hi) `div` 2
          (wMid, _) <- ctxMeasureText ctx (T.take mid text)
          if wMid <= x then search mid hi x else search lo mid x

