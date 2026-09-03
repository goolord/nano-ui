{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.TextInput
  ( TextInputGeom (..)
  , textInputGeom
  , drawTextInputCaret
  , drawTextInputSelection
  , textInputCharAtX
  , textWordBounds
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

import Control.Monad (when)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , WidgetStore (..)
  , getStore
  , intKey
  , markDirty
  , setStore
  , slotAnchor
  , slotCursor
  , slotKey
  )
import NanoUI.Draw (DrawArena, pushRect)
import NanoUI.Font
  ( FontMetrics
  , centeredTextY
  , fmLineHeight
  , layoutLineHeight
  , pickMonoFont
  , textDisplayWidth
  , widgetContentInset
  )
import NanoUI.Frame.Scroll.Geometry (padTextClipRect)
import NanoUI.Input (UiCursorKind (..))
import NanoUI.Frame.TextEdit qualified as FE
import NanoUI.Frame.TextEdit
  ( closeTextEditMenuOnEscape
  , closeTextEditMenuOnOutsideClick
  , collectTextEditMenuSpans
  , drawTextEditMenuOverlays
  , finalizeTextEditMenuPick
  , openTextEditMenu
  , textCharAtX
  , textEditMenuCursorKind
  )
import NanoUI.Frame.Chrome (textInputFocused, textInputValue)
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Id (WidgetId)
import NanoUI.Layout.Arena (NodeIdx, NodeType (NodeTextInput), arenaCount, getNodeType, getRect, getText, getWidgetId)
import NanoUI.Input (Input)
import NanoUI.Style (Style (..), Theme (..), styleBg, styleFg, themeAccent)
import NanoUI.Types (Color (..), Rect (..), lerpColor, rectH, rectIntersect, rectOverlapArea, rectW, rectX, rectY)
import NanoUI.WidgetText
  ( selectChevronReserve
  , textInputFieldHeight
  , textInputFieldText
  , textInputLabelGap
  )
import NanoUI.Widgets.TextEdit qualified as WE

selectTextClip :: HostProfile -> Float -> Float -> Float -> Float -> FontMetrics -> Rect
selectTextClip host x y w h fm =
  let (ix, _) = widgetContentInset host fm
   in Rect (x + ix) y (max 0 (w - ix - selectChevronReserve)) (max 0 h)

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
              theme = ctxTheme ctx
              accent = themeAccent theme
              selBg = lerpColor accent (styleBg style) 0.55
              (fieldFm, _) = pickMonoFont fm (ctxMonoFontMetrics ctx) value
              host = ctxHostProfile ctx
              wLo = textDisplayWidth host fieldFm (T.take selLo value)
              wHi = textDisplayWidth host fieldFm (T.take selHi value)
          let lineH = layoutLineHeight host fm
              ty = centeredTextY host fm (rectY fieldRect) (rectH fieldRect) lineH
              selX = rectX fieldRect + ix + wLo
              selW = max 1 (wHi - wLo)
              selH = max 4 lineH
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
            cursor = IM.findWithDefault (T.length value) (slotKey slotCursor key) (storeInt store)
        lbl <- getText (ctxNodeArena ctx) idx
        let fm = ctxFontMetrics ctx
            geom = textInputGeom (ctxHostProfile ctx) fm x y w h
            fieldRect = tigFieldRect geom
            (ix, _) = widgetContentInset (ctxHostProfile ctx) fm
            fieldTxt = textInputFieldText lbl value focus
            prefix = T.take (max 0 (min (T.length fieldTxt) cursor)) fieldTxt
            (fieldFm, _) = pickMonoFont fm (ctxMonoFontMetrics ctx) fieldTxt
            host = ctxHostProfile ctx
            pw = textDisplayWidth host fieldFm prefix
        let lineH = layoutLineHeight host fm
            ty = centeredTextY host fm (rectY fieldRect) (rectH fieldRect) lineH
            caretX = rectX fieldRect + ix + pw
            caretY = ty + 1
            caretH = max 4 (lineH - 2)
        pushRect da (Rect caretX caretY 1 caretH) (styleFg style)

collapseTextInputSelection :: Context -> WidgetId -> IO ()
collapseTextInputSelection = WE.collapseTextInputSelection

textInputCharAtX :: Context -> Text -> Float -> Float -> IO Int
textInputCharAtX = textCharAtX

textWordBounds :: Text -> Int -> (Int, Int)
textWordBounds = FE.textWordBounds

applyTextInputClick :: Context -> WidgetId -> Text -> Int -> Int -> IO ()
applyTextInputClick ctx wid value idx clicks
  | clicks >= 3 = updateTextInputSelection ctx wid 0 (T.length value)
  | clicks == 2 =
      let (lo, hi) = textWordBounds value idx
       in updateTextInputSelection ctx wid lo hi
  | otherwise = updateTextInputSelection ctx wid idx idx

applyTextInputDrag :: Context -> WidgetId -> Text -> Int -> Int -> Int -> IO ()
applyTextInputDrag ctx wid value anchor idx clicks
  | clicks >= 3 = updateTextInputSelection ctx wid 0 (T.length value)
  | clicks == 2 =
      let (a0, a1) = textWordBounds value anchor
          (c0, c1) = textWordBounds value idx
       in updateTextInputSelection ctx wid (min a0 c0) (max a1 c1)
  | otherwise = updateTextInputSelection ctx wid anchor idx

openTextInputMenu :: Context -> Input -> IO ()
openTextInputMenu = openTextEditMenu

finalizeTextInputMenuPick :: Context -> Input -> IO ()
finalizeTextInputMenuPick = finalizeTextEditMenuPick

closeTextInputMenuOnOutsideClick :: Context -> Input -> IO ()
closeTextInputMenuOnOutsideClick = closeTextEditMenuOnOutsideClick

closeTextInputMenuOnEscape :: Context -> Input -> IO ()
closeTextInputMenuOnEscape = closeTextEditMenuOnEscape

textInputMenuCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textInputMenuCursorKind = textEditMenuCursorKind

drawTextInputMenuOverlays :: Context -> Input -> IO ()
drawTextInputMenuOverlays = drawTextEditMenuOverlays

collectTextInputMenuSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextInputMenuSpans = collectTextEditMenuSpans

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
