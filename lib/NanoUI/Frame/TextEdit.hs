{-# LANGUAGE DataKinds #-}

module NanoUI.Frame.TextEdit
  ( TextEditMenuRow (..)
  , textCharAtX
  , textWordBounds
  , textEditMenuRect
  , textFieldMenuRect
  , openTextEditMenu
  , finalizeTextEditMenuPick
  , closeTextEditMenuOnOutsideClick
  , closeTextEditMenuOnEscape
  , drawTextEditMenuOverlays
  , collectTextEditMenuSpans
  , textEditMenuCursorKind
  , normalizeTextFieldClicks
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , TextFieldClickCell (..)
  , TextInputMenu (..)
  , markDirty
  , markEscapeConsumed
  )
import NanoUI.Draw (pushRect, pushRoundedRect, pushText)
import NanoUI.Font (FontMetrics, centeredTextY, hasMonoFontMarker, layoutLineHeight, stripMonoFontMarker, widgetContentInset)
import NanoUI.Frame.Chrome
  ( fillStyledRect
  , overlayMenuStyle
  , padDropText
  , pushMenuShadow
  , strokeStyledRect
  , textInputMenuItemPadX
  , textInputMenuOuterPad
  )
import NanoUI.Input (UiCursorKind (..))
import NanoUI.Frame.Hit (widgetOverlayAllowed)
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Id (WidgetId (..), hashWidgetId)
import NanoUI.Input
  ( Input (..)
  , Key (..)
  , inputKeys
  , inputKeysElem
  , inputMousePos
  , inputMousePressed
  , inputMouseRightPressed
  , inputWindowSize
  )
import NanoUI.Layout.Arena (NodeType (NodeTextArea, NodeTextInput), arenaCount, getNodeType, getRect, getWidgetId)
import NanoUI.WidgetText (textInputFieldHeight, textInputLabelGap)
import NanoUI.Style (Style (..), Theme (..), themeAccent, themeSeparator)
import NanoUI.Types (Color (..), Rect (..), Size (..), V2 (..), lerpColor, rectContains, rectH, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Widgets.TextEdit (applyTextFieldMenuAction, textFieldMenuActionEnabled)

data TextCharClass = TextWord | TextSpace | TextOther
  deriving (Eq)

textCharClass :: Char -> TextCharClass
textCharClass c
  | isAlphaNum c || c == '_' = TextWord
  | isSpace c = TextSpace
  | otherwise = TextOther

textCharAtX :: Context -> Text -> Float -> Float -> IO Int
textCharAtX ctx text startX mouseX = do
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

textWordBounds :: Text -> Int -> (Int, Int)
textWordBounds text raw
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

textFieldMenuRect :: Context -> WidgetId -> IO (Maybe Rect)
textFieldMenuRect ctx wid = do
  mInput <- textInputFieldRectForWidget ctx wid
  case mInput of
    Just fieldRect -> pure (Just fieldRect)
    Nothing -> textAreaFieldRectForWidget ctx wid

textInputFieldRectForWidget :: Context -> WidgetId -> IO (Maybe Rect)
textInputFieldRectForWidget ctx wid = do
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
                  (x, y, w, _) <- getRect (ctxNodeArena ctx) idx
                  let fm = ctxFontMetrics ctx
                      labelH = layoutLineHeight (ctxHostProfile ctx) fm
                      gap = textInputLabelGap fm
                      fieldH = textInputFieldHeight fm
                      fieldY = y + labelH + gap
                  pure (Just (Rect x fieldY w fieldH))

textAreaFieldRectForWidget :: Context -> WidgetId -> IO (Maybe Rect)
textAreaFieldRectForWidget ctx wid = do
  count <- arenaCount (ctxNodeArena ctx)
  go 0 count
  where
    go idx count
      | idx >= count = pure Nothing
      | otherwise = do
          nt <- getNodeType (ctxNodeArena ctx) idx
          if nt /= NodeTextArea
            then go (idx + 1) count
            else do
              w' <- getWidgetId (ctxNodeArena ctx) idx
              if w' /= wid
                then go (idx + 1) count
                else do
                  (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
                  let fm = ctxFontMetrics ctx
                      labelH = layoutLineHeight (ctxHostProfile ctx) fm
                      gap = textInputLabelGap fm
                      fieldY = y + labelH + gap
                      fieldH = max 0 (h - labelH - gap)
                  pure (Just (Rect x fieldY w fieldH))

openTextEditMenu :: Context -> Input -> IO ()
openTextEditMenu ctx inp =
  when (inputMouseRightPressed inp) $ do
    focus <- readIORef (ctxFocusId ctx)
    when (hashWidgetId focus /= 0) $ do
      mField <- textFieldMenuRect ctx focus
      case mField of
        Nothing -> pure ()
        Just fieldRect -> do
          let mouse = inputMousePos inp
          when (rectContains fieldRect mouse) $ do
            fm <- pure (ctxFontMetrics ctx)
            menuW <- textEditMenuWidth ctx
            let menuRect = textEditMenuRectAt (ctxHostProfile ctx) fm (v2X mouse) (v2Y mouse) menuW (inputWindowSize inp)
            writeIORef (ctxTextInputMenu ctx) (Just (TextInputMenu focus menuRect))
            markDirty ctx

finalizeTextEditMenuPick :: Context -> Input -> IO ()
finalizeTextEditMenuPick ctx inp =
  when (inputMousePressed inp) $ do
    mMenu <- readIORef (ctxTextInputMenu ctx)
    case mMenu of
      Nothing -> pure ()
      Just menu ->
        let mouse = inputMousePos inp
            rect = textEditMenuRect menu
         in when (rectContains rect mouse) $ do
              let fm = ctxFontMetrics ctx
              case textEditMenuPickAction (ctxHostProfile ctx) rect fm mouse of
                Nothing -> writeIORef (ctxTextInputMenu ctx) Nothing
                Just idx -> do
                  enabled <- textFieldMenuActionEnabled ctx (textInputMenuWidget menu) idx
                  if enabled
                    then applyTextFieldMenuAction ctx (textInputMenuWidget menu) idx
                    else do
                      writeIORef (ctxTextInputMenu ctx) Nothing
                      markDirty ctx

closeTextEditMenuOnOutsideClick :: Context -> Input -> IO ()
closeTextEditMenuOnOutsideClick ctx inp =
  when (inputMousePressed inp || inputMouseRightPressed inp) $ do
    mMenu <- readIORef (ctxTextInputMenu ctx)
    case mMenu of
      Nothing -> pure ()
      Just menu -> do
        let mouse = inputMousePos inp
        unless (rectContains (textEditMenuRect menu) mouse) $
          writeIORef (ctxTextInputMenu ctx) Nothing

closeTextEditMenuOnEscape :: Context -> Input -> IO ()
closeTextEditMenuOnEscape ctx inp =
  when (inputKeysElem KeyEscape (inputKeys inp)) $
    readIORef (ctxTextInputMenu ctx) >>= \case
      Nothing -> pure ()
      Just _ -> do
        writeIORef (ctxTextInputMenu ctx) Nothing
        markEscapeConsumed ctx
        markDirty ctx

textEditMenuCursorKind :: Context -> Input -> IO (Maybe UiCursorKind)
textEditMenuCursorKind ctx inp = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
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
                      (fm', shown) = if hasMonoFontMarker lbl
                                       then (ctxMonoFontMetrics ctx, stripMonoFontMarker lbl)
                                       else (fm, lbl)
                  pushText da fm' tx ty shown fg

collectTextEditMenuSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextEditMenuSpans ctx inp = do
  mMenu <- readIORef (ctxTextInputMenu ctx)
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
    then writeIORef (ctxTextFieldClickCell ctx) (Just cell) >> pure rawClicks
    else do
      mPrev <- readIORef (ctxTextFieldClickCell ctx)
      if maybe False (textFieldClickSameCell cell) mPrev
        then pure rawClicks
        else writeIORef (ctxTextFieldClickCell ctx) (Just cell) >> pure 1
