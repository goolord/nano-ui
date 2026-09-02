module NanoUI.Frame.TextArea
  ( TextAreaGeom (..)
  , TextAreaHit (..)
  , textAreaGeom
  , textAreaFieldClip
  , textAreaFocused
  , textAreaValue
  , loadTextAreaStateAt
  , syncTextAreaViewport
  , drawTextAreaContent
  , textAreaHitForWidget
  , textAreaCursorAt
  , applyTextAreaClick
  , applyTextAreaDrag
  , finalizeTextAreaMouse
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , TextInputDrag (..)
  , WidgetStore (..)
  , getStore
  , intKey
  , markDirty
  , setStore
  )
import NanoUI.Draw (DrawArena, pushRect, pushText, withClip)
import NanoUI.Font
  ( FontMetrics
  , fmLineHeight
  , layoutLineHeight
  , pickMonoFont
  , widgetContentInset
  )
import NanoUI.Frame.TextEdit (normalizeTextFieldClicks)
import NanoUI.Frame.TextInput (textInputCharAtX, textWordBounds)
import NanoUI.Types (HostProfile, isCellHost)
import NanoUI.Id (WidgetId (..))
import NanoUI.Input
  ( Input (..)
  , inputMouseClicks
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  )
import NanoUI.Layout.Arena (NodeIdx, NodeType (NodeTextArea), arenaCount, getNodeType, getRect, getWidgetId)
import NanoUI.Style (Style (..), Theme (..), styleBg, styleFg, themeAccent)
import NanoUI.Types (Rect (..), V2 (..), lerpColor, rectContains, rectH, rectW, rectX, rectY, v2X, v2Y)
import NanoUI.Widgets.TextArea as TA
import NanoUI.Widgets.TextBuffer as TB
import NanoUI.Store (slotKey, slotTextAreaViewport)
import NanoUI.WidgetText (textInputLabelGap)

data TextAreaGeom = TextAreaGeom
  { tagFieldRect :: !Rect
  , tagLineHeight :: !Float
  }

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
textAreaFocused ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  focus <- readIORef (ctxFocusId ctx)
  pure (focus == wid)

textAreaValue :: Context -> NodeIdx -> IO Text
textAreaValue ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  pure (IM.findWithDefault "" (intKey wid) (storeText store))

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
drawTextAreaSelection da ctx state geom host fm theme style = do
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
        accent = themeAccent theme
        selBg = lerpColor accent (styleBg style) 0.55
        loRow = TB.cursorRow lo
        loCol = TB.cursorCol lo
        hiRow = TB.cursorRow hi
        hiCol = TB.cursorCol hi
    forM_ [loRow .. hiRow] $ \row -> do
      let line =
            if row >= 0 && row < length lineTexts
              then lineTexts !! row
              else ""
          startCol =
            if row == loRow
              then loCol
              else 0
          endCol =
            if row == hiRow
              then hiCol
              else T.length line
      when (startCol < endCol) $ do
        (wLo, _) <- ctxMeasureText ctx (T.take startCol line)
        (wHi, _) <- ctxMeasureText ctx (T.take endCol line)
        let ly = contentTop + fromIntegral row * lineH - scrollYf
            selX = rectX field + ix + wLo
            selW = max 1 (wHi - wLo)
            selH = max 4 lineH
        pushRect da (Rect selX ly selW selH) selBg

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
              let (fm', shown) = pickMonoFont fm (ctxMonoFontMetrics ctx) line
              pushText da fm' contentX ly shown fg
        when focus $ do
          let TB.Cursor row col = TB.getCursor buf
              currentLine =
                if row >= 0 && row < length lineTexts
                  then lineTexts !! row
                  else ""
              prefix = T.take col currentLine
          (pw, _) <- ctxMeasureText ctx prefix
          let caretX = contentX + pw
              caretY = contentTop + fromIntegral row * lineH - scrollYf + 1
              caretH = max 4 (lineH - 2)
          pushRect da (Rect caretX caretY 1 caretH) fg

textAreaHitForWidget :: Context -> WidgetId -> IO (Maybe TextAreaHit)
textAreaHitForWidget ctx wid = do
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
  col <- textInputCharAtX ctx line (tahContentX hit) (v2X mouse)
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
          inField = rectContains (tahFieldRect hit) mouse
      if inputMousePressed inp && inField
        then do
          state <-
            loadTextAreaStateAt
              ctx
              (tahNodeIdx hit)
              (tahWidgetX hit)
              (tahWidgetY hit)
              (tahWidgetW hit)
              (tahWidgetH hit)
          (row, col) <- textAreaCursorAt ctx state hit mouse
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
          writeIORef (ctxTextInputDrag ctx) (Just (TextInputDrag wid 0 row col True clicks))
        else do
          mDrag <- readIORef (ctxTextInputDrag ctx)
          case mDrag of
            Just drag
              | textInputDragWidget drag == wid
                , textInputDragMultiline drag
                , inputMouseDown inp || inputMouseReleased inp -> do
                  state <-
                    loadTextAreaStateAt
                      ctx
                      (tahNodeIdx hit)
                      (tahWidgetX hit)
                      (tahWidgetY hit)
                      (tahWidgetW hit)
                      (tahWidgetH hit)
                  (row, col) <- textAreaCursorAt ctx state hit mouse
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
