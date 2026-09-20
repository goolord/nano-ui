{-# LANGUAGE DataKinds #-}

-- | Multi-line text areas: content painting (lines, selection, caret and
-- scrollbars) and mouse selection.
module NanoUI.Frame.TextArea
  ( TextAreaHit (..)
  , textAreaHitForWidget
  , drawTextAreaContentWith
  , finalizeTextAreaMouse
  , collapseTextAreaSelection
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (writeIORef)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import NanoUI.Context
  ( Context (..)
  , TextInputDrag (..)
  , getStore
  , intKey
  , markDirty
  , setStore
  , setTextInputDrag
  , slotKey
  , nodeTheme
  , getsInteraction
  , InteractionState (..)
  )
import NanoUI.Draw (DrawArena, getDrawSnapScale, pushText, withClip)
import NanoUI.Font (FontMetrics, caretXIO, prepareFontMetrics, selectionSpans, textIndexAtX, widgetContentInset)
import NanoUI.Frame.Chrome (paintScrollBarLayout, textInputFocused)
import NanoUI.Frame.Hit (withWidgetNode)
import NanoUI.Frame.TextArea.Content
  ( isMouseOnTextAreaScrollBarAt
  , resolveTextAreaFont
  , textAreaContentMetrics
  )
import NanoUI.Frame.TextArea.Geometry
import NanoUI.Frame.TextInput (drawTextCaret, drawTextSelectionLine, normalizeTextFieldClicks)
import NanoUI.Id (WidgetId)
import NanoUI.Input
  ( Input (..)
  , inputMouseClicks
  , inputMouseDown
  , inputMousePos
  , inputMousePressed
  , inputMouseReleased
  )
import NanoUI.Layout.Arena (NodeIdx, NodeType (NodeTextArea), getNodeType, getRect, getWidgetId)
import NanoUI.Store (Slot (..), fieldInt, fieldPoint, findSlot, insertSlot, lookupSlot)
import NanoUI.Style (Style (..), Theme, scrollBarThumbColor, scrollBarTrackColor, themePanel, themeSelection)
import NanoUI.Types (Rect (..), V2 (..), onGrid, rectContains)
import NanoUI.Widgets.TextArea (TextAreaState (..), loadTextAreaState, saveTextAreaState)
import qualified NanoUI.Widgets.TextArea as TA
import qualified NanoUI.Widgets.TextBuffer as TB
import NanoUI.Widgets.TextCommon (selectionCaretGeom, textWordBounds)

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

-- | Editor state of the text area at @idx@, its viewport set from the field
-- clip.
loadTextAreaStateAt :: Context -> NodeIdx -> FontMetrics -> Float -> Float -> Float -> Float -> IO TA.TextAreaState
loadTextAreaStateAt ctx idx fm x y w h = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  let key = intKey wid
  store <- getStore ctx
  let Rect _ _ vpW vpH = textAreaFieldClip fm (Rect x y w h)
      state0 = TA.loadTextAreaState store key
  pure (TA.setTextAreaViewport (realToFrac vpW, realToFrac vpH) (realToFrac (textAreaLineHeight fm)) state0)

loadHitState :: Context -> TextAreaHit -> IO TA.TextAreaState
loadHitState ctx hit = do
  fm <- resolveTextAreaFont ctx (tahNodeIdx hit)
  loadTextAreaStateAt ctx (tahNodeIdx hit) fm (tahWidgetX hit) (tahWidgetY hit) (tahWidgetW hit) (tahWidgetH hit)

-- | Record the text viewport and clamp the stored scroll to the content.
-- This paint already reflects both, so the write marks nothing dirty: a
-- window resize would otherwise request a second frame that has nothing to
-- repaint.
syncTextAreaViewport :: Context -> NodeIdx -> FontMetrics -> Float -> Float -> Float -> Float -> IO ()
syncTextAreaViewport ctx idx fm x y w h = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  (contentW, contentH) <- textAreaContentMetrics ctx idx
  -- Read after the metrics query: a cold query caches into the store.
  store <- getStore ctx
  let key = intKey wid
      Rect _ _ clipW clipH = textAreaFieldClip fm (Rect x y w h)
      bars = textAreaBars fm (Rect x y w h) contentW contentH
      scrollKey = slotKey SlotTextAreaScroll key
      (sx, sy) = findSlot fieldPoint (0, 0) scrollKey store
      sx' = max 0 (min (max 0 (contentW - tabViewW bars)) sx)
      sy' = max 0 (min (max 0 (contentH - tabViewH bars)) sy)
      viewportKey = slotKey SlotTextAreaViewport key
      clampScroll
        | sx' /= sx || sy' /= sy = insertSlot fieldPoint scrollKey (sx', sy')
        | otherwise = id
  unless (sx' == sx && sy' == sy && lookupSlot fieldPoint viewportKey store == Just (clipW, clipH)) $
    writeIORef (ctxStore ctx) $! clampScroll (insertSlot fieldPoint viewportKey (clipW, clipH) store)

-- | Snap a text-area scroll offset to the device pixel grid, the same grid
-- 'pushText' snaps to, so line pens and hit-testing stay in lockstep (and in
-- agreement with each other) while the text area scrolls. The raw 'Double'
-- offset keeps sub-pixel wheel deltas; only the applied value is quantized.
textAreaSnap :: DrawArena -> IO (Float -> Float)
textAreaSnap da = onGrid <$> getDrawSnapScale da

-- | The selection highlight on the rows between @firstRow@ and @lastRow@,
-- the ones in view.
drawTextAreaSelectionLines :: DrawArena -> Int -> Int -> TA.TextAreaState -> Rect -> FontMetrics -> Theme -> IO ()
drawTextAreaSelectionLines da firstRow lastRow state (Rect fieldX fieldY _ _) fm theme = do
  snap <- textAreaSnap da
  let anchor = TA.selectionAnchor state
      cursor = TB.getCursor (TA.buffer state)
  when (anchor /= cursor) $ do
    let (lo, hi) = TB.selectionRange anchor cursor
        lineH = textAreaLineHeight fm
        (ix, iy) = widgetContentInset fm
        (scrollX, scrollY) = TA.scrollOffset state
        scrollXf = snap (realToFrac scrollX)
        scrollYf = snap (realToFrac scrollY)
        contentTop = fieldY + iy
        selBg = themeSelection theme
        loRow = TB.cursorRow lo
        hiRow = TB.cursorRow hi
    forM_ [max loRow firstRow .. min hiRow lastRow] $ \row -> do
      let line = TB.lineAt row (TA.buffer state)
          clampCol c = max 0 (min (T.length line) c)
          startCol = clampCol (if row == loRow then TB.cursorCol lo else 0)
          endCol = clampCol (if row == hiRow then TB.cursorCol hi else T.length line)
      when (startCol < endCol) $ do
        prepared <- prepareFontMetrics fm line
        let ly = contentTop + fromIntegral row * lineH - scrollYf
        forM_ (selectionSpans prepared line startCol endCol) $ \(wLo, wHi) ->
          drawTextSelectionLine da (fieldX + ix + wLo - scrollXf) ly (wHi - wLo) (max 4 lineH) selBg

-- | Text-area content with the node font already resolved, so a paint pass
-- that also needs it (for the field frame) resolves it once.
drawTextAreaContentWith :: DrawArena -> Context -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextAreaContentWith da ctx fm idx x y w h style = do
  snap <- textAreaSnap da
  syncTextAreaViewport ctx idx fm x y w h
  focus <- textInputFocused ctx idx
  theme <- nodeTheme ctx idx
  let field = Rect x y w h
      lineH = textAreaLineHeight fm
      Rect clipX contentTop clipW clipH = textAreaFieldClip fm field
      fg = styleFg style
  state <- loadTextAreaStateAt ctx idx fm x y w h
  (contentW, contentH) <- textAreaContentMetrics ctx idx
  let buf = TA.buffer state
      (scrollX, scrollY) = TA.scrollOffset state
      scrollXf = snap (realToFrac scrollX)
      scrollYf = snap (realToFrac scrollY)
      contentX = clipX - scrollXf
      layouts = textAreaScrollBarLayouts fm field contentW contentH scrollXf scrollYf
      textClip =
        Rect
          clipX
          contentTop
          (if isJust (tasbVertical layouts) then max 0 (clipW - textAreaBarLane) else clipW)
          (if isJust (tasbHorizontal layouts) then max 0 (clipH - textAreaBarLane) else clipH)
      -- Only the rows in view are read, so painting costs the same however
      -- long the document is.
      rowAt py = floor ((py - contentTop + scrollYf) / max 1 lineH) :: Int
      firstRow = max 0 (rowAt y)
      lastRow = min (TB.getLineCount buf - 1) (rowAt (y + h))
  withClip da textClip $ do
    when focus $
      drawTextAreaSelectionLines da firstRow lastRow state field fm theme
    forM_ [firstRow .. lastRow] $ \row -> do
      let line = TB.lineAt row buf
          ly = contentTop + fromIntegral row * lineH - scrollYf
      unless (T.null line) $
        pushText da fm contentX ly line fg
    when focus $ do
      let TB.Cursor row col = TB.getCursor buf
          currentLine = TB.lineAt row buf
      pw <- caretXIO fm currentLine col
      let (caretX, caretY, caretH) = selectionCaretGeom contentX (contentTop + fromIntegral row * lineH - scrollYf) pw lineH
      drawTextCaret da caretX caretY caretH fg
  let base = themePanel theme
  mapM_
    (paintScrollBarLayout da (scrollBarTrackColor base theme) (scrollBarThumbColor base theme))
    (catMaybes [tasbVertical layouts, tasbHorizontal layouts])

textAreaHitForWidget :: Context -> WidgetId -> IO (Maybe TextAreaHit)
textAreaHitForWidget ctx wid = do
  withWidgetNode ctx wid Nothing $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    if nt /= NodeTextArea
      then pure Nothing
      else do
        (x, y, w, h) <- getRect (ctxNodeArena ctx) idx
        fm <- resolveTextAreaFont ctx idx
        let field = Rect x y w h
            Rect clipX _ _ _ = textAreaFieldClip fm field
        pure
          ( Just
              TextAreaHit
                { tahNodeIdx = idx
                , tahFieldRect = field
                , tahContentX = clipX
                , tahLineH = textAreaLineHeight fm
                , tahWidgetX = x
                , tahWidgetY = y
                , tahWidgetW = w
                , tahWidgetH = h
                }
          )

textAreaCursorAt :: Context -> TA.TextAreaState -> TextAreaHit -> V2 -> IO (Int, Int)
textAreaCursorAt ctx state hit (V2 mouseX mouseY) = do
  snap <- textAreaSnap (ctxDrawArena ctx)
  fm <- resolveTextAreaFont ctx (tahNodeIdx hit)
  let buf = TA.buffer state
      lineCount = max 1 (TB.getLineCount buf)
      (scrollX, scrollY) = TA.scrollOffset state
      scrollXf = snap (realToFrac scrollX)
      scrollYf = snap (realToFrac scrollY)
      (_, iy) = widgetContentInset fm
      Rect _ fieldY _ _ = tahFieldRect hit
      relY = mouseY - (fieldY + iy) + scrollYf
      row = max 0 (min (lineCount - 1) (floor (relY / max 1 (tahLineH hit))))
      line = TB.lineAt row buf
  prepared <- prepareFontMetrics fm line
  pure (row, textIndexAtX prepared line (max 0 (mouseX - (tahContentX hit - scrollXf))))

updateTextAreaSelection :: Context -> WidgetId -> TextAreaHit -> TB.Cursor -> TB.Cursor -> IO ()
updateTextAreaSelection ctx wid hit anchor cursor = do
  state0 <- loadHitState ctx hit
  store <- getStore ctx
  setStore ctx (TA.saveTextAreaState (intKey wid) (TA.setTextAreaSelection anchor cursor state0) store)
  markDirty ctx

applyTextAreaClick :: Context -> WidgetId -> TextAreaHit -> Int -> Int -> Int -> IO ()
applyTextAreaClick ctx wid hit row col clicks
  | clicks >= 3 = do
      state <- loadHitState ctx hit
      updateTextAreaSelection ctx wid hit (TB.Cursor 0 0) (TB.documentEnd (TA.buffer state))
  | clicks == 2 = do
      state <- loadHitState ctx hit
      let (lo, hi) = textWordBounds (TB.lineAt row (TA.buffer state)) col
      updateTextAreaSelection ctx wid hit (TB.Cursor row lo) (TB.Cursor row hi)
  | otherwise =
      updateTextAreaSelection ctx wid hit (TB.Cursor row col) (TB.Cursor row col)

applyTextAreaDrag :: Context -> WidgetId -> TextAreaHit -> Int -> Int -> Int -> Int -> Int -> IO ()
applyTextAreaDrag ctx wid hit anchorRow anchorCol row col clicks
  | clicks >= 3 = applyTextAreaClick ctx wid hit row col clicks
  | clicks == 2 = do
      state <- loadHitState ctx hit
      let buf = TA.buffer state
          (a0, a1) = textWordBounds (TB.lineAt anchorRow buf) anchorCol
          (c0, c1) = textWordBounds (TB.lineAt row buf) col
      updateTextAreaSelection ctx wid hit (TB.Cursor anchorRow (min a0 c0)) (TB.Cursor row (max a1 c1))
  | otherwise =
      updateTextAreaSelection ctx wid hit (TB.Cursor anchorRow anchorCol) (TB.Cursor row col)

-- | Mouse selection in text area @wid@: press (with word and document
-- multi-clicks) and drag. Presses on the scrollbars are left to the scroller.
finalizeTextAreaMouse :: Context -> Input -> WidgetId -> IO ()
finalizeTextAreaMouse ctx inp wid = do
  mHit <- textAreaHitForWidget ctx wid
  case mHit of
    Nothing -> pure ()
    Just hit -> do
      let mouse = inputMousePos inp
      onScroll <- isMouseOnTextAreaScrollBarAt ctx (tahNodeIdx hit) mouse
      let cursorAtMouse = do
            state <- loadHitState ctx hit
            textAreaCursorAt ctx state hit mouse
      if inputMousePressed inp && rectContains (tahFieldRect hit) mouse && not onScroll
        then do
          (row, col) <- cursorAtMouse
          clicks <- normalizeTextFieldClicks ctx wid 0 row col True (max 1 (inputMouseClicks inp))
          applyTextAreaClick ctx wid hit row col clicks
          setTextInputDrag ctx (Just (TextInputDrag wid 0 row col True clicks))
        else do
          mDrag <- getsInteraction ctx isTextInputDrag
          case mDrag of
            Just drag
              | textInputDragWidget drag == wid
                  , textInputDragMultiline drag
                  , inputMouseDown inp || inputMouseReleased inp -> do
                  (row, col) <- cursorAtMouse
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

collapseTextAreaSelection :: Context -> WidgetId -> IO ()
collapseTextAreaSelection ctx wid = do
  store <- getStore ctx
  let key = intKey wid
      row = findSlot fieldInt 0 (slotKey SlotTextAreaRow key) store
      col = findSlot fieldInt 0 (slotKey SlotTextAreaCol key) store
      state = loadTextAreaState store key
  setStore ctx (saveTextAreaState key state {selectionAnchor = TB.Cursor row col} store)
