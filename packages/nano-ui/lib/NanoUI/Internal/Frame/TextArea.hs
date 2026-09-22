-- | Multi-line text areas: content painting (lines, selection, caret and
-- scrollbars) and mouse selection.
module NanoUI.Internal.Frame.TextArea
  ( TextAreaHit (..)
  , textAreaHitForWidget
  , drawTextAreaContentWith
  , textAreaMouse
  ) where

import Control.Monad (forM_, unless, when)
import Data.IORef (writeIORef)
import Data.Maybe (catMaybes, isJust)
import qualified Data.Text as T
import NanoUI.Internal.Context
  ( Context (..)
  , getStore
  , intKey
  , markDirty
  , modifyStore
  , slotKey
  , nodeTheme
  )
import NanoUI.Internal.Draw (DrawArena, getDrawSnapScale, pushText, withClip)
import NanoUI.Internal.Font (FontMetrics, prepareFontMetrics, textIndexAtX, widgetContentInset)
import NanoUI.Internal.Frame.Chrome (paintScrollBarLayout, textInputFocused)
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.TextArea.Content
import NanoUI.Internal.Frame.TextInput (FieldDoc (..), drawLineCaret, drawLineSelection, selectWithMouse)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (Input, inputMousePos)
import NanoUI.Internal.Layout.Arena (NodeIdx, NodeType (NodeTextArea), getNodeRect, getNodeType, getWidgetId)
import NanoUI.Internal.Store (Slot (..), fieldPoint, findSlot, insertSlot, lookupSlot)
import NanoUI.Internal.Style (Style (..), scrollBarThumbColor, scrollBarTrackColor, themePanel, themeSelection)
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp, onGrid)
import qualified NanoUI.Internal.Widgets.TextArea as TA
import qualified NanoUI.Widgets.TextBuffer as TB

-- | Solved text-area geometry: its node, field rect and resolved line height,
-- in logical pixels. The node index is valid only this frame.
data TextAreaHit = TextAreaHit
  { tahNodeIdx :: !NodeIdx
  , tahFieldRect :: !Rect
  , tahLineH :: !Float
  }

-- | Geometry and resolved line height for a current text-area node. Returns
-- 'Nothing' for a missing id or a different node type. Call after layout.
textAreaHitForWidget :: Context -> WidgetId -> IO (Maybe TextAreaHit)
textAreaHitForWidget ctx wid = do
  withWidgetNode ctx wid Nothing $ \idx -> do
    nt <- getNodeType (ctxNodeArena ctx) idx
    if nt /= NodeTextArea
      then pure Nothing
      else do
        field <- getNodeRect (ctxNodeArena ctx) idx
        fm <- resolveTextAreaFont ctx idx
        pure (Just (TextAreaHit idx field (textAreaLineHeight fm)))

-- | Record the text viewport and clamp the stored scroll to the content, and
-- return the content extent. This paint already reflects both, so the write
-- marks nothing dirty: a window resize would otherwise request a second frame
-- that has nothing to repaint.
syncTextAreaViewport :: Context -> NodeIdx -> Int -> FontMetrics -> Rect -> IO (Float, Float)
syncTextAreaViewport ctx idx key fm field = do
  extent@(contentW, contentH) <- textAreaContentMetrics ctx idx
  -- Read after the metrics query: a cold query caches into the store.
  store <- getStore ctx
  let Rect _ _ clipW clipH = textAreaFieldClip fm field
      bars = textAreaBars fm field contentW contentH
      scrollKey = slotKey SlotTextAreaScroll key
      (sx, sy) = findSlot fieldPoint (0, 0) scrollKey store
      sx' = clamp 0 (max 0 (contentW - tabViewW bars)) sx
      sy' = clamp 0 (max 0 (contentH - tabViewH bars)) sy
      viewportKey = slotKey SlotTextAreaViewport key
      clampScroll
        | sx' /= sx || sy' /= sy = insertSlot fieldPoint scrollKey (sx', sy')
        | otherwise = id
  unless (sx' == sx && sy' == sy && lookupSlot fieldPoint viewportKey store == Just (clipW, clipH)) $
    writeIORef (ctxStore ctx) $! clampScroll (insertSlot fieldPoint viewportKey (clipW, clipH) store)
  pure extent

-- | A text area's scroll offset snapped to the device pixel grid, the same
-- grid 'pushText' snaps to, so line pens and hit-testing stay in lockstep
-- (and in agreement with each other) while the text area scrolls. The raw
-- 'Double' offset keeps sub-pixel wheel deltas; only the applied value is
-- quantized.
{-# INLINE textAreaScrollSnapped #-}
textAreaScrollSnapped :: DrawArena -> TA.TextAreaState -> IO (Float, Float)
textAreaScrollSnapped da state = do
  s <- getDrawSnapScale da
  let (scrollX, scrollY) = TA.scrollOffset state
  pure (onGrid s (realToFrac scrollX), onGrid s (realToFrac scrollY))

-- | Text-area content with the node font already resolved, so a paint pass
-- that also needs it (for the field frame) resolves it once.
drawTextAreaContentWith :: DrawArena -> Context -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextAreaContentWith da ctx fm idx x y w h style = do
  key <- intKey <$> getWidgetId (ctxNodeArena ctx) idx
  let field = Rect x y w h
  (contentW, contentH) <- syncTextAreaViewport ctx idx key fm field
  focus <- textInputFocused ctx idx
  theme <- nodeTheme ctx idx
  state <- (`TA.loadTextAreaState` key) <$> getStore ctx
  (scrollXf, scrollYf) <- textAreaScrollSnapped da state
  let lineH = textAreaLineHeight fm
      Rect clipX contentTop clipW clipH = textAreaFieldClip fm field
      fg = styleFg style
      buf = TA.buffer state
      cursor@(TB.Cursor caretRow caretCol) = TB.getCursor buf
      contentX = clipX - scrollXf
      rowY row = contentTop + fromIntegral row * lineH - scrollYf
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
      (lo, hi) = TB.selectionRange (TA.selectionAnchor state) cursor
  withClip da textClip $ do
    when (focus && lo /= hi) $ do
      -- The selection sits on the unsnapped inset, as the pointer hit test does.
      let (ix, iy) = widgetContentInset fm
      forM_ [max (TB.cursorRow lo) firstRow .. min (TB.cursorRow hi) lastRow] $ \row -> do
        let line = TB.lineAt row buf
            clampCol c = clamp 0 (T.length line) c
            startCol = clampCol (if row == TB.cursorRow lo then TB.cursorCol lo else 0)
            endCol = clampCol (if row == TB.cursorRow hi then TB.cursorCol hi else T.length line)
        when (startCol < endCol) $
          drawLineSelection da fm line startCol endCol (x + ix - scrollXf) (y + iy + fromIntegral row * lineH - scrollYf) lineH (themeSelection theme)
    forM_ [firstRow .. lastRow] $ \row -> do
      let line = TB.lineAt row buf
      unless (T.null line) $
        pushText da fm contentX (rowY row) line fg
    when focus $
      drawLineCaret da fm (TB.lineAt caretRow buf) caretCol contentX (rowY caretRow) lineH fg
  let base = themePanel theme
  mapM_
    (paintScrollBarLayout da (scrollBarTrackColor base theme) (scrollBarThumbColor base theme))
    (catMaybes [tasbVertical layouts, tasbHorizontal layouts])

-- | Mouse selection in text area @wid@ at @idx@ ('selectWithMouse'). Presses
-- on its scrollbars are left to the scroller.
textAreaMouse :: Context -> Input -> WidgetId -> NodeIdx -> IO ()
textAreaMouse ctx inp wid idx = do
  field@(Rect _ fieldY _ _) <- getNodeRect (ctxNodeArena ctx) idx
  let mouse@(V2 mouseX mouseY) = inputMousePos inp
      key = intKey wid
  selectWithMouse ctx inp wid field (isMouseOnTextAreaScrollBarAt ctx idx mouse) $ do
    fm <- resolveTextAreaFont ctx idx
    store <- getStore ctx
    -- The editor keeps the caret in view, so give it the viewport it has.
    let Rect clipX _ vpW vpH = textAreaFieldClip fm field
        lineH = textAreaLineHeight fm
        state =
          TA.setTextAreaViewport (realToFrac vpW, realToFrac vpH) (realToFrac lineH) (TA.loadTextAreaState store key)
        buf = TA.buffer state
        (_, iy) = widgetContentInset fm
    (scrollXf, scrollYf) <- textAreaScrollSnapped (ctxDrawArena ctx) state
    let row = clamp 0 (TB.getLineCount buf - 1) (floor ((mouseY - (fieldY + iy) + scrollYf) / max 1 lineH))
        line = TB.lineAt row buf
    prepared <- prepareFontMetrics fm line
    let pos = TB.Cursor row (textIndexAtX prepared line (max 0 (mouseX - (clipX - scrollXf))))
    -- Nothing writes the store between this load and the selection write
    -- (click counting lives in the interaction state).
    pure $ FieldDoc pos buf $ \anchor cursor -> do
      modifyStore ctx (TA.saveTextAreaState key (TA.setTextAreaSelection anchor cursor state))
      markDirty ctx
