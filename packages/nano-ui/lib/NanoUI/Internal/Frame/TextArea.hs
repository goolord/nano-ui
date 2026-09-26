-- | Multi-line text areas in the frame: their geometry (the field box,
-- content clip and which scrollbars show where), the node font and cached
-- content extent, content painting (lines, selection, caret and scrollbars)
-- and mouse selection. Also the dispatchers that pick between the two kinds
-- of text field.
module NanoUI.Internal.Frame.TextArea
  ( -- * Dispatch between field kinds
    finalizeTextFieldMouse
  , collapseTextFieldSelection
  , textWordBounds
  , TextInputArea (..)
  , textInputArea
    -- * Geometry
  , textAreaLineHeight
  , textAreaBarLane
  , TextAreaBars (..)
  , textAreaBars
  , textAreaBarLayouts
    -- * Content
  , resolveTextAreaFont
  , textAreaContentMetrics
  , textAreaScrollGeom
  , isMouseOnTextAreaScrollBarAt
  , TextAreaHit (..)
  , textAreaHitForWidget
    -- * Painting
  , drawTextAreaContentWith
  , textAreaTextPlacements
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawArena, getDrawSnapScale, pushText, withClip)
import NanoUI.Internal.Font
import NanoUI.Internal.Frame.Chrome (paintScrollBars, textInputFocused)
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollBarLayout (..), scrollBarLayout, scrollChromeLane)
import NanoUI.Internal.Frame.Node (nodeFontMetrics)
import NanoUI.Internal.Frame.TextInput (FieldDoc (..), Preedit (..), drawLineCaret, drawLinePreedit, drawLineSelection, fieldComposition, fieldEditLine, nodeTextFieldGeom, preeditSourceIndex, selectWithMouse, splicePreedit, textInputMouse, textWordBounds)
import NanoUI.Internal.Id (WidgetId, hashWidgetId)
import NanoUI.Internal.Input (Input, MouseButton (..), buttonHeld, buttonPressed, buttonReleased, inputMousePos)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Store (collapseFieldSelection, fieldFloat, fieldPoint, findSlot, insertDyn, insertSlot, lookupDyn, lookupSlot)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp, onGrid, rectContains)
import NanoUI.Internal.WidgetText (hasFlag, textInputFlagSelectable)
import qualified NanoUI.Internal.Widgets.TextArea as TA
import qualified NanoUI.Widgets.TextBuffer as TB

-- | Mouse selection in the focused field, whichever kind it is. A release
-- ends any drag. @onControl@: this frame's press is on a control drawn inside
-- the field ('NanoUI.Internal.Frame.Input.ptFieldControl').
finalizeTextFieldMouse :: Context -> Input -> Bool -> IO ()
finalizeTextFieldMouse ctx inp onControl = do
  focus <- readIORef (ctxFocusId ctx)
  when (hashWidgetId focus /= 0) $
    withWidgetNode ctx focus () $ \idx -> do
      getNodeType (ctxNodeArena ctx) idx >>= \case
        NodeTextInput -> textInputMouse ctx inp onControl focus idx
        -- An edited document is measured here, not in paint, whose cache
        -- write would damage and wake the next frame. Nothing else is read
        -- until the pointer acts.
        NodeTextArea -> do
          _ <- textAreaContentMetrics ctx idx
          when (buttonPressed MouseLeft inp || buttonHeld MouseLeft inp || buttonReleased MouseLeft inp) $
            textAreaMouse ctx inp focus idx
        _ -> pure ()
      -- A selection dragged past the field's edge scrolls a step a frame, as
      -- the caret follows the pointer. A pointer held still out there sends
      -- no input to run those frames, so ask for them while the drag lasts.
      when (buttonHeld MouseLeft inp) $ do
        mDrag <- getsInteraction ctx isTextInputDrag
        forM_ mDrag $ \drag ->
          when (textInputDragWidget drag == focus) $ do
            rect <- getNodeRect (ctxNodeArena ctx) idx
            unless (rectContains rect (inputMousePos inp)) $
              requestWakeAfter ctx (1 / 60)
  when (buttonReleased MouseLeft inp) $
    modifyInteraction ctx (\s -> s {isTextInputDrag = Nothing})

-- | Collapse selection in a current single-line or multiline field onto its
-- cursor. Zero, missing, and non-text widget ids do nothing.
collapseTextFieldSelection :: Context -> WidgetId -> IO ()
collapseTextFieldSelection ctx wid =
  withWidgetNode ctx wid () $ \idx ->
    getNodeType (ctxNodeArena ctx) idx >>= \case
      NodeTextInput -> modifyStore ctx (collapseFieldSelection key)
      NodeTextArea -> modifyStore ctx $ \store ->
        let state = TA.loadTextAreaState store key
         in TA.saveTextAreaState key state {TA.selectionAnchor = TB.getCursor (TA.buffer state)} store
      _ -> pure ()
 where
  key = intKey wid

-- | Where the focused text field is taking text, for an input method to put
-- its candidate window by, in logical window coordinates. See 'textInputArea'.
data TextInputArea = TextInputArea
  { textInputAreaRect :: !Rect
  -- ^ The text being composed, from where it starts to where it ends on its
  -- row, or a caret-wide rect at the caret while nothing is.
  , textInputAreaCursor :: !Float
  -- ^ How far right of the rect's left edge the caret is.
  }
  deriving (Eq, Show)

-- | Where the focused editable text field takes text ('TextInputArea'), or
-- 'Nothing' while none has the focus. A backend reads it after a frame and
-- passes it on to the input method (SDL's @SDL_SetTextInputArea@), which
-- puts its candidate window beside it rather than over the text. It follows
-- the field's scroll and the input method's composition, and a composition
-- in right-to-left text covers its runs as they are drawn.
textInputArea :: Context -> IO (Maybe TextInputArea)
textInputArea ctx@Context {ctxNodeArena = na} = do
  focus <- readIORef (ctxFocusId ctx)
  -- Widget id 0 names no widget: nothing has the focus.
  if hashWidgetId focus == 0
    then pure Nothing
    else withWidgetNode ctx focus Nothing $ \idx -> do
      store <- getStore ctx
      let key = intKey focus
      getNodeType na idx >>= \case
        NodeTextInput -> do
          si <- getStyleIdx na idx
          if hasFlag textInputFlagSelectable si
            then pure Nothing
            else do
              Rect x y w h <- getNodeRect na idx
              (Rect _ boxY _ boxH, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
              fm <- nodeFontMetrics ctx idx
              -- The scroll the frame settled; reading it writes nothing.
              let scrollX = findSlot fieldFloat 0 (slotKey SlotTextInputScroll key) store
              (line, caret, _, preedit) <- fieldEditLine ctx idx
              Just <$> lineInputArea fm line caret preedit (clipX - scrollX) (centeredTextY fm boxY boxH (fmLineHeight fm)) (fmLineHeight fm)
        NodeTextArea -> do
          fm <- resolveTextAreaFont ctx idx
          field <- getNodeRect na idx
          let state = TA.loadTextAreaState store key
              buf = TA.buffer state
              TB.Cursor caretRow caretCol = TB.getCursor buf
              lineH = textAreaLineHeight fm
              Rect clipX contentTop _ _ = textAreaFieldClip fm field
          (scrollXf, scrollYf) <- textAreaScrollSnapped (ctxDrawArena ctx) state
          preedit <- textAreaPreedit ctx focus state
          let (row, line, caret) = case preedit of
                Just (r, p) -> (r, preeditLine p, preeditCaret p)
                Nothing -> (caretRow, TB.lineAt caretRow buf, caretCol)
          Just <$> lineInputArea fm line caret (snd <$> preedit) (clipX - scrollXf) (contentTop + fromIntegral row * lineH - scrollYf) lineH
        _ -> pure Nothing

-- | The 'TextInputArea' of @line@, whose pen starts at @penX@, on a row at
-- @rowY@ of height @lineH@, with its caret before character @caret@.
lineInputArea :: FontMetrics -> T.Text -> Int -> Maybe Preedit -> Float -> Float -> Float -> IO TextInputArea
lineInputArea fm line caret preedit penX rowY lineH = do
  prepared <- prepareFontMetrics fm line
  let caretAt = penX + caretX prepared line caret
      runs = maybe [] (\p -> selectionSpans prepared line (preeditStart p) (preeditEnd p)) preedit
      (x0, x1)
        | null runs = (caretAt, caretAt + 1)
        | otherwise = (penX + minimum (map fst runs), penX + maximum (map snd runs))
  pure (TextInputArea (Rect x0 rowY (max 1 (x1 - x0)) lineH) (max 0 (caretAt - x0)))

-- | Text area row height, snapped to the device pixel grid.
textAreaLineHeight :: FontMetrics -> Float
textAreaLineHeight fm = onGrid (fmSnapScale fm) (fmLineHeight fm)

-- | Text clip of a text area field. A caption-less text area's field is its
-- whole node rect.
textAreaFieldClip :: FontMetrics -> Rect -> Rect
textAreaFieldClip fm (Rect fx fy fw fh) =
  let s = fmSnapScale fm
      (ix, iy) = widgetContentInset fm
   in Rect (fx + onGrid s ix) (fy + onGrid s iy) (max 0 (fw - 2 * ix)) (max 0 (fh - 2 * iy))

-- | Width of the vertical and height of the horizontal scrollbar lane.
textAreaBarLane :: Float
textAreaBarLane = fst (scrollBarGeomFor ScrollBarList) + scrollBarSideGap

-- | Which scrollbars a text area shows for its content extent, the offset
-- each axis reaches, the paddings that place each bar's lane, and the extent.
data TextAreaBars = TextAreaBars
  { tabVertical :: !Bool
  , tabHorizontal :: !Bool
  , tabRange :: {-# UNPACK #-} !V2
  , tabPadV :: !Padding
  , tabPadH :: !Padding
  , tabContent :: {-# UNPACK #-} !V2
  }

{-# INLINE textAreaBars #-}
textAreaBars :: FontMetrics -> Rect -> Float -> Float -> TextAreaBars
textAreaBars fm (Rect _ _ fw fh) contentW contentH =
  let (ix, iy) = widgetContentInset fm
      innerW = max 0 (fw - 2 * ix)
      innerH = max 0 (fh - 2 * iy)
      lane = textAreaBarLane
      -- Either bar's lane can push the other axis into overflow.
      hasV = contentH > (if contentW > innerW then max 0 (innerH - lane) else innerH)
      hasH = contentW > (if contentH > innerH then max 0 (innerW - lane) else innerW)
      viewW = if hasV then max 0 (innerW - lane) else innerW
      viewH = if hasH then max 0 (innerH - lane) else innerH
   in TextAreaBars
        { tabVertical = hasV
        , tabHorizontal = hasH
        , tabRange = V2 (max 0 (contentW - viewW)) (max 0 (contentH - viewH))
        , tabPadV = Padding 0 0 iy (if hasH then iy + lane else iy)
        , tabPadH = Padding ix (if hasV then ix + lane else ix) 0 0
        , tabContent = V2 contentW contentH
        }

-- | The vertical and the horizontal bar of the text area at @field@ at
-- offsets @scrollX scrollY@, in logical pixels; 'Nothing' for a bar it does
-- not show.
{-# INLINE textAreaBarLayouts #-}
textAreaBarLayouts :: Rect -> TextAreaBars -> Float -> Float -> (Maybe ScrollBarLayout, Maybe ScrollBarLayout)
textAreaBarLayouts (Rect x y w h) bars scrollX scrollY =
  let V2 contentW contentH = tabContent bars
      layout shown dir pad content off
        | shown = scrollBarLayout ScrollBarList dir x y w h pad content off
        | otherwise = Nothing
   in ( layout (tabVertical bars) DirColumn (tabPadV bars) contentH scrollY
      , layout (tabHorizontal bars) DirRow (tabPadH bars) contentW scrollX
      )

-- | Font the text-area content is laid out and painted in. Honors the node's
-- @layoutFontSize@ (set via 'fontSize' on the editor layout) so a single text
-- area can zoom without changing the rest of the UI. A size of 0 means the
-- base UI font.
resolveTextAreaFont :: Context -> NodeIdx -> IO FontMetrics
resolveTextAreaFont ctx idx = do
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  if size <= 0
    then pure (ctxFontMetrics ctx)
    else fst <$> ctxResolveFont ctx size WeightNormal FontStyleNormal FontRegular

-- | Content extent of a text area, @(contentWidth, contentHeight)@. Measuring
-- the width scans every character of the document, so the widths are kept
-- per line, with the extent, and only the lines an edit touched since they
-- were measured ('TB.changedLines') are measured again, or every line once the
-- node font or the metrics change.
textAreaContentMetrics :: Context -> NodeIdx -> IO (Float, Float)
textAreaContentMetrics ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  store <- getStore ctx
  let key = intKey wid
      widthsKey = slotKey SlotTextAreaWidths key
      buf = TA.textAreaBuffer store key
      lns = TB.bufferLines buf
      (seenHead, seenTail) = TB.changedLines buf
      cached = lookupDyn widthsKey store
  case cached of
    Just (LineWidths font _ _ _ widestW contentH)
      | font == size && seenHead >= Seq.length lns -> pure (widestW, contentH)
    _ -> do
      fm <- resolveTextAreaFont ctx idx
      gen <- readIORef (ctxMetricGen ctx)
      let contentH = fromIntegral (max 1 (Seq.length lns)) * textAreaLineHeight fm
          LineWidths _ _ measured widest widestW _ = case cached of
            Just lw@(LineWidths font fontGen _ _ _ _) | font == size && fontGen == gen -> lw
            _ -> LineWidths size gen Seq.empty (-1) 0 0
          -- Keep the widths of the lines no edit touched since the last
          -- measurement and measure the rest.
          keepHead = min seenHead (Seq.length measured)
          keepTail = min seenTail (Seq.length measured - keepHead)
          changed = Seq.take (Seq.length lns - keepHead - keepTail) (Seq.drop keepHead lns)
      fresh <- traverse (lineWidthIO fm) changed
      let widths = Seq.take keepHead measured <> fresh <> Seq.drop (Seq.length measured - keepTail) measured
          shift = Seq.length lns - Seq.length measured
          widestFrom :: Int -> Seq Float -> (Int, Float)
          widestFrom off = Seq.foldlWithIndex (\best i w -> if w > snd best then (off + i, w) else best) (-1, 0)
          freshWidest = widestFrom keepHead fresh
          -- The widest line so far still counts when it was kept; only when
          -- an edit touched it do the widths need a full pass.
          -- A changed line at least as wide as the old widest also still wins.
          (widest', contentW)
            | widest >= 0 && widest < keepHead = pick (widest, widestW) freshWidest
            | widest >= 0 && widest >= Seq.length measured - keepTail = pick (widest + shift, widestW) freshWidest
            | widest >= 0 && snd freshWidest >= widestW = freshWidest
            | otherwise = widestFrom 0 widths
          pick a b = if snd b > snd a then b else a
      modifyStore ctx $
        insertDyn widthsKey (LineWidths size gen widths widest' contentW contentH)
          . insertDyn (slotKey SlotTextAreaBuffer key) (TB.markLinesSeen buf)
      pure (contentW, contentH)

-- | Measured widths of a text area's lines, the font size and metric
-- generation they were measured at, the widest line with its width, and the
-- content height.
data LineWidths = LineWidths !Float !Int !(Seq Float) !Int !Float !Float

-- | Field rect and scrollbars of a text area, for its node font and content
-- extent. Zoom changes the node font, so scroll and hit math resolve it here
-- rather than using the base font, or the scroll range would clamp short.
textAreaScrollGeom :: Context -> NodeIdx -> IO (Rect, TextAreaBars)
textAreaScrollGeom ctx idx = do
  fm <- resolveTextAreaFont ctx idx
  field <- getNodeRect (ctxNodeArena ctx) idx
  (contentW, contentH) <- textAreaContentMetrics ctx idx
  pure (field, textAreaBars fm field contentW contentH)

-- | Whether @mouse@ is over a shown bar's lane or track. Uses the cached
-- content extent: this runs on every hover through the cursor query. Lanes
-- and tracks stay put as the text scrolls, so any offset places them.
isMouseOnTextAreaScrollBarAt :: Context -> NodeIdx -> V2 -> IO Bool
isMouseOnTextAreaScrollBarAt ctx idx mouse = do
  (field@(Rect x y w h), bars) <- textAreaScrollGeom ctx idx
  let (mV, mH) = textAreaBarLayouts field bars 0 0
      onBar dir pad =
        maybe False $ \layout ->
          rectContains (scrollChromeLane ScrollBarList dir x y w h pad) mouse
            || rectContains (sbTrack layout) mouse
  pure (onBar DirColumn (tabPadV bars) mV || onBar DirRow (tabPadH bars) mH)

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
      V2 sx' sy' = clampScrollOffset (tabRange bars) (V2 sx sy)
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
  wid <- getWidgetId (ctxNodeArena ctx) idx
  let key = intKey wid
      field = Rect x y w h
  (contentW, contentH) <- syncTextAreaViewport ctx idx key fm field
  focus <- textInputFocused ctx idx
  theme <- nodeTheme ctx idx
  state <- (`TA.loadTextAreaState` key) <$> getStore ctx
  (scrollXf, scrollYf) <- textAreaScrollSnapped da state
  preedit <- textAreaPreedit ctx wid state
  let lineH = textAreaLineHeight fm
      Rect clipX contentTop clipW clipH = textAreaFieldClip fm field
      fg = styleFg style
      buf = TA.buffer state
      cursor@(TB.Cursor caretRow caretCol) = TB.getCursor buf
      contentX = clipX - scrollXf
      rowY row = contentTop + fromIntegral row * lineH - scrollYf
      (mV, mH) = textAreaBarLayouts field (textAreaBars fm field contentW contentH) scrollXf scrollYf
      textClip = textClipBeside (Rect clipX contentTop clipW clipH) mV mH
      -- Only the rows in view are read, so painting costs the same however
      -- long the document is.
      rowAt py = floor ((py - contentTop + scrollYf) / max 1 lineH) :: Int
      firstRow = max 0 (rowAt y)
      lastRow = min (TB.getLineCount buf - 1) (rowAt (y + h))
      (lo, hi) = TB.selectionRange (TA.selectionAnchor state) cursor
      -- The selection sits on the unsnapped inset, as the pointer hit test does.
      (ix, iy) = widgetContentInset fm
  withClip da textClip $ do
    when (focus && (lo /= hi || isJust preedit)) $
      forM_ [max (TB.cursorRow lo) firstRow .. min (TB.cursorRow hi) lastRow] $ \row -> do
        let line = TB.lineAt row buf
            clampCol c = clamp 0 (T.length line) c
            -- On its row the composition takes the place of the selection,
            -- and the input method's selection shows instead.
            (shown, startCol, endCol) = case preedit of
              Just (prow, p) | prow == row -> (preeditLine p, preeditCaret p, preeditSelectionEnd p)
              _ ->
                ( line
                , clampCol (if row == TB.cursorRow lo then TB.cursorCol lo else 0)
                , clampCol (if row == TB.cursorRow hi then TB.cursorCol hi else T.length line)
                )
        when (startCol < endCol) $
          drawLineSelection da fm shown startCol endCol (x + ix - scrollXf) (y + iy + fromIntegral row * lineH - scrollYf) lineH (themeSelection theme)
    forM_ [firstRow .. lastRow] $ \row -> do
      let line = fst (shownRow preedit buf row)
      unless (T.null line) $
        pushText da fm contentX (rowY row) line fg
    when focus $
      case preedit of
        Just (row, p) -> drawLinePreedit da fm p contentX (rowY row) lineH fg
        Nothing -> drawLineCaret da fm (TB.lineAt caretRow buf) caretCol contentX (rowY caretRow) lineH fg
  paintScrollBars ctx da theme (themePanel theme) wid mV mH

-- | The composition an input method shows in text area @wid@
-- ('fieldComposition'): its row, and that row with the composition in place
-- of the selected part of it ('splicePreedit'). A selection over more rows
-- stays highlighted on the others, as committing replaces it all.
textAreaPreedit :: Context -> WidgetId -> TA.TextAreaState -> IO (Maybe (Int, Preedit))
textAreaPreedit ctx wid state = do
  composition <- fieldComposition ctx wid
  let buf = TA.buffer state
      (lo, hi) = TB.selectionRange (TA.selectionAnchor state) (TB.getCursor buf)
      row = TB.cursorRow lo
      line = TB.lineAt row buf
      endCol = if TB.cursorRow hi == row then TB.cursorCol hi else T.length line
  pure $ (\c -> (row, splicePreedit c line (TB.cursorCol lo) endCol)) <$> composition

-- | Row @row@ of a text area's document as it shows, with the composition in
-- it on its row ('textAreaPreedit'), and where a column of it is in the row.
shownRow :: Maybe (Int, Preedit) -> TB.TextBuffer -> Int -> (T.Text, Int -> Int)
shownRow (Just (prow, p)) _ row | prow == row = (preeditLine p, preeditSourceIndex p)
shownRow _ buf row = (TB.lineAt row buf, id)

-- | The clip a text area's rows are painted in: its content clip, short of
-- the lane of each scrollbar that shows.
textClipBeside :: Rect -> Maybe a -> Maybe b -> Rect
textClipBeside (Rect clipX contentTop clipW clipH) mV mH =
  Rect
    clipX
    contentTop
    (if isJust mV then max 0 (clipW - textAreaBarLane) else clipW)
    (if isJust mH then max 0 (clipH - textAreaBarLane) else clipH)

-- | Text area @idx@'s rows in view as text placements @(text, x, y, width,
-- height)@, and the clip they are painted in: what 'drawTextAreaContentWith'
-- draws, for hosts that draw text themselves. Empty rows are left out.
textAreaTextPlacements :: Context -> NodeIdx -> Rect -> IO (Rect, [(T.Text, Float, Float, Float, Float)])
textAreaTextPlacements ctx idx field@(Rect _ y _ h) = do
  fm <- resolveTextAreaFont ctx idx
  wid <- getWidgetId (ctxNodeArena ctx) idx
  let key = intKey wid
  (contentW, contentH) <- textAreaContentMetrics ctx idx
  state <- (`TA.loadTextAreaState` key) <$> getStore ctx
  (scrollXf, scrollYf) <- textAreaScrollSnapped (ctxDrawArena ctx) state
  preedit <- textAreaPreedit ctx wid state
  let lineH = textAreaLineHeight fm
      clip@(Rect clipX contentTop _ _) = textAreaFieldClip fm field
      (mV, mH) = textAreaBarLayouts field (textAreaBars fm field contentW contentH) scrollXf scrollYf
      buf = TA.buffer state
      rowAt py = floor ((py - contentTop + scrollYf) / max 1 lineH) :: Int
      rows = [max 0 (rowAt y) .. min (TB.getLineCount buf - 1) (rowAt (y + h))]
  placements <- forM [(row, line) | row <- rows, let line = fst (shownRow preedit buf row), not (T.null line)] $ \(row, line) -> do
    lw <- lineWidthIO fm line
    pure (line, clipX - scrollXf, contentTop + fromIntegral row * lineH - scrollYf, lw, lineH)
  pure (textClipBeside clip mV mH, placements)

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
    preedit <- textAreaPreedit ctx wid state
    let row = clamp 0 (TB.getLineCount buf - 1) (floor ((mouseY - (fieldY + iy) + scrollYf) / max 1 lineH))
        -- The pointer is over the row shown, composition and all, and lands
        -- in the text around it.
        (line, toSource) = shownRow preedit buf row
    prepared <- prepareFontMetrics fm line
    let pos = TB.Cursor row (toSource (textIndexAtX prepared line (max 0 (mouseX - (clipX - scrollXf)))))
    -- Nothing writes the store between this load and the selection write
    -- (click counting lives in the interaction state).
    pure $ FieldDoc pos buf $ \anchor cursor -> do
      modifyStore ctx (TA.saveTextAreaState key (TA.setTextAreaSelection anchor cursor state))
      markDirty ctx
