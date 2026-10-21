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

import Control.Monad (forM, forM_, guard, unless, when)
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawArena, getDrawSnapScale, pushText, withClip)
import NanoUI.Internal.Font
import NanoUI.Internal.Frame.Chrome (paintScrollBars, textInputFocused)
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.Scroll.Geometry (ScrollBarLayout (..), scrollBarLayoutIn, scrollChromeLane)
import NanoUI.Internal.Frame.Node (nodeFontMetrics, packedTextFont, resolveTextFont)
import NanoUI.Internal.Frame.TextInput (FieldDoc (..), Preedit (..), drawCaretAt, drawLineCaret, drawLinePreedit, drawLineSelection, fieldEditLine, nodeTextFieldGeom, preeditSourceIndex, selectWithMouse, splicePreedit, textInputMouse, textWordBounds)
import NanoUI.Internal.Id (WidgetId, hashWidgetId)
import NanoUI.Internal.Input (Input, InputPurpose, MouseButton (..), Pressable (..), inputMousePos)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Store (collapseFieldSelection, deleteSlot, fieldFloat, fieldInt, fieldQuiet, fieldPoint, fieldBuffer, fieldLineWidths, findSlot, insertSlot, lookupSlot, memberSlot)
import NanoUI.Internal.Store.Types (LineWidths (..))
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Rect (..), V2 (..), clamp, onGrid, rectContains)
import NanoUI.Internal.WidgetText (textNodeFontKey)
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
          when (pressedIn MouseLeft inp || heldIn MouseLeft inp || releasedIn MouseLeft inp) $
            textAreaMouse ctx inp onControl focus idx
        _ -> pure ()
      -- A selection dragged past the field's edge scrolls a step a frame, as
      -- the caret follows the pointer. A pointer held still out there sends
      -- no input to run those frames, so ask for them while the drag lasts.
      when (heldIn MouseLeft inp) $ do
        mDrag <- getsInteraction ctx isTextInputDrag
        forM_ mDrag $ \drag ->
          when (textInputDragWidget drag == focus) $ do
            rect <- getNodeRect (ctxNodeArena ctx) idx
            unless (rectContains rect (inputMousePos inp)) $
              requestWakeAfter ctx (1 / 60)
  when (releasedIn MouseLeft inp) $
    modifyInteraction ctx (\s -> s {isTextInputDrag = Nothing})

-- | Collapse selection in a current single-line or multiline field onto its
-- cursor. Zero, missing, and non-text widget ids do nothing.
collapseTextFieldSelection :: Context -> WidgetId -> IO ()
collapseTextFieldSelection ctx wid =
  withWidgetNode ctx wid () $ \idx ->
    getNodeType (ctxNodeArena ctx) idx >>= \case
      NodeTextInput -> modifyStore ctx (collapseFieldSelection key)
      -- An unstored anchor reads as the cursor ('TA.loadTextAreaState').
      NodeTextArea ->
        modifyStore ctx $
          deleteSlot fieldInt (slotKey SlotTextAreaAnchorRow key) . deleteSlot fieldInt (slotKey SlotTextAreaAnchorCol key)
      _ -> pure ()
 where
  key = intKey wid

-- | Where the focused widget takes text, in logical window coordinates, so
-- the input method can place its candidate window. See 'textInputArea'.
data TextInputArea = TextInputArea
  { textInputAreaRect :: !Rect
  -- ^ The composed text's extent on its row, or a caret-wide rect when
  -- nothing is composed.
  , textInputAreaCursor :: !Float
  -- ^ Caret offset from the rect's left edge.
  , textInputAreaPurpose :: !InputPurpose
  -- ^ Kind of input, which selects an on-screen keyboard layout.
  }
  deriving (Eq, Show)

-- | The text input area of the widget that requested the input method last
-- view, or 'Nothing'. Focused text fields request it, as do custom widgets
-- via 'NanoUI.useInputMethod'. Backends read it after each frame to start or
-- stop text input and pass it on (SDL's @SDL_SetTextInputArea@) so the
-- candidate window sits beside the text, not over it. For text fields it
-- tracks scroll and composition, including right-to-left runs.
textInputArea :: Context -> IO (Maybe TextInputArea)
textInputArea ctx@Context {ctxNodeArena = na} =
  readIORef (ctxInputMethod ctx) >>= \case
    Nothing -> pure Nothing
    Just (InputMethodRequest _ (Just caret) purpose) -> pure (Just (TextInputArea caret 0 purpose))
    Just (InputMethodRequest wid Nothing purpose) -> withWidgetNode ctx wid Nothing $ \idx -> do
      store <- getStore ctx
      let key = intKey wid
      getNodeType na idx >>= \case
        NodeTextInput -> do
          Rect x y w h <- getNodeRect na idx
          (Rect _ boxY _ boxH, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
          fm <- nodeFontMetrics ctx idx
          -- Read the scroll the frame already settled, without writing it.
          let scrollX = findSlot fieldFloat 0 (slotKey SlotTextInputScroll key) store
          (line, caret, _, preedit) <- fieldEditLine ctx idx
          Just <$> lineInputArea fm line caret preedit (clipX - scrollX) (centeredTextY fm boxY boxH (fmLineHeight fm)) (fmLineHeight fm) purpose
        NodeTextArea -> do
          fm <- resolveTextAreaFont ctx idx
          field <- getNodeRect na idx
          let state = TA.loadTextAreaState store key
              buf = TA.tasBuffer state
              TB.Cursor caretRow caretCol = TB.getCursor buf
              lineH = textAreaLineHeight fm
              Rect clipX contentTop _ _ = textAreaFieldClip fm field
          (scrollXf, scrollYf) <- textAreaScrollSnapped (ctxDrawArena ctx) state
          preedit <- textAreaPreedit ctx wid state
          let (row, line, caret) = case preedit of
                Just (r, p) -> (r, preeditLine p, preeditCaret p)
                Nothing -> (caretRow, TB.lineAt caretRow buf, caretCol)
          Just <$> lineInputArea fm line caret (snd <$> preedit) (clipX - scrollXf) (contentTop + fromIntegral row * lineH - scrollYf) lineH purpose
        _ -> pure Nothing

-- | 'TextInputArea' for @line@ drawn from @penX@ on a row at @rowY@ of
-- height @lineH@, caret before character @caret@.
lineInputArea :: FontMetrics -> T.Text -> Int -> Maybe Preedit -> Float -> Float -> Float -> InputPurpose -> IO TextInputArea
lineInputArea fm line caret preedit penX rowY lineH purpose = do
  prepared <- prepareFontMetrics fm line
  let caretAt = penX + caretX prepared line caret
      runs = maybe [] (\p -> selectionSpans prepared line (preeditStart p) (preeditEnd p)) preedit
      (x0, x1)
        | null runs = (caretAt, caretAt + 1)
        | otherwise = (penX + minimum (map fst runs), penX + maximum (map snd runs))
  pure (TextInputArea (Rect x0 rowY (max 1 (x1 - x0)) lineH) (max 0 (caretAt - x0)) purpose)

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
-- The extent across is a pixel wider than the widest line, for the caret
-- after its last character.
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
textAreaBars fm (Rect _ _ fw fh) widestW contentH =
  let (ix, iy) = widgetContentInset fm
      contentW = widestW + 1
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
      padV = tabPadV bars
      padH = tabPadH bars
      -- The pads only place the bars: the text has no trailing padding to
      -- scroll past, so each bar's range is its axis's 'tabRange'.
      layout shown dir pad view content off
        | shown = scrollBarLayoutIn ScrollBarList dir (Rect x y w h) pad view 0 content off
        | otherwise = Nothing
   in ( layout (tabVertical bars) DirColumn padV (h - padT padV - padB padV) contentH scrollY
      , layout (tabHorizontal bars) DirRow padH (w - padL padH - padR padH) contentW scrollX
      )

-- | Font the text-area content is laid out and painted in. Honors the
-- node's font size and the face, weight and slant packed in its style (set
-- via 'fontSize', 'fontMono' and the like on the editor layout) so a single
-- text area can zoom or set code without changing the rest of the UI. A size
-- of 0 means the base UI font's.
resolveTextAreaFont :: Context -> NodeIdx -> IO FontMetrics
resolveTextAreaFont ctx idx = do
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  fst <$> resolveTextFont ctx (packedTextFont size si)

-- | Content extent of a text area, @(contentWidth, contentHeight)@. Measuring
-- the width scans every character of the document, so the widths are kept
-- per line, with the extent, and only the lines an edit touched since they
-- were measured ('TB.changedLines') are measured again, or every line once the
-- node font or the metrics change.
textAreaContentMetrics :: Context -> NodeIdx -> IO (Float, Float)
textAreaContentMetrics ctx idx = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  size <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  store <- getStore ctx
  gen <- readIORef (ctxMetricGen ctx)
  let key = intKey wid
      face = textNodeFontKey size si
      widthsKey = slotKey SlotTextAreaWidths key
      buf = TA.textAreaBuffer store key
      lns = TB.bufferLines buf
      (seenHead, seenTail) = TB.changedLines buf
      cached = lookupSlot fieldLineWidths widthsKey store
  case cached of
    Just (LineWidths font fontGen _ _ widestW contentH)
      | font == face && fontGen == gen && seenHead >= Seq.length lns -> pure (widestW, contentH)
    _ -> do
      fm <- resolveTextAreaFont ctx idx
      let contentH = fromIntegral (max 1 (Seq.length lns)) * textAreaLineHeight fm
          LineWidths _ _ measured widest widestW _ = case cached of
            Just lw@(LineWidths font fontGen _ _ _ _) | font == face && fontGen == gen -> lw
            _ -> LineWidths face gen Seq.empty (-1) 0 0
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
        insertSlot fieldLineWidths widthsKey (LineWidths face gen widths widest' contentW contentH)
          . insertSlot fieldBuffer (slotKey SlotTextAreaBuffer key) (TB.markLinesSeen buf)
      pure (contentW, contentH)

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
-- return the content extent. A saved editor state ('SlotTextAreaReveal') has
-- its caret, 1 px wide, scrolled into view across, which takes the font, as
-- 'syncTextInputScroll' does for a field; its x is returned too, for drawing
-- the caret. This paint already reflects all of it, so the write marks
-- nothing dirty: a window resize would otherwise request a second frame that
-- has nothing to repaint.
syncTextAreaViewport :: Context -> NodeIdx -> Int -> FontMetrics -> Rect -> IO ((Float, Float), Maybe Float)
syncTextAreaViewport ctx idx key fm field = do
  extent@(contentW, contentH) <- textAreaContentMetrics ctx idx
  -- Read after the metrics query: a cold query caches into the store.
  store <- getStore ctx
  let bars = textAreaBars fm field contentW contentH
      Rect _ _ viewW viewH = textAreaTextClip fm field bars
      scrollKey = slotKey SlotTextAreaScroll key
      revealKey = slotKey SlotTextAreaReveal key
      (sx, sy) = findSlot fieldPoint (0, 0) scrollKey store
      reveal = memberSlot fieldQuiet revealKey store
  caretAt <-
    if reveal
      then do
        let buf = TA.textAreaBuffer store key
            TB.Cursor row col = TB.getCursor buf
        Just <$> caretXIO fm (TB.lineAt row buf) col
      else pure Nothing
  let V2 sx' sy' = clampScrollOffset (tabRange bars) (V2 (maybe sx (\at -> clamp (at + 1 - viewW) at sx) caretAt) sy)
      viewportKey = slotKey SlotTextAreaViewport key
      lineKey = slotKey SlotTextAreaLineH key
      lineH = textAreaLineHeight fm
      scrolled = sx' /= sx || sy' /= sy
      resized = lookupSlot fieldPoint viewportKey store /= Just (viewW, viewH)
      relined = lookupSlot fieldFloat lineKey store /= Just lineH
  -- Commands run between frames keep the caret in view with the viewport
  -- and the row height. Only what changed is written: every keystroke clears
  -- the reveal flag.
  when (reveal || scrolled || resized || relined) $
    writeIORef (ctxStore ctx) $!
      deleteSlot fieldQuiet revealKey
        . (if scrolled then insertSlot fieldPoint scrollKey (sx', sy') else id)
        . (if relined then insertSlot fieldFloat lineKey lineH else id)
        $ (if resized then insertSlot fieldPoint viewportKey (viewW, viewH) store else store)
  -- A glide aimed past a range that shrank (the text got shorter) would
  -- never land, since the offset is clamped back here every frame.
  wid <- getWidgetId (ctxNodeArena ctx) idx
  clampScrollGlide ctx wid (tabRange bars)
  pure (extent, caretAt)


-- | A text area's scroll offset snapped to the device pixel grid, the same
-- grid 'pushText' snaps to, so line pens and hit-testing stay in lockstep
-- (and in agreement with each other) while the text area scrolls. The raw
-- 'Double' offset keeps sub-pixel wheel deltas; only the applied value is
-- quantized.
{-# INLINE textAreaScrollSnapped #-}
textAreaScrollSnapped :: DrawArena -> TA.TextAreaState -> IO (Float, Float)
textAreaScrollSnapped da state = do
  s <- getDrawSnapScale da
  let (scrollX, scrollY) = TA.tasScrollOffset state
  pure (onGrid s (realToFrac scrollX), onGrid s (realToFrac scrollY))

-- | Text-area content with the node font already resolved, so a paint pass
-- that also needs it (for the field frame) resolves it once.
drawTextAreaContentWith :: DrawArena -> Context -> FontMetrics -> NodeIdx -> Float -> Float -> Float -> Float -> Style -> IO ()
drawTextAreaContentWith da ctx fm idx x y w h style = do
  wid <- getWidgetId (ctxNodeArena ctx) idx
  let key = intKey wid
      field = Rect x y w h
  ((contentW, contentH), caretAt) <- syncTextAreaViewport ctx idx key fm field
  focus <- textInputFocused ctx idx
  theme <- nodeTheme ctx idx
  state <- (`TA.loadTextAreaState` key) <$> getStore ctx
  (scrollXf, scrollYf) <- textAreaScrollSnapped da state
  preedit <- textAreaPreedit ctx wid state
  -- The layout's 'fontColor', as a text field's and the span path's.
  fg <- fromMaybe (styleFg style) <$> getNodeFontColor (ctxNodeArena ctx) idx
  let lineH = textAreaLineHeight fm
      Rect clipX contentTop clipW clipH = textAreaFieldClip fm field
      buf = TA.tasBuffer state
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
      (lo, hi) = TB.selectionRange (TA.tasSelectionAnchor state) cursor
  withClip da textClip $ do
    when (focus && (lo /= hi || isJust preedit)) $
      forM_ [max (TB.cursorRow lo) firstRow .. min (TB.cursorRow hi) lastRow] $ \row -> do
        let line = TB.lineAt row buf
            clampCol c = clamp 0 (T.length line) c
            -- On the composition row, show the input method's selection
            -- instead of the field's.
            (shown, startCol, endCol) = case preedit of
              Just (prow, p) | prow == row -> (preeditLine p, preeditCaret p, preeditSelectionEnd p)
              _ ->
                ( line
                , clampCol (if row == TB.cursorRow lo then TB.cursorCol lo else 0)
                , clampCol (if row == TB.cursorRow hi then TB.cursorCol hi else T.length line)
                )
        when (startCol < endCol) $
          drawLineSelection da fm shown startCol endCol contentX (rowY row) lineH (themeSelection theme)
    forM_ [firstRow .. lastRow] $ \row -> do
      let line = fst (shownRow preedit buf row)
      unless (T.null line) $
        pushText da fm contentX (rowY row) line fg
    when focus $
      case preedit of
        Just (row, p) -> drawLinePreedit da fm p contentX (rowY row) lineH fg
        -- Measured already when the sync scrolled it into view.
        Nothing -> case caretAt of
          Just at -> drawCaretAt da (contentX + at) (rowY caretRow) lineH fg
          Nothing -> drawLineCaret da fm (TB.lineAt caretRow buf) caretCol contentX (rowY caretRow) lineH fg
  paintScrollBars ctx da theme (themePanel theme) wid mV mH

-- | Text area @wid@'s composition ('fieldComposition'): its row, and that
-- row with the composition spliced over the selection ('splicePreedit'). A
-- multi-row selection stays highlighted on other rows, since a commit
-- replaces all of it.
textAreaPreedit :: Context -> WidgetId -> TA.TextAreaState -> IO (Maybe (Int, Preedit))
textAreaPreedit ctx wid state = do
  composition <- fieldComposition ctx wid
  let buf = TA.tasBuffer state
      (lo, hi) = TB.selectionRange (TA.tasSelectionAnchor state) (TB.getCursor buf)
      row = TB.cursorRow lo
      line = TB.lineAt row buf
      endCol = if TB.cursorRow hi == row then TB.cursorCol hi else T.length line
  pure $ (\c -> (row, splicePreedit c line (TB.cursorCol lo) endCol)) <$> composition

-- | Row @row@ as displayed, composition included ('textAreaPreedit'), with
-- a map from displayed columns to document columns.
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

-- | 'textClipBeside' for the text area at @field@ showing @bars@: the
-- viewport the editor keeps the caret in.
textAreaTextClip :: FontMetrics -> Rect -> TextAreaBars -> Rect
textAreaTextClip fm field bars =
  textClipBeside (textAreaFieldClip fm field) (guard (tabVertical bars)) (guard (tabHorizontal bars))

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
      buf = TA.tasBuffer state
      rowAt py = floor ((py - contentTop + scrollYf) / max 1 lineH) :: Int
      rows = [max 0 (rowAt y) .. min (TB.getLineCount buf - 1) (rowAt (y + h))]
  placements <- forM [(row, line) | row <- rows, let line = fst (shownRow preedit buf row), not (T.null line)] $ \(row, line) -> do
    lw <- lineWidthIO fm line
    pure (line, clipX - scrollXf, contentTop + fromIntegral row * lineH - scrollYf, lw, lineH)
  pure (textClipBeside clip mV mH, placements)

-- | Mouse selection in text area @wid@ at @idx@ ('selectWithMouse'). Presses
-- on its scrollbars are left to the scroller, and a press on a control drawn
-- inside it (@onControl@) to the control.
textAreaMouse :: Context -> Input -> Bool -> WidgetId -> NodeIdx -> IO ()
textAreaMouse ctx inp onControl wid idx = do
  field <- getNodeRect (ctxNodeArena ctx) idx
  let mouse@(V2 mouseX mouseY) = inputMousePos inp
      key = intKey wid
  selectWithMouse ctx inp wid field (if onControl then pure True else isMouseOnTextAreaScrollBarAt ctx idx mouse) $ do
    fm <- resolveTextAreaFont ctx idx
    (contentW, contentH) <- textAreaContentMetrics ctx idx
    store <- getStore ctx
    -- The editor keeps the caret in view, so give it the viewport it has.
    let Rect clipX contentTop vpW vpH = textAreaTextClip fm field (textAreaBars fm field contentW contentH)
        lineH = textAreaLineHeight fm
        state =
          TA.setTextAreaViewport (realToFrac vpW, realToFrac vpH) (realToFrac lineH) (TA.loadTextAreaState store key)
        buf = TA.tasBuffer state
    (scrollXf, scrollYf) <- textAreaScrollSnapped (ctxDrawArena ctx) state
    preedit <- textAreaPreedit ctx wid state
    let row = clamp 0 (TB.getLineCount buf - 1) (floor ((mouseY - contentTop + scrollYf) / max 1 lineH))
        -- Hit-test the displayed row, then map back to a document column.
        (line, toSource) = shownRow preedit buf row
    prepared <- prepareFontMetrics fm line
    let pos = TB.Cursor row (toSource (textIndexAtX prepared line (max 0 (mouseX - (clipX - scrollXf)))))
    -- Nothing writes the store between this load and the selection write
    -- (click counting lives in the interaction state). A selection that is
    -- already stored is not written again: the write would damage the area
    -- and wake the loop on every frame a button is held still.
    pure $ FieldDoc pos buf $ \anchor cursor ->
      unless (anchor == TA.tasSelectionAnchor state && cursor == TB.getCursor buf && TB.cursorCol cursor == TB.preferredCol buf) $
        modifyStore ctx (TA.saveTextAreaState key (TA.setTextAreaSelection anchor cursor state))
