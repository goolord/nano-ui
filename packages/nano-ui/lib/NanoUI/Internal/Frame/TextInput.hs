-- | Single-line text fields: field geometry, horizontal scroll, caret and
-- selection painting, and mouse selection. Also holds the line painting, word
-- bounds and mouse selection the text area shares, and an input method's
-- composition, which both show.
module NanoUI.Internal.Frame.TextInput
  ( textInputFieldRect
  , nodeTextFieldGeom
  , tagTextInputClippedSpans
  , syncTextInputScroll
  , FieldEdit
  , readFieldEdit
  , fieldEditLine
  , drawTextInputSelection
  , drawTextInputCaret
  , drawLineSelection
  , drawLineCaret
  , drawLinePreedit
  , searchClearHit
  , textWordBounds
  , FieldDoc (..)
  , selectWithMouse
  , textInputMouse
    -- * Input-method composition
  , claimComposition
  , fieldComposition
  , Preedit (..)
  , splicePreedit
  , preeditSourceIndex
  ) where

import Control.Monad (forM_, mfilter, when)
import Data.Char (isAlphaNum, isSpace)
import Data.IORef (readIORef)
import Data.Maybe (isJust, mapMaybe)
import Data.Sequence qualified as Seq
import Data.Text (Text)
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawArena, pushRect)
import NanoUI.Internal.Font (FontMetrics (..), caretXIO, centeredTextY, lineWidthIO, prepareFontMetrics, selectionSpans, textIndexAtX, widgetContentInset)
import NanoUI.Internal.Frame.Chrome (textInputFocused, textInputValue)
import NanoUI.Internal.Frame.Hit (withWidgetNode)
import NanoUI.Internal.Frame.Node (nodeAdornmentInsets, nodeFontMetrics)
import NanoUI.Internal.Frame.Scroll.Geometry (padTextClipRect)
import NanoUI.Internal.Id (WidgetId (..), hashWidgetId)
import NanoUI.Internal.Input
import NanoUI.Internal.Layout.Arena (NodeIdx, NodeType (..), getNodeRect, getNodeType, getOptions, getStyleIdx, getWidgetId)
import NanoUI.Internal.Monad (ifM, unlessM, (<&&>))
import NanoUI.Internal.Store (fieldFloat, fieldSelection, fieldSelectionWrite, fieldText, findSlot, insertSlot, setFieldSelection)
import NanoUI.Internal.Style (themeSelection)
import NanoUI.Internal.Types (Color (..), DamageBounds (..), Rect (..), V2 (..), clamp, rectContains, rectIntersect, rectOverlapArea, rectW)
import NanoUI.Internal.WidgetText
import NanoUI.Widgets.TextBuffer qualified as TB

textInputFieldRect :: FontMetrics -> Float -> Float -> Float -> Float -> Rect
textInputFieldRect fm x y w h =
  let fieldH = if h > 0 then h else textInputFieldHeight fm
   in Rect x y w fieldH

-- | Where plain field @idx@'s text goes in its box @field@: inside its own
-- font's content inset and clear of its adornments.
plainFieldTextClip :: Context -> NodeIdx -> Rect -> IO Rect
plainFieldTextClip ctx idx (Rect fx fy fw fh) = do
  fm <- nodeFontMetrics ctx idx
  (lead, trail) <- nodeAdornmentInsets (ctxNodeArena ctx) idx fx fw
  let (ix, _) = widgetContentInset fm
  pure (textClipBetween fm (max ix lead) (max ix trail) fx fy fw fh)

-- | Resolve the box a field paints/hits and the clip its text is confined to.
-- Search fields are caption-less: the whole node rect is the box and text is
-- clipped around the magnifier / clear chrome. Combo boxes (search fields
-- carrying dropdown options) clip to the left of the chevron instead. A plain
-- field clips its text short of its adornments.
nodeTextFieldGeom :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO (Rect, Rect)
nodeTextFieldGeom ctx@Context {ctxFontMetrics = fm} idx x y w h = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  opts <- getOptions (ctxNodeArena ctx) idx
  let box = Rect x y w h
      field = textInputFieldRect fm x y w h
      geom
        | hasFlag textInputFlagSelectable si = pure (box, box)
        | hasFlag textInputFlagNumeric si = pure (box, numericTextClip fm x y w h)
        | hasFlag textInputFlagSearch si =
            pure (box, if null opts then searchInputTextClip fm x y w h else comboTextClip fm x y w h)
        | otherwise = (field,) <$> plainFieldTextClip ctx idx field
  geom

-- | Whether the pointer is over the clear (×) button of a non-empty search
-- field. Search fields reserve that slot even when empty, but the button is
-- only active when there is text to clear.
searchClearHit :: Context -> WidgetId -> V2 -> IO Bool
searchClearHit ctx wid mouse = do
  withWidgetNode ctx wid False $ \idx -> do
    si <- getStyleIdx (ctxNodeArena ctx) idx
    opts <- getOptions (ctxNodeArena ctx) idx
    pure (hasFlag textInputFlagSearch si && null opts)
      <&&> (not . T.null <$> textInputValue ctx idx)
      <&&> do
        Rect x y w h <- getNodeRect (ctxNodeArena ctx) idx
        let (_, clearRect) = searchInputIconRects (ctxFontMetrics ctx) x y w h
        pure (rectContains clearRect mouse)

-- | Clear a search field. The debounced pulse picks the empty text up as an
-- immediate (empty) commit on the next frame.
clearSearchInput :: Context -> WidgetId -> IO ()
clearSearchInput ctx wid = do
  let key = intKey wid
  modifyStore ctx $
    insertSlot fieldText key "" . setFieldSelection key 0 0
  markDirty ctx

-- | Clip a field's spans: its text to @fieldClip@ ('nodeTextFieldGeom'), and
-- a caption to its first line.
tagTextInputClippedSpans ::
  Rect -> Rect -> Float -> Float -> Float -> FontMetrics -> [(Rect, T.Text, Color, Color)] -> [(Rect, T.Text, Color, Color, Rect)]
tagTextInputClippedSpans parentClip fieldClip x y w fm spans =
  let labelClip = Rect x y w (fmLineHeight fm)
      tagOne (rect, txt, fg, bg) =
        let clipRect = padTextClipRect rect
            isField = rectOverlapArea fieldClip clipRect > rectOverlapArea labelClip clipRect
            area = if isField then fieldClip else labelClip
         in (rect, txt, fg, bg,) <$> (rectIntersect area clipRect >>= rectIntersect parentClip)
   in mapMaybe tagOne spans

-- | The selection highlight behind characters @lo@ to @hi@ of @line@, whose
-- pen starts at @x@, on a row at @y@ of height @lineH@.
drawLineSelection :: DrawArena -> FontMetrics -> Text -> Int -> Int -> Float -> Float -> Float -> Color -> IO ()
drawLineSelection da fm line lo hi x y lineH color = do
  prepared <- prepareFontMetrics fm line
  forM_ (selectionSpans prepared line lo hi) $ \(wLo, wHi) ->
    when (wHi > wLo) $
      pushRect da (Rect (x + wLo) y (max 1 (wHi - wLo)) (max 4 lineH)) color

-- | The caret before character @col@ of @line@, whose pen starts at @x@, on a
-- row at @y@ of height @lineH@.
drawLineCaret :: DrawArena -> FontMetrics -> Text -> Int -> Float -> Float -> Float -> Color -> IO ()
drawLineCaret da fm line col x y lineH fg = do
  pw <- caretXIO fm line col
  pushRect da (Rect (x + pw) (y + 1) 1 (max 4 (lineH - 2))) fg

-- | An input method's composition over the line it is shown in
-- ('preeditLine'), whose pen starts at @x@, on a row at @y@ of height
-- @lineH@: the composition underlined as text decoration is, and the input
-- method's caret when it selects nothing. What it selects is highlighted
-- behind the text instead ('drawLineSelection'). The underline follows the
-- text's direction runs, so a composition in right-to-left text is
-- underlined where it is drawn.
drawLinePreedit :: DrawArena -> FontMetrics -> Preedit -> Float -> Float -> Float -> Color -> IO ()
drawLinePreedit da fm p x y lineH fg = do
  let line = preeditLine p
  prepared <- prepareFontMetrics fm line
  let thick = max 1 (0.06 * lineH)
      underY = y + fmAscent fm + max 1 (0.1 * lineH)
  forM_ (selectionSpans prepared line (preeditStart p) (preeditEnd p)) $ \(wLo, wHi) ->
    pushRect da (Rect (x + wLo) underY (max 1 (wHi - wLo)) thick) fg
  when (preeditSelectionEnd p == preeditCaret p) $
    drawLineCaret da fm line (preeditCaret p) x y lineH fg

-- | The horizontal scroll that keeps field @idx@'s caret in view, zero while
-- it is unfocused, stored as it changes.
syncTextInputScroll :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO Float
syncTextInputScroll ctx idx x y w h = do
  si <- getStyleIdx (ctxNodeArena ctx) idx
  if hasFlag textInputFlagSelectable si
    then pure 0
    else do
      wid <- getWidgetId (ctxNodeArena ctx) idx
      store <- getStore ctx
      let key = intKey wid
      focus <- textInputFocused ctx idx
      -- An unfocused field shows its start, so only a focused one needs its clip.
      viewW <- if focus then rectW . snd <$> nodeTextFieldGeom ctx idx x y w h else pure 0
      let oldScroll = findSlot fieldFloat 0 (slotKey SlotTextInputScroll key) store
      newScroll <-
        if viewW <= 0
          then pure 0
          else do
            fm <- nodeFontMetrics ctx idx
            -- A composition shows in the field's text, and it is the input
            -- method's caret that stays in view.
            (value, cursor, _, _) <- fieldEditLine ctx idx
            caretRelX <- caretXIO fm value cursor
            totalTextW <- lineWidthIO fm value
            let s0
                  | caretRelX < oldScroll = caretRelX
                  | caretRelX + 1 > oldScroll + viewW = caretRelX + 1 - viewW
                  | otherwise = oldScroll
            pure (clamp 0 (max 0 (totalTextW + 1 - viewW)) s0)
      when (newScroll /= oldScroll) $
        setStore ctx (insertSlot fieldFloat (slotKey SlotTextInputScroll key) newScroll store)
      pure newScroll

-- | What a focused single-line field paints its selection and caret from: the
-- displayed value, cursor and anchor, the node font, the top of its text row,
-- the x its text starts at with the scroll applied, and the composition an
-- input method shows in it.
data FieldEdit = FieldEdit !Text !Int !Int !FontMetrics !Float !Float !(Maybe Preedit)

-- | Editing state of field @idx@, whose box is @box@ ('nodeTextFieldGeom')
-- and whose text starts at @penX@, scroll applied, or Nothing while it is
-- unfocused.
readFieldEdit :: Context -> NodeIdx -> Rect -> Float -> IO (Maybe FieldEdit)
readFieldEdit ctx idx (Rect _ boxY _ boxH) penX = do
  focus <- textInputFocused ctx idx
  if not focus
    then pure Nothing
    else do
      (value, cursor, anchor, preedit) <- fieldEditLine ctx idx
      fm <- nodeFontMetrics ctx idx
      pure $! Just (FieldEdit value cursor anchor fm (centeredTextY fm boxY boxH (fmLineHeight fm)) penX preedit)

-- | The line single-line field @idx@ shows, its displayed value
-- ('textInputValue'), with its cursor and anchor in it. While an input
-- method composes in the field ('fieldComposition'), the composition takes
-- the place of the selection, which is what committing it replaces, and the
-- cursor and anchor are the input method's caret and the end of its
-- selection.
fieldEditLine :: Context -> NodeIdx -> IO (Text, Int, Int, Maybe Preedit)
fieldEditLine ctx idx = do
  value <- textInputValue ctx idx
  wid <- getWidgetId (ctxNodeArena ctx) idx
  store <- getStore ctx
  -- Lazy: a caller after the text alone reads no selection.
  let (anchor, cursor) = fieldSelection store (intKey wid) value
  fieldComposition ctx wid >>= \case
    Nothing -> pure (value, cursor, anchor, Nothing)
    Just c -> do
      si <- getStyleIdx (ctxNodeArena ctx) idx
      -- A password field masks what is composed as it masks its value.
      let shown
            | hasFlag textInputFlagPassword si = c {compositionText = T.replicate (T.length (compositionText c)) "*"}
            | otherwise = c
          p = splicePreedit shown value (min anchor cursor) (max anchor cursor)
      pure (preeditLine p, preeditCaret p, preeditSelectionEnd p, Just p)

drawTextInputSelection :: DrawArena -> Context -> NodeIdx -> FieldEdit -> IO ()
drawTextInputSelection da ctx idx (FieldEdit value cursor anchor fm rowY textX _) =
  when (anchor /= cursor) $ do
    theme <- nodeTheme ctx idx
    drawLineSelection da fm value (min anchor cursor) (max anchor cursor) textX rowY (fmLineHeight fm) (themeSelection theme)

-- | The field's caret, or while an input method composes in it, the
-- composition's underline and the input method's caret ('drawLinePreedit').
drawTextInputCaret :: DrawArena -> FieldEdit -> Color -> IO ()
drawTextInputCaret da (FieldEdit value cursor _ fm rowY textX preedit) fg =
  case preedit of
    Just p -> drawLinePreedit da fm p textX rowY (fmLineHeight fm) fg
    Nothing -> drawLineCaret da fm value cursor textX rowY (fmLineHeight fm) fg

data CharClass = WordChar | SpaceChar | OtherChar
  deriving (Eq)

-- | The run of word characters (letters, digits, underscores), spaces or
-- other characters around character @raw@ of @text@, clamped into it.
-- Positions count characters, not UTF-8 bytes.
textWordBounds :: Text -> Int -> (Int, Int)
textWordBounds text raw
  | T.null text = (0, 0)
  | otherwise =
      -- Split once: repeatedly indexing UTF-8 text makes long-word selection
      -- quadratic. The clamped index guarantees a non-empty suffix.
      let i = clamp 0 (T.length text - 1) raw
          (before, after) = T.splitAt i text
          sameClass = (== charClass (T.head after)) . charClass
       in ( i - T.length (T.takeWhileEnd sameClass before)
          , i + T.length (T.takeWhile sameClass after)
          )
 where
  charClass c
    | isAlphaNum c || c == '_' = WordChar
    | isSpace c = SpaceChar
    | otherwise = OtherChar

-- | A field's document at the pointer: the position under it, the document,
-- and how the field stores a selection (anchor, then cursor).
data FieldDoc = FieldDoc !TB.Cursor !TB.TextBuffer (TB.Cursor -> TB.Cursor -> IO ())

-- | Mouse selection in focused field @wid@: a press in @box@, with word and
-- whole-document multi-clicks, and the drag it starts. On a press
-- @chromePress@ runs first and takes the press when it lands on the field's
-- own chrome. @atMouse@ reads the document at the pointer.
selectWithMouse :: Context -> Input -> WidgetId -> Rect -> IO Bool -> IO FieldDoc -> IO ()
selectWithMouse ctx inp wid box chromePress atMouse
  | buttonPressed MouseLeft inp && rectContains box (inputMousePos inp) =
      unlessM chromePress $ do
        FieldDoc pos buf select <- atMouse
        -- A press counts as a multi-click only on the cell of the press before.
        prev <- getsInteraction ctx isTextFieldClickCell
        let cell = Just (TextFieldClickCell wid pos)
            clicks = if prev == cell then max 1 (inputMouseClicks inp) else 1
        uncurry select (dragSelection buf pos pos clicks)
        modifyInteraction ctx $ \s ->
          s {isTextFieldClickCell = cell, isTextInputDrag = Just (TextInputDrag wid pos clicks)}
  | buttonHeld MouseLeft inp || buttonReleased MouseLeft inp =
      getsInteraction ctx isTextInputDrag >>= \case
        Just (TextInputDrag dragWid anchor clicks) | dragWid == wid -> do
          FieldDoc pos buf select <- atMouse
          uncurry select (dragSelection buf anchor pos clicks)
        _ -> pure ()
  | otherwise = pure ()

-- | The selection a drag from @anchor@ to @pos@ makes after @clicks@ clicks:
-- characters, whole words, or the whole document. A click is a drag that has
-- not moved.
dragSelection :: TB.TextBuffer -> TB.Cursor -> TB.Cursor -> Int -> (TB.Cursor, TB.Cursor)
dragSelection buf anchor@(TB.Cursor ar ac) pos@(TB.Cursor r c) clicks
  | clicks >= 3 = (TB.Cursor 0 0, TB.documentEnd buf)
  | clicks == 2 =
      let (a0, a1) = textWordBounds (TB.lineAt ar buf) ac
          (c0, c1) = textWordBounds (TB.lineAt r buf) c
       in (TB.Cursor ar (min a0 c0), TB.Cursor r (max a1 c1))
  | otherwise = (anchor, pos)

-- | Mouse selection in single-line field @wid@ at @idx@ ('selectWithMouse'),
-- and a search field's clear button. A press on a control drawn inside the
-- field (@onControl@) is the control's, and leaves the caret alone.
textInputMouse :: Context -> Input -> Bool -> WidgetId -> NodeIdx -> IO ()
textInputMouse ctx inp onControl wid idx = do
  Rect x y w h <- getNodeRect (ctxNodeArena ctx) idx
  (box, Rect clipX _ _ _) <- nodeTextFieldGeom ctx idx x y w h
  scrollX <- syncTextInputScroll ctx idx x y w h
  let mouse@(V2 mouseX _) = inputMousePos inp
      key = intKey wid
      clearPress = ifM (searchClearHit ctx wid mouse) (True <$ clearSearchInput ctx wid) (pure False)
  selectWithMouse ctx inp wid box (if onControl then pure True else clearPress) $ do
    fm <- nodeFontMetrics ctx idx
    value <- textInputValue ctx idx
    -- The pointer is over the text shown, composition and all, and lands in
    -- the value around it.
    (shown, _, _, preedit) <- fieldEditLine ctx idx
    prepared <- prepareFontMetrics fm shown
    let atX = textIndexAtX prepared shown (max 0 (mouseX - (clipX - scrollX)))
        pos = TB.Cursor 0 (maybe atX (`preeditSourceIndex` atX) preedit)
    pure $ FieldDoc pos (TB.fromLines (Seq.singleton value)) $ \(TB.Cursor _ anchor) (TB.Cursor _ cursor) ->
      writeSlots ctx (fieldSelectionWrite key value anchor cursor)

-- | Settle which text field the frame's composition ('inputComposition')
-- shows in, before the view runs: the enabled, editable field that had the
-- focus when the composition last changed, while it keeps the focus. A
-- composition the focus left shows in no field until the input method
-- changes it, so it never turns up in the next one. Repaints the field it
-- leaves and the one it reaches.
--
-- Returns the input the frame runs on, and whether the input method has the
-- focused field's keys ('FocusComposing'). While the composition shows, they
-- are the input method's: the frame drops them, pressed, released and held,
-- so none edits the field or fires a shortcut. Its commit still arrives as
-- typed text. In the frame it commits, the keys it passes on, such as an
-- arrow after a Korean syllable, reach the field but fire no shortcut.
--
-- Inlined into the one frame step that runs it, which then builds no pair.
{-# INLINE claimComposition #-}
claimComposition :: Context -> Input -> IO (Input, Bool)
claimComposition ctx !inp = do
  focus <- readIORef (ctxFocusId ctx)
  held <- getsInteraction ctx isComposition
  let new = mfilter (not . T.null . compositionText) (inputComposition inp)
      -- An unchanged composition stays with its field, a new one goes to the
      -- focused field.
      claimant = case held of
        Just (old, o) | Just old == new -> if o == focus then o else WidgetId 0
        _ -> focus
      showing = maybe False (\(_, o) -> o == focus && hashWidgetId o /= 0)
  !owner <- if isJust new then ifM (editableField claimant) (pure claimant) (pure (WidgetId 0)) else pure (WidgetId 0)
  let next = (,owner) <$> new
  when (next /= held) $ do
    modifyInteraction ctx (\s -> s {isComposition = next})
    mapM_ (\(_, old) -> damageWidget ctx old DamageSelf) held
    damageWidget ctx owner DamageSelf
  let !committing = showing held && not (T.null (inputChars inp))
  pure $
    if showing next
      then (inp {inputKeys = mempty, inputKeysReleased = mempty, inputKeysHeld = mempty}, True)
      else (inp, committing)
  where
    editableField wid
      | hashWidgetId wid == 0 = pure False
      | otherwise = withWidgetNode ctx wid False $ \idx ->
          ( getNodeType (ctxNodeArena ctx) idx >>= \case
              NodeTextArea -> pure True
              NodeTextInput -> not . hasFlag textInputFlagSelectable <$> getStyleIdx (ctxNodeArena ctx) idx
              _ -> pure False
          )
            <&&> (not <$> isDisabled ctx wid)

-- | The composition showing in text field @wid@: the frame's, while @wid@
-- has the focus and the composition belongs to it ('claimComposition').
fieldComposition :: Context -> WidgetId -> IO (Maybe Composition)
fieldComposition ctx wid = do
  focus <- readIORef (ctxFocusId ctx)
  getsInteraction ctx $ \s -> case isComposition s of
    Just (c, owner) | owner == wid && owner == focus && hashWidgetId owner /= 0 -> Just c
    _ -> Nothing

-- | A line of text with a composition in place of some of its characters:
-- what a field shows while an input method composes in it. Positions count
-- characters of 'preeditLine'.
data Preedit = Preedit
  { preeditLine :: !Text
  , preeditStart :: !Int
  , preeditEnd :: !Int
  -- ^ Where the composition starts and ends.
  , preeditCaret :: !Int
  , preeditSelectionEnd :: !Int
  -- ^ The input method's caret, or its selection from the caret to here.
  , preeditReplaced :: !Int
  -- ^ How many characters of the original line the composition stands in for.
  }

-- | @line@ with composition @c@ in place of its characters @lo@ to @hi@:
-- the selection a commit replaces, or the caret with none. The positions are
-- clamped into the line.
splicePreedit :: Composition -> Text -> Int -> Int -> Preedit
splicePreedit (Composition txt cursor sel) line lo0 hi0 =
  let n = T.length line
      lo = max 0 (min n lo0)
      hi = max lo (min n hi0)
      len = T.length txt
      caret = lo + max 0 (min len cursor)
   in Preedit
        { preeditLine = T.take lo line <> txt <> T.drop hi line
        , preeditStart = lo
        , preeditEnd = lo + len
        , preeditCaret = caret
        , preeditSelectionEnd = min (lo + len) (caret + max 0 sel)
        , preeditReplaced = hi - lo
        }

-- | The position in the original line of position @i@ in the shown one. A
-- position inside the composition is where the composition starts.
preeditSourceIndex :: Preedit -> Int -> Int
preeditSourceIndex p i
  | i <= preeditStart p = i
  | i >= preeditEnd p = i - (preeditEnd p - preeditStart p) + preeditReplaced p
  | otherwise = preeditStart p
