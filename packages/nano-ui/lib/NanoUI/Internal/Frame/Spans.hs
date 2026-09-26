-- | Collect positioned and clipped text for external renderers and headless tests.
module NanoUI.Internal.Frame.Spans
  ( collectTextSpans
  , collectOverlayTextSpans
  , collectRasterSpans
  , widgetNodeCount
  , plainFieldPen
  , textInputFg
  , forWidgetTextPlacements_
  , selectableTextGeometry
  , textNodeSpanEntry
  ) where

import Control.Monad (forM, forM_, unless, when)
import Data.IORef (readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import NanoUI.Internal.Context
import NanoUI.Internal.Damage (floatingPanelRects)
import NanoUI.Internal.Font
import NanoUI.Internal.Frame.Chrome (displayText, textInputFocused, textInputValue, widgetVisualStyle)
import NanoUI.Internal.Frame.Node (nodeAdornmentInsets, readScrollNode, resolveFontFor)
import NanoUI.Internal.Frame.Scroll.Geometry (padContentClip, padTextClipRect, scrollNodeViewport, tagClippedSpans)
import NanoUI.Internal.Frame.Select (collectSelectDropdownSpans)
import NanoUI.Internal.Frame.SpanArena (SpanArena, pushSpans, resetSpanArena, spanArenaToList)
import NanoUI.Internal.Frame.TextArea (textAreaTextPlacements)
import NanoUI.Internal.Frame.TextEdit (collectTextEditMenuSpans)
import NanoUI.Internal.Frame.TextInput (fieldEditLine, nodeTextFieldGeom, syncTextInputScroll, tagTextInputClippedSpans)
import NanoUI.Internal.Input (Input)
import NanoUI.Internal.Layout.Arena
import NanoUI.Internal.Layout.Solve (findAncestorMaxW, textWrapCap)
import NanoUI.Internal.Style (AlignX (..), FontVariant (..), Style (..), Theme (..), themeAccent, themeMuted, themePanel)
import NanoUI.Internal.Types (Color (..), Rect (..), lerpColor, onGrid, rectIntersect)
import NanoUI.Internal.Widgets.ColorPicker (ColorPickerPart (..), colorPickerPartOf, colorPickerPartRect, colorPickerPreviewGeom)
import NanoUI.Internal.WidgetText

-- | Collect page text after layout, omitting spans covered by opaque floating
-- panels. Each tuple is bounds, text, foreground, background, clip; coordinates
-- are logical window coordinates. Rebuilds the context's base span arena.
collectTextSpans :: Context -> IO [(Rect, T.Text, Color, Color, Rect)]
collectTextSpans ctx@Context {ctxSpanBase = arena} = do
  count <- arenaCount (ctxNodeArena ctx)
  resetSpanArena arena
  when (count > 0) $
    collectClippedSpans ctx 0 (Rect 0 0 1e9 1e9) arena
  panels <- floatingPanelRects ctx
  spanArenaToList panels arena

-- | Collect window, modal, popup, dropdown, and edit-menu text in paint order.
-- Uses the same tuple format as 'collectTextSpans' and rebuilds the overlay arena.
collectOverlayTextSpans :: Context -> Input -> IO [(Rect, T.Text, Color, Color, Rect)]
collectOverlayTextSpans ctx@Context {ctxSpanOverlay = arena} inp = do
  resetSpanArena arena
  mapM_ (\nt -> collectFloatingSpansInto ctx nt arena) [NodeWindow, NodeModal, NodePopup]
  collectSelectDropdownSpans ctx inp >>= pushSpans arena
  collectTextEditMenuSpans ctx inp >>= pushSpans arena
  spanArenaToList IM.empty arena

-- | Collect base and overlay text separately for a host that rasterises text itself.
collectRasterSpans :: Context -> Input -> IO ([(Rect, T.Text, Color, Color, Rect)], [(Rect, T.Text, Color, Color, Rect)])
collectRasterSpans ctx inp = (,) <$> collectTextSpans ctx <*> collectOverlayTextSpans ctx inp

-- | Total live arena nodes, including containers and decorative nodes.
widgetNodeCount :: Context -> IO Int
widgetNodeCount ctx = arenaCount (ctxNodeArena ctx)

-- | Spans of node @idx@ and its subtree inside @clip@, floating subtrees left
-- out.
collectClippedSpans :: Context -> NodeIdx -> Rect -> SpanArena -> IO ()
collectClippedSpans ctx@Context {ctxFontMetrics = fm} idx clip arena = do
  nt <- getNodeType (ctxNodeArena ctx) idx
  unless (isFloatingNode nt) $ do
    Rect x y w h <- getNodeRect (ctxNodeArena ctx) idx
    mClipChildren <-
      if isScrollNode nt
        then
          getClipRect (ctxNodeArena ctx) idx >>= \case
            Just live -> pure (rectIntersect clip live)
            Nothing -> (\sn -> rectIntersect clip (scrollNodeViewport sn x y w h)) <$> readScrollNode (ctxNodeArena ctx) idx
        else pure (if nt == NodePanel then rectIntersect clip (Rect x y w h) else Just clip)
    forM_ mClipChildren $ \clipHere -> do
      -- A text node's spans are cached per node until its inputs change.
      -- Placement uses glyph ink ('alignedTextPen'), not TTF_GetStringSize;
      -- wrapping still measures with the host so line breaks stay on the TTF
      -- width.
      spans <-
        if nt == NodeText
          then sceSpans <$> textNodeSpanEntry ctx idx x y w h
          else if isWidgetNode nt then widgetTextSpans ctx nt idx x y w h else pure []
      here <-
        case nt of
          -- A select's label shares one clip, short of its chevron.
          NodeSelect -> do
            let (ix, _) = widgetContentInset fm
                label = padTextClipRect (Rect (x + ix) y (max 0 (w - ix - selectChevronReserve)) (max 0 h))
            pure [(r, t, fg, bg, c) | Just c <- [rectIntersect clipHere label], (r, t, fg, bg) <- spans]
          NodeTextInput -> do
            si <- getStyleIdx (ctxNodeArena ctx) idx
            if hasFlag textInputFlagSelectable si
              then pure (tagClippedSpans clipHere spans)
              else do
                -- The text clip paint uses ('nodeTextFieldGeom').
                (_, fieldClip) <- nodeTextFieldGeom ctx idx x y w h
                pure $
                  if hasFlag textInputFlagNumeric si
                    then maybe [] (`tagClippedSpans` spans) (rectIntersect clipHere fieldClip)
                    else tagTextInputClippedSpans clipHere fieldClip x y w fm spans
          -- Rows are clipped as paint clips them, short of the scrollbars.
          NodeTextArea -> do
            (textClip, _) <- textAreaTextPlacements ctx idx (Rect x y w h)
            pure (maybe [] (`tagClippedSpans` spans) (rectIntersect clipHere textClip))
          _ -> pure (tagClippedSpans clipHere spans)
      pushSpans arena here
      kids <- getFirstChild (ctxNodeArena ctx) idx
      -- A widget clips its children, as paint does.
      unless (kids < 0) $
        forM_ (if isWidgetNode nt then rectIntersect clipHere (Rect x y w h) else Just clipHere) $ \childClip ->
          walkChildSpans ctx idx childClip arena

walkChildSpans :: Context -> NodeIdx -> Rect -> SpanArena -> IO ()
walkChildSpans ctx idx clip arena = getFirstChild (ctxNodeArena ctx) idx >>= go
  where
    go ci
      | ci < 0 = pure ()
      | otherwise = do
          ns <- getNextSibling (ctxNodeArena ctx) ci
          -- Later siblings paint under earlier ones; walk reverse then collect.
          go ns
          collectClippedSpans ctx ci clip arena

-- | Text node @idx@'s span cache entry at @(x, y)@, @w@ by @h@, brought up to
-- date: its spans, and the metrics prepared for each line, which paint draws
-- with.
textNodeSpanEntry :: Context -> NodeIdx -> Float -> Float -> Float -> Float -> IO SpanCacheEntry
textNodeSpanEntry ctx@Context {ctxNodeArena = arena} idx x y w h = do
  theme <- nodeTheme ctx idx
  raw <- getText arena idx
  si <- getStyleIdx arena idx
  mCustomCol <- getNodeFontColor arena idx
  fontSize <- getNodeFontSize arena idx
  ax <- getAlignX arena idx
  AxisSizing wTag _ _ maxW <- getWidthSizing arena idx
  isRowChild <- parentIsRow arena idx
  -- 'nodeTextLines' wraps a row child only at its own newlines, so
  -- only then does the ancestor cap matter; skip the walk to the root.
  effMaxW <-
    if maxW < 1e8 || (isRowChild && not (T.any (== '\n') raw))
      then pure maxW
      else findAncestorMaxW arena idx
  let rect = Rect x y w h
      mStripe = tableStripeColor theme si
      variantFg = case textNodeFontVariant si of
        FontHeading -> themeAccent theme
        FontMuted -> themeMuted theme
        FontDanger -> themeRed theme
        FontWarning -> themeWarning theme
        _ -> styleFg (themePanel theme)
      fg = fromMaybe variantFg mCustomCol
      bg = fromMaybe (styleBg (themePanel theme)) mStripe
      !ix = if isJust mStripe then tableCellInset else 0
      -- What the lines depend on: everything but where the node is and
      -- how tall, and the colours. Inlined at both uses, so a cache hit
      -- allocates no closure for it.
      {-# INLINE sameLines #-}
      sameLines e =
        sceText e == raw
          && sceStyle e == si
          && sceFontSize e == fontSize
          && sceWidthTag e == fromEnum wTag
          && rectW (sceRect e) == w
          && sceEffMaxW e == effMaxW
          && sceRowChild e == isRowChild
          && sceInset e == ix
  cache <- readIORef (ctxSpanCache ctx)
  case IM.lookup idx cache of
    Just e
      | sameLines e
          && sceRect e == rect
          && sceFg e == fg
          && sceBg e == bg
          && sceAlign e == fromEnum ax ->
          pure e
    mEntry -> do
      (fm, textLines) <- case mEntry of
        Just e | sameLines e -> pure (sceFont e, sceLines e)
        _ -> nodeTextLines ctx raw si fontSize wTag maxW effMaxW isRowChild ix w
      let spans = [(r, line, fg, bg) | (r, line) <- placeSpanLines ax fm ix x y w h textLines]
          entry =
            SpanCacheEntry
              { sceText = raw
              , sceFg = fg
              , sceBg = bg
              , sceStyle = si
              , sceFontSize = fontSize
              , sceAlign = fromEnum ax
              , sceWidthTag = fromEnum wTag
              , sceRect = rect
              , sceEffMaxW = effMaxW
              , sceRowChild = isRowChild
              , sceInset = ix
              , sceFont = fm
              , sceLines = textLines
              , sceSpans = spans
              }
      writeIORef (ctxSpanCache ctx) $ IM.insert idx entry cache
      pure entry

-- | A text node's lines for width @w@ and the font they are set in: wrapped
-- where the node wraps (through the context's wrap cache, which the solve
-- shares), otherwise one line, truncated to fit a node that grows or has a
-- maximum width.
nodeTextLines ::
  Context -> T.Text -> Int -> Float -> SizingTag -> Float -> Float -> Bool -> Float -> Float -> IO (FontMetrics, SpanLines)
nodeTextLines ctx raw si fontSize wTag maxW effMaxW isRowChild ix w
  | T.null raw = pure (ctxFontMetrics ctx, SpanWrapped [])
  | otherwise = do
      (fm, _, measure) <- resolveFontFor ctx NodeText fontSize si
      let measureW = fmap fst . measure
          contentW = max 0 (w - 2 * ix)
          wrapCap = textWrapCap effMaxW wTag w
      tw <- measureW raw
      if T.any (== '\n') raw || (not isRowChild && wrapCap < 1e8 && wrapCap + 0.5 < tw)
        then do
          wrap <- cachedWrapText ctx (textNodeFontKey fontSize si) measureW raw (max 0 (wrapCap - 2 * ix))
          prepared <- forM (wrLines wrap) $ \line -> (,) line <$> prepareFontMetrics fm line
          pure (fm, SpanWrapped prepared)
        else do
          shown <-
            if tw > contentW && contentW > 0 && (wTag == SizingGrow || maxW < 1e8)
              then truncateTextIO measureW contentW raw
              else pure raw
          (,) fm . SpanSingle shown <$> prepareFontMetrics fm shown

-- | Where the lines of a text node at @(x, y)@, @w@ by @h@, go.
placeSpanLines :: AlignX -> FontMetrics -> Float -> Float -> Float -> Float -> Float -> SpanLines -> [(Rect, T.Text)]
placeSpanLines ax fm ix x y w h = \case
  SpanWrapped textLines ->
    [ (Rect tx ty used lineH, line)
    | (i, (line, prepared)) <- zip [(0 :: Int) ..] textLines
    , let (tx, used) = alignedTextPen ax x w ix prepared line
          ty = centeredTextY fm (y + onGrid (fmSnapScale fm) (fromIntegral i * lineH)) lineH lineH
    ]
  SpanSingle shown prepared ->
    let (tx, used) = alignedTextPen ax x w ix prepared shown
     in [(Rect tx (centeredTextY fm y h lineH) used lineH, shown)]
  where
    lineH = fmLineHeight fm

widgetTextSpans ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(Rect, T.Text, Color, Color)]
widgetTextSpans ctx nt idx x y w h = do
  style <- widgetVisualStyle ctx nt idx
  placements <-
    -- A centred label's placement depends on its text, style, font, alignment
    -- and size but not its origin, so it is cached. Field, picker and slider
    -- text depends on their data.
    if hasCenteredLabel nt
      then do
        (lx, lw, ax) <- labelBox ctx nt idx x w
        placement <- cachedWidgetLabel ctx nt idx ax lw h
        pure [(txt, lx + px, y + py, tw, th) | Just (WidgetTextPlacement txt px py tw th) <- [placement]]
      else computeWidgetTextPlacements ctx nt idx x y w h
  let bg = styleBg style
  case nt of
    NodeTextInput -> do
      fg <- textInputFg ctx style idx =<< textInputFocused ctx idx
      pure [(Rect px py tw th, txt, fg, bg) | (txt, px, py, tw, th) <- placements]
    _ -> do
      fg <- fromMaybe (styleFg style) <$> getNodeFontColor (ctxNodeArena ctx) idx
      pure [(Rect px py tw th, txt, fg, bg) | (txt, px, py, tw, th) <- placements, not (T.null txt)]

-- | The colour of a text input's text: the node's font colour, else the
-- style's, faded toward the background while the field is empty and
-- unfocused.
textInputFg :: Context -> Style -> NodeIdx -> Bool -> IO Color
textInputFg ctx style idx focus = do
  fg <- fromMaybe (styleFg style) <$> getNodeFontColor (ctxNodeArena ctx) idx
  value <- textInputValue ctx idx
  pure (if T.null value && not focus then lerpColor fg (styleBg style) 0.40 else fg)

-- | Runtime consumer API. The Bool marks the last placement (for table sort
-- arrows); cached labels are translated directly into the consumer.
{-# INLINE forWidgetTextPlacements_ #-}
forWidgetTextPlacements_ ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float ->
  (Bool -> T.Text -> Float -> Float -> Float -> Float -> IO ()) -> IO ()
forWidgetTextPlacements_ ctx nt idx x y w h emit
  | hasCenteredLabel nt = do
      (lx, lw, ax) <- labelBox ctx nt idx x w
      placement <- cachedWidgetLabel ctx nt idx ax lw h
      forM_ placement $ \(WidgetTextPlacement txt px py tw th) ->
        emit True txt (lx + px) (y + py) tw th
  | otherwise = do
      placements <- computeWidgetTextPlacements ctx nt idx x y w h
      let go [] = pure ()
          go ((txt, px, py, tw, th) : rest) =
            emit (null rest) txt px py tw th >> go rest
      go placements

-- | The span of widget @idx@, at @x@ and @w@ wide, its label is placed in,
-- and how a button's label aligns there: beside adornments, the room between
-- them, the label kept to their side so it never runs into one.
{-# INLINE labelBox #-}
labelBox :: Context -> NodeType -> NodeIdx -> Float -> Float -> IO (Float, Float, AlignX)
labelBox ctx nt idx x w
  | nt /= NodeButton = pure (x, w, AlignCenter)
  | otherwise = beside <$> nodeAdornmentInsets (ctxNodeArena ctx) idx x w
  where
    beside (lead, trail)
      | lead > 0 = (x + lead, max 0 (w - lead - trail), AlignStart)
      | trail > 0 = (x, max 0 (w - trail), AlignEnd)
      | otherwise = (x, w, AlignCenter)

-- | A widget's label placed across @w@, a button's aligned by @labelAlign@.
cachedWidgetLabel :: Context -> NodeType -> NodeIdx -> AlignX -> Float -> Float -> IO (Maybe WidgetTextPlacement)
cachedWidgetLabel ctx nt idx labelAlign w h = do
  fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  txt <- displayText ctx nt idx
  ax <-
    if nt == NodeButton && hasFlag buttonFlagTable si
      then getAlignX (ctxNodeArena ctx) idx
      else pure labelAlign
  let ntTag = fromEnum nt
  cache <- readIORef (ctxWidgetTextCache ctx)
  case IM.lookup idx cache of
    Just e
      | wtcNodeType e == ntTag
          && wtcStyle e == si
          && wtcFontSize e == fontSizeVal
          && wtcText e == txt
          && wtcWidth e == w
          && wtcHeight e == h
          && wtcAlign e == fromEnum ax -> pure (wtcPlacement e)
    _ -> do
      placement <- computeWidgetLabel ctx nt txt si fontSizeVal ax w h
      writeIORef
        (ctxWidgetTextCache ctx)
        (IM.insert idx (WidgetTextCacheEntry ntTag si fontSizeVal txt w h (fromEnum ax) placement) cache)
      pure placement

-- All coordinates here are local. centeredTextY snaps the baseline offset,
-- not the origin; final device-pixel snapping stays in the draw backend.
computeWidgetLabel :: Context -> NodeType -> T.Text -> Int -> Float -> AlignX -> Float -> Float -> IO (Maybe WidgetTextPlacement)
computeWidgetLabel ctx nt txt si fontSizeVal ax w h
  | nt == NodeButton && hasFlag buttonFlagClose si = pure Nothing
  | otherwise = do
      (source, _, measure) <- resolveFontFor ctx nt fontSizeVal si
      fm <- prepareFontMetrics source txt
      (tw, th) <- measure txt
      let (ix, _) = widgetContentInset fm
          (tx, used) = case nt of
            NodeButton
              | hasFlag buttonFlagChoice si -> (checkboxLeading fm, tw)
              | hasFlag buttonFlagRow si ->
                  let (depth, _, _) = treeDecodeStyle si
                   in (treeRowLeading fm depth, tw)
              | hasFlag buttonFlagTable si -> alignedTextPen ax 0 w tableCellInset fm txt
              | hasFlag buttonFlagMenu si ->
                  let inset = menuItemPadX + ix
                   in (inset, min tw (max 0 (w - inset - ix)))
              | otherwise -> alignedTextPen ax 0 w 0 fm txt
            -- A select, the other widget with a centred label.
            _ -> (ix, min tw (w - ix - selectChevronReserve))
      let !placement = WidgetTextPlacement txt tx (centeredTextY fm 0 h th) used th
      pure (Just placement)

-- | Pure geometry of selectable text: the pen origin, centered baseline box and
-- line height. Selectable text never scrolls, so the pen is just the node x.
-- Paint uses this and skips the width measure; span placement adds it.
selectableTextGeometry :: FontMetrics -> Float -> Float -> Float -> (Float, Float, Float)
selectableTextGeometry fm x y h =
  let lineH = fmLineHeight fm
   in (x, centeredTextY fm y h lineH, lineH)

-- | A non-selectable field's drawn text, its pen position with the scroll
-- applied, and the clip its text is confined to. The pen starts at the clip's
-- left edge ('nodeTextFieldGeom'), where the caret, selection and
-- hit-testing start too. Paint needs exactly this; only the span path also
-- needs the measured width, so the host measurement stays there.
plainFieldPen ::
  Context -> NodeIdx -> Int -> FontMetrics -> Float -> Float -> Float -> Float -> IO (T.Text, Float, Float, Rect)
plainFieldPen ctx idx si fm x y w h = do
  ph <- if hasFlag textInputFlagNumeric si then pure "" else getText (ctxNodeArena ctx) idx
  -- The value, with an input method's composition in it while one shows.
  (value, _, _, _) <- fieldEditLine ctx idx
  focus <- textInputFocused ctx idx
  (Rect _ boxY _ boxH, clip@(Rect clipX _ _ _)) <- nodeTextFieldGeom ctx idx x y w h
  scrollX <- syncTextInputScroll ctx idx x y w h
  pure (textInputFieldText ph value focus, clipX - scrollX, centeredTextY fm boxY boxH (fmLineHeight fm), clip)

computeWidgetTextPlacements ::
  Context -> NodeType -> NodeIdx -> Float -> Float -> Float -> Float -> IO [(T.Text, Float, Float, Float, Float)]
computeWidgetTextPlacements ctx nt idx x y w h = do
  fontSizeVal <- getNodeFontSize (ctxNodeArena ctx) idx
  si <- getStyleIdx (ctxNodeArena ctx) idx
  (fm, _, measureTxt) <- resolveFontFor ctx nt fontSizeVal si
  let lineH = fmLineHeight fm
  case nt of
    NodeColorPicker
      | colorPickerPartOf si /= PickerPreview -> pure []
      | otherwise -> do
          band@(Rect bx _ _ _) <- colorPickerPartRect (ctxNodeArena ctx) idx (Rect x y w h)
          let (currentY, _, newY, _) = colorPickerPreviewGeom fm band
          forM [("Current", currentY), ("New", newY)] $ \(lbl, ly) -> do
            (lw, lh) <- measureTxt lbl
            pure (lbl, bx, centeredTextY fm ly lineH lh, lw, lh)
    NodeTextInput
      | hasFlag textInputFlagSelectable si -> do
          value <- textInputValue ctx idx
          let (penX, ty, selLineH) = selectableTextGeometry fm x y h
          (fw, _) <- measureTxt value
          pure [(value, penX, ty, fw, selLineH)]
      | otherwise -> do
          (fieldTxt, penX, penY, _) <- plainFieldPen ctx idx si fm x y w h
          (fw, _) <- measureTxt fieldTxt
          pure [(fieldTxt, penX, penY, fw, lineH)]
    -- The document lives in the text area's buffer: its rows in view.
    NodeTextArea -> snd <$> textAreaTextPlacements ctx idx (Rect x y w h)
    -- Sliders, drawings and plain widgets carry no text; the other widgets
    -- have centred labels ('cachedWidgetLabel').
    _ -> pure []

-- | Spans inside every floating panel of one kind, clipped to its content box.
collectFloatingSpansInto :: Context -> NodeType -> SpanArena -> IO ()
collectFloatingSpansInto ctx wanted arena =
  forFloatingNodes_ (ctxNodeArena ctx) wanted $ \idx -> do
    Rect x y w h <- getNodeRect (ctxNodeArena ctx) idx
    clip <- padContentClip x y w h <$> getPadding (ctxNodeArena ctx) idx
    walkChildSpans ctx idx clip arena
