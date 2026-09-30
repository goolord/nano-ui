-- | Paragraphs of mixed-style text and links.
module NanoUI.Widgets.RichText
  ( Inline
  , inlineText
  , inlineWith
  , restyle
  , strong
  , emphasis
  , inlineCode
  , inlineBackground
  , hyperlink
  , richText
  , richText'
  , richTextWith
  , richTextWith'
  , selectableRichTextWith
  ) where

import Control.Monad (foldM, unless, when)
import Data.Hashable (hashWithSalt)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (dropWhileEnd, groupBy)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
import Data.Text (Text)
import Data.Text qualified as T
import NanoUI.Internal.Context
import NanoUI.Internal.Host qualified as Host
import NanoUI.Internal.RichText.Types
import NanoUI.Internal.Draw (DrawOp (..), TextFont (..))
import NanoUI.Internal.Font (FontMetrics (..), prepareFontMetrics, selectionSpans, textIndexAtX)
import NanoUI.Internal.Font qualified as Font
import NanoUI.Internal.Frame.Node (resolveTextFont)
import NanoUI.Internal.Id (WidgetId)
import NanoUI.Internal.Input (Input (..), MouseButton (MouseLeft), UiCursorKind (..), heldIn, inputMousePos, pressedIn, releasedIn)
import NanoUI.Internal.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Internal.Store (eqByPtr, ptrEq)
import NanoUI.Internal.Monad (NanoUI, askDefaultLayout, askInput, freshWidget, liftIO, requestFocus, uiTheme)
import NanoUI.Internal.Style hiding (Flow (..))
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..))
import NanoUI.Internal.Widgets.Node (Response, addWidget, respClicked, respHovered, respRect)
import NanoUI.Internal.Widgets.Behavior (keyboardFocused)
import NanoUI.Internal.Widgets.TextInput (fieldTextCommands)
import NanoUI.Widgets.TextBuffer qualified as TB
import NanoUI.Widgets.TextEditor (Editor (..), EditorMode (..), editorSelection, emptyHistory, multiLineMode, runCommandIO)
import System.IO.Unsafe (unsafeDupablePerformIO)

-- | Text styled by font modifiers (@fontBold@, @fontSize 20@,
-- @fontColor red . fontUnderline@), applied over the paragraph's layout.
inlineWith :: (Layout -> Layout) -> Text -> Inline
inlineWith f txt = Inline txt f Nothing Nothing

-- | Add font modifiers to a piece, a hyperlink included.
restyle :: (Layout -> Layout) -> Inline -> Inline
restyle f (Inline txt style target bg) = Inline txt (f . style) target bg

-- | Bold text.
strong :: Text -> Inline
strong = inlineWith fontBold

-- | Italic text.
emphasis :: Text -> Inline
emphasis = inlineWith fontItalic

-- | Monospaced text.
inlineCode :: Text -> Inline
inlineCode = inlineWith fontMono

-- | Paint a colour behind a piece, the full line-box height, e.g. a
-- highlight or an inline code tint. Leading and trailing spaces on each line
-- are left bare.
--
-- > richText ["Run ", inlineBackground codeTint (inlineCode "cabal build"), " first."]
inlineBackground :: Color -> Inline -> Inline
inlineBackground c (Inline txt style target _) = Inline txt style target (Just c)

-- | @hyperlink target label@: text in the theme's link colour, underlined while
-- hovered, whose click the paragraph reports as @target@.
hyperlink :: Text -> Text -> Inline
hyperlink target label = Inline label id (Just target) Nothing

-- | A paragraph of pieces, wrapped at its width. Returns the target of the
-- hyperlink clicked this frame.
richText :: [Inline] -> NanoUI (Maybe Text)
richText = richTextWith id

-- | 'richText' with a layout modifier, whose font choices are the default
-- for every piece. Horizontal alignment applies per line: with 'alignEnd'
-- every line ends at the right edge.
richTextWith :: (Layout -> Layout) -> [Inline] -> NanoUI (Maybe Text)
richTextWith = richTextWithMode False

-- | 'richTextWith' with read-only mouse selection and the standard text
-- commands for selecting and copying text. Link clicks still return their
-- destination.
selectableRichTextWith :: (Layout -> Layout) -> [Inline] -> NanoUI (Maybe Text)
selectableRichTextWith = richTextWithMode True

richTextWithMode :: Bool -> (Layout -> Layout) -> [Inline] -> NanoUI (Maybe Text)
richTextWithMode selectable f pieces = snd <$> richTextWithMode' selectable f pieces

-- | 'richText' returning the paragraph response and a link target clicked
-- this frame, or 'Nothing' when no link was activated.
richText' :: [Inline] -> NanoUI (Response, Maybe Text)
richText' = richTextWith' id

-- | 'richTextWith' returning the paragraph response and optional clicked link target.
richTextWith' :: (Layout -> Layout) -> [Inline] -> NanoUI (Response, Maybe Text)
richTextWith' = richTextWithMode' False

richTextWithMode' :: Bool -> (Layout -> Layout) -> [Inline] -> NanoUI (Response, Maybe Text)
richTextWithMode' selectable f pieces = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  base <- f <$> askDefaultLayout
  theme <- uiTheme
  let styled = [(piece, pieceFont l, pieceColor theme l target) | piece@(Inline _ style target _) <- pieces, let l = style base]
      plain = T.concat [txt | (Inline txt _ _ _, _, _) <- styled]
      align = layoutAlignX base
  Paragraphs cacheRef <- liftIO $ Host.hostOrInit (ctxParagraphs ctx) (Paragraphs <$> newIORef (ParagraphCache 0 paragraphBound IM.empty))
  gen <- liftIO (readIORef (ctxMetricGen ctx))
  let reused = \para -> do
        Inputs pieces0 base0 theme0 gen0 selectable0 <- readIORef (paraInputs para)
        pure (ptrEq pieces pieces0 && gen == gen0 && eqByPtr base base0 && eqByPtr theme theme0 && selectable == selectable0)
  cached <- liftIO ((\(ParagraphCache _ _ m) -> IM.lookup (intKey wid) m) <$> readIORef cacheRef)
  sameInputs <- liftIO (maybe (pure False) reused cached)
  let hashed =
        foldl'
          ( \h (Inline txt _ target bg, TextFont size variant weight fstyle deco, Color rgba) ->
              h `hashWithSalt` txt `hashWithSalt` size `hashWithSalt` fromEnum variant
                `hashWithSalt` fromEnum weight `hashWithSalt` fromEnum fstyle `hashWithSalt` fromEnum deco
                `hashWithSalt` rgba `hashWithSalt` target `hashWithSalt` fmap (\(Color c) -> c) bg
          )
          (gen `hashWithSalt` fromEnum align `hashWithSalt` selectable)
          styled
      key = case cached of
        Just para | sameInputs -> paraKey para
        _ -> hashed
      inputs = Inputs pieces base theme gen selectable
  para0 <- case cached of
    Just para | paraKey para == key -> liftIO (para <$ unless sameInputs (writeIORef (paraInputs para) inputs))
    _ -> liftIO $ do
      let starts = scanl (+) 0 [T.length txt | (Inline txt _ _ _, _, _) <- styled]
      resolved <- mapM (measurePiece ctx) (zip3 [0 ..] starts styled)
      let runs = smallArrayFromList (map fst resolved)
          tokens = concatMap snd resolved
          emptyLine = case resolved of
            (run, _) : _ -> (runLineHeight run, runAscent run)
            [] -> (fmLineHeight (ctxFontMetrics ctx), fmAscent (ctxFontMetrics ctx))
      measured <- newIORef Unmeasured
      inputsRef <- newIORef inputs
      selection <- if selectable then Just <$> newIORef (RichSelection plain (TB.Cursor 0 0) (TB.Cursor 0 0) False False) else pure Nothing
      pure (Paragraph key inputsRef runs tokens emptyLine (lineBoxes (layoutLines runs emptyLine AlignStart 1e9 tokens)) (-1) [] measured selection)
  when (selectable && not (T.null plain)) (liftIO (registerFocusable ctx wid))
  resp <- addWidget wid NodeDrawing T.empty 0 base
  let Rect rx ry rw _ = respRect resp
      runs = paraRuns para0
      layoutAt width = layoutLines runs (paraEmptyLine para0) align width (paraTokens para0)
      para
        | paraWidth para0 == rw = para0
        | otherwise = para0 {paraWidth = rw, paraLines = layoutAt rw}
      linesAt width
        | width == paraWidth para = paraLines para
        | otherwise = layoutAt width
  (selection, dragReleased) <- case paraSelection para of
    Nothing -> pure (Nothing, False)
    Just selectionRef -> do
      (current, dragged) <- updateSelection ctx inp wid resp plain (paraLines para) selectionRef
      pure (Just current, dragged)
  focused <- if selectable then keyboardFocused wid else pure False
  let
      V2 mx my = inputMousePos inp
      hoveredRun
        | not (respHovered resp) = Nothing
        | otherwise =
            listToMaybe
              [ tokenRun tok
              | line <- paraLines para
              , my >= ry + lineTop line && my < ry + lineTop line + lineHeight line
              , (x, tok) <- lineTokens line
              , tokenKind tok /= Break
              , mx >= rx + x && mx < rx + x + tokenWidth tok
              , isJust (runTarget (indexSmallArray runs (tokenRun tok)))
              ]
      -- Words are drawn separately, so backgrounds and decorations are drawn
      -- once per piece per line, spanning the inner spaces. Backgrounds go
      -- underneath.
      draw _cdc (Rect x0 y0 w h) =
        smallArrayFromList $
          concatMap lineOps (linesAt w)
            ++ [StrokeRoundedRect (Rect x0 y0 w h) 2 1 (themeFocusRing theme) | focused]
        where
          selected = selection >>= selectionOffsets
          lineOps line =
            let spans = pieceSpans line
                highlights = maybe [] (\(lo, hi) -> selectionOps x0 y0 line lo hi (themeSelection theme)) selected
             in [FillRect (Rect (x0 + x1) (y0 + lineTop line) (x2 - x1) (lineHeight line)) bg | (run, _, x1, x2) <- spans, Just bg <- [runBackground run]]
                  ++ highlights
                  ++ [ DrawTextStyled (x0 + x) (lineY line run) ((runFont run) {textFontDecoration = DecorationNone}) txt (runColor run)
                     | (x, Token txt runIdx Word _ _ _) <- lineTokens line
                     , let run = indexSmallArray runs runIdx
                     ]
                  ++ [ DrawTextStyled (x0 + x) (lineY line run) ((runFont run) {textFontDecoration = DecorationNone}) txt (runColor run)
                      | group@((x, Token _ runIdx _ _ _ _) : _) <- glyphGroups line
                     , let run = indexSmallArray runs runIdx
                           txt = T.concat [tokenText tok | (_, tok) <- group]
                     ]
                  ++ [ FillRect (Rect (x0 + x1) (lineY line run + offset) (x2 - x1) thick) (runColor run)
                     | (run, runIdx, x1, x2) <- spans
                     , let deco = decorationOf runIdx
                           thick = max 1 (0.06 * runLineHeight run)
                     , deco /= DecorationNone
                     , offset <- decorationOffsets deco run
                     ]
          lineY line run = y0 + lineTop line + lineAscent line - runAscent run
          isSpaceToken (_, tok) = tokenKind tok == Space || tokenKind tok == GlyphSpace
          glyphGroups line =
            groupBy joinsGlyphs [(x, tok) | (x, tok) <- lineTokens line, tokenKind tok == Glyph || tokenKind tok == GlyphSpace]
          joinsGlyphs (_, a) (_, b) = tokenRun a == tokenRun b
          -- Each piece's tokens on a line, trimmed of edge spaces: run, run
          -- index, start x and end x.
          pieceSpans line =
            [ (indexSmallArray runs (tokenRun first), tokenRun first, x1, lastX + tokenWidth lastTok)
            | group <- groupBy (\(_, a) (_, b) -> tokenRun a == tokenRun b) (lineTokens line)
            , let trimmed = dropWhileEnd isSpaceToken (dropWhile isSpaceToken group)
            , (x1, first) : _ <- [trimmed]
            , let (lastX, lastTok) = last trimmed
            ]
      decorationOf runIdx =
        (if Just runIdx == hoveredRun then addUnderline else id)
          (textFontDecoration (runFont (indexSmallArray runs runIdx)))
      -- Where underline and strikethrough sit below a line box's top, as
      -- styled labels draw them.
      decorationOffsets deco run =
        let lh = runLineHeight run
            under = runAscent run + max 1 (0.1 * lh)
            strike = runAscent run * 0.65
         in case deco of
              DecorationUnderline -> [under]
              DecorationStrikethrough -> [strike]
              DecorationUnderlineStrike -> [under, strike]
              DecorationNone -> []
      drawKey = key `hashWithSalt` fromMaybe (-1) hoveredRun `hashWithSalt` fmap selectionKey selection `hashWithSalt` focused
  liftIO $ do
    unless (paraWidth para0 == rw && fmap paraKey cached == Just key) $ do
      ParagraphCache n bound m <- readIORef cacheRef
      let n' = if isJust cached then n else n + 1
      writeIORef cacheRef
        =<< if n' <= bound
          then pure $! ParagraphCache n' bound (IM.insert (intKey wid) para m)
          else do
            -- Past the bound, drop paragraphs neither drawn last frame nor
            -- yet this one, then set the bound to twice what is left. Views
            -- with more paragraphs than the bound keep them all, and pruning
            -- runs occasionally rather than every frame.
            prev <- getsDamage ctx (pfRects . dsPrev)
            now <- dcsCustomDrawings <$> readIORef (ctxDrawingCache ctx)
            let kept = IM.insert (intKey wid) para (IM.filterWithKey (\k _ -> IM.member k prev || IM.member k now) m)
                size = IM.size kept
            pure $! ParagraphCache size (max paragraphBound (2 * size)) kept
    registerCustomMeasure ctx wid $ \_ (availW, _) ->
      if availW >= 1e9 then paraNatural para else measureAt (paraMeasured para) (lineBoxes . linesAt) availW
    registerCustomEntry ctx wid $
      CustomDrawingEntry
        (if drawKey == 0 then 1 else drawKey)
        draw
        (Just (\_ _ _ -> if isJust hoveredRun then UiCursorPointer else if selectable then UiCursorText else UiCursorDefault))
        0
        False
        (if selectable then KeysReadOnly else KeysNone)
  let clicked
        | respClicked resp && not dragReleased = hoveredRun >>= runTarget . indexSmallArray runs
        | otherwise = Nothing
  pure (resp, clicked)
  where
    lineBoxes lines' = (maximum (0 : map lineWidth lines'), sum (map lineHeight lines'))

-- | Keep the paragraph's selection in document coordinates, so it survives
-- rewrapping when the widget's width changes.
updateSelection :: Context -> Input -> WidgetId -> Response -> Text -> [Line] -> IORef RichSelection -> NanoUI (RichSelection, Bool)
updateSelection ctx inp wid resp plain lines' selectionRef = do
  stored <- liftIO (readIORef selectionRef)
  let synced = syncSelection plain stored
      RichSelection _ _ _ dragging moved = synced
      editor0 = selectionEditor plain synced
  focused <- keyboardFocused wid
  commands <- if focused then liftIO (fieldTextCommands ctx readOnlyMode inp) else pure []
  editor <- liftIO (foldM (flip (runCommandIO ctx readOnlyMode)) editor0 commands)
  let (keyAnchor, keyCursor) = editorSelection editor
      afterKeys = RichSelection plain keyAnchor keyCursor dragging moved
      mouse = inputMousePos inp
      press = pressedIn MouseLeft inp && respHovered resp
      held = heldIn MouseLeft inp
      released = releasedIn MouseLeft inp
  (next, dragReleased) <-
    if press
      then do
        let pos = cursorAtPoint plain lines' (respRect resp) mouse
        requestFocus wid
        pure (RichSelection plain pos pos held False, False)
      else case afterKeys of
        RichSelection _ dragAnchor _ True wasMoved
          | held || released -> do
              let pos = cursorAtPoint plain lines' (respRect resp) mouse
              let didMove = wasMoved || pos /= dragAnchor
              pure
                ( RichSelection plain dragAnchor pos held (didMove && held)
                , released && didMove
                )
        RichSelection _ a c True _ -> pure (RichSelection plain a c False False, False)
        RichSelection _ a c False _ -> pure (RichSelection plain a c False False, False)
  liftIO (writeIORef selectionRef next)
  pure (next, dragReleased)

readOnlyMode :: EditorMode
readOnlyMode = multiLineMode {modeEditable = False}

syncSelection :: Text -> RichSelection -> RichSelection
syncSelection plain previous@(RichSelection old a c _ _)
  | old == plain = previous
  | otherwise =
      let buf = TB.fromText plain
       in RichSelection plain (TB.clampCursor buf a) (TB.clampCursor buf c) False False

selectionEditor :: Text -> RichSelection -> Editor
selectionEditor plain (RichSelection _ anchor cursor _ _) =
  let buf = TB.fromText plain
      cursor' = TB.clampCursor buf cursor
   in Editor (TB.withCursor cursor' buf) (TB.clampCursor buf anchor) emptyHistory

selectionKey :: RichSelection -> (Int, Int)
selectionKey (RichSelection txt anchor cursor _ _) =
  let a = cursorOffset txt anchor
      c = cursorOffset txt cursor
   in (min a c, max a c)

selectionOffsets :: RichSelection -> Maybe (Int, Int)
selectionOffsets selection =
  let (lo, hi) = selectionKey selection
   in if lo == hi then Nothing else Just (lo, hi)

-- | Draw the selected portions of each laid-out token with the theme's native
-- selection colour, preserving the paragraph's per-run font shaping.
selectionOps :: Float -> Float -> Line -> Int -> Int -> Color -> [DrawOp]
selectionOps x0 y0 line lo hi color = concatMap drawToken (lineTokens line)
  where
    drawToken (x, tok) =
      let Token txt _ _ _ start fm = tok
          end = start + T.length txt
          a = max lo start
          b = min hi end
       in if b <= a
            then []
            else
              [ FillRect
                  (Rect (x0 + x + sx) (y0 + lineTop line) (ex - sx) (lineHeight line))
                  color
              | (sx, ex) <- selectionSpans fm txt (a - start) (b - start)
              ]

cursorAtPoint :: Text -> [Line] -> Rect -> V2 -> TB.Cursor
cursorAtPoint plain lines' (Rect rx ry _ _) (V2 px py) =
  let line = lineAtY (py - ry) lines'
      offset = tokenIndexAtX line (px - rx)
   in cursorAtOffset plain offset

lineAtY :: Float -> [Line] -> Line
lineAtY y = go
  where
    go [] = Line 0 0 0 0 0 []
    go [line] = line
    go (line : rest)
      | y < lineTop line + lineHeight line = line
      | otherwise = go rest

tokenIndexAtX :: Line -> Float -> Int
tokenIndexAtX line x = go (lineTokens line)
  where
    go [] = lineStart line
    go [(tokenX, tok)]
      | x <= tokenX = tokenStart tok
      | x >= tokenX + tokenWidth tok = tokenStart tok + T.length (tokenText tok)
      | otherwise = indexWithin tokenX tok
    go ((tokenX, tok) : rest)
      | x <= tokenX = tokenStart tok
      | x <= tokenX + tokenWidth tok = indexWithin tokenX tok
      | otherwise = go rest
    indexWithin tokenX tok =
      let txt = tokenText tok
       in tokenStart tok + textIndexAtX (tokenMetrics tok) txt (x - tokenX)

cursorOffset :: Text -> TB.Cursor -> Int
cursorOffset txt (TB.Cursor row col) =
  let ls = T.splitOn "\n" txt
      row' = max 0 (min row (length ls - 1))
      before = sum (map T.length (take row' ls)) + row'
      line = fromMaybe "" (listToMaybe (drop row' ls))
   in before + max 0 (min col (T.length line))

cursorAtOffset :: Text -> Int -> TB.Cursor
cursorAtOffset txt offset =
  let prefix = T.take (max 0 (min offset (T.length txt))) txt
      row = T.count "\n" prefix
      col = T.length (T.takeWhileEnd (/= '\n') prefix)
   in TB.Cursor row col

-- | @measureAt ref extentAt width@ is @extentAt width@, reused from @ref@
-- when it holds the extent at that width, and otherwise computed and left
-- there. A paragraph keeps its reference while its pieces stay the same, so
-- the solver's check of last frame's layout, which measures the paragraph
-- again at the width it offered then, costs nothing even when that width is a
-- cap the paragraph stays under, and its second measure at a new width reuses
-- the first. Only the extent is kept, never the lines. Reading and writing
-- the reference from pure code is safe because @extentAt@ is a pure function
-- of the width for those pieces: the reference only decides what is shared.
measureAt :: IORef Measured -> (Float -> (Float, Float)) -> Float -> (Float, Float)
measureAt ref extentAt width = unsafeDupablePerformIO $ do
  kept <- readIORef ref
  case kept of
    Measured w ew eh | w == width -> pure (ew, eh)
    _ -> case extentAt width of
      (!ew, !eh) -> (ew, eh) <$ writeIORef ref (Measured width ew eh)
{-# NOINLINE measureAt #-}

-- | Cache size at which stale paragraphs are first pruned.
paragraphBound :: Int
paragraphBound = 4096

-- | The font a piece's layout chooses. A colour-only variant resolves to the
-- regular face ('resolveTextFont').
pieceFont :: Layout -> TextFont
pieceFont l = TextFont (layoutFontSize l) (layoutFontVariant l) (layoutFontWeight l) (layoutFontStyle l) (layoutTextDecoration l)

-- | A piece's colour: its own, else the link colour for a link, else its
-- colour for its tone and face, as for a label.
pieceColor :: Theme -> Layout -> Maybe Text -> Color
pieceColor theme l target =
  let toneCol = textToneColor theme (layoutFontVariant l) (layoutFontTone l)
   in fromMaybe (maybe toneCol (const (themeLink theme)) target) (layoutFontColor l)

-- | A piece's line metrics and its tokens measured in its font.
measurePiece :: Context -> (Int, Int, (Inline, TextFont, Color)) -> IO (Run, [Token])
measurePiece ctx (i, start, (Inline txt _ target bg, font, color)) = do
  (fm, _) <- resolveTextFont ctx font
  let mono = case font of TextFont _ FontMono _ _ _ -> True; _ -> False
      parts
        | mono = map T.singleton (T.unpack txt)
        | otherwise = T.groupBy (\a b -> kindOf mono a == kindOf mono b && kindOf mono a /= Break) txt
  monoMetrics <- if mono then prepareFontMetrics fm txt else pure fm
  tokens <- reverse . snd <$> foldM (measure fm mono monoMetrics) (start, []) parts
  pure (Run font color (fmLineHeight fm) (fmAscent fm) target bg, tokens)
  where
    kindOf mono c
      | c == '\n' = Break
      | mono && (c == ' ' || c == '\t') = GlyphSpace
      | mono = Glyph
      | c == ' ' || c == '\t' = Space
      | otherwise = Word
    measure fm mono monoMetrics (at, acc) part = do
      let kind = kindOf mono (T.head part)
      prepared <- if mono then pure monoMetrics else prepareFontMetrics fm part
      let w = if kind == Break then 0 else Font.lineWidth prepared part
          token = Token part i kind w at prepared
      pure (at + T.length part, token : acc)

-- | Greedy line breaking at @width@, aligned by @align@. Breaks only at
-- spaces or newlines, drops spaces at a wrap, and gives an overlong word its
-- own line.
layoutLines :: SmallArray Run -> (Float, Float) -> AlignX -> Float -> [Token] -> [Line]
layoutLines runs (emptyH, emptyAscent) align width = go 0 0 [] 0 [] True
  where
    -- @placed@ holds the line's tokens in reverse, @pending@ the spaces since
    -- its last word; @fresh@ whether the line starts after a wrap.
    go top start placed x pending fresh toks = case toks of
      [] -> [finish start top placed x]
      tok : rest -> case tokenKind tok of
        Break ->
          let line = finish start top placed x
              nextStart = tokenStart tok + T.length (tokenText tok)
           in line : go (top + lineHeight line) nextStart [] 0 [] False rest
        Space ->
          let start' = if null placed && null pending && fresh then tokenStart tok else start
           in go top start' placed x (tok : pending) fresh rest
        Glyph -> placeGlyph top start placed x pending fresh tok rest
        GlyphSpace -> placeGlyph top start placed x pending fresh tok rest
        Word ->
          let (word, rest') = span (\t -> tokenKind t == Word) toks
              wordW = sum (map tokenWidth word)
              spaceW = if null placed && fresh then 0 else sum (map tokenWidth pending)
           in if not (null placed) && x + spaceW + wordW > width
                then
                  let line = finish start top placed x
                   in line : go (top + lineHeight line) (tokenStart tok) [] 0 [] True toks
                else
                  let (placed', x') = foldl' place (placed, x) (if null placed && fresh then [] else reverse pending)
                      (placed'', x'') = foldl' place (placed', x') word
                      start' = if null placed && fresh then maybe start tokenStart (listToMaybe word) else start
                   in go top start' placed'' x'' [] False rest'
    placeGlyph top start placed x pending fresh tok rest
      | not (null placed) && x + spaceW + tokenWidth tok > width =
          let line = finish start top placed x
           in line : go (top + lineHeight line) (tokenStart tok) [] 0 [] True (tok : rest)
      | otherwise =
          let (placed', x') = foldl' place (placed, x) (if null placed && fresh then [] else reverse pending)
              (placed'', x'') = place (placed', x') tok
              start' = if null placed && fresh then tokenStart tok else start
           in go top start' placed'' x'' [] False rest
      where
        spaceW = if null placed && fresh then 0 else sum (map tokenWidth pending)
    place (acc, x) tok = ((x, tok) : acc, x + tokenWidth tok)
    finish start top placed x =
      -- An overlong word's line starts at the left edge, as with
      -- 'AlignStart', rather than before it.
      let shift = max 0 (width - x) * alignXFraction align
          toks = reverse (if shift == 0 then placed else [(tx + shift, tok) | (tx, tok) <- placed])
          metrics = [indexSmallArray runs (tokenRun tok) | (_, tok) <- toks]
          (h, ascent) = case metrics of
            [] -> (emptyH, emptyAscent)
            _ ->
              let ascent' = maximum (map runAscent metrics)
                  descent = maximum [runLineHeight r - runAscent r | r <- metrics]
               in (ascent' + descent, ascent')
       in Line start top h ascent x toks
