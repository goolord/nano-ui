-- | Paragraphs of mixed-style text and links.
module NanoUI.Widgets.RichText
  ( Inline
  , inlineText
  , inlineWith
  , restyle
  , strong
  , emphasis
  , inlineCode
  , hyperlink
  , richText
  , richText'
  , richTextWith
  , richTextWith'
  ) where

import Control.Monad (unless)
import Data.Hashable (hashWithSalt)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (dropWhileEnd, groupBy)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI.Internal.Context
import NanoUI.Internal.Draw (DrawOp (..), TextFont (..))
import NanoUI.Internal.Font (FontMetrics (..), lineWidthIO)
import NanoUI.Internal.Frame.Node (resolveTextFont)
import NanoUI.Internal.Input (Input (..), UiCursorKind (..))
import NanoUI.Internal.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Internal.Monad (Ui, askDefaultLayout, askInput, freshWidget, uiIO, uiTheme)
import NanoUI.Internal.Style
import NanoUI.Internal.Types (Color (..), Rect (..), V2 (..))
import NanoUI.Internal.Widgets.Node (Response, addWidget, respClicked, respHovered, respRect)

-- | A piece of a paragraph: text in one style, and the hyperlink it follows when
-- it is one. A string literal produces unstyled text.
data Inline = Inline !Text (Layout -> Layout) !(Maybe Text)

instance IsString Inline where
  fromString = inlineText . T.pack

-- | Text in the paragraph's own style.
inlineText :: Text -> Inline
inlineText txt = Inline txt id Nothing

-- | Text styled by font modifiers (@fontBold@, @fontSize 20@,
-- @fontColor red . fontUnderline@), applied over the paragraph's layout.
inlineWith :: (Layout -> Layout) -> Text -> Inline
inlineWith f txt = Inline txt f Nothing

-- | Add font modifiers to a piece, a hyperlink included.
restyle :: (Layout -> Layout) -> Inline -> Inline
restyle f (Inline txt style target) = Inline txt (f . style) target

-- | Bold text.
strong :: Text -> Inline
strong = inlineWith fontBold

-- | Italic text.
emphasis :: Text -> Inline
emphasis = inlineWith fontItalic

-- | Monospaced text.
inlineCode :: Text -> Inline
inlineCode = inlineWith fontMono

-- | @hyperlink target label@: text in the theme's link colour, underlined while
-- hovered, whose click the paragraph reports as @target@.
hyperlink :: Text -> Text -> Inline
hyperlink target label = Inline label id (Just target)

-- | A paragraph of pieces, wrapped at its width. Returns the target of the
-- hyperlink clicked this frame.
richText :: Ui :> es => [Inline] -> Eff es (Maybe Text)
richText = richTextWith id

-- | 'richText' with a layout modifier, whose font choices are the default
-- for every piece. Its horizontal alignment places each line in the
-- paragraph: with 'alignEnd' every line ends at its right edge.
richTextWith :: Ui :> es => (Layout -> Layout) -> [Inline] -> Eff es (Maybe Text)
richTextWith f pieces = snd <$> richTextWith' f pieces

-- | 'richText' returning the paragraph response and a link target clicked
-- this frame, or 'Nothing' when no link was activated.
richText' :: Ui :> es => [Inline] -> Eff es (Response, Maybe Text)
richText' = richTextWith' id

-- A resolved piece: its font, colour, line metrics and hyperlink.
data Run = Run
  { runFont :: !TextFont
  , runColor :: !Color
  , runLineHeight :: !Float
  , runAscent :: !Float
  , runTarget :: !(Maybe Text)
  }

data TokenKind = Word | Space | Break
  deriving (Eq)

-- A word, a run of spaces or a line break, with its width in its piece's font.
data Token = Token
  { _tokenText :: !Text
  , tokenRun :: !Int
  , tokenKind :: !TokenKind
  , tokenWidth :: !Float
  }

-- A laid-out line: its top, height and baseline offset, and its tokens with
-- their x positions.
data Line = Line
  { lineTop :: !Float
  , lineHeight :: !Float
  , lineAscent :: !Float
  , lineWidth :: !Float
  , lineTokens :: ![(Float, Token)]
  }

-- A paragraph's measured pieces and its lines at the width it last had,
-- kept between frames while its pieces, fonts and colours stay the same.
data Paragraph = Paragraph
  { paraKey :: !Int
  , paraRuns :: !(SmallArray Run)
  , paraTokens :: ![Token]
  , paraEmptyLine :: !(Float, Float)
  , paraNatural :: (Float, Float)
  , paraWidth :: !Float
  , paraLines :: [Line]
  }

-- The paragraphs laid out lately, by widget key: how many there are, how
-- many there may be before those not drawn lately are dropped, and them.
data ParagraphCache = ParagraphCache !Int !Int !(IM.IntMap Paragraph)

newtype Paragraphs = Paragraphs (IORef ParagraphCache)

-- | 'richTextWith' returning the paragraph response and optional clicked link target.
richTextWith' :: Ui :> es => (Layout -> Layout) -> [Inline] -> Eff es (Response, Maybe Text)
richTextWith' f pieces = do
  (wid, ctx) <- freshWidget
  inp <- askInput
  base <- f <$> askDefaultLayout
  theme <- uiTheme
  let styled = [(txt, pieceFont l, pieceColor theme l target, target) | Inline txt style target <- pieces, let l = style base]
      align = layoutAlignX base
  Paragraphs cacheRef <- uiIO $ hostOrInit ctx (Paragraphs <$> newIORef (ParagraphCache 0 paragraphBound IM.empty))
  gen <- uiIO (readIORef (ctxMetricGen ctx))
  let key =
        foldl'
          ( \h (txt, TextFont size variant weight fstyle deco, Color rgba, target) ->
              h `hashWithSalt` txt `hashWithSalt` size `hashWithSalt` fromEnum variant
                `hashWithSalt` fromEnum weight `hashWithSalt` fromEnum fstyle `hashWithSalt` fromEnum deco
                `hashWithSalt` rgba `hashWithSalt` target
          )
          (gen `hashWithSalt` fromEnum align)
          styled
  cached <- uiIO ((\(ParagraphCache _ _ m) -> IM.lookup (intKey wid) m) <$> readIORef cacheRef)
  para0 <- case cached of
    Just para | paraKey para == key -> pure para
    _ -> uiIO $ do
      resolved <- mapM (measurePiece ctx) (zip [0 ..] styled)
      let runs = smallArrayFromList (map fst resolved)
          tokens = concatMap snd resolved
          emptyLine = case resolved of
            (run, _) : _ -> (runLineHeight run, runAscent run)
            [] -> (fmLineHeight (ctxFontMetrics ctx), fmAscent (ctxFontMetrics ctx))
      pure (Paragraph key runs tokens emptyLine (lineBoxes (layoutLines runs emptyLine AlignStart 1e9 tokens)) (-1) [])
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
      -- Words are drawn one by one, so a decoration is drawn once across a
      -- piece's words on a line and the spaces between them.
      draw _cdc (Rect x0 y0 w _) =
        smallArrayFromList $
          concat
            [ [ DrawTextStyled (x0 + x) (lineY line run) ((runFont run) {textFontDecoration = DecorationNone}) txt (runColor run)
              | (x, Token txt runIdx Word _) <- lineTokens line
              , let run = indexSmallArray runs runIdx
              ]
                ++ concat
                  [ [FillRect (Rect (x0 + x1) (y + offset) (x2 - x1) thick) (runColor run) | offset <- decorationOffsets deco run]
                  | group <- groupBy (\(_, a) (_, b) -> tokenRun a == tokenRun b) (lineTokens line)
                  , let trimmed = dropWhileEnd isSpaceToken (dropWhile isSpaceToken group)
                  , (x1, first) : _ <- [trimmed]
                  , let runIdx = tokenRun first
                        run = indexSmallArray runs runIdx
                        deco = decorationOf runIdx
                        (lastX, lastTok) = last trimmed
                        x2 = lastX + tokenWidth lastTok
                        y = lineY line run
                        thick = max 1 (0.06 * runLineHeight run)
                  , deco /= DecorationNone
                  ]
            | line <- linesAt w
            ]
        where
          lineY line run = y0 + lineTop line + lineAscent line - runAscent run
          isSpaceToken (_, tok) = tokenKind tok == Space
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
      drawKey = key `hashWithSalt` fromMaybe (-1) hoveredRun
  uiIO $ do
    unless (paraWidth para0 == rw && fmap paraKey cached == Just key) $ do
      ParagraphCache n bound m <- readIORef cacheRef
      let n' = if isJust cached then n else n + 1
      writeIORef cacheRef
        =<< if n' <= bound
          then pure $! ParagraphCache n' bound (IM.insert (intKey wid) para m)
          else do
            -- Past the bound, drop the paragraphs neither laid out last
            -- frame nor drawn yet this one, and let the bound be twice what
            -- is left: a view that draws more paragraphs than the bound
            -- keeps every one, and what a view stops drawing goes now and
            -- then rather than every frame.
            prev <- getsDamage ctx (pfRects . dsPrev)
            now <- dcsCustomDrawings <$> readIORef (ctxDrawingCache ctx)
            let kept = IM.insert (intKey wid) para (IM.filterWithKey (\k _ -> IM.member k prev || IM.member k now) m)
                size = IM.size kept
            pure $! ParagraphCache size (max paragraphBound (2 * size)) kept
    registerCustomMeasure ctx wid $ \_ (availW, _) ->
      if availW >= 1e9 then paraNatural para else lineBoxes (linesAt availW)
    registerCustomEntry ctx wid $
      CustomDrawingEntry
        (if drawKey == 0 then 1 else drawKey)
        draw
        (Just (\_ _ _ -> if isJust hoveredRun then UiCursorPointer else UiCursorDefault))
        0
        False
  let clicked
        | respClicked resp = hoveredRun >>= runTarget . indexSmallArray runs
        | otherwise = Nothing
  pure (resp, clicked)
  where
    lineBoxes lines' = (maximum (0 : map lineWidth lines'), sum (map lineHeight lines'))

-- | How many paragraphs the cache keeps before it drops those not drawn
-- lately.
paragraphBound :: Int
paragraphBound = 4096

-- | The font a piece's layout chooses.
pieceFont :: Layout -> TextFont
pieceFont l = TextFont (layoutFontSize l) (layoutFontVariant l) (layoutFontWeight l) (layoutFontStyle l) (layoutTextDecoration l)

-- | A piece's colour: its own, else the link colour for a link, else its
-- font variant's colour.
pieceColor :: Theme -> Layout -> Maybe Text -> Color
pieceColor theme l target =
  let variantColor = case layoutFontVariant l of
        FontHeading -> themeAccent theme
        FontMuted -> themeMuted theme
        FontDanger -> themeRed theme
        FontWarning -> themeWarning theme
        _ -> styleFg (themePanel theme)
   in fromMaybe (maybe variantColor (const (themeLink theme)) target) (layoutFontColor l)

-- | A piece's line metrics and its tokens measured in its font.
measurePiece :: Context -> (Int, (Text, TextFont, Color, Maybe Text)) -> IO (Run, [Token])
measurePiece ctx (i, (txt, font, color, target)) = do
  (fm, _) <- resolveTextFont ctx font
  tokens <- mapM (measure fm) (T.groupBy (\a b -> kindOf a == kindOf b && kindOf a /= Break) txt)
  pure (Run font color (fmLineHeight fm) (fmAscent fm) target, tokens)
  where
    kindOf c
      | c == '\n' = Break
      | c == ' ' || c == '\t' = Space
      | otherwise = Word
    measure fm part = do
      let kind = kindOf (T.head part)
      w <- if kind == Break then pure 0 else lineWidthIO fm part
      pure (Token part i kind w)

-- | Greedy lines at @width@, each placed in it as @align@ says: a break goes
-- between words only at spaces or line breaks, spaces at a wrap are dropped,
-- and a word wider than the line takes a line of its own.
layoutLines :: SmallArray Run -> (Float, Float) -> AlignX -> Float -> [Token] -> [Line]
layoutLines runs (emptyH, emptyAscent) align width = go 0 [] 0 [] True
  where
    -- @placed@ holds the line's tokens in reverse, @pending@ the spaces since
    -- its last word; @fresh@ whether the line starts after a wrap.
    go top placed x pending fresh toks = case toks of
      [] -> [finish top placed x]
      tok : rest -> case tokenKind tok of
        Break -> let line = finish top placed x in line : go (top + lineHeight line) [] 0 [] False rest
        Space -> go top placed x (tok : pending) fresh rest
        Word ->
          let (word, rest') = span (\t -> tokenKind t == Word) toks
              wordW = sum (map tokenWidth word)
              spaceW = if null placed && fresh then 0 else sum (map tokenWidth pending)
           in if not (null placed) && x + spaceW + wordW > width
                then
                  let line = finish top placed x
                   in line : go (top + lineHeight line) [] 0 [] True toks
                else
                  let (placed', x') = foldl' place (placed, x) (if null placed && fresh then [] else reverse pending)
                      (placed'', x'') = foldl' place (placed', x') word
                   in go top placed'' x'' [] False rest'
    place (acc, x) tok = ((x, tok) : acc, x + tokenWidth tok)
    finish top placed x =
      let shift = case align of
            AlignStart -> 0
            AlignCenter -> (width - x) / 2
            AlignEnd -> width - x
          toks = reverse (if shift == 0 then placed else [(tx + shift, tok) | (tx, tok) <- placed])
          metrics = [indexSmallArray runs (tokenRun tok) | (_, tok) <- toks]
          (h, ascent) = case metrics of
            [] -> (emptyH, emptyAscent)
            _ ->
              let ascent' = maximum (map runAscent metrics)
                  descent = maximum [runLineHeight r - runAscent r | r <- metrics]
               in (ascent' + descent, ascent')
       in Line top h ascent x toks
