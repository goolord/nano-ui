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
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.IntMap.Strict qualified as IM
import Data.List (dropWhileEnd, groupBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive.SmallArray (SmallArray, indexSmallArray, smallArrayFromList)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Effectful (Eff, type (:>))
import NanoUI.Context
  ( Context (..)
  , askHostIO
  , intKey
  , setHost
  , registerCustomCursor
  , registerCustomDrawing
  , registerCustomMeasure
  )
import NanoUI.Draw (DrawOp (..), TextFont (..))
import NanoUI.Font (FontMetrics (..), lineWidthIO)
import NanoUI.Frame.Node (resolveTextFont)
import NanoUI.Input (Input (..), UiCursorKind (..))
import NanoUI.Layout.Arena (NodeType (NodeDrawing))
import NanoUI.Monad (Ui, askContext, askDefaultLayout, askInput, nextId, uiIO, uiTheme)
import NanoUI.Style
  ( FontVariant (..)
  , Layout (..)
  , TextDecoration (..)
  , Theme (..)
  , fontBold
  , fontItalic
  , fontMono
  , styleFg
  )
import NanoUI.Types (Color (..), Rect (..), V2 (..))
import NanoUI.Widgets.Node (Response, addWidget, respClicked, respHovered, respRect)

-- | A piece of a paragraph: text in one style, and the hyperlink it follows when
-- it is one. A string literal is 'plain' text.
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
-- for every piece.
richTextWith :: Ui :> es => (Layout -> Layout) -> [Inline] -> Eff es (Maybe Text)
richTextWith f pieces = snd <$> richTextWith' f pieces

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

newtype Paragraphs = Paragraphs (IORef (IM.IntMap Paragraph))

richTextWith' :: Ui :> es => (Layout -> Layout) -> [Inline] -> Eff es (Response, Maybe Text)
richTextWith' f pieces = do
  ctx <- askContext
  inp <- askInput
  base <- f <$> askDefaultLayout
  theme <- uiTheme
  wid <- nextId
  let styled = [(txt, pieceFont l, pieceColor theme l target, target) | Inline txt style target <- pieces, let l = style base]
  cacheRef <-
    uiIO $
      askHostIO ctx >>= \case
        Just (Paragraphs ref) -> pure ref
        Nothing -> do
          ref <- newIORef IM.empty
          setHost ctx (Paragraphs ref)
          pure ref
  gen <- uiIO (readIORef (ctxMetricGen ctx))
  let key =
        foldl'
          ( \h (txt, TextFont size variant weight fstyle deco, Color rgba, target) ->
              h `hashWithSalt` txt `hashWithSalt` size `hashWithSalt` fromEnum variant
                `hashWithSalt` fromEnum weight `hashWithSalt` fromEnum fstyle `hashWithSalt` fromEnum deco
                `hashWithSalt` rgba `hashWithSalt` target
          )
          gen
          styled
  cached <- uiIO (IM.lookup (intKey wid) <$> readIORef cacheRef)
  para0 <- case cached of
    Just para | paraKey para == key -> pure para
    _ -> uiIO $ do
      resolved <- mapM (measurePiece ctx) (zip [0 ..] styled)
      let runs = smallArrayFromList (map fst resolved)
          tokens = concatMap snd resolved
          emptyLine = case resolved of
            (run, _) : _ -> (runLineHeight run, runAscent run)
            [] -> (fmLineHeight (ctxFontMetrics ctx), fmAscent (ctxFontMetrics ctx))
      pure (Paragraph key runs tokens emptyLine (lineBoxes (layoutLines runs emptyLine 1e9 tokens)) (-1) [])
  resp <- addWidget wid NodeDrawing T.empty 0 base
  let Rect rx ry rw _ = respRect resp
      runs = paraRuns para0
      layoutAt width = layoutLines runs (paraEmptyLine para0) width (paraTokens para0)
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
            case [ tokenRun tok
                 | line <- paraLines para
                 , my >= ry + lineTop line && my < ry + lineTop line + lineHeight line
                 , (x, tok) <- lineTokens line
                 , tokenKind tok /= Break
                 , mx >= rx + x && mx < rx + x + tokenWidth tok
                 , isJust (runTarget (indexSmallArray runs (tokenRun tok)))
                 ] of
              run : _ -> Just run
              [] -> Nothing
      -- Words are drawn one by one, so a decoration is drawn once across a
      -- piece's words on a line and the spaces between them.
      draw _cdc (Rect x0 y0 w _) =
        V.fromList $
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
      decorationOf runIdx
        | Just runIdx == hoveredRun = underlined (textFontDecoration (runFont (indexSmallArray runs runIdx)))
        | otherwise = textFontDecoration (runFont (indexSmallArray runs runIdx))
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
      drawKey = key `hashWithSalt` maybe (-1) id hoveredRun
  uiIO $ do
    unless (paraWidth para0 == rw && isJust cached && fmap paraKey cached == Just key) $
      modifyIORef' cacheRef $ \m ->
        -- Paragraphs no longer drawn are dropped all at once past a bound.
        IM.insert (intKey wid) para (if IM.size m > 4096 then IM.empty else m)
    registerCustomMeasure ctx wid $ \_ (availW, _) ->
      if availW >= 1e9 then paraNatural para else lineBoxes (linesAt availW)
    registerCustomDrawing ctx wid (if drawKey == 0 then 1 else drawKey) draw
    registerCustomCursor ctx wid (const (if isJust hoveredRun then UiCursorPointer else UiCursorDefault))
  let clicked
        | respClicked resp = hoveredRun >>= runTarget . indexSmallArray runs
        | otherwise = Nothing
  pure (resp, clicked)
  where
    underlined DecorationStrikethrough = DecorationUnderlineStrike
    underlined DecorationNone = DecorationUnderline
    underlined deco = deco
    lineBoxes lines' = (maximum (0 : map lineWidth lines'), sum (map lineHeight lines'))

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

-- | Greedy lines at @width@: a break goes between words only at spaces or
-- line breaks, spaces at a wrap are dropped, and a word wider than the line
-- takes a line of its own.
layoutLines :: SmallArray Run -> (Float, Float) -> Float -> [Token] -> [Line]
layoutLines runs (emptyH, emptyAscent) width = go 0 [] 0 [] True
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
      let toks = reverse placed
          metrics = [indexSmallArray runs (tokenRun tok) | (_, tok) <- toks]
          (h, ascent) = case metrics of
            [] -> (emptyH, emptyAscent)
            _ ->
              let ascent' = maximum (map runAscent metrics)
                  descent = maximum [runLineHeight r - runAscent r | r <- metrics]
               in (ascent' + descent, ascent')
       in Line top h ascent x toks
