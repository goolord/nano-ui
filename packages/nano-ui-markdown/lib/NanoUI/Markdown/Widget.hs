-- | The Markdown widget: a document drawn as nano-ui rich text and layout.
module NanoUI.Markdown.Widget
  ( MarkdownConfig (..)
  , defaultMarkdownConfig
  , markdown
  , markdownConfigured
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void, when, zipWithM)
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.Hashable (Hashable, hash)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, type (:>))
import NanoUI
  ( Color
  , FontMetrics (fmLineHeight)
  , ImageId
  , Inline
  , Layout (..)
  , Rect (..)
  , Size (..)
  , Style (..)
  , Theme (..)
  , Ui
  , V2 (..)
  , alignCenter
  , alignEnd
  , alignMid
  , alignStart
  , askDefaultLayout
  , background
  , borderColor
  , borderWidth
  , box
  , buttonWith
  , checkboxBoxSize
  , colorToWord32
  , columnWith
  , CanvasConfig (..)
  , canvasConfigured
  , cornerRadius
  , defaultCanvasConfig
  , defaultLayout
  , drawCheckbox
  , drawCircle
  , drawRect
  , drawStrokeCircle
  , fillH
  , fillW
  , fixedW
  , fixedWH
  , flex
  , fontBold
  , fontColor
  , fontItalic
  , fontMono
  , fontMuted
  , fontSemiBold
  , fontSize
  , fontSizeScale
  , fontStrike
  , gap
  , gridWith
  , hyperlink
  , image'
  , inlineBackground
  , inlineWith
  , labelWith
  , lerpColor
  , lineWidthUi
  , padAll
  , padXY
  , panelStyle
  , panelWith
  , resolveFontUi
  , respClicked
  , restyle
  , richTextWith
  , rowWith
  , scope
  , separator
  , setClipboard
  , styled
  , subtle
  , tight
  , tooltip
  , uiFontSize
  , uiTheme
  , whenM
  , withKey
  )
import NanoUI.Markdown.Document (MarkdownDoc, markdownBlocks)
import NanoUI.Markdown.Syntax

-- | How a document is drawn. Colours left 'Nothing' come from the theme,
-- and each style modifier goes over the look it is named for, so
-- @mdCodeBlock = background c@ changes the code block's background and
-- keeps its border.
--
-- The configuration carries the view's effect row, as
-- 'NanoUI.PaneGridConfig' does, since 'mdBlock' runs widgets of the caller's
-- own.
data MarkdownConfig es = MarkdownConfig
  { mdLayout :: !(Layout -> Layout)
  -- ^ The document's column.
  , mdText :: !(Layout -> Layout)
  -- ^ The body text's font: @fontSize 15@, @fontColor c@.
  , mdHeading :: !(Int -> Layout -> Layout)
  -- ^ A heading's font, from its level (1 to 6), over the body text's.
  , mdLinkColor :: !(Maybe Color)
  -- ^ Links, or the theme's link colour.
  , mdInlineCode :: !(Layout -> Layout)
  -- ^ Inline code's font, over monospace in the theme's orange (default:
  -- 'id').
  , mdInlineCodeBackground :: !(Maybe Color)
  -- ^ A colour behind inline code (default: none).
  , mdCodeBlock :: !(Style -> Style)
  -- ^ A code block's panel, over the theme's input background with a
  -- separator-coloured border and rounded corners (default: 'id').
  , mdQuote :: !(Layout -> Layout)
  -- ^ Quoted text, over the text around it in the theme's muted colour
  -- (default: 'id').
  , mdTableCell :: !(Bool -> Style -> Style)
  -- ^ A table cell's panel, given whether it is a header cell, over the
  -- flat panel colour, tinted for the header (default: @const id@).
  , mdCopyCode :: !Bool
  -- ^ Whether a code block has a button that copies its code.
  , mdImage :: !(Text -> Maybe (ImageId, Size))
  -- ^ The image registered for an image's source, and the size to draw it
  -- at in logical pixels. An image alone in its paragraph, or alone in a
  -- link there, is drawn when this has one: a click on it goes to the link,
  -- else to its source, and its title, else the link's, shows as a tooltip.
  -- Any other image shows its alt text as a link to the same place.
  , mdBlock :: !((Block -> Eff es (Maybe Text)) -> Block -> Eff es (Maybe Text))
  -- ^ Draw a block your own way, given the widget's own drawing of a block
  -- to fall back to or wrap (default: 'id'). Asked of every block at every
  -- depth, those in quotes and list items too, it can highlight code, load
  -- an image as it comes into view, or put chrome of its own around a block.
  -- The widget's drawing draws a block where it is, muted in a quote, and
  -- asks this of the blocks inside it. What it returns is the link clicked,
  -- as for 'markdown'. Either way a block is drawn under a key of its
  -- position and kind, so a block that is appended to keeps the ids of what
  -- is drawn for it.
  --
  -- > mdBlock = \own -> \case
  -- >   CodeBlock "haskell" code -> Nothing <$ highlighted code
  -- >   b -> own b
  }

-- | A full-width column, body text in the theme's font, headings from 1.6
-- times its size down to 0.9, theme colours, copy buttons on code blocks,
-- no images, and every block drawn by the widget.
defaultMarkdownConfig :: MarkdownConfig es
defaultMarkdownConfig =
  MarkdownConfig
    { mdLayout = tight . fillW . gap 10
    , mdText = id
    , mdHeading = \level ->
        let scale = case level of
              1 -> 1.6
              2 -> 1.35
              3 -> 1.2
              4 -> 1.05
              5 -> 1
              _ -> 0.9
         in fontSizeScale scale . (if level <= 4 then fontBold else fontSemiBold)
    , mdLinkColor = Nothing
    , mdInlineCode = id
    , mdInlineCodeBackground = Nothing
    , mdCodeBlock = id
    , mdQuote = id
    , mdTableCell = const id
    , mdCopyCode = True
    , mdImage = const Nothing
    , mdBlock = id
    }

-- | Draw a document. Returns the destination of a link clicked this frame.
--
-- Each top-level block is drawn under a key of its own, its position and
-- kind, so appending to a document keeps the ids, and with them the cached
-- text layout, of the blocks before the one that changed. Rich text is not
-- selectable; a code block's button copies its code.
markdown :: Ui :> es => MarkdownDoc -> Eff es (Maybe Text)
markdown = markdownConfigured defaultMarkdownConfig

-- | 'markdown' drawn as the configuration says.
markdownConfigured :: Ui :> es => MarkdownConfig es -> MarkdownDoc -> Eff es (Maybe Text)
markdownConfigured cfg doc = do
  theme <- uiTheme
  size <- uiFontSize
  -- The body text's size is set, to the backend's default where nothing
  -- else sets one, so that headings and small text scale it rather than 16.
  let text = mdText cfg . \l -> if layoutFontSize l > 0 then l else fontSize size l
  base <- text <$> askDefaultLayout
  fm <- resolveFontUi (layoutFontSize base) (layoutFontWeight base) (layoutFontStyle base) (layoutFontVariant base)
  let env =
        Env
          { envCfg = cfg
          , envTheme = theme
          , envText = text
          , envSize = layoutFontSize base
          , envDepth = 0
          , envMetrics = fm
          }
  columnWith (mdLayout cfg) (blocks env (markdownBlocks doc))

-- What a block is drawn with: the configuration, the theme, the text's
-- modifier where it is (a quote mutes it), the body text's size, how deep in
-- lists it is, and the body font's metrics, which list markers are sized by.
data Env es = Env
  { envCfg :: !(MarkdownConfig es)
  , envTheme :: !Theme
  , envText :: !(Layout -> Layout)
  , envSize :: !Float
  , envDepth :: !Int
  , envMetrics :: !FontMetrics
  }

-- | Blocks one after another, each under its position and kind as its key,
-- so a block that turns into another kind as text streams in, a paragraph
-- into a heading, a table or a drawn image, gets fresh ids.
--
-- 'mdBlock' draws each block under that key, given the widget's own
-- drawing.
blocks :: Ui :> es => Env es -> [Block] -> Eff es (Maybe Text)
blocks env bs = asum <$> zipWithM draw [0 :: Int ..] bs
  where
    own = block env
    draw i b = withKey (i, blockKind (envCfg env) b) (mdBlock (envCfg env) own b)

-- | A block's kind, for its key: a paragraph drawn as an image is a kind of
-- its own.
blockKind :: MarkdownConfig es -> Block -> Int
blockKind cfg = \case
  Paragraph xs
    | isJust (soleImage cfg xs) -> 13
    | otherwise -> 0
  Heading level _ -> level
  ThematicBreak -> 7
  CodeBlock {} -> 8
  BlockQuote _ -> 9
  List (Bullet _) _ _ -> 10
  List (Ordered _ _) _ _ -> 11
  Table {} -> 12

-- | How the widget draws a block. Kept out of 'blocks', which hands it to
-- 'mdBlock': inlined there, its closures over the 'Env' would be built at
-- every call of 'blocks', whichever blocks it draws.
{-# NOINLINE block #-}
block :: Ui :> es => Env es -> Block -> Eff es (Maybe Text)
block env = \case
  Paragraph xs
    | Just (target, title, iid, Size w h) <- soleImage (envCfg env) xs -> do
        resp <- image' (fixedWH w h) iid
        unless (T.null title) (tooltip resp title)
        pure (if respClicked resp then Just target else Nothing)
    | otherwise -> richTextWith (tight . fillW . envText env) (inlines env xs)
  Heading level xs ->
    let title = richTextWith (tight . fillW . mdHeading (envCfg env) level . envText env) (inlines env xs)
     in if level <= 2 then columnWith (tight . fillW . gap 4) (title <* separator) else title
  ThematicBreak -> Nothing <$ separator
  CodeBlock info code -> Nothing <$ codeBlock env info code
  BlockQuote bs ->
    rowWith (tight . fillW . gap 10) $ do
      box (fixedW 3 . fillH) (themeSeparator (envTheme env))
      columnWith (tight . fillW . gap 10) $
        blocks env {envText = mdQuote (envCfg env) . fontColor (themeMuted (envTheme env)) . envText env} bs
  List ty isTight items -> listBlock env ty isTight items
  Table aligns header rows -> tableBlock env aligns header rows

-- | The image a paragraph shows, when it holds an image alone, or alone in a
-- link, that the configuration draws: where a click on it goes, its title
-- (else the link's), and the image and its size.
soleImage :: MarkdownConfig es -> [Span] -> Maybe (Text, Text, ImageId, Size)
soleImage cfg = \case
  [Image src title _] -> drawn src src title
  [Link url linkTitle [Image src title _]] -> drawn url src (if T.null title then linkTitle else title)
  _ -> Nothing
  where
    drawn target src title = (\(iid, size) -> (target, title, iid, size)) <$> mdImage cfg src

-- | Spans as rich-text pieces: emphasis, strikethrough and code change the
-- font, and a link's pieces all go to its destination, as an image's alt
-- text does to its link's or its own source.
inlines :: Env es -> [Span] -> [Inline]
inlines env = concatMap (go id Nothing)
  where
    theme = envTheme env
    cfg = envCfg env
    linkColor' = fromMaybe (themeLink theme) (mdLinkColor cfg)
    codeBackground = maybe id inlineBackground (mdInlineCodeBackground cfg)
    go style target = \case
      Str t -> [piece style target t]
      SoftBreak -> [piece style target " "]
      HardBreak -> [piece style target "\n"]
      Emph xs -> concatMap (go (fontItalic . style) target) xs
      Strong xs -> concatMap (go (fontBold . style) target) xs
      Strike xs -> concatMap (go (fontStrike . style) target) xs
      Code t ->
        -- Code in a link keeps the link's colour.
        let ink = if isJust target then id else fontColor (themeOrange theme)
         in [codeBackground (piece (mdInlineCode cfg . fontMono . ink . style) target t)]
      Link url _ xs -> concatMap (go (fontColor linkColor' . style) (target <|> Just url)) xs
      Image src _ alt ->
        let txt = if null alt then "image" else spansText alt
         in [piece (fontItalic . fontColor linkColor' . style) (target <|> Just src) txt]
    piece style target t = case target of
      Nothing -> inlineWith style t
      Just url -> restyle style (hyperlink url t)

-- | Code in the theme's monospace font on a panel of its own, under a row
-- with its language and a copy button. A line wider than the document wraps,
-- keeping its indent and the spaces inside it: a sideways scroller would
-- take the wheel from the page scrolling past it, and its bar would cover
-- the code's last line.
codeBlock :: Ui :> es => Env es -> Text -> Text -> Eff es ()
codeBlock env info code = do
  let theme = envTheme env
      cfg = envCfg env
      language = T.takeWhile (not . isSpace) info
  styled (panelStyle (mdCodeBlock cfg . background (styleBg (themeInput theme)) . borderColor (themeSeparator theme) . cornerRadius 6)) $
    panelWith (padXY 10 8 . gap 4 . fillW) $ do
      scope $ when (mdCopyCode cfg || not (T.null language)) $
        rowWith (tight . fillW . alignMid) $ do
          labelWith (tight . fontMuted . fontSizeScale 0.8 . envText env) language
          flex
          when (mdCopyCode cfg) $
            whenM (styled subtle (buttonWith (padXY 6 1 . fontSize (0.8 * envSize env)) "Copy")) $
              void (setClipboard code)
      labelWith (tight . fillW . fontMono . fontColor (styleFg (themeInput theme)) . envText env) code

-- | A list: each item a marker beside its blocks, with less space between
-- a tight list's items than a loose one's. A marker takes the colour of the
-- text around it: a bullet is a disc, a ring one list deeper, and a square
-- deeper still, and a task item's check box is a nano-ui checkbox, whose
-- accent and border are the text's colour where that is not the theme's.
listBlock :: Ui :> es => Env es -> ListType -> Bool -> [ListItem] -> Eff es (Maybe Text)
listBlock env ty isTight items = do
  let fm = envMetrics env
      theme = envTheme env
      lineH = fmLineHeight fm
      side = checkboxBoxSize fm
      spacing = if isTight then 2 else 10
      number i = case ty of
        Ordered start delim -> T.pack (show (start + i)) <> T.singleton delim
        Bullet _ -> ""
  -- Every number is measured: in a proportional font an earlier one can be
  -- the widest ("10." against "11.").
  numberWs <- case ty of
    Ordered _ _ -> mapM (lineWidthUi fm . number) [0 .. length items - 1]
    Bullet _ -> pure []
  let markerW = maximum (lineH : [side | any (isJust . itemTask) items] ++ numberWs)
      ink = fromMaybe (styleFg (themePanel theme)) (layoutFontColor (envText env defaultLayout))
      marker v h shape = void (canvasConfigured defaultCanvasConfig {canvasLayout = fixedWH markerW h defaultLayout, canvasContent = v} (\(Rect x y _ _) -> shape x y))
      bullet = marker (version 1 (envDepth env, colorToWord32 ink)) lineH $ \x y ->
        let c@(V2 cx cy) = V2 (x + markerW / 2) (y + lineH / 2)
            r = max 2 (lineH * 0.15)
         in case envDepth env of
              0 -> drawCircle c r ink
              1 -> drawStrokeCircle c (r - 0.6) 1.2 ink
              _ -> drawRect (Rect (cx - r) (cy - r) (2 * r) (2 * r)) ink
      boxTheme
        | ink == styleFg (themePanel theme) = theme
        | otherwise = theme {themeAccent = ink, themeButton = (themeButton theme) {styleBorder = ink}}
      boxColors = map colorToWord32 [themeAccent boxTheme, themeOnAccent boxTheme, styleBg (themeInput boxTheme), styleBorder (themeButton boxTheme)]
      boxH = max lineH side
      checkBox done = marker (version 2 (done, boxColors)) boxH $ \x y ->
        drawCheckbox boxTheme (Rect (x + (markerW - side) / 2) (y + (boxH - side) / 2) side side) done
  columnWith (tight . fillW . gap spacing) $
    asum <$> zipWithM
      ( \i (ListItem task bs) -> withKey i $
          rowWith (tight . fillW . gap 6) $ do
            case (task, ty) of
              (Just done, _) -> checkBox done
              (Nothing, Ordered _ _) -> labelWith (tight . fixedW markerW . alignEnd . envText env) (number i)
              (Nothing, Bullet _) -> bullet
            columnWith (tight . fillW . gap spacing) $
              blocks env {envDepth = envDepth env + 1} bs
      )
      [0 :: Int ..]
      items

-- | A list marker's content key: its shape's tag (1 a bullet, 2 a check
-- box) in the low bits, so an item that turns into a task item redraws its
-- marker, over a hash of what else the marker's ops depend on besides its
-- size. It is never 0, which a canvas takes for no key.
version :: Hashable a => Int -> a -> Int
version tag deps = hash deps * 4 + tag

-- | A table as a grid of equal columns: header cells bold on a tinted row, and
-- one-pixel rules between the cells. A cell's lines, wrapped or not, sit as
-- its column's alignment says.
tableBlock :: Ui :> es => Env es -> [CellAlign] -> [[Span]] -> [[[Span]]] -> Eff es (Maybe Text)
tableBlock env aligns header rows = do
  let theme = envTheme env
      rule = themeSeparator theme
      bodyBg = styleBg (themePanel theme)
      headBg = lerpColor bodyBg rule 0.35
      flat c = background c . borderWidth 0 . cornerRadius 0
      cell isHeader (align, spans) =
        styled (panelStyle (mdTableCell (envCfg env) isHeader . flat (if isHeader then headBg else bodyBg))) $
          panelWith (padXY 8 5 . fillW . fillH) $
            richTextWith
              (tight . alignOf align . (if isHeader then fontBold else id) . envText env)
              (inlines env spans)
      alignOf = \case
        CellCenter -> alignCenter
        CellRight -> alignEnd
        _ -> alignStart
  styled (panelStyle (flat rule)) $
    panelWith (padAll 1 . fillW) $
      gridWith (length aligns) (tight . fillW . gap 1) $ do
        heads <- mapM (cell True) (zip aligns header)
        cells <- mapM (mapM (cell False) . zip aligns) rows
        pure (asum heads <|> asum (map asum cells))
