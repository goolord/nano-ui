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
  , cornerRadius
  , defaultLayout
  , drawCheckbox
  , drawCircle
  , drawRect
  , drawStrokeCircle
  , drawingVersioned
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
  , runCanvas
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

-- | How a document is drawn. Colours left 'Nothing' come from the theme.
data MarkdownConfig = MarkdownConfig
  { mdLayout :: !(Layout -> Layout)
  -- ^ The document's column.
  , mdText :: !(Layout -> Layout)
  -- ^ The body text's font: @fontSize 15@, @fontColor c@.
  , mdHeading :: !(Int -> Layout -> Layout)
  -- ^ A heading's font, from its level (1 to 6), over the body text's.
  , mdLinkColor :: !(Maybe Color)
  -- ^ Links, or the theme's link colour.
  , mdCodeColor :: !(Maybe Color)
  -- ^ Inline code, or the theme's orange.
  , mdCodeBackground :: !(Maybe Color)
  -- ^ Behind code blocks, or the theme's input background.
  , mdCopyCode :: !Bool
  -- ^ Whether a code block has a button that copies its code.
  , mdImage :: !(Text -> Maybe (ImageId, Size))
  -- ^ The image registered for an image's source, and the size to draw it
  -- at in logical pixels. An image alone in its paragraph, or alone in a
  -- link there, is drawn when this has one: a click on it goes to the link,
  -- else to its source, and its title, else the link's, shows as a tooltip.
  -- Any other image shows its alt text as a link to the same place.
  }

-- | A full-width column, body text in the theme's font, headings from 1.6
-- times its size down to 0.9, theme colours, copy buttons on code blocks, and
-- no images.
defaultMarkdownConfig :: MarkdownConfig
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
    , mdCodeColor = Nothing
    , mdCodeBackground = Nothing
    , mdCopyCode = True
    , mdImage = const Nothing
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
markdownConfigured :: Ui :> es => MarkdownConfig -> MarkdownDoc -> Eff es (Maybe Text)
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
data Env = Env
  { envCfg :: !MarkdownConfig
  , envTheme :: !Theme
  , envText :: !(Layout -> Layout)
  , envSize :: !Float
  , envDepth :: !Int
  , envMetrics :: !FontMetrics
  }

-- | Blocks one after another, each under its position and kind as its key,
-- so a block that turns into another kind as text streams in, a paragraph
-- into a heading, a table or a drawn image, gets fresh ids.
blocks :: Ui :> es => Env -> [Block] -> Eff es (Maybe Text)
blocks env bs = asum <$> zipWithM (\i b -> let (kind, draw) = block env b in withKey (i, kind) draw) [0 :: Int ..] bs

-- | A block's kind, and how it is drawn.
block :: Ui :> es => Env -> Block -> (Int, Eff es (Maybe Text))
block env = \case
  Paragraph xs
    | Just (target, title, iid, Size w h) <- soleImage (envCfg env) xs ->
        ( 13
        , do
            resp <- image' (fixedWH w h) iid
            unless (T.null title) (tooltip resp title)
            pure (if respClicked resp then Just target else Nothing)
        )
    | otherwise -> (0, richTextWith (tight . fillW . envText env) (inlines env xs))
  Heading level xs ->
    let title = richTextWith (tight . fillW . mdHeading (envCfg env) level . envText env) (inlines env xs)
     in (level, if level <= 2 then columnWith (tight . fillW . gap 4) (title <* separator) else title)
  ThematicBreak -> (7, Nothing <$ separator)
  CodeBlock info code -> (8, Nothing <$ codeBlock env info code)
  BlockQuote bs ->
    ( 9
    , rowWith (tight . fillW . gap 10) $ do
        box (fixedW 3 . fillH) (themeSeparator (envTheme env))
        columnWith (tight . fillW . gap 10) $
          blocks env {envText = fontColor (themeMuted (envTheme env)) . envText env} bs
    )
  List ty isTight items -> (case ty of Bullet _ -> 10; Ordered _ _ -> 11, listBlock env ty isTight items)
  Table aligns header rows -> (12, tableBlock env aligns header rows)

-- | The image a paragraph shows, when it holds an image alone, or alone in a
-- link, that the configuration draws: where a click on it goes, its title
-- (else the link's), and the image and its size.
soleImage :: MarkdownConfig -> [Span] -> Maybe (Text, Text, ImageId, Size)
soleImage cfg = \case
  [Image src title _] -> drawn src src title
  [Link url linkTitle [Image src title _]] -> drawn url src (if T.null title then linkTitle else title)
  _ -> Nothing
  where
    drawn target src title = (\(iid, size) -> (target, title, iid, size)) <$> mdImage cfg src

-- | Spans as rich-text pieces: emphasis, strikethrough and code change the
-- font, and a link's pieces all go to its destination, as an image's alt
-- text does to its link's or its own source.
inlines :: Env -> [Span] -> [Inline]
inlines env = concatMap (go id Nothing)
  where
    theme = envTheme env
    cfg = envCfg env
    linkColor' = fromMaybe (themeLink theme) (mdLinkColor cfg)
    codeColor = fromMaybe (themeOrange theme) (mdCodeColor cfg)
    go style target = \case
      Str t -> [piece style target t]
      SoftBreak -> [piece style target " "]
      HardBreak -> [piece style target "\n"]
      Emph xs -> concatMap (go (fontItalic . style) target) xs
      Strong xs -> concatMap (go (fontBold . style) target) xs
      Strike xs -> concatMap (go (fontStrike . style) target) xs
      Code t -> [piece (fontMono . maybe (fontColor codeColor) (const id) target . style) target t]
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
codeBlock :: Ui :> es => Env -> Text -> Text -> Eff es ()
codeBlock env info code = do
  let theme = envTheme env
      cfg = envCfg env
      bg = fromMaybe (styleBg (themeInput theme)) (mdCodeBackground cfg)
      language = T.takeWhile (not . isSpace) info
  styled (panelStyle (background bg . borderColor (themeSeparator theme) . cornerRadius 6)) $
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
listBlock :: Ui :> es => Env -> ListType -> Bool -> [ListItem] -> Eff es (Maybe Text)
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
      marker v h shape = void (drawingVersioned v (fixedWH markerW h) (\(Rect x y _ _) -> runCanvas (shape x y)))
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

-- | A list marker's drawing version: its shape's tag (1 a bullet, 2 a check
-- box) in the low bits, so an item that turns into a task item redraws its
-- marker, over a hash of what else the marker's ops depend on besides its
-- size. It is never 0, which 'drawingVersioned' takes for no version.
version :: Hashable a => Int -> a -> Int
version tag deps = hash deps * 4 + tag

-- | A table as a grid of equal columns: header cells bold on a tinted row, and
-- one-pixel rules between the cells. A cell's lines, wrapped or not, sit as
-- its column's alignment says.
tableBlock :: Ui :> es => Env -> [CellAlign] -> [[Span]] -> [[[Span]]] -> Eff es (Maybe Text)
tableBlock env aligns header rows = do
  let theme = envTheme env
      rule = themeSeparator theme
      bodyBg = styleBg (themePanel theme)
      headBg = lerpColor bodyBg rule 0.35
      flat c = panelStyle (background c . borderWidth 0 . cornerRadius 0)
      cell isHeader (align, spans) =
        styled (flat (if isHeader then headBg else bodyBg)) $
          panelWith (padXY 8 5 . fillW . fillH) $
            richTextWith
              (tight . alignOf align . (if isHeader then fontBold else id) . envText env)
              (inlines env spans)
      alignOf = \case
        CellCenter -> alignCenter
        CellRight -> alignEnd
        _ -> alignStart
  styled (flat rule) $
    panelWith (padAll 1 . fillW) $
      gridWith (length aligns) (tight . fillW . gap 1) $ do
        heads <- mapM (cell True) (zip aligns header)
        cells <- mapM (mapM (cell False) . zip aligns) rows
        pure (asum heads <|> asum (map asum cells))
